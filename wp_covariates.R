library(raster)
library(tidyverse)
library(rgdal)
library(rgeos)
library(data.table)

setwd("your_directory_path")


# Harmonised Map ----------------------------------------------------------

level <-
  # 'dept02'
# 'dept13'
'arr'
# 'com'
# 'x'

# 2002 Departments
if (level == 'dept02'){
  sen_map <- readOGR(dsn = './data/midsave/', layer="map_dept_harmonised", verbose = F)
  sen_map$geoid <- as.character(sen_map$dept02)
}

# 2002 Arrondissements
if (level == 'arr'){
  sen_map <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
  sen_map$geoid <- as.character(sen_map$CODE)
}

# 2013 Departments
if (level == 'dept13'){
  sen_map <- readOGR(dsn = './data/midsave/', layer="map_dept13_harmonised", verbose = F)
  sen_map$geoid <- as.character(sen_map$dept13)
}

# 2013 Communes
if (level == 'com'){
  sen_map <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
  sen_map$geoid <- as.character(sen_map$COD_ENTITE)
}

# Intersection
if (level == 'x'){
  sen_map <- readOGR(dsn = './data/midsave/', layer="map_x", verbose = F)
  sen_map$geoid <- as.character(sen_map$x_id)
}

# Planar Map --------------------------------------------------------------

to_utm <- "+proj=utm +zone=28 +a=6378137 +rf=298.257222932867 +units=m +no_defs"
sen_map_utm <- spTransform(sen_map, to_utm)
row.names(sen_map_utm) <- sen_map_utm$geoid


# Flag for pixel bootstrap ------------------------------------------------

boot <- TRUE
b <- 100
bb <- 100
set.seed(1234)

# Function to allocate out-of-bounds pixels to areas ----------------------

pixel_allocate <- function(tmp){
  tmp <- tmp %>% 
    as.data.frame()
  
  coordinates(tmp) <- ~x+y
  proj4string(tmp) <- proj4string(sen_map)
  
  dat <- tmp@data %>% 
    mutate(pixel_id = as.character(1:n()),
           geoid = over(tmp, sen_map[,'geoid'])[[1]],
           x = tmp@coords[,'x'],
           y = tmp@coords[,'y'])
  
  missings <- dat %>% 
    filter(is.na(geoid)) %>% 
    column_to_rownames('pixel_id')
  
  coordinates(missings) <- ~x+y
  proj4string(missings) <- proj4string(sen_map)
  missings_utm <- spTransform(missings, to_utm)
  
  missings <- gDistance(sen_map_utm, missings_utm, byid = TRUE) %>% 
    as.data.frame() %>% 
    rownames_to_column('pixel_id')  %>%
    mutate(geoid = colnames(.[,-1])[max.col(-.[,-1],"first")]) %>%
    select(pixel_id, geoid)
  
  dat <- dat %>% 
    filter(!is.na(geoid)) %>% 
    select(pixel_id, geoid) %>% 
    bind_rows(missings) %>% 
    left_join(dat %>% select(-geoid), by = 'pixel_id') %>% 
    select(-pixel_id, -x, -y)
}

# Population count estimates ----------------------------------------------

for (i in c(2003, 2013:2020)){ #2002:2020
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/sen_ppp_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/sen_ppp_",i,".tif")) %>% 
      rasterToPoints()
  }
  print(i)
}

# if(boot == TRUE){
#   pixel_df <- pixel_allocate(tmp)
#   
#   ppp <- data.frame()
#   for (i in 1:b){
#     ppp <- pixel_df %>% 
#       group_by(geoid) %>% 
#       sample_n(size = n(), replace = TRUE) %>% 
#       summarise_all(list(sum = sum, sd = sd)) %>% 
#       mutate(b = i) %>% 
#     bind_rows(ppp)
#     
#     print(paste0("Boot ",i," of ",b," runs done!"))
#   }
#   saveRDS(ppp, file = paste0("./data/midsave/ppp_",level,"_boot.rds"))
#   
# }else{
#   ppp <- pixel_allocate(tmp) %>% 
#     group_by(geoid) %>% 
#     summarise_all(list(sum = sum, sd = sd))
#   
#   saveRDS(ppp, file = paste0("./data/midsave/ppp_",level,".rds"))
# }
# 


if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  
  ppp <- data.table()
  for (i in 1:b){
    ppp_b <- pixel_df %>% 
      group_by(geoid) %>% 
      sample_n(size = n(), replace = TRUE)
    
    ppp_bb <- data.table()
    
    for (j in 1:bb){
      
      tmp <- ppp_b %>% 
        group_by(geoid) %>% 
        sample_n(size = n(), replace = TRUE) %>% 
        summarise_all(list(sum = sum)) %>% 
        mutate(b = i,
               bb = j) %>% 
        as.data.table()
      
      ppp_bb <- rbindlist(list(ppp_bb, tmp))
      
    }
    
    tmp <- ppp_b %>% 
      summarise_all(list(sum = sum)) %>% 
      mutate(b = i,
             bb = 0) %>% 
      as.data.table()
    
    ppp <- rbindlist(list(ppp, tmp, ppp_bb))
    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(ppp, file = paste0("./data/midsave/ppp_",level,"_boot.rds"))
  
}else{
  ppp <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    summarise_all(list(sum = sum, sd = sd))
  
  saveRDS(ppp, file = paste0("./data/midsave/ppp_",level,".rds"))
}




rm(tmp, ppp)

# Distance to Areas -------------------------------------------------------

# Area types

'
          011: Cultivated area
          040: Woody-tree area
          130: Shrub area
          140: Herbacious area
          150: Sparse vegetation area
          160: Aquatic vegetation area
          190: Artificial surface area
          200: Bare area
          '

if(boot == TRUE){
  esaccilc_dst <- data.frame(geoid = sort(rep(sen_map@data$geoid, b)),
                             b = rep((1:b), length(unique(sen_map@data$geoid)))) %>% 
    mutate(geoid = geoid %>% as.character())
}else{
  esaccilc_dst <- sen_map %>% as.data.frame() %>% mutate(geoid = geoid %>% as.character()) %>% select(geoid)
}



for (i in c('esaccilc_dst011',
            'esaccilc_dst040',
            'esaccilc_dst130',
            'esaccilc_dst140',
            'esaccilc_dst150',
            'esaccilc_dst160',
            'esaccilc_dst190',
            'esaccilc_dst200'
)){
  for (j in c(2002, 2003, 2012:2015)){ #2002:2015
    if (exists("tmp") == TRUE){
      tmp <- raster(paste0("./data/world_pop/sen_",i,"_100m_",j,".tif")) %>% 
        rasterToPoints() %>% 
        .[, 3, drop = F] %>% 
        cbind(tmp)
    } else{
      tmp <- raster(paste0("./data/world_pop/sen_",i,"_100m_",j,".tif")) %>% 
        rasterToPoints()
    }
    print(paste0(i,': ',j))
  }
  
  if(boot == TRUE){
    pixel_df <- pixel_allocate(tmp)
    
    ind_df <- data.frame()
    for (i in 1:b){
      ind_df <- pixel_df %>%
        group_by(geoid) %>%
        sample_n(size = n(), replace = TRUE) %>%
        transmute_at(vars(-group_cols()), abs) %>%
        summarise_all(list(mean = mean, sd = sd)) %>%
        mutate(b = i) %>%
        bind_rows(ind_df)
    }
    esaccilc_dst <- ind_df %>% 
      left_join(esaccilc_dst, by = c('geoid', 'b'))  
  }else{
    esaccilc_dst <- pixel_allocate(tmp) %>% 
      group_by(geoid) %>% 
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      right_join(esaccilc_dst, by = 'geoid')
  }
  rm(tmp)
}

if(boot == TRUE){
  saveRDS(esaccilc_dst, file = paste0("./data/midsave/esaccilc_dst_",level,"_boot.rds"))
}else{
  saveRDS(esaccilc_dst, file = paste0("./data/midsave/esaccilc_dst_",level,".rds"))
}


rm(esaccilc_dst)

# Other Distances

'
          - Coastline
          - Major roads
          - Major road intersections
          - Major waterways
          - Open coastlines
          '

for (i in c('sen_dst_coastline_100m_2000_2020',
            'sen_osm_dst_road_100m_2016',
            'sen_osm_dst_roadintersec_100m_2016',
            'sen_osm_dst_waterway_100m_2016',
            'sen_esaccilc_dst_water_100m_2000_2012'
            
)){
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/",i,".tif")) %>% 
      rasterToPoints()
  }
}

if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  

  other_dst <- data.frame()
  for (i in 1:b){
    other_dst <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(other_dst)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(other_dst, file = paste0("./data/midsave/other_dst_",level,"_boot.rds"))
  
}else{
  other_dst <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    transmute_at(vars(-group_cols()), abs) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(other_dst, file = paste0("./data/midsave/other_dst_",level,".rds"))
}

rm(tmp, other_dst)

# Distance to nature reserve & wilderness area

for (i in c(2002, 2003, 2012:2017)){ #2002:2017
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/sen_wdpa_dst_cat1_100m_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/sen_wdpa_dst_cat1_100m_",i,".tif")) %>% 
      rasterToPoints()
  }
}

if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  
  
  nature_dst <- data.frame()
  for (i in 1:b){
    nature_dst <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(nature_dst)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(nature_dst, file = paste0("./data/midsave/nature_dst_",level,"_boot.rds"))
  
}else{
  nature_dst <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    transmute_at(vars(-group_cols()), abs) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(nature_dst, file = paste0("./data/midsave/nature_dst_",level,".rds"))
}

rm(tmp, nature_dst)



# Night-time lights -------------------------------------------------------

# DMSP-OLS 2002-2011

for (i in c(2002, 2003, 2010, 2011)){ #2002:2011
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/sen_dmsp_100m_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/sen_dmsp_100m_",i,".tif")) %>% 
      rasterToPoints()
  }
}

if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  
  
  dmsp <- data.frame()
  for (i in 1:b){
    dmsp <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(dmsp)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(dmsp, file = paste0("./data/midsave/dmsp_",level,"_boot.rds"))
  
}else{
  dmsp <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(dmsp, file = paste0("./data/midsave/dmsp_",level,".rds"))
}

rm(tmp, dmsp)

# VIIRS 2012-2016

for (i in c(2012:2016)){ #2012:2016
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/sen_viirs_100m_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/sen_viirs_100m_",i,".tif")) %>% 
      rasterToPoints()
  }
}

if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  
  viirs <- data.frame()
  for (i in 1:b){
    viirs <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(viirs)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(viirs, file = paste0("./data/midsave/viirs_",level,"_boot.rds"))
  
}else{
  viirs <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(viirs, file = paste0("./data/midsave/viirs_",level,".rds"))
}

rm(tmp, viirs)


# Settlement areas --------------------------------------------------------

# Years 2002-2013

for (i in c(2002, 2003, 2011, 2013)){ #2002:2011,2013
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/sen_bsgmi_v0a_100m_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/sen_bsgmi_v0a_100m_",i,".tif")) %>% 
      rasterToPoints()
  }
}

pixel_df <- pixel_allocate(tmp)

if(boot == TRUE){
  
  
  bsgmi_sum <- data.frame()
  for (i in 1:b){
    bsgmi_sum <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(sum = sum, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(bsgmi_sum)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(bsgmi_sum, file = paste0("./data/midsave/bsgmi_sum_",level,"_boot.rds"))
  
}else{
  bsgmi_sum <- pixel_df %>% 
    group_by(geoid) %>% 
    summarise_all(list(sum = sum, sd = sd))
  
  saveRDS(bsgmi_sum, file = paste0("./data/midsave/bsgmi_sum_",level,".rds"))
}

if(boot == TRUE){
  
  
  bsgmi_mean <- data.frame()
  for (i in 1:b){
    bsgmi_mean <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(bsgmi_mean)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(bsgmi_mean, file = paste0("./data/midsave/bsgmi_mean_",level,"_boot.rds"))
  
}else{
  bsgmi_mean <- pixel_df %>% 
    group_by(geoid) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(bsgmi_mean, file = paste0("./data/midsave/bsgmi_mean_",level,".rds"))
}

rm(tmp, bsgmi_sum, bsgmi_mean)

# Years 2015-2020

for (i in 2015:2020){ #2015:2020
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/sen_bsgme_v0a_100m_",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/sen_bsgme_v0a_100m_",i,".tif")) %>% 
      rasterToPoints()
  }
}

pixel_df <- pixel_allocate(tmp)

if(boot == TRUE){
  
  bsgme_sum <- data.frame()
  for (i in 1:b){
    bsgme_sum <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(sum = sum, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(bsgme_sum)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(bsgme_sum, file = paste0("./data/midsave/bsgme_sum_",level,"_boot.rds"))
  
}else{
  bsgme_sum <- pixel_df %>% 
    group_by(geoid) %>% 
    summarise_all(list(sum = sum, sd = sd))
  
  saveRDS(bsgme_sum, file = paste0("./data/midsave/bsgme_sum_",level,".rds"))
}


if(boot == TRUE){
  
  
  bsgme_mean <- data.frame()
  for (i in 1:b){
    bsgme_mean <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(bsgme_mean)

    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(bsgme_mean, file = paste0("./data/midsave/bsgme_mean_",level,"_boot.rds"))
  
}else{
  bsgme_mean <- pixel_df %>% 
    group_by(geoid) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(bsgme_mean, file = paste0("./data/midsave/bsgme_mean_",level,".rds"))
}

rm(tmp)

# Slope and topography (SRTM)

for (i in c('sen_srtm_slope_100m',
            'sen_srtm_topo_100m'
            
)){
  if (exists("tmp") == TRUE){
    tmp <- raster(paste0("./data/world_pop/srtm/",i,".tif")) %>% 
      rasterToPoints() %>% 
      .[, 3, drop = F] %>% 
      cbind(tmp)
  } else{
    tmp <- raster(paste0("./data/world_pop/srtm/",i,".tif")) %>% 
      rasterToPoints()
  }
}

if(boot == TRUE){
  pixel_df <- pixel_allocate(tmp)
  
  
  srtm <- data.frame()
  for (i in 1:b){
    srtm <- pixel_df %>%
      group_by(geoid) %>%
      sample_n(size = n(), replace = TRUE) %>%
      transmute_at(vars(-group_cols()), abs) %>%
      summarise_all(list(mean = mean, sd = sd)) %>%
      mutate(b = i) %>%
      bind_rows(srtm)
    
    print(paste0("Boot ",i," of ",b," runs done!"))
  }
  saveRDS(srtm, file = paste0("./data/midsave/srtm_",level,"_boot.rds"))
  
}else{
  srtm <- pixel_allocate(tmp) %>% 
    group_by(geoid) %>% 
    transmute_at(vars(-group_cols()), abs) %>% 
    summarise_all(list(mean = mean, sd = sd))
  
  saveRDS(srtm, file = paste0("./data/midsave/srtm_",level,".rds"))
}

rm(tmp, srtm)

