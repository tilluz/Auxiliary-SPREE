library(rgdal)
library(tidyverse)
library(rgeos)

setwd("your_directory_path")

# Load harmonised map -----------------------------------------------------

sen_map <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
sen_map$geoid <- as.character(sen_map$CODE)

# Planar Map --------------------------------------------------------------

to_utm <- "+proj=utm +zone=28 +a=6378137 +rf=298.257222932867 +units=m +no_defs"
sen_map_utm <- spTransform(sen_map, to_utm)
row.names(sen_map_utm) <- sen_map_utm$geoid

# Define function to allocate clusters to harmonised map ------------------

cluster_allocate <- function(cl_map){
  tmp <- cl_map %>% 
    over(., sen_map, fn = NULL) %>% 
    select(geoid) %>% 
    rownames_to_column(., var = 'DHSCLUST') %>% 
    mutate(DHSCLUST = DHSCLUST %>% as.integer()) %>% 
    left_join(.
              , cl_map %>% 
                as.data.frame %>% 
                select(DHSCLUST, URBAN_RURA, LATNUM, LONGNUM) %>% 
                mutate(DHSCLUST = DHSCLUST %>% as.integer())
              , by = 'DHSCLUST')
  
  missings <- tmp %>% 
    filter(is.na(geoid) & LONGNUM != 0) %>% 
    column_to_rownames('DHSCLUST')
  
  if(dim(missings)[1] != 0){
    coordinates(missings) <- ~LONGNUM+LATNUM
    proj4string(missings) <- proj4string(sen_map)
    missings_utm <- spTransform(missings, to_utm)
    
    missings <- gDistance(sen_map_utm, missings_utm, byid = TRUE) %>% 
      as.data.frame() %>% 
      rownames_to_column('DHSCLUST')  %>%
      mutate(geoid = colnames(.[,-1])[max.col(-.[,-1],"first")],
             DHSCLUST = DHSCLUST %>% as.integer) %>%
      select(DHSCLUST, geoid)
    
    cl_map <- tmp %>% 
      filter(!is.na(geoid) & LONGNUM != 0) %>% 
      select(DHSCLUST, geoid) %>% 
      bind_rows(missings) %>% 
      mutate(DHSCLUST = DHSCLUST %>% as.integer()) %>% 
      left_join(tmp %>% select(-geoid), by = 'DHSCLUST')
  } else{
    cl_map <- tmp %>% 
      filter(!is.na(geoid) & LONGNUM != 0)
  }
} 

# Load survey cluster locations -------------------------------------------

# DHS 2005
cl05 <-  readOGR(dsn = './data/dhs_2005/snge4bfl/', layer="SNGE4BFL", verbose = F)
row.names(cl05) <- cl05$DHSCLUST

cl05 <- cluster_allocate(cl_map = cl05)

saveRDS(cl05, file = "./data/midsave/cl05.rds")

# DHS 2011
cl11 <-  readOGR(dsn = './data/dhs_2011/GPS/', layer="SNGE61FL", verbose = F)
row.names(cl11) <- cl11$DHSCLUST

cl11 <- cluster_allocate(cl_map = cl11)

saveRDS(cl11, file = "./data/midsave/cl11.rds")

# DHS 2013
cl13 <-  readOGR(dsn = './data/dhs_2013/snge6afl/', layer="SNGE6AFL", verbose = F)
row.names(cl13) <- cl13$DHSCLUST

cl13 <- cluster_allocate(cl_map = cl13)

saveRDS(cl13, file = "./data/midsave/cl13.rds")

# DHS 2014
cl14 <-  readOGR(dsn = './data/dhs_2014/snge71fl/', layer="SNGE6RFL", verbose = F)
row.names(cl14) <- cl14$DHSCLUST

cl14 <- cluster_allocate(cl_map = cl14)

saveRDS(cl14, file = "./data/midsave/cl14.rds")
