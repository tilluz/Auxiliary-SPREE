library(tidyverse)
library(randomForest)
library(rgdal)
library(foreach)
library(doParallel)
library(data.table)

registerDoParallel(5)
set.seed(12345)

years <- c(2013:2020)
B <- 1000

setwd("your_directory_path")

map_com <- readOGR(dsn = './data/midsave', layer="map_com_harmonised", verbose = F)
map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)
map_com$CODE <- as.character(map_com$CODE)
map_com$deptid <- substr(map_com$COD_ENTITE, start = 1, stop = 3)

# Census
pop13 <- readRDS('./data/midsave/census_2013_imputed_application_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n,
         deptid = substr(COD_ENTITE, start = 1, stop = 3),
         regid = substr(COD_ENTITE, start = 1, stop = 2)) %>% 
  filter(m == 1)


# ANSD population projections
pop_proj <- readOGR(dsn = './data/midsave', layer="demographic_projections", verbose = F)@data %>% 
  transmute(COD_ENTITE = as.character(COD_ENTITE),
            proj_2013 = as.integer(as.character(ENSEMBLE20)),
            proj_2014 = as.integer(as.character(ENSEMBLE_1)),
            proj_2015 = as.integer(as.character(ENSEMBLE_2)),
            proj_2016 = as.integer(as.character(ENSEMBLE_3)),
            proj_2017 = as.integer(as.character(ENSEMBLE_4)),
            proj_2018 = as.integer(as.character(ENSEMBLE_5)),
            proj_2019 = as.integer(as.character(ENSEMBLE_6)),
            proj_2020 = as.integer(as.character(ENSEMBLE_7))
  ) %>% left_join(pop13 %>% 
                    select(COD_ENTITE, deptid, regid) %>% 
                    distinct(), by = 'COD_ENTITE')

# Input data -----------------------------------------------------------

# Built-up areas (share of land)
bsgm_mean <- readRDS('./data/midsave/bsgmi_mean_com_boot.rds') %>% 
  left_join(readRDS('./data/midsave/bsgme_mean_com_boot.rds'), by = c('geoid', 'b')) %>% 
  rename_all(~str_replace_all(., '100m', '100m_mean')) %>%
  rename_all(~str_replace_all(., 'bsgmi', 'bsgm')) %>%
  rename_all(~str_replace_all(., 'bsgme', 'bsgm')) %>%
  rename_all(~str_replace_all(., 'sd', 'mean_sd')) %>%
  select(geoid, matches('2013|2014|2015|2016|2017|2018|2019|2020'), b) %>% 
  filter(b == sample.int(100, size = 1))

# # Built-up areas (number of pixels)
# bsgm_sum <- readRDS('./data/midsave/bsgmi_sum_com_boot.rds') %>% 
#   rename_all(~str_replace_all(., '100m', '100m_sum')) %>%
#   select(geoid, matches('2013|2014|2015|2016|2017|2018|2019|2020'), b) %>% 
#   filter(b == sample.int(100, size = 1))

esaccilc_dst <- readRDS('./data/midsave/esaccilc_dst_com_boot.rds') %>% 
  select(geoid, matches('2013|2014|2015|2016|2017|2018|2019|2020'), b) %>% 
  filter(b == sample.int(100, size = 1))

other_dst <- readRDS('./data/midsave/other_dst_com_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
  filter(b == sample.int(100, size = 1))

nature_dst <- readRDS('./data/midsave/nature_dst_com_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
  select(geoid, matches('2013|2014|2015|2016|2017|2018|2019|2020'), b) %>% 
  filter(b == sample.int(100, size = 1))

viirs <- readRDS('./data/midsave/viirs_com_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
  select(geoid, matches('2013|2014|2015|2016|2017|2018|2019|2020'), b) %>% 
  filter(b == sample.int(100, size = 1))

srtm <- readRDS('./data/midsave/srtm_com_boot.rds') %>% 
  filter(b == sample.int(100, size = 1))

weather <- readRDS('./data/midsave/weather_com.rds') %>% 
  rename(geoid = COD_ENTITE)

# 2013 -----------------------------------------------------------

dat <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(across(c(proj_2013:proj_2020), sum)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% 
              select(geoid, contains('2013')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% 
              select(geoid, contains('2013')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% 
              select(geoid, contains('2013')), 
            by = 'geoid') %>% 
  left_join(viirs %>% 
              select(geoid, contains('2013')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(proj_2013/SUPERFICIE))

x_data <- dat %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2013', '')) 

y_data <- dat %>% select(dens) %>% as.matrix() %>% as.numeric()


# 2014 --------------------------------------------------------------------

dat_14 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2014)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2013')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2014')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2014')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2014')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_14 <- dat_14 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2013', '')) %>% 
  rename_all(~str_replace_all(., '_2014', '')) 

# 2015 --------------------------------------------------------------------

dat_15 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2015)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2015')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_15 <- dat_15 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2015', '')) 

# 2016 --------------------------------------------------------------------

dat_16 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2016)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2016')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2016')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2016')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_16 <- dat_16 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2015', ''))  %>% 
  rename_all(~str_replace_all(., '_2016', '')) 

# 2017 --------------------------------------------------------------------

dat_17 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2017)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2017')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2017')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2016')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_17 <- dat_17 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2015', '')) %>% 
  rename_all(~str_replace_all(., '_2016', '')) %>% 
  rename_all(~str_replace_all(., '_2017', '')) 

# 2018 --------------------------------------------------------------------

dat_18 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2018)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2018')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2017')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2016')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_18 <- dat_18 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2015', '')) %>% 
  rename_all(~str_replace_all(., '_2016', '')) %>% 
  rename_all(~str_replace_all(., '_2017', '')) %>% 
  rename_all(~str_replace_all(., '_2018', '')) 

# 2019 --------------------------------------------------------------------

dat_19 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2019)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2019')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2017')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2016')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_19 <- dat_19 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2015', '')) %>% 
  rename_all(~str_replace_all(., '_2016', '')) %>% 
  rename_all(~str_replace_all(., '_2017', '')) %>% 
  rename_all(~str_replace_all(., '_2018', '')) %>% 
  rename_all(~str_replace_all(., '_2019', '')) 

# 2020 --------------------------------------------------------------------

dat_20 <- pop_proj %>% 
  rename(geoid = COD_ENTITE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2020)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% #2013:2020
              select(geoid, contains('2020')), 
            by = 'geoid') %>%
  left_join(esaccilc_dst %>% #2013:2015
              select(geoid, contains('2015')), 
            by = 'geoid') %>% 
  left_join(other_dst %>% 
              select(geoid),
            by = 'geoid') %>%
  left_join(nature_dst %>% #2013:2017
              select(geoid, contains('2017')), 
            by = 'geoid') %>% 
  left_join(viirs %>% #2013:2016
              select(geoid, contains('2016')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(geoid = COD_ENTITE, SUPERFICIE), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_20 <- dat_20 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2015', '')) %>% 
  rename_all(~str_replace_all(., '_2016', '')) %>% 
  rename_all(~str_replace_all(., '_2017', '')) %>% 
  rename_all(~str_replace_all(., '_2018', '')) %>% 
  rename_all(~str_replace_all(., '_2019', '')) %>% 
  rename_all(~str_replace_all(., '_2020', '')) 


##########

ppp_com <- foreach(b = 0:B,.packages = c('randomForest', 'tidyverse'), .combine=bind_rows) %dopar% {
  x_data_b <- x_data
  ##  Now we are going tune our randomForest population density regression: 
  init_popfit = tuneRF(
    x=x_data_b, y=y_data, plot=TRUE, mtryStart=length(x_data_b)/3, 
    ntreeTry=length(y_data)/20, improve=0.0001, stepFactor=1.20, trace=F,
    doBest=TRUE, nodesize=length(y_data)/1000, na.action=na.omit, 
    importance=TRUE, proximity=TRUE, sampsize=length(y_data), replace=TRUE
  ) 
  
  ##  Optimize the model by iteratively removing any 
  ##    covariates with negative increases in node purity:
  
  ##  Get list of covariates that have an importance score greater than 0:
  importance_scores <- 
    importance(init_popfit)[order(importance(init_popfit)[,1], decreasing=TRUE),]
  pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
  
  if(length(pos_importance) == length(importance_scores[,1])){
    popfit <- init_popfit
  }
  
  while (length(pos_importance) < length(importance_scores[,1])) {
    
    ## Subset the x_data to just those columns having positive scores:
    x_data_b <- x_data_b[pos_importance]
    popfit = tuneRF(
      x=x_data, y=y_data, plot=TRUE, mtryStart=length(x_data_b)/3, 
      ntreeTry=length(y_data)/20, improve=0.0001, stepFactor=1.20, trace=F, 
      doBest=TRUE, nodesize=length(y_data)/1000, na.action=na.omit, 
      importance=TRUE, proximity=TRUE, sampsize=length(y_data), replace=TRUE
    ) 
    
    ## Re-check importance scores:
    importance_scores <- importance(popfit)[order(importance(popfit)[,1], decreasing=TRUE),]
    pos_importance <- rownames(importance_scores)[importance_scores[,1] > 0]
    
  }
  
  output_com <- randomForest(x = x_data_b, y = y_data, ntree = popfit$ntree, mtry = popfit$mtry,
                             nodesize = length(y_data)/1000, importance = F, proximity = TRUE,
                             do.trace = F)
  
  dat %>% 
    mutate(pred_13 = exp(predict(output_com, newdata = x_data))*SUPERFICIE,
           pred_14 = exp(predict(output_com, newdata = x_data_14))*SUPERFICIE,
           pred_15 = exp(predict(output_com, newdata = x_data_15))*SUPERFICIE,
           pred_16 = exp(predict(output_com, newdata = x_data_16))*SUPERFICIE,
           pred_17 = exp(predict(output_com, newdata = x_data_17))*SUPERFICIE,
           pred_18 = exp(predict(output_com, newdata = x_data_18))*SUPERFICIE,
           pred_19 = exp(predict(output_com, newdata = x_data_19))*SUPERFICIE,
           pred_20 = exp(predict(output_com, newdata = x_data_20))*SUPERFICIE) %>% 
    group_by(regid) %>%
    transmute(geoid, 
              pred_2013 = pred_13/sum(pred_13)*sum(proj_2013),
              pred_2014 = pred_14/sum(pred_14)*sum(proj_2014),
              pred_2015 = pred_15/sum(pred_15)*sum(proj_2015),
              pred_2016 = pred_16/sum(pred_16)*sum(proj_2016),
              pred_2017 = pred_17/sum(pred_17)*sum(proj_2017),
              pred_2018 = pred_18/sum(pred_18)*sum(proj_2018),
              pred_2019 = pred_19/sum(pred_19)*sum(proj_2019),
              pred_2020 = pred_20/sum(pred_20)*sum(proj_2020),
              bb = b) %>% 
    ungroup() %>% 
    mutate(bb = b) %>% 
    select(-regid)
  
}

stopImplicitCluster()

saveRDS(ppp_com, file = paste0("./data/midsave/ppp_recalc_com_application.rds"))

