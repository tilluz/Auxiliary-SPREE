library(tidyverse)
library(randomForest)
library(rgdal)
library(foreach)
library(doParallel)
library(data.table)

registerDoParallel(detectCores()-1)
set.seed(12345)

R = 500
B = 100

setwd("your_directory_path")

map_x <- readOGR(dsn = './data/midsave/', layer="map_x", verbose = F)
map_x$geoid <- as.character(map_x$x_id)
map_x$COD_ENTITE <- as.character(map_x$COD_ENTITE)
map_x$CODE <- as.character(map_x$CODE)

map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)
map_com$CODE <- as.character(map_com$CODE)
map_com$deptid <- substr(map_com$CODE, start = 1, stop = 3)

# Census
pop02 <- readRDS('./data/midsave/census_2002_imputed_simulation_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n,
         CODE = as.character(geoid),
         deptid = substr(CODE, start = 1, stop = 3),
         regid = substr(CODE, start = 1, stop = 2),
         children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes')),
         hh_size = factor(ifelse(hhmembers_n == 1, 0,
                                 ifelse((hhmembers_n == 2) & (children_n == 0), 1,
                                        ifelse((hhmembers_n == 2) & (children_n != 0), 2,
                                               ifelse((hhmembers_n >2) & (hhmembers_n < 6), 2, 3)))),
                          levels = c(0,1,2,3),
                          labels = c('single', 'couple', 'small', 'large'))) %>% 
  filter(m == 1)

pop13 <- readRDS('./data/midsave/census_2013_imputed_application_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n,
         deptid = substr(CODE, start = 1, stop = 3),
         regid = substr(CODE, start = 1, stop = 2),
         children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes')),
         hh_size = factor(ifelse(hhmembers_n == 1, 0,
                                 ifelse((hhmembers_n == 2) & (children_n == 0), 1,
                                        ifelse((hhmembers_n == 2) & (children_n != 0), 2,
                                               ifelse((hhmembers_n >2) & (hhmembers_n < 6), 2, 3)))),
                          levels = c(0,1,2,3),
                          labels = c('single', 'couple', 'small', 'large'))) %>% 
  filter(m == 1)


# ANSD population projections
pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
  select(COD_ENTITE, URBAN, ENSEMBLE20) %>% 
  transmute(COD_ENTITE = as.character(COD_ENTITE),
            proj_2013 = as.integer(as.character(ENSEMBLE20))
  ) %>% left_join(pop13 %>% 
                    select(COD_ENTITE, CODE, deptid, regid) %>% 
                    distinct(), by = 'COD_ENTITE')


# 2002 Arrondissements -----------------------------------------------------------

ppp_arr <- foreach(r=1:R, .combine=bind_rows) %dopar% {

# Built-up areas (share of land)
bsgm_mean <- readRDS('./data/midsave/bsgmi_mean_arr_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_mean')) %>%
  select(geoid, contains('2002'), contains('2013'), b) %>% 
  filter(b == sample.int(100, size = 1))

# Built-up areas (number of pixels)
bsgm_sum <- readRDS('./data/midsave/bsgmi_sum_arr_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>%
  select(geoid, contains('2002'), contains('2013'), b) %>% 
  filter(b == sample.int(100, size = 1))

esaccilc_dst <- readRDS('./data/midsave/esaccilc_dst_arr_boot.rds') %>% 
  select(geoid, contains('2002'), contains('2013'), b) %>% 
  filter(b == sample.int(100, size = 1))

other_dst <- readRDS('./data/midsave/other_dst_arr_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
  filter(b == sample.int(100, size = 1))

nature_dst <- readRDS('./data/midsave/nature_dst_arr_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
  select(geoid, contains('2002'), contains('2013'), b) %>% 
  filter(b == sample.int(100, size = 1))

dmsp <- readRDS('./data/midsave/dmsp_arr_boot.rds') %>% 
  rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
  select(geoid, contains('2002'), contains('2011'), b) %>% 
  filter(b == sample.int(100, size = 1))

srtm <- readRDS('./data/midsave/srtm_arr_boot.rds') %>% 
  filter(b == sample.int(100, size = 1))

weather <- readRDS('./data/midsave/weather_arr.rds') %>% 
  rename(geoid = CODE)


dat <- pop02 %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(wt)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% 
              select(-contains('2013')), 
            by = 'geoid') %>%
  # left_join(bsgm_sum by = 'geoid') %>% 
  left_join(esaccilc_dst %>% 
              select(-contains('2013')), 
            by = 'geoid') %>% 
  left_join(other_dst, by = 'geoid') %>%
  left_join(nature_dst %>% 
              select(-contains('2013')), 
            by = 'geoid') %>% 
  left_join(dmsp %>% 
              select(-contains('2011')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(COD_ENTITE, geoid = CODE, SUPERFICIE) %>% 
              group_by(geoid) %>% 
              summarise(across(SUPERFICIE, sum)) %>% 
              ungroup(), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data <- dat %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2002', ''))

y_data <- dat %>% select(dens) %>% as.matrix() %>% as.numeric()


# 2013 Arrondissements -----------------------------------------------------------


dat_13 <- pop_proj %>% 
  rename(geoid = CODE) %>% 
  group_by(geoid, deptid, regid) %>% 
  summarise(pop = sum(proj_2013)) %>% 
  ungroup() %>% 
  left_join(bsgm_mean %>% 
              select(-contains('2002')), 
            by = 'geoid') %>%
  # left_join(bsgm_sum, by = 'geoid') %>% 
  left_join(esaccilc_dst %>% 
              select(-contains('2002')), 
            by = 'geoid') %>% 
  left_join(other_dst, by = 'geoid') %>%
  left_join(nature_dst %>% 
              select(-contains('2002')), 
            by = 'geoid') %>% 
  left_join(dmsp %>% 
              select(-contains('2002')), 
            by = 'geoid') %>% 
  left_join(weather, by = 'geoid') %>%
  left_join(srtm, by = 'geoid') %>%
  left_join(map_com@data %>% 
              select(COD_ENTITE, geoid = CODE, SUPERFICIE) %>% 
              group_by(geoid) %>% 
              summarise(across(SUPERFICIE, sum)) %>% 
              ungroup(), by = 'geoid') %>% 
  mutate(dens = log(pop/SUPERFICIE))

x_data_13 <- dat_13 %>% 
  select(regid, deptid, ends_with('_mean'), ends_with('sum'), ends_with('_sd')) %>% 
  rename_all(~str_replace_all(., '_2013', '')) %>% 
  rename_all(~str_replace_all(., '_2011', ''))  


##########

rf <- data.frame()

for (b in 0:B){
  
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
      x=x_data_b, y=y_data, plot=TRUE, mtryStart=length(x_data_b)/3, 
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
  
  rf <- dat_13 %>% 
    mutate(pred_log = predict(output_com, newdata = x_data_13),
           ppp13_wp = exp(pred_log)*SUPERFICIE) %>% 
    group_by(regid) %>%
    transmute(geoid, 
              ppp13_sum = ppp13_wp/sum(ppp13_wp)*sum(pop),
              bb = b) %>% 
    ungroup() %>% 
    select(-regid) %>% 
    bind_rows(rf)
  
}
rf %>% 
  mutate(b = r)
}

stopImplicitCluster()

saveRDS(ppp_arr, file = paste0("./data/midsave/ppp_recalc_arr_02_13_boot_alt.rds"))

