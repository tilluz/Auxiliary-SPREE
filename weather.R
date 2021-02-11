library(raster)
library(rgdal)
library(tidyverse)

setwd("your_directory_path")

# Communes
map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
weather_com <- map_com@data %>% 
  select(COD_ENTITE, lon = LONGITUDE, lat = LATITUDE)

prec.files <- list.files("./data/weather/prec/", ".tif", full.names=TRUE)
prec_raw <- stack(prec.files)

tavg.files <- list.files("./data/weather/tavg/", ".tif", full.names=TRUE)
tavg_raw <- stack(tavg.files)

prec <- raster::extract(prec_raw,weather_com[,2:3]) %>% 
  as.data.frame() %>% 
  mutate(COD_ENTITE = weather_com$COD_ENTITE,
         arr = substr(COD_ENTITE, start = 1, stop = 6)) %>% 
  group_by(arr) %>% 
  mutate(across(-c('COD_ENTITE'), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>% 
  ungroup() %>% 
  select(-COD_ENTITE, -arr) %>% 
  as.matrix()

tavg <- raster::extract(tavg_raw,weather_com[,2:3]) %>% 
  as.data.frame() %>% 
  mutate(COD_ENTITE = weather_com$COD_ENTITE,
         arr = substr(COD_ENTITE, start = 1, stop = 6)) %>% 
  group_by(arr) %>% 
  mutate(across(-c('COD_ENTITE'), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>% 
  ungroup() %>% 
  select(-COD_ENTITE, -arr) %>% 
  as.matrix()

weather_com <- weather_com %>% 
  select(-lon, -lat) %>% 
  mutate(prec_sum = apply(prec, 1, sum, na.rm = T),
         prec_sd = apply(prec, 1, sd, na.rm = T),
         tavg_mean = apply(tavg, 1, mean, na.rm = T),
         tavg_sd = apply(tavg, 1, sd, na.rm = T))

saveRDS(weather_com, file = "./data/midsave/weather_com.rds")
  
# Arrondissements
map_arr <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
weather_arr <- map_arr@data %>% 
  select(CODE, lon = x, lat = y)

prec.files <- list.files("./data/weather/prec/", ".tif", full.names=TRUE)
prec_raw <- stack(prec.files)

tavg.files <- list.files("./data/weather/tavg/", ".tif", full.names=TRUE)
tavg_raw <- stack(tavg.files)

prec <- raster::extract(prec_raw,weather_arr[,2:3]) %>% 
  as.data.frame() %>% 
  mutate(CODE = weather_arr$CODE,
         dept = substr(CODE, start = 1, stop = 3)) %>% 
  group_by(dept) %>% 
  mutate(across(-c('CODE'), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>% 
  ungroup() %>% 
  select(-CODE, -dept) %>% 
  as.matrix()

tavg <- raster::extract(tavg_raw,weather_arr[,2:3]) %>% 
  as.data.frame() %>% 
  mutate(CODE = weather_arr$CODE,
         dept = substr(CODE, start = 1, stop = 3)) %>% 
  group_by(dept) %>% 
  mutate(across(-c('CODE'), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>% 
  ungroup() %>% 
  select(-CODE, -dept) %>% 
  as.matrix()

weather_arr <- weather_arr %>% 
  select(-lon, -lat) %>% 
  mutate(prec_sum = apply(prec, 1, sum, na.rm = T),
         prec_sd = apply(prec, 1, sd, na.rm = T),
         tavg_mean = apply(tavg, 1, mean, na.rm = T),
         tavg_sd = apply(tavg, 1, sd, na.rm = T))

saveRDS(weather_arr, file = "./data/midsave/weather_arr.rds")

# Departments
map_dept <- readOGR(dsn = './data/midsave/', layer="map_dept_harmonised", verbose = F)
weather_dept <- map_dept@data %>% 
  select(deptid = dept02, lon = x, lat = y)

prec.files <- list.files("./data/weather/prec/", ".tif", full.names=TRUE)
prec_raw <- stack(prec.files)

tavg.files <- list.files("./data/weather/tavg/", ".tif", full.names=TRUE)
tavg_raw <- stack(tavg.files)

prec <- raster::extract(prec_raw,weather_dept[,2:3]) %>% 
  as.data.frame() %>% 
  mutate(deptid = weather_dept$deptid,
         reg = substr(deptid, start = 1, stop = 2)) %>% 
  group_by(reg) %>% 
  mutate(across(-c('deptid'), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>% 
  ungroup() %>% 
  select(-deptid, -reg) %>% 
  as.matrix()

tavg <- raster::extract(tavg_raw,weather_dept[,2:3]) %>% 
  as.data.frame() %>% 
  mutate(deptid = weather_dept$deptid,
         reg = substr(deptid, start = 1, stop = 2)) %>% 
  group_by(reg) %>% 
  mutate(across(-c('deptid'), ~ifelse(is.na(.), mean(., na.rm=TRUE), .))) %>% 
  ungroup() %>% 
  select(-deptid, -reg) %>% 
  as.matrix()

weather_dept <- weather_dept %>% 
  select(-lon, -lat) %>% 
  mutate(prec_sum = apply(prec, 1, sum, na.rm = T),
         prec_sd = apply(prec, 1, sd, na.rm = T),
         tavg_mean = apply(tavg, 1, mean, na.rm = T),
         tavg_sd = apply(tavg, 1, sd, na.rm = T))

saveRDS(weather_dept, file = "./data/midsave/weather_dept02.rds")