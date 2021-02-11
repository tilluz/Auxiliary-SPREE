library(tidyverse)
library(rgdal)
library(rgeos)
library(readxl)
library(runjags)
library(rjags)
library(coda)

setwd("your_directory_path")


# Select type of analysis -------------------------------------------------

type <- 'simulation02'

if(type == 'simulation02'){
  
  # Load data ---------------------------------------------------------------
  
  # Map
  map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
  map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)
  map_com$geoid <- as.character(map_com$CODE)
  
  map_arr <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
  map_arr$geoid <- as.character(map_arr$CODE)
  
  # Census 2002
  census02 <- readRDS('./data/midsave/census_2002_imputed_simulation.rds') %>% 
    group_by(geoid) %>% 
    summarise(type = median(as.integer(residence_type)),
              N_02 = sum(wt),
              G_02 = sum(wt[gender=="Feminin"], na.rm = T)/sum(wt, na.rm = T)) %>% 
    ungroup() %>% 
  mutate(type = as.integer(type))
  
  # Census 2013
  pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
    select(COD_ENTITE, ENSEMBLE20) %>% 
    transmute(COD_ENTITE = as.character(COD_ENTITE),
              N_13 = as.double(as.character(ENSEMBLE20))) %>% 
    left_join(map_com@data %>% select(COD_ENTITE, geoid), by = 'COD_ENTITE') %>% 
    group_by(geoid) %>% 
    summarise(N_13 = sum(N_13, na.rm = T)) %>% 
    ungroup()

  # Load covariates ---------------------------------------------------------

  # Built-up areas (share of land)
  bsgm_mean <- readRDS('./data/midsave/bsgmi_mean_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_mean'))
  
  # Built-up areas (number of pixels)
  bsgm_sum <- readRDS('./data/midsave/bsgmi_sum_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum'))
  
  esaccilc_dst <- readRDS('./data/midsave/esaccilc_dst_arr.rds')
  
  nature_dst <- readRDS('./data/midsave/nature_dst_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum'))
  
  dmsp <- readRDS('./data/midsave/dmsp_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum'))
  
  weather <- readRDS('./data/midsave/weather_arr.rds')
  
  # Prepare JAGS model ------------------------------------------------------
  
  model <- read_file("./data/hb/pop_estimation_jags_simulation_02_13_full.txt")
  
  # Prepare data ------------------------------------------------------------
  
  data <- map_com@data %>% 
    select(geoid, A = SUPERFICIE) %>% 
    group_by(geoid) %>% 
    summarise(A = sum(A, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(regid = as.double(substr(geoid, start = 1, stop = 2)),
           deptid = as.double(substr(geoid, start = 3, stop = 3))) %>% 
    left_join(pop_proj, by = 'geoid') %>% 
    left_join(census02, by = 'geoid') %>% 
    left_join(bsgm_mean %>% 
                select(geoid, contains('2002'), contains('2013')), by = 'geoid') %>% 
    left_join(bsgm_sum %>% 
                select(geoid, contains('2002'), contains('2013')), by = 'geoid') %>% 
    left_join(esaccilc_dst %>% 
                select(geoid, contains('2002'), contains('2013')), by = 'geoid') %>% 
    left_join(nature_dst %>% 
                select(geoid, contains('2002'), contains('2013')), by = 'geoid') %>% 
    left_join(dmsp %>% 
                select(geoid, contains('2002'), contains('2011')), by = 'geoid') %>% 
    left_join(weather %>% 
                rename(geoid = CODE), by = 'geoid')

  jd <- list(N = data$N_02 %>% as.double(), 
             A = data$A,
             type = data$type, 
             region = data$regid %>% as.double(), 
             state = data$deptid %>% as.double(),
             bsgm_02_m = data$sen_bsgmi_v0a_100m_mean_2002_mean,
             bsgm_02_m_sd = data$sen_bsgmi_v0a_100m_mean_2002_sd,
             # bsgm_02_m_agr = data$sen_bsgm_v0a_100m_mean_2002_mean_agr,
             # bsgm_02_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2002_mean_sd_agr,
             bsgm_02_s = data$sen_bsgmi_v0a_100m_sum_2002_sum,
             bsgm_02_s_sd = data$sen_bsgmi_v0a_100m_sum_2002_sd,
             # bsgm_02_s_agr = data$sen_bsgm_v0a_100m_sum_2002_sum_agr,
             # bsgm_02_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2002_sum_sd_agr,
             dst011_02 = data$sen_esaccilc_dst011_100m_2002_mean,
             dst011_02_sd = data$sen_esaccilc_dst011_100m_2002_sd,
             dst040_02 = data$sen_esaccilc_dst040_100m_2002_mean,
             dst040_02_sd = data$sen_esaccilc_dst040_100m_2002_sd,
             dst130_02 = data$sen_esaccilc_dst130_100m_2002_mean,
             dst130_02_sd = data$sen_esaccilc_dst130_100m_2002_sd,
             dst140_02 = data$sen_esaccilc_dst140_100m_2002_mean,
             dst140_02_sd = data$sen_esaccilc_dst140_100m_2002_sd,
             dst150_02 = data$sen_esaccilc_dst150_100m_2002_mean,
             dst150_02_sd = data$sen_esaccilc_dst150_100m_2002_sd,
             dst160_02 = data$sen_esaccilc_dst160_100m_2002_mean,
             dst160_02_sd = data$sen_esaccilc_dst160_100m_2002_sd,
             dst190_02 = data$sen_esaccilc_dst190_100m_2002_mean,
             dst190_02_sd = data$sen_esaccilc_dst190_100m_2002_sd,
             dst200_02 = data$sen_esaccilc_dst200_100m_2002_mean,
             dst200_02_sd = data$sen_esaccilc_dst200_100m_2002_sd,
             wdpa_02 = data$sen_wdpa_dst_cat1_100m_sum_2002_mean,
             wdpa_02_sd = data$sen_wdpa_dst_cat1_100m_sum_2002_sd,
             dmsp_02 = data$sen_dmsp_100m_sum_2002_mean,
             dmsp_02_sd = data$sen_dmsp_100m_sum_2002_sd,
             prec_02 = data$prec_sum,
             prec_02_sd = data$prec_sd,
             tavg_02 = data$tavg_mean,
             tavg_02_sd = data$tavg_sd,
             bsgm_13_m = data$sen_bsgmi_v0a_100m_mean_2013_mean,
             bsgm_13_m_sd = data$sen_bsgmi_v0a_100m_mean_2013_sd,
             # bsgm_13_m_agr = data$sen_bsgm_v0a_100m_mean_2013_mean_agr,
             # bsgm_13_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2013_mean_sd_agr,
             bsgm_13_s = data$sen_bsgmi_v0a_100m_sum_2013_sum,
             bsgm_13_s_sd = data$sen_bsgmi_v0a_100m_sum_2013_sd,
             # bsgm_13_s_agr = data$sen_bsgm_v0a_100m_sum_2013_sum_agr,
             # bsgm_13_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2013_sum_sd_agr,
             dst011_13 = data$sen_esaccilc_dst011_100m_2013_mean,
             dst011_13_sd = data$sen_esaccilc_dst011_100m_2013_sd,
             dst040_13 = data$sen_esaccilc_dst040_100m_2013_mean,
             dst040_13_sd = data$sen_esaccilc_dst040_100m_2013_sd,
             dst130_13 = data$sen_esaccilc_dst130_100m_2013_mean,
             dst130_13_sd = data$sen_esaccilc_dst130_100m_2013_sd,
             dst140_13 = data$sen_esaccilc_dst140_100m_2013_mean,
             dst140_13_sd = data$sen_esaccilc_dst140_100m_2013_sd,
             dst150_13 = data$sen_esaccilc_dst150_100m_2013_mean,
             dst150_13_sd = data$sen_esaccilc_dst150_100m_2013_sd,
             dst160_13 = data$sen_esaccilc_dst160_100m_2013_mean,
             dst160_13_sd = data$sen_esaccilc_dst160_100m_2013_sd,
             dst190_13 = data$sen_esaccilc_dst190_100m_2013_mean,
             dst190_13_sd = data$sen_esaccilc_dst190_100m_2013_sd,
             dst200_13 = data$sen_esaccilc_dst200_100m_2013_mean,
             dst200_13_sd = data$sen_esaccilc_dst200_100m_2013_sd,
             wdpa_13 = data$sen_wdpa_dst_cat1_100m_sum_2013_mean,
             wdpa_13_sd = data$sen_wdpa_dst_cat1_100m_sum_2013_sd,
             dmsp_11 = data$sen_dmsp_100m_sum_2011_mean,
             dmsp_11_sd = data$sen_dmsp_100m_sum_2011_sd,
             prec_13 = data$prec_sum,
             prec_13_sd = data$prec_sd,
             tavg_13 = data$tavg_mean,
             tavg_13_sd = data$tavg_sd,
             ntype=max(data$type),  
             nregion = max(data$regid),
             nstate = data %>% 
               select(regid, deptid) %>% 
               distinct() %>% 
               group_by(regid) %>% 
               count() %>% 
               ungroup() %>% 
               select(-regid) %>% 
               unlist() %>% 
               as.double(),
             nbeta = 28,
             ni = nrow(data)
  )
  
  
  # Prepare simulation ------------------------------------------------------
  
  set.seed(123)
  load.module('lecuyer')
  load.module('glm')
  
  n.adapt <- 1e3 #1e3
  n.burn <- 10e3 #10e3
  n.iter <- 10e3 #10e3
  thin <- 20 #20
  n.chains <- 1
  
  ## Inits function
  inits <- function(chains=n.chains, dat=data){
    inits.out <- list()
    
    for (c in 1:chains){
      inits.i <- list()
      
      inits.i$mu_alpha_national <- runif(1, 2, 6)
      inits.i$sigma_alpha_national <- runif(1, 0, 1)
      
      inits.i$beta <- runif(28, -1 , 1)
      
      inits.i$mu_sigmaD_national <- runif(1, 0, 1)
      inits.i$sigma_sigmaD_national <- runif(1, 0, 1)
      
      inits.i$D <- dat$N_02 / dat$A
      
      inits.i$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
      inits.i$.RNG.seed = c
      inits.out[[c]] <- inits.i
    }
    return(inits.out)
  }
  
  init <- inits(chains=n.chains)
  
  # Parameters to monitor (shiny app needs N[i,t])
  par.monitor <- c('mu_alpha_national'
                   ,'sigma_alpha_national'
                   ,'mu_sigmaD_national'
                   ,'sigma_sigmaD_national'
                   ,'beta','alpha0_type'
                   ,'sigmaD0_type'
                   ,'alpha0_region'
                   ,'sigmaD0_region'
                   # ,'alpha0_state'
                   # ,'sigmaD0_state'
                   ,'alpha'
                   ,'sigmaD'
                   ,'Dhat02'
                   ,'Nhat02'
                   ,'Dbar02'
                   # ,'Nbar'
                   ,'Dhat13'
                   ,'Nhat13'
                   ,'Dbar13'
  )
  
  
  # Run JAGS ----------------------------------------------------------------
  
  rj <- run.jags(model=model,
                 monitor=par.monitor,
                 data=jd,
                 n.chains=n.chains,
                 inits=init,
                 thin=thin,
                 adapt=n.adapt,
                 burnin=n.burn,
                 sample=n.iter,
                 summarise=T,
                 #keep.jags.files=TRUE, 
                 method='rjags', #'rjags' #'rjparallel' #'parallel
                 modules='glm'
  )
  
  rj.mcmc <- as.mcmc.list(rj)[[1]] %>% 
    as.data.frame() %>% 
    select(matches('Nhat13|Nhat02'))
  
  
  output <- rj.mcmc %>% 
    select(matches('Nhat02')) %>%
    pivot_longer(
      cols = starts_with("Nhat02"),
      names_to = "geoid",
      names_pattern = "Nhat02\\[(.*)\\]",
      values_to = "Nhat02"
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat13')) %>%
            pivot_longer(
              cols = starts_with("Nhat13"),
              names_to = "geoid",
              names_pattern = "Nhat13\\[(.*)\\]",
              values_to = "Nhat13"
            ) %>% 
            select(Nhat13)
          ) %>% 
    mutate(iter = 1) %>% 
    group_by(geoid) %>%
    mutate(iter = cumsum(iter)) %>% 
    ungroup() %>% 
    mutate(geoid = rep(data$geoid, max(iter)))
  
  metrics <- data.frame()
  
  for (i in 1:nrow(data)){
    metrics <- data.frame(
      'geoid' = data[i, 'geoid'],
      'true13' = data[i, 'N_13'],
      'pred13' = eval(parse(text = paste0("median(rj.mcmc$`Nhat13[",i,"]`)"))),
      'pred13_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat13[",i,"]`, probs = 0.025))"))),
      'pred13_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat13[",i,"]`, probs = 0.975))"))),
      'true02' = data[i, 'N_02'],
      'pred02' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat02[",i,"]`)"))),
      'pred02_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat02[",i,"]`, probs = 0.025))"))),
      'pred02_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat02[",i,"]`, probs = 0.975))")))) %>% 
      bind_rows(metrics)
  }
  
  saveRDS(output, file = "./data/midsave/pop_estimation_simulation_02_13.rds")
}

if(type == 'simulation13'){
  
  # Load data ---------------------------------------------------------------
  
  # Map
  map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
  map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)
  map_com$geoid <- as.character(map_com$CODE)
  
  map_arr <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
  map_arr$geoid <- as.character(map_arr$CODE)
  
  # Census 2013
  pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
    select(COD_ENTITE, URBAN, ENSEMBLE20) %>% 
    transmute(COD_ENTITE = as.character(COD_ENTITE),
              type = URBAN,
              N_13 = as.double(as.character(ENSEMBLE20))) %>% 
    left_join(map_com@data %>% select(COD_ENTITE, geoid), by = 'COD_ENTITE') %>% 
    group_by(geoid) %>% 
    summarise(type = median(as.integer(type)),
              N_13 = sum(N_13, na.rm = T)) %>% 
    mutate(type = as.integer(type)) %>% 
    ungroup()
  
  # Load covariates ---------------------------------------------------------
  
  # Built-up areas (share of land)
  bsgm_mean <- readRDS('./data/midsave/bsgmi_mean_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_mean'))
  
  # Built-up areas (number of pixels)
  bsgm_sum <- readRDS('./data/midsave/bsgmi_sum_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum'))
  
  esaccilc_dst <- readRDS('./data/midsave/esaccilc_dst_arr.rds')
  
  nature_dst <- readRDS('./data/midsave/nature_dst_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum'))
  
  dmsp <- readRDS('./data/midsave/dmsp_arr.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum'))
  
  weather <- readRDS('./data/midsave/weather_arr.rds')
  
  # Prepare JAGS model ------------------------------------------------------
  
  model <- read_file("./data/hb/pop_estimation_jags_simulation_13_13_full.txt")
  
  # Prepare data ------------------------------------------------------------
  
  data <- map_com@data %>% 
    select(geoid, A = SUPERFICIE) %>% 
    group_by(geoid) %>% 
    summarise(A = sum(A, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(regid = as.double(substr(geoid, start = 1, stop = 2)),
           deptid = as.double(substr(geoid, start = 3, stop = 3))) %>% 
    left_join(pop_proj, by = 'geoid') %>% 
    left_join(bsgm_mean %>% 
                select(geoid, contains('2013')), by = 'geoid') %>% 
    left_join(bsgm_sum %>% 
                select(geoid, contains('2013')), by = 'geoid') %>% 
    left_join(esaccilc_dst %>% 
                select(geoid, contains('2013')), by = 'geoid') %>% 
    left_join(nature_dst %>% 
                select(geoid, contains('2013')), by = 'geoid') %>% 
    left_join(dmsp %>% 
                select(geoid, contains('2011')), by = 'geoid') %>% 
    left_join(weather %>% 
                rename(geoid = CODE), by = 'geoid')
  
  jd <- list(N = data$N_13 %>% as.double(), 
             A = data$A,
             type = data$type, 
             region = data$regid %>% as.double(), 
             state = data$deptid %>% as.double(),
             bsgm_13_m = data$sen_bsgmi_v0a_100m_mean_2013_mean,
             bsgm_13_m_sd = data$sen_bsgmi_v0a_100m_mean_2013_sd,
             # bsgm_13_m_agr = data$sen_bsgm_v0a_100m_mean_2013_mean_agr,
             # bsgm_13_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2013_mean_sd_agr,
             bsgm_13_s = data$sen_bsgmi_v0a_100m_sum_2013_sum,
             bsgm_13_s_sd = data$sen_bsgmi_v0a_100m_sum_2013_sd,
             # bsgm_13_s_agr = data$sen_bsgm_v0a_100m_sum_2013_sum_agr,
             # bsgm_13_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2013_sum_sd_agr,
             dst011_13 = data$sen_esaccilc_dst011_100m_2013_mean,
             dst011_13_sd = data$sen_esaccilc_dst011_100m_2013_sd,
             dst040_13 = data$sen_esaccilc_dst040_100m_2013_mean,
             dst040_13_sd = data$sen_esaccilc_dst040_100m_2013_sd,
             dst130_13 = data$sen_esaccilc_dst130_100m_2013_mean,
             dst130_13_sd = data$sen_esaccilc_dst130_100m_2013_sd,
             dst140_13 = data$sen_esaccilc_dst140_100m_2013_mean,
             dst140_13_sd = data$sen_esaccilc_dst140_100m_2013_sd,
             dst150_13 = data$sen_esaccilc_dst150_100m_2013_mean,
             dst150_13_sd = data$sen_esaccilc_dst150_100m_2013_sd,
             dst160_13 = data$sen_esaccilc_dst160_100m_2013_mean,
             dst160_13_sd = data$sen_esaccilc_dst160_100m_2013_sd,
             dst190_13 = data$sen_esaccilc_dst190_100m_2013_mean,
             dst190_13_sd = data$sen_esaccilc_dst190_100m_2013_sd,
             dst200_13 = data$sen_esaccilc_dst200_100m_2013_mean,
             dst200_13_sd = data$sen_esaccilc_dst200_100m_2013_sd,
             wdpa_13 = data$sen_wdpa_dst_cat1_100m_sum_2013_mean,
             wdpa_13_sd = data$sen_wdpa_dst_cat1_100m_sum_2013_sd,
             dmsp_11 = data$sen_dmsp_100m_sum_2011_mean,
             dmsp_11_sd = data$sen_dmsp_100m_sum_2011_sd,
             prec_13 = data$prec_sum,
             prec_13_sd = data$prec_sd,
             tavg_13 = data$tavg_mean,
             tavg_13_sd = data$tavg_sd,
             ntype=max(data$type),  
             nregion = max(data$regid),
             nstate = data %>% 
               select(regid, deptid) %>% 
               distinct() %>% 
               group_by(regid) %>% 
               count() %>% 
               ungroup() %>% 
               select(-regid) %>% 
               unlist() %>% 
               as.double(),
             nbeta = 28,
             ni = nrow(data)
  )
  
  
  # Prepare simulation ------------------------------------------------------
  
  set.seed(123)
  load.module('lecuyer')
  load.module('glm')
  
  n.adapt <- 1e3 #1e3
  n.burn <- 10e3 #10e3
  n.iter <- 10e3 #10e3
  thin <- 20 #20
  n.chains <- 1
  
  ## Inits function
  inits <- function(chains=n.chains, dat=data){
    inits.out <- list()
    
    for (c in 1:chains){
      inits.i <- list()
      
      inits.i$mu_alpha_national <- runif(1, 2, 6)
      inits.i$sigma_alpha_national <- runif(1, 0, 1)
      
      inits.i$beta <- runif(28, -1 , 1)
      
      inits.i$mu_sigmaD_national <- runif(1, 0, 1)
      inits.i$sigma_sigmaD_national <- runif(1, 0, 1)
      
      inits.i$D <- dat$N_13 / dat$A
      
      inits.i$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
      inits.i$.RNG.seed = c
      inits.out[[c]] <- inits.i
    }
    return(inits.out)
  }
  
  init <- inits(chains=n.chains)
  
  # Parameters to monitor (shiny app needs N[i,t])
  par.monitor <- c('mu_alpha_national'
                   ,'sigma_alpha_national'
                   ,'mu_sigmaD_national'
                   ,'sigma_sigmaD_national'
                   ,'beta','alpha0_type'
                   ,'sigmaD0_type'
                   ,'alpha0_region'
                   ,'sigmaD0_region'
                   ,'Dhat13'
                   ,'Nhat13'
                   ,'Dbar13'
  )
  
  
  # Run JAGS ----------------------------------------------------------------
  
  rj <- run.jags(model=model,
                 monitor=par.monitor,
                 data=jd,
                 n.chains=n.chains,
                 inits=init,
                 thin=thin,
                 adapt=n.adapt,
                 burnin=n.burn,
                 sample=n.iter,
                 summarise=T,
                 #keep.jags.files=TRUE, 
                 method='rjags', #'rjags' #'rjparallel' #'parallel
                 modules='glm'
  )
  
  rj.mcmc <- as.mcmc.list(rj)[[1]] %>% 
    as.data.frame() %>% 
    select(matches('Nhat13'))
  
  output <- rj.mcmc %>% 
    select(matches('Nhat13')) %>%
    pivot_longer(
      cols = starts_with("Nhat13"),
      names_to = "geoid",
      names_pattern = "Nhat13\\[(.*)\\]",
      values_to = "Nhat13"
    ) %>% 
    mutate(iter = 1) %>% 
    group_by(geoid) %>%
    mutate(iter = cumsum(iter)) %>% 
    ungroup() %>% 
    mutate(geoid = rep(data$geoid, max(iter)))
  
  metrics <- data.frame()
  
  for (i in 1:nrow(data)){
    metrics <- data.frame(
      'geoid' = data[i, 'geoid'],
      'true13' = data[i, 'N_13'],
      'pred13' = eval(parse(text = paste0("median(rj.mcmc$`Nhat13[",i,"]`)"))),
      'pred13_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat13[",i,"]`, probs = 0.025))"))),
      'pred13_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat13[",i,"]`, probs = 0.975))")))) %>% 
      bind_rows(metrics)
  }
  
  saveRDS(output, file = "./data/midsave/pop_estimation_simulation_13_13.rds")
}

if(type == 'application'){
  
  # Load data ---------------------------------------------------------------
  
  # Map
  map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
  map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)
  map_com$geoid <- as.character(map_com$CODE)
  
  # Population projections
  pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
    select(COD_ENTITE, URBAN, 
           FEMME2013, HOMME2013, ENSEMBLE20, 
           FEMME2014, HOMME2014, ENSEMBLE_1,  
           FEMME2015, HOMME2015, ENSEMBLE_2, 
           FEMME2016, HOMME2016, ENSEMBLE_3, 
           FEMME2017, HOMME2017, ENSEMBLE_4, 
           FEMME2018, HOMME2018, ENSEMBLE_5, 
           FEMME2019, HOMME2019, ENSEMBLE_6, 
           FEMME2020, HOMME2020, ENSEMBLE_7) %>% 
    transmute(COD_ENTITE = as.character(COD_ENTITE),
              type = as.double(URBAN),
              N = as.double(as.character(ENSEMBLE20)),
              N_14 = as.double(as.character(ENSEMBLE_1)),
              N_15 = as.double(as.character(ENSEMBLE_2)),
              N_16 = as.double(as.character(ENSEMBLE_3)),
              N_17 = as.double(as.character(ENSEMBLE_4)),
              N_18 = as.double(as.character(ENSEMBLE_5)),
              N_19 = as.double(as.character(ENSEMBLE_6)),
              N_20 = as.double(as.character(ENSEMBLE_7)),
              g_13 = as.numeric(as.character(FEMME2013))/(as.numeric(as.character(FEMME2013)) + as.numeric(as.character(HOMME2013))),
              g_14 = as.numeric(as.character(FEMME2014))/(as.numeric(as.character(FEMME2014)) + as.numeric(as.character(HOMME2014))),
              g_15 = as.numeric(as.character(FEMME2015))/(as.numeric(as.character(FEMME2015)) + as.numeric(as.character(HOMME2015))),
              g_16 = as.numeric(as.character(FEMME2016))/(as.numeric(as.character(FEMME2016)) + as.numeric(as.character(HOMME2016))),
              g_17 = as.numeric(as.character(FEMME2017))/(as.numeric(as.character(FEMME2017)) + as.numeric(as.character(HOMME2017))),
              g_18 = as.numeric(as.character(FEMME2018))/(as.numeric(as.character(FEMME2018)) + as.numeric(as.character(HOMME2018))),
              g_19 = as.numeric(as.character(FEMME2019))/(as.numeric(as.character(FEMME2019)) + as.numeric(as.character(HOMME2019))),
              g_20 = as.numeric(as.character(FEMME2020))/(as.numeric(as.character(FEMME2020)) + as.numeric(as.character(HOMME2020))))
  
  
  # Load covariates ---------------------------------------------------------
  
  # Built-up areas (share of land)
  bsgm_mean_com <- readRDS('./data/midsave/bsgm_mean_com_boot.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_mean')) %>% 
    filter(b == 1)
  
  # Built-up areas (number of pixels)
  bsgm_sum_com <- readRDS('./data/midsave/bsgm_sum_com_boot.rds') %>% 
    rename_all(~str_replace_all(., '100m', '100m_sum')) %>% 
    filter(b == 1)
  
  # Prepare JAGS model ------------------------------------------------------
  
  model <- read_file("./data/hb/pop_estimation_jags_application.txt")
  
  # Prepare data ------------------------------------------------------------
  
  data <- map_com@data %>% 
    select(COD_ENTITE, geoid, A = SUPERFICIE) %>% 
    mutate(regid = as.double(substr(COD_ENTITE, start = 1, stop = 2)),
           deptid = as.double(substr(COD_ENTITE, start = 3, stop = 3))) %>% 
    group_by(regid, deptid) %>% 
    mutate(arrid = as.double(as.factor(geoid))) %>% 
    ungroup() %>% 
    left_join(pop_proj, by = 'COD_ENTITE') %>% 
    left_join(bsgm_mean_com %>% 
                select(COD_ENTITE = geoid, 
                       matches('2013|2014|2015|2016|2017|2018|2019|2020')), by = 'COD_ENTITE') %>% 
    left_join(bsgm_sum_com %>% 
                select(COD_ENTITE = geoid, 
                       matches('2013|2014|2015|2016|2017|2018|2019|2020')), by = 'COD_ENTITE')
  
  
  
  jd <- list(N = data$N %>% as.double(), 
             A = data$A,
             type = data$type, 
             region = data$regid %>% as.double(), 
             state = data$deptid %>% as.double(),
             local = data$arrid %>% as.double(), 
             bsgm_13_m = data$sen_bsgm_v0a_100m_mean_2013_mean,
             bsgm_13_m_sd = data$sen_bsgm_v0a_100m_mean_2013_mean_sd,
             bsgm_13_m_agr = data$sen_bsgm_v0a_100m_mean_2013_mean_agr,
             bsgm_13_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2013_mean_sd_agr,
             bsgm_13_s = data$sen_bsgm_v0a_100m_sum_2013_sum,
             bsgm_13_s_sd = data$sen_bsgm_v0a_100m_sum_2013_sum_sd,
             bsgm_13_s_agr = data$sen_bsgm_v0a_100m_sum_2013_sum_agr,
             bsgm_13_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2013_sum_sd_agr,
             bsgm_15_m = data$sen_bsgm_v0a_100m_mean_2015_mean,
             bsgm_15_m_sd = data$sen_bsgm_v0a_100m_mean_2015_mean_sd,
             bsgm_15_m_agr = data$sen_bsgm_v0a_100m_mean_2015_mean_agr,
             bsgm_15_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2015_mean_sd_agr,
             bsgm_15_s = data$sen_bsgm_v0a_100m_sum_2015_sum,
             bsgm_15_s_sd = data$sen_bsgm_v0a_100m_sum_2015_sum_sd,
             bsgm_15_s_agr = data$sen_bsgm_v0a_100m_sum_2015_sum_agr,
             bsgm_15_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2015_sum_sd_agr,
             bsgm_16_m = data$sen_bsgm_v0a_100m_mean_2016_mean,
             bsgm_16_m_sd = data$sen_bsgm_v0a_100m_mean_2016_mean_sd,
             bsgm_16_m_agr = data$sen_bsgm_v0a_100m_mean_2016_mean_agr,
             bsgm_16_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2016_mean_sd_agr,
             bsgm_16_s = data$sen_bsgm_v0a_100m_sum_2016_sum,
             bsgm_16_s_sd = data$sen_bsgm_v0a_100m_sum_2016_sum_sd,
             bsgm_16_s_agr = data$sen_bsgm_v0a_100m_sum_2016_sum_agr,
             bsgm_16_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2016_sum_sd_agr,
             bsgm_17_m = data$sen_bsgm_v0a_100m_mean_2017_mean,
             bsgm_17_m_sd = data$sen_bsgm_v0a_100m_mean_2017_mean_sd,
             bsgm_17_m_agr = data$sen_bsgm_v0a_100m_mean_2017_mean_agr,
             bsgm_17_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2017_mean_sd_agr,
             bsgm_17_s = data$sen_bsgm_v0a_100m_sum_2017_sum,
             bsgm_17_s_sd = data$sen_bsgm_v0a_100m_sum_2017_sum_sd,
             bsgm_17_s_agr = data$sen_bsgm_v0a_100m_sum_2017_sum_agr,
             bsgm_17_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2017_sum_sd_agr,
             bsgm_18_m = data$sen_bsgm_v0a_100m_mean_2018_mean,
             bsgm_18_m_sd = data$sen_bsgm_v0a_100m_mean_2018_mean_sd,
             bsgm_18_m_agr = data$sen_bsgm_v0a_100m_mean_2018_mean_agr,
             bsgm_18_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2018_mean_sd_agr,
             bsgm_18_s = data$sen_bsgm_v0a_100m_sum_2018_sum,
             bsgm_18_s_sd = data$sen_bsgm_v0a_100m_sum_2018_sum_sd,
             bsgm_18_s_agr = data$sen_bsgm_v0a_100m_sum_2018_sum_agr,
             bsgm_18_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2018_sum_sd_agr,
             bsgm_19_m = data$sen_bsgm_v0a_100m_mean_2019_mean,
             bsgm_19_m_sd = data$sen_bsgm_v0a_100m_mean_2019_mean_sd,
             bsgm_19_m_agr = data$sen_bsgm_v0a_100m_mean_2019_mean_agr,
             bsgm_19_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2019_mean_sd_agr,
             bsgm_19_s = data$sen_bsgm_v0a_100m_sum_2019_sum,
             bsgm_19_s_sd = data$sen_bsgm_v0a_100m_sum_2019_sum_sd,
             bsgm_19_s_agr = data$sen_bsgm_v0a_100m_sum_2019_sum_agr,
             bsgm_19_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2019_sum_sd_agr,
             bsgm_20_m = data$sen_bsgm_v0a_100m_mean_2020_mean,
             bsgm_20_m_sd = data$sen_bsgm_v0a_100m_mean_2020_mean_sd,
             bsgm_20_m_agr = data$sen_bsgm_v0a_100m_mean_2020_mean_agr,
             bsgm_20_m_sd_agr = data$sen_bsgm_v0a_100m_mean_2020_mean_sd_agr,
             bsgm_20_s = data$sen_bsgm_v0a_100m_sum_2020_sum,
             bsgm_20_s_sd = data$sen_bsgm_v0a_100m_sum_2020_sum_sd,
             bsgm_20_s_agr = data$sen_bsgm_v0a_100m_sum_2020_sum_agr,
             bsgm_20_s_sd_agr = data$sen_bsgm_v0a_100m_sum_2020_sum_sd_agr,
             ntype=max(data$type),  
             nregion = max(data$regid),
             nstate = data %>% 
               select(regid, deptid) %>% 
               distinct() %>% 
               group_by(regid) %>% 
               count() %>% 
               ungroup() %>% 
               select(-regid) %>% 
               unlist() %>% 
               as.double(),
             nlocal = data %>% 
               group_by(regid, deptid) %>% 
               count() %>% 
               ungroup() %>% 
               pivot_wider(
                 names_from = deptid,
                 values_from = n,
                 values_fill = 0) %>% 
               select(-regid) %>% 
               as.matrix(),
             nbeta = 8,
             ni = nrow(data)
  )
  
  
  # Prepare simulation ------------------------------------------------------
  
  set.seed(123)
  load.module('lecuyer')
  load.module('glm')
  
  n.adapt <- 1e2 #1e3
  n.burn <- 10e2 #10e3
  n.iter <- 10e2 #10e3
  thin <- 20 #20
  n.chains <- 1
  
  ## Inits function
  inits <- function(chains=n.chains, dat=data){
    inits.out <- list()
    
    for (c in 1:chains){
      inits.i <- list()
      
      inits.i$mu_alpha_national <- runif(1, 2, 6)
      inits.i$sigma_alpha_national <- runif(1, 0, 1)
      
      inits.i$beta <- runif(8, -1 , 1)
      
      inits.i$mu_sigmaD_national <- runif(1, 0, 1)
      inits.i$sigma_sigmaD_national <- runif(1, 0, 1)
      
      inits.i$D <- dat$N / dat$A
      
      inits.i$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
      inits.i$.RNG.seed = c
      inits.out[[c]] <- inits.i
    }
    return(inits.out)
  }
  
  init <- inits(chains=n.chains)
  
  # Parameters to monitor (shiny app needs N[i,t])
  par.monitor <- c('mu_alpha_national'
                   ,'sigma_alpha_national'
                   ,'mu_sigmaD_national'
                   ,'sigma_sigmaD_national'
                   ,'beta','alpha0_type'
                   ,'sigmaD0_type'
                   ,'alpha0_region'
                   ,'sigmaD0_region'
                   ,'alpha0_state'
                   ,'sigmaD0_state'
                   ,'alpha'
                   ,'sigmaD'
                   ,'Dhat13'
                   ,'Nhat13'
                   ,'Dbar13'
                   # ,'Nbar'
                   ,'Dhat15'
                   ,'Nhat15'
                   ,'Dbar15'
                   ,'Dhat16'
                   ,'Nhat16'
                   ,'Dbar16'
                   ,'Dhat17'
                   ,'Nhat17'
                   ,'Dbar17'
                   ,'Dhat18'
                   ,'Nhat18'
                   ,'Dbar18'
                   ,'Dhat19'
                   ,'Nhat19'
                   ,'Dbar19'
                   ,'Dhat20'
                   ,'Nhat20'
                   ,'Dbar20'
  )
  
  
  # Run JAGS ----------------------------------------------------------------
  
  rj <- run.jags(model=model,
                 monitor=par.monitor,
                 data=jd,
                 n.chains=n.chains,
                 inits=init,
                 thin=thin,
                 adapt=n.adapt,
                 burnin=n.burn,
                 sample=n.iter,
                 summarise=T,
                 #keep.jags.files=TRUE,
                 method='rjags', #'rjags' #'rjparallel' #'parallel
                 modules='glm'
  )
  
  
  rj.mcmc <- as.mcmc.list(rj)[[1]] %>% 
    as.data.frame() %>% 
    select(matches('Nhat13|Nhat15|Nhat16|Nhat17|Nhat18|Nhat19|Nhat20'))
  
  output <- rj.mcmc %>% 
    select(matches('Nhat13')) %>%
    pivot_longer(
      cols = everything(),
      names_to = "COD_ENTITE",
      names_pattern = "Nhat13\\[(.*)\\]",
      values_to = "Nhat13"
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat15')) %>%
            pivot_longer(
              cols = everything(),
              names_to = "COD_ENTITE",
              names_pattern = "Nhat15\\[(.*)\\]",
              values_to = "Nhat15"
            ) %>% 
            select(Nhat15)
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat16')) %>%
            pivot_longer(
              cols = everything(),
              names_to = "COD_ENTITE",
              names_pattern = "Nhat16\\[(.*)\\]",
              values_to = "Nhat16"
            ) %>% 
            select(Nhat16)
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat17')) %>%
            pivot_longer(
              cols = everything(),
              names_to = "COD_ENTITE",
              names_pattern = "Nhat17\\[(.*)\\]",
              values_to = "Nhat17"
            ) %>% 
            select(Nhat17)
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat18')) %>%
            pivot_longer(
              cols = everything(),
              names_to = "COD_ENTITE",
              names_pattern = "Nhat18\\[(.*)\\]",
              values_to = "Nhat18"
            ) %>% 
            select(Nhat18)
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat19')) %>%
            pivot_longer(
              cols = everything(),
              names_to = "COD_ENTITE",
              names_pattern = "Nhat19\\[(.*)\\]",
              values_to = "Nhat19"
            ) %>% 
            select(Nhat19)
    ) %>% 
    cbind(rj.mcmc %>% 
            select(matches('Nhat20')) %>%
            pivot_longer(
              cols = everything(),
              names_to = "COD_ENTITE",
              names_pattern = "Nhat20\\[(.*)\\]",
              values_to = "Nhat20"
            ) %>% 
            select(Nhat20)
    ) %>% 
    mutate(iter = 1) %>% 
    group_by(COD_ENTITE) %>%
    mutate(iter = cumsum(iter))
  
  metrics <- data.frame()
  
  for (i in 1:nrow(data)){
    metrics <- data.frame(
      'COD_ENTITE' = data[i, 'COD_ENTITE'],
      'true13' = data[i, 'N'],
      'pred13' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat13[",i,"]`)"))),
      'pred13_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat13[",i,"]`, probs = 0.025))"))),
      'pred13_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat13[",i,"]`, probs = 0.975))"))),
      'true15' = data[i, 'N_15'],
      'pred15' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat15[",i,"]`)"))),
      'pred15_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat15[",i,"]`, probs = 0.025))"))),
      'pred15_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat15[",i,"]`, probs = 0.975))"))),
      'true16' = data[i, 'N_16'],
      'pred16' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat16[",i,"]`)"))),
      'pred16_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat16[",i,"]`, probs = 0.025))"))),
      'pred16_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat16[",i,"]`, probs = 0.975))"))),
      'true17' = data[i, 'N_17'],
      'pred17' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat17[",i,"]`)"))),
      'pred17_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat17[",i,"]`, probs = 0.025))"))),
      'pred17_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat17[",i,"]`, probs = 0.975))"))),
      'true18' = data[i, 'N_18'],
      'pred18' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat18[",i,"]`)"))),
      'pred18_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat18[",i,"]`, probs = 0.025))"))),
      'pred18_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat18[",i,"]`, probs = 0.975))"))),
      'true19' = data[i, 'N_19'],
      'pred19' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat19[",i,"]`)"))),
      'pred19_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat19[",i,"]`, probs = 0.025))"))),
      'pred19_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat19[",i,"]`, probs = 0.975))"))),
      'true20' = data[i, 'N_20'],
      'pred20' = eval(parse(text = paste0("mean(rj.mcmc$`Nhat20[",i,"]`)"))),
      'pred20_cl' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat20[",i,"]`, probs = 0.025))"))),
      'pred20_cu' = eval(parse(text = paste0("as.numeric(quantile(rj.mcmc$`Nhat20[",i,"]`, probs = 0.975))")))) %>% 
      bind_rows(metrics)
  }
  
  saveRDS(output, file = "./data/midsave/pop_estimation_application.rds")
}


