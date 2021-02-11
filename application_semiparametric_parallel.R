
# Load modules ------------------------------------------------------------
library(tidyverse)
library(rgdal)
library(foreach)
library(doParallel)
library(data.table)

registerDoParallel(5)

# Load functions ----------------------------------------------------------

setwd("your_directory_path")

# Define flags ------------------------------------------------------------

Q <- 500
level <- 'com'

# Load maps ---------------------------------------------------------------

# Commune-level map
map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)


# Load data ---------------------------------------------------------------

# Census 2013
census13 <- readRDS('./data/midsave/census_2013_imputed_application_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n) %>% 
  filter(m == 1)

# DHS 2013
dhs_2013 <- readRDS('./data/midsave/dhs_2013.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# DHS 2014
dhs_2014 <- readRDS('./data/midsave/dhs_2014.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# DHS 2015
dhs_2015 <- readRDS('./data/midsave/dhs_2015.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# DHS 2016
dhs_2016 <- readRDS('./data/midsave/dhs_2016.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# DHS 2017
dhs_2017 <- readRDS('./data/midsave/dhs_2017.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# DHS 2018
dhs_2018 <- readRDS('./data/midsave/dhs_2018.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# DHS 2019
dhs_2019 <- readRDS('./data/midsave/dhs_2019.rds') %>% 
  mutate(wt = wt*hhmembers_n)

# Population counts
ppp <- readRDS(paste0("./data/midsave/ppp_recalc_",level,"_application.rds")) %>% 
  rename(ppp_2013 = pred_13,
         ppp_2014 = pred_14,
         ppp_2015 = pred_15,
         ppp_2016 = pred_16,
         ppp_2017 = pred_17,
         ppp_2018 = pred_18,
         ppp_2019 = pred_19,
         ppp_2020 = pred_20) %>% 
  arrange(geoid)

# Population projections
pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
  transmute(COD_ENTITE = as.character(COD_ENTITE),
            reg_name = tolower(as.character(REG)),
            proj_2013 = as.integer(as.character(ENSEMBLE20)),
            proj_2014 = as.integer(as.character(ENSEMBLE_1)),
            proj_2015 = as.integer(as.character(ENSEMBLE_2)),
            proj_2016 = as.integer(as.character(ENSEMBLE_3)),
            proj_2017 = as.integer(as.character(ENSEMBLE_4)),
            proj_2018 = as.integer(as.character(ENSEMBLE_5)),
            proj_2019 = as.integer(as.character(ENSEMBLE_6)),
            proj_2020 = as.integer(as.character(ENSEMBLE_7))) %>% 
  arrange(COD_ENTITE)

# Select high-growth regions for dynamic approach
dyn_reg <- pop_proj %>% 
  group_by(reg_name) %>% 
  summarise(across(is.numeric, sum)) %>% 
  ungroup %>% 
  mutate(g_2020 = abs(proj_2020 - proj_2013)/proj_2013) %>% 
  arrange(-g_2020) %>% 
  slice_head(n = dim(.)[1]*0.25) %>% 
  select(reg_name) %>% 
  as.matrix() %>% 
  as.character()

# Start loop across indicators --------------------------------------------

start.time <- Sys.time()

output_eval <- foreach(ind=c('ind1', 'ind3', 'ind4', 'ind5', 'ind6', 'ind7', 'ind8', 'ind9', 'ind10', 'h_ind'),
                       .combine=bind_rows) %dopar% 
  {
    
    # for (ind in c('ind1', 'ind3', 'ind4', 'ind5', 'ind6', 'ind7', 'ind8', 'ind9', 'ind10', 'h_ind')){
    
    output_point <- data.table()
    eval_point <- data.table()
    
    dimensions <- c(ind, 'hh_head')
    
    offset_value <- paste(c('COD_ENTITE',dimensions, 'freq'), collapse = '_')
    offset_name <- paste(dimensions, collapse = '_')
    
    for (year in 2013:2020){
      
      if (exists(paste0("dhs_",year)) == TRUE){
        eval(parse(text = paste0("dhs_up_raw <- dhs_",year)))
      }
      
      dhs_up <- dhs_up_raw %>% 
        mutate(reg_name = as.character(v024),
               reg_name = replace(reg_name, reg_name == 'thiès', 'thies'),
               reg_name = replace(reg_name, reg_name == 'thi?s', 'thies'),
               reg_name = replace(reg_name, reg_name == 'thiés', 'thies'))
      
      covariates_up <- ppp %>% 
        filter(bb == 0) %>% 
        select(COD_ENTITE = geoid, sen_ppp_sum = contains({as.character(year)}))
      
      row_margin_df <- pop_proj %>% 
        select(COD_ENTITE, reg_name, contains('2013')) %>%
        left_join(pop_proj %>% 
                    select(COD_ENTITE, reg_name, pop_proj = contains(as.character(year))),
                  by = c('COD_ENTITE', 'reg_name')) %>% 
        left_join(covariates_up %>% 
                    select(COD_ENTITE, dynamic_si = sen_ppp_sum), by = 'COD_ENTITE') %>% 
        group_by(reg_name) %>% 
        mutate(static = proj_2013/sum(proj_2013)*sum(pop_proj),
               dynamic_si_bench = dynamic_si/sum(dynamic_si)*sum(pop_proj)) %>% 
        ungroup() %>% 
        mutate(mix = ifelse(reg_name %in% dyn_reg, dynamic_si_bench, static)) %>% 
        arrange(COD_ENTITE)
      
      # Prepare offset
      base13_wide <- data.frame('COD_ENTITE' = sort(rep(unique(census13$COD_ENTITE), 4)), 
                                offset_name = census13 %>% 
                                  select(all_of(dimensions)) %>% 
                                  drop_na() %>%
                                  distinct() %>%
                                  unite({{offset_name}}, all_of(dimensions)))  
      
      input13_wide <- census13 %>% 
        select(COD_ENTITE, wt, all_of(dimensions)) %>% 
        drop_na() %>%
        unite({{offset_name}}, all_of(dimensions)) %>% 
        group_by_at(vars(COD_ENTITE, {{offset_name}})) %>%
        count(., wt = wt) %>%
        right_join(base13_wide, by = c('COD_ENTITE', {{offset_name}})) %>% 
        replace(is.na(.), 0) %>% 
        pivot_wider(
          names_from = {{offset_name}},
          values_from = n
        ) %>% 
        ungroup() %>% 
        left_join(pop_proj %>% 
                    select(COD_ENTITE, reg_name) %>% 
                    distinct(), by = 'COD_ENTITE') %>% 
        arrange(COD_ENTITE)
      
      output_point_ind <- data.table()
      
      for (r_m in c('mix')){
        
        for (reg in unique(pop_proj$reg_name)){
          
          # Create offset
          input13_wide_reg <- input13_wide %>% 
            filter(reg_name == {{reg}})
          
          input13_wide_reg[input13_wide_reg==0] <- 1
          
          offset_13 <- input13_wide_reg %>% 
            select(-COD_ENTITE, -reg_name) %>% 
            as.matrix()
          
          # Create row margin
          row_margin <- row_margin_df %>% 
            filter(reg_name == {{reg}}) %>% 
            select(r_freq = {{r_m}}) %>% 
            as.matrix() %>% 
            as.numeric()
          
          # Create column margin
          dhs_wide <- dhs_up %>% 
            select(wt, reg_name, all_of(dimensions)) %>%
            filter(reg_name == {{reg}}) %>% 
            drop_na %>% 
            group_by_at(vars(reg_name, all_of(dimensions))) %>% 
            summarise(dhs = sum(wt), .groups = 'drop_last') %>% 
            ungroup()
          
          dhs_wide_reg <- dhs_wide %>% 
            unite({{offset_name}}, all_of(dimensions)) %>% 
            pivot_wider(
              names_from = {{offset_name}},
              values_from = dhs,
              values_fn = sum
            ) %>% 
            select(-reg_name)
          
          dhs_wide_reg[setdiff(names(input13_wide %>% select(-COD_ENTITE, -reg_name)), names(dhs_wide_reg))] <- 0     
          
          dhs_wide_reg <- dhs_wide_reg[names(input13_wide %>% select(-COD_ENTITE, -reg_name))]
          
          col_margin <- NULL
          for (i in 1:ncol(offset_13)) {
            col_margin <- cbind(col_margin,as.vector(c(round(sum(row_margin)*prop.table(colSums(dhs_wide_reg))[i]))))
          }
          
          col_margin <- col_margin %>% 
            as.numeric()
          
          # Monte-Carlo simulation -------------------------------------------
          
          set.seed(1234)
          
          # Get point estimate
          ipf <- loglin(outer(row_margin,col_margin)/sum(row_margin),margin=list(1,2),
                        start=offset_13, fit=T, eps=1.e-05, iter=100, print = F)$fit
          
          input13_long_reg <- input13_wide_reg %>% 
            pivot_longer(cols = contains('male'), 
                         names_to = {dimensions}, 
                         names_pattern = '(.*)_(.*)', 
                         values_to = 'offset') %>% 
            select(-offset)
          
          ipf_long <- input13_long_reg %>% 
            mutate(offset = offset_13 %>% t() %>% as.vector(),
                   ipf = ipf %>% t() %>% as.vector(),
                   wt_type = r_m,
                   year = year) %>% 
            group_by(COD_ENTITE, hh_head) %>% 
            mutate(p_ipf = ipf/sum(ipf)) %>%
            ungroup()
          
          # Monte-Carlo simulation -------------------------------------------
          
          # Old table
          row_margin_13 <- rowSums(offset_13)
          theta_13 <- ipf/row_margin_13
          
          # Upated table
          row_margin_ipf <- rowSums(ipf)
          theta_ipf <- ipf/row_margin_ipf
          
          sum_diff <- matrix(0,length(row_margin)*length(col_margin),Q)
          p_diff <- matrix(0,length(row_margin)*length(col_margin),Q)
          sum_stats <- matrix(0,length(row_margin)*length(col_margin),Q)
          p_stats <- matrix(0,length(row_margin)*length(col_margin),Q)
          
          for (q in 1:Q){
            
            # Sample parametrically from underlying populations
            
            row_margin_13_b <- NULL
            offset_13_b <- NULL
            
            row_margin_ipf_b <- NULL
            offset_ipf_b <- NULL
            
            for (a in 1:length(row_margin)){	
              
              # Sample old table margin
              row_margin_13_b <- cbind(row_margin_13_b,(rpois(1,row_margin_13[a])))
              
              # Sample old population
              offset_13_b <- cbind(offset_13_b,(rmultinom(1,row_margin_13_b[a],theta_13[a,])))
              
              # Sample new table margin
              row_margin_ipf_b <- cbind(row_margin_ipf_b,(rpois(1,row_margin_ipf[a])))
              
              # Sample from new population
              offset_ipf_b <- cbind(offset_ipf_b,(rmultinom(1,row_margin_ipf_b[a],theta_ipf[a,])))
            }
            
            offset_13_b <- t(offset_13_b)
            offset_ipf_b <- t(offset_ipf_b)
            
            offset_ipf_b[offset_ipf_b==0] <- 1
            
            # Sample nonparametrically the row margin
            covariates_up_b <- ppp %>%
              filter(bb == q) %>%
              select(COD_ENTITE = geoid, sen_ppp_sum = contains({as.character(year)}))
            
            row_margin_b <- row_margin_df %>% 
              filter(reg_name == {{reg}}) %>% 
              select(-dynamic_si, -static, -proj_2013, -pop_proj) %>% 
              left_join(covariates_up_b %>% 
                          select(COD_ENTITE, dynamic_si = sen_ppp_sum), by = 'COD_ENTITE') %>% 
              bind_cols(proj_13 = rowSums(offset_13_b),
                        pop_proj = rowSums(offset_ipf_b)) %>% 
              group_by(reg_name) %>% 
              mutate(static = proj_13/sum(proj_13)*sum(pop_proj),
                     dynamic_si_bench = dynamic_si/sum(dynamic_si)*sum(pop_proj)) %>% 
              ungroup() %>% 
              mutate(mix = ifelse(reg_name %in% dyn_reg, dynamic_si_bench, static)) %>% 
              arrange(COD_ENTITE) %>% 
              select(r_freq = {{r_m}}) %>% 
              as.matrix() %>% 
              as.numeric()
            
            # Sample nonparametrically the column margin
            dhs_up_b <- dhs_up %>% 
              group_by(v024) %>% 
              sample_n(size = n(), replace = TRUE) %>% 
              ungroup()
            
            dhs_wide_reg_b <- dhs_up_b %>% 
              select(wt, reg_name, all_of(dimensions)) %>%
              filter(reg_name == {{reg}}) %>% 
              drop_na %>% 
              group_by_at(vars(reg_name, all_of(dimensions))) %>% 
              summarise(dhs = sum(wt), .groups = 'drop_last') %>% 
              ungroup() %>% 
              unite({{offset_name}}, all_of(dimensions)) %>% 
              pivot_wider(
                names_from = {{offset_name}},
                values_from = dhs,
                values_fn = sum
              ) %>% 
              select(-reg_name)
            
            dhs_wide_reg_b[setdiff(names(input13_wide %>% select(-COD_ENTITE, -reg_name)), names(dhs_wide_reg_b))] <- 0     
            
            dhs_wide_reg_b <- dhs_wide_reg_b[names(input13_wide %>% select(-COD_ENTITE, -reg_name))]
            
            col_margin_b <- NULL
            for (i in 1:ncol(dhs_wide_reg_b)) {
              col_margin_b <- cbind(col_margin_b,as.vector(c(round(sum(row_margin_b)*prop.table(colSums(dhs_wide_reg_b))[i]))))
            }
            
            col_margin_b <- col_margin_b %>% 
              as.numeric()
            
            ipf_b <- loglin(outer(row_margin_b,col_margin_b)/sum(row_margin_b),margin=list(1,2),
                            start=offset_ipf_b, fit=T, eps=1.e-05, iter=100, print = F)$fit
            
            diff_b <- ipf_b - offset_ipf_b
            
            sum_diff[,q] <- diff_b %>% t() %>% as.vector()
            
            p_diff[,q] <- input13_long_reg %>%
              mutate(ipf = ipf_b %>% t() %>% as.vector(),
                     offset = offset_ipf_b %>% t() %>% as.vector()) %>%
              group_by(COD_ENTITE, hh_head) %>% 
              mutate(ipf = ipf/sum(ipf),
                     offset = offset/sum(offset),
                     diff = ipf - offset) %>%
              ungroup() %>% 
              select(diff) %>% 
              as.matrix() %>% 
              as.numeric()
            
            sum_stats[,q] <- ipf_b %>% t() %>% as.vector()
            
            p_stats[,q] <- input13_long_reg %>%
              mutate(ipf = ipf_b %>% t() %>% as.vector()) %>%
              group_by(COD_ENTITE, hh_head) %>%
              mutate(ipf = ipf/sum(ipf)) %>%
              ungroup() %>%
              select(ipf) %>%
              as.matrix() %>%
              as.numeric()
          }
          
          # Total statistics
          sum_quant <- sum_stats %>% apply(1,quantile,probs=c(0.025, 0.975)) %>% t()
          sum_stat <- sum_stats %>% apply(1,function(x) summary(x)) %>% t()
          
          sum_bias <- sum_diff %>% 
            t() %>% 
            as.data.frame() %>% 
            summarise(across(everything(), mean)) %>% 
            t() %>% 
            as.vector()
          
          sum_mse <- sum_diff %>% 
            t() %>% 
            as.data.frame() %>% 
            summarise(across(everything(), ~mean(.^2))) %>% 
            t() %>% 
            as.vector()
          
          sum_cv <- sum_stats %>% 
            t() %>% 
            as.data.frame() %>% 
            summarise(across(everything(), ~(sd(.)/mean(.)))) %>% 
            t() %>% 
            as.vector()
          
          # Ratio statistics
          p_quant <- p_stats %>% apply(1,quantile,probs=c(0.025, 0.975)) %>% t()
          p_stat <- p_stats %>% apply(1,function(x) summary(x)) %>% t()
          
          p_bias <- p_diff %>% 
            t() %>% 
            as.data.frame() %>% 
            summarise(across(everything(), mean)) %>% 
            t() %>% 
            as.vector()
          
          p_mse <- p_diff %>% 
            t() %>% 
            as.data.frame() %>% 
            summarise(across(everything(), ~mean(.^2))) %>% 
            t() %>% 
            as.vector()
          
          p_cv <- p_stats %>% 
            t() %>% 
            as.data.frame() %>% 
            summarise(across(everything(), ~(sd(.)/mean(.)))) %>% 
            t() %>% 
            as.vector()
          
          ipf_long <- ipf_long %>% 
            mutate(
              bias = sum_bias,
              mse = sum_mse,
              cv = sum_cv,
              cilo = sum_quant[,1],
              ciup = sum_quant[,2],
              min = sum_stat[,1],
              q1 = sum_stat[,2],
              median = sum_stat[,3],
              mean = sum_stat[,4],
              q3 = sum_stat[,5],
              max = sum_stat[,6],
              ipf_type = 'sum'
            ) %>% 
            bind_rows(ipf_long %>% 
                        mutate(
                          bias = p_bias,
                          mse = p_mse,
                          cv = p_cv,
                          cilo = p_quant[,1],
                          ciup = p_quant[,2],
                          min = p_stat[,1],
                          q1 = p_stat[,2],
                          median = p_stat[,3],
                          mean = p_stat[,4],
                          q3 = p_stat[,5],
                          max = p_stat[,6],
                          ipf_type = 'prop'
                        )) %>% 
            as.data.table()
          
          output_point_ind <- rbindlist(list(output_point_ind, ipf_long))
          
          tmp <- ipf_long %>% 
            filter(ipf_type == 'sum') %>% 
            group_by_at(vars(reg_name, all_of(dimensions), wt_type, year)) %>% 
            summarise(across(is.numeric, sum), .groups = 'drop_last') %>% 
            ungroup() %>% 
            left_join(dhs_wide, by = c('reg_name', dimensions)) %>% 
            group_by_at(vars(reg_name, all_of(dimensions)[-1], wt_type)) %>% 
            summarise(year = year,
                      status = .data[[ind]],
                      ind = ind,
                      ipf = ipf/sum(ipf),
                      offset = offset/sum(offset),
                      dhs = dhs/sum(dhs), .groups = 'drop_last') %>% 
            ungroup() %>% 
            filter(!str_detect(status, 'no')) %>% 
            data.table()
          
          eval_point <- rbindlist(list(eval_point, tmp))
          
        }
      }
      
      output_point <- rbindlist(list(output_point, output_point_ind %>% 
                                       rename(status = .data[[ind]]) %>% 
                                       mutate(ind = ind)
      ))
      
    }
    
    list(output_point, eval_point)
    
  }

stopImplicitCluster()

output_point <- output_eval %>% 
  filter(is.na(dhs)) %>% 
  select(-dhs)

eval_point <- output_eval %>% 
  filter(!is.na(dhs)) %>% 
  select(-c(COD_ENTITE, p_ipf:ipf_type))

print(paste0(round(Sys.time() - start.time, digits = 2),' minutes to execute.'))

output_point <- data.frame(output_point)
eval_point <- data.frame(eval_point)

saveRDS(eval_point, file = paste0("./data/midsave/application_evaluation_semiparametric_hh_head_",Q,".rds"))
saveRDS(output_point, file = paste0("./data/midsave/application_output_semiparametric_hh_head_",Q,".rds"))
