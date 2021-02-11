# Load modules ------------------------------------------------------------
library(tidyverse)
library(rgdal)
library(foreach)
library(doParallel)
library(data.table)

registerDoParallel(detectCores()-1)

# Load functions ----------------------------------------------------------

setwd("your_directory_path")

# Define flags ------------------------------------------------------------

year <- 2013
R <- 500
Q <- 100
level <- 'arr'
wp_margin_type <- 'self' #pre
set.seed(1234)

# Load maps ---------------------------------------------------------------

# 2013 map
map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
map_com$CODE <- as.character(map_com$CODE)
map_com$geoid <- as.character(map_com$CODE)

# 2002 map
map_arr <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
map_arr$geoid <- as.character(map_arr$CODE)

# Census 2002
census02 <- readRDS('./data/midsave/census_2002_imputed_simulation_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n,
         CODE = as.character(geoid),
         regid = substr(CODE, start = 1, stop = 2),
         children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes')),
         hh_size = factor(ifelse(hhmembers_n == 1, 0,
                                 ifelse((hhmembers_n == 2) & (children_n == 0), 1,
                                        ifelse((hhmembers_n == 2) & (children_n != 0), 2,
                                               ifelse((hhmembers_n >2) & (hhmembers_n < 6), 2, 3)))),
                          levels = c(0,1,2,3),
                          labels = c('single', 'couple', 'small', 'large'))) %>% 
  filter(m == 1)

# Census 2013
census13 <- readRDS('./data/midsave/census_2013_imputed_application_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n,
         regid = substr(CODE, start = 1, stop = 2),
         children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes')),
         hh_size = factor(ifelse(hhmembers_n == 1, 0,
                                 ifelse((hhmembers_n == 2) & (children_n == 0), 1,
                                        ifelse((hhmembers_n == 2) & (children_n != 0), 2,
                                               ifelse((hhmembers_n >2) & (hhmembers_n < 6), 2, 3)))),
                          levels = c(0,1,2,3),
                          labels = c('single', 'couple', 'small', 'large'))) %>% 
  filter(m == 1)

# DHS 2013
dhs_up_raw <- readRDS('./data/midsave/dhs_2013.rds') %>% 
  left_join(readRDS('./data/midsave/cl13.rds') %>% 
              select(DHSCLUST, CODE = geoid), by = c('v021' = 'DHSCLUST')) %>% 
  mutate(wt = wt*hhmembers_n,
         regid = substr(CODE, start = 1, stop = 2),
         children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes')),
         hh_size = factor(ifelse(hhmembers_n == 1, 0,
                                 ifelse((hhmembers_n == 2) & (children_n == 0), 1,
                                        ifelse((hhmembers_n == 2) & (children_n != 0), 2,
                                               ifelse((hhmembers_n >2) & (hhmembers_n < 6), 2, 3)))),
                          levels = c(0,1,2,3),
                          labels = c('single', 'couple', 'small', 'large')))

# Population counts
if(wp_margin_type == 'self'){
  # ppp <- readRDS(paste0("./data/midsave/ppp_wp.rds"))  
  ppp <- readRDS(paste0("./data/midsave/ppp_recalc_",level,"_02_13_boot_alt.rds"))
}else{
  ppp <- readRDS(paste0("./data/midsave/ppp_",level,"_boot.rds"))
}


# Prepare output ----------------------------------------------------------

# output_point <- data.table()
# eval_point <- data.table()

# Start loop across indicators --------------------------------------------

start.time <- Sys.time()

output_eval <- foreach(r=1:R, .combine=bind_rows) %dopar% {
  
  output_point <- data.table()
  eval_point <- data.table()
  
  for (ind in c('ind5', 'h_ind')){    
    
    dimensions <- c(ind, 'hh_head')
    
    offset_value <- paste(c('CODE',dimensions, 'freq'), collapse = '_')
    offset_name <- paste(dimensions, collapse = '_')
    
    
    # Bootstrap census --------------------------------------------------------
    
    # Prepare offset
    base02_wide <- data.frame('CODE' = sort(rep(unique(census02$CODE), 4)), 
                              offset_name = census02 %>% 
                                select(all_of(dimensions)) %>% 
                                drop_na() %>%
                                distinct() %>%
                                unite({{offset_name}}, all_of(dimensions)))
    
    input02_wide <- census02 %>% 
      select(CODE, wt, all_of(dimensions)) %>% 
      drop_na() %>%
      unite({{offset_name}}, all_of(dimensions)) %>% 
      group_by_at(vars(CODE, {{offset_name}})) %>%
      count(., wt = wt) %>%
      right_join(base02_wide, by = c('CODE', {{offset_name}})) %>% 
      replace(is.na(.), 0) %>% 
      pivot_wider(
        names_from = {{offset_name}},
        values_from = n
      ) %>% 
      ungroup()  %>% 
      mutate(regid = substr(CODE, start = 1, stop = 2)) %>% 
      arrange(CODE)
    
    # Prepare evaluation
    base13_wide <- data.frame('CODE' = sort(rep(unique(census13$CODE), 4)), 
                              offset_name = census13 %>% 
                                select(all_of(dimensions)) %>% 
                                drop_na() %>%
                                distinct() %>%
                                unite({{offset_name}}, all_of(dimensions)))
    
    input13_wide <- census13 %>% 
      select(CODE, wt, all_of(dimensions)) %>% 
      drop_na() %>%
      unite({{offset_name}}, all_of(dimensions)) %>% 
      group_by_at(vars(CODE, {{offset_name}})) %>%
      count(., wt = wt) %>%
      right_join(base13_wide, by = c('CODE', {{offset_name}})) %>% 
      replace(is.na(.), 0) %>% 
      pivot_wider(
        names_from = {{offset_name}},
        values_from = n
      ) %>% 
      ungroup()  %>% 
      mutate(regid = substr(CODE, start = 1, stop = 2)) %>% 
      arrange(CODE)
    
    # Bootstrap from DHS
    dhs_up <- dhs_up_raw %>% 
      group_by(v024) %>% 
      sample_n(size = n(), replace = TRUE) %>% 
      ungroup()
    
    # Bootstrap from pixels (pre-computed)
    covariates_up <- ppp %>% 
      filter(b == r,
             bb == 1) %>% 
      select(CODE = geoid, sen_ppp_sum = ppp13_sum)
    
    output_point_ind <- data.table()
    
    for (reg in unique(census02$regid)){
      
      input02_wide_reg <- input02_wide %>%
        filter(regid == {{reg}})
      
      offset_02 <- input02_wide_reg %>% 
        select(-CODE, -regid) %>% 
        as.matrix()
      
      eval_13 <- input13_wide %>% 
        filter(regid == {{reg}})  %>% 
        select(-CODE, -regid) %>% 
        as.matrix()
      
      # Old census
      row_margin_02 <- rowSums(offset_02)
      theta_02_r <- offset_02/row_margin_02
      
      # New census
      row_margin_13 <- rowSums(eval_13)
      theta_13_r <- eval_13/row_margin_13
      
      row_margin_02_r <- NULL
      row_margin_13_r <- NULL
      offset_02_r <- NULL
      eval_13_r <- NULL
      
      # Sample from underlying populations
      for (a in 1:length(row_margin_13)){
        
        # Old area margin
        row_margin_02_r <- cbind(row_margin_02_r,(rpois(1,row_margin_02[a])))
        
        # True area margin
        row_margin_13_r <- cbind(row_margin_13_r,(rpois(1,row_margin_13[a])))
        
        # Old population
        offset_02_r <- cbind(offset_02_r,(rmultinom(1,row_margin_02_r[a],theta_02_r[a,])))
        
        # True population
        eval_13_r <- cbind(eval_13_r,(rmultinom(1,row_margin_13_r[a],theta_13_r[a,])))
      }
      
      offset_02_r <- t(offset_02_r)
      eval_13_r <- t(eval_13_r)
      
      offset_02_r[offset_02_r==0] <- 1
      eval_13_r[eval_13_r==0] <- 1
      
      # Row margin
      row_margin_df <- input02_wide_reg %>% 
        select(CODE, regid) %>% 
        bind_cols(pop02 = rowSums(offset_02_r),
                  pop13 = rowSums(eval_13_r)) %>% 
        left_join(covariates_up %>% 
                    select(CODE, dynamic_si = sen_ppp_sum), by = 'CODE') %>% 
        group_by(regid) %>% 
        mutate(static = pop02/sum(pop02)*sum(pop13),
               dynamic_si_bench = dynamic_si/sum(dynamic_si)*sum(pop13),
               mix = (static + dynamic_si_bench)/2) %>% 
        ungroup() %>% 
        arrange(CODE)
      
      # Column margin
      dhs_wide <- dhs_up %>% 
        select(wt, regid, all_of(dimensions)) %>%
        filter(regid == {{reg}}) %>% 
        drop_na %>% 
        group_by_at(vars(regid, all_of(dimensions))) %>% 
        summarise(dhs = sum(wt), .groups = 'drop_last') %>% 
        ungroup()
      
      dhs_wide_reg <- dhs_wide %>% 
        unite({{offset_name}}, all_of(dimensions)) %>% 
        pivot_wider(
          names_from = {{offset_name}},
          values_from = dhs,
          values_fn = sum
        ) %>% 
        select(-regid)
      
      dhs_wide_reg[setdiff(names(input02_wide %>% select(-CODE, -regid)), names(dhs_wide_reg))] <- 0     
      
      dhs_wide_reg <- dhs_wide_reg[names(input02_wide %>% select(-CODE, -regid))]
      
      for (r_m in c('dynamic_si_bench', 'static', 'mix')){
        
        row_margin <- row_margin_df %>% 
          select(r_freq = {{r_m}}) %>% 
          as.matrix() %>% 
          as.numeric()
        
        col_margin <- NULL
        for (i in 1:ncol(dhs_wide_reg)) {
          col_margin <- cbind(col_margin,as.vector(c(round(sum(row_margin)*prop.table(colSums(dhs_wide_reg))[i]))))
        }
        
        col_margin <- col_margin %>% 
          as.numeric()
        
        # Get point estimate
        ipf_r <- loglin(outer(row_margin,col_margin)/sum(row_margin),margin=list(1,2),
                        start=offset_02_r, fit=T, eps=1.e-05, iter=100, print = F)$fit
        
        input02_long_reg <- input02_wide_reg %>% 
          pivot_longer(cols = contains('male'), 
                       names_to = {dimensions}, 
                       names_pattern = '(.*)_(.*)', 
                       values_to = 'offset') %>% 
          select(-offset)
        
        ipf_r_long <- input02_long_reg %>% 
          mutate(offset = offset_02_r %>% t() %>% as.vector(),
                 ipf = ipf_r %>% t() %>% as.vector(),
                 true = eval_13_r %>% t() %>% as.vector(),
                 wt_type = r_m,
                 run = r) %>% 
          group_by(CODE, hh_head) %>% 
          mutate(p_ipf = ipf/sum(ipf),
                 p_true = true/sum(true)) %>%
          ungroup()
        
        
        # Get MSE -----------------------------------------------------------------
        
        # Old census
        row_margin_02_p <- rowSums(offset_02_r)
        theta_02_p <- offset_02_r/row_margin_02_p
        
        # Updated census
        row_margin_13_p <- rowSums(ipf_r)
        theta_13_p <- ipf_r/row_margin_13_p
        
        sum_diff <- matrix(0,length(row_margin)*length(col_margin),Q)
        p_diff <- matrix(0,length(row_margin)*length(col_margin),Q)
        sum_stats <- matrix(0,length(row_margin)*length(col_margin),Q)
        p_stats <- matrix(0,length(row_margin)*length(col_margin),Q)
        
        for (q in 1:Q){
          
          row_margin_02_b <- NULL
          row_margin_13_b <- NULL
          offset_02_b <- NULL
          eval_13_b <- NULL
          
          # Sample parametrically from underlying populations
          for (a in 1:length(row_margin_02_p)){	
            
            # Old area margin
            row_margin_02_b <- cbind(row_margin_02_b,(rpois(1,row_margin_02_p[a])))
            
            # True area margin
            row_margin_13_b <- cbind(row_margin_13_b,(rpois(1,row_margin_13_p[a])))
            
            # Old population
            offset_02_b <- cbind(offset_02_b,(rmultinom(1,row_margin_02_b[a],theta_02_p[a,])))
            
            # True population
            eval_13_b <- cbind(eval_13_b,(rmultinom(1,row_margin_13_b[a],theta_13_p[a,])))
            
          }
          
          # row_margin_13_b <- t(row_margin_13_b)
          offset_02_b <- t(offset_02_b)
          eval_13_b <- t(eval_13_b)
          
          offset_02_b[offset_02_b==0] <- 1
          eval_13_b[eval_13_b==0] <- 1
          
          # Sample nonparametrically the row margin
          covariates_up_b <- ppp %>%
            filter(b == r &
                     bb == q) %>%
            select(CODE = geoid, sen_ppp_sum = ppp13_sum) %>% 
            arrange(CODE)
          
          row_margin_b <- row_margin_df %>% 
            select(-dynamic_si, -pop02, -pop13) %>% 
            left_join(covariates_up_b %>% 
                        select(CODE, dynamic_si = sen_ppp_sum), by = 'CODE') %>% 
            arrange(CODE) %>% 
            bind_cols(pop02 = rowSums(offset_02_b),
                      pop13 = rowSums(eval_13_b)) %>% 
            group_by(regid) %>% 
            mutate(static = pop02/sum(pop02)*sum(pop13),
                   dynamic_si_bench = dynamic_si/sum(dynamic_si)*sum(pop13),
                   mix = (static + dynamic_si_bench)/2) %>% 
            ungroup() %>% 
            select(r_freq = {{r_m}}) %>% 
            as.matrix() %>% 
            as.numeric()
          
          # Sample nonparametrically the column margin
          dhs_up_b <- dhs_up %>% 
            group_by(v024) %>% 
            sample_n(size = n(), replace = TRUE) %>% 
            ungroup()
          
          dhs_wide_reg_b <- dhs_up_b %>% 
            select(wt, regid, all_of(dimensions)) %>%
            filter(regid == {{reg}}) %>% 
            drop_na %>% 
            group_by_at(vars(regid, all_of(dimensions))) %>% 
            summarise(dhs = sum(wt), .groups = 'drop_last') %>% 
            ungroup() %>% 
            unite({{offset_name}}, all_of(dimensions)) %>% 
            pivot_wider(
              names_from = {{offset_name}},
              values_from = dhs,
              values_fn = sum
            ) %>% 
            select(-regid)
          
          dhs_wide_reg_b[setdiff(names(input02_wide %>% select(-CODE, -regid)), names(dhs_wide_reg_b))] <- 0     
          
          dhs_wide_reg_b <- dhs_wide_reg_b[names(input02_wide %>% select(-CODE, -regid))]
          
          col_margin_b <- NULL
          for (i in 1:ncol(dhs_wide_reg_b)) {
            col_margin_b <- cbind(col_margin_b,as.vector(c(round(sum(row_margin_b)*prop.table(colSums(dhs_wide_reg_b))[i]))))
          }
          
          col_margin_b <- col_margin_b %>% 
            as.numeric()
          
          ipf_b <- loglin(outer(row_margin_b,col_margin_b)/sum(row_margin_b),margin=list(1,2),
                          start=eval_13_b, fit=T, eps=1.e-05, iter=100, print = F)$fit
          
          diff_b <- ipf_b - eval_13_b
          
          sum_diff[,q] <- diff_b %>% t() %>% as.vector()
          
          p_diff[,q] <- input02_long_reg %>%
            mutate(ipf = ipf_b %>% t() %>% as.vector(),
                   offset = eval_13_b %>% t() %>% as.vector()) %>%
            group_by(CODE, hh_head) %>% 
            mutate(ipf = ipf/sum(ipf),
                   offset = offset/sum(offset),
                   diff = ipf - offset) %>%
            ungroup() %>% 
            select(diff) %>% 
            as.matrix() %>% 
            as.numeric()
          
          sum_stats[,q] <- ipf_b %>% t() %>% as.vector()
          
          p_stats[,q] <- input02_long_reg %>%
            mutate(ipf = ipf_b %>% t() %>% as.vector()) %>%
            group_by(CODE, hh_head) %>% 
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
        
        ipf_r_long <- ipf_r_long %>% 
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
          bind_rows(ipf_r_long %>% 
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
        
        output_point_ind <- rbindlist(list(output_point_ind, ipf_r_long))
        
        tmp <- ipf_r_long %>% 
          filter(ipf_type == 'sum') %>% 
          group_by_at(vars(regid, all_of(dimensions), wt_type, run)) %>% 
          summarise(across(is.numeric, sum), .groups = 'drop_last') %>% 
          ungroup() %>% 
          left_join(dhs_wide, by = c('regid', dimensions)) %>% 
          group_by_at(vars(regid, all_of(dimensions)[-1], wt_type, run)) %>% 
          summarise(year = year,
                    status = .data[[ind]],
                    ind = ind,
                    ipf = ipf/sum(ipf),
                    offset = offset/sum(offset),
                    true = true/sum(true),
                    dhs = dhs/sum(dhs), .groups = 'drop_last') %>% 
          ungroup() %>% 
          filter(!str_detect(status, 'no')) %>% 
          data.table()
        
        eval_point <- rbindlist(list(eval_point, tmp))
        
      }
    }
    
    output_point_ind <- output_point_ind %>% 
      rename(status = .data[[ind]]) %>%
      mutate(ind = ind)
    
    output_point <- rbindlist(list(output_point, output_point_ind))
    
  }
  
  list(output_point, eval_point)
  
}

stopImplicitCluster()

output_point <- output_eval %>% 
  filter(is.na(year)) %>% 
  select(-year,-dhs)

eval_point <- output_eval %>% 
  filter(!is.na(year)) %>% 
  select(-c(CODE, p_ipf:ipf_type))

print(paste0(round(Sys.time() - start.time, digits = 2),' minutes to execute.'))

output_point <- data.frame(output_point)
eval_point <- data.frame(eval_point)

saveRDS(eval_point, file = paste0("./data/midsave/simulation_evaluation_semiparametric_",level,"_",R,"_",wp_margin_type,"_alt.rds"))
saveRDS(output_point, file = paste0("./data/midsave/simulation_output_semiparametric_",level,"_",R,"_",wp_margin_type,"_alt.rds"))
