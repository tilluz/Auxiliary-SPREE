library(haven)
library(tidyverse)
library(survey)

# Functions and directories -----------------------------------------------
datawd <- "your_directory_path"

read.dhs <- function(x,y){
  file <- read_dta(paste0(x,'.DTA'))
  
  names(file) <- sub(y,"",names(file))
  file <- file[, !duplicated(colnames(file))]
  var.labels  <- file %>% 
    map_chr(~attributes(.)$label) %>% 
    data.frame()
  
  list <- list(file = file, crosswalk = var.labels)
  return(list)
}


# Load surveys ------------------------------------------------------------
# https://dhsprogram.com/what-we-do/survey/survey-display-457.cfm
# For variable coding: https://dhsprogram.com/pubs/pdf/DHSG4/Recode6_Map_22March2013_DHSG4.pdf

# Births
setwd(paste0(datawd,'/data/dhs_2013/snbr6ddt/'))
temp              <- read.dhs('SNBR6DFL','h*')
births            <- temp$file
births_var_labels <- temp$crosswalk
rm(temp)

# Households
setwd(paste0(datawd,'/data/dhs_2013/snhr6ddt/'))
temp              <- read.dhs('SNHR6DFL','h*')
hhs               <- temp$file
hhs_var_labels    <- temp$crosswalk
rm(temp)

# Household members
setwd(paste0(datawd,'/data/dhs_2013/snpr6ddt/'))
temp                 <- read.dhs('SNPR6DFL','h*')
hhmembers            <- temp$file
hhmembers_var_labels <- temp$crosswalk
rm(temp)

# Individuals (women and children)
setwd(paste0(datawd,'/data/dhs_2013/snir6ddt/'))
temp              <- read.dhs('SNIR6DFL','')
female            <- temp$file
female_var_labels <- temp$crosswalk
rm(temp)


# Create Household Identifiers & weights ----------------------------------

births <- births %>% 
  mutate(hhid = paste(v001, v002, sep='.')) %>% 
  mutate(wt = v005/1000000)

hhs <- hhs %>% 
  mutate(hhid = paste(v001, v002, sep='.')) %>% 
  mutate(wt = v005/1000000)

hhmembers <- hhmembers %>% 
  mutate(hhid = paste(v001, v002, sep='.')) %>% 
  mutate(wt = v005/1000000)

female <- female %>% 
  mutate(hhid = paste(v001, v002, sep='.')) %>% 
  mutate(wt = v005/1000000)


# MPI ---------------------------------------------------------------------

# Health
### Indicator 1: Child mortality - Deprived if any child has died in the family
ind1 <- births %>% 
  na_if(., 97) %>% # replace inconsistent values with NA
  na_if(., 98) %>% # replace "don't know" with NA
  na_if(., 99) %>% # replace missing values with NA
  select(hhid, v206, v207) %>% 
  drop_na %>% 
  mutate(n.child=ifelse(v206>0 | v207>0, 1, 0)) %>% # At least one child has died
  group_by(hhid) %>% 
  summarise(n.child.hh=sum(n.child, na.rm = T)) %>% 
  mutate(ind1=factor(x = ifelse(n.child.hh>0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind1)

### Indicator 2: Nutrition - Deprived if any adult or child, for whom there is nutritional information, is underweight
### Defined as BMI below 18.5 for individuals aged 20 and above (http://hdr.undp.org/en/mpi-2019-faq)
ind2 <- hhmembers %>% 
  na_if(., 97) %>%
  na_if(., 98) %>%
  na_if(., 99) %>%
  select(hhid, v105, a40) %>% 
  drop_na %>% 
  mutate(n.malnourished = ifelse(v105 > 19 & a40 < 1850, 1, 0)) %>% 
  group_by(hhid) %>% 
  summarise(n.malnourished.hh = sum(n.malnourished, na.rm = T)) %>% 
  mutate(ind2 = factor(x = ifelse(n.malnourished.hh > 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind2)

# Education
### Indicator 3: Years of schooling - Deprived if no household member has completed six years of schooling
# remaining indicators which can be extracted from the DHS data
ind3 <- hhs %>% 
  na_if(., 97) %>%
  na_if(., 98) %>%
  na_if(., 99) %>%
  select(hhid, contains("v108_")) %>% 
  filter(across(contains("v108_"),any_vars(!is.na(.)))) %>% 
  replace(is.na(.),0) %>% 
  mutate(ind3.sum = rowSums(across(v108_06:ncol(.)), na.rm = T),
         ind3 = factor(x = ifelse(ind3.sum == 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind3) %>% 
  distinct(hhid, .keep_all = TRUE)

### Indicator 4: School attendance - Deprived if any school-aged child is not attending school up to class 8
### For details on the calculation see census 2002 R file
ind4 <- hhmembers %>% 
  na_if(., 97) %>%
  na_if(., 98) %>%
  na_if(., 99) %>%
  select(hhid, v129, v105, v124) %>% 
  drop_na(any_of(c('v105', 'v124'))) %>% 
  filter((v105 >= 7) & (v105 <= 16)) %>% 
  mutate(
    ind4 = case_when(
      (v129 == 0 & v105 >= 7)
      | (v105 == 9 & v124  <= 1)
      | (v105 == 10 & v124 <= 2)
      | (v105 == 11 & v124 <= 3)
      | (v105 == 12 & v124 <= 4)
      | (v105 == 13 & v124 <= 5)
      | (v105 == 14 & v124 <= 6)
      | (v105 == 15 & v124 <= 7)
      | (v105 == 16 & v124 <= 8)
      ~ 1,
      TRUE ~ 0)
  ) %>% 
  group_by(hhid) %>% 
  summarize(ind4 = factor(x = max(ind4, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))

# Living Standards
### Indicator 5: Cooking fuel - Deprived if the household cooks with dung, wood or charcoal
ind5 <- hhs %>% 
  na_if(., 99) %>%
  select(hhid, v226) %>% 
  drop_na %>% 
  mutate(ind5 = factor(x = ifelse(v226 %in% c(7, 8, 9, 10, 11), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind5) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 6: Sanitation - Deprived if the household's sanitation facility is not improved (according to MDG guidelines), or it is improved but shared with other households
ind6 <- hhs %>% 
  na_if(., 99) %>%
  select(hhid, v205, v225) %>% 
  drop_na %>% 
  mutate(ind6 = factor(x = ifelse((v205 %in% c(23, 24, 26, 31) | v225 == 1), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind6) %>% 
  distinct(hhid, .keep_all = TRUE)

### Indicator 7: Drinking Water - Deprived if the household does not have access to safe drinking water (according to MDG guidelines) or safe drinking water is more than a 30-minute walk from home roundtrip
ind7 <- hhs %>% 
  na_if(., 99) %>%
  na_if(., 999) %>%
  select(hhid, v201, v204) %>% 
  drop_na %>% 
  mutate(ind7 = factor(x = ifelse((v201 %in% c(32, 40, 42, 43, 51, 61, 62, 71)), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% # | v204 %>% replace(996,0) %>% replace_na(0) > 30
  select(hhid, ind7) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 8: Electricity - Deprived if the household has no electricity
ind8 <- hhs %>% 
  na_if(., 9) %>%
  select(hhid, v206) %>% 
  drop_na %>% 
  mutate(ind8 = factor(x = ifelse(v206 == 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind8) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 9: Housing - Deprived if the household has a dirt floor
ind9 <- hhs %>% 
  na_if(., 99) %>%
  select(hhid, v213) %>% 
  drop_na %>% 
  mutate(ind9 = factor(x = ifelse(v213 %in% c(10, 11, 12), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind9) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 10: Assets - Deprived if the household does not own more than one of these assets: radio, TV, telephone, computer, Animal cart, bicycle, motorbike or refrigerator and does not own a car or truck
ind10 <- hhs %>% 
  na_if(., 9) %>%
  select(hhid, v207, v208, v209, v210, v211, v221, v212) %>% 
  drop_na %>% 
  mutate(ind10 = factor(x = ifelse(
    ((v207+v208+v209+v210+v211+v221) == 0 & v212 == 0), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind10) %>% 
  distinct(hhid, .keep_all = TRUE)

# Other indicators --------------------------------------------------------

# Female-headed households (male 0, female 1)
hh_head <- 
  hhs %>% 
  select(hhid, v219) %>% 
  drop_na() %>% 
  transmute(hhid = hhid,
            hh_head = factor(x = (v219 - 1), levels = c(0,1), labels = c('male', 'female'))) %>% 
  distinct()

# Age of head of household
age <- 
  hhs %>% 
  select(hhid, v220) %>% 
  drop_na() %>% 
  transmute(hhid = hhid,
            age_head = cut(v220, c(seq(0, 100, 5), Inf), right = FALSE)) %>% 
  distinct()

# Urban status (rural 0, urban 1)
urban <- 
  hhs %>% 
  select(hhid, v025) %>% 
  drop_na() %>% 
  transmute(hhid = hhid,
            urban_head = factor(x = (v025 - 1), levels = c(0,1), labels = c('urban', 'rural'))) %>%
  distinct()

# Marital status of head of household
marital <- 
  hhmembers %>% 
  select(hhid, v101, v115) %>% 
  drop_na() %>% 
  filter(as_factor(v101) == 'head') %>% 
  transmute(hhid = hhid,
            marital_head = as_factor(v115)) %>%
  distinct()

# Educational attainment of head of household
education <- 
  hhmembers %>% 
  select(hhid, v101, v107) %>% #we could also use v106 here, less non-response
  drop_na() %>% 
  filter(as_factor(v101) == 'head') %>% 
  transmute(hhid = hhid,
            education_head = as_factor(v107)) %>%
  distinct()

# Number of children
children <- 
  hhmembers %>% 
  select(hhid, v105) %>% 
  drop_na() %>% 
  group_by(hhid) %>% 
  summarise(children_n = sum(ifelse(v105 <= 16, 1, 0))) %>% 
  ungroup()


# Create area-level output -------------------------------------------

dhs_2013 <- hhs %>% 
  select(hhid, v021, v023, v024, wt, hhmembers_n = v009) %>% 
  mutate(v024 = as_factor(v024, levels = "labels")) %>% 
  distinct() %>% 
  left_join(ind1, by = 'hhid') %>% 
  left_join(ind2, by = 'hhid') %>% 
  left_join(ind3, by = 'hhid') %>% 
  left_join(ind4, by = 'hhid') %>% 
  left_join(ind5, by = 'hhid') %>% 
  left_join(ind6, by = 'hhid') %>% 
  left_join(ind7, by = 'hhid') %>% 
  left_join(ind8, by = 'hhid') %>% 
  left_join(ind9, by = 'hhid') %>% 
  left_join(ind10, by = 'hhid') %>% 
  left_join(hh_head, by = 'hhid') %>% 
  left_join(age, by = 'hhid') %>% 
  left_join(urban, by = 'hhid') %>% 
  left_join(marital, by = 'hhid') %>% 
  left_join(education, by = 'hhid') %>% 
  left_join(children, by = 'hhid') %>% 
  mutate(health = rowMeans(select(.,ind1, ind2) %>% 
                             transmute_if(is.factor, as.integer), 
                           na.rm = T) - 1,
         edu = rowMeans(select(.,ind3, ind4) %>% 
                          transmute_if(is.factor, as.integer), 
                        na.rm = T) - 1,
         liv = rowMeans(select(.,ind5, ind6, ind7, ind8, ind9, ind10) %>% 
                          transmute_if(is.factor, as.integer), 
                        na.rm = T) - 1) %>% 
  mutate_at(vars(health, edu, liv), list(~na_if(., "NaN"))) %>% 
  mutate(score = rowMeans(select(.,health, edu, liv), na.rm = T),
         h_ind = factor(x = ifelse(score >= 1/3, 1, 0), levels = c(0,1), labels = c('non-poor', 'poor')),
         v_ind = factor(x = ifelse((score < 1/3) & (score >= 1/5), 1, 0), levels = c(0,1), labels = c('no', 'yes')),
         s_ind = factor(x = ifelse(score >= 1/2, 1, 0), levels = c(0,1), labels = c('no', 'yes')))

# dhs_2014 <- hhs %>% 
#   select(hhid, v024, wt) %>% 
#   mutate(v024 = as_factor(v024, levels = "labels")) %>% 
#   distinct() %>% 
#   left_join(ind1, by = 'hhid') %>% 
#   left_join(ind2, by = 'hhid') %>% 
#   left_join(ind3, by = 'hhid') %>% 
#   left_join(ind4, by = 'hhid') %>% 
#   left_join(ind5, by = 'hhid') %>% 
#   left_join(ind6, by = 'hhid') %>% 
#   left_join(ind7, by = 'hhid') %>% 
#   left_join(ind8, by = 'hhid') %>% 
#   left_join(ind9, by = 'hhid') %>% 
#   left_join(ind10, by = 'hhid') %>% 
#   left_join(hh_head, by = 'hhid') %>% 
#   left_join(age, by = 'hhid') %>% 
#   left_join(urban, by = 'hhid') %>% 
#   left_join(marital, by = 'hhid') %>% 
#   left_join(education, by = 'hhid') %>% 
#   mutate(health = rowMeans(select(.,ind1, ind2), na.rm = T),
#          edu = rowMeans(select(.,ind3, ind4), na.rm = T),
#          liv = rowMeans(select(.,ind5, ind6, ind7, ind8, ind9, ind10), na.rm = T)) %>% 
#   mutate_at(vars(health, edu, liv), list(~na_if(., "NaN"))) %>% 
#   mutate(score = rowMeans(select(.,health, edu, liv), na.rm = T),
#          h_ind = ifelse(score >= 1/3, 1, 0)) %>% 
#   group_by(v024) %>% 
#   summarise(
#     ind1 = weighted.mean(ind1, wt, na.rm = T),
#     ind2 = weighted.mean(ind2, wt, na.rm = T),
#     ind3 = weighted.mean(ind3, wt, na.rm = T),
#     ind4 = weighted.mean(ind4, wt, na.rm = T),
#     ind5 = weighted.mean(ind5, wt, na.rm = T),
#     ind6 = weighted.mean(ind6, wt, na.rm = T),
#     ind7 = weighted.mean(ind7, wt, na.rm = T),
#     ind8 = weighted.mean(ind8, wt, na.rm = T),
#     ind9 = weighted.mean(ind9, wt, na.rm = T),
#     ind10 = weighted.mean(ind10, wt, na.rm = T),
#     hh_head = weighted.mean(hh_head, wt, na.rm = T),
#     health = weighted.mean(health, wt, na.rm = T),
#     edu = weighted.mean(edu, wt, na.rm = T),
#     liv = weighted.mean(liv, wt, na.rm = T),
#     h = weighted.mean(h_ind, wt, na.rm = T),
#     n = sum(wt),
#     n_poor = sum(h_ind * wt, na.rm = T),
#     a = weighted.mean(score[h_ind == 1], wt[h_ind == 1], na.rm = T)) %>% 
#   mutate(mpi = h * a) %>% 
#   mutate(mpi = replace_na(mpi, 0))


# Other indicators --------------------------------------------------------


# Append and Save ---------------------------------------------------------

setwd(datawd)
saveRDS(dhs_2013, file = "./data/midsave/dhs_2013.rds")
