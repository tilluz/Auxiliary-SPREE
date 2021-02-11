library(foreign)
library(rgdal)
library(broom)
library(tidyverse)
library(haven)
library(mice)

setwd("your_directory_path")

# Read census 2002 -----------------------------------------------------------

'Source: http://anads.ansd.sn/index.php/catalog/9'

# Population census
pop02_raw <- read.spss("./data/census_2002/Population_2002_10pc_ano.sav",
                       to.data.frame=TRUE) %>% 
  mutate(men_ch = as.character(men_ch)) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  mutate(QB06 = as.integer(QB06),
         QB14A = as.character(QB14A),
         QB15 = as.character(QB15),
         QB15 = replace(QB15, QB15 == 'aucun', '0'),
         QB15 = replace(QB15, QB15 == 'préscolaire', '1'),
         QB15 = as.integer(QB15),
         QB21F = as.integer(QB21F),
         QB21M = as.integer(QB21M),
         QB22F = as.integer(QB22F),
         QB22M = as.integer(QB22M)) %>% 
  mutate(across(c(QB21F, QB21M, QB22F, QB22M), ~ifelse(. >= 20, NA, .)),
         ind1 = factor(x = if_else(QB21F + QB21M - QB22F - QB22M > 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived')),
         ind3 = factor(x = case_when(
           QB14A == 'Oui' | QB15 < 7
           ~1,
           TRUE ~ 0), levels = c(0,1), labels = c('not_deprived', 'deprived')),
         ind3 = replace(ind3, (is.na(QB14A)) & (is.na(QB15)), NA),
         ind4 = factor(x = case_when(
           (QB14A == "Oui")
           | (QB06 == 9 & QB15 <= 2)
           | (QB06 == 10 & QB15 <= 3)
           | (QB06 == 11 & QB15 <= 4)
           | (QB06 == 12 & QB15 <= 5)
           | (QB06 == 13 & QB15 <= 6)
           | (QB06 == 14 & QB15 <= 7)
           | (QB06 == 15 & QB15 <= 8)
           | (QB06 == 16 & QB15 <= 9) ~ 1,
           TRUE ~ 0), levels = c(0,1), labels = c('not_deprived', 'deprived')),
         ind4 = replace(ind4, (is.na(QB15)) & (QB14A != "Oui"), NA),
         ind4 = replace(ind4, (QB06 < 7) | (QB06 > 16), NA))

# Housing census
hab02_raw <- read.spss("./data/census_2002/Habitat_2002_10pc_ano.sav",
                       to.data.frame=TRUE) %>% 
  mutate(men_ch = as.character(men_ch))


# Set flags ---------------------------------------------------------------

impute = TRUE
m = 1

boot = FALSE
b = 100

# Impute missing values ---------------------------------------------------

if (impute == TRUE){
  
  # Population file
  pop02_red <- pop02_raw %>% 
    select_at(vars(-DR_ch, -men_ch, -contains('QA'), -contains('QB07'), -contains('QB09'), -QB10, -QB11A, -QB11B, -QB18, 
                   -arrond, -depart, -comarr, -ethnie, -LP1, -LP2, -educ, -age5, -age10, ))
  
  pop02_mice <- mice(data = pop02_red,
                     pred = quickpred(pop02_red),
                     m = m,
                     maxit = 1,
                     nnet.MaxNWts = 10000)
  
  pop02_complete <- complete(pop02_mice, action = 'long') %>% 
    mutate(m = .imp) %>% 
    select(-.imp,-.id)
  
  # Habitat file
  hab02_red <- hab02_raw %>% 
    select_at(vars(-DR_ch, -men_ch, -contains('QA'), -arrond, -depart, -comarr)) %>% 
    mutate_if(is.labelled, as_factor)
  
  hab02_mice <- mice(data = hab02_red,
                     m = m,
                     maxit = 1)
  
  hab02_complete <- complete(hab02_mice, action = 'long') %>% 
    mutate(m = .imp) %>% 
    select(-.imp,-.id)
  
}else{
  m = 1
}

# Start loop over imputations ---------------------------------------------

census02_simulation <- data.frame()

r = 1

for (i in 1:m){
  
  if (impute == TRUE){
    pop02_imp <- pop02_complete %>% 
      filter(m == i) %>% 
      bind_cols(pop02_raw[,!(names(pop02_raw) %in% c(names(pop02_red), 'm'))])
    
    hab02_imp <- hab02_complete %>% 
      filter(m == i) %>% 
      bind_cols(hab02_raw[,!(names(hab02_raw) %in% c(names(hab02_red), 'm'))])
  }else{
    pop02_imp <- pop02_raw
    hab02_imp <- hab02_raw
  }
  
  
  # Start loop over bootstrap runs ------------------------------------------
  
  if (boot == FALSE){
    pop02 <- pop02_imp
    hab02 <- hab02_imp
    b = 1
  }
  
  for (j in 1:b){
    
    if (boot == TRUE){
      pop02 <- pop02_imp %>% 
        group_by(DR_ch) %>% 
        sample_n(size = n(), replace = TRUE) %>% 
        ungroup()
      
      hab02 <- pop02 %>% 
        select(men_ch) %>% 
        distinct() %>% 
        left_join(hab02_imp, by = 'men_ch')
    }
    
    # Household-level MPI -----------------------------------------------------
    
    'Source: https://en.wikipedia.org/wiki/Multidimensional_Poverty_Index'
    
    # Health
    ### Indicator 1: Child mortality - Deprived if any child has died in the family
    ind1 <- 
      pop02 %>% 
      select(men_ch, QB06, ind1) %>% 
      drop_na() %>% 
      filter(QB06 >= 10) %>% 
      group_by(men_ch) %>% 
      summarize(ind1 = factor(x = max(as.integer(ind1)-1, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    
    ### Indicator 2: Nutrition - Deprived if any adult or child, for whom there is nutritional information, is underweight
    
    
    # Education
    ### Indicator 3: Years of schooling - Deprived if no household member has completed six years of schooling
    ind3 <- 
      pop02 %>% 
      select(men_ch, QB14A, QB15, ind3) %>% 
      filter(!is.na(QB14A) | !is.na(QB15)) %>% 
      group_by(men_ch) %>% 
      summarize(ind3 = factor(x = min(as.integer(ind3)-1, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 4: School attendance - Deprived if any school-aged child is not attending school up to class 8
    ##### Primary school in Senegal starts at age 7 (https://www.who.int/immunization/programmes_systems/policies_strategies/Senegal_country_report.pdf?ua=1),
    ##### consequently we define target group similar to "Alkire, Sabina, et al. "Exploring multidimensional poverty in China: 2010 to 2014."
    ##### Research on Economic Inequality 25 (2017): 161-228."
    ##### Consequently, we say: 15-year olds are deprived if they neither completed class 8 or above nor ever visited school, 
    ##### 14-year olds are deprived if they neither completed class 7 or above nor ever visited school and so on...
    ind4 <- 
      pop02 %>%
      select(men_ch, QB15, QB06, QB14A, ind4) %>% 
      filter((QB06 >= 7) & (QB06 <= 16)) %>% 
      filter(!is.na(QB15) | QB14A == 'Oui') %>% 
      group_by(men_ch) %>% 
      summarize(ind4 = factor(x = max(as.integer(ind4)-1, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    # Living Standards
    ### Indicator 5: Cooking fuel - Deprived if the household cooks with dung, wood or charcoal
    ind5 <- 
      hab02 %>% 
      select(men_ch, E07) %>% 
      drop_na() %>%
      mutate(
        ind5 = case_when(
          (E07 == "Bois")
          | (E07 == "Charbon")
          | (E07 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(men_ch) %>% 
      summarize(ind5 = factor(x = max(ind5, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 6: Sanitation - Deprived if the household's sanitation facility is not improved (according to MDG guidelines), or it is improved but shared with other households
    ind6 <- 
      hab02 %>% 
      select(men_ch, E04) %>% 
      drop_na() %>% 
      mutate(
        ind6 = case_when(
          (E04 == "Dans la nature")
          | (E04 == "Edicule public")
          | (E04 == "Latrines")
          | (E04 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(men_ch) %>% 
      summarize(ind6 = factor(x = max(ind6, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 7: Drinking Water - Deprived if the household does not have access to safe drinking water (according to MDG guidelines) or safe drinking water is more than a 30-minute walk from home roundtrip
    ind7 <- 
      hab02 %>% 
      select(men_ch, E05) %>% 
      drop_na() %>%
      mutate(
        ind7 = case_when(
          (E05 == "Source, cours d'eau")
          | (E05 == "Vendeurs d'eau")
          | (E05 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(men_ch) %>% 
      summarize(ind7 = factor(x = max(ind7, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 8: Electricity - Deprived if the household has no electricity
    ind8 <- 
      hab02 %>% 
      select(men_ch, E06) %>% 
      drop_na() %>% 
      mutate(
        ind8 = case_when(
          (E06 == "Gaz")
          | (E06 == "Lampe a petrole")
          | (E06 == "Lampe tempete")
          | (E06 == "Bougie")
          | (E06 == "Bois")
          | (E06 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(men_ch) %>% 
      summarize(ind8 = factor(x = max(ind8, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 9: Housing - Deprived if the household has a dirt floor
    ind9 <- 
      hab02 %>% 
      select(men_ch, E10) %>% 
      drop_na() %>% 
      mutate(
        ind9 = case_when(
          (E10 == "Sable")
          | (E10 == "Argile/banco")
          | (E10 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(men_ch) %>% 
      summarize(ind9 = factor(x = max(ind9, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 10: Assets - Deprived if the household does not own more than one of these assets: radio, TV, telephone, computer, Animal cart, bicycle, motorbike or refrigerator and does not own a car or truck
    ind10 <- 
      hab02 %>% 
      select(men_ch, E11B, E11C, E11E, E11F, E12B, E12C, E12D, E12E, E12F) %>% 
      drop_na() %>% 
      mutate(across(where(is.factor), as.integer)) %>% 
      mutate(ind10 = ifelse(
        ((E11B+E11C+E11E+E11F+E12C+E12D+E12E+E12F) <=9 & E12B == 1), 1, 0)) %>% 
      select(men_ch, ind10) %>% 
      group_by(men_ch) %>% 
      summarize(ind10 = factor(x = max(ind10, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    # Other indicators --------------------------------------------------------
    
    # Female-headed households (male 0, female 1)
    hh_head <- 
      pop02 %>% 
      select(men_ch, QB03, QB04) %>% 
      drop_na() %>% 
      mutate(hh_head = if_else((QB03 == 'Chef de menage') & (QB04 == 'Feminin'), 1, 0)) %>% 
      group_by(men_ch) %>% 
      summarise(hh_head = factor(x = max(hh_head, na.rm = T), levels = c(0,1), labels = c('male', 'female')))
    
    # Age of head of household
    age <- 
      pop02 %>% 
      select(men_ch, QB03, QB06) %>% 
      drop_na() %>% 
      filter(QB03 == 'Chef de menage') %>% 
      transmute(men_ch = men_ch, 
                age=cut(QB06, c(seq(0, 100, 5), Inf), right = FALSE)) %>% 
      distinct(men_ch, .keep_all = TRUE)
    
    # Urban status (rural 0, urban 1)
    urban <- 
      pop02 %>% 
      select(men_ch, QA03) %>% 
      drop_na() %>% 
      mutate(urban = if_else(QA03 == 'Urbain', 1, 0)) %>%
      group_by(men_ch) %>% 
      summarise(urban_head = factor(x = max(urban, na.rm = T), levels = c(0,1), labels = c('rural', 'urban')))
    
    # Marital status of head of household
    marital <- 
      pop02 %>% 
      select(men_ch, QB03, QB20) %>% 
      drop_na() %>% 
      filter(QB03 == 'Chef de menage') %>%
      mutate(marital = if_else(QB20 == 'Célibataire', 0, 
                               if_else((QB20 == 'Monogame')
                                       |(QB20 == 'Polygame à 2')
                                       |(QB20 == 'Polygame à 3')
                                       |(QB20 == 'Polygame à 4 et+'), 1, 
                                       if_else(QB20 == 'Veuf / Veuve', 2, 
                                               if_else(QB20 == 'Divorcé', 3, 4))))) %>%
      transmute(men_ch = men_ch,
                marital_head = factor(x = marital, 
                                      levels = c(0,1,2,3,4), 
                                      labels = c('never married','married','widowed','divorced','others'))) %>% 
      distinct(men_ch, .keep_all = TRUE)
    
    # Educational attainment of head of household
    education <- 
      pop02 %>% 
      select(men_ch, QB03, QB15) %>% 
      drop_na() %>% 
      filter(QB03 == 'Chef de menage') %>%
      mutate(QB15 = QB15 %>% as.integer,
             education = if_else(QB15 < 2, 0, 
                                 if_else((QB15 >= 2) & (QB15 < 8), 1, 
                                         if_else((QB15 >= 8) & (QB15 < 15), 2, 
                                                 if_else((QB15 >= 15) & (QB15 < 20), 3, 4))))) %>%
      transmute(men_ch = men_ch,
                education_head = factor(x = education, 
                                        levels = c(0,1,2,3,4), 
                                        labels = c('no education, preschool','primary','secondary','higher','others'))) %>% 
      distinct(men_ch, .keep_all = TRUE)
    
    # Create map identfier ----------------------------------------------------
    '
    Geographic shapes of urban communes are not available, but dissolved in 
    surrounding arrondissements. The same procedure is here applied to census data.
    The geo-codes for urban communes have been provided by the statistical office
    of Senegal in the file Codes_RGPH02.xlsx. Corresponding arrondissement geo-
    codes have been identified using the variables CCRCA and geoid
    of the map_com_harmonised shapefile.
    '
    census02_simulation <- pop02 %>%
      mutate_at(.vars = c('QA01', 'QA02', 'QA04'), ~as.integer(.)) %>%
      mutate(QA01 = if_else(QA01 < 10,
                            paste0("0",QA01),
                            as.character(QA01)),
             wt = 10,
             geoid = paste0(QA01, QA02, QA04),
             geoid = replace(geoid, geoid == '0148', '0142'),
             geoid = replace(geoid, geoid == '0218', '0212'),
             geoid = replace(geoid, geoid == '0228', '0222'),
             geoid = replace(geoid, geoid == '0238', '0232'),
             geoid = replace(geoid, geoid == '0318', '0314'),
             geoid = replace(geoid, geoid == '0328', '0321'),
             geoid = replace(geoid, geoid == '0338', '0333'),
             geoid = replace(geoid, geoid == '0418', '0413'),
             geoid = replace(geoid, geoid == '0428', '0421'),
             geoid = replace(geoid, geoid == '0438', '0432'),
             geoid = replace(geoid, geoid == '0518', '0512'),
             geoid = replace(geoid, geoid == '0528', '0523'),
             geoid = replace(geoid, geoid == '0538', '0532'),
             geoid = replace(geoid, geoid == '0618', '0612'),
             geoid = replace(geoid, geoid == '0628', '0622'),
             geoid = replace(geoid, geoid == '0638', '0633'),
             geoid = replace(geoid, geoid == '0718', '0712'),
             geoid = replace(geoid, geoid == '0728', '0722'),
             geoid = replace(geoid, geoid == '0738', '0731'),
             geoid = replace(geoid, geoid == '0818', '0811'),
             geoid = replace(geoid, geoid == '0828', '0822'),
             geoid = replace(geoid, geoid == '0838', '0831'),
             geoid = replace(geoid, geoid == '0918', '0915'),
             geoid = replace(geoid, geoid == '0928', '0921'),
             geoid = replace(geoid, geoid == '0938', '0932'),
             geoid = replace(geoid, geoid == '1018', '1013'),
             geoid = replace(geoid, geoid == '1028', '1021'),
             geoid = replace(geoid, geoid == '1038', '1034'),
             geoid = replace(geoid, geoid == '1118', '1114'),
             geoid = replace(geoid, geoid == '1128', '1122'),
             geoid = replace(geoid, geoid == '1138', '1131')) %>%
      group_by(men_ch, geoid, wt) %>%
      summarize(hhmembers_n = n(),
                children_n = sum(ifelse(QB06 <= 16, 1, 0))) %>%
      ungroup() %>% 
      distinct() %>%
      left_join(ind1, by = 'men_ch') %>%
      left_join(ind3, by = 'men_ch') %>%
      left_join(ind4, by = 'men_ch') %>%
      left_join(ind5, by = 'men_ch') %>%
      left_join(ind6, by = 'men_ch') %>%
      left_join(ind7, by = 'men_ch') %>%
      left_join(ind8, by = 'men_ch') %>%
      left_join(ind9, by = 'men_ch') %>%
      left_join(ind10, by = 'men_ch') %>%
      left_join(hh_head, by = 'men_ch') %>% 
      left_join(age, by = 'men_ch') %>% 
      left_join(urban, by = 'men_ch') %>% 
      left_join(marital, by = 'men_ch') %>% 
      left_join(education, by = 'men_ch') %>% 
      mutate(health = rowMeans(select(.,ind1) %>% 
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
             s_ind = factor(x = ifelse(score >= 1/2, 1, 0), levels = c(0,1), labels = c('no', 'yes')),
             m = i,
             b = j) %>% 
      bind_rows(census02_simulation)
    
    # census02_simulation <- pop02 %>% 
    #   mutate_at(.vars = c('QA01', 'QA02', 'QA04'), ~as.integer(.)) %>% 
    #   mutate(QA01 = if_else(QA01 < 10, 
    #                         paste0("0",QA01), 
    #                         as.character(QA01)),
    #          wt = 10,
    #          geoid = paste0(QA01, QA02, QA04),
    #          geoid = replace(geoid, geoid == '0148', '0142'),
    #          geoid = replace(geoid, geoid == '0218', '0212'),
    #          geoid = replace(geoid, geoid == '0228', '0222'),
    #          geoid = replace(geoid, geoid == '0238', '0232'),
    #          geoid = replace(geoid, geoid == '0318', '0314'),
    #          geoid = replace(geoid, geoid == '0328', '0321'),
    #          geoid = replace(geoid, geoid == '0338', '0333'),
    #          geoid = replace(geoid, geoid == '0418', '0413'),
    #          geoid = replace(geoid, geoid == '0428', '0421'),
    #          geoid = replace(geoid, geoid == '0438', '0432'),
    #          geoid = replace(geoid, geoid == '0518', '0512'),
    #          geoid = replace(geoid, geoid == '0528', '0523'),
    #          geoid = replace(geoid, geoid == '0538', '0532'),
    #          geoid = replace(geoid, geoid == '0618', '0612'),
    #          geoid = replace(geoid, geoid == '0628', '0622'),
    #          geoid = replace(geoid, geoid == '0638', '0633'),
    #          geoid = replace(geoid, geoid == '0718', '0712'),
    #          geoid = replace(geoid, geoid == '0728', '0722'),
    #          geoid = replace(geoid, geoid == '0738', '0731'),
    #          geoid = replace(geoid, geoid == '0818', '0811'),
    #          geoid = replace(geoid, geoid == '0828', '0822'),
    #          geoid = replace(geoid, geoid == '0838', '0831'),
    #          geoid = replace(geoid, geoid == '0918', '0915'),
    #          geoid = replace(geoid, geoid == '0928', '0921'),
    #          geoid = replace(geoid, geoid == '0938', '0932'),
    #          geoid = replace(geoid, geoid == '1018', '1013'),
    #          geoid = replace(geoid, geoid == '1028', '1021'),
    #          geoid = replace(geoid, geoid == '1038', '1034'),
    #          geoid = replace(geoid, geoid == '1118', '1114'),
    #          geoid = replace(geoid, geoid == '1128', '1122'),
    #          geoid = replace(geoid, geoid == '1138', '1131')) %>% 
    #   ungroup() %>% 
    #   select(hhid = men_ch,
    #          geoid, 
    #          wt,
    #          residence_type = QA03, 
    #          gender = QB04, 
    #          age = QB06, 
    #          edu_attainment = QB15, 
    #          literacy_status = QB16A, 
    #          employment_type = QB19, 
    #          marital_status = QB20) %>% 
    #   mutate(m = i,
    #          b = j) %>% 
    #   bind_rows(census02_simulation)
    
    # Save it -----------------------------------------------------------------
    
    if (impute == TRUE){
      if (boot == FALSE){
        saveRDS(census02_simulation, file = "./data/midsave/census_2002_imputed_simulation_alt.rds")
      }else{
        saveRDS(census02_simulation, file = paste0("./data/midsave/census/census_2002_imputed_simulation_",r,"_alt.rds"))
      }
    } else{
      if (boot == FALSE){
        saveRDS(census02_simulation, file = "./data/midsave/census/census_2002_simulation_alt.rds")
      }else{
        saveRDS(census02_simulation, file = paste0("./data/midsave/census/census_2002_simulation_",r,"_alt.rds"))
      }
    }
    
    r = r + 1
    
    print(paste0("Bootstrap: ",j," of ",b,", imputation: ",i," of ",m))
    
  }
}
