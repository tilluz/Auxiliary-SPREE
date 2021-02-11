library(foreign)
library(rgdal)
library(broom)
library(tidyverse)
library(haven)
library(sp)
library(mice)

setwd("your_directory_path")

# Read census 2013 -----------------------------------------------------------

'Source: http://anads.ansd.sn/index.php/catalog/51'

# Population census
pop13_raw <- read.spss("./data/census_2013/spss_car_individus_10eme_dr.sav",
                       to.data.frame = TRUE)

# Housing census
hab13_raw <- read.spss("./data/census_2013/spss_habitat_10eme_dr.sav",
                       to.data.frame=TRUE)

# Set flags ---------------------------------------------------------------

impute = TRUE
m = 1

boot = FALSE
b = 100

# Prepare census 2013 -----------------------------------------------------

# Create common household identifier for population and habitat file
pop13_raw <- pop13_raw %>% 
  mutate(A08_conces = A08_conces %>% 
           as.character() %>% 
           as.numeric(),
         A09_Menage = A09_Menage %>% 
           as.character() %>% 
           as.numeric(),
         A08 = if_else(A08_conces<10, paste0("000",as.character(A08_conces)),
                       if_else(A08_conces>=10 & A08_conces<100, paste0("00",as.character(A08_conces)),
                               if_else(A08_conces>=100 & A08_conces<1000, paste0("0",as.character(A08_conces)),
                                       as.character(A08_conces)
                               )
                       )
         ),
         A09 = if_else(A09_Menage<10, paste0("00",as.character(A09_Menage)),
                       if_else(A09_Menage>=10 & A09_Menage<100, paste0("0",as.character(A09_Menage)),
                               as.character(A09_Menage)
                       )
         ),
         hhid = paste0(IDDR, A08, A09),
         m = 1
  ) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  mutate(B08 = as.integer(B08),
         B29 = as.character(B29),
         B32 = as.integer(B32),
         B43 = as.integer(as.character(B43)),
         B44 = as.integer(as.character(B44)),
         B45 = as.integer(B45),
         B46 = as.integer(B46)) %>% 
  mutate(ind1 = factor(x = if_else(B43 + B44 - B45 - B46 > 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived')),
         ind3 = factor(x = case_when(
           B29 == 'non, n’a jamais fréquenté' | B32 < 9
           ~1,
           TRUE ~ 0), levels = c(0,1), labels = c('not_deprived', 'deprived')),
         ind3 = replace(ind3, (is.na(B29)) & (is.na(B32)), NA),
         ind4 = factor(x = case_when(
           (B29 == "non, n’a jamais fréquenté")
           | (B08 == 9 & B32 <= 4)
           | (B08 == 10 & B32 <= 5)
           | (B08 == 11 & B32 <= 6)
           | (B08 == 12 & B32 <= 7)
           | (B08 == 13 & B32 <= 8)
           | (B08 == 14 & B32 <= 9)
           | (B08 == 15 & B32 <= 10)
           | (B08 == 16 & B32 <= 11) ~ 1,
           TRUE ~ 0), levels = c(0,1), labels = c('not_deprived', 'deprived')),
         ind4 = replace(ind4, (is.na(B32)) & (B29 != "non, n’a jamais fréquenté"), NA),
         ind4 = replace(ind4, (B08 < 7) | (B08 > 16), NA))

hab13_raw <- hab13_raw %>% 
  mutate(A08_conces = A08_concess %>% 
           as.character() %>% 
           as.numeric(),
         A09_Menage = A09_menage %>% 
           as.character() %>% 
           as.numeric(),
         A08 = if_else(A08_conces<10, paste0("000",as.character(A08_conces)),
                       if_else(A08_conces>=10 & A08_conces<100, paste0("00",as.character(A08_conces)),
                               if_else(A08_conces>=100 & A08_conces<1000, paste0("0",as.character(A08_conces)),
                                       as.character(A08_conces)
                               )
                       )
         ),
         A09 = if_else(A09_Menage<10, paste0("00",as.character(A09_Menage)),
                       if_else(A09_Menage>=10 & A09_Menage<100, paste0("0",as.character(A09_Menage)),
                               as.character(A09_Menage)
                       )
         ),
         hhid = paste0(IDDR, A08, A09),
         m = 1
  ) %>% 
  mutate_if(is.labelled, as_factor)

# Impute missing values ---------------------------------------------------

if (impute == TRUE){
  
  # Population file (variables used: B08, B29, B32, B43, B44, B45, B46)
  # pop13_red <- pop13_raw %>% 
  #   select_at(vars(B08, B29, B32, B43, B44, B45, B46))
  
  pop13_red <- pop13_raw %>%
    select_at(vars(-IDDR, -hhid, -contains('A'), -contains('_'), -B15, -B16, -B01, -B47, -B48, -m))
  
  pop13_mice <- mice(data = pop13_red,
                     pred = quickpred(pop13_red),
                     m = m,
                     maxit = 1,
                     nnet.MaxNWts = 10000)
  
  pop13_complete <- complete(pop13_mice, action = 'long') %>% 
    mutate(m = .imp) %>% 
    select(-.imp,-.id)
  
  # Habitat file (variables used: E07, E08, E10, E11, E12, contains('E13'), contains('E14'))
  hab13_red <- hab13_raw %>% 
    select_at(vars(E07, E08, E10, E11, E12, contains('E13'), contains('E14')))
  
  # hab13_red <- hab13_raw %>% 
  # select_at(vars(-IDDR, -contains('A'), -typ_pop, -hhid, -cacr, -m))
  
  hab13_mice <- mice(data = hab13_red,
                     pred = quickpred(hab13_red),
                     m = m,
                     maxit = 1,
                     nnet.MaxNWts = 10000)
  
  hab13_complete <- complete(hab13_mice, action = 'long') %>% 
    mutate(m = .imp) %>% 
    select(-.imp,-.id)
  
}else{
  m = 1
}

# Start loop over imputations ---------------------------------------------

census13_application = data.frame()
census13_simulation = data.frame()

r = 1

for (i in 1:m){
  
  if (impute == TRUE){
    pop13_imp <- pop13_complete %>% 
      filter(m == i) %>% 
      bind_cols(pop13_raw[,!(names(pop13_raw) %in% c(names(pop13_red), 'm'))])
    
    hab13_imp <- hab13_complete %>% 
      filter(m == i) %>% 
      bind_cols(hab13_raw[,!(names(hab13_raw) %in% c(names(hab13_red), 'm'))])
  }else{
    pop13_imp <- pop13_raw
    hab13_imp <- hab13_raw
  }
  
  
  # Start loop over bootstrap runs ------------------------------------------
  
  if (boot == FALSE){
    pop13 <- pop13_imp
    hab13 <- hab13_imp
    b = 1
  }
  
  for (j in 1:b){
    
    if (boot == TRUE){
      pop13 <- pop13_imp %>% 
        group_by(IDDR) %>% 
        sample_n(size = n(), replace = TRUE) %>% 
        ungroup()
      
      hab13 <- pop13 %>% 
        select(hhid) %>% 
        distinct() %>% 
        left_join(hab13_imp, by = 'hhid')
    }
    
    # Household-level MPI -----------------------------------------------------
    
    'Source: https://en.wikipedia.org/wiki/Multidimensional_Poverty_Index'
    
    # Health
    ### Indicator 1: Child mortality - Deprived if any child has died in the family
    ind1 <- 
      pop13 %>% 
      select(hhid, B06, B08, ind1) %>% 
      drop_na() %>% 
      filter(B08 >= 15 & B08 <= 49) %>% 
      group_by(hhid) %>% 
      summarize(ind1 = factor(x = max(as.integer(ind1)-1, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 2: Nutrition - Deprived if any adult or child, for whom there is nutritional information, is underweight
    
    
    # Education
    ### Indicator 3: Years of schooling - Deprived if no household member has completed six years of schooling
    ind3 <- 
      pop13 %>% 
      select(hhid, B29, B32, ind3) %>% 
      filter(!is.na(B29) | !is.na(B32)) %>% 
      group_by(hhid) %>% 
      summarize(ind3 = factor(x = min(as.integer(ind3)-1, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 4: School attendance - Deprived if any school-aged child is not attending school up to class 8
    ##### Primary school in Senegal starts at age 7 (https://www.who.int/immunization/programmes_systems/policies_strategies/Senegal_country_report.pdf?ua=1),
    ##### consequently we define target group similar to "Alkire, Sabina, et al. "Exploring multidimensional poverty in China: 2010 to 2014."
    ##### Research on Economic Inequality 25 (2017): 161-228."
    ##### Consequently, we say: 15-year olds are deprived if they neither completed class 8 or above nor ever visited school, 
    ##### 14-year olds are deprived if they neither completed class 7 or above nor ever visited school and so on...
    ind4 <- 
      pop13 %>%
      select(hhid, B29, B08, B32, ind4) %>% 
      filter((B08 >= 7) & (B08 <= 16)) %>% 
      filter(!is.na(B32) | B29 == "non, n’a jamais fréquenté") %>% 
      group_by(hhid) %>% 
      summarize(ind4 = factor(x = max(as.integer(ind4)-1, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    # Living Standards
    ### Indicator 5: Cooking fuel - Deprived if the household cooks with dung, wood or charcoal
    ind5 <- 
      hab13 %>% 
      select(hhid, E12) %>% 
      drop_na() %>% 
      mutate(
        ind5 = case_when(
          (E12 == "Bois")
          | (E12 == "Charbon")
          | (E12 == "Bouse de vache")
          | (E12 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(hhid) %>% 
      summarize(ind5 = factor(x = max(ind5, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 6: Sanitation - Deprived if the household's sanitation facility is not improved (according to MDG guidelines), or it is improved but shared with other households
    ind6 <- 
      hab13 %>% 
      select(hhid, E08) %>% 
      drop_na() %>% 
      mutate(
        ind6 = case_when(
          (E08 == "Dans la nature")
          | (E08 == "Edicule public")
          | (E08 == "Non couverte")
          | (E08 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(hhid) %>% 
      summarize(ind6 = factor(x = max(ind6, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 7: Drinking Water - Deprived if the household does not have access to safe drinking water (according to MDG guidelines) or safe drinking water is more than a 30-minute walk from home roundtrip
    ind7 <- 
      hab13 %>% 
      select(hhid, E10) %>% 
      drop_na() %>% 
      mutate(
        ind7 = case_when(
          (E10 == "Puits non protégé")
          | (E10 == "Source non protégée")
          | (E10 == "Camion citerne")
          | (E10 == "Eau minérale/améliorée")
          | (E10 == "Eau de surface")
          | (E10 == "Charrette avec petite citerne/tonneau")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(hhid) %>% 
      summarize(ind7 = factor(x = max(ind7, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 8: Electricity - Deprived if the household has no electricity
    ind8 <- 
      hab13 %>% 
      select(hhid, E11) %>% 
      drop_na() %>% 
      mutate(
        ind8 = case_when(
          (E11 == "Gaz")
          | (E11 == "Lampe à pétrole artisanale")
          | (E11 == "Lampe tempête")
          | (E11 == "Bougie")
          | (E11 == "Bois")
          | (E11 == "Lampe rechargeable")
          | (E11 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(hhid) %>% 
      summarize(ind8 = factor(x = max(ind8, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 9: Housing - Deprived if the household has a dirt floor
    ind9 <- 
      hab13 %>% 
      select(hhid, E07) %>% 
      drop_na() %>% 
      mutate(
        ind9 = case_when(
          (E07 == "Sable")
          | (E07 == "Argile/banco")
          | (E07 == "Autre")
          ~ 1,
          TRUE ~ 0)
      ) %>% 
      group_by(hhid) %>% 
      summarize(ind9 = factor(x = max(ind9, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    ### Indicator 10: Assets - Deprived if the household does not own more than one of these assets: radio, TV, telephone, computer, Animal cart, bicycle, motorbike or refrigerator and does not own a car or truck
    ind10 <- 
      hab13 %>% 
      select(hhid, E13_1, E13_2, E13_4, E13_5, E13_6, E14_1, E14_2, E14_3, E14_4, E14_5) %>% 
      drop_na() %>% 
      mutate(across(where(is.factor), as.integer)) %>% 
      mutate(ind10 = ifelse(
        ((E13_1+E13_2+E13_4+E13_5+E13_6+E14_2+E14_3+E14_4+E14_5) <=10 & E14_1 == 1), 1, 0)) %>% 
      select(hhid, ind10) %>% 
      group_by(hhid) %>% 
      summarize(ind10 = factor(x = max(ind10, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
    
    
    # Other indicators --------------------------------------------------------
    
    # Female-headed households (male 0, female 1)
    hh_head <- 
      pop13 %>% 
      select(hhid, B04, B06) %>% 
      drop_na() %>% 
      mutate(hh_head = if_else((B04 == 'Chef de ménage') & (B06 == 'Féminin'), 1, 0)) %>% 
      group_by(hhid) %>% 
      summarise(hh_head = factor(x = max(hh_head, na.rm = T), levels = c(0,1), labels = c('male', 'female')))
    
    # Age of head of household
    age <- 
      pop13 %>% 
      select(hhid, B04, B08) %>% 
      drop_na() %>% 
      filter(B04 == 'Chef de ménage') %>% 
      transmute(hhid = hhid, 
                age_head = cut(B08, c(seq(0, 100, 5), Inf), right = FALSE)) %>% 
      distinct(hhid, .keep_all = T)
    
    # Urban status (rural 0, urban 1)
    urban <- 
      pop13 %>% 
      select(hhid, A10) %>% 
      drop_na() %>% 
      mutate(urban = if_else(A10 == 'Urbain', 1, 0)) %>%
      group_by(hhid) %>% 
      summarise(urban_head = factor(x = max(urban, na.rm = T), levels = c(0,1), labels = c('rural', 'urban'))) %>% 
      distinct(hhid, .keep_all = T)
    
    # Marital status of head of household
    '
    We had to summarize here two categories: apart and living together - to others to align with census 2002.
    '
    
    marital <- 
      pop13 %>% 
      select(hhid, B04, B41) %>% 
      drop_na() %>% 
      filter(B04 == 'Chef de ménage') %>%
      mutate(marital = if_else(B41 == 'Célibataire', 0, 
                               if_else((B41 == 'Monogame')
                                       |(B41 == 'Poly/1ère épouse')
                                       |(B41 == 'Poly/2ié  épouse/2 épouses')
                                       |(B41 == 'Poly/3iè épouse/3 épouses')
                                       |(B41 == 'Poly/4iè épouse /4 épouses')
                                       |(B41 == 'Poly/5iè épouse /5 épouses'), 1, 
                                       if_else(B41 == 'Veuf/Veuve', 2, 
                                               if_else(B41 == 'Divorcé(e)', 3, 4))))) %>%
      transmute(hhid = hhid,
                marital_head = factor(x = marital, 
                                      levels = c(0,1,2,3,4), 
                                      labels = c('never married','married','widowed','divorced','others'))) %>% 
      distinct(hhid, .keep_all = T)
    
    # Educational attainment of head of household
    education <- 
      pop13 %>% 
      select(hhid, B04, B32) %>% 
      drop_na() %>% 
      filter(B04 == 'Chef de ménage') %>%
      mutate(B32 = B32 %>% as.integer,
             education = if_else(B32 < 4, 0, 
                                 if_else((B32 >= 4) & (B32 < 10), 1, 
                                         if_else((B32 >= 10) & (B32 < 17), 2, 
                                                 if_else((B32 >= 17) & (B32 < 25), 3, 4))))) %>%
      transmute(hhid = hhid,
                education_head = factor(x = education, 
                                        levels = c(0,1,2,3,4), 
                                        labels = c('no education, preschool','primary','secondary','higher','others'))) %>% 
      distinct(hhid, .keep_all = T)
    
    
    # Fix sampling weight to align with official numbers ----------------------
    
    
    
    # pop_proj <- readRDS('./data/midsave/ppp_com.rds') %>%
    #   select(geoid, sen_ppp_2013) %>%
    #   transmute(COD_ENTITE = as.character(geoid),
    #             proj_2013 = sen_ppp_2013)
    
    '
    We should use demographic projections here, but as long as we do not have
    proper sub-national SI estimates, we will use SI estimates instead for
    consistency reasons. Official sub-national census 2013 counts are available
    at:
    Source: http://www.ansd.sn/ressources/publications/indicateurs/Projections-demographiques-2013-2025+.htm
    '
    
    pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
      select(COD_ENTITE, URBAN, ENSEMBLE20) %>% 
      transmute(COD_ENTITE = as.character(COD_ENTITE),
                proj_2013 = as.integer(as.character(ENSEMBLE20)))
    
    map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
    map_com$CODE <- as.character(map_com$CODE)
    
    census13_application <- pop13 %>% 
      ungroup() %>% 
      mutate(COD_ENTITE = substr(IDDR, start = 1, stop = 8),
             wt = coef2*10) %>% 
      group_by(hhid, COD_ENTITE) %>% 
      summarize(wt = wt,
                hhmembers_n = n(),
                children_n = sum(ifelse(B08 <= 16, 1, 0))) %>% 
      ungroup() %>% 
      distinct() %>% 
      left_join(ind1, by = 'hhid') %>% 
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
             s_ind = factor(x = ifelse(score >= 1/2, 1, 0), levels = c(0,1), labels = c('no', 'yes'))) %>% 
      left_join(pop_proj, by = 'COD_ENTITE') %>% 
      group_by(COD_ENTITE) %>% 
      mutate(wt = wt*proj_2013/sum(wt*hhmembers_n)) %>% 
      select(-proj_2013) %>% 
      ungroup() %>% 
      left_join(map_com@data %>% 
                  select(COD_ENTITE, CODE), by = 'COD_ENTITE') %>% 
      mutate(m = i,
             b = j) %>% 
      bind_rows(census13_application)
    
    # Create map identfier ----------------------------------------------------
    
    
    # census13_application <- census13 %>% 
    #   left_join(pop_proj, by = 'COD_ENTITE') %>% 
    #   group_by(COD_ENTITE) %>% 
    #   mutate(wt = wt*proj_2013/sum(wt*hhmembers_n)) %>% 
    #   select(-proj_2013) %>% 
    #   ungroup() %>% 
    #   mutate(m = i,
    #          b = j) %>% 
    #   bind_rows(census13_application)
    
    # census13_simulation <- census13 %>% 
    #   left_join(map_com@data %>% 
    #               select(COD_ENTITE, CODE), by = 'COD_ENTITE') %>% 
    #   group_by(CODE) %>% 
    #   mutate(wt = wt*proj_2013/sum(wt*hhmembers_n)) %>% 
    #   select(-proj_2013) %>% 
    #   ungroup() %>% 
    #   mutate(m = i,
    #          b = j) %>% 
    #   bind_rows(census13_application)
    
    # census13_simulation <- pop13 %>% 
    #   mutate(COD_ENTITE = substr(IDDR, start = 1, stop = 8)) %>% 
    #   left_join(census13_application %>% filter(m == i) %>% select(hhid, wt), by = 'hhid') %>% 
    #   select(hhid,
    #          COD_ENTITE,
    #          wt,
    #          residence_type = A10, 
    #          gender = B06, 
    #          age = B08, 
    #          edu_attainment = B32, 
    #          literacy_status = B34, 
    #          employment_type = B38, 
    #          marital_status = B41) %>% 
    #   mutate(m = i,
    #          b = j) %>% 
    #   bind_rows(census13_simulation)
    
    # Save it -----------------------------------------------------------------
    
    if (impute == TRUE){
      if (boot == FALSE){
        saveRDS(census13_application, file = "./data/midsave/census_2013_imputed_application_alt.rds")
        saveRDS(census13_simulation, file = "./data/midsave/census_2013_imputed_simulation_alt.rds")
      }else{
        saveRDS(census13_application, file = paste0("./data/midsave/census/census_2013_imputed_application_",r,"_alt.rds"))
        saveRDS(census13_simulation, file = paste0("./data/midsave/census/census_2013_imputed_simulation_",r,"_alt.rds"))
      }
    } else{
      if (boot == FALSE){
        saveRDS(census13_application, file = "./data/midsave/census/census_2013_application_alt.rds")
        saveRDS(census13_simulation, file = "./data/midsave/census/census_2013_simulation_alt.rds")
      }else{
        saveRDS(census13_application, file = paste0("./data/midsave/census/census_2013_application_",r,"_alt.rds"))
        saveRDS(census13_simulation, file = paste0("./data/midsave/census/census_2013_simulation_",r,"_alt.rds"))
      }
    }
    
    r = r + 1
    
    print(paste0("Bootstrap: ",j," of ",b,", imputation: ",i," of ",m))
  }
}






# Fix map -----------------------------------------------------------------

'Source: ANSD upon request'

map13 <- readOGR(dsn = './data/census_2013', layer="LimiteCommuneSenegal", verbose = F)
map13$MAP_ID <- row.names(map13)
map13 <- spTransform(map13, "+proj=longlat +datum=WGS84 +no_defs")
map13$COD_ENTITE <- as.character(map13$COD_ENTITE)

map13 %>% 
  as.data.frame() %>% 
  select(COD_ENTITE) %>% 
  n_distinct

'
Apparently, some codes are not unique. We use "CCRCA" from the map file to look it up in "cacr"
of the habitat file and extract the correct "COD_ENTITE" as the first 8 digits of "IDDR".
'

map13 %>% 
  as.data.frame() %>% 
  group_by(COD_ENTITE) %>% 
  filter(n()>1) %>% 
  select(CCRCA, COD_ENTITE)

hab13 %>% 
  mutate(geoid = substr(IDDR, start = 1, stop = 8)) %>% 
  select(geoid, cacr) %>% 
  distinct() %>% 
  View()

map13$COD_ENTITE[map13$COD_ENTITE == '04220201' & map13$CCRCA == 'PETE'] <- '04211100'
map13$COD_ENTITE[map13$COD_ENTITE == '04220202' & map13$CCRCA == 'GALOYA TOUCOULEUR'] <- '04210700'

map13 %>% 
  as.data.frame() %>% 
  select(COD_ENTITE) %>% 
  n_distinct

'
Now all codes are unique. Save for further use in map alignment.
'


# Save it -----------------------------------------------------------------

writeOGR(map13, "./data/census_2013/", "census_2013", driver="ESRI Shapefile", overwrite_layer=TRUE)