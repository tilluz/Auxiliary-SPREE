library(tidyverse)
library(rgdal)
library(ggplot2)
library(ggridges)
library(viridis)
library(ggthemes)
library(broom)
library(xtable)


setwd("your_directory_path")

# DHS
dhs_2013 <- readRDS("./data/midsave/dhs_2013.rds") %>% 
  mutate(wt = wt*hhmembers_n)
dhs_2014 <- readRDS("./data/midsave/dhs_2014.rds") %>% 
  mutate(wt = wt*hhmembers_n)
dhs_2015 <- readRDS("./data/midsave/dhs_2015.rds") %>% 
  mutate(wt = wt*hhmembers_n)
dhs_2016 <- readRDS("./data/midsave/dhs_2016.rds") %>% 
  mutate(wt = wt*hhmembers_n)
dhs_2017 <- readRDS("./data/midsave/dhs_2017.rds") %>% 
  mutate(wt = wt*hhmembers_n)
dhs_2018 <- readRDS("./data/midsave/dhs_2018.rds") %>% 
  mutate(wt = wt*hhmembers_n)
dhs_2019 <- readRDS("./data/midsave/dhs_2019.rds") %>% 
  mutate(wt = wt*hhmembers_n)

# Census
census13_raw <- readRDS('./data/midsave/census_2013_imputed_application_alt.rds') %>% 
  mutate(wt = wt*hhmembers_n) %>% 
  filter(m == 1)

# Population projections
high_pop <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
  transmute(COD_ENTITE = as.character(COD_ENTITE),
            reg_name = tolower(as.character(REG)),
            proj_2013 = as.integer(as.character(ENSEMBLE20)),
            proj_2014 = as.integer(as.character(ENSEMBLE_1)),
            proj_2015 = as.integer(as.character(ENSEMBLE_2)),
            proj_2016 = as.integer(as.character(ENSEMBLE_3)),
            proj_2017 = as.integer(as.character(ENSEMBLE_4)),
            proj_2018 = as.integer(as.character(ENSEMBLE_5)),
            proj_2019 = as.integer(as.character(ENSEMBLE_6)),
            proj_2020 = as.integer(as.character(ENSEMBLE_7)),
  ) %>% 
  group_by(reg_name) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  mutate(mape_2013 = 0,
            mape_2014 = abs((proj_2014 - proj_2013)/proj_2014)*100,
            mape_2015 = abs((proj_2015 - proj_2013)/proj_2015)*100,
            mape_2016 = abs((proj_2016 - proj_2013)/proj_2016)*100,
            mape_2017 = abs((proj_2017 - proj_2013)/proj_2017)*100,
            mape_2018 = abs((proj_2018 - proj_2013)/proj_2018)*100,
            mape_2019 = abs((proj_2019 - proj_2013)/proj_2019)*100,
            mape_2020 = abs((proj_2020 - proj_2013)/proj_2020)*100) %>% 
  ungroup %>% 
  arrange(-mape_2020) %>% 
  slice_head(n = 3) %>% 
  select(reg_name) %>% 
  as.matrix() %>% 
  as.character()

# Population estimates
ppp_com <- readRDS('./data/midsave/ppp_recalc_com_application.rds') %>% 
  rename(sen_ppp_2013 = pred_13,
         sen_ppp_2014 = pred_14,
         sen_ppp_2015 = pred_15,
         sen_ppp_2016 = pred_16,
         sen_ppp_2017 = pred_17,
         sen_ppp_2018 = pred_18,
         sen_ppp_2019 = pred_19,
         sen_ppp_2020 = pred_20
  )

# Updated Census (incl. bootstraps)
output_df <- readRDS("./data/midsave/application_output_semiparametric_hh_head_500.rds")
eval_df <- readRDS("./data/midsave/application_evaluation_semiparametric_hh_head_500.rds")

# Map
map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)

# Set theme
theme_set(theme_minimal())


# Commune-level headcount ratio in 2013 -----------------------------------

png(file="./visualizations/sen_hcr.png",
    width=1000, height=700)

map_com %>% 
  tidy() %>% 
  left_join(
    map_com@data %>% 
      select(MAP_ID, COD_ENTITE),
    by = c('id' = 'MAP_ID')
  ) %>% 
  left_join(
    census13_raw %>% 
      group_by(COD_ENTITE) %>% 
      summarise(hcr = sum(wt[h_ind == 'poor'], na.rm = T)/sum(wt[h_ind == 'poor' | h_ind == 'non-poor'], 
                                                              na.rm = T)) %>% 
      ungroup() %>% 
      mutate(hcr = cut(hcr, quantile(hcr, seq(0,1, 0.1)))), by = 'COD_ENTITE') %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = hcr)) +
  # geom_text(aes(label = COD_ENTITE), size = 3, hjust = 0.5)+
  scale_fill_brewer(10, palette = 'BrBG') +
  # scale_fill_gradient2(midpoint = 0.68, low = '#003c30', mid = "white" , high = '#543005', 
  # name = "Headcount\nRatio") +
  # scale_fill_viridis_c(option = "D", limit = c(-1,1)) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        legend.position = "none")

dev.off()


# Regional-level headcount ratios by sub-group ----------------------------

census13_raw %>% 
  mutate(COD_REG = substr(COD_ENTITE, start = 1, stop = 2)) %>% 
  group_by(COD_REG) %>% 
  summarise(hcr = 100*sum(wt[h_ind == 'poor'], na.rm = T)/sum(wt[h_ind == 'poor' | h_ind == 'non-poor'], 
                                                              na.rm = T),
            hcr_f = 100*sum(wt[h_ind == 'poor' & hh_head == 'female'], na.rm = T)/sum(wt[(h_ind == 'poor' | h_ind == 'non-poor') & hh_head == 'female'], na.rm = T),
            # hcr_y = 100*sum(wt[h_ind == 'poor' & (age_head == '[15,20)' | age_head == '[20,25)')], na.rm = T)/sum(wt[(h_ind == 'poor' | h_ind == 'non-poor') & (age_head == '[15,20)' | age_head == '[20,25)')], na.rm = T),
            # hcr_target = 100*sum(wt[h_ind == 'poor' & hh_head == 'female' & (age_head == '[15,20)' | age_head == '[20,25)')], na.rm = T)/sum(wt[(h_ind == 'poor' | h_ind == 'non-poor') & hh_head == 'female' & (age_head == '[15,20)' | age_head == '[20,25)')], na.rm = T)
  ) %>% 
  ungroup() %>% 
  left_join(map_com@data %>% 
              select(REG, COD_REG) %>% 
              distinct(), by = 'COD_REG') %>% 
  select(-COD_REG) %>% 
  arrange(REG) %>% 
  mutate(REG = str_to_title(REG)) %>% 
  relocate(REG) %>% 
  xtable(digits = 1) %>% 
  print(., include.rownames = FALSE)


# National stacked barchart in 2013 by gender ----------------------------

png(file="./visualizations/sen_contribution_bar_by_gender.png",
    width=1000, height=500)

census13_raw %>% 
  mutate(children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes'))) %>% 
  select(starts_with('ind'), children, hh_head, age_head, education_head, marital_head, urban_head, wt) %>% 
  pivot_longer(
    cols = starts_with("ind"),
    names_to = "ind",
    names_prefix = "ind",
    values_to = "value"
  ) %>% 
  drop_na() %>% 
  mutate(ind = factor(ind, levels = unique(ind))) %>% 
  group_by(hh_head, ind) %>% 
  summarise(h_i = sum(wt[value == 'deprived'])/sum(wt[value == 'deprived' | value == 'not_deprived']),
            n_i = sum(wt)) %>% 
  group_by(hh_head) %>% 
  mutate(p = max(n_i)/census13_raw$wt %>% sum(),
         # ind = ind %>% as.integer,
         w = 1/18,
         w = replace(w, ind == 1, 1/3),
         w = replace(w, ind == 3, 1/6),
         w = replace(w, ind == 4, 1/6),
         w = w,
         h_mean = weighted.mean(x = h_i, w = w),
         p_contr = w*h_i/h_mean,
         p_contr_adj = p_contr/sum(p_contr)*h_mean) %>% 
  ungroup() %>% 
  # unite('urban_head_children', c('children','urban_head')) %>%
  arrange(hh_head, ind) %>% 
  # mutate(ind = factor(ind, levels = unique(ind)),
  #        hh_head_urban_head = factor(hh_head_urban_head,
  #                                 levels = c("single_female", "single_male", "couple_female", "couple_male",
  #                                            "small_female", "small_male", "large_female", "large_male"))) %>% 
  mutate(hh_head = factor(hh_head,
                          levels = c("female", "male"))) %>% 
  # filter(hh_head_age_head == 'female_[15,20)' | hh_head_age_head == 'female_[20,25)' |
  #        hh_head_age_head == 'male_[15,20)' | hh_head_age_head == 'male_[20,25)') %>% 
  ggplot(aes(fill=ind, y=p_contr, x=hh_head)) + 
  geom_bar(position="stack", stat="identity", width = 0.9, alpha = 1) + 
  scale_fill_viridis_d(option = "D", name = "", labels = c("Child mortality",
                                                           "Years of schooling",
                                                           "School attendance",
                                                           "Cooking fuel",
                                                           "Sanitation",
                                                           "Drinking water",
                                                           "Electricity",
                                                           "Housing",
                                                           "Assets")) +
  geom_bar(aes(x = hh_head, y = p, color="Population share", group = 1), 
           stat="identity", position="identity",
           width = 0.25, alpha = 1, fill = 'white',
           size = 1) +
  geom_line(aes(x = hh_head, y = h_mean, color="Headcount ratio", group = 1),
            size = 2) +
  scale_color_manual(name="Indicators", values=c("Population share"="grey90", "Headcount ratio"="#d95f0e" )) +
  labs(x = "Sex of head of household",
       y = '(Contributions to) H') + 
  # scale_x_discrete(labels=c("F\n0-\n4", "F\n5-\n9", "F\n10-\n14", "F\n15-\n19", "F\n20-\n24", "F\n25-\n29", "F\n30-\n34", "F\n35-\n39",
  #                           "F\n40-\n44", "F\n45-\n49", "F\n50-\n54", "F\n55-\n59", "F\n60-\n64", "F\n65-\n69", "F\n70-\n74", "F\n75-\n79",
  #                           "F\n80-\n84", "F\n85-\n89", "F\n90-\n94", "F\n95-\n99", "F\n100+",
  #                           "M\n0-\n4", "M\n5-\n9", "M\n10-\n14", "M\n15-\n19", "M\n20-\n24", "M\n25-\n29", "M\n30-\n34", "M\n35-\n39",
  #                           "M\n40-\n44", "M\n45-\n49", "M\n50-\n54", "M\n55-\n59", "M\n60-\n64", "M\n65-\n69", "M\n70-\n74", "M\n75-\n79",
  #                           "M\n80-\n84", "M\n85-\n89", "M\n90-\n94", "M\n95-\n99", "M\n100+")) +
  # scale_x_discrete(labels=c("Female\n15-19", "Female\n20-24",
  #                           "Male\n15-19", "Male\n20-24")) +
  # scale_x_discrete(breaks = c("single_female", "single_male", "couple_female", "couple_male",
  #                             "small_female", "small_male", "large_female", "large_male"),
  #                  labels=c("Single\nfemale", "Single\nmale", "Couple\nfemale", "Couple\nmale",
#                           "Small\nfemale", "Small\nmale", "Large\nfemale", "Large\nmale")) +
scale_x_discrete(breaks = c("female", "male"),
                 labels=c("Female-headed\nhouseholds", "Male-headed\nhouseholds")) +
  # geom_rangeframe() +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        legend.spacing.y = unit(-0.01, "cm"))

dev.off()


# Indicator-specific headcount ratios by data source ----------------------

data <- list(census13_raw, dhs_2013, dhs_2014, dhs_2015, dhs_2016, dhs_2017, dhs_2018, dhs_2019)

h_by_source = data.frame()

for (i in 1:8){
  h_by_source <- data[[i]] %>% summarise(
    ind1 = sum(wt[ind1 == 'deprived'], na.rm = T)/sum(wt[ind1 == 'deprived' | ind1 == 'not_deprived'], na.rm = T)*100,
    ind3 = sum(wt[ind3 == 'deprived'], na.rm = T)/sum(wt[ind3 == 'deprived' | ind3 == 'not_deprived'], na.rm = T)*100,
    ind4 = sum(wt[ind4 == 'deprived'], na.rm = T)/sum(wt[ind4 == 'deprived' | ind4 == 'not_deprived'], na.rm = T)*100,
    ind5 = sum(wt[ind5 == 'deprived'], na.rm = T)/sum(wt[ind5 == 'deprived' | ind5 == 'not_deprived'], na.rm = T)*100,
    ind6 = sum(wt[ind6 == 'deprived'], na.rm = T)/sum(wt[ind6 == 'deprived' | ind6 == 'not_deprived'], na.rm = T)*100,
    ind7 = sum(wt[ind7 == 'deprived'], na.rm = T)/sum(wt[ind7 == 'deprived' | ind7 == 'not_deprived'], na.rm = T)*100,
    ind8 = sum(wt[ind8 == 'deprived'], na.rm = T)/sum(wt[ind8 == 'deprived' | ind8 == 'not_deprived'], na.rm = T)*100,
    ind9 = sum(wt[ind9 == 'deprived'], na.rm = T)/sum(wt[ind9 == 'deprived' | ind9 == 'not_deprived'], na.rm = T)*100,
    ind10 = sum(wt[ind10 == 'deprived'], na.rm = T)/sum(wt[ind10 == 'deprived' | ind10 == 'not_deprived'], na.rm = T)*100,
    h_ind = sum(wt[h_ind == 'poor'], na.rm = T)/sum(wt[(h_ind == 'poor' | h_ind == 'non-poor')], na.rm = T)*100) %>% 
    bind_rows(h_by_source)
}

h_by_source %>% 
  arrange(-row_number()) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(across(is.numeric, round, 1)) %>% 
  as.data.frame(row.names = c('Child mortality', 'Years of schooling', 'School attendance',
                              'Cooking fuel', 'Sanitation', 'Drinking water', 'Electricity',
                              'Housing', 'Assets', 'MPI headcount')) %>% 
  xtable(type = "latex")

# Distribution of Deprivation Score ---------------------------------------

png(file="./visualizations/score_distribution.png",
    width=1000, height=600)

score_dist <- census13_raw %>% 
  mutate(year = 'RGPHAE 2013') %>% 
  select(score, year, wt) %>% 
  bind_rows(
    dhs_2013 %>% 
      mutate(year = 'DHS 2013') %>% 
      select(score, year, wt)
  ) %>% 
  bind_rows(
    dhs_2014 %>% 
      mutate(year = 'DHS 2014') %>% 
      select(score, year, wt)
  ) %>% 
  bind_rows(
    dhs_2015 %>% 
      mutate(year = 'DHS 2015') %>% 
      select(score, year, wt)
  ) %>% 
  bind_rows(
    dhs_2016 %>% 
      mutate(year = 'DHS 2016') %>% 
      select(score, year, wt)
  ) %>% 
  bind_rows(
    dhs_2017 %>% 
      mutate(year = 'DHS 2017') %>% 
      select(score, year, wt)
  ) %>% 
  bind_rows(
    dhs_2018 %>% 
      mutate(year = 'DHS 2018') %>% 
      select(score, year, wt)
  ) %>% 
  bind_rows(
    dhs_2019 %>% 
      mutate(year = 'DHS 2019') %>% 
      select(score, year, wt)
  )

score_dist[rep(1:nrow(score_dist), score_dist$wt),] %>%
  data.frame() %>% 
  drop_na() %>% 
  transmute(year = factor(year, levels = unique(year)),
            score = score) %>% 
  ggplot(aes(x = score, y = year, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = 4,
    quantile_lines = FALSE) +
  scale_fill_viridis_d(name = "Quartile",
                       alpha = .7,
                       labels = c("1st", "2nd", "3rd", "4th")) + 
  geom_vline(xintercept = c(0.2, 0.33, 0.5),
             show.legend = FALSE) +
  annotate("text",
           x=c(0, 0.25, 0.38, 0.59),
           y=c(0.70, 0.70, 0.70, 0.70),
           label = c("Not MPI-poor", "Vulnerable", "MPI-poor", "Severely MPI-poor"), size = 4) +
  labs(x= "Deprivation Score",
       y = 'Data Source'
       # , title = 'Distribution of household-level deprivation scores by data source'
  ) +
  theme_tufte() +
  theme(text = element_text(size=20),
        axis.ticks = element_blank())

dev.off()


# Correlations between DHS 2013 and RGPHAE 2013 ---------------------------

census13_raw %>% 
  mutate(COD_REG = substr(COD_ENTITE, start = 1, stop = 2)) %>% 
  group_by(COD_REG) %>% 
  summarise(across(starts_with('ind'), ~(sum(wt[. == 'deprived'], na.rm = T)/sum(wt[. == 'deprived' | ind1 == 'not_deprived'], na.rm = T)*100))) %>% 
  ungroup() %>% 
  left_join(map_com@data %>% 
              select(REG, COD_REG) %>% 
              distinct(), by = 'COD_REG') %>% 
  select(-COD_REG) %>% 
  mutate(REG = tolower(REG)) %>% 
  left_join(dhs_2013 %>% 
              mutate(REG = as.character(v024)) %>% 
              group_by(REG) %>% 
              summarise(across(starts_with('ind'), ~(sum(wt[. == 'deprived'], na.rm = T)/sum(wt[. == 'deprived' | ind1 == 'not_deprived'], na.rm = T)*100))) %>% 
              ungroup() %>% 
              mutate(REG = gsub("è", "e", REG)) %>% 
              rename_with(~str_replace_all(.,"ind", "cind"), starts_with('ind')),by = 'REG') %>% 
  arrange(REG) %>% 
  select(-REG) %>% 
  cor() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl('c', rowname)) %>% 
  select(ind1:ind10)

# Changes of sub-national population shares over time ---------------------

# png(file="./visualizations/sim_pop_shares_over_time_appl.png",
#     width=1000, height=500)
# 
# ppp_com %>% 
#   filter(bb == 0) %>% 
#   mutate(reg = substr(geoid, start = 1, stop = 2)) %>% 
#   group_by(reg) %>% 
#   mutate_if(is.numeric, ~(./sum(.))) %>%
#   ungroup %>% 
#   transmute(geoid = geoid,
#             delta_2013 = sen_ppp_2013/sen_ppp_2013 - 1,
#             delta_2014 = sen_ppp_2014/sen_ppp_2013 - 1,
#             delta_2015 = sen_ppp_2015/sen_ppp_2013 - 1,
#             delta_2016 = sen_ppp_2016/sen_ppp_2013 - 1,
#             delta_2017 = sen_ppp_2017/sen_ppp_2013 - 1,
#             delta_2018 = sen_ppp_2018/sen_ppp_2013 - 1,
#             delta_2019 = sen_ppp_2019/sen_ppp_2013 - 1,
#             delta_2020 = sen_ppp_2020/sen_ppp_2013 - 1
#   ) %>% 
#   pivot_longer(
#     cols = starts_with("delta_"),
#     names_to = "year",
#     names_prefix = "delta_",
#     names_transform = list(year = as.integer),
#     values_to = "ppp",
#     values_drop_na = TRUE,
#   ) %>% 
#   ggplot(aes(x=year, y=ppp, colour = ppp, group=geoid)) +
#   geom_line(size=0.2) + 
#   geom_abline(intercept = 0, slope = 0) +
#   scale_colour_viridis(option = "D") +
#   # ggtitle('Changes in population shares over time (by commune, in %, 2013 baseline)') +
#   labs(x = "Year",
#        y = 'Change in %') + 
#   # geom_rangeframe() +
#   theme_tufte() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position="none",
#         text =  element_text(size=20),
#         axis.ticks = element_blank())
# 
# dev.off()

# Updated census maps -----------------------------------------------------

headcount <- output_df %>% 
  filter(!is.na(COD_ENTITE),
         ind == 'h_ind') %>% 
  group_by(COD_ENTITE, year) %>% 
  summarise(h = sum(ipf[status == 'poor' & hh_head == 'male'])/sum(ipf[hh_head == 'male']),
            hf = sum(ipf[status == 'poor' & hh_head == 'female'])/sum(ipf[hh_head == 'female'])) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = c(h, hf)) %>% 
  transmute(COD_ENTITE = COD_ENTITE,
            g_diff_2013 = 0,
            g_diff_2014 = hf_2014/hf_2013 - h_2014/h_2013,
            g_diff_2015 = hf_2015/hf_2013 - h_2015/h_2013,
            g_diff_2016 = hf_2016/hf_2013 - h_2016/h_2013,
            g_diff_2017 = hf_2017/hf_2013 - h_2017/h_2013,
            g_diff_2018 = hf_2018/hf_2013 - h_2018/h_2013,
            g_diff_2019 = hf_2019/hf_2013 - h_2019/h_2013,
            g_diff_2020 = hf_2020/hf_2013 - h_2020/h_2013)
# mutate(across(g_diff_2014:g_diff_2020, ~(cut(., quantile(., seq(0, 1, 0.1))))))

headcount_map <- map_com %>% 
  tidy() %>% 
  left_join(
    map_com@data %>% 
      select(MAP_ID, COD_ENTITE),
    by = c('id' = 'MAP_ID')
  ) %>% 
  left_join(
    headcount, by = 'COD_ENTITE')

plot_list = list()
for (i in 2013:2020){
  p = headcount_map %>% 
    ggplot(aes(x = long, y = lat)) +
    geom_polygon(aes(group = group, fill = !!sym(paste0("g_diff_",i)))) +
    # geom_text(aes(label = COD_ENTITE), size = 3, hjust = 0.5)+
    scale_fill_distiller(type = "div", palette = 'BrBG', limit = c(-0.5,0.5), direction = -1) +
    # scale_fill_brewer(10, palette = 'BrBG') + 
    # scale_fill_viridis_c(option = "D", limit = c(-1,1)) +
    theme_void() +
    theme(legend.position="none")
  plot_list[[i]] = p
}

for (i in 2013:2020){
  file_name = paste0("./visualizations/headcount_map_",i,".png")
  png(file_name, width=1000, height=700)
  print(plot_list[[i]])
  dev.off()
}

png(file="./visualizations/headcount_map_legend.png", width=1000, height=700)

headcount_map %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = !!sym(paste0("g_diff_",2020)))) +
  # geom_text(aes(label = COD_ENTITE), size = 3, hjust = 0.5)+
  scale_fill_distiller(type = "div", palette = 'BrBG', limit = c(-0.5,0.5), direction = -1, name = 'Difference in\ngrowth rates') +
  # scale_fill_brewer(10, palette = 'BrBG') + 
  # scale_fill_viridis_c(option = "D", limit = c(-1,1)) +
  theme_void() +
  theme(text = element_text(size=20),
        legend.position="right")

dev.off()

# National stacked bar chart with contributions by gender -----------------------------------

sen_headcount <- output_df %>% 
  filter(!is.na(COD_ENTITE),
         ipf_type == 'prop',
         status == 'poor',
         ind == 'h_ind') %>% 
  group_by(year) %>% 
  summarise(h_mean = mean(p_ipf[hh_head == 'male']),
            hf_mean = mean(p_ipf[hh_head == 'female']),
            h_cilo = mean(cilo[hh_head == 'male']),
            h_ciup = mean(ciup[hh_head == 'male']),
            hf_cilo = mean(cilo[hh_head == 'female']),
            hf_ciup = mean(ciup[hh_head == 'female'])) %>% 
  ungroup()

sen_headcount_df <- output_df %>% 
  filter(!is.na(COD_ENTITE),
         ipf_type == 'prop',
         status == 'deprived',
         ind != 'h_ind') %>%
  group_by(year, ind) %>% 
  summarise(h_i = mean(p_ipf[hh_head == 'male']),
            hf_i = mean(p_ipf[hh_head == 'female'])) %>% 
  ungroup() %>% 
  left_join(sen_headcount, by = 'year') %>% 
  group_by(year) %>% 
  mutate(ind = gsub("ind", "", ind) %>% as.integer,
         w = 1/18,
         w = replace(w, ind == 1, 1/3),
         w = replace(w, ind == 3, 1/6),
         w = replace(w, ind == 4, 1/6),
         p_contr = w*h_i/h_mean,
         p_contr_f = w*hf_i/hf_mean,
         p_contr_adj = p_contr/sum(p_contr)*h_mean,
         p_contr_f_adj = p_contr_f/sum(p_contr_f)*hf_mean) %>% 
  ungroup() %>% 
  arrange(year, ind) %>% 
  mutate(ind = factor(ind, levels = unique(ind)))

png(file="./visualizations/appl_contribution_bar_sen.png", 
    width=1000, height=500)

sen_headcount_df %>% 
  ggplot(aes(fill=ind, y=p_contr_f_adj, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 0.9, alpha = 1) + 
  scale_fill_viridis_d(option = "D", name = "", labels = c("Child mortality",
                                                           "Years of schooling",
                                                           "School attendance",
                                                           "Cooking fuel",
                                                           "Sanitation",
                                                           "Drinking water",
                                                           "Electricity",
                                                           "Housing",
                                                           "Assets")) +
  geom_ribbon(aes(ymin = ifelse(hf_cilo < 0, 0, hf_cilo),
                  ymax = ifelse(hf_ciup > 1, 1, hf_ciup)),
              color = "grey70",
              alpha=0.1,) + 
  geom_line(aes(x = year, y = hf_mean, color="H"),
            size = 1) +
  scale_color_manual(name="Indicators", values=c("H"="grey90")) +
  labs(x = "Year",
       y = '(Contributions to) H') + 
  # geom_rangeframe() +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        legend.spacing.y = unit(-0.01, "cm"))

dev.off()

# Communal evolution of headcount ratio -----------------------------------

com_headcount <- output_df %>% 
  filter(COD_ENTITE == '01130133', #05120103 #01130133
         status == 'poor',
         ipf_type == 'prop',
         ind == 'h_ind') %>% 
  group_by(year) %>% 
  summarise(h_mean = mean(p_ipf[hh_head == 'male']),
            hf_mean = mean(p_ipf[hh_head == 'female']),
            h_cilo = mean(cilo[hh_head == 'male']),
            h_ciup = mean(ciup[hh_head == 'male']),
            hf_cilo = mean(cilo[hh_head == 'female']),
            hf_ciup = mean(ciup[hh_head == 'female'])) %>% 
  ungroup()

com_headcount_df <- output_df %>% 
  filter(COD_ENTITE == '01130133',
         ipf_type == 'prop',
         status == 'deprived',
         ind != 'h_ind') %>%
  group_by(year, ind) %>% 
  summarise(h_i = mean(p_ipf[hh_head == 'male']),
            hf_i = mean(p_ipf[hh_head == 'female'])) %>% 
  ungroup() %>% 
  left_join(com_headcount, by = 'year') %>% 
  group_by(year) %>% 
  mutate(ind = gsub("ind", "", ind) %>% as.integer,
         w = 1/18,
         w = replace(w, ind == 1, 1/3),
         w = replace(w, ind == 3, 1/6),
         w = replace(w, ind == 4, 1/6),
         p_contr = w*h_i/h_mean,
         p_contr_f = w*hf_i/hf_mean,
         p_contr_adj = p_contr/sum(p_contr)*h_mean,
         p_contr_f_adj = p_contr_f/sum(p_contr_f)*hf_mean) %>% 
  ungroup() %>% 
  arrange(year, ind) %>% 
  mutate(ind = factor(ind, levels = unique(ind)))

# Communal stacked barchart with contributions ----------------------------

png(file="./visualizations/appl_contribution_bar_yoff.png", 
    width=1000, height=300)

com_headcount_df %>% 
  ggplot(aes(fill=ind, y=p_contr_f_adj, x=year)) + 
  geom_bar(position="stack", stat="identity", width = 0.9, alpha = 1) + 
  scale_fill_viridis_d(option = "D", name = "", labels = c("Child mortality",
                                                           "Years of schooling",
                                                           "School attendance",
                                                           "Cooking fuel",
                                                           "Sanitation",
                                                           "Drinking water",
                                                           "Electricity",
                                                           "Housing",
                                                           "Assets")) +
  geom_ribbon(aes(ymin = ifelse(hf_cilo < 0, 0, hf_cilo),
                  ymax = ifelse(hf_ciup > 1, 1, hf_ciup)),
              color = "grey70",
              alpha=0.1,) + 
  geom_line(aes(x = year, y = hf_mean, color="H"),
            size = 1) +
  scale_color_manual(name="Indicators", values=c("H"="grey90")) +
  labs(x = "Year",
       y = '(Contributions to) H') + 
  # geom_rangeframe() +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        legend.spacing.y = unit(-0.01, "cm"))

dev.off()



# Evaluate the CV ---------------------------------------------------------

# Female
output_df %>% 
  filter(ipf_type == 'prop', ind == 'h_ind', hh_head == 'female', status == 'poor') %>% 
  mutate(cv_sae = sqrt(mse)/p_ipf) %>% 
  group_by(year) %>% 
  summarise(ipf_mean = mean(p_ipf),
            across(cv_sae, list(q25 = ~quantile(., probs = 0.025)*100,
                                    q250 = ~quantile(., probs = 0.25)*100,
                                    median = ~median(.)*100,
                                    mean = ~mean(.)*100, 
                                    q750 = ~quantile(., probs = 0.75)*100,
                                    q975 = ~quantile(., probs = 0.975)*100))) %>% 
  xtable(digits = 2) %>% 
  print(., include.rownames=FALSE)

# Male
output_df %>% 
  filter(ipf_type == 'prop', ind == 'h_ind', hh_head == 'male', status == 'poor') %>% 
  mutate(cv_sae = sqrt(mse)/p_ipf) %>% 
  group_by(year) %>% 
  summarise(ipf_mean = mean(p_ipf),
            across(cv_sae, list(q25 = ~quantile(., probs = 0.025)*100,
                                q250 = ~quantile(., probs = 0.25)*100,
                                median = ~median(.)*100,
                                mean = ~mean(.)*100, 
                                q750 = ~quantile(., probs = 0.75)*100,
                                q975 = ~quantile(., probs = 0.975)*100))) %>% 
  xtable(digits = 2) %>% 
  print(., include.rownames=FALSE)

# True vs. predicted plots ------------------------------------------------
# 
# plot_list = list()
# for (i in 2013:2020){
#   p = eval_df %>% 
#     filter(ind == 'h_ind',
#            hh_head == 'female', 
#            year == {i},
#            (reg_name %in% high_pop & wt_type == 'dynamic_si_bench') | 
#              (!reg_name %in% high_pop & wt_type == 'static')) %>% 
#     mutate(year = as.factor(year)) %>% 
#     ggplot(aes(x = ipf, y = dhs)) +
#     geom_abline(intercept = 0, slope = 1) +
#     geom_point(aes(color = model, size = 2*glm_fit_sd), alpha = 0.5) +
#     geom_smooth(aes(color = model, fill = model), method = lm, size = 2) +
#     scale_color_viridis_d(option = 'D', end = 0.5) +
#     scale_fill_viridis_d(option = 'D', end = 0.5) +
#     labs(x = "Predicted",
#          y = 'Actual') +
#     xlim(0.2, 0.6) +
#     ylim(0.2, 0.6) +
#     theme_void() +
#     theme(plot.title = element_text(hjust = 0.5),
#           text =  element_text(size=20),
#           axis.ticks = element_blank(),
#           legend.position="none",
#           axis.text.x = element_blank(),
#           axis.text.y = element_blank())
#   plot_list[[i]] = p
# }
# 
# for (i in 2013:2020){
#   file_name = paste0("./visualizations/appl_tvp_reg_",i,".tif")
#   tiff(file_name, width=250, height=250)
#   print(plot_list[[i]])
#   dev.off()
# }
