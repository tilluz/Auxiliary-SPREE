library(tidyverse)
library(rgdal)
library(ggplot2)
library(ggridges)
library(viridis)
library(ggthemes)
library(broom)
library(rgeos)
library(sp)
library(foreign)
library(haven)
library(xtable)

setwd("your_directory_path")

# Map
map_x <- readOGR(dsn = './data/midsave/', layer="map_x", verbose = F)
map_x$geoid <- as.character(map_x$x_id)
map_x$COD_ENTITE <- as.character(map_x$COD_ENTITE)
map_x$CODE <- as.character(map_x$CODE)

map_com <- readOGR(dsn = './data/midsave/', layer="map_com_harmonised", verbose = F)
map_com$COD_ENTITE <- as.character(map_com$COD_ENTITE)
map_com$CODE <- as.character(map_com$CODE)

map_arr <- readOGR(dsn = './data/midsave/', layer="map_arr_harmonised", verbose = F)
map_arr$COD_ENTITE <- as.character(map_arr$COD_ENTITE)
map_arr$CODE <- as.character(map_arr$CODE)

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

# DHS 2013
dhs_2013 <- readRDS('./data/midsave/dhs_2013.rds') %>% 
  left_join(readRDS('./data/midsave/cl13.rds') %>% 
              select(DHSCLUST, CODE = geoid), by = c('v021' = 'DHSCLUST')) %>% 
  mutate(wt = wt*hhmembers_n,
         deptid = substr(CODE, start = 1, stop = 3),
         regid = substr(CODE, start = 1, stop = 2),
         children = factor(ifelse(children_n == 0, 0, 1), levels = c(0,1), labels = c('no', 'yes')),
         hh_size = factor(ifelse(hhmembers_n == 1, 0,
                                 ifelse((hhmembers_n == 2) & (children_n == 0), 1,
                                        ifelse((hhmembers_n == 2) & (children_n != 0), 2,
                                               ifelse((hhmembers_n >2) & (hhmembers_n < 6), 2, 3)))),
                          levels = c(0,1,2,3),
                          labels = c('single', 'couple', 'small', 'large')))

# WorldPop Population
ppp_x <- readRDS('./data/midsave/ppp_x.rds')
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
ppp_com_old <- readRDS('./data/midsave/ppp_com.rds')
ppp_arr <- readRDS(paste0("./data/midsave/ppp_arr_boot.rds"))
ppp_dept <- readRDS(paste0("./data/midsave/ppp_dept02_boot.rds"))

# HB population
ppp_hb <- readRDS(paste0("./data/midsave/pop_estimation_simulation_02_13.rds"))

# WP population
ppp_wp <- readRDS(paste0("./data/midsave/ppp_recalc_arr_02_13_boot_alt.rds"))

# ANSD population projections
pop_proj <- readOGR(dsn = './data/midsave/', layer="demographic_projections", verbose = F)@data %>% 
  select(COD_ENTITE, URBAN, ENSEMBLE20) %>% 
  transmute(COD_ENTITE = as.character(COD_ENTITE),
            proj_2013 = as.integer(as.character(ENSEMBLE20))
  )

# Habitat 2002 census
hab02 <- read.spss("./data/census_2002/Habitat_2002_10pc_ano.sav",
                   to.data.frame=TRUE) %>% 
  mutate(men_ch = as.character(men_ch),
         regid = if_else(QA01 %>% as.integer() < 10,
                         paste0("0",QA01 %>% as.integer()),
                         QA01 %>% as.integer() %>% as.character()),
         reg_name = QA01 %>% as.character())

# Simulation results
eval_df <- readRDS(file = "./data/midsave/simulation_evaluation_semiparametric_arr_500_self_alt.rds")
output_df <- readRDS(file = "./data/midsave/simulation_output_semiparametric_arr_500_self_alt.rds")

# Overlay of 2013 communes and 2002 arrondissements -----------------------

to_utm <- "+proj=utm +zone=28 +a=6378137 +rf=298.257222932867 +units=m +no_defs"
map_com <- spTransform(map_com, to_utm)
map_com <- gBuffer(map_com, byid=TRUE, width=20)
map_arr <- gUnaryUnion(map_com, map_com$CODE)


png(file="./visualizations/sim_overlay_2002_2013.png",
    width=1000, height=700)

plot(map_arr, border = viridis(4, option = "D")[1], lwd = 9)
plot(map_com, add = TRUE, border = viridis(4, option = "D")[3], lwd = 2)

dev.off()


# Sub-national population shares ------------------------------------------

# Arrondissements
pop_share_arr <- pop02 %>% 
  group_by(CODE) %>% 
  summarise(pop02 = sum(wt)) %>% 
  ungroup() %>% 
  left_join(
    pop13 %>% 
      group_by(CODE) %>% 
      summarise(pop13 = sum(wt)) %>% 
      ungroup(), by = 'CODE') %>% 
  left_join(ppp_arr %>% 
              select(CODE = geoid, ppp13 = sen_ppp_2013_sum) %>% 
              group_by(CODE) %>% 
              summarise(across(ppp13, list(mean = median, sd = sd))), by = 'CODE') %>% 
  mutate(regid = substr(CODE, start = 1, stop = 2)) %>% 
  group_by(regid) %>% 
  mutate(ppp13_mean = ppp13_mean/sum(ppp13_mean)*sum(pop13),
         true02_p = pop02/sum(pop02),
         pop02_p = pop02/sum(pop02),
         pop13_p = pop13/sum(pop13),
         ppp13_p = ppp13_mean/sum(pop13),
         pop02_sd = 0,
         pop13_sd = 0,
         ppp13_sd = ppp13_sd/sum(pop13)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(mix13_p = sum(pop02_p + ppp13_p)/2) %>% 
  ungroup()

# Department
pop_share_dept <- pop02 %>% 
  group_by(deptid) %>% 
  summarise(pop02 = sum(wt)) %>% 
  ungroup() %>% 
  left_join(
    pop13 %>% 
      group_by(deptid) %>% 
      summarise(pop13 = sum(wt)) %>% 
      ungroup(), by = 'deptid') %>% 
  left_join(ppp_dept %>% 
              select(deptid = geoid, ppp13 = sen_ppp_2013_sum) %>% 
              group_by(deptid) %>% 
              summarise(across(ppp13, list(mean = median, sd = sd))), by = 'deptid') %>% 
  mutate(regid = substr(deptid, start = 1, stop = 2)) %>% 
  group_by(regid) %>% 
  mutate(ppp13_mean = ppp13_mean/sum(ppp13_mean)*sum(pop13),
         true02_p = pop02/sum(pop02),
         pop02_p = pop02/sum(pop02),
         pop13_p = pop13/sum(pop13),
         ppp13_p = ppp13_mean/sum(pop13),
         pop02_sd = 0,
         pop13_sd = 0,
         ppp13_sd = ppp13_sd/sum(pop13)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(mix13_p = sum(pop02_p + ppp13_p)/2) %>% 
  ungroup()

# HB population
pop_share_hb <- pop02 %>% 
  group_by(CODE) %>% 
  summarise(pop02 = sum(wt)) %>% 
  ungroup() %>% 
  left_join(
    pop13 %>% 
      group_by(CODE) %>% 
      summarise(pop13 = sum(wt)) %>% 
      ungroup(), by = 'CODE') %>% 
  left_join(ppp_hb %>% 
              select(CODE = geoid, ppp13 = Nhat13) %>% 
              group_by(CODE) %>% 
              summarise(across(ppp13, list(mean = median, sd = sd))) %>% 
              ungroup(), by = 'CODE') %>% 
  mutate(regid = substr(CODE, start = 1, stop = 2)) %>% 
  group_by(regid) %>% 
  mutate(ppp13_mean = ppp13_mean/sum(ppp13_mean)*sum(pop13),
         true02_p = pop02/sum(pop02),
         pop02_p = pop02/sum(pop02),
         pop13_p = pop13/sum(pop13),
         ppp13_p = ppp13_mean/sum(pop13),
         pop02_sd = 0,
         pop13_sd = 0,
         ppp13_sd = ppp13_sd/sum(pop13)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(mix13_p = sum(pop02_p + ppp13_p)/2) %>% 
  ungroup()

# WP population
pop_share_wp <- pop02 %>% 
  group_by(CODE) %>% 
  summarise(pop02 = sum(wt)) %>% 
  ungroup() %>% 
  left_join(
    pop13 %>% 
      group_by(CODE) %>% 
      summarise(pop13 = sum(wt)) %>% 
      ungroup(), by = 'CODE') %>% 
  left_join(ppp_wp %>% 
              select(CODE = geoid, ppp13 = ppp13_sum) %>% 
              group_by(CODE) %>%
              summarise(across(ppp13, list(mean = median, sd = sd)))
            , by = 'CODE') %>% 
  mutate(regid = substr(CODE, start = 1, stop = 2)) %>% 
  group_by(regid) %>% 
  mutate(ppp13_mean = ppp13_mean/sum(ppp13_mean)*sum(pop13),
         true02_p = pop02/sum(pop02),
         pop02_p = pop02/sum(pop02),
         pop13_p = pop13/sum(pop13),
         ppp13_p = ppp13_mean/sum(ppp13_mean)) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(mix13_p = sum(pop02_p + ppp13_p)/2) %>% 
  ungroup()

png(file="./visualizations/sim_pop_shares.png",
    width=750, height=750)

pop_share_wp %>% 
  pivot_longer(
    cols = c('pop02_p', 'pop13_p', 'ppp13_p'),
    names_to = c("type"),
    names_pattern = "(.*)_p",
    values_to = "p"
  ) %>% 
  select(CODE, true02_p, type, p) %>% left_join(
    pop_share_arr %>% 
      pivot_longer(
        cols = c('pop02_sd', 'pop13_sd', 'ppp13_sd'),
        names_to = c("type"),
        names_pattern = "(.*)_sd",
        values_to = "sd") %>% 
      select(CODE, type, sd), by = c('CODE', 'type') 
  ) %>% 
  ggplot(aes(x = p, y = true02_p)) +
  geom_point(aes(colour = type, size = sd), alpha = 0.5) +
  # geom_smooth(aes(x = p, y = pop02_p, color = type, fill = type), method = lm) +
  scale_color_viridis_d(end = 1, option = 'D', name = "", labels = c("Fixed", "True", "Dynamic")) +
  scale_fill_viridis_c(option = 'D') +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Population shares 2013",
       y = 'Population shares RGPH 2002') +
  xlim(0, 0.6) +
  ylim(0, 0.6) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        legend.position="bottom") + 
  scale_size(guide = 'none',
             range = c(4, 20))+ 
  guides(colour = guide_legend(override.aes = list(size=8)))

dev.off()

# Within-region population shifts (arrondissements)
within_arr <- pop_share_arr %>% 
  group_by(regid) %>% 
  summarise(mape_pop13 = sum(abs((pop02_p - pop13_p)/pop02_p))/n()*100,
            mape_fixed = sum(abs((pop13_p - pop02_p)/pop13_p))/n()*100,
            mape_dynamic = sum(abs((pop13_p - ppp13_p)/pop13_p))/n()*100,
            mape_mix = sum(abs((pop13_p - mix13_p)/pop13_p))/n()*100,
            bias_fixed = sum(pop02_p - pop13_p)/n(),
            bias_dynamic = sum(ppp13_p - pop13_p)/n(),
            mse_fixed = sum((pop02_p - pop13_p)^2)/n(),
            mse_dynamic = sum((ppp13_p - pop13_p)^2)/n(),
            cor_pop13 = cor(pop02_p, pop13_p),
            cor_ppp13 = cor(pop13_p, ppp13_p)) %>% 
  ungroup() %>% 
  mutate(across(starts_with('ma'), round, 1),
         across(starts_with('cor'), round, 2),
         across(starts_with('bias'), round, 2),
         across(starts_with('mse'), round, 2)) %>% 
  left_join(hab02 %>% 
              select(regid, reg_name) %>% 
              distinct(), by = 'regid') %>% 
  select(-starts_with('mae'), -regid)

# Within-region population shifts (arrondissements, hb)
within_hb <- pop_share_hb %>% 
  group_by(regid) %>% 
  summarise(mape_pop13 = sum(abs((pop02_p - pop13_p)/pop02_p))/n()*100,
            mape_fixed = sum(abs((pop13_p - pop02_p)/pop13_p))/n()*100,
            mape_hb = sum(abs((pop13_p - ppp13_p)/pop13_p))/n()*100,
            mape_mix = sum(abs((pop13_p - mix13_p)/pop13_p))/n()*100,
            bias_fixed = sum(pop02_p - pop13_p)/n(),
            bias_hb = sum(ppp13_p - pop13_p)/n(),
            mse_fixed = sum((pop02_p - pop13_p)^2)/n(),
            mse_hb = sum((ppp13_p - pop13_p)^2)/n(),
            cor_pop13 = cor(pop02_p, pop13_p),
            cor_hb = cor(pop13_p, ppp13_p)) %>% 
  ungroup() %>% 
  mutate(across(starts_with('ma'), round, 1),
         across(starts_with('cor'), round, 2),
         across(starts_with('bias'), round, 2),
         across(starts_with('mse'), round, 2)) %>% 
  left_join(hab02 %>% 
              select(regid, reg_name) %>% 
              distinct(), by = 'regid') %>% 
  select(-starts_with('mae'), -regid)

# Within-region population shifts (arrondissements, wp)
within_wp <- pop_share_wp %>% 
  group_by(regid) %>% 
  summarise(mape_pop13 = sum(abs((pop02_p - pop13_p)/pop02_p))/n()*100,
            mape_fixed = sum(abs((pop13_p - pop02_p)/pop13_p))/n()*100,
            mape_hb = sum(abs((pop13_p - ppp13_p)/pop13_p))/n()*100,
            mape_mix = sum(abs((pop13_p - mix13_p)/pop13_p))/n()*100,
            bias_fixed = sum(pop02_p - pop13_p)/n(),
            bias_hb = sum(ppp13_p - pop13_p)/n(),
            mse_fixed = sum((pop02_p - pop13_p)^2)/n(),
            mse_hb = sum((ppp13_p - pop13_p)^2)/n(),
            cor_pop13 = cor(pop02_p, pop13_p),
            cor_hb = cor(pop13_p, ppp13_p)) %>% 
  ungroup() %>% 
  mutate(across(starts_with('ma'), round, 1),
         across(starts_with('cor'), round, 2),
         across(starts_with('bias'), round, 2),
         across(starts_with('mse'), round, 2)) %>% 
  left_join(hab02 %>% 
              select(regid, reg_name) %>% 
              distinct(), by = 'regid') %>% 
  select(-starts_with('mae'), -regid)

# Within-region population shifts (departments)
within_dept <- pop_share_dept %>% 
  group_by(regid) %>% 
  summarise(mape_pop13 = sum(abs((pop02_p - pop13_p)/pop02_p))/n()*100,
            mape_fixed = sum(abs((pop13_p - pop02_p)/pop13_p))/n()*100,
            mape_dynamic = sum(abs((pop13_p - ppp13_p)/pop13_p))/n()*100,
            mape_mix = sum(abs((pop13_p - mix13_p)/pop13_p))/n()*100,
            bias_fixed = sum(pop02_p - pop13_p)/n(),
            bias_dynamic = sum(ppp13_p - pop13_p)/n(),
            mse_fixed = sum((pop02_p - pop13_p)^2)/n(),
            mse_dynamic = sum((ppp13_p - pop13_p)^2)/n(),
            cor_pop13 = cor(pop02_p, pop13_p),
            cor_ppp13 = cor(pop13_p, ppp13_p)) %>% 
  ungroup() %>% 
  mutate(across(starts_with('ma'), round, 1),
         across(starts_with('cor'), round, 2),
         across(starts_with('bias'), round, 2),
         across(starts_with('mse'), round, 2)) %>% 
  left_join(hab02 %>% 
              select(regid, reg_name) %>% 
              distinct(), by = 'regid') %>% 
  select(-starts_with('mae'), -regid)

test <- within_arr %>% 
  select(reg_name, fixed_arr = mape_fixed, dynamic_arr = mape_dynamic, mixed_arr = mape_mix) %>% 
  # left_join(within_dept %>% 
  #             select(reg_name, rate_dept = mape_pop13, fixed_dept = mape_fixed, dynamic_dept = mape_dynamic, mixed_dept = mape_mix), 
  #           by = 'reg_name') %>% 
  left_join(within_hb %>% 
              select(reg_name, dynamic_hb = mape_hb, mixed_hb = mape_mix), 
            by = 'reg_name') %>% 
  left_join(within_wp %>% 
              select(reg_name, dynamic_wp = mape_hb, mixed_wp = mape_mix), 
            by = 'reg_name')


# Population fit analytics ------------------------------------------------

diff_pop_raw <- pop02 %>% 
  group_by(CODE) %>% 
  summarise(pop02 = sum(wt)) %>% 
  ungroup() %>% 
  left_join(
    pop13 %>% 
      group_by(CODE) %>% 
      summarise(pop13 = sum(wt)) %>% 
      ungroup(), by = 'CODE') %>% 
  left_join(ppp_wp %>% 
              select(CODE = geoid, ppp13 = ppp13_sum)
            , by = 'CODE') %>% 
  mutate(regid = substr(CODE, start = 1, stop = 2)) %>% 
  group_by(regid) %>% 
  mutate(ppp13 = ppp13/sum(ppp13)*sum(pop13),
         true02_p = pop02/sum(pop02),
         pop02_p = pop02/sum(pop02),
         pop13_p = pop13/sum(pop13),
         ppp13_p = ppp13/sum(ppp13)) %>%
  ungroup() %>% 
  # rowwise() %>% 
  mutate(mix13_p = (pop02_p + ppp13_p)/2)

# diff_pop <- diff_pop_raw %>% 
#   mutate(diff = abs((pop02_p - pop13_p)/pop02_p)*100,
#          diff_dynamic = abs((pop13_p - ppp13_p)/pop13_p)*100,
#          diff_static = abs((pop13_p - pop02_p)/pop13_p)*100,
#          diff_mix = abs((pop13_p - mix13_p)/pop13_p)*100) %>% 
#   arrange(diff)

diff_pop <- diff_pop_raw %>%
  mutate(diff = (pop13_p - pop02_p),
         diff_dynamic = (ppp13_p - pop13_p)/pop13_p,
         diff_static = (pop02_p - pop13_p)/pop13_p,
         diff_mix = (mix13_p - pop13_p)/pop13_p) %>%
  arrange(diff)

# diff_pop <- diff_pop_raw %>% 
#   mutate(diff = (pop13_p - pop02_p),
#          diff_dynamic = (ppp13_p - pop13_p),
#          diff_static = (pop02_p - pop13_p),
#          diff_mix = (mix13_p - pop13_p)) %>% 
#   arrange(diff)

diff_plot <- diff_pop %>%
  select(CODE, contains('diff')) %>% 
  pivot_longer(
    cols = starts_with("diff_"),
    names_to = "type",
    names_prefix = "diff_",
    values_to = "value",
    values_drop_na = TRUE,
  )

diff_summary <- diff_plot %>% 
  group_by(CODE, type) %>% 
  summarise(diff_mean = mean(diff)*100,
            value_mean = mean(value)*100,
            value_cilo = (mean(value) - 1.5*IQR(value))*100,
            value_ciup = (mean(value) + 1.5*IQR(value))*100) %>% 
  ungroup %>% 
  filter(type != 'mix')

# diff_plot %>%
#   ggplot(aes(x=reorder(CODE, diff), y=value)) +
#   # stat_boxplot(geom = "errorbar", width = 0.2) + 
#   geom_boxplot(aes(fill = type), outlier.shape = NA) +
# geom_ribbon(data = diff_summary, 
#             aes(x = reorder(CODE, diff_mean),
#                                     y = value_mean,
#                                     ymin = value_cilo,
#                                     ymax =value_ciup,
#                 colour = type,
#                 fill = type,
#                                     group = type),
#             alpha=0.1,
#             size = 0.1) +
# geom_line(data = diff_summary, aes(x = reorder(CODE, diff_mean),
#                                   y = value_mean,
#                                   colour = type,
#                                   group = type),
#           size = 1) +
# # scale_color_manual(name="", values=c("RGPHAE 2013 (+/- 1.5*IQR)"="#440154FF")) +
# # geom_abline(intercept = 0, slope = 0) +
#   scale_fill_viridis_d(alpha = 0.7, option = 'D') +
#   scale_colour_viridis_d(alpha = 0.7, option = "D") +
#   # ggtitle('Changes in population shares over time (by commune, in %, 2013 baseline)') +
#   labs(x = "Arrondissements ordered by absolute change in population share",
#        y = 'Deviation in %') +
#   # geom_rangeframe() +
#   theme_tufte() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position="none",
#         text =  element_text(size=20),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank())

png(file="./visualizations/sim_pop_diff.png",
    width=1000, height=500)

diff_summary %>%
  mutate(type = gsub("static", "fixed", type)) %>% 
  ggplot(aes(x=reorder(CODE, diff_mean), y=value_mean, group = type)) +
  # stat_boxplot(geom = "errorbar", width = 0.2) + 
  # geom_boxplot(aes(fill = type), outlier.shape = NA) +
  geom_ribbon(aes(ymin = value_cilo,
                  ymax =value_ciup,
                  colour = type,
                  fill = type),
              alpha=0.2,
              size = 0.1) +
  geom_line(aes(colour = type), size = 2) +
  # scale_color_manual(name="", values=c("RGPHAE 2013 (+/- 1.5*IQR)"="#440154FF")) +
  # geom_abline(intercept = 0, slope = 0) +
  scale_fill_viridis_d(alpha = 0.7, end = 0.5, option = 'D') +
  scale_colour_viridis_d(alpha = 0.7, end = 0.5, option = "D") +
  # ggtitle('Changes in population shares over time (by commune, in %, 2013 baseline)') +
  labs(x = "Arrondissements",
       y = 'Relative Bias (in %)',
       fill = '',
       color = '') +
  # geom_rangeframe() +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        # legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x = element_blank())

dev.off()


# diff_table <- pop_share_wp %>% 
#   mutate(diff = abs((pop02_p - pop13_p)/pop02_p)*100,
#          diff_dynamic = abs((pop13_p - ppp13_p)/pop13_p)*100,
#          pred_dyn_diff = abs((pop02_p - ppp13_p)/pop02_p)*100,
#          diff_static = abs((pop13_p - pop02_p)/pop13_p)*100,
#          diff_mix = abs((pop13_p - mix13_p)/pop13_p)*100,
#          pred_mix_diff = abs((pop02_p - mix13_p)/pop02_p)*100) %>% 
#   # group_by(CODE) %>%
#   # summarise(across(contains('diff'), mean)) %>%
#   # ungroup %>%
#   mutate(diff_q = cut(diff, breaks = quantile(diff, probs = seq(0, 1, 0.25)), include.lowest = T))

diff_table <- pop_share_wp %>% 
  mutate(diff = abs(pop13_p - pop02_p),
         diff_dynamic = (ppp13_p - pop13_p)/pop13_p,
         pred_dyn_diff = abs(ppp13_p - pop02_p),
         diff_static = (pop02_p - pop13_p)/pop13_p,
         diff_mix = (mix13_p - pop13_p)/pop13_p,
         pred_mix_diff = (mix13_p - pop02_p)/pop02_p,
         diff_diff = abs(diff_dynamic) - abs(diff_static)) %>% 
  mutate(diff_q = cut(diff, breaks = quantile(diff, probs = seq(0, 1, 0.25)), include.lowest = T))

diff_table %>% 
  group_by(diff_q) %>% 
  summarise(across(c(diff_dynamic, diff_static, diff_mix), ~mean(.)*100)) %>% 
  ungroup %>% 
  xtable()

# Alternatively, slope plot -----------------------------------------------

pop_slope <- pop_share_wp %>%
  filter(regid == '01') %>% 
  select(CODE, pop02_p, pop13_p, ppp13_p) %>% 
  pivot_longer(
    cols = ends_with("13_p"),
    names_to = "wt_type",
    names_pattern = "(.*)13_p",
    values_to = "wt",
    values_drop_na = TRUE
  ) %>% 
  mutate(pop02_p = pop02_p *100,
         wt = wt * 100,
         pop_growth = wt > pop02_p)

# True population

png(file="./visualizations/sim_slope_true.png",
    width=800, height=1000)

pop_slope %>%
  ggplot() +
  # add a line segment that goes from men to women for each discipline
  geom_segment(aes(x = 1, xend = 2, 
                   y = pop02_p, 
                   yend = wt,
                   group = CODE,
                   col = wt_type), 
               size = 1.2) +
  ylim(min(pop_slope$wt), max(pop_slope$wt) + 2) + 
  # set the colors
  scale_color_viridis_d(alpha = 0.5, end = 0.5, option = 'D', guide = "none")  +
  # remove all axis stuff
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_segment(x = 1, xend = 1, 
               y = min(pop_slope$wt) - 2,
               yend = max(pop_slope$wt) + 1,
               col = "grey70", size = 0.5) +
  # add vertical lines that act as axis for women
  geom_segment(x = 2, xend = 2, 
               y = min(pop_slope$wt) - 2,
               yend = max(pop_slope$wt) + 1,
               col = "grey70", size = 0.5) +
  # add the words "men" and "women" above their axes
  geom_text(aes(x = x, y = y, label = label),
            data = data.frame(x = 1:2, 
                              y = 2 + max(pop_slope$wt),
                              label = c("2002", "2013")),
            col = "grey30",
            size = 14) + 
  geom_text(aes(x = 1 - 0.03, 
                y = pop02_p, 
                label = paste0(round(pop02_p, 1), "%")),
            col = "grey30", hjust = "right", size = 9) +
  # add the success rate next to each point on the women axis
  geom_text(aes(x = 2 + 0.08, 
                y = wt, 
                label = paste0(round(wt, 1), "%")),
            col = "grey30", size = 9) +
  # set the limits of the x-axis so that the labels are not cut off
  scale_x_continuous(limits = c(0.8, 2.2)) +
  geom_point(aes(x = 1, 
                 y = pop02_p), size = 4.5,
             col = "white") +
  # add the white outline for the points at each rate for women
  geom_point(aes(x = 2, 
                 y = wt), size = 4.5,
             col = "white") +
  # add the actual points at each rate for men
  geom_point(aes(x = 1, 
                 y = pop02_p), size = 4,
             col = "grey60") +
  # add the actual points at each rate for men
  geom_point(aes(x = 2, 
                 y = wt), size = 4,
             col = "grey60") 

dev.off()

# Population estimates

png(file="./visualizations/sim_slope_ppp.png",
    width=800, height=1000)

pop_slope %>%
  filter(wt_type == 'ppp') %>%
  ggplot() +
  # add a line segment that goes from men to women for each discipline
  geom_segment(aes(x = 1, xend = 2, 
                   y = pop02_p, 
                   yend = wt,
                   group = CODE,
                   col = wt_type), 
               size = 1.2) +
  ylim(min(pop_slope$wt), max(pop_slope$wt) + 2) + 
  # set the colors
  scale_color_viridis_d(alpha = 0.5, begin = 0.5, option = 'D', guide = "none")  +
  # remove all axis stuff
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank()) +
  geom_segment(x = 1, xend = 1, 
               y = min(pop_slope$wt) - 2,
               yend = max(pop_slope$wt) + 1,
               col = "grey70", size = 0.5) +
  # add vertical lines that act as axis for women
  geom_segment(x = 2, xend = 2, 
               y = min(pop_slope$wt) - 2,
               yend = max(pop_slope$wt) + 1,
               col = "grey70", size = 0.5) +
  # add the words "men" and "women" above their axes
  geom_text(aes(x = x, y = y, label = label),
            data = data.frame(x = 1:2, 
                              y = 2 + max(pop_slope$wt),
                              label = c("2002", "2013")),
            col = "grey30",
            size = 14) + 
  geom_text(aes(x = 1 - 0.03, 
                y = pop02_p, 
                label = paste0(round(pop02_p, 1), "%")),
            col = "grey30", hjust = "right", size = 9) +
  # add the success rate next to each point on the women axis
  geom_text(aes(x = 2 + 0.08, 
                y = wt, 
                label = paste0(round(wt, 1), "%")),
            col = "grey30", size = 9) +
  # set the limits of the x-axis so that the labels are not cut off
  scale_x_continuous(limits = c(0.8, 2.2)) +
  geom_point(aes(x = 1, 
                 y = pop02_p), size = 4.5,
             col = "white") +
  # add the white outline for the points at each rate for women
  geom_point(aes(x = 2, 
                 y = wt), size = 4.5,
             col = "white") +
  # add the actual points at each rate for men
  geom_point(aes(x = 1, 
                 y = pop02_p), size = 4,
             col = "grey60") +
  # add the actual points at each rate for men
  geom_point(aes(x = 2, 
                 y = wt), size = 4,
             col = "grey60") 

dev.off()

# Population estimates 2013 -----------------------------------------------

# WP original
pop_com <- map_com@data %>% 
  select(REG, DEPT, geoid = CODE, COD_ENTITE) %>% 
  left_join(pop_proj, by = 'COD_ENTITE') %>% 
  left_join(ppp_com_old, by = c('COD_ENTITE' = 'geoid'))

png(file="./visualizations/sim_pop13_tvp_com.png",
    width=500, height=500)

pop_com %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[1], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(0, 800000) +
  ylim(0, 800000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()

png(file="./visualizations/sim_pop13_tvp_arr.png",
    width=500, height=500)

pop_com %>% 
  group_by(geoid) %>% 
  summarise_at(vars(proj_2013, sen_ppp_2013), sum) %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[2], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(9000, 900000) +
  ylim(9000, 900000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()

png(file="./visualizations/sim_pop13_tvp_dept.png",
    width=500, height=500)

pop_com %>% 
  group_by(DEPT) %>% 
  summarise_at(vars(proj_2013, sen_ppp_2013), sum) %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[3], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(20000, 1250000) +
  ylim(20000, 1250000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()

png(file="./visualizations/sim_pop13_tvp_reg.png",
    width=500, height=500)

pop_com %>% 
  group_by(REG) %>% 
  summarise_at(vars(proj_2013, sen_ppp_2013), sum) %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[4], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(150000, 3150000) +
  ylim(150000, 3150000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()


# WP recalculated
pop_com <- map_com@data %>% 
  select(REG, DEPT, geoid = CODE, COD_ENTITE) %>% 
  left_join(pop_proj, by = 'COD_ENTITE') %>% 
  left_join(ppp_com %>% group_by(geoid) %>% 
              summarise(across(sen_ppp_2013, mean)), by = c('COD_ENTITE' = 'geoid'))

png(file="./visualizations/sim_pop13_recalc_tvp_com.png",
    width=500, height=500)

pop_com %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[1], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(0, 800000) +
  ylim(0, 800000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()

png(file="./visualizations/sim_pop13_recalc_tvp_arr.png",
    width=500, height=500)

pop_com %>% 
  group_by(geoid) %>% 
  summarise_at(vars(proj_2013, sen_ppp_2013), sum) %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[2], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(9000, 900000) +
  ylim(9000, 900000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()

png(file="./visualizations/sim_pop13_recalc_tvp_dept.png",
    width=500, height=500)

pop_com %>% 
  group_by(DEPT) %>% 
  summarise_at(vars(proj_2013, sen_ppp_2013), sum) %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[3], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(20000, 1250000) +
  ylim(20000, 1250000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()

png(file="./visualizations/sim_pop13_recalc_tvp_reg.png",
    width=500, height=500)

pop_com %>% 
  group_by(REG) %>% 
  summarise_at(vars(proj_2013, sen_ppp_2013), sum) %>% 
  ggplot(aes(x = sen_ppp_2013, y = proj_2013)) +
  geom_point(colour = viridis(5, option = 'D')[4], size = 3, alpha = 1) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Predicted",
       y = 'Actual') +
  xlim(150000, 3150000) +
  ylim(150000, 3150000) +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=40),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

dev.off()


# Bias, RMSE, CI ----------------------------------------------------------

# Relative Bias (point)
output_df %>% 
  filter(status == 'deprived' | status == 'poor' | status == 'yes') %>%
  filter(ind == 'h_ind' & ipf_type == 'prop' & wt_type != 'mix') %>% 
  left_join(diff_table %>% 
              select(CODE, diff_q),
            by = 'CODE') %>% 
  mutate(emp_r_bias = (p_ipf - p_true)/p_true,
         wt_type = gsub("dynamic_si_bench", "dynamic", wt_type)) %>% 
  group_by(CODE, wt_type, diff_q) %>% 
  summarise(emp_r_bias = mean(emp_r_bias)) %>% 
  ungroup() %>% 
  group_by(diff_q, wt_type) %>% 
  summarise(across(emp_r_bias, list(q25 = ~quantile(., probs = 0.025)*100,
                               q250 = ~quantile(., probs = 0.25)*100,
                               median = ~median(.)*100,
                               mean = ~mean(.)*100, 
                               q750 = ~quantile(., probs = 0.75)*100,
                               q975 = ~quantile(., probs = 0.975)*100))) %>% 
  xtable(digits = 2)

# Relative RMSE (point)
output_df %>% 
  filter(status == 'deprived' | status == 'poor' | status == 'yes') %>%
  filter(ind == 'h_ind' & ipf_type == 'prop' & wt_type != 'mix') %>% 
  left_join(diff_table %>% 
              select(CODE, diff_q),
            by = 'CODE') %>% 
  mutate(emp_r_mse = ((p_ipf - p_true)/p_true)^2,
         wt_type = gsub("dynamic_si_bench", "dynamic", wt_type)) %>% 
  group_by(CODE, wt_type, diff_q) %>% 
  summarise(emp_r_mse = mean(emp_r_mse)) %>% 
  ungroup() %>% 
  mutate(emp_r_rmse = sqrt(emp_r_mse)) %>% 
  group_by(diff_q, wt_type) %>% 
  summarise(across(emp_r_rmse, list(q25 = ~quantile(., probs = 0.025)*100,
                                    q250 = ~quantile(., probs = 0.25)*100,
                                    median = ~median(.)*100,
                                    mean = ~mean(.)*100, 
                                    q750 = ~quantile(., probs = 0.75)*100,
                                    q975 = ~quantile(., probs = 0.975)*100))) %>% 
  xtable(digits = 2)

# Relative Bias (MSE)



# Relative RMSE (MSE)
output_df %>% 
  filter(status == 'deprived' | status == 'poor' | status == 'yes') %>%
  filter(ind == 'h_ind' & ipf_type == 'prop') %>% 
  left_join(diff_table %>% 
              select(CODE, diff_q),
            by = 'CODE') %>% 
  mutate(emp_r_mse = ((p_ipf - p_true)/p_true)^2) %>% 
  group_by(CODE, wt_type, diff_q) %>% 
  summarise(emp_r_mse = mean(emp_r_mse),
            est_r_mse = mean(mse)) %>% 
  ungroup() %>% 
  mutate(emp_r_rmse = sqrt(emp_r_mse),
         est_r_rmse = sqrt(est_r_mse),
         rb_est_rmse = (est_r_rmse - emp_r_rmse)/ emp_r_rmse) %>% 
  group_by(diff_q, wt_type) %>% 
  summarise(across(emp_r_rmse, list(q25 = ~quantile(., probs = 0.025)*100,
                                    q250 = ~quantile(., probs = 0.25)*100,
                                    median = ~median(.)*100,
                                    mean = ~mean(.)*100, 
                                    q750 = ~quantile(., probs = 0.75)*100,
                                    q975 = ~quantile(., probs = 0.975)*100))) %>% 
  xtable(digits = 2)


# Bar chart: Population of a region (DHS 2013, Updates, Census 2013)

# png(file="./visualizations/sim_bar_dakar.png",
#      width=1000, height=500)
# 
# eval_df %>% 
#   filter(regid == '01' & hh_head == 'female' & ind == 'h_ind') %>% 
#   group_by(wt_type) %>% 
#   summarise(across(c(offset, dhs, ipf, true), mean)) %>% 
#   pivot_longer(
#     cols = offset:true,
#     names_to = "model",
#     values_to = "values"
#   ) %>% 
#   unite('data_source', c('wt_type','model')) %>% 
#   filter(data_source == 'static_offset' |
#            data_source == 'static_dhs' |
#            data_source == 'static_ipf' |
#            data_source == 'dynamic_si_bench_ipf' |
#            data_source == 'static_true'
#   ) %>% 
#   mutate(data_source = factor(data_source,
#                               levels = c('static_offset', 
#                                          'static_dhs', 
#                                          'static_ipf', 'dynamic_si_bench_ipf',
#                                          'static_true'),
#                               labels = c('RGPH\n2002', 
#                                          'DHS\n2013',
#                                          'IPF\nPM', 'IPF\nSI',
#                                          'RGPHAE\n2013'))) %>% 
#   ggplot() +
#   geom_bar(aes(x = data_source, y = values, fill = data_source),
#            stat = "identity", position = "dodge") +
#   scale_y_continuous("Headcount ratio", expand = c(0, 0)) +
#   scale_x_discrete("Data source") +
#   scale_fill_viridis_d(alpha = 0.7, option = 'D') +
#   theme_tufte() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position="none",
#         text =  element_text(size=20),
#         axis.ticks = element_blank())
# 
# dev.off()

png(file="./visualizations/sim_box_dakar.png",
     width=1000, height=500)

eval_df %>% 
  filter(regid == '01' & hh_head == 'female' & ind == 'h_ind') %>% 
  select(wt_type, offset, dhs, ipf, true) %>% 
  pivot_longer(
    cols = offset:true,
    names_to = "model",
    values_to = "values"
  ) %>% 
  unite('data_source', c('wt_type','model')) %>% 
  filter(data_source == 'static_offset' |
           data_source == 'static_dhs' |
           data_source == 'static_ipf' |
           data_source == 'dynamic_si_bench_ipf' |
           data_source == 'static_true'
  ) %>% 
  mutate(data_source = factor(data_source,
                              levels = c('static_offset', 
                                         'static_dhs', 
                                         'static_ipf', 'dynamic_si_bench_ipf',
                                         'static_true'),
                              labels = c('RGPH\n2002', 
                                         'DHS\n2013',
                                         'IPF\nPM', 'IPF\nSI',
                                         'RGPHAE\n2013'))) %>% 
  ggplot(aes(x = data_source, y = values, fill = data_source)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot() +
  labs(x = "Data source",
       y = 'Headcount ratio') + 
  scale_fill_viridis_d(alpha = 0.7, option = 'D') +
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",
        text =  element_text(size=20),
        axis.ticks = element_blank())

dev.off()


# Box plot all arrondissements + true census ------------------------------

png(file="./visualizations/sim_box_all_ipf.png",
     width=1000, height=250)


# box_all_arr <- deprived_df %>% 
#   filter(ind == 'h_ind' & wt_type == 'dynamic_si_bench' & hh_head == 'female') %>% 
#   select(CODE, y = ipf, true)

box_all_arr <- output_df %>%
  filter(status == 'deprived' | status == 'poor' | status == 'yes') %>%
  filter(ind == 'h_ind' & 
           hh_head == 'female' & 
           ipf_type == 'prop' & 
           wt_type != 'benchmark') %>% 
  select(CODE, wt_type, y = p_ipf, offset, true = p_true, run) %>% 
  left_join(diff_table %>% 
              select(CODE, diff),
            by = 'CODE')

# %>% 
#   mutate(pop_change = abs((true - offset)/offset), #growth rate
#          y = abs((true - y)/true)) #(mean) absolute percentage deviation

# %>% 
#   group_by(CODE, run) %>% 
#   mutate(pop_change = log(pop_change),
#          y = log(y)) %>% 
  # ungroup()

box_summary <- box_all_arr %>% 
  group_by(CODE, diff, wt_type) %>% 
  summarise(true_mean = mean(true),
            true_cilo = mean(true) - 1.5*IQR(true), #mean(true) - 1.5*IQR(true)
            true_ciup = mean(true) + 1.5*IQR(true)) %>% 
  ungroup()


# Sort by pop size
box_all_arr %>% 
  ggplot(aes(x = reorder(CODE, true), y = y)) +
  # stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(aes(fill = wt_type), outlier.alpha = 0.1, outlier.size = 0.2) +
  scale_fill_viridis_d(begin = 0.33, alpha = 0.7, option = 'D', name = ""
                       , breaks = c('dynamic_si_bench', 'static', 'benchmark')
                       , labels = c('dynamic', 'static', 'benchmark')) +
geom_ribbon(data = box_summary, aes(x = reorder(CODE, true_mean),
                                    y = true_mean,
                                    ymin = true_cilo,
                                    ymax =true_ciup,
                                    group = 1),
            color = "#440154FF",
            fill = "#440154FF",
            alpha=0.1,
            size = 0.1) +
geom_line(data = box_summary, aes(x = reorder(CODE, true_mean),
                                  y = true_mean,
                                  group = 1,
                                  color="RGPHAE 2013 (+/- 1.5*IQR)"),
          size = 1) +
scale_color_manual(name="", values=c("RGPHAE 2013 (+/- 1.5*IQR)"="#440154FF")) +
labs(x = "Arrondissements",
     y = 'Headcount') + 
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        text =  element_text(size=20),
        axis.ticks = element_blank(),
        axis.text.x=element_blank())

  dev.off()  


# Plot percentage RMSE ----------------------------------------------------

png(file="./visualizations/sim_box_cor.png",
    width=1000, height=300)

output_df %>% 
  left_join(diff_table %>% 
              select(CODE, diff, diff_q),
            by = 'CODE') %>% 
  filter(ipf_type == 'prop', ind == 'h_ind', status == 'poor', wt_type != 'mix') %>% 
  mutate(diff_q = fct_recode(diff_q, 
                             "Lowest" = "[-0.0439,-0.0118]",
                             "2nd" = "(-0.0118,-7.17e-05]",
                             "3rd" = "(-7.17e-05,0.00993]",
                             "Highest" = "(0.00993,0.064]")) %>% 
  group_by(diff_q, wt_type, run) %>% 
  summarise(corr = cor(p_ipf, p_true)) %>% 
  ggplot(aes(x = diff_q, y = corr)) +
  geom_boxplot(aes(fill = wt_type), outlier.alpha = 0.3, outlier.size = 0.5) +
  scale_fill_viridis_d(end = 0.5, alpha = 0.7, option = 'D', name = "Proportion"
                       , breaks = c('dynamic_si_bench', 'static')
                       , labels = c('dynamic', 'fixed')) +
  labs(x = "Quartile",
       y = 'Pearson correlation coefficient') + 
  theme_tufte() +
  theme(plot.title = element_text(hjust = 0.5),
        text =  element_text(size=20),
        axis.ticks = element_blank())

dev.off()
