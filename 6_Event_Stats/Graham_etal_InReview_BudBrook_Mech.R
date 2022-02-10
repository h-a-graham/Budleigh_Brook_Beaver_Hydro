set.seed(123)
options(scipen = 999)

# ------------- Load Libraries ----------------------
library(tidyverse)
library(lubridate)
library(broom)
library(grid)
library(gridExtra)
library(performance)
library(glm2)
# library(ggpubr)
library(gt)
library(png)
library(gtable)
library(patchwork)
library(ggpattern)
library(GGally)

# import functions
source('6_Event_Stats/stats_functions.R')
source('6_Event_Stats/add_general_facet_labs.R')
source('6_Event_Stats/mechanisms_paper_functions.R')
source('6_Event_Stats/fdc_functions.R')

# check if table export folder exists.
tab_dir <- "6_Event_Stats/Model_Compare_Tabs"
if (!dir.exists(tab_dir)) dir.create(tab_dir)

# ------------- Read Data ------------------------------
EBUD_hyd_dat1 <- read_rds(file.path(here::here(),"5_event_extraction/eventExtraction__beaver/EastBud/",
                                    "run_20200619_1022_value__padding2880_alpha0.98_passes3_BFI0.814/eventEx_EVENTS_metrics.rds"))

EBUD_all_Q <- read_rds(file.path(here::here(),'4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds'))  # reload flow inout for calculating correct Excedence...
EBUD_all_flow <- EBUD_all_Q %>%
  select(q) %>%
  drop_na()

POP_hyd_dat1 <- read_rds(file.path(here::here(), "5_event_extraction/eventExtraction__beaver/Pophams/",
                                   "run_20200710_1222_value__padding2880_alpha0.98_passes3_BFI0.74/eventEx_EVENTS_metrics.rds"))

POP_all_Q <- read_rds(file.path(here::here(),'4_Join_Rain_to_Q/exports/Pophams_Q_R_S_ts.rds'))  # reload flow inout for calculating correct Excedence...
POP_all_flow <- POP_all_Q %>%
  select(q) %>%
  drop_na()


EBUD_hyd_dat <- CleanData(EBUD_hyd_dat1)
POP_hyd_dat <- CleanData(POP_hyd_dat1)

# ------ Add Exceedance limits ------------------------

EBUD_perc_flow<- ecdf(EBUD_all_flow$q) # calculate Empirical Cumulative Distribution for original Q data
POP_perc_flow<- ecdf(POP_all_flow$q)


EBUD_hyd_dat <- EBUD_hyd_dat %>%
  mutate(per_q = (1 - EBUD_perc_flow(Q.peak.m3.s)) * 100)

POP_hyd_dat <- POP_hyd_dat %>%
  mutate(per_q = (1 - POP_perc_flow(Q.peak.m3.s)) * 100)

# -------- Join Data -----------------------------
BB_events <- EBUD_hyd_dat %>%
  mutate(Site='Budleigh Brook (impact)') %>%
  select(rain.tot.mm, Q.peak.m3.s, rain.peak.ts, Q.peak.ts, anti.rain.mm5d, 
         per_q, Beaver, Hydro.Seas, Site, rain.mean, event.Q.tot.m3, 
         event.quickflow.tot.m3,anti.rain.mm5d)

CB_evetns <- POP_hyd_dat %>%
  mutate(Site='Colaton Brook (control)')%>%
  select(rain.tot.mm, Q.peak.m3.s, rain.peak.ts, Q.peak.ts, anti.rain.mm5d, 
         per_q, Beaver, Hydro.Seas, Site,rain.mean, event.Q.tot.m3,
         event.quickflow.tot.m3,anti.rain.mm5d)


BB.CB.bind <- BB_events %>%
  bind_rows(CB_evetns) %>%
  mutate(Site = fct_relevel(Site, "Colaton Brook (control)", "Budleigh Brook (impact)"),
         Beaver = case_when(Beaver == 'Yes' ~ "Present",
                                     TRUE ~ "Absent"))

# Pearson's r for paper.

ggpairs(EBUD_hyd_dat, columns = c("Q.peak.m3.s","rain.tot.mm", "rain.mean",
                                  "event.quickflow.tot.m3", "per_q", "event.Q.tot.m3",
                                  "anti.rain.mm5d"))
ggpairs(POP_hyd_dat, columns = c("Q.peak.m3.s","rain.tot.mm", "rain.mean",
                                 "event.quickflow.tot.m3", "per_q", "event.Q.tot.m3",
                                 "anti.rain.mm5d"))

# ------------------- Fit Gamma General Linear Models (GLM) -------------------

# ggplot(BB.CB.bind, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver)) +
#   geom_point() +
#   stat_smooth(method='lm', formula = y~poly(x,2)) +
#   # coord_cartesian(xlim=c(0, 105e3), ylim=c(0,5))+
#   facet_wrap(~Site) +
#   theme_bw()


# Fit regression models

# Original Additive model
BACI_m1.init <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site , 
                     data= BB.CB.bind, family = Gamma(link='identity')) # prelim run to get starting vals
BACI_m1 <- glm2(Q.peak.m3.s ~  rain.tot.mm + Beaver * Site , 
                data= BB.CB.bind, family = Gamma(link='identity'), 
                start = coef(BACI_m1.init)) # final model

# Model with interaction effect
BACI_m2.init <- glm2(Q.peak.m3.s ~ rain.tot.mm * Beaver * Site , 
                     data= BB.CB.bind, family = Gamma(link='identity')) # prelim run to get starting vals
BACI_m2 <- glm2(Q.peak.m3.s ~  rain.tot.mm * Beaver * Site , 
                data= BB.CB.bind, family = Gamma(link='identity'), 
                start = coef(BACI_m2.init)) # final model

# Log link interaction
BACI_m3 <- glm(Q.peak.m3.s ~ rain.tot.mm * Beaver * Site , data= BB.CB.bind, 
               family = Gamma(link='log'))

# Polynomial Model with Interaction
BACI_m4 <- glm(Q.peak.m3.s ~ poly(rain.tot.mm,2) * Beaver * Site , 
               data= BB.CB.bind, family = Gamma(link='identity'))

# Polynomial (log-link) Model with Interaction
BACI_m5 <- glm(Q.peak.m3.s ~ poly(rain.tot.mm,2) * Beaver * Site, 
               data= BB.CB.bind, family = Gamma(link='log'))


summary(BACI_m1) # Crucially, interaction between site and BEaver is significant and negative i.e beaver effect is stat sig. and reduced and impacted site
summary(BACI_m2)
summary(BACI_m3) # Lowest AIC
summary(BACI_m4) 
summary(BACI_m5) 

reg_names_full <- c('Linear Additive (Identity-link)', 
                    'Linear Interactive (Identity-link)', 
                    'Linear Interactive (log-link)', 
                    '2nd Order Polynomial Interactive (Identity-link)', 
                    '2nd Order Polynomial Interactive (log-link)')

reg_names <- c('M1 Linear Add.', 'M2 Linear Int.', 'M3 Linear Int. (log-link)', 
               'M4 Poly. Int.', 'M5 Poly. Int. (log-link)')

model_list <- list(BACI_m1, BACI_m2, BACI_m3,BACI_m4, BACI_m5)

# plot model diagnostics.
list(model_list, seq_along(model_list), reg_names_full) %>%
  purrr::pwalk(., ~check_model_title(..1, ..2, ..3, .size = 25))
 
# create model predictions for all models.
merge_model_preds <- model_list %>%
  purrr::map2(.x=., .y=reg_names, ~makes_preds(.x, .y, BB.CB.bind)) %>%
  bind_rows() %>%
  mutate(mod.name = fct_relevel(mod.name,reg_names))

# ----- Create GLM Plots ------------------
BACI.glmLOG <- model_compare_plot(BB.CB.bind, .log=TRUE)
BACI.glmLIN <- model_compare_plot(BB.CB.bind, .log=FALSE)

ggsave("6_Event_Stats/BACI_Plots/Model_CompareLOG2.png", plot = BACI.glmLOG ,
       width = 18, height = 18, units = 'cm', dpi = 600)
ggsave("6_Event_Stats/BACI_Plots/Model_CompareLIN.png", plot = BACI.glmLIN ,
       width = 18, height = 18, units = 'cm', dpi = 600)

# ------- Create Model Summary tables -------
p <- model_list %>%
  purrr::map2(.x=., .y=reg_names, ~add.stat.tab(.) %>%
                pretty.tab(., .y))

p %>%
  purrr::walk2(.x=., .y=purrr::imap(reg_names_full, 
                                     ~paste0(sprintf('Model%s_',.y), .x, '.html')),
               ~ .x %>% gtsave(.,file.path(tab_dir, .y)))


# ------------- Create Model Performance Table -----------------
model_list %>% 
  purrr::map2_dfr(., reg_names, 
                  ~ tibble(model_performance(.x) %>%
                             mutate(`Model ID` = .y, 
                                    K = with(summary(.x),nrow(coefficients))
                                    )))%>%
  mutate_if(is.numeric, round, 2) %>%
  relocate(`Model ID`, K) %>%
  rename(`R2 Nagelkerke` = R2_Nagelkerke) %>%
  gt() %>%
  tab_header(
    title = md(sprintf('**<div style="text-align: left"> Model Performance </div>**', title))) %>%
  gtsave(.,file.path(tab_dir, 'GoodnessOfFit.html'))


# ----- Predicted Attenuation volume vs rainfall. ---------------

merge_tidy_preds <- makes_preds(BACI_m5, 'Polynomial', 
                                BB.CB.bind, NoBeav.min=0, 
                                NoBeav.max=60,  YesBeav.min=0, 
                                YesBeav.max=60, l=500) %>% 
  bind_rows()


# Get summary stats for flow attenuation range etc.
rain_ecdf <- ecdf(BB.CB.bind$rain.tot.mm)

t <- merge_tidy_preds %>%
  group_by(Site) %>%
  group_map(., ~pred_diff(.), .keep=T) %>%
  bind_rows() %>%
  mutate(.name = '95% confidence range',
         rain_event_perc = rain_ecdf(rain.tot.mm...3)*100)

att_region <- t %>%
  filter(Site...5=='Budleigh Brook (impact)',
         U.ci > 0) %>%
  mutate(.name='Attenuation Effect')


Attenuation_p_PERC <- Attenuation_plot(t, rain_event_perc) +
  coord_cartesian(ylim=c(-0.1,3)) +
  labs(x='Event Total Rainfall (percentile)')
Attenuation_p_PERC

Attenuation_p_RAIN <- Attenuation_plot(t, rain.tot.mm...3) +
  coord_cartesian(ylim=c(-0.1,0.6), xlim=c(0,60))+
  labs(x=expression("Total Event Rainfall   " (mm)))
Attenuation_p_RAIN

ggsave("6_Event_Stats/BACI_Plots/AttenuationPlot.png", plot = Attenuation_p_RAIN ,width = 15, height = 9, units = 'cm', dpi = 300)
ggsave("6_Event_Stats/BACI_Plots/AttenuationPlotPERCENTILE.png", plot = Attenuation_p_PERC ,width = 15, height = 9, units = 'cm', dpi = 300)


# get some stats from predictions for paper.
summ_tab <- t %>%
  filter(Site...5=='Budleigh Brook (impact)',
         U.ci > 0) %>%
  summarise(max_event = max(rain.tot.mm...3),
            peak_U.Lci = max(U.ci),
            peak_event = rain.tot.mm...3[U.ci==max(U.ci)],
            peak_U.Uci= L.ci[U.ci==max(U.ci)],
            peak_zero = L.ci[rain.tot.mm...3==max_event],
            low_zero = U.ci[rain.tot.mm...3==max_event],
            meanQMAX = .fitted...2[rain.tot.mm...3==peak_event],
            lowQMAX = meanQMAX-.se.fit...4[rain.tot.mm...3==peak_event]*1.96,
            highQMAX = meanQMAX+.se.fit...4[rain.tot.mm...3==peak_event]*1.96) %>%
  mutate(peak_percentile = rain_ecdf(peak_event),
         max_percentile = rain_ecdf(max_event),
         prop_atten_low = peak_U.Uci/highQMAX*100,
         prop_atten_high = peak_U.Lci/lowQMAX*100) %>% 
  slice(1L) %>%
  select('low_zero','peak_zero','peak_U.Uci', 'peak_U.Lci', 'peak_event', 
         'peak_percentile', 'max_event', 'max_percentile', 'meanQMAX', 'lowQMAX',
         'highQMAX', 'prop_atten_low', 'prop_atten_high')

select(summ_tab, prop_atten_low, prop_atten_high) # get attenuation as percentage of peak flow preds
# hist(BB.CB.bind$rain.tot.mm)


# ---------------- GLM with hydrological season -------------------------------------------
# **** Not included in paper - but here for comparison with 
# Puttock, et al (2021). Doesn't really tell us anything new particularly *****

#  Fit Regression 

BACI_m6 <- glm(Q.peak.m3.s ~ poly(rain.tot.mm,2) * Hydro.Seas * Site * Beaver, 
               data= BB.CB.bind, family = Gamma(link='log')) #  prelim model run for start values

summary(BACI_m6)

check_model(BACI_m6)


BACI.m2.ND <- Create_Data(.data=BB.CB.bind, var='rain.tot.mm') %>%
  broom::augment(BACI_m6, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

#plotting

BACI.glm2 <- glm.plot(BB.CB.bind, BACI.m2.ND, .y=Q.peak.m3.s) + 
  facet_grid(Hydro.Seas ~ Site) + 
  scale_y_continuous(breaks = c(0,4, 10, 20),
                       trans=scales::pseudo_log_trans(base = 10)) +
  coord_trans(ylim=c(0, 20))
BACI.glm2.all <- add_general_facet_labs(BACI.glm2, 'Season', 'Site')

add.stat.tab(BACI_m6) %>%
  pretty.tab(., 'Seasonal Polynomial Interactive (log-link)') %>%
  gtsave(file.path(tab_dir, 'SeasPolyInt(log-link).html'))

ggsave("6_Event_Stats/BACI_Plots/PolyIntSeasonLOG.png", plot = BACI.glm2.all ,width = 15, height = 12, units = 'cm', dpi = 600)


# --- raincloud plot to show difference in Q max across beaver time and site ---

fb1 <- flowbox(BB.CB.bind, expression("Event Maximum Flow   " (m^{3}~s^{-1})))+
  scale_y_continuous(breaks = c(0.03,1,4, 10, 20),
                     trans='log')
fb1

print_sum_stats <- function(.data){
  message(.data$Site[1])
  message(.data$Beaver[1])
  message(median(.data$Q.peak.m3.s, na.rm=T))
  message(IQR(.data$Q.peak.m3.s, na.rm=T))
  
}

BB.CB.bind %>%
  group_by(Site, Beaver) %>%
  group_split() %>%
  purrr::walk(., ~print_sum_stats(.))

ggsave("6_Event_Stats/BACI_plots/Fig1.QMax_BoxplotV3.png", plot = fb1,width = 15, height = 15, units = 'cm', dpi = 600)


# ------ Flow duration Curve Analysis ---------------


EB_full_flow <- Read.Edit.Flow(EBUD_all_Q) %>%
  mutate(Site = 'Budleigh Brook (impact)')
POP_full_flow <- Read.Edit.Flow(POP_all_Q) %>%
  mutate(Site = 'Colaton Brook (control)')



EB_FlowSumTab <- Flow.Sum.Tab(EB_full_flow)
POP_FlowSumTab <- Flow.Sum.Tab(POP_full_flow)

FDC_summBind <- bind_rows(Flow.Sum.Tab(EB_full_flow),
                          Flow.Sum.Tab(POP_full_flow)) %>%
  group_by(Site) %>%
  mutate_if(is.numeric, ~round(.,2))

pretty.tab(FDC_summBind, 'Flow Duration Curve Metrics') %>%
  gtsave(file.path(tab_dir, 'Flow_Duration_Metrics.html'))

# ----------------- flow duration curves ------------------------

Joint.Flow <- EB_full_flow %>%
  calc.fdc(.)%>%
  bind_rows(calc.fdc(POP_full_flow)) %>%
  mutate(Site = fct_relevel(Site, "Colaton Brook (control)", "Budleigh Brook (impact)")) 


fdc.plots <- plot.fdc(Joint.Flow)
fdc.plots

ggsave(file.path(here::here(),"6_Event_Stats/BACI_Plots/Fig6.FlowDurCurve.png"), plot = fdc.plots, width = 18, height = 15, units = 'cm', dpi = 600)







