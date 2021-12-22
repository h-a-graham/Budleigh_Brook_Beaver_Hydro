set.seed(123)
options(scipen = 999)

# ------------- Load Libraries ----------------------
library(tidyverse)
library(lubridate)
library(broom)
library(ggfortify)
library(grid)
library(gridExtra)
library(performance)
library(glm2)
library(ggpubr)
library(emmeans)
library(gt)
library(png)
library(gtable)
# ------------- Read Data ------------------------------
EBUD_hyd_dat1 <- read_rds(file.path(here::here(),"5_event_extraction/eventExtraction__beaver/EastBud/",
                                    "run_20200619_1022_value__padding2880_alpha0.98_passes3_BFI0.814/eventEx_EVENTS_metrics.rds"))

EBUD_all_flow <- read_rds(file.path(here::here(),'4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds')) %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()

POP_hyd_dat1 <- read_rds(file.path(here::here(), "5_event_extraction/eventExtraction__beaver/Pophams/",
                                   "run_20200710_1222_value__padding2880_alpha0.98_passes3_BFI0.74/eventEx_EVENTS_metrics.rds"))

POP_all_flow <- read_rds(file.path(here::here(),'4_Join_Rain_to_Q/exports/Pophams_Q_R_S_ts.rds')) %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()




# -------------- Functions ------------------------------

# Create New Data for predictions

Create_Data <- function(.data, vari, NoBeav.min, NoBeav.max, YesBeav.min, YesBeav.max, ...){
  if(missing(NoBeav.min)) {
    NoBeav.min <- .data %>% filter(Beaver == 'No') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% min()}
  if(missing(NoBeav.max)) { 
    NoBeav.max <- .data %>% filter(Beaver == 'No') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% max()}
  if(missing(YesBeav.min)) {
    YesBeav.min <- .data %>% filter(Beaver == 'Yes') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% min()}
  if(missing(YesBeav.max)) {
    YesBeav.max <- .data %>% filter(Beaver == 'Yes') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% max()}
  
  new_NoBeavWet.BB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=100), Beaver='No', Hydro.Seas = 'Wet', Site = 'Budleigh Brook (impact)')
  new_NoBeavDry.BB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=100), Beaver='No', Hydro.Seas = 'Dry', Site = 'Budleigh Brook (impact)')
  new_NoBeavWet.CB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=100), Beaver='No', Hydro.Seas = 'Wet', Site = 'Colaton Brook (control)')
  new_NoBeavDry.CB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=100), Beaver='No', Hydro.Seas = 'Dry', Site = 'Colaton Brook (control)')
  new_YesBeavWet.BB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=100), Beaver = 'Yes', Hydro.Seas = 'Wet', Site = 'Budleigh Brook (impact)')
  new_YesBeavDry.BB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=100), Beaver = 'Yes', Hydro.Seas = 'Dry', Site = 'Budleigh Brook (impact)')
  new_YesBeavWet.CB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=100), Beaver = 'Yes', Hydro.Seas = 'Wet', Site = 'Colaton Brook (control)')
  new_YesBeavDry.CB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=100), Beaver = 'Yes', Hydro.Seas = 'Dry', Site = 'Colaton Brook (control)')
  
  df <- bind_rows(new_NoBeavWet.BB, new_NoBeavDry.BB, new_NoBeavWet.CB, new_NoBeavDry.CB, 
                  new_YesBeavWet.BB, new_YesBeavDry.BB, new_YesBeavWet.CB, new_YesBeavDry.CB)%>%
    mutate(!!vari := newvar) %>%
    select(-newvar)
  return(df)
  
}

# Create New data for predictions
makes_preds <- function(.model, .name, df, ...) {
  Create_Data(.data=df, var='rain.tot.mm', ...) %>%
    broom::augment(.model, type.predict = "response",
                   type.residuals = "deviance",
                   se_fit = T, newdata=.) %>%
    mutate(mod.name = .name)
}

#-------- functions for model plotting and tables --------
# Function to Create Tidy table of marginal means

# plotting...
glm.plot <- function(.data, model.data, .y) {
  .y <- enquo(.y)
  ggplot(model.data, aes(x=rain.tot.mm, y=!!.y, colour=Beaver, fill=Beaver))+
    geom_point(data=.data, alpha = 0.4, size=0.8)+
    geom_line(aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    # coord_cartesian(ylim=c(0,7))+
    labs(y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
         x=expression("Total Event Rainfall   " (mm)),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          legend.position = 'top')
}


pretty.tab <- function(model.tab, title){
  model.tab %>%
    gt() %>%
    tab_header(
      title = md(sprintf('**<div style="text-align: left"> %s </div>**', title))) #%>% 
  # gtsave(.,tempfile('tab', fileext = '.html')) #%>%
  # readPNG(.) %>%
  # rasterGrob( interpolate=TRUE, width = unit(size.cm,"cm"))
}

# ------------- Clean Data -----------------------------

CleanData <- function(.data){
  .data %>%
    mutate(Beaver = as.factor(ifelse(event.start.ts > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                     ifelse(event.start.ts > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                              event.start.ts < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No"))))%>%
    filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>% # removes period of uncertainty where beaver's presence unknown.
    mutate(Beaver = fct_relevel(Beaver, "No", "Yes")) %>%
    mutate(rain.rate = rain.tot.mm/(rain.dur@.Data/3600)) %>%
    mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
    mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
    mutate(Month = (month(event.start.ts))) %>%
    mutate(Year = as.factor(year(event.start.ts))) %>%
    mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
    mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
    mutate(Season = (ifelse(Month >= 3 & Month < 6, "Spring",
                            (ifelse(Month >=6 & Month <9, "Summer",
                                    (ifelse(Month >=9 & Month <12, "Autumn",
                                            (ifelse(Month >=12 | Month <3, "Winter", ""))))))))) %>%
    mutate(Hydro.Seas = ifelse(Month >= 10 | Month < 4, "Wet", "Dry")) %>%
    mutate(Season = fct_relevel(Season, "Winter","Spring", "Summer", "Autumn"))%>%
    mutate(Hydro.Seas = fct_relevel(Hydro.Seas, "Dry", "Wet")) %>%
    select(-c(response.eventID,check.outlier1)) %>%
    drop_na()
}

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
         event.quickflow.tot.m3)

CB_evetns <- POP_hyd_dat %>%
  mutate(Site='Colaton Brook (control)')%>%
  select(rain.tot.mm, Q.peak.m3.s, rain.peak.ts, Q.peak.ts, anti.rain.mm5d, 
         per_q, Beaver, Hydro.Seas, Site,rain.mean, event.Q.tot.m3,
         event.quickflow.tot.m3)


BB.CB.bind <- BB_events %>%
  bind_rows(CB_evetns) %>%
  mutate(Site = fct_relevel(Site, "Colaton Brook (control)", "Budleigh Brook (impact)"))

# Pearson's r for paper.
# library(GGally)
# ggpairs(EBUD_hyd_dat, columns = c("Q.peak.m3.s","rain.tot.mm", "rain.mean", 
#                                   "event.quickflow.tot.m3", "per_q", "event.Q.tot.m3"))
# ggpairs(POP_hyd_dat, columns = c("Q.peak.m3.s","rain.tot.mm", "rain.mean", 
#                                  "event.quickflow.tot.m3", "per_q", "event.Q.tot.m3"))

# ----------------------- Fit Gamma (identity) Models --------------------------------

ggplot(BB.CB.bind, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver)) +
  geom_point() +
  stat_smooth(method='lm', formula = y~poly(x,2)) +
  # coord_cartesian(xlim=c(0, 105e3), ylim=c(0,5))+
  facet_wrap(~Site) +
  theme_bw()


# Fit regression

# Original Additive model
BACI_m1.init <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity')) # prelim run to get starting vals

BACI_m1 <- glm2(Q.peak.m3.s ~  rain.tot.mm + Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity'), start = coef(BACI_m1.init)) # final model

# Model with interaction effect
BACI_m2.init <- glm2(Q.peak.m3.s ~ rain.tot.mm * Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity')) # prelim run to get starting vals

BACI_m2 <- glm2(Q.peak.m3.s ~  rain.tot.mm * Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity'), start = coef(BACI_m2.init)) # final model

# Polynomial Model with Interaction

BACI_m3 <- glm(Q.peak.m3.s ~ poly(rain.tot.mm,2) * Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity'))
# BACI_m3 <- glm2(Q.peak.m3.s ~ poly(rain.mean,2) + Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity'), start = coef(BACI_m3))

summary(BACI_m1) # Crucially, interaction between site and BEaver is significant and negative i.e beaver effect is stat sig. and reduced and impacted site
summary(BACI_m2)
summary(BACI_m3) # Lowest AIC
#interactive test

check_model(BACI_m1, check='all') 
check_model(BACI_m2, check='all') 
check_model(BACI_m3, check='all') 


reg_names <- c('Linear Additive', 'Linear Interaction', 'Polynomial Interaction')

merge_model_preds <- list(BACI_m1, BACI_m2, BACI_m3) %>%
  purrr::map2(.x=., .y=reg_names, ~makes_preds(.x, .y, BB.CB.bind)) %>%
  bind_rows()


source('6_Event_Stats/add_general_facet_labs.R')

BACI.glm1 <- glm.plot(BB.CB.bind, merge_model_preds, .y=Q.peak.m3.s) +
  facet_grid(mod.name~Site) +
  coord_cartesian(ylim=c(0,6))#+
  # scale_y_continuous(trans='log2')
   
BACI.glm1.all <- add_general_facet_labs(BACI.glm1, 'Model', 'Site')

source('6_Event_Stats/stats_functions.R')
reg_tabs <- list(BACI_m1, BACI_m2, BACI_m3)

p <- reg_tabs %>%
  purrr::map2(.x=., .y=reg_names, ~add.stat.tab(.) %>%
                pretty.tab(., .y))

# check if folder exists.
tab_dir <- "6_Event_Stats/Model_Compare_Tabs"
if (!dir.exists(tab_dir)) dir.create(tab_dir)


p %>%
  purrr::walk2(.x=., .y=c('Model1_Lin_Add.html', 'Model2_LinInt.html', 'Model3_PolyInt.html'),
               ~ .x %>% gtsave(.,file.path(tab_dir, .y)))


ggsave("6_Event_Stats/BACI_Plots/Model_Compare.png", plot = BACI.glm1.all ,width = 15, height = 15, units = 'cm', dpi = 600)
# ggsave("6_Event_Stats/BACI_Plots/Fig2.GLM1.pdf", plot = BACI.glm1.all ,width = 15, height = 15, units = 'cm', dpi = 900)

# pred differences:

merge_tidy_preds <- makes_preds(BACI_m3, 'Polynomial', BB.CB.bind, 
                                               NoBeav.min=0, NoBeav.max=60, 
                                               YesBeav.min=0, YesBeav.max=60) %>%
  bind_rows()


pred_diff <- function(df){
  pred.diff <- df %>%
    select(Beaver, .fitted,rain.tot.mm) %>%
    group_by(Beaver) %>%
    group_split() %>%
    bind_cols() #%>%
    # mutate(.diff = .fitted...2 - .fitted...4) %>%
    # pull(.diff)
  pred.diff
  # df3  <- df %>%
  #   group_by(Beaver) %>%
  #   group_split()
  # 
  # df3[[1]] %>%
  #   select(-Beaver) %>%
  #   mutate(preds_diff = pred.diff)
  
}


t <- merge_tidy_preds %>%
  
  group_by(Site) %>%
  group_split()
  group_map(., )
  pivot_longer(names_from = Beaver, values_from = .fitted) %>%
  tail()

t2 <- pred_diff(t[[2]]) %>%
  mutate(.diff = .fitted...2 - .fitted...5)
  
ggplot(t2, aes(x=rain.tot.mm...6, y=.diff))+
  geom_line() +
  theme_bw() +
  facet_grid(mod.name~Site)

# ---------------- GLM with hydrological season -------------------------------------------

#  Fit Regression 

BACI_m4 <- glm(Q.peak.m3.s ~ poly(rain.tot.mm,2) * Beaver * Site * Hydro.Seas, 
               data= BB.CB.bind, family = Gamma(link='identity')) #  prelim model run for start values

summary(BACI_m4)

check_model(BACI_m4)


BACI.m2.ND <- Create_Data(.data=BB.CB.bind, var='rain.tot.mm') %>%
  broom::augment(BACI_m4, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

#plotting

BACI.glm2 <- glm.plot(BB.CB.bind, BACI.m2.ND, .y=Q.peak.m3.s) + 
  facet_grid(Hydro.Seas ~ Site)
BACI.glm2.all <- add_general_facet_labs(BACI.glm2, 'Season', 'Site')

add.stat.tab(BACI_m4) %>%
  pretty.tab(., 'Seasonal Polynomial Interactive') %>%
  gtsave(file.path(tab_dir, 'SeasPolyInt.html'))

ggsave("6_Event_Stats/BACI_Plots/GLM4_PolyIntSeason.png", plot = BACI.glm2.all ,width = 15, height = 12, units = 'cm', dpi = 600)



# ------- boxplot to show difference in Q max --------------------

flowbox <- function(.data, yax) {
  .data %>%
    mutate(Site = fct_relevel(Site, "Budleigh Brook (impact)", "Colaton Brook (control)")) %>%
    ggplot(., aes(x = Beaver, y=Q.peak.m3.s, fill = Beaver))+
    geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1, size=0.9) +
    geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    ylab(yax)+
    xlab('Beaver Present in Budleigh Brook') +
    theme_bw() +
    coord_cartesian(y=c(0,5))+
    facet_wrap(~Site, ncol=2)+
    theme(legend.position="none",
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
  
}

fb1 <- flowbox(BB.CB.bind, expression("Event Maximum Flow   " (m^{3}~s^{-1})))
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
  purrr::map(., ~print_sum_stats(.))

ggsave("6_Event_Stats/BACI_plots/Fig1.QMax_Boxplot.png", plot = fb1,width = 15, height = 15, units = 'cm', dpi = 600)
