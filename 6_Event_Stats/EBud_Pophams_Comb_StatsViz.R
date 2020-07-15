set.seed(123)
options(scipen = 999)

# ------------- Load Libraries ----------------------
library(tidyverse)
library(mgcv)
library(lubridate)
library(lme4)
library(boot)
library(broom)
library(broom.mixed)
library(tcltk)
library(mgcv)
library(ggfortify)
library(GGally)
library(patchwork)
library(quantreg)
library(ggpmisc)
library(grid)
library(gridExtra)
library(performance)
library(glm2)
library(ggpubr)
# ------------- Read Data ------------------------------
EBUD_hyd_dat <- read_rds("./5_event_extraction/eventExtraction__beaver/EastBud/run_20200619_1022_value__padding2880_alpha0.98_passes3_BFI0.814/eventEx_EVENTS_metrics.rds")

EBUD_all_flow <- read_rds('4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds') %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()

POP_hyd_dat <- read_rds("./5_event_extraction/eventExtraction__beaver/Pophams/run_20200710_1222_value__padding2880_alpha0.98_passes3_BFI0.74/eventEx_EVENTS_metrics.rds")

POP_all_flow <- read_rds('4_Join_Rain_to_Q/exports/Pophams_Q_R_S_ts.rds') %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()


# -------------- Functions ------------------------------

# Create New Data for predictions

Create_Data <- function(.data, vari, NoBeav.min, NoBeav.max, YesBeav.min, YesBeav.max){
  if(missing(NoBeav.min)) {
    NoBeav.min <- .data %>% filter(Beaver == 'No') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% min()}
  if(missing(NoBeav.max)) { 
    NoBeav.max <- .data %>% filter(Beaver == 'No') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% max()}
  if(missing(YesBeav.min)) {
    YesBeav.min <- .data %>% filter(Beaver == 'Yes') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% min()}
  if(missing(YesBeav.max)) {
    YesBeav.max <- .data %>% filter(Beaver == 'Yes') %>% select(!!vari) %>% mutate_all(~(ifelse(is.na(.), 0, .))) %>% max()}
  
  new_NoBeavWetc <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=100), Beaver='No', Hydro.Seas = 'Wet')
  new_NoBeavDryc <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=100), Beaver='No', Hydro.Seas = 'Dry')
  new_YesBeavWetc <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=100), Beaver = 'Yes', Hydro.Seas = 'Wet')
  new_YesBeavDryc <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=100), Beaver = 'Yes', Hydro.Seas = 'Dry')
  
  df <- bind_rows(new_NoBeavWetc, new_NoBeavDryc, new_YesBeavWetc, new_YesBeavDryc)%>%
    mutate(!!vari := newvar) %>%
    select(-newvar)
  return(df)
  
}


# ------------- Clean Data -----------------------------

CleanData <- function(.data){
  .data %>%
  mutate(Beaver = as.factor(ifelse(event.start.ts > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                   ifelse(event.start.ts > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                            event.start.ts < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No"))))%>%
  filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>% # removes period of uncertainty where beaver's presence unknown.
  mutate(Beaver = fct_relevel(Beaver, "No", "Yes")) %>%
  # select_if(~ !any(is.na(.))) %>%
  # drop_na() %>%
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
  mutate(Hydro.Seas = fct_relevel(Hydro.Seas, "Wet", "Dry")) %>%
  select(-c(response.eventID,check.outlier1)) %>%
  drop_na()
}

EBUD_hyd_dat <- CleanData(EBUD_hyd_dat)
POP_hyd_dat <- CleanData(POP_hyd_dat)

# ------ Add Exceedance limits ------------------------

EBUD_perc_flow<- ecdf(EBUD_all_flow$q) # calculate Empirical Cumulative Distribution for original Q data
POP_perc_flow<- ecdf(POP_all_flow$q)


EBUD_hyd_dat <- EBUD_hyd_dat %>%
  mutate(per_q = (1 - EBUD_perc_flow(Q.peak.m3.s)) * 100)

POP_hyd_dat <- POP_hyd_dat %>%
  mutate(per_q = (1 - POP_perc_flow(Q.peak.m3.s)) * 100)

# head(EBUD_hyd_dat)
# tail(EBUD_hyd_dat)
# summary(EBUD_hyd_dat)


# ------- boxplot to show difference in Q max --------------------

flowbox <- function(.data, title, yax) {
  ggplot(.data, aes(x = Beaver, y=Q.peak.m3.s, fill = Beaver))+
  geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1) +
  geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  ggtitle(title) + 
  ylab(yax)+
  theme_bw() +
    coord_cartesian(y=c(0,10))+
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5))

}

fb1 <- flowbox(EBUD_hyd_dat, 'Budleigh Brook', expression("Event Maximum Flow   " (m^{3}~s^{-1})))
fb2 <- flowbox(POP_hyd_dat, 'Colaton Brook', "") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

fb3 <- grid.arrange(
  fb1, fb2, ncol = 2,
  bottom = textGrob("Beaver Present", gp=gpar(fontsize=11)))

ggsave("6_Event_Stats/Join_plots/Fig1.QMax_Boxplot.jpg", plot = fb3,width = 15, height = 15, units = 'cm', dpi = 600)


# ----------------------- Fit Gamma (identity) Model --------------------------------

# fit Regression for Budleigh Brook.

EBUD_hyd_datb<- EBUD_hyd_dat %>%
  filter(row_number() != 599) # remove outlier value with large influence.

EB_m2 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EBUD_hyd_datb, family = Gamma(link='identity')) # prelim run to get starting vals

EB_m2b <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EBUD_hyd_datb, family = Gamma(link='identity'), start = coef(m2)) # final model
summary(EB_m2b)

autoplot(EB_m2b, which = 1:6, ncol = 3, label.size = 3)
check_model(EB_m2b)


EB_m2.ND <- Create_Data(.data=EBUD_hyd_datb, var='rain.tot.mm') %>%
  broom::augment(EB_m2b, type.predict = "response",
          type.residuals = "deviance",
          se_fit = T, newdata=.)


add.stat.tab <- function(.model){ # function to create a table for additive model
  tidy(.model) %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic) %>%
    mutate(term = c('Intercept', 'Total Rainfall', 'Beaver'))
}

EB.m2.tidy <- add.stat.tab(EB_m2b)


# fit regression for Colaton Brook

POP_m2 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= POP_hyd_dat, family = Gamma(link='identity')) # model converges with defaults
summary(POP_m2)

autoplot(POP_m2, which = 1:6, ncol = 3, label.size = 3)
check_model(POP_m2)

POP_m2.ND <- Create_Data(.data=POP_hyd_dat, var='rain.tot.mm')%>%
  broom::augment(POP_m2, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

POP.m2.tidy <- add.stat.tab(POP_m2)

# plotting...
glm.plot <- function(.data, model.data, title) {
  ggplot(.data, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
  geom_point(alpha = 0.5, size=0.8)+
  geom_line(data=model.data, aes(x=rain.tot.mm, y = .fitted)) +
  geom_ribbon(data=model.data,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
              alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  coord_cartesian(ylim=c(0,7))+
    labs(y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle(title) +
  theme_bw() +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 11))
}
 
merge.plots <- function(plot.1, plot.2){
  p <- ggarrange(plot.1, plot.2, ncol=2, common.legend = TRUE, legend="top", widths = c(3, 2.8))
  
  p2 <- grid.arrange(p, bottom = textGrob(expression("              Total Event Rainfall" (mm/hr^{-1})), gp=gpar(fontsize=11)), ncol=1)
  
  return(p2)
}

glm1.tab <- function(model.tab){
  tableGrob(model.tab, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.55)),
                                                         colhead = list(fg_params=list(cex = 0.55)),
                                                         rowhead = list(fg_params=list(cex = 0.55)),
                                                         padding = unit(c(2,2),"mm")))
}

join.hori <- function(tab.1, tab.2) {
  grid.arrange(glm1.tab(tab.1), glm1.tab(tab.2), nrow=1, widths = c(0.5,0.5))
}

join.vert <- function(.plot, .tab){
  fin <- grid.arrange(.plot, .tab,
                         nrow=2,
                         as.table=TRUE,
                         heights=c(3,0.7))
} 



EB.glm1 <- glm.plot(EBUD_hyd_datb, EB_m2.ND, "Budleigh Brook") 

POP.glm1 <- glm.plot(POP_hyd_dat, POP_m2.ND, "Colaton Brook") +theme(axis.title.y = element_blank(),
                                                                      axis.text.y=element_blank(),
                                                                      axis.ticks.y=element_blank())

join.glm1.plot <- merge.plots(EB.glm1, POP.glm1)

join.glm1.tab <- join.hori(EB.m2.tidy, POP.m2.tidy)

join.glm1.all <- join.vert(join.glm1.plot, join.glm1.tab)

ggsave("6_Event_Stats/Join_plots/Fig2.GLM1.jpg", plot = join.glm1.all ,width = 15, height = 15, units = 'cm', dpi = 600)

# ---------------- GLM with hydrological season -------------------------------------------

#  Fit Regression - Budleigh Brook
EBUD_hyd_datc<- EBUD_hyd_datb %>%
  filter(row_number() != 490) 

EB_m3 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver  ,data= EBUD_hyd_datc, family = Gamma(link='identity')) #  prelim model run for start values
EB_m3b <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver ,data= EBUD_hyd_datc, family = Gamma(link='identity'), # final model
            start = coef(EB_m3))
summary(EB_m3b)

check_model(EB_m3b)
autoplot(EB_m3b, which = 1:6, ncol = 3, label.size = 3)

EB_m3.ND <- Create_Data(.data=EBUD_hyd_datc, var='rain.tot.mm') %>%
  broom::augment(EB_m3b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

inter.stat.tab <- function(.model){
  tidy(.model) %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic) %>%
    mutate(term = c('Intercept', 'Total Rainfall', 'Hyd. Season', 'Beaver', 'Beaver:Hyd. Season'))
}

EB.m3.tidy <- inter.stat.tab(EB_m3b)

# Fit regression - Colaton Brook

POP_m3 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver   ,data= POP_hyd_dat, family = Gamma(link='identity')) # prelim model for starting vals
POP_m3b <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver  ,data= POP_hyd_dat, family = Gamma(link='identity'), # final model
                start = coef(POP_m3))

summary(POP_m3b)

check_model(POP_m3b)
autoplot(POP_m3b, which = 1:6, ncol = 3, label.size = 3)

POP_m3.ND <- Create_Data(.data=POP_hyd_dat, var='rain.tot.mm') %>%
  broom::augment(POP_m3b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

POP.m3.tidy <- inter.stat.tab(POP_m3b)

#plotting

EB.glm2 <- glm1.plot(EBUD_hyd_datc, EB_m3.ND, "Budleigh Brook") + 
  facet_wrap(~Hydro.Seas, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))

POP.glm2 <- glm1.plot(POP_hyd_dat, POP_m3.ND, "Colaton Brook") +
  facet_wrap(~Hydro.Seas, ncol = 1) +
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))

join.glm2.plot <- merge.plots(EB.glm2, POP.glm2)

join.glm2.tab <- join.hori(EB.m3.tidy, POP.m3.tidy)

join.glm2.all <- join.vert(join.glm2.plot, join.glm2.tab)

ggsave("6_Event_Stats/Join_plots/Fig3.GLM2.jpg", plot = join.glm2.all ,width = 15, height = 15, units = 'cm', dpi = 600)


# ---------------- GLM for high flow events --------------------------------

# Budleigh Brook

# Select all flow events with magnitude greater than Q2 (Flow exceeded 2% of the time)
EB_big_storms <- EBUD_hyd_dat %>%
  filter(per_q < 5)

EB_m8  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EB_big_storms, family = Gamma(link='identity')) # prelim model to get starting vals
EB_m8b  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EB_big_storms, family = Gamma(link='identity'), start = coef(EB_m8)) # final model

summary(EB_m8b) 

check_model(EB_m8b)
autoplot(EB_m8b, which = 1:6, ncol = 3, label.size = 3)

EB_m8.ND <- Create_Data(.data=EB_big_storms, var='rain.tot.mm')%>%
  broom::augment(EB_m8b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

EB.m8.tidy <- add.stat.tab(EB_m8b)


# Colaton Brook

# Select all flow events with magnitude greater than Q2 (Flow exceeded 2% of the time)
POP_big_storms <- POP_hyd_dat %>%
  filter(per_q < 5)

POP_m8  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= POP_big_storms, family = Gamma(link='identity')) # prelim model to get starting vals
POP_m8b  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= POP_big_storms, family = Gamma(link='identity'), start = coef(POP_m8)) # final model

summary(POP_m8b) 

check_model(POP_m8b)
autoplot(POP_m8b, which = 1:6, ncol = 3, label.size = 3)

POP_m8.ND <- Create_Data(.data=POP_big_storms, var='rain.tot.mm')%>%
  broom::augment(POP_m8b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

POP.m8.tidy <- add.stat.tab(POP_m8b)

# plotting

EB.glm3 <- glm.plot(EB_big_storms, EB_m8.ND, "Budleigh Brook") + coord_cartesian(ylim=c(0,10))

POP.glm3 <- glm.plot(POP_big_storms, POP_m8.ND, "Colaton Brook") +
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_cartesian(ylim=c(0,10))

join.glm3.plot <- merge.plots(EB.glm3, POP.glm3)

join.glm3.tab <- join.hori(EB.m8.tidy, POP.m8.tidy)

join.glm3.all <- join.vert(join.glm3.plot, join.glm3.tab)

ggsave("6_Event_Stats/Join_plots/Fig4.GLM3.jpg", plot = join.glm3.all ,width = 15, height = 15, units = 'cm', dpi = 600)


# ------------------ GLM for high flows and wet antecedent conditions -----------------------------------

# Select flow events with the wettest (top 25%) antecedent (5 days preceeding) condition and with a peak flow
# with magnitude greater than Q10 (Flow exceeded 5% of the time)

# Budleigh Brook

EB_Wet_Ante_df <- EB_big_storms %>%
  filter(anti.rain.mm5d > quantile(anti.rain.mm5d, .75))


EB_m9  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EB_Wet_Ante_df, family = Gamma(link='identity'))
EB_m9b  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EB_Wet_Ante_df, family = Gamma(link='identity'), start = coef(EB_m9))


summary(EB_m9b)

check_model(EB_m9b)
autoplot(EB_m9b, which = 1:6, ncol = 3, label.size = 3)

EB_m9.ND <- Create_Data(.data=EB_Wet_Ante_df, var='rain.tot.mm')%>%
  broom::augment(EB_m9b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

EB.m9.tidy <- add.stat.tab(EB_m9b)

# Colaton Brook


POP_Wet_Ante_df <- POP_big_storms %>%
  filter(anti.rain.mm5d > quantile(anti.rain.mm5d, .75))


POP_m9  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= POP_Wet_Ante_df, family = Gamma(link='identity'))
# EB_m9b  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= EB_Wet_Ante_df, family = Gamma(link='identity'), start = coef(EB_m9))


summary(POP_m9)

check_model(POP_m9)
autoplot(POP_m9, which = 1:6, ncol = 3, label.size = 3)

POP_m9.ND <- Create_Data(.data=POP_Wet_Ante_df, var='rain.tot.mm')%>%
  broom::augment(POP_m9, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

POP.m9.tidy <- add.stat.tab(POP_m9)

#plotting

EB.glm4 <- glm.plot(EB_Wet_Ante_df, EB_m9.ND, "Budleigh Brook") + coord_cartesian(ylim=c(0,10))

POP.glm4 <- glm.plot(POP_Wet_Ante_df, POP_m9.ND, "Colaton Brook") +
  theme(axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  coord_cartesian(ylim=c(0,10))

join.glm4.plot <- merge.plots(EB.glm4, POP.glm4)

join.glm4.tab <- join.hori(EB.m9.tidy, POP.m9.tidy)

join.glm4.all <- join.vert(join.glm4.plot, join.glm4.tab)

ggsave("6_Event_Stats/Join_plots/Fig4.GLM4.jpg", plot = join.glm4.all ,width = 15, height = 15, units = 'cm', dpi = 600)


