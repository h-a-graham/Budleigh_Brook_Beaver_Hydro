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
# ------------- Read Data ------------------------------
EBUD_hyd_dat1 <- read_rds("./5_event_extraction/eventExtraction__beaver/EastBud/run_20200619_1022_value__padding2880_alpha0.98_passes3_BFI0.814/eventEx_EVENTS_metrics.rds")

EBUD_all_flow <- read_rds('4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds') %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()

POP_hyd_dat1 <- read_rds("./5_event_extraction/eventExtraction__beaver/Pophams/run_20200710_1222_value__padding2880_alpha0.98_passes3_BFI0.74/eventEx_EVENTS_metrics.rds")

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
  select(rain.tot.mm, Q.peak.m3.s, rain.peak.ts, Q.peak.ts, anti.rain.mm5d, per_q, Beaver, Hydro.Seas, Site)

CB_evetns <- POP_hyd_dat %>%
  mutate(Site='Colaton Brook (control)')%>%
  select(rain.tot.mm, Q.peak.m3.s, rain.peak.ts, Q.peak.ts, anti.rain.mm5d, per_q, Beaver, Hydro.Seas, Site)


BB.CB.bind <- BB_events %>%
  bind_rows(CB_evetns) %>%
  mutate(Site = fct_relevel(Site, "Colaton Brook (control)", "Budleigh Brook (impact)"))



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


ggsave("6_Event_Stats/BACI_plots/Fig1.QMax_Boxplot.jpg", plot = fb1,width = 15, height = 15, units = 'cm', dpi = 600)


# ----------------------- Fit Gamma (identity) Model --------------------------------

# Fit regression
BACI_m1 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity')) # prelim run to get starting vals

BACI_m1b <- glm2(Q.peak.m3.s ~  rain.tot.mm + Beaver * Site , data= BB.CB.bind, family = Gamma(link='identity'), start = coef(BACI_m1)) # final model

summary(BACI_m1b) # Crucially, interaction between site and BEaver is significant and negative i.e beaver effect is stat sig. and reduced and impacted site

# Check Diagnostics
autoplot(BACI_m1b, which = 1:6, ncol = 3, label.size = 3) #
check_model(BACI_m1b) #

# Run Anaova on model (useful to compare means but not really what we're intersted in - here Beaver alone is significant.)
BACI.aov1 <- aov(BACI_m1b)
summary(BACI.aov1)

# Create New data for predictions
BACI.m1.ND <- Create_Data(.data=BB.CB.bind, var='rain.tot.mm') %>%
  broom::augment(BACI_m1b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

# function to create Tidy Regression SUmmary table
add.stat.tab <- function(.model){ # function to create a table for additive model
  tidy(.model) %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic) %>%
    mutate(term = c('Intercept', 'Total Rainfall', 'Beaver', 'Budleigh Brook', 'Beaver:Budleigh Brook'))
}

BACI.m1.tidy <- add.stat.tab(BACI_m1b)

# Function to Create Tidy table of marginal means

emmeans.tab <- function(.emmeans.obj){
  tidy(.emmeans.obj) %>%
    rename(`Lower CL` = asymp.LCL) %>%
    rename(`Upper CL` = asymp.UCL) %>%
    select(-df) %>%
    mutate_at(vars(estimate, std.error, `Lower CL`, `Upper CL`), round,3)
    
}

BACI.emm.1 <- emmeans(BACI_m1b,  ~Beaver * Site)
  
BACI.mm1.tidy <- emmeans.tab(BACI.emm.1)


# plotting...
glm.plot <- function(.data, model.data) {
  ggplot(.data, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=model.data, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=model.data,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,7))+
    labs(y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
         x=expression("Total Event Rainfall" (mm/hr^{-1})),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          legend.position = 'top')
}


pretty.tab <- function(model.tab, title, size.cm){
  model.tab %>%
    gt() %>%
    tab_header(
      title = md(sprintf('**<div style="text-align: left"> %s </div>**', title))) %>% 
    gtsave(.,tempfile('tab', fileext = '.png')) %>%
    readPNG(.) %>%
    rasterGrob( interpolate=TRUE, width = unit(size.cm,"cm"))
}
# 
join.hori <- function(tab.1, tab.2, w1, w2, t1, t2) {
  x11()
  g <- grid.arrange(pretty.tab(tab.1, t1, w1), pretty.tab(tab.2, t2, w2), ncol=2)
  dev.off()
  return(g)
}

join.vert <- function(.plot, .tab1, .tab2, w1, w2, t1, t2){
  grid.arrange(.plot, join.hori(.tab1, .tab2, w1, w2, t1, t2),
                      nrow=2,
                      as.table=TRUE,
                      heights=c(3.5,1.5))
} 



BACI.glm1 <- glm.plot(BB.CB.bind, BACI.m1.ND) + 
  facet_wrap(~Site, ncol=2)

BACI.glm1.all <- join.vert(BACI.glm1, BACI.m1.tidy, BACI.mm1.tidy, 5.9, 6.3, 'Regression Summary', 'Marginal Means')

ggsave("6_Event_Stats/BACI_Plots/Fig2.GLM1.jpg", plot = BACI.glm1.all ,width = 15, height = 15, units = 'cm', dpi = 600)


# ---------------- GLM with hydrological season -------------------------------------------

#  Fit Regression 

BACI_m2 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver * Site, data= BB.CB.bind, family = Gamma(link='identity')) #  prelim model run for start values
BACI_m2b <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver * Site, data= BB.CB.bind, family = Gamma(link='identity'), start = coef(BACI_m2))
BACI_m2c <- glm2(Q.peak.m3.s ~ rain.tot.mm + Hydro.Seas * Beaver * Site, data= BB.CB.bind, family = Gamma(link='identity'), start = coef(BACI_m2b))

summary(BACI_m2c)

check_model(BACI_m2c)
autoplot(BACI_m2c, which = 1:6, ncol = 3, label.size = 3)

# Run Anaova on model (useful to compare means but not really what we're intersted in - here Beaver alone is significant.)
BACI.aov2 <- aov(BACI_m2c)
summary(BACI.aov2)


BACI.m2.ND <- Create_Data(.data=BB.CB.bind, var='rain.tot.mm') %>%
  broom::augment(BACI_m2c, type.predict = "response",
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
    mutate(term = c('Intercept', 'Total Rainfall', 'Wet Season', 'Beaver', 'Budleigh Brook', 'Beaver:Wet Season',
                    'Wet Season:Budleigh Brook (impact)', 'Beaver:Budleigh Brook', 'Wet Season:Beaver:Budleigh Brook'))
}

BACI.m2.tidy <- inter.stat.tab(BACI_m2c)

BACI.emm.2 <- emmeans(BACI_m2c, ~Beaver * Hydro.Seas * Site)

BACI.mm2.tidy <- emmeans.tab(BACI.emm.2)


#plotting

BACI.glm2 <- glm.plot(BB.CB.bind, BACI.m2.ND) + 
  facet_grid(Hydro.Seas ~ Site)

BACI.glm2.all <- join.vert(BACI.glm2, BACI.m2.tidy, BACI.mm2.tidy, 5.9, 6.3, 'Regression Summary', 'Marginal Means')


ggsave("6_Event_Stats/BACI_Plots/Fig3.GLM2.jpg", plot = BACI.glm2.all ,width = 15, height = 15, units = 'cm', dpi = 600)


# ---------------- GLM for high flow events --------------------------------

# Budleigh Brook

# Select all flow events with magnitude greater than Q5 (Flow exceeded 5% of the time)
BB.CB_big_storms <- BB.CB.bind %>%
  filter(per_q < 5)

BACI_m3  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site, data= BB.CB_big_storms, family = Gamma(link='identity')) # prelim model to get starting vals
BACI_m3b  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site, data= BB.CB_big_storms, family = Gamma(link='identity'), start = coef(BACI_m3)) # final model

summary(BACI_m3b) 

check_model(BACI_m3b)
autoplot(BACI_m3b, which = 1:6, ncol = 3, label.size = 3)

BACI.m3.ND <- Create_Data(.data=BB.CB_big_storms, var='rain.tot.mm')%>%
  broom::augment(BACI_m3b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

BACI.m3.tidy <- add.stat.tab(BACI_m3b)

BACI.emm.3 <- emmeans(BACI_m3b, ~Beaver * Site)

BACI.mm3.tidy <- emmeans.tab(BACI.emm.3)

# plotting

BACI.glm3 <- glm.plot(BB.CB_big_storms, BACI.m3.ND) + 
  facet_wrap(~ Site, ncol=2)

BACI.glm3.all <- join.vert(BACI.glm3, BACI.m3.tidy, BACI.mm3.tidy, 5.9, 6.3, 'Regression Summary', 'Marginal Means')


ggsave("6_Event_Stats/BACI_Plots/Fig4.GLM3.jpg", plot = BACI.glm3.all ,width = 15, height = 15, units = 'cm', dpi = 600)

# ------------------ GLM for high flows and wet antecedent conditions -----------------------------------

# Select flow events with the wettest (top 25%) antecedent (5 days preceeding) condition and with a peak flow
# with magnitude greater than Q5 (Flow exceeded 5% of the time)

# Budleigh Brook

BB.CB_Wet_Ante_df <- BB.CB_big_storms %>%
  filter(anti.rain.mm5d > quantile(anti.rain.mm5d, .75))


BACI_m4  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site, data= BB.CB_Wet_Ante_df, family = Gamma(link='identity')) # prelim model to get starting vals
BACI_m4b  <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver * Site, data= BB.CB_Wet_Ante_df, family = Gamma(link='identity'), start = coef(BACI_m4)) # final model

summary(BACI_m4b) 

check_model(BACI_m4b)
autoplot(BACI_m4b, which = 1:6, ncol = 3, label.size = 3)

BACI.m4.ND <- Create_Data(.data=BB.CB_Wet_Ante_df, var='rain.tot.mm')%>%
  broom::augment(BACI_m4b, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

BACI.m4.tidy <- add.stat.tab(BACI_m4b)

BACI.emm.4 <- emmeans(BACI_m4b, ~Beaver * Site)

BACI.mm4.tidy <- emmeans.tab(BACI.emm.4)

# plotting

BACI.glm4 <- glm.plot(BB.CB_Wet_Ante_df, BACI.m4.ND) + 
  facet_wrap(~ Site, ncol=2)

BACI.glm4.all <- join.vert(BACI.glm4, BACI.m4.tidy, BACI.mm4.tidy, 5.9, 6.3, 'Regression Summary', 'Marginal Means')


ggsave("6_Event_Stats/BACI_Plots/Fig5.GLM4.jpg", plot = BACI.glm4.all ,width = 15, height = 15, units = 'cm', dpi = 600)


# --------------- Q5 Q95 (Flashiness Ratios) -------------------------

Read.Edit.Flow <- function(.filePath){
  read_rds(.filePath) %>%
    mutate(Beaver = as.factor(ifelse(datetime > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                     ifelse(datetime > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                              datetime < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No"))))%>%
    filter(Beaver != "Unsure") %>%
    drop_na()
}

EB_full_flow <- Read.Edit.Flow('4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds') %>%
  mutate(Site = 'Budleigh Brook (impact)')
POP_full_flow <- Read.Edit.Flow('4_Join_Rain_to_Q/exports/Pophams_Q_R_S_ts.rds') %>%
  mutate(Site = 'Colaton Brook (control)')



# create Summary tibbles
Flow.Sum.Tab <- function(.data){
  .data %>%
    group_by(Beaver) %>% 
    summarize(Mean = mean(q), Median = median(q), R2FDC = (log10(quantile(q, 0.66)) - log10(quantile(q, 0.33)))/(0.66-0.33),
              Q5 = quantile(q, 0.95), Q95 = quantile(q, 0.05)) %>%
    mutate(Beaver = c('No Beaver','Beaver'), `Q5:Q95 ratio` = Q5/Q95) %>%
    rename(" " = Beaver) %>%
    bind_rows(summarise(.," " = '% Change',
                        Mean = (Mean[2]-Mean[1])/Mean[1]*100,
                        Median = (Median[2]-Median[1])/Median[1]*100,
                        R2FDC= (R2FDC[2]-R2FDC[1])/R2FDC[1]*100,
                        Q5 = (Q5[2]-Q5[1])/Q5[1]*100,
                        Q95 = (Q95[2]-Q95[1])/Q95[1]*100,
                        `Q5:Q95 ratio` = (`Q5:Q95 ratio`[2]-`Q5:Q95 ratio`[1])/`Q5:Q95 ratio`[1]*100,))%>%
    mutate_at(vars(Mean, Median, R2FDC, Q5, Q95, `Q5:Q95 ratio`), round,3) 
}

EB_FlowSumTab <- Flow.Sum.Tab(EB_full_flow)
POP_FlowSumTab <- Flow.Sum.Tab(POP_full_flow)

# ----------------- flow duration curves ------------------------

# Create joint table for plotting
calc.fdc <- function(.data){
  NoBeavCurve <-  .data %>%
    drop_na() %>%
    filter(Beaver == 'No')%>%
    select(q, Beaver, Site) %>%
    arrange(desc(q)) %>%
    mutate(pcntexceedance = seq (0, 1, by = 1/(n()-1)))
  
  
  YesBeavCurve <-  .data %>%
    drop_na() %>%
    filter(Beaver == 'Yes')%>%
    select(q, Beaver, Site) %>%
    arrange(desc(q)) %>%
    mutate(pcntexceedance = seq (0, 1, by = 1/(n()-1)))
  
  BefAftCurve <- bind_rows(NoBeavCurve, YesBeavCurve)
}

Joint.Flow <- EB_full_flow %>%
  calc.fdc(.)%>%
  bind_rows(calc.fdc(POP_full_flow)) %>%
  mutate(Site = fct_relevel(Site, "Colaton Brook (control)", "Budleigh Brook (impact)")) 


#plot FDC
plot.fdc <- function(.data){
  .data %>%
    mutate(Site = fct_relevel(Site, "Budleigh Brook (impact)", "Colaton Brook (control)")) %>%
    ggplot(., aes(x = pcntexceedance, y = q, colour=Beaver)) +
    geom_line() +
    ylab(expression(Flow~(m^{3}~s^{-1})))+
    xlab('% time flow equalled or exceeded')+
    scale_x_continuous(labels = scales::percent) +
    scale_y_log10(limits=c()) +
    geom_vline(xintercept=0.05, lwd = 0.5, linetype=5, colour='grey50') +
    annotate("text", x=0.1, label="Q5", y=1, size=3) +
    geom_vline(xintercept=0.95, lwd = 0.5, linetype=5, colour='grey50') +
    annotate("text", x=0.89, label="Q95", y=1, size=3) +
    # annotation_logticks(sides = "l", colour='grey') +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    facet_wrap(~Site) +
    theme_bw()+
    theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          legend.position=c(.18,.87),
          legend.background=element_blank(),
          legend.title=element_text(size=10),
          panel.border = element_blank())
  
}


fdc.plots <- plot.fdc(Joint.Flow)


fdc.join.plot <- join.vert(fdc.plots, EB_FlowSumTab, POP_FlowSumTab, 5.9, 5.9, 'Budliegh Brook Flow Summary', 'Colaton Brook Flow Summary' )



ggsave("6_Event_Stats/BACI_Plots/Fig6.FlowDurCurve.jpg", plot = fdc.join.plot, width = 15, height = 15, units = 'cm', dpi = 600)

