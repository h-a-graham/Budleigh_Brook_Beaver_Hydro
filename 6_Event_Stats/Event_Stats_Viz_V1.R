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
library(gridExtra)
library(performance)
library(glm2)

# ------------- Read Data ------------------------------
hyd_dat <- read_rds("./5_event_extraction/eventExtraction__beaver/EastBud/run_20200619_1022_value__padding2880_alpha0.98_passes3_BFI0.814/eventEx_EVENTS_metrics.rds")

all_flow <- read_rds('4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds') %>% # reload flow inout for calculating correct Excedence...
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

hyd_dat2 <- hyd_dat %>%
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


# ------ Add Exceedance limits ------------------------

perc_flow<- ecdf(all_flow$q) # calculate Empirical Cumulative Distribution for original Q data

hyd_dat2 <- hyd_dat2 %>%
  mutate(per_q = (1 - perc_flow(Q.peak.m3.s)) * 100)

head(hyd_dat2)
tail(hyd_dat2)
summary(hyd_dat2)


# ------- boxplot to show difference in Q max --------------------

ggplot(hyd_dat2, aes(x = Beaver, y=Q.peak.m3.s, fill = Beaver))+
  geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1) +
  geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  xlab("Beaver Present") + 
  ylab(expression("Event Maximum Flow   " (m^{3}~s^{-1})))+
  theme_bw()+
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.border = element_blank())

ggsave("6_Event_Stats/plots/Fig1.QMax_Boxplot.jpg", width = 15, height = 15, units = 'cm', dpi = 600)


# ------ Test Correlations - find best variables to describe peak flow... ---------
# indicates that rain.mean - i.e. the average rainfall across the entire event is the best predictor of peak Q.

corr_df <- hyd_dat2 %>%
  select(Q.peak.m3.s, rain.tot.mm, init.rain.tot.mm, rain.rate, init.rain.rate, rain.mean, init.rain.mean, Beaver)

ggpairs(corr_df, mapping = aes(colour=Beaver))


# How does this peak flow relate to rain...
# GAM I think is most reliable but other options commented out for alternatives...

ggplot(hyd_dat2, aes(x = rain.tot.mm, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +

  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = 'cs'), method.args = list( method = "REML"),se=F) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = 'cs'), method.args = list( method = "REML"),
              geom = "ribbon", alpha = 0.2, linetype=2) +
  # geom_smooth(method = 'loess', se=F) +
  # stat_smooth(method = "loess", geom = "ribbon", alpha = 0.2, linetype=2) +
  # stat_smooth(method = "glm", se = TRUE,
  #             method.args = list(family = Gamma(link='log')), linetype = 1) +
  # stat_smooth(method = "glm", se = TRUE, geom = "ribbon",
  #             method.args = list(family = Gamma(link='log')), alpha = 0.2,linetype = 2) +
  # geom_smooth(method = 'lm', se = T)+
  # geom_smooth(method = 'lm', formula = y~poly(x,2), se=F)+
  # stat_smooth(method = "lm", formula = y~poly(x,2), geom = "ribbon", alpha = 0.1, linetype=2) +
  # coord_cartesian(ylim = c(0,10)) +
  # ylim(c(0,1))+
  # xlim(c(0,30))+
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
       colour = "Beaver Present", fill = "Beaver Present") +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.border = element_blank())


# basic ANCOVA - Diagnostics show massive heteroscedaticity and lack of normallity... Reject
m1 <- lm(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= hyd_dat2)


autoplot(m1, which = 1:6, ncol = 3, label.size = 3)


# glm with Gamma fit - This now works!!!


hyd_dat2b<- hyd_dat2 %>%
  filter(row_number() != 599) 

m2 <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= hyd_dat2b, family = Gamma(link='identity'))

m2b <- glm2(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= hyd_dat2b, family = Gamma(link='identity'), start = coef(m2))
summary(m2b)

autoplot(m2b, which = 1:6, ncol = 3, label.size = 3)
check_model(m2b)

m2b %>%
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T) %>%
  ggplot(., aes(x=rain.tot.mm, y=.fitted, colour=Beaver))+
  geom_point(aes(y=Q.peak.m3.s))+
  geom_line() +
  geom_ribbon(aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
              alpha=0.2, linetype=2, lwd=0.2) +
  coord_cartesian(ylim=c(0,7))+
  theme_bw()



# glm with Hydrological Seasona as additional variable...struggling with convergence...
hyd_dat2c<- hyd_dat2b %>%
  filter(row_number() != 490) 
#   filter(row_number()!= 228L) %>%
#   filter(row_number()!= 108L) %>%
#   filter(row_number()!= 192L)
m3 <- glm2(Q.peak.m3.s ~ Hydro.Seas * Beaver + rain.tot.mm  ,data= hyd_dat2c, family = Gamma(link='identity'))
m3b <- glm2(Q.peak.m3.s ~ Hydro.Seas * Beaver + rain.tot.mm ,data= hyd_dat2c, family = Gamma(link='identity'), 
            start = coef(m3))
            # start = c(0.4, -0.3, -0.3, 0.01, 0.2))
# 0.45501502, -0.40199354, -0.41626150, 0.03327306, 0.37657013
summary(m3b)

check_model(m3b)
autoplot(m3b, which = 1:6, ncol = 3, label.size = 3)

m3b %>%
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T) %>%
  ggplot(., aes(x=rain.tot.mm, y=.fitted, colour=Beaver))+
  geom_point(aes(y=Q.peak.m3.s))+
  geom_line() +
  geom_ribbon(aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
              alpha=0.2, linetype=2, lwd=0.2) +
  coord_cartesian(ylim=c(0,7))+
  facet_wrap(~Hydro.Seas, ncol=1) +
  theme_bw()

# GAM to understand more complex relationship? - Issues still arising due to small flow events.

gam1d <- gam(Q.peak.m3.s ~ s(rain.tot.mm, by=Beaver, bs='cs') + Beaver, 
             method = "REML", data = hyd_dat2)

plot(gam1d, pages = 1, all.terms = TRUE, shade=TRUE, shade.col = "lightblue", shift = coef(gam1d)[1])

summary(gam1d)

layout(matrix(1:4, ncol = 2, byrow = TRUE))
gam.check(gam1d)
layout(1)

concurvity(gam1d, full = TRUE)

gam_df <- gam1d %>% 
  augment() %>%
  mutate(.fitted.shift = .fitted - coef(gam1d)[1])

gam_df %>%
  ggplot(aes(x=.fitted, y=.resid))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(method = "loess")


ggplot(hyd_dat2, aes(x = rain.tot.mm, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +

  geom_point(alpha = 0.5, size=0.8) +
  geom_line(data = gam_df, aes(y=.fitted))+
  geom_ribbon(data = gam_df, aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
              alpha=0.2, linetype=2, lwd=0.2)+
  # coord_cartesian(ylim = c(0,11), xlim = c(0,1.5))+

  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
       colour = "Beaver Present", fill = "Beaver Present") +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.border = element_blank(),
        legend.position=c(.14,.85),
        legend.background=element_blank(),
        legend.title=element_text(size=10))

ggsave("6_Event_Stats/plots/Fig2.QMax_meanRain_GAM.jpg", width = 15, height = 15, units = 'cm', dpi = 600)

# SO THESE RESULTS ARE A STRONG INDICATION THAT BEAVER HAVE AN IMPACT ON PEAK FLOWS... BUT NONE OF THESE MODELS
# CAN BE RELIED ON TOO MUCH DUE THE DIAGNOSTIC FAILURES. THESE FAILURES RESULT FROM THE WIDE RANGE OF FLOWS AND
# INCREASED VARIANCE WITH INCREASED RAINFALL (TO BE EXPECTED).

# FIRST OPTION IS TO RESCALE VARAIBLES:


#  ------------ Rescaling peak Q to Exceedance rate... --------------------

hyd_dat3 <- hyd_dat2 %>%
  filter(per_q > 0) #%>%   # max recorded flow removed as zeros not allowed 


#prelim plot to show concept - axis needs flipping...
ggplot(hyd_dat3, aes(x = rain.mean, y = per_q, colour = Beaver, fill = Beaver)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "glm", formula = 'y ~ x', se = TRUE,
              method.args = list(family = Gamma(link='log')), linetype = 1, alpha = 0.1) +
  stat_smooth(method = "glm", formula = 'y ~ x', se = TRUE, geom = "ribbon",
              method.args = list(family = Gamma(link='log')), alpha = 0.1, linetype = 2) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= ("Peak Flow Exceedance "),
       colour = "Beaver Present", fill = "Beaver Present") +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) 


# GLM with Gamma(log) link - looks reasonable...
m5 <- glm(per_q ~ rain.mean + Beaver, data= hyd_dat3, family = Gamma(link='log'))

glance(m5) 

summary(m5)
tidy(m5)

m5.tidy <- tidy(m5) %>%
  mutate_at(vars(estimate, std.error, statistic), round,3) %>%
  mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                          ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                 ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                        formatC(p.value,format = "f", 3))))) %>%
  rename(T.statistic = statistic) %>%
  mutate(term = c('Intercept', 'Mean Rainfall', 'Beaver Present'))

autoplot(m5, which = 1:6, ncol = 3, label.size = 3)

hyd_dat3 <- hyd_dat3 %>%
filter(row_number() != 120L) # Remove outliers??

m5 <- glm(per_q ~ rain.mean + Beaver, data= hyd_dat3, family = Gamma(link='log')) # refit model
autoplot(m5, which = 1:6, ncol = 3, label.size = 3)

# Create tibble with predicted and residual values with Broom
glm_df <- m5 %>% 
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T)

# Residual Plot
glm_df  %>%
  mutate(glm_fit = stats::predict(m5))%>%
  ggplot(aes(x=glm_fit, y=.resid))+
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_smooth(method = "loess")

# create fake data for prediction
new_dat.m5 <- Create_Data(.data=hyd_dat3, var='rain.mean')

# generate tibble with new predictions
m5_df <- m5 %>%
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T, newdata=new_dat.m5)

# Main Plot
Exc.glm.plt <- ggplot(m5_df, aes(x = rain.mean, y = .fitted, colour = Beaver, fill = Beaver)) +
  geom_point(data=hyd_dat3, aes(y=per_q), alpha = 0.5, size=0.8) +
  geom_ribbon(aes(ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), alpha=0.2, linetype=2, lwd=0.2) +
  geom_line() +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= ("Peak Flow Exceedance "),
       colour = "Beaver Present", 
       fill = "Beaver Present") +
  scale_y_reverse()+
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 1, l = 0)),
        legend.position=c(.74,.2),
        legend.background=element_blank(),
        legend.title=element_text(size=10),
        panel.border = element_blank()) 


Exc.glm.tab <- tableGrob(m5.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.8)),
                                                                    colhead = list(fg_params=list(cex = 0.8)),
                                                                    rowhead = list(fg_params=list(cex = 0.8)),
                                                                    padding = unit(c(3,2),"mm")))

Exc.glm.save_plt <- grid.arrange(Exc.glm.plt, Exc.glm.tab,
                         nrow=2,
                         as.table=TRUE,
                         heights=c(3,0.7))

ggsave("6_Event_Stats/plots/Fig3.Excedence_Rain.jpg", plot = Exc.glm.save_plt, width = 15, height = 15, units = 'cm', dpi = 600)


# model with hydro season as fixed efffect.

m6 <- glm(per_q ~ rain.mean + Beaver + Hydro.Seas, data= hyd_dat3, family = Gamma('log')) # I think this is the appropriate option here...

summary(m6)
tidy(m6)
glance(m6)

m6.tidy <- tidy(m6) %>%
  mutate_at(vars(estimate, std.error, statistic), round,3) %>%
  mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                          ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                 ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                        formatC(p.value,format = "f", 3))))) %>%
  rename(T.statistic = statistic) %>%
  mutate(term = c('Intercept','Mean Rainfall', 'Beaver Present', 'Hydro. Season'))

m6 %>% 
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T) %>%
  ggplot(aes(x=.fitted, y=.resid))+
  geom_point() +
  geom_hline(yintercept = 0, linetype=2) +
  geom_smooth(method = "loess")


check_model(m6) # some possible minor heteroscedasticity? probably acceptable?

# create new data for prediction
new_dat.m6 <- Create_Data(.data=hyd_dat3, var='rain.mean')

# generate tibble with new predictions
m6_df <- m6 %>%
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T, newdata=new_dat.m6)


# ---- PLot GLM with CIs --------------

Exc.glm.plt.hs <- ggplot(m6_df, aes(x = rain.mean, y = .fitted, colour = Beaver, fill = Beaver)) +
  geom_point(data=hyd_dat3, aes(y=per_q), alpha = 0.5, size=0.8) +
  geom_ribbon(aes(ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), alpha=0.2, linetype=2, lwd=0.2) +
  geom_line() +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  scale_y_reverse()+
  facet_wrap(~Hydro.Seas, ncol=1) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= ("Peak Flow Exceedance "),
       colour = "Beaver Present", 
       fill = "Beaver Present") +
  theme_bw() +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
        legend.background=element_blank(),
        legend.title=element_text(size=10),
        panel.border = element_blank())


Exc.glm.tab.hs <- tableGrob(m6.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.8)),
                                                                     colhead = list(fg_params=list(cex = 0.8)),
                                                                     rowhead = list(fg_params=list(cex = 0.8)),
                                                                     padding = unit(c(3,2),"mm")))

Exc.glm.hs.save_plt <- grid.arrange(Exc.glm.plt.hs, Exc.glm.tab.hs,
                                  nrow=2,
                                  as.table=TRUE,
                                  heights=c(3,0.7))

ggsave("6_Event_Stats/plots/Fig4.Excedence_Rain_Season.jpg", plot=Exc.glm.hs.save_plt, width = 15, height = 15, units = 'cm', dpi = 600)



# --------------- Quantile Regression ------------------------
# Now adjusted to interactive model...

qr.mod <- rq(Q.peak.m3.s ~ rain.mean * Beaver, tau = c(0.25, 0.5, 0.7, 0.9), data= hyd_dat2)
print(tidy(qr.mod, se = "nid"), n=24)


qr.tidy <- tidy(qr.mod, se = "nid") %>%
  mutate_at(vars(estimate, std.error, statistic, conf.low, conf.high), round,3) %>%
  mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                          ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                 ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                        formatC(p.value,format = "f", 3))))) %>%
  rename(T.statistic = statistic, quantile = tau) %>%
  mutate(term = rep(c('Intercept', 'Mean Rainfall', 'Beaver Present', 'Mean Rainfall:Beaver Present'),4)) %>%
  select(term, quantile, estimate, std.error, conf.low, conf.high, T.statistic, p.value)



rq.new_dat <- Create_Data(.data=hyd_dat2, var='rain.mean')

rq_pred_func <- function(.data, new.data, q){
  qr.mod <- rq(Q.peak.m3.s ~ rain.mean * Beaver, tau = c(q), data= .data)
  as_tibble(predict.rq(object=qr.mod, newdata = new.data, interval ='confidence', level = .95))%>%
    bind_cols(., new.data) %>%
    mutate(.tau = as_factor(!!q))
}


qr.se.df <- bind_rows(lapply(c(0.25, 0.5, 0.7, 0.9), function(x)rq_pred_func(hyd_dat2, new.data = rq.new_dat, q=x)))



QuRe.plot <- ggplot(qr.se.df, aes(x = rain.mean, y = fit, colour = Beaver, fill = Beaver)) +
  geom_point(data=hyd_dat2, aes(y=Q.peak.m3.s), alpha = 0.3, size=1.2) +
  geom_line(lwd=0.6) +
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha=0.2, linetype=2, lwd=0.2) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  facet_wrap(~.tau, ncol=2) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
       colour = "Beaver Present", 
       fill = "Beaver Present",
       linetype='Quantile') +
  # guides(colour=FALSE, fill=FALSE, linetype=guide_legend(reverse=T))+
  theme_bw() +
  coord_cartesian(xlim=c(0,1.5), ylim = c(0,10))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
        legend.background=element_blank(),
        legend.title=element_text(size=10),
        panel.border = element_rect(colour = "grey", linetype = 1))


QuRe.tab <- tableGrob(qr.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.6)),
                                                                     colhead = list(fg_params=list(cex = 0.5)),
                                                                     rowhead = list(fg_params=list(cex = 0.5)),
                                                                     padding = unit(c(2,1),"mm")))

QuRe.save_plt <- grid.arrange(QuRe.plot, QuRe.tab,
                                  nrow=2,
                                  as.table=TRUE,
                                  heights=c(3,1.5))



ggsave("6_Event_Stats/plots/Fig5.QuantileReg.jpg", plot = QuRe.save_plt, width = 15, height = 18, units = 'cm', dpi = 600)



# --------- Largest Storm Events -------------------

# Select all flow events with magnitude greater than Q2 (Flow exceeded 2% of the time)
big_storms <- hyd_dat2 %>%
  filter(per_q < 5)


# check correlations
BS_df <- big_storms %>%
  select(Q.peak.m3.s, rain.tot.mm, init.rain.tot.mm, rain.rate, init.rain.rate, rain.mean, init.rain.mean,
         rain.peak.mm.h, init.rain.peak.mm.h, Beaver)

ggpairs(BS_df, mapping = aes(colour=Beaver))

# for large events total rain or init total rain possible favourable?

# prelim plot
ggplot(big_storms, aes(x = rain.tot.mm, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +

  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = 'cs'), method.args = list( method = "REML"),se=F) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = 'cs'), method.args = list( method = "REML"),
              geom = "ribbon", alpha = 0.2, linetype=2) +
scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
       colour = "Beaver Present", fill = "Beaver Present") +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        panel.border = element_blank())



# basic ANCOVA - Diagnostics show massive heteroscedaticity and lack of normallity... Reject

m7 <- lm(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= big_storms)


summary(m7)

autoplot(m7, which = 1:6, ncol = 3, label.size = 3)


# glm with Gamma fit - still lots of issues with residuals 

m8  <- glm(Q.peak.m3.s ~ rain.tot.mm + Beaver, data= big_storms, family = Gamma(link='identity'), start = c(0.4, 0.05, 0))


glance(m8) 


summary(m8)
tidy(m8)
m8.tidy <- tidy(m8) %>%
  mutate_at(vars(estimate, std.error, statistic), round,3) %>%
  mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                          ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                 ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                        formatC(p.value,format = "f", 3))))) %>%
  rename(T.statistic = statistic) %>%

  mutate(term = c('Intercept', 'Mean Rainfall', 'Beaver Present'))


# autoplot(m8c, which = 1:6, ncol = 3, label.size = 3)
autoplot(m8, which = 1:6, ncol = 3, label.size = 3) # residuals are better here - go with m8...
check_model(m8)


# Create New Data for prediction...

# get max rainfall values for beaver and non-beaver periods

new_datb <- Create_Data(.data=big_storms, var='rain.tot.mm')


# generate tibble with new predictions
m8_df <- m8 %>%
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T, newdata=new_datb)


# create the > 1 in 50 year Floods i.e < Q2 events plot...

q2plt <- ggplot(big_storms, aes(x = rain.tot.mm, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +

  geom_point(alpha = 0.5) +
  geom_ribbon(data = m8_df, aes(y = .fitted, ymin = .fitted - (1.96 *.se.fit), 
                                ymax = .fitted + (1.96 *.se.fit)), alpha=0.2, linetype=2, lwd=0.2) +
  geom_line(data = m8_df, aes(y = .fitted)) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
       colour = "Beaver Present", fill = "Beaver Present",
       subtitle = '       Flow events with return period > 1 in 20 Years (< Q5)') +
  # coord_cartesian(ylim = c(0,17), xlim = c(0,1.6))+

  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)),
        legend.position=c(.14,.85),
        legend.background=element_blank(),
        legend.title=element_text(size=10),
        panel.border = element_blank()) 


q2tab <- tableGrob(m8.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.8)),
                                                              colhead = list(fg_params=list(cex = 0.8)),
                                                              rowhead = list(fg_params=list(cex = 0.8)),
                                                              padding = unit(c(3,2),"mm")))

q2.plt <- grid.arrange(q2plt, q2tab,
                         nrow=2,
                         as.table=TRUE,
                         heights=c(3,0.7))
ggsave("6_Event_Stats/plots/Fig6.Q5Plus_glm.jpg", plot = q2.plt ,width = 15, height = 15, units = 'cm', dpi = 600)



#  ----------- wet anticedent conditions ------------ 

# Select flow events with the wettest (top 25%) antecedent (5 days preceeding) condition and with a peak flow
# with magnitude greater than Q10 (Flow exceeded 5% of the time)

Wet_Ante_df <- hyd_dat2 %>%
  filter(anti.rain.mm5d > quantile(anti.rain.mm5d, .75) & per_q <5)


m9  <- glm(Q.peak.m3.s ~ rain.mean * Beaver, data= Wet_Ante_df, family = Gamma(link='identity'), start = c(0.1, 2.5, 0, 0))

glance(m9) # not the lowest AIC/BIC but better residuals - go with this.


summary(m9)
m9.tidy <- tidy(m9) %>%
  mutate_at(vars(estimate, std.error, statistic), round,3) %>%
  mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                          ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                 ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                        formatC(p.value,format = "f", 3))))) %>%
  rename(T.statistic = statistic) %>%
  mutate(term = c('Intercept', 'Mean Rainfall', 'Beaver Present', 'Mean Rainfall:Beaver Present'))


autoplot(m9, which = 1:6, ncol = 3, label.size = 3)
check_model(m9)

new_datc <- Create_Data(.data=Wet_Ante_df, var='rain.mean')

# # generate tibble with new predictions
wet_mod_df <- m9 %>%
  augment(type.predict = "response",
          type.residuals = "deviance",
          se_fit = T, newdata=new_datc)


wet.anti.plt <- ggplot(Wet_Ante_df, aes(x=rain.mean, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +
  geom_point(alpha = 0.5)+
  geom_line(data = wet_mod_df, aes(y=.fitted))+
  geom_ribbon(data = wet_mod_df, aes(y = .fitted, ymin = .fitted - (1.96 *.se.fit), 
                  ymax = .fitted + (1.96 *.se.fit)), alpha=0.2, linetype=2, lwd=0.2) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(x = (expression("Mean Rainfall Rate  " (mm/hr^{-1}))),
       y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
       colour = "Beaver Present", fill= "Beaver Present",
       subtitle = '       Flow events with wettest (>75th percentile) 5 days prior and 
       return period > 1 in 20 Years (< Q5)') +
  coord_cartesian(ylim = c(0,17), xlim = c(0,1.6))+
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 10, l = 0)),
        legend.position=c(.14,.85),
        legend.background=element_blank(),
        legend.title=element_text(size=10),
        panel.border = element_blank()) 


wet.anti.tab <- tableGrob(m9.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.8)),
                                                            colhead = list(fg_params=list(cex = 0.8)),
                                                            rowhead = list(fg_params=list(cex = 0.8)),
                                                            padding = unit(c(3,2),"mm")))

wet.anti.plt <- grid.arrange(wet.anti.plt, wet.anti.tab,
             nrow=2,
             as.table=TRUE,
             heights=c(3,0.7))
ggsave("6_Event_Stats/plots/Fig7.Wet_Ante_Q10_PeakQ.jpg", plot = wet.anti.plt ,width = 15, height = 15, units = 'cm', dpi = 600)

#------------------- Investigating the cause - Effects on rising and falling limb duration ----------------------------
# Take home is - nothing clearly going on with lag times. perhaps this is sue to the approach taken or need for greater sensitivity.
# Therefore can it be assumed that the attenuation observed above can largely be attributed to storage?

# check correlations - nothing really to see here??
expl_df <- hyd_dat2 %>%
  select(flimb.dur.hrs, rlimb.dur.hrs, Q.peak.m3.s, Q.response.tot.m3 ,rain.tot.mm, init.rain.tot.mm, rain.rate, init.rain.rate, rain.mean, init.rain.mean,
         rain.peak.mm.h, init.rain.peak.mm.h, Beaver)

ggpairs(expl_df, mapping = aes(colour=Beaver))

# - possibly a small difference in rising limb for big storms but considerable uncertainty and high p-value (0.2).

m10 <- glm(rlimb.dur.hrs ~ Beaver, data=big_storms, family = Gamma(link='identity'))
summary(m10)
autoplot(m10, which = 1:6, ncol = 3, label.size = 3)

# Multi Box plot for viz. 

 bp1 <- ggplot(hyd_dat2, aes(x = Beaver, y=flimb.dur.hrs, fill = Beaver))+
  geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1) +
  geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
  coord_cartesian(y=c(0,50)) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  labs(y= "Falling limb Duration (hrs)",
       subtitle = "All Flow events") +
  theme_bw()+
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_rect(linetype = "dashed", colour = 'grey'),
        plot.subtitle = element_text(hjust = 0.5))
 
 bp2 <- ggplot(hyd_dat2, aes(x = Beaver, y=rlimb.dur.hrs, fill = Beaver))+
   geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1) +
   geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
   coord_cartesian(y=c(0,30)) +
   scale_color_manual(values = c('#A6190D', '#244ED3')) +
   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
   labs(x = ("Beaver Present"),
        y= "Rising limb Duration (hrs)",
        colour = 'Beaver Present', fill = 'Beaver Present')+
   theme_bw()+
   theme(legend.position= "bottom",
         legend.direction= 'horizontal',
         axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         panel.border = element_rect(linetype = "dashed", colour = 'grey'))
 
 bp3 <- ggplot(big_storms, aes(x = Beaver, y=flimb.dur.hrs, fill = Beaver))+
   geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1) +
   geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
   coord_cartesian(y=c(0,50)) +
   scale_color_manual(values = c('#A6190D', '#244ED3')) +
   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
   labs(subtitle= "Flow events > 1 in 50 year return") +
   theme_bw()+
   theme(legend.position="none",
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank(),
         panel.border = element_rect(linetype = "dashed", colour = 'grey'),
         plot.subtitle = element_text(hjust = 0.5))
 
 bp4 <- ggplot(big_storms, aes(x = Beaver, y=rlimb.dur.hrs, fill = Beaver))+
   geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(), stroke = 0.1) +
   geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.3, lwd=0.2) +
   coord_cartesian(y=c(0,30)) +
   scale_color_manual(values = c('#A6190D', '#244ED3')) +
   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
   labs(x = "Beaver Present")+
   theme_bw()+
   theme(legend.position="none",
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         axis.text.y = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         axis.ticks.y = element_blank(),
         panel.border = element_rect(linetype = "dashed", colour = 'grey'))
 
lagtime.Boxplots <- (bp1 / bp2) | (bp3 / bp4)
   
ggsave("6_Event_Stats/plots/Fig8.LagTimes_BoxPlots.jpg", plot = lagtime.Boxplots, width = 15, height = 15, units = 'cm', dpi = 600)



# --------------- Q5 Q95 (Flashiness Ratios) -------------------------

full_flow <- read_rds('4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds') %>%
  mutate(Beaver = as.factor(ifelse(datetime > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                   ifelse(datetime > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                            datetime < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No"))))%>%
  filter(Beaver != "Unsure")


# create Summary tibble
Q5Q95.tab <- full_flow %>%
  drop_na()%>%
  group_by(Beaver) %>% 
  summarize(Mean = mean(q), Median = median(q), R2FDC = (log10(quantile(q, 0.66)) - log10(quantile(q, 0.33)))/(0.66-0.33),
            Q5 = quantile(q, 0.95), Q95 = quantile(q, 0.05)) %>%
  mutate(Beaver = c('Beaver Absent','Beaver Present'), `Q5:Q95 ratio` = Q5/Q95) %>%
  rename(" " = Beaver) %>%
  bind_rows(summarise(.," " = '% Change',
                        Mean = (Mean[2]-Mean[1])/Mean[1]*100,
                        Median = (Median[2]-Median[1])/Median[1]*100,
                      R2FDC= (R2FDC[2]-R2FDC[1])/R2FDC[1]*100,
                        Q5 = (Q5[2]-Q5[1])/Q5[1]*100,
                        Q95 = (Q95[2]-Q95[1])/Q95[1]*100,
                      `Q5:Q95 ratio` = (`Q5:Q95 ratio`[2]-`Q5:Q95 ratio`[1])/`Q5:Q95 ratio`[1]*100,))%>%
  mutate_at(vars(Mean, Median, R2FDC, Q5, Q95, `Q5:Q95 ratio`), round,3) 


# Creat table Grob for adding to flow duration curve plot
g1 <- tableGrob(Q5Q95.tab, rows=NULL,
                theme = ttheme_minimal(core = list(fg_params=list(cex = 0.8)),
                                       colhead = list(fg_params=list(cex = 0.8)),
                                       rowhead = list(fg_params=list(cex = 0.8)),
                                       padding = unit(c(3,2),"mm")))
g2 <- tableGrob(Q5Q95.tab, rows=NULL, 
                cols=as.character(Q5Q95.tab[3, 1:7]),
                theme = ttheme_minimal(core = list(fg_params=list(cex = 0.8)),
                                       colhead = list(fg_params=list(cex = 0.8)),
                                       rowhead = list(fg_params=list(cex = 0.8)),
                                       padding = unit(c(3,2),"mm"))) 

FlowDur.tab <- rbind(g1[-nrow(g1), ], g2[1,])



# ----------------- flow duration curves ------------------------

NoBeavCurve <-  full_flow %>%
  drop_na() %>%
  filter(Beaver == 'No')%>%
  select(q, Beaver) %>%
  arrange(desc(q)) %>%
  mutate(pcntexceedance = seq (0, 1, by = 1/(n()-1)))
  
  
YesBeavCurve <-  full_flow %>%
  drop_na() %>%
  filter(Beaver == 'Yes')%>%
  select(q, Beaver) %>%
  arrange(desc(q)) %>%
  mutate(pcntexceedance = seq (0, 1, by = 1/(n()-1)))

BefAftCurve <- bind_rows(NoBeavCurve, YesBeavCurve)
head(BefAftCurve)
tail(BefAftCurve)


FlowDur.plt <- ggplot(BefAftCurve, aes(x = pcntexceedance, y = q, colour=Beaver)) +
  geom_line() +
  xlab ("% time flow equalled or exceeded") +
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10() +
  geom_vline(xintercept=0.05, lwd = 0.5, linetype=3) +
  annotate("text", x=0.08, label="Q5", y=1, size=3) +
  geom_vline(xintercept=0.95, lwd = 0.5, linetype=3) +
  annotate("text", x=0.91, label="Q95", y=1, size=3) +
  annotation_logticks(sides = "l", colour='grey') +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  theme_bw()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
        legend.position=c(.78,.87),
        legend.background=element_blank(),
        legend.title=element_text(size=10),
        panel.border = element_blank())


flow_dur.save <- grid.arrange(FlowDur.plt, FlowDur.tab,
                         nrow=2,
                         as.table=TRUE,
                         heights=c(3,0.7))

ggsave("6_Event_Stats/plots/Fig9.FlowExCurve.jpg", plot = flow_dur.save, width = 15, height = 15, units = 'cm', dpi = 600)




# ----------- Reverse calculate predicted flow from Exceednace Models -----------------------
# THIS RE-SCALED ANALAYSIS OF PEAK FLOW EXCEEDANCE LEVELS 

# CAN THIS BE BACK CALCULATED TO REAL FLOW?

# WHAT IF WE REMOVE THE PROBLEMATIC SMALLER EVENTS AND FOCUS ON THE LARGEST MOST IMPORTANT EVENTS?
# I DON'T THINK THIS IS AN APPROPRIATE WAY TO EVALUATE EFFECT ON TRUE FLOW VALUES - LEAVE THIS HERE FOR NOW BUT PERHAPS REMOVE LATER.

#GLM
# 
# glm_df2 <- glm_df%>%
#   mutate(Low.CI = .fitted + .se.fit *1.96)%>%
#   mutate(High.CI = .fitted - .se.fit *1.96)%>%
#   mutate(pQ_rank = 1-(.fitted/100))%>%
#   mutate(pQUL_rank = 1-(Low.CI/100))%>%
#   mutate(pQLL_rank = 1-(High.CI/100))%>%
#   mutate(Pred_flow = quantile(all_flow$q, pQ_rank))%>%
#   mutate(Pred_CIL = quantile(all_flow$q, pQLL_rank))%>%
#   mutate(Pred_CIU = quantile(all_flow$q, pQUL_rank))
# 
# 
# ggplot(hyd_dat3, aes(x = rain.mean, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +
#   geom_point(alpha = 0.5, size=0.8) +
#   geom_ribbon(data = glm_df2, aes(y = Pred_flow, ymin = Pred_CIL, ymax = Pred_CIU), alpha=0.2, linetype=2, lwd=0.2) +
#   geom_line(data = glm_df2, aes(y = Pred_flow)) +
#   scale_color_manual(values = c('#A6190D', '#244ED3')) +
#   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
#   labs(x = ("Mean Rainfall Rate (mm/hr)"),
#        y= (expression("Event Maximum Flow   " (m^3/s))),
#        colour = "Beaver Present", 
#        fill = "Beaver Present") +
#   theme_bw() +
#   coord_cartesian(ylim=c(0,3))+
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         legend.position=c(.14,.85),
#         legend.background=element_blank(),
#         legend.title=element_text(size=10),
#         panel.border = element_blank(),)
# 
# # ggsave("6_Event_Stats/plots/Fig5.rain_peakQ_glm_convert.jpg", width = 15, height = 15, units = 'cm', dpi = 600)
# 
# 
# # GLMM with Season as RF...
# new_dat3 <- new_dat2 %>%
#   mutate(pQ_rank = 1-(fitted.BS/100))%>%
#   mutate(pQUL_rank = 1-(Low.CI/100))%>%
#   mutate(pQLL_rank = 1-(High.CI/100))%>%
#   mutate(Pred_flow = quantile(all_flow$q, pQ_rank))%>%
#   mutate(Pred_CIL = quantile(all_flow$q, pQLL_rank))%>%
#   mutate(Pred_CIU = quantile(all_flow$q, pQUL_rank))
# 
# 
# ggplot(hyd_dat3, aes(x = rain.mean, y = Q.peak.m3.s, colour = Beaver, fill = Beaver)) +
#   geom_point(alpha = 0.5, size=0.8) +
#   geom_ribbon(data = new_dat3, aes(y = Pred_flow, ymin = Pred_CIL, ymax = Pred_CIU), alpha=0.2, linetype=2, lwd=0.2) +
#   geom_line(data = new_dat3, aes(y = Pred_flow)) +
#   scale_color_manual(values = c('#A6190D', '#244ED3')) +
#   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
#   facet_wrap(~Hydro.Seas, ncol=1) +
#   labs(x = ("Mean Rainfall Rate (mm/hr)"),
#        y= (expression("Event Maximum Flow   " (m^3/s))),
#        colour = "Beaver Present", 
#        fill = "Beaver Present") +
#   theme_bw() +
#   coord_cartesian(ylim=c(0,3))+
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         strip.text.x = element_text(size = 12, color = "black", face = "italic"),
#         strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
#         legend.background=element_blank(),
#         legend.title=element_text(size=10),
#         panel.border = element_blank())

# ggsave("6_Event_Stats/plots/Fig6.rain_peakQ_glmm_Season_convert.jpg", width = 15, height = 15, units = 'cm', dpi = 600)

