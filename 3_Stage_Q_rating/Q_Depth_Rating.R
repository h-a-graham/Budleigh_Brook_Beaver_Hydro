# script to develop rating equation for Discharge (Q) from EA gauge data and Flow meter.

# ---------- INSATALL AND LOAD PACKAGES -------------------
# devtools::install_github("ZheyuanLi/SplinesUtils")
library(tidyverse)
library(lubridate)
library(minpack.lm)
library(splines)
library(SplinesUtils)
library(patchwork)
library(broom)

# ------------------- DEFINE DATA PATHS  --------------------------

Flow_path1 <-"./1_Clean_Flow_Data/exports/EB_Flow_round1.rds"
Flow_path2 <- "./1_Clean_Flow_Data/exports/EB_Flow_round2.rds"
EB_Stage_path <- "./2_Join_Clean_Stage_Data/exports/EB_Stage_Clean.rds"

# ----------------- READ DATA ------------------------------------

Q.df.1 <- read_rds(Flow_path1) %>%
  mutate(mon_period = "first")%>%
  rename(stage = depth_m)
Q.df.2 <- read_rds(Flow_path2)%>%
  mutate(mon_period = "second")%>%
  rename(stage = depth_m)
stage.df <- read_rds(EB_Stage_path)%>%
  rename(stage = depth_m)

# ------------------ JOIN TWO FLOW MONITORING PERIODS AND DEPTH VALUES-------------

Q.S.df <- bind_rows(Q.df.1, Q.df.2) %>%
  drop_na() %>%
  left_join(stage.df, by='date_time') %>%
  rename(EA_depth_m = stage.y) %>%
  rename(FM_depth_m = stage.x) %>%
  select(date_time, FM_depth_m, velocity_ms, flow_ls, EA_depth_m, mon_period)

# ------------------- DO SOME PLOTS... ----------------------

ggplot(Q.S.df, aes(x=EA_depth_m, y=flow_ls)) +
  geom_point( size = 1.0, alpha = 0.4) +
  geom_smooth( method = "lm", formula = y ~ splines::bs(x, knots = c(0.29,0.58,0.7)), fullrange = FALSE,
               se = T, linetype=4, lwd = 0.8) +
  # labs(colour = 'monitoring period') +
  xlab("Depth (m)") +
  ylab("Flow (l/s)") +
  scale_colour_brewer(palette = "Set1") +
  theme_bw()

ggplot(Q.S.df, aes(x=EA_depth_m, y=flow_ls, colour = mon_period)) +
  geom_point( size = 1.0, alpha = 0.4) +
  # geom_smooth( method = "lm", formula = y ~ splines::bs(x, knots = c(0.2,0.58,0.7)), fullrange = FALSE,
  #              se = T, linetype=4, lwd = 0.8) +
  # coord_cartesian(ylim = c(0,1250))+
  geom_smooth(method = 'loess', formula = 'y~x', size = 0.8, alpha = 0.4) +
  labs(colour = 'monitoring period') +
  xlab("Depth (m)") +
  ylab("Flow (l/s)") +
  scale_colour_brewer(palette = "Set1") +
  theme_bw()

ggsave('./3_Stage_Q_rating/plots/Q_Stage_rating_check.jpg', 
       width = 18,
       height = 15,
       units = c("cm"))


# --------------- CALCULATE DEPTH AT ZERO FLOW --------------------------

# a survey was carried out by AP Land Surveys on  21/04/2010 - Survey Cross Section in '3_Stage_Q_rating/Cross_Section/APLS0024-013 East Budleigh Cross Section.pdf'

pond_hod <- 17.68          # Height above datum value for water level
WeirCrest_hod <- 17.48     # height above datum for weir crest

diff_to_zero <- pond_hod - WeirCrest_hod  # drop in elevation required for zero flow

CS_day_Stage <- stage.df %>%
  filter(date_time > ymd_hms('2010-04-21 09:00:00') & date_time < ymd_hms('2010-04-21 17:00:00')) # filter stage data to working hours of survey day (No time given for water surface survey...)


depth.survey <- mean(CS_day_Stage$stage) # mean depth for working day [1] 0.4730913
sd.survey <- sd(CS_day_Stage$stage) # stdev for working day [1] 0.004509152

depth.zeroQ <- depth.survey - diff_to_zero # pond depth at zero flow [1] 0.2730913

# ggplot(CS_day_Stage, aes(x=date_time, y=stage))+
#   geom_line()+
#   ylim(c(0.4, 0.5)) +
# theme_bw()


# ------------------- DEVELOP RATING EQUATION ---------------------
# clearly, the second deployment of the flow meter, as shown by the previous graph, was not as successful as the first.
# As the two time periods cover similar flows we now disregard the second flow measurement period.

# Sort and split data - take only first sampling period. 

Q.S.select <- Q.S.df %>% 
  filter(mon_period == 'first') %>%
  rename(stage = EA_depth_m)

# -------- SPLINE REGRESSION --------------------
# Here, we use spline regression to derive a piecewise cubic spline to describe the complex rating
# that results from the multi-angled weir structure at the site. 

# create tibble with 1 row that desribes the conditions at zero flow
row_rep <- function(.data, n) {
  .data[rep(1:nrow(.data), times = n),]
}

zero_row <- tibble(FM_depth_m = 0, velocity_ms = 0, 
                   flow_ls = 0, stage = depth.zeroQ, mon_period  = 'zero') %>%
  row_rep(n=100000)

# bind this to the end of the Q / Stage tibble to force the spline through this origin
Q.S.select.ed <- Q.S.select %>%
  select(FM_depth_m, velocity_ms, flow_ls, stage, mon_period) %>%
  bind_rows(zero_row) 

# generate spline regression
Q.S.spline <- lm(flow_ls ~ splines::ns(stage, df = 2, Boundary.knots = c(0.58, 0.8)), data = Q.S.select.ed) # The selected model with 2 degrees of freedom

# create fake data to predict line - better than using ggplot to be sure the prediction is correct.
pred_data <- tibble(stage=seq(depth.zeroQ, max(stage.df$stage, na.rm = T), length=1000)) %>%
  mutate(flow_pred = predict(Q.S.spline, data.frame(stage)))

# pred_data[1,] # check predicted level at zero flow...0.001613599

# Generate Spline Equations for plotting
spline_equations <- summary(RegBsplineAsPiecePoly(Q.S.spline, "splines::ns(stage, df = 2, Boundary.knots = c(0.58, 0.8))")) # from https://github.com/ZheyuanLi/SplinesUtils which provides access to the piecewise-cubic splines

print(spline_equations)

pw_eq_list <- paste("Piecewise polynomials of degree 3:",
  str_c(paste("y = ", spline_equations, sep = ""),  collapse = "\n"), sep = "\n")


# ------- PLOT MODELS -----------------

rating_zoom <- ggplot(Q.S.select, aes(x=stage, y=flow_ls)) +
  geom_point(alpha=0.3, colour='#68BAF2') +
  geom_line(data=pred_data, aes(y=flow_pred), linetype=1, lwd = 0.5, colour='#062E47') +
  coord_cartesian(xlim = c(0.515,0.63), ylim = c(0,1200)) +
  scale_x_continuous(breaks = seq(0.3,0.65, by = 0.025)) +
  scale_y_continuous(breaks = seq(0,1250, by = 250)) +
  xlab('Depth at Weir (m)') +
  ylab(expression("measured Flow   " (l/s^{-1}))) +
  labs(title = 'Flow rating curve for Hayes Lane - Budleigh Brook')+
  theme_bw() +
  theme(axis.title.x=element_blank())
rating_zoom

rating_wide <- ggplot(Q.S.select, aes(x=stage, y=flow_ls)) +
  geom_point(color='#68BAF2',alpha=0.5) +
  geom_line(data=pred_data, aes(y=flow_pred), linetype=1, lwd = 0.5, colour='#062E47') +
  geom_vline(xintercept = depth.zeroQ, linetype = 2) + # zero flow line.
  annotate(geom = "text", x = depth.zeroQ - 0.01 , y = 5000, label = "depth at zero-flow", angle = 90) +
  scale_x_continuous(limits = c(0.26,0.88),breaks = seq(0.26,0.88, by = 0.05)) +
  scale_y_continuous(limits = c(0,20000), breaks = seq(0,20000, by = 5000)) +
  xlab('Depth at Weir (m)') +
  ylab(expression("measured Flow   " (l/s^{-1}))) +
  labs(caption=pw_eq_list)+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = 'plot')

rating_zoom /
rating_wide 


ggsave('./3_Stage_Q_rating/plots/Q_Stage_rating.jpg',
       width = 25,
       height = 18,
       units = c("cm"))



# --------------- APPLY RATING TO STAGE TIME SERIES -------------------
# q = predict(Q.S.spline, stage.df)
Q.S.ts.df <- stage.df %>%
  mutate(q = predict(Q.S.spline, .)) %>%
  rename(datetime = date_time) %>%
  select(datetime, stage, q)

# --------------- PLOT FULL Q TIME SERIES ---------------------
ggplot(Q.S.ts.df, aes(x=datetime, y=q))+
  geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
  scale_x_datetime(date_breaks = "6 months" , date_labels = "%b-%y") +
  # ylim(c(0,7000))+
  xlab("Date/Time") +
  ylab(expression("Flow   " (l/s^{-1}))) +
  labs(title="Budleigh Brook, Hayes Lane EA gauging station: 07/2009 - 03/2020")+
  theme_bw()

ggsave('./3_Stage_Q_rating/plots/Converted_Q_ts.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))

# --------------- SAVE Q TIME SERIES DATA FRAME ----------------

saveRDS(Q.S.ts.df, file = "./3_Stage_Q_rating/exports/EB_Q_Stage_ts.rds")


# ------- CHECK SPECIFIC TIMES IF YOU LIKE... -------------------
# Function to get the index of a dataframe by data-time character
idx_from_date <- function(.data, date_chr){ 
  .data %>% rowid_to_column("idx")%>%
    filter(datetime  == date_chr) %>%
    pull(1)
}

# Function to return slice of df based on start and end date-time characters
clip_data <- function(.data, start, end){
  start_id <- idx_from_date(.data, start)
  end_id <- idx_from_date(.data, end)
  return(.data %>% slice(start_id:end_id))
}

checkplot <- function(.data, s.time, e.time){
  clip_data(.data, s.time, e.time) %>%
    ggplot(aes(x=datetime, y=q))+
    geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
    # scale_x_datetime(date_breaks = "1 month" , date_labels = "%d-%m")+
    theme_bw()+
    theme(text = element_text(size=10))
}

checkplot(Q.S.ts.df, '2020-02-14 22:00:00', '2020-02-18 00:00:00') # storm Dennis
checkplot(Q.S.ts.df, '2019-02-18 22:00:00', '2020-02-18 00:00:00') # 19-20

checkplot(Q.S.ts.df, '2012-11-24 00:00:00', '2012-11-27 00:00:00') # Biggest recorded Event

checkplot(Q.S.ts.df, '2015-11-05 00:00:00', '2015-11-11 00:00:00') # issues with flat peaks seems to be resolved...

#------ EXTRAS ------------
# --- COMPARE HIGH FLOW CURVE OPTIONS 
# ggplot(Q.S.select, aes(x = EA_depth_m, y = flow_ls)) +
#   geom_point(colour='#60B8EF', alpha=0.7) +
#   
#   stat_smooth(data=Q.S.low, method = 'nlsLM', formula = 'y ~ o + a * exp(b^x)',
#               method.args=list(start = c(o = Q.offset, a = 0, b=380), control= m.iter),
#               se=FALSE,  color='#062E47', linetype=2, lwd = 0.8) +
#   geom_smooth(data = Q.S.select.ed, aes(colour='spline'), method = "lm", formula = y ~ splines::ns(x, 11), fullrange = TRUE,
#               se = F, linetype=4, lwd = 0.8) +
#   
#   stat_smooth(data =Q.S.high, aes(colour='Large Exp'), method = 'nlsLM',formula = 'y ~ o + a * exp(b^x)',  fullrange=TRUE,
#               method.args=list(start = c(o = Q.offset, a = 0, b=3), control= m.iter),se=FALSE, linetype=2) +
#   
#   stat_smooth(data =Q.S.high, aes(colour='Small Exp'), method = 'nlsLM',formula = 'y ~ o + a * exp(b^x)',  fullrange=TRUE,
#               method.args=list(start = c(o = Q.offset, a = 0, b=1), control= m.iter),se=FALSE, linetype=2) +
# 
#   stat_smooth(data=Q.S.high, aes(colour='Linear'),method = 'lm', formula = 'y ~  x', fullrange = TRUE,
#               se=FALSE,  linetype=2) +
#   
#   stat_smooth(data =Q.S.high, aes(colour='Power'), method = 'nlsLM', formula = 'y ~ o + a * (x^b) ',
#               method.args=list(start = c(o = Q.offset, a = 0, b=1), control= m.iter), fullrange=TRUE,
#               se=FALSE, linetype=2) +
# 
#   scale_x_continuous(limits = c(0.5,0.9), breaks = seq(0.5,0.9, by = 0.1))+
#   scale_y_continuous(limits = c(0,8000), breaks = seq(0,8000, by = 1000))+
# 
#   xlab('Depth at Weir (m)') +
#   ylab('measured Flow (l/s)') +
#   labs(title = 'Flow rating curve for Hayes Lane - Budleigh Brook', colour='Curve Options')+
#   theme_bw()
# 
# start.H <- list(o = Q.offset, a =0, b=1)
# LowFlowMod <- nlsLM(flow_ls ~ o + a * exp(b ^ EA_depth_m), data = Q.S.high, start = start.H, control = m.iter)
# coef(LowFlowMod)

# ggsave('./3_Stage_Q_rating/plots/Q_Stage_ratingOptions.jpg',
#        width = 18,
#        height = 15,
#        units = c("cm"))



# ---------------- ALTERNATIVE RATINGS NOW SUPERSEDED 

# === Power Model for full df ===
# m.iter <- nls.lm.control(maxiter= 200)
# start.L = list(o = Q.offset, a =0, b=0.1)
# m = nlsLM(flow_ls ~ o + a * ( EA_depth_m ^ b), data = Q.S.select, start = start.L, control = m.iter)
# c1 <- as.numeric(as.character(format(coef(m)[1], digits = 10)))
# c2 <- as.numeric(as.character(format(coef(m)[2], digits = 10)))
# c3 <- as.numeric(as.character(format(coef(m)[3], digits = 10)))
# 


# --- EXPONENTIAL CURVE 
# ggplot(Q.S.select, aes(x = EA_depth_m, y = flow_ls)) +
#   geom_point(colour='#60B8EF', alpha=0.7) +
#   stat_smooth(method = 'nlsLM', formula = 'y ~ o + a * exp(b^x)', method.args=list(start = c(o = Q.offset, a = 0, b=380), control= m.iter),se=FALSE, color='#062E47', linetype=2) +
#   annotate("text", x = 0.55, y = 1100,
#            label = sprintf("y == %s + %s * italic(exp) (%s ^ x)",
#                            round(c1, 2), formatC(c2, format = "e", digits = 2),
#                            round(c3, 2)), parse = TRUE) +
#   xlab('Depth at Weir (m)') +
#   ylab('measured Flow (l/s)') +
#   labs(title = 'Flow rating curve for Hayes Lane - Budleigh Brook')+
#   theme_bw()


# # ------- EXPONTENTIAL MODEL FOR LOWER FLOWS
# m.iter <- nls.lm.control(maxiter= 100)
# start.L <- list(o = Q.offset, a =0, b=380)
# LowFlowMod <- nlsLM(flow_ls ~ o + a * exp(b ^ EA_depth_m), data = Q.S.low, start = start.L, control = m.iter)
# coef(LowFlowMod)
# lf.c1 <- as.numeric(as.character(format(coef(LowFlowMod)[1], digits = 10)))
# lf.c2 <- as.numeric(as.character(format(coef(LowFlowMod)[2], digits = 10)))
# lf.c3 <- as.numeric(as.character(format(coef(LowFlowMod)[3], digits = 10)))
# 
# 
# # ----- EXPONTENTIAL MODEL FOR HIGHER FLOWS
# 
# start.L.hf <- list(o = Q.offset, a =0, b=1)
# HighFlowMod <- nlsLM(flow_ls ~ o + a * exp(b ^ EA_depth_m), data = Q.S.high, start = start.L.hf, control = m.iter)
# coef(HighFlowMod)
# hf.c1 <- as.numeric(as.character(format(coef(HighFlowMod)[1], digits = 10)))
# hf.c2 <- as.numeric(as.character(format(coef(HighFlowMod)[2], digits = 10)))
# hf.c3 <- as.numeric(as.character(format(coef(HighFlowMod)[3], digits = 10)))


# #----- Evaluating model intersection
# 
# Q.S.BasePlot <-ggplot(Q.S.select, aes(x = EA_depth_m, y = flow_ls, colour=s_group)) +
#   geom_point(alpha=0.7) 
# 
# intersect.plot <- Q.S.BasePlot +
#   stat_smooth(data=Q.S.low, method = 'nlsLM', formula = 'y ~ o + a * exp(b^x)',
#               method.args=list(start = c(o = Q.offset, a = 0, b=380), control= m.iter), fullrange = TRUE,
#               se=FALSE,  color='#062E47', linetype=1, lwd = 0.8) +
#   stat_smooth(data =Q.S.high, method = 'nlsLM',formula = 'y ~ o + a * exp(b^x)',  fullrange=TRUE,
#               method.args=list(start = c(o = Q.offset, a = 0, b=1), control= m.iter),
#               se=FALSE, color='#062E47', linetype=1, lwd = 0.8) +
#   geom_abline(intercept = -13373.03, slope = 22507.80 , color='#062E47', linetype=1, lwd=0.8)+
#   scale_x_continuous(limits = c(0.601,0.6025), breaks = seq(0.6,0.605, by = 0.0002))+
#   scale_y_continuous(limits = c(150,200), breaks = seq(150,200, by = 25)) +
#   theme_bw()
# intersect.plot 

# #Intersection of Model Approximates 0.6019 And will therefore mark the breakpoint between models.
# 
# hf_start <- -13373.03 + 0.6019 * 22507.80  # define the point at which flow switches to 
# hf_end <- -13373.03 + 0.65 * 22507.80



# Q.S.ModsPlot <- Q.S.BasePlot +
#   # stat_smooth(data=Q.S.low, method = 'nlsLM', formula = 'y ~ o + a * exp(b^x)',
#   #             method.args=list(start = c(o = Q.offset, a = 0, b=380), control= m.iter),fullrange = TRUE,
#   #             se=FALSE,  color='#062E47', linetype=2, lwd = 0.8) +
#   # 
#   geom_smooth(data= Q.S.select.ed, method = "lm", formula = y ~ splines::ns(x, 15), fullrange = TRUE,
#               se = F,  color='#062E47', linetype=3, lwd = 0.8) +
#   
# 
#   
#   # stat_smooth(data=Q.S.low, method = 'nlsLM', formula = 'y ~ o + a * exp(b^x)',
#   #             method.args=list(start = c(o = Q.offset, a = 0, b=380), control= m.iter),fullrange = TRUE,
#   #             se=FALSE,  color='#062E47', linetype=2, lwd = 0.8) +
#   
#   stat_smooth(data =Q.S.high, method = 'nlsLM',formula = 'y ~ o + a * exp(b^x)',
#               method.args=list(start = c(o = Q.offset, a = 0, b=1), control= m.iter), fullrange = TRUE,
#               se=FALSE,  color='#062E47', linetype=2) +
#   
#   scale_x_continuous(limits = c(0.27,0.9), breaks = seq(0.5,0.9, by = 0.1))+
#   scale_y_continuous(limits = c(0,10000), breaks = seq(0,10000, by = 500))+
#   
#     annotate("text", x = 0.54, y = 250,
#              label = sprintf("y == %s + %s * italic(exp) (%s ^ x)",
#                              round(lf.c1, 2), formatC(lf.c2, format = "e", digits = 2),
#                              round(lf.c3, 2)), parse = TRUE) +
#   annotate("text", x = 0.62, y = 1300,
#            label = sprintf("y == %s + %s * italic(exp) (%s ^ x)",
#                            round(hf.c1, 2), formatC(hf.c2, format = "e", digits = 2),
#                            round(hf.c3, 2)), parse = TRUE) +
#   
# xlab('Depth at Weir (m)') +
#   ylab('measured Flow (l/s)') +
#   labs(title = 'Flow rating curve for Hayes Lane - Budleigh Brook', colour = 'Flow Category')+
#   theme_bw()
# 
# Q.S.ModsPlot 
# 
# ggsave('./3_Stage_Q_rating/plots/Q_Stage_rating.jpg',
#        width = 18,
#        height = 15,
#        units = c("cm"))

# ------ explain use of splines:

rating_zoom_CP <- ggplot(Q.S.select, aes(x=stage, y=flow_ls)) +
  geom_point(alpha=0.3, colour='#68BAF2') +
  geom_line(data=pred_data, aes(y=flow_pred), linetype=1, lwd = 0.5, colour='#062E47') +
  stat_smooth(method = 'nlsLM', formula = y ~ a * (x^b), se = FALSE, colour='red', lwd = 0.5, fullrange=T) +
  coord_cartesian(xlim = c(0.515,0.63), ylim = c(0,1200)) +
  scale_x_continuous(breaks = seq(0.3,0.65, by = 0.025)) +
  scale_y_continuous(breaks = seq(0,1250, by = 250)) +
  xlab('Depth at Weir (m)') +
  ylab(expression("measured Flow   " (l/s^{-1}))) +
  labs(title = 'Flow rating curve for Hayes Lane - Budleigh Brook',
       subtitle = 'A comparison of piecewise-spline and Power regression')+
  theme_bw() +
  theme(axis.title.x=element_blank())
rating_zoom_CP

rating_wide_CP <- ggplot(Q.S.select, aes(x=stage, y=flow_ls)) +
  geom_point(color='#68BAF2',alpha=0.5) +
  geom_line(data=pred_data, aes(y=flow_pred, colour='piecewise spline'), linetype=1, lwd = 0.5) +
  geom_smooth(aes(colour = 'power'), method = 'nlsLM', formula = y ~ a * (x^b), se = FALSE, lwd = 0.5, fullrange=T) +
  geom_vline(xintercept = depth.zeroQ, linetype = 2) + # zero flow line.
  annotate(geom = "text", x = depth.zeroQ - 0.01 , y = 5000, label = "depth at zero-flow", angle = 90) +
  scale_x_continuous(limits = c(0.26,0.88),breaks = seq(0.26,0.88, by = 0.05)) +
  scale_y_continuous(limits = c(0,20000), breaks = seq(0,20000, by = 5000)) +
  scale_colour_manual(values = c('#062E47', 'red')) +
  xlab('Depth at Weir (m)') +
  ylab(expression("measured Flow   " (l/s^{-1}))) +
  labs(caption=pw_eq_list)+
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = 'plot', 
        legend.position = "bottom",
        legend.title=element_blank())

rating_zoom_CP /
  rating_wide_CP 

ggsave('./3_Stage_Q_rating/plots/Q_Stage_rating_compare.jpg',
       width = 25,
       height = 18,
       units = c("cm"))
