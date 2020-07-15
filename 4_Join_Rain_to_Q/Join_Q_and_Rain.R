# Join Rain and Q data for input to Event Extraction

# ---------- INSATALL AND LOAD PACKAGES -------------------
library(tidyverse)
library(lubridate)
library(patchwork)
# ----------------- READ DATA ------------------------

# Budleigh Brook Data
# Q.S.df <- read_rds("./3_Stage_Q_rating/exports/EB_Q_Stage_ts.rds")
BudBrook.Q.S.df <- read_rds("./3_Stage_Q_rating/exports/EB_Q_Stage_ts_NR.rds") # version post noise cleaning

BudBrook.Rain.df <- read_rds("./4_Join_Rain_to_Q/rain_data_in/BudBrook_Rain_09_20.rds") 

#Pophams control data
PopCont.Q.S.df <- read_rds("./1.1_Clean_Control_Flow/exports/Pophams_Q_S_ts.rds") # version post noise cleaning

PopCont.Rain.df <- read_csv("./4_Join_Rain_to_Q/rain_data_in/Pophams_rain/15_min_data/AOI_15_min_200907090000_202003112345.csv") %>%  
  mutate(datetime = date_time) %>%
  select(datetime, rain_intensity_mmhr)

# ---------------- JOIN DATA ---------------------------
join_Q.R <- function(Q.S.df, Rain.df) {
  inner_join(Q.S.df, Rain.df, by='datetime') %>%
    rename(rainfall = rain_intensity_mmhr)
  }
 
BudBrook.Q.S.R.df <- join_Q.R(BudBrook.Q.S.df, BudBrook.Rain.df) %>% 
  mutate(q= q/1000)

PopCont.Q.S.R.df <- join_Q.R(PopCont.Q.S.df, PopCont.Rain.df)

# ---------------- PLOT Q AND RAIN --------------------

rain_plot <-  function(.data, tit) {
  ggplot(.data, aes(x = datetime, y = rainfall)) +

  geom_line(colour = "steelblue3") +
  scale_x_datetime(date_breaks = "1 year" , date_labels = "%y") +
  theme_bw() +
  ylab("Rainfall Rate mm/hr") +
  labs(title=tit)+
  scale_y_reverse()+
  # coord_cartesian(ylim=c(30,0)) +


  theme(axis.title.x    = element_blank(),
        axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        )

}
# rain_plot

Q_plot <- function(.data) {
  ggplot(.data, aes(x=datetime, y=q)) +

  geom_line(colour='#193C54', size = 0.1)+
  scale_x_datetime(date_breaks = "1 year" , date_labels = "%Y") +
  # ylim(c(0,7000))+
  xlab("Date/Time") +
  ylab(bquote('Discharge '~(m^3/s))) +

    # coord_cartesian(ylim=c(0,12)) +
  theme_bw() 
}
# Q_plot

rain_plot(BudBrook.Q.S.R.df, "Budleigh Brook, Hayes Lane EA gauging station: 07/2009 - 03/2020")/Q_plot(BudBrook.Q.S.R.df) + 
  plot_layout( heights = c(1, 3))



ggsave('./4_Join_Rain_to_Q/plots/Q_Rain_ts.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))

rain_plot(PopCont.Q.S.R.df, "Colaton Brook, Pophams EA gauging station: 07/2009 - 03/2020")/Q_plot(PopCont.Q.S.R.df) + 
  plot_layout( heights = c(1, 3))


ggsave('./4_Join_Rain_to_Q/plots/PopControl_Q_Rain_ts.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))

# ----------- SAVE RDS ---------------------

saveRDS(BudBrook.Q.S.R.df, file='./4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds')

saveRDS(PopCont.Q.S.R.df, file='./4_Join_Rain_to_Q/exports/Pophams_Q_R_S_ts.rds')

