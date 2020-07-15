
# Script to load, clean and save Pophams flow data for full time series...

# ---------- INSATALL AND LOAD PACKAGES -------------------

library(tidyverse)
library(lubridate)

# ------------------- READ DATA  --------------------------

control_flow_path <- '1.1_Clean_Control_Flow/Pophams GS 15min flow data 2009 - 2020/Pophams GS 15min flow data 2009 - 2020.csv'
control_stage_path <- '1.1_Clean_Control_Flow/Pophams GS 15min flow data 2009 - 2020/Pophams GS 15min level data 2009 - 2020.csv'


flow_df <- read_csv(control_flow_path, skip = 21, col_names= c('datetime', 'q', 'state_of_value', 'tags', 'comments'))

stage_df <- read_csv(control_stage_path, skip = 21, col_names= c('datetime', 'stage', 'state_of_value')) # don't really need this...

# ------------------ CLEAN DATA ---------------------------

pop_stage <- stage_df %>%
  select(stage)

pop_flow_df <- flow_df %>%
  mutate(datetime = lubridate::dmy_hm(datetime)) %>%
  select(datetime, q) %>%
  bind_cols(pop_stage)
  
head(flow_df)
head(pop_flow_df)
tail(pop_flow_df)

# --------------- PLOT FULL Q TIME SERIES ---------------------
ggplot(pop_flow_df, aes(x=datetime, y=q))+
  geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
  scale_x_datetime(date_breaks = "6 months" , date_labels = "%b-%y") +
  xlab("Date/Time") +
  ylab(expression("Flow   " (m/s^{-1}))) +
  labs(title="Budleigh Brook, Pophams EA gauging station: 07/2009 - 03/2020")+
  theme_bw()


ggsave('./1.1_Clean_Control_Flow/plots/all_flow_pophams.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))

#----------------- SAEV RDS FILE -------------------------------

saveRDS(pop_flow_df, file = "./1.1_Clean_Control_Flow/exports/Pophams_Q_S_ts.rds")


# check rating:
ggplot(pop_flow_df, aes(x = stage, y = q))+
  geom_point(alpha=0.5) +
  # geom_smooth() +
  theme_bw()
#NOTE: strange looking double rating is due to the reconstruction of the weir in 2011

