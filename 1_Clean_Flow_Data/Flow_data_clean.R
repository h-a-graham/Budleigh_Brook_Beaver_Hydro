# Script to load, clean and save usable flow data from deployment of flow sensor at Hayes Lane during the Winter of 2019-2020.

# ---------- INSATALL AND LOAD PACKAGES -------------------

library(tidyverse)
library(lubridate)

# ------------------- READ DATA  --------------------------

flow_data_raw1 <- '1_Clean_Flow_Data/OTTERXXX.TXT'
flow_data_raw2 <- '1_Clean_Flow_Data/OTTERXXX_round2.TXT'


flow_df1 <- read_tsv(flow_data_raw1, skip = 8)

flow_df2 <- read_tsv(flow_data_raw2, skip = 8)

# ------------------ CLEAN DATA ---------------------------

#function to clean the messy Flow data
clean_flow_df <- function(df){
  df %>%
  add_column(date_time= "", .after = 2)%>%
  mutate(date_time = lubridate::dmy_hms(paste(date, time)))%>%
  mutate(date = lubridate::dmy(date)) %>%
  mutate(time = lubridate::hms(time)) %>%
  rename(depth_m = names(.)[4]) %>%
  mutate(depth_m = as.numeric(gsub(",", ".", gsub("\\.", "", depth_m)))) %>%
  rename(velocity_ms = names(.)[5]) %>%
  mutate(velocity_ms = as.numeric(gsub(",", ".", gsub("\\.", "", velocity_ms)))) %>%
  rename(flow_ls = names(.)[6]) %>%
  mutate(flow_ls = as.numeric(gsub(",", ".", gsub("\\.", "", flow_ls)))) %>%
  mutate(flow_ls = na_if(flow_ls, 0)) %>%
  rename(Temp_c = names(.)[7]) %>%
  mutate(Temp_c = Temp_c/10) %>%
  rename(ADC_1_m = names(.)[8]) %>%
  rename(ADC_2_mA = names(.)[9]) %>%
  mutate(ADC_2_mA = ADC_2_mA/1000) %>%
  rename(ADC_3_v = names(.)[10]) %>%
  mutate(ADC_3_v = ADC_3_v/1000)%>%
  rename(ADC_4_m = names(.)[11]) %>%
  select(-12)
} 

flow_df1 <- clean_flow_df(flow_df1)
flow_df2 <- clean_flow_df(flow_df2)

# ----------------- SELECT MAIN COLUMNS --------------------
main_columns <- function(df){
  df %>%
    select(c(date_time, depth_m, velocity_ms, flow_ls))
}

FLOW.df1 <- main_columns(flow_df1)
FLOW.df2 <- main_columns(flow_df2)

# --------------------- CHECK DATA -------------------------

# simple plotting function...
all_plot_func <- function(df){
  ggplot(df, aes(x=date_time, y=flow_ls)) +
    geom_line(colour='#567AC2')+
    theme_bw()
}

all_plot_func(FLOW.df1)
all_plot_func(FLOW.df2)


# ------------------- REMOVE JUNK DATA ----------------------

# Function to get the index of a dataframe by data-time character
idx_from_date <- function(df, date_chr){ 
  df %>% rowid_to_column("idx")%>%
  filter(date_time  == date_chr) %>%
  pull(1)
}

# Function to return slice of df based on start and end date-time characters
clip_data <- function(df, start, end){
  start_id <- idx_from_date(df, start)
  end_id <- idx_from_date(df, end)
  return(df %>% slice(start_id:end_id))
}

FLOW.CLIP.df1 <- clip_data(FLOW.df1, "2019-12-10 12:30:00", "2019-12-19 17:00:00")

FLOW.CLIP.df2 <- clip_data(FLOW.df2, "2020-02-21 15:00:00", "2020-02-29 04:00:00")

# ----- Plot and export img of two flow time periods -------------
all_plot_func(FLOW.CLIP.df1)
ggsave(filename = "1_Clean_Flow_Data/exports/EB_Flow_round1.jpg" )
all_plot_func(FLOW.CLIP.df2)
ggsave(filename = "1_Clean_Flow_Data/exports/EB_Flow_round2.jpg" )

# ------- Save flow time periods as RDS files ---------
saveRDS(FLOW.CLIP.df1, file = "1_Clean_Flow_Data/exports/EB_Flow_round1.rds")
saveRDS(FLOW.CLIP.df2, file = "1_Clean_Flow_Data/exports/EB_Flow_round2.rds")


