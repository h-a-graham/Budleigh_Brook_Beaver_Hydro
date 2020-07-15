# Script to read clean and sort all stage data for Hayes Lane Gauging Station - East Budleigh

# ---------- INSATALL AND LOAD PACKAGES -------------------

library(tidyverse)
library(lubridate)
library(quantreg)

# ------------------- DEFINE DATA PATHS  --------------------------

EB_Stage1 <- './2_Join_Clean_Stage_Data/EnvAgency_Data_EastBudleigh/EastBudleighStream_SG_POR_csv.csv'
EB_Stage2 <- './2_Join_Clean_Stage_Data/EnvAgency_Data_EastBudleigh/East Budleigh Hayes Lane.2017 - date.csv'
EB_Stage3 <- './2_Join_Clean_Stage_Data/EnvAgency_Data_EastBudleigh/Hayes Lane 11.2018 - 03.2019.csv'
EB_Stage4 <- './2_Join_Clean_Stage_Data/EnvAgency_Data_EastBudleigh/East Budleigh Hayes Lane 2019 -2020.csv'


# ------------------ SET OF FUNCTIONS TO LOAD AND CLEAN DATA FROM EA --------------

# Function to Read and clean data into tidy format.
read_n_clean1 <- function(path){
  read_csv(path, skip = 20, na = c('NA', '---', '')) %>%
    mutate(date_time = lubridate::dmy_hms(`Time stamp`))%>%
    rename(depth_m = `Value[m]`) %>%
    rename(Quality = `State of value`)%>%
    select(date_time, depth_m, Quality, Comments)
}

# Function to Read and clean data into tidy format.
read_n_clean2 <- function(path){
  read_csv(path, skip = 20, na = c('NA', '---', '')) %>%
    mutate(date_time = lubridate::dmy_hms(paste(Date, Time)))%>%
    rename(depth_m = Value) %>%
    add_column(Comments= "")%>%
    select(date_time, depth_m, Quality, Comments)
}

#function 3 to read and clean data
read_n_clean3 <- function(path){
  read_csv(path, skip = 20, na = c('NA', '---', '')) %>%
    mutate(date_time = lubridate::dmy_hm(`Time stamp`))%>%
    rename(depth_m = `Value[m]`) %>%
    rename(Quality = `State of value`)%>%
    select(date_time, depth_m, Quality, Comments)
}

# ----------------- READ DATA ------------------------

EB_df1 <- read_n_clean1(EB_Stage1)
EB_df2 <- read_n_clean2(EB_Stage2)
EB_df3 <- read_n_clean3(EB_Stage3)
EB_df4 <- read_n_clean3(EB_Stage4)


# ----- SLICE AND JOIN DATA --------

# Function to get the index of a dataframe by data-time character
idx_from_date <- function(.data, date_chr){ 
  .data %>% rowid_to_column("idx")%>%
    filter(date_time  == date_chr) %>%
    pull(1)
}

# Function to return slice of df based on start and end date-time characters
clip_data <- function(df, start, end){
  start_id <- idx_from_date(df, start)
  end_id <- idx_from_date(df, end)
  return(df %>% slice(start_id:end_id))
}


# Clip data frames to remove any overlap in time
EB_df2 <- clip_data(EB_df2, '2017-02-13 04:15:00', '2018-10-23 04:00:00' ) 
EB_df3 <- clip_data(EB_df3, '2018-11-01 00:00:00', '2018-12-31 23:45:00' )
EB_df4 <- clip_data(EB_df4, '2019-01-01 00:00:00', '2020-03-17 04:00:00' )


# join all data frames and fill time gaps
EB_df_all <- bind_rows(list(EB_df1, EB_df2, EB_df3, EB_df4)) %>%
  complete(date_time = seq.POSIXt(min(date_time, na.rm = TRUE), max(date_time, na.rm = TRUE), by = "15 mins")) %>% 
  filter_all(any_vars(!is.na(.)))


# ---------- PLOT ALL NON-CORRECTED STAGE -------------------
ggplot(EB_df_all, aes(x=date_time, y=depth_m))+
  geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
  coord_cartesian(ylim = c(0.4, 0.7)) +
  scale_x_datetime(date_breaks = "6 months" , date_labels = "%b-%y") +
  theme_bw()

ggsave('./2_Join_Clean_Stage_Data/plots/all_dirty_plot.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))

# -------------- INVESTIGATIVE PLOTTING.... ---------------
# Plots to identify areas of maintenance/desilting works that will allow for corrections to
# time series by removing drift.

checkplot <- function(df, s.time, e.time){
  clip_data(df, s.time, e.time) %>%
    ggplot(aes(x=date_time, y=depth_m))+
    geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
    # scale_x_datetime(date_breaks = "1 day" , date_labels = "%d-%m")+
    theme_bw()+
    theme(text = element_text(size=10))
}

checkplot(EB_df_all, '2019-07-31 22:00:00', '2019-08-20 00:00:00')
checkplot(EB_df_all, '2019-03-15 22:00:00', '2019-06-01 00:00:00')
checkplot(EB_df_all, '2017-07-01 00:00:00', '2017-08-07 00:00:00')
checkplot(EB_df_all, '2016-07-10 00:00:00', '2016-09-10 00:00:00')
checkplot(EB_df_all, '2014-07-01 00:00:00', '2014-08-15 00:00:00')
checkplot(EB_df_all, '2013-08-18 00:00:00', '2013-10-15 00:00:00')
checkplot(EB_df_all, '2012-06-10 00:00:00', '2012-07-25 00:00:00')
checkplot(EB_df_all, '2011-05-20 00:00:00', '2011-12-20 00:00:00') 
checkplot(EB_df_all, '2010-10-01 00:00:00', '2010-12-05 00:00:00')


# --------- REMOVE JUNK AND ALIGN ------------------

# These linked functions do the following: (1) Delete crap data that occurs during gauge maintenance at requested points
# and (2): gets the stage values for the requested start and end point of a break in the series -
# typically these breaks occur where the stilling pond is desilted or there is maintenance. After each
# maintenance period the correct stage is assumed and a correction factor is calcualted based on the unaltered 
# dataframe. This adjustment is then applied for the preceeding time period until the next 'break'.

align_series <- function(.data, s.time, e.time, b.time){
  s.idx <- idx_from_date(.data=.data, s.time)
  e.idx <- idx_from_date(.data=.data, e.time)
  
  target_depth <- EB_df_all %>% 
    filter(date_time  == ymd_hms(e.time)) %>%
    pull(depth_m)
  
  move_depth <- EB_df_all %>% 
    filter(date_time  == ymd_hms(s.time)) %>%
    pull(depth_m)
  
  diff <- target_depth - move_depth
  
  .data %>%
    mutate(depth_m = case_when(date_time <= ymd_hms(s.time) & date_time >= ymd_hms(b.time) ~ depth_m + diff,
                               TRUE ~ depth_m)) 
}

remove_drift <- function(.data, s.time, e.time, b.time, align){
  if (missing(align)){
    align <- TRUE
  }
  
  if  (align == TRUE) {
  .data %>%
    mutate(depth_m = case_when(date_time > ymd_hms(s.time) & date_time < ymd_hms(e.time) ~ NA_real_,
                               TRUE ~ depth_m)) %>%
    align_series(s.time=s.time, e.time=e.time, b.time=b.time)
  }
  else{
    .data %>%
      mutate(depth_m = case_when(date_time > ymd_hms(s.time) & date_time < ymd_hms(e.time) ~ NA_real_,
                                 TRUE ~ depth_m))
  }
}

# Pipe to delete junk and align time series...
EB_df_fix  <- 
  remove_drift(EB_df_all, '2019-08-01 22:00:00', '2019-08-15 21:15:00','2019-05-24 21:30:00') %>% 
  remove_drift('2019-03-20 05:00:00', '2019-05-24 21:30:00', '2017-07-28 00:00:00') %>%
  remove_drift('2017-07-10 14:00:00', '2017-07-28 00:00:00', '2016-08-23 16:15:00') %>% 
  remove_drift('2016-07-28 17:30:00', '2016-08-23 16:15:00', '2014-07-31 20:15:00') %>%
  remove_drift('2014-07-10 18:30:00', '2014-07-31 20:15:00', '2013-10-10 18:15:00') %>%
  remove_drift('2013-08-20 17:00:00', '2013-10-10 18:15:00', '2012-07-19 18:15:00') %>%
  remove_drift('2012-06-18 17:30:00', '2012-07-19 18:15:00', '2011-08-02 17:30:00') %>%
  remove_drift('2011-07-11 19:30:00', '2011-10-31 18:00:00', '2009-07-09 14:15:00') %>%
  remove_drift('2010-10-14 00:00:00', '2010-11-25 00:00:00', '2009-07-09 14:15:00', FALSE)

  # remove_drift('2011-07-11 19:30:00', '2011-08-02 17:30:00', '2010-11-25 00:00:00') %>%
  # remove_drift('2010-10-14 00:00:00', '2010-11-25 00:00:00', '2009-07-09 14:15:00') 



# Check plots to see alignment looks sensible...
checkplot(EB_df_fix, '2019-07-31 22:00:00', '2019-08-20 00:00:00')
checkplot(EB_df_fix, '2019-03-15 22:00:00', '2019-06-01 00:00:00')
checkplot(EB_df_fix, '2017-07-01 00:00:00', '2017-08-07 00:00:00') 
checkplot(EB_df_fix, '2016-07-10 00:00:00', '2016-09-10 00:00:00') 
checkplot(EB_df_fix, '2014-07-01 00:00:00', '2014-08-15 00:00:00')
checkplot(EB_df_fix, '2013-08-18 00:00:00', '2013-10-15 00:00:00')
checkplot(EB_df_fix, '2012-06-10 00:00:00', '2012-07-25 00:00:00')
checkplot(EB_df_fix, '2011-05-20 00:00:00', '2011-12-20 00:00:00') 
checkplot(EB_df_fix, '2010-10-01 00:00:00', '2010-12-05 00:00:00')


# Use Quantile regression of basefloe (Q95) to remove drift in time series.
# EB_df_fix <- EB_df_fix %>% 
#   rowid_to_column("idx")
# 
# Q95_qr <- rq(depth_m ~ idx, data = EB_df_fix, tau = 0.05)
# Q95_qr.int <- as.numeric(as.character(format(coef(Q95_qr)[1], digits = 10)))
# Q95_qr.c <- as.numeric(as.character(format(coef(Q95_qr)[2], digits = 10)))
# 
# target_offset <- Q95_qr.int + max(EB_df_fix$idx)* Q95_qr.c
# 
# 
# 
# EB_df_fix <- EB_df_fix %>% 
#   mutate(adj_factor = target_offset - (Q95_qr.int + idx *Q95_qr.c)) %>%
#   mutate(depth_m_fix = depth_m + adj_factor) %>%
#   rename(depth_remove = depth_m) %>%
#   rename(depth_m = depth_m_fix) %>%
#   select(date_time, depth_m, Quality, Comments)




# ---------- PLOT ALL CORRECTED STAGE -------------------
ggplot(EB_df_fix, aes(x=date_time, y=depth_m))+
  geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
  scale_x_datetime(date_breaks = "6 months" , date_labels = "%b-%y") +
  xlab("Date/Time") +
  ylab("Depth (m)") +
  labs(title="Budleigh Brook, Hayes Lane EA gauging station: 07/2009 - 03/2020")+
  theme_bw()

ggsave('./2_Join_Clean_Stage_Data/plots/all_stage_clean_plot.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))

# ----------- SAVE CLEANED DATA FRAME AS RDS ----------------

saveRDS(EB_df_fix, file = "./2_Join_Clean_Stage_Data/exports/EB_Stage_Clean.rds")



