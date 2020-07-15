# quick script to join the two blocks of rainfall data (I didn't want to have to run the entire record again :)

library(tidyverse)
library(lubridate)

Rain_block1 <- read_csv("./4_Join_Rain_to_Q/rain_data_in/15_min_data/AOI_200907090000_201904040900.csv")
Rain_block2 <- read_csv("./4_Join_Rain_to_Q/rain_data_in/15_min_data/AOI_15_min_201904040900_202003170400.csv")


rain1_clean <- Rain_block1 %>%
  rename(rain_intensity_mmhr = mean_rain_rate_mm_hr) %>%
  select(datetime, rain_intensity_mmhr)

tail(rain1_clean)

rain2_clean <- Rain_block2 %>%
  rename(datetime = date_time) %>%
  filter(datetime <= ymd_hms('2020-03-11 00:00:00'))%>%
  select(datetime, rain_intensity_mmhr)

rainfall_combined <- bind_rows(rain1_clean, rain2_clean)

head(rainfall_combined)
tail(rainfall_combined)

saveRDS(rainfall_combined, file = './4_Join_Rain_to_Q/rain_data_in/BudBrook_Rain_09_20.rds')

ggplot(rainfall_combined, aes(x=datetime, y=rain_intensity_mmhr)) +
  geom_line(colour = "steelblue3", alpha=0.8) +
  xlab('Date/Time') +
  ylab('Rainfall (mm/hr)') +
  labs(title = 'Budleigh Brook Rainfall - extracted from MetOffice Radar Data') +
  theme_bw()

ggsave('./4_Join_Rain_to_Q/plots/rainfall_timeseries.jpg', 
       width = 30,
       height = 15,
       units = c("cm"))  

