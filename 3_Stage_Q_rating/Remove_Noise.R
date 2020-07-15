# Attempt at filtering Q to remove noise at low flows...


library(tidyverse)
library(lubridate)
library(robfilter)
library(zoo)
library(quantreg)
library(caTools)
library(patchwork)
Q_data_path <- "./3_Stage_Q_rating/exports/EB_Q_Stage_ts.rds"

Q.in.df <- read_rds(Q_data_path)


ggplot(Q.in.df, aes(x=datetime, y=q))+
  geom_line(colour='#567AC2', size = 0.1, alpha = 0.8)+
  scale_x_datetime(date_breaks = "6 months" , date_labels = "%b-%y") +
  # ylim(c(0,7000))+
  xlab("Date/Time") +
  ylab("Q (l/s)") +
  labs(title="Budleigh Brook, No filter: 07/2009 - 03/2020")+
  theme_bw()



Q.out.df <- Q.in.df %>%
  mutate(q_zero = ifelse(is.na(q), 0, q)) %>%
  mutate(q.weights = (q - min(!is.na(q)))/
                        (max(!is.na(q)) - min(!is.na(q)))) %>%
  # filter(datetime > '2020-01-01 00:15:00' & datetime < '2020-03-27 00:15:00')%>%
  mutate(q_rol = rollapply(q, width=30, FUN=mean,  fill=NA)) %>%
  mutate(q_q75 = runquantile(q, k=50, endrule = 'NA',  probs=0.75)) %>%
  mutate(q_q25 = runquantile(q, k=50, endrule = 'NA',  probs=0.25))%>%
  mutate(q_gap = as.numeric(q_q75-q_q25))%>%
  mutate(q_gr = runquantile(q_gap, k=2920, endrule = 'constant',  probs=0.7))%>%
  mutate(denoise_q = ifelse(q_gap > q_gr, q, q_rol))



a = ggplot(Q.out.df, aes(x=datetime)) +
  geom_line(aes(y=q), colour='grey80') +
  geom_line(aes(y=denoise_q), colour='#062E47', lwd = 0.5, linetype=1, alpha=0.5) +
  theme_bw()
# a

b =ggplot(Q.out.df, aes(x=datetime, y=q_gap)) +
  geom_line(colour='grey80') +
  geom_line(aes(y=q_gr), colour='#062E47', lwd = 0.8, linetype=1) +
  scale_y_continuous(limits = c(0,500), breaks = seq(0,500, by = 50)) +
  theme_bw()

# a/b

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
    geom_line(colour='red', size = 0.1, alpha = 0.8)+
    geom_line(aes(y=denoise_q), colour='#062E47', lwd = 0.5, linetype=1, alpha=0.9) +
    # scale_x_datetime(date_breaks = "1 month" , date_labels = "%d-%m")+
    theme_bw()+
    theme(text = element_text(size=10))
}

checkplot(Q.out.df, '2020-02-14 22:00:00', '2020-02-18 00:00:00') # storm Dennis
checkplot(Q.out.df, '2014-02-01 22:00:00', '2014-02-28 00:00:00') #  problematic period...
checkplot(Q.out.df, '2013-02-01 22:00:00', '2013-02-28 00:00:00') #  problematic period...
checkplot(Q.out.df, '2012-11-24 00:00:00', '2012-11-27 00:00:00') # Biggest recorded Event

#---------- SAVE NOISE-REMOVED Q DATA ---------------

Noise.rem.Q <- Q.out.df %>%
  rename(q_orig = q)%>%
  dplyr::select(datetime, stage, denoise_q, q_orig) %>%
  rename(q=denoise_q) %>%
  mutate(q = ifelse(is.na(q), q_orig, q))
    


saveRDS(Noise.rem.Q, file = "./3_Stage_Q_rating/exports/EB_Q_Stage_ts_NR.rds")


