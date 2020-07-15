##### TITLE: eventExtract_findPeaks ############################################

# Filename: eventExtract_findPeaks.R
# Type: Script
# Title: 
# Version: DRAFT 1.0
# Date: 2018-11-14
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: 
# License: GPL

# Project: eventExtract

##### LOESS PEAK FINDER ########################################################

#==== Loess (Locally Weighted Scatterplot Smoothing) ===========================

# ## set span
# span <- (6*4) / nrow(eventEx)                                                   # 6 hours * 4 records (as 15 min data)

## model loess
modLoess <- (loess(value ~ userID, 
                   data = calc_peaks,
                   span = span))                                                # Loess curve for value (span=0.0003 is 0.03 smoothing span)

## Fit model to data
smoothed <- predict(modLoess,calc_peaks$userID)
calc_peaks$smoothed <- smoothed

## Check these are the same:
head(smoothed)
head(modLoess$fitted)
head(calc_peaks$smoothed)

#.... function from https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data

find_peaks <- function (x, m = 3){                                              # 
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) 
    else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

peakrowID <- find_peaks(calc_peaks$smoothed)

peaks <- setNames(
  data.frame(matrix(ncol = 3, nrow = length(peakrowID))),                       # Name columns of a new dataframe
  c("peakrowID",
    "datetime", 
    "smoothed"))

## Used for rate of change threashold
calc_peaks$diffSmoothed <- c(diff(calc_peaks$smoothed),NA)
calc_peaks$diffSmoothedHr <- as.numeric(rollapply(calc_peaks$diffSmoothed, 
                                               list(c(-3:0)), 
                                               min, fill = NA))
## Identift peaks
calc_peaks$peaks <- F
calc_peaks$peaks[c(peakrowID)] <- T

## Find peaks over median
calc_peaks$peaks.overmedian <- NA
calc_peaks$peaks.overQ30 <- NA
calc_peaks$peaks.overQmag.high <- NA

calc_peaks <- calc_peaks %>%
  mutate(peaks.overmedian =
           ifelse(peaks == T & smoothed >= (Q00$Q50),
                  T, F)) %>%
  mutate(peaks.overQ30 =
           ifelse(peaks == T & smoothed >= (Q00$Q30),
                  T, F)) %>%
  mutate(peaks.overQmag.high =
           ifelse(peaks == T & smoothed >= (as.numeric(Q00[userinput$th_Qmagnitude[1]])),
                  T, F))

## Save dataframe of peaks only
peaks$peakrowID <- peakrowID
peaks$datetime <- calc_peaks$datetime[c(peakrowID)]
peaks$q <- calc_peaks$q[c(peakrowID)]
peaks$smoothed <- smoothed[c(peakrowID)]
peaks$peaks.overmedian <- calc_peaks$peaks.overmedian[c(peakrowID)]
peaks$peaks.overQ30 <- calc_peaks$peaks.overQ30[c(peakrowID)]
peaks$peaks.overQmag.high <- calc_peaks$peaks.overQmag.high[c(peakrowID)]

peaks.overmedian <- subset(peaks, peaks.overmedian == T)
peaks.overQ30 <- subset(peaks, peaks.overQ30 == T)
peaks.overQmag.high <- subset(peaks, peaks.overQmag.high == T)

rm(peakrowID)

#### *** plot checks ***
# 
# # legnd <- c("Event","Rainfall", "Baseflow","Q", "Responce event", "value", "smoothed", "infl")
# # xlim_min <- as.POSIXct(strptime("2018-04-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
# # xlim_max <- as.POSIXct(strptime("2018-04-08 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot
# 
# p1 <- ggplot(calc_peaks) + 
#   geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
#   ylab("Rainfall\nintensity\n(mm h⁻¹)") +
#   scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
#                    date_labels = ("%Y-%m-%d\n%H:%M")) +
#   scale_y_reverse(limits = c()) +
#   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                                   legend.title=element_blank(), 
#                                   legend.position = "none",
#                                   axis.title.x=element_blank()
#                                   ,axis.text.x=element_blank()
#                                   ,axis.ticks.x=element_blank()) +
#   scale_colour_manual(values= c("grey50")) +
#   scale_fill_manual(values = c("cyan","grey50","blue")) +
#   scale_alpha(range=c(0.1))
# 
# p2 <- ggplot(calc_peaks) +
#   geom_line(aes(datetime, value, colour = legnd[6], linetype =legnd[6])) +
#   geom_line(aes(datetime, smoothed, colour = legnd[7], linetype =legnd[7])) +
#   geom_point(data = peaks, aes(datetime, smoothed, colour = legnd[8]), shape = 1) +
#   #geom_point(data = peaks.overmedian, aes(datetime, smoothed, colour = legnd[9]), shape = 16) +
#   geom_point(data = peaks.overQ30, aes(datetime, smoothed, colour = legnd[9]), shape = 16) +
#   xlab ("Date") +
#   ylab (paste0("Flow\n(m³ s⁻¹)")) +
#   scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
#                    date_labels = ("%Y-%m-%d\n%H:%M")) +
#   #scale_y_continuous(limits = c(0,50)) +
#   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                      legend.title=element_blank(),
#                      legend.position = "bottom") + #(base_size = 12) +          # blank element removes background to legend
#   scale_colour_manual(values= c( "blue", "blue", "black", "red")) +
#   scale_linetype_manual(values= c("solid", "solid"))
# 
# # plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
# 
# # rm(p1, p2, xlim_min, xlim_max)
# 
# #----  Rolling difference over preceeding hour (rate of change threashold) -----
# 
# p3 <- ggplot(calc_peaks) +
#   geom_point(aes(datetime, diffSmoothedHr), shape = 1) +
#   scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
#                    date_labels = ("%Y-%m-%d\n%H:%M"))+
#   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                      legend.title=element_blank(),
#                      legend.position = "bottom")
# 
# # p.peaks <- plot_grid(p1, p2, p3, nrow = 3, align = "v", rel_heights = c(0.5, 1.5, 1))
# # 
# # rm(p1, p2, p3)

