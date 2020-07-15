# setwd('C:/HG_Projects/East_Bud_Hydrology_Proj')
proj_root <- 'C:/HG_Projects/East_Bud_Hydrology_Proj'
ev_extr_root <- 'C:/HG_Projects/East_Bud_Hydrology_Proj/5_event_extraction/eventExtraction__beaver'
current_wd <- getwd()
if (current_wd == proj_root) {
  print('Updating Working Directory...')
  setwd('./5_event_extraction/eventExtraction__beaver')
} else if (current_wd == ev_extr_root){
  print('working dir already set correvtly...')
} else {
  warning("argh! The working directory isn't set correctly importing modules may fail!!!")
}

print(getwd())


##### TITLE: eventEx_00_RUN ##################################################

# Filename: eventEx.R
# Type: Script
# Title: 
# Version: DRAFT 1.0
# Date: 2018-11-14
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: 
# License: 
# Project: eventEx

##### NOTES: ###################################################################
# 
# * in headings indicates user input REQUIRED
# block comments can be turned on/off by selecting code and [Ctrl]+[SHIFT]+[c]

# Dataset example optimised for: SPOONERS flow (Mires UoE) with kinsford gate rainfall (EA TBR)
# 
# DESCRIPTION:
# Automated rainfall-response (rainfall-runoff) event seperation for high frequency timeseries (15 min)
# Seperated using quick flow component - slow flow/baseflow removed.
# 
# Event classification criteria.
# D.J.Luscombe (2014) Understanding the ecohydrology of shallow, drained and marginal blanket peatlands,
# Doctor of Philosophy In Geography. University of Exeter. Exeter, UK. pp124.
# 
# BUGS:
#     how to deal with en event that startes befor the rainfall event (e.g. if rainfall is missing) ... this then extends the event back to previous event
#     N.B. NA  records can act as limits to rainfall events (i.e. they will isolate, start or end an event)
#
##### CODE: ####################################################################  

#==== A. Set up for R ==========================================================

#---- > A.1 Source library and function scrips -----------------------------------

if(!exists("foo", mode = "function")) source("eventEx_libraries.R")
if(!exists("foo", mode = "function")) source("eventEx_functions.R")

## set up folders
if(!dir.exists(paste0("plots"))) dir.create(paste0(paste0("plots")))
if(!dir.exists(paste0("working"))) dir.create(paste0(paste0("working")))
if(!dir.exists(paste0("data"))) dir.create(paste0(paste0("data")))
if(!dir.exists(paste0("ARCHIVED"))) dir.create(paste0(paste0("ARCHIVED")))


#---- >*A.2 Set user defined variables -----------------------------------------

#.... *** START manual user input *** ..........................................

input_site <- list(
  
  ## ***site name (required)
  site = "EastBud",                                                                 # this is used for file and plot naming
  
  ## ***Timeseries details (frequency, start, end) (required)
  ##  (e.g. use data at 15 min frequency for hydrological year 2017-2018)
  tsfrq = 15,                                                                   # frequency of data (in minutes)
  ts1 = as.POSIXct("2009-07-09 14:15:00", tz = "UTC"),                             # timeseries start (TO DO default: first timestamp avaiable) 
  tsn = as.POSIXct("2020-03-11 00:00:00", tz = "UTC")                              # timeseries end (TO DO default: last timestamp avaiable)
)

input_baseSep <- list(
  
  ## for RDF baseflow seperation
  alpha = 0.98,                                                                 # in range 0.9-0.95 (Nathan and McMahon, 1990) or for higher frequency timeseries 0.98
  pad = 30*24*60/input_site$tsfrq, #(default to 30 days *24 hours/15 mins)      # number of reflected valued used in padding (at each end)
  
  ## number of passes for baseflow estimation 
  ## use 5 for headwater cathments. Higher numbers for larger catchments
  passesbase = 5,                                                               # *MUST BE ODD NO* set number of passes (affects degree of smoothing and phase distortion)
  
  ## number of passes for filter value seperation (used for event ID)
  ## use 3 for headwater catchments. Use 5 or 7 for larger catchments.
  passesfilter = 3                                                             # *MUST BE ODD NO* set number of passes (affects degree of smoothing and phase distortion)
) 

## Other threasholds set here
input_thresholds <- list(
  
  rainLinkSpan = 6,
  
  th_QslopeAsc = .95,
  th_QslopeDec = .10,   #.05,  
  th_Qbase = "Q95",
  th_Qmagnitude = c("Q10", "Q95")
)

# ## BFI (optional input for published BFI, if non avaiable use 0.00)
# input_baseSep$publishedBFI <- 0.00 # e.g. Exe at Stoodleigh (0.51)            # not activly used (slowflow seperation limited by pass number)

## combine input lists
listof_input <- list(input_site,input_baseSep, input_thresholds)
userinput <- do.call(c, listof_input)

## remove seperate input files
rm(list = ls(pattern = "^input")) 

useroutput <- userinput[c("site", 
                          "tsfrq", 
                          "ts1", 
                          "tsn")]


#.... *** END manualuser input *** ............................................. 

## use ts1 and tsn to set period of interest
datetime <- seq.POSIXt(userinput$ts1,                                           # start date time for sequence
                       userinput$tsn,                                              # end date time for sequence
                       paste0(userinput$tsfrq," min"))                             # increment (e.g. 15 mins)

datetime <- data.frame(datetime)                                                # assign ts.UTC as a dataframe to ts
str(datetime)


#==== 1. SET UP ================================================================

# Data structure required:
# [,’datetime’] date (format yy-mm-dd HH:MM:SS or dd/mm/yy HH:MM)               # as POSIXct or factor
# (?) [,’stage’] river stage (m)                                                # as num (?) OPTIONAL: may not be used
# [,’q’] discharge (m³/s)                                                       # as num
# [,’rainfall’] rainfall intensity (mm/h)                                       # as num


#---- >*1.0 Import data from existing rds --------------------------------------

## read in RDS file (opens a pop up)
#dat <- readRDS(file.choose())

dat <- readRDS('C:/HG_Projects/East_Bud_Hydrology_Proj/4_Join_Rain_to_Q/exports/EastBud_Q_R_S_ts.rds')

#---- >*1.1 Data checks and setup ----------------------------------------------

## print to console checks on the data
str(dat)                                                                        # get structure of dataframe
head(dat)                                                                       # get head of dataframe
tail(dat)                                                                       # get tail of dataframe

summary(dat[,c("q", "rainfall")])                                               # get summary of data (min max quartiles NAs etc.)

## ***y limit for rainfall plots. Max rainfall rounded up to the nearest 2
ylimit1 <- max(dat$rainfall, na.rm = T) + (2 - max(dat$rainfall, na.rm = T)%% 2)
ylimit1 <- 30 # OVERRIDES DEFAULT

## ***y limit for flow plots. Max flow rounded up to the nearest 2
# ylimit2 <- max(dat$q, na.rm = T) + (2 - max(dat$q, na.rm = T)%% 2)
ylimit2 <- 2 # OVERRIDES DEFAULT

#---- > 1.2 Create event seperation data frame ---------------------------------

names(dat)                                                                      # visual check for column names

colNames <- setNames(data.frame(matrix(ncol = 16, nrow = 0)),                   # Name columns of a new dataframe
                     c("userID",                                                #
                       "datetime",                                              # 
                       "rainfall",                                              #
                       "sg",                                                    #
                       "q",                                                     #
                       "baseflow",                                              #
                       "stormflow",                                             #
                       "storminterflow",                                        #
                       "stormquickflow",                                        #
                       "value",                                                 #
                       #"event",                                                #
                       #"eventID",                                              #
                       "event.temp",
                       "eventID.temp",                                          #
                       "eventnew",
                       "eventIDnew",                                            #
                       "response.event",                                        #
                       "response.eventID"))                                     #     

## For using Q only
eventEx <- bind_rows(colNames,                                                  # bind dataframe  one on top of the other ...colNames and ...
                     dat[,c("datetime",                                         # ...date - using only specified columns
                            "rainfall",
                            "q")],
                     .id = NULL)                                                # bind dataframes one on top of the other (do not add ID column)

# ## with stage added toggle this line of code on by select [Ctrl]+[SHIFT]+[c]
# eventEx$sg <- dat3$value

# ---- > 1.3 Set User ID (row number) --------------------------------------------

eventEx$userID <- 1:nrow(eventEx)                                               # create user ID (row number for subset used)

## check structure
str(eventEx)                                                                    # check structure


# ---- > 1.4 Set outputs dataframe (row number) --------------------------------

outputs <- setNames(
  data.frame(matrix(ncol = 13, nrow = nrow(eventEx))),                           
  c("datetime",                                                                 # time stamp
    "q_m3_s",
    "rainfall_mm_h",
    "baseflow_m3_s",                                              
    "stormflow_m3_s",                                                 
    "storminterflow_m3_s",                                        
    "stormquickflow_m3_s",                                        
    "rainfall.event",
    "rainfall.eventID",
    "response.event",
    "response.eventID",
    "event",
    "eventID"))

outputs$datetime <- eventEx$datetime
outputs$q_m3_s <- eventEx$q

# ---- > 1.5 Set value to Q ----------------------------------------------------

## Use Q for baseflow seperation initialising value
eventEx$value <- eventEx$q

# ---- > 1.6 PLOT: inputs ----------------------------------------------------

ts1 <- userinput$ts1
tsn <- userinput$tsn

## or specify the start and end times for the plot
xlim_min <- ts1
xlim_max <- tsn

legnd <- c("Event","Rainfall", "Baseflow","Q", 
           "Event (rain)", "Event (response)", "Event (flow sep.)", "value")
update_geom_defaults("line", list(size = 0.2))

p1 <- ggplot(eventEx) + 
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), 
           width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()
                                  ,axis.text.x=element_blank()
                                  ,axis.ticks.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c("grey50"))

p2 <- ggplot(eventEx) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "none") +                 # blank element removes background to legend
  scale_colour_manual(values= c("black" )) +
  scale_linetype_manual(values= c("solid" ))

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))

## plot to file
tiff(paste0("plots/ts_01_eventEx_input_",
            userinput$site, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

print(p)



rm(p, p1, p2)

#.... > Save input values (if required) ........................................

# write.csv(eventEx, paste0("./working/eventEx_00_input_precalc_eventEx_", userinput$site, "_", testtime, ".csv"))
# saveRDS(eventEx, paste0("./working/eventEx_00_input_precalc_eventEx_", userinput$site, "_", testtime,  ".rds"))

#.... tidy R enviroment ........................................................

rm(colNames)
# rm(filepath) 
rm(dat) 
# rm(datwide, datlong)
# rm(testtime)
# rm(dat)

## possible bugs:
# > manually chose file for read.csv(file.choose())
# > date format must be (dd/mm/yy HH:MM) "%d/%m/%Y %H:%M"
# > Columns names required "datetime","rainfall","sg", "q" 

#---- > 1.6 Outlier Filtering --------------------------------------------------

## Removes outliers in Q - use if required (recommended)

#.... Generalised ESD filter (flow) ............................................

## if required, remove outliers from q value

## set window size (e.g. 9)
# (for 15 min data 4records+recordofinterest+4records is window of 1 hr befor a 1 hr afterwards)
w <- 4+1+4

## set up calculationg data.frame
calc <- eventEx[,c("datetime", "q", "baseflow")]

## Using q
calc$value <- eventEx$q


#.... i. Apply filter ..........................................................

## apply rolling generalised ESD (also called 3 sigma outlier test)
calc <- calc %>% 
  mutate(roll.mean = zoo::rollapplyr(data = value, 
                                     width = w, 
                                     mean, na.rm = T,
                                     fill = NA,
                                     align = "center")) %>%  
  mutate(roll.sd = zoo::rollapplyr(data = value, 
                                   width = w, 
                                   sd, na.rm = T,
                                   fill = NA,
                                   align = "center")) %>% 
  mutate(roll.mean = ifelse(is.nan(roll.mean), NA, roll.mean)) %>%  
  
  # gap fill for adjacent values to prevent threshold disappeasing during data gaps
  fill(roll.mean, roll.sd, .direction = "up") %>%
  fill(roll.mean, roll.sd, .direction = "down") %>% 
  
  # calculate the rolling threasholds (low and high) for the filter
  # for 3 standard deviations (typically used 3 sigma outlier test)
  mutate(roll.genesd.thlow = roll.mean - (3*roll.sd)) %>% 
  mutate(roll.genesd.thhigh = roll.mean + (3*roll.sd))


#.... ii. Lable outliers .......................................................

## generate lables for outliers
calc$code <- ifelse(is.na(calc$value), NA,
                    ifelse(is.na(calc$roll.mean), "NOTCALC",
                           ifelse(calc$value < calc$roll.genesd.thlow| 
                                    calc$value > calc$roll.genesd.thhigh, "OUTLIER",
                                  "OK")))

## remove values with the error flag TRUE (if is not OK)
calc$removed <- as.numeric(ifelse(calc$code != "OK", calc$value, NA))

## keep working values with the error flag FLASE (if is OK)
calc$working <- as.numeric(ifelse(calc$code == "OK", calc$value, NA))

#.... iii. Plot to check .......................................................

## X limits for total period of interest
xlim_min <- userinput$ts1 #as.POSIXct(strptime("2018-02-15 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
xlim_max <- userinput$tsn #as.POSIXct(strptime("2018-03-20 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot

p <- ggESDcheck(calc)

## save plot to file
tiff(paste0("plots/","ts_01_eventEx_input_outlierfilter_",
            userinput$site, "_", 
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
#width = 210, height = 297, units = 'mm', res = 600) # A4
print(p)
dev.off()

## plot to R studio
print(p)

## If ok - update value
eventEx$value <- calc$working

## no. values removed
useroutput$q_noutliersremoved <- sum(!is.na(calc$value)) - sum(!is.na(calc$working))

rm(calc, w)


#---- > 1.7 Completness ratios for input data  -----------------------------------

## completeness ratio (full record of interest)
useroutput$rain_completenessratio <-  sum(!is.na(eventEx$rainfall))/length(eventEx$rainfall)

## completeness ratio (full record of interest)
useroutput$q_completenessratio <-  sum(!is.na(eventEx$value))/length(eventEx$value)

message(  "For the full record:\n\nCount n outliers removed (flow) = ",
          useroutput$q_noutliersremoved,
          "\n\nCompleteness ratio (rain) = ", 
          useroutput$rain_completenessratio,
          "\nCompleteness ratio (flow) = ", 
          useroutput$q_completenessratio
)


#==== 2. BASEFLOW FILTER for Q =================================================

## Single Parameter Digital Filter with timeseries padding and multiple passes


#*******************************************************************************
#---- > 2.1 Manually set run options for baseSep ---------------------------------

message(  "Baseflow estimation RDF parameters:\n",
          
          "\nSite = ", userinput$site ,
          "\nStart time = ", userinput$ts1,
          "\nEnd time = ", userinput$tsn,
          "\nRDF value for alpha = ", userinput$alpha,
          "\nNumber of reflected padding records = ", userinput$pad,
          "\nNumber of filter passes (alternating directions, must be odd) = ", userinput$passesbase
          
)


## Can be changed to do multiple run tests. Default runs in baseflowSep code = 1 
runs <- 1

## call period of interest
ts1 <- userinput$ts1
tsn <- userinput$tsn

## call sitename
site <- userinput$site  

## call baseflow seperation user defined variables (defaults below)
alpha <- ifelse( !exists("alpha", where = userinput) | is.na(userinput$alpha),
                 0.98, userinput$alpha)  

pad <- ifelse( !exists("pad", where = userinput) | is.na(userinput$pad),
               2880, userinput$pad)  

passes <- ifelse( !exists("passesbase", where = userinput) | is.na(userinput$passesbase),
                  9, userinput$passesbase)  

publishedBFI <- ifelse( !exists("publishedBFI", where = userinput) | 
                          is.na(userinput$publishedBFI),
                        0.00, userinput$publishedBFI)  


#*******************************************************************************

#---- > 2.2 Run filter -----------------------------------------------------------

## set working dataframe dat
dat <- eventEx

## run filter for i-1 (so 15 min) 
if(!exists("foo", mode = "function")) source("eventEx_02_baseSep_rdf_lyneHollick_fb_20191216.R") # run baseflow sep code


#---- > 2.3 Baseflow outputs ---------------------------------------------------

## Save Baseflow sep output to main event Ex calculation dataframe
eventEx$baseflow <- baseSep$bconstrained                                        # update eventEx dataframe baseflow
eventEx$stormflow <- eventEx$value - eventEx$baseflow                           # update eventEx dataframe stormflow

useroutput <- c(useroutput, calcBFI = BFI, runname = runname, runnamefilter = as.character(NA)) 

# tidy R environment    
rm(publishedBFI, BFI, runname) 


#---- > Plot: Estimated baseflow sep. ------------------------------------------

legnd <- c("Event","Rainfall", "Baseflow", "Q", "Response event", "Value", "Intermediate flow")

## set x-axis limits
xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000 #(first 30 days)                                        # set the end date for the plot

## or specify the start and end times for the plot
# xlim_min <- as.POSIXct(strptime("2018-03-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
# xlim_max <- as.POSIXct(strptime("2018-06-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

update_geom_defaults("line", list(size = 0.2))

## plot rainfall and runodd with baseflow
p1 <- ggRainSimple(eventEx, xlim_min, xlim_max) +
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_y_reverse(limits = c(ylimit1,0))


p2 <- ggQBaseflowSimple(eventEx, xlim_min, xlim_max) +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  #scale_y_continuous(limits = c(0,ylimit2)) +
  labs(caption = paste0("calculated BFI = ", round(useroutput$calcBFI,5)))

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))


print(p)

## save plot to file
tiff(paste0("plots/","ts_02_baseSep_output_",
            userinput$site,
            userinput$runname, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
#width = 210, height = 297, units = 'mm', res = 600) # A4
print(p)
dev.off()

rm(baseSep,
   p1, p2, p,
   legnd, xlim_max, xlim_min)


##### EVENT DETECTION: #########################################################

#---- > Manually set/check eventEx user input ----------------------------------

sitename <- userinput$site

legnd <- c("Event","Rainfall", "Baseflow", "Q", "Response event", "Value", "Intermediate flow")


#==== 3. RAINFALL EVENTS =======================================================

quantile(eventEx$rainfall, 0.50, na.rm = T)
rain.median <- median(eventEx$rainfall, na.rm = T)

## rank the flow
rainfall <-  eventEx$rainfall
rainfall <-  sort(rainfall, decreasing = TRUE)
rankedrainfall <- data.frame(rainfall)
rankedrainfall$pcntexceedance <- seq (0, 1, by = 1/(nrow(rankedrainfall)-1))

rm(rainfall)

legnd <- c("rain")

p.log <- ggplot() + 
  geom_line(data = rankedrainfall, 
            aes(x = pcntexceedance, y = rainfall, colour = legnd[1], linetype =legnd[1])) +
  xlab ("% time rainfall equalled or exceeded") + 
  ylab(expression(Rainfall~intensity~(mm~h^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  panel.grid.minor = element_blank()) +
  scale_colour_manual(values= c("black"))+
  scale_linetype_manual(values= c("solid"))

print(p.log)


#---- > run rainfall event ID --------------------------------------------------

## How many timesteps should the rainfall link bridge?                                                           
# userinput$rainLinkSpan <- 6                                                   # set in main inputs list
message(paste0("rainlink span (records) = ", 
               userinput$rainLinkSpan,
               "\nrainlink span (mins) = ", 
               userinput$rainLinkSpan*userinput$tsfrq))

# > links two rainfall events if they fall within 6 timesteps 
#   (with 15 min data, 1.5 hr) of each other
if(!exists("foo", mode = "function")) source("eventEx_04_rainfallEvents_20200127.R")


#---- > Plot: rainfall events --------------------------------------------------

## set x-axis limits
xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000 #(first 30 days)                                        # set the end date for the plot

## or specify the start and end times for the plot
#xlim_min <- as.POSIXct(strptime("2015-11-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
#xlim_max <- as.POSIXct(strptime("2015-11-10 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q", 
           "Event (rain)", "Event (response)", "Event (flow sep.)", "value")
update_geom_defaults("line", list(size = 0.2))

p1 <- ggplot(eventEx) + 
  geom_col(aes(datetime, rainfall.event*ylimit1, fill=legnd[5]),
  #geom_col(aes(datetime, rainfall.event*5, fill=legnd[5]),
           width=1*60*15, 
           position=position_dodge(1*60*15), alpha = 0.3) +
  geom_point(aes(datetime, rainfall.eventcentroid*ylimit1), 
             shape = 8, size = 1, colour = "grey30") +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), 
           width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + 
  theme(legend.key = element_blank(),
        legend.title=element_blank(), 
        legend.position = "none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c("cyan", "grey50"))

p2 <- ggplot(eventEx) +
  geom_col(aes(datetime, rainfall.event*ylimit2, fill=legnd[5]), 
           width=1*60*15, 
           position=position_dodge(1*60*15), alpha=0.3) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom") +                 # blank element removes background to legend
  scale_colour_manual(values= c("grey50", "black" )) +
  scale_linetype_manual(values= c("longdash", "solid" )) +
  scale_fill_manual(values = c('cyan'))

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))

## plot to file
tiff(paste0("plots/ts_03_eventEx_rainEvents_check_",
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

print(p)

## tidy R enviroment
rm(p, p1, p2, xlim_max, xlim_min)


#==== 4. FLOW value setup, peak ID and thresholds ========================

eventEx$value <- eventEx$q
dat <- eventEx

## checks (to useroutput list - see global env)

sum(dat$rainfall/4, na.rm = TRUE) / time_length(difftime(as.Date(tsn), as.Date(ts1)),"years")

useroutput$qmean <- as.numeric(mean(dat$q, na.rm=TRUE))

useroutput$q95 <- as.numeric(quantile(dat$q, .05, na.rm=TRUE))
useroutput$q70 <- as.numeric(quantile(dat$q, .30, na.rm=TRUE))  
useroutput$q50 <- as.numeric(quantile(dat$q, .50, na.rm=TRUE))  
useroutput$q10 <- as.numeric(quantile(dat$q, .90, na.rm=TRUE))
useroutput$q05 <- as.numeric(quantile(dat$q, .95, na.rm=TRUE))

## Q5:Q95 ratio
useroutput$Q5Q95ratio <- as.numeric(quantile(eventEx$q, .95, na.rm=TRUE))/
  as.numeric(quantile(eventEx$q, .05, na.rm=TRUE))

##---- > 4.0 stats summary: by year ------------------------------------------------

dat <- eventEx

## Hydrological year
dat$wateryear <- NA

## start of the water year
wyear.STR <- as.character("2010-10-01 00:00:00")

## number of years needed
wyear.n <- 10
wateryearSTR <- seq(as.POSIXct(wyear.STR,tz="UTC"),by="1 year",length.out=wyear.n)

## mutate the column wateryear for each year of the sequence defined above
for (i in 1:wyear.n){ # start loop
  dat <- dat %>%                           
    
    mutate(wateryear =                                 
             ifelse(datetime >= wateryearSTR[i] & datetime < wateryearSTR[i+1], 
                    paste0(format(wateryearSTR[i], format="%Y"),"-", format(wateryearSTR[i+1], format="%Y")),
                    wateryear)) 
} # end loop

dat$wateryear_f <- factor(dat$wateryear)

dat.grp.wyear <- group_by(dat,                                                  # data_frame object
                          wateryear_f)                                          # column name to group by

stats <- as.data.frame(summarize(dat.grp.wyear, 
                    count = (sum(!is.na(value)) + sum(is.na(value))),
                    countZero = sum(value == 0, na.rm = TRUE) ,
                    countNotNA = sum(!is.na(value)),                            # count the number of not NA values          
                    countNA = sum(is.na(value)),
                    completeness = sum(!is.na(value))/(sum(!is.na(value)) + sum(is.na(value))),
                    min = min(value, na.rm = TRUE),
                    max = max(value, na.rm = TRUE),
                    mean = mean(value, na.rm = TRUE),
                    sd = sd(value, na.rm = TRUE),
                    median = median(value, na.rm = TRUE),
                    p5th = quantile(value, 0.05, na.rm = TRUE),
                    p30th = quantile(value, 0.30, na.rm = TRUE),
                    p50th = quantile(value, 0.50, na.rm = TRUE),
                    p90th = quantile(value, 0.90, na.rm = TRUE),
                    p95th = quantile(value, 0.95, na.rm = TRUE),
                    q1 = quantile(value, 0.25, na.rm = TRUE),
                    q2 = quantile(value, 0.50, na.rm = TRUE),
                    q3 = quantile(value, 0.75, na.rm = TRUE),
                    totalm3 = sum(value, na.rm = TRUE) *60*15
))                  # count the number of NA values


stats

write.csv(stats, paste0("./EBpost_eventEx_flow_wateryear_stats.csv"))

stats <- as.data.frame(summarize(dat.grp.wyear, 
                                 count = (sum(!is.na(rainfall)) + sum(is.na(rainfall))),
                                 countZero = sum(rainfall == 0, na.rm = TRUE) ,
                                 countNotNA = sum(!is.na(rainfall)),                            # count the number of not NA values          
                                 countNA = sum(is.na(rainfall)),
                                 completeness = sum(!is.na(rainfall))/(sum(!is.na(rainfall)) + sum(is.na(rainfall))),
                                 min = min(rainfall, na.rm = TRUE),
                                 max = max(rainfall, na.rm = TRUE),
                                 mean = mean(rainfall, na.rm = TRUE),
                                 sd = sd(rainfall, na.rm = TRUE),
                                 median = median(rainfall, na.rm = TRUE),
                                 p5th = quantile(rainfall, 0.05, na.rm = TRUE),
                                 p30th = quantile(rainfall, 0.30, na.rm = TRUE),
                                 p50th = quantile(rainfall, 0.50, na.rm = TRUE),
                                 p90th = quantile(rainfall, 0.90, na.rm = TRUE),
                                 p95th = quantile(rainfall, 0.95, na.rm = TRUE),
                                 q1 = quantile(rainfall, 0.25, na.rm = TRUE),
                                 q2 = quantile(rainfall, 0.50, na.rm = TRUE),
                                 q3 = quantile(rainfall, 0.75, na.rm = TRUE),
                                 totalmm = sum(rainfall, na.rm = TRUE) / 4
)  )                              # count the number of NA values


stats

write.csv(stats, paste0("./EBpost_eventEx_rain_wateryear_stats.csv"))

stats <- as.data.frame(summarize(eventEx, 
                                 count = (sum(!is.na(rainfall)) + sum(is.na(rainfall))),
                                 countZero = sum(rainfall == 0, na.rm = TRUE) ,
                                 countNotNA = sum(!is.na(rainfall)),                            # count the number of not NA values          
                                 countNA = sum(is.na(rainfall)),
                                 completeness = sum(!is.na(rainfall))/(sum(!is.na(rainfall)) + sum(is.na(rainfall))),
                                 min = min(rainfall, na.rm = TRUE),
                                 max = max(rainfall, na.rm = TRUE),
                                 mean = mean(rainfall, na.rm = TRUE),
                                 sd = sd(rainfall, na.rm = TRUE),
                                 median = median(rainfall, na.rm = TRUE),
                                 p5th = quantile(rainfall, 0.05, na.rm = TRUE),
                                 p30th = quantile(rainfall, 0.30, na.rm = TRUE),
                                 p50th = quantile(rainfall, 0.50, na.rm = TRUE),
                                 p90th = quantile(rainfall, 0.90, na.rm = TRUE),
                                 p95th = quantile(rainfall, 0.95, na.rm = TRUE),
                                 q1 = quantile(rainfall, 0.25, na.rm = TRUE),
                                 q2 = quantile(rainfall, 0.50, na.rm = TRUE),
                                 q3 = quantile(rainfall, 0.75, na.rm = TRUE)
)  )                              # count the number of NA values

stats
write.csv(stats, paste0("./EBpost_eventEx_rain_all_stats.csv"))

stats <- as.data.frame(summarize(eventEx, 
                                 count = (sum(!is.na(q)) + sum(is.na(q))),
                                 countZero = sum(q == 0, na.rm = TRUE) ,
                                 countNotNA = sum(!is.na(q)),                            # count the number of not NA values          
                                 countNA = sum(is.na(q)),
                                 completeness = sum(!is.na(q))/(sum(!is.na(q)) + sum(is.na(q))),
                                 min = min(q, na.rm = TRUE),
                                 max = max(q, na.rm = TRUE),
                                 mean = mean(q, na.rm = TRUE),
                                 sd = sd(q, na.rm = TRUE),
                                 median = median(q, na.rm = TRUE),
                                 p5th = quantile(q, 0.05, na.rm = TRUE),
                                 p30th = quantile(q, 0.30, na.rm = TRUE),
                                 p50th = quantile(q, 0.50, na.rm = TRUE),
                                 p90th = quantile(q, 0.90, na.rm = TRUE),
                                 p95th = quantile(q, 0.95, na.rm = TRUE),
                                 q1 = quantile(q, 0.25, na.rm = TRUE),
                                 q2 = quantile(q, 0.50, na.rm = TRUE),
                                 q3 = quantile(q, 0.75, na.rm = TRUE)
)  )                              # count the number of NA values

stats
write.csv(stats, paste0("./EBpost_eventEx_flow_all_stats.csv"))

#---- > 4.1 Flow Duration Curve ------------------------------------------------

#.... calculations .............................................................

## rank the flow
flow <-  dat$q
flow <-  sort(flow, decreasing = TRUE)
rankedflow <- data.frame(flow)
rankedflow$pcntexceedance <- seq (0, 1, by = 1/(nrow(rankedflow)-1))

rm(flow)

#.... PLOT: Flow Duration Curve ................................................

legnd <- c("flow", "flow (smoothed)")

p.log <- ggplot() + 
  geom_line(data = rankedflow, 
            aes(x = pcntexceedance, y = flow, colour = legnd[1], linetype =legnd[1])) +
  xlab ("% time flow equalled or exceeded") + 
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  panel.grid.minor = element_blank()) +
  scale_colour_manual(values= c("black"))+
  scale_linetype_manual(values= c("solid"))

print(p.log)

p<- ggplot() + 
  geom_line(data = rankedflow, 
            aes(x = pcntexceedance, y = flow, colour = legnd[1], linetype =legnd[1])) +
  xlab ("% time flow equalled or exceeded") + 
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  panel.grid.minor = element_blank()) +
  scale_colour_manual(values= c("black"))+
  scale_linetype_manual(values= c("solid"))

print(p)

## save plot to file
tiff(paste0("plots/","FDClog_",
            userinput$sitename, "_100x100_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 100, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p.log)
dev.off()

tiff(paste0("plots/","FDClog_",
            userinput$sitename, "_60x60_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 60, height = 60, units = 'mm', res = 600, compression = "zip") # to make squares
print(p.log)
dev.off()

tiff(paste0("plots/","FDC_",
            userinput$sitename, "_100x100_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 100, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

tiff(paste0("plots/","FDC_",
            userinput$sitename, "_60x60_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 60, height = 60, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

rm(p, p.log)

#save!
#---- >*4.2 Select value of filter for value -----------------------------------

## Check main calculation dataframe and userinputs
str(eventEx)
print(userinput)

message(  "Filtered fastflow value RDF parameters:\n",
          
          "\nSite = ", userinput$site ,
          "\nStart time = ", userinput$ts1,
          "\nEnd time = ", userinput$tsn,
          "\nRDF value for alpha = ", userinput$alpha,
          "\nNumber of reflected padding records = ", userinput$pad,
          "\nNumber of filter passes (alternating directions, must be odd) = ", userinput$passesfilter
          
)


# or use basic filtered flow (low frequency removed) ...........................

## call userinputs for filtered flow (low frequency removed) 
runs <- 1 

site <- userinput$site 
ts1 <- userinput$ts1
tsn <- userinput$tsn

alpha <- userinput$alpha
pad <- userinput$pad
passes <- userinput$passesfilter #5                                                # set to 5 for basic filtering
publishedBFI <- 0.00       # set to zero for basic filtering


## run recursive digital filter
if(!exists("foo", mode = "function")) source("eventEx_02_baseSep_rdf_lyneHollick_fb_20191216.R") # run baseflow sep code with limited passes

useroutput$runnamefilter <- runname

## seperating stormflow
eventEx$storminterflow <- baseSep$bconstrained - eventEx$baseflow
eventEx$stormquickflow <- eventEx$q - baseSep$bconstrained

## for plotting interflow
eventEx$interflow <- eventEx$baseflow + eventEx$storminterflow

## *** response value to be used:
#save!
# eventEx$value <- eventEx$q                                                    # flow
# eventEx$value <- eventEx$stormflow                                            # stormflow
# eventEx$value <- eventEx$baseflow                                             # baseflow
# eventEx$value <- eventEx$sg                                                   # stage
eventEx$value <- eventEx$stormquickflow                                         # filtered quick flow
 #eventEx$value <- eventEx$storminterflow                                       # filtered intermediate flow (use if high short term variability)  

# tidy R environment    
rm(publishedBFI, BFI, runname) 

#---- > Plot: filter seperation ------------------------------------------------

## set x-axis limits
xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000 #(first 30 days)                                        # set the end date for the plot

## or specify the start and end times for the plot
# xlim_min <- as.POSIXct(strptime("2018-03-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
# xlim_max <- as.POSIXct(strptime("2018-06-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow") #9

p1 <- ggRainSimple(eventEx, xlim_min, xlim_max) +
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)")

p2 <- ggQBaseflowSimple(eventEx, xlim_min, xlim_max) +
  geom_line(aes(datetime, y = interflow, colour = legnd[9], linetype =legnd[9])) +
  geom_line(aes(datetime, value, colour = legnd[8], linetype =legnd[8])) +
  scale_fill_manual(values = c("blue")) +
  scale_colour_manual(values= c("grey30", "grey30", "black", "red")) +
  scale_alpha(range=c(0.3)) +
  scale_linetype_manual(values= c("longdash", "dotted","solid", "solid"))

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))

tiff(paste0("plots/ts_04_eventEx_filtervalues_check_",
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

print(p)

# tidy R environment    
rm(p)
rm(baseSep)

#---- > 3.4 Set threasholds ----------------------------------------------------

## Exceedeance values QXX
Q00 <- setNames(
  data.frame(matrix(ncol = 11, nrow = 1)),                                      # Name columns of a new dataframe
  c("Q05","Q10","Q20","Q30","Q40","Q50","Q60","Q70","Q80","Q90","Q95"))

## Hughs (value adjusted) threashold code for Q:
Q00$Q05 <- as.numeric(quantile(eventEx$value, .95, na.rm=TRUE))                
Q00$Q10 <- as.numeric(quantile(eventEx$value, .9, na.rm=TRUE))                  # the 90 percentile for value, equalled or exceeded for 10% of the flow record
Q00$Q20 <- as.numeric(quantile(eventEx$value, .8, na.rm=TRUE))
Q00$Q30 <- as.numeric(quantile(eventEx$value, .7, na.rm=TRUE))
Q00$Q40 <- as.numeric(quantile(eventEx$value, .6, na.rm=TRUE))
Q00$Q50 <- as.numeric(quantile(eventEx$value, .5, na.rm=TRUE))                  # the 50 percentile for value, equalled or exceeded for 50% of the flow record
Q00$Q60 <- as.numeric(quantile(eventEx$value, .4, na.rm=TRUE))
Q00$Q70 <- as.numeric(quantile(eventEx$value, .3, na.rm=TRUE))                  # the 30 percentile for value, equalled or exceeded for 30% of the flow record
Q00$Q80 <- as.numeric(quantile(eventEx$value, .2, na.rm=TRUE))
Q00$Q90 <- as.numeric(quantile(eventEx$value, .1, na.rm=TRUE))
Q00$Q95 <- as.numeric(quantile(eventEx$value, .05, na.rm=TRUE))                 # the 5 percentile for value, equalled or exceeded for 95% of the flow record

## check
Q00                                                                             # check the values

## Exe Stoodleigh Stormflow - auto settings

#Qbase <- Q00$Q95                          # 0                                  # Q95 (low base)
#Qbase <- as.numeric(Q00["Q95"])
Qbase <- as.numeric(Q00[userinput$th_Qbase])

#Qmagnitude <- (Q00$Q30-Q00$Q95)                   # 0.2552364                  # Q10 - Q95 (interested in any magnitude over (Q10, representing low flow, minus Q95 )
#Qmagnitude <- as.numeric(Q00["Q30"]-Q00["Q95"])
Qmagnitude <- as.numeric(Q00[userinput$th_Qmagnitude[1]]-Q00[userinput$th_Qmagnitude[2]])

#---- > 3.1 set up plot for peaks check ------------------------------------------

legnd <- c("Event","Rainfall", "Baseflow","Q", "response event", "value", 
           "smoothed", "peaks", "peaks over Qmagnitude")

## set x-axis limits
xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000 #(first 30 days)                                        # set the end date for the plot

## or specify the start and end times for the plot
# xlim_min <- as.POSIXct(strptime("2018-03-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
# xlim_max <- as.POSIXct(strptime("2018-06-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

#---- > 3.2 run peak finder ------------------------------------------------------

## set span

spanhr <- 6
span <- (spanhr*4) / nrow(eventEx)   

calc_peaks <- eventEx[,c("userID", "datetime", "q", "value", "rainfall")]

## USES value. Find peaks, change points and rate of change for threashold
if(!exists("foo", mode = "function")) source("eventEx_03_findPeaks_20191209.R")

p1 <- ggplot(calc_peaks) + 
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall\nintensity\n(mm h⁻¹)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()
                                  ,axis.text.x=element_blank()
                                  ,axis.ticks.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c("cyan","grey50","blue")) +
  scale_alpha(range=c(0.1))

p2 <- ggplot(calc_peaks) +
  geom_line(aes(datetime, value, colour = legnd[6], linetype =legnd[6])) +
  geom_line(aes(datetime, smoothed, colour = legnd[7], linetype =legnd[7])) +
  geom_point(data = peaks, aes(datetime, smoothed, colour = legnd[8]), shape = 1) +
  geom_point(data = peaks.overQmag.high, aes(datetime, smoothed, colour = legnd[9]), shape = 16) +
  xlab ("Date") +
  ylab (paste0("Flow\n(m³ s⁻¹)")) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  #scale_y_continuous(limits = c(0,50)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom") + #(base_size = 12) +          # blank element removes background to legend
  scale_colour_manual(values= c( "blue", "blue", "black", "red")) +
  scale_linetype_manual(values= c("solid", "solid"))

p3 <- ggplot(calc_peaks) +
  geom_point(aes(datetime, diffSmoothedHr), shape = 1) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M"))+
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom")

p.peaks <- plot_grid(p1, p2, p3, nrow = 3, align = "v", rel_heights = c(0.5, 1.5, 1))

print(p.peaks)


## save to main calculations datframe
eventEx$peaks <- calc_peaks$peaks
eventEx$peaks.over.Qmag.high <- calc_peaks$peaks.overQmag.high
eventEx$diffSmoothedHr <- calc_peaks$diffSmoothedHr

## save plot to file
tiff(paste0("plots/","ts_04_eventEx_findPeaks_output_",
            userinput$site,
            "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
#width = 210, height = 297, units = 'mm', res = 600) # A4
print(p.peaks)
dev.off()

print(p.peaks)

rm(p1, p2, p3, p.peaks)
rm(modLoess, smoothed)
rm(find_peaks)

#..... The "STABLE", "ELEVATED" threashold is from slope change ................

## set slope thresholds
# QslopeAsc <-  as.numeric(quantile(eventEx$diffSmoothedHr, .95, na.rm=TRUE)) #0.05 # 0.2
# QslopeDec <- as.numeric(quantile(eventEx$diffSmoothedHr, .10, na.rm=TRUE)) #-0.05 # -0.2

calc_diffcheck <- as.data.frame(eventEx[,c("userID", "datetime", "q", "value", "rainfall", "diffSmoothedHr")])

str(calc_diffcheck)

##plot the density
ggplot(calc_diffcheck, aes(diffSmoothedHr)) +
  geom_density(stat = "density",
               position = "identity",
               na.rm = T,
               show.legend = NA) +
  scale_x_continuous(limits = c(-0.05,0.05), breaks = c(seq(-0.05,0.05, by =0.005))) +
  #scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(0,500)) +
  theme_bw() + theme(legend.key = element_blank(),
                     legend.title=element_blank())

# auto
QslopeAsc <- as.numeric(quantile(eventEx$diffSmoothedHr, userinput$th_QslopeAsc, na.rm=TRUE)) #0.05 # 0.2
QslopeDec <- as.numeric(quantile(eventEx$diffSmoothedHr, userinput$th_QslopeDec, na.rm=TRUE)) #-0.05 # -0.2





calc_diffcheck$slopeAsc_diffcheck <- NA
calc_diffcheck$slopeDec_diffcheck <- NA
calc_diffcheck$slope_diffcheck <- NA

calc_diffcheck <- calc_diffcheck %>%
  mutate(slopeAsc_diffcheck =
           ifelse(diffSmoothedHr>QslopeAsc,
                  T, F)) %>%
  
  mutate(slopeDec_diffcheck =
           ifelse(diffSmoothedHr<QslopeDec,
                  T, F)) %>%
  
  mutate(slope_diffcheck =                                                       
           ifelse(diffSmoothedHr>QslopeAsc,"red",
                  ifelse(diffSmoothedHr<QslopeDec, "green",
                         "black")))

str(calc_diffcheck)

xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000                                                         # set the end date for the plot

## PLOT: example of stopes above and below thresholds

p <- ggplot() +
  geom_line(data = calc_diffcheck, aes(datetime, value), colour = calc_diffcheck$slope_diffcheck) +
  #xlab ("Date") +
  #ylab (paste0("Flow\n(m³ s⁻¹)")) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  #scale_y_continuous(limits = c(0,50)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom") + #(base_size = 12) +          # blank element removes background to legend
  scale_colour_manual(values= c( "red", "blue", "black")) +
  scale_linetype_manual(values= c("solid", "solid", "solid"))

p

## save plot to file
tiff(paste0("plots/","ts_04_eventEx_slopechecks_",
            userinput$site,
            "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
#width = 210, height = 297, units = 'mm', res = 600) # A4
print(p)
dev.off()

#---- > 3.4 missing data treatment for value -------------------------------------

## Basic - fill with preceeding value
summary(eventEx)                                                                # Check NAs in value column
eventEx <- eventEx %>%                                                          # temporary NA fill pipe
  fill(value) %>%                                                          
  fill(value, .direction = "up")                                                                  

summary(eventEx)


#==== 5. response EVENTS =======================================================

#---- > run response event ID --------------------------------------------------

## select stable peak test threshold (N.B if noise or variable use Q50, if clean can use Q10)
eventEx$Q50win24hr <- as.numeric(rollapply(
  eventEx$value, list(c(-144:144)),  #-48:48)),
  FUN = function(z) quantile(z, .5, na.rm=TRUE), 
  fill = NA))

eventEx$value.median.win24hr <- as.numeric(rollapply(
  eventEx$value, list(c(-144:144)),  #-48:48)),
  FUN = function(z) quantile(z, .5, na.rm=TRUE), 
  fill = NA))

#eventEx$QXXwin24hr <- ifelse(is.na(eventEx$Q50win24hr), Q00$Q50, eventEx$Q50win24hr)
eventEx$value.median.win24hr <- ifelse(is.na(eventEx$value.median.win24hr), Q00$Q50, eventEx$value.median.win24hr) 

eventEx$smoothed <- calc_peaks$smoothed

eventEx$diffSmoothedHr <- calc_peaks$diffSmoothedHr

## find and ID response events
if(!exists("foo", mode = "function")) source("eventEx_05_responseEvents_20191209.R")


#---- > Plot: response events ---------------------------------------------------

xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000                                                         # set the end date for the plot

## or specify the start and end times for the plot
# xlim_min <- as.POSIXct(strptime("2018-03-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
# xlim_max <- as.POSIXct(strptime("2018-06-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow") #9
update_geom_defaults("line", list(size = 0.2))

p1 <- ggplot(eventEx) + 
  geom_col(aes(datetime, rainfall.event*ylimit1, fill=legnd[5]), 
           width=1*60*15, 
           position=position_dodge(1*60*15), alpha = 0.3) +
  geom_point(aes(datetime, rainfall.eventcentroid*ylimit1), 
             shape = 8, size = 1, colour = "grey30") +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), 
           width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + 
  theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank(),
                                  axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c("cyan", "grey50"))

p2 <- ggplot(eventEx) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), 
           width=1*60*15, 
           position=position_dodge(1*60*15), alpha=0.2) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom") +                 # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "grey40", "black")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid")) +
  scale_fill_manual(values = ("dodgerblue3"))

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
print(p)

## save plot to file
tiff(paste0("plots/ts_05_eventEx_responseEvents_check_",
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

rm(p, p1, p2, legnd, xlim_max, xlim_min)

#==== 6. COMBINE EVENTS ========================================================

#---- > run combine code -------------------------------------------------------

## Combine rain and response events
if(!exists("foo", mode = "function")) source("eventEx_06_combEvents_20191210.R")


#---- > Plot: events ------------------------------------------------------------

xlim_min <- ts1                                                                 # set the start date for the plot
xlim_max <- ts1+2592000                                                         # set the end date for the plot

## or specify the start and end times for the plot
# xlim_min <- as.POSIXct(strptime("2018-03-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
# xlim_max <- as.POSIXct(strptime("2018-06-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow") #9
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c("cyan", "dodgerblue3", "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.3, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_fill_manual(values = c("cyan", "dodgerblue3")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_colour_manual(values= c("grey40", "grey40", "black")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid"))

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()


#==== 7. EVENT METRICS ========================================================

#set event values for plotting
eventEx$event <- eventEx$event.temp

## check if folder exstes and set runID
runID <- paste0(format(Sys.time(),"%Y%m%d_%H%M"), "_value_", useroutput$runnamefilter)

if(!dir.exists(paste0(sitename))) dir.create(paste0(sitename))
if(!dir.exists(paste0(sitename, "/run_",runID))) dir.create(paste0(sitename, "/run_",runID))
if(!dir.exists(paste0(sitename, "/run_",runID,"/CHECK"))) dir.create(paste0(sitename, "/run_",runID,"/CHECK"))
if(!dir.exists(paste0(sitename, "/run_",runID,"/CHECK/plots"))) dir.create(paste0(sitename, "/run_",runID,"/CHECK/plots"))
if(!dir.exists(paste0(sitename, "/run_",runID,"/CHECK/eventWin"))) dir.create(paste0(sitename, "/run_",runID,"/CHECK/eventWin"))


if(!exists("foo", mode = "function")) source("eventEx_07_metrics_20191211.R")

## Populate temporary event ID column 
eventEx$eventID.temp <- eventEx$response.eventID

# Fillup for full rainfall-events (N.B binary - overlappin events will be IDed with only one ID)
eventEx[c(which(eventEx$event.temp == 1)),] <- eventEx[c(which(eventEx$event.temp == 1)),] %>% 
  fill(eventID.temp, .direction = "up")

eventEx$eventID <- eventEx$eventID.temp

## save event metrics
write.csv(EVENTS, paste0(sitename, "/run_",runID,"/CHECK/eventEx_EVENTS_CHECK_", runID, ".csv"))

#----  > plot each event ---------------------------------------------------------

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Q peaks", "Q (smoothed)", "Water quality sampling") #9:11
update_geom_defaults("line", list(size = 0.5))

for (i in c(1:nrow(EVENTS))){ 
  
  message(paste0("Plotting check event no: ", EVENTS$eventID[i]))
  
  # fixed window size (3.5 days)
  xlim_min <- as.POSIXct(EVENTS$Q.response.start.ts[i]-(86400*3), tz = "UTC")   # set the start date for the plot (12 hr befor start)
  xlim_max <- as.POSIXct(EVENTS$Q.response.start.ts[i]+(86400*5), tz = "UTC")   # set the end date for the plot   (3 days after start)
  
  ylimit1d <- max(eventEx[eventEx$datetime %in% xlim_min:xlim_max, "rainfall"], na.rm = TRUE) 
  ylimit2d <- max(eventEx[eventEx$datetime %in% xlim_min:xlim_max, "q"], na.rm = TRUE)
  
  ## open file connection
  tiff(paste0(sitename, "/run_",runID,"/CHECK/plots/eventPlot",i, "_",
              format(EVENTS$Q.response.start.ts[i],"%Y%m%d_%H%M"),
              ".tiff"), 
       width = 100, height = 100, units = 'mm', res = 600, compression = "zip")  
  
  ## save plots
  p1 <-   ggplot(eventEx[eventEx$datetime %in% xlim_min:xlim_max, ]) + 
    
    geom_col(data = subset(eventEx,                                             # Subset origional data for year of ts between dates
                           datetime >= as.POSIXct(EVENTS$event.start.ts[i]) &   # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
                             datetime <= as.POSIXct(EVENTS$event.end.ts[i])),   # 24 h after year start
             aes(datetime, event*ylimit1d, fill=legnd[5]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
    
    geom_col(data = subset(eventEx, 
                           response.eventID == i),
             aes(datetime, response.event*ylimit1d, fill=legnd[6]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
    
    geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2])) + 
    
    ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_reverse(limits = c(ylimit1d,0)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(), 
                                    legend.position = "none",
                                    axis.title.x=element_blank()
                                    ,axis.text.x=element_blank()
                                    ,axis.ticks.x=element_blank()) +
    scale_colour_manual(values= c("grey50")) +
    scale_fill_manual(values = c("cyan", 
                                 "dodgerblue3", 
                                 "grey50"))
  
  p2 <- ggplot(eventEx[eventEx$datetime %in% xlim_min:xlim_max, ]) +
    
    geom_col(data = subset(eventEx,                                             # Subset origional data for year of ts between dates
                           datetime >= as.POSIXct(EVENTS$event.start.ts[i]) &    # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
                             datetime <= as.POSIXct(EVENTS$event.end.ts[i])),   # 24 h after year start
             aes(datetime, event*ylimit2d, fill=legnd[5]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
    
    geom_col(data = subset(eventEx, 
                           response.eventID == i),
             aes(datetime, response.event*ylimit2d, fill=legnd[6]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
    
    geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
    geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +

    geom_point(data = peaks, aes(datetime, q, colour = legnd[10]), shape = 1) +
    geom_point(data = peaks.overQmag.high, aes(datetime, q, colour = legnd[10]), shape = 16) +
    
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_continuous(limits = c(0,ylimit2d)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "none") + #(base_size = 12) +          # blank element removes background to legend
    scale_colour_manual(values= c("grey40","grey40","black", "red", "deeppink")) +
    scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) +
    scale_fill_manual(values = c("cyan", "dodgerblue3"))
  
  p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
  print(p)
  
  dev.off()
  
}

## N.B.
# > set to run on columns generated through 
#    "eventExtract_rainEvents_6ts.R",
#    "eventExtract_responseEvents.R", 
#    "eventExtract_combEvents.R" 

## Possible bugs:
# BUG - rainfall metrics: max of all event window rain - not for initial rain
# BUG - rainfall metrics: this includes rainfall is event window. Outside rainfall evnts and after falling limb starts
# BUG - rainfall metrics: combined total or rainfall events that END during the event
# BUG - Q metrics: Q.peak is max if multiple peaks exist
#save!

#==== 6. CHECKS ===================================================================

## Using Hample identifier outlier tests (indicated errronious event)

#---- > Check 1 for initial rainfall peak to event peak --------------------------

names(EVENTS)

check1 <- EVENTS[, c("eventID", "lag.peakinit.dur", "Q.response.tot.m3")]
check1$value <- unclass(check1$lag.peakinit.dur)

check1$MADoutlier.thrs.low <- median(check1$value) - 3*(mad(check1$value, 
                                                            center = median(check1$value), 
                                                            constant = 1.4826, na.rm = TRUE))
check1$MADoutlier.thrs.high  <- median(check1$value) + 3*(mad(check1$value, 
                                                              center = median(check1$value), 
                                                              constant = 1.4826, na.rm = TRUE))
check1$MADoutlier <- NA
check1$MADoutlier <- ifelse( ((check1$value < check1$MADoutlier.thrs.low) |
                                check1$value > check1$MADoutlier.thrs.high) | is.na(check1$lag.peakinit.dur),
                             TRUE, FALSE)

subset(check1 , check1$MADoutlier == TRUE)

# ## Plot: ouliers with total Q for each event
# ggplot () +
#   geom_point(data = subset(check1, MADoutlier == F), 
#              aes(x = Q.response.tot.m3, y = lag.peakinit.dur), shape = 1) +
#   geom_point(data = subset(check1, MADoutlier == T), 
#              aes(x = Q.response.tot.m3, y = lag.peakinit.dur), shape = 16, colour = "red") +
#   geom_text(data = subset(check1, MADoutlier == T), 
#             aes(x = Q.response.tot.m3, y = lag.peakinit.dur, label = eventID),
#             hjust = -1, vjust = 1, colour =  "red") +
#   geom_ribbon(data = check1,
#               aes(x = Q.response.tot.m3,
#                   ymin = head(check1$MADoutlier.thrs.low, 1), 
#                   ymax = head(check1$MADoutlier.thrs.high, 1)),
#               fill = "red",
#               alpha = 0.1,
#               linetype = 0)

#---- > Check 2 for start rainfall to start event lag ----------------------------


check2 <- EVENTS[, c("eventID", "lag.start.dur", "Q.response.tot.m3")]
check2$value <- unclass(check2$lag.start.dur)

check2$MADoutlier.thrs.low <- median(check2$value) - 3*(mad(check2$value, 
                                                            center = median(check2$value), 
                                                            constant = 1.4826, na.rm = TRUE))
check2$MADoutlier.thrs.high  <- median(check2$value) + 3*(mad(check2$value, 
                                                              center = median(check2$value), 
                                                              constant = 1.4826, na.rm = TRUE))
check2$MADoutlier <- NA
check2$MADoutlier <- ifelse( (check2$value < check2$MADoutlier.thrs.low) |
                               check2$value > check2$MADoutlier.thrs.high,
                             TRUE, FALSE)

## Check the subset of potential erronious events
subset(check2 , check2$MADoutlier == TRUE)

# ## plot: scatter for two parameters with univariate outliers (y axis) 
# ggplot () +
#   geom_point(data = subset(check2, MADoutlier == F), 
#              aes(x = Q.response.tot.m3, y = lag.start.dur), shape = 1) +
#   geom_point(data = subset(check2, MADoutlier == T), 
#              aes(x = Q.response.tot.m3, y = lag.start.dur), shape = 3, colour = "red") +
#   geom_text(data = subset(check2, MADoutlier == T), 
#             aes(x = Q.response.tot.m3, y = lag.start.dur, label = eventID),
#             hjust = -1, vjust = 1, colour =  "red") +
#   geom_ribbon(data = check2,
#               aes(x = Q.response.tot.m3,
#                   ymin = head(check2$MADoutlier.thrs.low, 1),
#                   ymax = head(check2$MADoutlier.thrs.high, 1)),
#               fill = "red",
#               alpha = 0.1,
#               linetype = 0) +
#   scale_x_continuous() +
#   scale_y_continuous() +                              
#   theme_bw(base_size = 8) + theme(legend.key = element_blank())


#---- > Check 3 number of responce peaks ---------------------------------------


check3 <- EVENTS[, c("eventID", "Q.nopeaks", "Q.response.tot.m3")]
check3$value <- unclass(check3$Q.nopeaks)

check3$threesigmaoutlier.thrs.low <- mean(check3$value) - 3*(sd(check3$value, na.rm = TRUE))
check3$threesigmaoutlier.thrs.high  <- mean(check3$value) + 3*(sd(check3$value, na.rm = TRUE))
check3$threesigmaoutlier <- NA
check3$threesigmaoutlier <- ifelse( (check3$value < check3$threesigmaoutlier.thrs.low) |
                                      check3$value > check3$threesigmaoutlier.thrs.high | check3$value == 0,
                                    TRUE, FALSE)

## Check the subset of potential erronious events
subset(check3 , check3$threesigmaoutlier == TRUE)

# ## Plot: ouliers with total Q for each event
# ggplot () +
#   geom_point(data = subset(check3, threesigmaoutlier == F),
#              aes(x = Q.response.tot.m3, y = value), shape = 1) +
#   geom_point(data = subset(check3, threesigmaoutlier == T),
#              aes(x = Q.response.tot.m3, y = value), shape = 3, colour = "red") +
#   geom_text(data = subset(check3, threesigmaoutlier == T),
#             aes(x = Q.response.tot.m3, y = value, label = eventID),
#             hjust = -1, vjust = 1, colour =  "red") +
#   geom_ribbon(data = check3,
#               aes(x = Q.response.tot.m3,
#                   ymin = head(check3$threesigmaoutlier.thrs.low, 1),
#                   ymax = head(check3$threesigmaoutlier.thrs.high, 1)),
#               fill = "red",
#               alpha = 0.1,
#               linetype = 0)


##---- > Combined auto checks ---------------------------------------------------------

check <- EVENTS[, c("eventID", "rain.start.ts")]

check$check.outlier1 <- check1$MADoutlier
check$check.outlier2 <- check2$MADoutlier
check$check.outlier3 <- check3$threesigmaoutlier

EVENTS$check.outlier1 <- check$check.outlier1
EVENTS$check.outlier2 <- check$check.outlier2
EVENTS$check.outlier3 <- check$check.outlier3

## These are the events that will automatically be binned
subset(check , (check.outlier1 == TRUE & check.outlier2 == TRUE) |
         check.outlier3 == TRUE)

# ## These are the events that will automatically be retained
# subset(check , (check.outlier1 == F | check.outlier2 == F) &
#   check.outlier3 == FALSE)

##---- > Remove additional events through manual checks ------------------------

## Manually
EVENTS$check.manual <- FALSE

## input the event numbers you want to remove through manual selection
# check.manual.row <- NULL         # use NULL, c() or c(0) if there are no events to remove manually
check.manual.row <- c(20, 30, 69, 70, 72, 73, 74, 75, 80,
                      81, 87, 88, 111, 168, 177, 179, 182,
                      184, 196, 199, 207, 216, 217, 258, 259,
                      260, 263, 274, 277, 282, 284, 288, 297,
                      301, 305, 307, 309, 310, 312, 313, 319,
                      320, 321, 322, 323, 324, 325, 326, 327, 
                      328, 329, 330, 332, 333, 334, 335, 337,
                      340, 341, 342, 343, 345, 346, 347, 349, 
                      350, 351, 354, 356, 357, 358, 363, 364, 
                      365, 366, 368, 371, 372, 380, 383, 384,
                      423, 424, 547, 589, 592, 593, 607, 619,
                      664, 667) # new values - crappy events to be deleted following fixes to rating curve.


# check.manual.row <- c(28, 53, 55, 58, 59, 60, 61, 62, 63, 68, # old values 
#                       69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
#                       79, 80, 81, 82, 83, 84, 85, 86, 92, 93,
#                       94, 95, 100, 101, 103, 104, 199, 286,
#                       298, 299, 303, 307, 309, 312, 313, 316,
#                       318, 319, 320, 321, 322, 323, 324, 325,
#                       327, 328, 329, 335, 336, 337, 339, 340,
#                       341, 350, 351, 352, 356, 357, 358, 365, 
#                       366, 378, 384, 385, 386, 396, 405, 406,
#                       463, 464, 504, 505, 574, 590, 603, 632,
#                       634, 647, 648, 651, 702)    # overwrite this if you want to remove events

## Use this to set maual checks to TRUE
if(!is.null(check.manual.row)) {
  message(paste0("Set to remove event manually: ", check.manual.row, "\n"))
  EVENTS$check.manual[check.manual.row] <- TRUE
}

# #EVENTSerr <- subset(EVENTS , EVENTS$check.outlier1 == TRUE & EVENTS$check.outlier2 == TRUE)
# EVENTSerr <- subset(EVENTS , (EVENTS$check.outlier1 == TRUE & EVENTS$check.outlier2 == TRUE) | 
#                       EVENTS$check.outlier3 == TRUE |
#                       EVENTS$check.manual == TRUE )

#EVENTSerr <- subset(EVENTS , EVENTS$check.outlier1 == TRUE & EVENTS$check.outlier2 == TRUE)
EVENTSerr <- subset(EVENTS , (EVENTS$check.manual == TRUE ))

EVENTS <- EVENTS[! EVENTS$eventID %in% c(EVENTSerr$eventID),]

message(paste0("The following events have been removed: \n", 
               paste0(capture.output(EVENTSerr[,c("eventID", "event.start.ts")]),
                      collapse = "\n")))


#==== 7. OUTPUTS ===============================================================

# Outputs are:
#    > EVENTS        Dataframe of key metrics for each event
#    > exentEx       Event Extraction calculation sheet (timeseries)
#    > output        Event Extraction output sheet (timeseries)
#    > list_eventWin Event Extraction indervidual events windows in a list (timeseries)

## check if folder existes and set runID
if(!dir.exists(paste0(sitename, "/run_",runID,"/"))) dir.create(paste0(sitename,"/run_",runID,"/"))
if(!dir.exists(paste0(sitename, "/run_",runID,"/final_plots_fixedscale/"))) dir.create(paste0(sitename,"/run_",runID,"/final_plots_fixedscale/"))
if(!dir.exists(paste0(sitename, "/run_",runID,"/final_plots/"))) dir.create(paste0(sitename,"/run_",runID,"/final_plots/"))
if(!dir.exists(paste0(sitename, "/run_",runID,"/final_event_windows/"))) dir.create(paste0(sitename,"/run_",runID,"/final_event_windows/"))
if(!dir.exists(paste0(sitename, "/run_",runID,"/removed_events/"))) dir.create(paste0(sitename,"/run_",runID,"/removed_events/"))
# if(!dir.exists(paste0(sitename, "/run_",runID,"/FINAL/"))) dir.create(paste0(sitename,"/run_",runID,"/FINAL/"))
# if(!dir.exists(paste0(sitename, "/run_",runID,"/FINAL/PLOTS/"))) dir.create(paste0(sitename,"/run_",runID,"/FINAL/PLOTS/"))
# if(!dir.exists(paste0(sitename, "/run_",runID,"/FINAL/eventWin/"))) dir.create(paste0(sitename,"/run_",runID,"/FINAL/eventWin/"))

#---- > EVENTS: update summary dataframe with new event IDs ---------------------

#EVENTS$eventID.temp <- EVENTS$eventID

n <- c(1:nrow(EVENTS))
EVENTS$eventIDnew <- as.numeric(n)

lookup <- unique(EVENTS[c("eventID.temp","eventIDnew")])

lookup$response.eventID <- lookup$eventID.temp
lookup$response.eventIDnew <- lookup$eventIDnew

print(lookup)

#EVENTS$response.eventID <- EVENTS$eventID
#EVENTS$response.eventID <- NULL

# N.B  run after erronious events removed
names(EVENTS)
names(eventEx)

eventEx$eventIDnew <- NULL
eventEx$response.eventIDnew <- NULL

## set up and populate new event.responce ID column
eventEx <- left_join(eventEx, lookup[c("response.eventID", "response.eventIDnew")], 
                     by = c("response.eventID"))


## set up and populate new event ID column
eventEx <- left_join(eventEx, lookup[c("eventID.temp","eventIDnew")], 
                     by = c("eventID.temp"))

names(eventEx)
tail(eventEx)
# eventEx$eventIDnew <- eventEx$eventIDnew.y

#---- > outputs: Update and save -------------------------------------------------

#---- > EVENTS: recalculculate interevent metrics --------------------------------------

#EVENTS$rainplus.rlimb.rain <- NA

for (i in n){ 
  
  #Loop counter to console
  message(paste0("Calculating interevent metrics for event: ", i))
  
  ## Row the previous event ends on (if 1st event use row 1)
  interevent.row1 <- ifelse(i == 1, 1,
                            tail(which(eventEx$eventIDnew == (i-1)),1) + 1)
  interevent.rown <- head(which(eventEx$datetime == EVENTS$event.start.ts[i]),1) - 1
  
  ## Duration between events (if the length is zero use NA)
  EVENTS$interevent.dur[i] <- as.duration(
    ifelse(interevent.row1 == interevent.rown, NA,
           as.duration(eventEx$datetime[interevent.rown]-
                         eventEx$datetime[interevent.row1])))                   # as.durtion used twice otherwise BUG
  
  ## Total rainfall in the interevent period (intensity sum / 4 as 15 min data)
  EVENTS$interevent.rainfall.tot.mm[i] <- sum(eventEx$rainfall[
    c(interevent.row1:interevent.rown)],
    na.rm=TRUE) /4
  
  ## Median Q50 of the interevent period 
  EVENTS$interevent.Q.median[i] <- median(eventEx$q[
    c(interevent.row1:interevent.rown)],
    na.rm=TRUE)
  
  rm(interevent.row1)
  
}

#---- > EVENTS: save event metrics ---------------------------------------------

EVENTS$eventID <- EVENTS$eventIDnew
EVENTSerr$eventID <- EVENTSerr$eventIDnew

## save event metrics
write.csv(EVENTS, paste0(sitename, "/run_",runID,"/eventEx_EVENTS_metrics.csv"))
saveRDS(EVENTS, paste0(sitename, "/run_",runID,"/eventEx_EVENTS_metrics.rds"))

## save discarded events
write.csv(EVENTSerr, paste0(sitename, "/run_",runID,"/removed_events/eventEx_EVENTS_eventsdeleted.csv"))
saveRDS(EVENTSerr, paste0(sitename, "/run_",runID,"/removed_events/eventEx_EVENTS_eventsdeleted.rds"))

##.... outputs .................................................................

names(outputs)
#outputs$datetime <- eventEx$datetime
#outputs$q <- eventEx$q
outputs$rainfall_mm_h <- eventEx$rainfall

outputs$baseflow_m3_s <- eventEx$baseflow
outputs$stormflow_m3_s <- eventEx$stormflow
outputs$storminterflow_m3_s <- eventEx$storminterflow
outputs$stormquickflow_m3_s <- eventEx$stormquickflow

outputs$rainfall.event <- eventEx$rainfall.event
outputs$rainfall.eventID <- eventEx$rainfall.eventID

outputs$response.eventID <- eventEx$response.eventIDnew
outputs$response.event <- ifelse(is.na(outputs$response.eventID), 0, 1)

outputs$eventID <- eventEx$eventIDnew
outputs$event <- ifelse(is.na(outputs$eventID), 0, 1)

## save outputs full csv
write.csv(outputs, paste0(sitename, "/run_",runID,"/eventEx_OUTPUTS_timeseries.csv"))
saveRDS(outputs, paste0(sitename, "/run_",runID,"/eventEx_OUTPUTS_timeseries.rds"))

## save event extraction  (calculations) full csv
write.csv(eventEx, paste0(sitename, "/run_",runID,"/eventEx_calc_timeseries.csv"))
saveRDS(eventEx, paste0(sitename, "/run_",runID,"/eventEx_calc_timeseries.rds"))

#---- > recalculculate event windows -------------------------------------------

rm(eventrows)
eventrows <- list()    

eventEx$q.orig <- eventEx$q

list_eventWin <- list()

## Save event windows to fill and R list

for (i in n){ 
  
  message(paste0("extracting event: ", i))
  eventrows[[i]] <- c(which(outputs$datetime == EVENTS$event.start.ts[i]):which(outputs$datetime == EVENTS$event.end.ts[i]))   
  
  eventWin <- data.frame(outputs[c(eventrows[[i]]),])  
  write.csv(eventWin, paste0(sitename, "/run_",runID,"/final_event_windows/eventWin", i, ".csv"))
  
  list_eventWin[[i]] <- eventWin
  
}

#---- > re plots events --------------------------------------------------------

## plot each event
#legnd <- c("Event","Rainfall", "Baseflow","Q", "Event (rain)", "Event (response)", "Event (flow sep.)", "Intermediate", "peaks", "Q (smoothed)")  
legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Q peaks", "Q (smoothed)") #9:11
update_geom_defaults("line", list(size = 0.5))

for (i in n){ 
  
  message(paste0("Plotting final event no: ", EVENTS$eventIDnew[i]))
  
  # fixed window size (3.5 days)
  xlim_min <- as.POSIXct(EVENTS$Q.response.start.ts[i]-(86400*3), tz = "UTC")     # set the start date for the plot (12 hr befor start)
  xlim_max <- as.POSIXct(EVENTS$Q.response.start.ts[i]+(86400*3), tz = "UTC")       # set the end date for the plot   (3 days after start)
  
  ## open file connection
  tiff(paste0(sitename, "/run_",runID,"/final_plots_fixedscale/eventPlot",i, "_",
              format(EVENTS$Q.response.start.ts[i],"%Y%m%d_%H%M"),
              ".tiff"),
       width = 100, height = 100, units = 'mm', res = 600, compression = "zip")  
  
  ## save plots
  p1 <-   ggplot(outputs) + 
    
    geom_col(data = subset(outputs,                                             # Subset origional data for year of ts between dates
                           datetime >= as.POSIXct(EVENTS$event.start.ts[i]) &   # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
                             datetime <= as.POSIXct(EVENTS$event.end.ts[i])),   # 24 h after year start
             aes(datetime, event*ylimit1, fill=legnd[5]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
    
    geom_col(data = subset(outputs, 
                           response.eventID == i),
             aes(datetime, response.event*ylimit1, fill=legnd[6]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
    
    geom_col(aes(datetime, rainfall_mm_h, colour = legnd[2], fill=legnd[2])) + 
    
    ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_reverse(limits = c(ylimit1, 0)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(), 
                                    legend.position = "none",
                                    axis.title.x=element_blank()
                                    ,axis.text.x=element_blank()
                                    ,axis.ticks.x=element_blank()) +
    scale_colour_manual(values= c("grey50")) +
    scale_fill_manual(values = c("cyan", "dodgerblue3", 
                                 "grey50"))
  
  p2 <- ggplot(outputs) +
    
    geom_col(data = subset(outputs,                                             # Subset origional data for year of ts between dates
                           datetime >= as.POSIXct(EVENTS$event.start.ts[i]) &    # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
                             datetime <= as.POSIXct(EVENTS$event.end.ts[i])),   # 24 h after year start
             aes(datetime, event*ylimit2, fill=legnd[5]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
    
    geom_col(data = subset(outputs, 
                           response.eventID == i),
             aes(datetime, response.event*ylimit2, fill=legnd[6]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
    
    geom_line(aes(datetime, baseflow_m3_s, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, baseflow_m3_s+storminterflow_m3_s, colour = legnd[9], linetype =legnd[9])) +
    geom_line(aes(datetime, q_m3_s, colour = legnd[4], linetype =legnd[4])) +
    
    geom_point(data = peaks, aes(datetime, q, colour = legnd[10]), shape = 1) +
    geom_point(data = peaks.overQmag.high, aes(datetime, q, colour = legnd[10]), shape = 16) +
    
    
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_continuous(limits = c(0,ylimit2)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "none") + #(base_size = 12) +          # blank element removes background to legend
    scale_colour_manual(values= c("grey40","grey40","black", "red")) +
    scale_linetype_manual(values= c("longdash", "dotted", "solid")) +
    scale_fill_manual(values = c("cyan", "dodgerblue3"))
  
  p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
  print(p)
  
  dev.off()
  
  
}

# Dynaminc y scale

for (i in n){ 
  
  message(paste0("Plotting final event no: ", EVENTS$eventIDnew[i]))
  
  # fixed window size (3.5 days)
  xlim_min <- as.POSIXct(EVENTS$Q.response.start.ts[i]-(86400*3), tz = "UTC")     # set the start date for the plot (12 hr befor start)
  xlim_max <- as.POSIXct(EVENTS$Q.response.start.ts[i]+(86400*3), tz = "UTC") 
  
  ylimit1d <- max(outputs[outputs$datetime %in% xlim_min:xlim_max, "rainfall_mm_h"], na.rm = TRUE) 
  ylimit2d <- max(outputs[outputs$datetime %in% xlim_min:xlim_max, "q_m3_s"], na.rm = TRUE)
  
  ## open file connection
  tiff(paste0(sitename, "/run_",runID,"/final_plots/eventPlot",i, "_",
              format(EVENTS$Q.response.start.ts[i],"%Y%m%d_%H%M"),
              ".tiff"),
       width = 100, height = 100, units = 'mm', res = 600, compression = "zip")  
  
  ## save plots
  p1 <-   ggplot(outputs[outputs$datetime %in% xlim_min:xlim_max, ]) + 
    
    geom_col(data = subset(outputs,                                             # Subset origional data for year of ts between dates
                           datetime >= as.POSIXct(EVENTS$event.start.ts[i]) &   # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
                             datetime <= as.POSIXct(EVENTS$event.end.ts[i])),   # 24 h after year start
             aes(datetime, event*ylimit1d, fill=legnd[5]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
    
    geom_col(data = subset(outputs, 
                           response.eventID == i),
             aes(datetime, response.event*ylimit1d, fill=legnd[6]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
    
    geom_col(aes(datetime, rainfall_mm_h, colour = legnd[2], fill=legnd[2])) + 
    
    ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_reverse(limits = c(ylimit1d,0)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(), 
                                    legend.position = "none",
                                    axis.title.x=element_blank()
                                    ,axis.text.x=element_blank()
                                    ,axis.ticks.x=element_blank()) +
    scale_colour_manual(values= c("grey50")) +
    scale_fill_manual(values = c("cyan", "dodgerblue3", "grey50"))
  
  p2 <- ggplot(outputs[outputs$datetime %in% xlim_min:xlim_max, ]) +
    
    geom_col(data = subset(outputs,                                             # Subset origional data for year of ts between dates
                           datetime >= as.POSIXct(EVENTS$event.start.ts[i]) &    # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
                             datetime <= as.POSIXct(EVENTS$event.end.ts[i])),   # 24 h after year start
             aes(datetime, event*ylimit2d, fill=legnd[5]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
    
    geom_col(data = subset(outputs, 
                           response.eventID == i),
             aes(datetime, response.event*ylimit2d, fill=legnd[6]), 
             width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
    
    geom_line(aes(datetime, baseflow_m3_s, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, baseflow_m3_s+storminterflow_m3_s, colour = legnd[9], linetype =legnd[9])) +
    geom_line(aes(datetime, q_m3_s, colour = legnd[4], linetype =legnd[4])) +
    
    geom_point(data = peaks, aes(datetime, q, colour = legnd[10]), shape = 1) +
    geom_point(data = peaks.overQmag.high, aes(datetime, q, colour = legnd[10]), shape = 16) +
    
    
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_continuous(limits = c(0,ylimit2d)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "none") + #(base_size = 12) +          # blank element removes background to legend
    scale_colour_manual(values= c("grey40","grey40","black", "red")) +
    scale_linetype_manual(values= c("longdash", "dotted", "solid")) +
    scale_fill_manual(values = c("cyan", "dodgerblue3"))
  
  p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
  print(p)
  
  dev.off()
  
}

#---- > INPUTSOUTPUTS: save variables and other key stats ---------------------

# INPUTSOUTPUTS[["baseSep_alpha"]] <- userinput$alpha
# INPUTSOUTPUTS[["baseSep_pad"]] <- userinput$pad
# INPUTSOUTPUTS[["baseSep_passes"]] <- userinput$passes
# INPUTSOUTPUTS[["baseSep_calcbfi"]] <- round(BFI, 5)
# 
# INPUTSOUTPUTS[["eventEx_rainfall"]] <- "stoodleigh_radar"
# INPUTSOUTPUTS[["eventEx_alpha"]] <- userinput$alpha
# INPUTSOUTPUTS[["eventEx_pad"]] <- pad
# INPUTSOUTPUTS[["eventEx_passes"]] <- passes
# INPUTSOUTPUTS[["eventEx_value"]] <- "stormflow from q"
# INPUTSOUTPUTS[["eventEx_peakfvalue"]] <- "stormflow from q"
# INPUTSOUTPUTS[["eventEx_peakfspanhr"]] <- spanhr
# INPUTSOUTPUTS[["eventEx_peakfspan"]] <- round(span, 5)
# 
# INPUTSOUTPUTS[["eventEx_Qbase"]] <- Qbase
# #INPUTSOUTPUTS[["eventEx_Qchange"]] <- Qchange
# INPUTSOUTPUTS[["eventEx_Qmagnitude"]] <- Qmagnitude
# INPUTSOUTPUTS[["eventEx_QslopeAsc"]] <- QslopeAsc
# INPUTSOUTPUTS[["eventEx_QslopeDec"]] <- QslopeDec
# INPUTSOUTPUTS[["eventEx_stablepeakthrhldQXX24hr"]] <- "Q50in24hrCentered"
# 
# INPUTSOUTPUTS[["eventEx_nrainfallevents"]] <- max(eventEx$rainfall.eventID, na.rm = T)
# INPUTSOUTPUTS[["eventEx_nresponseevents"]] <- max(eventEx$response.eventID, na.rm = T)
# INPUTSOUTPUTS[["eventEx_pctrainfallevents"]] <- sum(!is.na(eventEx$rainfall.eventID))/nrow(eventEx)*100
# INPUTSOUTPUTS[["eventEx_pctresponseevents"]] <- sum(!is.na(eventEx$response.eventID))/nrow(eventEx)*100
# INPUTSOUTPUTS[["eventEx_nEVENTS"]] <- nrow(EVENTS)
# INPUTSOUTPUTS[["eventEx_nEVENTSerr"]] <- nrow(EVENTSerr)

useroutput[["eventEx_nrainfallevents"]] <- max(eventEx$rainfall.eventID, na.rm = T)
useroutput[["eventEx_nresponseevents"]] <- max(eventEx$response.eventID, na.rm = T)
useroutput[["eventEx_pctrainfallevents"]] <- sum(!is.na(eventEx$rainfall.eventID))/nrow(eventEx)*100
useroutput[["eventEx_pctresponseevents"]] <- sum(!is.na(eventEx$response.eventID))/nrow(eventEx)*100
useroutput[["eventEx_nEVENTS"]] <- nrow(EVENTS)
useroutput[["eventEx_nEVENTSerr"]] <- nrow(EVENTSerr)

# summary(eventEx["rainfall.eventID"])
# sum(!is.na(eventEx$rainfall.eventID))/nrow(eventEx)*100
# 
# 
# summary(eventEx["response.eventID"])
# sum(!is.na(eventEx$response.eventID))

str(eventEx)

## save inputsoutputs
write.csv(userinput, paste0(sitename, "/run_",runID,"/eventEx_user_inputs.csv"))
write.csv(useroutput, paste0(sitename, "/run_",runID,"/eventEx_user_outputs.csv"))


#### END #######################################################################


# # PLOT an indervidual event or time period
#
xlim_min <- as.POSIXct(strptime("2017-06-17 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
lim_max <- as.POSIXct(strptime("2018-11-30 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot

legnd <- c("Event","Rainfall", "I. Slow flow","III. Flow",
           "Event (rain)", "Event (response)", "II. Intermediate flow")

update_geom_defaults("line", list(size = 0.2))

p1 <-  ggplot(outputs) +
   geom_col(aes(datetime, event*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
   geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
   geom_col(aes(datetime, rainfall_mm_h, colour = legnd[2], fill=legnd[2]), width = 0.5) +
   ylab("Rainfall (mm/h)")+
   ylab (expression(Rainfall~(mm~h^{-1}))) +
   ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
   scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                    date_labels = ("%Y-%m-%d\n%H:%M")) +
   scale_y_reverse(limits = c()) +
   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                   legend.title=element_blank(),
                                   legend.position = "none",
                                   axis.title.x=element_blank()) +
   scale_colour_manual(values= c("grey50")) +
   scale_fill_manual(values = c("cyan", "dodgerblue3", "grey50"))

 p2 <-   ggplot(outputs) +
   geom_col(aes(datetime, event*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15), alpha=0.3) +
   geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15), alpha=0.2) +
   geom_line(aes(datetime, baseflow_m3_s, colour = legnd[3], linetype =legnd[3])) +
   geom_line(aes(datetime, baseflow_m3_s+storminterflow_m3_s, colour = legnd[7], linetype =legnd[7])) +
   geom_line(aes(datetime, q_m3_s, colour = legnd[4], linetype =legnd[4])) +
   xlab ("Date") +
   ylab (expression(Flow~(m^{3}~s^{-1}))) +
   scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                    date_labels = ("%Y-%m-%d\n%H:%M")) +
   scale_y_continuous(limits = c(0,ylimit2)) +
   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                   legend.title=element_blank(),
                                   legend.position = "bottom",
                                   legend.justification="left") +                           # blank element removes background to legend

   scale_colour_manual(values= c("grey40", "grey40",  "black")) +
   scale_linetype_manual(values= c("longdash", "dotted","solid")) +
   scale_fill_manual(values = c("cyan", "dodgerblue3")) +
   guides(colour = guide_legend(ncol = 1)) +
   guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                              ncol = 1))


 p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
 print(p)

 tiff(paste0("plots/ts_07_eventEx_rainrunoff_events_OUTPUT_",
             sitename, "East Budleigh",
             format(head(outputs$datetime, 1),"%Y%m%d"), "_",
             format(tail(outputs$datetime, 1),"%Y%m%d"),".tiff"),
      width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
 print(p)

 dev.off()

 
 setwd(proj_root) # reset working directory
