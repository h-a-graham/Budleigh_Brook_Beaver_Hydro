##### TITLE: eventEx_01_input ##################################################

# Filename: eventEx_01_input.R
# Type: Script
# Title: 
# Version: DRAFT 1.0
# Date: 2019-09-18
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: 
# License: 

# Project: eventExtraction

##### NOTES: ###################################################################

# Dataset example optimised for: EXE at Stoodleigh

# DESCRIPTION:
# Input codes for preparing data frame for eventEx_00_RUN

# BUGS:


##### CODE: ####################################################################  

#==== A. Set up for R ==========================================================

#---- > A.1 Source library and function scrips ---------------------------------

if(!exists("foo", mode = "function")) source("eventEx_libraries.R")
if(!exists("foo", mode = "function")) source("eventEx_functions.R")


#---- > A.2 Set user defined variables -----------------------------------------

#.... *** START Manually set *** ............................................... 

# ### SET IN MAIN RUN SCRIPT ###
#
# site_input <- list(
#   
#   ## site name
#   site = "stoodleigh",
#   
#   ## BFI (published, if non avaiable use 0.00)
#   publishedBFI = 0.51, # e.g. Exe at Stoodleigh (0.51)
#   
#   ## Timeseries details 
#   # (e.g. use data at 15 min frequency for hydrological year 2017-2018)
#   tsfrq = 15,                                                                   # frequency of data (in minutes)
#   ts1 = as.POSIXct("2017-10-01 00:00", tz = "UTC"),                             # ts start (default: first timestamp avaiable) 
#   tsn = as.POSIXct("2018-09-30 23:55", tz = "UTC")                              # ts end (default: last timestamp avaiable)
#   
# )
# 
# baseSep_input <- list(
#   
#   ## user defined variables (for L&H recursive digital filter baseflow seperation)
#   alpha = 0.995,                                                                # in range 0.9-0.95 (Nathan and McMahon, 1990) or 0.98
#   pad = 30*24*60/site_input$tsfrq, #(30 days *24 hours* every 15 mins)          # number of reflected valued used in padding (at each end)
#   passes = 9                                                                    # *** MUST BE ODD NO**** set number of passes (affects degree of smoothing and phase distortion)
#   
# ) 
# 
# ## combine input lists
# listof_input <- list(site_input,baseSep_input)
# userinput <- do.call(c, listof_input)
# 
# ## remove seperate input files
# rm(list = ls(pattern = "_input$")) 
# 
# #.... *** END manually set *** ................................................. 
# 
# ## use above to set period of interest and frequency of interest
# datetime <- seq.POSIXt(userinput$ts1,                                           # start date time for sequence
#                        userinput$tsn,                                              # end date time for sequence
#                        paste0(userinput$tsfrq," min"))                             # increment (e.g. 15 mins)
# datetime <- data.frame(datetime)                                                # assign ts.UTC as a dataframe to ts
# str(datetime)


#==== 1. SET UP ================================================================

#---- > 1.0 Import data from existing rds --------------------------------------

## read in RDS file
datlong <- readRDS(file.choose())

## order the data alphabetically by time
datlong <- datlong[order(datlong$parameter , decreasing = FALSE ),]
datlong <- datlong[order(datlong$datetime , decreasing = FALSE ),]

## check structure and file
str(datlong)
head(datlong)
tail(datlong)


#---- > 1.0 Import data from existing csv ------------------------------------

datlong <- read.csv(file.choose())

## date time to POSIXct
str(datlong)
datlong$datetime  <- as.POSIXct(strptime(as.character(datlong$datetime),                       # N.B. strptime() converts from character to POSIXct time
                                  "%Y-%m-%d %H:%M:%S",                           # Format of origional text date time
                                  tz = "UTC"))                                   # (the UTC specification needed to avoid GMT to BST bug!)

## order the data alphabetically by time
datlong <- datlong[order(datlong$parameter , decreasing = FALSE ),]
datlong <- datlong[order(datlong$datetime , decreasing = FALSE ),]

## check structure and file
str(datlong)
head(datlong)
tail(datlong)


#---- > 1.0 Import data from csv (seperate) ------------------------------------

## import 15 min rain data from csv ............................................

dat1 <- read.csv(file.choose())                                                 # selectfile e.g. Stoodleigh_Q_Rain_20140101_20161231_RInput.csv
str(dat1)

source <- "rainfall_radar" # "ea_tbr"

## select date and time from columns avaiable
# Using output from HGraham / BJackson work on c-band-radar
dat1$datetime <- dat1$date_time 
dat1$value <- dat1$rain_intensity_mmhr

# # Using EA TBR files
# dat1$datetime <- dat1$Time_stamp
# dat1$value <- dat1$Value_mm_

# ## ***IF REQUIRED*** values from mm per 15 min to mm per hr intensity
# dat1$value <- dat1$value*4

## subset to required columns
dat1 <- dat1[,c("datetime", "value")]

## rain time stamps
str(dat1)
dat1$datetime <- as.POSIXct(strptime(as.character(dat1$datetime),             # N.B. strptime() converts from character to POSIXct time
                                     "%Y-%m-%dT%H:%M:%SZ",                          # Format of origional text date time
                                     #"%d/%m/%Y %H:%M",                             # Format of origional text date time
                                     tz = "UTC"))                                 # (the UTC specification needed to avoid GMT to BST bug!)
str(dat1)

## cut to period of interest
dat1 <- left_join(datetime,data.frame(dat1))

## update units and parameter
dat1$units <- "mm/h"
dat1$parameter <- "rainfall"

dat1$source <- "c_band_rain_radar_1km"

# dat1$source <- "ea_tbr"

str(dat1)


## import q data from csv ......................................................

dat2 <- read.csv(file.choose())
str(dat2)

## select date and time from columns avaiable
dat2$datetime <- dat2$tsUTC

dat2 <- dat2[,c("datetime", "value", "units")]

## q time stamps
str(dat2)
dat2$datetime <- as.POSIXct(strptime(as.character(dat2$datetime),               # N.B. strptime() converts from character to POSIXct time
                                  "%d/%m/%Y %H:%M",                             # Format of origional text date time
                                  tz = "UTC"))                                  # (the UTC specification needed to avoid GMT to BST bug!)
str(dat2)

## cut to period of interest
dat2 <- left_join(datetime,data.frame(dat2))

## update units and parameter
dat2$units <- as.character(dat2$units)
dat2$parameter <- "q"
dat2$source <- "EA"

ea_flow_THO_20120101_20190930_JA <- dat2
ea_flow_THO <- data.frame("Date_Time" = dat2$datetime,
                          "Q_m3_s_THO" = dat2$value,
                          "Q_Flag_THO" = NA)

write.csv(dat2, paste0("../../3_Working_data_(Cleaned)/Flow_data/ea_flow_THO_20120101_20190930_JA.csv"))
write.csv(ea_flow_THO, paste0("../../3_Working_data_(Cleaned)/Flow_data/EA_Flow_THO.csv"))

#.... apply offset of 105 ......................................................

offset105_flow_NOR_20120101_20190930_JA <- dat2
head(offset105_flow_NOR_20120101_20190930_JA)

offset105_flow_NOR_20120101_20190930_JA$flow_m3_s_THO <- offset105_flow_NOR_20120101_20190930_JA$value
offset105_flow_NOR_20120101_20190930_JA$flow_m3_s_NOR <- lag(offset105_flow_NOR_20120101_20190930_JA$flow_m3_s_THO, 105/15)
offset105_flow_NOR_20120101_20190930_JA$value <- offset105_flow_NOR_20120101_20190930_JA$flow_m3_s_NOR

offset105_flow_NOR_20120101_20190930_JA$source <-"ea_offset_NOR_THOlag105min"

head(offset105_flow_NOR_20120101_20190930_JA, 10)

offset105_flow_NOR <- data.frame("Date_Time" = dat2$datetime,
                          "Q_m3_s_NOR" = dat2$value,
                          "Q_Flag_NOR" = "ea_offset_NOR_THOlag105min")

write.csv(offset105_flow_NOR_20120101_20190930_JA, paste0("../../3_Working_data_(Cleaned)/Flow_data/eaoffset105_flow_NOR_20120101_20190930_JA.csv"))
write.csv(offset105_flow_NOR, paste0("../../3_Working_data_(Cleaned)/Flow_data/eaoffset105_flow_NOR.csv"))

#.... apply offset of 090 ......................................................

head (ea_flow_THO_20120101_20190930_JA)

offset090scaled_flow_NOR_20120101_20190930_JA <- ea_flow_THO_20120101_20190930_JA
head(offset090scaled_flow_NOR_20120101_20190930_JA)

## THORVERTON: 
offset090scaled_flow_NOR_20120101_20190930_JA$flow_m3_s_THO <- offset090scaled_flow_NOR_20120101_20190930_JA$value

## NORTHBRIDGE: adjust for ossfet and adjust for contribution area differnce
offset090scaled_flow_NOR_20120101_20190930_JA$flow_m3_s_NOR <- NA
offset090scaled_flow_NOR_20120101_20190930_JA$flow_m3_s_NOR <- lag(offset090scaled_flow_NOR_20120101_20190930_JA$flow_m3_s_THO, 090/15) *1.038
offset090scaled_flow_NOR_20120101_20190930_JA$flow_m3_s_NOR
offset090scaled_flow_NOR_20120101_20190930_JA$value <- offset090scaled_flow_NOR_20120101_20190930_JA$flow_m3_s_NOR

offset090scaled_flow_NOR_20120101_20190930_JA$source <-"ea_offset_THOlag090min_ratio1point038"

head(offset090scaled_flow_NOR_20120101_20190930_JA, 10)



offset090scaled_flow_NOR <- data.frame("Date_Time" = offset090scaled_flow_NOR_20120101_20190930_JA$datetime,
                                 "Q_m3_s_NOR" = offset090scaled_flow_NOR_20120101_20190930_JA$value,
                                 "Q_Source_NOR" = "ea_offset_THOlag090min_ratio1point038")

write.csv(offset090scaled_flow_NOR_20120101_20190930_JA, paste0("../../3_Working_data_(Cleaned)/Flow_data/EArecalc_flow_NOR_20120101_20190930_JA.csv"))
write.csv(offset090scaled_flow_NOR, paste0("../../3_Working_data_(Cleaned)/Flow_data/EArecalc_flow_NOR.csv"))

#.... Summary ..................................................................

str(dat1)
str(offset090scaled_flow_NOR_20120101_20190930_JA)

head(dat1)
head(offset090scaled_flow_NOR_20120101_20190930_JA)

## bind into long dataframe
datlong <- bind_rows(dat1, offset090scaled_flow_NOR_20120101_20190930_JA[,1:5], .id = NULL)

## order the data alphabetically by time
datlong <- datlong[order(datlong$parameter , decreasing = FALSE ),]  
datlong <- datlong[order(datlong$datetime , decreasing = FALSE ),]  

head(datlong)
tail(datlong)

# ## bind into list
# dat_list <- list(q = offset105_flow_NOR_20120101_20190930_JA[,1:5],
#         rainfall = dat1)

#assign(paste0(userinput$site), dat_list)


#.... > 1.0 transform input data ...............................................

## convert to wide dataframe
datwide <- spread(datlong[,c("datetime",                                        # transpose the data using listed columns
                             "value",
                             "parameter")],
                  parameter, value)                                             # transposed column headers and records

str(datlong)
str(datwide)

# Structure includes:
# [,’datetime’] date (format dd/mm/yy HH:MM:SS)                                    # as POSIXct
#* [,’stage’] river stage (m)                                                   # as num *may not be used
# [,’q’] discharge (m³/s)                                                       # as num
# [,’rainfall’] rainfall intensity (mm/h)                                       # as num

rm(dat1, dat2)

## set up working dataframe
dat <- datwide

names(dat)                                                                      # get column headings


#---- > 1.1 Data checks and setup ----------------------------------------------


# ## select rainfall
# dat$rainfall <- dat$rainfall_radar

str(dat)                                                                        # get structure of dataframe
head(dat)                                                                       # get head of dataframe
tail(dat)                                                                       # get tail of dataframe

summary(dat[,c("q", "rainfall")])                                               # get summary of data (min max quartiles NAs etc.)


#.... > Save input values (if required) ........................................

testtime <- paste0(format(Sys.time(),"%Y%m%d_%H%M"))

write.csv(datlong, paste0("./working/eventEx_00_input__long_", userinput$site, "_", testtime, ".csv"))
saveRDS(datlong, paste0("./working/eventEx_00_input__long_", userinput$site, "_", testtime,  ".rds"))

write.csv(datwide, paste0("./working/eventEx_00_input__wide_", userinput$site, "_", testtime, ".csv"))
saveRDS(datwide, paste0("./working/eventEx_00_input__wide_", userinput$site, "_", testtime,  ".rds"))

write.csv(dat, paste0("./working/eventEx_00_input__dat_", userinput$site, "_", testtime, ".csv"))
saveRDS(dat, paste0("./working/eventEx_00_input__dat_", userinput$site, "_", testtime,  ".rds"))

rm(dat_list, datlong, datwide)


# list_site <- list(input = dat)
# assign(paste0(userinput$site), list_site)
# 
# list_oflists <- list(mget(paste0(userinput$site)))
# 
# do.call(c, list_oflists)

################################################################################
