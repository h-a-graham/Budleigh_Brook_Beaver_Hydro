##### TITLE: RDF_LyneHollick ###################################################

# Filename: RDF_LyneHollick.R
# Type: Script
# Title: 
# Version: DRAFT 1.0
# Date: 2019-09-09
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: 
# License: 

# Project: eventExtraction

##### BASEFLOW SEPERATION (MASTER) #############################################

#==== Single Parameter Digital Filter ==========================================

#.... Description ..............................................................

#  Baseflow seperation from:
# 
#  Nathan, R.J. and T.A. McMahon, 1990. 
#  Evaluation of Automated Techniques for Baseflow and Recession Analysis. 
#  Water Resources Research, 26(7):1465-1473.
#                                                                                                 
#  Method based on signal analysis filter (Lyne  and  Hollick,  1979) with standard format presented by Ladson et al. (2013)
#
#  CAVEATS:
#     "The above techniques are essentially analytical tools for achieving an approximate separation of base flow. 
#      Although they are recognized to be arbitrary, they at least have the merit of constantcy"
#                                                                                                   (Nathan and McMahon, 1990)
# 
#     "simulate actual base flow  conditions, but rather they are aimed at deriving an objective index related to base flow response"
#                                                                                                   (Nathan and McMahon, 1990)
# 
#  Pros: objective, repeatable, easily automated
#  Cons: arbitrary and physically unrealistic
# 
#  N.B 
#
#   = The value of alpha affects the degree of attenuation (0.98 selected by visual inspection)
#
#   = N.B multiple passes 
#     - The number of passes affects the degree of smoothing. Nathan and McMahon (1990) apply forward, backward, then forward passes
#     - a 'padding' of XX reflected values must be applied during the calculation stages to the start and end of the input q(i) time series 
#       (to avoid 'warming up' and 'cooling down' effects) (Ladson et al. 2013).
#     - For time series with higher temporal frequency it is recommended the number of passes are increased; 
#       for example, 9 passes have been used for hourly data (Ladson et al. 2013; Murphy et al. 2011).


##### CODE: ####################################################################  

#==== A. Set up for R (all set in RUN script) ==================================

# #---- (X) A.1 Set user defined variables -------------------------------------------
#
# #---- (X) *** Manually set *** ---- 
# ##  *** NOTE: these are set in the main RUN code, if they need to be set here:
# 
# site <- "test001"
# publishedBFI <- 0.00
# 
# ## set in baseflowSep code as default, can be changed to do multiple run tests
# runs <- 1                                                                       # set the number of runs (1 for event extraction)
# 
# ## Timeseries details (e.g. use data at 15 min frequency for only 2015)
# tsfrq <- 15                                                                     # frequency of data (in minutes)
# ts1 <- as.POSIXct("2015-01-01 00:00", tz = "UTC")                               # ts start (default: first timestamp avaiable)
# tsn <- as.POSIXct("2015-06-30 23:45", tz = "UTC")                               # ts end (default: last timestamp avaiable)
# 
# ## set in run code
# alpha <- 0.98      # rep(seq(0.9, 0.99, by=0.005))                              # in range 0.9-0.95 (Nathan and McMahon, 1990) or 0.98
# pad <- (60*24*30)/tsfrq  # (30 days)                                                  # number of reflected valued used in padding (at each end)
# passes <- 9        # default for 15 min date used is 9 passes                   # *** MUST BE ODD NO**** set number of passes (affects degree of smoothing and phase distortion)
# 
# #---- (X) *** END manually set *** ----
#
# #---- (X) A.2 Source library and function scrips -----------------------------------
#
# #..... Packages in use .......................................................
# 
# # installing the packages and loading libraries:
# 
# if(!require(dplyr)) {
#   install.packages("dplyr"); require(dplyr)}  #load / install+load dplyr
# if(!require(psych)) {
#   install.packages("psych"); require(psych)}  #load / install+load psych
# if(!require(tidyr)) {
#   install.packages("tidyr"); require(tidyr)}  #load / install+load tidyr
# if(!require(zoo)) {
#   install.packages("zoo"); require(zoo)}  #load / install+load zoo
# if(!require(lubridate)) {
#   install.packages("lubridate"); require(lubridate)}  #load / install+load lubridate
# 
# #.... Packages in use (PLOTS) ................................................
# 
# if(!require(ggplot2)) {
#   install.packages("ggplot2"); require(ggplot2)}  #load / install+load ggplot2
# if(!require(gtable)) {
#   install.packages("gtable"); require(gtable)}  #load / install+load gtable
# if(!require(cowplot)) {
#   install.packages("cowplot"); require(cowplot)}  #load / install+load cowplot
#
#
# #---- (X) A.3 Set up data ----------------------------------------------------------
#
# ## use above to set period of interest and frequency of interest
# datetime <- seq.POSIXt(ts1,                                                        # start date time for sequence
#                        tsn,                                                        # end date time for sequence
#                        paste0(tsfrq," min"))                                       # increment (e.g. 15 mins)
# datetime <- data.frame(datetime)                                                      # assign ts.UTC as a dataframe to ts
# str(datetime)
# 
# dat <- datetime


#==== FILTER ===================================================================

# .... set the run counter .....................................................

runCount <- 1:runs                                                              # run counter

#---- I. Set up the base seperation dataframe ----------------------------------

## set passcounter
passCount <- 1:passes                                                           # total passcount (always ending on forward)

## in 'normal' run mode
r = 1

## Set up baseSep dataframe
baseSep <- setNames(
  data.frame(matrix(ncol = 7, nrow = nrow(dat))),                           
  c("k",                                                                        # kth timestep
    "ts",                                                                       # time stamp
    "q",                                                                        # flow (Q)
    "f",                                                                        # filtered fast flow componenet
    "fconstrained",                                                             # filtered constrained fast flow componenet
    "bconstrained",                                                             # filtered constrained base flow componenet
    "qworking"))                                                                # working q for each pass

## Import time date and flow values
baseSep$ts <- dat$datetime                                                      # get timestamp
baseSep$q <- dat$value                                                          # get Streamflow (Q) at timestep k

## Populate working q from observed flow 
baseSep$qworking <- baseSep$q                                                     # get working Q column (updated for each pass)

#.... NA treatment ...........................................................

baseSep <- baseSep %>% 
  fill(qworking, .direction = c("down")) %>%                                    # NA value filled with preededing value (creats a flatline)
  fill(qworking, .direction = c("up"))                                   # eremaining NA value filled with following value (creats a flatline)

#.... PLOT ...................................................................

## set x limits
xlim_min <- ts1 #as.POSIXct(strptime("2017-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
xlim_max <- tsn #as.POSIXct(strptime("2018-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot

legnd <- c("q", "f", "fconstrained", "qworking")

p <- ggplot(baseSep) +
  geom_line(aes(ts, q, colour = legnd[1], linetype =legnd[1])) +
  xlab ("Date") +
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  #scale_y_continuous(limits = c(0,120)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom") + #(base_size = 12) +            # blank element removes background to legend
  scale_colour_manual(values= c("black")) +
  scale_linetype_manual(values= c("solid"))

print(p)


#---- II. Generate padded dataframe baseSep  after, Ladson et al. (2013) -----

## Generate dataframe padding from reflected start and end values
padSTR <- baseSep[(pad+1):2,]                                                   # e.g pad=30 30 values reflected at STaRt q[31], q[30], ..., q[2], q[1], q[2,...                                       
padEND <- baseSep[c((nrow(baseSep)-1):(nrow(baseSep)-pad)),]                    # e.g pad=30 30 values reflected at END ..., q[n-1], q[n], q[n-1], ..., q[n-30]

## bind reflected padding for start and end to the main dataframe
baseSep <- dplyr::bind_rows(padSTR, baseSep, padEND, .id = NULL)  


#---- III. Apply filter (multiple passes) ------------------------------------

# "For the first pass, the starting value of quickflow is set equal to the initial value of streamflow for the padded data set" (Ladson et al. 2013)

baseSep$k <- 1:nrow(baseSep)                                                    # set k to match new row numbers (subset/padded)
#baseSep$f[1] <- baseSep$q[1]                                                   # initialise: fastflow/stormflow with first value of padded Q
baseSep$f[1] <- baseSep$qworking[1]                                             # initialise: fastflow/stormflow with first value of padded Q 
baseSep$b[1] <- 0                                                               # initialise: baseflow with zero





#.... filter loops ...........................................................

## set up count for filter 
count <- (2:((nrow(baseSep))))                                                  # after initial value to nrows

for (p in passCount){                                                           # for each pass of the passcount
  
  for (k in count){                                                             # for each row of the count                                                        
    
    baseSep$f[k] <-
      (alpha[r]*baseSep$f[k-1]) + (((1+alpha[r])/2) * 
                                     (baseSep$qworking[k]-baseSep$qworking[k-1])) # filtered quick response for timestep k + 1
  } 
  
  ## Calculate constrained constrained quick flow and constrained baseflow
  # "The output of the filter was constrained so that the separated slow flow was not negative or greater than the original streamflow."
  # (Nathan and McMahon, 1990) 
  
  baseSep <- baseSep %>%                                                        # pipe
    mutate(fconstrained =                                                       # constrain baseflow by Q
             ifelse(is.na(qworking), NA, 
                    ifelse(f > 0, f, 0))) %>%   
    mutate(bconstrained =                                                       # constrain baseflow by Q
             ifelse(is.na(qworking), NA, qworking-fconstrained))
  
  
  ## temporary BFI estimagte (with padding and NA fills)
  baseSep.temp <- baseSep[c((pad+1):(nrow(baseSep)-pad)),]
  bfitemp <- sum(baseSep.temp$bconstrained[which(!is.na(baseSep.temp$q))],na.rm = TRUE)/
    sum(baseSep.temp$q[which(!is.na(baseSep.temp$q))], na.rm = TRUE)
  rm(baseSep.temp)
  
  ## if (test) reverse dataframe / pass direction
  ## fb fb fb fb f etc.
  if(((p+1) %% 2L == 0L) | ((p) %% 2L == 0L)) {
    baseSep$k <- rev(baseSep$k)                                                 # reverse count k (for backward/forward pass alternation)
    baseSep<-baseSep[nrow(baseSep):1,]}
  
  ## update working q for the next pass
  baseSep$qworking <- baseSep$bconstrained                                    # update working Q "filter applied using b first pass time series as input" (Ladson et al. 2013)
  
  ## initiate f for the next pass
  baseSep$f[1] <- baseSep$bconstrained[1]                                     # "initial value quickflow set to final value of b from previous pass" (Ladson et al. 2013)
  
  message(
    sprintf("Single Parameter Digital Filter:   pass no. %s   alpha = %s   pass BFI = %s", 
            p, alpha[r], bfitemp))                                      # console message with pass number complete & alpha value
  
  ## pass BFI estimate
  roundBFI2 <- round(bfitemp,digits=2)
  
  # ## test - if pass is on an uneven run and BFI matches published BFI then exit loop
  # if ((p %% 2) != 0 & roundBFI2 <= round(publishedBFI,digits=2)) {
  #   break
  #   }
  
}                                                                             # END pass loop    

#.... re-sort dataframe ........................................................

## if the pass number is odd reverse the dataframe
if((p+1) %% 2L == 0L) {
  baseSep$k <- rev(baseSep$k)
  baseSep<-baseSep[nrow(baseSep):1,]}


# #.... PLOT .....................................................................
# 
# legnd <- c("q", "f", "f constrained", "b constrained")
# update_geom_defaults("line", list(size = 0.3))
# 
# ggp <- ggplot(baseSep) +
#   geom_line(aes(k, bconstrained, colour = legnd[4], linetype =legnd[4])) +
#   geom_line(aes(k, q, colour = legnd[1], linetype =legnd[1])) +
#   geom_line(aes(k, f, colour = legnd[2], linetype =legnd[2])) +
#   geom_line(aes(k, fconstrained, colour = legnd[3], linetype =legnd[3])) +
#   labs(caption = paste0("pass no = ", p, 
#                         ",  calculated BFI = ", round(bfitemp,digits=3))) +
#   xlab ("Date") +
#   ylab (paste0("Flow\n(m³ s⁻¹)")) +
#   scale_x_continuous(limits = c(2880,(nrow(baseSep)-2880))) +
#   #scale_y_continuous(limits = c(-20,120)) +
#   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                                   legend.title=element_blank(),
#                                   legend.position = "bottom") +               
#   scale_colour_manual(values= c( "grey60", "orange", "red", "black")) +
#   scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid"))
# 
# png(paste0("plots/","ts_02_baseSep_passoutput_", site, "_",
#            "_pass", p, "_",
#            format(Sys.time(),"%Y%m%d_%H%M"),".png"),
#     width = 210, height = 90, units = 'mm', res = 600) # to make squares
# #width = 210, height = 250, units = 'mm', res = 600) # to make squares
# print(ggp)
# dev.off()
# 
# #...............................................................................


#---- IV. Baseflow index (BFI) ------------------------------------------------

baseSep <- baseSep[c((pad+1):(nrow(baseSep)-pad)),]                             # delete padding values

#BFI <- sum(baseSep$bconstrained,na.rm = TRUE)/sum(baseSep$q, na.rm = TRUE)

BFI <- sum(baseSep$bconstrained[which(!is.na(baseSep$q))],na.rm = TRUE)/
  sum(baseSep$q[which(!is.na(baseSep$q))], na.rm = TRUE)

message(sprintf(
  "Baseflow Index (BFI): %s\n ----------------------------------------------", 
  BFI))                              

roundBFI <- round(BFI,digits=3)

# e.g.
# DTM subset 2015-01-01 00:00 to 2015-02-01 00:00
# passes 5, alpha 0.925, Baseflow Index (BFI): 0.68640797376965
# passes 5, alpha 0.98, Baseflow Index (BFI): 0.539188006392839
# passes 9, alpha 0.925, Baseflow Index (BFI): 0.576663967323473  
# passes 9, alpha 0.98, Baseflow Index (BFI): 0.430133294547281                 # *** this combination used for proof of concept HIC2018

#}                                                                               # END run loop


#---- Save run outputs (if required) -------------------------------------------

testtime <- paste0(format(Sys.time(),"%Y%m%d_%H%M"))
runname <- paste0("_padding", pad,
                  "_alpha", alpha,
                  "_passes", p, 
                  "_BFI",roundBFI)

write.csv(baseSep, paste0("./working/baseSep_", site, runname,"_", testtime, ".csv"))
saveRDS(baseSep, paste0("./working/baseSep_", site, runname,"_", testtime,  ".rds"))

#return(baseSep)

#---- V. Baseflow outputs ------------------------------------------------------

# ## *** NOTE: this is run in the main RUN code
#
# #.... Baseflow sep output to main dataframe...................................
# 
# eventEx$baseflow <- baseSep$bconstrained                                      # update eventEx dataframe baseflow
# eventEx$stormflow <- eventEx$value - eventEx$baseflow                         # update eventEx dataframe stormflow

rm(alpha, count, k,
   pad, 
   padEND, padSTR,
   p, passCount, 
   passes,
   r, runCount, runs,
   site, roundBFI, roundBFI2, bfitemp)                                                           # tidy R environment

rm(testtime)

#.... Plot check - eventEx dataframe ..........................................

## *** NOTE: this is run in the main RUN code

# legnd <- c("Event","Rainfall", "Baseflow","Q", "Response event", "value", "smoothed", "peak")
# 
# xlim_min <- as.POSIXct(strptime("2017-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
# xlim_max <- as.POSIXct(strptime("2018-10-15 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot
# 
# update_geom_defaults("line", list(size = 0.3))
# 
# p1 <- ggplot() +
#   geom_col(data = eventEx, aes(datetime, rainfall, colour = legnd[1], fill=legnd[1]), width = 0.5) +
#   #geom_col(data = rain_daily, aes(datetime, prcp_amt, colour = legnd[1], fill=legnd[1]), width = 0.5) +
#   #xlab ("Date") +
#   ylab("Rainfall\nintensity\n(mm h⁻¹)")+
#   scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
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
# p2 <- ggplot(eventEx) +
#   geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
#   geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
#   xlab ("Date") +
#   ylab (paste0("Flow\n(m³ s⁻¹)")) +
#   labs(caption = paste0("calculated BFI = ", roundBFI3)) +
#   scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
#                    date_labels = ("%Y-%m-%d\n%H:%M")) +
#   #scale_y_continuous(limits = c(0,0.05)) +
#   theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                      legend.title=element_blank(),
#                      legend.position = "none") + #(base_size = 12) +            # blank element removes background to legend
#   scale_colour_manual(values= c("grey60", "black")) +
#   scale_linetype_manual(values= c("longdash", "solid"))
# 
# p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
# print(p)
# 
# {png(paste0("plots/","ts_baseSep_output_",
#            userinput$site,
#            runname, "_",
#            format(Sys.time(),"%Y%m%d_%H%M"),".png"),
#     width = 210, height = 100, units = 'mm', res = 600) # to make squares
# #width = 210, height = 297, units = 'mm', res = 600) # A4
# print(p)
# dev.off()}


##### END ######################################################################
