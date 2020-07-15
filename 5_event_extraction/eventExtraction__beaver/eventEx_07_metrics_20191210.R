#### EVENT metrics #############################################################

BACKUP <- eventEx

## N.B. event metrics are based on each RESPONSE event. 

## Find out where these start:
rowQSTART <- which(eventEx$response.eventFlag == "STARTresponse")
rowQEND <- which(eventEx$response.eventFlag == "END")
head(rowrainSTART)                                                              # from "eventExtract_combEvents.R"

#---- I. dataframe setup -------------------------------------------------------

n <- c(1:length(rowQSTART))                                                     # event counter

EVENTS <- setNames(data.frame(matrix(ncol = 49, nrow = (length(n)))),           # Name columns of a new dataframe
                   c("eventID",                                                 # Event ID
                     "eventID.temp",
                     "eventIDnew",
                     "response.eventID",
                     
                     "event.start.ts",                                          # event start time (start of rainfall)
                     "event.end.ts",                                            # event end time (end of responce)
                     "event.dur",                                               # event duration (combined event window)
                     "event.Q.tot.m3",                                          # Q total (combined event window) 
                     
                     "event.baseflow.tot.m3",                                   # total filtered baseflow (combined event window) 
                     "event.quickflow.tot.m3",                                  # total flow elevated over base (combined event window) 
                     
                     ## rainfall
                     
                     "rain.start.ts",                                           # rainfall event (preceding responce) start time
                     "rain.eventn",                                             # number of rainfall events in overall event window
                     "rain.mean",                                               # rainfall mean intensity (for whole event window)
                     "rain.peak.mm.h",                                          # rainfall max (not first) peak (for whole event window)
                     "rain.peak.ts",                                            # rainfall max  (not first) peak timestamp(for whole event window)
                     "rain.tot.mm",                                             # rainfall total (for whole event window)
                     "rain.dur",                                                # rainfall duration (for whole event window)

                     "init.rain.mean",                                          # rainfall mean intensity (initialising rainfall event)                        
                     "init.rain.peak.mm.h",                                     # rainfall max (initialising rainfall event)
                     "init.rain.peak.ts",                                       # rainfall max timestamp (initialising rainfall event)
                     "init.rain.centroid.ts",                                   # rainfall centroid timestamp (initialising rainfall event)
                     "init.rain.tot.mm",                                        # rainfall total (initialising rainfall event)
                     "init.rain.dur",                                           # rainfall duration (initialising rainfall event)
                   
                     ## Q values in response event
                     
                     "Q.response.start.ts",                                      # start time for response event
                     "Q.response.end.ts",                                        # end time for response event
                     "Q.response.dur",                                           # dur time for response event
                     "Q.response.tot.m3",                                        # Q total for response event
                     "Q.response.base.m3",                                       # Q baseflow for response event  
                     "Q.response.quick.m3",                                      # Q non-baseflow for response event  
                     
                     ## Q peak
                     
                     "Q.nopeaks",                                               # number of peaks in an event
                     "Q.peak.m3.s",                                             # Q max (not first) peak (for whole event window)
                     "Q.peak.ts",                                               # Q max (not first) timestamp peak (for whole event window)
                     "Q.peak.rank",                                             # in comparison to the full record of interest - which percentile does peak Q fall in?
                     "Q.QXX",                                                   # in comparison to the full record of interest - which QXX bracket does this value fall in?
                     
                     ## lag and other times
                     
                     "lag.start.dur",                                           # start rain to start event
                     "lag.peak.dur",                                            # peak rain to peak runoff
                     "lag.peakinit.dur",                                        # peak of initialising rain to peak runoff
                      # "lag.dur",                                              # peak rainfall to peak Q
                      # "init.lag.dur",                                           # peak rainfall (initialising event) to peak Q
                     
                     
                     "rlimb.dur",                                               # quickflow.start.ts to Q.peak.ts [see quickflow.XXX.ts above]
                     "flimb.dur",                                               # Q.peak.ts to quickflow.end.ts [see quickflow.XXX.ts above]
                     "rlimb.rain",                                              # total rainfall (mm) during the rising limb
                     
                     ## interevent
                     
                     "interevent.dur",                                          # preceeding interevent period duration
                     "interevent.rainfall.tot.mm",                              # preceeding interevent period total rainfall
                     "interevent.Q.median",                                      # preceeding interevent median flow
                     
                     "anti.rain.mm3h",                                          # antecedent rain (mm in 3 hours)
                     "anti.rain.mm6h",                                          # antecedent rain (mm in 6 hours)                                       
                     "anti.rain.mm12h",                                         # antecedent rain (mm in 12 hours)
                     "anti.rain.mm24h",                                         # antecedent rain (mm in 24 hours)
                     "anti.rain.mm5d",                                          # antecedent rain (mm in 5 days)
                     "anti.rain.mm30d"                                          # antecedent rain (mm in 30 days)
                     ))

## columns as.POSIXct timestamps

EVENTS$event.start.ts <- as.POSIXct(-62167219200, origin = "1970-01-01", tz = "UTC")
EVENTS$event.end.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")

EVENTS$rain.start.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")
EVENTS$rain.peak.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")

EVENTS$init.rain.peak.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")
EVENTS$init.rain.peak.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")
EVENTS$init.rain.centroid.ts  <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")

EVENTS$Q.response.start.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")
EVENTS$Q.response.end.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")
EVENTS$Q.peak.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")

#EVENTS$quickflow.start.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")
#EVENTS$quickflow.end.ts <- as.POSIXct(-62167219200,origin = "1970-01-01", tz = "UTC")



## columns as durations (using package "lubridate")
EVENTS$event.dur <- as.duration(NA)
EVENTS$rain.dur <- as.duration(NA)
EVENTS$init.rain.dur <- as.duration(NA)

EVENTS$event.dur <- as.duration(NA)
EVENTS$Q.response.dur <- as.duration(NA)
#EVENTS$quickflow.dur <- as.duration(NA)

#EVENTS$lag.hr <- as.duration(NA)
EVENTS$rlimb.dur <- as.duration(NA)
EVENTS$flimb.dur <- as.duration(NA)

EVENTS$interevent.dur <- as.duration(NA)

str(EVENTS)

#---- II. Extract an event window and find out metrics for it ------------------

eventrows <- list()                                                             # used to create event window 

## for each responce event 
for (i in n){ 
  
  message(paste0("Extracting event: ", i))
  
 #.... define your event window: ...............................................
  
  eventrows[[i]] <- c(rowrainSTART[i]:rowQEND[i])                               # get row rain started in and row responce ended in
  
  # if starting rain matches starting rain of previous event...
  if(i>1) {
    if (rowrainSTART[i] == rowrainSTART[i-1]) {
      if(length(which(eventEx$response.eventID == (i-1) & eventEx$peaks == T)) != 0)
      {
    ## then start of rainfall is last peak of previous event
    eventrows[[i]] <- c(tail(which(eventEx$response.eventID == (i-1) & eventEx$peaks == T),1):rowQEND[i])

       }}}
  
  eventWin <- data.frame(eventEx[c(eventrows[[i]]),])                           # create a dataframe for the event window
  ux <- unique(eventWin$response.eventID)                                       # what event IDs exist in the window
  
  ##update main eventEx dataframe with event ID (correct for overlapping events)
     # eventEx$eventID.temp[c(rowrainSTART[i]: rowQEND[i])] <- i
  
  EVENTS$eventID[i] <- i
  EVENTS$eventID.temp[i] <- EVENTS$eventID[i]
  
  # eventEx$eventID[c(rowrainSTART[i]: rowQEND[i])] <- max(ux, na.rm = TRUE)                                    # extract the max ID value (the min might be end of previous event)
  # eventEx$eventID.temp[c(rowrainSTART[i]: rowQEND[i])] <- max(ux, na.rm = TRUE) 
  
  EVENTS$event.start.ts[i] <- as.POSIXct(eventWin$datetime[1],              
                                         origin = "1970-01-01", tz = "UTC") 
  # EVENTS$event.start.ts[i] <- as.POSIXct(eventWin$rainfall.startTs[1],              
  #                                        origin = "1970-01-01", tz = "UTC")     # event start time (start of rainfall)
  EVENTS$event.end.ts[i]  <- as.POSIXct(tail(eventWin$datetime,1),              
                                        origin = "1970-01-01", tz = "UTC")      # event end time (end of responce)
  EVENTS$event.dur[i] <- as.duration(tail(eventWin$datetime,1)-eventWin$datetime[1])  # event duration
  
 #.... rainfall metrics: .......................................................
  
  # overall:
  EVENTS$rain.start.ts[i] <- as.POSIXct(eventWin$datetime[1],
                                        origin = "1970-01-01", tz = "UTC")
  
  # EVENTS$rain.start.ts[i] <- as.POSIXct(ifelse(is.na(eventWin$rainfall.startTs[1]), 
  #                                   as.POSIXct(eventWin$datetime[1],              
  #                                              origin = "1970-01-01", tz = "UTC"),
  #                                   as.POSIXct(eventWin$rainfall.startTs[1],              
  #                                              origin = "1970-01-01", tz = "UTC")),
  #                                   origin = "1970-01-01", tz = "UTC")
  
  EVENTS$rain.eventn[i] <- sum(!is.na(unique(eventWin$rainfall.eventID)))   
  EVENTS$rain.mean[i] <- mean(eventWin$rainfall, na.rm = T) 
  EVENTS$rain.peak.mm.h[i] <- max(eventWin$rainfall, na.rm = T)                 # N.B - this is the max of all event window rain - not for initial rain              
 
  EVENTS$rain.peak.ts[i] <- as.POSIXct(
    eventWin$datetime[head(which(eventWin$rainfall == max(eventWin$rainfall, 
                                                    na.rm = T)),1)],
    origin = "1970-01-01", tz = "UTC") 

  EVENTS$rain.tot.mm[i] <- sum(eventWin$rainfall, na.rm = TRUE) / 4 #1h/15min   # N.B- this includes rainfall outside rainfall evnts
  
  EVENTS$rain.dur[i] <- as.duration(length(c(which(eventWin$rainfall > 0))) * 900)
  # EVENTS$rain.dur[i] <- as.period(sum(as.duration(eventWin$rainfall.eventDur),
  # na.rm=TRUE))                                                                  # BUG - combined totoal or rainfall events that END during the event
  
  if(length(which(!is.na(eventWin$rainfall.eventID))) == 0) {
    EVENTS$init.rain.mean[i] <- 0
    EVENTS$init.rain.peak.mm.h[i] <- 0
    EVENTS$init.rain.peak.ts[i] <- NA 
    EVENTS$init.rain.centroid.ts[i] <- NA 
    EVENTS$init.rain.tot.mm[i]  <- 0
    EVENTS$init.rain.dur[i] <- 0 
    
  } else {
    
    ## initialising rainfall event:
    EVENTS$init.rain.mean[i] <- mean(
      eventWin$rainfall[which(eventWin$rainfall.eventID == (min(eventWin$rainfall.eventID, na.rm = TRUE)))],
      na.rm = T)
    EVENTS$init.rain.peak.mm.h[i] <- max(
      eventWin$rainfall[which(eventWin$rainfall.eventID == (min(eventWin$rainfall.eventID, na.rm = T)))],
      na.rm = T)
    
    EVENTS$init.rain.peak.ts[i] <- as.POSIXct(eventWin$datetime[
      which.max(eventWin$rainfall == EVENTS$init.rain.peak.mm.h[i])],              
      origin = "1970-01-01", tz = "UTC")
    
    EVENTS$init.rain.centroid.ts[i] <- as.POSIXct(ifelse(length(head(which(eventWin$rainfall.eventcentroid == 1),1)) > 0, 
                                                         eventWin$datetime[head(which(eventWin$rainfall.eventcentroid == 1),1)],
                                                         NA), 
                                                  origin = "1970-01-01", tz = "UTC")
    
    EVENTS$init.rain.tot.mm[i] <- sum(
      eventWin$rainfall[which(eventWin$rainfall.eventID == (min(eventWin$rainfall.eventID, na.rm = T)))],
      na.rm = TRUE) /4 #1h/15min 
    
    EVENTS$init.rain.dur[i] <- as.duration(
      eventWin$rainfall.eventDur[tail(
        which(eventWin$rainfall.eventID == min(eventWin$rainfall.eventID, na.rm = TRUE)),1)])
    if (is.na(EVENTS$init.rain.dur[i])) {                                         # default to whole event window
      EVENTS$init.rain.dur[i] = EVENTS$event.dur[i]}
    
    # EVENTS$init.rain.dur[i] <- as.duration(
    #   eventWin$rainfall.eventDur[tail(
    #     which(eventWin$rainfall.eventID == eventWin$rainfall.eventID[1]),1)])
    # if (is.na(EVENTS$init.rain.dur[i])) {                                         # default to whole event window
    #   EVENTS$init.rain.dur[i] = EVENTS$event.dur[i]}
    
  }
  
  ## anticeedent rainfall: 
  eventEx.rain.start.row <- which(eventEx$datetime == (eventWin$datetime[1]))
  
  EVENTS$anti.rain.mm3h[i] <- sum(eventEx$rainfall[c((
    ifelse(eventEx.rain.start.row-(3*4) < 1, 1, eventEx.rain.start.row-(3*4))):
      eventEx.rain.start.row )], na.rm=TRUE) /4
  EVENTS$anti.rain.mm6h[i] <- sum(eventEx$rainfall[c((
    ifelse(eventEx.rain.start.row-(6*4) < 1, 1, eventEx.rain.start.row-(6*4))):
      eventEx.rain.start.row )], na.rm=TRUE) /4
  EVENTS$anti.rain.mm12h[i] <- sum(eventEx$rainfall[c((
    ifelse(eventEx.rain.start.row-(12*4) < 1, 1, eventEx.rain.start.row-(12*4))):
      eventEx.rain.start.row )], na.rm=TRUE) /4
  EVENTS$anti.rain.mm24h[i] <- sum(eventEx$rainfall[c((
    ifelse(eventEx.rain.start.row-(24*4) < 1, 1, eventEx.rain.start.row-(24*4))):
      eventEx.rain.start.row )], na.rm=TRUE) /4
  EVENTS$anti.rain.mm5d[i] <- sum(eventEx$rainfall[c((
    ifelse(eventEx.rain.start.row-(5*24*4) < 1, 1, eventEx.rain.start.row-(5*24*4))):
      eventEx.rain.start.row )], na.rm=TRUE) /4
  EVENTS$anti.rain.mm30d[i] <- sum(eventEx$rainfall[c((
    ifelse(eventEx.rain.start.row-(30*24*4) < 1, 1, eventEx.rain.start.row-(30*24*4))):
      eventEx.rain.start.row )], na.rm=TRUE) /4
  
 #.... Q responce metrics .......................................................
  
  ## Q for responce event period:
  
  EVENTS$Q.response.start.ts[i] <- as.POSIXct(
    eventWin$datetime[head(which(eventWin$response.eventID == i),1)],  
    origin = "1970-01-01", tz = "UTC")
  EVENTS$Q.response.end.ts[i] <- as.POSIXct(
    eventWin$datetime[tail(which(eventWin$response.eventID == i),1)],  
    origin = "1970-01-01", tz = "UTC") 
  EVENTS$Q.response.dur[i] <- as.duration(
    EVENTS$Q.response.end.ts[i]-EVENTS$Q.response.start.ts[i])
  EVENTS$Q.response.tot.m3[i] <- sum(eventWin$q[c(
    which(eventWin$response.eventID == i))], 
    na.rm = TRUE) * 900 #seconds in 15 min                                      # this is for flow response duration 
  EVENTS$Q.response.base.m3[i] <- sum(eventWin$baseflow[c(
    which(eventWin$response.eventID == i))], 
    na.rm = TRUE) * 900 #seconds in 15 min                                      # this is for flow response duration 
  EVENTS$Q.response.quick.m3[i] <- sum(eventWin$stormflow[c(
    which(eventWin$response.eventID == i))], 
    na.rm = TRUE) * 900 #seconds in 15 min                                      # this is for flow response duration  
  
  ## Q peak metrics:
  
  #eventWin$q[c(which(eventWin$response.eventID == i))]
  
  # for responce event window
  
  # EVENTS$Q.nopeaks[i] <- sum(eventWin$peaks[
  #   c(which(eventWin$value > median(eventWin$value, na.rm = TRUE)))])
  
  #EVENTS$Q.nopeaks[i] <- sum(eventWin$peaks)
  EVENTS$Q.nopeaks[i] <- sum(eventWin$peaks[c(which(eventWin$response.eventID == i))])
  
  EVENTS$Q.peak.m3.s[i] <- max(eventWin$q[
    c(which(eventWin$response.eventID == i))], na.rm = TRUE) 
  
  EVENTS$Q.peak.ts[i] <- as.POSIXct(
    eventWin$datetime[head(which(eventWin$q == EVENTS$Q.peak.m3.s[i] &          # Which rows math the max identiied and
                                eventWin$response.eventID == i)                 # are in the responce window
                        , 1)],                                                  # if multiple max values use the first one only           
    origin = "1970-01-01", tz = "UTC")   

  rankedQ <- eventEx$q[order(eventEx$q)]
  
  EVENTS$Q.peak.rank[i] <- round(
    mean(which(rankedQ == EVENTS$Q.peak.m3.s[i]),rm.na = T) / 
      length(!is.na(rankedQ)), digits = 3)
  EVENTS$Q.QXX[i] <- paste0("Q", 
                            ceiling( ( (1-EVENTS$Q.peak.rank[i])*100) /5) *5 )

#.... Q for whole event period: (including during initial rainfall event) ......
  
  ## Overview:
  
  EVENTS$Q.dur[i] <- as.duration(tail(eventWin$datetime,1)-eventWin$datetime[1])      # this is event duration not flow response duration
  EVENTS$event.Q.tot.m3[i] <- sum(eventWin$q, na.rm = T) * 900 #seconds in 15 min # this is event duration not flow response duration                                   
  EVENTS$event.baseflow.tot.m3[i] <- sum(eventWin$baseflow, na.rm = T) * 900 #seconds in 15 min                # total filtered baseflow (for whole event window)
  EVENTS$event.quickflow.tot.m3[i] <- EVENTS$event.Q.tot.m3[i] - EVENTS$event.baseflow.tot.m3[i]

  
  ## Additional base/quickflow starts (only useful when toal flow return to baseflow between events)
  
  quickflow.start.row <- which(lag(eventWin$Q) == lag(eventWin$baseflow) &
                                 eventWin$Q > eventWin$baseflow &
                                 lead(eventWin$Q, n = 1L) > lead(eventWin$baseflow, n = 1L) &
                                 lead(eventWin$Q, n = 2L) > lead(eventWin$baseflow, n = 2L) &
                                 lead(eventWin$Q, n = 3L) > lead(eventWin$baseflow, n = 3L) &
                                 lead(eventWin$Q, n = 4L) > lead(eventWin$baseflow, n = 4L))

  # default to start of event window
  quickflow.start.row <- ifelse(length(quickflow.start.row) == 0, 1,quickflow.start.row)

  EVENTS$quickflow.start.ts[i] <- as.POSIXct(
    eventWin$datetime[quickflow.start.row],
    origin = "1970-01-01", tz = "UTC")

  quickflow.end.row <- which(eventWin$Q == eventWin$baseflow &
                               lag(eventWin$Q, n = 1L) > lag(eventWin$baseflow, n = 1L) &
                               lag(eventWin$Q, n = 2L) > lag(eventWin$baseflow, n = 2L) &
                               lag(eventWin$Q, n = 3L) > lag(eventWin$baseflow, n = 3L) &
                               lag(eventWin$Q, n = 4L) > lag(eventWin$baseflow, n = 4L))

  # default to end of event window
  quickflow.end.row <- ifelse(length(quickflow.end.row) == 0, nrow(eventWin),quickflow.end.row)

  EVENTS$quickflow.end.ts[i] <- as.POSIXct(
    eventWin$datetime[quickflow.end.row],
    origin = "1970-01-01", tz = "UTC")
  EVENTS$quickflow.dur[i] <- as.duration(
    EVENTS$quickflow.end.ts[i]-EVENTS$quickflow.start.ts[i])

  
 #.... lag and other times .....................................................
  
  # EVENTS$lag.dur[i] <- as.duration(EVENTS$Q.peak.ts[i]-EVENTS$rain.peak.ts[i])                # peak rainfall to peak Q.peak
  # EVENTS$init.lag.dur[i] <- as.duration(EVENTS$Q.peak.ts[i]-EVENTS$init.rain.peak.ts[i])      # peak rainfall to peak Q.peak
  
  # start rain to start runoff
  EVENTS$lag.start.dur[i] <- as.duration(EVENTS$Q.response.start.ts[i]-EVENTS$rain.start.ts[i])  
  # peak rain to peak runoff
  EVENTS$lag.peak.dur[i] <- as.duration(EVENTS$Q.peak.ts[i]-EVENTS$rain.peak.ts[i])  
  # peak of initial rain to peak runoff
  EVENTS$lag.peakinit.dur[i] <- as.duration(EVENTS$Q.peak.ts[i]-EVENTS$init.rain.peak.ts[i])  
  
  # rising and falling limb duration
  EVENTS$rlimb.dur[i] <- as.duration(EVENTS$Q.peak.ts[i] - EVENTS$Q.response.start.ts[i])   # responce event start to Q.peak
  EVENTS$flimb.dur[i] <- as.duration(EVENTS$Q.response.end.ts[i] - EVENTS$Q.peak.ts[i])     # Q.peak to responce event end
  
  EVENTS$rlimb.rain[i] <- sum(eventWin$rainfall[1:head(which(eventWin$q == EVENTS$Q.peak.m3.s[i] &             # Which rows math the max identiied and
                                                               eventWin$response.eventID == i)                 # are in the responce window
                                                       , 1)], na.rm = TRUE) / 4 #1h/15min

  
  # #.... interevent (runin mane code after events checked) ....................
  #
  # eventEx.interevent.start.row <- ifelse(i == 1, 1,
  #                                        tail(which(eventEx$response.eventID == (i-1)),1) + 1)
  # 
  # EVENTS$interevent.dur[i] <- as.duration(
  #   eventEx$datetime[(eventEx.rain.start.row-1)]-
  #     eventEx$datetime[eventEx.interevent.start.row])  
  # EVENTS$interevent.rainfall.tot.mm[i] <- sum(eventEx$rainfall[
  #   c(eventEx.interevent.start.row:eventEx.rain.start.row-1)], 
  #   na.rm=TRUE) /4
  # EVENTS$interevent.Q.median[i] <- median(eventEx$q[
  #   c(eventEx.interevent.start.row:eventEx.rain.start.row-1)], 
  #   na.rm=TRUE)

   #.... save event extraction full csv .........................................
  
  #write.csv(eventWin, paste0(sitename, "/run_",runID,"/eventWin/eventWin", i, ".csv"))
  write.csv(eventWin, paste0(sitename, "/run_" ,runID ,"/CHECK/eventWin/eventWin", i,"_",
                             format(EVENTS$Q.response.start.ts[i],"%Y%m%d_%H%M"), ".csv"))
  
}

# ## check if folder existes and set runID
# 
# # if(!dir.exists("plots")) dir.create("plots")
# # if(!dir.exists(paste0("plots/",runID))) dir.create(paste0("plots/",runID))
# 
# ## plot each event
#   #legnd <- c("Event","Rainfall", "Baseflow","Q", "Event (rain)", "Event (responce)", "Event (flow sep.)", "value", "peaks")  
#   legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
#              "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
#              "Intermediate flow", "Q peaks") #9:10
#   
#   for (i in n){
#   
#     #print(paste0("plotting event: ",i))
#     message(paste0("plotting event: ",i))
#     
#     #eventrows[[i]] <- c(rowrainSTART[i]:rowQEND[i])
#     
#     eventWin <- data.frame(eventEx[c(eventrows[[i]]),])
#     
#     # fixed window size (3.5 days)
#     xlim_min <- as.POSIXct(eventWin$datetime[1]-(86400*0.5), tz = "UTC", origin = "1970-01-01")     # set the start date for the plot (12 hr befor start)
#     xlim_max <- as.POSIXct(eventWin$datetime[1]+(86400*3), tz = "UTC", origin = "1970-01-01")       # set the end date for the plot   (3 days after start)
#       
#     #png(paste0("plots/",runID,"/TEST_event",i,".png"), width = 15, height = 15, units = 'cm', res = 100)
#     
#     #png(paste0(sitename, "/run_",runID,"/plots/eventPlot",i,".png"), width = 15, height = 15, units = 'cm', res = 100)
#     tiff(paste0(sitename, "/run_",runID,"/CHECK/plots/eventPlot",i, "_",
#                 format(EVENTS$Q.response.start.ts[i],"%Y%m%d_%H%M"),
#                 ".tiff"), 
#          width = 100, height = 100, units = 'mm', res = 600, compression = "zip")  
# 
#     p1 <- ggplot(eventEx) + 
#       geom_col(data = 
#                  subset(eventEx,                                                      # Subset origional data for year of ts between dates
#                         datetime >= as.POSIXct(EVENTS$rain.start.ts[i]) &             # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
#                           datetime <= as.POSIXct(eventWin$datetime[nrow(eventWin)])), # 24 h after year start
#                aes(datetime, event*ylimit1, fill=legnd[5]), width=1*60*15, position=position_dodge(1*60*15)) +    
#       geom_col(data = subset(eventEx, response.eventID == i),
#                aes(datetime, response.event*ylimit1, fill=legnd[6]), 
#                width=1*60*15, position=position_dodge(1*60*15)) +
#       geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
#       ylab("Rainfall (mm/h)")+
#       scale_x_datetime(limits = c(xlim_min,xlim_max),                           # get the x axis limits previously set
#                        date_labels = ("%Y-%m-%d\n%H:%M")) +
#       scale_y_reverse(limits = c()) +
#       theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                                       legend.title=element_blank(), 
#                                       legend.position = "none",
#                                       axis.title.x=element_blank()) +
#       scale_colour_manual(values= c("grey50")) +
#       scale_fill_manual(values = c(alpha (c("cyan"), 0.3), 
#                                    alpha (c("dodgerblue3"), 0.2), 
#                                    "grey50"))
# 
#     p2 <- ggplot(eventEx) +
#       geom_col(data = 
#                  subset(eventEx,                                                      # Subset origional data for year of ts between dates
#                         datetime >= as.POSIXct(EVENTS$rain.start.ts[i]) &             # 24 h befor year start (to allow for calcs using a 6hr or 12hr moving window)
#                           datetime <= as.POSIXct(eventWin$datetime[nrow(eventWin)])), # 24 h after year start
#                aes(datetime, event*ylimit2, fill=legnd[5]), width=1*60*15, position=position_dodge(1*60*15)) +    
#       geom_col(data = subset(eventEx, response.eventID == i),
#                aes(datetime, response.event*ylimit2, fill=legnd[6]), 
#                width=1*60*15, position=position_dodge(1*60*15)) +
#       geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
#       geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
#       geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
#       geom_point(data = peaks, aes(datetime, q, colour = legnd[10]), shape = 1) +
#       xlab ("Date") +
#       ylab (expression(Flow~(m^{3}~s^{-1}))) +
#       scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
#                        date_labels = ("%Y-%m-%d\n%H:%M")) +
#       scale_y_continuous(limits = c(0,ylimit2)) +
#       theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                                       legend.title=element_blank(),
#                                       legend.position = "none") +                           # blank element removes background to legend
#       scale_colour_manual(values= c("grey40", "grey40", "black", "red")) +
#       scale_linetype_manual(values= c("longdash", "dotted", "solid")) +
#       scale_fill_manual(values = c(alpha (c("cyan"), 0.3), 
#                                    alpha (c("dodgerblue3"), 0.2)))
#     
# 
#   p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#   print(p)
#   
#   dev.off() 
#   
# }

#rm(rowQSTART, rowQEND, rowrainSTART)