##### TITLE: eventExtract_rainEvents #######################################

# Filename: eventExtract_rainEvents.R
# Type: Script
# Title: 
# Version: DRAFT 1.0
# Date: 2018-11-14
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: 
# License: GPL

# Project: eventExtract

##### rainfall EVENTS ##########################################################

#.... Description ..............................................................

#  rainfall event flagging from:
#    D.J.Luscombe (2014) Understanding the ecohydrology of shallow, drained and marginal blanket peatlands, 
#    Doctor of Philosophy In Geography. University of Exeter. Exeter, UK. pp124.

#  CAVEATS:
#  Pros: objective, repeatable, easily automated
#  Cons: arbitrary and physically unrealistic
#  Notes:
#...............................................................................

#==== 2. rainfall events =======================================================

## Span is set in the main run script
rainLinkSpan <- userinput$rainLinkSpan #6                                                            # How many timesteps should the rainfall link bridge?
rainLinkWin <- 1:rainLinkSpan

eventEx$rainfall.flag <- NA # is it a continous rainfall event (start, link, end, or isolate)
eventEx$rainfall.link <- NA 
eventEx$rainfall.eventFlag <- NA
eventEx$rainfall.eventID <- 0
eventEx$rainfall.startTs <- NA
eventEx$rainfall.eventDur <- NA

#---- 2.1 rainfall ts flag -----------------------------------------------------

## Is it a continous rainfall event (start, link, end) or isolate rain(Isolate) or no rain (BLANK)
# N.B. NA  records act as limits to rainfall events (i.e. they will isolate, start or end an event)

#library(dplyr)                                                                 # for %>%, lag, lead and mutate

eventEx <- eventEx %>%                                                                        # pipe
  
  mutate(rainfall.flag =                                                                      # Generate rainfall timestamp flag
           ifelse( is.na(rainfall),NA,                                                        # if rainfall is NA flag is "NA"    
                   ifelse( rainfall==0,"",                                                    # elseif if rainfall is zero flag is blank
                           ifelse( (lag(rainfall)==0 | is.na(lag(rainfall))) 
                                   & (lead(rainfall)==0 | is.na(lead(rainfall))),
                                   "ISOLATE",                                                 # elseif lag and lead rainfall is zero or NA flag is ISOLATE
                                   ifelse((lag(rainfall)>0 & !is.na(lag(rainfall))) 
                                          & (lead(rainfall)>0 & !is.na(lead(rainfall))),
                                          "LINK",                                             # elseif lag and lead rainfall is >zero and not NA flag is LINK
                                          ifelse(lag(rainfall)==0 | is.na(lag(rainfall)), 
                                                 "START",                                     # elseif preceeding rainfall is zero or NA flag is START
                                                 "END"))))))                                  # else flag is END

eventEx$rainfall.flag[is.na(eventEx$rainfall.flag)] <- ""                       # prevent NA spread

#---- 2.2 Identify gaps in rainfall events and convert to links ----------------

## Fixed window for rainfall linking span
# eventEx <- eventEx %>%                                                          # pipe
#   
#   mutate(rainfall.link =                                                        # 2.2 Identifies when END flags should link to the next rainfall START
#              ifelse(
#                (rainfall.flag == "END" &                                        # elseif the rainfall event flag is END and
#                   ((lead(rainfall.flag, n = 6L)) == "START"|                    # another rainfall event starts in th next 4 timestps (15 min data 1 hr)
#                      (lead(rainfall.flag, n = 5L)) == "START"|
#                      (lead(rainfall.flag, n = 4L)) == "START"|
#                      (lead(rainfall.flag, n = 3L)) == "START"|
#                      (lead(rainfall.flag, n = 2L)) == "START"|
#                      (lead(rainfall.flag, n = 1L)) == "START")), "LINK",""))    # flag is LINK

## User set window for rainfall linking span
fillrainfallgaps <- function(x) {
  test <- ifelse(head(x,1) == "END" & 
   any(grep(pattern = "START", x, ignore.case = TRUE)),
   "LINK","")
  return(test)
}

## run in for loop (N.B. loop less efficient but BUG with zoo:rollapply)
for (i in 1:(nrow(eventEx)-rainLinkSpan+1)){
 x <- eventEx$rainfall.flag[i:(i+rainLinkSpan-1)]
 eventEx$rainfall.link[i] <- fillrainfallgaps(x)
 }

##  repeat down to fill gap
for (i in rainLinkWin){                                                         # repeat for the duration of the rain link window set (e.g. 4 timestamps default)
  
  eventEx <- eventEx %>%                                                        # pipe
    
    mutate(rainfall.link =                                                      # Check for rainfall START 1hr (4 timestamps) after the END of rainfall flag
             ifelse( rainfall.link == "LINK", "LINK",
                     ifelse( (lag(rainfall.link, n = 1L) == "LINK" & 
                                (rainfall.flag == "" | 
                                   rainfall.flag == "ISOLATE" | 
                                   rainfall.flag == "START")),
                             "LINK","")))
}                                                                               # END rain link loop

eventEx$rainfall.link[is.na(eventEx$rainfall.link)] <- ""                       # prevent NA spread

#---- 2.3 Identify rainfall events and give ID ---------------------------------

eventEx <- eventEx %>%                                                          # pipe
  
  mutate(rainfall.eventFlag =                                                   # Identify rainfall events
           ifelse( rainfall.link == "LINK","LINK",                              # if gap link is present "LINK"
                   ifelse( rainfall.flag == "START","START",                    # else use ts flag "START"
                           ifelse( rainfall.flag == "LINK","LINK",              # else use ts flag "LINK"
                                   ifelse( rainfall.flag == "END","END",""))))) %>%   # else use ts flag "END"

  mutate(rainfall.eventID =                                                     # rainfall Event ID
           ifelse( rainfall.eventFlag == "START",1,0 )) %>%                     # for each rainfall event START return 1
  
  mutate(rainfall.eventID =
           cumsum( ifelse(is.na(rainfall.eventID),0, rainfall.eventID) )        # ID numbering from cumulative sum of 1's returned (skipping NA)
         + rainfall.eventID*0) %>%                                              # retained NA rows in the cumsum (to make cumsum cont. toggle off this line of code)
  
  mutate(rainfall.eventID =
           ifelse( rainfall.eventFlag == "",NA,rainfall.eventID )) %>%              # for each non rainfall adjust rainfall ID to blank

  mutate(rainfall.event =                                                       # rainfall Event ID
           ifelse( is.na(rainfall.eventID),0,1))                                # is it a rainfall event 0 1 bianary

#---- 2.4 Identify rainfall event duration -------------------------------------

eventEx <- eventEx %>%                                                          # pipe
  
  mutate(rainfall.startTs = as.POSIXct(                                         # Identify rainfall event start time (save results as POSIXct time
    ifelse( rainfall.eventFlag == "START",datetime,lag(rainfall.startTs)),         # for the start of a rainfall event create a time stamp
    origin = "1970-01-01", tz = "UTC")) %>%
  
  fill(rainfall.startTs) %>%                                                    # fill NA will preceeding value
  
  mutate(rainfall.eventDur = seconds_to_period(                                 # Identify rainfall event duration
    ifelse( rainfall.eventFlag == "END",datetime-rainfall.startTs,NA)))            # if the end of a rainfall event report end timestamp minus the start timestamp 


#---- 3.5 find event total rainfall  -------------------------------------------

eventEx$rainfall.eventtot <- NA
eventEx$rainfall.eventcumsum <- NA
eventEx$rainfall.eventcentroidr <- NA
eventEx$rainfall.eventcentroid <- NA

eventEx <- eventEx %>%                                                          # pipe
  
  group_by(rainfall.eventID) %>%                                                # group dataframe rainfall event ID
  
  mutate(rainfall.eventtot =                                                    # update event magnitude
           ifelse(!is.na(rainfall.eventID),
                   sum(rainfall, na.rm = TRUE), NA)) %>%    
  
  mutate(rainfall.eventcumsum =                                                    # update event magnitude
           ifelse(!is.na(rainfall.eventID),
                  cumsum(rainfall), NA)) %>%  
  
  # mutate(rainfall.eventcentroid =                                                    # update event magnitude
  #          ifelse(!is.na(rainfall.eventID),
  #                 ifelse(cumsum(rainfall) >= (sum(rainfall, na.rm = TRUE)/2),
  #                        1,0), 
  #        NA)) %>%
  
  mutate(rainfall.eventcentroidr = 
           ifelse(!is.na(rainfall.eventID),
                  userID[which.min(abs(cumsum(rainfall)-(sum(rainfall, na.rm = TRUE)/2)))],
                                NA)) %>%
  
  mutate(rainfall.eventcentroid = 
           ifelse(rainfall.eventcentroidr == userID, 1, NA)) %>%
  
  ungroup()                                                                     # ungroup

# str(eventEx)                                                                    # check structure to make sure ungroup worked

eventEx$rainfall.eventtot <- NULL
eventEx$rainfall.eventcumsum <- NULL
eventEx$rainfall.eventcentroidr <- NULL

#eventEx$rainfall.eventcentroid <- ifelse(rainfall.eventcentroidr == userID

#...............................................................................

rm(rainLinkWin, x, i)

# eventEx$rainfall.link = NULL                                                  # remove rainfall working columns as needed
# eventEx$rainfall.flag = NULL
# eventEx$rainfall.eventFlag = NULL
# eventEx$rainfall.eventID = NULL
# eventEx$rainfall.startTs = NULL
# eventEx$rainfall.eventDur = NULL


##### END ######################################################################


