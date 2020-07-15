##### TITLE: eventExtract_responseEvents #######################################

# Filename: eventExtract_responseEvents.R
# Type: Script
# Title: 
# Version: DRAFT 1.0
# Date: 2018-11-14
# Author: JAshe
# Maintainer: j.ashe@exeter.ac.uk
# Description: 
# License: GPL

# Project: eventExtract

##### RESPONSE EVENTS ##########################################################

#.... Description ..............................................................

#TBC

#...............................................................................

#==== 3. response events =======================================================

#---- 3.2b allocate stable or elevated flow (rate of change) -------------------

# ## alternative "STABLE", "ELEVATED" threashold is from slope change:
# QslopeAsc <- 0.05 # 0.2
# QslopeDec <- -0.05 # -0.2

eventEx$response.change <- NA

eventEx$response.slope <- NA

eventEx <- eventEx %>%                                                          # pipe
  
  ## use Qchange threshhold to allocate stable or elevated
  mutate(response.slope =                                                       
           ifelse(diffSmoothedHr<=QslopeAsc & 
                    diffSmoothedHr>=QslopeDec,
                  "STABLE", "ELEVATED")) %>%
  
  ## is the response ts within and hour of (near) an elevated response     
  ## if the rate of change is stable - but the smoothed value is over the 24 hour Q10 (e.g. flat peak)
  mutate(response.slope =                                                       
           ifelse(
             response.slope == "ELEVATED", "ELEVATED",
             ifelse(
               response.slope == "STABLE" &
                 ((lag(response.slope, n = 4L)) == "ELEVATED"|
                    (lag(response.slope, n = 3L)) == "ELEVATED"|
                    (lag(response.slope, n = 2L)) == "ELEVATED"|
                    (lag(response.slope, n = 1L)) == "ELEVATED"|
                    response.slope == "ELEVATED"|
                    (lead(response.slope, n = 1L)) == "ELEVATED"|
                    (lead(response.slope, n = 2L)) == "ELEVATED"|
                    (lead(response.slope, n = 3L)) == "ELEVATED"|
                    (lead(response.slope, n = 4L)) == "ELEVATED"),
               "nrELEVATED",
               ifelse(
                 response.slope == "STABLE" & smoothed >= value.median.win24hr,           # flat peak check      
                 "STABLEbutELEVATED",                                           # flag as STABLEbutELEVATED (n.b. event criteria uses text string ELEVATED)
                 "STABLE"))))

eventEx$response.change <- eventEx$response.slope

#eventEx$response.slope <-NULL
#eventEx$smoothed <-NULL
#eventEx$diffSmoothed <-NULL
#eventEx$diffSmoothedHr <-NULL
#eventEx$Q70win24hr <-NULL
#eventEx$Q50win24hr <-NULL
#eventEx$Q10win24hr <-NULL

#---- 3.3 use Qbase threshhold to identify response over base conditions  ------

eventEx$response.base <- NA

eventEx <- eventEx %>%                                                          # pipe
  
  mutate(response.base =                                                        # use Qbase threshhold to identify response above base conditions (abvBASE) 
           ifelse(value <= Qbase,"BASE", "abvBASE"))  

#---- 3.4 Flag events where response is both elevated (rate of change) and above base, and give ID ------------------

eventEx$response.EabvBase <- NA
eventEx$response.EabvBaseID <- NA

eventEx <- eventEx %>%                                                          # pipe
  
  mutate(response.EabvBase =                                                    # response is both elevated (rate of change) and above base
           ifelse( grepl("ELEVATED", response.change) &                         # if response.change includes ELEVATED and
                     response.base == "abvBASE",                                # response.base is exceeded        
                   1,0 )) %>%                                                   # true return 1, false return 0
  
  mutate(response.EabvBaseID =                                                  # Flag each event start (transition from 0 to 1)
           ifelse( lag(response.EabvBase) == 0 &
                     response.EabvBase == 1,          
                   1,0 )) %>%                                                   # for each new change in 0 to 1 return 1 for first elevated ts
  
  mutate(response.EabvBaseID =                                                  # Flag each event start (transition from 0 to 1)
           cumsum( ifelse(is.na(response.EabvBaseID),                           # cumulative sum (skipping NA)
                          0, response.EabvBaseID) )                                    
         + response.EabvBaseID*0                                                # retained NA rows in the cumsum (to make cumsum continuous toggle)
  ) %>%                                                                         # allows line  of code above to be toggled on or off
  
  mutate(response.EabvBaseID = as.numeric(
    ifelse( response.EabvBase == 1,
            response.EabvBaseID,NA )))                                          # for each non rainfall flag as NA

eventEx$response.EabvBase = NULL                                                # remove working column

#---- 3.5 find event magnitude  ------------------------------------------------------------------------------------

eventEx$response.magnitude <- NA

eventEx <- eventEx %>%                                                          # pipe
  
  group_by(response.EabvBaseID) %>%                                             # group dataframe by column containing elevated above base ID group
  
  mutate(response.magnitude =                                                   # update event magnitude
           ifelse(!is.na(response.EabvBaseID),                                  # if the Event (elevated and above base) ID is populated (not NA)        
                  (max(value)-min(value)), NA )) %>%                            # use the max value - min value, otherwise use NA 
  
  ungroup()                                                                     # ungroup

str(eventEx)                                                                    # check structure to make sure ungroup worked

#---- 3.6 Flag and ID for overall response events (change, base and magnitude) ----------------------------------

eventEx$response.event <- NA
eventEx$response.eventFlag <- NA
eventEx$response.eventID <- NA

eventEx <- eventEx %>%                                                          # pipe
  
  mutate(response.event =                                                       # Flag of 1 then flow change is elevated, base exceeded, and magnitude exceeded
           ifelse( !is.na(response.EabvBaseID) &                                # if the Event (elevated and above base) ID is populated (not NA) AND
                     response.magnitude > Qmagnitude,                           # Magnitude threashold is exceeded          
                   1,0 )) %>%                                                   # then flag 1, if not flag zero
  
  # mutate(response.eventFlag =                                                   # Add a START flag at the start of each event
  #          ifelse( lag(response.event) == 0 &
  #                    response.event == 1,          
  #                  "STARTresponse",
  #                  ifelse( lag(response.event) == 1 &
  #                            response.event == 0,
  #                          "END",
  #                          ifelse( response.event == 1,
  #                                  "LINK",NA )))) %>%
  
  mutate(response.eventFlag =                                                   # Add a START flag at the start of each event
           ifelse( lag(response.event) == 0 &
                     response.event == 1,          
                   "STARTresponse",
                   ifelse( response.event == 1 &
                             lead(response.event) == 0,
                           "END",
                           ifelse( response.event == 1,
                                   "LINK",NA )))) %>%  
  
  # mutate(response.eventFlag =                                                 # Add a START flag at the start of each event
  #          ifelse( lag(response.event) == 0 &
  #                    response.event == 1,          
  #                  "START","" )) %>%  
  
  mutate(response.eventID =                                                     # Add a value for the START of each event ID
           ifelse( response.eventFlag == "STARTresponse",          
                   1,0 )) %>%
  
  mutate(response.eventID =                                                     # Use this to creat a cumulative sum (running count)
           cumsum( ifelse(is.na(response.eventID),                              # Cumulative sum for event ID
                          0, response.eventID) )                                # cumulative sum (skipping NA)
         + response.eventID*0) %>%                                              # retained NA rows in the cumsum (to make cumsum cont. toggle off this line of code)
  
  mutate(response.eventID = as.numeric(                                         # edit this to replace non event rows with NA (ensure as.numeric, not character)
    ifelse( response.event == 1,                                                # if the response event is true (1)
            response.eventID,NA)))                                              # use the ID generated, if not flag as NA

#eventEx1 <- eventEx

