##### TITLE: eventExtract_combEvents #######################################

# Filename: eventExtract_combEvents.R
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

#==== 4. Combined events =======================================================

#---- 4.1 rainfall.eventID -----------------------------------------------------

## Identify (for each response event) the most recent rainfall event

eventEx$rainfall.eventID[1] <- 0                                                # set initial rainfall event ID to 0

eventEx$LastRain <- NA
eventEx$event.rainSTART <- NA
eventEx$event.FLAG <- NA

eventEx <- eventEx %>%
  mutate(LastRain =                                                             # For each responce event START give most recent (or ongoing) rainfall event ID
           ifelse( response.eventFlag == "STARTresponse",
                   na.locf(rainfall.eventID),
                   NA))

#---- 4.2 event.rainSTART ------------------------------------------------------

## Use extracted pre event 'check' windows to calculate the start of combined R-R events 

inds = which(eventEx$response.eventFlag == "STARTresponse")                     # return the row numbers for each responce event START

rows <- lapply(inds, function(x) (x-23):(x))                                    # get all rows for all indices in the required pre event window (generates a list)
n <- length(rows)                                                               # how many pre event windows are there?

rowrainSTART <- NA

for (i in 1:n){      
  preWin <- data.frame(eventEx[c(rows[[i]][rows[[i]]>0]),])                     # extract dataframe of pre event window for the 1st element in the list (1st window)
  #preWin <- data.frame(eventEx[c(rows[[i]]),])                                 # ext... [bug: can select non existent (negative) row numbers, to correct see above)]
  #str(preWin)
  
  startrow <- preWin$userID[1]                                                  # starting row (userID) o the pre Event window
  min <- min(preWin$value)                                                      # minimum value in the pre Event window                                 
  
  # N.B. in the excel sheet match() is used - what logic desifed which min value row no is returned if there are multiple?
  
  rowmin <- which(preWin$value == min(preWin$value))                            # row number (in the extracted fataframe) for this minimum value
  rowformin <- preWin$userID[rowmin[length(rowmin)]]                            # User ID for this row (if the min occures more than once, row for last occurance)     
  
  preWinRising <- subset(preWin, userID >= rowformin)                           # subset the pre event window to include rows equal to or exceeding the row no for the lowerst value.
  
  LastRainID <- preWinRising$LastRain[length(preWinRising$LastRain)]            # What is the most recent rainfall event listed in the subset the pre event window
  
  rowlastrain <- which(eventEx$rainfall.eventID == LastRainID)                  # row number (in the extracted fataframe) for this minimum value
  rowrainSTART[i] <- eventEx$userID[rowlastrain[1]]
  
  eventEx$event.rainSTART[c(rowrainSTART[i]:inds[i])] = "LINK"                  # flag START for begginig of the startingrainfall event
  eventEx$event.rainSTART[rowrainSTART[i]] = "STARTrain" 
  
  rm(startrow,   min,   rowformin,   LastRainID)
  
}

rm(inds,preWin, preWinRising, rows, i, n, rowlastrain, rowmin)

#---- 4.3 event.FLAG, event(T/F), eventID --------------------------------------

## Link up the rain start and the responce start events

eventEx <- eventEx %>%
  mutate(event.FLAG =                                                           # For each responce event START give most recent (or ongoing) rainfall event ID
           ifelse(!is.na(response.eventFlag), response.eventFlag,
                  ifelse(is.na(response.eventFlag) & !is.na(event.rainSTART), 
                         event.rainSTART, NA))) %>%
  
  mutate(event.temp =                                                                # Add a value for the START of each event ID
           ifelse( !is.na(event.FLAG),          
                   1,0 ))
  
  # mutate(eventID.temp =                                                              # Add a value for the START of each event ID
  #          ifelse( event.FLAG == "STARTrain",          
  #                  1,0 )) %>%
  # 
  # mutate(eventID.temp  =                                                             # Use this to creat a cumulative sum (running count)
  #          cumsum( ifelse(is.na(eventID.temp),                                       # Cumulative sum for event ID
  #                         0, eventID.temp) )                                         # cumulative sum (skipping NA)
  #        + eventID.temp*0) %>%                                                       # retained NA rows in the cumsum (to make cumsum cont. toggle off this line of code)
  # 
  # mutate(eventID.temp  = as.numeric(                                            # edit this to replace non event rows with NA (ensure as.numeric, not character)
  #   ifelse( !is.na(event.FLAG),                                                 # if the responce event is true (1)
  #           eventID.temp,NA)))                                                  # use the ID generated, if not flag as NA

