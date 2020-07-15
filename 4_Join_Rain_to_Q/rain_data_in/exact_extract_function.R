# Title     : ExactExtract function for Rainfall extraction
# Objective : calculate precise raster stats for catchment areas using the exact extract algorithm...
# Created by: Hugh Graham
# Created on: 22/11/2019

# ----------- timer ------------

s_time <- Sys.time()

sprintf("start time = %s", s_time)

#----------- Project Set Up --------------------------

#! /usr/bin/Rscript
.libPaths("C:/Program Files/R/R-3.6.1/library")
# Check that the required packages are installed
list.of.packages <- c("exactextractr", "tidyverse", "raster", "sf", "rgdal", "lubridate", "foreach", "doParallel", "padr", "tcltk")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(raster)
library(sf)
library(rgdal)
library(exactextractr)
library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(padr)
library(tcltk)

options(scipen=999) # turns off scientific notation
#---------- Project Paths - User input ----------------------

Data_folder <- "D:/MetOfficeRadar_Data/UK_1km_Rain_Radar_Processed"  # Folder containing all Rainfall Rasters

bound_shp <- "C:/HG_Projects/East_Bud_Hydrology_Proj/4_Join_Rain_to_Q/rain_data_in/Catchment_Boundary/Bud_Brook_Catch.shp"  # An input polygon file

Export_folder <- "C:/HG_Projects/East_Bud_Hydrology_Proj/4_Join_Rain_to_Q/rain_data_in"

area_field_name <- NA # This is the name of the attribute you want to use to name your files.
                             # if you only have one shape you can set as NA and a default of AOI is used

# start_date <- 201908050000 # Let's test things... 200404062320 #
# end_date   <- 201908150000

start_date <- 201904040900
end_date   <- 202003170400

timestep <- '15 min' # The desired timestep to aggregate files requires lubridate time format.

EPSG <- 27700  # If working in UK and using MetOffice Rain Radar data leave this as 27700

#--------------------- Check export folder and create sub folders ----------------------------------------------------

if (isFALSE(dir.exists(Export_folder))){
  print("Export folder does not exist - creating it now...")
  dir.create(Export_folder)
} else {
  print("Export folder already exists")
}

folder_5min <- file.path(Export_folder, "5_min_data")

if (isFALSE(dir.exists(folder_5min))){
  dir.create(folder_5min)
}

folder_Xmin <- file.path(Export_folder, sprintf("%s_data", str_replace(timestep, " ", "_")))

if (isFALSE(dir.exists(folder_Xmin))){
  dir.create(folder_Xmin)
}

# --------------- check polygon features --------------------------------------------------

shape <- read_sf(dsn = bound_shp)                                                      # Read in polygon(s)

epsg_df <- data.frame(make_EPSG())                                                     # get epsg dataframe from rgdal to compare proj4 and epsg codes

CRS <- as.numeric(na.omit(epsg_df$code[epsg_df$prj4 == crs(shape)]))                   # get the epsg code for the provided polgon

if (CRS != EPSG){                                                                      # give message if original polygon not in OSGB.
  print("Supplied polygons not in correct CRS - transforming now...")
}

shape <- st_transform(shape, crs = EPSG)                                                # transform anyway to clarify epsg code if NA

count <-  nrow(shape)

if (count == 0) {                                                                      # check number of features in polygon and assign correct naming column
 stop("Error - boundary shp file provided contains no features!")
} else if (count == 1) {
  if (is.na(area_field_name)){
    
    shape$Area_Name <- "AOI"   # add AOI name
  }   
  if (area_field_name %in% colnames(shape)){
    shape <- shape %>% 
      rename(Area_Name = area_field_name)
  }
  
} else if (count > 1) {
  if (is.na(area_field_name)){
    stop(" ####### Aborting script ############ \n 
        ######## Multiple shapes provided without unique names \n 
        ######## Add new field and create unique names")
  }
  if (area_field_name %in% colnames(shape)){
    print("this happened3")
    shape <- shape %>% 
      rename(Area_Name = area_field_name)
  }
  
}


shape <- shape %>%
  add_column(shp_area = st_area(shape))

#--------------------- Select desired rasters for processing ----------------------------------

rasterlist <- list.files(Data_folder, pattern = "\\.tif$")                              # retrieve all raster files from folder
rasterlist <- rasterlist[order(as.numeric(substr(rasterlist, start = 32, stop = 43)))]  # make sure they are in order

date_list <-  as.numeric(substr(rasterlist, start = 32, stop = 43))                     # Create numeric list of date times

if (start_date %in% date_list){                                                         # retrieve start date/time index
  s_row <- base::match(start_date, date_list)
  print(sprintf("Start date/time is: %1.0f", date_list[s_row]))
} else {                                                                                  # if requested start date/time not available get nearest
  
  s_row <- which(abs(date_list-start_date)==min(abs(date_list-start_date)))
  print(sprintf("Start date/time not available - choosing nearest date: %1.0f", date_list[s_row]))
}

if (end_date %in% date_list){                                                           # retrieve end date/time index
  e_row <- base::match(end_date, date_list)
  print(sprintf("End date/time is: %1.0f", date_list[e_row]))
} else {                                                                                  # if requested end date/time not available get nearest
  
  e_row <- which(abs(date_list-end_date)==min(abs(date_list-end_date)))
  print(sprintf("End date/time not available - choosing nearest date: %1.0f", date_list[e_row]))
}


selec_file_list = rasterlist[s_row:e_row]                                               # subset the list of rasters to get list of desired rasters


#-------------- Run Raster Statistics with exactextractr ----------------------------

substrRight <- function(x, n){                                                          # function to extact substrings from right
  substr(x, nchar(x)-n+1, nchar(x))
}


selec_rasters <-  paste(Data_folder, selec_file_list, sep="/")                         # create full file paths for list of rasters


#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

n = ceiling(length(selec_rasters)/(cores[1]-1))
# clusterExport(cl, c("n"))
# run rainfall extraction
combined_df <- foreach(ras = selec_rasters, .combine = rbind, 
                       .packages = c("raster", "sf", "rgdal", "exactextractr", "tidyverse", "lubridate", "tcltk" )) %dopar% {
                         
                         if(!exists("counter")) counter <- 0
                         counter = counter + 1 
                         
                         if(!exists("pb")) pb <- tkProgressBar("Parallel task", min=1, max=n)
                         setTkProgressBar(pb, counter)
                         
                         try({date_num <- substr(substrRight(ras, 30), start = 1, stop = 12)    
                         date_form <- paste(substr(date_num, 1,4), substr(date_num, 5,6), substr(date_num, 7,8), sep = "/")
                         time_form <- paste(substr(date_num, 9,10), substr(date_num, 11,12), sep = ":")
                         date_time_ex <-  ymd_hm(paste(date_form, time_form, sep = " "))
                         
                         grid <- raster(x = ras)
                         
                         result <- as.tibble(exact_extract(grid, shape, fun = c('mean','sum', 'min', 'max'), progress = FALSE))%>%
                           rename(rain_intensity_mmhr = mean)%>%
                           mutate(rain_intensity_mmhr = rain_intensity_mmhr/32) %>%
                           rename(rain_volume_m3 = sum) %>%
                           add_column(shp_area = as.double(shape$shp_area)) %>%
                           mutate(rain_volume_m3 = (rain_volume_m3/32/12/1000)*shp_area) %>%
                           rename(min_intensity_mmhr = min) %>%
                           mutate(min_intensity_mmhr = min_intensity_mmhr/32) %>%
                           rename(max_intensity_mmhr = max) %>%
                           mutate(max_intensity_mmhr = max_intensity_mmhr/32) %>%
                           add_column(Area_Name = shape$Area_Name) %>%
                           add_column(date_time = date_time_ex)
                         
                         result})
                         
                         # combined_df <- rbind(combined_df,result)
                       }

stopCluster(cl)

combined_df <- combined_df %>%   # Group df by area name
  group_by(Area_Name)

table_list <- group_split(combined_df)   # spli

# function to conver the time integers provided to lubridate values
convert_time <- function(time_val){
  dt_val <- paste(substr(as.character(time_val), start = 1, stop = 4), 
                   substr(as.character(time_val), start = 5, stop = 6),
                   substr(as.character(time_val), start = 7, stop = 8), sep = "/")  
  tm_val <- paste(substr(as.character(time_val), start = 9, stop = 10), 
                  substr(as.character(time_val), start = 11, stop = 12), sep = ":") 
  dt_tm_val <- ymd_hm(paste(dt_val, tm_val, sep = " "))
  return(dt_tm_val)
}

Start_val <- convert_time(start_date)
End_val <- convert_time(end_date)


for (tab in table_list){
  
  name <- tab$Area_Name[1]
  
  tab <- tab %>%
    select(date_time, rain_intensity_mmhr, rain_volume_m3, min_intensity_mmhr, max_intensity_mmhr) %>%
    padr::pad(interval = '5 min',
              start_val = Start_val,
              end_val = End_val,
              break_above = 2)
  

  write_csv(tab , path = file.path(folder_5min, 
                                     paste(name, "5_min", as.character(start_date), 
                                           as.character(paste(end_date, "csv", sep = ".")), sep = "_")))
  
  resam_tab <- tab %>% 
    group_by(requested_interval = ceiling_date(date_time, unit = timestep)) %>% 
    summarise(rain_intensity_mmhr = mean(rain_intensity_mmhr, na.rm = TRUE),
              rain_volume_m3 = sum(rain_volume_m3, na.rm = TRUE),
              min_intensity_mmhr = min(min_intensity_mmhr, na.rm = TRUE),
              max_intensity_mmhr = max(max_intensity_mmhr, na.rm = TRUE)) %>% 
    rename(date_time = requested_interval) 
  
  resam_tab<- do.call(data.frame,lapply(resam_tab, function(x) replace(x, is.infinite(x),NA))) # convert non finite numbers to NA
  
  resam_tab$rain_volume_m3[is.na(resam_tab$rain_intensity_mmhr)] <- NA   # just to change zeros to NA where they'ved been summed in summarise
  
  write_csv(resam_tab , path = file.path(folder_Xmin, 
                                   paste(name, str_replace(timestep, " ", "_"), as.character(start_date), 
                                         as.character(paste(end_date, "csv", sep = ".")), sep = "_")))
}


sprintf("script completed check %s for results", Export_folder)
print(Sys.time() - s_time)



