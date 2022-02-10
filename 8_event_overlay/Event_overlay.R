# script to generate GAM hydrographs

library(tidyverse)
library(here)
library(broom)
library(patchwork)
library(gam)


# ---------- Read Files function --------------
# function to read the files
read_event <- function(events_folder, event, id, beaver_time){
  evFile <- file.path(events_folder, event) 
  
  
  if (is.null(beaver_time)){
    evDf <- read_csv(evFile, col_types = cols()) %>%
      mutate(event_step = row_number()) %>%
      mutate(beaver = as.factor(ifelse(datetime > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                       ifelse(datetime > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                                datetime < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No")))) %>%
      filter(beaver != "Unsure") %>%
      mutate(tot_rain = sum(rainfall_mm_h)/4) %>%
      mutate(s_flow = pluck(q_m3_s, 1)) %>%
      # mutate(q_m3_s = q_m3_s**(1/(tot_rain))) %>%
      select(q_m3_s, tot_rain, event_step, beaver, s_flow, rainfall_mm_h) %>%
      mutate(event_id = as.factor(id))  
    
  } else {
    evDf <- read_csv(evFile, col_types = cols()) %>%
      mutate(event_step = row_number()) %>%
      mutate(beaver = as.factor(ifelse(datetime > as.POSIXct(beaver_time, "%Y-%m-%d %H:%M", tz = "UTC"), "Yes", "No"))) %>%
      mutate(tot_rain = sum(rainfall_mm_h)/(max(event_step)/4)) %>%
      select(q_m3_s, event_step, beaver, tot_rain, rainfall_mm_h) %>%
      mutate(event_id = as.factor(id)) 
  }
  
  return(evDf)
  
}

# -------- safely read data function -------------
safe_read <- function(evf, x, id, pb, beaver_time){
  f  = purrr::safely(function() read_event(evf, x, id, beaver_time), otherwise = NA)
  pb$tick()
  f()
}

# function to fit gam and return full dataframe
gam_func <- function(.data) {
  
  gamA <- mgcv::gam(q_m3_s ~ s(event_step, by=beaver, bs='cs', k=5) + beaver,
              method = "REML", data = .data, family=Gamma(link='log')) 
  gam1 <- gamA %>%
    broom::augment(type.predict='response') %>%
    rename_with(., starts_with("."), .fn = ~(paste0("gam", .)))
  
  gamB <- mgcv::gam(rainfall_mm_h ~ s(event_step, by=beaver, bs='cs', k=-1) + beaver,
                    method = "REML", data = .data,family=gaussian(link='identity')) 
  gam2 <- gamB %>%
    broom::augment(type.predict='response') %>%
    select(-c(rainfall_mm_h, beaver, event_step)) %>%
    rename_all(., ~paste0(., "_rain"))
  
  df <- gam1 %>%
    bind_cols(select(.data, event_id, tot_rain, s_flow, rainfall_mm_h), ., gam2) 

  return(list(data=df, flowGAM = gamA, rainGAM=gamB))
  
}



# -------------- main function to map the functions to each row ---------------
#map function to file list and join
# beaver time = NULL defaults to Budleigh Brook beaver timings - use "2017-01-01 00:00" type sting for custom timings.

df_overlay <- function(events_folder,  beaver_time=NULL){
  
  
  event_path_list = list.files(events_folder)
  
  pb <- progress::progress_bar$new(total = length(event_path_list), clear = FALSE)
  
  events_combined <- event_path_list %>%
    purrr::imap(., ~ safe_read(events_folder, .x, .y, pb, beaver_time)) %>%
    purrr::map(., purrr::pluck, "result") %>%
    Filter(Negate(anyNA), .) %>%
    bind_rows() %>%
    mutate(event_step = event_step/4) 
  
  
  
}

fit_gams <- function(events_combined, maxhrs = NULL, perc_cutoff = 0.95){
  if (is.null(maxhrs)){
    maxhrs <- quantile(events_combined$event_step[events_combined$beaver == 'Yes'], probs = c(perc_cutoff))
  } 
  
  events_combined %>%
    filter(event_step < maxhrs) %>%
    gam_func(.) 
}


# --------------- Function to plot event overlays -----------------------

plot_overlay <- function(.data, se=TRUE, method='gam', ticks= TRUE, colours=c("#7c668c", "#a56457")){ #'#A6190D', '#244ED3'
  
  p <- ggplot(.data, aes(x=event_step, y=q_m3_s , group=event_id, colour=beaver, fill=beaver)) +
    geom_line(alpha=0.05, lwd=0.4) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    labs(x = 'Time since event start (hrs)', y= (expression('Flow  ' (m^{3}~s^{-1})))) +
    scale_color_manual(values = colours) +
    scale_fill_manual(values = colours) +
    theme_bw() + 
    labs(color='Beaver Status at Impacted Site', fill='Beaver Status at Impacted Site') 
  
  if (isTRUE(ticks)){
    p <- p + annotation_logticks(sides='l')
  }
  
  if (method =='gam'){
    if (isTRUE(se)){
      p <- p + geom_ribbon(aes(x=event_step, ymin = gam.fitted - (gam.se.fit*1.96), 
                               ymax = gam.fitted + (gam.se.fit*1.96), colour=beaver,
                               fill=beaver),lwd=0.7, alpha=0.7, inherit.aes = F)
    } else {
      p <- p + geom_line(aes(y=gam.fitted),lwd=1.1, alpha=0.9)
    }
  } else {
    # Loess/other option
    if (isTRUE(se)){
      p <- p +  geom_smooth(method=method, aes(y=q_m3_s, group=beaver), colour=NA,  se=se, span=0.3, lwd=0.7, alpha=0.9)
    } else{
      p <- p +  geom_smooth(method=method, aes(y=q_m3_s, group=beaver), se=se, span=0.3, lwd=0.7, alpha=0.9)
    }
    
  }
  
  return(p)
  
}



# ----------

df_from_gam <- function(mod, offset){ 
  gen_tibble <- function(.data){
    event_step <- .data$x 
    .fitted <- .data$fit + offset
    .se <- .data$se
    lab <- .data$ylab
    tibble(event_step,.fitted , .se, lab)
  }
  
  gam_smooth_df <- plot(mod, pages = 1, seWithMean = TRUE, shift=offset) %>%
    purrr::map(., ~gen_tibble(.)) %>%
    bind_rows() %>%
    mutate(lab = fct_rev(lab)) %>%
    mutate(beaver = sub('.*:beaver', '', lab))
  
  return(gam_smooth_df)
  
}

