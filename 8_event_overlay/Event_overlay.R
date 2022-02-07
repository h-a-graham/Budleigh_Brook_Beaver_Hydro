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
  
  # read_csv(evFile, col_types = cols()) %>%
  #   mutate(event_step = row_number()) %>%
  #   select(event_step, q_m3_s) %>%
  #   mutate(s_flow = pluck(q_m3_s, 1))
  
  # max_eventStep <- max(evDf$event_step)
  # Beaver_pres <- evDf$beaver[1]
  # 
  # Q_loess <- loess(q_m3_s ~ event_step, evDf, control=loess.control(surface="direct"), span=0.1)
  
  # new_NoBeavWet.BB <- tibble(event_step=seq(0, max_eventStep, length=max_eventStep*10), 
  #                            Beaver=Beaver_pres, Site = 'Budleigh Brook (impact)') %>%
  # new_NoBeavWet.BB <- evDf  %>%
  #   broom::augment(Q_loess, type.predict = "response",
  #                  type.residuals = "deviance",
  #                  se_fit = F, data=.) %>%
  #   mutate(event_id = as.factor(id)) 
  
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
    
  
  # gam.lo=gam(q_m3_s~ s(event_step, df=7) * beaver, 
  #            data=.data)
  # gam.lo=gam(q_m3_s~ lo(event_step,by=beaver, df=7) * tot_rain * s_flow,
  #            data=.data)
  # 
  # gam.lo=gam(q_m3_s ~ lo(event_step, by=beaver),
  #            data=.data)
  # plot(gam.lo)
  
  # preds <- predict(gam.lo, se.fit=TRUE)
  # 
  # df <- .data %>%
  #   mutate(gam.fitted = preds[[1]]) %>%
  #   mutate(gam.se.fit = preds[[2]])
  
  
  
  # return(list(gam1, df))
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





# # ----------------- define data folders --------------------------------
# # Set root folder containing event csv files
# #BB Events
# bb_events_folder <- file.path(here(),'5_event_extraction/eventExtraction__beaver/EastBud',
#                               'run_20201002_1454_value__padding2880_alpha0.98_passes3_BFI0.814/final_event_windows')
# 
# #CB Events
# cb_events_folder <- file.path(here(),'5_event_extraction/eventExtraction__beaver/Pophams',
#                               'run_20201002_1643_value__padding2880_alpha0.98_passes3_BFI0.74/final_event_windows')
# 
# 
# # --------- Run the functions ----------------
# 
# bb <- df_overlay(events_folder=bb_events_folder) %>%
#   mutate(Site = as.factor('Budleigh Brook (impact)'))
# 
# cb_cutoff <- max(bb$event_step)
# 
# cb <- df_overlay(events_folder=cb_events_folder, maxhrs = cb_cutoff)%>%
#   mutate(Site = as.factor('Colaton Brook (control)'))
# 
# 
# # bb <- df_overlay(events_folder=bb_events_folder, beaver_time = "2019-01-01 00:00") %>%
# #   mutate(Site = 'Budleigh Brook (impact)')
# 
# 
# # ----------- Create combined plot for control vs impact ---------------
# combine_sites <- bind_rows(bb, cb) %>%
#   plot_overlay(., se=T, method = 'gam') +  # for faster plotting set se=FALSE
#   facet_wrap(~ Site, ncol=2) +
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         # panel.border = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         strip.text.x = element_text(size = 12, color = "black", face = "italic"),
#         strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
# 
# 
# # ------------------------- save image ----------------------------------
# # combine_sites
# out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam.jpg')
# ggsave(filename = out_path,plot = combine_sites, dpi=300)
# 
# # 
# 
# 
# 
# #  The Future --------------------------
# # qunatile appraoch ---------------- Very experimental!!!
# # This disn't get very far because I couldn't get things working with interactive variables.
# #has milage and will probeably require a quantred or qgam appraoch...
# # library(quantreg)
# # 
# # 
# # .rqss_mapper <- function(.data, .quant){
# #   
# #   # Run model
# #   m <-rqss(q_m3_s ~ qss(event_step), lambda=0.5, tau = .quant, data = .data)
# #   
# #   #generate augment df
# #   new_dat <- .data %>%
# #     dplyr::select(q_m3_s, event_step, beaver)
# #   
# #   pred_tib <- .data %>%
# #     select(event_step, beaver)%>%
# #     bind_cols(., predict.rqss(m, newdata = new_dat)) %>%
# #     rename(.rqss.fit = 3) %>%
# #     mutate(.tau = .quant)
# #   
# #   
# #   # return(pred_tib)
# # }
# # 
# # 
# # tid_rqss <- function(.data, Q.list = c(0.1, 0.5, 0.75)) {
# #   get_quants <- Q.list %>%
# #     purrr::map(., ~.rqss_mapper(.data, .)) %>%
# #     bind_rows() #%>%
# #   # bind_cols(., .data) 
# # }
# # 
# # 
# # 
# # t1 <- tid_rqss(bb)
# # 
# # 
# # ggplot(t1, aes(x=event_step, y=.rqss.fit , colour=beaver, fill=beaver)) +
# #   geom_line() +
# #   facet_wrap(~.tau)
# # 
# # 
# # geom_line(mapping = aes( y=q_01),alpha=0.1, lwd=1) +
# #   geom_line(mapping = aes( y=q_05),alpha=0.1, lwd=1) +
# #   geom_line(mapping = aes( y=q_075),alpha=0.1, lwd=1) +
# #   
# #   scale_y_log10(
# #     breaks = scales::trans_breaks("log10", function(x) 10^x),
# #     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
# #   labs(x = 'time since event start (hrs)', y= (expression('Flow  ' (m^{3}~s^{-1})))) +
# #   scale_color_manual(values = c('#A6190D', '#244ED3')) +
# #   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
# #   theme_bw() + 
# #   annotation_logticks(sides='l') 
# # 
# # ggplot(bb, aes(x=event_step, y=q_m3_s , colour=beaver, fill=beaver)) +
# #   geom_smooth(method = 'gam')
# #   
# #   
# # # wait but GAMS
# # library(qgam); library(mgcv)

