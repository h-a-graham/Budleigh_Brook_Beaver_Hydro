# A bit of an experiment - to overlay all events, get rolling mean...

library(tidyverse)
library(here)
library(broom)
library(patchwork)

# ----------------- define data folders --------------------------------
# Set root folder containing event csv files
#BB Events
bb_events_folder <- file.path(here(),'5_event_extraction/eventExtraction__beaver/EastBud',
                           'run_20201002_1454_value__padding2880_alpha0.98_passes3_BFI0.814/final_event_windows')

#CB Events
cb_events_folder <- file.path(here(),'5_event_extraction/eventExtraction__beaver/Pophams',
                              'run_20201002_1643_value__padding2880_alpha0.98_passes3_BFI0.74/final_event_windows')



# ---------- Read Files function --------------
# function to read the files
read_event <- function(events_folder, event, id){
  evFile <- file.path(events_folder, event) 
  
  evDf <- read_csv(evFile, col_types = cols()) %>%
    mutate(event_step = row_number()) %>%
    mutate(`Beaver Present` = as.factor(ifelse(datetime > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                     ifelse(datetime > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                              datetime < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No")))) %>%
    filter(`Beaver Present` != "Unsure") %>%
    select(q_m3_s, event_step, `Beaver Present`) 
  
  
  max_eventStep <- max(evDf$event_step)
  Beaver_pres <- evDf$`Beaver Present`[1]
  
  Q_loess <- loess(q_m3_s ~ event_step, evDf, control=loess.control(surface="direct"), span=0.1)
  
  # new_NoBeavWet.BB <- tibble(event_step=seq(0, max_eventStep, length=max_eventStep*10), 
  #                            Beaver=Beaver_pres, Site = 'Budleigh Brook (impact)') %>%
  new_NoBeavWet.BB <- evDf  %>%
  broom::augment(Q_loess, type.predict = "response",
                   type.residuals = "deviance",
                   se_fit = F, data=.) %>%
    mutate(event_id = as.factor(id)) 

  return(new_NoBeavWet.BB)
    
}



# -------- safely read data function -------------
safe_loess <- function(evf, x, id, pb, ms){
  f  = purrr::safely(function() read_event(evf, x, id, ms), otherwise = NA)
  pb$tick()
  f()
}


# -------------- main function to map the functions to each row ---------------
#map function to file list and join
df_overlay <- function(events_folder, perc_cutoff = 0.95, maxhrs = NULL){
  

  event_path_list = list.files(events_folder)
  
  pb <- progress::progress_bar$new(total = length(event_path_list), clear = FALSE)
  
  events_combined <- event_path_list %>%
    purrr::imap(., ~ safe_loess(events_folder, .x, .y, pb)) %>%
    purrr::map(., purrr::pluck, "result") %>%
    Filter(Negate(anyNA), .) %>%
    bind_rows() %>%
    mutate(event_step = event_step/4)
  
  if (is.null(maxhrs)){
    maxhrs <- quantile(events_combined$event_step[events_combined$`Beaver Present` == 'Yes'], probs = c(perc_cutoff))
  } 
  
  events_combined <- events_combined %>%
    filter(event_step < maxhrs)
  
}
  
# --------------- Function to plot event overlays -----------------------

plot_overlay <- function(.data, se=TRUE){
  
  p <- ggplot(.data, aes(x=event_step, y=q_m3_s , group=event_id, colour=`Beaver Present`, fill=`Beaver Present`)) +
    geom_line(alpha=0.1, lwd=0.4) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    labs(x = 'time since event start (hrs)', y= (expression('Flow  ' (m^{3}~s^{-1})))) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    theme_bw() + 
    annotation_logticks(sides='l') 
  
  if (isTRUE(se)){
    # Loess option
    p <- p +  geom_smooth(method="loess", aes(y=q_m3_s, group=`Beaver Present`), colour=NA,  se=T, span=0.3, lwd=1.1, alpha=0.9)
    
    # Polynomial if preferred...
    # p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, 5, raw = TRUE),
    #               aes(y=q_m3_s, group=`Beaver Present`),
    #               colour=NA,  se=T, span=0.3, lwd=1.1, alpha=0.8)
  } else {
    p = p + geom_smooth(method="loess", aes(group=`Beaver Present`), se=F, span=0.3, lwd=1.1, alpha=0.9)
      
  }
  
  return(p)
  
}

# --------- Run the functions ----------------

bb <- df_overlay(events_folder=bb_events_folder) %>%
  mutate(Site = 'Budleigh Brook (impact)')

# bb$event_step[bb$`Beaver Present` == 'Yes']

cb_cutoff <- max(bb$event_step)

cb <- df_overlay(events_folder=cb_events_folder, maxhrs = cb_cutoff)%>%
  mutate(Site = 'Colaton Brook (control)')


# ----------- Create combined plot for control vs impact ---------------
combine_sites <- bind_rows(bb, cb) %>%
  plot_overlay(., se=T) +  # for faster plotting set se=FALSE
  facet_wrap(~ Site, ncol=2) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        # panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))

# ------------------------- save image ----------------------------------
# combine_sites
out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Loess.jpg')
ggsave(filename = out_path,plot = combine_sites, dpi=300)

# 
# p <- bb %>% group_by(event_step) %>%
#   summarise(count = n())
# 
# ggplot(data=p, aes(x = event_step, y=count)) +
#   geom_line(colour = '#048099', stat='identity') +
#   theme_light()
# quantile(bb$event_step, probs = c(0.95))
