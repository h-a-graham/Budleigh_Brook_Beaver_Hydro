
# optional sourcing version (not needed as the main script is stand alone..)
source(file.path(here::here(), '8_event_overlay/Event_overlay.R'))

# ----------------- define data folders --------------------------------
# Set root folder containing event csv files
#BB Events
bb_events_folder <- file.path(here(),'5_event_extraction/eventExtraction__beaver/EastBud',
                              'run_20201002_1454_value__padding2880_alpha0.98_passes3_BFI0.814/final_event_windows')

#CB Events
cb_events_folder <- file.path(here(),'5_event_extraction/eventExtraction__beaver/Pophams',
                              'run_20201002_1643_value__padding2880_alpha0.98_passes3_BFI0.74/final_event_windows')


# --------- Run the functions ----------------

bb <- df_overlay(events_folder=bb_events_folder) %>%
  mutate(Site = 'Budleigh Brook (impact)')

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
