library(ggforce)
library(patchwork)
# library(MetBrewer)
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

bb_dat <- df_overlay(events_folder=bb_events_folder) 
bb_list <- fit_gams(bb_dat)
bb <- bb_list$data %>%
  mutate(Site = as.factor('Budleigh Brook (impact)'))


cb_cutoff <- max(bb$event_step)

cb_dat <- df_overlay(events_folder=cb_events_folder)
cb_list <- fit_gams(cb_dat, maxhrs = cb_cutoff)
cb <- cb_list$data %>%
  mutate(Site = as.factor('Colaton Brook (control)'))

check_gam <- function(.gam){
  layout(matrix(1:4, ncol = 2, byrow = TRUE))
  mgcv::gam.check(.gam)
  layout(1)
}
check_gam(bb_list$flowGAM)
check_gam(cb_list$flowGAM)
check_gam(bb_list$rainGAM)
check_gam(cb_list$rainGAM)
# bb <- df_overlay(events_folder=bb_events_folder, beaver_time = "2019-01-01 00:00") %>%
#   mutate(Site = 'Budleigh Brook (impact)')


# ----------- Create combined plot for control vs impact ---------------
sites_bind <- bind_rows(bb, cb) %>%
  mutate(beaver = case_when(beaver == 'Yes' ~ "Present",
                              TRUE ~ "Absent"))


PeakQ.df <- sites_bind %>%
  mutate(gam.LowCI = gam.fitted-(gam.se.fit*1.96),
         gam.HighCI = gam.fitted+(gam.se.fit*1.96)) %>%
  group_by(Site, beaver) %>%
  summarise(PredQMax = max(gam.fitted),
            PredQMaxTime = event_step[which.max(gam.fitted)],
            PredPrecMax = max(.fitted_rain ),
            PredPrecMaxTime = event_step[which.max(.fitted_rain)],
            lagTime = PredQMaxTime - PredPrecMaxTime,
            RL_gradientAvg = (max(gam.fitted) - gam.fitted[which.max(.fitted_rain)])/lagTime,
            RL_gradientLow = (gam.LowCI[which.max(gam.fitted)] - gam.HighCI[which.max(.fitted_rain)])/lagTime,
            RL_gradientHigh = (gam.HighCI[which.max(gam.fitted)] - gam.LowCI[which.max(.fitted_rain)])/lagTime) %>%

  mutate(height = case_when(beaver == "Present" ~ 1,
                            TRUE ~ 2.5),
         .label = case_when(Site == 'Budleigh Brook (impact)' ~ "Lag time (Peak rain to Peak Q)",
                           TRUE ~ ''))

# summ stats.

PeakQ.df %>%
  group_by(Site) %>%
  summarise(lagChange = (lagTime[beaver=='Present']-lagTime[beaver=='Absent'])/
              lagTime[beaver=='Absent'] *100,
            lagGradientChangeAvg = (RL_gradientAvg[beaver=='Present']-RL_gradientAvg[beaver=='Absent'])/
              RL_gradientAvg[beaver=='Absent'] *100,
            lagGradientChangeLow = (RL_gradientLow[beaver=='Present']-RL_gradientLow[beaver=='Absent'])/
              RL_gradientLow[beaver=='Absent'] *100,
            lagGradientChangeHigh = (RL_gradientHigh[beaver=='Present']-RL_gradientHigh[beaver=='Absent'])/
              RL_gradientHigh[beaver=='Absent'] *100)

# flow plot
combine_sites <- sites_bind %>%
  plot_overlay(., se=T, method = 'gam', ticks = FALSE, colours = c("#dd5129", "#0f7ba2")) +  # for faster plotting set se=FALSE
  geom_point(data=PeakQ.df, aes(x=PredQMaxTime, y=PredQMax, group=NULL, pch='GAM Hydrograph Peak Q and Rainfall'), colour='black')+
  geom_segment(data=PeakQ.df, aes(x=PredPrecMaxTime,xend=PredQMaxTime, y=height, yend=height,
                                   colour=beaver, group=NULL),
               arrow = arrow(length = unit(0.03, "npc")), show.legend=FALSE)+
  geom_linerange(data=PeakQ.df, aes(x=PredPrecMaxTime, ymin=height, ymax=20,
                                    colour=beaver, group=NULL, y=NULL), 
                 linetype=2, alpha=0.6, show.legend=FALSE) +
  geom_linerange(data=PeakQ.df, aes(x=PredQMaxTime, ymin=PredQMax, ymax=height,
                                    colour=beaver, group=NULL, y=NULL), 
                 linetype=2, alpha=0.6, show.legend=FALSE) +
  geom_text(data=PeakQ.df, mapping = aes(x=25, y=2.5, label= .label, group=NULL), 
            size=3, colour='black') + 
  scale_y_log10()+
  coord_trans(ylim=c(1.5e-2, 10)) +
  scale_shape_manual(values=4, name=NULL)+
  guides(fill = guide_legend(override.aes = list(shape = NA),
                             order=1),
         colour = guide_legend(order=1)) +
  facet_wrap(~ Site, ncol=2) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        # panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))


# # ------------------------- save image ----------------------------------
combine_sites

out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam_NoTrans.png')
# ggsave(filename = out_path,plot = alt_plot, dpi=300)

# ---- plotting rainfall -----------------------

PeakQ.df %>%
  select(-PredPrecMax) %>%
  group_by(Site) %>%
  summarise(lagChange = (PredPrecMaxTime[beaver=='Present']-PredPrecMaxTime[beaver=='Absent'])/
              PredPrecMaxTime[beaver=='Absent'] *100)

p1 <- ggplot(sites_bind, aes(x=event_step, y=.fitted_rain , colour=beaver, fill=beaver)) +
  geom_point(aes(y=rainfall_mm_h),alpha=0.06, lwd=0.4) +
  # geom_line(lwd=1.1, alpha=0.4) +
  geom_ribbon(aes(x=event_step, ymin = .fitted_rain - (.se.fit_rain*1.96),
                  ymax = .fitted_rain + (.se.fit_rain*1.96), colour=beaver,
                  fill=beaver),lwd=0.5, alpha=0.7, inherit.aes = F) +
  geom_point(data=PeakQ.df, aes(x=PredPrecMaxTime, y=PredPrecMax, group=NULL),pch=4, colour='black')+
  geom_linerange(data=PeakQ.df, aes(x=PredPrecMaxTime, ymin=PredPrecMax, ymax=1,
                                    colour=beaver, group=NULL, y=NULL), 
                 linetype=2, alpha=0.6) +
  labs(x = 'time since event start (hrs)', y= (expression('Rainfall   ' (mm/hr^{-1})))) +
  scale_color_manual(values = c("#dd5129", "#0f7ba2")) +
  scale_fill_manual(values = c("#dd5129", "#0f7ba2")) +
  theme_bw() + 
  labs(color='Beaver Present', fill='Beaver Present') +
  facet_wrap(~Site) +
  guides(fill='none', colour='none')+
  scale_y_reverse(limits = c(1, 0),labels = scales::number_format(accuracy = 0.1)) +
  theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(), 
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3)) 


p2 <- combine_sites +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position="bottom",
    legend.box="vertical",
    legend.key.height = unit(4, 'mm'),
    legend.spacing.y = unit(0.2, "mm")) +
    annotation_logticks(
    short = unit(0,"mm"),
    mid = unit(0,"mm"),
    long = unit(0,"mm")
  )


rain_FlowPlot <- p1/p2 + plot_layout(heights = c(1, 2))
rain_FlowPlot
rain_FlowPlot_path <- file.path(here(), '8_event_overlay/exports', 'FlowRainOverlayNOTRANSrevV2.png')
ggsave(filename = rain_FlowPlot_path,plot = rain_FlowPlot, 
       width = 18, height = 18, units = 'cm', dpi = 300)
  


