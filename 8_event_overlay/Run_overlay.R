library(ggforce)
library(patchwork)

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
sites_bind <- bind_rows(bb, cb)

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

  mutate(height = case_when(beaver == 'Yes' ~ 1,
                            TRUE ~ 2.5),
         .label = case_when(Site == 'Budleigh Brook (impact)' ~ "Lag time (Peak rain to Peak Q)",
                           TRUE ~ ''))

# summ stats.

PeakQ.df %>%
  group_by(Site) %>%
  summarise(lagChange = (lagTime[beaver=='Yes']-lagTime[beaver=='No'])/
              lagTime[beaver=='No'] *100,
            lagGradientChangeAvg = (RL_gradientAvg[beaver=='Yes']-RL_gradientAvg[beaver=='No'])/
              RL_gradientAvg[beaver=='No'] *100,
            lagGradientChangeLow = (RL_gradientLow[beaver=='Yes']-RL_gradientLow[beaver=='No'])/
              RL_gradientLow[beaver=='No'] *100,
            lagGradientChangeHigh = (RL_gradientHigh[beaver=='Yes']-RL_gradientHigh[beaver=='No'])/
              RL_gradientHigh[beaver=='No'] *100)

combine_sites <- sites_bind %>%
  plot_overlay(., se=T, method = 'gam', ticks = FALSE) +  # for faster plotting set se=FALSE
  geom_point(data=PeakQ.df, aes(x=PredQMaxTime, y=PredQMax, group=NULL, pch='Average Event Peak Q and Rainfall'), colour='black')+
  geom_segment(data=PeakQ.df, aes(x=PredPrecMaxTime,xend=PredQMaxTime, y=height, yend=height,
                                   colour=beaver, group=NULL),
               arrow = arrow(length = unit(0.03, "npc")))+
  geom_linerange(data=PeakQ.df, aes(x=PredPrecMaxTime, ymin=height, ymax=20,
                                    colour=beaver, group=NULL, y=NULL), 
                 linetype=2, alpha=0.6) +
  geom_linerange(data=PeakQ.df, aes(x=PredQMaxTime, ymin=PredQMax, ymax=height,
                                    colour=beaver, group=NULL, y=NULL), 
                 linetype=2, alpha=0.6) +
  geom_text(data=PeakQ.df, mapping = aes(x=25, y=2.5, label= .label, group=NULL), 
            size=3, colour='black') + 
  scale_y_log10()+
  coord_trans(ylim=c(1.5e-2, 10)) +
  scale_shape_manual(values=4, name=NULL)+
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  facet_wrap(~ Site, ncol=2) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        # panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))


# # ------------------------- save image ----------------------------------
combine_sites
# out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam.png')
# ggsave(filename = out_path,plot = combine_sites, dpi=300)

# 


# alt_plot <- combine_sites +
#   scale_y_continuous()+
#   coord_cartesian(ylim=c(0,0.5)) +
#   theme(
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )
# alt_plot
out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam_NoTrans.png')
# ggsave(filename = out_path,plot = alt_plot, dpi=300)

# ---- plotting rainfall -----------------------

PeakPrec.df %>%
  select(-PredPrecMax) %>%
  group_by(Site) %>%
  summarise(lagChange = (PredPrecMaxTime[beaver=='Yes']-PredPrecMaxTime[beaver=='No'])/
              PredPrecMaxTime[beaver=='No'] *100)

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
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
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
  # scale_y_continuous(trans='log10')+
  # scale_y_continuous() +
  # coord_cartesian(y=c(limits = c(0, 0.4)))+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.position="bottom") +
    annotation_logticks(
    short = unit(0,"mm"),
    mid = unit(0,"mm"),
    long = unit(0,"mm")
  )


rain_FlowPlot <- p1/p2 + plot_layout(heights = c(1, 2))
rain_FlowPlot
rain_FlowPlot_path <- file.path(here(), '8_event_overlay/exports', 'FlowRainOverlayNOTRANSrev.png')
ggsave(filename = rain_FlowPlot_path,plot = rain_FlowPlot, 
       width = 18, height = 18, units = 'cm', dpi = 300)
  


# ---------- predictive GAM appraoch with multiple variables --------------
# - has legs but I think not that helpful - better to use qualitative approach above
#
# hydroGAMbb <- mgcv::gam(q_m3_s ~  s(event_step, by=beaver, bs='cs') + tot_rain + s_flow,
#                   method = "REML", data = bb)
# 
# bb_offset <- sum(coef(hydroGAMbb)[1:3])
# 
# GAM_s_BB <- df_from_gam(hydroGAMbb, offset = bb_offset) %>%
#   mutate(Site = as.factor('Budleigh Brook (impact)'))
# 
# hydroGAMcb <- mgcv::gam(q_m3_s ~  s(event_step, by=beaver, bs='cs') + tot_rain + s_flow,
#                       method = "REML", data = cb)
# 
# cb_offset <- sum(coef(hydroGAMbb)[1:3])
# GAM_s_CB <- df_from_gam(hydroGAMcb, offset = cb_offset) %>%
#   mutate(Site = as.factor('Colaton Brook (control)'))
# 
# GAM_s_plot <- bind_rows(GAM_s_BB, GAM_s_CB) %>%
#   ggplot(., aes(x=event_step, y=.fitted , colour=beaver, fill=beaver)) +
#   geom_line() +
#   labs(x = 'time since event start (hrs)', y= (expression('Flow  ' (m^{3}~s^{-1})))) +
#   # coord_cartesian(y=c(0.1,0.5))+
#   scale_color_manual(values = c('#A6190D', '#244ED3')) +
#   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
#   geom_ribbon(aes(x=event_step, ymin = .fitted - (.se*1.96), 
#                   ymax = .fitted + (.se*1.96), colour=beaver,
#                   fill=beaver),lwd=0.7, alpha=0.7, inherit.aes = F) +
#   facet_wrap(~Site)+
#   theme_bw()+
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         # panel.border = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         strip.text.x = element_text(size = 12, color = "black", face = "italic"),
#         strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
# GAM_s_plot
# 
# out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam_Smooths.jpg')
# ggsave(filename = out_path,plot = GAM_s_plot, dpi=300)
#   
# 
# GAM_s_BB %>%
#   group_by(beaver)%>%
#   filter(.fitted == max(.fitted))
# 
# GAM_s_CB %>%
#   group_by(beaver)%>%
#   filter(.fitted == max(.fitted))
# 
# 
# 
# 
# 
# 
# 
# 
#  
# 
# 
# new_dat_gen <- function(.data, tot_rain, s_flow){
#   add_beav <- function(beav){
#     event_step <- seq(0, max(.data$event_step), by=0.25)
#     tibble(event_step, tot_rain, s_flow)%>%
#       mutate(beaver=beav)
#   }
#   
#   bind_rows(add_beav('Yes'), add_beav('No'))
#   
# }
# 
# # 
# hydroGAM <- mgcv::gam(q_m3_s ~  s(event_step, by=beaver, bs='cs') + tot_rain +
#                         s_flow + beaver,
#                         method = "REML", data = bb, gamma=2)
# 
# new_df_bb <- new_dat_gen(bb, quantile(bb$tot_rain, 0.5), quantile(bb$s_flow, 0.5))
# 
# preds <- predict(hydroGAM, newdata = new_df_bb, se.fit=TRUE)
# 
# new_df_bb2 <- new_df_bb %>%
#   mutate(.fitted = preds[[1]]) %>%
#   mutate(.se = preds[[2]])
# 
# 
#   ggplot(new_df_bb2, aes(x=event_step, y=.fitted , colour=beaver, fill=beaver)) +
#   geom_line() +
#   labs(x = 'time since event start (hrs)', y= (expression('Flow  ' (m^{3}~s^{-1})))) +
#   # coord_cartesian(y=c(0.1,0.5))+
#   scale_color_manual(values = c('#A6190D', '#244ED3')) +
#   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
#   geom_ribbon(aes(x=event_step, ymin = .fitted - (.se*1.96),
#                   ymax = .fitted + (.se*1.96), colour=beaver,
#                   fill=beaver),lwd=0.7, alpha=0.7, inherit.aes = F) +
#   theme_bw()+
#   theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
#         # panel.border = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         strip.text.x = element_text(size = 12, color = "black", face = "italic"),
#         strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
# 
# 
# 
# sts<-  new_df_bb2 %>%
#     group_by(beaver)%>%
#     filter(.fitted == max(.fitted))
# sts
# 
# (sts[2,]$.fitted - sts[1,]$.fitted)/sts[2,]$.fitted*100
# 





#  The Future --------------------------
# qunatile appraoch ---------------- Very experimental!!!
# This disn't get very far because I couldn't get things working with interactive variables.
#has milage and will probeably require a quantred or qgam appraoch...
# library(quantreg)
# 
# 
# .rqss_mapper <- function(.data, .quant){
#   
#   # Run model
#   m <-rqss(q_m3_s ~ qss(event_step), lambda=0.5, tau = .quant, data = .data)
#   
#   #generate augment df
#   new_dat <- .data %>%
#     dplyr::select(q_m3_s, event_step, beaver)
#   
#   pred_tib <- .data %>%
#     select(event_step, beaver)%>%
#     bind_cols(., predict.rqss(m, newdata = new_dat)) %>%
#     rename(.rqss.fit = 3) %>%
#     mutate(.tau = .quant)
#   
#   
#   # return(pred_tib)
# }
# 
# 
# tid_rqss <- function(.data, Q.list = c(0.1, 0.5, 0.75)) {
#   get_quants <- Q.list %>%
#     purrr::map(., ~.rqss_mapper(.data, .)) %>%
#     bind_rows() #%>%
#   # bind_cols(., .data) 
# }
# 
# 
# 
# t1 <- tid_rqss(bb)
# 
# 
# ggplot(t1, aes(x=event_step, y=.rqss.fit , colour=beaver, fill=beaver)) +
#   geom_line() +
#   facet_wrap(~.tau)
# 
# 
# geom_line(mapping = aes( y=q_01),alpha=0.1, lwd=1) +
#   geom_line(mapping = aes( y=q_05),alpha=0.1, lwd=1) +
#   geom_line(mapping = aes( y=q_075),alpha=0.1, lwd=1) +
#   
#   scale_y_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = scales::trans_format("log10", scales::math_format(10^.x))) +
#   labs(x = 'time since event start (hrs)', y= (expression('Flow  ' (m^{3}~s^{-1})))) +
#   scale_color_manual(values = c('#A6190D', '#244ED3')) +
#   scale_fill_manual(values = c('#A6190D', '#244ED3')) +
#   theme_bw() + 
#   annotation_logticks(sides='l') 
# 
# ggplot(bb, aes(x=event_step, y=q_m3_s , colour=beaver, fill=beaver)) +
#   geom_smooth(method = 'gam')
#   
#   
# # wait but GAMS
# library(qgam); library(mgcv)
# 
