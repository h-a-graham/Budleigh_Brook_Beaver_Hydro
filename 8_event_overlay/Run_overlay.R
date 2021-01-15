
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
  mutate(Site = as.factor('Budleigh Brook (impact)'))

cb_cutoff <- max(bb$event_step)

cb <- df_overlay(events_folder=cb_events_folder, maxhrs = cb_cutoff)%>%
  mutate(Site = as.factor('Colaton Brook (control)'))


# bb <- df_overlay(events_folder=bb_events_folder, beaver_time = "2019-01-01 00:00") %>%
#   mutate(Site = 'Budleigh Brook (impact)')


# ----------- Create combined plot for control vs impact ---------------
combine_sites <- bind_rows(bb, cb) %>%
  plot_overlay(., se=T, method = 'gam') +  # for faster plotting set se=FALSE
  facet_wrap(~ Site, ncol=2) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
        # panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))


# # ------------------------- save image ----------------------------------
combine_sites
out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam.jpg')
ggsave(filename = out_path,plot = combine_sites, dpi=300)

# 


alt_plot <- combine_sites +
  scale_y_continuous()+
  coord_cartesian(ylim=c(0,1)) 
alt_plot

out_path <- file.path(here(), '8_event_overlay/exports', 'FlowOverlay_Gam_NoTrans.jpg')
ggsave(filename = out_path,plot = alt_plot, dpi=300)


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
