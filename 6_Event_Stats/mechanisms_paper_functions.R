
#' Create Data frame for use with model predictions.
#' 
Create_Data <- function(.data, vari, NoBeav.min, NoBeav.max, YesBeav.min, 
                        YesBeav.max, l =100){
  if(missing(NoBeav.min)) {
    NoBeav.min <- .data %>% filter(Beaver == 'Before') %>% select(!!vari) %>% 
      mutate_all(~(ifelse(is.na(.), 0, .))) %>% min()}
  if(missing(NoBeav.max)) { 
    NoBeav.max <- .data %>% filter(Beaver == 'Before') %>% select(!!vari) %>% 
      
      mutate_all(~(ifelse(is.na(.), 0, .))) %>% max()}
  if(missing(YesBeav.min)) {
    YesBeav.min <- .data %>% filter(Beaver == 'After') %>% select(!!vari) %>% 
      mutate_all(~(ifelse(is.na(.), 0, .))) %>% min()}
  if(missing(YesBeav.max)) {
    YesBeav.max <- .data %>% filter(Beaver == 'After') %>% select(!!vari) %>% 
      mutate_all(~(ifelse(is.na(.), 0, .))) %>% max()}
  
  new_NoBeavWet.BB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=l), 
                             Beaver='Before', Hydro.Seas = 'Wet', 
                             Site = 'Budleigh Brook (impact)')
  new_NoBeavDry.BB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=l), 
                             Beaver='Before', Hydro.Seas = 'Dry', 
                             Site = 'Budleigh Brook (impact)')
  new_NoBeavWet.CB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=l), 
                             Beaver='Before', Hydro.Seas = 'Wet', 
                             Site = 'Colaton Brook (control)')
  new_NoBeavDry.CB <- tibble(newvar=seq(NoBeav.min, NoBeav.max, length=l), 
                             Beaver='Before', Hydro.Seas = 'Dry', 
                             Site = 'Colaton Brook (control)')
  new_YesBeavWet.BB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=l), 
                              Beaver = 'After', Hydro.Seas = 'Wet', 
                              Site = 'Budleigh Brook (impact)')
  new_YesBeavDry.BB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=l), 
                              Beaver = 'After', Hydro.Seas = 'Dry', 
                              Site = 'Budleigh Brook (impact)')
  new_YesBeavWet.CB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=l), 
                              Beaver = 'After', Hydro.Seas = 'Wet', 
                              Site = 'Colaton Brook (control)')
  new_YesBeavDry.CB <- tibble(newvar=seq(YesBeav.min, YesBeav.max, length=l), 
                              Beaver = 'After', Hydro.Seas = 'Dry', 
                              Site = 'Colaton Brook (control)')
  
  df <- bind_rows(new_NoBeavWet.BB, new_NoBeavDry.BB, new_NoBeavWet.CB, 
                  new_NoBeavDry.CB, new_YesBeavWet.BB, new_YesBeavDry.BB, 
                  new_YesBeavWet.CB, new_YesBeavDry.CB) %>%
    mutate(!!vari := newvar,
           Beaver=fct_relevel(Beaver, "Before", "After")) %>%
    select(-newvar) %>% 
    
  
  return(df)
  
}

#' Create Model Predictions dataframe  
#' 
#' Wraps the above function to return a tidy data frame with model predictions
#' 
makes_preds <- function(.model, .name, df, ...) {
  Create_Data(.data=df, var='rain.tot.mm', ...) %>%
    broom::augment(.model, type.predict = "response",
                   type.residuals = "deviance",
                   se_fit = T, newdata=.) %>%
    mutate(mod.name = .name)
}

#' Plot the result of a GLM
#' 
#' return a customised plot of a moel output.
#' 
glm.plot <- function(.data, model.data, .y, line=TRUE) {
  
  .y <- enquo(.y)
  
  p <- ggplot(model.data, aes(x=rain.tot.mm, y=!!.y, colour=Beaver, fill=Beaver))+
    geom_point(data=.data, alpha = 0.4, size=0.9, stroke=0)
  
  if (isTRUE(lines)) {
    p <- p + geom_line(aes(x=rain.tot.mm, y = .fitted))
  }
  p +
    geom_ribbon(aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), 
                    ymax = .fitted + (1.96 *.se.fit)), 
                alpha=0.4, linetype=2, lwd=0.2) +
    scale_color_manual(values = c("#dd5129", "#0f7ba2")) +
    scale_fill_manual(values = c("#dd5129", "#0f7ba2")) +
    labs(y= (expression("Event Maximum Flow   " (m^{3}~s^{-1}))),
         x=expression("Total Event Rainfall   " (mm)),
         colour = "Beaver Status at Impacted Site", 
         fill = "Beaver Status at Impacted Site") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.text.y = element_text(size = 7, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          legend.position = 'top')
}

val.plot.dir <- '6_Event_Stats/ModelValidation'
if (!dir.exists(val.plot.dir)) dir.create(val.plot.dir)

#' Clean up {performance} model diagnostics plots
#' 
#' Edits the title and a few otherbits of model diagnostics plots
#' 
check_model_title <- function(.model, mod.num, .title, .size=30){
  pp <- check_model(.model)
  p <- plot(pp) 
  p$patches$plots[[2]] <- p$patches$plots[[2]]+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  p <- p + plot_annotation(title=.title)
  
  fname <- file.path(val.plot.dir, paste0(sprintf('Model%s_',mod.num),.title, '.png'))
  ggsave(fname, p, width = .size,
         height = .size, units='cm')
  
} 

#' Create a nice {gt} table
pretty.tab <- function(model.tab, title){
  model.tab %>%
    gt() %>%
    tab_header(
      title = md(sprintf('**<div style="text-align: left"> %s </div>**', title))) 
}


#' Clean data from earlier processing steps ready for stats analysis.
#' 
#' set the data range for beaver occupancy convert some values to hours etc.
#' 
CleanData <- function(.data){
  .data %>%
    mutate(Beaver = as.factor(ifelse(event.start.ts > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Yes",
                                     ifelse(event.start.ts > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                              event.start.ts < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "No"))))%>%
    filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>% # removes period of uncertainty where beaver's presence unknown.
    mutate(Beaver = fct_relevel(Beaver, "No", "Yes")) %>%
    mutate(rain.rate = rain.tot.mm/(rain.dur@.Data/3600)) %>%
    mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
    mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
    mutate(Month = (month(event.start.ts))) %>%
    mutate(Year = as.factor(year(event.start.ts))) %>%
    mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
    mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
    mutate(Season = (ifelse(Month >= 3 & Month < 6, "Spring",
                            (ifelse(Month >=6 & Month <9, "Summer",
                                    (ifelse(Month >=9 & Month <12, "Autumn",
                                            (ifelse(Month >=12 | Month <3, "Winter", ""))))))))) %>%
    mutate(Hydro.Seas = ifelse(Month >= 10 | Month < 4, "Wet", "Dry")) %>%
    mutate(Season = fct_relevel(Season, "Winter","Spring", "Summer", "Autumn"))%>%
    mutate(Hydro.Seas = fct_relevel(Hydro.Seas, "Dry", "Wet")) %>%
    select(-c(response.eventID,check.outlier1)) %>%
    drop_na()
}

#' function compare the different models wrapper for `glm.plot()` basically...
model_compare_plot <- function(.df, .log){
  p <- glm.plot(.df, merge_model_preds, .y=Q.peak.m3.s, line = FALSE) +
    facet_grid(mod.name~Site) 
  if (isTRUE(.log)) {
    p <- p + scale_y_continuous(breaks = c(0,4, 10, 20),
                                trans=scales::pseudo_log_trans(base = 10)) +
      coord_trans(ylim=c(0, 20))
  } else {
    p <- p + coord_cartesian(ylim=c(0,6))
  }
  add_general_facet_labs(p, 'Model', 'Site')
}



#' get the difference between pre and post beaver flow
#'
pred_diff <- function(df){
  pred.diff <- df %>%
    select(Beaver, .fitted,rain.tot.mm, .se.fit, Site) %>%
    group_by(Beaver) %>%
    group_split() %>%
    bind_cols() %>%
    mutate(.diff = .fitted...2 - .fitted...7,
           L.ci = (.fitted...2 + .se.fit...4*1.96)-(.fitted...7  - .se.fit...9*1.96),
           U.ci = (.fitted...2 - .se.fit...4*1.96)-(.fitted...7  + .se.fit...9*1.96)) #%>%
  pred.diff
  
}

#' Plot the Attenuation effect at each site.
Attenuation_plot <- function(.df, .var){
  
  .var = enquo(.var)
  
  ggplot(.df, aes(x=!!.var, y=.diff))+
    # geom_line() +
    geom_ribbon(aes(ymin=U.ci, ymax=L.ci, fill=.name), alpha=0.5, colour="#fab255", lwd=0.4) +
    ggpattern::geom_ribbon_pattern(data=att_region,aes(ymin=0,
                                                       ymax=U.ci,
                                                       pattern_colour=.name,
                                                       pattern_fill=.name
                                                       ),
                                   fill=NA, colour=NA, pattern_size=0.1,
                                   pattern_alpha=0.8,
                                   pattern ='crosshatch',pattern_density = 0.05,
                                   pattern_spacing=0.02
                                   ) +
    geom_hline(aes(yintercept=0), linetype=2)+
    theme_bw() +
    scale_fill_manual(values = "#fab255", name= '') +
    scale_pattern_fill_manual(values = c("#43b284"), name='') +
    scale_pattern_colour_manual(values = c("#43b284"), name='') +
    labs(y= (expression("Predicted Flow Attenuation  " (m^{3}~s^{-1})))) +
    facet_wrap(~Site...5) +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          legend.position = 'bottom')
} 

#' Function to create Raincloud plots 
#' 
#' visulises the raw data, IQR, median etc. with density distribution...
#' 

flowbox <- function(.data, yax) {
  .data %>%
    mutate(Site = fct_relevel(Site, "Budleigh Brook (impact)", "Colaton Brook (control)")) %>%
    ggplot(., aes(x = Beaver, y=Q.peak.m3.s, fill = Beaver))+
    geom_point(shape = 21, alpha = 0.5,position = position_jitterdodge(jitter.width = 0.2),
               stroke = 0.1, size=0.5) +
    geom_boxplot(colour = "black", alpha = 0.3, outlier.shape = NA, width = 0.2, 
                 lwd=0.2) +
    ggdist::stat_halfeye(aes(fill=Beaver),adjust = .7, width = .6, .width = 0, justification = -.25,
                         point_colour = 'NA', slab_alpha=0.5, slab_colour='black',
                         slab_size=0.4) +
    coord_cartesian(xlim = c(1, NA), clip = "off") +
    
    scale_fill_manual(values = c("#dd5129", "#0f7ba2")) +
    ylab(yax)+
    xlab('Beaver Status at Impacted Site') +
    theme_bw() +
    facet_wrap(~Site, ncol=2)+
    theme(legend.position="none",
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
  
}

