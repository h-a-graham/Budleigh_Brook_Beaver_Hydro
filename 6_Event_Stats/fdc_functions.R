#FDC_functions


Read.Edit.Flow <- function(.allFLow){
  .allFLow %>%
    mutate(Beaver = as.factor(ifelse(datetime > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Present",
                                     ifelse(datetime > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") &
                                              datetime < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"), "Unsure", "Absent"))))%>%
    filter(Beaver != "Unsure") %>%
    drop_na()
}

# create Summary tibbles
Flow.Sum.Tab <- function(.data){
  site <- .data[['Site']][1]
  .data %>%
    group_by(Beaver) %>% 
    summarize(Mean = mean(q), Median = median(q), R2FDC = ((log10(quantile(q, 0.66)) - log10(quantile(q, 0.33)))/(0.66-0.33))*-1,
              Q5 = quantile(q, 0.95), Q95 = quantile(q, 0.05)) %>%
    mutate(Beaver = c('No Beaver','Beaver'), `Q5:Q95 ratio` = Q5/Q95) %>%
    rename(" " = Beaver) %>%
    bind_rows(summarise(.," " = '% Change',
                        Mean = (Mean[2]-Mean[1])/Mean[1]*100,
                        Median = (Median[2]-Median[1])/Median[1]*100,
                        R2FDC= (R2FDC[2]-R2FDC[1])/R2FDC[1]*100,
                        Q5 = (Q5[2]-Q5[1])/Q5[1]*100,
                        Q95 = (Q95[2]-Q95[1])/Q95[1]*100,
                        `Q5:Q95 ratio` = (`Q5:Q95 ratio`[2]-`Q5:Q95 ratio`[1])/`Q5:Q95 ratio`[1]*100,))%>%
    mutate_at(vars(Mean, Median, R2FDC, Q5, Q95, `Q5:Q95 ratio`), round,3) %>%
    mutate(Site=site)
}


# Create joint table for plotting
calc.fdc <- function(.data){
  NoBeavCurve <-  .data %>%
    drop_na() %>%
    filter(Beaver == 'Absent')%>%
    select(q, Beaver, Site) %>%
    arrange(desc(q)) %>%
    mutate(pcntexceedance = seq (0, 1, by = 1/(n()-1)))
  
  
  YesBeavCurve <-  .data %>%
    drop_na() %>%
    filter(Beaver == 'Present')%>%
    select(q, Beaver, Site) %>%
    arrange(desc(q)) %>%
    mutate(pcntexceedance = seq (0, 1, by = 1/(n()-1)))
  
  BefAftCurve <- bind_rows(NoBeavCurve, YesBeavCurve)
}


#plot FDC
plot.fdc <- function(.data){
  .data %>%
    mutate(Site = fct_relevel(Site, "Budleigh Brook (impact)", "Colaton Brook (control)")) %>%
    ggplot(., aes(x = pcntexceedance, y = q, colour=Beaver)) +
    geom_vline(xintercept=0.05, lwd = 0.5, linetype=5, colour='grey20') +
    annotate("text", x=0.1, label="Q5", y=1, size=3, colour='grey20') +
    geom_vline(xintercept=0.95, lwd = 0.5, linetype=5, colour='grey20') +
    annotate("text", x=0.89, label="Q95", y=1, size=3, colour='grey20') +
    geom_line() +
    ylab(expression(Flow~(m^{3}~s^{-1})))+
    xlab('% time flow equalled or exceeded')+
    labs(colour="Beaver Status at Impacted Site") +
    scale_x_continuous(labels = scales::percent) +
    scale_y_log10(limits=c()) +
    scale_color_manual(values = c("#dd5129", "#0f7ba2")) +
    facet_wrap(~Site) +
    theme_bw()+
    theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          legend.position=c(.23,.87),
          legend.background=element_blank(),
          legend.title=element_text(size=10),
          panel.border = element_blank())
  
}