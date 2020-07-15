## Functions

#----  Timeseries plot functions -----------------------------------------------

# set the x axis limits in POSIXct (as used in the ggplot x axis) 1st ts "2011-12-31 15:10" nth is "2015-12-31 11:05"

# xlim_min <- as.POSIXct(strptime("2015-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
# xlim_max <- as.POSIXct(strptime("2015-02-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot

# legnd <- c("Event","Rainfall", "Baseflow","Q", "Response event", "value", "smoothed", "peak")

update_geom_defaults("line", list(size = 0.3))

#----  PLOT: ESD check for outliers  -------------------------------------------


## Flow, with 3sigma outlier hreasholds, and any removed outliers as points
ggESDcheck <- function(DAT){
  update_geom_defaults("line", list(size = 0.3))
  p <- ggplot(data = DAT) +
    geom_line(aes(x = datetime, y= working),
              size = 0.2) +
    geom_ribbon(aes(ymin = roll.genesd.thlow, 
                    ymax = roll.genesd.thhigh,
                    x = datetime),
                fill = "red",
                alpha = 0.2,
                linetype = 0) +
    geom_point(aes(x = datetime, y = removed),
               color = "red",
               shape = 3) +
    ylab("Value") +
    xlab ("Date") +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                               # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    theme_bw() + theme(legend.key = element_blank(),
                       legend.title = element_blank(),
                       legend.position = "bottom",
                       text = element_text(size = 8)) +
    scale_colour_manual(values= c("grey", "black"))
  
  print(p)
}


#---- PLOT: Rain (simple) ------------------------------------------------------

ggRainSimple <- function(DAT, xlim_min, xlim_max){
  
  legnd <- c("Event","Rainfall", "Baseflow", "Q")
  update_geom_defaults("line", list(size = 0.3))
  
  ggplot() + 
    geom_col(data = DAT, 
             aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), 
             width = 0.5) + 
    ylab("Rainfall (mm h⁻¹)")+
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_reverse(limits = c()) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(), 
                                    legend.position = "none",
                                    axis.title.x=element_blank()
                                    ,axis.text.x=element_blank()
                                    ,axis.ticks.x=element_blank()) +
    scale_colour_manual(values= c("grey50")) +
    scale_fill_manual(values = c("cyan","grey50", "blue")) +
    scale_alpha(range=c(0.1))
}


#---- PLOT: Flow (simple) ------------------------------------------------------

ggQBaseflowSimple <- function(DAT, xlim_min, xlim_max){
  
  legnd <- c("Event","Rainfall", "Baseflow", "Q", "Response event", "Value", "Intermediate flow")
  update_geom_defaults("line", list(size = 0.2))
  
  ggplot(DAT) +
    geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "bottom") +               # blank element removes background to legend
    scale_colour_manual(values= c("grey40", "black" )) +
    scale_linetype_manual(values= c("longdash", "solid" ))
}


#---- PLOT: Rain with events ---------------------------------------------------

ggRainfall <- function(DAT, ylimit1){
  
  ggplot(DAT) + 
    geom_col(aes(datetime, event*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
    geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
    geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
    ylab("Rainfall (mm/h)")+
    scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_reverse(limits = c()) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(), 
                                    legend.position = "none",
                                    axis.title.x=element_blank()) +
    scale_colour_manual(values= c("grey50")) +
    scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))
  
}


#---- PLOT: REsponse with events ---------------------------------------------------

ggQ <- function(DAT,ylimit2){
  
  ggplot(DAT) +
    geom_col(aes(datetime, event*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
    geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
    geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_continuous(limits = c(0,ylimit2)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "bottom",
                                    legend.justification="left") +                           # blank element removes background to legend
    scale_colour_manual(values= c("grey40", "black")) +
    scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
    scale_linetype_manual(values= c("longdash", "solid")) +
    guides(colour = guide_legend(ncol = 1)) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                               ncol = 1))
}


########################












# ggQ <- function(DAT,ylimit){
#   
#   ggplot(DAT) +
#     geom_col(aes(datetime, event*ylimit, fill=legnd[1]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
#     geom_col(aes(datetime, response.event*ylimit, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
#     geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
#     geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
#     xlab ("Date") +
#     ylab (expression(Flow~(m^{3}~s^{-1}))) +
#     scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
#                      date_labels = ("%Y-%m-%d\n%H:%M")) +
#     scale_y_continuous(limits = c(0,ylimit)) +
#     theme_bw(base_size = 8) + theme(legend.key = element_blank(),
#                        legend.title=element_blank(),
#                        legend.position = "bottom",
#                        legend.justification="left") +                           # blank element removes background to legend
#     scale_colour_manual(values= c("grey50", "black")) +
#     scale_fill_manual(values = c("cyan","grey50")) +
#     scale_alpha(range=c(0.2)) +
#     scale_linetype_manual(values= c("longdash", "solid")) +
#     guides(colour = guide_legend(ncol = 1)) +
#     guides(fill = guide_legend(override.aes = list(alpha = 0.2),
#            ncol = 1))
# }

ggValue <- function(DAT,ylimit){
  
  ggplot(DAT) +
    geom_col(aes(datetime, event*ylimit, fill=legnd[1], alpha = 1), width=1*60*15, position=position_dodge(1*60*15)) +
    geom_col(aes(datetime, response.event*ylimit, fill=legnd[5], alpha = 1), width=1*60*15, position=position_dodge(1*60*15)) +
    geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
    geom_line(aes(datetime, value, colour = legnd[5], linetype =legnd[5])) +
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_continuous(limits = c(0,ylimit)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                       legend.title=element_blank(),
                       legend.position = "none") + #(base_size = 12) +     # blank element removes background to legend
    scale_colour_manual(values= c("grey50", "black", "red")) +
    scale_fill_manual(values = c("cyan","blue")) +
    scale_alpha(range=c(0.1)) +
    scale_linetype_manual(values= c("longdash", "solid", "solid")) +
    guides(fill=guide_legend(nrow=1))
}


#---- quick checks -------------------------------------------------------------





ggvalueSimple <- function(DAT){
  
  legnd <- c("Event","Rainfall", "Baseflow","Q", "Responce event", "value")
  
  ggplot(DAT) +
    geom_line(aes(datetime, value, colour = legnd[6], linetype =legnd[6])) +
    geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                     date_labels = ("%Y-%m-%d\n%H:%M")) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "bottom") +                 # blank element removes background to legend
    scale_colour_manual(values= c("black", "red")) +
    scale_linetype_manual(values= c("solid","solid" ))
}



