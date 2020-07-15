
#==== Plot: Scatter with simple liniar regression model ========================

## plot: scatter for two parameters
p <- ggplot (EVENTS, aes(x = rlimb.rain, y = Q.response.quick.m3)) +
  geom_point(colour = "black", 
             shape = 1) +
  geom_smooth(method = lm,                                                      
              level = 0.95, # confidence interval 0.95 by default
              size = 0.3, 
              colour = "black", fill = "black", alpha = 0.1) +
  # geom_text(aes(label = eventIDnew),
  #           hjust = -1, vjust = 1, 
  #           colour =  "red") +
  scale_x_continuous() +
  scale_y_continuous() +                              
  theme_bw(base_size = 8) + theme(legend.key = element_blank()) 

print(p)

#==== Model: Simple liniar model ===============================================

## simple liniar model
mod1 <- lm(formula = Q.response.quick.m3 ~ rlimb.rain,
           data = EVENTS,
           na.action = na.omit)

mod1

plot(mod1)

print(p)

#==== Tests for assoiation between continuous valuaboes: =======================

#---- Coefficient of determination (r squared) ---------------------------------

## For normal distributions

## Extract from modal
r_squared <- summary(mod1)$r.squared 
r_squared

## Pearson's correlation coefficiaent R squared using cor()
## (N.B.  the ^2 in the code must be inlcuded)
cor(EVENTS[,c("Q.response.quick.m3", "rlimb.rain")] , 
    method = "pearson",
    use = "pairwise.complete.obs")^2 



#---- Non- parametric correlation ----------------------------------------------

## N.B. Rank based - do not use for calculating r squared
## N.B. exact p-values are not computed where there are ties in the data

## Spearmans Rank correlation (rho).............................................

## (N.B.  does not handel ties pairs or small sample sizes well - consider Kendall's Tau)
spearmans_rank <- cor.test(x = EVENTS$rlimb.rain, 
                           y = EVENTS$Q.response.quick.m3, 
                           method = "spearman",
                           conf.level = 0.95)

spearmans_rank # Check for warning "Cannot compute exact p-value with ties"
  

## Kendall's Tau correlation (tau)..............................................

## (N.B.  uncertain Which method is used in this function
##        can use tau-a, tau-b, and tau-c; tau-b is adapted to handle ties 
##        https://stackoverflow.com/questions/10711395/spearman-correlation-and-ties)
kendalls_tau <- cor.test(x = EVENTS$rlimb.rain, 
                           y = EVENTS$Q.response.quick.m3, 
                           method = "kendall",
                           conf.level = 0.95)

kendalls_tau

#==== Tidy plot ================================================================

## plot: scatter for two parameters
p <- ggplot (EVENTS, aes(x = rlimb.rain, y = Q.response.quick.m3)) +
  
  geom_point(colour = "black", 
             shape = 1) +
  geom_smooth(method = lm,                                                      
              level = 0.95, # confidence interval 0.95 by default
              size = 0.3, 
              colour = "black", fill = "black", alpha = 0.1) +

  xlab (expression(Rising~limb~rainfall~(mm))) +
  ylab (expression(Quick~flow~(m^{3}))) +
  labs(caption = paste0("R² = ", round(r_squared, digits = 3))) +
  
  scale_x_continuous() +
  scale_y_continuous() +
  
  theme_bw(base_size = 8) + theme(legend.key = element_blank()) 

print(p) 

#===============================================================================
  



