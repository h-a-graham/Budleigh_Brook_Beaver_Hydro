# function to create Tidy Regression SUmmary table
add.stat.tab <- function(.model, poly=F){ # function to create a table for additive model
  tidy(.model, type.predict = "response") %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic)
}