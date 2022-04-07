# need dataframe w just last 10 cycles (endpoint) to make this work
library(tidyverse)
library(mosaic)
library(magrittr)

endpoint <- full_data %>% 
  filter(Cycle > 49) %>% 
  group_by(Target) %>%
  mutate(avg_f = mean(value),
         sd = sd(value)) %>% distinct()

controls <- endpoint %>% 
  group_by(Target) %>% 
  summarize(avg = mean(value), sd = sd(value), var = var(value)) %>% 
  filter(Target %in% c("INFA", "INFB", "RSVA"))

######## TESTING ###################################
# This eliminates a step - better to use than above
endpoint1 <- full_data %>% 
  filter(Cycle > 49) %>% 
  group_by(Target) %>%
  summarize(avg = mean(value), sd = sd(value), var = var(value)) %>% 
  filter(Target %in% c("INFA", "INFB", "RSVA"))


# NOW OBSOLETE:)
# new_control <- as.data.frame(expand.grid(controls$Target, controls$Target, controls$Target)) %>%
#   rename("V1" = Var1, "V2" = Var2, "V3" = Var3) %>%
#   mutate(V2 = ifelse(V2 == V1, NA, as.character(V2)),
#          V3 = ifelse(V3 == V2 | V3 == V1, NA, as.character(V3))) %>%
#   distinct()
# 
# new_control %<>%  left_join(controls, by = c(V1 = "Target"))
# new_control %<>%  left_join(controls, by = c(V2 = "Target") )
# new_control %<>%  left_join(controls, by = c(V3 = "Target") )
# 
# new_control %<>% 
#   group_by(V1, V2, V3) %>% 
#   mutate(total = sum(avg, avg.x, avg.y, na.rm = TRUE),
#          sum_var = sum(var, var.x, var.y, na.rm = TRUE),
#          sum_sd = sqrt(sum_var),
#          Target = paste(V1, V2, V3),
#          Target = str_trim(str_remove_all(Target, "NA"))) %>% 
#   ungroup() %>%
#   distinct(sum_sd, .keep_all = TRUE) %>%
#   select(Target, total, sum_sd)]




############### function work #####################

## combn

# TODO: need to change how we get this - DONE
controls <- endpoint %>% 
  group_by(Target) %>% 
  summarize(avg = mean(value), sd = sd(value), var = var(value)) %>% 
  filter(Target %in% c("INFA", "INFB", "RSVA"))

# TODO: may have to make a df w/ just controls
n <- controls$Target %>% n_distinct()
combos <- list()
datalist <- list()
t = 1
 
for (i in 1:n) {
  x <- combn(controls$Target, i)
  
  for (j in 1:ncol(x)) {
    combos[t] <- paste(x[,j], collapse = ",")
    t = t + 1
  }
}

i = 1
 for (target in combos) {

    dat <- more_dat %>% 
      filter(Target %in% c(str_split(target, ",")[[1]])) %>%
      group_by(Cycle, Target) %>% 
      summarize(avg_point = mean(avg_point),
                se = mean(se)) %>%
      ungroup() %>%
      group_by(Cycle) %>%
      summarize(avg_point = sum(avg_point),
                se = sqrt(sum(se^2))) %>%
      mutate(Target = target)
    
    datalist[[i]] = dat
    i = i + 1
}

all_controls = do.call(rbind, datalist)
