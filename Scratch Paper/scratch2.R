library(tidyverse)
library(mosaic)
library(readxl)
library(magrittr)
library(stats)
############ Data Wrangling #################################################
f <- read_csv("qPCR data.csv") %>%
  pivot_longer(cols = 2:97, names_to = "Well") #, values_to = "Value"

p <- read_excel("qPRC plate.xlsx", sheet = "Data")

full_data <- f %>% left_join(p, by = c("Well" = "Sample")) %>% 
  mutate(Target = case_when(Target == "INFA/INFBRSVA" ~ "INFA/INFB/RSVA",
                            Target == "INFA/NFB/RSVA" ~ "INFA/INFB/RSVA",
                            TRUE ~ Target)) 

find_outliers <- full_data %>% 
  filter(Cycle %in% 45:60) %>%
  group_by(Target,Well) %>%
  mutate(avg = mean(value)) %>%
  ungroup() %>%
  group_by(Target) %>%
  mutate(Q1 = quantile(value, .25),
         Q3 = quantile(value, .75),
         IQR_x = iqr(value),
         mini = Q1 - 1.5*IQR_x,
         maxi = Q3 + 1.5*IQR_x,
         outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                             TRUE ~ TRUE)) %>%
  select(Well, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
  unique() %>%
  filter(outlier)

full_data %<>% filter(!Well %in% find_outliers$Well)

# endpoint <- full_data %>% 
#   filter(Cycle > 49) 
# %>% 
#   group_by(Target) %>%
#   mutate(avg_f = mean(Value),
#          sd = sd(Value)) %>% 
#   distinct()

# source("control_data.R")
# 
# new_control %>% 
#   mutate(se = sum_sd/60)

more_dat <- full_data %>% 
  # filter(Target != "INFA/INFB/RSVA") %>%
  group_by(Target, Cycle) %>%
  mutate(avg_point = mean(value),
         sd = sd(value),
         se = sd/sqrt(n())) %>% 
  ungroup()

# new dataset, filter 2 controls together to get combos

###### NOW OBSOLETE - SEE CONTROL_DATA.R FOR FUNCTION VERSION :D #######
infa_b <- more_dat %>% filter(Target %in% c("INFA", "INFB")) %>%
  group_by(Cycle, Target) %>%
  summarize(avg_point = mean(avg_point),
            se = mean(se)) %>%
  ungroup() %>%
  group_by(Cycle) %>%
  summarize(avg_point = sum(avg_point),
            se = sqrt(sum(se^2))) %>%
  mutate(Target = "INFA/INFB")

infa_rsva <- more_dat %>% filter(Target %in% c("INFA", "RSVA")) %>%
  group_by(Cycle, Target) %>%
  summarize(avg_point = mean(avg_point),
            se = mean(se)) %>%
  ungroup() %>%
  group_by(Cycle) %>%
  summarize(avg_point = sum(avg_point),
            se = sqrt(sum(se^2))) %>%
  mutate(Target = "INFA/RSVA")

infb_rsva <- more_dat %>% filter(Target %in% c("INFB", "RSVA")) %>%
  group_by(Cycle, Target) %>%
  summarize(avg_point = mean(avg_point),
            se = mean(se)) %>%
  ungroup() %>%
  group_by(Cycle) %>%
  summarize(avg_point = sum(avg_point),
            se = sqrt(sum(se^2))) %>%
  mutate(Target = "INFB/RSVA")

infa_infb_rsva <- more_dat %>% filter(Target %in% c("INFA", "INFB", "RSVA")) %>%
  group_by(Cycle, Target) %>%
  summarize(avg_point = mean(avg_point),
            se = mean(se)) %>%
  ungroup() %>%
  group_by(Cycle) %>%
  summarize(avg_point = sum(avg_point),
            se = sqrt(sum(se^2))) %>%
  mutate(Target = "INFA/INFB/RSVA")

combo_controls <- rbind(infa_b, infa_rsva, infb_rsva, infa_infb_rsva)
# could probably add the other single targets to this too, that way the for loop 
# only has to run once

##########################################################################################
####################### Exploratory Graphs ################################################
ggplot() +
  # geom_point(data = new_control, aes(x = 55, y = total)) +
  # geom_segment(data = new_control, aes(x = 55, xend = 55, y = total - sum_sd,
  #                                       yend = total + sum_sd, color = Target)) +
  geom_line(data = full_data, aes(x = Cycle, y = value, group = Well, 
                                  color = Well %in% positive$well)) +
  # geom_ribbon(data = more_dat, aes(x = Cycle, ymin = avg_point - 2*se,
  #                                  ymax = avg_point + 2*se, group = Target), alpha = .5) +
  # geom_line(data = more_dat, aes(x = Cycle, y = avg_point, group = Target), 
  #           color = "firebrick") +
  geom_ribbon(data = all_controls, aes(x = Cycle, ymin = avg_point - 2*se, 
                                        ymax = avg_point + 2*se, group = Target), 
              alpha = .5) +
  geom_line(data = all_controls, aes(x = Cycle, y = avg_point, group = Target),
            color = "firebrick") +
  theme_bw() +
  labs(title = "Standard Curve",
       x = "Cycle",
       y = "Fluorescence") +
  theme(plot.title = element_text(hjust = .5)) +
  # geom_label(data = all_controls %>% filter(Cycle == 60), aes(label = Target, x = 60, 
  #                                                               y = avg_point),hjust = 0) +
  geom_label(data = more_dat %>% filter(Cycle == 60), aes(label = Target, x = 60, 
                                                          y = avg_point), hjust = 0) +
  scale_x_continuous(limits = c(0, 75))
  
###########################################################################################
##################### Find the right test #################################################
# data to test if the unk doesn't match any of the controls - this doesn't work w/ KW b/c 
# it's testing the controls against each other and we know they're different. Even if you had 
# it just compare unk to all the controls, it wouldn't actually help b/c it would say that one
# was significant b/c it didn't match the others. Do I need to do them each individually? Ugh.
# A for loop might be handy here.

## For loop to check all targets ##
tar_names <- more_dat$Target %>% unique() 

for (tar in tar_names) {
  dat <- endpoint %>% filter(Target %in% c(tar, "INFA/INFB/RSVA"))
    x <- kruskal.test(value ~ Target, data = dat)
    
    print(tar)
    print(x)
     if (x$p.value > .05) {
       print(tar)
       print(x)
     }
}

# add controls wilcoxon test - signed rank - Take 1 cycle, 
# there's data for the individual controls - infa, infb, rsva, so we can do a
# kruskal-wallis test for those but we don't have anything to compare the 
# combos of the individuals. There's no data for that. We DO have the calculated 

combo_test <- all_controls %>% 
  filter(Cycle %in% 50:60) %>% 
  group_by(Target) %>% 
  mutate(avg = mean(avg_point)) # the 55 can be up to user when in function

## take span of each well, compare to the span of control
combo_tar_names <- all_controls$Target %>% unique()
sample <- full_data %>% filter(Target == "INFA/INFB/RSVA")
wells <- sample$Well %>% unique()

i = 1
pos_virus <- list()

for (well in wells) {
  for (tar in combo_tar_names) {
    dat <- full_data %>% filter(Cycle %in% 50:60)
    x <- t.test(dat$value[dat$Well == well], 
                     combo_test$avg_point[combo_test$Target == tar],  
                     mu = 0, alternative = "two.sided")
    
    # print(tar)
    # print(well)
    # print(x)
    
    if (x$p.value > .05/length(wells)) {
      x$well <- well
      x$virus <- tar
      pos_virus[[i]] <- x
      i = i + 1
    }
  }
}

positive <- do.call(rbind, pos_virus) %>% 
  as.data.frame %>% 
  select(well, virus, p.value) %>%
  mutate(p.value = round(as.numeric(p.value), 3))
############################################################
# Make my own test:) #######################################
############################################################
combo_test <- all_controls %>% 
  filter(Cycle %in% 50:60) %>% 
  group_by(Target) %>% 
  mutate(control_avg = mean(avg_point)) # the 55 can be up to user when in function

## take span of each well, compare to the span of control
combo_tar_names <- all_controls$Target %>% unique()
sample <- full_data %>% filter(Target == "INFA/INFB/RSVA")
wells <- sample$Well %>% unique()

i = 1
pos_virus <- list()


for (well in wells) {
  
  x1 <- full_data %>% 
    filter(Well == well & Cycle %in% 50:60) %>% 
    summarize(avg = mean(value))
    #dat <- full_data %>% filter(Cycle %in% 40:60)
    x2 <- combo_test %>% 
      group_by(Target) %>% 
      mutate(numerator  = x1$avg - control_avg,
             t_val = numerator/sqrt(mean(se^2)))
    
      x2$well <- well
      pos_virus[[i]] <- x2
      i = i + 1
  }

positive <- do.call(rbind, pos_virus) %>% 
  as.data.frame() %>% 
  select(well, Target, t_val) %>%
  unique() %>%
  ungroup() %>%
  mutate(p_val = round(2*pt(-abs(t_val), df = 10), 3))

####################################################
###### determine outliers ##########################
####################################################

ggplot(more_dat %>% filter(!Target %in% c("INFA/INFB/RSVA", "Blank")), 
       aes(x = Cycle, y = avg_point, group = Target)) +
  geom_line(color = "firebrick") +
  geom_ribbon(aes(x = Cycle, ymin = avg_point - se*2, ymax = avg_point + se*2, 
                  group = Target), alpha = .3) +
  geom_label(data = more_dat %>% filter(!Target %in% c("INFA/INFB/RSVA", "Blank") & Cycle == 60),
             aes(x = 60, y = avg_point, label = Target), hjust = 0) +
  scale_x_continuous(limits = c(0, 65)) +
  labs(title = "Standard Curve of Controls",
       y = "Fluorescence") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

ggplot(all_controls, 
       aes(x = Cycle, y = avg_point, group = Target)) +
  geom_line(color = "firebrick") +
  geom_ribbon(aes(x = Cycle, ymin = avg_point - se*2, ymax = avg_point + se*2, 
                  group = Target), alpha = .3) +
  geom_label(data = all_controls %>% filter(Cycle == 60),
             aes(x = 60, y = avg_point, label = Target), hjust = 0) +
  scale_x_continuous(limits = c(0, 75)) +
  labs(title = "Standard Curve of Controls and Combinations",
       y = "Fluorescence") +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5))

