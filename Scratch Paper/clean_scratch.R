library(tidyverse)
library(mosaic)
library(readxl)
library(magrittr)
library(stats)
############ Data Wrangling #################################################
# Load data
f <- read_csv("qPCR data.csv") %>%
  pivot_longer(cols = 2:97, names_to = "Well") #, values_to = "Value"

p <- read_excel("qPRC plate.xlsx", sheet = "Data")

targets <- c("INFA", "INFB", "RSVA")

full_data <- f %>% left_join(p, by = c("Well" = "Sample")) %>% 
  mutate(Target = case_when(Target == "INFA/INFBRSVA" ~ "INFA/INFB/RSVA",
                            Target == "INFA/NFB/RSVA" ~ "INFA/INFB/RSVA",
                            TRUE ~ Target)) 

# Find outliers
find_outliers <- full_data %>% 
  filter(Cycle %in% 45:60) %>%
  group_by(Target,Well) %>%
  mutate(avg = mean(value)) %>%
  ungroup() %>%
  group_by(Target) %>%
  mutate(Q1 = quantile(value, .25),
         Q3 = quantile(value, .75),
         IQR_x = IQR(value),
         mini = Q1 - 1.5*IQR_x,
         maxi = Q3 + 1.5*IQR_x,
         outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                             TRUE ~ TRUE)) %>%
  select(Well, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
  unique() %>%
  filter(outlier & Target %in% targets)

#Get rid of outliers
full_data %<>% filter(!Well %in% find_outliers$Well)

find_outliers <- full_data %>% 
  filter(Cycle %in% 45:60) %>%
  group_by(Target,Well) %>%
  mutate(avg = mean(value)) %>%
  ungroup() %>%
  group_by(Target) %>%
  mutate(Q1 = quantile(value, .25),
         Q3 = quantile(value, .75),
         IQR_x = IQR(value),
         mini = Q1 - 1.5*IQR_x,
         maxi = Q3 + 1.5*IQR_x,
         outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                             TRUE ~ TRUE)) %>%
  select(Well, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
  unique() %>%
  filter(outlier & Target %in% targets)

#Get rid of outliers
full_data %<>% filter(!Well %in% find_outliers$Well)

# get sd and se for every target and every cycle
more_dat <- full_data %>%
  group_by(Target, Cycle) %>%
  mutate(avg_point = mean(value),
         sd = sd(value),
         se = sd/sqrt(n())) %>%
  ungroup()

# create controls dataframe - uses just the targets and finds avg, sd, and var of the last few cycles
controls <- full_data %>% 
  filter(Cycle > 49) %>% 
  group_by(Target) %>%
  summarize(avg = mean(value), 
            sd = sd(value), 
            var = var(value)) %>% 
  filter(Target %in% c("INFA", "INFB", "RSVA"))

# get all combinations
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

# find wells that test "positive"

combo_test <- all_controls %>% 
  filter(Cycle %in% 50:60) %>% 
  group_by(Target, Cycle) %>% 
  mutate(control_avg = mean(avg_point)) # the 50:60 can be up to user when in app

## take span of each well, compare to the span of control
combo_tar_names <- all_controls$Target %>% unique()
sample <- full_data %>% filter(Target == "INFA/INFB/RSVA")
wells <- sample$Well %>% unique()

i = 1
pos_virus <- list()
pos_well <- list()

for (well in wells) {
  for (combo in combo_tar_names) {
  
  x1 <- full_data %>% 
    filter(Well == well & Cycle %in% 50:60)
  
  x2 <- combo_test %>% 
    filter(Target == combo)
  
  pos <- mean(abs(x1$value - x2$avg_point) < x2$se*2)
  x3 <- if (pos > .7) well
  x4 <- if (pos > .7) combo
  
  pos_well[[i]] <- x3
  pos_virus[[i]] <- x4
  i = i + 1
  }
}

if (length(pos_virus) > 0){
  y <- do.call(rbind, pos_well) %>% as.data.frame()
  
  positive <- do.call(rbind, pos_virus) %>% 
    as.data.frame() %>% cbind("well" = y)
  
  #names(positive) <- c("Positive", "Well")
}



# visualize it

ggplot() +
  geom_line(data = sample, aes(x = Cycle, y = value, group = Well, 
                                  color = Well %in% positive$V1)) +
  geom_ribbon(data = all_controls, aes(x = Cycle, ymin = avg_point - 2*se, 
                                       ymax = avg_point + 2*se, group = Target), 
              alpha = .5) +
  geom_line(data = all_controls, aes(x = Cycle, y = avg_point, group = Target),
            color = "firebrick") +
  theme_bw() +
  labs(title = "Standard Curve",
       x = "Cycle",
       y = "Fluorescence",
       color = "Positive") +
  theme(plot.title = element_text(hjust = .5)) +
  geom_label(data = all_controls %>% filter(Cycle == 60), aes(label = Target, x = 60, 
                                                              y = avg_point),hjust = 0) +
  scale_x_continuous(limits = c(0, 75))

# inf_b <- full_data %>% filter(Target == "INFB")
# inf_b %>% 
#   filter(Cycle %in% 45:60) %>%
#   group_by(Target, Well) %>%
#   summarize(avg = mean(value)) %>%
#   ungroup() %>%
#   group_by(Target) %>%
#   mutate(Q1 = quantile(avg, .25),
#          Q3 = quantile(avg, .75),
#          IQR_x = IQR(avg),
#          mini = Q1 - .75*IQR_x,
#          maxi = Q3 + .75*IQR_x,
#          outlier = case_when(avg > mini & avg < maxi ~FALSE, 
#                              TRUE ~ TRUE)) %>% view()

