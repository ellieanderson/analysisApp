library(tidyverse)
library(mosaic)
library(readxl)

results <- read_csv("qPCR data.csv")
plate <- read_csv("qpcr_plate1.csv")

# sample (from person we're testing) should be a drop down so they can choose
# results will be fluorescence file - already read in
# plate will be template file - already read in
# might not need both separately - they might already be combined in the app depending on 
# where this part is being added.

#test_function <- function(results, plate, sample){
f <- read_csv("qPCR data.csv") %>% pivot_longer(cols = 2:97, names_to = "Well", 
                                                   values_to = "Value")
p <- read_excel("qPRC plate.xlsx", sheet = "Data")

full_data <- f %>% left_join(p, by = c("Well" = "Sample")) %>% 
  mutate(Target = case_when(Target == "INFA/INFBRSVA" ~ "INFA/INFB/RSVA",
                            Target == "INFA/NFB/RSVA" ~ "INFA/INFB/RSVA",
                            TRUE ~ Target)) %>% 
  filter(!Well %in% c("B3", "A8"))

test <- full_data %>% 
  mutate(control = ifelse(Target %in% c("Blank", "INFA", "INFB", "RSVA"), Target, "Unknown")) %>%
  select(-"Target Concentration")

# filter for the sample you want and blank (neg control) to see if there's anything significant 
# that doesn't include the controls
endpoint <- full_data %>% 
  filter(Cycle > 49) %>% 
  group_by(Target) %>%
  mutate(avg_f = mean(Value),
         sd = sd(Value)) %>% distinct()

# possible_combos <- 
endpoint %>% 
  filter(Target %in% c("INFA", "INFB", "RSVA")) %>% 
  mutate(combo = case_when(Target == "INFA" ~ avg_f,
                           Target %in% c("INFA", "INFB") ~ sum(avg_f)/n()))

kruskal.test(Value ~ as.factor(Target), data = endpoint)
#}

#xyplot(Value~sample, data = endpoint)

ggplot(endpoint, aes(x = Target, y = avg_f)) +
  geom_jitter(alpha = .5) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point(data = endpoint %>% 
               group_by(Target) %>% 
               summarize(avg = median(Value)), 
             aes(x = Target, y = avg), color = "firebrick") +
  geom_segment(data = endpoint %>% group_by(Target) %>% summarize(avg = median(Value), sd = sd(Value)), 
            aes(x = Target, y = avg + sd, xend = Target, yend = avg - sd), 
            color = "firebrick",
            size = 1,
            lty = 1)

ggplot(full_data, aes(x = Cycle, y = Value, color = `Target Concentration`, group = Well)) +
  geom_line() +
  geom_point(data = endpoint, aes(x = 55, y = avg_f), size = 3) +
  geom_segment(data = endpoint,  aes(x = 55, xend = 55, y = avg_f - sd, yend= avg_f + sd), 
               alpha = .5)
  
# TukeyHSD(end.aov)  - I won't use this one
pairwise.t.test(endpoint$Value, endpoint$Target, "bonferroni")

par(mfrow = c(1,2))
plot(end.aov, which = c(1,2))
