f <- read_csv("qPCR data.csv") %>%
  pivot_longer(cols = 2:97, names_to = "Well", values_to = "Value")

p <- read_excel("qPRC plate copy.xlsx", sheet = "Data")

full_data <- f %>% left_join(p, by = c("Well" = "Sample")) %>% 
  mutate(Target = case_when(Target == "INFA/INFBRSVA" ~ "INFA/INFB/RSVA",
                            Target == "INFA/NFB/RSVA" ~ "INFA/INFB/RSVA",
                            TRUE ~ Target))


ggplot() +
  geom_line(data = full_data, aes(x = Cycle, y = Value, color = Target, group = Well)) +
  geom_ribbon(data = more_dat, aes(x = Cycle, ymin = avg_point - 2*se,
                                   ymax = avg_point + 2*se, group = Target), alpha = .5) +
  geom_line(data = more_dat, aes(x = Cycle, y = avg_point, group = Target), 
            color = "firebrick") +
  geom_ribbon(data = combo_controls, aes(x = Cycle, ymin = avg_point - 2*se, 
                                         ymax = avg_point + 2*se, group = Target), 
              alpha = .5) +
  geom_line(data = combo_controls, aes(x = Cycle, y = avg_point, group = Target),
            color = "firebrick")

#ep1 <- full_data %>% filter(Cycle > 49) 
