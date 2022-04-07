full_data <- read_csv("analysisApp/TitrationLevels.csv")
template <- read_csv("analysisApp/TemplateExample.csv")

colnames(full_data)[1] <- "Cycle"
full_data$name <- factor(full_data$name, levels = unique(full_data$name))
full_data <- full_data %>% 
  pivot_longer(-Cycle)

full_data <- left_join(full_data, template, by = c("name" = "Sample"))

some <-full_data %>% 
  filter(`Target Concentration` %in% c("10^0", "10^6", "10^6/10^6/10^1", 
                                       "10^6/10^1/10^6", "10^1/10^6/10^6"))

ggplotly(
  ggplot(data = some) +
    geom_line(aes(x = Cycle, y = Value, color = Target, group = Well)) +
    theme_minimal() +
    ylab("")
) %>% 
  config(displayModeBar = F) 

