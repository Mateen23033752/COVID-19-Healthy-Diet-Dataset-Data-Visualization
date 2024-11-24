

library(ggplot2)
library(dplyr)


dataset <- read.csv("dataset.csv")


summary_data <- dataset %>%
  group_by(Country) %>%
  summarise(
    Avg_Vegetables = mean(Vegetables, na.rm = TRUE),
    Avg_Recovered = mean(Recovered, na.rm = TRUE)
  )

max_veg <- max(summary_data$Avg_Vegetables, na.rm = TRUE)
max_recov <- max(summary_data$Avg_Recovered, na.rm = TRUE)

summary_data <- summary_data %>%
  mutate(
    Scaled_Recovered = Avg_Recovered * max_veg / max_recov  # Scaling recovery rate
  )

ggplot(summary_data, aes(x = Country)) +
  # Bar chart for vegetable consumption
  geom_bar(aes(y = Avg_Vegetables), stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
  
  # Red line for vegetable consumption
  geom_line(aes(y = Avg_Vegetables), color = "red", size = 1) +
  
  # Blue line for recovery rates, scaled to fit the vegetable axis
  geom_line(aes(y = Scaled_Recovered), color = "blue", size = 1) +
  
  # Red dots for vegetable consumption
  geom_point(aes(y = Avg_Vegetables), color = "red", size = 3) +
  
  # Blue dots for recovery rates
  geom_point(aes(y = Scaled_Recovered), color = "blue", size = 3) +
  
  # Scale y-axis for vegetable consumption
  scale_y_continuous(
    name = "Average Vegetable Consumption (per capita)",
    sec.axis = sec_axis(~ . * max_recov / max_veg, name = "Average Recovery Rate (%)")
  ) +
  
  labs(
    title = "Comparison of Vegetable Consumption and Recovery Rates by Country",
    x = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "skyblue"),
    axis.title.y.right = element_text(color = "blue")
  )
