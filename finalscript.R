
library(ggplot2)
library(dplyr)

dataset <- read.csv("dataset.csv")

summary_data <- dataset %>%
  group_by(Country) %>%
  summarise(
    Avg_Vegetables = mean(Vegetables),
    Avg_Recovered = mean(Recovered)
  )


ggplot(summary_data, aes(x = Country, y = Avg_Vegetables, group = 1)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_line(aes(y = Avg_Vegetables), color = "red", size = 1) +
  geom_point(aes(y = Avg_Vegetables), color = "red", size = 3) +
  labs(
    title = "Average Vegetable Consumption by Country",
    x = "Country",
    y = "Average Vegetable Consumption (per capita)"
  ) +
  theme_minimal()


ggplot(summary_data, aes(x = Country, y = Avg_Recovered, group = 1)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_line(aes(y = Avg_Recovered), color = "blue", size = 1) +
  geom_point(aes(y = Avg_Recovered), color = "blue", size = 3) +
  labs(
    title = "Average COVID-19 Recovery Rates by Country",
    x = "Country",
    y = "Average Recovery Rate (%)"
  ) +
  theme_minimal()


max_veg <- max(summary_data$Avg_Vegetables, na.rm = TRUE)
max_recov <- max(summary_data$Avg_Recovered, na.rm = TRUE)

summary_data <- summary_data %>%
  mutate(
    Scaled_Recovered = Avg_Recovered * max_veg / max_recov  
  )

ggplot(summary_data, aes(x = Country)) +
  
  geom_bar(aes(y = Avg_Vegetables), stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
  
  geom_line(aes(y = Avg_Vegetables), color = "red", size = 1) +
  geom_line(aes(y = Scaled_Recovered), color = "blue", size = 1) +
  
  geom_point(aes(y = Avg_Vegetables), color = "red", size = 3) +
  
  geom_point(aes(y = Scaled_Recovered), color = "blue", size = 3) +
  
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
