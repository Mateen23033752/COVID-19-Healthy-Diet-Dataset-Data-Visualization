
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