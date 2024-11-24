
library(ggplot2)
library(dplyr)

dataset <- read.csv("dataset.csv")


summary_data <- dataset %>%
  group_by(Country) %>%
  summarise(
    Avg_Vegetables = mean(Vegetables),
    Avg_Recovered = mean(Recovered)
  )

ggplot(summary_data, aes(x = Country, y = Avg_Recovered, group = 1)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_line(aes(y = Avg_Recovered), color = "blue", size = 1) +
  geom_point(aes(y = Avg_Recovered), color = "blue", size = 3) +
   theme_minimal()
