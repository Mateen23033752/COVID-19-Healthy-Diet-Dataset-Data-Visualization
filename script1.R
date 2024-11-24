library(ggplot2)
library(dplyr)

dataset <- read.csv("dataset.csv")


summary_data <- dataset %>%
  group_by(Country) %>%
  summarise(
    Avg_Vegetables = mean(Vegetables),
    Avg_Recovered = mean(Recovered)
  )