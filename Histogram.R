# Load necessary libraries
library(ggplot2)

# Load the dataset
data <- read.csv("Food_Supply_Quantity_kg_Data 2.csv")

# Extract relevant columns for the analysis
vegetable_consumption <- data$Vegetables
covid_recovery_rates <- data$Recovered

# Remove non-finite values and ensure both columns have the same length
cleaned_data <- data.frame(Vegetables = vegetable_consumption, Recovered = covid_recovery_rates)
cleaned_data <- cleaned_data[complete.cases(cleaned_data), ]

# Create a histogram for vegetable consumption with a normal curve overlay
ggplot(cleaned_data, aes(x = Vegetables)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green", alpha = 0.6) +
  geom_density(color = "blue", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(cleaned_data$Vegetables, na.rm = TRUE), 
                                         sd = sd(cleaned_data$Vegetables, na.rm = TRUE)), 
                color = "red", linewidth = 1, linetype = "dashed") +
  labs(title = "Histogram of Vegetable Consumption with Normal Curve Overlay",
       x = "Vegetable Consumption (kg)",
       y = "Density") +
  theme_minimal()

# Save the plot
ggsave("vegetable_consumption_histogram.png")

# Create a histogram for COVID-19 recovery rates with a normal curve overlay
ggplot(cleaned_data, aes(x = Recovered)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.6) +
  geom_density(color = "green", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(cleaned_data$Recovered, na.rm = TRUE), 
                                         sd = sd(cleaned_data$Recovered, na.rm = TRUE)), 
                color = "red", linewidth = 1, linetype = "dashed") +
  labs(title = "Histogram of COVID-19 Recovery Rates with Normal Curve Overlay",
       x = "COVID-19 Recovery Rates",
       y = "Density") +
  theme_minimal()

# Save the plot
ggsave("covid_recovery_rates_histogram.png")
