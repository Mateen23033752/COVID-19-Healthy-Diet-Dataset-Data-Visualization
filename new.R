# Load required libraries
library(dplyr)
library(ggplot2)
library(readr)

# Define file path (update the path if running locally)
file_path <- "Food_Supply_Quantity_kg_Data.csv"  # Adjust path as needed

# Load the dataset
data <- read.csv(file_path)

# Step 1: Explore the dataset and inspect column names
print("Column Names:")
print(colnames(data))

# Step 2: Select relevant columns based on dataset
# Correct columns: 'Vegetables' for vegetable consumption, 'Recovered' for recovery rate
selected_data <- data %>%
  select(Country, Vegetables, Recovered)

# Step 3: Check for missing values
print("Summary of Selected Data:")
summary(selected_data)

# Handle missing data (if any rows have NA, remove them)
clean_data <- selected_data %>%
  filter(!is.na(Vegetables) & !is.na(Recovered))

# Step 4: Compute Correlation
correlation <- cor(clean_data$Vegetables, clean_data$Recovered, use = "complete.obs")
print(paste("Correlation between Vegetables and Recovered:", correlation))

# Step 5: Create Scatterplot with regression line
scatter_plot <- ggplot(clean_data, aes(x = Vegetables, y = Recovered)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add regression line
  labs(title = "Vegetable Consumption vs. COVID-19 Recovery Rate",
       x = "Vegetable Consumption (kg per capita)",
       y = "COVID-19 Recovery Rate (Recovered Cases)") +
  theme_minimal()

# Open a new graphics window to display the plot (use dev.new() in case of issues with displaying)
dev.new()  # Open new plot window
print(scatter_plot)  # Display the scatter plot
ggsave("scatter_plot.png", scatter_plot)  # Save plot to file

# Step 6: Hypotheses
# Null Hypothesis (H₀): There is no correlation between vegetable consumption and COVID-19 recovery rates.
# Alternative Hypothesis (H₁): There is a correlation between vegetable consumption and COVID-19 recovery rates.

# Step 7: Output Findings
print("Dataset Snippet (First 5 Rows):")
print(head(clean_data))

# Notes:
# - Include 'scatter_plot.png' in your presentation.
# - Discuss the significance of the correlation value.

# End of Script
