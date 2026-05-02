## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Clausen, Mohammad Baher and Terrance Carpenter
## Team Project || Team One
## 05-02-2026

library(dplyr)      # Data manipulation
library(lubridate)  # Date handling
library(ggplot2)    # Visualization
library(scales)     # Format axes

# Load cleaned data
crime_data <- readRDS("data/crime_cleaned.rds")  # Load saved data from script 01

# ================ WARNING MESSAGE ================
if(!"date_occurred" %in% names(crime_data)) {
  warning("No date column found! Please add 'date_occurred' column from the complete dataset.")
  warning("Temporal analysis cannot proceed without date information.")
  stop("Missing required date column. Download full dataset from Data.Gov link in abstract.")
}
# =================================================

# Convert date column if it exists
crime_data <- crime_data %>%
  mutate(
    date_occurred = as.Date(date_occurred),  # Convert to Date format
    year = year(date_occurred),              # Extract year (2020-2024)
    month = month(date_occurred),            # Extract month number (1-12)
    week = week(date_occurred),              # Extract week number (1-52)
    day_of_week = wday(date_occurred, label = TRUE, abbr = FALSE),  # Full day name
    quarter = quarter(date_occurred)         # Extract quarter (Q1-Q4)
  )

# Filter to study period (2020-2024 as specified in abstract assignment turn in)
crime_period <- crime_data %>%
  filter(year >= 2020, year <= 2024)  # Focus on 5-year analysis window

# 1. Monthly crime trends by year
monthly_trends <- crime_period %>%
  group_by(year, month) %>%                      # Group by year and month
  summarise(total_crimes = n(), .groups = "drop") %>%  # Count incidents per month
  mutate(date = make_date(year, month, 1))       # Create date for plotting

# Plot monthly trends
monthly_plot <- ggplot(monthly_trends, aes(x = date, y = total_crimes)) +
  geom_line(color = "steelblue", size = 1) +      # Blue trend line
  geom_point(color = "darkred", size = 1.5) +     # Red points on line
  labs(
    title = "Monthly Crime Trends in Los Angeles (2020-2024)",
    subtitle = "Five-year analysis period including COVID-19 pandemic",
    x = "Date",                                    # X-axis label
    y = "Total Crimes Reported"                    # Y-axis label
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +  # Format dates
  scale_y_continuous(labels = comma) +             # Add commas to large numbers
  theme_minimal() +                                # Clean background
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center title
    plot.subtitle = element_text(hjust = 0.5)         # Center subtitle
  )

# Save plot
ggsave("outputs/figures/monthly_trends.png", monthly_plot, width = 12, height = 6, dpi = 300)

# 2. COVID-19 period comparison (pre, during, post)
crime_period <- crime_period %>%
  mutate(
    covid_period = case_when(
      date_occurred < as.Date("2020-03-15") ~ "Pre-COVID (Jan-Feb 2020)",      # Before pandemic
      date_occurred < as.Date("2021-03-15") ~ "Peak COVID (Mar 2020-Mar 2021)", # First year
      date_occurred < as.Date("2023-05-05") ~ "Mid-COVID (Apr 2021-May 2023)",  # End of emergency
      TRUE ~ "Post-COVID (June 2023-Dec 2024)"                                   # After emergency
    )
  )

# Compare crime counts by COVID period
covid_comparison <- crime_period %>%
  group_by(covid_period) %>%
  summarise(
    total_crimes = n(),                              # Total incidents
    avg_daily_crimes = n() / n_distinct(date_occurred)  # Average per day
  ) %>%
  arrange(desc(covid_period))  # Order chronologically

# Print comparison table
print("===== CRIME TRENDS BY COVID PERIOD =====")  # Header
print(covid_comparison)  # Display table

# Save comparison table
write.csv(covid_comparison, "outputs/tables/covid_period_comparison.csv", row.names = FALSE)

# 3. Day of week patterns (which days have most crime?)
dow_patterns <- crime_period %>%
  group_by(day_of_week) %>%
  summarise(total_crimes = n()) %>%
  arrange(desc(total_crimes))  # Sort from most to least crime

# Day of week plot
dow_plot <- ggplot(dow_patterns, aes(x = reorder(day_of_week, -total_crimes), y = total_crimes)) +
  geom_bar(stat = "identity", fill = "coral2") +      # Coral-colored bars
  labs(
    title = "Crime by Day of Week: Los Angeles (2020-2024)",
    x = "Day of Week",                                 # X-axis label
    y = "Total Incidents"                              # Y-axis label
  ) +
  scale_y_continuous(labels = comma) +                 # Format y-axis numbers
  theme_minimal() +                                    # Clean theme
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Center title

ggsave("outputs/figures/day_of_week_patterns.png", dow_plot, width = 10, height = 6, dpi = 300)

print("===== TEMPORAL ANALYSIS COMPLETE =====")  # Completion message
