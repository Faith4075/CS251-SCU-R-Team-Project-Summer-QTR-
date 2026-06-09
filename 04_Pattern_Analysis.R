## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 4: Crime Pattern Analysis
# Purpose: Analyze crime types, time of day patterns, victim demographics
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Load required libraries
library(dplyr) # Data manipulation
library(ggplot2) # Visualization
library(viridis) # Colorblind-friendly palettes
library(scales) # Format axis labels

# Set display options to show all columns
options(tibble.width = Inf)

# Load cleaned data
crime_data <- readRDS("data/crime_cleaned.rds") # Load from Script 1

cat(" **~~ Pattern Analysis Starting ~~** \n") # Section header

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4A ~ Most Common Crime Types (Top 20)
## tables/top_crime_types.csv
## top_crime_types.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Calculate frequency of each crime type
crime_type_counts <- crime_data |>
  group_by(crime_code_description) |> # Group by crime description
  summarise(
    count = n(),
    percentage = round(100 * n() / nrow(crime_data), 2) # Percent of total
  ) |>
  arrange(desc(count)) |> # Sort most frequent first
  mutate(rank = row_number()) # Add rank column

# Display top 10 crimes
cat("\n **~~ Top 10 Most Common Crimes in Los Angeles ~~** \n") # Header
print(head(crime_type_counts, 10)) # Show top 10

# Save full crime type analysis
write.csv(crime_type_counts, "outputs/tables/top_crime_types.csv",
  row.names = FALSE
)

# Create bar plot for top 15 crime types
crime_type_plot <- ggplot(
  head(crime_type_counts, 15),
  aes(
    x = reorder(crime_code_description, count),
    y = count, fill = percentage
  )
) +
  geom_bar(stat = "identity") + # Bar chart
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  # Add percent labels
  coord_flip() + # Flip for readability
  scale_fill_gradient(low = "lightblue", high = "darkred") + # Color gradient
  scale_y_continuous(labels = comma) + # Commas on numbers
  labs(
    title = "Most Frequent Crime Types in Los Angeles (2020-2024)",
    subtitle = "Top 15 crime categories by incident count",
    x = "Crime Description",
    y = "Number of Incidents",
    fill = "Percent of\nTotal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/top_crime_types.png", crime_type_plot,
  width = 12, height = 9, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4B ~ Time of Day Patterns
## time_of_day_patterns.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Analyze when crimes occur (using time categories created in Script 1)
time_patterns <- crime_data |>
  filter(!is.na(time_category), time_category != "Unknown") |>
  # Remove unknown times
  group_by(time_category) |> # Group by time period
  summarise(
    total_crimes = n(),
    percentage = round(100 * n() / nrow(crime_data), 2) # Percent of total
  ) |>
  # Order chronologically (not alphabetically)
  mutate(time_category = factor(time_category,
    levels = c(
      "Late Night (00:00-05:59)", "Morning (06:00-11:59)",
      "Afternoon (12:00-16:59)", "Evening (17:00-20:59)",
      "Late Night (21:00-23:59)"
    )
  ))

cat("\n **~~ Crime by Time of Day ~~** \n") # Header
print(time_patterns) # Display results

# Create time of day bar plot
time_plot <- ggplot(
  time_patterns,
  aes(
    x = time_category, y = total_crimes,
    fill = time_category
  )
) +
  geom_bar(stat = "identity") + # Bar chart
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  # Add percent labels
  scale_fill_viridis(discrete = TRUE) + # Colorblind-friendly colors
  scale_y_continuous(labels = comma) + # Commas on y-axis
  labs(
    title = "Crime by Time of Day: Los Angeles (2020-2024)",
    subtitle = "When do most crimes occur? (24-hour military time)",
    x = "Time Period",
    y = "Total Incidents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x labels
  )

ggsave("outputs/figures/time_of_day_patterns.png", time_plot,
  width = 10, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4C ~ Detailed Hourly Crime Patterns
## hourly_crime_patterns.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Analyze crime counts by individual hour (0-23)
hourly_patterns <- crime_data |>
  filter(!is.na(hour_of_day), hour_of_day >= 0, hour_of_day <= 23) |>
  # Valid hours
  group_by(hour_of_day) |>
  summarise(
    total_crimes = n(),
    percentage = round(100 * n() / nrow(crime_data), 2)
  )

# Create hourly line plot (shows peaks and valleys throughout day)
hourly_plot <- ggplot(hourly_patterns, aes(x = hour_of_day, y = total_crimes)) +
  geom_line(color = "steelblue", size = 1.5) + # Blue line
  geom_point(color = "darkred", size = 2) + # Red points
  geom_area(fill = "steelblue", alpha = 0.3) + # Filled area under curve
  scale_x_continuous(breaks = seq(0, 23, by = 2)) + # X-axis every 2 hours
  scale_y_continuous(labels = comma) + # Commas on y-axis
  labs(
    title = "Hourly Crime Patterns in Los Angeles",
    subtitle = "Crime frequency by hour of day (Military Time)",
    x = "Hour of Day (0 = Midnight, 12 = Noon, 23 = 11 PM)",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/hourly_crime_patterns.png", hourly_plot,
  width = 12, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4D ~ Victim Demographics Analysis
## victim_age_distribution.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Victim age distribution
age_summary <- crime_data |>
  filter(!is.na(victim_age_clean)) |> # Only valid ages
  summarise(
    mean_age = round(mean(victim_age_clean, na.rm = TRUE), 1),
    median_age = round(median(victim_age_clean, na.rm = TRUE), 1),
    min_age = min(victim_age_clean, na.rm = TRUE),
    max_age = max(victim_age_clean, na.rm = TRUE),
    q25_age = quantile(victim_age_clean, 0.25, na.rm = TRUE),
    # 25th percentile
    q75_age = quantile(victim_age_clean, 0.75, na.rm = TRUE)
    # 75th percentile
  )

cat("\n **~~ Victim Age Statistics ~~** \n") # Header
print(age_summary) # Display results

# Age histogram
age_histogram <- ggplot(
  crime_data |> filter(
    !is.na(victim_age_clean),
    victim_age_clean <= 100
  ),
  aes(x = victim_age_clean)
) +
  geom_histogram(
    binwidth = 5, fill = "steelblue",
    color = "black", alpha = 0.7
  ) + # 5-year bins
  geom_vline(aes(xintercept = mean_age),
    data = age_summary,
    color = "red", linetype = "dashed", size = 1
  ) + # Mean line
  labs(
    title = "Age Distribution of Crime Victims in Los Angeles",
    subtitle = paste(
      "Mean age:", age_summary$mean_age,
      "| Median age:", age_summary$median_age
    ),
    x = "Victim Age (Years)",
    y = "Number of Victims"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/victim_age_distribution.png", age_histogram,
  width = 10, height = 6, dpi = 300
)

# Age distribution by age group (created in Script 1)
age_group_dist <- crime_data |>
  group_by(age_group) |>
  summarise(
    count = n(),
    percentage = round(100 * n() / nrow(crime_data), 1)
  ) |>
  arrange(desc(count))

cat("\n **~~ Victim Age Groups ~~** \n") # Header
print(age_group_dist) # Display results

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4E ~ Victim Sex Distribution
## victim_sex_distribution.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

sex_distribution <- crime_data |>
  group_by(victim_sex_clean) |>
  summarise(
    count = n(),
    percentage = round(100 * n() / nrow(crime_data), 1)
  ) |>
  arrange(desc(count))

cat("\n **~~ Victim Sex Distribution ~~** \n") # Header
print(sex_distribution) # Display results

# Create pie chart for sex distribution
sex_pie <- ggplot(
  sex_distribution,
  aes(x = "", y = percentage, fill = victim_sex_clean)
) +
  geom_bar(stat = "identity", width = 1) + # Bars for pie
  coord_polar("y", start = 0) + # Convert to pie chart
  geom_text(aes(label = paste0(victim_sex_clean, "\n", percentage, "%")),
    position = position_stack(vjust = 0.5)
  ) + # Labels inside slices
  scale_fill_viridis_d() + # Color palette
  labs(
    title = "Victim Sex Distribution (2020-2024)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(), # Remove axis titles
    axis.text = element_blank(), # Remove axis text
    panel.grid = element_blank(), # Remove grid lines
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("outputs/figures/victim_sex_distribution.png", sex_pie,
  width = 8, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4F ~ Weapon Usage Analysis
## weapon_analysis.csv
## top_weapons.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Analyze weapons used in crimes
weapon_summary <- crime_data |>
  filter(
    !is.na(weapon_description), weapon_description != "",
    weapon_description != "Unknown"
  ) |>
  # Known weapons
  group_by(weapon_description) |>
  summarise(
    total_incidents = n(),
    percentage = round(100 * n() / nrow(crime_data), 2)
  ) |>
  arrange(desc(total_incidents)) |>
  head(15) # Top 15 weapons

cat("\n **~~ Top 10 Weapons Used In Crimes ~~** \n") # Header
print(head(weapon_summary, 10)) # Display top 10

# Save weapon analysis
write.csv(weapon_summary, "outputs/tables/weapon_analysis.csv",
  row.names = FALSE
)

# Create weapon plot
weapon_plot <- ggplot(
  head(weapon_summary, 10),
  aes(
    x = reorder(weapon_description, total_incidents),
    y = total_incidents, fill = percentage
  )
) +
  geom_bar(stat = "identity") + # Bar chart
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  # Add percent labels
  coord_flip() + # Horizontal bars
  scale_fill_gradient(low = "orange", high = "darkred") + # Color gradient
  scale_y_continuous(labels = comma) + # Commas on numbers
  labs(
    title = "Most Common Weapons Used in Los Angeles Crimes",
    subtitle = "Weapon types associated with criminal incidents",
    x = "Weapon Type",
    y = "Number of Incidents",
    fill = "Percent of\nTotal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/top_weapons.png", weapon_plot,
  width = 12, height = 7, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4G ~ Cross-Tabulation: Crime Type x Time of Day
## crime_time_heatmap.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Create heatmap of top 5 crimes by time of day
top_5_crimes <- crime_type_counts$crime_code_description[1:5] # Get top 5 crimes

crime_time_heatmap_data <- crime_data |>
  filter(
    crime_code_description %in% top_5_crimes,
    !is.na(time_category),
    time_category != "Unknown"
  ) |>
  group_by(crime_code_description, time_category) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(crime_code_description) |>
  mutate(percentage_of_crime = round(100 * count / sum(count), 1))
# Percent within each crime type

# Create heatmap
heatmap_plot <- ggplot(
  crime_time_heatmap_data,
  aes(
    x = time_category, y = crime_code_description,
    fill = percentage_of_crime
  )
) +
  geom_tile() + # Tile plot (heatmap)
  geom_text(aes(label = paste0(percentage_of_crime, "%")), size = 3) +
  # Add text labels
  scale_fill_gradient(low = "white", high = "steelblue") + # Color gradient
  labs(
    title = "When Different Crimes Occur: Time of Day Patterns",
    subtitle = "Percentage distribution for top 5 crime types",
    x = "Time Period",
    y = "Crime Type",
    fill = "Percent of\nCrime Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/crime_time_heatmap.png", heatmap_plot,
  width = 10, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 4H ~ Completion Message
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

cat("\n **~~ Pattern Analysis Complete ~~** \n") # Completion message
cat("Generated files:\n") # List outputs
cat("  - outputs/tables/top_crime_types.csv\n")
cat("  - outputs/tables/weapon_analysis.csv\n")
cat("  - outputs/figures/top_crime_types.png\n")
cat("  - outputs/figures/time_of_day_patterns.png\n")
cat("  - outputs/figures/hourly_crime_patterns.png\n")
cat("  - outputs/figures/victim_age_distribution.png\n")
cat("  - outputs/figures/victim_sex_distribution.png\n")
cat("  - outputs/figures/top_weapons.png\n")
cat("  - outputs/figures/crime_time_heatmap.png\n")
