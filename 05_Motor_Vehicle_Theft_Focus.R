## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 5: Motor Vehicle Theft Focused Analysis
# Purpose: Deep dive into motor vehicle theft patterns
# This crime type is highlighted in the abstract
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Load required libraries
library(dplyr) # Data manipulation
library(ggplot2) # Visualization
library(scales) # Format axis labels
library(lubridate) # For make_date() function

# Set display options to show all columns
options(tibble.width = Inf)

# Load cleaned data
crime_data <- readRDS("data/crime_cleaned.rds") # Load from Script 1

cat(" **~~ Motor Vehicle Theft Analysis Starting ... ~~** \n") # Section header

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5A ~ Identify Motor Vehicle Theft Incidents
# mvt_breakdown.csv
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Search for vehicle theft-related crime descriptions
# Note: Based on the source Excel data, both "VEHICLE - STOLEN"
# and "THEFT FROM MOTOR VEHICLE" appear on the file.
# This section will scan for both keywords.
mvt_keywords <- c(
  "VEHICLE - STOLEN", "THEFT FROM MOTOR VEHICLE",
  "VEHICLE - ATTEMPT STOLEN"
)

mvt_data <- crime_data |>
  filter(grepl(
    paste(mvt_keywords, collapse = "|"),
    crime_code_description
  )) |>
  # Pattern matching
  mutate(
    mvt_type = case_when(
      grepl("STOLEN", crime_code_description) &
        !grepl("FROM", crime_code_description) ~
        "Vehicle Theft (Complete)",
      grepl("ATTEMPT", crime_code_description) ~
        "Vehicle Theft (Attempted)",
      grepl("THEFT FROM", crime_code_description) ~
        "Theft from Vehicle",
      TRUE ~ "Other Vehicle Crime"
    )
  )

# Alternative route: Filter by crime code if available
# Common LA crime codes: 510 = Vehicle Stolen
#  520 = Attempt Stolen,
#  330/420 = Theft from vehicle
if ("crime_code" %in% names(crime_data)) {
  mvt_by_code <- crime_data |>
    filter(crime_code %in% c(510, 520, 330, 420)) |> # Vehicle theft codes
    mutate(mvt_type = case_when(
      crime_code == 510 ~ "Vehicle Theft (Complete)",
      crime_code == 520 ~ "Vehicle Theft (Attempted)",
      crime_code %in% c(330, 420) ~ "Theft from Vehicle",
      TRUE ~ "Other"
    ))
  # Use the larger dataset (by description or by code)
  if (nrow(mvt_by_code) > nrow(mvt_data)) {
    mvt_data <- mvt_by_code
    cat("Using crime code filtering (", nrow(mvt_data), " incidents)\n")
  } else {
    cat("Using keyword filtering (", nrow(mvt_data), " incidents)\n")
  }
}

cat("\n **~~ Motor Vehicle Theft Overview ~~** \n") # Header
cat("Total motor vehicle-related incidents:", nrow(mvt_data), "\n") # Count
cat(
  "Percentage of all crimes:",
  round(100 * nrow(mvt_data) / nrow(crime_data), 2), "%\n"
) # Percent

# Breakdown by type
mvt_breakdown <- mvt_data |>
  group_by(mvt_type) |>
  summarise(count = n(), percentage = round(100 * n() / nrow(mvt_data), 1))

print(mvt_breakdown)

# Save MVT breakdown
write.csv(mvt_breakdown, "outputs/tables/mvt_breakdown.csv", row.names = FALSE)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5B ~ Motor Vehicle Theft Hotspots by Area
## mvt_by_area.csv
## mvt_by_area.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Calculate area totals separately for efficiency
area_totals <- crime_data |>
  group_by(area_name_of_crime) |>
  summarise(area_total_crimes = n(), .groups = "drop")

mvt_by_area <- mvt_data |>
  group_by(area_name_of_crime) |>
  summarise(mvt_count = n(), .groups = "drop") |>
  left_join(area_totals, by = "area_name_of_crime") |>
  mutate(
    mvt_percentage_of_area = round(100 * mvt_count / area_total_crimes, 2)
  ) |>
  arrange(desc(mvt_count)) |>
  head(15) # Top 15 areas

cat("\n **~~ Top 10 Areas for Motor Vehicle-Related Crimes ~~** \n") # Header
print(head(mvt_by_area, 10)) # Display top 10

# Save MVT by area
write.csv(mvt_by_area, "outputs/tables/mvt_by_area.csv", row.names = FALSE)

# Create MVT area plot
mvt_area_plot <- ggplot(
  mvt_by_area,
  aes(
    x = reorder(area_name_of_crime, mvt_count),
    y = mvt_count, fill = mvt_percentage_of_area
  )
) +
  geom_bar(stat = "identity") + # Bar chart
  geom_text(aes(label = paste0(mvt_count, "\n(", mvt_percentage_of_area, "%)")),
    hjust = -0.1, size = 3
  ) + # Labels with percent
  coord_flip() + # Horizontal bars
  scale_fill_gradient(low = "yellow", high = "red") + # Heat colors
  scale_y_continuous(labels = comma) + # Commas on numbers
  labs(
    title = "Motor Vehicle Crime Hotspots in Los Angeles",
    subtitle = "Top 15 areas for vehicle theft and theft from vehicles",
    x = "Police Area/District",
    y = "Number of Vehicle-Related Incidents",
    fill = "% of Area\nCrimes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/mvt_by_area.png", mvt_area_plot,
  width = 12, height = 8, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5C ~ Temporal Patterns of Vehicle Theft
## mvt_monthly_trends.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Monthly trends for MVT
mvt_monthly <- mvt_data |>
  group_by(occurrence_year, occurrence_month) |>
  summarise(count = n(), .groups = "drop") |>
  mutate(date = make_date(occurrence_year, occurrence_month, 1))

# Create MVT monthly trend line
mvt_monthly_plot <- ggplot(mvt_monthly, aes(x = date, y = count)) +
  geom_line(color = "darkred", size = 1.5) + # Red trend line
  geom_point(color = "black", size = 2) + # Black points
  geom_smooth(
    method = "loess", se = TRUE, color = "gray50",
    linetype = "dashed"
  ) + # Smooth trend
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  # Format x-axis
  scale_y_continuous(labels = comma) + # Commas on y-axis
  labs(
    title = "Monthly Motor Vehicle Crime Trends (2020-2024)",
    subtitle = "Vehicle theft and theft from vehicles over time",
    x = "Date",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("outputs/figures/mvt_monthly_trends.png", mvt_monthly_plot,
  width = 12, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5D ~ Time of Day for Motor Vehicle Theft
## mvt_time_patterns.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

mvt_time <- mvt_data |>
  filter(!is.na(time_category), time_category != "Unknown") |>
  group_by(time_category) |>
  summarise(
    theft_count = n(),
    percentage = round(100 * n() / nrow(mvt_data), 1)
  ) |>
  mutate(time_category = factor(time_category,
    levels = c(
      "Late Night (00:00-05:59)", "Morning (06:00-11:59)",
      "Afternoon (12:00-16:59)", "Evening (17:00-20:59)",
      "Late Night (21:00-23:59)"
    )
  ))

cat("\n **~~ Vehicle Crime by Time of Day ~~** \n") # Header
print(mvt_time) # Display results

# Create time of day plot for MVT
mvt_time_plot <- ggplot(
  mvt_time,
  aes(
    x = time_category, y = theft_count,
    fill = time_category
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "When Do Vehicle Crimes Occur?",
    subtitle = "Time of day patterns for motor vehicle-related incidents",
    x = "Time Period",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("outputs/figures/mvt_time_patterns.png", mvt_time_plot,
  width = 10, height = 6, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5E ~ Location Types for Vehicle Theft
## mvt_by_location.csv
## mvt_locations.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

mvt_locations <- mvt_data |>
  filter(!is.na(premesis_description), premesis_description != "") |>
  group_by(premesis_description) |>
  summarise(
    theft_count = n(),
    percentage = round(100 * n() / nrow(mvt_data), 1)
  ) |>
  arrange(desc(theft_count)) |>
  head(10) # Top 10 locations

cat("\n **~~ Top 10 Locations for Vehicle Crimes ~~** \n") # Header
print(mvt_locations) # Display results

# Save location analysis
write.csv(mvt_locations, "outputs/tables/mvt_by_location.csv", row.names = FALSE)

# Create location plot for MVT
mvt_location_plot <- ggplot(
  mvt_locations,
  aes(
    x = reorder(premesis_description, theft_count),
    y = theft_count, fill = percentage
  )
) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Where Do Vehicle Crimes Occur?",
    subtitle = "Top 10 location types for motor vehicle incidents",
    x = "Location Type",
    y = "Number of Incidents",
    fill = "Percent of\nMVT"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/mvt_locations.png", mvt_location_plot,
  width = 12, height = 7, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5F ~ Victim Demographics for Vehicle Theft
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

mvt_victims <- mvt_data |>
  filter(!is.na(victim_age_clean)) |>
  summarise(
    avg_age = round(mean(victim_age_clean), 1),
    median_age = round(median(victim_age_clean), 1),
    pct_male = round(100 * sum(victim_sex_clean == "Male", na.rm = TRUE) /
      n(), 1),
    pct_female = round(100 * sum(victim_sex_clean == "Female", na.rm = TRUE) /
      n(), 1)
  )

cat("\n **~~ Victim Demographics for Vehicle Crimes ~~** \n") # Header
print(mvt_victims) # Display results

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5G ~ Compare MVT to Other Crimes
## mvt_vs_other_crimes.csv
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Get non-MVT crimes for comparison (using row indices for efficiency)
mvt_indices <- which(grepl(
  paste(mvt_keywords, collapse = "|"),
  crime_data$crime_code_description
))
non_mvt_data <- crime_data[-mvt_indices, ]

comparison_stats <- data.frame(
  Category = c("Vehicle Crimes", "All Other Crimes"),
  Total_Incidents = c(nrow(mvt_data), nrow(non_mvt_data)),
  Percentage_of_Total = c(
    round(100 * nrow(mvt_data) / nrow(crime_data), 1),
    round(100 * nrow(non_mvt_data) / nrow(crime_data), 1)
  ),
  Avg_Victim_Age = c(
    round(mean(mvt_data$victim_age_clean, na.rm = TRUE), 1),
    round(mean(non_mvt_data$victim_age_clean, na.rm = TRUE), 1)
  ),
  Percent_Male_Victims = c(
    round(100 * sum(mvt_data$victim_sex_clean == "Male", na.rm = TRUE) /
      nrow(mvt_data), 1),
    round(100 * sum(non_mvt_data$victim_sex_clean == "Male", na.rm = TRUE) /
      nrow(non_mvt_data), 1)
  )
)

cat("\n **~~ MVT vs Other Crimes Comparison ~~** \n") # Header
print(comparison_stats) # Display results

# Save comparison
write.csv(comparison_stats, "outputs/tables/mvt_vs_other_crimes.csv",
  row.names = FALSE
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 5H ~ Save MVT Data for future Use
# And Completion Message
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

saveRDS(mvt_data, "data/motor_vehicle_theft_data.rds")
# Save for potential later use

cat("\n **~~ Motor Vehicle Theft Analysis Complete ~~**\n") # Completion
cat("Generated files:\n") # List outputs
cat("  - outputs/tables/mvt_breakdown.csv\n")
cat("  - outputs/tables/mvt_by_area.csv\n")
cat("  - outputs/tables/mvt_by_location.csv\n")
cat("  - outputs/tables/mvt_vs_other_crimes.csv\n")
cat("  - outputs/figures/mvt_by_area.png\n")
cat("  - outputs/figures/mvt_monthly_trends.png\n")
cat("  - outputs/figures/mvt_time_patterns.png\n")
cat("  - outputs/figures/mvt_locations.png\n")
