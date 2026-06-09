## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 3: Spatial Analysis
# Purpose: Analyze crime distribution across Los Angeles areas
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Load required libraries
library(dplyr) # Data manipulation
library(ggplot2) # Visualization
library(forcats) # Work with factors (for reordering)
library(scales) # For comma() function in labels

# Set display options to show all columns
options(tibble.width = Inf)

# Load cleaned data
crime_data <- readRDS("data/crime_cleaned.rds") # Load from Script 1

cat(" **~~ Spatial Analysis Starting... ~~**\n") # Section header

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3A ~ Crime Counts by Police Area
## outputs/tables/crime_by_area.csv
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Calculate crime statistics for each area
area_crime_stats <- crime_data |>
  group_by(area_name_of_crime) |> # Group by police area/district
  summarise(
    total_crimes = n(), # Total incidents in area
    unique_crime_types = n_distinct(crime_code_description),
    # Diversity of crime types
    avg_victim_age = round(mean(victim_age_clean, na.rm = TRUE), 1),
    # Mean victim age
    most_common_crime = names(sort(table(crime_code_description),
      decreasing = TRUE
    ))[1],
    # Top crime in area
    most_common_location = names(sort(table(premesis_description),
      decreasing = TRUE
    ))[1]
    # Top location
  ) |>
  arrange(desc(total_crimes)) # Sort from highest to lowest crime areas

# Display top 10 areas with highest crime
cat("\n **~~ Top 10 Highest Crime Areas ~~**\n") # Header
print(head(area_crime_stats, 10)) # Show top 10

# Save full area analysis
write.csv(area_crime_stats, "outputs/tables/crime_by_area.csv",
  row.names = FALSE
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3B ~ Bar Plot of Top 15 Areas
## top_15_areas.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Create horizontal bar chart for top 15 areas
area_plot <- ggplot(
  head(area_crime_stats, 15),
  aes(
    x = reorder(area_name_of_crime, total_crimes),
    y = total_crimes, fill = total_crimes
  )
) +
  geom_bar(stat = "identity") + # Bar chart
  geom_text(aes(label = comma(total_crimes)), hjust = -0.1, size = 3) +
  # Add count labels
  coord_flip() + # Flip to horizontal (easier to read area names)
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  # Color by crime count
  scale_y_continuous(labels = comma) + # Commas on numbers
  labs(
    title = "Top 15 Los Angeles Areas with Highest Crime (2020-2024)",
    subtitle = "Based on total reported incidents",
    x = "Area Name (Police District)",
    y = "Total Number of Crimes",
    fill = "Crime Count" # Legend title
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right" # Keep legend for color scale
  )

ggsave("outputs/figures/top_15_areas.png", area_plot,
  width = 12, height = 8, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3C ~ Crime Density by Location Type (Premesis Description)
## crime_by_location.csv
## location_types.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Analyze where crimes happen most frequently
location_analysis <- crime_data |>
  filter(!is.na(premesis_description), premesis_description != "") |>
  # Remove missing location data
  group_by(premesis_description) |> # Group by location type
  summarise(
    total_crimes = n(),
    percentage = round(100 * n() / nrow(crime_data), 2) # Percent of all crimes
  ) |>
  arrange(desc(total_crimes)) |> # Sort most common first
  head(15) # Top 15 location types

cat("\n **~~ Top 15 Location Types for Crime ~~**\n") # Header
print(location_analysis) # Display results

# Save location analysis
write.csv(location_analysis, "outputs/tables/crime_by_location.csv",
  row.names = FALSE
)

# Create location type plot
location_plot <- ggplot(
  location_analysis,
  aes(
    x = reorder(premesis_description, total_crimes),
    y = total_crimes, fill = percentage
  )
) +
  geom_bar(stat = "identity") + # Bar chart
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  # Add percentage labels
  coord_flip() + # Horizontal bars
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") + # Green gradient
  scale_y_continuous(labels = comma) + # Commas on numbers
  labs(
    title = "Crime by Location Type in Los Angeles (Top 15)",
    subtitle = "Where do crimes most frequently occur?",
    x = "Premesis Description (Location Type)",
    y = "Total Crimes",
    fill = "Percent of\nTotal Crimes" # Legend with line break
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/figures/location_types.png", location_plot,
  width = 12, height = 8, dpi = 300
)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3D ~ Crime Hotspot Categories (Area + crime type combo)
## area_crime_profiles.csv
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Identify which areas have highest rates of specific crime types
# Calculate area-specific crime proportions
area_crime_profiles <- crime_data |>
  group_by(area_name_of_crime, crime_code_description) |>
  # Group by area and crime type
  summarise(count = n(), .groups = "drop") |> # Count per area-crime pair
  group_by(area_name_of_crime) |> # Regroup by area
  mutate(
    area_total = sum(count), # Total crimes in that area
    percentage_of_area = round(100 * count / area_total, 1)
    # Percent of area's crimes
  ) |>
  arrange(area_name_of_crime, desc(count)) |> # Sort by area then crime frequency
  group_by(area_name_of_crime) |>
  slice_head(n = 3) # Keep top 3 crime types per area

# Save area crime profiles
write.csv(area_crime_profiles, "outputs/tables/area_crime_profiles.csv",
  row.names = FALSE
)

cat("\n **~~ Area Crime Profiles (Top 3 Crimes per Area) ~~**\n") # Header
print(head(area_crime_profiles, 30)) # Show first 30 rows (10 areas x 3 crimes)

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3E ~ Victim Demographics by Area
## area_victim_demographics.csv
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Analyze victim characteristics by geographic area
area_victim_demographics <- crime_data |>
  group_by(area_name_of_crime) |>
  summarise(
    total_victims = n(), # Total victims in area
    avg_victim_age = round(mean(victim_age_clean, na.rm = TRUE), 1),
    # Mean age
    pct_male = round(100 * sum(victim_sex_clean == "Male", na.rm = TRUE) /
      n(), 1), # % male
    pct_female = round(100 * sum(victim_sex_clean == "Female", na.rm = TRUE) /
      n(), 1), # % female
    most_common_ethnicity = names(sort(table(victim_ethnicity_description),
      decreasing = TRUE
    ))[1]
    # Most common ethnicity
  ) |>
  arrange(desc(total_victims)) # Sort by total victims

# Save victim demographics by area
write.csv(area_victim_demographics, "outputs/tables/area_victim_demographics.csv",
  row.names = FALSE
)

cat("\n **~~ Victim Demographics by Top 5 Areas ~~** \n") # Header
print(head(area_victim_demographics, 5)) # Display top 5 areas

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3F ~ Geographic Visualization (Scatter Plot of Crime Locations)
## crime_location_map.png
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Create a map-like scatter plot using latitude/longitude
# Filter to incidents with valid coordinates
crime_with_coords <- crime_data |>
  filter(
    !is.na(latitude), !is.na(longitude), # Must have both coordinates
    latitude != 0, longitude != 0
  ) # Exclude zero coordinates

cat("\n **~~ Geographic Data Quality ~~** \n") # Header
cat(
  "Incidents with valid coordinates:", nrow(crime_with_coords),
  "out of", nrow(crime_data), "\n"
) # Count
cat(
  "Percentage:", round(100 * nrow(crime_with_coords) / nrow(crime_data), 1),
  "%\n"
) # Percent

if (nrow(crime_with_coords) > 1000) { # Only create map if enough points exist
  # Sample points for better performance (too many points makes plot slow)
  set.seed(123) # For reproducible sampling
  sample_size <- min(10000, nrow(crime_with_coords)) # Max 10,000 points
  crime_sample <- crime_with_coords |>
    sample_n(sample_size) # Random sample of points

  # Create scatter plot of crime locations (using top 10 crime types only)
  top_crimes <- crime_sample |>
    group_by(crime_code_description) |>
    summarise(count = n()) |>
    slice_max(count, n = 10) |>
    pull(crime_code_description)

  crime_sample_filtered <- crime_sample |>
    filter(crime_code_description %in% top_crimes)

  coord_plot <- ggplot(
    crime_sample_filtered,
    aes(
      x = longitude, y = latitude,
      color = crime_code_description
    )
  ) +
    geom_point(alpha = 0.3, size = 0.5) + # Semi-transparent points
    scale_color_viridis_d() + # Colorblind-friendly colors
    labs(
      title = "Spatial Distribution of Crime Incidents in Los Angeles",
      subtitle = paste(
        "Sample of", sample_size, "incidents (",
        round(100 * sample_size / nrow(crime_with_coords), 1),
        "% of total) - Top 10 crime types shown"
      ),
      x = "Longitude",
      y = "Latitude",
      color = "Crime Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom", # Put legend at bottom
      legend.text = element_text(size = 8) # Smaller legend text
    ) +
    guides(color = guide_legend(ncol = 2)) # Arrange legend in 2 columns

  # Save map || WARNING: may take a moment to render ||
  ggsave("outputs/figures/crime_location_map.png", coord_plot,
    width = 12, height = 10, dpi = 300
  )
  cat("Map saved to outputs/figures/crime_location_map.png\n") # Confirmation
} else {
  cat("Not enough coordinates to create a meaningful map.\n") # Warning
}

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 3G ~ Completion Message
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

cat("\n **~~ Spatial Analysis Complete ~~** \n") # Completion message
cat("Generated files:\n") # List outputs
cat("  - outputs/tables/crime_by_area.csv\n")
cat("  - outputs/tables/crime_by_location.csv\n")
cat("  - outputs/tables/area_crime_profiles.csv\n")
cat("  - outputs/tables/area_victim_demographics.csv\n")
cat("  - outputs/figures/top_15_areas.png\n")
cat("  - outputs/figures/location_types.png\n")
if (exists("coord_plot")) {
  cat("  - outputs/figures/crime_location_map.png\n")
}
