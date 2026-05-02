## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Clausen, Mohammad Baher and Terrance Carpenter
## Team Project || Team One
## 05-02-2026

# Spatial analysis of crime across Los Angeles areas

library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization
library(maps)       # Map data
library(ggrepel)    # Avoid label overlap

# Load cleaned data
crime_data <- readRDS("data/crime_cleaned.rds")  # Load from script 01

# 1. Crime counts by Area Name (Police Districts)
area_crime_counts <- crime_data %>%
  group_by(area_name) %>%                                   # Group by police area
  summarise(
    total_crimes = n(),                                     # Count incidents
    unique_crime_types = n_distinct(crime_desc),            # Variety of crimes
    avg_victim_age = round(mean(victim_age, na.rm = TRUE), 1),  # Mean victim age
    most_common_location = names(sort(table(premesis_desc), decreasing = TRUE))[1]  # Top location type
  ) %>%
  arrange(desc(total_crimes))  # Sort from highest to lowest crime areas

# Print top 10 highest crime areas
print("===== TOP 10 AREAS WITH HIGHEST CRIME RATES =====")  # Header
print(head(area_crime_counts, 10))  # Show top 10

# Save full area analysis
write.csv(area_crime_counts, "outputs/tables/crime_by_area.csv", row.names = FALSE)

# 2. Bar plot of top 10 areas
area_plot <- ggplot(head(area_crime_counts, 10), 
                    aes(x = reorder(area_name, total_crimes), y = total_crimes)) +
  geom_bar(stat = "identity", fill = "steelblue") +         # Blue bars
  geom_text(aes(label = total_crimes), hjust = -0.1, size = 3) +  # Add labels
  coord_flip() +                                             # Flip to horizontal bars
  labs(
    title = "Top 10 Los Angeles Areas with Highest Crime (2020-2024)",
    x = "Area Name (Police District)",                       # X-axis after flip
    y = "Total Number of Crimes"                             # Y-axis after flip
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/top_10_areas.png", area_plot, width = 10, height = 6, dpi = 300)

# 3. Crime density by location type (Premises Description column in excel file)
location_types <- crime_data %>%
  filter(!is.na(premesis_desc)) %>%                         # Remove missing locations
  group_by(premesis_desc) %>%                               # Group by location type
  summarise(
    total_crimes = n(),
    percentage = round(100 * n() / nrow(crime_data), 2)     # Calculate percent of total
  ) %>%
  arrange(desc(total_crimes)) %>%                           # Sort descending
  head(15)                                                  # Top 15 location types

# Plot location types
location_plot <- ggplot(location_types, 
                        aes(x = reorder(premesis_desc, total_crimes), y = total_crimes)) +
  geom_bar(stat = "identity", fill = "darkgreen") +         # Green bars
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +  # Add percent labels
  coord_flip() +                                             # Horizontal bars
  labs(
    title = "Crime by Location Type (Top 15)",
    subtitle = "Where crimes most frequently occur in Los Angeles",
    x = "Premises Description (Location Type)",              # X-axis label
    y = "Total Crimes"                                       # Y-axis label
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/location_types.png", location_plot, width = 12, height = 7, dpi = 300)

# 4. Map visualization (if latitude/longitude data is complete. Could also be NA)
# Remove rows with missing coordinates
crime_with_coords <- crime_data %>%
  filter(!is.na(latitude), !is.na(longitude))  # Keep only geocoded incidents

if(nrow(crime_with_coords) > 100) {  # Only create map if enough coordinates exist
  
  coord_plot <- ggplot(crime_with_coords %>% sample_n(min(5000, nrow(.))),  # Sample for performance
                       aes(x = longitude, y = latitude)) +
    geom_point(alpha = 0.3, size = 0.5, color = "darkred") +  # Semi-transparent points
    labs(
      title = "Spatial Distribution of Crime Incidents",
      subtitle = paste("Sample of", min(5000, nrow(crime_with_coords)), "incidents"),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal()
  
  ggsave("outputs/figures/crime_map_scatter.png", coord_plot, width = 10, height = 8, dpi = 300)
}

print("===== SPATIAL ANALYSIS COMPLETE =====")
print(paste("Total geographic areas analyzed:", n_distinct(crime_data$area_name)))
