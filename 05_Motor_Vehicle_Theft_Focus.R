## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Clausen, Mohammad Baher and Terrance Carpenter
## Team Project || Team One
## 05-02-2026

library(dplyr)       # Data manipulation
library(ggplot2)     # Visualization
library(lubridate)   # Date handling (if dates available)

crime_data <- readRDS("data/crime_cleaned.rds")  # Load from script 01

# Identify motor vehicle theft incidents
mvt_data <- crime_data %>%
  filter(grepl("VEHICLE|AUTO|CAR|MOTOR", toupper(crime_desc))) %>%  # Case-insensitive search
  mutate(
    is_motor_vehicle_theft = TRUE  # Flag for filtering
  )

print(paste("Total Motor Vehicle Theft incidents identified:", nrow(mvt_data)))
print(paste("Percentage of all crimes:", round(100 * nrow(mvt_data) / nrow(crime_data), 2), "%"))

# 1. Motor vehicle theft by area
mvt_by_area <- mvt_data %>%
  group_by(area_name) %>%
  summarise(
    mvt_count = n(),
    mvt_percentage_of_area_crimes = round(100 * n() / 
                                           sum(crime_data$area_name == area_name, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mvt_count)) %>%
  head(10)

print("===== TOP 10 AREAS FOR MOTOR VEHICLE THEFT =====")
print(mvt_by_area)

# Plot MVT by area
mvt_area_plot <- ggplot(mvt_by_area, aes(x = reorder(area_name, mvt_count), y = mvt_count)) +
  geom_bar(stat = "identity", fill = "firebrick") +      # Red bars
  geom_text(aes(label = mvt_count), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Motor Vehicle Theft Hotspots in Los Angeles",
    subtitle = "Top 10 areas with highest number of vehicle thefts",
    x = "Police Area/District",
    y = "Number of Vehicle Thefts"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/motor_vehicle_theft_by_area.png", mvt_area_plot, width = 10, height = 6, dpi = 300)

# 2. Time patterns for vehicle theft
mvt_time <- mvt_data %>%
  filter(!is.na(time_category)) %>%
  group_by(time_category) %>%
  summarise(
    theft_count = n(),
    percentage_of_mvt = round(100 * n() / nrow(mvt_data), 2)
  )

print("===== VEHICLE THEFT BY TIME OF DAY =====")
print(mvt_time)

# 3. Location types where vehicle theft occurs
mvt_locations <- mvt_data %>%
  filter(!is.na(premesis_desc)) %>%
  group_by(premesis_desc) %>%
  summarise(
    theft_count = n(),
    percentage = round(100 * n() / nrow(mvt_data), 2)
  ) %>%
  arrange(desc(theft_count)) %>%
  head(10)

print("===== COMMON LOCATIONS FOR VEHICLE THEFT =====")
print(mvt_locations)

# Save MVT analysis
saveRDS(mvt_data, "data/motor_vehicle_theft_data.rds")
write.csv(mvt_by_area, "outputs/tables/mvt_by_area.csv", row.names = FALSE)
write.csv(mvt_time, "outputs/tables/mvt_by_time.csv", row.names = FALSE)

print("===== MOTOR VEHICLE THEFT ANALYSIS COMPLETE =====")
