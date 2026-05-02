## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Clausen, Mohammad Baher and Terrance Carpenter
## Team Project || Team One
## 05-02-2026

# Analysis of crime patterns by type, time of day, and victim demographics

library(dplyr)       # Data manipulation
library(ggplot2)     # Visualization
library(viridis)     # Colorblind-friendly palettes

crime_data <- readRDS("data/crime_cleaned.rds")  # Load from script 01

# 1. Most common crime types (top 20)
crime_type_counts <- crime_data %>%
  group_by(crime_desc) %>%                                 # Group by crime description
  summarise(
    count = n(),
    percentage = round(100 * n() / nrow(crime_data), 2)    # Percent of total
  ) %>%
  arrange(desc(count)) %>%                                 # Sort most frequent first
  head(20)                                                 # Top 20 crime types

# Print top crimes
print("===== TOP 10 MOST COMMON CRIMES IN LOS ANGELES =====")  # Header
print(head(crime_type_counts, 10))  # Show top 10

# Save full crime type analysis
write.csv(crime_type_counts, "outputs/tables/top_crime_types.csv", row.names = FALSE)

# Plot top 15 crime types
crime_type_plot <- ggplot(head(crime_type_counts, 15), 
                          aes(x = reorder(crime_desc, count), y = count)) +
  geom_bar(stat = "identity", fill = "purple4") +          # Purple bars
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +  # Add percent
  coord_flip() +                                           # Flip for readability
  labs(
    title = "Most Frequent Crime Types in Los Angeles (2020-2024)",
    subtitle = "Top 15 crime categories by incident count",
    x = "Crime Description",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/top_crime_types.png", crime_type_plot, width = 12, height = 8, dpi = 300)

# 2. Time of day patterns (using military time from excel file)
time_patterns <- crime_data %>%
  filter(!is.na(time_category), time_category != "Unknown") %>%  # Remove unknown
  group_by(time_category) %>%                                    # Group by time period
  summarise(
    total_crimes = n(),
    percentage = round(100 * n() / nrow(crime_data), 2)          # Calculate percent
  ) %>%
  mutate(time_category = factor(time_category, 
         levels = c("Late Night (00:00-05:59)", "Morning (06:00-11:59)",
                    "Afternoon (12:00-16:59)", "Evening (17:00-20:59)",
                    "Late Night (21:00-23:59)")))  # Order chronologically

# Time of day plot
time_plot <- ggplot(time_patterns, aes(x = time_category, y = total_crimes, fill = time_category)) +
  geom_bar(stat = "identity") +                                  # Bar chart
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5) + # Add percent labels
  scale_fill_viridis(discrete = TRUE) +                          # Colorblind-friendly colors
  labs(
    title = "Crime by Time of Day: Los Angeles (2020-2024)",
    subtitle = "When do most crimes occur?",
    x = "Time Period (24-hour Military Time)",
    y = "Total Incidents"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",                                     # Remove legend (I don't think this is needed)
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)            # Rotate x labels
  )

ggsave("outputs/figures/time_of_day_patterns.png", time_plot, width = 10, height = 6, dpi = 300)

# 3. Victim demographics analysis
victim_summary <- crime_data %>%
  filter(!is.na(victim_age), victim_age > 0, victim_age < 120) %>%  # Reasonable age range
  summarise(
    avg_age = round(mean(victim_age, na.rm = TRUE), 1),        # Mean victim age
    median_age = round(median(victim_age, na.rm = TRUE), 1),    # Median victim age
    min_age = min(victim_age, na.rm = TRUE),                   # Youngest victim
    max_age = max(victim_age, na.rm = TRUE),                   # Oldest victim
    total_victims = n()                                        # Count with valid age
  )

print("===== VICTIM AGE SUMMARY =====")
print(victim_summary)

# Age distribution by crime type (top 5 crimes)
top_5_crimes <- crime_type_counts$crime_desc[1:5]  # Get names of top 5 crimes

age_by_crime <- crime_data %>%
  filter(crime_desc %in% top_5_crimes, !is.na(victim_age), victim_age < 100) %>%
  group_by(crime_desc) %>%
  summarise(
    avg_victim_age = round(mean(victim_age), 1),
    median_victim_age = round(median(victim_age), 1)
  )

print("===== VICTIM AGE BY CRIME TYPE (TOP 5 CRIMES) =====")
print(age_by_crime)

# 4. Victim sex distribution
sex_distribution <- crime_data %>%
  filter(!is.na(victim_sex), victim_sex %in% c("M", "F")) %>%  # Keep only M/F
  group_by(victim_sex) %>%
  summarise(
    count = n(),
    percentage = round(100 * n() / n(), 2)
  )

print("===== VICTIM SEX DISTRIBUTION =====")
print(sex_distribution)

# 5. Weapon usage analysis
weapon_summary <- crime_data %>%
  filter(!is.na(weapon_desc), weapon_desc != "") %>%          # Crimes with known weapons
  group_by(weapon_desc) %>%
  summarise(
    total_incidents = n(),
    percentage = round(100 * n() / nrow(crime_data), 2)
  ) %>%
  arrange(desc(total_incidents)) %>%
  head(15)  # Top 15 weapons

# Save weapon analysis
write.csv(weapon_summary, "outputs/tables/weapon_analysis.csv", row.names = FALSE)

print("===== TOP 10 WEAPONS USED IN CRIMES =====")
print(head(weapon_summary, 10))

# Weapon plot
weapon_plot <- ggplot(head(weapon_summary, 10), 
                      aes(x = reorder(weapon_desc, total_incidents), y = total_incidents)) +
  geom_bar(stat = "identity", fill = "orange3") +            # Orange bars
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Most Common Weapons Used in LA Crimes",
    x = "Weapon Type",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("outputs/figures/top_weapons.png", weapon_plot, width = 12, height = 7, dpi = 300)

print("===== PATTERN ANALYSIS COMPLETE =====")
