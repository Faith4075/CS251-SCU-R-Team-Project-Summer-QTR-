## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Clausen, Mohammad Baher and Terrance Carpenter
## Team Project || Team One
## 05-02-2026

# Load required libraries
library(readxl)      # Read Excel files
library(dplyr)       # Data manipulation
library(tidyr)       # Data cleaning
library(janitor)     # Clean column names

# Define file path
file_path <- "Crime_Data_from_2020_to_2024_NEW.xlsx"

# Load the raw data
crime_raw <- read_excel(file_path)  # Import Excel data into R

# This section with make standard formatting
crime_clean <- crime_raw %>%
  clean_names() %>%  # Converts to snake_case (example -  "Area Name of Crime" -> "area_name_of_crime")
  rename(
    # Rename columns for easier typing
    time_occurred = time_occurred_in_military_time,  # Military format (0-2359)
    area_name = area_name_of_crime,                  # Geographic area name
    district_num = reporting_district_number,        # Police district ID
    crime_code = crime_code,                         # Numeric crime code
    crime_desc = crime_code_decription,              # Full crime description
    officer_badge = officers_badge_numbers_that_reported_on_scene,  # Responding officers
    victim_age = victim_age,                         # Age of victim
    victim_sex = victim_sex,                         # M/F/X
    victim_ethnicity = victim_ethnicity_description, # Ethnicity description
    premesis_code = premesis_code,                   # Location type code
    premesis_desc = premesis_description,            # Location type description
    weapon_code = weapon_used_code,                  # Weapon type code
    weapon_desc = weapon_description,                # Weapon description
    status = status,                                 # Case status code
    status_desc = status_description,                # Case status description
    crime_code_one = crime_code_one,                 # Primary crime code
    address = address_location_of_disturbance,       # Street address
    cross_street = cross_street_at_location,         # Intersection/nearest cross street
    latitude = latitude,                             # GPS latitude coordinate
    longitude = longititude                          # GPS longitude coordinate
  ) %>%
  # Filter out rows with missing essential data
  filter(!is.na(crime_desc))  # Remove rows with no crime description

# Handle missing values in key columns
crime_clean <- crime_clean %>%
  mutate(
    # Replace empty victim_age with NA (numeric columns)
    victim_age = na_if(victim_age, ""),  # Convert blanks to NA
    victim_age = as.numeric(victim_age),  # Ensure numeric type
    
    # Clean time_occurred (convert military time to numeric)
    time_occurred = as.numeric(time_occurred),  # Convert to number (e.g., 1430 for 2:30 PM)
    
    # Create time of day categories from military time
    time_category = case_when(
      is.na(time_occurred) ~ "Unknown",                    # Missing times
      time_occurred < 600 ~ "Late Night (00:00-05:59)",    # Midnight to dawn
      time_occurred < 1200 ~ "Morning (06:00-11:59)",       # Morning hours
      time_occurred < 1700 ~ "Afternoon (12:00-16:59)",     # Afternoon hours
      time_occurred < 2100 ~ "Evening (17:00-20:59)",       # Evening hours
      TRUE ~ "Late Night (21:00-23:59)"                     # Late evening
    )
  )

# This will create a summary of cleaned data
data_summary <- list(
  total_rows = nrow(crime_clean),                    # Count total incidents
  total_columns = ncol(crime_clean),                 # Count variables
  unique_areas = n_distinct(crime_clean$area_name),  # Count distinct geographic areas
  unique_crimes = n_distinct(crime_clean$crime_desc) # Count distinct crime types
)

# Print summary to console
print("===== DATA CLEANING COMPLETE =====")  # Section header
print(data_summary)                         # Display summary statistics

# Save cleaned data for other scripts to use
saveRDS(crime_clean, "data/crime_cleaned.rds")  # Save as R binary file (fast loading)
write.csv(crime_clean, "data/crime_cleaned.csv", row.names = FALSE)  # Save as CSV just in case

# Print sample of data
print("===== SAMPLE OF CLEANED DATA (first 5 rows) =====")  # Header
print(head(crime_clean, 5))  # Show first 5 rows to verify cleaning worked
