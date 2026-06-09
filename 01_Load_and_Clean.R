## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 1: Data loading and cleaning
# Purpose: Import Excel data, clean column names, prepare for analysis
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Load required libraries
library(readxl) # Load Excel files into R
library(dplyr) # Data manipulation and transformation
library(lubridate) # Handle dates and times
library(janitor) # Clean column names automatically

# Set display options to show all columns
options(tibble.width = Inf)

# Define file path (Excel file is in the 'data' subfolder)
file_path <- "data/Crime_Data_from_2020_to_2024_NEW.xlsx"

# Load the raw data from Excel
crime_raw <- read_excel(file_path) # Import all rows and columns

# Display basic info about the dataset
cat("~~** Raw Data Overview **~~\n") # Print header
cat("Total rows:", nrow(crime_raw), "\n") # Count of incidents
cat("Total columns:", ncol(crime_raw), "\n") # Count of variables
cat("Column names:\n") # Header for column list
print(names(crime_raw)) # Show all column names

crime_clean <- crime_raw

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 1A ~ Date and time processing section
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Convert dates and process all columns
crime_clean <- crime_clean |>
  mutate(
    # Parse dates (format: "2020 Nov 07 12:00:00 AM")
    date_occurred = as.Date(date_occ, format = "%Y %b %d"),
    date_reported = as.Date(`date rptd`, format = "%Y %b %d"),
    # Extract temporal components from occurrence date
    occurrence_year = year(date_occurred), # Extract year
    occurrence_month = month(date_occurred), # Extract month number
    occurrence_day = day(date_occurred), # Extract day of month
    occurrence_week = week(date_occurred), # Extract week number
    occurrence_quarter = quarter(date_occurred), # Extract quarter
    # Day of week. Opted for full name for readability in plots
    day_of_week = wday(date_occurred, label = TRUE, abbr = FALSE),
    # Calculate reporting delay (days between occurrence and report)
    reporting_delay = as.numeric(date_reported - date_occurred),
    # Process military time
    time_occurred = as.numeric(time_occurred_in_military_time),
    time_category = case_when(
      is.na(time_occurred) ~ "Unknown", # Missing time data
      time_occurred < 600 ~ "Late Night (00:00-05:59)", # Midnight to dawn
      time_occurred < 1200 ~ "Morning (06:00-11:59)", # Morning hours
      time_occurred < 1700 ~ "Afternoon (12:00-16:59)", # Afternoon hours
      time_occurred < 2100 ~ "Evening (17:00-20:59)", # Evening hours
      TRUE ~ "Late Night (21:00-23:59)" # Late night
    ),
    hour_of_day = floor(time_occurred / 100), # 1430 -> 14 (2 PM)
    # Clean victim age (remove unrealistic values)
    victim_age = as.numeric(victim__age),
    victim_age_clean = case_when(
      is.na(victim_age) ~ NA_real_, # Keep NAs as NA
      victim_age < 0 ~ NA_real_, # Negative ages are impossible
      victim_age > 120 ~ NA_real_, # Oldest recorded person is 122
      TRUE ~ victim_age # Keep valid ages
    ),
    # Create age groups for demographic analysis
    age_group = case_when(
      is.na(victim_age_clean) ~ "Unknown",
      victim_age_clean < 18 ~ "Juvenile (<18)",
      victim_age_clean < 30 ~ "Young Adult (18-29)",
      victim_age_clean < 50 ~ "Adult (30-49)",
      victim_age_clean < 65 ~ "Middle Age (50-64)",
      TRUE ~ "Senior (65+)"
    ),
    # Standardize categories in victim sex
    victim_sex_clean = case_when(
      victim_sex %in% c("M", "MALE") ~ "Male", # Male victims
      victim_sex %in% c("F", "FEMALE") ~ "Female", # Female victims
      victim_sex %in% c("X", "X Male", "X Female") ~ "Other/Unknown",
      # Non-binary or unknown
      TRUE ~ "Not Recorded" # Missing data
    )
  )

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 1C ~ This section will filter to study period which is 2020-2024
# as specified in the abstract
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

crime_study_period <- crime_clean |>
  filter(occurrence_year >= 2020, occurrence_year <= 2024)
# 5-year analysis window

# Creating structure for summary statistics section
# Dataset summary
data_summary <- list(
  total_incidents = nrow(crime_study_period), # Total crimes in study period
  date_range_start = min(crime_study_period$date_occurred, na.rm = TRUE),
  # Earliest date
  date_range_end = max(crime_study_period$date_occurred, na.rm = TRUE),
  # Latest date
  unique_areas = n_distinct(crime_study_period$area_name_of_crime),
  # Number of police areas
  unique_crime_types = n_distinct(crime_study_period$crime_code_description),
  # Types of crimes
  unique_locations = n_distinct(crime_study_period$premises_description),
  # Location types (note: premises, not premesis)
  missing_coordinates = sum(is.na(crime_study_period$latitude))
  # Count of missing GPS data
)

# Print summary to console
cat("\n **~~ Data Cleaning Complete ~~**\n") # Section header
for (item in names(data_summary)) { # Loop through each summary item
  cat(item, ":", data_summary[[item]], "\n") # Print each statistic
}

# Save cleaned data for other scripts
saveRDS(crime_study_period, "data/crime_cleaned.rds") # Binary format
write.csv(crime_study_period, "data/crime_cleaned.csv", row.names = FALSE)

# Display first few rows to verify cleaning worked
cat("\n **~~ Sample of Cleaned Data (First 5 Rows) ~~** \n") # Header
print(head(crime_study_period, 5) |>
  select(
    date_occurred, area_name_of_crime, crime_code_description,
    victim_age, day_of_week
  ))

cat("\n **~~ Script 1 Complete. Data ready for analysis ~~** \n")

# Display the column names for reference
cat("\n **~~ Available Columns in Cleaned Data ~~** \n")
print(names(crime_study_period))
