## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 6 (Master Script): Run All Analyses
# Purpose: Execute all analysis scripts in order
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Clear environment for fresh start
rm(list = ls()) # Remove all existing objects from memory
cat("\n **~~ L.A. Crime Analysis Project ~~** \n") # Title header
cat("Starting comprehensive crime analysis of Los Angeles (2020-2024)\n")
# Description
cat("Start time:", as.character(Sys.time()), "\n\n") # Timestamp

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 6A ~ Create Directory Structure (If it doesn't exist)
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# List of required directories
required_dirs <- c("data", "outputs", "outputs/figures", "outputs/tables")

# Create each directory if missing
for (dir_name in required_dirs) {
  if (!dir.exists(dir_name)) { # Check if directory exists
    dir.create(dir_name, recursive = TRUE) # Create directory (and parents)
    cat("Created directory:", dir_name, "\n") # Confirmation message
  }
}

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 6B ~ Check for Data File
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Look for Excel files in data folder
excel_files <- list.files("data",
  pattern = "\\.xlsx$|\\.xls$",
  full.names = TRUE
)

if (length(excel_files) == 0) {
  # No data file found - show error
  cat("\nERROR: No Excel data file found in 'data' folder!\n") # Error msg
  cat("Please place your LA Crime Excel file in the 'data' folder.\n")
  # Instruction
  cat("The file should be named: Crime_Data_from_2020_to_2024_NEW.xlsx\n")
  # Next step
  stop("Missing data file") # Stop execution
} else {
  cat("\nFound data file:", basename(excel_files[1]), "\n") # Show found file
}

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 6C ~ Run Each Analysis Script in Order
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Define scripts in order of execution
scripts <- c(
  "01_Load_and_Clean.R",
  "02_Time_Analysis.R",
  "03_Spatial_Analysis.R",
  "04_Pattern_Analysis.R",
  "05_Motor_Vehicle_Theft_Focus.R"
)

# Counter for tracking progress
successful_scripts <- 0 # Count how many completed
failed_scripts <- 0 # Count how many failed

# Run each script
for (script_name in scripts) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "") # Separator
  cat("RUNNING:", script_name, "\n") # Current script name
  cat(paste(rep("=", 60), collapse = ""), "\n", sep = "") # Separator line

  # Check if script exists before trying to run it
  if (!file.exists(script_name)) {
    cat("ERROR: Script file not found:", script_name, "\n")
    cat("Make sure this file is in the current working directory.\n")
    failed_scripts <- failed_scripts + 1
    next
  }

  # Try to run the script, catch any errors
  tryCatch(
    {
      source(script_name, echo = FALSE) # Execute (don't echo to console)
      cat("SUCCESS:", script_name, "completed\n") # Success message
      successful_scripts <- successful_scripts + 1 # Increment counter
    },
    error = function(e) {
      # If error occurs, show error message
      cat("✗ ERROR in", script_name, ":\n") # Error header
      cat("  ", e$message, "\n") # Show error message
      failed_scripts <- failed_scripts + 1 # Increment failure counter
    }
  )
}

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Section 6D ~ Generate Summary Report
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "") # Separator
cat(" **~~ Project Summary ~~**\n") # Header
cat(paste(rep("=", 60), collapse = ""), "\n", sep = "") # Separator line

# Execution summary
cat("\n **~~ Script Execution Results ~~** \n") # Header
cat("  - Successful:", successful_scripts, "of", length(scripts), "\n")
# Success count
cat("  - Failed:", failed_scripts, "of", length(scripts), "\n")
# Failure count

# Output files summary
cat("\n **~~ Generated Output Files ~~** \n") # Header

# Count figures
if (dir.exists("outputs/figures")) {
  figure_count <- length(list.files("outputs/figures", pattern = "\\.png$"))
  # Count PNG files
  cat("  - Figures (PNG):", figure_count, "files in outputs/figures/\n")
  # Figure count
}

# Count tables
if (dir.exists("outputs/tables")) {
  table_count <- length(list.files("outputs/tables", pattern = "\\.csv$"))
  # Count CSV files
  cat("  - Tables (CSV):", table_count, "files in outputs/tables/\n")
  # Table count
}

# List all generated files
cat("\n **~~ Detailed File List ~~** \n")
if (dir.exists("outputs/figures")) {
  cat("\n Figures:\n")
  figures <- list.files("outputs/figures", pattern = "\\.png$")
  for (f in figures) {
    cat("  -", f, "\n")
  }
}

if (dir.exists("outputs/tables")) {
  cat("\n Tables:\n")
  tables <- list.files("outputs/tables", pattern = "\\.csv$")
  for (t in tables) {
    cat("  -", t, "\n")
  }
}

# Completion message
cat("\nEnd time:", as.character(Sys.time()), "\n") # End timestamp
cat("\n **~~ Analysis Complete ~~** \n") # Completion message
cat("Check the 'outputs/figures' folder for visualizations.\n") # Instruction
cat("Check the 'outputs/tables' folder for data tables.\n") # Instruction

# Summary of what was accomplished
cat("\n **~~ Analysis Summary ~~** \n")
cat("This analysis examined crime patterns in Los Angeles from 2020-2024.\n")
cat("Key outputs include:\n")
cat("  - Temporal trends (annual, monthly, and COVID-19 period comparisons)\n")
cat("  - Spatial distribution (crime by police area and location type)\n")
cat("  - Pattern analysis (crime types, time of day, victim demographics)\n")
cat("  - Motor vehicle theft deep dive\n")
