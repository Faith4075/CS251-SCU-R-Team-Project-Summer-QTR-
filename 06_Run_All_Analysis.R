## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith Clausen, Mohammad Baher and Terrance Carpenter
## Team Project || Team One
## 05-02-2026

# Master script to run all analyses and generate final report

# Create output directories if they don't exist
if(!dir.exists("outputs")) dir.create("outputs")        # Main output folder
if(!dir.exists("outputs/figures")) dir.create("outputs/figures")  # Figures subfolder
if(!dir.exists("outputs/tables")) dir.create("outputs/tables")    # Tables subfolder
if(!dir.exists("data")) dir.create("data")              # Data folder

# Print start message
print("=============================================")
print("LA CRIME ANALYSIS PROJECT - STARTING")
print("=============================================")
print(paste("Start time:", Sys.time()))

# Run each analysis script in order
print("\n[1/5] Running data loading and cleaning...")
source("01_load_and_clean.R", echo = TRUE)  # Load & clean data

print("\n[2/5] Running temporal analysis...")
tryCatch({
  source("02_time_analysis.R", echo = TRUE)  # May fail without dates
}, error = function(e) {
  print("NOTE: Temporal analysis requires date column. Add when available.")
})

print("\n[3/5] Running spatial analysis...")
source("03_spatial_analysis.R", echo = TRUE)  # Geographic patterns

print("\n[4/5] Running crime pattern analysis...")
source("04_pattern_analysis.R", echo = TRUE)  # Crime types & time patterns

print("\n[5/5] Running motor vehicle theft focus analysis...")
source("05_motor_vehicle_theft_focus.R", echo = TRUE)  # MVT deep dive

# Generate summary of outputs
print("\n=============================================")
print("ANALYSIS COMPLETE - OUTPUT SUMMARY")
print("=============================================")

# List all generated files
if(dir.exists("outputs/figures")) {
  figures <- list.files("outputs/figures", pattern = "\\.png$")
  print(paste("Figures created:", length(figures)))
  print(figures)
}

if(dir.exists("outputs/tables")) {
  tables <- list.files("outputs/tables", pattern = "\\.csv$")
  print(paste("Tables created:", length(tables)))
  print(tables)
}

print(paste("End time:", Sys.time()))
print("=============================================")
