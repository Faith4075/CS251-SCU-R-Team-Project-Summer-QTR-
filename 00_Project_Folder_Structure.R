## CS251 || Dr. Ahreum Ju
## Seattle City University
## Authors: Faith, Terrance, Mohammad
## Team Project || Team One
## 06-10-2026

# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~
# Script 0: This will create all folders needed for the LA Crime Project
# **~~**~~**~~**~~**~~**~~**~~**~~**~~**~~

# Define the main project folder name
project_folder <- "LA_Crime_Project"

# Create the main project folder
if (!dir.exists(project_folder)) {
  dir.create(project_folder)
  cat(" Created main folder:", project_folder, "\n")
} else {
  cat(" Folder already exists:", project_folder, "\n")
}

# Create subfolders inside the project folder
subfolders <- c(
  "data", # For your Excel data file
  "outputs", # For all results
  "outputs/figures", # For charts and graphs
  "outputs/tables" # For CSV data tables
)

# Loop through and create each subfolder
for (folder in subfolders) {
  full_path <- file.path(project_folder, folder)
  if (!dir.exists(full_path)) {
    dir.create(full_path, recursive = TRUE)
    # recursive=TRUE creates parent folders
    cat(" Created:", full_path, "\n")
  } else {
    cat(" Already exists:", full_path, "\n")
  }
}

cat("\n~~**~~**~~**~~**~~**~~**~~**~~**~~**~~**\n")
cat("Folder structure created successfully!\n")
cat("~~**~~**~~**~~**~~**~~**~~**~~**~~**~~**~~**\n")
cat("\nYour project folder is located at:\n")
cat(getwd(), "/", project_folder, "\n", sep = "")
cat("\nNext steps:\n")
cat("1. Copy your Excel file into the 'data' folder\n")
cat("2. Save the R scripts in the main project folder\n")
cat("3. Run 01_load_and_clean.R\n")
