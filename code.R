# Load necessary packages
library(haven)    # For reading .dta files
library(readxl)   # For reading Excel files
library(dplyr)    # For data manipulation

# Define file paths (adjust these based on your working directory)
background_data_path <- "/Users/diegomoers/Desktop/SRAEMO/data/background_data_062020.dta"
automation_data_path <- "/Users/diegomoers/Desktop/SRAEMO/data/SW_automation_2020.dta"
lmi_index_path <- "/Users/diegomoers/Desktop/SRAEMO/data/LMII-AE Index-2019.xlsx"
occupation_data_path <- "/Users/diegomoers/Desktop/SRAEMO/data/occupation_w13_WaS.dta"

# Load .dta files
background_data <- read_dta(background_data_path)
automation_data <- read_dta(automation_data_path)
occupation_data <- read_dta(occupation_data_path)

# Load Excel file (assuming data is in the first sheet)
lmi_index <- read_excel(lmi_index_path, sheet = 2)

# Merge background_data and automation_data using nomem_encr
merged_data <- background_data %>%
  inner_join(automation_data, by = "nomem_encr") %>%
  inner_join(occupation_data, by = "nomem_encr")  # Adding occupation data

# Check structure of the merged dataset
glimpse(merged_data)

# Check for unmatched rows
unmatched_bg <- anti_join(background_data, automation_data, by = "nomem_encr")
unmatched_auto <- anti_join(automation_data, background_data, by = "nomem_encr")
unmatched_occ <- anti_join(occupation_data, background_data, by = "nomem_encr")

# Print counts of unmatched observations
cat("Unmatched in background data:", nrow(unmatched_bg), "\n")
cat("Unmatched in automation data:", nrow(unmatched_auto), "\n")
cat("Unmatched in occupation data:", nrow(unmatched_occ), "\n")
