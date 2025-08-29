
# This file cleans up the raw data file for boundary changes by removing unnecessary constituencies and columns

# Load required packages
library(readxl)

# Set working directory to Source File location

# Load in raw data file for boundary changes
Boundary_changes_data_file <- read_excel("Boundary_changes_data_file.xlsx", sheet = "All overlaps", skip = 1)

# Load in the 2019 General Election results
Election_Results <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')

# Remove any unnecessary constituencies from boundary changes file
Boundary_changes <- Boundary_changes_data_file[Boundary_changes_data_file$`Current constituency name` %in% Election_Results$Constituency, ]

# Save the cleaned up boundary changes data as a CSV file
write.csv(Boundary_changes, 'Boundary Changes.csv')

