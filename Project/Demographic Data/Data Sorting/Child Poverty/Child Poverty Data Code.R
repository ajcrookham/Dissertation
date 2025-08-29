
# This file cleans up the child poverty data

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Load in the child poverty raw data and 2024 election results
child_poverty <- read_excel('Child Poverty.xlsx', sheet = 'Constituencies')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Add Year column to child poverty data
child_poverty$Year <- rep(2015:2024, rep(650, 10))

# Vector of the years we want to keep
years <- c(2015, 2017, 2019, 2024)

# Remove unnecessary columns and constituencies
child_poverty <- child_poverty[child_poverty$ConstituencyName %in% results_2024$Constituency, c(2, 4, 9, 13, 21)]

# Remove unnecessary years
child_poverty_sorted <- child_poverty[child_poverty$Year %in% years, ]

# Save cleaned up data into CSV file
write.csv(child_poverty_sorted, 'Child Poverty Sorted.csv')


