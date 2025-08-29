
# This file produces the income data for each general election from 2010 to 2024 on the old and new boundaries

# Load required packages
library(readxl)
library(readODS)
library(openxlsx)

# Set working directory to Source File location

# Function that converts income data to 2024 boundaries
income_converter <- function(Year, Old_Boundaries = TRUE) {
  
  # Load 2010 and 2024 results and boundary changes data
  results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
  results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
  boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
  boundary_change <- boundary_change[, c(3, 5, 10, 11)]
  
  # National median income for 2024
  median_2024 <- 37430
  
  # Load and update data for 2010
  if (Year == 2010) {
    
    # Load data
    income_data <- read_excel('Income and Tax 2010 - boundaries 2010.xls', skip = 6)
    # Remove unnecessary constituencies and columns
    income_data <- income_data[income_data$`Parliamentary Constituency` %in% results_2010$Constituency, c(1, 18)]
    # Reorder constituencies
    income_data <- with(income_data, income_data[order(income_data$`Parliamentary Constituency`), ])
    # Set income column to be numeric
    income_data$...18 <- as.numeric(income_data$...18)
    
    # Adjust median incomes to 2024 level
    median <- 25882
    ratio <- median_2024 / median
    income_data$...18 <- income_data$...18 * ratio
    
    # Load and update data for 2015
  } else if (Year == 2015) {
    
    # Load data
    income_data <- read_excel('Income and Tax 2015 - boundaries 2010.xlsx', skip = 6)
    # Remove unnecessary constituencies and columns
    income_data <- income_data[income_data$`Parliamentary Constituency` %in% results_2010$Constituency, c(1, 18)]
    # Reorder constituencies
    income_data <- with(income_data, income_data[order(income_data$`Parliamentary Constituency`), ])
    # Set income column to be numeric
    income_data$...18 <- as.numeric(income_data$...18)
    
    # Adjust median incomes to 2024 level
    median <- 27615
    ratio <- median_2024 / median
    income_data$...18 <- income_data$...18 * ratio
    
    # Load and update data for 2017
  } else if (Year == 2017) {
    
    # Load data
    income_data <- read_excel('Income and Tax 2017 - boundaries 2010.xlsx', skip = 6)
    # Remove unnecessary constituencies and columns
    income_data <- income_data[income_data$`Parliamentary Constituency` %in% results_2010$Constituency, c(2, 19)]
    # Reorder constituencies
    income_data <- with(income_data, income_data[order(income_data$`Parliamentary Constituency`), ])
    # Set income column to be numeric
    income_data$...19 <- as.numeric(income_data$...19)
    
    # Adjust median incomes to 2024 level
    median <- 28759
    ratio <- median_2024 / median
    income_data$...19 <- income_data$...19 * ratio
    
    # Load and update data for 2019
  } else {
    
    # Load data
    income_data <- read_ods('Income and Tax 2019 - boundaries 2010.ods', sheet = 'Table_3_15', skip = 4)
    # Remove unnecessary constituencies and columns
    income_data <- income_data[income_data$`Parliamentary Constituency` %in% results_2010$Constituency, c(2, 14)]
    # Reorder constituencies
    income_data <- with(income_data, income_data[order(income_data$`Parliamentary Constituency`), ])
    
    # Adjust median incomes to 2024 level
    median <- 30378
    ratio <- median_2024 / median
    income_data$`Total income: Median` <- income_data$`Total income: Median` * ratio
    
  }
  
  # Update column names of income data
  names(income_data) <- c('Constituency', 'Adjusted Median Income')
  
  if (Old_Boundaries) {
    
    return(income_data)
    
  }
  
  # Create data frame for income data for new boundaries
  income_data_new <- data.frame(matrix(nrow = 543, ncol = 2))
  names(income_data_new) <- names(income_data)
  
  # Convert income data to new boundaries
  for (i in 1:543) {
    
    # Load new seat
    seat <- results_2024$Constituency[i]
    
    # Find all old constituencies contained within new seat
    old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
    old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
    
    # Get income data for all old constituencies
    income_data_subset <- income_data[income_data$Constituency %in% old_to_new$Current.constituency.name, ]
    
    # Compute adjusted income data
    adjusted_income_data <- sum(income_data_subset[, 2] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.) / sum(old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
    
    # Update data frame
    income_data_new$Constituency[i] <- seat
    income_data_new$`Adjusted Median Income`[i] <- adjusted_income_data
    
  }
  
  return(income_data_new)
  
}

# Sort income data for old boundaries from 2010 to 2019
income_2010 <- income_converter(2010)
income_2015 <- income_converter(2015)
income_2017 <- income_converter(2017)
income_2019 <- income_converter(2019)

# Save income data for old boundaries to excel file
sheets = list('2010' = income_2010, '2015' = income_2015, '2017' = income_2017, '2019' = income_2019)
write.xlsx(sheets, 'Income Data Old Boundaries.xlsx', firstRow = TRUE, firstCol = TRUE)


# Compute income data for new boundaries from 2010 to 2019
income_2010_new <- income_converter(2010, Old_Boundaries = FALSE)
income_2015_new <- income_converter(2015, Old_Boundaries = FALSE)
income_2017_new <- income_converter(2017, Old_Boundaries = FALSE)
income_2019_new <- income_converter(2019, Old_Boundaries = FALSE)

# Load 2024 results and 2023 income and remove unnecessary constituencies and columns and reorder the rows
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
income_2024 <- read_ods('Income and Tax 2023 - boundaries 2024.ods', sheet = 'Table_3_15', skip = 4)
income_2024 <- income_2024[income_2024$`Parliamentary Constituency` %in% results_2024$Constituency, c(2, 14)]
income_2024 <- with(income_2024, income_2024[order(income_2024$`Parliamentary Constituency`), ])
names(income_2024) <- c('Constituency', 'Adjusted Median Income')

# Save income data for new boundaries to excel file
sheets = list('2010' = income_2010_new, '2015' = income_2015_new, '2017' = income_2017_new, '2019' = income_2019_new, 
              '2024' = income_2024)
write.xlsx(sheets, 'Income Data New Boundaries.xlsx', firstRow = TRUE, firstCol = TRUE)

