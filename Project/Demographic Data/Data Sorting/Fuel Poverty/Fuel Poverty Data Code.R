
# This file produces the fuel poverty data for each general election from 2010 to 2024 for the new boundaries

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Function to convert fuel poverty data to new boundaries
fuel_converter <- function(Year){
  
  # Load in 2010 and 2024 results and boundary changes data
  results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
  results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
  boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
  boundary_change <- boundary_change[, c(3,5,10,11)]
  # Alter constituency names in boundary changes data
  boundary_change$Current.constituency.name <- gsub(',', '', boundary_change$Current.constituency.name)
  
  # Load 2010 fuel poverty data
  fuel_2010 <- read_excel('Fuel Poverty 2010 - boundaries 2010.xls', sheet = 'Parliamentary Constituency', skip = 2)
  
  # Load fuel poverty data for given year
  file_name <- paste('Fuel Poverty', Year, '- boundaries 2010.xlsx', sep = ' ')
  
  if (Year == 2010){
    
    file_name <- paste('Fuel Poverty', Year, '- boundaries 2010.xls', sep = ' ')
    sheet_name <- 'Parliamentary Constituency'
    
  } else if (Year == 2019) {
    
    sheet_name <- 'Table 4'
    
  } else{
    
    sheet_name <- 'Table 5'
    
  }
  
  fuel_data <- read_excel(file_name, sheet = sheet_name, skip = 2)
  
  if (!Year == 2010) {
    
    fuel_data <- fuel_data[1:533, ]
    
  } else {
    
    fuel_data$`PC Name` <- gsub(',', '', fuel_2010$`PC Name`)
    
  }
  
  names(fuel_data) <- names(fuel_2010)
  
  # Order fuel data by constituency and remove unnecessary columns
  fuel_data <- with(fuel_data, fuel_data[order(fuel_data$`PC Name`), ])
  fuel_data <- fuel_data[, c(2,3,6)]
  fuel_data$`Percent Fuel Poor` <- as.numeric(fuel_data$`Percent Fuel Poor`)
  
  # Create data frame for new boundaries
  fuel_data_new <- data.frame(matrix(nrow = 543, ncol = 3))
  names(fuel_data_new) <- c('Constituency', 'Region', 'Proportion')
  
  # Convert data to new boundaries
  for (i in 1:543) {
    
    # Load seat and region
    seat <- results_2024$Constituency[i]
    region <- results_2024$Region[i]
    
    # Find all old constituencies contained within new one
    old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
    old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
    
    # Get data for old constituencies
    fuel_data_subset <- fuel_data[fuel_data$`PC Name` %in% old_to_new$Current.constituency.name, ]
    
    # Compute data for new seat
    adjusted_fuel_data <- sum(fuel_data_subset[, 3] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.) / sum(old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
    
    # Update data frame
    fuel_data_new$Constituency[i] <- seat
    fuel_data_new$Region[i] <- region
    fuel_data_new$Proportion[i] <- adjusted_fuel_data
    
  }
  
  # Multiply by 100 for 2010 data
  if (Year == 2010) {
    
    fuel_data_new$Proportion <- fuel_data_new$Proportion * 100
    
  }
  
  return(fuel_data_new)
  
}

# Fuel poverty data for new boundaries for 2010 to 2019
fuel_2010 <- fuel_converter(2010)
fuel_2015 <- fuel_converter(2015)
fuel_2017 <- fuel_converter(2017)
fuel_2019 <- fuel_converter(2019)

# Load 2024 results
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Clean up fuel poverty data for 2024
fuel_2023 <- read_excel('Fuel Poverty 2023 - boundaries 2024.xlsx', sheet = 'Table 3', skip = 2)
fuel_2023 <- fuel_2023[fuel_2023$`Parliamentary Constituency` %in% results_2024$Constituency, ]
fuel_2023$`Region` <- results_2024$Region
fuel_2023$`Proportion of households fuel poor (%)` <- as.numeric(fuel_2023$`Proportion of households fuel poor (%)`)
fuel_2023 <- fuel_2023[, c(3, 7, 6)]

# Save fuel poverty data for new boundaries in an excel file
sheets = list('2010' = fuel_2010, '2015' = fuel_2015, '2017' = fuel_2017, '2019' = fuel_2019, 
              '2023' = fuel_2023)

write.xlsx(sheets, 'Fuel Poverty Data New Boundaries.xlsx', firstRow = TRUE, firstCol = TRUE)



