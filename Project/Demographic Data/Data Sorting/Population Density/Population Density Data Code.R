
# This file produces the population density data for each general election from 2010 to 2024 on the new boundaries

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Function that computes population density for each new constituency from 2010 to 2019
pop_density_converter <- function(Year) {
  
  # Load in 2010 and 2024 results and the boundary changes
  results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
  results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
  boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
  boundary_change <- boundary_change[, c(3, 5, 10, 11)]
  
  # Load data on size of constituencies and remove unnecessary constituencies and columns
  area <- read.csv('Area of New Boundaries.csv')
  area <- area[area$PCON24NM %in% results_2024$Constituency, c(3, 9)]
  area$Shape__Area <- area$Shape__Area / 1000000
  
  # Set up file name and sheet name to load in raw age data for given year
  file_name <- paste('~/Project/Demographic Data/Data Sorting/Age/', Year, ' Age Data.xls', sep = '')
  
  if (Year == 2019) {
    
    file_name <- paste('~/Project/Demographic Data/Data Sorting/Age/', Year, ' Age Data.xlsx', sep = '')
    
  }
  
  sheet_name <- paste('Mid-', Year, ' Persons', sep = '')
  # Load in the raw age data for given year
  age_data <- read_excel(file_name, sheet = sheet_name, skip = 3)
  
  # Create data frame for number of people in new constituencies
  age_data_new <- data.frame(matrix(nrow = 543, ncol = 2))
  names(age_data_new) <- c('Constituency', 'Total')
  
  if (!Year == 2010) {
    
    # Remove unnecessary constituencies
    age_data <- age_data[age_data$PCON11NM %in% results_2010$Constituency,]
    
    # Convert populations to new constituencies
    for (i in 1:543) {
      
      # Load seat
      seat <- results_2024$Constituency[i]
      
      # Find all old constituencies within new seat
      old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
      old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
      
      # Get data for old constituencies
      age_data_subset <- age_data[age_data$PCON11NM %in% old_to_new$Current.constituency.name, ]
      
      # Compute total population for new seat
      adjusted_age_data <- colSums(age_data_subset[, 3] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
      
      # Update data frame
      age_data_new$Constituency[i] <- seat
      age_data_new[i, 2] <- round(adjusted_age_data, 0)
      
    }
    
  } else {
    
    # Remove unnecessary constituencies
    age_data <- age_data[age_data$...2 %in% results_2010$Constituency, ]
    
    # Convert populations to new constituencies
    for (i in 1:543) {
      
      # Load seat
      seat <- results_2024$Constituency[i]
      
      # Find all old constituencies within new seat
      old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
      old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
      
      # Get data for old constituencies
      age_data_subset <- age_data[age_data$...2 %in% old_to_new$Current.constituency.name, ]
      
      # Compute total population for new seat
      adjusted_age_data <- colSums(age_data_subset[, 3] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
      
      # Update data frame
      age_data_new$Constituency[i] <- seat
      age_data_new[i, 2] <- round(adjusted_age_data, 0)
      
    }
    
  }
  
  # Compute population density for each constituency in new boundaries
  pop_density <- age_data_new$Total / area$Shape__Area
  
  return(pop_density)
  
}

# Compute population densities for 2010 to 2019 on new boundaries
pop_density_2010 <- pop_density_converter(2010)
pop_density_2015 <- pop_density_converter(2015)
pop_density_2017 <- pop_density_converter(2017)
pop_density_2019 <- pop_density_converter(2019)

# Load 2024 results and 2024 age data and remove unnecessary constituencies
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
age_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2022 Age Data.xlsx', sheet = 'Mid-2022 PCON 2025', skip = 3)
age_2024 <- age_2024[age_2024$`PCON 2025 Name` %in% results_2024$Constituency, ]

# Load area data for new constituencies
area <- read.csv('Area of New Boundaries.csv')
area <- area[area$PCON24NM %in% results_2024$Constituency, c(3, 9)]
area$Shape__Area <- area$Shape__Area / 1000000

# Compute population density for 2024
pop_density_2024 <- age_2024$Total / area$Shape__Area

# Create data frame to store all the population densities
pop_density <- data.frame(matrix(nrow = 543 * 5, ncol = 4))
names(pop_density) <- c('Constituency', 'Year', 'Population density')

# Add data to population density data frames
pop_density$Constituency <- rep(results_2024$Constituency, 5)
pop_density$Year <- rep(c(2010, 2015, 2017, 2019, 2024), rep(543, 5))
pop_density$`Population density` <- c(pop_density_2010, pop_density_2015, pop_density_2017, pop_density_2019, pop_density_2024)

# Save population density to CSV file
write.csv(pop_density, 'Population Density New Boundaries.csv')
