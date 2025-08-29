
# This file produces the home ownership data for each election from 2010 to 2024 by linear interpolation for the new boundaries

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Load 2019 results and home ownership 2011 and 2021 census data
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
home <- read_excel('Home Ownership 2011 - Old Boundaries.xlsx', sheet = 'Comparison')

# Remove unnecessary constituencies and columsn
home <- home[home$Constituency %in% results_2019$Constituency, c(2,4,6,7,9,10,12)]

# Create data frames for home owners, private renters, and social renters
own <- home[home$`Home ownership` == 'Home owners', ]
pri <- home[home$`Home ownership` == 'Private renters', ]
soc <- home[home$`Home ownership` == 'Social renters', ]

# Create empty data frames for linear interpolation values
own_all <- data.frame(matrix(nrow = 533, ncol = 5))
pri_all <- data.frame(matrix(nrow = 533, ncol = 5))
soc_all <- data.frame(matrix(nrow = 533, ncol = 5))

# List of years we want data for
years <- c(2010, 2015, 2017, 2019, 2024)
# Initialise counter
i <- 0

names(own_all) <- years
names(pri_all) <- years
names(soc_all) <- years

# Compute linear interpolation values and update data frames
for (x in years) {
  
  i <- i + 1
  
  own_int <- own$`Constituency total 2011` + (own$`Constituency total` - own$`Constituency total 2011`) / (2021 - 2011) * (x - 2011)  
  pri_int <- pri$`Constituency total 2011` + (pri$`Constituency total` - pri$`Constituency total 2011`) / (2021 - 2011) * (x - 2011)  
  soc_int <- soc$`Constituency total 2011` + (soc$`Constituency total` - soc$`Constituency total 2011`) / (2021 - 2011) * (x - 2011)
  
  own_all[, i] <- round(own_int, 0)
  pri_all[, i] <- round(pri_int, 0)
  soc_all[, i] <- round(soc_int, 0)
  
}

# Create data frame to store all of the home ownership data
housing <- data.frame(matrix(nrow = 2665, ncol = 6))

names(housing) <- c('Constituency', 'Year', 'Owners', 'Private renters', 'Social renters', 'Total')

# Fill data frame with constituencies, years, and linear interpolation values
housing$Constituency <- rep(own$Constituency, 5)
housing$Year <- rep(years, rep(533, 5))
housing$Owners <- c(own_all$`2010`, own_all$`2015`, own_all$`2017`, own_all$`2019`, own_all$`2024`)
housing$`Private renters` <- c(pri_all$`2010`, pri_all$`2015`, pri_all$`2017`, pri_all$`2019`, pri_all$`2024`)
housing$`Social renters` <- c(soc_all$`2010`, soc_all$`2015`, soc_all$`2017`, soc_all$`2019`, soc_all$`2024`)
housing$Total <- housing$Owners + housing$`Private renters` + housing$`Social renters`

# Save home ownership data for old boundaries in an excel file
sheets = list('Home Ownership' = housing)
write.xlsx(sheets, 'Home Ownership Old Boundaries All Years.xlsx', firstRow = TRUE, firstCol = TRUE)

# Convert data to new boundaries
# Load 2010 and 2024 results and boundary changes data
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
boundary_change <- boundary_change[, c(3, 5, 10, 11)]

# Create data frame for home ownership data on new boundaries
housing_new <- data.frame(matrix(nrow = 2715, ncol = 7))
names(housing_new) <- c('Constituency', 'Region', 'Year', 'Owners', 'Private renters', 'Social renters', 'Total')

# Convert data to new boundaries
for (i in 1:543) {
  
  # Load seat and region
  seat <- results_2024$Constituency[i]
  region <- results_2024$Region[i]
  
  # Find all old constituencies contained within new seat
  old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
  old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
  
  # Get the home ownership data for old constituencies in new seat
  home_2 <- housing[housing$Constituency %in% old_to_new$Current.constituency.name, ]
  
  # Convert to new boundaries for each election
  for (j in 1:5) {
    
    # Compute adjusted values
    home_3 <- home_2[home_2$Year == years[j], ]
    home_3 <- colSums(home_3[, 3:5] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
    
    # Store adjusted values in data frame
    k <- 5 * i - 5 + j
    housing_new$Constituency[k] <- seat
    housing_new$Region[k] <- region
    housing_new$Year[k] <- years[j]
    housing_new$Owners[k] <- round(home_3[1], 0)
    housing_new$`Private renters`[k] <- round(home_3[2], 0)
    housing_new$`Social renters`[k] <- round(home_3[3], 0)
    
  }
  
}

# Add total to data frame
housing_new$Total <- housing_new$Owners + housing_new$`Private renters` + housing_new$`Social renters`

# Save home ownership data for new boundaries to excel file
sheets = list('Home Ownership' = housing_new)
write.xlsx(sheets, 'Home Ownership New Boundaries All Years.xlsx', firstRow = TRUE, firstCol = TRUE)



