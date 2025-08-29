
# This file produces the ethnicity data for each general election from 2010 to 2024 by linear interpolation

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Load ethnicity data from 2011 and 2021 censuses
ethnicity <- read_excel('Ethnicity.xlsx', sheet = '2011 Comparison')

# Load the 2010 and 2024 results
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Remove unnecessary rows and constituencies
ethnicity <- ethnicity[ethnicity$`Broad Ethnic Groups` == 'White', ]
ethnicity <- ethnicity[ethnicity$ConstituencyName %in% results_2010$Constituency, ]

# Create a new data frame to store linear interpolation data
ethnic_int <- data.frame(matrix(nrow = 533, ncol = 5))

# Years we want linear interpolation for
years <- c(2010, 2015, 2017, 2019, 2024)
# Initialise counter
i <- 0
# Set up names for data frame
names(ethnic_int) <- years

# Perform linear interpolation
for (x in years) {
  
  # Add one to counter
  i <- i + 1
  # Compute linear interpolation values
  ethnic <- ethnicity$`Constituency 2011` + (ethnicity$`Constituency 2021` - ethnicity$`Constituency 2011`) / (2021 - 2011) * (x - 2011)  
  # Add linear interpolation values to data frame
  ethnic_int[, i] <- ethnic
  
}

# Create an empty data frame to store the linear interpolation values
ethnic_2 <- data.frame(matrix(nrow = 2665, ncol = 3))
names(ethnic_2) <- c('Constituency', 'Year', 'White')

# Add constituencies, years, and linear interpolation values
ethnic_2$Constituency <- rep(ethnicity$ConstituencyName, 5)
ethnic_2$Year <- rep(years, rep(533, 5))
ethnic_2$White <- c(ethnic_int$`2010`, ethnic_int$`2015`, ethnic_int$`2017`, ethnic_int$`2019`, ethnic_int$`2024`)

# Save the ethnicity data on old boundaries to an excel file
sheets = list('Ethnicity' = ethnic_2)
write.xlsx(sheets, 'Ethnicity Old Boundaries All Years.xlsx', firstRow = TRUE, firstCol = TRUE)


# Convert the data to the new boundaries
# Load in the boundary changes data
boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
boundary_change <- boundary_change[, c(3,5,10,11)]

# Create an empty data frame for ethnicity data on new boundaries
ethnic_new <- data.frame(matrix(nrow = 2715, ncol = 4))
names(ethnic_new) <- c('Constituency', 'Region', 'Year', 'White')

# Convert the data to the new boundaries
for (i in 1:543) {
  
  # Load seat and region
  seat <- results_2024$Constituency[i]
  region <- results_2024$Region[i]
  
  # Find all of the old constituencies contained within new seat
  old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
  old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
  
  # Find ethnicity data for old constituencies
  ethnic_3 <- ethnic_2[ethnic_2$Constituency %in% old_to_new$Current.constituency.name, ]
  
  # Convert data for new seat for each year
  for (j in 1:5) {
    
    # Find ethnicity data for single year and convert to new seat
    ethnic_4 <- ethnic_3[ethnic_3$Year == years[j], ]
    ethnic_4 <- sum(ethnic_4[, 3] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.) / sum(old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
    
    # Store converted data in data frame
    k <- 5 * i - 5 + j
    ethnic_new$Constituency[k] <- seat
    ethnic_new$Region[k] <- region
    ethnic_new$Year[k] <- years[j]
    ethnic_new$White[k] <- ethnic_4
    
  }
  
}

# Save ethnicity data on new boundaries to excel file
sheets = list('Ethnicity' = ethnic_new)
write.xlsx(sheets, 'Ethnicity New Boundaries All Years.xlsx', firstRow = TRUE, firstCol = TRUE)



