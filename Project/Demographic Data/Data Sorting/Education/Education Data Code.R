
# This file produces the education data for each general election from 2010 to 2024 by linear interpolation

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Sort and clean up the census data for 2011 and 2021
# Load census data and 2019 results for the constituency names
census_2011 <- read_excel('Qualification Census 2011.xlsx', sheet = 'Data', skip = 8)
census_2021 <- read_excel('Qualification Census 2021 - 2010 Boundaries.xlsx', sheet = 'Dataset')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')

# Remove unnecessary constituencies
census_2011 <- census_2011[census_2011$`parliamentary constituency 2010` %in% results_2019$Constituency, ]
census_2021 <- census_2021[census_2021$`Westminster Parliamentary constituencies` %in% results_2019$Constituency, ]

# Reorder the constituencies
census_2011 <- with(census_2011, census_2011[order(census_2011$`parliamentary constituency 2010`), ])

# Add columns to 2021 to compute the total number of people in each constituency and the proportions
census_2021$Proportion <- rep(0, 533 * 8)
census_2021$Total <- rep(0, 533 * 8)

# Compute totals and proportions for 2021 census
for (i in 1:533) {
  
  total <- sum(census_2021$Observation[(8*i - 6):(8*i)])
  census_2021$Total[(8*i - 6):(8*i)] <- rep(total, 7) 
  
  for (j in 1:7) {
    
    prop <- census_2021$Observation[8*i - 7 + j] / total
    census_2021$Proportion[8*i - 7 + j] <- prop
    
  }
  
}

# Get data for no qualifications and degrees
level_4_2021 <- census_2021[census_2021$`Highest level of qualification (8 categories) Code` == 5, ]
no_qual_2021 <- census_2021[census_2021$`Highest level of qualification (8 categories) Code` == 0, ]

# Create a new data frame to store data for just the degree and no qualifications data
census_2021_2 <- data.frame(matrix(nrow = 533, ncol = 0))
# Fill new data frame with relevant information
census_2021_2$Constituency <- level_4_2021$`Westminster Parliamentary constituencies`
census_2021_2$`Total` <- level_4_2021$Total
census_2021_2$`Level 4` <- level_4_2021$Observation
census_2021_2$`No Qualifications` <- no_qual_2021$Observation

# Create empty data frames for data on no qualifications, degrees, and totals for each years
no_qual_all <- data.frame(matrix(nrow = 533, ncol = 6))
level_4_all <- data.frame(matrix(nrow = 533, ncol = 6))
total_all <- data.frame(matrix(nrow = 533, ncol = 6))

# Years we want interpolation for
years <- c(2010, 2015, 2017, 2019, 2024)
# Initialise counter
i <- 1

# Set up names for data frames
names(no_qual_all) <- c('Constituency', years)
names(level_4_all) <- c('Constituency', years)
names(total_all) <- c('Constituency', years)

# Add constituencies to data frames
no_qual_all$Constituency <- level_4_2021$`Westminster Parliamentary constituencies`
level_4_all$Constituency <- level_4_2021$`Westminster Parliamentary constituencies`
total_all$Constituency <- level_4_2021$`Westminster Parliamentary constituencies`

# Perform linear interpolation for each year
for (x in years) {
  
  i <- i + 1
  
  no_qual <- census_2011$`No qualifications` + (census_2021_2$`No Qualifications` - census_2011$`No qualifications`) / (2021 - 2011) * (x - 2011)  
  level_4 <- census_2011$`Level 4 qualifications and above` + (census_2021_2$`Level 4` - census_2011$`Level 4 qualifications and above`) / (2021 - 2011) * (x - 2011)  
  total <- census_2011$`All categories: Highest level of qualification` + (census_2021_2$Total - census_2011$`All categories: Highest level of qualification`) / (2021 - 2011) * (x - 2011)
  
  # Round linear interpolation values and add to data frames  
  no_qual_all[, i] <- round(no_qual, 0)
  level_4_all[, i] <- round(level_4, 0)
  total_all[, i] <- round(total, 0)
  
}

# Create an overall data frame for education data on old boundaries
qualifications <- data.frame(matrix(nrow = 2665, ncol = 5))

# Set up names of data frame
names(qualifications) <- c('Constituency', 'Year', 'No Qualifications', 'Level 4', 'Total')

# Add education data for each year to data frame
qualifications$Constituency <- rep(level_4_2021$`Westminster Parliamentary constituencies`, 5)
qualifications$Year <- rep(years, rep(533, 5))
qualifications$`No Qualifications` <- c(no_qual_all$`2010`, no_qual_all$`2015`, no_qual_all$`2017`, no_qual_all$`2019`, no_qual_all$`2024`)
qualifications$`Level 4` <- c(level_4_all$`2010`, level_4_all$`2015`, level_4_all$`2017`, level_4_all$`2019`, level_4_all$`2024`)
qualifications$Total <- c(total_all$`2010`, total_all$`2015`, total_all$`2017`, total_all$`2019`, total_all$`2024`)

# Save qualifications on old boundaries to an excel file
sheets = list('Qualifications' = qualifications)
write.xlsx(sheets, 'Qualifications Old Boundaries All Years.xlsx', firstRow = TRUE, firstCol = TRUE)

# Load 2010 and 2024 results and boundary changes data
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
boundary_change <- boundary_change[, c(3,5,10,11)]

# Create a new data frame for education data on new boundaries and set up names for data frame
qualifications_new <- data.frame(matrix(nrow = 2715, ncol = 6))
names(qualifications_new) <- c('Constituency', 'Region', 'Year', 'Total', 'Level 4', 'No')

# Convert education data to new boundaries
for (i in 1:543) {
  
  # Load seat name and region
  seat <- results_2024$Constituency[i]
  region <- results_2024$Region[i]
  
  # Find all of the old constituencies that are contained within the new seat
  old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
  old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
  
  # Get the education data for old constituencies
  qual_2 <- qualifications[qualifications$Constituency %in% old_to_new$Current.constituency.name, ]
  
  for (j in 1:5) {
    
    # Get individual year data and convert to new boundaries  
    qual_3 <- qual_2[qual_2$Year == years[j], ]
    qual_3 <- colSums(qual_3[, 3:5] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
    
    # Store new boundaries data to new data frame
    k <- 5 * i - 5 + j
    qualifications_new$Constituency[k] <- seat
    qualifications_new$Region[k] <- region
    qualifications_new$Year[k] <- years[j]
    qualifications_new$Total[k] <- round(qual_3[3], 0)
    qualifications_new$`Level 4`[k] <- round(qual_3[2], 0)
    qualifications_new$No[k] <- round(qual_3[1], 0)
    
  }
  
}

# Save education data for new boundaries to an excel file
sheets = list('Qualifications' = qualifications_new)
write.xlsx(sheets, 'Qualifications New Boundaries All Years.xlsx', firstRow = TRUE, firstCol = TRUE)

