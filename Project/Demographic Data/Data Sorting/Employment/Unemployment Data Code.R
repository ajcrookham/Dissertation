
# This file produces the unemployment data on the new electoral map

# Load required packages
library(readxl)

# Set working directory to Source File location

# Load unemployment data
unemployment <- read_excel('Unemployment - 2010 boundaries.xlsx', sheet = 'Data')

# Load 2010 and 2024 results and boundary changes data
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
boundary_change <- boundary_change[, c(3,5,10,11)]

# Remove unnecessary constituencies and columns, and convert dates to characters
unemployment <- unemployment[unemployment$ConstituencyName %in% results_2010$Constituency, ]
unemployment <- unemployment[, c(2, 4, 8, 10, 12, 14)]
unemployment$DateOfDataset <- as.character(unemployment$DateOfDataset)

# List the dates we want
dates <- c('2010-05-01', '2011-05-01', '2012-05-01', '2013-05-01', '2014-05-03', '2015-05-01',
           '2016-05-01', '2017-05-01', '2018-05-01', '2019-05-01', '2020-05-01', '2021-05-01',
           '2022-05-01', '2023-05-01', '2024-05-01')

# Remove unwanted dates from data frame
unemployment <- unemployment[unemployment$DateOfDataset %in% dates, ]

# Create a data frame to store the new electoral map data
unemployment_new <- data.frame(matrix(nrow = 543 * 15, ncol = 6))
names(unemployment_new) <- names(unemployment)

# Convert data to new boundaries
for (j in 1:15) {
  
  for (i in 1:543) {
    
    # Load seat and region
    seat <- results_2024$Constituency[i]
    region <- results_2024$Region[i]
    
    # Find all old constituencies that are contained within new constituency
    old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
    old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
    
    # Find data for old constituencies and correct year
    unemployment_2 <- unemployment[unemployment$ConstituencyName %in% old_to_new$Current.constituency.name, ]
    unemployment_2 <- unemployment_2[unemployment_2$DateOfDataset == dates[j],]
    
    # Convert data to new boundaries
    unemployment_3 <- colSums(unemployment_2[, 4] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.) / sum(old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
    
    # Store converted data to new data frame
    k <- 543 * (j - 1) + i
    unemployment_new$UnempConstRate[k] <- unemployment_3
    unemployment_new$ConstituencyName[k] <- seat
    unemployment_new$RegionName[k] <- region
    
    # Add regional rate and national rate to new data frame
    region_rate <- as.numeric(unemployment_2[unemployment_2$RegionName == region, 5][1,1])
    nation_rate <- as.numeric(unemployment_2$UnempCountryRate[1])
    unemployment_new$UnempRegionRate[k] <- region_rate
    unemployment_new$UnempCountryRate[k] <- nation_rate
    
    # Add date to new data frame
    unemployment_new$DateOfDataset[k] <- dates[j]
              
  }
  
}

# List of years we want instead of the dates
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)

# Convert dates to year in new data frame
for (i in 1:15) {
  
  for (j in 1:8145){
    
    year <- as.character(years[i])
    
    if (grepl(year, unemployment_new$DateOfDataset[j])){
      
      unemployment_new$DateOfDataset[j] <- years[i] 
      
    }
    
  }
  
}

# Save new boundaries data frame to a CSV file
write.csv(unemployment_new, 'Unemployment - 2024 boundaries.csv')

