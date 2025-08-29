
# This file produces the median house price data for each general election from 2010 to 2024

# Load required packages
library(readxl)

# Set working directory to Source File location

# Load original house price data and 2024 results
house_data <- read_excel('House Prices Raw Data.xlsx', sheet = 'Constituency data')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Update constituency names in house price data and remove unnecessary constituencies and columns
house_data$ConstituencyName <- gsub('&', 'and', house_data$ConstituencyName)
house_data <- house_data[house_data$ConstituencyName %in% results_2024$Constituency, c(2,4,8,9,10,11)]

# Dates we want in our sorted data
dates <- c('2010-06-30', '2011-06-30', '2012-06-30', '2013-06-30', '2014-06-30', '2015-06-30',
           '2016-06-30', '2017-06-30', '2018-06-01', '2019-06-01', '2020-06-01', '2021-06-01',
           '2022-06-01', '2023-06-01', '2024-06-01')

# Update dates in raw data to be characters
house_data$DateOfDataset <- as.character(house_data$DateOfDataset)

# Remove unwanted dates
house_data <- house_data[house_data$DateOfDataset %in% dates, ]

# Adjust median house prices to 2024 level
median_2024 <- house_data$HouseCountryMedianPrice[8145]
house_data$Ratio <- median_2024 / house_data$HouseCountryMedianPrice
house_data$AdjustedMedianHousePrice <- as.numeric(house_data$HouseConstMedianPrice) * house_data$Ratio

# Add year column to data
house_data$Year <- rep(2010:2024, rep(543, 15))

# Remove unnecessary columns
house_data <- house_data[, c(1, 2, 8, 9)]

# Save sorted house price data
write.csv(house_data, 'House Prices Data New Boundaries.csv')

