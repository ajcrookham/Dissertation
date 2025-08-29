
# This file compiles all of the data into a single data frame

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Load election results on new boundaries
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2024')

# Add year to results data frames
results_2010$Year <- 2010
results_2015$Year <- 2015
results_2017$Year <- 2017
results_2019$Year <- 2019
results_2024$Year <- 2024

# Combine results data frames into a single data frame and order by constituency
results <- rbind(results_2010, results_2015, results_2017, results_2019, results_2024)
results <- with(results, results[order(results$Constituency), ])

# Load and sort age data on new boundaries
age_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2010')
age_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2015')
age_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2017')
age_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2019')
age_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2024')

age <- rbind(age_2010, age_2015, age_2017, age_2019, age_2024)
age <- with(age, age[order(age$Constituency), ])
age <- age[, 3:7]

# Load and sort population densities
pop_density <- read.csv('~/Project/Demographic Data/Data Sorting/Population Density/Population Density New Boundaries.csv')
pop_density <- with(pop_density, pop_density[order(pop_density$Constituency), ])
pop_density <- pop_density[, 4]

# Load and sort qualifications data
qualifications <- read_excel('~/Project/Demographic Data/Data Sorting/Education/Qualifications New Boundaries All Years.xlsx', sheet = 'Qualifications')
qualifications <- qualifications[, 4:6]
qualifications <- data.frame(cbind(qualifications$`Level 4` / qualifications$Total, qualifications$No / qualifications$Total))
names(qualifications) <- c('Level 4 Prop', 'No Qualifications Prop')

# Load and sort unemployment data
unemployment <- read.csv('~/Project/Demographic Data/Data Sorting/Employment/Unemployment - 2024 boundaries.csv')
unemployment <- unemployment[, c(2,4,5)]
unemployment <- with(unemployment, unemployment[order(unemployment$ConstituencyName), ])

avg_unemployment <- rep(NA, 543 * 5)

for (i in 1:543) {
  
  avg_unemployment[5 * i - 3] <- mean(unemployment[(15 * i - 13):(15 * i - 9), 3])
  avg_unemployment[5 * i - 2] <- mean(unemployment[(15 * i - 8):(15 * i - 7), 3])
  avg_unemployment[5 * i - 1] <- mean(unemployment[(15 * i - 6):(15 * i - 5), 3])
  avg_unemployment[5 * i] <- mean(unemployment[(15 * i - 4):(15 * i), 3])
  
}

# Load and sort ethnicity data
ethnicity <- read_excel('~/Project/Demographic Data/Data Sorting/Ethnicity/Ethnicity New Boundaries All Years.xlsx', sheet = 'Ethnicity')
ethnicity <- ethnicity[, 4]

# Load and sort fuel poverty data
fuel_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Fuel Poverty/Fuel Poverty Data New Boundaries.xlsx', sheet = '2010')
fuel_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Fuel Poverty/Fuel Poverty Data New Boundaries.xlsx', sheet = '2015')
fuel_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Fuel Poverty/Fuel Poverty Data New Boundaries.xlsx', sheet = '2017')
fuel_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Fuel Poverty/Fuel Poverty Data New Boundaries.xlsx', sheet = '2019')
fuel_2023 <- read_excel('~/Project/Demographic Data/Data Sorting/Fuel Poverty/Fuel Poverty Data New Boundaries.xlsx', sheet = '2023')
names(fuel_2023) <- names(fuel_2010)

fuel <- rbind(fuel_2010, fuel_2015, fuel_2017, fuel_2019, fuel_2023)
fuel <- with(fuel, fuel[order(fuel$Constituency), 3])

# Load and sort home ownership data
home_own <- read_excel('~/Project/Demographic Data/Data Sorting/Home Ownership/Home Ownership New Boundaries All Years.xlsx', sheet = 'Home Ownership')
home_own <- home_own[, 4:7]
names(home_own) <- c('Owners', 'Private renters', 'Social renters', 'Total - Housing')

# Load and sort house price data
years <- c(2010, 2015, 2017, 2019, 2024)

home_prices <- read.csv('~/Project/Demographic Data/Data Sorting/House Prices/House Prices Data New Boundaries.csv')
home_prices <- home_prices[home_prices$Year %in% years, ]
home_prices <- with(home_prices, home_prices[order(home_prices$ConstituencyName), 4])
home_prices <- home_prices / 1000

# Load and sort income data
income_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2010')
income_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2015')
income_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2017')
income_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2019')
income_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2024')

names(income_2024) <- names(income_2019)

income <- rbind(income_2010, income_2015, income_2017, income_2019, income_2024)
income <- income[income$Constituency %in% results_2024$Constituency, ]
income <- with(income, income[order(Constituency), 2])
income <- income / 1000

inflation <- read.csv('~/Project/Demographic Data/Data Sorting/Inflation/Inflation.csv', skip = 7)
names(inflation) <- c('Year', 'Inflation')
inflation <- inflation[23:36, ]

inflation_2015 <- mean(inflation[1:5, 2])
inflation_2017 <- mean(inflation[6:7, 2])
inflation_2019 <- mean(inflation[8:9, 2])
inflation_2024 <- mean(inflation[10:14, 2])

inflation$Inflation <- inflation$Inflation / 100 + 1
inflation_2015_2 <- (prod(inflation[1:5, 2]) - 1) * 100 - (1.02 ** 5 - 1) * 100
inflation_2017_2 <- (prod(inflation[6:7, 2]) - 1) * 100 - (1.02 ** 2 - 1) * 100
inflation_2019_2 <- (prod(inflation[8:9, 2]) - 1) * 100 - (1.02 ** 2 - 1) * 100
inflation_2024_2 <- (prod(inflation[10:14, 2]) - 1) * 100 - (1.02 ** 5 - 1) * 100

inflation <- rep(c(NA, inflation_2015_2, inflation_2017_2, inflation_2019_2, inflation_2024_2), 543)

# Load and sort child poverty data
child_poverty <- read.csv('~/Project/Demographic Data/Data Sorting/Child Poverty/Child Poverty Sorted.csv')
child_poverty_2015 <- child_poverty[child_poverty$Year == 2015, c(2, 5)]
child_poverty_2017 <- child_poverty[child_poverty$Year == 2017, c(2, 5)]
child_poverty_2019 <- child_poverty[child_poverty$Year == 2019, c(2, 5)]
child_poverty_2024 <- child_poverty[child_poverty$Year == 2024, c(2, 5)]

child_poverty_2010 <- data.frame(matrix(nrow = 543, ncol = 0))
child_poverty_2010$Constituency <- child_poverty_2015$ConstituencyName
child_poverty_2010$Rate <- rep(NA, 543)

names(child_poverty_2015) <- names(child_poverty_2010)
names(child_poverty_2017) <- names(child_poverty_2010)
names(child_poverty_2019) <- names(child_poverty_2010)
names(child_poverty_2024) <- names(child_poverty_2010)

child_poverty <- rbind(child_poverty_2010, child_poverty_2015, child_poverty_2017, child_poverty_2019, child_poverty_2024)
child_poverty <- with(child_poverty, child_poverty[order(Constituency), 2])

# Combine all of the data into a single data frame and save it
all_data <- cbind(results, age, pop_density, qualifications, avg_unemployment, ethnicity, fuel, home_own, home_prices, income, inflation, child_poverty)

sheets = list('Data' = all_data)

write.xlsx(sheets, 'All Election Data.xlsx', firstRow = TRUE, firstCol = TRUE)





