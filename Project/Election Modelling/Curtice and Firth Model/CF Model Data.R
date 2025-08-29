
# This file produces a data frame for the CF model

# Load required packages
library(readxl)

# Set working directory to Source File location

# Load general election results on new boundaries
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2024')

# Compute vote shares with votes for Other removed for each major party for each election
results_2010$`Con Share` <- results_2010$Con / (results_2010$Total - results_2010$Other) * 100
results_2010$`Lab Share` <- results_2010$Lab / (results_2010$Total - results_2010$Other) * 100
results_2010$`LD Share` <- results_2010$LD / (results_2010$Total - results_2010$Other) * 100
results_2010$`RFM Share` <- results_2010$RFM / (results_2010$Total - results_2010$Other) * 100
results_2010$`GRN Share` <- results_2010$Green / (results_2010$Total - results_2010$Other) * 100

results_2015$`Con Share` <- results_2015$Con / (results_2015$Total - results_2015$Other) * 100
results_2015$`Lab Share` <- results_2015$Lab / (results_2015$Total - results_2015$Other) * 100
results_2015$`LD Share` <- results_2015$LD / (results_2015$Total - results_2015$Other) * 100
results_2015$`RFM Share` <- results_2015$RFM / (results_2015$Total - results_2015$Other) * 100
results_2015$`GRN Share` <- results_2015$Green / (results_2015$Total - results_2015$Other) * 100

results_2017$`Con Share` <- results_2017$Con / (results_2017$Total - results_2017$Other) * 100
results_2017$`Lab Share` <- results_2017$Lab / (results_2017$Total - results_2017$Other) * 100
results_2017$`LD Share` <- results_2017$LD / (results_2017$Total - results_2017$Other) * 100
results_2017$`RFM Share` <- results_2017$RFM / (results_2017$Total - results_2017$Other) * 100
results_2017$`GRN Share` <- results_2017$Green / (results_2017$Total - results_2017$Other) * 100

results_2019$`Con Share` <- results_2019$Con / (results_2019$Total - results_2019$Other) * 100
results_2019$`Lab Share` <- results_2019$Lab / (results_2019$Total - results_2019$Other) * 100
results_2019$`LD Share` <- results_2019$LD / (results_2019$Total - results_2019$Other) * 100
results_2019$`RFM Share` <- results_2019$RFM / (results_2019$Total - results_2019$Other) * 100
results_2019$`GRN Share` <- results_2019$Green / (results_2019$Total - results_2019$Other) * 100

results_2024$`Con Share` <- results_2024$Con / (results_2024$Total - results_2024$Other) * 100
results_2024$`Lab Share` <- results_2024$Lab / (results_2024$Total - results_2024$Other) * 100
results_2024$`LD Share` <- results_2024$LD / (results_2024$Total - results_2024$Other) * 100
results_2024$`RFM Share` <- results_2024$RFM / (results_2024$Total - results_2024$Other) * 100
results_2024$`GRN Share` <- results_2024$Green / (results_2024$Total - results_2024$Other) * 100

# Compute national vote shares in England for each major party for each election
con_2010 <- sum(results_2010$Con) / sum(results_2010$Total) * 100
con_2015 <- sum(results_2015$Con) / sum(results_2015$Total) * 100
con_2017 <- sum(results_2017$Con) / sum(results_2017$Total) * 100
con_2019 <- sum(results_2019$Con) / sum(results_2019$Total) * 100
con_2024 <- sum(results_2024$Con) / sum(results_2024$Total) * 100

lab_2010 <- sum(results_2010$Lab) / sum(results_2010$Total) * 100
lab_2015 <- sum(results_2015$Lab) / sum(results_2015$Total) * 100
lab_2017 <- sum(results_2017$Lab) / sum(results_2017$Total) * 100
lab_2019 <- sum(results_2019$Lab) / sum(results_2019$Total) * 100
lab_2024 <- sum(results_2024$Lab) / sum(results_2024$Total) * 100

ld_2010 <- sum(results_2010$LD) / sum(results_2010$Total) * 100
ld_2015 <- sum(results_2015$LD) / sum(results_2015$Total) * 100
ld_2017 <- sum(results_2017$LD) / sum(results_2017$Total) * 100
ld_2019 <- sum(results_2019$LD) / sum(results_2019$Total) * 100
ld_2024 <- sum(results_2024$LD) / sum(results_2024$Total) * 100

rfm_2010 <- sum(results_2010$RFM) / sum(results_2010$Total) * 100
rfm_2015 <- sum(results_2015$RFM) / sum(results_2015$Total) * 100
rfm_2017 <- sum(results_2017$RFM) / sum(results_2017$Total) * 100
rfm_2019 <- sum(results_2019$RFM) / sum(results_2019$Total) * 100
rfm_2024 <- sum(results_2024$RFM) / sum(results_2024$Total) * 100

grn_2010 <- sum(results_2010$Green) / sum(results_2010$Total) * 100
grn_2015 <- sum(results_2015$Green) / sum(results_2015$Total) * 100
grn_2017 <- sum(results_2017$Green) / sum(results_2017$Total) * 100
grn_2019 <- sum(results_2019$Green) / sum(results_2019$Total) * 100
grn_2024 <- sum(results_2024$Green) / sum(results_2024$Total) * 100


# Compute responses for 2015
response_2015 <- data.frame(matrix(nrow = 543, ncol = 6))
names(response_2015) <- c('Constituency', 'Region', 'y_1', 'y_2', 'y_3', 'y_4')

response_2015$Constituency <- results_2015$Constituency
response_2015$Region <- results_2015$Region
response_2015$y_1 <- results_2015$`GRN Share` - results_2010$`GRN Share`
response_2015$y_2 <- (100 * results_2015$`RFM Share` / (100 - results_2015$`GRN Share`)) - (100 * results_2010$`RFM Share` / (100 - results_2010$`GRN Share`))
response_2015$y_3 <- (100 * results_2015$`LD Share` / (100 - results_2015$`RFM Share` - results_2015$`GRN Share`)) - (100 * results_2010$`LD Share` / (100 - results_2010$`RFM Share` - results_2010$`GRN Share`))
response_2015$y_4 <- (100 * results_2015$`Con Share` / (100 - results_2015$`GRN Share` - results_2015$`RFM Share` - results_2015$`LD Share`)) - (100 * results_2010$`Con Share` / (100 - results_2010$`GRN Share` - results_2010$`RFM Share` - results_2010$`LD Share`))

# Compute responses for 2017
response_2017 <- data.frame(matrix(nrow = 543, ncol = 6))
names(response_2017) <- c('Constituency', 'Region', 'y_1', 'y_2', 'y_3', 'y_4')

response_2017$Constituency <- results_2017$Constituency
response_2017$Region <- results_2017$Region
response_2017$y_1 <- results_2017$`GRN Share` - results_2015$`GRN Share`
response_2017$y_2 <- (100 * results_2017$`RFM Share` / (100 - results_2017$`GRN Share`)) - (100 * results_2015$`RFM Share` / (100 - results_2015$`GRN Share`))
response_2017$y_3 <- (100 * results_2017$`LD Share` / (100 - results_2017$`RFM Share` - results_2017$`GRN Share`)) - (100 * results_2015$`LD Share` / (100 - results_2015$`RFM Share` - results_2015$`GRN Share`))
response_2017$y_4 <- (100 * results_2017$`Con Share` / (100 - results_2017$`GRN Share` - results_2017$`RFM Share` - results_2017$`LD Share`)) - (100 * results_2015$`Con Share` / (100 - results_2015$`GRN Share` - results_2015$`RFM Share` - results_2015$`LD Share`))

# Compute responses for 2019
response_2019 <- data.frame(matrix(nrow = 543, ncol = 6))
names(response_2019) <- c('Constituency', 'Region', 'y_1', 'y_2', 'y_3', 'y_4')

response_2019$Constituency <- results_2019$Constituency
response_2019$Region <- results_2019$Region
response_2019$y_1 <- results_2019$`GRN Share` - results_2017$`GRN Share`
response_2019$y_2 <- (100 * results_2019$`RFM Share` / (100 - results_2019$`GRN Share`)) - (100 * results_2017$`RFM Share` / (100 - results_2017$`GRN Share`))
response_2019$y_3 <- (100 * results_2019$`LD Share` / (100 - results_2019$`RFM Share` - results_2019$`GRN Share`)) - (100 * results_2017$`LD Share` / (100 - results_2017$`RFM Share` - results_2017$`GRN Share`))
response_2019$y_4 <- (100 * results_2019$`Con Share` / (100 - results_2019$`GRN Share` - results_2019$`RFM Share` - results_2019$`LD Share`)) - (100 * results_2017$`Con Share` / (100 - results_2017$`GRN Share` - results_2017$`RFM Share` - results_2017$`LD Share`))

# Compute responses for 2024
response_2024 <- data.frame(matrix(nrow = 543, ncol = 6))
names(response_2024) <- c('Constituency', 'Region', 'y_1', 'y_2', 'y_3', 'y_4')

response_2024$Constituency <- results_2024$Constituency
response_2024$Region <- results_2024$Region
response_2024$y_1 <- results_2024$`GRN Share` - results_2019$`GRN Share`
response_2024$y_2 <- (100 * results_2024$`RFM Share` / (100 - results_2024$`GRN Share`)) - (100 * results_2019$`RFM Share` / (100 - results_2019$`GRN Share`))
response_2024$y_3 <- (100 * results_2024$`LD Share` / (100 - results_2024$`RFM Share` - results_2024$`GRN Share`)) - (100 * results_2019$`LD Share` / (100 - results_2019$`RFM Share` - results_2019$`GRN Share`))
response_2024$y_4 <- (100 * results_2024$`Con Share` / (100 - results_2024$`GRN Share` - results_2024$`RFM Share` - results_2024$`LD Share`)) - (100 * results_2019$`Con Share` / (100 - results_2019$`GRN Share` - results_2019$`RFM Share` - results_2019$`LD Share`))

# Combine response data frames into a single data frame and order by constituency
response <- rbind(response_2015, response_2017, response_2019, response_2024)
response <- with(response, response[order(response$Constituency), ])

# Combine results in a single data frame and order by constituency and add column for year
results <- rbind(results_2010, results_2015, results_2017, results_2019, results_2024)
results <- with(results, results[order(results$Constituency), ])
results$Year <- rep(c(2010, 2015, 2017, 2019, 2024), 543)

# Results for before and after each election and set names
results_2 <- results[results$Year %in% c(2010, 2015, 2017, 2019), ]
results_3 <- results[results$Year %in% c(2015, 2017, 2019, 2024), ]

names(results_3) <- c('Constituency', 'Region', 'Winner', 'Majority', 'Con', 'Lab', 'LD', 'RFM', 'Green', 'Other', 'Total',
                      'Electorate', 'Con Share After', 'Lab Share After', 'LD Share After', 'RFM Share After', 'GRN Share After', 'Year')


election_data <- read_xlsx('~/Project/Election Modelling/All Election Data.xlsx', sheet = 'Data')

election_data$Year <- as.factor(election_data$Year)
election_data$`Adjusted Median Income` <- as.numeric(election_data$`Adjusted Median Income`)

# Add national vote shares to data frame
election_data$`Nat Con` <- rep(c(con_2010, con_2015, con_2017, con_2019, con_2024), 543)
election_data$`Nat Lab` <- rep(c(lab_2010, lab_2015, lab_2017, lab_2019, lab_2024), 543)
election_data$`Nat LD` <- rep(c(ld_2010, ld_2015, ld_2017, ld_2019, ld_2024), 543)
election_data$`Nat RFM` <- rep(c(rfm_2010, rfm_2015, rfm_2017, rfm_2019, rfm_2024), 543)
election_data$`Nat GRN` <- rep(c(grn_2010, grn_2015, grn_2017, grn_2019, grn_2024), 543)

# Add home ownership proportions to data frame
election_data$`Owner Prop` <- election_data$Owners / election_data$`Total - Housing`
election_data$`Private Prop` <- election_data$`Private renters` / election_data$`Total - Housing`
election_data$`Social Prop` <- election_data$`Social renters` / election_data$`Total - Housing`

# Remove data for 2010
election_data_training <- election_data[election_data$Year %in% c(2015, 2017, 2019, 2024), ]

# Create empty data frame for change in covariates
election_change_data <- data.frame(matrix(nrow = 543 * 4, ncol = 14))
names(election_change_data) <- c('Constituency', 'Year', 'X18.29 Change', 'X30.49 Change', 'X50.64 Change', 'X65. Change', 'Level 4 Prop Change', 'No Qualifications Prop Change','White Change', 'Owner Prop Change', 'Private Prop Change', 'Social Prop Change', 'Adjusted Median House Prices Change', 'Adjusted Median Income Change')

# Compute change in covariates
for (i in 1:543) {
  
  for (j in 1:4) {
    
    k <- 4 * i - 4 + j
    
    election_change_data$Constituency[k] <- election_data$Constituency[5 * i - 4]
    election_change_data$Year[k] <- levels(election_data$Year)[election_data$Year[5 * i - 4 + j]]
    election_change_data$`X18.29 Change`[k] <- election_data$X18.29[5 * i - 4 + j] - election_data$X18.29[5 * i - 5 + j]
    election_change_data$`X30.49 Change`[k] <- election_data$X30.49[5 * i - 4 + j] - election_data$X30.49[5 * i - 5 + j]
    election_change_data$`X50.64 Change`[k] <- election_data$X50.64[5 * i - 4 + j] - election_data$X50.64[5 * i - 5 + j]
    election_change_data$`X65. Change`[k] <- election_data$X65.[5 * i - 4 + j] - election_data$X65.[5 * i - 5 + j]
    election_change_data$`Level 4 Prop Change`[k] <- election_data$`Level 4 Prop`[5 * i - 4 + j] - election_data$`Level 4 Prop`[5 * i - 5 + j]
    election_change_data$`No Qualifications Prop Change`[k] <- election_data$`No Qualifications Prop`[5 * i - 4 + j] - election_data$`No Qualifications Prop`[5 * i - 5 + j]
    election_change_data$`White Change`[k] <- election_data$White[5 * i - 4 + j] - election_data$White[5 * i - 5 + j]
    election_change_data$`Owner Prop Change`[k] <- election_data$`Owner Prop`[5 * i - 4 + j] - election_data$`Owner Prop`[5 * i - 5 + j]
    election_change_data$`Private Prop Change`[k] <- election_data$`Private Prop`[5 * i - 4 + j] - election_data$`Private Prop`[5 * i - 5 + j]
    election_change_data$`Social Prop Change`[k] <- election_data$`Social Prop`[5 * i - 4 + j] - election_data$`Social Prop`[5 * i - 5 + j]
    election_change_data$`Adjusted Median House Prices Change`[k] <- election_data$home_prices[5 * i - 4 + j] - election_data$home_prices[5 * i - 5 + j]
    election_change_data$`Adjusted Median Income Change`[k] <- election_data$`Adjusted Median Income`[5 * i - 4 + j] - election_data$`Adjusted Median Income`[5 * i - 5 + j]
    
    
  }
  
  
}

# Add change in national vote share
election_change_data$`Nat Con Change` <- rep(c(con_2015 - con_2010, con_2017 - con_2015, con_2019 - con_2017, con_2024 - con_2019), 543)
election_change_data$`Nat Lab Change` <- rep(c(lab_2015 - lab_2010, lab_2017 - lab_2015, lab_2019 - lab_2017, lab_2024 - lab_2019), 543)
election_change_data$`Nat LD Change` <- rep(c(ld_2015 - ld_2010, ld_2017 - ld_2015, ld_2019 - ld_2017, ld_2024 - ld_2019), 543)
election_change_data$`Nat RFM Change` <- rep(c(rfm_2015 - rfm_2010, rfm_2017 - rfm_2015, rfm_2019 - rfm_2017, rfm_2024 - rfm_2019), 543)
election_change_data$`Nat GRN Change` <- rep(c(grn_2015 - grn_2010, grn_2017 - grn_2015, grn_2019 - grn_2017, grn_2024 - grn_2019), 543)


# Combine repsonses, covariates, change in covariates, results before and after elections, and factors for if a party ran before or not
election_data_training <- cbind(response[, 3:6], election_data_training[, 13:40], election_change_data[, 3:19], results_2[, c(1, 13:17)], results_3[, c(3, 13:17)])
election_data_training$Year <- as.factor(as.character(election_data_training$Year))
election_data_training$`No GRN Before` <- as.factor(as.numeric(results_2$Green == 0))
election_data_training$`No RFM Before` <- as.factor(as.numeric(results_2$RFM == 0))
election_data_training$`No LD Before` <- as.factor(as.numeric(results_2$LD == 0))
election_data_training$`No Con Before` <- as.factor(as.numeric(results_2$Con == 0))
election_data_training$`No Lab Before` <- as.factor(as.numeric(results_2$Lab == 0))
election_data_training$Year_2024 <- as.factor(as.numeric(election_data_training$Year == 2024))

# Remove Speaker's seats
election_data_training <- election_data_training[!(election_data_training$Constituency == 'Chorley' & election_data_training$Year %in% c(2019, 2024)), ]
election_data_training <- election_data_training[!(election_data_training$Constituency %in% c('Buckingham and Bletchley', 'Mid Buckinghamshire', 'Aylesbury') & election_data_training$Year %in% c(2015, 2017)),]

# Save data frame as a CSV file
write.csv(election_data_training, 'CF Model Data.csv')
