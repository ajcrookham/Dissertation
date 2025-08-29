
# This file collates the general election results from 2010 to 2024 and the EU referendum results by constituency
# It removes any unnecessary constituencies and columns from the raw data files and produces a single data file


# Load required packages
library(readxl)
library(openxlsx)
library(tidyr)

# Need to set working directory to Source File location

# Load and clean up 2010 General Election results
GE2010 <- read_excel('GE2010.xlsx', sheet = 'Data', skip = 2)
GE2010 <- GE2010[GE2010$`Country name` == 'England', c(3,4,11:15,17:22,30)]
names(GE2010) <- c('Constituency', 'Region', 'Result', 'Winner', 'Second', 'Electorate', 'Total', 'Majority', 'Con', 
                   'Lab', 'LD', 'UKIP', 'Green', 'Other')

# Load and clean up 2015 General Election results
GE2015 <- read_excel('GE2015.xlsx', sheet = 'Data', skip = 2)
GE2015 <- GE2015[GE2015$`Country name` == 'England', c(3,4,11:15,17:22,30)]
names(GE2015) <- c('Constituency', 'Region', 'Result', 'Winner', 'Second', 'Electorate', 'Total', 'Majority', 'Con', 
                   'Lab', 'LD', 'UKIP', 'Green', 'Other')

# Load and clean up 2017 General Election results
GE2017 <- read_excel('GE2017.xlsx', sheet = 'Data', skip = 2)
GE2017 <- GE2017[GE2017$`Country name` == 'England', c(3,4,11:15,17:22,30:31)]
names(GE2017) <- c('Constituency', 'Region', 'Result', 'Winner', 'Second', 'Electorate', 'Total', 'Majority', 'Con', 
                   'Lab', 'LD', 'UKIP', 'Green', 'Other', 'Of which other winner')

# Load and clean up 2019 General Election results
GE2019 <- read_excel('GE2019.xlsx', sheet = 'Data', skip = 2)
GE2019 <- GE2019[GE2019$`Country name` == 'England', c(3,4,11:15,17:22,30,31)]
names(GE2019) <- c('Constituency', 'Region', 'Result', 'Winner', 'Second', 'Electorate', 'Total', 'Majority', 'Con', 
                   'Lab', 'LD', 'BRX', 'Green', 'Other', 'Of which other winner')

# Correct a couple of the constituency names
GE2019$Constituency[233] <- 'Isle of Wight'
GE2019$Constituency[448] <- 'Stratford-on-Avon'

# Load and clean up 2024 General Election results
GE2024 <- read_excel('GE2024.xlsx', sheet = 'Data', skip = 2)
GE2024 <- GE2024[GE2024$`Country name` == 'England', c(3,4,10:14,16:21,29,30)]
names(GE2024) <- c('Constituency', 'Region', 'Result', 'Winner', 'Second', 'Electorate', 'Total', 'Majority', 'Con', 
                   'Lab', 'LD', 'RFM', 'Green', 'Other', 'Of which other winner')

# Load and clean up the 2016 EU referendu results
Brexit_Results <- read_excel('Brexit Results.xlsx', sheet = 'DATA', skip = 5)
Brexit_Results <- drop_na(Brexit_Results)
Brexit_Results <- Brexit_Results[Brexit_Results$Constituency %in% GE2017$Constituency, c(2,6)]
Brexit_Results <- with(Brexit_Results, Brexit_Results[order(Brexit_Results$Constituency), ])
names(Brexit_Results) <- c('Constituency', 'Leave')

# Create excel spreadsheet containing all of the results in individual sheets
sheets = list('2024' = GE2024, '2019' = GE2019, '2017' = GE2017, 
              '2015' = GE2015, '2010' = GE2010, 'Brexit' = Brexit_Results)

write.xlsx(sheets, 'Election Results.xlsx', firstRow = TRUE, firstCol = TRUE)








