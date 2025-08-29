
# This file converts all of the raw data files for age onto the 2024 electoral map
# It also computes the proportions for the four age groups and the median age of each constituency in the 2024 electoral map

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Function for converting the raw age data to the 2024 electoral map
age_converter <- function(Year) {
  
  # Load in 2010 and 2024 results and the boundary changes
  results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
  results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
  boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
  boundary_change <- boundary_change[, c(3, 5, 10, 11)]
  
  # Set up file name and sheet name to load in raw age data for given year
  file_name <- paste(Year, 'Age Data.xls', sep = ' ')
  
  if (Year == 2019) {
    
    file_name <- paste(Year, 'Age Data.xlsx', sep = ' ')
    
  }
  
  sheet_name <- paste('Mid-', Year, ' Persons', sep = '')
  # Load in the raw age data for given year
  age_data <- read_excel(file_name, sheet = sheet_name, skip = 3)
  
  # Need two methods here as 2010 data is slightly different
  # For years that are not 2010 we do the following
  if (!Year == 2010) {
    
    # Remove unnecessary constituencies
    age_data <- age_data[age_data$PCON11NM %in% results_2010$Constituency,]
    
    # Set up empty data frame to store age data for 2024 electoral map
    age_data_new <- data.frame(matrix(nrow = 543, ncol = 94))
    names(age_data_new) <- c('Constituency', 'Region', 'All Ages', 0:89, '90+')
    
    for (i in 1:543) {
      
      # Load in the seat name and region
      seat <- results_2024$Constituency[i]
      region <- results_2024$Region[i]
      
      # Find all of the old constituencies contained within the new one
      old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
      old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
      
      # Compute age data for new seat
      subset_of_age_data <- age_data[age_data$PCON11NM %in% old_to_new$Current.constituency.name, ]
      adjusted_age_data <- colSums(subset_of_age_data[, 3:94] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
      
      # Update data frame with the data for the 2024 electoral map
      age_data_new$Constituency[i] <- seat
      age_data_new$Region[i] <- region
      age_data_new[i, 3:94] <- round(adjusted_age_data, 0)
      
    }
  
  # For 2010 data we do the following  
  } else {
    
    # Remove unnecessary constituencies
    age_data <- age_data[age_data$...2 %in% results_2010$Constituency, ]
    
    # Set up empty data frame to store age data for the 2024 electoral map
    age_data_new <- data.frame(matrix(nrow = 543, ncol = 21))
    names(age_data_new) <- c('Constituency', 'Region', names(age_data)[3:21])
    
    for (i in 1:543) {
      
      # Load in the seat name and region
      seat <- results_2024$Constituency[i]
      region <- results_2024$Region[i]
      
      # Find all of the old constituencies contained within the new constituency
      old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
      old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
      
      # Compute age data for new seat
      subset_of_age_data <- age_data[age_data$...2 %in% old_to_new$Current.constituency.name, ]
      adjusted_age_data <- colSums(subset_of_age_data[, 3:21] * old_to_new$Percentage.of.old.constituency.in.this.segment..population...notes.1.and.2.)
      
      # Update data frame with new age data
      age_data_new$Constituency[i] <- seat
      age_data_new$Region[i] <- region
      age_data_new[i, 3:21] <- round(adjusted_age_data, 0)
      
    }
    
  }
  
  return(age_data_new)
  
}

# Convert age data from 2010 to 2019 to the 2024 electoral map
age_2010 <- age_converter(2010)
age_2015 <- age_converter(2015)
age_2017 <- age_converter(2017)
age_2019 <- age_converter(2019)

# Compute proportions for 2010
age_2010_18_29 <- (age_2010$`15-19` * 0.4 + age_2010$`20-24` + age_2010$`25-29`) / (rowSums(age_2010[8:21]) + 0.4 * age_2010$`15-19`)
age_2010_30_49 <- (age_2010$`30-34` + age_2010$`35-39` + age_2010$`40-44` + age_2010$`45-49`) / (rowSums(age_2010[8:21]) + 0.4 * age_2010$`15-19`)
age_2010_50_64 <- (age_2010$`50-54` + age_2010$`55-59` + age_2010$`60-64`) / (rowSums(age_2010[8:21]) + 0.4 * age_2010$`15-19`)
age_2010_65 <- (age_2010$`65-69` + age_2010$`70-74` + age_2010$`75-79` + age_2010$`80-84` + age_2010$`85+`) / (rowSums(age_2010[8:21]) + 0.4 * age_2010$`15-19`)

# Compute proportions for 2015
age_2015_18_29 <- rowSums(age_2015[, 22:33]) / rowSums(age_2015[, 22:94])
age_2015_30_49 <- rowSums(age_2015[, 34:53]) / rowSums(age_2015[, 22:94])
age_2015_50_64 <- rowSums(age_2015[, 54:68]) / rowSums(age_2015[, 22:94])
age_2015_65 <- rowSums(age_2015[, 69:94]) / rowSums(age_2015[, 22:94])

# Compute proportions for 2017
age_2017_18_29 <- rowSums(age_2017[, 22:33]) / rowSums(age_2017[, 22:94])
age_2017_30_49 <- rowSums(age_2017[, 34:53]) / rowSums(age_2017[, 22:94])
age_2017_50_64 <- rowSums(age_2017[, 54:68]) / rowSums(age_2017[, 22:94])
age_2017_65 <- rowSums(age_2017[, 69:94]) / rowSums(age_2017[, 22:94])

# Compute proportions for 2019
age_2019_18_29 <- rowSums(age_2019[, 22:33]) / rowSums(age_2019[, 22:94])
age_2019_30_49 <- rowSums(age_2019[, 34:53]) / rowSums(age_2019[, 22:94])
age_2019_50_64 <- rowSums(age_2019[, 54:68]) / rowSums(age_2019[, 22:94])
age_2019_65 <- rowSums(age_2019[, 69:94]) / rowSums(age_2019[, 22:94])

# Load data for 2024
age_2024 <- read_excel('2022 Age Data.xlsx', sheet = 'Mid-2022 PCON 2025', skip = 3)
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
age_2024 <- age_2024[age_2024$`PCON 2025 Name` %in% results_2024$Constituency, ]
age_2024$Region <- results_2024$Region

# Compute proportions for 2024
age_2024_18_29 <- rowSums(age_2024[, c(22:33, 113:124)]) / rowSums(age_2024[, c(22:94, 113:185)])
age_2024_30_49 <- rowSums(age_2024[, c(34:53, 125:144)]) / rowSums(age_2024[, c(22:94, 113:185)])
age_2024_50_64 <- rowSums(age_2024[, c(54:68, 145:159)]) / rowSums(age_2024[, c(22:94, 113:185)])
age_2024_65 <- rowSums(age_2024[, c(69:94, 160:185)]) / rowSums(age_2024[, c(22:94, 113:185)])


# Function that computes the median age of constituencies for a given year
median_age_computer <- function(Year){
  
  # Empty vector to be filled with median ages
  median_ages <- rep(0, 543)
  
  # Need two methods here as 2010 and 2024 data is slightly different
  # For years that are not 2010 or 2024 we do the following
  if (!(Year == 2010 | Year == 2024)) {
    
    # Compute the age data on the 2024 electoral map
    age_data <- age_converter(Year)
    
    for (i in 1:543) {
      
      # Compute value where median would occur and initialise other values
      median_total <- age_data$`All Ages`[i] / 2
      current_total <- 0
      current_age <- 0
      
      for (j in 1:91) {
        
        current_total <- current_total + age_data[i, j + 3]
        
        # Test if value where median occurs has been passed or not 
        if (current_total >= median_total) {
          
          # Store median age in vector
          median_ages[i] <- current_age
          
          break
          
        }
        
        # If median value not met then add one to current age value
        current_age <- current_age + 1
        
      }
      
    }  
    
  }  
  
  # For 2010
  if (Year == 2010){
    
    # Compute age data for 2010
    age_data <- age_converter(Year)
    
    # Expand 2010 to be individual years by assuming a linear interpolation
    age_data_expanded <- data.frame(matrix(nrow = 543, ncol = 87))
    age_data_expanded[,1] <- age_data$Constituency
    age_data_expanded[,2] <- age_data$`All Ages`
    
    for (i in 1:17) {
      
      sub_total <- age_data[, i + 3] / 5
      age_data_expanded[, 5 * i - 2] <- sub_total
      age_data_expanded[, 5 * i - 1] <- sub_total
      age_data_expanded[, 5 * i] <- sub_total
      age_data_expanded[, 5 * i + 1] <- sub_total
      age_data_expanded[, 5 * i + 2] <- sub_total
      
    }
    
    for (i in 1:543) {
      
      # Compute value where median would occur and initialise other values
      median_total <- age_data_expanded[i, 2] / 2
      current_total <- 0
      current_age <- 0
      
      for (j in 1:85) {
        
        current_total <- current_total + age_data_expanded[i, j + 2]
        
        # Test if value where median occurs has been passed or not
        if (current_total >= median_total) {
          
          # Store median age in vector
          median_ages[i] <- current_age
          break
          
        }
        
        # If median value not met then add one to current age
        current_age <- current_age + 1
        
      }
      
    }
    
  } 
  
  # For 2024
  if (Year == 2024) {
    
    # Load 2024 data and remove unnecessary constituencies
    age_data <- read_excel('2022 Age Data.xlsx', sheet = 'Mid-2022 PCON 2025', skip = 3)
    age_data <- age_data[age_data$`PCON 2025 Name` %in% results_2024$Constituency, ]
    
    for (i in 1:543) {
      
      # Compute value where median occurs and initialise other values
      median_total <- age_data$Total[i] / 2
      current_total <- 0
      current_age <- 0
      
      for (j in 1:91) {
        
        current_total <- current_total + age_data[i, j + 3] + age_data[i, j + 94]
        
        # Test if value where median occurs has been passed or not
        if (current_total >= median_total) {
          
          # Store median age in vector
          median_ages[i] <- current_age
          break
          
        }
        
        # If median value not met then add one to current age
        current_age <- current_age + 1
        
      }
      
    }
    
  }
  
  return(median_ages)
  
}

# Compute median ages for 2010 to 2019
median_2010 <- median_age_computer(2010)
median_2015 <- median_age_computer(2015)
median_2017 <- median_age_computer(2017)
median_2019 <- median_age_computer(2019)
median_2024 <- median_age_computer(2024)

results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

age_2010 <- data.frame('Constituency' = results_2024$Constituency, 'Region' = results_2024$Region, 
                       '18-29' = age_2010_18_29, '30-49' = age_2010_30_49,
                       '50-64' = age_2010_50_64, '65+' = age_2010_65, 'Median Age' = median_2010)
age_2015 <- data.frame('Constituency' = results_2024$Constituency, 'Region' = results_2024$Region, 
                       '18-29' = age_2015_18_29, '30-49' = age_2015_30_49,
                       '50-64' = age_2015_50_64, '65+' = age_2015_65, 'Median Age' = median_2015)
age_2017 <- data.frame('Constituency' = results_2024$Constituency, 'Region' = results_2024$Region, 
                       '18-29' = age_2017_18_29, '30-49' = age_2017_30_49,
                       '50-64' = age_2017_50_64, '65+' = age_2017_65, 'Median Age' = median_2017)
age_2019 <- data.frame('Constituency' = results_2024$Constituency, 'Region' = results_2024$Region, 
                       '18-29' = age_2019_18_29, '30-49' = age_2019_30_49,
                       '50-64' = age_2019_50_64, '65+' = age_2019_65, 'Median Age' = median_2019)
age_2024 <- data.frame('Constituency' = results_2024$Constituency, 'Region' = results_2024$Region, 
                       '18-29' = age_2024_18_29, '30-49' = age_2024_30_49,
                       '50-64' = age_2024_50_64, '65+' = age_2024_65, 'Median Age' = median_2024)


sheets = list('2010' = age_2010, '2015' = age_2015, '2017' = age_2017,
              '2019' = age_2019, '2024' = age_2024)

write.xlsx(sheets, 'Age Data New Boundaries.xlsx', firstRow = TRUE, firstCol = TRUE)






