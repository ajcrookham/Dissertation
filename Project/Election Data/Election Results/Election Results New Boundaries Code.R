
# This file converts all of the general elections from 2010 to 2024 and the 2016 EU referendum to the 2024 electoral map

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Function that converts General Election results to the 2024 electoral map
convert_ge_results <- function(Year) {
  
  # Load in results for given year, 2024 results, boundary changes
  original_results <- read_excel('Election Results.xlsx', sheet = as.character(Year))
  results_2024 <- read_excel('Election Results.xlsx', sheet = '2024')
  boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
  boundary_change <- boundary_change[, c(3, 5, 10, 11)]
  parties <- c('Con', 'Lab', 'LD', 'RFM', 'Green', 'Other')
  
  # Create empty data frame for the results on the 2024 electoral map
  new_results <- data.frame(matrix(nrow = 543, ncol = 12))
  names(new_results) <- c('Constituency', 'Region', 'Winner', 'Majority', 
                          'Con', 'Lab', 'LD', 'RFM', 'Green', 'Other', 'Total',
                          'Electorate')
  
  # Fill in constituencies and regions for the 2024 electoral map
  new_results$Constituency <- results_2024$Constituency
  new_results$Region <- results_2024$Region
  
  # Compute results for each constituency in the 2024 electoral map
  for (i in 1:543) {
    
    # Load in the seat name
    seat <- results_2024$Constituency[i]
    
    # Find all of the old seats that are contained within the new seat
    old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
    old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
    
    # Compute the results for the new seat
    subset_of_original_results <- original_results[original_results$Constituency %in% old_to_new$Current.constituency.name, ]
    adjusted_results <- as.numeric(colSums(subset_of_original_results[, c(6, 9:14)] * old_to_new$Percentage.of.old.constituency.in.this.segment..population.))
    
    # Fill in the data frame for the new seat
    new_results$Con[i] <- round(adjusted_results[2], 0)
    new_results$Lab[i] <- round(adjusted_results[3], 0)
    new_results$LD[i] <- round(adjusted_results[4], 0)
    new_results$RFM[i] <- round(adjusted_results[5], 0)
    new_results$Green[i] <- round(adjusted_results[6], 0)
    new_results$Other[i] <- round(adjusted_results[7], 0)
    new_results$Total[i] <- new_results$Con[i] +  new_results$Lab[i] +  new_results$LD[i] +  new_results$RFM[i] + new_results$Green[i] + new_results$Other[i]
    new_results$Electorate[i] <- round(adjusted_results[1], 0)
    
    # Compute the winner of the new seat
    winner_index <- which.max(adjusted_results[2:7])
    new_results$Winner[i] <- parties[winner_index]
    
    # Compute the majority for the winner of the new seat
    ordered_results <- sort(adjusted_results[2:7], decreasing = TRUE)
    new_results$Majority[i] <- round(ordered_results[1] - ordered_results[2], 0)
    
  }
  
  # Return the data frame of results on the 2024 electoral map
  return(new_results)
  
}


# Convert the general election results from 2010 to 2019 onto the 2024 electoral map
results_2010_new <- convert_ge_results(2010)
results_2015_new <- convert_ge_results(2015)
results_2017_new <- convert_ge_results(2017)
results_2019_new <- convert_ge_results(2019)

# Reorder the columns in the 2024 General Election results to match the rest
results_2024 <- read_excel('Election Results.xlsx', sheet = '2024')
results_2024 <- results_2024[, c(1:2, 4, 8:14, 7, 6)]

# Convert the 2016 EU referendum results to the 2024 electoral map

results_brexit <- read_excel('Election Results.xlsx', sheet = 'Brexit')
boundary_change <- read.csv('~/Project/Election Data/Boundary Changes/Boundary Changes.csv')
boundary_change <- boundary_change[, c(3, 5, 10, 11)]

# Create an empty data frame for the results on the 2024 electoral map
results_brexit_new <- data.frame(matrix(nrow = 543, ncol = 2))
names(results_brexit_new) <- c('Constituency', 'Leave')

# Compute the results for each constituency in the 2024 electoral map
for (i in 1:543) {
  
  # Load in the seat name
  seat <- results_2024$Constituency[i]
  
  # Find all of the old seats that are contained within the new seat
  old_to_new <- boundary_change[boundary_change$New.constituency.name == seat, ]
  old_to_new <- with(old_to_new, old_to_new[order(Current.constituency.name), ])
  
  # Compute the results for the new seat
  results_brexit_2 <- results_brexit[results_brexit$Constituency %in% old_to_new$Current.constituency.name, ]
  results_brexit_3 <- as.numeric(colSums(results_brexit_2[, 2] * old_to_new$Percentage.of.old.constituency.in.this.segment..population.)) / sum(old_to_new$Percentage.of.old.constituency.in.this.segment..population.)
  
  # Fill in the data frame for the new seat
  results_brexit_new$Constituency[i] <- seat
  results_brexit_new$Leave[i] <- results_brexit_3
  
}


# Save the converted results to an excel file
sheets = list('2010' = results_2010_new, '2015' = results_2015_new, '2017' = results_2017_new, 
              '2019' = results_2019_new, '2024' = results_2024, 'Brexit' = results_brexit_new)

write.xlsx(sheets, 'Election Results New Boundaries.xlsx', firstRow = TRUE, firstCol = TRUE)

