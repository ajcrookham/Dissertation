
# This file works out which seats satisfy our condition for a Blue Wall seat and creates an excel file with the relevant information

# Load required packages
library(readxl)
library(openxlsx)

# Set working directory to Source File location

# Function that finds Blue Wall seats and creates a data frame with them and their results in
blue_wall_builder <- function(new_map = TRUE) {
  
  # Load in the results for the correct electoral map
  if (new_map) {
    
    # Results on the 2024 electoral map
    results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2010')
    results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2015')
    results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2017')
    results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2019')
    results_brexit <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = 'Brexit')
    
  } else {
    
    # Results on the 2010 electoral map
    results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
    results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2015')
    results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2017')
    results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
    results_brexit <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = 'Brexit')
    
  }
  
  # Make sure that all of the results are in the same order
  results_2010 <- with(results_2010, results_2010[order(results_2015$Constituency), ])
  results_2015 <- with(results_2015, results_2015[order(results_2015$Constituency), ])
  results_2017 <- with(results_2017, results_2017[order(results_2015$Constituency), ])
  results_2019 <- with(results_2019, results_2019[order(results_2015$Constituency), ])
  results_brexit <- with(results_brexit, results_brexit[order(results_brexit$Constituency), ])
  
  # Create empty vectors to be filled with information about Blue Wall seats
  blue_wall_seats <- c()
  blue_wall_region <- c()
  blue_wall_2010 <- c()
  blue_wall_2015 <- c()
  blue_wall_2017 <- c()
  blue_wall_2019 <- c()
  blue_wall_brexit <- c()
  
  total <- length(results_2010$Constituency)
  
  for (i in 1:total){
    
    # Compute the Conservative vote share for each year and test if greater than 50%
    test_2010 <- results_2010$Con[i] / results_2010$Total[i] >= 0.5
    test_2015 <- results_2015$Con[i] / results_2015$Total[i] >= 0.5
    test_2017 <- results_2017$Con[i] / results_2017$Total[i] >= 0.5
    test_2019 <- results_2019$Con[i] / results_2019$Total[i] >= 0.5
    
    # Test if constituency voted to remain more than the whole of the nation
    test_brexit <- results_brexit$Leave[i] < (17410742 / (17410742 + 16141241))
    
    # Apply tests and add constituency to blue wall vectors if all tests are true 
    if (test_2010 & test_2015 & test_2017 & test_2019 & test_brexit) {
      
      blue_wall_seats <- append(blue_wall_seats, results_2015$Constituency[i])
      blue_wall_region <- append(blue_wall_region, results_2015$Region[i])
      
      blue_wall_2010 <- append(blue_wall_2010, results_2010$Con[i] / results_2010$Total[i])
      blue_wall_2015 <- append(blue_wall_2015, results_2015$Con[i] / results_2015$Total[i])
      blue_wall_2017 <- append(blue_wall_2017, results_2017$Con[i] / results_2017$Total[i])
      blue_wall_2019 <- append(blue_wall_2019, results_2019$Con[i] / results_2019$Total[i])
      
      blue_wall_brexit <- append(blue_wall_brexit, results_brexit$Leave[i])
      
    }
    
  }
  
  # Convert Brexit data to be numeric
  blue_wall_brexit <- as.numeric(blue_wall_brexit)
  
  # Store all the Blue Wall data into a data frame
  blue_wall <- data.frame('Constituency' = blue_wall_seats, 'Region' = blue_wall_region, 'Con 2010' = round(blue_wall_2010, 4), 'Con 2015' = round(blue_wall_2015, 4), 'Con 2017' = round(blue_wall_2017, 4),
                          'Con 2019' = round(blue_wall_2019, 4), 'Leave vote' = round(blue_wall_brexit, 4))
  
  return(blue_wall)
  
}

# Build the Blue Walls for both of the electoral maps
blue_wall_old <- blue_wall_builder(new_map = FALSE)
blue_wall_new <- blue_wall_builder(new_map = TRUE)

# Create excel file with for the Blue Walls
sheets = list('Old Boundaries' = blue_wall_old, 'New Boundaries' = blue_wall_new)
write.xlsx(sheets, 'Blue Wall Seats.xlsx', firstRow = TRUE, firstCol = TRUE)






