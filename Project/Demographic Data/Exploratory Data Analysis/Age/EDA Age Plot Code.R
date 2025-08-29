
# This file produces the median age plot in Section 3.3.1

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Function that computes the median age of constituencies for a given year
median_age_computer <- function(Year){
  
  # Empty vector to be filled with median ages
  median_ages <- rep(0, 533)
  
  # Need two methods here as 2010 and 2024 data is slightly different
  # For years that are not 2010 or 2024 we do the following
  if (!(Year == 2010 | Year == 2024)) {
    
    # Load age data
    file_name <- paste(Year, 'Age Data.xls', sep = ' ')
    
    if (Year == 2019) {
      
      file_name <- paste(Year, 'Age Data.xlsx', sep = ' ')
      
    }
    
    file_name <- paste('~/Project/Demographic Data/Data Sorting/Age/', file_name, sep = '')
    sheet_name <- paste('Mid-', Year, ' Persons', sep = '')
    age_data <- read_excel(file_name, sheet = sheet_name, skip = 3)
    
    # Compute the median ages for old boundaries
    for (i in 1:533) {
      
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
    
    # Load 2010 age data
    age_data <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2010 Age Data.xls', sheet = 'Mid-2010 Persons', skip = 3)
    
    # Expand 2010 to be individual years by assuming a linear interpolation
    age_data_expanded <- data.frame(matrix(nrow = 533, ncol = 87))
    age_data_expanded[,1] <- age_data$...2[1:533]
    age_data_expanded[,2] <- age_data$`All Ages`[1:533]
    
    for (i in 1:17) {
      
      sub_total <- age_data[1:533, i + 3] / 5
      age_data_expanded[, 5 * i - 2] <- sub_total
      age_data_expanded[, 5 * i - 1] <- sub_total
      age_data_expanded[, 5 * i] <- sub_total
      age_data_expanded[, 5 * i + 1] <- sub_total
      age_data_expanded[, 5 * i + 2] <- sub_total
      
    }
    
    for (i in 1:533) {
      
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
    
    median_ages <- rep(0, 543)
    
    # Load 2024 data and remove unnecessary constituencies
    age_data <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2022 Age Data.xlsx', sheet = 'Mid-2022 PCON 2025', skip = 3)
    results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
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

median_2010 <- median_age_computer(2010)
median_2015 <- median_age_computer(2015)
median_2017 <- median_age_computer(2017)
median_2019 <- median_age_computer(2019)
median_2024 <- median_age_computer(2024)


# Load in general election results from 2010 to 2024
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Compute vote shares for each of the major parties for each election
con_2010 <- results_2010$Con / results_2010$Total * 100
con_2015 <- results_2015$Con / results_2015$Total * 100
con_2017 <- results_2017$Con / results_2017$Total * 100
con_2019 <- results_2019$Con / results_2019$Total * 100
con_2024 <- results_2024$Con / results_2024$Total * 100

lab_2010 <- results_2010$Lab / results_2010$Total * 100
lab_2015 <- results_2015$Lab / results_2015$Total * 100
lab_2017 <- results_2017$Lab / results_2017$Total * 100
lab_2019 <- results_2019$Lab / results_2019$Total * 100
lab_2024 <- results_2024$Lab / results_2024$Total * 100

ld_2010 <- results_2010$LD / results_2010$Total * 100
ld_2015 <- results_2015$LD / results_2015$Total * 100
ld_2017 <- results_2017$LD / results_2017$Total * 100
ld_2019 <- results_2019$LD / results_2019$Total * 100
ld_2024 <- results_2024$LD / results_2024$Total * 100

rfm_2010 <- results_2010$UKIP / results_2010$Total * 100
rfm_2015 <- results_2015$UKIP / results_2015$Total * 100
rfm_2017 <- results_2017$UKIP / results_2017$Total * 100
rfm_2019 <- results_2019$BRX / results_2019$Total * 100
rfm_2024 <- results_2024$RFM / results_2024$Total * 100

grn_2010 <- results_2010$Green / results_2010$Total * 100
grn_2015 <- results_2015$Green / results_2015$Total * 100
grn_2017 <- results_2017$Green / results_2017$Total * 100
grn_2019 <- results_2019$Green / results_2019$Total * 100
grn_2024 <- results_2024$Green / results_2024$Total * 100

# Create data frame containing vote shares and median ages
age <- data.frame(matrix(nrow = 2675, ncol = 0))

age$Constituency <- c(results_2010$Constituency, results_2015$Constituency, results_2017$Constituency, results_2019$Constituency, results_2024$Constituency)
age$Year <- c(rep(2010, 533), rep(2015, 533), rep(2017, 533), rep(2019, 533), rep(2024, 543))
age$Year <- factor(age$Year)
age$`Median Age` <- c(median_2010, median_2015, median_2017, median_2019, median_2024)
age$`Con Share` <- c(con_2010, con_2015, con_2017, con_2019, con_2024)
age$`Lab Share` <- c(lab_2010, lab_2015, lab_2017, lab_2019, lab_2024)
age$`LD Share` <- c(ld_2010, ld_2015, ld_2017, ld_2019, ld_2024)
age$`RFM Share` <- c(rfm_2010, rfm_2015, rfm_2017, rfm_2019, rfm_2024)
age$`GRN Share` <- c(grn_2010, grn_2015, grn_2017, grn_2019, grn_2024)


# Conservative plot
plot_con <- ggplot(age, aes(x = `Median Age`, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) + 
  theme_bw() + 
  labs(x = 'Median Age', y = 'Vote Share (%)') +
  theme(legend.position = 'none') +
  ggtitle('Conservative')

# Labour plot
plot_lab <- ggplot(age, aes(x = `Median Age`, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) + 
  theme_bw() + 
  labs(x = 'Median Age', y = 'Vote Share (%)') +
  theme(legend.position = 'none') +
  ggtitle('Labour')

# Lib Dem plot
plot_ld <- ggplot(age, aes(x = `Median Age`, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) + 
  theme_bw() + 
  labs(x = 'Median Age', y = 'Vote Share (%)') +
  theme(legend.position = 'none') +
  ggtitle('Liberal Democrats')

# Reform plot
plot_rfm <- ggplot(age, aes(x = `Median Age`, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) + 
  theme_bw() + 
  labs(x = 'Median Age', y = 'Vote Share (%)') +
  theme(legend.position = 'none') +
  ggtitle('Reform UK')

# Green plot
plot_grn <- ggplot(age, aes(x = `Median Age`, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) + 
  theme_bw() + 
  labs(x = 'Median Age', y = 'Vote Share (%)') +
  theme(legend.position = 'right') +
  ggtitle('Green Party')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_grn)

plot_grn <- plot_grn + theme(legend.position = 'none')

# Combine plots into a single figure
age_plot <- grid.arrange(plot_con, plot_lab, plot_ld, plot_rfm, plot_grn, legend, ncol = 2, nrow = 3,
                         layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                         widths = c(2, 2), heights = c(4, 4, 4))

# Save plot
ggsave('Age Breakdown.png', age_plot,  width = 15.66, height = 12, units = 'cm')




