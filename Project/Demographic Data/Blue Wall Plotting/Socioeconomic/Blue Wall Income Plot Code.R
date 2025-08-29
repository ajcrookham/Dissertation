
# This file produces the income plot in Section 3.4.4

# Load required packages
library(readxl)
library(ggplot2)

# Load income data for each election
income_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2010')
income_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2015')
income_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2017')
income_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2019')
income_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2024')

# Load Blue Wall seats
blue_wall <- read_excel('~/Project/Election Data/Blue Wall/Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Add year to income data
income_2010$Year <- rep(2010, 543)
income_2015$Year <- rep(2015, 543)
income_2017$Year <- rep(2017, 543)
income_2019$Year <- rep(2019, 543)
income_2024$Year <- rep(2024, 543)

# Compute national average median income for each election
nat_2010 <- mean(income_2010$`Adjusted Median Income`)
nat_2015 <- mean(income_2015$`Adjusted Median Income`)
nat_2017 <- mean(income_2017$`Adjusted Median Income`)
nat_2019 <- mean(income_2019$`Adjusted Median Income`)
nat_2024 <- mean(income_2024$`Adjusted Median Income`)

# Create data frames containing income data for Blue Wall seats
blue_wall_income_2010 <- income_2010[income_2010$Constituency %in% blue_wall$Constituency, ]
blue_wall_income_2015 <- income_2015[income_2015$Constituency %in% blue_wall$Constituency, ]
blue_wall_income_2017 <- income_2017[income_2017$Constituency %in% blue_wall$Constituency, ]
blue_wall_income_2019 <- income_2019[income_2019$Constituency %in% blue_wall$Constituency, ]
blue_wall_income_2024 <- income_2024[income_2024$Constituency %in% blue_wall$Constituency, ]

names(blue_wall_income_2015) <- names(blue_wall_income_2010)
names(blue_wall_income_2017) <- names(blue_wall_income_2010)
names(blue_wall_income_2019) <- names(blue_wall_income_2010)
names(blue_wall_income_2024) <- names(blue_wall_income_2010)

# Compute Blue Wall average median income for each election
blue_2010 <- mean(blue_wall_income_2010$`Adjusted Median Income`)
blue_2015 <- mean(blue_wall_income_2015$`Adjusted Median Income`)
blue_2017 <- mean(blue_wall_income_2017$`Adjusted Median Income`)
blue_2019 <- mean(blue_wall_income_2019$`Adjusted Median Income`)
blue_2024 <- mean(blue_wall_income_2024$`Adjusted Median Income`)

# Create overall data frame for Blue Wall seats
blue_wall_income <- rbind(blue_wall_income_2010, blue_wall_income_2015, 
                       blue_wall_income_2017, blue_wall_income_2019, blue_wall_income_2024)
names(blue_wall_income) <- c('Constituency', 'Income', 'Year')

add_income <- data.frame(matrix(nrow = 10, ncol = 0))

add_income$Constituency <- c(rep('Blue Wall', 5), rep('England', 5))
add_income$Income <- c(blue_2010, blue_2015, blue_2017, blue_2019, blue_2024, nat_2010, nat_2015, nat_2017, nat_2019, nat_2024)
add_income$Year <- rep(c(2010, 2015, 2017, 2019, 2024), 2)

# Add Blue Wall and national averages to income data frame
blue_wall_income <- rbind(blue_wall_income, add_income)

# Set up variables for plotting
blue_wall_income$Colours <- c(rep('Individual Seats', 200), rep('Blue Wall', 5), rep('England', 5))
colours <- c('Individual Seats' = 'black', 'England' = 'red', 'Blue Wall' = 'blue')
alphas <- c(rep(0.3, 200), rep(1, 10))

# Income plot
ggplot(data = blue_wall_income, aes(x = Year, y = Income, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Median Income (GBP)', colour = '') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'bottom')

# Save plot
ggsave('Blue Wall Income.png')


