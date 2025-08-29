
# This file produces the House Price plot in Section 3.4.4

# Load required packages
library(readxl)
library(ggplot2)

# Set working directory to Source File location

# Load house price data
house_price <- read.csv('~/Project/Demographic Data/Data Sorting/House Prices/House Prices Data New Boundaries.csv')

# Load Blue Wall seats
blue_wall <- read_excel('~/Project/Election Data/Blue Wall/Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Create data frame containing only Blue Wall seats
bw_house_prices <- house_price[house_price$ConstituencyName %in% blue_wall$Constituency, ]

# Compute the national and blue wall average median house price for each year from 2010 to 2024
years <- 2010:2024
nat_price <- rep(0, 15)
bw_price <- rep(0, 15)

for (i in 1:15) {
  
  year <- years[i]
  
  house_year <- house_price[house_price$Year == year, ]
  bw_house_year <- bw_house_prices[bw_house_prices$Year == year,]
  
  nat_price[i] <- mean(house_year$AdjustedMedianHousePrice)  
  bw_price[i] <- mean(bw_house_year$AdjustedMedianHousePrice)

}

# Blue Wall constituencies house prices
blue_wall_house_prices <- bw_house_prices[, c(2, 5, 4)]
names(blue_wall_house_prices) <- c('Constituency', 'Year', 'Adjusted Median House Price')

# National house price data
nat_house_prices <- data.frame(matrix(nrow = 15, ncol = 0))
nat_house_prices$Constituency <- rep('England', 15)
nat_house_prices$Year <- 2010:2024
nat_house_prices$`Adjusted Median House Price` <- nat_price

# Blue Wall house price data
bw_house_prices <- data.frame(matrix(nrow = 15, ncol = 0))
bw_house_prices$Constituency <- rep('Blue Wall', 15)
bw_house_prices$Year <- 2010:2024
bw_house_prices$`Adjusted Median House Price` <- bw_price

# Data frame containing Blue Wall, national, and individual constituencies
blue_wall_house_prices <- rbind(blue_wall_house_prices, bw_house_prices, nat_house_prices)

# Set up variables for plotting
blue_wall_house_prices$Colours <- c(rep('Individual Seats', 600), rep('Blue Wall', 15), rep('England', 15))
colours <- c('Individual Seats' = 'black', 'England' = 'red', 'Blue Wall' = 'blue')
alphas <- c(rep(0.3, 600), rep(1, 30))

# House Price plot
plot_price <- ggplot(data = blue_wall_house_prices, aes(x = Year, y = `Adjusted Median House Price`, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Adjusted Median House Price (GBP)', colour = '') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'bottom')

# Save plot
ggsave('Blue Wall House Price.png', plot_price)

