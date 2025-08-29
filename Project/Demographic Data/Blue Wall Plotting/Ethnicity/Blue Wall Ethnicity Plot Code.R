
# This file produces the plot in Section 3.4.5

# Load required packages
library(readxl)
library(ggplot2)

# Set working directory to Source File location

ethnicity <- read_excel('~/Project/Demographic Data/Data Sorting/Ethnicity/Ethnicity New Boundaries All Years.xlsx',
                        sheet = 'Ethnicity')
ethnicity <- ethnicity[, c(1,3,4)]

# Load 2010 results and Blue Wall seats
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
blue_wall <- read_excel('~/Project/Election Data/Blue Wall/Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Load raw ethnicity data
ethnicity_raw <- read_excel('~/Project/Demographic Data/Data Sorting/Ethnicity/Ethnicity.xlsx', sheet = '2011 Comparison')
ethnicity_raw <- ethnicity_raw[ethnicity_raw$`Broad Ethnic Groups` == 'White' & ethnicity_raw$ConstituencyName %in% results_2010$Constituency,]

# Create a data frame for national proportions for each election
ethnic_nat <- data.frame(matrix(nrow = 5, ncol = 3))

i <- 0
years <- c(2010, 2015, 2017, 2019, 2024)

names(ethnic_nat) <- c('Constituency', 'Year', 'White')
ethnic_nat$Constituency <- rep('England/Wales', 5)
ethnic_nat$Year <- c(2010, 2015, 2017, 2019, 2024)

for (x in years) {
  
  i <- i + 1
  
  ethnic <- ethnicity_raw$`England/Wales 2011`[1] + (ethnicity_raw$`England/Wales 2021`[1] - ethnicity_raw$`England/Wales 2011`[1]) / (2021 - 2011) * (x - 2011)  
  
  ethnic_nat[i, 3] <- ethnic
  
}

# Data frame for Blue Wall proportions for each election
ethnic_bw <- ethnicity[ethnicity$Constituency %in% blue_wall$Constituency, ]

ethnic_bw_2010 <- ethnic_bw[ethnic_bw$Year == 2010, ]
ethnic_bw_2010 <- mean(ethnic_bw_2010$White)

ethnic_bw_2015 <- ethnic_bw[ethnic_bw$Year == 2015, ]
ethnic_bw_2015 <- mean(ethnic_bw_2015$White)

ethnic_bw_2017 <- ethnic_bw[ethnic_bw$Year == 2017, ]
ethnic_bw_2017 <- mean(ethnic_bw_2017$White)

ethnic_bw_2019 <- ethnic_bw[ethnic_bw$Year == 2019, ]
ethnic_bw_2019 <- mean(ethnic_bw_2019$White)

ethnic_bw_2024 <- ethnic_bw[ethnic_bw$Year == 2024, ]
ethnic_bw_2024 <- mean(ethnic_bw_2024$White)

ethnic_bw <- data.frame(matrix(nrow = 5, ncol = 0))
ethnic_bw$Constituency <- rep('Blue Wall', 5)
ethnic_bw$Year <- c(2010, 2015, 2017, 2019, 2024)
ethnic_bw$White <- c(ethnic_bw_2010, ethnic_bw_2015, ethnic_bw_2017, ethnic_bw_2019, ethnic_bw_2024)

# Data frame for individual constituencies within Blue Wall 
blue_wall_ethnic <- ethnicity[ethnicity$Constituency %in% blue_wall$Constituency, ]

# Combine three data frames into one
blue_wall_ethnic <- rbind(blue_wall_ethnic, ethnic_bw, ethnic_nat)

# Set up variables for plotting
blue_wall_ethnic$Colours <- c(rep('Individual Seats', 200), rep('Blue Wall', 5), rep('England and Wales', 5))
colours <- c('Individual Seats' = 'black', 'England and Wales' = 'red', 'Blue Wall' = 'blue')
alphas <- c(rep(0.8, 200), rep(1, 10))

# Plot for ethnicity
ggplot(data = blue_wall_ethnic, aes(x = Year, y = White, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide = 'none') +
  labs(y = 'Proportion', colour = '') +
  theme_classic() +
  theme(legend.position = 'bottom')

# Save plot
ggsave('Blue Wall Ethnicity.png')



