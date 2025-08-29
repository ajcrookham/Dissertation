
# This file produces a plot of the change in the Blue Wall vote share for Con, Lab, LD

# Load required packages
library(readxl)
library(ggplot2)

# Set working directory to Source File location

# Load in original general election results
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Load in the two Blue Walls
blue_wall_old <- read_excel('Blue Wall Seats.xlsx', sheet = 'Old Boundaries')
blue_wall_new <- read_excel('Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Remove data from constituencies not in the Blue Wall
blue_wall_2010 <- results_2010[results_2010$Constituency %in% blue_wall_old$Constituency, ]
blue_wall_2015 <- results_2015[results_2015$Constituency %in% blue_wall_old$Constituency, ]
blue_wall_2017 <- results_2017[results_2017$Constituency %in% blue_wall_old$Constituency, ]
blue_wall_2019 <- results_2019[results_2019$Constituency %in% blue_wall_old$Constituency, ]
blue_wall_2024 <- results_2024[results_2024$Constituency %in% blue_wall_new$Constituency, ]

# Compute the vote shares for each constituency in the Blue Wall for Con, Lab, LD in 2010
bw_2010 <- data.frame(matrix(nrow = 37, ncol = 0))
bw_2010$Constituency <- blue_wall_2010$Constituency
bw_2010$Con <- blue_wall_2010$Con / blue_wall_2010$Total * 100
bw_2010$Lab <- blue_wall_2010$Lab / blue_wall_2010$Total * 100
bw_2010$LD <- blue_wall_2010$LD / blue_wall_2010$Total * 100

# Compute the vote shares for each constituency in the Blue Wall for Con, Lab, LD in 2015
bw_2015 <- data.frame(matrix(nrow = 37, ncol = 0))
bw_2015$Constituency <- blue_wall_2010$Constituency
bw_2015$Con <- blue_wall_2015$Con / blue_wall_2015$Total * 100
bw_2015$Lab <- blue_wall_2015$Lab / blue_wall_2015$Total * 100
bw_2015$LD <- blue_wall_2015$LD / blue_wall_2015$Total * 100

# Compute the vote shares for each constituency in the Blue Wall for Con, Lab, LD in 2017
bw_2017 <- data.frame(matrix(nrow = 37, ncol = 0))
bw_2017$Constituency <- blue_wall_2010$Constituency
bw_2017$Con <- blue_wall_2017$Con / blue_wall_2017$Total * 100
bw_2017$Lab <- blue_wall_2017$Lab / blue_wall_2017$Total * 100
bw_2017$LD <- blue_wall_2017$LD / blue_wall_2017$Total * 100

# Compute the vote shares for each constituency in the Blue Wall for Con, Lab, LD in 2019
bw_2019 <- data.frame(matrix(nrow = 37, ncol = 0))
bw_2019$Constituency <- blue_wall_2010$Constituency
bw_2019$Con <- blue_wall_2019$Con / blue_wall_2019$Total * 100
bw_2019$Lab <- blue_wall_2019$Lab / blue_wall_2019$Total * 100
bw_2019$LD <- blue_wall_2019$LD / blue_wall_2019$Total * 100

# Compute the vote shares for each constituency in the Blue Wall for Con, Lab, LD in 2024
bw_2024 <- data.frame(matrix(nrow = 40, ncol = 0))
bw_2024$Constituency <- blue_wall_2024$Constituency
bw_2024$Con <- blue_wall_2024$Con / blue_wall_2024$Total * 100
bw_2024$Lab <- blue_wall_2024$Lab / blue_wall_2024$Total * 100
bw_2024$LD <- blue_wall_2024$LD / blue_wall_2024$Total * 100

# Compute the median, 5% quantile, and 95% quantile for the Conservatives for each year in the Blue Wall
con_2010 <- quantile(bw_2010$Con, 0.5)
con_2015 <- quantile(bw_2015$Con, 0.5)
con_2017 <- quantile(bw_2017$Con, 0.5)
con_2019 <- quantile(bw_2019$Con, 0.5)
con_2024 <- quantile(bw_2024$Con, 0.5)
con_upper_2010 <- quantile(bw_2010$Con, 0.95)
con_upper_2015 <- quantile(bw_2015$Con, 0.95)
con_upper_2017 <- quantile(bw_2017$Con, 0.95)
con_upper_2019 <- quantile(bw_2019$Con, 0.95)
con_upper_2024 <- quantile(bw_2024$Con, 0.95)
con_lower_2010 <- quantile(bw_2010$Con, 0.05)
con_lower_2015 <- quantile(bw_2015$Con, 0.05)
con_lower_2017 <- quantile(bw_2017$Con, 0.05)
con_lower_2019 <- quantile(bw_2019$Con, 0.05)
con_lower_2024 <- quantile(bw_2024$Con, 0.05)

# Compute the median, 5% quantile, and 95% quantile for Labour for each year in the Blue Wall
lab_2010 <- quantile(bw_2010$Lab, 0.5)
lab_2015 <- quantile(bw_2015$Lab, 0.5)
lab_2017 <- quantile(bw_2017$Lab, 0.5)
lab_2019 <- quantile(bw_2019$Lab, 0.5)
lab_2024 <- quantile(bw_2024$Lab, 0.5)
lab_upper_2010 <- quantile(bw_2010$Lab, 0.95)
lab_upper_2015 <- quantile(bw_2015$Lab, 0.95)
lab_upper_2017 <- quantile(bw_2017$Lab, 0.95)
lab_upper_2019 <- quantile(bw_2019$Lab, 0.95)
lab_upper_2024 <- quantile(bw_2024$Lab, 0.95)
lab_lower_2010 <- quantile(bw_2010$Lab, 0.05)
lab_lower_2015 <- quantile(bw_2015$Lab, 0.05)
lab_lower_2017 <- quantile(bw_2017$Lab, 0.05)
lab_lower_2019 <- quantile(bw_2019$Lab, 0.05)
lab_lower_2024 <- quantile(bw_2024$Lab, 0.05)

# Compute the median, 5% quantile, and 95% quantile for the Liberal Democrats for each year in the Blue Wall
ld_2010 <- quantile(bw_2010$LD, 0.5)
ld_2015 <- quantile(bw_2015$LD, 0.5)
ld_2017 <- quantile(bw_2017$LD, 0.5)
ld_2019 <- quantile(bw_2019$LD, 0.5)
ld_2024 <- quantile(bw_2024$LD, 0.5)
ld_upper_2010 <- quantile(bw_2010$LD, 0.95)
ld_upper_2015 <- quantile(bw_2015$LD, 0.95)
ld_upper_2017 <- quantile(bw_2017$LD, 0.95)
ld_upper_2019 <- quantile(bw_2019$LD, 0.95)
ld_upper_2024 <- quantile(bw_2024$LD, 0.95)
ld_lower_2010 <- quantile(bw_2010$LD, 0.05)
ld_lower_2015 <- quantile(bw_2015$LD, 0.05)
ld_lower_2017 <- quantile(bw_2017$LD, 0.05)
ld_lower_2019 <- quantile(bw_2019$LD, 0.05)
ld_lower_2024 <- quantile(bw_2024$LD, 0.05)

# Store median values for each year as a vector
con_bw <- c(con_2010, con_2015, con_2017, con_2019, con_2024)
lab_bw <- c(lab_2010, lab_2015, lab_2017, lab_2019, lab_2024)
ld_bw <- c(ld_2010, ld_2015, ld_2017, ld_2019, ld_2024)

# Store 95% quantiles for each year as a vector
con_upper_bw <- c(con_upper_2010, con_upper_2015, con_upper_2017, con_upper_2019, con_upper_2024)
lab_upper_bw <- c(lab_upper_2010, lab_upper_2015, lab_upper_2017, lab_upper_2019, lab_upper_2024)
ld_upper_bw <- c(ld_upper_2010, ld_upper_2015, ld_upper_2017, ld_upper_2019, ld_upper_2024)

# Store 5% quantiles for each year as a vector
con_lower_bw <- c(con_lower_2010, con_lower_2015, con_lower_2017, con_lower_2019, con_lower_2024)
lab_lower_bw <- c(lab_lower_2010, lab_lower_2015, lab_lower_2017, lab_lower_2019, lab_lower_2024)
ld_lower_bw <- c(ld_lower_2010, ld_lower_2015, ld_lower_2017, ld_lower_2019, ld_lower_2024)

# Store median, 5% quantiles, 95% quantiles in data frame
blue_wall <- data.frame(matrix(nrow = 15, ncol = 0))
blue_wall$Year <- rep(c(2010, 2015, 2017, 2019, 2024), 3)
blue_wall$`Vote Share` <- c(con_bw, lab_bw, ld_bw)
blue_wall$Lower <- c(con_lower_bw, lab_lower_bw, ld_lower_bw)
blue_wall$Upper <- c(con_upper_bw, lab_upper_bw, ld_upper_bw)
blue_wall$Party <- rep(c('Con', 'Lab', 'LD'), rep(5,3))
blue_wall$Party <- factor(blue_wall$Party)

# Create and save plot
ggplot(blue_wall, aes(x = Year, y = `Vote Share`, colour = Party)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(x = Year, ymin = Lower, ymax = Upper, colour = Party), alpha = 0.1) +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  ylab('Vote Share (%)') +
  theme_bw()

ggsave('Blue Wall Vote Share.png')





