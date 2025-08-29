
# This file finds seats that had an unusual swing in vote share for Con, Lab, or LD in the Blue Wall in 2024

# Load required packages
library(readxl)

# Set working directory to Source File location

# Load in the required general election results
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2024')

# Load in the Blue Wall on the 2024 electoral map
blue_wall_new <- read_excel('Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Remove constituencies not in the Blue Wall
bw_2019 <- results_2019[results_2019$Constituency %in% blue_wall_new$Constituency, ]
bw_2024 <- results_2024[results_2024$Constituency %in% blue_wall_new$Constituency, ]

bw_con_swing <- bw_2024$Con / bw_2024$Total - bw_2019$Con / bw_2019$Total
bw_lab_swing <- bw_2024$Lab / bw_2024$Total - bw_2019$Lab / bw_2019$Total
bw_ld_swing <- bw_2024$LD / bw_2024$Total - bw_2019$LD / bw_2019$Total

# Average vote swing in England for each party
mean_swing_con <- mean(results_2024$Con / results_2024$Total - results_2019$Con / results_2019$Total)
mean_swing_lab <- mean(results_2024$Lab / results_2024$Total - results_2019$Lab / results_2019$Total)
mean_swing_ld <- mean(results_2024$LD / results_2024$Total - results_2019$LD / results_2019$Total)

# Standard deviation of vote swing in England for each party
sd_swing_con <- sd(results_2024$Con / results_2024$Total - results_2019$Con / results_2019$Total)
sd_swing_lab <- sd(results_2024$Lab / results_2024$Total - results_2019$Lab / results_2019$Total)
sd_swing_ld <- sd(results_2024$LD / results_2024$Total - results_2019$LD / results_2019$Total)

# Compute the upper and lower bounds for potential outliers for each party
lower_con <- mean_swing_con - 2 * sd_swing_con
lower_lab <- mean_swing_lab - 2 * sd_swing_lab
lower_ld <- mean_swing_ld - 2 * sd_swing_ld
upper_con <- mean_swing_con + 2 * sd_swing_con
upper_lab <- mean_swing_lab + 2 * sd_swing_lab
upper_ld <- mean_swing_ld + 2 * sd_swing_ld

# Find which seats are outliers within the Blue Wall
con_outliers <- bw_2024[!(lower_con < bw_con_swing & bw_con_swing < upper_con), ]
lab_outliers <- bw_2024[!(lower_lab < bw_lab_swing & bw_lab_swing < upper_lab),]
ld_outliers <- bw_2024[!(lower_ld < bw_ld_swing & bw_ld_swing < upper_ld),]

# Output the tibbles of outliers within the Blue Wall
con_outliers
lab_outliers
ld_outliers


