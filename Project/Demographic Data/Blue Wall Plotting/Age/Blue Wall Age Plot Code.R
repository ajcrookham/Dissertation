
# This file produces the plots in Section 3.4.2

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load in raw age data files
age_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2010 Age Data.xls', sheet = 'Mid-2010 Persons', skip = 3)
age_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2015 Age Data.xls', sheet = 'Mid-2015 Persons', skip = 3)
age_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2017 Age Data.xls', sheet = 'Mid-2017 Persons', skip = 3)
age_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2019 Age Data.xlsx', sheet = 'Mid-2019 Persons', skip = 3)
age_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/2022 Age Data.xlsx', sheet = 'Mid-2022 PCON 2025', skip = 3)

# Compute national proportions for each election
national_2010 <- c(colSums(age_2010[, 7] * 0.4), colSums(age_2010[, 8:21]))
nat_2010_18_29 <- sum(national_2010[1:3]) / sum(national_2010)
nat_2010_30_49 <- sum(national_2010[4:7]) / sum(national_2010)
nat_2010_50_64 <- sum(national_2010[8:10]) / sum(national_2010)
nat_2010_65 <- sum(national_2010[11:15]) / sum(national_2010)

national_2015 <- colSums(age_2015[, 22:94])
nat_2015_18_29 <- sum(national_2015[1:11]) / sum(national_2015)
nat_2015_30_49 <- sum(national_2015[12:31]) / sum(national_2015)
nat_2015_50_64 <- sum(national_2015[32:46]) / sum(national_2015)
nat_2015_65 <- sum(national_2015[47:73]) / sum(national_2015)

national_2017 <- colSums(age_2017[, 22:94])
nat_2017_18_29 <- sum(national_2017[1:11]) / sum(national_2017)
nat_2017_30_49 <- sum(national_2017[12:31]) / sum(national_2017)
nat_2017_50_64 <- sum(national_2017[32:46]) / sum(national_2017)
nat_2017_65 <- sum(national_2017[47:73]) / sum(national_2017)

national_2019 <- colSums(age_2019[, 22:94])
nat_2019_18_29 <- sum(national_2019[1:11]) / sum(national_2019)
nat_2019_30_49 <- sum(national_2019[12:31]) / sum(national_2019)
nat_2019_50_64 <- sum(national_2019[32:46]) / sum(national_2019)
nat_2019_65 <- sum(national_2019[47:73]) / sum(national_2019)

national_2024 <- colSums(age_2024[, c(22:94, 113:185)])
nat_2024_18_29 <- sum(national_2024[c(1:11, 74:84)]) / sum(national_2024)
nat_2024_30_49 <- sum(national_2024[c(12:31, 85:104)]) / sum(national_2024)
nat_2024_50_64 <- sum(national_2024[c(32:46, 105:119)]) / sum(national_2024)
nat_2024_65 <- sum(national_2024[c(47:73, 120:146)]) / sum(national_2024)

# Load 2010 and 2024 results and Blue Wall seats for new boundaries
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')
blue_wall <- read_excel('~/Project/Election Data/Blue Wall/Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Load age data for new boundaries
age_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2010')
age_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2015')
age_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2017')
age_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2019')
age_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Age/Age Data New Boundaries.xlsx', sheet = '2024')

# Add year to age data
age_2010$Year <- rep(2010, 543)
age_2015$Year <- rep(2015, 543)
age_2017$Year <- rep(2017, 543)
age_2019$Year <- rep(2019, 543)
age_2024$Year <- rep(2024, 543)

# Set constituencies to be factors
age_2010$Constituency <- as.factor(age_2010$Constituency)
age_2015$Constituency <- as.factor(age_2015$Constituency)
age_2017$Constituency <- as.factor(age_2017$Constituency)
age_2019$Constituency <- as.factor(age_2019$Constituency)
age_2024$Constituency <- as.factor(age_2024$Constituency)

# Create a data frame containing constituencies in the Blue Wall
blue_wall_age_2010 <- age_2010[age_2010$Constituency %in% blue_wall$Constituency, ]
blue_wall_age_2015 <- age_2015[age_2015$Constituency %in% blue_wall$Constituency, ]
blue_wall_age_2017 <- age_2017[age_2017$Constituency %in% blue_wall$Constituency, ]
blue_wall_age_2019 <- age_2019[age_2019$Constituency %in% blue_wall$Constituency, ]
blue_wall_age_2024 <- age_2024[age_2024$Constituency %in% blue_wall$Constituency, ]

names(blue_wall_age_2024) <- names(blue_wall_age_2010)
blue_wall_age <- rbind(blue_wall_age_2010, blue_wall_age_2015, 
                       blue_wall_age_2017, blue_wall_age_2019, blue_wall_age_2024)

# Compute national average median age for each election
nat_median_2010 <- mean(age_2010$Median.Age)
nat_median_2015 <- mean(age_2015$Median.Age)
nat_median_2017 <- mean(age_2017$Median.Age)
nat_median_2019 <- mean(age_2019$Median.Age)
nat_median_2024 <- mean(age_2024$Median.Age)

# Create data frame for national age data
nat_2010 <- data.frame(list(factor('England and Wales'), factor('England and Wales'), nat_2010_18_29, nat_2010_30_49, nat_2010_50_64, nat_2010_65, nat_median_2010, 2010))
nat_2015 <- data.frame(list(factor('England and Wales'), factor('England and Wales'), nat_2015_18_29, nat_2015_30_49, nat_2015_50_64, nat_2015_65, nat_median_2015, 2015))
nat_2017 <- data.frame(list(factor('England and Wales'), factor('England and Wales'), nat_2017_18_29, nat_2017_30_49, nat_2017_50_64, nat_2017_65, nat_median_2017, 2017))
nat_2019 <- data.frame(list(factor('England and Wales'), factor('England and Wales'), nat_2019_18_29, nat_2019_30_49, nat_2019_50_64, nat_2019_65, nat_median_2019, 2019))
nat_2024 <- data.frame(list(factor('England and Wales'), factor('England and Wales'), nat_2024_18_29, nat_2024_30_49, nat_2024_50_64, nat_2024_65, nat_median_2024, 2024))

names(nat_2010) <- names(blue_wall_age)
names(nat_2015) <- names(blue_wall_age)
names(nat_2017) <- names(blue_wall_age)
names(nat_2019) <- names(blue_wall_age)
names(nat_2024) <- names(blue_wall_age)

nat_age <- rbind(nat_2010, nat_2015, nat_2017, nat_2019, nat_2024)

# Compute the average Blue Wall proportions and median age for each election
mean_18_2010 <- mean(blue_wall_age[blue_wall_age$Year == '2010', ]$X18.29)
mean_30_2010 <- mean(blue_wall_age[blue_wall_age$Year == '2010', ]$X30.49)
mean_50_2010 <- mean(blue_wall_age[blue_wall_age$Year == '2010', ]$X50.64)
mean_65_2010 <- mean(blue_wall_age[blue_wall_age$Year == '2010', ]$X65.)
bw_median_2010 <- mean(blue_wall_age[blue_wall_age$Year == '2010', ]$Median.Age)

mean_18_2015 <- mean(blue_wall_age[blue_wall_age$Year == '2015', ]$X18.29)
mean_30_2015 <- mean(blue_wall_age[blue_wall_age$Year == '2015', ]$X30.49)
mean_50_2015 <- mean(blue_wall_age[blue_wall_age$Year == '2015', ]$X50.64)
mean_65_2015 <- mean(blue_wall_age[blue_wall_age$Year == '2015', ]$X65.)
bw_median_2015 <- mean(blue_wall_age[blue_wall_age$Year == '2015', ]$Median.Age)

mean_18_2017 <- mean(blue_wall_age[blue_wall_age$Year == '2017', ]$X18.29)
mean_30_2017 <- mean(blue_wall_age[blue_wall_age$Year == '2017', ]$X30.49)
mean_50_2017 <- mean(blue_wall_age[blue_wall_age$Year == '2017', ]$X50.64)
mean_65_2017 <- mean(blue_wall_age[blue_wall_age$Year == '2017', ]$X65.)
bw_median_2017 <- mean(blue_wall_age[blue_wall_age$Year == '2017', ]$Median.Age)

mean_18_2019 <- mean(blue_wall_age[blue_wall_age$Year == '2019', ]$X18.29)
mean_30_2019 <- mean(blue_wall_age[blue_wall_age$Year == '2019', ]$X30.49)
mean_50_2019 <- mean(blue_wall_age[blue_wall_age$Year == '2019', ]$X50.64)
mean_65_2019 <- mean(blue_wall_age[blue_wall_age$Year == '2019', ]$X65.)
bw_median_2019 <- mean(blue_wall_age[blue_wall_age$Year == '2019', ]$Median.Age)

mean_18_2024 <- mean(blue_wall_age[blue_wall_age$Year == '2024', ]$X18.29)
mean_30_2024 <- mean(blue_wall_age[blue_wall_age$Year == '2024', ]$X30.49)
mean_50_2024 <- mean(blue_wall_age[blue_wall_age$Year == '2024', ]$X50.64)
mean_65_2024 <- mean(blue_wall_age[blue_wall_age$Year == '2024', ]$X65.)
bw_median_2024 <- mean(blue_wall_age[blue_wall_age$Year == '2024', ]$Median.Age)

# Create data frame for Blue Wall age data
mean_age <- data.frame(matrix(nrow = 5, ncol = 0))

mean_age$Constituency <- rep('Blue Wall', 5)
mean_age$Region <- rep('Blue Wall', 5)
mean_age$X18.29 <- c(mean_18_2010, mean_18_2015, mean_18_2017, mean_18_2019, mean_18_2024)
mean_age$X30.49 <- c(mean_30_2010, mean_30_2015, mean_30_2017, mean_30_2019, mean_30_2024)
mean_age$X50.64 <- c(mean_50_2010, mean_50_2015, mean_50_2017, mean_50_2019, mean_50_2024)
mean_age$X65. <- c(mean_65_2010, mean_65_2015, mean_65_2017, mean_65_2019, mean_65_2024)
mean_age$Median.Age <- c(bw_median_2010, bw_median_2015, bw_median_2017, bw_median_2019, bw_median_2024)
mean_age$Year <- c(2010, 2015, 2017, 2019, 2024)

# Combine all data frames into one
blue_wall_age <- rbind(blue_wall_age, mean_age, nat_age[nat_age$Constituency == 'England and Wales', ])

# Set up colours and other variables for plots
blue_wall_age$Colours <- c(rep('Individual Seats', 200), rep('Blue Wall', 5), rep('England and Wales', 5))
colours <- c('Individual Seats' = 'black', 'England and Wales' = 'red', 'Blue Wall' = 'blue')
alphas <- c(rep(0.3, 200), rep(1, 10))

# Plot for proportion of 18 to 29
plot_18 <- ggplot(data = blue_wall_age, aes(x = Year, y = X18.29, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('18 to 29') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'top')

# Plot for proportion of 30 to 49
plot_30 <- ggplot(data = blue_wall_age, aes(x = Year, y = X30.49, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('30 to 49') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Plot for proportion of 50 to 64
plot_50 <- ggplot(data = blue_wall_age, aes(x = Year, y = X50.64, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('50 to 64') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Plot for proportion for over 65s
plot_65 <- ggplot(data = blue_wall_age, aes(x = Year, y = X65., group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('65+') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_18)

plot_18 <- plot_18 + theme(legend.position = 'none')

# Combine plots into single figure
age_plot <- grid.arrange(plot_18, plot_30, plot_50, plot_65, legend, ncol = 2, nrow = 3,
                        layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
                        widths = c(2, 2), heights = c(5, 5, 0.5))

# Save proportions plot
ggsave('Blue Wall Age.png', age_plot)

# Plot for median age
median_age_plot <- ggplot(data = blue_wall_age, aes(x = Year, y = Median.Age, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Median Age', colour = '') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Save median age plot
ggsave('Blue Wall Median Age.png', median_age_plot)
