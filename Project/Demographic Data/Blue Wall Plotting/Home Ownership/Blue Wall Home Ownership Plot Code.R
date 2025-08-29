
# This file produces the plot in Section 3.4.6

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load in home ownership data
home_own <- read_excel('~/Project/Demographic Data/Data Sorting/Home Ownership/Home Ownership New Boundaries All Years.xlsx', sheet = 'Home Ownership')

# Create data frames with home ownership data for each election
home_own_2010 <- home_own[home_own$Year == 2010, ]
home_own_2015 <- home_own[home_own$Year == 2015, ]
home_own_2017 <- home_own[home_own$Year == 2017, ]
home_own_2019 <- home_own[home_own$Year == 2019, ]
home_own_2024 <- home_own[home_own$Year == 2024, ]

# Compute national proportions for home owners, private renters, and social renters for each election
nat_home_owners_2010 <- sum(home_own_2010$Owners) / sum(home_own_2010$Total)
nat_home_pri_2010 <- sum(home_own_2010$`Private renters`) / sum(home_own_2010$Total)
nat_home_soc_2010 <- sum(home_own_2010$`Social renters`) / sum(home_own_2010$Total)

nat_home_owners_2015 <- sum(home_own_2015$Owners) / sum(home_own_2015$Total)
nat_home_pri_2015 <- sum(home_own_2015$`Private renters`) / sum(home_own_2015$Total)
nat_home_soc_2015 <- sum(home_own_2015$`Social renters`) / sum(home_own_2015$Total)

nat_home_owners_2017 <- sum(home_own_2017$Owners) / sum(home_own_2017$Total)
nat_home_pri_2017 <- sum(home_own_2017$`Private renters`) / sum(home_own_2017$Total)
nat_home_soc_2017 <- sum(home_own_2017$`Social renters`) / sum(home_own_2017$Total)

nat_home_owners_2019 <- sum(home_own_2019$Owners) / sum(home_own_2019$Total)
nat_home_pri_2019 <- sum(home_own_2019$`Private renters`) / sum(home_own_2019$Total)
nat_home_soc_2019 <- sum(home_own_2019$`Social renters`) / sum(home_own_2019$Total)

nat_home_owners_2024 <- sum(home_own_2024$Owners) / sum(home_own_2024$Total)
nat_home_pri_2024 <- sum(home_own_2024$`Private renters`) / sum(home_own_2024$Total)
nat_home_soc_2024 <- sum(home_own_2024$`Social renters`) / sum(home_own_2024$Total)

# Create data frame for national proportions
nat_home_prop <- data.frame(matrix(nrow = 5, ncol = 0))
nat_home_prop$Constituency <- rep('UK', 5)
nat_home_prop$Year <- c(2010, 2015, 2017, 2019, 2024)
nat_home_prop$Owners <- c(nat_home_owners_2010, nat_home_owners_2015, nat_home_owners_2017, nat_home_owners_2019, nat_home_owners_2024)
nat_home_prop$Private <- c(nat_home_pri_2010, nat_home_pri_2015, nat_home_pri_2017, nat_home_pri_2019, nat_home_pri_2024)
nat_home_prop$Social <- c(nat_home_soc_2010, nat_home_soc_2015, nat_home_soc_2017, nat_home_soc_2019, nat_home_soc_2024)

# Load Blue Wall seats
blue_wall <- read_excel('~/Project/Election Data/Blue Wall/Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Create data frames for blue wall home ownership data for each election
blue_wall_home_2010 <- home_own_2010[home_own_2010$Constituency %in% blue_wall$Constituency, ]
blue_wall_home_2015 <- home_own_2015[home_own_2015$Constituency %in% blue_wall$Constituency, ]
blue_wall_home_2017 <- home_own_2017[home_own_2017$Constituency %in% blue_wall$Constituency, ]
blue_wall_home_2019 <- home_own_2019[home_own_2019$Constituency %in% blue_wall$Constituency, ]
blue_wall_home_2024 <- home_own_2024[home_own_2024$Constituency %in% blue_wall$Constituency, ]

# Compute Blue Wall proportions for each election
bw_home_owners_2010 <- sum(blue_wall_home_2010$Owners) / sum(blue_wall_home_2010$Total)
bw_home_pri_2010 <- sum(blue_wall_home_2010$`Private renters`) / sum(blue_wall_home_2010$Total)
bw_home_soc_2010 <- sum(blue_wall_home_2010$`Social renters`) / sum(blue_wall_home_2010$Total)

bw_home_owners_2015 <- sum(blue_wall_home_2015$Owners) / sum(blue_wall_home_2015$Total)
bw_home_pri_2015 <- sum(blue_wall_home_2015$`Private renters`) / sum(blue_wall_home_2015$Total)
bw_home_soc_2015 <- sum(blue_wall_home_2015$`Social renters`) / sum(blue_wall_home_2015$Total)

bw_home_owners_2017 <- sum(blue_wall_home_2017$Owners) / sum(blue_wall_home_2017$Total)
bw_home_pri_2017 <- sum(blue_wall_home_2017$`Private renters`) / sum(blue_wall_home_2017$Total)
bw_home_soc_2017 <- sum(blue_wall_home_2017$`Social renters`) / sum(blue_wall_home_2017$Total)

bw_home_owners_2019 <- sum(blue_wall_home_2019$Owners) / sum(blue_wall_home_2019$Total)
bw_home_pri_2019 <- sum(blue_wall_home_2019$`Private renters`) / sum(blue_wall_home_2019$Total)
bw_home_soc_2019 <- sum(blue_wall_home_2019$`Social renters`) / sum(blue_wall_home_2019$Total)

bw_home_owners_2024 <- sum(blue_wall_home_2024$Owners) / sum(blue_wall_home_2024$Total)
bw_home_pri_2024 <- sum(blue_wall_home_2024$`Private renters`) / sum(blue_wall_home_2024$Total)
bw_home_soc_2024 <- sum(blue_wall_home_2024$`Social renters`) / sum(blue_wall_home_2024$Total)

# Create data frame for Blue Wall proportions
bw_home_prop <- data.frame(matrix(nrow = 5, ncol = 0))
bw_home_prop$Constituency <- rep('Blue Wall', 5)
bw_home_prop$Year <- c(2010, 2015, 2017, 2019, 2024)
bw_home_prop$Owners <- c(bw_home_owners_2010, bw_home_owners_2015, bw_home_owners_2017, bw_home_owners_2019, bw_home_owners_2024)
bw_home_prop$Private <- c(bw_home_pri_2010, bw_home_pri_2015, bw_home_pri_2017, bw_home_pri_2019, bw_home_pri_2024)
bw_home_prop$Social <- c(bw_home_soc_2010, bw_home_soc_2015, bw_home_soc_2017, bw_home_soc_2019, bw_home_soc_2024)

# Create data frame for individual constituencies
home_own_prop <- data.frame(matrix(nrow = 2715, ncol = 0))
home_own_prop$Constituency <- as.factor(home_own$Constituency)
home_own_prop$Year <- home_own$Year
home_own_prop$Owners <- home_own$Owners / home_own$Total
home_own_prop$Private <- home_own$`Private renters` / home_own$Total
home_own_prop$Social <- home_own$`Social renters` / home_own$Total

# Reduce data frame to just Blue Wall seats
blue_wall_prop <- home_own_prop[home_own_prop$Constituency %in% blue_wall$Constituency, ]

# Combine data frames
blue_wall_prop <- rbind(blue_wall_prop, bw_home_prop, nat_home_prop)

# Set up variables for plotting
blue_wall_prop$Colours <- c(rep('Individual Seats', 200), rep('Blue Wall', 5), rep('England', 5))
colours <- c('Individual Seats' = 'black', 'England' = 'red', 'Blue Wall' = 'blue')
alphas <- c(rep(0.3, 200), rep(1, 10))

# Home Owners plot
plot_own <- ggplot(data = blue_wall_prop, aes(x = Year, y = Owners, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('Home Owners') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'bottom')

# Private renters plot
plot_pri <- ggplot(data = blue_wall_prop, aes(x = Year, y = Private, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide=FALSE) +
  labs(y = 'Proportion', colour = '') +
  ggtitle('Private renters') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Social renters plot
plot_soc <- ggplot(data = blue_wall_prop, aes(x = Year, y = Social, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('Social renters') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_own)

plot_own <- plot_own + theme(legend.position = 'none')

# Combine plots into one figure
home_plot <- grid.arrange(plot_own, plot_pri, plot_soc, legend, ncol = 1, nrow = 4,
                          heights = c(3, 3, 3, 0.5))

# Save plot
ggsave('Blue Wall Home Ownership.png', home_plot,
       width = 12, height = 15, units = 'cm')





