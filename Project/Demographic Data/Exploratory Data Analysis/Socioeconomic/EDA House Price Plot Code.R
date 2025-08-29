
# This file produces a plot for Section 3.3.3

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load house price data
house_data <- read.csv('~/Project/Demographic Data/Data Sorting/House Prices/House Prices Data New Boundaries.csv')

# Create data frames for house price for each year
house_2010 <- house_data[house_data$Year == 2010, ]
house_2015 <- house_data[house_data$Year == 2015, ]
house_2017 <- house_data[house_data$Year == 2017, ]
house_2019 <- house_data[house_data$Year == 2019, ]
house_2024 <- house_data[house_data$Year == 2024, ]

# Load general election results on new boundaries
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2024')

# Compute vote shares for major parties for each election
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

rfm_2010 <- results_2010$RFM / results_2010$Total * 100
rfm_2015 <- results_2015$RFM / results_2015$Total * 100
rfm_2017 <- results_2017$RFM / results_2017$Total * 100
rfm_2019 <- results_2019$RFM / results_2019$Total * 100
rfm_2024 <- results_2024$RFM / results_2024$Total * 100

grn_2010 <- results_2010$Green / results_2010$Total * 100
grn_2015 <- results_2015$Green / results_2015$Total * 100
grn_2017 <- results_2017$Green / results_2017$Total * 100
grn_2019 <- results_2019$Green / results_2019$Total * 100
grn_2024 <- results_2024$Green / results_2024$Total * 100

# Add vote shares to house price data frames
house_2010$`Con Share` <- con_2010
house_2015$`Con Share` <- con_2015
house_2017$`Con Share` <- con_2017
house_2019$`Con Share` <- con_2019
house_2024$`Con Share` <- con_2024

house_2010$`Lab Share` <- lab_2010
house_2015$`Lab Share` <- lab_2015
house_2017$`Lab Share` <- lab_2017
house_2019$`Lab Share` <- lab_2019
house_2024$`Lab Share` <- lab_2024

house_2010$`LD Share` <- ld_2010
house_2015$`LD Share` <- ld_2015
house_2017$`LD Share` <- ld_2017
house_2019$`LD Share` <- ld_2019
house_2024$`LD Share` <- ld_2024

house_2010$`RFM Share` <- rfm_2010
house_2015$`RFM Share` <- rfm_2015
house_2017$`RFM Share` <- rfm_2017
house_2019$`RFM Share` <- rfm_2019
house_2024$`RFM Share` <- rfm_2024

house_2010$`GRN Share` <- grn_2010
house_2015$`GRN Share` <- grn_2015
house_2017$`GRN Share` <- grn_2017
house_2019$`GRN Share` <- grn_2019
house_2024$`GRN Share` <- grn_2024

# Combine house price data frames into a single data frame
house <- rbind(house_2010, house_2015, house_2017, house_2019, house_2024)
house$Year <- factor(house$Year)

# Conservative house price plot
plot_con <- ggplot(house, aes(x = AdjustedMedianHousePrice, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Adjusted Median House Price (GBP)', y = 'Vote Share (%)') +
  ggtitle('Conservative') +
  theme(legend.position = 'none')

# Labour house price plot
plot_lab <- ggplot(house, aes(x = AdjustedMedianHousePrice, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Adjusted Median House Price (GBP)', y = 'Vote Share (%)') +
  ggtitle('Labour') +
  theme(legend.position = 'none')

# Lib Dem house price plot
plot_ld <- ggplot(house, aes(x = AdjustedMedianHousePrice, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Adjusted Median House Price (GBP)', y = 'Vote Share (%)') +
  ggtitle('Liberal Democrats') +
  theme(legend.position = 'none')

# Reform house price plot
plot_rfm <- ggplot(house, aes(x = AdjustedMedianHousePrice, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Adjusted Median House Price (GBP)', y = 'Vote Share (%)') +
  ggtitle('Reform UK') +
  theme(legend.position = 'none')

# Green house price plot
plot_grn <- ggplot(house, aes(x = AdjustedMedianHousePrice, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Adjusted Median House Price (GBP)', y = 'Vote Share (%)') +
  ggtitle('Green Party')
  
get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }

legend <- get_legend(plot_grn)

plot_grn <- plot_grn + theme(legend.position = 'none')

# Combine house price plots into a single figure
house_price_plot <- grid.arrange(plot_con, plot_lab, plot_ld, plot_rfm, plot_grn, legend, ncol = 2, nrow = 3,
                            layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                            widths = c(2, 2), heights = c(4, 4, 4))

# Save house price plot
ggsave('House Price Breakdown.png', house_price_plot, width = 15.66, height = 12, units = 'cm')


