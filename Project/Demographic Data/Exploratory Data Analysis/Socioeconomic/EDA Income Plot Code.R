
# This file produces a plot in Section 3.3.3

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load income data
income_2010 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data Old Boundaries.xlsx', sheet = '2010')
income_2015 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data Old Boundaries.xlsx', sheet = '2015')
income_2017 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data Old Boundaries.xlsx', sheet = '2017')
income_2019 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data Old Boundaries.xlsx', sheet = '2019')
income_2024 <- read_excel('~/Project/Demographic Data/Data Sorting/Income/Income Data New Boundaries.xlsx', sheet = '2024')

# Load general election results from 2010 to 2024
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Remove unnecessary constituencies
income_2010 <- income_2010[income_2010$Constituency %in% results_2010$Constituency, ]
income_2015 <- income_2015[income_2015$Constituency %in% results_2015$Constituency, ]
income_2017 <- income_2017[income_2017$Constituency %in% results_2017$Constituency, ]
income_2019 <- income_2019[income_2019$Constituency %in% results_2019$Constituency, ]
income_2024 <- income_2024[income_2024$Constituency %in% results_2024$Constituency, ]

# Compute vote shares for each major party for each election
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

# Add vote shares to income data frames
income_2010$`Con Share` <- con_2010
income_2015$`Con Share` <- con_2015
income_2017$`Con Share` <- con_2017
income_2019$`Con Share` <- con_2019
income_2024$`Con Share` <- con_2024

income_2010$`Lab Share` <- lab_2010
income_2015$`Lab Share` <- lab_2015
income_2017$`Lab Share` <- lab_2017
income_2019$`Lab Share` <- lab_2019
income_2024$`Lab Share` <- lab_2024

income_2010$`LD Share` <- ld_2010
income_2015$`LD Share` <- ld_2015
income_2017$`LD Share` <- ld_2017
income_2019$`LD Share` <- ld_2019
income_2024$`LD Share` <- ld_2024

income_2010$`RFM Share` <- rfm_2010
income_2015$`RFM Share` <- rfm_2015
income_2017$`RFM Share` <- rfm_2017
income_2019$`RFM Share` <- rfm_2019
income_2024$`RFM Share` <- rfm_2024

income_2010$`GRN Share` <- grn_2010
income_2015$`GRN Share` <- grn_2015
income_2017$`GRN Share` <- grn_2017
income_2019$`GRN Share` <- grn_2019
income_2024$`GRN Share` <- grn_2024

# Add year to income data frames
income_2010$Year <- 2010
income_2015$Year <- 2015
income_2017$Year <- 2017
income_2019$Year <- 2019
income_2024$Year <- 2024

# Make names of data frames the same
names(income_2010) <- names(income_2019)
names(income_2015) <- names(income_2019)
names(income_2017) <- names(income_2019)
names(income_2024) <- names(income_2019)

# Combine income data frames into a single data frame
results_income <- rbind(income_2010, income_2015, income_2017, income_2019, income_2024)
results_income$Year <- as.factor(results_income$Year)

# Conservative income plot
plot_con <- ggplot(results_income, aes(x = `Adjusted Median Income`, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) +
  labs(x = 'Adjusted Median Income (GBP)', y = 'Vote Share (%)') +
  ggtitle('Conservative') +
  theme_bw() +
  theme(legend.position = 'none')

# Labour income plot
plot_lab <- ggplot(results_income, aes(x = `Adjusted Median Income`, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) +
  labs(x = 'Adjusted Median Income (GBP)', y = 'Vote Share (%)') +
  ggtitle('Labour') +
  theme_bw() +
  theme(legend.position = 'none')

# Lib Dem income plot
plot_ld <- ggplot(results_income, aes(x = `Adjusted Median Income`, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) +
  labs(x = 'Adjusted Median Income (GBP)', y = 'Vote Share (%)') +
  ggtitle('Liberal Democrats') +
  theme_bw() +
  theme(legend.position = 'none')

# Reform income plot
plot_rfm <- ggplot(results_income, aes(x = `Adjusted Median Income`, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) +
  labs(x = 'Adjusted Median Income (GBP)', y = 'Vote Share (%)') +
  ggtitle('Reform UK') +
  theme_bw() +
  theme(legend.position = 'none')

# Green income plot
plot_grn <- ggplot(results_income, aes(x = `Adjusted Median Income`, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) +
  labs(x = 'Adjusted Median Income (GBP)', y = 'Vote Share (%)') +
  ggtitle('Green Party') +
  theme_bw()

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_grn)

plot_grn <- plot_grn + theme(legend.position = 'none')

# Combine income plots into a single figure
income_plot <- grid.arrange(plot_con, plot_lab, plot_ld, plot_rfm, plot_grn, legend, ncol = 2, nrow = 3,
                             layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                             widths = c(2, 2), heights = c(4, 4, 4))

# Save income plot
ggsave('Income Breakdown.png', income_plot, width = 15.66, height = 12, units = 'cm')






