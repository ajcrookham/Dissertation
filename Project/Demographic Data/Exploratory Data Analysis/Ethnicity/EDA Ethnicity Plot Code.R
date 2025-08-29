
# This file produces a plot in Section 3.3.4

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load ethnicity data and combine into a single data frame
ethnicity_old <- read_excel('~/Project/Demographic Data/Data Sorting/Ethnicity/Ethnicity Old Boundaries All Years.xlsx', sheet = 'Ethnicity')
ethnicity_new <- read_excel('~/Project/Demographic Data/Data Sorting/Ethnicity/Ethnicity New Boundaries All Years.xlsx', sheet = 'Ethnicity')
ethnicity_new <- ethnicity_new[, c(1,3,4)]
ethnicity <- rbind(ethnicity_old[1:2132,], ethnicity_new[ethnicity_new$Year == 2024,])

# Load general election results from 2010 to 2024
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

# Compute vote shares for the major parties for each election
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

# Add vote shares to ethnicity data frame
ethnicity$`Con Share` <- c(con_2010, con_2015, con_2017, con_2019, con_2024)
ethnicity$`Lab Share` <- c(lab_2010, lab_2015, lab_2017, lab_2019, lab_2024)
ethnicity$`LD Share` <- c(ld_2010, ld_2015, ld_2017, ld_2019, ld_2024)
ethnicity$`RFM Share` <- c(rfm_2010, rfm_2015, rfm_2017, rfm_2019, rfm_2024)
ethnicity$`GRN Share` <- c(grn_2010, grn_2015, grn_2017, grn_2019, grn_2024)
ethnicity$Year <- factor(ethnicity$Year)

# Conservative plot
plot_con <- ggplot(ethnicity, aes(x = `White`, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Conservative') +
  theme(legend.position = 'none')

# Labour plot
plot_lab <- ggplot(ethnicity, aes(x = `White`, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Labour') +
  theme(legend.position = 'none')

# Lib Dem plot
plot_ld <- ggplot(ethnicity, aes(x = `White`, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Liberal Democrats') +
  theme(legend.position = 'none')

# Reform plot
plot_rfm <- ggplot(ethnicity, aes(x = `White`, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Reform UK') +
  theme(legend.position = 'none')

# Green plot
plot_grn <- ggplot(ethnicity, aes(x = `White`, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Green')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_grn)

plot_grn <- plot_grn + theme(legend.position = 'none')

# Combine plots into a single figure
ethnicity_plot <- grid.arrange(plot_con, plot_lab, plot_ld, plot_rfm, plot_grn, legend, ncol = 2, nrow = 3,
                             layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                             widths = c(2, 2), heights = c(4, 4, 4))

# Save ethnicity plot
ggsave('Ethnicity Breakdown.png', ethnicity_plot, width = 15.66, height = 12, units = 'cm')

