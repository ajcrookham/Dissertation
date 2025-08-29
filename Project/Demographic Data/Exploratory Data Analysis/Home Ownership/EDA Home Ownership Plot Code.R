
# This file produces three plots in Section 3.3.4

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load housing data and combine into a single data frame
housing_old <- read_excel('~/Project/Demographic Data/Data Sorting/Home Ownership/Home Ownership Old Boundaries All Years.xlsx', sheet = 'Home Ownership')
housing_new <- read_excel('~/Project/Demographic Data/Data Sorting/Home Ownership/Home Ownership New Boundaries All Years.xlsx', sheet = 'Home Ownership')
housing_new <- housing_new[, c(1, 3:7)]
housing <- rbind(housing_old[1:2132, ], housing_new[housing_new$Year == 2024, ])

# Load general election results from 2010 to 2024
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2010')
results_2015 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2015')
results_2017 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2017')
results_2019 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2019')
results_2024 <- read_excel('~/Project/Election Data/Election Results/Election Results.xlsx', sheet = '2024')

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

# Add vote shares to housing data frame and compute proportions
housing$`Con Share` <- c(con_2010, con_2015, con_2017, con_2019, con_2024)
housing$`Lab Share` <- c(lab_2010, lab_2015, lab_2017, lab_2019, lab_2024)
housing$`LD Share` <- c(ld_2010, ld_2015, ld_2017, ld_2019, ld_2024)
housing$`RFM Share` <- c(rfm_2010, rfm_2015, rfm_2017, rfm_2019, rfm_2024)
housing$`GRN Share` <- c(grn_2010, grn_2015, grn_2017, grn_2019, grn_2024)
housing$`Owners Prop` <- housing$Owners / housing$Total
housing$`Private Prop` <- housing$`Private renters` / housing$Total
housing$`Social Prop` <- housing$`Social renters` / housing$Total
housing$Year <- as.factor(housing$Year)

# Conservative owners plot
plot_con_own <- ggplot(housing, aes(x = `Owners Prop`, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Conservative') +
  theme(legend.position = 'none')

# Labour owners plot
plot_lab_own <- ggplot(housing, aes(x = `Owners Prop`, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Labour') +
  theme(legend.position = 'none')

# Lib Dem owners plot
plot_ld_own <- ggplot(housing, aes(x = `Owners Prop`, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Liberal Democrats') +
  theme(legend.position = 'none')

# Reform owners plot
plot_rfm_own <- ggplot(housing, aes(x = `Owners Prop`, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Reform UK') +
  theme(legend.position = 'none')

# Green owners plot
plot_grn_own <- ggplot(housing, aes(x = `Owners Prop`, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Green Party')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_grn_own)

plot_grn_own <- plot_grn_own + theme(legend.position = 'none')

# Combine owners plots into a single figure
ownership_plot <- grid.arrange(plot_con_own, plot_lab_own, plot_ld_own, plot_rfm_own, plot_grn_own, legend, ncol = 2, nrow = 3,
                             layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                             widths = c(2, 2), heights = c(4, 4, 4))

# Save home owners plot
ggsave('Home Ownership Breakdown.png', ownership_plot, width = 15.66, height = 12, units = 'cm')

# Conservative private plot
plot_con_pri <- ggplot(housing, aes(x = `Private Prop`, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Conservative') +
  theme(legend.position = 'none')

# Labour private plot
plot_lab_pri <- ggplot(housing, aes(x = `Private Prop`, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Labour') +
  theme(legend.position = 'none')

# Lib Dem private plot
plot_ld_pri <- ggplot(housing, aes(x = `Private Prop`, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Liberal Democrats') +
  theme(legend.position = 'none')

# Reform private plot
plot_rfm_pri <- ggplot(housing, aes(x = `Private Prop`, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Reform UK') +
  theme(legend.position = 'none')

# Green private plot
plot_grn_pri <- ggplot(housing, aes(x = `Private Prop`, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Green Party')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_grn_pri)

plot_grn_pri <- plot_grn_pri + theme(legend.position = 'none')

# Combine private plots into a single figure
private_plot <- grid.arrange(plot_con_pri, plot_lab_pri, plot_ld_pri, plot_rfm_pri, plot_grn_pri, legend, ncol = 2, nrow = 3,
                               layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                               widths = c(2, 2), heights = c(4, 4, 4))

# Save private plot
ggsave('Private Renters Breakdown.png', private_plot, width = 15.66, height = 12, units = 'cm')

# Conservative social plot
plot_con_soc <- ggplot(housing, aes(x = `Social Prop`, y = `Con Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Conservative') +
  theme(legend.position = 'none')

# Labour social plot
plot_lab_soc <- ggplot(housing, aes(x = `Social Prop`, y = `Lab Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Labour') +
  theme(legend.position = 'none')

# Lib Dem social plot
plot_ld_soc <- ggplot(housing, aes(x = `Social Prop`, y = `LD Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Liberal Democrats') +
  theme(legend.position = 'none')

# Reform social plot
plot_rfm_soc <- ggplot(housing, aes(x = `Social Prop`, y = `RFM Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Reform UK') +
  theme(legend.position = 'none')

# Green social plot
plot_grn_soc <- ggplot(housing, aes(x = `Social Prop`, y = `GRN Share`, colour = Year)) +
  geom_smooth(level = 0) +
  theme_bw() +
  labs(x = 'Proportion', y = 'Vote Share (%)') +
  ggtitle('Green Party')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_grn_soc)

plot_grn_soc <- plot_grn_soc + theme(legend.position = 'none')

# Combine social plots into a single figure
social_plot <- grid.arrange(plot_con_soc, plot_lab_soc, plot_ld_soc, plot_rfm_soc, plot_grn_soc, legend, ncol = 2, nrow = 3,
                             layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                             widths = c(2, 2), heights = c(4, 4, 4))

# Save social plot
ggsave('Social Renters Breakdown.png', social_plot, width = 15.66, height = 12, units = 'cm')




