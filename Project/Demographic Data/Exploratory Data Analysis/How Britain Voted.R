
# This file produces the plots in Section 3.3 involving Ipsos polling data

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Function to extract legend from ggplot
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Age plot - Section 3.3.1

# Load age polling data
age <- read_excel('How Britain Voted.xlsx', sheet = 'Age')
age_18_24 <- age[age$Age == '18-24', ]
age_65 <- age[age$Age == '65+', ]

# 18 to 24 plot
plot_18_24 <- ggplot(data = age_18_24, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('18-24') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Over 65 plot
plot_65 <- ggplot(data = age_65, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('65+') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

legend <- get_legend(plot_18_24)

plot_18_24 <- plot_18_24 + theme(legend.position = 'none')

# Combine age plots into a single figure
age_plot <- grid.arrange(plot_18_24, plot_65, legend, ncol = 2, nrow = 2,
                         layout_matrix = rbind(c(1,2), c(3,3)),
                         widths = c(2, 2), heights = c(5, 0.5))

# Save age plot to Age file
ggsave('Age/Age Vote Share.png', age_plot)


# Education plot - Section 3.3.2

# Load education polling data
qualifications <- read_excel('How Britain Voted.xlsx', sheet = 'Qualifications')
no_qual <- qualifications[qualifications$Qualifications == 'No qualifications', ]
degree <- qualifications[qualifications$Qualifications == 'Degree', ]

# No qualifications polling plot
no_qual_plot <- ggplot(data = no_qual, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('No qualifications') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Degree polling plot
degree_plot <- ggplot(data = degree, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('Degree') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

legend <- get_legend(no_qual_plot)

no_qual_plot <- no_qual_plot + theme(legend.position = 'none')

# Combine plots into a single figure
edu_plot <- grid.arrange(no_qual_plot, degree_plot, legend, ncol = 2, nrow = 2,
                         layout_matrix = rbind(c(1,2), c(3,3)),
                         widths = c(2, 2), heights = c(5, 0.5))

# Save education plot
ggsave('Education/Qualifications Vote Share.png', edu_plot)


# Socioeconomic plot - Section 3.3.3

# Load social class polling data
social <- read_excel('How Britain Voted.xlsx', sheet = 'Social Class')
AB <- social[social$`Social Class` == 'AB', ]
C1 <- social[social$`Social Class` == 'C1', ]
C2 <- social[social$`Social Class` == 'C2', ]
DE <- social[social$`Social Class` == 'DE', ]

# AB social class plot
AB_plot <- ggplot(data = AB, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('AB') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# C1 social class plot
C1_plot <- ggplot(data = C1, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('C1') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

# C2 social class plot
C2_plot <- ggplot(data = C2, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('C2') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

# DE social class plot
DE_plot <- ggplot(data = DE, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('DE') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

legend <- get_legend(AB_plot)

AB_plot <- AB_plot + theme(legend.position = 'none')

# Combine social class plots into a single figure
social_plot <- grid.arrange(AB_plot, C1_plot, C2_plot, DE_plot, legend, ncol = 2, nrow = 3,
                            layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
                            widths = c(2, 2), heights = c(5, 5, 0.5))

# Save social class plot
ggsave('Socioeconomic/Social Class Vote Share.png', social_plot)


# Ethnicity plot - Section 3.3.4

# Load ethnicity polling data
ethnicity <- read_excel('How Britain Voted.xlsx', sheet = 'Ethnicity')
white <- ethnicity[ethnicity$`Ethnic Group` == 'White', ]
bme <- ethnicity[ethnicity$`Ethnic Group` == 'BME', ]

# White plot
white_plot <- ggplot(data = white, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ggtitle('White') +
  ylab('Vote Share (%)') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# BME plot
bme_plot <- ggplot(data = bme, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ggtitle('BME') +
  ylab('Vote Share (%)') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

legend <- get_legend(white_plot)

white_plot <- white_plot + theme(legend.position = 'none')

# Combine plots into a single figure
ethnic_plot <- grid.arrange(white_plot, bme_plot, legend, ncol = 2, nrow = 2,
                            layout_matrix = rbind(c(1,2), c(3,3)),
                            widths = c(2, 2), heights = c(5, 0.5))
# Save ethnicity plot
ggsave('Ethnicity/Ethnicity Vote Share.png', ethnic_plot)


# Home Ownership plot - Section 3.3.5

# Load housing data
housing <- read_excel('How Britain Voted.xlsx', sheet = 'Housing')
own <- housing[housing$`Housing Tenure` == 'Owned', ]
mortgage <- housing[housing$`Housing Tenure` == 'Mortgage', ]
social_rent <- housing[housing$`Housing Tenure` == 'Social renter', ]
private_rent <- housing[housing$`Housing Tenure` == 'Private renter', ]

# Home owners plot
own_plot <- ggplot(data = own, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('Owns outright') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'bottom')

# Mortgage plot
mortgage_plot <- ggplot(data = mortgage, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('Owns with a mortgage') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

# Private renters plot
private_plot <- ggplot(data = private_rent, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('Private renting') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

# Social renters plot
social_plot <- ggplot(data = social_rent, aes(x = Year, y = Vote, group = Party, colour = Party)) +
  geom_point() +
  geom_line() +
  ylab('Vote Share (%)') +
  ggtitle('Social renting') +
  scale_colour_manual(values = c('blue', 'red', 'orange')) +
  theme_bw() +
  theme(legend.position = 'none')

legend <- get_legend(own_plot)

own_plot <- own_plot + theme(legend.position = 'none')

# Combine plots into a single figure
home_plot <- grid.arrange(own_plot, mortgage_plot, private_plot, social_plot, legend, ncol = 2, nrow = 3,
                         layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
                         widths = c(2, 2), heights = c(5, 5, 0.5))

# Save home ownership plot
ggsave('Home Ownership/Home Ownership Vote Share.png', home_plot)

