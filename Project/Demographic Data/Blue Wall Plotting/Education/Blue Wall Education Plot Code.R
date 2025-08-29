
# This file produces the plot in Section 3.4.3

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)

# Set working directory to Source File location

# Load education data
education <- read_excel('~/Project/Demographic Data/Data Sorting/Education/Qualifications New Boundaries All Years.xlsx', sheet = 'Qualifications')

# Create data frames for each year
edu_2010 <- education[education$Year == 2010, ]
edu_2015 <- education[education$Year == 2015, ]
edu_2017 <- education[education$Year == 2017, ]
edu_2019 <- education[education$Year == 2019, ]
edu_2024 <- education[education$Year == 2024, ]

# Compute national proportions for degrees and no qualifications for each election
nat_level_4_2010 <- sum(edu_2010$`Level 4`) / sum(edu_2010$Total)
nat_no_qual_2010 <- sum(edu_2010$No) / sum(edu_2010$Total)

nat_level_4_2015 <- sum(edu_2015$`Level 4`) / sum(edu_2015$Total)
nat_no_qual_2015 <- sum(edu_2015$No) / sum(edu_2015$Total)

nat_level_4_2017 <- sum(edu_2017$`Level 4`) / sum(edu_2017$Total)
nat_no_qual_2017 <- sum(edu_2017$No) / sum(edu_2017$Total)

nat_level_4_2019 <- sum(edu_2019$`Level 4`) / sum(edu_2019$Total)
nat_no_qual_2019 <- sum(edu_2019$No) / sum(edu_2019$Total)

nat_level_4_2024 <- sum(edu_2024$`Level 4`) / sum(edu_2024$Total)
nat_no_qual_2024 <- sum(edu_2024$No) / sum(edu_2024$Total)

# Put national proportions into data frame
nat_edu_prop <- data.frame(matrix(nrow = 5, ncol = 0))
nat_edu_prop$Constituency <- rep('UK', 5)
nat_edu_prop$Year <- c(2010, 2015, 2017, 2019, 2024)
nat_edu_prop$`Level 4` <- c(nat_level_4_2010, nat_level_4_2015, nat_level_4_2017, nat_level_4_2019, nat_level_4_2024)
nat_edu_prop$`No Qual` <- c(nat_no_qual_2010, nat_no_qual_2015, nat_no_qual_2017, nat_no_qual_2019, nat_no_qual_2024)

# Load Blue Wall seats
blue_wall <- read_excel('~/Project/Election Data/Blue Wall/Blue Wall Seats.xlsx', sheet = 'New Boundaries')

# Create data frames for Blue Wall seats for each election
bw_edu_2010 <- edu_2010[edu_2010$Constituency %in% blue_wall$Constituency, ]
bw_edu_2015 <- edu_2015[edu_2015$Constituency %in% blue_wall$Constituency, ]
bw_edu_2017 <- edu_2017[edu_2017$Constituency %in% blue_wall$Constituency, ]
bw_edu_2019 <- edu_2019[edu_2019$Constituency %in% blue_wall$Constituency, ]
bw_edu_2024 <- edu_2024[edu_2024$Constituency %in% blue_wall$Constituency, ]

# Compute Blue Wall proportions for degrees and no qualifications for each election
bw_level_4_2010 <- sum(bw_edu_2010$`Level 4`) / sum(bw_edu_2010$Total)
bw_no_qual_2010 <- sum(bw_edu_2010$No) / sum(bw_edu_2010$Total)

bw_level_4_2015 <- sum(bw_edu_2015$`Level 4`) / sum(bw_edu_2015$Total)
bw_no_qual_2015 <- sum(bw_edu_2015$No) / sum(bw_edu_2015$Total)

bw_level_4_2017 <- sum(bw_edu_2017$`Level 4`) / sum(bw_edu_2017$Total)
bw_no_qual_2017 <- sum(bw_edu_2017$No) / sum(bw_edu_2017$Total)

bw_level_4_2019 <- sum(bw_edu_2019$`Level 4`) / sum(bw_edu_2019$Total)
bw_no_qual_2019 <- sum(bw_edu_2019$No) / sum(bw_edu_2019$Total)

bw_level_4_2024 <- sum(bw_edu_2024$`Level 4`) / sum(bw_edu_2024$Total)
bw_no_qual_2024 <- sum(bw_edu_2024$No) / sum(bw_edu_2024$Total)

# Create data frame for Blue Wall proportions
bw_edu_prop <- data.frame(matrix(nrow = 5, ncol = 0))
bw_edu_prop$Constituency <- rep('Blue Wall', 5)
bw_edu_prop$Year <- c(2010, 2015, 2017, 2019, 2024)
bw_edu_prop$`Level 4` <- c(bw_level_4_2010, bw_level_4_2015, bw_level_4_2017, bw_level_4_2019, bw_level_4_2024)
bw_edu_prop$`No Qual` <- c(bw_no_qual_2010, bw_no_qual_2015, bw_no_qual_2017, bw_no_qual_2019, bw_no_qual_2024)

# Compute proportions for degrees and no qualifications for each constituency
edu_prop <- data.frame(matrix(nrow = 2715, ncol = 0))
edu_prop$Constituency <- education$Constituency
edu_prop$Year <- education$Year
edu_prop$`Level 4` <- education$`Level 4` / education$Total
edu_prop$`No Qual` <- education$No / education$Total

# Create data frame for individual constituency proportions
blue_wall_qual <- edu_prop[edu_prop$Constituency %in% blue_wall$Constituency, ]

# Create data frame for all proportions - national, blue wall, and individual
blue_wall_qual <- rbind(blue_wall_qual, nat_edu_prop, bw_edu_prop)

# Define variables for plotting
blue_wall_qual$Colours <- c(rep('Individual Seats', 200), rep('England and Wales', 5), rep('Blue Wall', 5))
colours <- c('Individual Seats' = 'black', 'England and Wales' = 'red', 'Blue Wall' = 'blue')
alphas <- c(rep(0.3, 200), rep(1, 10))

# Plot for no qualifications proportion
no_qual_plot <- ggplot(data = blue_wall_qual, aes(x = Year, y = `No Qual`, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('No Qualifications') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'bottom')

# Plot for degree proportion
level_4_plot <- ggplot(data = blue_wall_qual, aes(x = Year, y = `Level 4`, group = Constituency, colour = Colours, alpha = alphas)) +
  geom_line() +
  scale_color_manual(values = colours) +
  scale_alpha_continuous(guide='none') +
  labs(y = 'Proportion', colour = '') +
  ggtitle('Degree') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(no_qual_plot)

no_qual_plot <- no_qual_plot + theme(legend.position = 'none')

# Combine two plots into one figure
qual_plot <- grid.arrange(no_qual_plot, level_4_plot, legend, ncol = 2, nrow = 2,
                          layout_matrix = rbind(c(1,2), c(3,3)),
                          widths = c(2,2), heights = c(5, 0.5))

# Save plot
ggsave('Blue Wall Education.png', qual_plot)



