
# This file builds the CF-A models and computes the R squared values for the test set

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(yardstick)
library(rsample)
library(stargazer)

# Set working directory to Source File location

# Load 2010 results
results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2010')

# Load Adapted CF data
election_data_training <- read.csv('Adapted CF Model Data.csv')
election_data_training <- election_data_training[, 2:70]

new_names <- c('y_1', 'y_2', 'y_3', 'y_4', 'y_5', 'Year', 'X18.29', 'X30.49', 'X50.64', 'X65.', 'Median Age', 'pop_density', 'Level 4 Prop',
               'No Qualifications Prop', 'avg_unemployment', 'White', 'Proportion', 'Owners', 'Private renters',
               'Social renters', 'Total - Housing', 'home_prices', 'Median Income', 'inflation', 'child_poverty',
               'Nat Con', 'Nat Lab', 'Nat LD', 'Nat RFM', 'Nat GRN', 'Owner Prop', 'Private Prop', 'Social Prop',
               'X18.29 Change', 'X30.49 Change', 'X50.64 Change', 'X65. Change', 'Level 4 Prop Change', 
               'No Qualifications Prop Change', 'White Change', 'Owner Prop Change', 'Private Prop Change', 
               'Social Prop Change', 'House Prices Change', 'Median Income Change', 'Nat Con Change', 'Nat Lab Change',
               'Nat LD Change', 'Nat RFM Change', 'Nat GRN Change', 'Constituency', 'Con Share', 'Lab Share', 
               'LD Share', 'RFM Share', 'GRN Share', 'Winner', 'Con Share After', 'Lab Share After', 'LD Share After',
               'RFM Share After', 'GRN Share After', 'No GRN Before', 'No RFM Before', 'No LD Before', 'No Con Before',
               'No Lab Before', 'Year_2024', 'Blue_Wall')

names(election_data_training) <- new_names

# Create the test set
set.seed(36581771)

constituency_split <- results_2010 |>
  initial_split(prop = 0.8, strata = Region)

training_constituency <- training(constituency_split)
testing_constituency <- testing(constituency_split)

election_data_testing <- election_data_training[election_data_training$Constituency %in% testing_constituency$Constituency, ]
election_data_training <- election_data_training[election_data_training$Constituency %in% training_constituency$Constituency, ]


# Green model
null_1 <- lm(y_1 ~ 1, data = election_data_training[!election_data_training$`GRN Share After` == 0, ])
full_1 <- lm(y_1 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`
               , data = election_data_training[!election_data_training$`GRN Share After` == 0, c(1, 6:17, 22:25, 31:45, 64)])

model_build <- step(null_1, scope = list(lower = null_1, upper = full_1), direction = 'both', trace = 0)
mod_1 <- lm(model_build$terms, data = election_data_training[!election_data_training$`GRN Share After` == 0,])

summary(mod_1)

# Reform model
null_2 <- lm(y_2 ~ 1, data = election_data_training[!election_data_training$`RFM Share After` == 0,])
full_2 <- lm(y_2 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`
             , data = election_data_training[!election_data_training$`RFM Share After` == 0, c(2, 6:17, 22:25, 31:45, 64)])

model_build <- step(null_2, scope = list(lower = null_2, upper = full_2), direction = 'both', trace = 0)
mod_2 <- lm(model_build$terms,data = election_data_training[!election_data_training$`RFM Share After` == 0, ])

summary(mod_2)

# Lib Dem model
null_3 <- lm(y_3 ~ 1, data = election_data_training[!election_data_training$`LD Share After` == 0,])
full_3 <- lm(y_3 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`
             , data = election_data_training[!election_data_training$`LD Share After` == 0, c(3, 6:17, 22:25, 31:45, 64)])

model_build <- step(null_3, scope = list(lower = null_3, upper = full_3), direction = 'both', trace = 0)
mod_3 <- lm(model_build$terms,data = election_data_training[!election_data_training$`LD Share After` == 0,])

summary(mod_3)

# Conservatives model
null_4 <- lm(y_4 ~ 1, data = election_data_training[!election_data_training$`Con Share After` == 0, ])
full_4 <- lm(y_4 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`
             , data = election_data_training[!election_data_training$`Con Share After` == 0, c(4, 6:17, 22:25, 31:45, 64)])

model_build <- step(null_4, scope = list(lower = null_4, upper = full_4), direction = 'both', trace = 0)
mod_4 <- lm(model_build$terms, data = election_data_training[!election_data_training$`Con Share After` == 0,])

summary(mod_4)

# Labour model
null_5 <- lm(y_5 ~ 1, data = election_data_training[!election_data_training$`Lab Share After` == 0,])
full_5 <- lm(y_5 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`
             , data = election_data_training[!election_data_training$`Lab Share After` == 0, c(5, 6:17, 22:25, 31:45, 64)])

model_build <- step(null_5, scope = list(lower = null_5, upper = full_5), direction = 'both', trace = 0)
mod_5 <- lm(model_build$terms, data = election_data_training[!election_data_training$`Lab Share After` == 0,])

summary(mod_5)

# Predict upon the test set and compute predicted vote shares
grn_change <- predict(mod_1, newdata = election_data_testing)
grn <- election_data_testing$`GRN Share` + grn_change

rfm_change <- predict(mod_2, newdata = election_data_testing)
rfm <- election_data_testing$`RFM Share` + rfm_change

ld_change <- predict(mod_3, newdata = election_data_testing)
ld <- election_data_testing$`LD Share` + ld_change

con_change <- predict(mod_4, newdata = election_data_testing)
con <- election_data_testing$`Con Share` + con_change

lab_change <- predict(mod_5, newdata = election_data_testing)
lab <- election_data_testing$`Lab Share` + lab_change

# Set vote shares to zero when the party did not run
for (i in 1:440) {
  
  if (grn[i] < 0 | election_data_testing$`GRN Share After`[i] == 0) {
    
    grn[i] <- 0
    
  }
  
  if (rfm[i] < 0 | election_data_testing$`RFM Share After`[i] == 0) {
    
    rfm[i] <- 0
    
  }
  
  if (ld[i] < 0 | election_data_testing$`LD Share After`[i] == 0) {
    
    ld[i] <- 0
    
  }
  
  if (con[i] < 0 | election_data_testing$`Con Share After`[i] == 0) {
    
    con[i] <- 0
    
  }
  
  if (lab[i] < 0 | election_data_testing$`Lab Share After`[i] == 0) {
    
    lab[i] <- 0
    
  }
  
}

# Adjust vote shares to ensure sum to 100
total <- grn + rfm + ld + con + lab

grn <- grn / total * 100
rfm <- rfm / total * 100
ld <- ld / total * 100
con <- con / total * 100
lab <- lab / total * 100

# Create data frame for test set with predicted and actual vote shares
test <- data.frame(matrix(nrow = 440, ncol = 0))
test$Constituency <- election_data_testing$Constituency
test$Year <- as.factor(election_data_testing$Year)
test$`Con Model` <- con
test$`Lab Model` <- lab
test$`LD Model` <- ld
test$`RFM Model` <- rfm
test$`GRN Model` <- grn
test$`Con Actual` <- election_data_testing$`Con Share After`
test$`Lab Actual` <- election_data_testing$`Lab Share After`
test$`LD Actual` <- election_data_testing$`LD Share After`
test$`RFM Actual` <- election_data_testing$`RFM Share After`
test$`GRN Actual` <- election_data_testing$`GRN Share After`

# Conservatives plot
con_plot <- ggplot(test, aes(x = `Con Actual`, y = `Con Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Conservative') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Labour plot
lab_plot <- ggplot(test, aes(x = `Lab Actual`, y = `Lab Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Labour') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Lib Dem plot
ld_plot <- ggplot(test, aes(x = `LD Actual`, y = `LD Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Liberal Democrats') +
  theme_bw() +
  theme_classic() + 
  theme(legend.position = 'none')

# Reform plot
rfm_plot <- ggplot(test, aes(x = `RFM Actual`, y = `RFM Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Reform UK') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Green plot
grn_plot <- ggplot(test, aes(x = `GRN Actual`, y = `GRN Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Green Party') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'right')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(grn_plot)

grn_plot <- grn_plot + theme(legend.position = 'none')

# Combine plots into a single figure
cf_a_model_plot <- grid.arrange(con_plot, lab_plot, ld_plot, rfm_plot, grn_plot, legend, ncol = 2, nrow = 3,
                               layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                               widths = c(2, 2), heights = c(4, 4, 4))

ggsave('CF-A Model Vote Share.png', cf_a_model_plot, width = 15.66, height = 12, units = 'cm')

# Compute R squared values
con_rsq <- rsq(test, `Con Actual`, `Con Model`)$.estimate
lab_rsq <- rsq(test, `Lab Actual`, `Lab Model`)$.estimate
ld_rsq <- rsq(test, `LD Actual`, `LD Model`)$.estimate
rfm_rsq <- rsq(test, `RFM Actual`, `RFM Model`)$.estimate
grn_rsq <- rsq(test, `GRN Actual`, `GRN Model`)$.estimate

cf_a_rsq <- c(con_rsq, lab_rsq, ld_rsq, rfm_rsq, grn_rsq)
cf_a_rsq

# Compute R squared values for 2015, 2017, and 2019
con_rsq_2 <- rsq(test[!test$Year == 2024,], `Con Actual`, `Con Model`)$.estimate
lab_rsq_2 <- rsq(test[!test$Year == 2024,], `Lab Actual`, `Lab Model`)$.estimate

# Create data frame for change in vote share
test2 <- data.frame(matrix(nrow = 440, ncol = 0))
test2$Constituency <- election_data_testing$Constituency
test2$Year <- as.factor(election_data_testing$Year)
test2$`Con Model` <- con_change
test2$`Lab Model` <- lab_change
test2$`LD Model` <- ld_change
test2$`RFM Model` <- rfm_change
test2$`GRN Model` <- grn_change
test2$`Con Actual` <- election_data_testing$`Con Share After` - election_data_testing$`Con Share`
test2$`Lab Actual` <- election_data_testing$`Lab Share After` - election_data_testing$`Lab Share`
test2$`LD Actual` <- election_data_testing$`LD Share After` - election_data_testing$`LD Share`
test2$`RFM Actual` <- election_data_testing$`RFM Share After` - election_data_testing$`RFM Share`
test2$`GRN Actual` <- election_data_testing$`GRN Share After` - election_data_testing$`GRN Share`

# Conservative plot
con_plot <- ggplot(test2, aes(x = `Con Actual`, y = `Con Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Conservative') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Labour plot
lab_plot <- ggplot(test2, aes(x = `Lab Actual`, y = `Lab Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Labour') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Lib Dem plot
ld_plot <- ggplot(test2, aes(x = `LD Actual`, y = `LD Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Liberal Democrats') +
  theme_bw() +
  theme_classic() + 
  theme(legend.position = 'none')

# Reform plot
rfm_plot <- ggplot(test2, aes(x = `RFM Actual`, y = `RFM Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Reform UK') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'none')

# Green plot
grn_plot <- ggplot(test2, aes(x = `GRN Actual`, y = `GRN Model`, colour = Year)) +
  geom_point(size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, colour = 'black') +
  labs(x = 'Actual', y = 'Model', colour = 'Year') +
  ggtitle('Green Party') +
  theme_bw() +
  theme_classic() +
  theme(legend.position = 'right')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(grn_plot)

grn_plot <- grn_plot + theme(legend.position = 'none')

# Combine plots into a single figure
cf_a_model_change_plot <- grid.arrange(con_plot, lab_plot, ld_plot, rfm_plot, grn_plot, legend, ncol = 2, nrow = 3,
                                  layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                                  widths = c(2, 2), heights = c(4, 4, 4))

ggsave('CF-A Model Change.png', cf_a_model_change_plot, width = 15.66, height = 12, units = 'cm')

# R squared values for change in vote share
con_rsq_change <- rsq(test2, `Con Actual`, `Con Model`)$.estimate
lab_rsq_change <- rsq(test2, `Lab Actual`, `Lab Model`)$.estimate
ld_rsq_change <- rsq(test2, `LD Actual`, `LD Model`)$.estimate
rfm_rsq_change <- rsq(test2, `RFM Actual`, `RFM Model`)$.estimate
grn_rsq_change <- rsq(test2, `GRN Actual`, `GRN Model`)$.estimate

cf_a_rsq_change <- c(con_rsq_change, lab_rsq_change, ld_rsq_change, rfm_rsq_change, grn_rsq_change)
cf_a_rsq_change


# Writes LaTeX code for table in appendices

covariate.labels <- c('Intercept', '18 to 29', '30 to 49', '50 to 64', '65+', 'Change in 18 to 29',
                      'Change in 50 to 64', 'Change in 65+', 'Median Age', 'Degree',
                      'No Qualifications', 'Change in Degree', 'Change in No Qual.', 'Median Income', 'Median House Price',
                      'Change in Median Income', 'Median House Price Change', 'Inflation', 'Unemployment', 
                      'Child Poverty', 'Fuel Poverty', 'Ethnicity', 'Change in Ethnicity',
                      'Home Owners', 'Private Housing', 'Social Housing',
                      'Home Owners Change', 'Private Housing Change', 'Social Housing Change', 'No RFM Before',
                      '18 to 29 Int.', '50 to 64 Int.', 'Degree Int.', 'Degree Int. 2',
                      'No Qual. Int.', 'No Qual. Int. 2', 'Median Income Int.', 'Median House Price Int.', 'Ethnicity Int.',
                      'Home Owners Int.', 'Private Housing Int.')


covariate.stargazer <- c('Intercept', 'Change in Degree', 'Inflation', 'Unemployment', 'Home Owners',
                         'No Qualifications', 'Degree', '30 to 49', 'Change in Median Income', 'Social Housing Change',
                         'Fuel Poverty', 'No RFM Before', 'Social Housing', 'Home Owners Change',
                         'Median House Price Change'  ,'Private Housing Change', 'Change in Ethnicity', 'No Qual. Int.',
                         'Degree Int.', 'Median Income Int.', 'Median House Price Int.', 'Degree Int. 2', 'Ethnicity Int.',
                         'Home Owners Int.', '50 to 64 Int.', 'Change in No Qual.', 'Ethnicity', '65+', 'Median House Price',
                         '18 to 29', 'Child Poverty', 'Median Age', 'Private Housing', 'Change in 65+', 'Change in 50 to 64',
                         'Change in 18 to 29', '50 to 64', 'Median Income', 'No Qual. Int. 2', 'Private Housing Int.',
                         '18 to 29 Int.')

covariate.order <- match(covariate.labels, covariate.stargazer)
covariate.order

stargazer(mod_1, mod_2, mod_3, mod_4, mod_5, align = TRUE, omit.stat=c("LL","ser","f"), no.space = TRUE,
          report = "vc", intercept.top = TRUE, intercept.bottom = FALSE, digits = 2, 
          order = covariate.order, covariate.labels = covariate.labels,
          model.numbers = FALSE, object.names = TRUE)




