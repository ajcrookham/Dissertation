
# This file builds the multinomial logisitic regression models and computes the R squared values on the test data

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

# Load multinomial logistic regression data, remove excess columns, and change names
election_data_training <- read.csv('Multinomial Logistic Data.csv')
election_data_training <- election_data_training[, 2:68]

new_names <- c('y_1', 'y_2', 'y_3', 'y_4', 'Year', 'X18.29', 'X30.49', 'X50.64', 'X65.', 'Median Age', 'pop_density', 'Level 4 Prop',
               'No Qualifications Prop', 'avg_unemployment', 'White', 'Proportion', 'Owners', 'Private renters',
               'Social renters', 'Total - Housing', 'home_prices', 'Median Income', 'inflation', 'child_poverty',
               'Nat Con', 'Nat Lab', 'Nat LD', 'Nat RFM', 'Nat GRN', 'Owner Prop', 'Private Prop', 'Social Prop',
               'X18.29 Change', 'X30.49 Change', 'X50.64 Change', 'X65. Change', 'Level 4 Prop Change', 
               'No Qualifications Prop Change', 'White Change', 'Owner Prop Change', 'Private Prop Change', 
               'Social Prop Change', 'House Prices Change', 'Median Income Change', 'Nat Con Change', 'Nat Lab Change',
               'Nat LD Change', 'Nat RFM Change', 'Nat GRN Change', 'Constituency', 'Con Share', 'Lab Share', 
               'LD Share', 'RFM Share', 'GRN Share', 'Winner', 'Con Share After', 'Lab Share After', 'LD Share After',
               'RFM Share After', 'GRN Share After', 'No GRN Before', 'No RFM Before', 'No LD Before', 'No Con Before',
               'No Lab Before', 'Year_2024')

names(election_data_training) <- new_names


# Create test set
set.seed(36581771)

constituency_split <- results_2010 |>
  initial_split(prop = 0.8, strata = Region)

training_constituency <- training(constituency_split)
testing_constituency <- testing(constituency_split)

election_data_testing <- election_data_training[election_data_training$Constituency %in% testing_constituency$Constituency, ]
election_data_training <- election_data_training[election_data_training$Constituency %in% training_constituency$Constituency, ]


# Green party model
null_1 <- lm(y_1 ~ 1, data = election_data_training[!(election_data_training$y_1 == -Inf | election_data_training$y_1 == Inf | election_data_training$`GRN Share After` == 0),])
full_1 <- lm(y_1 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`, 
            election_data_training[!(election_data_training$y_1 == -Inf | election_data_training$y_1 == Inf | election_data_training$`GRN Share After` == 0),
            c(1, 5:16, 21:24, 30:44, 63)])

model_build <- step(null_1, scope = list(lower = null_1, upper = full_1), direction = 'both', trace = 0)
mod_1 <- lm(model_build$terms, election_data_training[!(election_data_training$y_1 == -Inf | election_data_training$y_1 == Inf | election_data_training$`GRN Share After` == 0),])

summary(mod_1)

# Reform model
null_2 <- lm(y_2 ~ 1, data = election_data_training[!(election_data_training$y_2 == -Inf | election_data_training$y_2 == Inf | election_data_training$`RFM Share After` == 0),])
full_2 <- lm(y_2 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change`, 
             data = election_data_training[!(election_data_training$y_2 == -Inf | election_data_training$y_2 == Inf | election_data_training$`RFM Share After` == 0),
                                    c(2, 5:16, 21:24, 30:44, 63)])

model_build <- step(null_2, scope = list(lower = null_2, upper = full_2), direction = 'both', trace = 0)
mod_2 <- lm(model_build$terms, data = election_data_training[!(election_data_training$y_2 == -Inf | election_data_training$y_2 == Inf | election_data_training$`RFM Share After` == 0),])

summary(mod_2)

# Lib Dem model
null_3 <- lm(y_3 ~ 1, data = election_data_training[!(election_data_training$y_3 == -Inf | election_data_training$y_3 == Inf | election_data_training$`LD Share After` == 0),])
full_3 <- lm(y_3 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change` , 
             data = election_data_training[!(election_data_training$y_3 == -Inf | election_data_training$y_3 == Inf | election_data_training$`LD Share After` == 0),
                                           c(3, 5:16, 21:24, 30:44, 63)])

model_build <- step(null_3, scope = list(lower = null_3, upper = full_3), direction = 'both', trace = 0)
mod_3 <- lm(model_build$terms, data = election_data_training[!(election_data_training$y_3 == -Inf | election_data_training$y_3 == Inf | election_data_training$`LD Share After` == 0),])

summary(mod_3)

# Labour model
null_4 <- lm(y_4 ~ 1, data = election_data_training[!(election_data_training$y_4 == -Inf | election_data_training$y_4 == Inf | election_data_training$`Lab Share After` == 0),])
full_4 <- lm(y_4 ~ (. - Year) + X18.29:`X18.29 Change` + X30.49:`X30.49 Change` + X50.64:`X50.64 Change` + X65.:`X65. Change`
             + `Level 4 Prop`:`Level 4 Prop Change` + `No Qualifications Prop`:`No Qualifications Prop Change`
             + White:`White Change` + `Owner Prop`:`Owner Prop Change` + `Private Prop`:`Private Prop Change`
             + `Social Prop`:`Social Prop Change` + home_prices:`House Prices Change` + `Median Income`:`Median Income Change` , 
             data = election_data_training[!(election_data_training$y_4 == -Inf | election_data_training$y_4 == Inf | election_data_training$`Lab Share After` == 0),
                                           c(4, 5:16, 21:24, 30:44, 63)])

model_build <- step(null_4, scope = list(lower = null_4, upper = full_4), direction = 'both', trace = 0)
mod_4 <- lm(model_build$terms, data = election_data_training[!(election_data_training$y_4 == -Inf | election_data_training$y_4 == Inf | election_data_training$`Lab Share After` == 0),])

summary(mod_4)


# Predict upon test set
y_1 <- predict(mod_1, newdata = election_data_testing)
y_2 <- predict(mod_2, newdata = election_data_testing)
y_3 <- predict(mod_3, newdata = election_data_testing)
y_4 <- predict(mod_4, newdata = election_data_testing)

# Compute predicted vote shares for test set
con <- 100 / (1 + exp(y_1) + exp(y_2) + exp(y_3) + exp(y_4))
grn <- exp(y_1) * con
rfm <- exp(y_2) * con
ld <- exp(y_3) * con
lab <- exp(y_4) * con

# Set to zero any seat in test set where the party did not run
for (i in 1:440) {
  
  if (election_data_testing$`GRN Share After`[i] == 0) {
    
    grn[i] <- 0
    
  }
  
  if (election_data_testing$`RFM Share After`[i] == 0) {
    
    rfm[i] <- 0
    
  }
  
  if (election_data_testing$`LD Share After`[i] == 0) {
    
    ld[i] <- 0
    
  }
  
  if (election_data_testing$`Lab Share After`[i] == 0) {
    
    lab[i] <- 0
    
  }
  
  if (election_data_testing$`Con Share After`[i] == 0) {
    
    con[i] <- 0
    
  }
  
}

# Readjust predicted totals after setting some to zero
total <- grn + rfm + ld + lab + con
grn <- grn / total * 100
rfm <- rfm / total * 100
ld <- ld / total * 100
lab <- lab / total * 100
con <- con / total * 100


# Create data frame of predicted vote shares and actual vote shares
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

# Conservative plot
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

# Put plots into a single figure
logistic_model_plot <- grid.arrange(con_plot, lab_plot, ld_plot, rfm_plot, grn_plot, legend, ncol = 2, nrow = 3,
                                  layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                                  widths = c(2, 2), heights = c(4, 4, 4))

# Save plot
ggsave('Logistic Model Vote Share.png', logistic_model_plot, width = 15.66, height = 12, units = 'cm')

# R squared values for each party
con_rsq <- rsq(test, `Con Actual`, `Con Model`)$.estimate
lab_rsq <- rsq(test, `Lab Actual`, `Lab Model`)$.estimate
ld_rsq <- rsq(test, `LD Actual`, `LD Model`)$.estimate
rfm_rsq <- rsq(test, `RFM Actual`, `RFM Model`)$.estimate
grn_rsq <- rsq(test, `GRN Actual`, `GRN Model`)$.estimate

logistic_rsq <- c(con_rsq, lab_rsq, ld_rsq, rfm_rsq, grn_rsq)
logistic_rsq


# Writes LaTeX code for table in appendices

covariate.labels <- c('Intercept', '18 to 29', '30 to 49', '50 to 64', '65+', 'Change in 18 to 29', 'Change in 30 to 49',
                      'Change in 50 to 64', 'Change in 65+', 'Median Age', 'Population Density', 'Degree',
                      'No Qualifications', 'Change in Degree', 'Change in No Qual.', 'Median Income', 'Median House Price',
                      'Change in Median Income', 'Median House Price Change', 'Inflation', 'Unemployment', 
                      'Child Poverty', 'Fuel Poverty', 'Ethnicity', 'Change in Ethnicity',
                      'Home Owners', 'Private Housing', 'Social Housing',
                      'Home Owners Change', 'Private Housing Change', 'No RFM Before',
                      '65+ Int.', '65+ Int. 2', 'No Qual. Int.', 'No Qual. Int. 2',
                      'Median Income Int.', 'Private Housing Int.')

covariate.stargazer <- c('Intercept', 'Home Owners', 'Inflation', '18 to 29', 'Change in Degree', 'Change in 18 to 29',
                         'Unemployment', 'Private Housing Change', '30 to 49', 'No Qualifications', 'Fuel Poverty', 'Home Owners Change',
                         'No RFM Before', 'Median House Price Change', 'Social Housing', 'Change in Median Income',
                         'Population Density', 'Private Housing', 'Median Income', 'Change in Ethnicity', 'Median House Price',
                         'Change in No Qual.', 'Degree', '50 to 64', 'Change in 65+', '65+', 'Median Age', 'Change in 50 to 64',
                         'Change in 30 to 49', 'Ethnicity', 'Child Poverty', 'No Qual. Int.', '65+ Int.', 
                         'Median Income Int.', '65+ Int. 2', 'No Qual. Int. 2', 'Private Housing Int.')

covariate.order <- match(covariate.labels, covariate.stargazer)
covariate.order

stargazer(mod_1, mod_2, mod_3, mod_4, align = TRUE, omit.stat=c("LL","ser","f"), no.space = TRUE,
          report = "vc", intercept.top = TRUE, intercept.bottom = FALSE, digits = 2,
          order = covariate.order, covariate.labels = covariate.labels,
          model.numbers = FALSE, object.names = TRUE)







