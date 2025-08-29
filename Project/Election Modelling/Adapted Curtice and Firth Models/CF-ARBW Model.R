
# This file builds the CF-ARBW models and computes the R squared values for the test set

# Load required packages
library(readxl)
library(ggplot2)
library(gridExtra)
library(yardstick)
library(lme4)
library(stargazer)
library(rsample)

# Set working directory to Source File location

results_2010 <- read_excel('~/Project/Election Data/Election Results/Election Results New Boundaries.xlsx', sheet = '2010')

# Load adapted CF model data
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

# Create test set
set.seed(36581771)

constituency_split <- results_2010 |>
  initial_split(prop = 0.8, strata = Region)

training_constituency <- training(constituency_split)
testing_constituency <- testing(constituency_split)

election_data_testing <- election_data_training[election_data_training$Constituency %in% testing_constituency$Constituency, ]
election_data_training <- election_data_training[election_data_training$Constituency %in% training_constituency$Constituency, ]



# Green model
bw_null_1 <- lm(y_1 ~ 1 + Blue_Wall + `Level 4 Prop Change` + inflation + avg_unemployment + `No Qualifications Prop`
                + X30.49 + `Median Income Change` + Proportion + `No RFM Before` + `Social Prop`
                + `Owner Prop Change` + `House Prices Change` + `Private Prop Change`
                + `No Qualifications Prop Change` + White + home_prices + X18.29 + child_poverty + `Median Age`
                + `Owner Prop` + `X65. Change` + `X18.29 Change` + X50.64
                + `No Qualifications Prop`:`No Qualifications Prop Change` + X18.29:`X18.29 Change`
                , data = election_data_training[!election_data_training$`GRN Share After` == 0,])

bw_full_1 <- lm(y_1 ~ (1 + `Level 4 Prop Change` + inflation + avg_unemployment + `No Qualifications Prop`
                       + X30.49 + `Median Income Change` + Proportion + `No RFM Before` + `Social Prop`
                       + `Owner Prop Change` + `House Prices Change` + `Private Prop Change`
                       + `No Qualifications Prop Change` + White + home_prices + X18.29 + child_poverty + `Median Age`
                       + `Owner Prop` + `X65. Change` + `X18.29 Change` + X50.64
                       + `No Qualifications Prop`:`No Qualifications Prop Change` + X18.29:`X18.29 Change`) * Blue_Wall,
                data = election_data_training[!election_data_training$`GRN Share After` == 0,])

model_build <- step(bw_null_1, scope = list(lower = bw_null_1, upper = bw_full_1), direction = 'both', trace = 0)
bw_mod_1 <- lm(model_build$terms, data = election_data_training[!election_data_training$`GRN Share After` == 0, ])

summary(bw_mod_1)

bw_mixed_mod_1 <- lmer(y_1 ~ 1 + Blue_Wall + `Level 4 Prop Change` + inflation + avg_unemployment 
                          + `No Qualifications Prop` + X30.49 + `Median Income Change` + Proportion + `No RFM Before`
                          + `Social Prop` + `Owner Prop Change` + `House Prices Change` + `Private Prop Change`
                          + `No Qualifications Prop Change` + White + home_prices + X18.29 + child_poverty
                          + `Median Age` + `Owner Prop` + `X65. Change` + `X18.29 Change` + X50.64
                          + `No Qualifications Prop`:`No Qualifications Prop Change` + X18.29:`X18.29 Change`
                          + Blue_Wall:inflation + Blue_Wall:White
                          + (1 | Year), 
                       data = election_data_training[!election_data_training$`GRN Share After` == 0,])

summary(bw_mixed_mod_1)


# Reform model
bw_null_2 <- lm(y_2 ~ 1 + Blue_Wall + `No Qualifications Prop Change` + inflation + `Owner Prop Change` + `No RFM Before`
                + `Level 4 Prop Change` + Proportion + `X65. Change` + `X50.64 Change` + X30.49
                + `No Qualifications Prop` + `Level 4 Prop` + avg_unemployment + `Median Income Change`
                + home_prices + `X18.29 Change` + `Median Age` + `Private Prop Change`
                + `No Qualifications Prop Change`:`No Qualifications Prop` + `Level 4 Prop Change`:`Level 4 Prop`
                , data = election_data_training[!election_data_training$`RFM Share After` == 0, ])

bw_full_2 <- lm(y_2 ~ (1 + `No Qualifications Prop Change` + inflation + `Owner Prop Change` + `No RFM Before`
                       + `Level 4 Prop Change` + Proportion + `X65. Change` + `X50.64 Change` + X30.49
                       + `No Qualifications Prop` + `Level 4 Prop` + avg_unemployment + `Median Income Change`
                       + home_prices + `X18.29 Change` + `Median Age` + `Private Prop Change`
                       + `No Qualifications Prop Change`:`No Qualifications Prop` + `Level 4 Prop Change`:`Level 4 Prop`) * Blue_Wall
                , data = election_data_training[!election_data_training$`RFM Share After` == 0, ])

model_build <- step(bw_null_2, scope = list(lower = bw_null_2, upper = bw_full_2), direction = 'both', trace = 0)
bw_mod_2 <- lm(model_build$terms, data = election_data_training[!election_data_training$`RFM Share After` == 0,])

summary(bw_mod_2)


bw_mixed_mod_2 <- lmer(y_2 ~ 1 + Blue_Wall + `No Qualifications Prop Change` + inflation + `Owner Prop Change`
                          + `No RFM Before` + `Level 4 Prop Change` + Proportion + `X65. Change` + `X50.64 Change`
                          + X30.49 + `No Qualifications Prop` + `Level 4 Prop` + avg_unemployment + `Median Income Change`
                          + home_prices + `X18.29 Change` + `Median Age` + `Private Prop Change` 
                          + `No Qualifications Prop Change`:`No Qualifications Prop` 
                          + `Level 4 Prop Change`:`Level 4 Prop` + Blue_Wall:`Level 4 Prop Change`
                          + Blue_Wall:`No RFM Before` + Blue_Wall:`Median Income Change` + Blue_Wall:home_prices
                          + Blue_Wall:Proportion + (1| Year),
                       data = election_data_training[!election_data_training$`RFM Share After` == 0, ])

summary(bw_mixed_mod_2)


# Lib Dem model
bw_null_3 <- lm(y_3 ~ 1 + Blue_Wall + `Median Income Change` + `Level 4 Prop Change` + `No Qualifications Prop Change`
                + `Median Income` + `Owner Prop Change` + inflation + X50.64 + child_poverty + `X65. Change`
                + Proportion + `X18.29 Change` + White + X65. + `Social Prop` + home_prices
                + `No Qualifications Prop` + `Level 4 Prop` + `House Prices Change` + `No RFM Before` 
                + `Private Prop Change` + `No Qualifications Prop Change`:`No Qualifications Prop` 
                + `Level 4 Prop Change`:`Level 4 Prop` + `Median Income Change`:`Median Income`
                + home_prices:`House Prices Change`
                , data = election_data_training[!election_data_training$`LD Share After` == 0, ])

bw_full_3 <- lm(y_3 ~ (1 + `Median Income Change` + `Level 4 Prop Change` + `No Qualifications Prop Change`
                       + `Median Income` + `Owner Prop Change` + inflation + X50.64 + child_poverty + `X65. Change`
                       + Proportion + `X18.29 Change` + White + X65. + `Social Prop` + home_prices
                       + `No Qualifications Prop` + `Level 4 Prop` + `House Prices Change` + `No RFM Before` 
                       + `Private Prop Change` + `No Qualifications Prop Change`:`No Qualifications Prop` 
                       + `Level 4 Prop Change`:`Level 4 Prop` + `Median Income Change`:`Median Income`
                       + home_prices:`House Prices Change`) * Blue_Wall
                , data = election_data_training[!election_data_training$`LD Share After` == 0, ])

model_build <- step(bw_null_3, scope = list(lower = bw_null_3, upper = bw_full_3), direction = 'both', trace = 0)
bw_mod_3 <- lm(model_build$terms, data = election_data_training[!election_data_training$`LD Share After` == 0,])

summary(bw_mod_3)


bw_mixed_mod_3 <- lmer(y_3 ~ 1 + Blue_Wall + `Median Income Change` + `Level 4 Prop Change` 
                          + `No Qualifications Prop Change` + `Median Income` + `Owner Prop Change` + inflation
                          + X50.64 + child_poverty + `X65. Change` + Proportion + `X18.29 Change` + White + X65.
                          + `Social Prop` + home_prices + `No Qualifications Prop` + `Level 4 Prop`
                          + `House Prices Change` + `No RFM Before` + `Private Prop Change`
                          + `No Qualifications Prop Change`:`No Qualifications Prop`
                          + `Level 4 Prop Change`:`Level 4 Prop` + `Median Income Change`:`Median Income`
                          + home_prices:`House Prices Change` + Blue_Wall:`X65. Change` + Blue_Wall:Proportion
                          + Blue_Wall:`House Prices Change` + Blue_Wall:`No Qualifications Prop Change` 
                          + Blue_Wall:`Level 4 Prop Change` + Blue_Wall:`Median Income` + (1 | Year),
                       data = election_data_training[!election_data_training$`LD Share After` == 0,])

summary(bw_mixed_mod_3)


# Conservative model
bw_null_4 <- lm(y_4 ~ 1 + Blue_Wall + inflation + `No RFM Before` + `No Qualifications Prop Change` + `No Qualifications Prop`
                + `Level 4 Prop` + White + `Level 4 Prop Change` + `House Prices Change` + `White Change`
                + `X65. Change` + X18.29 + Proportion + X50.64 + `Owner Prop` + avg_unemployment
                + `Owner Prop Change` + `X50.64 Change` + `Median Income`
                + `No Qualifications Prop Change`:`No Qualifications Prop` 
                + `Level 4 Prop`:`Level 4 Prop Change` + White:`White Change`
                + `Owner Prop`:`Owner Prop Change` + X50.64:`X50.64 Change`
                , data = election_data_training[!election_data_training$`Con Share After` == 0, ])

bw_full_4 <- lm(y_4 ~ (1 + inflation + `No RFM Before` + `No Qualifications Prop Change` + `No Qualifications Prop`
                       + `Level 4 Prop` + White + `Level 4 Prop Change` + `House Prices Change` + `White Change`
                       + `X65. Change` + X18.29 + Proportion + X50.64 + `Owner Prop` + avg_unemployment
                       + `Owner Prop Change` + `X50.64 Change` + `Median Income`
                       + `No Qualifications Prop Change`:`No Qualifications Prop` 
                       + `Level 4 Prop`:`Level 4 Prop Change` + White:`White Change`
                       + `Owner Prop`:`Owner Prop Change` + X50.64:`X50.64 Change`) * Blue_Wall,
                data = election_data_training[!election_data_training$`Con Share After` == 0, ])

model_build <- step(bw_null_4, scope = list(lower = bw_null_4, upper = bw_full_4), direction = 'both', trace = 0)
bw_mod_4 <- lm(model_build$terms, data = election_data_training[!election_data_training$`Con Share After` == 0,])

summary(bw_mod_4)


bw_mixed_mod_4 <- lmer(y_4 ~ 1 + Blue_Wall + inflation + `No RFM Before` + `No Qualifications Prop Change`
                          + `No Qualifications Prop` + `Level 4 Prop` + White + `Level 4 Prop Change`
                          + `House Prices Change` + `White Change` + `X65. Change` + X18.29 + Proportion + X50.64
                          + `Owner Prop` + avg_unemployment + `Owner Prop Change` + `X50.64 Change` + `Median Income`
                          + `No Qualifications Prop Change`:`No Qualifications Prop` + `Level 4 Prop`:`Level 4 Prop Change`
                          + White:`White Change` + `Owner Prop`:`Owner Prop Change` + X50.64:`X50.64 Change`
                          + Blue_Wall:`House Prices Change` + Blue_Wall:Proportion + Blue_Wall:`Median Income` 
                          + Blue_Wall:inflation + Blue_Wall:`X65. Change` + Blue_Wall:`White Change` 
                          + Blue_Wall:`Owner Prop Change`
                          + (1 | Year),
                       data = election_data_training[!election_data_training$`Con Share After` == 0,])

summary(bw_mixed_mod_4)


# Labour model
bw_null_5 <- lm(y_5 ~ 1 + Blue_Wall + Proportion + X50.64 + `No RFM Before` + `Median Income` + `House Prices Change`
                + avg_unemployment + `X50.64 Change` + `No Qualifications Prop Change` + `X18.29 Change`
                + `Social Prop Change` + `Private Prop` + `No Qualifications Prop` + `Level 4 Prop` + X18.29
                + `Level 4 Prop Change` + inflation + `Median Income Change` + `Owner Prop Change`
                + `Owner Prop` + `No Qualifications Prop Change`:`No Qualifications Prop`
                + `Level 4 Prop`:`Level 4 Prop Change`
                , data = election_data_training[!election_data_training$`Lab Share After` == 0, ])

bw_full_5 <- lm(y_5 ~ (1 + Proportion + X50.64 + `No RFM Before` + `Median Income` + `House Prices Change`
                       + avg_unemployment + `X50.64 Change` + `No Qualifications Prop Change` + `X18.29 Change`
                       + `Social Prop Change` + `Private Prop` + `No Qualifications Prop` + `Level 4 Prop` + X18.29
                       + `Level 4 Prop Change` + inflation + `Median Income Change` + `Owner Prop Change`
                       + `Owner Prop` + `No Qualifications Prop Change`:`No Qualifications Prop`
                       + `Level 4 Prop`:`Level 4 Prop Change`) * Blue_Wall
                , data = election_data_training[!election_data_training$`Lab Share After` == 0, ])

model_build <- step(bw_null_5, scope = list(lower = bw_null_5, upper = bw_full_5), direction = 'both', trace = 0)
bw_mod_5 <- lm(model_build$terms, data = election_data_training[!election_data_training$`Lab Share After` == 0,])

summary(bw_mod_5)


bw_mixed_mod_5 <- lmer(y_5 ~ 1 + Blue_Wall + Proportion + X50.64 + `No RFM Before` + `Median Income`
                          + `House Prices Change` + avg_unemployment + `X50.64 Change` + `No Qualifications Prop Change`
                          + `X18.29 Change` + `Social Prop Change` + `Private Prop` + `No Qualifications Prop` 
                          + `Level 4 Prop` + X18.29 + `Level 4 Prop Change` + inflation + `Median Income Change`
                          + `Owner Prop Change` + `Owner Prop` + `No Qualifications Prop Change`:`No Qualifications Prop`
                          + `Level 4 Prop`:`Level 4 Prop Change` + Blue_Wall:Proportion + Blue_Wall:`House Prices Change`
                          + Blue_Wall:inflation + Blue_Wall:`Median Income` + Blue_Wall:avg_unemployment 
                          + Blue_Wall:`No Qualifications Prop` + Blue_Wall:`Social Prop Change` + Blue_Wall:`Owner Prop`
                          + (1 | Year),
                       data = election_data_training[!election_data_training$`Lab Share After` == 0, ])

summary(bw_mixed_mod_5)


# Predict upon test set and compute predicted vote shares
grn_change <- predict(bw_mixed_mod_1, newdata = election_data_testing)
grn <- election_data_testing$`GRN Share` + grn_change

rfm_change <- predict(bw_mixed_mod_2, newdata = election_data_testing)
rfm <- election_data_testing$`RFM Share` + rfm_change

ld_change <- predict(bw_mixed_mod_3, newdata = election_data_testing)
ld <- election_data_testing$`LD Share` + ld_change

con_change <- predict(bw_mixed_mod_4, newdata = election_data_testing)
con <- election_data_testing$`Con Share` + con_change

lab_change <- predict(bw_mixed_mod_5, newdata = election_data_testing)
lab <- election_data_testing$`Lab Share` + lab_change

# Set vote share to zero if that party does not stand
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

# Adjust total
total <- grn + rfm + ld + con + lab

grn <- grn / total * 100
rfm <- rfm / total * 100
ld <- ld / total * 100
con <- con / total * 100
lab <- lab / total * 100

# Create data frame for test set predicted and actual vote shares
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

# Combine plots into a single figure
cf_arbw_model_plot <- grid.arrange(con_plot, lab_plot, ld_plot, rfm_plot, grn_plot, legend, ncol = 2, nrow = 3,
                                         layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                                         widths = c(2, 2), heights = c(4, 4, 4))

ggsave('CF-ARBW Model Vote Share.png', cf_arbw_model_plot, width = 15.66, height = 12, units = 'cm')

# Vote share R squared values
con_rsq <- rsq(test, `Con Actual`, `Con Model`)$.estimate
lab_rsq <- rsq(test, `Lab Actual`, `Lab Model`)$.estimate
ld_rsq <- rsq(test, `LD Actual`, `LD Model`)$.estimate
rfm_rsq <- rsq(test, `RFM Actual`, `RFM Model`)$.estimate
grn_rsq <- rsq(test, `GRN Actual`, `GRN Model`)$.estimate

cf_arbw_rsq <- c(con_rsq, lab_rsq, ld_rsq, rfm_rsq, grn_rsq)
cf_arbw_rsq

# Create data frame for predicted change in vote share
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
cf_arbw_model_change_plot <- grid.arrange(con_plot, lab_plot, ld_plot, rfm_plot, grn_plot, legend, ncol = 2, nrow = 3,
                                                layout_matrix = rbind(c(1,2), c(3,4), c(5,6)),
                                                widths = c(2, 2), heights = c(4, 4, 4))

ggsave('CF-ARBW Model Change.png', cf_arbw_model_change_plot, width = 15.66, height = 12, units = 'cm')

# Compute R squared values for change in vote share
con_rsq_change <- rsq(test2, `Con Actual`, `Con Model`)$.estimate
lab_rsq_change <- rsq(test2, `Lab Actual`, `Lab Model`)$.estimate
ld_rsq_change <- rsq(test2, `LD Actual`, `LD Model`)$.estimate
rfm_rsq_change <- rsq(test2, `RFM Actual`, `RFM Model`)$.estimate
grn_rsq_change <- rsq(test2, `GRN Actual`, `GRN Model`)$.estimate

cf_arbw_rsq_change <- c(con_rsq_change, lab_rsq_change, ld_rsq_change, rfm_rsq_change, grn_rsq_change)

cf_arbw_rsq_change

# Some specific examples for Section 5
test[test$Constituency == 'Aldershot' & test$Year == 2024,]
test[test$Constituency == 'Horsham' & test$Year == 2024, ]
test[test$Constituency == 'Waveney Valley' & test$Year == 2024, ]


# Writes LaTeX code for table in appendices

covariate.labels <- c('Intercept', 'Blue Wall', '18 to 29', '30 to 49', '50 to 64', '65+', 'Change in 18 to 29',
                          'Change in 50 to 64', 'Change in 65+', 'Median Age', 'Degree',
                          'No Qualifications', 'Change in Degree', 'Change in No Qual.', 'Median Income', 'Median House Price',
                          'Change in Median Income', 'Median House Price Change', 'Inflation', 'Unemployment', 
                          'Child Poverty', 'Fuel Poverty', 'Ethnicity', 'Change in Ethnicity',
                          'Home Owners', 'Private Housing', 'Social Housing',
                          'Home Owners Change', 'Private Housing Change', 'Social Housing Change', 'No RFM Before',
                          '18 to 29 Int.', '50 to 64 Int.', 'Degree Int.', 'Degree Int. 2',
                          'No Qual. Int.', 'No Qual. Int. 2', 'Median Income Int.', 'Median House Price Int.', 'Ethnicity Int.',
                          'Home Owners Int.', 'Blue Wall Change in 65+',
                          'Blue Wall No Qual.', 'Blue Wall Change in Degree', 'Blue Wall Change in No Qual.',
                          'Blue Wall Median Income', 'Blue Wall Median House Price', 
                          'Blue Wall Median Income Change', 'Blue Wall Median House Price Change', 'Blue Wall Inflation',
                          'Blue Wall Unemployment', 'Blue Wall Fuel Poverty', 'Blue Wall Ethnicity',
                          'Blue Wall Change in Ethnicity',  'Blue Wall Home Owners',
                          'Blue Wall Home Owners Change', 'Blue Wall Social Housing Change', 'Blue Wall No RFM Before')



covariate.stargazer <- c('Intercept', 'Blue Wall', 'Change in Degree', 'Inflation', 'Unemployment', 'No Qualifications',
                         'Degree', '30 to 49', 'Change in Median Income', 'Fuel Poverty', 'No RFM Before', 
                         'Social Housing', 'Home Owners Change', 'Median House Price Change', 'Private Housing Change', 
                         'Change in Ethnicity',
                         'No Qual. Int.', 'Degree Int.', 'Median Income Int.', 'Median House Price Int.',
                         'Degree Int. 2', 'Ethnicity Int.', 'Home Owners Int.', '50 to 64 Int.', 'Blue Wall Change in 65+',
                         'Blue Wall Change in Degree', 'Blue Wall No RFM Before', 'Blue Wall Median Income Change',
                         'Blue Wall Median House Price', 'Blue Wall Change in Ethnicity', 'Blue Wall Home Owners Change',
                         'Blue Wall Median Income',
                         'Blue Wall Unemployment', 'Blue Wall No Qual.', 'Blue Wall Social Housing Change',
                         'Blue Wall Home Owners', 'Blue Wall Fuel Poverty', 'Blue Wall Median House Price Change', 
                         'Blue Wall Change in No Qual.', 'Change in No Qual.', 'Median Income', 'Ethnicity',
                         '65+', 'Median House Price', '18 to 29', 'Child Poverty', 'Median Age', 'Home Owners',
                         'Change in 65+', 'Change in 50 to 64', 'Change in 18 to 29', 'Social Housing Change', 
                         'Private Housing', '50 to 64', 'No Qual. Int. 2', '18 to 29 Int.', 'Blue Wall Inflation',
                         'Blue Wall Ethnicity')

covariate.order <- match(covariate.labels, covariate.stargazer)
covariate.order

bw_mm_1 <- bw_mixed_mod_1
bw_mm_2 <- bw_mixed_mod_2
bw_mm_3 <- bw_mixed_mod_3
bw_mm_4 <- bw_mixed_mod_4
bw_mm_5 <- bw_mixed_mod_5

stargazer(bw_mm_1, bw_mm_2, bw_mm_3, bw_mm_4, bw_mm_5, 
          align = TRUE, omit.stat=c("LL","ser","f"), no.space = TRUE,
          report = "vc", intercept.top = TRUE, intercept.bottom = FALSE, digits = 2,
          order = covariate.order, covariate.labels = covariate.labels,
          model.numbers = FALSE, object.names = TRUE)




