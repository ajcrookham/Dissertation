
# This file contains the code to perform seat prediction for given years
# Note the model code file must be run first to get the predicted vote shares

# Create data frame to store predicted vote shares
predicted_votes <- data.frame(matrix(nrow = 440, ncol = 7))
names(predicted_votes) <- c('Constituency', 'Year', 'Con', 'Lab', 'LD', 'RFM', 'GRN')
predicted_votes$Constituency <- election_data_testing$Constituency
predicted_votes$Year <- election_data_testing$Year

predicted_votes$Con <- con
predicted_votes$Lab <- lab
predicted_votes$LD <- ld
predicted_votes$RFM <- rfm
predicted_votes$GRN <- grn

# Function computes the probability that each party wins a given seat in a given year
prob <- function(prediction, seat, year, lambda, sigma){
  
  ind_seat <- prediction[prediction$Constituency == seat & prediction$Year == year, ]
  vote_share <- c(ind_seat$Con, ind_seat$Lab, ind_seat$LD, ind_seat$RFM, ind_seat$GRN)
  max_vote_share <- max(vote_share)
  
  r_values <- rep(0,5)
  
  for (i in 1:5) {
    
    r_values[i] <- exp(-((max_vote_share - vote_share[i]) / sigma) ** lambda)
    
  }
  
  probs <- r_values / sum(r_values)
  
  return(probs)  
  
}

# Function computes the number of seats each party wins for a given year
seat_predictor <- function(predicted_values, years, lambda, sigma) {
  
  subset <- predicted_votes[predicted_votes$Year %in% years, ]
  total <- length(subset$Constituency)
  
  test_prob <- data.frame(matrix(nrow = total, ncol = 5))
  
  for (i in 1:total) {
    
    test_prob[i, ] <- prob(subset, subset$Constituency[i], subset$Year[i], lambda, sigma)
    
  }  
  
  seat_prob <- colSums(na.omit(test_prob), 0)
  seat_pred <- round(seat_prob, 0)
  
  # Solve discrete constrained optimisation problem
  con_range <- seq(seat_pred[1] - 5, seat_pred[1] + 5, 1)
  lab_range <- seq(seat_pred[2] - 5, seat_pred[2] + 5, 1)
  ld_range <- seq(max(seat_pred[3] - 5, 0), seat_pred[3] + 5, 1)
  rfm_range <- seq(max(seat_pred[4] - 5, 0), seat_pred[4] + 5, 1)
  grn_range <- seq(max(seat_pred[5] - 5, 0), seat_pred[5] + 5, 1)
  
  combinations <- matrix(nrow = 0, ncol = 5)
  
  for (i1 in con_range) {
    
    for (i2 in lab_range) {
      
      for (i3 in ld_range) {
        
        for (i4 in rfm_range) {
          
          for (i5 in grn_range) {
            
            if (i1 + i2 + i3 + i4 + i5 == total) {
              
              combinations <- rbind(combinations, c(i1, i2, i3, i4, i5))
              
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  rows <- dim(combinations)[1]
  cols <- dim(combinations)[2]
  
  seat_prob <- matrix(data = seat_prob, nrow = rows, ncol = cols, byrow = TRUE)
  
  solution <- combinations[which.min(rowSums((combinations - seat_prob) ** 2)),]
  
  return(solution)
  
}

# Perform seat prediction for each year
seats_2015 <- seat_predictor(predicted_votes, 2015, 1.5, 4)
seats_2017 <- seat_predictor(predicted_votes, 2017, 1.5, 4)
seats_2019 <- seat_predictor(predicted_votes, 2019, 1.5, 4)
seats_2024 <- seat_predictor(predicted_votes, 2024, 1.5, 4)
seats_total <- seats_2015 + seats_2017 + seats_2019 + seats_2024

# Create data frame containing seat predictions
seat_predictions <- data.frame(matrix(data = c(seats_2015, seats_2017, seats_2019, seats_2024, seats_total), ncol = 5, byrow = TRUE))
names(seat_predictions) <- c('Con', 'Lab', 'LD', 'RFM', 'GRN')
row.names(seat_predictions) <- c(2015, 2017, 2019, 2024, 'Total')

seat_predictions
