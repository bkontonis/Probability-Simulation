#************ Probability Simulation Tool - Consecutive successes With K EXTRA LIVES


# R Options
options(scipen = 999) # This is to disable scientific notation
options(digits = 10)  # This is to define the number of digits you want to see in every R's decimal output


#*********************************************************************************************************************************************************************************#
#*********************** USER INPUT REQUIRED *************************************************************************************************************************************# 
#*********************************************************************************************************************************************************************************#
t = 100000             # Number of Sample - The greater the better
p = 0.6                # Probability of success in every trial
n = 300                # Number of total trials
successes_up_to = 100  # Number up to which we want to simulate consecutive successes
lives = 0              # Number of lives  available - Please note that a life does not count for success, it just helps the consecutive successes not break
#*********************************************************************************************************************************************************************************#
#*********************************************************************************************************************************************************************************#
#*********************************************************************************************************************************************************************************#



# Run the function below once
consecutiveSuccess_with_Lives_Parameter <- function(p, count, n, lives) {
  trials <- rbinom(n, 1, p)
  for (i in 1:(n-count+1-lives)) {
    if (sum(trials[i:(i+count-1+lives)]) >= count)
      return(1)
  }
  return(0)
}



# Run the rest of the code to get the result
# At this point feel free to play with the parameters and re-run the code below - There is no need to re-run the function

Probabilities_Table <- matrix(ncol = 2, nrow = successes_up_to)

for (i in 1:successes_up_to) {
  count = i
  sum   = 0
  for (j in 1:t) {
    sum = sum + consecutiveSuccess_with_Lives_Parameter(p, count, n, lives)
  }
  Probabilities_Table[i,1] <- i
  Probabilities_Table[i,2] <- (sum*1.0)/t
}

Probabilities_Table        <- data.frame(Probabilities_Table)
names(Probabilities_Table) <- c("Number of Consecutive Successes with Lives", "Probability")



write.csv(Probabilities_Table
          ,file = paste0("./Probabilities_Table_",t, "_", n, "_", lives,".csv")
          ,row.names = FALSE)


rm(list = ls())









