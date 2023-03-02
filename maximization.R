#   This code is to write a maximization function for the E-M Algorithm 

#   Maximization Function
#   PROCESS: 
#     function that returns new estimates of mean, sigma, and lambda for each of
#     the three groups 
#
#   INPUTS:
#     fishlength: a numeric vector storing the fish lengths
#     estimates: 3x3 dataframe storing the mean, sd, and lambda for each group
#               (rows: age group, columns: parameter estimate)
#     posterior: NxK dataframe storing the posterior probabilities (updated in
#               the expectation step)
#            
#   OUTPUTS:
#     estimates: same as estimates described in "INPUTS", now storing the updated
#                values


maximization <- function(fishLength, estimates, posterior) {
    
  for (k in 1:3) {
    # estimate for mean updated for the kth group
    estimates$mu[k] <- sum((posterior[,k])*fishLength) / sum(posterior[,k])
      
    # estimate for sigma updated for the kth group
    estimates$sigma[k] <- sqrt(sum((posterior[,k]) *
                              (fishLength - estimates$mu[k])**2) /
                              (sum(posterior[,k])))
      
    # estimate for lambda updated for the kth group
    estimates$lambda[k] <- (1/length(fishLength)) * sum(posterior[,k])
  }  
    
  return(estimates)
    
}
    

# Test Maximization Function
# checking that the lambdas sum to 1, if not test fails 
test_maximization <- function(lambda) {
  test.passed <- FALSE
  test.passed <- sum(lambda) == 1
  
  return(test.passed)
}
