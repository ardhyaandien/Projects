

# My rnorm with n as a variable and default mean of 0 and default standard deviation of 1
my_rnorm <- function(n, mean = 0, sd = 1) {
  
# An error message will pop up if n is less than 0, this is applied to my_rchisq() and my_rt() functions as well
  if (n < 0) stop("invalid arguments")
  
# Uses a while loop to create random variables of a normal distribution whilst the sample length is less than n  
  sample <- NULL 
  while(length(sample) < n) {
    A <- runif(1, 0, 1) #creates 1 random value for X1
    B <- runif(1, 0, 1) #creates 1 random value for X2

# Create X1 and X2 using the box-muller method with values created from the runif() functions 
    X1 <- sin(2*pi*A)*sqrt(-2*log(B)) 
    X2 <- cos(2*pi*A)*sqrt(-2*log(B))
    
# If a mean and standard deviation is specified in my_rnorm() function
# X1 and X2 will be transformed with the specified standard deviation and mean, below
    X1 <- (X1*sd) + mean
    X2 <- (X2*sd) + mean
    
# The X1 and X2 created in this loop will be added to the sample created from the previous loops
    sample <- c(sample, X1, X2)
  }
  
# If the sample length is even, the sample is returned
# If the sample length is odd, the last sample is deleted, as coded below
  if(n %% 2 == 0) {
    return(sample)
  } else {
    return(sample[1:n])
  }
  
}



# A function is created with the number of values as n and degrees of freedom as df (with a default of 1)
my_rchisq <- function(n, df = 1) {
  
  if (n < 0) stop("invalid arguments")
  
# my_rnorm is used to generate independent standard normal variables
# The degrees of freedom of my_rchisq is the n for the my_rnorm in the loop
# A power of 2 are applied to the standard normal variables
# Next, all are summed to create chi-square (labelled chisq)
  sample <- NULL
  while(length(sample) < n) {
    chisq <- sum((my_rnorm(df))**2)
    
# After a loop tof the chi-square value is applied to the sample
    sample <- c(sample, chisq)
  }
  return(sample)
  
}



# A function is made for the t-distribution, with degrees of freedom defaulting to 1
my_rt <- function(n, df = 1) {
  
  if (n < 0) stop("invalid arguments")
  
# While length of the sample is less than n, the t distribution value is calculated
  sample <- NULL
  while(length(sample) < n) {
    
# my_rnorm and my_rchisq is set to produce one value every loop
    t <-  my_rnorm(1)/(sqrt(my_rchisq(1)/df))
    

    sample <- c(sample, t)
  }
  
  return(sample)
}


# Additional function to test my_rnorm(), my_chisq() and my_rt()
# This function creates my own summary table to replace the summary() function
# The table includes the minimum and max, median, and 1st and 3rd quantile.
# To present the table, I use the tibble function from tidyverse,
# I added the mean and standard deviation to the table to view the function works, specifically useful for my_rnorm()
library(tibble)


my_summ <- function(x) {
  
  min <- min(x)
  fst <- quantile(x, prob = c(0.25))
  median <- median(x)
  thr <- quantile(x, prob = c(0.75))
  max <- max(x)
  mean <- mean(x)
  stdev <- sd(x)
  
  
  tibble("Min" = min, "1st Qu" = fst, "Median" = median, "3rd Qu" = thr, "Max" = max, "Mean" = mean, "Stdev" = stdev)
  
}



# I confirm that the attached is my own work, except where clearly indicated in the text.

