# R Code:
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(factoextra)
library(tsfknn)
library(dtwclust)


#Upload data
NGN2 <- read_csv("~/Desktop/NGN2.csv")

HES5 <- read_csv("~/Desktop/HES5.csv")

#Remove deleted columns
NGN2 <- NGN2[, !(names(NGN2) %in% c('S', 'V', 'AU', 'AX'))]

HES5 <- HES5[, !(names(HES5) %in% c('S', 'V', 'AU', 'AX'))]


#------Impute Missing Value---------------------------------------------------------
# Create impute missing value function
impute <- function(df, k = 3) {
  for (i in 2:ncol(df)) {
    missing_count <- sum(is.na(df[[i]]))
    na_indices <- which(is.na(df[[i]]))
    
    non_missing_data <- df[, c(1, i)][complete.cases(df[, c(1, i)]), ]
    
    if (missing_count >= 1) {
      pred <- knn_forecasting(ts(non_missing_data), h = missing_count, k = k)$prediction
      
      # Check if any prediction is negative and keep NA in that case
      negative_pred_indices <- which(pred < 0)
      pred[negative_pred_indices] <- NA
      
      df[[i]][na_indices] <- as.numeric(pred)
    } else {
      # Handle the case when forecasting is not possible
      # You might choose to do something else, like filling with a default value
    }
  }
  
  return(df)
}

set.seed(101)


#Impute the KNN for NGN2
df1 <- impute(NGN2, k = 10)
#Removing missing values
df_cleaned1 <- df1[, colSums(is.na(df1)) == 0]

removed_columns <- setdiff(names(NGN2), names(df_cleaned1))
#Conduct the k-mean clustering
result <- tsclust(t(df_cleaned1[, -1]), type = "partitional", k = 2, distance = "dtw", 
                  clustering = "dba",
                  trace = FALSE)
#Plot 
plot(result, type = "sc", colour = "black", linetype = "solid", size = 3, alpha = 2.0,
     labels = list(nudge_x = 5, nudge_y = 3, size = 3, max.overlaps = 40)) +
  xlab("Time") + ylab("NGN2") + ggtitle("NGN2 Cluster")


#Conduct the same process with Hes5
df2 <- impute(HES5, k = 10)

df_cleaned2 <- df2[, colSums(is.na(df2)) == 0]
removed_columns2 <- setdiff(names(HES5), names(df_cleaned2))


result2 <- tsclust(t(df_cleaned2[, -1]), type = "partitional", k = 2, distance = "dtw", 
                   clustering = "dba",
                   trace = FALSE)
plot(result2, type = "sc", colour = "black", linetype = "solid", size = 3, alpha = 2.0,
     labels = list(nudge_x = 5, nudge_y = 3, size = 3, max.overlaps = 40)) +
  xlab("Time") + ylab("HES5") + ggtitle("HES5 Cluster")

#----------------------------------------------------------------------------------
#Extra diagrams for DTW methodology
t <- dtw( x = as.numeric(df_cleaned2$G), y = as.numeric(df_cleaned2$H), keep=TRUE)
# plot two way plot
dtwPlotTwoWay(t)
# plot a three way plot 
dtwPlotThreeWay(t, xlab = "G", ylab = "H")

#For K-mean discussion
#Find the common columns in both datasets
common_columns <- intersect(colnames(df_cleaned1), colnames(df_cleaned2))



#----Within-Cluster Sum of Squares-----------------------------------------------
# Define a function to plot the within-cluster sum of squares (WSS) for different k values
plot_wss <- function(data, k_range) {
  wss <- numeric(length(k_range))  # Create an empty vector to store WSS values
  
  # Loop through each k value in the specified range
  for (i in seq_along(k_range)) {
    k <- k_range[i]  # Get the current k value
    kmeans_result <- kmeans(data, centers = k, nstart = 10)  # Perform K-means clustering
    wss[i] <- kmeans_result$tot.withinss  # Store the total within-cluster sum of squares
  }
  
  # Plot the WSS values against the range of k values
  plot(k_range, wss, type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of Clusters (k)", ylab = "Within-Cluster Sum of Squares (WSS)",
       main = "Elbow Method for Optimal k: HES5")
  
  # Find the optimal k value (elbow point) by looking for the bend in the plot
  # You can use different techniques to automatically detect the elbow point,
  # such as the "knee" point or a statistical approach.
  
  # For example, you could find the index of the minimum rate of change of WSS
  diff_wss <- diff(wss)
  elbow_index <- which(diff(diff_wss) == max(diff(diff_wss))) + 1
  
  # Add a vertical line at the optimal k value
  abline(v = k_range[elbow_index], col = "red", lty = 2)
  
  # Print the optimal k value
  cat("Optimal number of clusters (k):", k_range[elbow_index], "\n")
}
## alter the title of the plot and code input for NGN2

#WSS for NGN2
plot_wss(df_cleaned1, 1:10)

#WSS for Hes5
plot_wss(df_cleaned2, 1:10)