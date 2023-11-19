#----Load library and data-------------------------------------------
set.seed(1)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(factoextra)
load_data <- function(){
  train <- read_csv("emnist-digits-train.csv")
  colnames(train)[1] <- "label"
  train[,2:785] <- train[,2:785]/255
  
  train <<- train %>% 
    group_by(label) %>% 
    slice_sample(n = 1000) %>% 
    ungroup()
  
  
  
}
load_data()

hist(train$label, xlab = "Labels", main = "Label Frequency Histogram")

#--------Useful function to find a number----------------------------
#Find a specific number function
#Helps find where a specific number starts 

number_finder <- function(x){
  n <- 1000 * x
  print(n)
}
number_finder(6)
## [1] 6000
print_image <- function(data, x) {
  matrix <- as.matrix(data[,2:785])
  
  list <- list()
  for (i in 1:nrow(matrix)) {
    list[[i]] <- matrix(matrix[i,], ncol=28, nrow=28)
  }
  
  par(mfrow=c(2,3))
  for (i in x) {
    image(t(apply(list[[i]], 2, rev)), col=gray((0:255)/255),
          main=paste("Label:", data[i,1]))
  }
  
}


#----------PCA----------------------------------------------------------
set.seed(1)
# apply prcomp
pca <- prcomp(train)
# convert pca$x to data frame
scores <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], label = as.factor(train$label))

# plot
ggplot(scores) + 
  geom_point(aes(x = PC2, y = PC1, colour = label)) + 
  labs(title = "PCA plot")

# Get covariance matrix
Sigma <- cov(train)

# Get eigenvalues and eigenvectors
eig <- eigen(Sigma)

pc_var <- eig$values
# Proportion of variance explained
pc_prop_var <- pc_var/sum(pc_var)
# Cumulative proportion of variance explained
pc_cumul_prop_var <- cumsum(pc_prop_var)

plot(pc_prop_var, xlab = "principal component index", 
     ylab = "proportion of variance explained", 
     main = "scree plot")

# Cumulative proportion of variance explained
pc_cumul_prop_var <- cumsum(pc_prop_var)

plot(pc_cumul_prop_var, xlab = "principal component index", 
     ylab = "cumulative proportion of variance explained")
abline(h = 0.9)

x <- pc_cumul_prop_var[pc_cumul_prop_var < 0.9]

plot(x, xlab = "principal component index", 
     ylab = "cumulative proportion of variance explained")
abline(h = 0.9)

plot(pc_prop_var[1:59], xlab = "principal component index", 
     ylab = "proportion of variance explained", 
     main = "scree plot")

final_pca <- as.data.frame(pca$x[,1:58])

#-----------------K-mean with PCA----------------------------------------------
set.seed(1)
km2 <- kmeans(final_pca, centers = 10, nstart = 50, iter.max = 500)

pca <- as.data.frame(pca$x[,1:58])
km2_clustered <- data.frame(x = pca[, 1], y = pca[, 2],
                            cluster = as.factor(km2$cluster), label = as.factor(train$label))
# whole plot
ggplot(km2_clustered) +
  geom_point(aes(x = x, y = y, color = label), size = 0.5, alpha = 0.8) +
  scale_color_manual(values = rainbow(10)) +
  labs(x = "PC1", y = "PC2", color = "Label", title = "Labeled digits in EMNIST dataset")

# plot presenting the individual clusters
ggplot(km2_clustered) +
  geom_point(aes(x = x, y = y, color = label), size = 0.5, alpha = 0.8) +
  scale_color_manual(values = rainbow(10)) +
  facet_wrap(~ cluster, nrow = 2, scale = "free") +
  labs(x = "PC1", y = "PC2", color = "Label", title = "Labeled digits in EMNIST dataset")

#------------K-mean only (without PCA)---------------------------------------------
emnist_matrix <- as.matrix(train[, 2:785])
emnist_labels <- train$label
set.seed(1)
km3 <- kmeans(train, centers = 10, nstart = 50, iter.max = 500)

# Whole plot
fviz_cluster(km3, data = emnist_matrix, stand = FALSE, PCA = F, geom = "point") +
  geom_text(aes(label = as.factor(emnist_labels)), size = 3) +
  labs(title = "K-Means Clustering of EMNIST Data Without PCA Full")

# Individual clusters
fviz_cluster(km3, data = emnist_matrix, stand = FALSE, PCA = F, geom = "point") +
  geom_text(aes(label = as.factor(emnist_labels)), size = 3) +
  facet_wrap(~ cluster, nrow = 2, scale = "free") +
  labs(title = "K-Means Clustering of EMNIST Data Without PCA Split")

#--------Anomolous Results---------------------------------------------------------
set.seed(1)
print_image(train, c(3796, 3139, 5872, 5328, 6978, 6368))

