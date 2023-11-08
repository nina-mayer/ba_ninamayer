library(MASS)


### sim_imbalanced_data
###
### This method creates synthetic binary classification data, that is imbalanced 
### to a degree that can be manually set. The covariates follow the normal distribution 
### with two means and have a standard deviation of 1. The resulting data set
### servers primarily visual purposes
### Input:
###   n             An integer. The size of the data set
###   n_variables   An integer. The number of dependent covariates
###   imb_ratio     An integer. The imbalance ratio (the majority class is imb_ratio
###                 times bigger than the minority class)
###   mean_diff     An integer. The difference of the means in the covariates.
### Output:
###   A dataframe with n rows and n_variables + 1 columns. The column "class" 
###   serves as the target variable and has two outcomes (1,2).

sim_imbalanced_data <- function(n, n_variables, imb_ratio, mean_diff) {
  n_minor <- n/(imb_ratio + 1)
  n_major <- n - n_minor
  data <- data.frame(X1 = rep(0,n), X2 = rep(0,n), X3 = rep(0,n))
  mean_minor <- sample(-5:5, 1)
  mean_major <- mean_minor + mean_diff
  for(i in 1:n_variables) {
    data[, i] <- c(rnorm(n_minor, mean_minor, 1), rnorm(n_major, mean_major, 1))
  }
  data$class <- factor(c(rep(1, n_minor), rep(0, n_major)))
  data
}




### simulate-function
###
### creates data with three features and given means and covariances
### Input:
###   n     An integer. The size of the data set that needs to be simulated
###   imb   An integer. The imbalance ratio in the data set
### Output:
###   A dataframe with n rows and four columns. Three numerical features X and
###     one binary target variable class with values 0 and 1 and class imbalance
###     1:imb

means1 <- c(27, 12, 56)
means2 <- c(29, 10, 54)
variances <- c(4, 1, 5, 1, 1.5, 2, 5, 2, 7)
cov_matrix <- matrix(data = variances, nrow = 3, ncol = 3, byrow = TRUE)
rownames(cov_matrix) <- c("X1", "X2", "X3")
colnames(cov_matrix) <- c("X1", "X2", "X3")

simulate <- function(n, imb){
  n1 <- (1/(imb+1)) * n
  n2 <- n - n1
  
  sim_data1 <- mvrnorm(n = n1, mu = means1, Sigma = cov_matrix)
  class1 <- rep(x = 1, times = n1)
  cl_1 <- data.frame(sim_data1, class1)
  colnames(cl_1) <- c("X1", "X2", "X3", "class")
  
  sim_data2 <- mvrnorm(n = n2, mu = means2, Sigma = cov_matrix)
  class2 <- rep(x = 0, times = n2)
  cl_2 <- data.frame(sim_data2, class2)
  colnames(cl_2) <- c("X1", "X2", "X3", "class")
  
  data <- rbind(cl_1, cl_2)
  data$class <- as.factor(data$class)
  data
}

### create synthetic data

set.seed(12226947)

data4 <- simulate(10000,4)
data9 <- simulate(10000,9)
data19 <- simulate(10000,19)
data99 <- simulate(10000,99)
data199 <- simulate(10000,199)
