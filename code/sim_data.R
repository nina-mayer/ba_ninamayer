library(ggplot2)
library(ggpubr)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)
library(MASS)

#tests

x1 <- c(rnorm(500, 0,1), rnorm(500,3,1))
x2 <- rnorm(1000)
x3 <- rnorm(1000)
class1 <- factor(rep(1:2, each = 500))

sim_1 <- data.frame(x1, x2, x3, class1)

ggplot(sim_1, mapping = aes(x = x1, y = class1)) + geom_point()
ggplot(sim_1, mapping = aes(x = class1)) + geom_bar()

x4 <- c(rnorm(200, 0,1), rnorm(800,3,1))
class2 <- factor(c(rep(1, 200), rep(2, 800)))
sim_2 <- data.frame(x4, x2, x3, class2)
ggplot(sim_2, mapping = aes(x = x4, y = class2)) + geom_point()
ggplot(sim_2, mapping = aes(x = class2)) + geom_bar()

x5 <- c(rnorm(50, 0,1), rnorm(950,3,1))
x7 <- c(rnorm(50, -1,1), rnorm(950,1,1))
class3 <- factor(c(rep(1, 50), rep(2, 950)))
sim_3 <- data.frame(x5, x7, x3, class3)
ggplot(sim_3, mapping = aes(x = x5, y = class3)) + geom_point()
ggplot(sim_3, mapping = aes(x = class3)) + geom_bar()

x6 <- c(rnorm(10, 0,1), rnorm(990,3,1))
class4 <- factor(c(rep(1, 10), rep(2, 990)))
sim_4 <- data.frame(x6, x2, x3, class4)
ggplot(sim_4, mapping = aes(x = x6, y = class4)) + geom_point()
ggplot(sim_4, mapping = aes(x = class4)) + geom_bar()

ggplot(sim_3, mapping = aes(x = x5, y = x7, color = class3)) + geom_point()




### sim_imbalanced_data
###
### This method creates synthetic binary classification data, that is imbalanced 
### to a degree that can be manually set. The covariates follow the normal distribution 
### with two means and have a standard deviation of 1.
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
  data <- data.frame(class = factor(c(rep(1, n_minor), rep(2, n_major))))
  mean_minor <- sample(-5:5, 1)
  mean_major <- mean_minor + mean_diff
  for(i in 1:n_variables) {
    data[, 1 + i] <- c(rnorm(n_minor, mean_minor, 1), rnorm(n_major, mean_major, 1))
  }
  data
}


### create sythetic data
## size = 100
# imbalance ratio: 1:1
data11 <- sim_imbalanced_data(100, 3, 1, 2)
plot11 <- ggplot(data11, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:1") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:2
data12 <- sim_imbalanced_data(100, 3, 2, 2)
plot12 <- ggplot(data12, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:2") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:4
data13 <- sim_imbalanced_data(100, 3, 4, 2)
plot13 <- ggplot(data13, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:3") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:9
data14 <- sim_imbalanced_data(100, 3, 9, 2)
plot14 <- ggplot(data14, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:4") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()


## size = 1000
# imbalance ratio: 1:1
data21 <- sim_imbalanced_data(1000, 3, 1, 2)
plot21 <- ggplot(data21, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:1") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:4
data22 <- sim_imbalanced_data(1000, 3, 4, 2)
plot22 <- ggplot(data22, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:4") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:9
data23 <- sim_imbalanced_data(1000, 3, 9, 2)
plot23 <- ggplot(data23, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:9") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:19
data24 <- sim_imbalanced_data(1000, 3, 19, 2)
plot24 <- ggplot(data24, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:19") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()


## size = 10000
# imbalance ratio: 1:1
data31 <- sim_imbalanced_data(10000, 3, 1, 2)
plot31 <- ggplot(data31, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point(alpha = 0.8, stroke = NA) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:1") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:9
data32 <- sim_imbalanced_data(10000, 3, 9, 2)
plot32 <- ggplot(data32, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point(alpha = 0.8, stroke = NA) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:9") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:19
data33 <- sim_imbalanced_data(10000, 3, 19, 2)
plot33 <- ggplot(data33, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point(alpha = 0.8, stroke = NA) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:19") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

# imbalance ratio: 1:99
data34 <- sim_imbalanced_data(10000, 3, 99, 2)
plot34 <- ggplot(data34, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point(alpha = 0.8, stroke = NA) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:99") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()


## visualize synthetic data
# size = 100
plots100 <- ggarrange(plot11, plot12, plot13, plot14, ncol = 2, nrow = 2)
plots100 <- annotate_figure(plots100, top = text_grob("Data sets with size 100", face = "bold", size = 14))
# size = 1000
plots1000 <- ggarrange(plot21, plot22, plot23, plot24, ncol = 2, nrow = 2)
plots1000 <- annotate_figure(plots1000, top = text_grob("Data sets with size 1000", face = "bold", size = 14))
# size = 10000
plots10000 <- ggarrange(plot31, plot32, plot33, plot34, ncol = 2, nrow = 2)
plots10000 <- annotate_figure(plots10000, top = text_grob("Data sets with size 10000", face = "bold", size = 14))

# export
jpeg("plots100.jpg", units = "in", width = 14, height = 8, res = 600)
plots100
dev.off()

jpeg("plots1000.jpg", units = "in", width = 14, height = 8, res = 600)
plots1000
dev.off()

jpeg("plots10000.jpg", units = "in", width = 14, height = 8, res = 600)
plots10000
dev.off()


### MASS

#variances
variances <- c(4, 1, 5, 1, 1.5, 2, 5, 2, 7)
cov_matrix <- matrix(data = variances, nrow = 3, ncol = 3, byrow = TRUE)
rownames(cov_matrix) <- c("X1", "X2", "X3")
colnames(cov_matrix) <- c("X1", "X2", "X3")
cov_matrix

#class1
n1 <- 20
means1 <- c(27, 12, 56)
sim_data1 <- mvrnorm(n = n1, mu = means1, Sigma = cov_matrix)
class1 <- rep(x = 1, times = 20)
cl_1 <- data.frame(class1, sim_data1)
colnames(cl_1) <- c("class", "X1", "X2", "X3")

#class2
n2 <- 80
means2 <- c(31, 10, 53)
sim_data2 <- mvrnorm(n = n2, mu = means2, Sigma = cov_matrix)
class2 <- rep(x = 2, times = 80)
cl_2 <- data.frame(class2, sim_data2)
colnames(cl_2) <- c("class", "X1", "X2", "X3")


data <- rbind(cl_1, cl_2)
data$class <- as.factor(data$class)
ggplot(data, mapping = aes(x = X2, y = X3, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:4") + xlab("x2") + ylab("x3") + 
  theme_bw()

data$class <- as.numeric(data$class)
lin_mod <- lm(class ~ M1 + M2 + M3 + M1*M2 + M2*M3 + M1*M3, data = data)
summary(lin_mod)




### simulate-function
###
### creates data with three features and given means and covariances
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
  cl_1 <- data.frame(class1, sim_data1)
  colnames(cl_1) <- c("class", "X1", "X2", "X3")
  
  sim_data2 <- mvrnorm(n = n2, mu = means2, Sigma = cov_matrix)
  class2 <- rep(x = 0, times = n2)
  cl_2 <- data.frame(class2, sim_data2)
  colnames(cl_2) <- c("class", "X1", "X2", "X3")
  
  data <- rbind(cl_1, cl_2)
  data$class <- as.factor(data$class)
  data
}

data1 <- simulate(1000,1)
data2 <- simulate(1000,4)
data3 <- simulate(1000,9)
data4 <- simulate(1000,19)
data5 <- simulate(1000,99)
 