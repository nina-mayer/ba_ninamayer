library(ggplot2)
library(ggpubr)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)

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
ggexport(plotlist = plots100, filename = "plots100.png", nrow = 2, ncol = 2)