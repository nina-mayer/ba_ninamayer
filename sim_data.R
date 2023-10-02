library(ggplot2)

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

data1 <- sim_imbalanced_data(1000, 3, 9, 2)
ggplot(data1, mapping = aes(x = V2, y = V3, color = class)) + geom_point()
