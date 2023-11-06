library(ggplot2)
library(ggpubr)
library(ggforce)
library(patchwork)
library(smotefamily)

### imbalanced data set

imbdata <- sim_imbalanced_data(500, 3, 9, 2)
jpeg("imb_data.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(imbdata, mapping = aes(x = V2, y = V3, color = class, shape = class)) + 
  geom_point(size = 3) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Imbalanced Data") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
dev.off()




### undersampling vs oversampling

#imbalanced dataset
imbdata <- sim_imbalanced_data(500, 3, 9, 2)
plot_imbdata <- ggplot(imbdata, mapping = aes(x = V2, y = V3, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalanced Data") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
#undersampling
underdata <- sim_imbalanced_data(100, 3, 1, 2)
plot_underdata <- ggplot(underdata, mapping = aes(x = V2, y = V3, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Undersampling") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
#oversampling
overdata <- sim_imbalanced_data(900, 3, 1, 2)
plot_overdata <- ggplot(overdata, mapping = aes(x = V2, y = V3, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Oversampling") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

#plot
top_row <- ggarrange(NULL, plot_imbdata, NULL, ncol = 3, widths = c(1,2,1))
bottom_row <- ggarrange(plot_underdata, plot_overdata, ncol = 2)

jpeg("underoversampling.jpg", units = "in", width = 14, height = 8, res = 800)
ggarrange(top_row, bottom_row, ncol = 1)
dev.off()





### kNN

class1 <- data.frame(c(6,8,4,8,4), c(6,3,8,7,6), c(1,1,1,1,1))
colnames(class1) <- c("x", "y", "class")
class2 <- data.frame(c(2,6,1,4,4), c(5,1,4,5,3), c(2,2,2,2,2))
colnames(class2) <- c("x", "y", "class")
class3 <- data.frame(c(5), c(5), c(3))
colnames(class3) <- c("x", "y", "class")

knn <- rbind(class1, class2, class3)
knn$class <- as.factor(knn$class)

jpeg("kNN.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(knn, mapping = aes(x = x, y = y, color = class, shape = class)) + 
  geom_point(size = 5) + scale_color_manual(values = c("orangered3", "steelblue4", "green")) + 
  ggtitle("k-Nearest-Neighbors") + xlab("") + ylab("") + coord_fixed(ratio = 1) +
  geom_circle(aes(x0 = 5, y0 = 5, r = 1.4142135), inherit.aes = FALSE) + 
  geom_circle(aes(x0 = 5, y0 = 5, r = 1), inherit.aes = FALSE) + 
  theme_bw() + theme(legend.position = "none")
dev.off()




### SMOTE

data_smote_before <- sim_imbalanced_data(500, 3, 9, 2)
plot_data_before <- ggplot(data_smote_before, mapping = aes(x = V2, y = V3, color = class, shape = class)) + 
  geom_point() + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Data before SMOTE") + xlab("") + ylab("") + theme_bw() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

list_smote <- SMOTE(data_smote_before[,2:4], data_smote_before[,1], 3)
data_smote_after <- list_smote$data

plot_data_smote <- ggplot(data_smote_after, mapping = aes(x = V2, y = V3, color = class, shape = class)) + 
  geom_point() + scale_color_manual(values = c("steelblue4","orangered3")) + 
  ggtitle("Data after SMOTE with k = 3") + xlab("") + ylab("") + theme_bw() + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank())


jpeg("smote.jpg", units = "in", width = 14, height = 8, res = 800)
ggarrange(plot_data_before, plot_data_smote, ncol = 3)
dev.off()




### Synthetic Data

syn_data_viz <- simulate(1000,9)
jpeg("syn_data_viz.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(syn_data_viz, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalance Ratio: 1:9") + xlab("X1") + ylab("X2") + 
  theme_bw()
dev.off()
