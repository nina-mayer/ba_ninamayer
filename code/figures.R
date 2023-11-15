library(ggplot2)
library(ggpubr)
library(ggforce)
library(patchwork)
library(bimba)
library(caret)
library(RColorBrewer)

### set text sizes

my_theme = theme(
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 16),
  plot.title = element_text(size = 22),
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 16))


### imbalanced data set

imbdata <- sim_imbalanced_data(500, 3, 9, 2)
jpeg("imb_data.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(imbdata, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 3) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Imbalanced Data") + xlab("") + ylab("") + theme_bw() + my_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
dev.off()




### undersampling vs oversampling

#imbalanced dataset
imbdata <- sim_imbalanced_data(500, 3, 9, 2)
plot_imbdata <- ggplot(imbdata, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Imbalanced Data") + xlab("") + ylab("") + theme_bw() + my_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
#undersampling
underdata <- RUS(imbdata)
plot_underdata <- ggplot(underdata, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Undersampling (RUS)") + xlab("") + ylab("") + theme_bw() + my_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
#oversampling
overdata <- SMOTE(imbdata)
plot_overdata <- ggplot(overdata, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Oversampling (SMOTE)") + xlab("") + ylab("") + theme_bw() + my_theme +
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
plot_data_before <- ggplot(data_smote_before, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Data before SMOTE") + xlab("") + ylab("") + theme_bw() + my_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

data_smote_after <- SMOTE(data_smote_before, k = 3)

plot_data_smote <- ggplot(data_smote_after, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4","orangered3")) + 
  ggtitle("Data after SMOTE with k = 3") + xlab("") + ylab("") + theme_bw() + my_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks.y = element_blank())


jpeg("smote.jpg", units = "in", width = 14, height = 8, res = 800)
ggarrange(plot_data_before, plot_data_smote, ncol = 2)
dev.off()




### Synthetic Data

syn_data_viz <- simulate(1000,9)
jpeg("syn_data_viz.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(syn_data_viz, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4", "orangered3")) + 
  ggtitle("Imbalance Ratio: 1:9") + xlab("X1") + ylab("X2") + 
  theme_bw() + my_theme
dev.off()



### Classification Results 

# no resampling
jpeg("raw_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(raw, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                          group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# smote
jpeg("smote_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(smote, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                          group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# sbc
jpeg("sbc_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(sbc, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                            group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# ros
jpeg("ros_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(ros, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                          group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# rus
jpeg("rus_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(rus, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                          group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# comp1
jpeg("comp1.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(comp1, mapping = aes(x = imbalance, y = bACC, group = method, color = method)) + 
  geom_point() + geom_line(size = 1) + scale_color_viridis_d() + 
  xlab("Imbalance Ratio")  + labs(color = "Resampling Method") + ylim(0.75,1) +
  theme_bw() + my_theme
dev.off()



# smotesbc
jpeg("smotesbc_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(smotesbc, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                          group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# smoterus
jpeg("smoterus_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(smoterus, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                               group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# smotebag
jpeg("smotebag_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(smotebag, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                               group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#663300", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# rusbag
jpeg("rusbag_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(rusbag, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                               group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#663300", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# comp2
jpeg("comp2.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(comp2, mapping = aes(x = imbalance, y = bACC, group = method, color = method)) + 
  geom_point() + geom_line(size = 1) + scale_color_viridis_d() + 
  xlab("Imbalance Ratio")  + labs(color = "Resampling Method") + ylim(0.75,1) +
  theme_bw() + my_theme
dev.off()
