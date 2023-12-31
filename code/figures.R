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



### Linear Classifier

data_linclass <- sim_imbalanced_data(500, 2, 4, 3)
jpeg("linclass.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(data_linclass, mapping = aes(x = X1, y = X2, color = class, shape = class)) + 
  geom_point(size = 2) + scale_color_manual(values = c("steelblue4", "orangered3")) +
  geom_segment(aes(x = -7, y = 1, xend = 0, yend = -8), color = "black", size = 2) +
  xlab("") + ylab("") + theme_bw() + my_theme +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
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
  ggtitle("Data after SMOTE") +xlab("") + ylab("") + theme_bw() + my_theme +
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

# comp_hyper_smote
smote_h_rf <- ggplot(comp_hyper_smote_rf, mapping = aes(x = imbalance, y = value, group = hyper, color = hyper)) + 
  geom_point() + geom_line(size = 1) + scale_color_manual(values = c("#336699", "#99CCFF")) +
  xlab("Imbalance Ratio")  + ylab("bACC") + ylim(0.85,1) + ggtitle("Random Forrest") +
  labs(color = "Hyperparametertuning") + theme_bw() + my_theme
smote_h_knn <- ggplot(comp_hyper_smote_knn, mapping = aes(x = imbalance, y = value, group = hyper, color = hyper)) + 
  geom_point() + geom_line(size = 1) + scale_color_manual(values = c("#336699", "#99CCFF")) +
  xlab("Imbalance Ratio")  + ylab("bACC") + ylim(0.85,1) + ggtitle("k-Nearest Neighbours") +
  labs(color = "Hyperparametertuning") + theme_bw() + my_theme

jpeg("comp_hyper_smote.jpg", units = "in", width = 14, height = 8, res = 800)
ggarrange(smote_h_rf,smote_h_knn, common.legend = TRUE, legend = "bottom")
dev.off()

# comp_hyper_sbc
sbc_h_rf <- ggplot(comp_hyper_sbc_rf, mapping = aes(x = imbalance, y = value, group = hyper, color = hyper)) + 
  geom_point() + geom_line(size = 1) + scale_color_manual(values = c("#336699", "#99CCFF")) +
  xlab("Imbalance Ratio")  + ylab("bACC") + ylim(0.85,1) + ggtitle("Random Forrest") +
  labs(color = "Hyperparametertuning") + theme_bw() + my_theme
sbc_h_knn <- ggplot(comp_hyper_sbc_knn, mapping = aes(x = imbalance, y = value, group = hyper, color = hyper)) + 
  geom_point() + geom_line(size = 1) + scale_color_manual(values = c("#336699", "#99CCFF")) +
  xlab("Imbalance Ratio")  + ylab("bACC") + ylim(0.85,1) + ggtitle("k-Nearest Neighbours") +
  labs(color = "Hyperparametertuning") + theme_bw() + my_theme

jpeg("comp_hyper_sbc.jpg", units = "in", width = 14, height = 8, res = 800)
ggarrange(sbc_h_rf,sbc_h_knn, common.legend = TRUE, legend = "bottom")
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
  scale_color_manual(values = c("#E69F00", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
  labs(color = "Classifier", linetype = "Performance Measure") + ylim(0,1) +
  theme_bw() + my_theme
dev.off()

# rusbag
jpeg("rusbag_results.jpg", units = "in", width = 14, height = 8, res = 800)
ggplot(rusbag, mapping = aes(x = imbalance, y = value, color = classifier, linetype = performance, 
                               group = interaction(classifier, performance))) + 
  geom_point() + geom_line(size = 1) + scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_color_manual(values = c("#E69F00", "#6A3D9A")) + xlab("Imbalance Ratio") + ylab("Value") + 
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
