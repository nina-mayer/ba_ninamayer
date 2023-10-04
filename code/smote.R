library(ggplot2)
library(ggpubr)
library(smotefamily)

#test

plot_data_before <- ggplot(data24, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Data before SMOTE") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

list_smote <- SMOTE(data24[,2:4], data24[,1], 5)
data_smote <- list_smote$data

plot_data_smote <- ggplot(data_smote, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Data after SMOTE") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

ggarrange(plot_data_before, plot_data_smote, ncol = 2)
