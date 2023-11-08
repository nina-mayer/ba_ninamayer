library(ggplot2)
library(ggpubr)
library(smotefamily)
library(bimba)
library(caret)

#test smote

plot_data_before <- ggplot(data4, mapping = aes(x = X1, y = X2, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Data before SMOTE") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

data_smote <- SMOTE(data4)


plot_data_smote <- ggplot(data_smote, mapping = aes(x = X1, y = X2, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Data after SMOTE") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

ggarrange(plot_data_before, plot_data_smote, ncol = 2)

#test sbc

plot_data_before <- ggplot(data24, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Data before SBC") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

data24[,5] <- data24[,1]
data24 <- data24[,-1]
data_sbc <- SBC(data24, perc_maj = 50)

plot_data_sbc <- ggplot(data_sbc, mapping = aes(x = V2, y = V3, color = V5)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Data after SBC") + xlab("Variable 1") + ylab("Variable 2") + 
  theme_bw()

ggarrange(plot_data_before, plot_data_sbc, ncol = 2)
