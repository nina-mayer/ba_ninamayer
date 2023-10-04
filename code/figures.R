library(ggplot2)
library(ggpubr)
library(patchwork)

### undersampling vs oversampling

#imbalanced dataset
imbdata <- sim_imbalanced_data(1000, 3, 9, 2)
plot_imbdata <- ggplot(imbdata, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalanced Data") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
#undersampling
underdata <- sim_imbalanced_data(200, 3, 1, 2)
plot_underdata <- ggplot(underdata, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Undersampling") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
#oversampling
overdata <- sim_imbalanced_data(2000, 3, 1, 2)
plot_overdata <- ggplot(overdata, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Oversampling") + xlab("") + ylab("") + theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

#plot
top_row <- ggarrange(NULL, plot_imbdata, NULL, ncol = 3, widths = c(1,2,1))
bottom_row <- ggarrange(plot_underdata, plot_overdata, ncol = 2)

jpeg("underoversampling.jpg", units = "in", width = 14, height = 8, res = 600)
ggarrange(top_row, bottom_row, ncol = 1)
dev.off()
