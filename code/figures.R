library(ggplot2)
library(ggpubr)
library(patchwork)

### undersampling vs oversampling

#imbalanced dataset
imbdata <- sim_imbalanced_data(1000, 3, 9, 2)
plot_imbdata <- ggplot(imbdata, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Imbalanced Data") + xlab("") + ylab("") + 
  theme_bw()
#undersampling
underdata <- sim_imbalanced_data(200, 3, 1, 2)
plot_underdata <- ggplot(underdata, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Undersampling") + xlab("") + ylab("") + 
  theme_bw()
#oversampling
overdata <- sim_imbalanced_data(2000, 3, 1, 2)
plot_overdata <- ggplot(overdata, mapping = aes(x = V2, y = V3, color = class)) + 
  geom_point() + scale_color_manual(values = c("orangered3", "steelblue4")) + 
  ggtitle("Oversampling") + xlab("") + ylab("") + 
  theme_bw()

layout <- "
#A#
B#C"

plot_imbdata + plot_underdata + plot_overdata + plot_layout(design = layout)

ggarrange(plot_imbdata,
          ggarrange(plot_underdata, plot_overdata, ncol = 2),
          nrow = 2)
