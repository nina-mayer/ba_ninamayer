library(OpenML)
library(farff)
library(ggplot2)

thoracic_surgery <- readARFF("phpjX67St.arff")

tsbarplot <- ggplot(thoracic_surgery, aes(x = Class)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for Thoracic Surgery Dataset") +
  theme_bw()
