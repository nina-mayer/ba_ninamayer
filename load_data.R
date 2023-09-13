library(OpenML)
library(farff)
library(ggplot2)

thoracic_surgery <- readARFF("thoracic_surgery.arff")

thoracic_surgery$Class <- as.factor(thoracic_surgery$Class)

jpeg("tsbarplot.jpg", units = "in", width = 14, height = 8, res = 600)
ggplot(thoracic_surgery, aes(x = Class)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for Thoracic Surgery Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()
dev.off()

#good_customer <- readARFF("good_customer")

#jpeg("gcbarplot.jpg", units = "in", width = 14, height = 8, res = 600)
#ggplot(good_customer, aes(x = bad_client_target)) + geom_bar(fill = c("dodgerblue4", "chocolate3")) + 
#  ggtitle("Classes for Good Customer Dataset") +
#  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
#        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
#        legend.title =  element_text(size = 18)) + 
#  theme_bw()
#dev.off()

grade_prediction <- readARFF("grade_prediction")

grade_prediction$pass <- as.factor(grade_prediction$pass)

jpeg("gpbarplot.jpg", units = "in", width = 14, height = 8, res = 600)
ggplot(grade_prediction, aes(x = pass)) + geom_bar(fill = c("dodgerblue4", "chocolate3")) + 
  ggtitle("Classes for Grade Prediction Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()
dev.off()

