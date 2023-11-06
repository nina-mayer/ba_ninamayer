library(OpenML)
library(farff)
library(ggplot2)

### thoraric surgery data set

ts_list <- getOMLDataSet(data.id = 4329L)
thoracic_surgery <- ts_list$data

thoracic_surgery$Risk1Yr <- as.factor(thoracic_surgery$Risk1Yr)
levels(thoracic_surgery$Risk1Yr) <- c(1, 0)

ggplot(thoracic_surgery, aes(x = Risk1Yr)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for Thoracic Surgery Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()

table(thoracic_surgery$Risk1Yr)


### fraud detection data set

fd_list <- getOMLDataSet(data.id = 42175L)
fraud_detection <- fd_list$data

fraud_detection$Class <- as.factor(fraud_detection$Class)

ggplot(fraud_detection, aes(x = Class)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for Fraud Detection Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()

table(fraud_detection$Class)
