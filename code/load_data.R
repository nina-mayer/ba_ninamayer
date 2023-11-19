library(OpenML)
library(farff)
library(ggplot2)



### fraud detection data set

fd_list <- getOMLDataSet(data.id = 42175L)
fraud_detection <- fd_list$data

fraud_detection$Class <- as.factor(fraud_detection$Class)
colnames(fraud_detection)[31] <- "class"

ggplot(fraud_detection, aes(x = class)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for Fraud Detection Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()

table(fraud_detection$class)


### spectf heart data set

sh_list <- getOMLDataSet(data.id = 1600L)
spectf_heart <- sh_list$data

colnames(spectf_heart)[45] <- "class"
spectf_heart$class <- as.factor(spectf_heart$class)
levels(spectf_heart$class) <- c(2,3)
levels(spectf_heart$class) <- c(1,0)
spectf_heart$class <- factor(spectf_heart$class, levels = c(0,1))

ggplot(spectf_heart, aes(x = class)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for SPECTF Heart Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()


table(spectf_heart$class)
