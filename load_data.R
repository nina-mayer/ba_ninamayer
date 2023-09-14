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




good_customer <- readARFF("good_customer")

good_customer$bad_client_target <- as.factor(good_customer$bad_client_target)
good_customer$sex <- as.factor(good_customer$sex)
good_customer$education <- as.factor(good_customer$education)
good_customer$product_type <- as.factor(good_customer$product_type)
good_customer$family_status <- as.factor(good_customer$family_status)

jpeg("gcbarplot.jpg", units = "in", width = 14, height = 8, res = 600)
ggplot(good_customer, aes(x = bad_client_target)) + geom_bar(fill = c("coral4", "palegreen4")) + 
  ggtitle("Classes for Good Customer Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()
dev.off()




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




law_school_admission_list <- getOMLDataSet(data.id = 43904L)
law_school_admission <- law_school_admission_list$data

typeof(law_school_admission$ugpagt3)
law_school_admission$ugpagt3 <- as.factor(law_school_admission$ugpagt3)
levels(law_school_admission$ugpagt3) <- c(0,1)

jpeg("lsabarplot.jpg", units = "in", width = 14, height = 8, res = 600)
ggplot(law_school_admission, aes(x = ugpagt3)) + geom_bar(fill = c("dodgerblue4", "chocolate3")) + 
  ggtitle("Classes for Law School Admission Dataset") +
  theme(plot.title = element_text(size = 21), axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18), legend.text = element_text(size = 16), 
        legend.title =  element_text(size = 18)) + 
  theme_bw()
dev.off()
