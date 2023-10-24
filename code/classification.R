library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)

cv5 = rsmp("cv", folds = 5)

###data 1
d1_task <- TaskClassif$new("data1", data1, "class", positive = "1")

##logistic regression
d1_logreg <- lrn("classif.glmnet")
d1_logreg$train(d1_task)
d1_logreg$model

#resampling
d1_logreg_rr <- resample(d1_task, d1_logreg, cv5)
d1_logreg_rr$aggregate(msr("classif.bacc"))
d1_logreg_rr$aggregate(msr("classif.fbeta"))
d1_logreg_rr$aggregate(msr("classif.recall"))
d1_logreg_rr$prediction()$confusion


##qda
d1_qda <- lrn("classif.qda")
d1_qda$train(d1_task)
d1_qda$model

#resampling
d1_qda_rr <- resample(d1_task, d1_qda, cv5)
d1_qda_rr$aggregate(msr("classif.bacc"))
d1_qda_rr$aggregate(msr("classif.fbeta"))
d1_qda_rr$aggregate(msr("classif.recall"))
d1_qda_rr$prediction()$confusion


##desicion tree
d1_cart <- lrn("classif.rpart")
d1_cart$train(d1_task)
d1_cart$model

#resampling
d1_cart_rr <- resample(d1_task, d1_cart, cv5)
d1_cart_rr$aggregate(msr("classif.bacc"))
d1_cart_rr$aggregate(msr("classif.fbeta"))
d1_cart_rr$aggregate(msr("classif.recall"))
d1_cart_rr$prediction()$confusion


##knn
d1_knn <- lrn("classif.kknn")
d1_knn$train(d1_task)
d1_knn$model

#resampling
d1_knn_rr <- resample(d1_task, d1_knn, cv5)
d1_knn_rr$aggregate(msr("classif.bacc"))
d1_knn_rr$aggregate(msr("classif.fbeta"))
d1_knn_rr$aggregate(msr("classif.recall"))
d1_knn_rr$prediction()$confusion




###data 2
d2_task <- TaskClassif$new("data2", data2, "class", positive = "1")

##logistic regression
d2_logreg <- lrn("classif.glmnet")
d2_logreg$train(d2_task)
d2_logreg$model

#resampling
d2_logreg_rr <- resample(d2_task, d2_logreg, cv5)
d2_logreg_rr$aggregate(msr("classif.bacc"))
d2_logreg_rr$aggregate(msr("classif.fbeta"))
d2_logreg_rr$aggregate(msr("classif.recall"))
d2_logreg_rr$prediction()$confusion


##qda
d2_qda <- lrn("classif.qda")
d2_qda$train(d2_task)
d2_qda$model

#resampling
d2_qda_rr <- resample(d2_task, d2_qda, cv5)
d2_qda_rr$aggregate(msr("classif.bacc"))
d2_qda_rr$aggregate(msr("classif.fbeta"))
d2_qda_rr$aggregate(msr("classif.recall"))
d2_qda_rr$prediction()$confusion


##desicion tree
d2_cart <- lrn("classif.rpart")
d2_cart$train(d2_task)
d2_cart$model

#resampling
d2_cart_rr <- resample(d2_task, d2_cart, cv5)
d2_cart_rr$aggregate(msr("classif.bacc"))
d2_cart_rr$aggregate(msr("classif.fbeta"))
d2_cart_rr$aggregate(msr("classif.recall"))
d2_cart_rr$prediction()$confusion


##knn
d2_knn <- lrn("classif.kknn")
d2_knn$train(d2_task)
d2_knn$model

#resampling
d2_knn_rr <- resample(d2_task, d2_knn, cv5)
d2_knn_rr$aggregate(msr("classif.bacc"))
d2_knn_rr$aggregate(msr("classif.fbeta"))
d2_knn_rr$aggregate(msr("classif.recall"))
d2_knn_rr$prediction()$confusion



###data 3
d3_task <- TaskClassif$new("data3", data3, "class", positive = "1")

##logistic regression
d3_logreg <- lrn("classif.glmnet")
d3_logreg$train(d2_task)
d3_logreg$model

#resampling
d3_logreg_rr <- resample(d3_task, d3_logreg, cv5)
d3_logreg_rr$aggregate(msr("classif.bacc"))
d3_logreg_rr$aggregate(msr("classif.fbeta"))
d3_logreg_rr$aggregate(msr("classif.recall"))
d3_logreg_rr$prediction()$confusion


##qda
d3_qda <- lrn("classif.qda")
d3_qda$train(d3_task)
d3_qda$model

#resampling
d3_qda_rr <- resample(d3_task, d3_qda, cv5)
d3_qda_rr$aggregate(msr("classif.bacc"))
d3_qda_rr$aggregate(msr("classif.fbeta"))
d3_qda_rr$aggregate(msr("classif.recall"))
d3_qda_rr$prediction()$confusion


##desicion tree
d3_cart <- lrn("classif.rpart")
d3_cart$train(d3_task)
d3_cart$model

#resampling
d3_cart_rr <- resample(d3_task, d3_cart, cv5)
d3_cart_rr$aggregate(msr("classif.bacc"))
d3_cart_rr$aggregate(msr("classif.fbeta"))
d3_cart_rr$aggregate(msr("classif.recall"))
d3_cart_rr$prediction()$confusion


##knn
d3_knn <- lrn("classif.kknn")
d3_knn$train(d3_task)
d3_knn$model

#resampling
d3_knn_rr <- resample(d3_task, d3_knn, cv5)
d3_knn_rr$aggregate(msr("classif.bacc"))
d3_knn_rr$aggregate(msr("classif.fbeta"))
d3_knn_rr$aggregate(msr("classif.recall"))
d3_knn_rr$prediction()$confusion



### data 4
d4_task <- TaskClassif$new("data4", data4, "class", positive = "1")

##logistic regression
d4_logreg <- lrn("classif.glmnet")
d4_logreg$train(d2_task)
d4_logreg$model

#resampling
d4_logreg_rr <- resample(d4_task, d4_logreg, cv5)
d4_logreg_rr$aggregate(msr("classif.bacc"))
d4_logreg_rr$aggregate(msr("classif.fbeta"))
d4_logreg_rr$aggregate(msr("classif.recall"))
d4_logreg_rr$prediction()$confusion


##qda
d4_qda <- lrn("classif.qda")
d4_qda$train(d4_task)
d4_qda$model

#resampling
d4_qda_rr <- resample(d4_task, d4_qda, cv5)
d4_qda_rr$aggregate(msr("classif.bacc"))
d4_qda_rr$aggregate(msr("classif.fbeta"))
d4_qda_rr$aggregate(msr("classif.recall"))
d4_qda_rr$prediction()$confusion


##desicion tree
d4_cart <- lrn("classif.rpart")
d4_cart$train(d4_task)
d4_cart$model

#resampling
d4_cart_rr <- resample(d4_task, d4_cart, cv5)
d4_cart_rr$aggregate(msr("classif.bacc"))
d4_cart_rr$aggregate(msr("classif.fbeta"))
d4_cart_rr$aggregate(msr("classif.recall"))
d4_cart_rr$prediction()$confusion


##knn
d4_knn <- lrn("classif.kknn")
d4_knn$train(d4_task)
d4_knn$model

#resampling
d4_knn_rr <- resample(d4_task, d4_knn, cv5)
d4_knn_rr$aggregate(msr("classif.bacc"))
d4_knn_rr$aggregate(msr("classif.fbeta"))
d4_knn_rr$aggregate(msr("classif.recall"))
d4_knn_rr$prediction()$confusion



###data 5
d5_task <- TaskClassif$new("data5", data5, "class", positive = "1")

##logistic regression
d5_logreg <- lrn("classif.glmnet")
d5_logreg$train(d2_task)
d5_logreg$model

#resampling
d5_logreg_rr <- resample(d5_task, d5_logreg, cv5)
d5_logreg_rr$aggregate(msr("classif.bacc"))
d5_logreg_rr$aggregate(msr("classif.fbeta"))
d5_logreg_rr$aggregate(msr("classif.recall"))
d5_logreg_rr$prediction()$confusion


##qda
d5_qda <- lrn("classif.qda")
d5_qda$train(d5_task)
d5_qda$model

#resampling
d5_qda_rr <- resample(d5_task, d5_qda, cv5)
d5_qda_rr$aggregate(msr("classif.bacc"))
d5_qda_rr$aggregate(msr("classif.fbeta"))
d5_qda_rr$aggregate(msr("classif.recall"))
d5_qda_rr$prediction()$confusion


##desicion tree
d5_cart <- lrn("classif.rpart")
d5_cart$train(d5_task)
d5_cart$model

#resampling
d5_cart_rr <- resample(d5_task, d5_cart, cv5)
d5_cart_rr$aggregate(msr("classif.bacc"))
d5_cart_rr$aggregate(msr("classif.fbeta"))
d5_cart_rr$aggregate(msr("classif.recall"))
d5_cart_rr$prediction()$confusion


##knn
d5_knn <- lrn("classif.kknn")
d5_knn$train(d5_task)
d5_knn$model

#resampling
d5_knn_rr <- resample(d5_task, d5_knn, cv5)
d5_knn_rr$aggregate(msr("classif.bacc"))
d5_knn_rr$aggregate(msr("classif.fbeta"))
d5_knn_rr$aggregate(msr("classif.recall"))
d5_knn_rr$prediction()$confusion



### classify-function
###
### classifies input data according to 4 classifiers and evaluates performance
### with 5-fold crossvalidation and 3 performance metrics
### Input:
###   data  A dataframe. The data to be classified
### Output:
###   a list with length 5
###     [1] a dataframe with the performance values
###     [2]-[5] confusion matrices


classify <- function(data) {
  values <- data.frame(matrix(rep(0, times = 12), ncol = 4, nrow = 3))
  colnames(values) <- c("logreg", "qda", "cart", "knn")
  rownames(values) <- c("bACC", "fbeta", "recall")
  output <- list(values)
  cv5 = rsmp("cv", folds = 5)
  
  task <- TaskClassif$new("data", data, "class", positive = "1")
  
  ##logistic regression
  logreg <- lrn("classif.glmnet")
  #resampling
  logreg_rr <- resample(task, logreg, cv5)
  output[[1]][1,1] <- logreg_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,1] <- logreg_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,1] <- logreg_rr$aggregate(msr("classif.recall"))
  output[[2]] <- logreg_rr$prediction()$confusion
  
  
  ##qda
  qda <- lrn("classif.qda")
  #resampling
  qda_rr <- resample(task, qda, cv5)
  output[[1]][1,2] <- qda_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,2] <- qda_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,2] <- qda_rr$aggregate(msr("classif.recall"))
  output[[3]] <- qda_rr$prediction()$confusion
  
  
  ##desicion tree
  cart <- lrn("classif.rpart")
  #resampling
  cart_rr <- resample(task, cart, cv5)
  output[[1]][1,3] <- cart_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,3] <- cart_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,3] <- cart_rr$aggregate(msr("classif.recall"))
  output[[4]] <- cart_rr$prediction()$confusion
  
  
  ##knn
  knn <- lrn("classif.kknn")
  #resampling
  knn_rr <- resample(task, knn, cv5)
  output[[1]][1,4] <- knn_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,4] <- knn_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,4] <- knn_rr$aggregate(msr("classif.recall"))
  output[[5]] <- knn_rr$prediction()$confusion
  
  output
}


task <- TaskClassif$new("data61", data61, "class", positive = "1")
nb <- lrn("classif.naive_bayes")
#resampling
nb_rr <- resample(task, nb, cv5)
nb_rr$aggregate(msr("classif.bacc"))
nb_rr$aggregate(msr("classif.fbeta"))
nb_rr$aggregate(msr("classif.recall"))
nb_rr$prediction()$confusion
