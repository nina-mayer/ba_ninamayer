library(mlr3)
library(mlr3learners)
library(mlr3measures)
library(smotefamily)
library(tidyverse)
library(e1071)
library(bimba)
library(caret)


set.seed(12226947)

### NO RESAMPLING

### classify - function
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
  colnames(values) <- c("logreg", "nb", "rf", "knn")
  rownames(values) <- c("bACC", "f1", "recall")
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
  
  
  ##nb
  nb <- lrn("classif.naive_bayes")
  #resampling
  nb_rr <- resample(task, nb, cv5)
  output[[1]][1,2] <- nb_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,2] <- nb_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,2] <- nb_rr$aggregate(msr("classif.recall"))
  output[[3]] <- nb_rr$prediction()$confusion
  
  
  ##random forest
  rf <- lrn("classif.ranger")
  #resampling
  rf_rr <- resample(task, rf, cv5)
  output[[1]][1,3] <- rf_rr$aggregate(msr("classif.bacc"))
  output[[1]][2,3] <- rf_rr$aggregate(msr("classif.fbeta"))
  output[[1]][3,3] <- rf_rr$aggregate(msr("classif.recall"))
  output[[4]] <- rf_rr$prediction()$confusion
  
  
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

# classify data

classify(data4)
classify(data9)
classify(data19)
classify(data99)
classify(data199)
colnames(thoracic_surgery)[17] <- "class"
classify(thoracic_surgery)
colnames(fraud_detection)[31] <- "class"
classify(fraud_detection)


### RESAMPLING METHODS

### resampling_cv - function
###
### classifies data with 5 fold cv while applying the resampling method to the 
### training data in each fold. The classifying learner and the performance measure 
### is to be specified
### Input:
###   data      A dataframe. The data to be classified
###   lrnr      An object of mlr3-class Learner. The learner to be trained
###   perf      An object of mlr3-class Measure. The measure of the performance evaluation.
###   resample  A "bimba"-method. The resampling method
### Output:
###   A numeric. The aggregated performance measure


resampling_cv <- function(data, lrnr, perf, resample) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = rep(2000, times = 5))))
  results <- rep(0, times = 5)
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- 1:5
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in 1:4){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #apply smote to training data
    train <- resample(train)
    
    #learn model with new training data
    task <- TaskClassif$new("train", train, "class", positive = "1")
    learner <- lrnr
    learner$train(task)
    
    #predict test data with learner
    prediction <- learner$predict_newdata(test)
    results[i] <- prediction$score(perf)
  }
  #aggregate results
  mean(results)
}


### classify_resampling - function
###
### classifies a (imbalanced) data set according to 4 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while resampling the data
### Input:
###   data    A dataframe. The data to be classified
###   resample  A "bimba"-method. The resampling method
### Output:
###   A dataframe with the performance scores for each learner

classify_resampling <- function(data, resample) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 12), ncol = 4, nrow = 3))
  colnames(output) <- c("logreg", "nb", "rf", "knn")
  rownames(output) <- c("bACC", "f1", "recall") 
  
  #logreg
  output[1,1] <- resampling_cv(data, lrn("classif.glmnet"), msr("classif.bacc"), resample)
  output[2,1] <- resampling_cv(data, lrn("classif.glmnet"), msr("classif.fbeta"), resample)
  output[3,1] <- resampling_cv(data, lrn("classif.glmnet"), msr("classif.recall"), resample)
  
  #naive bayes
  output[1,2] <- resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.bacc"), resample)
  output[2,2] <- resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.fbeta"), resample)
  output[3,2] <- resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.recall"), resample)
  
  #rf
  output[1,3] <- resampling_cv(data, lrn("classif.ranger"), msr("classif.bacc"), resample)
  output[2,3] <- resampling_cv(data, lrn("classif.ranger"), msr("classif.fbeta"), resample)
  output[3,3] <- resampling_cv(data, lrn("classif.ranger"), msr("classif.recall"), resample)
  
  #knn
  output[1,4] <- resampling_cv(data, lrn("classif.kknn"), msr("classif.bacc"), resample)
  output[2,4] <- resampling_cv(data, lrn("classif.kknn"), msr("classif.fbeta"), resample)
  output[3,4] <- resampling_cv(data, lrn("classif.kknn"), msr("classif.recall"), resample)
  
  output
}

### SMOTE

classify_resampling(data4, SMOTE)
classify_resampling(data9, SMOTE)
classify_resampling(data19, SMOTE)
classify_resampling(data99, SMOTE)
classify_resampling(data199, SMOTE)


### SBC

classify_resampling(data4, SBC)
classify_resampling(data9, SBC)
classify_resampling(data19, SBC)
classify_resampling(data99, SBC)
classify_resampling(data199, SBC)

### ROS

classify_resampling(data4, ROS)
classify_resampling(data9, ROS)
classify_resampling(data19, ROS)
classify_resampling(data99, ROS)
classify_resampling(data199, ROS)

### RUS

classify_resampling(data4, RUS)
classify_resampling(data9, RUS)
classify_resampling(data19, RUS)
classify_resampling(data99, RUS)
classify_resampling(data199, RUS)
