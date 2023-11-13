library(mlr3)
library(mlr3learners)
library(mlr3measures)
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
###   imb   A string. The data imbalance
### Output:
###   a list with length 5
###     [1] a dataframe with the performance values
###     [2]-[5] confusion matrices


classify <- function(data, imb) {
  output <- data.frame(matrix(rep(0, times = 36), ncol = 4, nrow = 9))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 9)
  output[,4] <- rep(c("bACC", "F1", "Recall"), times = 3)
  cv5 = rsmp("cv", folds = 5)
  
  task <- TaskClassif$new("data", data, "class", positive = "1")
  
  
  ##nb
  nb <- lrn("classif.naive_bayes")
  #resampling
  nb_rr <- resample(task, nb, cv5)
  output[1,2] <- nb_rr$aggregate(msr("classif.bacc"))
  output[2,2] <- nb_rr$aggregate(msr("classif.fbeta"))
  output[3,2] <- nb_rr$aggregate(msr("classif.recall"))
  
  
  ##random forest
  rf <- lrn("classif.ranger")
  #resampling
  rf_rr <- resample(task, rf, cv5)
  output[4,2] <- rf_rr$aggregate(msr("classif.bacc"))
  output[5,2] <- rf_rr$aggregate(msr("classif.fbeta"))
  output[6,2] <- rf_rr$aggregate(msr("classif.recall"))
  
  
  ##knn
  knn <- lrn("classif.kknn")
  #resampling
  knn_rr <- resample(task, knn, cv5)
  output[7,2] <- knn_rr$aggregate(msr("classif.bacc"))
  output[8,2] <- knn_rr$aggregate(msr("classif.fbeta"))
  output[9,2] <- knn_rr$aggregate(msr("classif.recall"))
  
  output
}

# classify data

raw4 <- classify(data4, "1:4")
raw9 <- classify(data9, "1:9")
raw19 <- classify(data19, "1:19")
raw99 <- classify(data99, "1:99")
raw199 <- classify(data199, "1:199")

raw <- rbind(raw4, raw9, raw19, raw99, raw199)
raw$imbalance <- factor(raw$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))


classify(spectf_heart)
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
###   imb   A string. The data imbalance
### Output:
###   A dataframe with the performance scores for each learner

classify_resampling <- function(data, resample, imb) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 36), ncol = 4, nrow = 9))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 9)
  output[,4] <- rep(c("bACC", "F1", "Recall"), times = 3) 
  
  #naive bayes
  output[1,2] <- resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.bacc"), resample)
  output[2,2] <- resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.fbeta"), resample)
  output[3,2] <- resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.recall"), resample)
  
  #rf
  output[4,2] <- resampling_cv(data, lrn("classif.ranger"), msr("classif.bacc"), resample)
  output[5,2] <- resampling_cv(data, lrn("classif.ranger"), msr("classif.fbeta"), resample)
  output[6,2] <- resampling_cv(data, lrn("classif.ranger"), msr("classif.recall"), resample)
  
  #knn
  output[7,2] <- resampling_cv(data, lrn("classif.kknn"), msr("classif.bacc"), resample)
  output[8,2] <- resampling_cv(data, lrn("classif.kknn"), msr("classif.fbeta"), resample)
  output[9,2] <- resampling_cv(data, lrn("classif.kknn"), msr("classif.recall"), resample)
  
  output
}

### SMOTE

smote4 <- classify_resampling(data4, SMOTE, "1:4")
smote9 <- classify_resampling(data9, SMOTE, "1:9")
smote19 <- classify_resampling(data19, SMOTE, "1:19")
smote99 <- classify_resampling(data99, SMOTE, "1:99")
smote199 <- classify_resampling(data199, SMOTE, "1:199")

smote <- rbind(smote4, smote9, smote19, smote99, smote199)
smote$imbalance <- factor(smote$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))


### SBC

sbc4 <- classify_resampling(data4, SBC, "1:4")
sbc9 <- classify_resampling(data9, SBC, "1:9")
sbc19 <- classify_resampling(data19, SBC, "1:19")
sbc99 <- classify_resampling(data99, SBC, "1:99")
sbc199 <- classify_resampling(data199, SBC, "1:199")

sbc <- rbind(sbc4, sbc9, sbc19, sbc99, sbc199)
sbc$imbalance <- factor(sbc$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))


### ROS

ros4 <- classify_resampling(data4, ROS, "1:4")
ros9 <- classify_resampling(data9, ROS, "1:9")
ros19 <- classify_resampling(data19, ROS, "1:19")
ros99 <- classify_resampling(data99, ROS, "1:99")
ros199 <- classify_resampling(data199, ROS, "1:199")

ros <- rbind(ros4, ros9, ros19, ros99, ros199)
ros$imbalance <- factor(ros$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))


### RUS

rus4 <- classify_resampling(data4, RUS, "1:4")
rus9 <- classify_resampling(data9, RUS, "1:9")
rus19 <- classify_resampling(data19, RUS, "1:19")
rus99 <- classify_resampling(data99, RUS, "1:99")
rus199 <- classify_resampling(data199, RUS, "1:199")

rus <- rbind(rus4, rus9, rus19, rus99, rus199)
rus$imbalance <- factor(rus$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

### COMPARISON

comp1 <- data.frame(matrix(rep(0, times = 75), ncol = 3, nrow = 25))
colnames(comp1) <- c("method", "imbalance", "bACC")
comp1[,1] <- c(rep("No Resampling", times = 5), rep("SMOTE", times = 5), 
               rep("ROS", times = 5), rep("SBC", times = 5), 
               rep("RUS", times = 5))
comp1[,2] <- rep(c("1:4", "1:9", "1:19", "1:99", "1:199"), times = 5) 
comp1[,3] <- c(raw[4,2], raw[13,2], raw[22,2], raw[31,2], raw[40,2],
               smote[4,2], smote[13,2], smote[22,2], smote[31,2], smote[40,2],
               ros[4,2], ros[13,2], ros[22,2], ros[31,2], ros[40,2],
               sbc[4,2], sbc[13,2], sbc[22,2], sbc[31,2], sbc[40,2],
               rus[4,2], rus[13,2], rus[22,2], rus[31,2], rus[40,2])
comp1$imbalance <- factor(comp1$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))
comp1$method <- factor(comp1$method, levels = c("No Resampling", "SMOTE", "ROS", "SBC", "RUS"))





### HYBRID

### hyb_resampling_cv - function
###
### classifies data with 5 fold cv while applying the resampling method to the 
### training data in each fold. The classifying learner and the performance measure 
### is to be specified
### Input:
###   data      A dataframe. The data to be classified
###   lrnr      An object of mlr3-class Learner. The learner to be trained
###   perf      An object of mlr3-class Measure. The measure of the performance evaluation.
###   oversample  A "bimba"-method. The oversampling method
###   undersample  A "bimba"-method. The undersampling method
### Output:
###   A numeric. The aggregated performance measure

hyb_resampling_cv <- function(data, lrnr, perf, oversample, undersample) {
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
    train <- oversample(train, 50)
    train <- undersample(train, 33.33)
    
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


### hyb_classify_resampling - function
###
### classifies a (imbalanced) data set according to 4 classifiers and evaluates 
### performance with 5-fold cv and 3 performance metrics while resampling the data
### with one oversampling and one undersampling method
### Input:
###   data    A dataframe. The data to be classified
###   oversample  A "bimba"-method. The oversampling method
###   undersample  A "bimba"-method. The undersampling method
###   imb   A string. The data imbalance
### Output:
###   A dataframe with the performance scores for each learner

hyb_classify_resampling <- function(data, oversample, undersample, imb) {
  #define output dataframe
  output <- data.frame(matrix(rep(0, times = 36), ncol = 4, nrow = 9))
  colnames(output) <- c("classifier", "value", "imbalance", "performance")
  output[,1] <- c(rep("NB", times = 3), rep("RF", times = 3), rep("kNN", times = 3))
  output[,3] <- rep(imb, times = 9)
  output[,4] <- rep(c("bACC", "F1", "Recall"), times = 3) 
  
  
  #naive bayes
  output[1,2] <- hyb_resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.bacc"), oversample, undersample)
  output[2,2] <- hyb_resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.fbeta"), oversample, undersample)
  output[3,2] <- hyb_resampling_cv(data, lrn("classif.naive_bayes"), msr("classif.recall"), oversample, undersample)
  
  #rf
  output[4,2] <- hyb_resampling_cv(data, lrn("classif.ranger"), msr("classif.bacc"), oversample, undersample)
  output[5,2] <- hyb_resampling_cv(data, lrn("classif.ranger"), msr("classif.fbeta"), oversample, undersample)
  output[6,2] <- hyb_resampling_cv(data, lrn("classif.ranger"), msr("classif.recall"), oversample, undersample)
  
  #knn
  output[7,2] <- hyb_resampling_cv(data, lrn("classif.kknn"), msr("classif.bacc"), oversample, undersample)
  output[8,2] <- hyb_resampling_cv(data, lrn("classif.kknn"), msr("classif.fbeta"), oversample, undersample)
  output[9,2] <- hyb_resampling_cv(data, lrn("classif.kknn"), msr("classif.recall"), oversample, undersample)
  
  output
}

### SMOTE + SBC

smotesbc4 <- hyb_classify_resampling(data4, SMOTE, SBC, "1:4")
smotesbc9 <- hyb_classify_resampling(data9, SMOTE, SBC, "1:9")
smotesbc19 <- hyb_classify_resampling(data19, SMOTE, SBC, "1:19")
smotesbc99 <- hyb_classify_resampling(data99, SMOTE, SBC, "1:99")
smotesbc199 <- hyb_classify_resampling(data199, SMOTE, SBC, "1:199")

smotesbc <- rbind(smotesbc4, smotesbc9, smotesbc19, smotesbc99, smotesbc199)
smotesbc$imbalance <- factor(smotesbc$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

### SMOTE + RUS

smoterus4 <- hyb_classify_resampling(data4, SMOTE, RUS, "1:4")
smoterus9 <- hyb_classify_resampling(data9, SMOTE, RUS, "1:9")
smoterus19 <- hyb_classify_resampling(data19, SMOTE, RUS, "1:19")
smoterus99 <- hyb_classify_resampling(data99, SMOTE, RUS, "1:99")
smoterus199 <- hyb_classify_resampling(data199, SMOTE, RUS, "1:199")

smoterus <- rbind(smoterus4, smoterus9, smoterus19, smoterus99, smoterus199)
smoterus$imbalance <- factor(smoterus$imbalance, levels = c("1:4", "1:9", "1:19", "1:99", "1:199"))

