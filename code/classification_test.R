library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3measures)
library(mlr3tuning)

###grade_prediction

#task
gp_task <- TaskClassif$new("grade_prediction", grade_prediction, "pass")
mlr_tasks$add("grade_prediction", gp_task)

#desicion tree
gp_learner <- lrn("classif.rpart")
gp_learner$train(gp_task)
gp_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
gp_rr <- resample(gp_task, gp_learner, cv5)
gp_rr$aggregate(msr("classif.ce"))
gp_rr$prediction()

gp_scores <- gp_rr$score()
gp_scores[, c("iteration", "classif.ce")]



###thoracic_surgery

#task
ts_task <- TaskClassif$new("thoracic_surgery", thoracic_surgery, "Class")
mlr_tasks$add("thoracic_surgery", ts_task)

#decision tree
ts_learner <- lrn("classif.rpart")
ts_learner$train(ts_task)
ts_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
ts_rr <- resample(ts_task, ts_learner, cv5)
ts_rr$aggregate(msr("classif.ce"))
ts_rr$prediction()

ts_scores <- ts_rr$score()
ts_scores[, c("iteration", "classif.ce")]



###law_school_admission

#task
lsa_task <- TaskClassif$new("law_school_admission", law_school_admission, "ugpagt3")
mlr_tasks$add("law_school_admission", lsa_task)

#decision tree
lsa_learner <- lrn("classif.rpart")
lsa_learner$train(lsa_task)
lsa_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
lsa_rr <- resample(lsa_task, lsa_learner, cv5)
lsa_rr$aggregate(msr("classif.ce"))
lsa_rr$prediction()

lsa_scores <- lsa_rr$score()
lsa_scores[, c("iteration", "classif.ce")]



###good_customer

#task
gc_task <- TaskClassif$new("good_customer", good_customer, "bad_client_target")
mlr_tasks$add("good_customer", gc_task)

#decision tree
gc_learner <- lrn("classif.rpart")
gc_learner$train(gc_task)
gc_learner$model

#resampling
cv5 = rsmp("cv", folds = 5)
gc_rr <- resample(gc_task, gc_learner, cv5)
gc_rr$aggregate(msr("classif.ce"))
gc_rr$prediction()

gc_scores <- gc_rr$score()
gc_scores[, c("iteration", "classif.ce")]



###benchmark

learners <- list(lrn("classif.rpart"), lrn("classif.kknn"), lrn("classif.ranger"))
tasks <- list(tsk("grade_prediction"), tsk("thoracic_surgery"), tsk("law_school_admission"), tsk("good_customer"))

design <- benchmark_grid(tasks, learners, cv5)
bmr <- benchmark(design)

bmr_ag <- bmr$aggregate()
bmr_ag[, c("task_id", "learner_id", "classif.ce")]

autoplot(bmr)


### hyperparametertuning

task <- TaskClassif$new("train", train_smote, "class")
lrnr <- lrn("classif.kknn", k = to_tune(1,10))
tuner <- tnr("grid_search", batch_size = 5)
terminator <- trm("stagnation", iters = 3, threshold = 0.01)

instance <- ti(task = task, learner = lrnr, resampling = cv5, terminator = terminator)
tuner$optimize(instance)
instance$result$learner_param_vals
lrnr_tuned <- lrn("classif.kknn")
lrnr_tuned$param_set$values <- instance$result_learner_param_vals
lrnr_tuned$train(task)
at <- auto_tuner(tuner = tuner, learner = lrnr, resampling = cv5, measure = msr("classif.ce"))
at$train(task)
prediction <- at$predict_newdata(test)
prediction$score(msr("classif.bacc"))
prediction$score(msr("classif.recall"))
prediction$score(msr("classif.fbeta"))





tune_resampling <- function(data, lrnr, resample) {
  #3-fold-CV
  splits <- split(data, sample(rep(1:3, times = c(2666, 2667, 2667))))
  best <- data.frame(matrix(rep(0, times = 9), ncol = 3, nrow = 3))

  for(i in 1:3){
    #define train and validation set for split
    train <- data.frame()
    trainsets <- 1:3
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in trainsets){
      train <- rbind(train, splits[[j]])
    }
    valid <- splits[[i]]
    
    
    #learn models different hyperparameters with training data and find the best
    if(lrnr == "rf"){
      hypers <- data.frame(matrix(rep(0, times = 30), ncol = 3, nrow = 10))
      maxdepth <- 1:35
      numtrees <- 1:2000
      
      #train models
      for(l in 1:10){
        hypers[l,1] <- sample(maxdepth, 1)
        hypers[l,2] <- sample(numtrees, 1)
        #apply resampling to training data
        train <- resample(train)
        
        #learn model with new training data
        task <- TaskClassif$new("train", train, "class", positive = "1")
        learner <- lrn("classif.ranger", max.depth = hypers[l,1], num.trees = hypers[l,2])
        learner$train(task)
        
        #predict test data with learner
        prediction <- learner$predict_newdata(valid)
        hypers[l,3] <- prediction$score(msr("classif.bacc"))
      }
      
      #find best model and save hyperparameters
      best[i,] <- hypers[which.max(hypers[,3]),]
    } else if(lrnr == "knn") {
      hypers <- data.frame(matrix(rep(0, times = 30), ncol = 3, nrow = 10))
      k <- 3:30
      
      for(l in 1:10){
        hypers[l,1] <- sample(k, 1)
        
        #apply resampling to training data
        train <- resample(train)
        
        #learn model with new training data
        task <- TaskClassif$new("train", train, "class", positive = "1")
        learner <- lrn("classif.kknn", k = hypers[l,1])
        learner$train(task)
        
        #predict test data with learner
        prediction <- learner$predict_newdata(test)
        hypers[l,3] <- prediction$score(msr("classif.bacc"))
      }
      
      #find best model and save hyperparameters
      best[i,] <- hypers[which.max(hypers[,3]),]
    }
    
    #find best model from the 3 resampling procedures
    best[which.max(best[,3]),]
  
  }
}

hyper_resampling_cv <- function(data, lrnr, resample) {
  #manual 5-fold CV
  splits <- split(data, sample(rep(1:5, times = rep(2000, times = 5))))
  results <- data.frame(matrix(rep(0, times = 15), ncol = 3, nrow = 5))
  for(i in 1:5){
    #define train and test for split
    train <- data.frame()
    trainsets <- 1:5
    trainsets <- trainsets[!trainsets %in% c(i)]
    for(j in trainsets){
      train <- rbind(train, splits[[j]])
    }
    test <- splits[[i]]
    
    #find best hyperparameters
    hypers <- tune_resampling(train, lrnr, resample)
    
    #apply smote to training data
    train <- resample(train)
    
    #learn model with new training data
    task <- TaskClassif$new("train", train, "class", positive = "1")
    if(lrnr == "rf") {
      learner <- lrn("classif.ranger", max.depth = hypers[1,1], num.trees = hypers[1,2])
    } else if (lrnr == "knn") {
      learner <- lrn("classif.kknn", k = hypers[1,1])
    }
    learner$train(task)
    
    #predict test data with learner
    prediction <- learner$predict_newdata(test)
    results[i,1] <- prediction$score(msr("classif.bacc"))
    results[i,2] <- prediction$score(msr("classif.recall"))
    results[i,3] <- prediction$score(msr("classif.fbeta"))
  }
  #aggregate results
  output <- rep(0, times = 3)
  for(i in 1:3){
    output[i] <- mean(results[,i])
  }
  output
}
