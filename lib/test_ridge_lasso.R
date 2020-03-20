########################################
### Classification with testing data ###
########################################

regression.test <- function(models, dat_test){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### make predictions
  if ("label" %in% colnames(dat_test)) {
    
    X_test <- as.matrix(dat_test[, -ncol(dat_test)])
  } else {
    dat_test$emotion_idx <- rep(1, nrow(dat_test))
    X_test <- as.matrix(dat_test[, -ncol(dat_test)])
  }
  pred <- as.matrix(predict(models, s = 'lambda.min', newx = X_test, type = 'class'))
  #pred <- as.factor(unname(sapply(X = colnames(probabilities)[apply(probabilities, 1, which.max)], as.integer)))
  return(pred)
}
