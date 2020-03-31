########################################
### Classification with testing data ###
########################################

regression.test <- function(models, dat_test, info){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### make predictions

  X_test <- as.matrix(dat_test)
  pred <- as.matrix(predict(models, s = 'lambda.min', newx = X_test, type = 'class'))
  labels <- data.frame(Index = 1:nrow(dat_test), identity = rep(NA, nrow(dat_test)), pred)
  colnames(labels)[3] <- "emotion_idx"
  labels$identity <- info$identity
  return(labels)
}
