########################################
### Classification with testing data ###
########################################

regression.test <- function(models, dat_test){
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  - processed features from testing images 
  ### Output: training model specification
  
  ### make predictions

  X_test <- as.matrix(dat_test)
  pred <- as.matrix(predict(models, s = 'lambda.min', newx = X_test, type = 'class'))
  return(pred)
}
