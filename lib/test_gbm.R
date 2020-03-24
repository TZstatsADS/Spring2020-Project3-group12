########################################
### Classification with testing data ###
########################################

test_gbm <- function(gbm.fit, input.test, n){
  
  ### Input: 
  ###  - the fitted GBM model using training data
  ###  - number of trees
  ###  - processed features from testing images 
  ### Output: testing model performance
  
  ### Make predictions
  pred <- predict.gbm(object = gbm.fit,
                      newdata = input.test,
                      n.trees = n,
                      type = "response")
  
  labels = colnames(pred)[apply(pred, 1, which.max)]
  
  return(labels)
}
