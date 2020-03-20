###########################################################
### Train a classification model with training features ###
###########################################################
regression.train <- function(feature_df, alpha, K = 5){
  library(glmnet)
  ### Train a multinomial lasso or ridge model with K fold cross validation, using processed features from training images
  
  ### Input:
  ### - a data frame containing features and labels
  ### - a parameter list
  ### Output: trained model

  X_train <- as.matrix(feature_df[, -ncol(feature_df)])
  y_train <- as.factor(feature_df[, ncol(feature_df)])
  
  cv.model <- cv.glmnet(x = X_train, 
                     y = y_train, 
                     alpha = alpha,
                     family = 'multinomial',
                     type.measure = 'class',
                     nfolds = K
                     )
  
  return(cv.model)
}