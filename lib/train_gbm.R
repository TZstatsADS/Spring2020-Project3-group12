###########################################################
### Train a classification model with training features ###
###########################################################
library(gbm)

train_gbm <- function(train.df, s, K = 5, n){
  ### Train a GBM model with K fold cross validation and n number of tress
  
  ### Input:
  ### - a data frame containing features and labels
  ### - number of cross-validation
  ### - number of trees
  ### Output: trained model
  
  gbm.fit<- gbm(label~. ,data = train.df,
                distribution = "multinomial", 
                n.trees = n,
                shrinkage = s,
                n.minobsinnode = 10, 
                cv.folds = K)
  
  return(gbm.fit)
}
