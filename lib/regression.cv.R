regression.cv <- function(models, data, K=5) {
  
  train_scores <- c()
  test_scores <- c()
  
  for(i in 1:K) {
    
    sample <- sample.int(nrow(data), size = floor(0.8 * nrow(data)), replace = F)
    train_data <- data[sample, ]
    validation_data <- data[-sample, ]

    x_train <- as.matrix(train_data[, -ncol(train_data)])
    x_validation <- as.matrix(validation_data[, -ncol(validation_data)])
    
    y_train <- as.factor(train_data[, ncol(train_data)])
    y_validation <- as.factor(validation_data[, ncol(validation_data)])
    
    train_pred <- as.matrix(predict(models, s = 'lambda.min', newx= x_train, type = 'class'))
    test_pred <- as.matrix(predict(models, s = 'lambda.min', newx= x_validation, type = 'class'))
    
    train_scores[i] <- mean(y_train == train_pred)
    test_scores[i] <- mean(y_validation == test_pred)
  }
  return(c(mean(train_scores), mean(test_scores)))
}