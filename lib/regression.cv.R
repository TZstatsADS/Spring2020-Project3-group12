regression.cv <- function(models, data, K=5, alpha=1) {
    
  X <- data[, -ncol(data)]
  y <- data[, ncol(data)]

  ctrl <- trainControl(method="cv", number=K)
  cvv <- train(X, y, method = "glmnet", 
                    trControl = ctrl,
                    tuneGrid = expand.grid(alpha = alpha, 
                                           lambda = models$lambda.min))

  return(cvv$results$Accuracy)
}