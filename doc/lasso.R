

# lasso

library(glmnet)
x = model.matrix(emotion_idx~., data = dat_train)
y = factor(dat_train$emotion_idx)
lassoModel = glmnet(x,y,alpha =1,family = "multinomial")

# cross validation
set.seed(1031)
cv.lasso = cv.glmnet(x,y,alpha = 1,family = "multinomial")
system.time(cv.glmnet(x,y,alpha = 1,family = "multinomial"))   # 2926.84 secs = 48.78 min
coef(cv.lasso)  ## Print out coefficients at optimal lambda
time<-system.time(lassoModel = glmnet(x,y,alpha =1))

# fit test set 
fit_test <- predict(cv.lasso, newx=newx)

accu1 <- mean(dat_test$emotion_idx == fit_test)               # 52%
accu1

tm_test1 <- system.time(pred <- test(model_best, dat_test))   # 12.70 secs







