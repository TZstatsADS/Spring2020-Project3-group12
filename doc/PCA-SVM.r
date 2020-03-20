setwd("D:/¸çÂ×±ÈÑÇ/GR5243/HW3")
library(R.matlab)
library(dplyr)
library(tidyr)
library(neuralnet)
library(e1071)

#_____________________________________________________#
load("train_euc.RData")

set.seed(5243)
sample<-sample(1:2500,250,replace = F)
SAMPLE<-sample(1:2250,250,replace = F)
data_train<-train_set_euc[-sample,]
data_validation<-data_train[SAMPLE,]
data_train<-data_train[-SAMPLE,]
data_test<-train_set_euc[sample,]

#__________Directly build SVM model____________________#
traindata<-rbind(data_train,data_validation)
testdata<-data_test
svm<-svm(traindata[,-3004],y = as.factor(traindata[,3004]))
p<-predict(svm,newdata = testdata[,-3004])
MSE<-mean(p != as.factor(testdata[,3004]))

#___________PCA&SVM(TRAIN,VALIDATION & TEST SET)_______________________#
train_pca<-prcomp(data_train[,-3004])
sdev<-train_pca$sdev
sum(sdev[1:500]^2)/sum(sdev^2)
validation_pca<-scale(data_validation[,-3004], train_pca$center, train_pca$scale) %*% train_pca$rotation
svm_list<-list()
MSE_validation<-c()
for(i in 1:500){
  svm<-svm(train_pca$x[,1:i],y = as.factor(data_train[,3004]))
  svm_list[[i]]<-svm
  mean(svm$fitted != as.factor(data_train[,3004]))
  p<-predict(svm,newdata = validation_pca[,1:i])
  MSE_validation[i]<-mean(p != as.factor(data_validation[,3004]))
}
plot(x = 1:500,MSE_validation)
which.min(MSE_validation)
#I've just got svm model using the first 500 PC because of limited RAM(TAT)#

#_________select first 57th PC to build the SVM model_________#
model_svm<-svm_list[[57]]

mypredict<-function(data){
  test_pca<-scale(data, train_pca$center, train_pca$scale) %*% train_pca$rotation
  p<-predict(model_svm,newdata = test_pca[,1:57])
  return(p)
}
p<-mypredict(data_test[,-3004])
mean(p != as.factor(data_test[,3004]))

save(train_pca,model_svm,mypredict,file = "model_pca+svm.RData")
save(data_test,file = "test_data.RData")
