train = as.matrix(read.table("train2_5.txt", quote="\"", comment.char=""))
train_labels = as.matrix(read.table("train2_5Labels.txt", quote="\"", comment.char=""))
test = as.matrix(read.table("test2_5.txt", quote="\"", comment.char=""))
test_labels = as.matrix(read.table("test2_5Labels.txt", quote="\"", comment.char=""))



trainGradientDescent <- function(X,Y,learningRate,momentum){
X<-cbind(X,c(1))
W<-matrix(data = runif(257,min = 0,max = 0.1), nrow = 1, ncol = 257, byrow = TRUE,
dimnames = NULL)
deltaOld<-matrix(data = 0, nrow = 1, ncol = 257, byrow = TRUE,
dimnames = NULL)
for (i in 1:100){
for (k in 1:nrow(X)){
Yhat<-X%*%t(W)
YDiff<-Y-Yhat
delta<-learningRate*(YDiff[k,]) * X[k,]
delta<-momentum*(deltaOld) + (delta)
W<-W+delta
deltaOld = delta
}
}
return (W)
}

testGradientDescent <- function (testX,regressionCoefficients){
  testX<-cbind(testX,c(1))
  return(testX%*%t(regressionCoefficients))
}

Matrix <- function(prediction,actualLabels){
fiveClass = prediction > 3.5			#this will give you all indices of class = 1
twoClass = prediction < 3.5			#this will give you all indices of class = 0
prediction[fiveClass,] = 5
prediction[twoClass,] = 2
TP = (prediction == 2 & actualLabels == 2)
FN = (prediction == 5 & actualLabels == 2)
TN = (prediction == 5 & actualLabels == 5)
FP = (prediction == 2 & actualLabels == 5)
count_tp = length(actualLabels[TP,])
count_fn = length(actualLabels[FN,])
count_tn = length(actualLabels[TN,])
count_fp = length(actualLabels[FP,])
totalPositiveLabels = length(actualLabels[actualLabels==2])
totalNegativeLabels = length(actualLabels[actualLabels==5])
BalancedAccuracyRate = (count_tp/totalPositiveLabels+count_tn/totalNegativeLabels)/2
cat("TP",count_tp,"\n","FN",count_fn,"\n","TN",count_tn,"\n","FP",count_fp,"\n")
return(BalancedAccuracyRate)
}
Matrix(testGradientDescent(train,trainGradientDescent(train,train_labels,0.001,0)),train_labels)
Matrix(testGradientDescent(test,trainGradientDescent(train,train_labels,0.001,0)),test_labels)
Matrix(testGradientDescent(train,trainGradientDescent(train,train_labels,0.7,0.9)),train_labels)
Matrix(testGradientDescent(test,trainGradientDescent(train,train_labels,0.1,0.5)),test_labels)

Matrix(testGradientDescent(train,trainGradientDescent(train,train_labels,0.0001,0.1)),train_labels)
Matrix(testGradientDescent(test,trainGradientDescent(train,train_labels,0.0001,0.1)),test_labels)
Matrix(testGradientDescent(train,trainGradientDescent(train,train_labels,0.0001,0.001)),train_labels)
Matrix(testGradientDescent(test,trainGradientDescent(train,train_labels,0.0001,0.001)),test_labels)


