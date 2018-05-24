#Defining the cut point at 2/3 rd of the data set 
cutPoint <- floor(2 * dim(commonData)[1]/3)
cutPoint

#Sampling
randomIndices <- sample(1:dim(commonData)[1])


#Creating training data set  -  2/3 rd of the data set and the remaining as the test data
trainData <- commonData[randomIndices[1:cutPoint],]

#The remaining 1/3rd  part is the testing of data 
testData <- commonData[randomIndices[(cutPoint+1):dim(commonData)[1]],]

library(kernlab)
#Load the required packages
library(caret)

ksvmOutput_1 <- ksvm(LTR ~ ., data=trainData,  kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
ksvmOutput_1

ksvmOutput_2 <- ksvm(LTR ~ internet + f&b, data=trainData,  kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
ksvmOutput_2

ksvmOutput_3 <- ksvm(LTR ~ f&b+roomCond, data=trainData,  kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
ksvmOutput_3

ksvmOutput_4 <- ksvm(LTR ~ internet+roomCond, data=trainData,  kernel = "rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
ksvmOutput_4

#Testing the model on the test dataset
ksvmPred_1 <- predict(ksvmOutput_1, testData, type = "response")
ksvmPred_2 <- predict(ksvmOutput_2, testData, type = "response")
ksvmPred_3 <- predict(ksvmOutput_3, testData, type = "response")
ksvmPred_4 <- predict(ksvmOutput_4, testData, type = "response")

#Load the Metrics package for calculation of Root Mean Square
library("Metrics")

#Computation of root mean square using the fucntion rmse
rmse_1<-rmse(testData$LTR, ksvmPred_1)
rmse_2<-rmse(testData$LTR, ksvmPred_2)
rmse_3<-rmse(testData$LTR, ksvmPred_3)
rmse_4<-rmse(testData$LTR, ksvmPred_4)



#Building the lm model for the same train data
lmModel_1 <- lm(formula=LTR=~.,data=trainData)
lmModel_2 <- lm(formula=LTR=f&b+internet,data=trainData)
lmModel_3 <- lm(formula=LTR=~f&b+roomCond,data=trainData)
lmModel_4 <- lm(formula=LTR=~roomCond+internet,data=trainData)

#Predicting the values based on the svmModel
lmPred_1 <- predict(lmModel_1, testData, type="response")
lmPred_2 <- predict(lmModel_2, testData, type="response")
lmPred_3 <- predict(lmModel_3, testData, type="response")
lmPred_4 <- predict(lmModel_4, testData, type="response")


rmse_lm_1<-rmse(testData$LTR, lmPred_1)
rmse_lm_2<-rmse(testData$LTR, lmPred_2)
rmse_lm_3<-rmse(testData$LTR, lmPred_3)
rmse_lm_4<-rmse(testData$LTR, lmPred_4)

#Determining the parsimonous models
#Based on these we determine the best combination of variables to be used for our LTR value
step(ksvmOutput_1, data = inputData, direction = 'backward') 

step(lmModel_1, data = inputData, direction = 'backward') 


#Determining the confusion matrix for the predicted model
confMatrix<-confusionMatrix(goodtestData[,6],goodksvmPred[1,])
confMatrix
