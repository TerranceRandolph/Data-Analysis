
# --- HW9 --- #
# professor code tweaked a little 

# 1
pkgs <- c("kernlab", "e1071", "gridExtra", "ggplot2", "caret","bindr")
#install.packages("bindr")
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(pkgs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
package.check
# load packages
library(ggplot2);library(kernlab);library(e1071);
library(gridExtra);library(caret);library(bindr)

# load airquility data
data("airquality")
airQ <- airquality

# replace NA with mean, online code
airQ[sapply(airQ, is.numeric)] <- 
  lapply(airQ[sapply(airQ, is.numeric)], 
         function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
# check if NA remaining
any(is.na(airQ))

# --- Step 2 --- #
# get the dimensions
dim(airQ)
airQ[1:5,]
# store a sample of indexs from airQ dataset
randIndex <- sample(1:dim(airQ)[1])
head(randIndex)
length(randIndex)
airQ[148,]
airQ[45,]
# create a 2/3rd split of indexes
cutpoint2_3 <- floor(2*dim(airQ)[1]/3)
# check the 2/3 cutpoint
cutpoint2_3
######
# create train data set, which contains the first 2/3 of overall data
trainData <- airQ[randIndex[1:cutpoint2_3],]
dim(trainData)
head(trainData)
######
# create test data, which contains the left 1/3 of the overall data
testData <- airQ[randIndex[(cutpoint2_3+1):dim(airQ)[1]],]
# check the test dataset
dim(testData)   
head(trainData)
# --- Linear Model --- #
model <- lm(Ozone ~.,data=trainData)
lmPred <- predict(model,testData)
str(lmPred)
# data frame of testing data against predicted model
compTable3 <- data.frame(testData[,1],lmPred)
colnames(compTable3) <- c("test","Pred")
# advg variability between the two columns
sqrt(mean((compTable3$test-compTable3$Pred)^2))

#lm plot
# build a column for the error  between the two columns
compTable3$error <- abs(compTable3$test - compTable3$Pred)
svmPlot3 <- data.frame(compTable3$error, testData$Temp, testData$Wind)
colnames(svmPlot3) <- c("error","Temp","Wind")
ggplot(svmPlot3, aes(x=Temp,y=Wind)) + 
  geom_point(aes(size=error, color=error)) + ggtitle("lm")

lm.plot <- ggplot(svmPlot3, aes(x=Temp,y=Wind)) + 
              geom_point(aes(size=error, color=error)) + ggtitle("lm")
lm.plot

# --- Step 3 --- #
# Step 3: Build a Model using KSVM & visualize the results
# 1) Build a model to predict Ozone and name it "svmOutput"
#    This is the Training step
#  
svmOutput <- ksvm(Ozone~., # set "Ozone" as the target predicting variable; "." means use all other variables to predict "Ozone"
                  data = trainData, # specify the data to use in the analysis
                  kernel = "rbfdot", # kernel function that projects the low dimensional problem into higher dimensional space
                  kpar = "automatic",# kpar refer to parameters that can be used to control the radial function kernel(rbfdot)
                  C = 10, # C refers to "Cost of Constrains"
                  cross = 10, # use 10 fold cross validation in this model
                  prob.model = TRUE # use probability model in this model
)
# check the model
svmOutput

# 2) Test the model with the testData data set
svmPred <- predict(svmOutput, # use the built model "svmOutput" to predict 
                   testData, # use testData to generate predictions
                   type = "votes" # request "votes" from the prediction process
)
str(svmPred)

# create a comparison dataframe that contains the exact "Ozone" value and the predicted "Ozone" value
# use for RMSE calc 
compTable <- data.frame(testData[,1], svmPred[,1])
# change the column names to "test" and "Pred"
colnames(compTable) <- c("test","Pred")
#  
# compute the Root Mean Squared Error
#  
sqrt(mean((compTable$test-compTable$Pred)^2)) #A smaller value indicates better model performance.

# 3) Plot the results
#    library(ggplot2)

# compute absolute error for each case
compTable$error <- abs(compTable$test - compTable$Pred)
# create a new dataframe contains error, tempreture and wind
svmPlot <- data.frame(compTable$error, testData$Temp, testData$Wind, testData$Ozone)
# assign column names
colnames(svmPlot) <- c("error","Temp","Wind", "Ozone")
# polt result using ggplot, setting "Temp" as x-axis and "Wind" as y-axis
plot.ksvm <- ggplot(svmPlot, aes(x=Temp,y=Wind)) + 
              # use point size and color shade to illustrate how big is the error
              geom_point(aes(size=error, color=error))+
              ggtitle("ksvm")

plot.ksvm



# --- Step 4 --- #

# Create a "goodOzone" variable
# calculate average Ozone
meanOzone <- mean(airQ$Ozone,na.rm=TRUE)
# create a new variable named "goodOzone" in train data set
# goodOzone = 0 if Ozone is below average Ozone
# googOzone = 1 if Ozone is eaqual or above the average ozone
trainData$goodOzone <- ifelse(trainData$Ozone<meanOzone, 0, 1)
# do the same thing for test dataset
testData$goodOzone <- ifelse(testData$Ozone<meanOzone, 0, 1)
# remove "Ozone" from train data
trainData <- trainData[,-1]
# remove "Ozone" from test data
testData <- testData[,-1]


# --- Step 5 --- #

# See if we can do a better job predicting 'good' and 'bad' days
# convert "goodOzone" in train data from numeric to factor
trainData$goodOzone <- as.factor(trainData$goodOzone)
# convert "goodOzone" in test data from numeric to factor
testData$goodOzone <- as.factor(testData$goodOzone)

# 1)	Build a model 
# build a model using ksvm function,and use all other variables to predict
svmGood <- ksvm(goodOzone~., # set "Ozone" as target variable; "." means use all other variables to predict "Ozone"
                data=trainData, # specify the data to use in the analysis
                kernel="rbfdot", # kernel function that projects the low dimensional problem into higher dimensional space
                kpar="automatic",# kpar refer to parameters that can be used to control the radial function kernel(rbfdot)
                C=10, # C refers to "Cost of Constrains"
                cross=10, # use 10 fold cross validation in this model
                prob.model=TRUE # use probability model in this model
)
# check the model
svmGood

# 2) Test the model
goodPred <- predict(svmGood, # use model "svmGood" to predict
                    testData # use testData to do the test
)
# create a dataframe that contains the exact "goodOzone" value and the predicted "goodOzone"
compGood1 <- data.frame(testData[,6], goodPred)
# change column names
colnames(compGood1) <- c("test","Pred")
# Compute the percentage of correct cases
perc_ksvm <- length(which(compGood1$test==compGood1$Pred))/dim(compGood1)[1]
perc_ksvm

# Confusion Matrix
#   
results <- table(test=compGood1$test, pred=compGood1$Pred)
print(results)

#       pred
#  test  0  1
#     0 19  7      #  read horizontal,, 0 class, 19 identified correctly, 7 incorrectly
#     1  7 18      #                    1 class, 7 identified incorrectly, 18 correctly




# 3)	Plot the results. 
# determine the prediction is "correct" or "wrong" for each case
compGood1$correct <- ifelse(compGood1$test==compGood1$Pred,"correct","wrong")
# create a new dataframe contains correct, tempreture and wind, and goodZone
Plot_ksvm <- data.frame(compGood1$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood1$Pred)
# change column names
colnames(Plot_ksvm) <- c("correct","Temp","Wind","goodOzone","Predict")
# polt result using ggplot
# size representing correct/wrong; color representing actual good/bad day; shape representing predicted good/bad day.
plot.ksvm.good <- ggplot(Plot_ksvm, aes(x=Temp,y=Wind)) + 
                  geom_point(aes(size=correct,color=goodOzone,shape = Predict))+
                  ggtitle("ksvm - good/bad ozone")

plot.ksvm.good

# --- SVM Model --- #
svmGood2 <- svm(goodOzone~.,data=trainData,kernel="radial",
                C=10,cross=10,prob.model=TRUE)
svmGood2

#test the svm model
goodPred2 <- predict(svmGood2,testData)
compGood2 <- data.frame(testData[,6],goodPred2)
colnames(compGood2) <- c("test","Pred")
perc_svm <- length(which(compGood2$test==compGood2$Pred))/dim(compGood2)[1]
perc_svm
#confusion matrix
results2 <- table(test=compGood2$test,pred=compGood2$Pred)
print(results2)

#plot results of svm model
compGood2$correct <- ifelse(compGood2$test==compGood2$Pred,"correct","wrong")
plot.svm <- data.frame(compGood2$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood2$Pred)
colnames(plot.svm) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.svm.good <- ggplot(plot.svm,aes(x=Temp,y=Wind)) + 
                  geom_point(aes(size=correct,color=goodOzone,shape=Predict)) + 
                  ggtitle("svm - good/bad ozone")
plot.svm.good

#buid a naive bayes function
svmGood3 <- naiveBayes(goodOzone~.,data=trainData)
svmGood3

#test the naive bayes function
goodPred3 <- predict(svmGood3,testData)
compGood3 <- data.frame(testData[,6],goodPred3)
colnames(compGood3) <- c("test","Pred")
perc_nb <- length(which(compGood3$test==compGood3$Pred))/dim(compGood3)[1]
perc_nb
#confusion matrix
results3 <- table(test=compGood3$test,pred=compGood3$Pred)
print(results3)

#plot results of naive bayes function
compGood3$correct <- ifelse(compGood3$test==compGood3$Pred,"correct","wrong")
plot.nb <- data.frame(compGood3$correct,testData$Temp,testData$Wind,testData$goodOzone,compGood3$Pred)
colnames(plot.nb) <- c("correct","Temp","Wind","goodOzone","Predict")
plot.nb.good <- ggplot(plot.nb,aes(x=Temp,y=Wind)) + 
                geom_point(aes(size=correct,color=goodOzone,shape=Predict)) + 
                ggtitle("naive bayesian - good/bad ozone")
plot.nb.good

#5. Show all three results in one window using gridArrange function
grid.arrange(plot.ksvm.good,plot.svm.good,plot.nb.good, 
             ncol=2, nrow=2, top="Model Comparison")

##################################################
# Step 6: Which are the best models for this data?
##################################################
#Using "real Ozone" value, the RMSE for
#ksvm was: 25.89787
#svm was: 25.90284
#lm was: 24.29834
#Based on these results, all three had very similar RMSE values, but the linear model had
#a slightly lower value, making it better.

#Using "goodOzone" value, the percent correct for
#ksvm was: 0.8235294
#ksvm confusion matrix:
#         pred
#     test  0  1
#        0 16  5
#        1  4 26

#svm was: 0.8039216
#svm confusion matrix:
#         pred
#     test  0  1
#        0 15  6
#        1  4 26

#naive bayesion was: 0.8039216
#nb confusion matrix: 
#         pred
#     test  0  1
#        0 14  7
#        1  3 27

#based on classification, the ksvm model had the highest percentage correct, although all
#three models were similar. The ksvm model also had the fewest classification errors. 


