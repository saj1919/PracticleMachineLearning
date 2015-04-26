### Import libraries

library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
library(survival)
library(splines)
library(parallel)
library(plyr)
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)

### Set working directory
setwd('/home/saj1919/MyData/R Study/Practicle Machine Learning Course/project')
### Check if files are in place
dir()

### Load train-test data
training <- read.csv("pml-training.csv", row.names = 1)
testing <- read.csv("pml-testing.csv", row.names = 1)

### In later steps, I got many errors during Random-forest run saying test and train data predictors do not match.
### So whatever transformation I am doing on train should also happen. So merging them.

### Removing last column of test data
drops <- c("problem_id")
testing <- testing[,!(names(testing) %in% drops)]

### Adding dummy last column named 'classe' with default value 'A'.
testing$classe <- "A"

### Merging test and train data
training <- rbind(training, testing)

### Remove near zero covariates
### Do not know if I am doing correct thing here 
### Found this online for removing DIV/0 and very small values
nsv <- nearZeroVar(training, saveMetrics = T)
training <- training[, !nsv$nzv]

### Data have many rows with less information. 
### So keeping only data which have 1/4rth or more columns filled
nav <- sapply(colnames(training), function(x) if(sum(is.na(training[, x])) > 0.75*nrow(training)){return(T)}else{return(F)})
training <- training[, !nav]

### Now splitting test and train data to get orginal datasets
testing <- training[19623:19642,]
training <- training[1:19622,]


### Dividing data in 2/3rd training and 1/3rd testing
inTrain <- createDataPartition(y=training$classe, p=0.67, list=FALSE)
myTraining <- training[inTrain, ] 
myTesting <- training[-inTrain, ]

### Week 3 videos lectures mostly talked about tree models
### So here exploring with tree classifiers

### First using rpart
set.seed(1919)
fit <- rpart(classe ~ ., data=myTraining, method="class")

### Plot the decision tree
fancyRpartPlot(fit)

### Display the fit 
printcp(fit)

### Plot cross-validation over fit
plotcp(fit)

### Predict and check accuracy
predictions <- predict(fit, myTesting, type = "class")
confusionMatrix(predictions, myTesting$classe)


### Didn't cross 90% accuracy
### Now try with Random-Forest
set.seed(1919)
fit <- randomForest(classe ~. , data=myTraining)

### Plot error with cross validation 
plot(fit)

###Predict and check accuracy
predictions <- predict(fit, myTesting, type = "class")
confusionMatrix(predictions, myTesting$classe)

### Nice !!! Got > 99% accuracy
### Create model over all training data

set.seed(1919)
fit <- randomForest(classe ~. , data=training)
predictions <- predict(fit, testing, type = "class")
predictions
### Generate Files for assignment generation
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictions)
