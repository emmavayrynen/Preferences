#caret model: Preferences, Random forrest
#https://github.com/emmavayrynen/Preferences.git
#model training: Random forrest
#model measurement:tuneLenght = 1 (trains with 1 mtry value for RandomForest)
#dataframe: CompleteResponses
#Y-value: brand

#load library, get data and set seed
library(caret)
library(randomForest)
library(mlbench)
CompleteResponses <- read.csv("~/Dota/Preferences/Survey/CompleteResponses.csv")
set.seed(998)

#change data type
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
CompleteResponses$brand <- factor(CompleteResponses$brand,
                                  levels = c(0,1), labels = c("Acer","Sony"))
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)

# calculate correlation matrix OBS bara exempel - numerical values only
correlationMatrix <- cor(CompleteResponses[,c(1,2,6)])

# summarize the correlation matrix
print(correlationMatrix)

#Normalize 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
salary_norm<-normalize(CompleteResponses$salary)
credit_norm<-normalize(CompleteResponses$credit)
age_norm<-normalize(CompleteResponses$age)
  
# define an 75%/25% train/test split of the dataset
  inTraining<-createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
  training <- CompleteResponses[inTraining,]
  testing <- CompleteResponses[-inTraining,]
  
  
#Cross Validation Fold 10 times
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(29,30))


#Train Random Forest Regression model with tunegrid
rfFit1 <- train(brand ~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid, importance=TRUE)


#Training results
rfFit1   
print(rfFit1)
summary(rfFit1)

#confusion matrix
ConfusionMatrixrFit <- confusionMatrix(rfFit1)


#predictions 
PredicitionsrfFit1 <- predict(rfFit1,testing)
summary(PredicitionsrfFit1)

PredicitionsrfFit1

postResample(PredicitionsrfFit1,testing$brand)

