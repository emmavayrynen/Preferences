##caret model: Preferences, C5.0 Decision tree
#https://github.com/emmavayrynen/Preferences.git
#model training: c5.0 
#model measurement:
#dataframe: CompleteResponses
#Y-value:brand


#load library, get data and set seed
library(caret)
library(C50)
library(ggplot2)
CompleteResponses <- read.csv("~/Dota/Preferences/Survey/CompleteResponses.csv")
set.seed(998)

#change data type
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
CompleteResponses$brand <- factor(CompleteResponses$brand,
                                  levels = c(0,1), labels = c("Acer","Sony"))
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)


# calculate correlation matrix - numeriacal values only
correlationMatrix <- cor(CompleteResponses[c(1, 2, 6)])

# summarize the correlation matrix
print(correlationMatrix)

#Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
salary_norm<-normalize(CompleteResponses$salary)
credit_norm<-normalize(CompleteResponses$credit)
age_norm<-normalize(CompleteResponses$age)

newdata<-c("salary_norm", "age_norm", "CompleteResponses$brand")

# define an 75%/25% train/test split of the dataset
inTraining<-caret::createDataPartition(CompleteResponses$brand, p = .75, list = FALSE)
trainingC5 <-CompleteResponses[inTraining,]
testingC5 <- CompleteResponses[-inTraining,]

#Cross Validation Fold 10 times
fitControlC5 <- caret::trainControl(method = "repeatedcv", number = 10, repeats = 1)


#C5.0 model
C5Model <- C50::C5.0(brand ~ ., data = trainingC5, rules = TRUE, tunelenght=2)

summary(C5Model)
print(C5Model)
C5Model

predictC5<-predict(C5Model, newdata = testingC5)
summary(predictC5)


#confusion matrix
caret::confusionMatrix(testingC5$brand,predictC5)

#predictions and errors
predict(C5Model)
postResample(predictC5,testingC5$brand)

varImp(CompleteResponses$brand)

#Import of new data and use model on new data
Withoutbrand<- read.csv("~/Dota/Preferences/Survey/SurveyIncomplete.csv")

Withoutbrand$car<-as.factor(Withoutbrand$car)
Withoutbrand$zipcode<-as.factor(Withoutbrand$zipcode)
Withoutbrand$elevel<-as.factor(Withoutbrand$elevel)


C5Model(Withoutbrand)
PredictNewdata<-predict(C5Model,newdata=Withoutbrand)
summary(PredictNewdata)
PredictNewdata

#Data put together (survey incomplete and predicted preferences)
CompletePreference<- read.csv("~/Dota/Preferences/Predicted preference.csv")

#Fråga om hur detta ligger till
postResample(PredictNewdata,testingC5$brand)

##### Graphs for report #####

## Graphs of complete data ##

#Relation between salary, age and brand
ggplot(CompleteResponses, aes(x=age, y=salary, col=brand)) + geom_point()+ theme_light()

#Brand preference
ggplot(CompleteResponses, aes(x=brand, fill=brand)) + geom_bar()+theme_gray()+ labs(title = "Brand preferences")

#Salary
ggplot(CompleteResponses, aes(x=salary)) + geom_histogram(color="black", fill="yellow", bins=25)+ labs(title= "Distribution of salary")

## Graphs of new data ##
Withoutbrand$predicted <- PredictNewdata
Withoutbrand$predicted <- as.factor(Withoutbrand$predicted)
                                  levels = c(0,1), labels = c("Acer","Sony"))

#Relation between salary, age and brand
ggplot(Withoutbrand, aes(x=age, y=salary, col=predicted)) + geom_point()+ theme_light()

#Brand preference
ggplot(Withoutbrand, aes(x=predicted, fill=predicted)) + geom_bar()+theme_gray()+ labs(title = "Predicted brand preferences")

