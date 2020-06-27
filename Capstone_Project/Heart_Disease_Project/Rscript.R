## Battula Damodhar
## Heart Disease Prediction Project 
## HarvardX: PH125.9x - Capstone Project
## https://github.com/DamodharB/MyProject/tree/master/Capstone_Project

#Function needed to convert classes of predictor values.
convert_magic <- function(obj,types){
  for (i in 1:length(obj)){
    FUN <- switch(types[i],character = as.character, 
                  numeric = as.numeric, 
                  factor = as.factor)
    obj[,i] <- FUN(obj[,i])
  }
  obj
}


# Data pre-processing
process_data <- function(data_source){
  data <- read.table(data_source, sep = ',', na = '?', header = FALSE)
  colnames(data) <- c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","ca","thal","num")
  return(data)
}

cleveland_data <- process_data('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data')

dim(cleveland_data)
head(cleveland_data)
str(cleveland_data)
summary(cleveland_data)

# Data visualization

#Explore the data quickly, how many had heart attack, women or men, age?
  
#Values of num > 0 are cases of heart disease. Dummify some variables.

cleveland_data$num[cleveland_data$num > 0] <- 1
barplot(table(cleveland_data$num), main="Fate", col="brown")

#change a few predictor variables from integer to factors (make dummies).
chclass <-c("numeric","factor","factor","numeric","numeric","factor","factor","numeric","factor","numeric","factor","factor","factor","factor")

cleveland_data1 <- convert_magic(cleveland_data,chclass)

heart = cleveland_data1 #add labels only for plot
levels(heart$num) = c("No disease","Disease")
levels(heart$sex) = c("female","male","")
mosaicplot(heart$sex ~ heart$num,
           main="Fate by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease")


boxplot(heart$age ~ heart$num,
        main="Fate by Age",
        ylab="Age",xlab="Heart disease")


#We install the corrplot package to visualize our correlation matrix easily.
library(corrplot)

corr_matrix <- function(dataframe, t){
  df_clean <- na.omit(dataframe)
  cat("\n")
  corrplot(cor(df_clean), method = "circle", title = t)
}

corr_matrix(cleveland_data, "Cleveland correlation matrix visualisation")



# Methods and Analysis
## Data cleaning

s = sum(is.na(cleveland_data))
cleveland_data_clean <- na.omit(cleveland_data)

cat("CLEVELAND DATAFRAME\n")
summary(cleveland_data_clean) #Now the cleveland dataset is fully cleaned.


## Data Analysis
library(FactoMineR)

cleveland_dataframe = as.data.frame(cleveland_data_clean)
result = PCA(cleveland_dataframe[,1:14])

cleveland_dataframe$num <- as.factor(cleveland_dataframe$num)
cleveland_dataframe$sex <- as.factor(cleveland_dataframe$sex)
cleveland_dataframe$cp <- as.factor(cleveland_dataframe$cp)
cleveland_dataframe$restecg <- as.factor(cleveland_dataframe$restecg)
cleveland_dataframe$fbs <- as.factor(cleveland_dataframe$fbs)
cleveland_dataframe$thal <- as.factor(cleveland_dataframe$thal)
cleveland_dataframe$ca <- as.factor(cleveland_dataframe$ca)
cleveland_dataframe$slope <- as.factor(cleveland_dataframe$slope)

create_level <- function(city_categorical_attributes,categorical_attribute,number_of_category){
  i=0
  for (i in number_of_category){
    levels(city_categorical_attributes$categorical_attribute)[levels(city_categorical_attributes$categorical_attribute)==i]
  }
}

create_level(cleveland_dataframe,sex,1)
create_level(cleveland_dataframe,fbs,1)
create_level(cleveland_dataframe,thal,3)
create_level(cleveland_dataframe,num,4)
create_level(cleveland_dataframe,exang,1)
create_level(cleveland_dataframe,cp,3)
create_level(cleveland_dataframe,restecg,2)
create_level(cleveland_dataframe,slope,2)


#Once each categorical value has been converted into vector, we can plot each attribute:
library(ggplot2)
library(ggthemes)

plot_histogram <- function(city_dataframe, continuous_attribute,title_plot){
  ggplot(data = city_dataframe,mapping = aes(x = continuous_attribute, fill=num)) + geom_histogram(aes(y=..density..), color="grey17")+
    facet_wrap(~num, ncol=1,scale="fixed") + ggtitle(title_plot)
}
plot_bar <- function(city_dataframe, categorical_attribute,title_plot){
  ggplot(data = city_dataframe, mapping = aes(x = categorical_attribute)) + geom_bar() + facet_wrap(~num) + ggtitle(title_plot)
}

plot_histogram(cleveland_dataframe,cleveland_dataframe$trestbps,"trestbps")
plot_histogram(cleveland_dataframe,cleveland_dataframe$chol,"chol")
plot_histogram(cleveland_dataframe,cleveland_dataframe$thalach,"thalach")
plot_histogram(cleveland_dataframe,cleveland_dataframe$oldpeak,"oldpeak")
plot_histogram(cleveland_dataframe,cleveland_dataframe$age,"age")
plot_bar(cleveland_dataframe,cleveland_dataframe$sex,"sex")
plot_bar(cleveland_dataframe,cleveland_dataframe$ca,"ca")
plot_bar(cleveland_dataframe,cleveland_dataframe$cp,"cp")
plot_bar(cleveland_dataframe,cleveland_dataframe$exang,"exang")
plot_bar(cleveland_dataframe,cleveland_dataframe$restecg,"restecg")
plot_bar(cleveland_dataframe,cleveland_dataframe$fbs,"fbs")
plot_bar(cleveland_dataframe,cleveland_dataframe$slope,"slope")
plot_bar(cleveland_dataframe,cleveland_dataframe$thal,"thal")



## Modelling Approach

### Training and Testing data for validation
library(caret)
set.seed(10, sample.kind="Rounding")
cleveland_data_clean <- convert_magic(cleveland_data_clean,chclass)
inTrainRows <- createDataPartition(cleveland_data_clean$num,p=0.7,list=FALSE)
trainData <- cleveland_data_clean[inTrainRows,]
testData <-  cleveland_data_clean[-inTrainRows,]
nrow(trainData)/(nrow(testData)+nrow(trainData)) #checking whether really 70% -> OK


### Predict with different methods with different tuning parameters and compare best model of each method
AUC = list()
Accuracy = list()

#### Logistic regression
set.seed(10, sample.kind="Rounding")
logRegModel <- train(num ~ ., data=trainData, method = 'glm', family = 'binomial')
logRegPrediction <- predict(logRegModel, testData)
logRegPredictionprob <- predict(logRegModel, testData, type='prob')[2]
logRegConfMat <- confusionMatrix(logRegPrediction, testData[,"num"])

#ROC Curve
library(pROC)
AUC$logReg <- roc(as.numeric(testData$num),as.numeric(as.matrix((logRegPredictionprob))))$auc
Accuracy$logReg <- logRegConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 1, ncol = 2,
                           dimnames = list(row.names, col.names))))


#### Random Forest model without tuning (but checked a few number of trees)
library(randomForest)
set.seed(10, sample.kind="Rounding")
RFModel <- randomForest(num ~ .,
                        data=trainData,
                        importance=TRUE,
                        ntree=2000)
#varImpPlot(RFModel)
RFPrediction <- predict(RFModel, testData)
RFPredictionprob = predict(RFModel,testData,type="prob")[, 2]

RFConfMat <- confusionMatrix(RFPrediction, testData[,"num"])

AUC$RF <- roc(as.numeric(testData$num),as.numeric(as.matrix((RFPredictionprob))))$auc
Accuracy$RF <- RFConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 2, ncol = 2,
                           dimnames = list(row.names, col.names))))


#### Boosted tree model with tuning (grid search)
#Boosted tree model (gbm) with adjusting learning rate and and trees.

library(caret)
library(gbm)
set.seed(10, sample.kind="Rounding")
objControl <- trainControl(method='cv', number=10)
gbmGrid <-  expand.grid(interaction.depth =  c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode =10)
# run model
boostModel <- train(num ~ .,data=trainData, method='gbm',
                    trControl=objControl, tuneGrid = gbmGrid, verbose=F)

# See model output in Appendix to get an idea how it selects best model
boostPrediction <- predict(boostModel, testData)
boostPredictionprob <- predict(boostModel, testData, type='prob')[2]
boostConfMat <- confusionMatrix(boostPrediction, testData[,"num"])

#ROC Curve
AUC$boost <- roc(as.numeric(testData$num),as.numeric(as.matrix((boostPredictionprob))))$auc
Accuracy$boost <- boostConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 3, ncol = 2,
                           dimnames = list(row.names, col.names))))


# Results 
## Comparison of AUC and Accuracy between models
row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 3, ncol = 2,
                           dimnames = list(row.names, col.names))))


## Interpretation of logistic regression model and importance of variables from boosted tree
summary(logRegModel)$coeff

boostImp =varImp(boostModel, scale = FALSE)
plot(boostImp,main = 'Variable importance for heart failure prediction with boosted tree')



# Appendix
## Confusion matrix output

### Logistic Regression:
logRegConfMat

### Random Forest:
RFConfMat

### Boosted Tree:
boostConfMat

## Example of Model output for selection of tuning parameters
boostModel


## Environment
print("Operating System:")

version
