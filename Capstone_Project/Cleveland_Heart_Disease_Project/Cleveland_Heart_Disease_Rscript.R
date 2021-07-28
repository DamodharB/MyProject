# Dataset downloading and preperation
## Used Libraries
#Installing required packages:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

## Used Dataset

# Preprocessed Heart disease dataset
# https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data

# The description of the dataset can be found
# https://archive.ics.uci.edu/ml/datasets/Heart+Disease

#Reading data in CSV format as dataframe
heart_disease_dataset <- read.csv(file = 
                                    'https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data',
                                  sep = ',', na = '?', header = F)

#Prepare column names
column_names <- c("Age",
                  "Sex",
                  "Chest_Pain_Type",
                  "Resting_Blood_Pressure",
                  "Serum_Cholesterol",
                  "Fasting_Blood_Sugar",
                  "Resting_ECG",
                  "Max_Heart_Rate_Achieved",
                  "Exercise_Induced_Angina",
                  "ST_Depression_Exercise",
                  "Peak_Exercise_ST_Segment",
                  "Num_Major_Vessels_Flouro",
                  "Thalassemia",
                  "Diagnosis_Heart_Disease")

#Apply column names to the dataframe
colnames(heart_disease_dataset) <- column_names


# Methods and Analysis
## Data visualization

#To get familiar with data, let's have a look on general overview of dataset.

# Glimpse data
heart_disease_dataset %>% glimpse()

# heading few records of data
heart_disease_dataset %>% head()

# Summary of data
heart_disease_dataset %>% summary()

# Missing values
sum(is.na(heart_disease_dataset))


# some visualizations that provide some insights into the data.
####################################################
# Distinct values
####################################################

heart_disease_dataset %>% summarise(n_age = n_distinct(Age), 
                                    n_sex = n_distinct(Sex),
                                    n_cp = n_distinct(Chest_Pain_Type),
                                    n_trestbps = n_distinct(Resting_Blood_Pressure),
                                    n_chol = n_distinct(Serum_Cholesterol),
                                    n_fbs = n_distinct(Fasting_Blood_Sugar),
                                    n_restecg = n_distinct(Resting_ECG),
                                    n_thalach = n_distinct(Max_Heart_Rate_Achieved),
                                    n_exang = n_distinct(Exercise_Induced_Angina),
                                    n_oldpeak = n_distinct(ST_Depression_Exercise),
                                    n_slope = n_distinct(Peak_Exercise_ST_Segment),
                                    n_ca = n_distinct(Num_Major_Vessels_Flouro),
                                    n_thal = n_distinct(Thalassemia),
                                    n_condition = n_distinct(Diagnosis_Heart_Disease))

#Identify the count of Sex:
heart_disease_dataset %>% 
  group_by(Sex) %>%
  count() %>% 
  knitr::kable()

#Identify the different levels of Heart Disease:
heart_disease_dataset %>% 
  drop_na() %>%
  group_by(Diagnosis_Heart_Disease) %>%
  count() %>% 
  knitr::kable()

#Identify the different levels of Thalassemia:
heart_disease_dataset %>% 
  drop_na() %>%
  group_by(Thalassemia) %>%
  count() %>% 
  knitr::kable()


## Data Pre-processing
#Drop NA's, convert to factors, lump target variable to 2 levels, remove "?", reorder variables
library(tidyverse)
heart_dataset_clean_tbl <- heart_disease_dataset %>% 
  drop_na() %>%
  mutate_at(c("Resting_ECG", 
              "Fasting_Blood_Sugar", 
              "Sex", 
              "Diagnosis_Heart_Disease", 
              "Exercise_Induced_Angina",
              "Peak_Exercise_ST_Segment", 
              "Chest_Pain_Type",
              "Thalassemia"), as_factor) %>%
  mutate(Num_Major_Vessels_Flouro = as.numeric(Num_Major_Vessels_Flouro)) %>%
  mutate(Diagnosis_Heart_Disease = fct_lump(Diagnosis_Heart_Disease, other_level = "1")) %>% 
  filter(Thalassemia != "na") %>%
  select(Age, 
         Resting_Blood_Pressure, 
         Serum_Cholesterol, 
         Max_Heart_Rate_Achieved, 
         ST_Depression_Exercise,
         Num_Major_Vessels_Flouro,
         everything())

#Glimpse data
heart_dataset_clean_tbl %>% glimpse()

#Summary
heart_dataset_clean_tbl %>% summary()


## Data Analysis and Exploration
#Select categorical vars, recode them to their character values, convert to long format
hd_long_fact_tbl <- heart_dataset_clean_tbl  %>%
  select(Sex,
         Chest_Pain_Type,
         Fasting_Blood_Sugar,
         Resting_ECG,
         Exercise_Induced_Angina,
         Peak_Exercise_ST_Segment,
         Thalassemia,
         Diagnosis_Heart_Disease) %>%
  mutate(Sex = recode_factor(Sex, `0` = "female", 
                             `1` = "male" ),
         Chest_Pain_Type = recode_factor(Chest_Pain_Type, `1` = "typical",   
                                         `2` = "atypical",
                                         `3` = "non-angina", 
                                         `4` = "asymptomatic"),
         Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl", 
                                             `1` = "> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                     `1` = "ST-T abnormality",
                                     `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                 `1` = "yes"),
         Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment, `1` = "up-sloaping",
                                                  `2` = "flat",
                                                  `3` = "down-sloaping"),
         Thalassemia = recode_factor(Thalassemia, `3` = "normal",
                                     `6` = "fixed defect",
                                     `7` = "reversible defect")) %>%
  gather(key = "key", value = "value", -Diagnosis_Heart_Disease)


#Visualize with bar plot
hd_long_fact_tbl %>% 
  ggplot(aes(value)) +
  geom_bar(aes(x        = value, 
               fill     = Diagnosis_Heart_Disease), 
           alpha    = .6, 
           position = "dodge", 
           color    = "black",
           width    = .8
  ) +
  labs(x = "",
       y = "",
       title = "Scaled Effect of Categorical Variables") +
  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()) +
  facet_wrap(~ key, scales = "free", nrow = 4) +
  scale_fill_manual(
    values = c("#fde725ff", "#20a486ff"),
    name   = "Heart\nDisease",
    labels = c("No HD", "Yes HD"))

# Evaluating the numeric variables using boxplot.
# Must gather() data first in order to facet wrap by key 
# (default gather call puts all var names into new key col)
hd_long_cont_tbl <- heart_dataset_clean_tbl  %>%
  select(Age,
         Resting_Blood_Pressure,
         Serum_Cholesterol,
         Max_Heart_Rate_Achieved,
         ST_Depression_Exercise,
         Num_Major_Vessels_Flouro,
         Diagnosis_Heart_Disease) %>% 
  gather(key   = "key", 
         value = "value",
         -Diagnosis_Heart_Disease)

#Visualize numeric variables as boxplots
hd_long_cont_tbl %>% 
  ggplot(aes(y = value)) +
  geom_boxplot(aes(fill = Diagnosis_Heart_Disease),
               alpha  = .6,
               fatten = .7) +
  labs(x = "",
       y = "",
       title = "Boxplots for Numeric Variables") +
  scale_fill_manual(
    values = c("#fde725ff", "#20a486ff"),
    name   = "Heart\nDisease",
    labels = c("No HD", "Yes HD")) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()) +
  facet_wrap(~ key, 
             scales = "free", 
             ncol   = 2)

# Heart disease distribution by Sex:
####################################################
# Disease distribution by Sex.
# 0 = Female
# 1 = Male
####################################################
heart_dataset_clean_tbl %>% group_by(Sex, Diagnosis_Heart_Disease) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Sex, count, fill = Diagnosis_Heart_Disease), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Sex") + labs(fill = "Diagnosis_Heart_Disease")


# Heart disease distribution by Age:
####################################################
# Disease distribution by Age. 
####################################################

heart_dataset_clean_tbl %>% group_by(Age, Diagnosis_Heart_Disease) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Age, count, fill = Diagnosis_Heart_Disease), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Diagnosis_Heart_Disease")


# Heart disease distribution by Chest pain type:
####################################################
# Chest pain type for diseased people
# You can see - Majority as chest pain type 4
# 1: typical angina 2: atypical angina  Value 3: non-anginal pain Value 4: asymptomatic
####################################################

heart_dataset_clean_tbl %>% filter(Diagnosis_Heart_Disease == '1') %>% group_by(Age, Chest_Pain_Type) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(Age, count, fill = Chest_Pain_Type),stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("Age") + labs(fill = "Chest_Pain_Type") + 
  ggtitle("Age vs. Count (disease only) for various chest pain conditions")


## Modelling Approach

# Data partition into train and test set for model development:
#set seed for repeatability
set.seed(2021, sample.kind = "Rounding")

# Divide data into train and test set
train_index <- createDataPartition(heart_dataset_clean_tbl$Diagnosis_Heart_Disease, p = 0.8, list= FALSE)
train_set <- heart_dataset_clean_tbl[train_index, ]
test_set <- heart_dataset_clean_tbl[-train_index, ]

#checking whether really 80%
nrow(train_set)/(nrow(test_set)+nrow(train_set))


AUC = list()
Accuracy = list()

### Linear Discriminant Analysis (LDA)

###############################
# LDA Analysis
###############################
#set seed for repeat ability
set.seed(2021, sample.kind = "Rounding")

lda_fit <- train(Diagnosis_Heart_Disease ~ ., method = "lda", data = train_set)
ldaPred <- predict(lda_fit, test_set)

ldaPredProb <- predict(lda_fit, test_set, type='prob')[2]
ldaConfMat <- confusionMatrix(ldaPred, test_set$Diagnosis_Heart_Disease)

ldaConfMat

#ROC Curve
AUC$lda <- roc(as.numeric(test_set$Diagnosis_Heart_Disease),
               as.numeric(as.matrix((ldaPredProb))))$auc
Accuracy$LDA <- ldaConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
Model_result <- cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 1, ncol = 2,
                                           dimnames = list(row.names, col.names))))
Model_result


### Quadrant Discriminant Analysis (QDA)
###############################
# QDA Analysis
###############################
#set seed for repeat ability
set.seed(2021, sample.kind = "Rounding")

qda_fit <- train(Diagnosis_Heart_Disease ~ ., method = "qda", data = train_set)

qdaPred <- predict(qda_fit, test_set)

qdaPredProb <- predict(qda_fit, test_set, type='prob')[2]

qdaConfMat <- confusionMatrix(qdaPred, test_set$Diagnosis_Heart_Disease)

qdaConfMat

#ROC Curve
AUC$qda <- roc(as.numeric(test_set$Diagnosis_Heart_Disease),
               as.numeric(as.matrix((qdaPredProb))))$auc
Accuracy$QDA <- qdaConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
Model_result <- cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 2, ncol = 2,
                                           dimnames = list(row.names, col.names))))
Model_result



### Logistic Regression
###############################
# Logistic Regression
###############################
#set seed for repeat ability
set.seed(2021, sample.kind = "Rounding")

logReg_fit <- train(Diagnosis_Heart_Disease ~ ., 
                    data=train_set, method = 'glm', family = 'binomial')
logRegPred <- predict(logReg_fit, test_set)
logRegPredProb <- predict(logReg_fit, test_set, type='prob')[2]
logRegConfMat <- confusionMatrix(logRegPred, test_set$Diagnosis_Heart_Disease)

logRegConfMat

#ROC Curve
AUC$logReg <- roc(as.numeric(test_set$Diagnosis_Heart_Disease),
                  as.numeric(as.matrix((logRegPredProb))))$auc
Accuracy$logReg <- logRegConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
model_result <- cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 3, ncol = 2,
                                           dimnames = list(row.names, col.names))))
model_result


### KNN Classifier

5-fold cross validation was used, and tuning was done on all the next algorithms discussed here to avoid over-training the algorithms.
###############################
# Knn Classifier
###############################
#set seed for repeat ability
set.seed(2021, sample.kind = "Rounding")

ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)
knn_fit <- train(Diagnosis_Heart_Disease ~ ., 
                 data = train_set, method = "knn", preProcess = c("center","scale"),
                 trControl = ctrl , tuneGrid = expand.grid(k = seq(1, 20, 2)))

plot(knn_fit)

knnPred <- predict(knn_fit,newdata = test_set)
knnPredProb <- predict(knn_fit, test_set, type='prob')[2]
knnConfMat <- confusionMatrix(knnPred, test_set$Diagnosis_Heart_Disease )

knnConfMat

#ROC Curve
AUC$Knn <- roc(as.numeric(test_set$Diagnosis_Heart_Disease),
               as.numeric(as.matrix((knnPredProb))))$auc
Accuracy$Knn <- knnConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
model_result <- cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 4, ncol = 2,
                                           dimnames = list(row.names, col.names))))
model_result


### Support Vector Machine (SVM)
############################
# SVM
############################
#set seed for repeat ability
set.seed(2021, sample.kind = "Rounding")

ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)

grid_svm <- expand.grid(C = c(0.01, 0.1, 1, 10, 20))

svm_fit <- train(Diagnosis_Heart_Disease ~ .,data = train_set,
                 method = "svmLinear", preProcess = c("center","scale"),
                 tuneGrid = grid_svm, trControl = ctrl)

plot(svm_fit)

svmPred <- predict(svm_fit, newdata = test_set)
svmPredProb <- predict(svm_fit, test_set, type='prob')[2]
svmConfMat <- confusionMatrix(svmPred, test_set$Diagnosis_Heart_Disease)

svmConfMat

#ROC Curve
AUC$svm <- roc(as.numeric(test_set$Diagnosis_Heart_Disease),
               as.numeric(as.matrix((svmPred))))$auc
Accuracy$SVM <- svmConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
model_result <- cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 5, ncol = 2,
                                           dimnames = list(row.names, col.names))))
model_result


### Random Forest (RF)
############################
# RF
############################
#set seed for repeat ability
set.seed(2021, sample.kind = "Rounding")

ctrl<- trainControl(method = "cv", number = 5, verboseIter = FALSE)
grid <- data.frame(mtry = seq(1, 10, 2))

rf_fit <- train(Diagnosis_Heart_Disease ~ ., method = "rf", data = train_set,
                ntree = 200, trControl = ctrl, tuneGrid = grid)

plot(rf_fit)

rfPred <- predict(rf_fit, newdata = test_set)
rfPredProb <- predict(rf_fit, test_set, type='prob')[2]
rfConfMat <- confusionMatrix(rfPred, test_set$Diagnosis_Heart_Disease)

rfConfMat

#ROC Curve
AUC$rf <- roc(as.numeric(test_set$Diagnosis_Heart_Disease),
              as.numeric(as.matrix((rfPredProb))))$auc
Accuracy$RF <- rfConfMat$overall['Accuracy']

row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
model_result <- cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 6, ncol = 2,
                                           dimnames = list(row.names, col.names))))
model_result


# Results 
## Comparison of AUC and Accuracy between models

model_result


