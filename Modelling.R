library(readr)
Data <- read.csv("processed_data.csv")
View(Data)

Data$gender <- as.factor(Data$gender)
Data$hypertension <- as.factor(Data$hypertension)
Data$heart_disease <- as.factor(Data$heart_disease)
Data$ever_married <- as.factor(Data$ever_married)
Data$work_type <- as.factor(Data$work_type)
Data$Residence_type <- as.factor(Data$Residence_type)
Data$smoking_status <- as.factor(Data$smoking_status)
Data$stroke <- as.factor(Data$stroke)
str(Data)

#Split into train and test dataset 
set.seed(1)
train.index <- sample(x=1:nrow(Data), size=ceiling(0.7*nrow(Data)))

Data.train <- Data[train.index, ]
Data.test <- Data[-train.index, ]

#check classes distribution
table(Data$stroke)     
prop.table(table(Data$stroke))

#Rose for imbalanced dataset
#install.packages("ROSE")  #only for the first time
library(ROSE)
Data.train_balanced <- ROSE(stroke ~ ., data = Data.train, seed = 1)$data
table(Data.train_balanced$stroke)
prop.table(table(Data.train_balanced$stroke))

## Model Training

# glm = logistic regression model
Model_glm <-glm(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, 
              family = 'binomial')
summary(Model_glm)

Model_glm1 <-glm(stroke ~ hypertension + heart_disease + ever_married + work_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, 
                family = 'binomial')
summary(Model_glm1)

## Predict on test
p <- predict(Model_glm1, Data.test, type = "response")

## If p exceeds threshold of 0.5, 1 else 0
stroke_noStroke <- ifelse(p > 0.5, 1, 0)

## Convert to factor: p_class
p_class <- factor(stroke_noStroke, levels = levels(Data.test[["stroke"]]))

## Create confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(p_class, Data.test[["stroke"]])

#build prediction model
install.packages("lattice")
install.packages("ggplot2")
library(lattice)
library(ggplot2)
library(caret)
fitted.results_glm <- predict(Model_glm,newdata=Data.test,type='response')
fitted.results_glm <- ifelse(fitted.results_glm > 0.5,1,0)
misClasificError <- mean(fitted.results_glm!= Data.test$stroke)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(fitted.results_glm , Data.test$stroke)

#build prediction model1
fitted.results_glm1 <- predict(Model_glm1,newdata=Data.test,type='response')
fitted.results_glm1 <- ifelse(fitted.results_glm1 > 0.5,1,0)
misClasificError <- mean(fitted.results_glm1 != Data.test$stroke)
print(paste('Accuracy',1-misClasificError))
confusionMatrix(fitted.results_glm1, Data.test$stroke)

#install.packages("ROCR")
library(ROCR)
p_glm <- predict(fitted.results_glm1, newdata=Data.test, type="response")
pr_glm <- prediction(p_glm, Data.test$stroke)
prf_glm <- performance(pr_glm, measure = "tpr", x.measure = "fpr")
plot(prf_glm)

auc <- performance(pr_glm, measure = "auc")
auc <- auc@y.values[[1]]
auc

#confusion matrix
confusionMatrix(fitted.results_glm, Data.test$stroke)

##The tree starts at the top and finds the best data to split into nodes. It does this by recursive binary splitting using either the Gini index or cross-entropy measure. 
### Build tree model
library(rpart)
Model_rpart <-rpart(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, method = 'class',cp=0.01,parms = list(split = "gini"))
Model_rpart1 <-rpart(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, method = 'class',cp=0.001,parms = list(split = "gini"))
Model_rpart2 <-rpart(stroke ~ age + hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, method = 'class',cp=0.001,parms = list(split = "gini"))
Model_rpart3 <-rpart(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, method = 'class',cp=0.0005)
Model_rpart4 <-rpart(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, method = 'class',cp=0.001,parms = list(split = "information"))


### Build prediction model
fitted.results.tree <- predict(Model_rpart,newdata=Data.test,type='class')
fitted.results.tree1 <- predict(Model_rpart1,newdata=Data.test,type='class')
fitted.results.tree2 <- predict(Model_rpart2,newdata=Data.test,type='class')
fitted.results.tree3 <- predict(Model_rpart3,newdata=Data.test,type='class')
fitted.results.tree4 <- predict(Model_rpart4,newdata=Data.test,type='class')


### confusionMatrix
confusionMatrix(fitted.results.tree,Data.test$stroke)
confusionMatrix(fitted.results.tree1,Data.test$stroke)
confusionMatrix(fitted.results.tree2,Data.test$stroke)
confusionMatrix(fitted.results.tree3,Data.test$stroke)
confusionMatrix(fitted.results.tree4,Data.test$stroke)

#**Model_rpart1 has the highest accuracy and sensitivity**
  
### Model Plotting
library(rpart.plot)
rpart.plot(Model_rpart1,main="Decision Tree")

### Model Rule
rpart.rules(Model_rpart1,cover = TRUE)

## Decision Tree Pruning
### Plotting the error vs complexity parameter
plotcp(Model_rpart1)
plotcp(Model_rpart3)


### Generating complexity parameter table
print(Model_rpart1$cptable, main='Model_rpart1')
print(Model_rpart3$cptable, main='Model_rpart3')

#Even in Model_rpart3, the lowest CP is not reached yet. However, the accuracy of Model_rpart3 is smaller than accuracy of Model_rpart1.We decided to focouse on Model_rpart1 only.


### Obtaining an optimal pruned model
index1 <- which.min(Model_rpart1$cptable[, "xerror"])
cp_optimal1 <- Model_rpart1$cptable[index1, "CP"]
cp_optimal1

Model_rpart_opt1 <- prune(tree = Model_rpart1, cp = cp_optimal1)
rpart.plot(x = Model_rpart_opt1, yesno = 2, type = 0, extra = 0)

### Pruned tree performance
fitted.results.tree.opt1 <- predict(Model_rpart_opt1,newdata=Data.test,type='class')
tbl_prune <- table(predicted = fitted.results.tree.opt1, actual = Data.test$stroke)
tbl_prune  
accuracy_prune <- sum(diag(tbl_prune)) / sum(tbl_prune)
accuracy_prune

#**After pruning the tree, the accuracy of optimum model is the same as Model_rpart1.**

## Other Model: Random Forest
#install.packages("randomForest")
library(randomForest)
set.seed(101)
fit.rf <- randomForest(stroke ~ hypertension + heart_disease + ever_married + work_type + Residence_type + avg_glucose_level + smoking_status + bmi, data = Data.train_balanced, na.action = na.omit)

tbl.rf <- fit.rf$confusion[,c(1,2)]
accuracy_rf <- sum(diag(tbl.rf)) / sum(tbl.rf)
accuracy_rf

## Evaluating Performace 
Data.test1 <- predict(Model_rpart1, Data.test, type="prob")
pred_Data.test <-prediction(Data.test1[,2],Data.test$stroke)

#perf_Data.test <- performance(pred_Data.test,"auc")
plot(performance(pred_Data.test,  measure = "tpr", x.measure = "fpr"), colorize=TRUE)
auc <- performance(pred_Data.test, measure = "auc")
auc <- auc@y.values[[1]]
auc
