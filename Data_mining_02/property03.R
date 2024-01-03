install.packages("hydroGOF")
library(hydroGOF)
library(e1071)
library(caret)
library(dplyr)
library(ggplot2)
library(glmnet)
library(kernlab)
library(rpart)
library(randomForest)
library(gbm)
library(metrics) 
library(datasets) 
library(tidyr) 
library(dplyr) 

raw_data <- read.csv("C:/Users/jhaga/Desktop/Data1.csv")
no_missing_data <- raw_data[-(1:10), ]
missing_data <- raw_data[1:10,-c(1,2)]

# Outlier
# Select only the features for the boxplot
features_only <- no_missing_data[, 4:ncol(no_missing_data)]

# Create a boxplot for outlier analysis
boxplot(features_only, col = "lightblue", main = "Boxplot for Outlier Analysis", xlab = "Features", ylab = "Values")

#Creating heat map
data(no_missing_data)
no_missing_data_std<-as.data.frame(scale(no_missing_data))
no_missing_data_matrix<-data.matrix(no_missing_data_std)
heatmap(no_missing_data_matrix,col=rainbow(256))

#Principal component analysis
data(features_only)
features_only_std<-as.data.frame(scale(features_only))
pca1<-prcomp(features_only_std)
pca1$sdev/sum(pca1$sdev)
loads<-pca1$rotation
scores<-pca1$x
#Select number of PCs
plot(pca1$sdev)
(pca1$sdev[1]+pca1$sdev[2]+pca1$sdev[3]+pca1$sdev[4])/sum(pca1$sdev)

#VIP Calculation
#update loadings w/ reduced no. of PCs
loads_vip<-loads[,1:4]
property_vip<-loads_vip[1,]
features_vip<-loads_vip[2:11,]
weight_vip<-property_vip*features_vip
#no. of weights should be equal to number of PCs included
vip<-weight_vip[,1]+weight_vip[,2]+weight_vip[,3]+weight_vip[,4]
barplot(vip)


# split columns property wise

prop1 <- no_missing_data[, -c(2, 3)]
prop2 <- no_missing_data[, -c(1, 3)]
prop3 <- no_missing_data[, -c(1, 2)]

#Property 03

########################

# CATEGORICAL REGRESSIONS

#Support Vector Machine
#Categorize the Data
hist(prop3$Property.3)
#from dataset, we can pick a cutoff.  For this, we will define 22.5 as it is in the middle.  Obviously not particularly rigorous in our selection, but we'll go with it for the example.
High=ifelse(prop3$Property.3 == 0,-1,1)
class=data.frame(prop3,High)
class=class[,-1]
set.seed(508)
sort<-sample(1:nrow(class),nrow(class)*0.8)
train_svm<-class[sort,]
test_svm<-class[-sort,]
svmfit=svm(train_svm$High~.,data=train_svm,kernel="linear",epsilon=0.1,cost=1,scale=FALSE)
svm.probs_train <- predict(svmfit,train_svm,type = "response")
svm.pred_train <- ifelse(svm.probs_train > 0.5, "1", "-1")
(misclass <- table(svm.pred_train, truth = train_svm$High))
svm.probs_test <- predict(svmfit,newdata=test_svm,type = "response")
svm.pred_test <- ifelse(svm.probs_test > 0.5, "1", "-1")
(misclass <- table(svm.pred_test, truth = test_svm$High))

# Now predict using the svm model
svm.filteredx <- predict(svmfit, newdata = missing_data, type = "response")
svm.filteredy <- ifelse(svm.filteredx > 0.5, "1", "-1")
#svm.probs_test <- predict(svmfit,newdata=filtered_data,type = "response")
#svm.pred_test <- ifelse(svm.probs_test > 0.5, "1", "-1")
#(misclass <- table(svm.pred_test, truth = test_svm$High))

# Check the predictions
print(svm.filteredx)
print(svm.filteredy)

#Decision Tree
#Categorize the Data
hist(prop3$Property.3)
#from dataset, we can pick a cutoff.  For this, we will define 22.5 as it is in the middle.  Obviously not particularly rigorous in our selection, but we'll go with it for the example.
High=ifelse(prop3$Property.3 == 0,"No","Yes")
class=data.frame(prop3,High)
class=class[,-1]
set.seed(508)
sort<-sample(1:nrow(class),nrow(class)*0.8)
train_dt<-class[sort,]
test_dt<-class[-sort,]
fit<-rpart(High~.,train_dt)
tree.pred_train=predict(fit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(fit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))
accuracy_tune<-function(fit){
  predict_unseen<-predict(fit,test_dt,type='class')
  table_mat<-table(test_dt$High,predict_unseen)
  accuracy_Test<-sum(diag(table_mat))/sum(table_mat)
  accuracy_Test
}
#Allow us to change the parameters (minsplit,minbucket,maxdepth,cp) and see how the model changes
control<-rpart.control(minsplit=4,minbucket=2,maxdepth=3,cp=0)
tune_fit<-rpart(High~.,data=test_dt,method='class',control=control)
accuracy_tune(tune_fit)
#Identify the tuned parameters
tune_fit$control
#Pruning the tree as a function of cp (complexity parameter)
pfit<-prune(tune_fit,cp=0)
plot(pfit, uniform=TRUE,main="Pruned Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
tree.pred_train=predict(pfit,train_dt,type="class")
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(pfit,test_dt,type="class")
with(test_dt,table(tree.pred_test,High))


# Calculate the table of predictions vs actual
conf_mat <- with(test_dt, table(tree.pred_test, High))

# Create a barplot
barplot(conf_mat, beside = TRUE, legend = TRUE, 
        args.legend = list(title = "Legend", x = "topright"),
        col = c("red", "green"), 
        main = "Predicted vs Actual")

# Adding labels and title for clarity
xlabel <- "Outcome"
ylabel <- "Count"
title <- "Predicted vs Actual"

# Assign names to the plot
names.arg <- c("No", "Yes")
names(xlabel) <- "Predicted Outcome"
names(ylabel) <- "Actual Outcome"

# Labeling the axes
xlab(xlabel)
ylab(ylabel)

# Adding a main title
main(title)






importance_scores <- fit$variable.importance
barplot(importance_scores, main="Variable Importance")


# Predict using the decision tree model
filtered_data$tree_pred <- predict(pfit, missing_data, type = "class")

# Check the predictions
print(filtered_data$tree_pred)


#Bagging
cvcontrol<-trainControl(method="repeatedcv",number=10,allowParallel=TRUE)
train.bagg<-train(as.factor(High)~.,data=train_dt,method="treebag",trControl=cvcontrol,importance=TRUE,nbagg=50,minsplit=4,minbucket=2,maxdepth=3,cp=0)
train.bagg
plot(varImp(train.bagg))
tree.pred_train=predict(train.bagg,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.bagg,test_dt)
with(test_dt,table(tree.pred_test,High))


#Boosting
#Random Forest
train.rf <- train(as.factor(High) ~ ., data=train_dt,method="rf",trControl=cvcontrol,importance=TRUE)
train.rf
tree.pred_train=predict(train.rf,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.rf,test_dt)
with(test_dt,table(tree.pred_test,High))



# Convert both the predicted and actual values to factors with the same levels
tree.pred_test <- factor(tree.pred_test, levels = c("0", "1"))
test_dt$High <- factor(test_dt$High, levels = c("0", "1"))

# Now try the confusionMatrix function again
confusionMatrix <- confusionMatrix(tree.pred_test, test_dt$High)

# If there are still issues, ensure that the levels are indeed the same
# This can be checked with:
levels(tree.pred_test)
levels(test_dt$High)


#Random Forest Boosting
train.gbm <- train(as.factor(High) ~ ., data=train_dt,method="gbm",verbose=F,trControl=cvcontrol)
train.gbm
tree.pred_train=predict(train.gbm,train_dt)
with(train_dt,table(tree.pred_train,High))
tree.pred_test=predict(train.gbm,test_dt)
with(test_dt,table(tree.pred_test,High))


#Logistic Regression
#Break into training and test data
High=ifelse(prop3$Property.3 == 0,0,1)
class=data.frame(prop3,High)
class=class[,-1]
set.seed(508)
sort<-sample(1:nrow(class),nrow(class)*0.8)
train_logit<-class[sort,]
test_logit<-class[-sort,]
#Develop Logistic Regression Model on the Training Data
glm.fit <- glm(train_logit$High ~ ., data = train_logit, family = binomial)
glm.probs_train <- predict(glm.fit,train_logit,type = "response")
glm.pred_train <- ifelse(glm.probs_train > 0.5, "1", "0")
#Assess prediction result as table as yes / no accuracy
(misclass <- table(glm.pred_train, truth = train_logit$High))
#Apply model and repeat on training data
glm.probs_test <- predict(glm.fit,test_logit,type = "response")
glm.pred_test <- ifelse(glm.probs_test > 0.5, "1", "0")
(misclass <- table(glm.pred_test, truth = test_logit$High))

# Assuming 'train_logit' and 'test_logit' are your training and test datasets

# Develop Logistic Regression Model on the Training Data
glm.fit <- glm(train_logit$High ~ ., data = train_logit, family = binomial)

# Predictions on the Training Data
glm.probs_train <- predict(glm.fit, train_logit, type = "response")
glm.pred_train <- ifelse(glm.probs_train > 0.5, 1, 0)

# Assess prediction result as a confusion matrix for training data
conf_matrix_train <- table(glm.pred_train, truth = train_logit$High)
print("Confusion Matrix for Training Data:")
print(conf_matrix_train)

# Plot for predicted vs. actual on Training Data
plot(glm.pred_train, train_logit$High, col = "blue", pch = 19, xlab = "Predicted", ylab = "Actual",
     main = "Logistic Regression: Predicted vs. Actual (Training Data)")

# Repeat for test data
# Predictions on the Test Data
glm.probs_test <- predict(glm.fit, test_logit, type = "response")
glm.pred_test <- ifelse(glm.probs_test > 0.5, 1, 0)

# Assess prediction result as a confusion matrix for test data
conf_matrix_test <- table(glm.pred_test, truth = test_logit$High)
print("Confusion Matrix for Test Data:")
print(conf_matrix_test)

# Plot for predicted vs. actual on Test Data
plot(glm.pred_test, test_logit$High, col = "red", pch = 19, xlab = "Predicted", ylab = "Actual",
     main = "Logistic Regression: Predicted vs. Actual (Test Data)")


missing_data$glm_pred_prob <- predict(glm.fit, newdata = missing_data, type = "response")
missing_data$glm_pred_class <- ifelse(missing_data$glm_pred_prob > 0.5, "1", "0")



# Check the predictions
print(missing_data$glm_pred_class)
