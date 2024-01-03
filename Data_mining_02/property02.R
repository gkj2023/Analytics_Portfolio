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
library(cran)
install.packages("kernlab")
library(kernlab)

raw_data <- read.csv("C:/Users/jhaga/Desktop/Data1.csv")
no_missing_data <- raw_data[-(1:10), ]
missing_data <- raw_data[1:10,]

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

#Property 01

install.packages("caret")
library(caret)

#Multiple Linear Regression

data(prop2)
set.seed(508)
prop2_datasort_mlr<-sample(1:nrow(prop2),nrow(prop2)*.8)
train_mlr<-prop2[prop2_datasort_mlr,]
test_mlr<-prop2[-prop2_datasort_mlr,]
mdl_mlr<-lm(Property2~.,data=train_mlr)
pred_train_mlr<-predict(mdl_mlr,train_mlr)
pred_test_mlr<-predict(mdl_mlr,test_mlr)
#Get RMSE values
rmse_mlr_train<-rmse(pred_train_mlr,train_mlr$Property2)
rmse_mlr_train
rmse_mlr_test<-rmse(pred_test_mlr,test_mlr$Property2)
rmse_mlr_test
#R2 value for training data
sst<-sum((train_mlr$Property2-mean(train_mlr$Property2))^2)
sse<-sum((pred_train_mlr-train_mlr$Property2)^2)
rsq<-1-sse/sst
rsq
plot(train_mlr$Property2,pred_train_mlr,xlab="Actual",ylab="Predicted")

predict_MLR <- predict(mdl_mlr, missing_data)
View(predict_MLR)


# Principal Component Regression (PCR)
prop_PCR<-prop2$Property2
feature_PCR<-prop2[,!names(prop2)%in%c("Property2")]
feature_PCR_std<-as.data.frame(scale(feature_PCR))
pca<-prcomp(feature_PCR_std)
plot(pca$sdev)
#Select number of PCs
#In this case, we'll select 4
scores<-pca$x[,1:4]
loads<-pca$rotation[,1:4]
pcr_data<-cbind(prop_PCR,scores)
#Have now defined new data with property and PCs
#Now repeat the MLR process
set.seed(508)
mtcars_datasort_pcr<-sample(1:nrow(pcr_data),nrow(pcr_data)*.8)
train_pcr<-pcr_data[mtcars_datasort_pcr,]
test_pcr<-pcr_data[-mtcars_datasort_pcr,]
train_pcr_d<-as.data.frame(train_pcr)
test_pcr_d<-as.data.frame(test_pcr)
mdl_pcr<-lm(prop_PCR~.,data=train_pcr_d)
pred_train_pcr<-predict(mdl_pcr,train_pcr_d)
pred_test_pcr<-predict(mdl_pcr,test_pcr_d)
#Get RMSE values
rmse_pcr_train<-rmse(pred_train_pcr,train_pcr_d$prop_PCR)
rmse_pcr_train
rmse_pcr_test<-rmse(pred_test_pcr,test_pcr_d$prop_PCR)
rmse_pcr_test
#R2 value for training data
sst<-sum((train_pcr_d$prop_PCR-mean(train_pcr_d$prop_PCR))^2)
sse<-sum((pred_train_pcr-train_pcr_d$prop_PCR)^2)
rsq<-1-sse/sst
rsq
plot(train_pcr_d$prop_PCR,pred_train_pcr,xlab="Actual",ylab="Predicted")

# Assuming 'missing_data' is your new dataset
missing_prop_PCR <- missing_data$Property2
missing_feature_PCR <- missing_data[, !names(missing_data) %in% c("Property2")]
missing_feature_PCR_std <- as.data.frame(scale(missing_feature_PCR))

# Use the existing PCA model to transform the new data
missing_scores <- predict(pca, newdata = missing_feature_PCR_std)[, 1:4]

# Combine the property and PCs for the new dataset
missing_pcr_data <- cbind(missing_prop_PCR, missing_scores)

# Predict using the existing PCR model
pred_missing_pcr <- predict(mdl_pcr, newdata = as.data.frame(missing_pcr_data))

# Print the predicted values
print("Predicted values for missing_data:")
print(pred_missing_pcr)


#Ridge Regression
data(prop2)
set.seed(508)
datasort<-sample(1:nrow(prop2),nrow(prop2)*0.8)
train_ridge<-prop2[datasort,]
test_ridge<-prop2[-datasort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("Property2")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("Property2")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$Property2,alpha=0)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$Property2,alpha=0)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$Property2,alpha=0,lambda=best_lambda)
coef(mdl_ridge_best)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)

#pred_train_ridge<-as.data.frame(pred_train_ridge)
#pred_test_ridge<-as.data.frame(pred_test_ridge)

pred_train_ridge <- as.vector(pred_train_ridge)
pred_test_ridge <- as.vector(pred_test_ridge)

rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$Property2)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$Property2)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$Property2-mean(train_ridge$Property2))^2)
sse<-sum((pred_train_ridge-train_ridge$Property2)^2)
rsq<-1-sse/sst
rsq

#Lasso Regression
data(prop2)
set.seed(508)
datasort<-sample(1:nrow(prop2),nrow(prop2)*0.8)
train_ridge<-prop2[datasort,]
test_ridge<-prop2[-datasort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("Property2")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("Property2")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$Property2,alpha=1)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$Property2,alpha=1)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$Property2,alpha=1,lambda=best_lambda)
coef(mdl_ridge_best)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)

#pred_train_ridge<-as.data.frame(pred_train_ridge)
#pred_test_ridge<-as.data.frame(pred_test_ridge)

pred_train_ridge <- as.vector(pred_train_ridge)
pred_test_ridge <- as.vector(pred_test_ridge)

rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$Property2)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$Property2)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$Property2-mean(train_ridge$Property2))^2)
sse<-sum((pred_train_ridge-train_ridge$Property2)^2)
rsq<-1-sse/sst
rsq


# Assuming 'train_ridge' is your training dataset

# Plot the predictor vs actual values
plot(train_ridge$Property2, pred_train_ridge, xlab = "Actual", ylab = "Predicted", main = "Lasso Regression: Actual vs Predicted")

# Add a line representing the ideal linear relationship
abline(a = 0, b = 1, col = "red", lty = 2)

# Calculate R-squared value
sst <- sum((train_ridge$Property2 - mean(train_ridge$Property2))^2)
sse <- sum((pred_train_ridge - train_ridge$Property2)^2)
rsq <- 1 - sse / sst

# Add the R-squared value as a label
text(mean(train_ridge$Property2), mean(pred_train_ridge), paste("R-squared = ", round(rsq, 3)), pos = 1, col = "red")

# Add labels and title
title("Actual vs Predicted Values")



#Support vector regression
data(prop2)
set.seed(508)
svr_datasort<-sample(1:nrow(prop2),nrow(prop2)*0.8)
train_svr<-prop2[svr_datasort,]
test_svr<-prop2[-svr_datasort,]
train_svr_d<-data.frame(train_svr)
descriptors_train_svr<-train_svr[,! names(train_svr) %in% c("Property2")]
descriptors_test_svr<-test_svr[,! names(test_svr) %in% c("Property2")]
descriptors_train_svr<-as.matrix(descriptors_train_svr)
descriptors_test_svr<-as.data.frame(descriptors_test_svr)
prop_train_svr<-train_svr$Property2
prop_test_svr<-test_svr$Property2
mdl_svr<-tune(svm,prop_train_svr~descriptors_train_svr,ranges=list(epsilon=seq(0,1,0.1),cost=1:10))
BstModel<-mdl_svr$best.model
summary(BstModel)
#Update the regression model with the selections from BstModel (kernel, cost, gamma, epsilon)
svmfit <- svm(train_svr$Property2 ~., data = train_svr, method="eps-regression",kernel = 'radial', cost = 3, gamma=0.1,epsilon=.3,scale=FALSE)
pred_train_svr<-predict(svmfit, data=descriptors_train_svr)
pred_test_svr<-predict(svmfit,newdata=descriptors_test_svr)
rmse_SVR_train<-rmse(pred_train_svr,prop_train_svr)
rmse_SVR_test<-rmse(pred_test_svr,prop_test_svr)
rmse_SVR_train
rmse_SVR_test
sst<-sum((train_svr$Property2-mean(train_svr$Property2))^2)
sse<-sum((pred_train_svr-train_svr$Property2)^2)
rsq<-1-sse/sst
rsq

#Gaussian Process Regression
data(prop2)
set.seed(508)
datasort<-sample(1:nrow(prop2),nrow(prop2)*0.8)
train_gpr<-prop2[datasort,]
test_gpr<-prop2[-datasort,]
descriptors_train_gpr<-train_gpr[,! names(train_gpr) %in% c("Property2")]
descriptors_test_gpr<-test_gpr[,! names(test_gpr) %in% c("Property2")]
mdl_gpr<-gausspr(descriptors_train_gpr,train_gpr$Property2)
pred_train_gpr<-predict(mdl_gpr,descriptors_train_gpr)
pred_test_gpr<-predict(mdl_gpr,descriptors_test_gpr)
rmse_gpr_train<-rmse(pred_train_gpr,as.matrix(train_gpr$Property2))
rmse_gpr_test<-rmse(pred_test_gpr,as.matrix(test_gpr$Property2))
rmse_gpr_train
rmse_gpr_test
sst<-sum((train_gpr$Property2-mean(train_gpr$Property2))^2)
sse<-sum((pred_train_gpr-train_gpr$Property2)^2)
rsq<-1-sse/sst
rsq

# Assuming 'train_gpr' is your training dataset

# Plot the predictor vs actual values
plot(train_gpr$Property2, pred_train_gpr, xlab = "Actual", ylab = "Predicted", main = "Gaussian Process Regression: Actual vs Predicted")

# Add a line representing the ideal linear relationship
abline(a = 0, b = 1, col = "red", lty = 2)

# Calculate R-squared value
sst <- sum((train_gpr$Property2 - mean(train_gpr$Property2))^2)
sse <- sum((pred_train_gpr - train_gpr$Property2)^2)
rsq <- 1 - sse / sst

# Add the R-squared value as a label
text(mean(train_gpr$Property2), mean(pred_train_gpr), paste("R-squared = ", round(rsq, 3)), pos = 1, col = "red")

# Add labels and title
title("Actual vs Predicted Values")


# Assuming 'missing_data' is your dataset

missing_data_ <- missing_data[, -c(2,3)]

# Assuming 'missing_data' is your dataset

# Sample 80% of the rows for training
set.seed(508)
datasort_missing <- sample(1:nrow(missing_data), nrow(missing_data) * 0.8)
train_gpr_missing <- missing_data[datasort_missing, ]
test_gpr_missing <- missing_data[-datasort_missing, ]

# Separate descriptors and target variable for training
descriptors_train_gpr_missing <- train_gpr_missing[, !names(train_gpr_missing) %in% c("Property2")]
target_train_gpr_missing <- train_gpr_missing$Property2

# Separate descriptors and target variable for testing
descriptors_test_gpr_missing <- test_gpr_missing[, !names(test_gpr_missing) %in% c("Property2")]
target_test_gpr_missing <- test_gpr_missing$Property2

# Handle missing values
complete_cases_index_train <- complete.cases(descriptors_train_gpr_missing)
descriptors_train_gpr_missing <- descriptors_train_gpr_missing[complete_cases_index_train, ]
target_train_gpr_missing <- target_train_gpr_missing[complete_cases_index_train]

complete_cases_index_test <- complete.cases(descriptors_test_gpr_missing)
descriptors_test_gpr_missing <- descriptors_test_gpr_missing[complete_cases_index_test, ]
target_test_gpr_missing <- target_test_gpr_missing[complete_cases_index_test]

# Train Gaussian Process Regression model
mdl_gpr_missing <- gausspr(descriptors_train_gpr_missing, target_train_gpr_missing)

# Predict on training and testing sets
pred_train_gpr_missing <- predict(mdl_gpr_missing, descriptors_train_gpr_missing)
pred_test_gpr_missing <- predict(mdl_gpr_missing, descriptors_test_gpr_missing)

# Calculate RMSE for training and testing sets
rmse_gpr_train_missing <- rmse(pred_train_gpr_missing, target_train_gpr_missing)
rmse_gpr_test_missing <- rmse(pred_test_gpr_missing, target_test_gpr_missing)

# Calculate R-squared for training set
sst_missing <- sum((target_train_gpr_missing - mean(target_train_gpr_missing))^2)
sse_missing <- sum((pred_train_gpr_missing - target_train_gpr_missing)^2)
rsq_missing <- 1 - sse_missing / sst_missing

# Print the results
print(paste("RMSE (Training):", rmse_gpr_train_missing))
print(paste("RMSE (Testing):", rmse_gpr_test_missing))
print(paste("R-squared (Training):", rsq_missing))


#Random Forest Regression
data(prop2)
set.seed(508)
datasort<-sample(1:nrow(prop2),nrow(prop2)*0.8)
train_rf<-prop2[datasort,]
test_rf<-prop2[-datasort,]
model_rf<-randomForest(train_rf$Property2~.,data=train_rf,mtry=3,importance=TRUE,na.action=na.omit)
pred_train_rf<-predict(model_rf,train_rf)
pred_test_rf<-predict(model_rf,newdata=test_rf)
rmse_rf_train<-rmse(pred_train_rf,train_rf$Property2)
rmse_rf_test<-rmse(pred_test_rf,test_rf$Property2)
rmse_rf_train
rmse_rf_test
sst<-sum((train_rf$Property2-mean(train_rf$Property2))^2)
sse<-sum((pred_train_rf-train_rf$Property2)^2)
rsq<-1-sse/sst
rsq
plot(model_rf)

# Assuming 'train_rf' is your training dataset

# Plot the predictor vs actual values
plot(train_rf$Property2, pred_train_rf, xlab = "Actual", ylab = "Predicted", main = "Random Forest Regression: Actual vs Predicted")

# Add a line representing the ideal linear relationship
abline(a = 0, b = 1, col = "red", lty = 2)

# Calculate R-squared value
sst <- sum((train_rf$Property2 - mean(train_rf$Property2))^2)
sse <- sum((pred_train_rf - train_rf$Property2)^2)
rsq <- 1 - sse / sst

# Add the R-squared value as a label
text(mean(train_rf$Property2), mean(pred_train_rf), paste("R-squared = ", round(rsq, 3)), pos = 1, col = "red")

installed.packages('ModelMetrics')
library(ModelMetrics)
