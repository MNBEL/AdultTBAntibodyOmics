#Import required libraries
set.seed(1234)
library(e1071)
library(caret)
library(glmnet)
library(randomForest)
library(gbm)
library(matrixStats)
library(readxl)
library(cvAUC)
library(pROC)
library(DMwR2)
library(tidyverse)
library(superml)
library(ROCR)
library(readxl)
library(dplyr)
library(mlr3)

poc_data <- read_xlsx("/Users/sarahmali/Desktop/Datasets/Emory_POC_allFeatures.xlsx")
#poc_data
data <- poc_data[,-1]
data <- data[,-1]
#data
train_Y <- data[1]
#Removing the Y column from the data
data_withoutY <- data[, -c(1)]

#Z-scoring the data
scaledData <- scale(data_withoutY)
#head(scaledData)

# Bringing back the Y column into the data
data <- cbind.data.frame(train_Y, scaledData)
# head(data)

# Changing labels to 1 or 0; 1-Active, 0- Latent
#data$Y <- factor(data$Y, levels = c("Active", "Latent"), labels = c(1, 0))
#data$Y

#Initialising variables and vectors for holding values during training of the model
k <- 10
aucSVM_original <- c()
aucSVM_permuted <- c()
dF <- c(1:nrow(data))
displayVars_original <- c()
displayVars_permuted <- c()
allFeats_original <- c()
allFeats_permuted <- c()    
dual_list <- c()

names(data)[1] ="Y"
Y <- data$Y
# LASSO and SVM together in cross-val framework
# set.seed(7654)
for (replicateCV in 1:50)
{
  #Creating negative control
  print(replicateCV)
  permutedY <- sample(Y)
  data$permutedY <- permutedY
  
  folds <- createFolds(y = data$Y, k=k, list = FALSE, returnTrain = FALSE)
  myData <- cbind(data, folds) #Creating k bins
  
  Y_original <- c()
  Y_permuted <- c()
  
  append.SVM_original <- c()
  append.SVM_permuted <- c()
  
  selectedVars_original <- c()
  selectedVars_permuted <- c()
  
  for (NoF in 1:k)
  {
    fold = which(folds == NoF)
    
    train <- myData[-fold, ]
    test <- myData[fold, ]
    
    train <- train[, -ncol(train)]
    test <- test[, -ncol(test)]
    
    X = as.matrix(train[ ,  !names(train) %in% c("Y" , "permutedY")])
    y_original = train$Y
    y_permuted = train$permutedY
    
    glmnet1_original <-cv.glmnet(X , y = y_original , alpha = 1 , nfolds = k)
    lambda_original <- glmnet1_original$lambda.min
    ##lambda_original <- lambda_original*0.75
    
    glmnet2_original <- glmnet(X , y = y_original , alpha=1 , lambda = lambda_original)
    
    c_original <- coef(glmnet2_original)
    # print(c_original)
    
    inds_original <- which(c_original != 0)
    variables_original <- row.names(c_original)[inds_original]
    len_original <- length(variables_original)
    
    if (len_original == 1)
    {
      data_temp <- data[ ,  !names(data) %in% c("Y" , "permutedY")]
      randomSelect_original <- sample(ncol(data_temp), 3) #If LASSO does not return any "imp" features, it is taking any 3 random features
      variables_original <- row.names(c_original)[randomSelect_original]
    } else
      
    {
      variables_original <- row.names(c_original)[inds_original]
      variables_original <- variables_original[2:len_original]
    }
    
    selectedVars_original <- append(selectedVars_original, len_original)
    displayVars_original <- append(displayVars_original , variables_original)
    allFeats_original <- append(allFeats_original , displayVars_original)
    
    dual_list <- append(dual_list, list(variables_original))
    tempTr_original <- train[, (names(train) %in% variables_original)]    
    tempTrain_original <- cbind.data.frame(train$Y, tempTr_original)
    
    tempTr_original <- test[, (names(test) %in% variables_original)]
    tempTest_original <- cbind.data.frame(test$Y, tempTr_original)
    
    tempTr_permuted <- train[, (names(train) %in% variables_original)]
    tempTrain_permuted <- cbind.data.frame(train$permutedY, tempTr_permuted)
    tempTr_permuted <- test[, (names(test) %in% variables_original)]
    tempTest_permuted <- cbind.data.frame(test$permutedY, tempTr_permuted)
    
    colnames(tempTrain_original)[1] <- "Y"
    colnames(tempTest_original)[1] <- "Y"
    
    colnames(tempTrain_permuted)[1] <- "Y"
    colnames(tempTest_permuted)[1] <- "Y"
    
    Y_original <- append(Y_original , tempTest_original$Y)
    Y_permuted <- append(Y_permuted , tempTest_permuted$Y)
    
    #tune_out <- tune.svm(x = tempTrain_original[,-1], y = tempTrain_original[, 1],  
    #          cost = c(0.01, 0.1, 1, 10, 100), 
    #         kernel = "linear")
    
    #svmfit_original = svm(as.factor(Y)~ ., data = tempTrain_original , kernel="linear", cost=tune_out$best.parameters$cost, na.action = na.omit , scale=FALSE)
    svmfit_original = svm(as.factor(Y)~ ., data = tempTrain_original , kernel="linear", cost=0.1, na.action = na.omit , scale=FALSE)
    yhat_original.SVM = predict(svmfit_original, newdata = tempTest_original[, -1])
    append.SVM_original <- append(append.SVM_original, yhat_original.SVM)
    
    svmfit_permuted = svm(as.factor(Y)~ ., data = tempTrain_permuted , kernel="linear", cost=0.1, na.action = na.omit , scale=FALSE)
    yhat_permuted.SVM = predict(svmfit_permuted, newdata = tempTest_permuted[, -1])
    append.SVM_permuted <- append(append.SVM_permuted, yhat_permuted.SVM)  
  }
  
  # Numeric tranformation in order to calculate AUC
  append.SVM_original_num = as.numeric(as.character(append.SVM_original))
  append.SVM_permuted_num = as.numeric(as.character(append.SVM_permuted))
  
  aucSVM_original[replicateCV] <- auc(Y_original , append.SVM_original_num)
  aucSVM_permuted[replicateCV] <- auc(Y_permuted , append.SVM_permuted_num)    
}

# This dual_list shows which variables were picked by LASSO in each iteration
length(dual_list)

sort(table(displayVars_original), decreasing=TRUE)

median(aucSVM_original)
median(aucSVM_permuted)

boxplot_data <- cbind(aucSVM_original,aucSVM_permuted)
boxplot(boxplot_data, beside=TRUE, boxwex=0.2, col='orange')

# t-test to check if the NULL hypothesis is disproved
diff_auc <- c()
for (i in 1:length(aucSVM_original))
{
  diff_auc[i] <- aucSVM_original[i] - aucSVM_permuted[i]
}
t.test(diff_auc, mu = 0, alternative = "greater")


trainset <- data[,c(1:11)]
trainset
Y_train <- data$Y

which(colnames(trainset)=="Ag85A IgG")
which(colnames(trainset)=="HSPX FCR3B")
which(colnames(trainset)=="PPD FCR3B")




# Select only the top LASSO features and form the new training data set
new_trainset <- trainset[,c(1,7,6,4)]
new_trainset

tune_out <- tune.svm(x = new_trainset[,-1], y = new_trainset[, 1],  
                     cost = c(0.01, 0.1, 1, 10, 100), 
                     kernel = "linear")

svmfit_2 <- svm(as.factor(Y_train)~ ., data = new_trainset[,-1], kernel="linear", cost=tune_out$best.parameters$cost, na.action = na.omit, scale=FALSE, probability=TRUE)

# See what the model looks like
svmfit_2

#Find and plot variable importance values for the SVM model

var_imp <- t(svmfit_2$coefs) %*% svmfit_2$SV
values <- abs(unlist(var_imp))
par(mar=c(4,2,5,0))
# Create the bar plot
barplot(values, names.arg = colnames(var_imp), col='orange', ylim=c(0,0.15), ylab="Importance")


## Prediction on entire training dataset (consisting of top LASSO-selected features and actual Y-labels)
### FRom previous SVM codes

pred_train <- predict(svmfit_2, new_trainset[c(-1)], returnData=TRUE, type='response', probability=TRUE)
train_probability_values <- attr(pred_train, "probabilities")
#train_probability_values

y <- as.factor(new_trainset$Y)
pred <- predict(svmfit_2,new_trainset[c(-1)],decision.values=TRUE, probability=TRUE, type="response", returnData=TRUE)
confusionMatrix(pred,y)

## Adding all the positive class (class 1) prediction probabilities to a new list for ROC curve plotting
train_data_pred_prob_class1 <- train_probability_values[,c(2)]

# Compute the ROC curve for Training data
roc_curve <- roc(new_trainset$Y, train_data_pred_prob_class1)

# Plot the ROC curve
plot(roc_curve, main = "Receiver Operating Characteristic Curve", 
     xlab = "False Positive Rate", ylab = "True Positive Rate", print.auc=TRUE, type = "b")

# Add the diagonal reference line
abline(a = 0, b = 1, lty = 2)

# Calculate the Area Under the Curve (AUC)
auc_value <- auc(roc_curve)
legend("bottomright", paste0("AUC = ", round(auc_value, 3)), bty = "n")

# Save the classifier
saveRDS(svmfit_2, file='emory_poc_trained_SVM_hiv_neg.RData')
#########################################################
library("pls")
POC_pls.fit = plsr(new_trainset$Y~., data = new_trainset[,c(2:4)], scale=TRUE)
summary(POC_pls.fit)

Outcome <- new_trainset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "Active"
Outcome[loc0] <- "Latent"

PC1 <- POC_pls.fit$scores[,1]
PC2 <- POC_pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")

#####################################################################################################
# Access loading values
loading_values <- coef(POC_pls.fit)
print(loading_values)

PC1 <- POC_pls.fit$scores[,1]
PC2 <- POC_pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point(size=4)
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")

#########################################################################
#########################################################################
#############    TEST DATA BEGINS    ####################################

test_data <- read_xlsx("/Users/sarahmali/Desktop/Datasets/test_data_FIND.xlsx")
test_data
test_data <- test_data[,-c(1)]
test_data <- test_data[,-c(1)]
Y_test <- test_data$Y
test_data_withoutY <- test_data[, -c(1)]
scaled_test_data <- scale(test_data_withoutY)
head(scaled_test_data)

test_data <- cbind.data.frame(Y_test, scaled_test_data)
head(test_data)

x <- test_data[,-c(1)]
y <- as.factor(test_data$Y)

pred <- predict(svmfit_2, x, returnData=TRUE, type='response', probability=TRUE)
test_probability_values <- attr(pred, "probabilities")
y <- as.factor(test_data$Y)
pred <- predict(svmfit_2, x, decision.values=TRUE, probability=TRUE, type="response", returnData=TRUE)
confusionMatrix(pred,y)

## Adding all the positive class (class 1) prediction probabilities to a new list for ROC curve plotting
train_data_pred_prob_class1 <- test_probability_values[,c(2)]

# Compute the ROC curve for Training data
roc_curve <- roc(test_data$Y, train_data_pred_prob_class1)

# Plot the ROC curve
plot(roc_curve, main = "Receiver Operating Characteristic Curve", xlab = "False Positive Rate", ylab = "True Positive Rate", print.auc=TRUE, type = "b")

# Add the diagonal reference line
abline(a = 1, b = -1, lty = 2)

# Calculate the Area Under the Curve (AUC)
auc_value <- auc(roc_curve)
legend("bottomright", paste0("AUC = ", round(auc_value, 3)), bty = "n")

#################### TEST DATA ENDS #####################################
#########################################################################
#########################################################################

library("pls")
POC_pls.fit = plsr(new_trainset$Y~., data = new_trainset[,c(2:4)], scale=TRUE)
summary(POC_pls.fit)

Outcome <- new_trainset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "Active"
Outcome[loc0] <- "Latent"

PC1 <- POC_pls.fit$scores[,1]
PC2 <- POC_pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")

trainset
allFeatures_POC_pls.fit = plsr(trainset$Y~., data = trainset[,c(2:11)], scale=TRUE)
summary(allFeatures_POC_pls.fit)

Outcome <- trainset$Y
loc1 <- which(Outcome == 1)
loc0 <- which(Outcome == 0)
Outcome[loc1] <- "Active"
Outcome[loc0] <- "Latent"

PC1 <- allFeatures_POC_pls.fit$scores[,1]
PC2 <- allFeatures_POC_pls.fit$scores[,2]
dF <- cbind.data.frame(Outcome, PC1, PC2)
dF$Outcome <- as.factor(dF$Outcome)
p <- ggplot(dF, aes(x=PC1, y=PC2, colour = Outcome)) + geom_point()
# p <- ggplot(dF, aes(x=PC1, y=PC2), colour = c("red", "green")) + geom_point()
p + theme_classic()  + xlab("LV1") + ylab("LV2")
