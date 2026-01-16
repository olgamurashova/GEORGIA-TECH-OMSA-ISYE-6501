#load dataset
data <- read.table('uscrime.txt', header=TRUE)
head(data)

#Build a simple linear regression model including all predictors

lm <- lm(Crime~., data=data)
summary(lm)

library(ggplot2)
library(lattice)
library(caret)
library(glmnet) #for lasso and elastic net
library(MASS) #for stepwise regression


#Perform stepwise regression in both directions (forward and backward)
#Stepwise regression selects variables by automatically adding or removing them to improve the model.

stepwise <-stepAIC(lm, direction="both")
summary(stepwise)

coef(stepwise)

#Apply lasso method
#Lasso is a regression method that performs both variable selection 
#and regularization by penalizing the absolute size of the regression coefficients.

#In a function call like glmnet(x,y,family=”mgaussian”,alpha=1) the predictors x need to be in R’s matrix format, rather than data frame format.  
#I can convert a data frame to a matrix using as.matrix – for example, x <- as.matrix(data[,1:n-1])
#family=”gaussian” for a single response $Crime

x <- as.matrix(data[,-ncol(data)])
y <- data$Crime

#Scaled the data first (predictor) – otherwise, the regression coefficients will be on different scales 
#and the constraint won’t have the desired effect.
 x_scaled <- scale(x)

 
#Perform k-fold cross-validation to find the best lambda (tuning parameter)
# alpha = 1 for Lasso
set.seed(123)

lasso_cv <- cv.glmnet(x_scaled, y, family='gaussian', alpha=1)
lasso_cv

#Get the optimal lambda value

lasso_lambda <- lasso_cv$lambda.min
lasso_lambda
[1] 3.319887

#Apply lasso method

lasso <- glmnet(x_scaled, y, family='gaussian',alpha=1, lambda=lasso_lambda)

coef(lasso)

#Elastic net is a hybrid of Lasso and Ridge regression that combines their penalties.
#It is also performed using glmnet and cross-validation, but the alpha parameter is between 0 and 1. An alpha of 0.5 is a common choice, 
#but I can also use cross-validation to find the best alpha.

#Run cross validation for Ridge regression with alpha = 0

ridge_cv <- cv.glmnet(x_scaled, y, family='gaussian', alpha=0)

#Get the optimal lambda value
ridge_lambda <- ridge_cv$lambda.min
ridge_lambda
66.70427

#Fit the Ridge model

ridge <- glmnet(x_scaled, y, family='gaussian', alpha=0, lambda = ridge_lambda)

coef(ridge)

#Run cross validation for elastic net with alpha = 0.5

elastic_net_cv <- cv.glmnet(x_scaled, y, family="gaussian", alpha=0.5)

#Get the optimal lambda value
elastic_net_lambda <- elastic_net_cv$lambda.min
elastic_net_lambda
[1] 13.97609

#Fit the elastic net model
elastic_net <- glmnet(x_scaled, y, family="gaussian", alpha=0.5, lambda = elastic_net_lambda)
coef(elastic_net)