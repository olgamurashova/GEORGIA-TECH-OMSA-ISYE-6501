#Load, inspect dataset

data <-read.table('uscrime.txt', header=TRUE)
head(data)

library(tree)
library(randomForest)

# Set a seed for reproducibility
set.seed(123)

#build tree model

tree_model <- tree(Crime~., data=data)

#Inspect and plot the tree_model results
summary(tree_model)
plot(tree_model)
text(tree_model)
tree_model$frame

#Perform cross-validation:cv.tree() to find the best tree size. 
#The function evaluates different sized subtrees using cross-validation.

tree_model_cv <- cv.tree(tree_model)

print(tree_model_cv)

#Plot the results for tree_model_cv

plot(tree_model_cv$size, tree_model_cv$dev, type='b')

#Prune the tree to find the size that minimizes the cross-validation error.

optimal_size <- tree_model_cv$size[which.min(tree_model_cv$dev)]
optimal_size
[1] 7 #the optimal number of terminal nodes for the tree



pruned_tree <- prune.tree(tree_model, best = optimal_size)

#Plot the results

plot(pruned_tree)
text(pruned_tree)

# the minimum error corresponds to the largest tree size, and prune.tree() returned the full tree.
#This indicates that the model isn't overfitting the training data, 
#and the full tree provides the best generalization to new data, based on the cross-validation analysis. 
#However, after building the regression models during previous assignments, we do know that there is overfitting and I may have to reduce the variance in the model.

#Use the pruned tree to make predictions on new data

# created test dataset using given factors
test_data <- data.frame(M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1,   
                        U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

pred <- predict(pruned_tree, data=test_data)
pred
# the predicted crime rates for each observation in the dataset, based on the regression tree model
#The fact that the predictions are repeated indicates that the tree has grouped observations into a small number of distinct categories based on the predictor variables.
summary(pruned_tree)



#Calculate the Sum of Squared Residuals (SSE). It finds the difference between the predicted values (pred) and the actual values (data$Crime), 
#squares each difference, and sums them up. 

sse <- sum((pred - data$Crime)^2)
#The SSE represents the variation that is not explained by the model

#Calculate the Total Sum of Squares (SST). It finds the difference between each actual value and the mean of all actual values, squares each difference, and sums them up. 
#The SST represents the total variation in the dependent variable.

sst <- sum((data$Crime - mean(data$Crime))^2)

#Calculate the R-squared value

r_squared <- 1-(sse/sst)
r_squared
[1] 0.7244962

#The R-squared value tells the proportion of the total variance in the dependent variable (crime rate) that is explained by the regression tree model. 
#A value of 1.0 means the model perfectly explains all the variance, while a value of 0.0 means the model explains none of it.
#72% of variability in the pruned_tree decision tree is explained by the model.

---------------------------------------------------------------------------------------
#Build the random forest model
  # ntree: number of trees to grow (default is 500)
  # mtry: number of variables to randomly sample at each split
  # For regression, the default value for mtry is the number of predictor variables divided by three.
  # importance = TRUE: tells R to calculate variable importance
  
  
random_forest <- randomForest(Crime~.,data=data)
random_forest

#Inspect and plot the results
plot(random_forest)

#Tune the model: Random forests have hyperparameters I can tune to improve performance.
#The tuneRF() function can help find the optimal mtry value.
#stepFactor = 1.5: Using a value of 1.5 means the function will increase the mtry value by 50% in each step (e.g., from 5 to 7 or 8)
#improve = 0.01: Using a value of 0.01 means tuneRF() will stop when the model's error doesn't improve by at least 1% for a certain number of iterations
#stepFactor controls the speed and granularity of the search for the optimal mtry value, while improve controls the stopping condition by setting a minimum threshold for improvement [1]


optimal_mtry <-tuneRF(data[,1:15], data$Crime, ntreeTry = 500, stepFactor = 1.5, improve = 0.01)
#the tuneRF() function concluded that mtry = 4 is the best choice because it yielded the minimum OOB error among the values tested

#Improve the random forest model using mtry = 4
random_forest_improved <- randomForest(Crime~.,data=data, mtry = 4)
random_forest_improved

#Plot the results
plot(random_forest_improved)

summary(random_forest_improved)

#Plot the variable importance
varImpPlot(random_forest_improved)

#Make a prediction
pred_random_forest <- predict(random_forest_improved, data=test_data)

pred_random_forest

#Calculate the Sum of Squared Residuals (SSE). It finds the difference between the predicted values (pred) and the actual values (data$Crime), 
#squares each difference, and sums them up. 

sse_rf <- sum((pred_random_forest - data$Crime)^2)
#The SSE represents the variation that is not explained by the model

#Calculate the Total Sum of Squares (SST). It finds the difference between each actual value and the mean of all actual values, squares each difference, and sums them up. 
#The SST represents the total variation in the dependent variable.

sst_rf <- sum((data$Crime - mean(data$Crime))^2)

#Calculate the R-squared value

r_squared_rf <- 1-(sse_rf/sst_rf)
r_squared_rf
[1] 0.4108106