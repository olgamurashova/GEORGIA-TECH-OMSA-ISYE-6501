#Load, inspect dataset

data <-read.table('germancredit.txt', header=FALSE)
head(data)

#Create binary response for the logistic regression

data$V21[data$V21==1] <- 0 #good
data$V21[data$V21==2] <- 1 #bad

#Split data into training set 70% and test set 30%

set.seed(123)

training <- sample(nrow(data), 0.70*nrow(data), replace = FALSE)

train_set <- data[training,]
test_set <-  data[-training,]

#Build logistic regression

glm_model <- glm(V21~ .,family=binomial(link='logit'), data=train_set)

#Inspect the results

summary(glm_model)

#Use the predict() function to see how your model performs on the test data it has not seen before.
#This will give an unbiased estimate of the model's performance. 
#Predict probabilities on the test set
#type = "response" gives probabilities (0 to 1)

pred1 <- predict(glm_model, newdata=test_set, type="response")
head(pred1)

#Use ROC curve to calculate AUC values to determine fit of the model

install.packages("pROC")
library(pROC)

#Calculate ROC curve
roc <- roc(response=test_set$V21, predictor=pred1)
#Calculate AUC value
auc <- auc(roc)

auc
Area under the curve: 0.7737

#Plot ROC curve
plot(roc)

#Setting new threshold: 0.167 (1/6)

new_threshold <- 1/6

#Convert predicted probabilities to class labels using the new threshold
#The levels are 0 (good) and 1 (bad).

pred_classes <- ifelse(pred1 > new_threshold, 1, 0)

#Create confusion matrix

conf_matrix <- table(Predicted = pred_classes, Actual = test_set$V21)

print(conf_matrix)
---------------------------

