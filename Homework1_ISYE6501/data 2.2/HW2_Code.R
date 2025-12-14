setwd("~/GT HOMEWORK/Homework1_ISYE6501/Homework1_ISYE6501/data 2.2")
data <- read.table("credit_card_data-headers.txt", header = TRUE, sep= "\t")

#convert first 10 columns into matrix
x <- as.matrix(data[,1:10])

# convert R1 values (11th column) into a factor
y <- as.factor(data[,11])

#Try multiple k values for kNN
k_values <- c(1,3,5,7,10)


#loop through k_values
for (k in k_values) {
  kknn_model <- kknn(R1 ~ ., train=data, test=data, k=k, kernel = "optimal", scale = TRUE)
  
  pred <- fitted(kknn_model)
  
  #accuracy
  accuracy <- mean(pred == data$R1)
  
  #concatenate results
  cat("For k =", k, "accuracy =", accuracy, "\n")
  
    
  }
  
For k = 1 accuracy = 1 
For k = 3 accuracy = 0.6850153 
For k = 5 accuracy = 0.5764526 
For k = 7 accuracy = 0.4923547 
For k = 10 accuracy = 0.4051988 



#Cross-validation using k-fold

#number of folds
k_folds <- 5

set.seed(123)

#number of rows
num_rows <- nrow(data)

k_values <- c(1,3,5,7,10)

library('cvTools')

#create folds

folds <- cut(seq(1, num_rows), breaks = k_folds, labels = FALSE)
       
#loop through folds
for (fold in 1:k_folds) {
  train <- data[folds != fold, ]
  test <- data[folds == fold, ]

  max_score <- 0
  
#loop through k values
 for (kv in seq_along(k_values)) {
  k <- k_values[kv]
  
  model <- kknn(R1 ~., train = train, test = test, k = k, kernel = "optimal", scale = TRUE)
  
  #fit model
  pred <- fitted(model)
  #model accuracy
  model_acc <- mean(pred == test$R1)
  
  #print results
  cat("Fold =", fold, "k=", k, "accuracy=", model_acc,"\n")
  
  
  if(model_acc > max_score){
    max_score <- model_acc
    best_k <- k
    
    
  }
 }
  
cat("Fold =", fold, "k=", best_k, "accuracy=", max_score,"\n")

}




