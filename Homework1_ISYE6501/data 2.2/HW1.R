> setwd("~/GT HOMEWORK/Homework1_ISYE6501/Homework1_ISYE6501/data 2.2")

> dat <- read.table("credit_card_data-headers.txt", header = TRUE, sep= "\t")

> head(dat)
A1    A2    A3   A8 A9 A10 A11 A12 A14 A15 R1
1  1 30.83 0.000 1.25  1   0   1   1 202   0  1
2  0 58.67 4.460 3.04  1   0   6   1  43 560  1
3  0 24.50 0.500 1.50  1   1   0   1 280 824  1
4  1 27.83 1.540 3.75  1   0   5   0 100   3  1
5  1 20.17 5.625 1.71  1   1   0   1 120   0  1
6  1 32.08 4.000 2.50  1   1   0   0 360   0  1

#R1 column is a response variable
#Attributes (A1, A2, ..A15 are predictors)
#Convert R1 into classifiers(positive/negative)
> dat$R1 <- factor(dat$R1, levels = c(0,1), labels = c('Negative', 'Positive'))

> library(kernlab)
#call ksvm. Vanilladot is a simple linear kernel

#convert first 10 columns into matrix
x <- as.matrix(dat[,1:10])
# convert R1 values (11th column) into a factor
y <- as.factor(dat[,11])

#call ksvm
model <- ksvm(x, y, type="C-svc", kernel=vanilladot(), C=100, scaled=TRUE)
> model
Support Vector Machine object of class "ksvm" 

SV type: C-svc  (classification) 
parameter : cost C = 100 

Linear (vanilla) kernel function. 

Number of Support Vectors : 189 

Objective Function Value : -17887.92 
Training error : 0.136086 

# calculate a1…am
> a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
> a
A1            A2            A3            A8            A9           A10           A11           A12 
-0.0010065348 -0.0011729048 -0.0016261967  0.0030064203  1.0049405641 -0.0028259432  0.0002600295 -0.0005349551 
A14           A15 
-0.0012283758  0.1063633995 

# calculate a0
> a0 <- -model@b
> a0
[1] 0.08158492

# check what data predicts
pred <- predict(model,x)
pred
# see what fraction of the model’s predictions match the actual classification
> sum(pred == y)/nrow(dat)
[1] 0.8639144

#Load kknn 
> library(kknn)
> set.seed(123)

#training knn classifiers using values of k (1,3,5,7,10) and using R1 attribute as a predictor
> k1 <- kknn(R1 ~ ., train=dat, test=dat, k=1, kernel='optimal',scale=TRUE)
#check if the predictions equal to dat$R1
> acc_k1 <- mean(fitted(k1) == dat$R1)

> k3 <- kknn(R1~ ., train=dat, test=dat, k=3, kernel='optimal', scale=TRUE)
> acc_k3 <- mean(fitted(k3)==dat$R1)

> k5 <- kknn(R1 ~ ., train=dat, test=dat, k=5, kernel='optimal',scale=TRUE)
> acc_k5 <- mean(fitted(k5) == dat$R1)

> k7 <- kknn(R1 ~ ., train=dat, test=dat, k=7, kernel='optimal',scale=TRUE)
> acc_k7 <- mean(fitted(k7)==dat$R1)

> k10 <- kknn(R1 ~ ., train=dat, test=dat, k=10, kernel='optimal',scale=TRUE)
> acc_k10 <- mean(fitted(k10) == dat$R1)

#vector of accuracies
> c(k1=acc_k1, k3= acc_k3, k5=acc_k5, k7= acc_k7, k10=acc_k10)
k1        k3        k5        k7       k10 
1.0000000 1.0000000 0.9510703 0.9342508 0.9235474 
