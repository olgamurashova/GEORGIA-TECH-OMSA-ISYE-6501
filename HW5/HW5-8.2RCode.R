#Load, inspect dataset

setwd("~/GT HOMEWORK/HW5")
data <-read.table('uscrime.txt', header=TRUE)
head(data)


#extract min/max crime values as we are to predict the observed crime rate:

min_max_crime <- c(min(data$Crime), max(data$Crime))
min_max_crime
[1]  342 1993

#load the given predictors into dataframe:
given_preds <- data.frame(M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1,   
                       U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

> given_preds
M  So Ed Po1  Po2   LF M.F Pop  NW   U1  U2 Wealth Ineq Prob Time
14  0 10  12  15.5 0.64  94 150 1.1 0.12 3.6   3200 20.1 0.04   39

# build a model using lm function and all predictors in the uscrime dataset except the crime predictor:

all_preds_model <- lm(Crime ~ M + So + Ed + Po1 + Po2 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq + Prob + Time, data=data)

#inspect model summary
summary(all_preds_model)

# use the given predictors and all_preds_model to predict crime rate
all_preds_predicted <- predict(all_preds_model, given_preds)
all_preds_predicted

155.4349 # the predicted crime rate is lower than min crime rate value in the uscrime dataset.Since there are only 47 data points and 15 predictors, this might be related to some overfitting

#build a new model using less predictors, eliminate So, Time, Po2, Prob
less_preds_model1 <- lm(Crime ~ M + Ed + Po1 + LF + M.F + Pop + NW + U1 + U2 + Wealth + Ineq, data=data)
#inspect model summary
summary(less_preds_model1)

#check how model performs when predicting crime rates
less_preds_predicted <- predict(less_preds_model1, given_preds)
less_preds_predicted
 
827.7773 

#build a new model leaving out the following indicators:U1, Po1, P2, Time, NW, M.F, So, Prob
less_preds_model2 <- lm(Crime ~ M + Ed + LF + Pop + U2 + Wealth + Ineq, data=data)
                    
summary(less_preds_model2)        #RSE is high and R-squared is lower

#check how model performs when predicting crime rates
less_preds_predicted2 <- predict(less_preds_model2, given_preds)
less_preds_predicted2
260.3332

#build a new model leaving out the following indicators:U1, Po1, P2, Time, NW, M.F, So, Pop, Wealth, Prob
less_preds_model3 <- lm(Crime ~ M + Ed + LF + U2 + Ineq, data=data)

summary(less_preds_model3) #RSE is even higher and R-squared is lower


#check how model performs when predicting crime rates
less_preds_predicted3 <- predict(less_preds_model3, given_preds)
less_preds_predicted3
930.7027

#build a new model leaving out the following indicators:U1, Po1, P2, NW, M.F, So, Pop, Wealth, LF
less_preds_model4 <- lm(Crime ~ M + Ed + U2 + Ineq + Prob + Time, data=data)
summary(less_preds_model4) # high RSE and R-squared

#check how model performs when predicting crime rates
less_preds_predicted4 <- predict(less_preds_model4, given_preds)
less_preds_predicted4
926.0082

#less predictors increase RSE and lower R-squared

#build a new model using less predictors, eliminate So, Time, Po2, Wealth, NW
less_preds_model5 <- lm(Crime ~ M + Ed + Po1 + LF + M.F + Pop + U1 + U2 + Ineq + Prob, data=data)
#inspect model summary
summary(less_preds_model5)

#check how model performs when predicting crime rates
less_preds_predicted5 <- predict(less_preds_model5, given_preds)
less_preds_predicted5
968.1572


plot(less_preds_model5)

#build a new model using only predictors from a previous model whose p-values are lower than 0.05 and are marked with asterisk
less_preds_model6 <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, data=data)
#inspect model summary
summary(less_preds_model6)

#check how model performs when predicting crime rates
less_preds_predicted6 <- predict(less_preds_model6, given_preds)
less_preds_predicted6
1304.245


plot(less_preds_model6)

# Calculated AIC for each model, compared the results. AIC method may not be reliable as we have a small dataset.
#Corrected AIC method is more appropriate in our case. 

aic_model5 <- AIC(less_preds_model5)
642.7136

aic_model6 <- AIC(less_preds_model6)
640.1661

diff <- aic_model5 - aic_model6
diff
#Difference > 2: If the difference between two models' AIC scores is greater than 2, the model with the lower AIC is considered significantly better.
#A model with a lower AIC value is considered better, as it indicates a better balance between fit and complexity.


#Calculated corrected AIC for each model
aicc_model5 <- AICc(less_preds_model5)
651.8901

aicc_model6 <- AICc(less_preds_model6)
643.9556

diff1 <- aicc_model5 - aicc_model6
diff1
7.934486
