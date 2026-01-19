#Load, inspect dataset

data <-read.table('uscrime.txt', header=TRUE)
head(data)

# created test dataset using given factors
test_data <- data.frame(M = 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5, LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1,   
                       U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

#apply Principal Component Analysis using R function prcomp; crime column is not included

pca <- prcomp(data[,1:15], center = TRUE, scale = TRUE)

#Inspected PCA analysis results
summary(pca)

#plot the results
plot(pca, type="l")

#choosing the optimal number of principal components

#The elbow diagram shows that there is less variance beyond 6th principal component, most pronounced "elbow"

#Extract based on Kaiser's rule (eigenvalues > 1)

#get the eigenvalues(the square root of std)
eigenvalues <- pca$sdev^2

eigenvalues_selected <- sum(eigenvalues >1)
4

#choosing between Kaiser's 4 components or originally selected 6th principal components
#Check the cumulative variance using Cumulative Proportion values and calculate how much total variance is captured by 4 components versus 6 components

summary_pca  <- summary(pca)
# 6 components
pca_importance <- summary_pca$importance

pca_importance <- pca_importance["Cumulative Proportion", ]

pca_importance_6 <-pca_importance[6]

pca_importance_6
PC6 0.89996 


#4 components

pca_importance_4 <-pca_importance[4]
pca_importance_4
PC4 0.7992

#calculate diff 
diff <- (pca_importance_6 - pca_importance_4)*100
> diff
PC6 
10.076

#the difference in cumulative variance explained between the first 6 components and the first 4 components is approximately 10.076 percentage points,

#Expecting 5th and 6th components using pca rotation values

rotation_results <- pca$rotation

rotation_results_5_6 <- rotation_results[,5:6]

#Keeping all original 6 principal components; use x values that stands for scores

primary_components <- pca$x[,1:6]

summary(primary_components)

#preparing a dataset for regression, where the selected PC scores are combined with the target variable, which came from the original dataset. 
pca_target_combine <- cbind(primary_components, data[,16])

pca_target_combined <- as.data.frame(pca_target_combine)

head(pca_target_combined)

#build lm model using pca_target_combined data set 

pca_model <- lm(V7~., data=pca_target_combined)

summary(pca_model)

#plot the model

plot(pca_model) 

#To get the final unscaled coefficients and intercept for the model, the following components need to be calculated:
#PCA_model(PCR)
pcr_beta0_scaled <- coef(pca_model)['Intercept']

pcr_betas_scaled <- coef(pca_model)[-1]

#PCA results
#extract pca rotation/loadings for 6 primary component, use rotation_results calculated above

rotation_results_6 <- rotation_results[,1:6]

#PCA original means
pca_orig_means <- pca$center
#PCA original standard deviation
pca_orig_stds <-pca$scale

#Calculate the unscaled coefficients for the original variables using components found above

betas_unscaled <- (rotation_results_6 %*% pcr_betas_scaled) / pca_orig_stds

# Calculate the final unscaled intercept

beta0_unscaled <- pcr_beta0_scaled - sum(betas_unscaled * pca_orig_means)

#Compare this weeks predictions to last weeks prediction
#Transform the test data into PC scores using the pca object using first 6 components
test_data_transformed <- data.frame(predict(pca,test_data))

#Predict using the `pca_model` that was trained on the PC scores

predicted_crime_rate <- predict(pca_model, newdata=test_data_transformed)

> predicted_crime_rate
1 
1248.427 