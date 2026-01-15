#Setup




#Load and inspect the data set
#missing values in the bare_nuclei column, which are represented by the character ? and not an integer. 
#To perform numerical imputation methods like mean imputation or regression, first load the data correctly, 
#ensuring that these ? values are interpreted as NA and the column is converted to a numeric 

data <-read.csv('breast-cancer-wisconsin.data.txt', na.strings = "?", header = FALSE)

colnames(data) <- c("Sample_code_number", "Clump_Thickness", "Uniformity_of_cell_size", "Uniformity_of_cell_shape",
                    "Marginal_Adhesion", "Single_epithelial_cell_size", "Bare_nuclei", 
                    "Bland_chromatin","Normal_nucleoli", "Mitoses", "Class")

head(data)

#Calculate number of na values in "Bare_nuclei" column
sum(is.na(data$Bare_nuclei))

16 # 16 missing values in "Bare_nuclei" column

#Convert values in "Bare_nuclei" column to numeric

data$Bare_nuclei <- as.numeric(data$Bare_nuclei)

#Use the mean/mode imputation method to impute values for the missing data
#na.rm = TRUE tells a function to remove or ignore any NA (missing) values before performing its calculation

data_mean <- data

mean_Bare_nuclei <- mean(data_mean$Bare_nuclei, na.rm = TRUE)

#replace NA values in $Bare_nuclei with mean_Bare_nuclei output

data_mean$Bare_nuclei[is.na(data_mean$Bare_nuclei)] <- mean_Bare_nuclei

sum(is.na(data_mean$Bare_nuclei))
[1] 0

print(colSums(is.na(data_mean)))

#Use regression to impute values for the missing data using mice function
#The mice (Multivariate Imputation by Chained Equations) function in R is used for multiple imputation, 
#an advanced statistical technique for handling missing data. 
#Instead of simply replacing NA values with a single estimate (like the mean), 
#mice creates several complete datasets by filling in the missing values with plausible imputed values

install.packages("mice")
library(mice)

regression_data <- mice(data, m = 1, method = 'norm.predict', seed = 123, printFlag = FALSE)

#The complete function from the mice package takes an object containing multiple imputed datasets 
#and returns a single, complete dataset

regression_data_imputed <- complete(regression_data, 1)

sum(is.na(regression_data_imputed$Bare_nuclei))
[1] 0

print(colSums(is.na(regression_data_imputed)))

#Use regression with perturbation to impute values for the missing data

regression_data_perturbation <- mice(data, m = 1, method = 'norm.nob', seed = 123, printFlag = FALSE)

regression_data_perturbation_imputed <- complete(regression_data_perturbation, 1)

sum(is.na(regression_data_perturbation_imputed$Bare_nuclei))
[1] 0

#Visualize the results

par(mfrow = c(1,3))

plot(density(na.omit(data$Bare_nuclei)), main = 'Mean', col = 'blue', ylim = c(0, 1.2))
lines(density(data_mean$Bare_nuclei), col = 'red')
     
legend("top", legend = c("Original", "Imputed"), col = c("blue", "red"), lty = 1)


plot(density(na.omit(data$Bare_nuclei)), main = 'Regression', col = 'blue', ylim = c(0, 1.2))
lines(density(regression_data_imputed$Bare_nuclei), col = 'red')

legend("top", legend = c("Original", "Imputed"), col = c("blue", "red"), lty = 1)



plot(density(na.omit(data$Bare_nuclei)), main = 'Regression with perturbation', col = 'blue', ylim = c(0, 1.2))
lines(density(regression_data_perturbation_imputed$Bare_nuclei), col = 'red')

legend("top", legend = c("Original", "Imputed"), col = c("blue", "red"), lty = 1)

