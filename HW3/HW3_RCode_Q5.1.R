data <- read.table("uscrime.txt", header=TRUE)

#install 'outliers' package and load the library
install.packages('outliers')
library(outliers)

#extract last column crime
crime_col <- data$Crime

#Use the grubbs.test function to detect one outlier
grubbs.test(crime_col)

data:  crime_col
G = 2.81287, U = 0.82426, p-value = 0.07887
alternative hypothesis: highest value 1993 is an outlier
#p value is > than 0.05

# Grubbs test for two opposite outliers
grubbs.test(crime_col, type=11)

data:  crime_col
G = 4.26877, U = 0.78103, p-value = 1
alternative hypothesis: 342 and 1993 are outliers

# Loop through all the values in the column to detect the lowest value for outliers

outliers <- c()

repeat {
  grubbs <- grubbs.test(crime_col)
  
  if(grubbs$p.value <= 0.05) {
    
    outlier_val <- as.numeric(gsub("[^0-9]", "",grubbs$alternative))
    
    outliers <- c(outliers, outlier_val)
    
    break
    
  } else {
    
    break
  }
  
  }

# no outliers returned



