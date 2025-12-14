setwd("~/GT HOMEWORK/HW3/data 6.2")
data <- read.table('temps.txt', header=TRUE)
head(data)
DAY X1996 X1997 X1998 X1999 X2000 X2001 X2002 X2003 X2004 X2005 X2006 X2007 X2008 X2009 X2010 X2011 X2012 X2013 X2014 X2015
1-Jul    98    86    91    84    89    84    90    73    82    91    93    95    85    95    87    92   105    82    90    85
2-Jul    97    90    88    82    91    87    90    81    81    89    93    85    87    90    84    94    93    85    93    87
3-Jul    97    93    91    87    93    87    87    87    86    86    93    82    91    89    83    95    99    76    87    79
4-Jul    90    91    91    88    95    84    89    86    88    86    91    86    90    91    85    92    98    77    84    85
5-Jul    89    84    91    90    96    86    93    80    90    89    90    88    88    80    88    90   100    83    86    84
6-Jul    93    84    89    91    96    87    93    84    90    82    81    87    82    87    89    90    98    83    87    84

# convert year and temperature values into columns, extract values
library(tidyr)
library(dplyr)

data_reshaped <- pivot_longer(data,cols = 2:ncol(data),names_to = "Year", values_to = 'Temp')
              
data_reshaped$Year <- as.integer(sub("X", "", data_reshaped$Year))


head(data_reshaped)

DAY    Year  Temp
<chr> <int> <int>
  1-Jul  1996    98
  1-Jul  1997    86
  1-Jul  1998    91
  1-Jul  1999    84
  1-Jul  2000    89
  1-Jul  2001    84
  
# Extract month and day values as a column

data_reshaped$DAY <- as.character(data_reshaped$DAY)

data_reshaped$month <- as.numeric(format(strptime(data_reshaped$DAY, "%d-%b"), "%m"))

head(data_reshaped)

DAY    Year  Temp month
<chr> <int> <int> <dbl>
  1-Jul  1996    98     7
2 1-Jul  1997    86     7
3 1-Jul  1998    91     7
4 1-Jul  1999    84     7
5 1-Jul  2000    89     7
6 1-Jul  2001    84     7


#Calculated baseline (July mean and sd) for each year


years <- sort(unique(data_reshaped$Year))

results <- list()

for (yr in years) {
  df <- data_reshaped[data_reshaped$Year == yr & data_reshaped$month == 7, ]
  
  mu <- mean(df$Temp, na.rm=TRUE)
  sd <- sd(df$Temp, na.rm=TRUE)
  C <- 5
  T <- 35
  
  results[[as.character(yr)]] <- data.frame(Year=yr, mu=mu, st=sd, C=C, T=T)
}

#combining all the results into a single dataframe 

baseline_stats <- do.call(rbind, results)
rownames(baseline_stats) <-NULL

baseline_stats
Y   Year       mu       st C  T
1  1996 91.19355 4.901832 5 35
2  1997 87.25806 4.426946 5 35
3  1998 89.70968 3.046239 5 35
4  1999 87.64516 5.782435 5 35
5  2000 91.74194 5.507375 5 35
6  2001 86.74194 2.607269 5 35
7  2002 89.25806 3.741370 5 35
8  2003 85.58065 3.490694 5 35
9  2004 87.83871 3.099428 5 35
10 2005 86.93548 4.404055 5 35
11 2006 90.19355 4.238076 5 35
12 2007 86.41935 3.354342 5 35
13 2008 89.16129 2.745867 5 35
14 2009 86.64516 3.692771 5 35
15 2010 91.25806 4.057649 5 35
16 2011 91.93548 3.473092 5 35
17 2012 94.09677 4.585156 5 35
18 2013 84.70968 3.874649 5 35
19 2014 86.61290 3.584015 5 35
20 2015 90.06452 3.558422 5 35



#Calculate CUSUM CUSUM_today = MAX(0, prevCUSUM + (μ - Temp_today - k))

cusum_results <- list()

# Loop through each year
for (yr in unique(data_reshaped$Year)) {
  
  year_data <- data_reshaped[data_reshaped$Year == yr, ]
  
  mu <- baseline_stats$mu[baseline_stats$Year == yr]
  C  <- baseline_stats$C[baseline_stats$Year == yr]
  T  <- baseline_stats$T[baseline_stats$Year == yr]
  
  year_data$CUSUM <- numeric(nrow(year_data))
  year_data$CUSUM[1] <- 0
  
  if (nrow(year_data) > 1) {
    for (i in 2:nrow(year_data)) {
      year_data$CUSUM[i] <- max(0,year_data$CUSUM[i-1] + (mu - year_data$Temp[i] - C))
    }
  }
  
  year_data$crossed <- year_data$CUSUM >= T
  year_data$first_cross <- seq_len(nrow(year_data)) == match(TRUE, year_data$crossed, nomatch = 0)
  
  cusum_results[[as.character(yr)]] <- year_data
}

#combine all year results

cusum_all_years <- do.call(rbind, cusum_results)
rownames(cusum_all_years) <- NULL

head(cusum_all_years)

DAY    Year  Temp month Deviation CUSUM crossed first_cross
<chr> <int> <int> <dbl>     <dbl> <dbl> <lgl>   <lgl>      
  1 1-Jul  1996    98     7     1.81  0     FALSE   FALSE      
2 2-Jul  1996    97     7     0.806 0.806 FALSE   FALSE      
3 3-Jul  1996    97     7     0.806 1.61  FALSE   FALSE      
4 4-Jul  1996    90     7    -6.19  0     FALSE   FALSE      
5 5-Jul  1996    89     7    -7.19  0     FALSE   FALSE      
6 6-Jul  1996    93     7    -3.19  0     FALSE   FALSE   


#Summarize data when cusum first crossed T
first_cross_summary <- cusum_all_years[cusum_all_years$first_cross, c("Year", "DAY", "CUSUM")]
first_cross_summary

Year DAY    CUSUM
<int> <chr>  <dbl>
  1  1996 17-Sep  38.6
2  1997 26-Sep  43.3
3  1998 30-Sep  41.6
4  1999 22-Sep  36.5
5  2000 6-Sep   44.5
6  2001 26-Sep  38.2
7  2002 25-Sep  45.4
8  2003 1-Oct   41.8
9  2004 16-Sep  37.4
10  2005 9-Oct   41.7
11  2006 14-Sep  38.9
12  2007 13-Oct  38.3
13  2008 21-Sep  44.1
14  2009 19-Sep  38.8
15  2010 30-Sep  43.3
16  2011 7-Sep   45.7
17  2012 22-Aug  36.8
18  2013 18-Aug  35.8
19  2014 29-Sep  42.3
20  2015 16-Sep  36.3

#Exponential Smoothing Step
install.packages("forecast")
library(forecast)

# Convert day values into numeric
first_cross_summary$DayNum <- as.numeric(strptime(first_cross_summary$DAY, "%d-%b")$yday) + 1

#create time series

time_series <- ts(first_cross_summary$DayNum, start = min(first_cross_summary$Year), end=max(first_cross_summary$Year), frequency=1)

model <- HoltWinters(time_series, beta=TRUE, gamma=FALSE) #beta equals trend

Smoothing parameters:
  alpha: 0.372709
beta : TRUE
gamma: FALSE

Coefficients:
  [,1]
a 249.03446
b  12.11755


#Plotting the results 

plot(model, main = "Holt-Winters Smoothing",
     ylab = "Day of Year", xlab = "Year", xaxt="n")
points(first_cross_summary$Year, first_cross_summary$DayNum, col="blue", pch=19)

axis(1, at=first_cross_summary$Year, labels=first_cross_summary$Year)
