data <- read.table('uscrime.txt',stringsAsFactors = FALSE, header = TRUE)

str(data)
#convert all data to numeric
#data <- lapply(data, function(x) as.numeric(as.character(x)))



par(mar=c(4,4,2,1))
for (col in names(data)) { 
  hist(data[[col]], main=col, xlab='', col="lightblue")
}

#Shapiro Wilk test

Shapiro_test <- lapply(data, function(col) {
  shapiro.test(col)
})

Shapiro_test_results <- c(Shapiro_test)

Shapiro_test_results <- do.call(rbind, Shapiro_test_results)

       statistic  p.value      method                        data.name
M      0.9479701 0.03607538   "Shapiro-Wilk normality test" "col"    
So     0.5988287 4.170456e-10 "Shapiro-Wilk normality test" "col"    
Ed     0.9194494 0.003196959  "Shapiro-Wilk normality test" "col"    
Po1    0.9230723 0.004280818  "Shapiro-Wilk normality test" "col"    
Po2    0.928306  0.006580799  "Shapiro-Wilk normality test" "col"    
LF     0.9651545 0.1719905    "Shapiro-Wilk normality test" "col"    
M.F    0.9215104 0.003772404  "Shapiro-Wilk normality test" "col"    
Pop    0.7622085 2.490781e-07 "Shapiro-Wilk normality test" "col"    
NW     0.820361  4.68722e-06  "Shapiro-Wilk normality test" "col"    
U1     0.9315271 0.008616725  "Shapiro-Wilk normality test" "col"    
U2     0.9675499 0.2133081    "Shapiro-Wilk normality test" "col"    
Wealth 0.9728094 0.3375134    "Shapiro-Wilk normality test" "col"    
Ineq   0.936528  0.01319091   "Shapiro-Wilk normality test" "col"    
Prob   0.9400547 0.01790518   "Shapiro-Wilk normality test" "col"    
Time   0.9805057 0.6132446    "Shapiro-Wilk normality test" "col"    
Crime  0.9127259 0.001882496  "Shapiro-Wilk normality test" "col"    

# check p.values < 0.05:

p_value <- lapply(Shapiro_test, function(x) x$p.value)
alt_hyp <- p_value < 0.05

  M     So     Ed    Po1    Po2     LF    M.F    Pop     NW     U1     U2 Wealth   Ineq   Prob   Time 
TRUE   TRUE   TRUE   TRUE   TRUE  FALSE   TRUE   TRUE   TRUE   TRUE  FALSE  FALSE   TRUE   TRUE  FALSE 
Crime 
TRUE

#So columns like LF and U2 are roughly normal, while the others show significant deviation from normality.

