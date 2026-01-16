#To create a fractional factorial design for 10 factors with 16 runs using R's FrF2 package, use the FrF2() function with nruns=16 and nfactors=10. 
#The function will generate a data frame where each row represents one of the 16 fictitious houses (runs) and each column corresponds to a feature (factor). 
#The output uses 1 to indicate that a feature is included and -1 to indicate it is not.

library('FrF2')


# Generate the fractional factorial design
# nruns = 16 for the number of houses
# nfactors = 10 for the number of yes/no features
# randomize = FALSE is used here for a fixed, reproducible design

factorial_design <- FrF2(nruns = 16, nfactors = 10, randomize = FALSE)

print(factorial_design)
#The factor letters (A, B, C, etc.) represent the 10 different features. 
#We would label these based on our specific features, e.g., A = large yard, B = solar roof, etc.

