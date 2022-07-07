set.seed(123)

library(MASS)
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others

source("./estimators.R")
source("./generateDGPs.R")
options(dplyr.summarise.inform = FALSE)

shifted.covariates <- data.frame("estimate" = c(),
                                 "pt" = c(), 
                                 "covariate.set" =  c(),
                                 "n" = c())

for (pteff in seq(0.05, 0.5, by = 0.05)){
  print(pteff)
  
  for (neff in c(1000, 5000, 10000, 20000)){
      for (i in 1:1000){
        
        simulation <- simulation.multivariate.categorical.X(n = neff, m = 50000, prop.X2.Target = pteff)
        
        extended <- ipsw.binned(simulation, covariates_names_vector = c("X1", "X2"), oracle.e = F, oracle.pt = F, oracle.pr = F)
        minimal <- ipsw.binned(simulation, covariates_names_vector = c("X1"), oracle.e = F, oracle.pt = F, oracle.pr = F)
        
        new.row <- data.frame("estimate" = c(extended, minimal),
                              "pt" = c(pteff, pteff),
                              "covariate.set" = c("Extended", "Minimal"),
                              "n" = c(neff, neff))
        
        shifted.covariates <- rbind(shifted.covariates, new.row)
        
      } 
  }
  
}

write.csv(shifted.covariates, "./results/toy.example.shifted.covariates.csv", row.names = TRUE)

