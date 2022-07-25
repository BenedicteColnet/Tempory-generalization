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

for (pteff in seq(0.1, 0.5, by = 0.05)){
  print(pteff)
  
  for (neff in c(250, 500, 1000)){
    for (meff in c(1000)){
      for (i in 1:10000){
        
        simulation <- toy.example(n = neff, m = meff, output.oracles = F, symetric.po = F, noisier.var.X1 = F)
        
        # useless covariates
        X.shifted.obs <- rbinom(n = meff, 1, pteff)
        X.shifted.trial <- rbinom(n = neff, 1, 0.5)
        
        
        useless.covariates <- c(X.shifted.trial, X.shifted.obs)
        simulation$X.supp <- useless.covariates
        
        extended <- ipsw.binned(simulation, covariates_names_vector = c("X", "X.supp"), oracle.e = F, oracle.pt = F, oracle.pr = F)
        minimal <- ipsw.binned(simulation, covariates_names_vector = c("X"), oracle.e = F, oracle.pt = F, oracle.pr = F)
        
        new.row <- data.frame("estimate" = c(extended, minimal),
                              "pt" = c(pteff, pteff),
                              "covariate.set" = c("Extended", "Minimal"),
                              "n" = c(neff, neff),
                              "m" =  c(meff, meff))
        
        shifted.covariates <- rbind(shifted.covariates, new.row)
        
      } 
    }
  }
  
}

write.csv(shifted.covariates, "./results/toy.example.shifted.covariates.csv", row.names = TRUE)

