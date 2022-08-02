set.seed(123)

library(MASS)
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others

source("./estimators.R")
source("./generateDGPs.R")
options(dplyr.summarise.inform = FALSE)

shifted.covariates <- data.frame("estimate" = c(),
                                 "pt" = c(), 
                                 "pr" = c(),
                                 "covariate.set" =  c(),
                                 "n" = c())

for (pteff in seq(0.1, 0.9, by = 0.05)){
  
  print(pteff)
  
  for (neff in c(150)){
    for (meff in c(1000)){
      for (i in 1:1000){
        for (preff in c(0.5, 0.25)){
          
            # generate data
            simulation <- toy.example(n = neff, m = meff, output.oracles = F, symetric.po = F, noisier.var.X1 = F)
            
            # useless covariates
            X.shifted.obs <- rbinom(n = meff, 1, pteff)
            X.shifted.trial <- rbinom(n = neff, 1, preff)
            
            
            useless.covariates <- c(X.shifted.trial, X.shifted.obs)
            simulation$X.supp <- useless.covariates
            
            extended <- ipsw.binned(simulation, covariates_names_vector = c("X", "X.supp"), oracle.e = T, oracle.pt = F, oracle.pr = F)
            minimal <- ipsw.binned(simulation, covariates_names_vector = c("X"), oracle.e = T, oracle.pt = F, oracle.pr = F)
            
            new.row <- data.frame("estimate" = c(extended, minimal),
                                  "pt" = c(pteff, pteff),
                                  "pr" = c(preff, preff),
                                  "covariate.set" = c("Extended", "Minimal"),
                                  "n" = c(neff, neff),
                                  "m" =  c(meff, meff))
            
            shifted.covariates <- rbind(shifted.covariates, new.row)
        }
      } 
    }
  }
}

write.csv(shifted.covariates, "./results/toy.example.shifted.covariates.csv", row.names = TRUE)

