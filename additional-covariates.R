set.seed(123)

library(MASS)
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others

options(dplyr.summarise.inform = FALSE)

source("./estimators.R")
source("./generateDGPs.R")


additional.covariates.varying.n.m <- data.frame("n" = c(),
                                       "m" = c(),
                                       "estimate" = c(),
                                       "subset" = c())


for ( neff in seq(200, 1000, by = 200)){
  for ( meff in seq(100, 500, by = 100)){
    
    for (subset in c("minimal", 'add.shifted', "add.useless")){
      
      if (subset == "minimal"){
        covariates = c("X1")
      } else if (subset == "add.shifted"){
        covariates <- c("X1", "X4")
      } else if (subset == "add.useless"){
        covariates <- c("X1", "X2")
      }
    }
    
    print(paste0("Starting n = ", neff, " and m = ", meff))
    
    for (rep in 1:1000){
      simulation <- simulation.multivariate.categorical.X(n = neff, 
                                m = meff, 
                                ratio = 0.5, 
                                output.oracles = FALSE,
                                prop.X1.RCT = 0.75,
                                prop.X1.Target = 0.3)
      
      # estimation
      ipsw.estimate <- ipsw.binned(dataframe = simulation, covariates_names_vector = covariates, oracle.e = T, oracle.pt = F, oracle.pr = F)
      
      # new row  
      new.row <- data.frame("n" = neff,
                            "m" = meff,
                            "estimate" = simulation,
                            "subset" = subset)
      additional.covariates.varying.n.m <- rbind(additional.covariates.varying.n.m, new.row)
    }
  }
}


write.csv(additional.covariates.varying.n.m, "./additional.covariates.varying.n.m.csv", row.names = TRUE)