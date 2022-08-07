set.seed(123)

library(MASS)
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others

options(dplyr.summarise.inform = FALSE)

source("./estimators.R")
source("./generateDGPs.R")


semi.synthetic.simulations <- data.frame("estimate" = c(),
                                        "n" = c(),
                                        "m" = c(),
                                        "method" = c())

# Load all data necessary for semi-synthetic simulation
load("./data/semi-synthetic-DGP.rds")
load("./data/semi-synthetic-oracle-target.rds")
load("./data/semi-synthetic-oracle-trial.rds")


for (neff in seq(500, 3000, by = 500)){
  
  for (meff in seq(500, 9500, by = 1000)){
    
    print(paste0("Starting n = ", neff, " and m = ", meff))
    
    for (i in 1:500){ 
      
      simulation <- simulation.semi.synthetic(n = neff, m = meff, output.oracles = F, extra.noise.on.high.ttt = F)
      
      oracle <- ipsw.binned(simulation, covariates_names_vector = c("time_to_treatment.categorized"), oracle.e = T, oracle.pt = T, oracle.pr = T, oracle.pt.data = count.observations.in.each.strata.target, oracle.pr.data =count.observations.in.each.strata.trial )
      
      semi.oracle <- ipsw.binned(simulation, covariates_names_vector = c("time_to_treatment.categorized"), oracle.e = T, oracle.pt = T, oracle.pr = F, oracle.pt.data = count.observations.in.each.strata.target)
      
      ipsw.estimate <- ipsw.binned(simulation, covariates_names_vector = c("time_to_treatment.categorized"), oracle.e = T, oracle.pt = F, oracle.pr = F)
      
      naive.estimate <- difference.in.means(simulation, oracle.e.too = T)
      
      new.row <- data.frame("estimate" = c(oracle, semi.oracle, ipsw.estimate, naive.estimate),
                            "n" = rep(neff,4),
                            "m" = rep(meff, 4),
                            "method" = c("oracle", "semi-oracle", "ipsw", "naive"))
      semi.synthetic.simulations <- rbind(semi.synthetic.simulations, new.row)
      
    }
  }
}


write.csv(semi.synthetic.simulations, "./results/semisynthetic.varying.n.m.csv", row.names = TRUE)
