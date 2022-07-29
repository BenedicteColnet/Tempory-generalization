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

# Prepare oracles
source.data <- load("./data/semi-synthetic-DGP.rds")
source.RCT <- total.with.overlap[total.with.overlap$S == 1,]
source.Obs <- total.with.overlap[total.with.overlap$S == 0,]


oracle.proba.trial <- source.RCT %>% 
  group_by(time_to_treatment.categorized, Glasgow.initial) %>%
  summarise(pr = n()/nrow(source.RCT))

oracle.proba.target <- source.Obs %>% 
  group_by(time_to_treatment.categorized, Glasgow.initial) %>%
  summarise(pt = n()/nrow(source.Obs))


for (neff in seq(500, 3000, by = 500)){
  
  for (meff in seq(5000, 10000, by = 1000)){
    
    print(paste0("Starting n = ", neff, " and m = ", meff))
    
    for (i in 1:500){ 
      
      simulation <- simulation.semi.synthetic(n = neff, m = meff, output.oracles = TRUE, extra.noise.on.high.ttt = F)
      
      # add oracles
      simulation <- merge(simulation, oracle.proba.target)
      simulation <- merge(simulation, oracle.proba.trial)
      
      oracle <- ipsw.binned(simulation, covariates_names_vector = c("time_to_treatment.categorized", "Glasgow.initial"), oracle.e = T, oracle.pt = T, oracle.pr = T)
      
      semi.oracle <- ipsw.binned(simulation, covariates_names_vector = c("time_to_treatment.categorized", "Glasgow.initial"), oracle.e = T, oracle.pt = T, oracle.pr = F)
      
      ipsw.estimate <- ipsw.binned(simulation, covariates_names_vector = c("time_to_treatment.categorized", "Glasgow.initial"), oracle.e = T, oracle.pt = F, oracle.pr = F)
      
      naive.estimate <- difference.in.means(simulation)
      
      new.row <- data.frame("estimate" = c(oracle, semi.oracle, ipsw.estimate, naive.estimate),
                            "n" = rep(neff,4),
                            "m" = rep(meff, 4),
                            "method" = c("oracle", "semi-oracle", "ipsw", "naive"))
      semi.synthetic.simulations <- rbind(semi.synthetic.simulations, new.row)
      
    }
  }
}


write.csv(semi.synthetic.simulations, "./results/semisynthetic.varying.n.m.csv", row.names = TRUE)
