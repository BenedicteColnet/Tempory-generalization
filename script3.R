source("./estimators.R")
source("./generateDGPs.R")


set.seed(123)
options(dplyr.summarise.inform = FALSE)

# libraries
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others


# Load all data necessary for semi-synthetic simulation
load("./data/semi-synthetic-DGP.rds")
load("./data/semi-synthetic-oracle-target.rds")
load("./data/semi-synthetic-oracle-trial.rds")

# covariates needed
BASELINE_COVARIATES <- c("time_to_treatment.categorized", "systolicBloodPressure.categorized", "time_to_treatment", "Glasgow.initial", "pupilReact_num", "age.categorized")
MINIMAL_SET <- c("time_to_treatment.categorized",  "Glasgow.initial")


semi.synthetic.e.hat <- data.frame("estimate" = c(),
                                   "method" = c(),
                                   "additional.noise" = c(),
                                    "m" = c(),
                                    "n" = c())

for (i in 1:1000){
  
  print(i)
  
  
  for (additional.noise in c(F, T)){
    
    for (neff in c(1000)){
      for (meff in c(5000)){
        
        # generate data
        simulation <- simulation.semi.synthetic(n = neff, m = meff, source.data = total.with.overlap, extra.noise.on.high.ttt = additional.noise)
        
        # estimation
        
        ## RCT
        ht <- difference.in.means(simulation, oracle.e.too = T)
        dm <- difference.in.means(simulation, oracle.e.too = F)
        
        ## Generalization
        ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, 
                            oracle.e = T, oracle.pt = F, oracle.pr = F)
        
        ipsw.e.hat <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, 
                              oracle.e = F, oracle.pt = F, oracle.pr = F)
        
        new.row <- data.frame("estimate" = c(dm, ht, ipsw, ipsw.e.hat),
                              "method" = c("DM", "HT", "ipsw", "ipsw.e.hat"),
                              "additional.noise" = rep(additional.noise, 4),
                              "n" = rep(neff, 4),
                              "m" = rep(meff, 4))
        
        semi.synthetic.e.hat <- rbind(semi.synthetic.e.hat, new.row)
        
      }
    }
  }
}

write.csv(semi.synthetic.e.hat, "./results/semi.synthetic.e.hat.csv", row.names = TRUE)


