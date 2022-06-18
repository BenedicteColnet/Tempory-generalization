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


semi.synthetic.minimal <- data.frame("estimate" = c(),
                                     "method" = c(),
                                     "additional.noise" = c(),
                                     "m" = c(),
                                     "n" = c())

for (i in 1:300){
  
  if(i == 100){
    print("i=100")
  }
  
  if(i == 500){
    print("i=500")
  }
  
  
  for (additional.noise in c(T, F)){
    
    for (neff in c(300, 9000)){
      for (meff in c(100, 10000)){
        
        # generate data
        simulation <- simulation.semi.synthetic(n = neff, m = meff, source.data = total.with.overlap, extra.noise.on.high.ttt = additional.noise)
        
        # estimation
        dm <- difference.in.means(simulation)
        ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, 
                            oracle.e = T, oracle.pt = F, oracle.pr = F)
        semi.oracle <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, 
                                   oracle.e = T, oracle.pt = T, oracle.pr = F, oracle.pt.data = count.observations.in.each.strata.target)
        oracle <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, 
                              oracle.e = T, oracle.pt = T, oracle.pr = T, oracle.pr.data = count.observations.in.each.strata.trial, oracle.pt.data = count.observations.in.each.strata.target)
        
        new.row <- data.frame("estimate" = c(dm, ipsw, semi.oracle, oracle),
                              "method" = c("DM", "IPSW", "semi-oracle", "oracle"),
                              "additional.noise" = rep(additional.noise, 4),
                              "n" = rep(neff, 4),
                              "m" = rep(meff, 4))
        
        semi.synthetic.minimal <- rbind(semi.synthetic.minimal, new.row)
        
      }
    }
  }
}

write.csv(semi.synthetic.minimal, "./results/semi.synthetic.minimal.csv", row.names = TRUE)