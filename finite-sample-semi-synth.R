# libraries
library(MASS) # simulations
library(ggplot2) # plots
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others

source("./estimators.R")
source("./generateDGPs.R")

options(dplyr.summarise.inform = FALSE)

# Load all data necessary for semi-synthetic simulation
load("./data/semi-synthetic-DGP.rds")
load("./data/semi-synthetic-oracle-target.rds")
load("./data/semi-synthetic-oracle-trial.rds")

# covariates needed
MINIMAL_SET <- c("time_to_treatment.categorized",  "Glasgow.initial")
EXTENDED_PRECISION_SET <- c("time_to_treatment.categorized",  "Glasgow.initial", "gender")
EXTENDED_SHIFTED_SET <- c("time_to_treatment.categorized",  "Glasgow.initial", "systolicBloodPressure.categorized")

finite.sample.semi.oracle <- data.frame("estimate" = c(),
                                        "method" = c(),
                                        "n" = c(),
                                        "m" = c())

for (i in 1:1000){
  
  print(i)
  
  for (neff in c(500, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000)){
    
    # Generate data for oracle and semi oracle
    simulation <- simulation.semi.synthetic(n = neff, m = 1000, extra.noise.on.high.ttt = F, source.data = total.with.overlap)
    
    # fully oracle
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = F, oracle.pt = T, oracle.pr = T, oracle.pt.data = count.observations.in.each.strata.target, oracle.pr.data = count.observations.in.each.strata.trial)
    method = "oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # semi oracle
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = F, oracle.pt = T, oracle.pr = F, oracle.pt.data = count.observations.in.each.strata.target)
    method = "semi.oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    for (meff in c(5000, 10000, 20000)) {
      
      # Generate data
      simulation <- simulation.semi.synthetic(n = neff, m = meff, extra.noise.on.high.ttt = F, source.data = total.with.overlap)
      ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = F, oracle.pt = F, oracle.pr = F)
      method = "ipsw"

      new.row <- data.frame("estimate" = ipsw,
                            "method" = method,
                            "n" = neff,
                            "m" = meff)
      
      finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    }
  }
}


write.csv(finite.sample.semi.oracle, "./results/finite.sample.semi.synthetic.csv", row.names = TRUE)