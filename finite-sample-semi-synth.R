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
MINIMAL_SET <- c("time_to_treatment.categorized", "systolicBloodPressure.categorized")

finite.sample.semi.oracle <- data.frame("estimate" = c(),
                                        "method" = c(),
                                        "n" = c(),
                                        "m" = c())

for (i in 1:1000){
  
  print(i)
  
  for (neff in c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000)){
    
    # Generate data for oracle and semi oracle
    simulation <- simulation.semi.synthetic(n = neff, m = 1000, extra.noise.on.high.ttt = F, source.data = total.with.overlap)
    
    # fully oracle
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = T, oracle.pt = T, oracle.pr = T, oracle.pt.data = count.observations.in.each.strata.target, oracle.pr.data = count.observations.in.each.strata.trial)
    method = "oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # semi oracle
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = T, oracle.pt = T, oracle.pr = F, oracle.pt.data = count.observations.in.each.strata.target)
    method = "semi.oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # n = m
    simulation <- simulation.semi.synthetic(n = neff, m = neff, extra.noise.on.high.ttt = F, source.data = total.with.overlap)
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - m = n"

    new.row <- data.frame("estimate" = ipsw,
                            "method" = method,
                            "n" = neff,
                            "m" = neff)
      
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # m >> n
    simulation <- simulation.semi.synthetic(n = neff, m = 10*neff, extra.noise.on.high.ttt = F, source.data = total.with.overlap)
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - m >> n"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = 10*neff)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # n >> m
    simulation <- simulation.semi.synthetic(n = neff, m = neff/10, extra.noise.on.high.ttt = F, source.data = total.with.overlap)
    ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - n >> m"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = neff/10)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
  }
}


write.csv(finite.sample.semi.oracle, "./results/finite.sample.semi.synthetic.csv", row.names = TRUE)