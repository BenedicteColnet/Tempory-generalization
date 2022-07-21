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


finite.sample.semi.oracle <- data.frame("estimate" = c(),
                                        "method" = c(),
                                        "n" = c())

for (i in 1:5000){
  
  print(i)
  
  
  for (additional.noise in c(F)){
    
    for (neff in seq(500, 3000, by = 500)){
      
      # Generate data
      simulation <- simulation.semi.synthetic(n = neff, m = 5, source.data = total.with.overlap, extra.noise.on.high.ttt = additional.noise)
      
      # Generalization
      ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, 
                          oracle.e = F, oracle.pt = T, oracle.pr = F, oracle.pt.data = count.observations.in.each.strata.target)
      
      new.row <- data.frame("estimate" = ipsw,
                            "method" = "semi-oracle",
                            "n" = neff )
      
      finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    }
  }
}


write.csv(semi.synthetic.e.hat, "./results/finite.sample.so.csv", row.names = TRUE)