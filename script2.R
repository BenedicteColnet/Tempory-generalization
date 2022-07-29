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
MINIMAL_SET <- c("time_to_treatment.categorized",  "Glasgow.initial")
EXTENDED_PRECISION_SET <- c("time_to_treatment.categorized",  "Glasgow.initial", "gender")
EXTENDED_SHIFTED_SET <- c("time_to_treatment.categorized",  "Glasgow.initial", "systolicBloodPressure.categorized", "pupilReact_num")

additional.covariates.shifted <- data.frame("estimate" = c(),
                                            "covariate.set" = c())

for (i in 1:1000){
  
  print(i)
  
  simulation <- simulation.semi.synthetic(n = 3000, m = 10000, source.data = total.with.overlap)
  
  ipsw.minimal <- ipsw.binned(dataframe = simulation, covariates_names_vector = MINIMAL_SET, oracle.e = F, oracle.pt = F, oracle.pr = F)
  
  ipsw.precision <- ipsw.binned(dataframe = simulation, covariates_names_vector = EXTENDED_PRECISION_SET, oracle.e = F, oracle.pt = F, oracle.pr = F)
  
  ipsw.shifted <- ipsw.binned(dataframe = simulation, covariates_names_vector = EXTENDED_SHIFTED_SET, oracle.e = F, oracle.pt = F, oracle.pr = F)
  
  new.row <- data.frame("estimate" = c(ipsw.minimal, ipsw.precision, ipsw.shifted),
                        "covariate.set" = c("Minimal", "Extended - Precision", 'Extended shifted'))
  
  additional.covariates.shifted <- rbind(additional.covariates.shifted, new.row)
  
}

write.csv(additional.covariates.shifted, "./results/semi.synthetic.shifted.csv", row.names = TRUE)