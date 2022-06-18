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

RCT <- as.data.frame(total.with.overlap[total.with.overlap$S == 1,])
MEAN.FEMALE.RCT <- mean(as.integer(RCT$gender)) - 1

semi.synthetic.shifted <- data.frame("estimate" = c(),
                                     "set" = c(),
                                     "m" = c(),
                                     "n" = c(),
                                     "pt" = c())

for (i in 1:200){
  
  for (set in c("minimal", "extended - shifted")){
    
    if(set == "minimal"){
      covariate.to.adjust.on <- MINIMAL_SET
    } else {
      covariate.to.adjust.on <- c(MINIMAL_SET, "X.shifted")
    }
    
    
    for (neff in c(3000)){
      for (meff in c(10000)){
        
        for (pteff in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)){
          
          # generate data
          simulation <- simulation.semi.synthetic(n = neff, m = meff, source.data = total.with.overlap, extra.noise.on.high.ttt = F)
          
          
          if (set == "extended - shifted"){
            # Adding a shifted covariates -- the covariate explaining nothing and shifted
            X.RCT <- rbinom(n = neff, 1, MEAN.FEMALE.RCT)
            X.obs <- rbinom(n = meff, 1, pteff)
            
            RCT <- simulation[simulation$S == 1, ]
            RCT$X.shifted <- X.RCT
            Obs <- simulation[simulation$S == 0,]
            Obs$X.shifted <- X.obs
            
            simulation <- rbind(RCT, Obs)
          } 
          
          # estimation
          ipsw <- ipsw.binned(dataframe = simulation, covariates_names_vector = covariate.to.adjust.on,
                              oracle.e = T, oracle.pt = F, oracle.pr = F)
        
          
          new.row <-  data.frame("estimate" = semi.oracle,
                                 "set" = set,
                                 "m" = meff,
                                 "n" = neff,
                                 "pt" = pteff)
          
          semi.synthetic.shifted <- rbind(semi.synthetic.shifted, new.row)
          
        }
      }
    }
  }
}



write.csv(semi.synthetic.shifted, "./results/semi.synthetic.X.shifted.csv", row.names = TRUE)