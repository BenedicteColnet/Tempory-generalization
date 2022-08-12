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

finite.sample.semi.oracle <- data.frame("estimate" = c(),
                                        "method" = c(),
                                        "n" = c(),
                                        "m" = c())

for (i in 1:6000){
  
  print(i)
  
  for (neff in c(50, 100, 150, 200, 250, 300, 350)){
    
    # Generate data for oracle and semi oracle
    simulation <- toy.example(n = neff, m = 20, output.oracles = T, noisier.var.X1 = F)
    
    # fully oracle
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = T, oracle.pr = T)
    method = "Completely-oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # semi oracle
    ipsw <-  ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = T, oracle.pr = F)
    method = "Semi-oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # n = m
    simulation <- toy.example(n = neff, m = neff, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "IPSW: m = m"

    new.row <- data.frame("estimate" = ipsw,
                            "method" = method,
                            "n" = neff,
                            "m" = neff)
      
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # m = n^2
    simulation <- toy.example(n = neff, m = neff*neff, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "IPSW: n = √m"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = neff*neff)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # n = m^2
    simulation <- toy.example(n = neff, m = sqrt(neff), output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "IPSW: n = m*m"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = sqrt(neff))
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # n = 2 * m
    simulation <- toy.example(n = neff, m = neff/2, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "IPSW: n = 2m"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = neff/2)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # m = 2* n
    simulation <- toy.example(n = neff, m = 2*neff, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "IPSW: n = m/2"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = 2*neff)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
  }
}


write.csv(finite.sample.semi.oracle, "./results/finite.sample.toy.example.csv", row.names = TRUE)