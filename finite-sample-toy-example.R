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

for (i in 1:1000){
  
  print(i)
  
  for (neff in c(50, 100, 250, 500)){
    
    # Generate data for oracle and semi oracle
    simulation <- toy.example(n = neff, m = 20, output.oracles = T, noisier.var.X1 = F)
    
    # fully oracle
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = T, oracle.pr = T)
    method = "oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # semi oracle
    ipsw <-  ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = T, oracle.pr = F)
    method = "semi.oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # n = m
    simulation <- toy.example(n = neff, m = neff, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - m = n"

    new.row <- data.frame("estimate" = ipsw,
                            "method" = method,
                            "n" = neff,
                            "m" = neff)
      
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # m >> n
    simulation <- toy.example(n = neff, m = 5*neff, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - m >> n"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = 5*neff)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    
    # n = 10 m
    simulation <- toy.example(n = neff, m = neff/5, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - n > m"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = neff/5)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # n >> m
    simulation <- toy.example(n = neff, m = neff/10, output.oracles = T, noisier.var.X1 = F)
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = T, oracle.pt = F, oracle.pr = F)
    method = "ipsw - n >> m"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = neff/10)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
  }
}


write.csv(finite.sample.semi.oracle, "./results/finite.sample.toy.example.csv", row.names = TRUE)