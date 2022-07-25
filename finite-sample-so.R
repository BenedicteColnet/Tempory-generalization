source("./estimators.R")
source("./generateDGPs.R")


set.seed(123)
options(dplyr.summarise.inform = FALSE)

# libraries
library(tidyr) # pivot longer or wider
library(dplyr) # case_when and others


finite.sample.semi.oracle <- data.frame("estimate" = c(),
                                        "method" = c(),
                                        "n" = c(),
                                        "m" = c())

for (i in 1:100){
  
  print(i)
  
  for (neff in seq(25, 325, by = 25)){
    
    
    # Generate data for oracle and semi oracle
    simulation <- toy.example(n = neff, m = 100, output.oracles = T, symetric.po = F, noisier.var.X1 = F)
    
    # fully oracle
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = F, oracle.pt = T, oracle.pr = T)
    method = "oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    # semi oracle
    ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = F, oracle.pt = T, oracle.pr = F)
    method = "semi.oracle"
    
    new.row <- data.frame("estimate" = ipsw,
                          "method" = method,
                          "n" = neff,
                          "m" = NA)
    
    finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    
    for (meff in seq(25, 325, by = 25)) {
      
      # Generate data
      simulation <- toy.example(n = neff, m = meff, output.oracles = T, symetric.po = F, noisier.var.X1 = F)
      
      ipsw <- ipsw.univariate.and.categorical.X(dataframe = simulation, oracle.e = F, oracle.pt = F, oracle.pr = F)
      method = "ipsw"
      new.row <- data.frame("estimate" = ipsw,
                            "method" = method,
                            "n" = neff,
                            "m" = meff)
      
      finite.sample.semi.oracle <- rbind(finite.sample.semi.oracle, new.row)
    }
  }
}


write.csv(finite.sample.semi.oracle, "./results/finite.sample.so.csv", row.names = TRUE)