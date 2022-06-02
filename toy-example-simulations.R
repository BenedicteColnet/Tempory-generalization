set.seed(123)

library(MASS)

source("./estimators.R")
source("./generateDGPs.R")


toy.examples.varying.n.m <- data.frame("n" = c(),
                                       "m" = c(),
                                       "estimate" = c(),
                                       "estimator" = c())


for ( neff in seq(50, 200, by = 10)){
  for ( meff in seq(5, 50, by = 5)){
    
    print(paste0("Starting n = ", neff, " and m = ", meff))
    
    for (rep in 1:1000){
      simulation <- toy.example(n = neff, 
                                m = meff, 
                                output.oracles = TRUE, 
                                symetric.po = FALSE,
                                noisier.var.X1 = FALSE,
                                prop.X1.RCT = 0.75,
                                prop.X1.Target = 0.5,
                                ATE.on.X1 = 10,
                                factor.var = 0)
      
      # estimation
      oracle.estimate <- ipsw.univariate.and.categorical.X(dataframe = simulation, estimand = "ATE", oracle.e = T, oracle.pt = T, oracle.pr = T)
      estimate.pr.only.estimate <- ipsw.univariate.and.categorical.X(dataframe = simulation, estimand = "ATE", oracle.e = T, oracle.pt = T, oracle.pr = F)
      ipsw.estimate <- ipsw.univariate.and.categorical.X(dataframe = simulation, estimand = "ATE", oracle.e = T, oracle.pt = F, oracle.pr = F)
      
      # new row  
      new.row <- data.frame("n" = rep(neff, 3), "m" = rep(meff, 3), "estimate" = c(oracle.estimate, estimate.pr.only.estimate, ipsw.estimate),  "estimator" = c("oracle", "semi-oracle", "ipsw"))
      toy.examples.varying.n.m <- rbind(toy.examples.varying.n.m, new.row)
    }
  }
}


write.csv(toy.examples.varying.n.m, "./results/toy.example.varying.n.m.csv", row.names = TRUE)