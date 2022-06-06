# toy simulation example in the spirit of Hirano & Imbens
toy.example <- function(n = 1000, m = 1000, 
                        ratio = 0.5, 
                        output.oracles = TRUE,
                        symetric.po = TRUE,
                        var.Y = 2,
                        noisier.var.X1 = TRUE,
                        prop.X1.RCT = 0.75,
                        prop.X1.Target = 0.3,
                        ATE.on.X1 = 10,
                        factor.var = 5){
  
  
  X.RCT <- rbinom(n = n, 1, prop.X1.RCT)
  X.obs <- rbinom(n = m, 1, prop.X1.Target)
  
  # random treatment assignment within the RCT / Bernoulli trial
  treat.assignment.in.RCT <-  rbinom(n, 1, ratio)
  
  output <- data.frame("X" = c(X.RCT, X.obs),
                       "S" = c(rep(1, n), rep(0, m)),
                       "A" = c(treat.assignment.in.RCT, rep(NA, m)))
  
  
  if (symetric.po){
    error_0 = rnorm(n = nrow(output), mean = 0, sd = var.Y)
    error_1 = rnorm(n = nrow(output), mean = 0, sd = var.Y)
    output$Y_0  = ifelse(output$X == 1, -ATE.on.X1/2, -2.5)
    output$Y_0 <- output$Y_0 + error_0
    output$Y_1  = ifelse(output$X == 1, ATE.on.X1/2, 2.5)
    output$Y_1 <- output$Y_1 + error_1
    
  } else {
    error_0 = rnorm(n = nrow(output), mean = 0, sd = var.Y)
    error_1 = rnorm(n = nrow(output), mean = 0, sd = var.Y)
    output$Y_0  = ifelse(output$X == 1, 0, 0)
    output$Y_0 <- output$Y_0 + error_0
    output$Y_1  = ifelse(output$X == 1, ATE.on.X1, 5)
    output$Y_1 <- output$Y_1 + error_1
    
  }
  
  if (noisier.var.X1){
    error_sup_Y1 = rnorm(n = nrow(output), mean = 0, sd = factor.var*var.Y)
    error_sup_Y0 = 0
    output$Y_1  = ifelse(output$X == 1, output$Y_1 + error_sup_Y1,  output$Y_1)
    output$Y_0  = ifelse(output$X == 1, output$Y_0 + error_sup_Y0,  output$Y_0)
  }
  
  # observed outcome
  output$Y <- ifelse(output$S == 1, ifelse(output$A == 1, output$Y_1, output$Y_0), NA)
  
  if(output.oracles){
    
    # add oracle quantities
    output$e <- ifelse(output$S == 1, ratio, NA)
    output$pt <- ifelse(output$S == 1, ifelse(output$X == 1, prop.X1.Target, (1-prop.X1.Target)), NA)
    output$pr <- ifelse(output$S == 1, ifelse(output$X == 1, prop.X1.RCT, (1-prop.X1.RCT)), NA)
    
    
  } else {
    # keep only the interesting subset of covariates
    output <- output[, c("X", "A", "Y", "S")]
  }
  
  return(output)
}

simulation.multivariate.categorical.X <- function(n = 1000, m = 1000, 
                                                  ratio = 0.5, 
                                                  output.oracles = TRUE,
                                                  prop.X1.RCT = 0.75,
                                                  prop.X1.Target = 0.3){
  
  
  
  # X1 -- the treatment effect modifier
  X1.RCT <- rbinom(n = n, 1, prop.X1.RCT)
  X1.obs <- rbinom(n = m, 1, prop.X1.Target)
  
  # X2 -- the covariate explaining nothing and not shifted
  X2.RCT <- rbinom(n = n, 1, 0.1)
  X2.obs <- rbinom(n = m, 1, 0.1)
  
  # X3 -- the covariate explaining Y but not shifted
  X3.RCT <- rbinom(n = n, 1, 0.2)
  X3.obs <- rbinom(n = m, 1, 0.2)
  
  # X4 -- strongly shifted covariates, but not effect on Y at all
  X4.RCT <- rbinom(n = n, 1, 0.2)
  X4.obs <- rbinom(n = m, 1, 0.7)
  
  # X5 -- the covariate explaining Y and shifted
  X5.RCT <- rbinom(n = n, 1, 0.2)
  X5.obs <- rbinom(n = m, 1, 0.8)
  
  # random treatment assignment within the RCT / Bernoulli trial
  treat.assignment.in.RCT <-  rbinom(n, 1, ratio)
  
  output <- data.frame("X1" = c(X1.RCT, X1.obs),
                       "X2" = c(X2.RCT, X2.obs),
                       "X3" = c(X3.RCT, X3.obs),
                       "X4" = c(X4.RCT, X4.obs),
                       "X5" = c(X5.RCT, X5.obs),
                       "S" = c(rep(1, n), rep(0, m)),
                       "A" = c(treat.assignment.in.RCT, rep(NA, m)))
  
  
  error_0 = rnorm(n = nrow(output), mean = 0, sd = 1)
  error_1 = rnorm(n = nrow(output), mean = 0, sd = 1)
  
  # Additional error due to X3
  error_0_X3 = rnorm(n = nrow(output), mean = 0, sd = 1)
  error_1_X3 = rnorm(n = nrow(output), mean = 0, sd = 1)
  
  # Additional error due to X5
  error_0_X5 = rnorm(n = nrow(output), mean = 0, sd = 1)
  error_1_X5 = rnorm(n = nrow(output), mean = 0, sd = 1)
  
  
  Y_0 <- ifelse(output$X1 == 1, 0, 0) + ifelse(output$X3 == 1, 100 + error_0_X3, error_0_X3) + ifelse(output$X5 == 1, -100 + error_0_X5, error_0_X5)
  output$Y_0 <- Y_0 + error_0
  Y_1 <- ifelse(output$X1 == 1, 10, 5) +  ifelse(output$X3 == 1, 100 + error_1_X3 , error_1_X3) + ifelse(output$X5 == 1, -100 + error_1_X5, error_1_X5)
  output$Y_1 <- Y_1 + error_1
  
  
  # observed outcome
  output$Y <- ifelse(output$S == 1, ifelse(output$A == 1, output$Y_1, output$Y_0), NA)
  
  if(output.oracles){
    
    # add oracle quantities
    output$e <- ifelse(output$S == 1, ratio, NA)
    output$pt <- ifelse(output$X1 == 1, prop.X1.Target, 1-prop.X1.Target)
    output$pt <- ifelse(output$S == 1, output$pt, NA)
    output$pr <- ifelse(output$X1 == 1, prop.X1.RCT,  1-prop.X1.RCT)
    output$pr <- ifelse(output$S == 1, output$pr, NA)
    
    
    
  } else {
    # keep only the interesting subset of covariates
    output <- output[, c("X1", "X2", "X3", "X4", "X5", "A", "Y", "S")]
  }
  
  return(output)
}