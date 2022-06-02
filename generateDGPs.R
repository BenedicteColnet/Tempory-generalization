# toy simulation example in the spirit of Hirano & Imbens
toy.example <- function(n = 1000, m = 1000, 
                        ratio = 0.5, 
                        output.oracles = TRUE,
                        symetric.po = TRUE,
                        var.Y = 2,
                        noisier.var.X1 = TRUE,
                        prop.X1.RCT = 0.75,
                        prop.X1.Target = 0.5,
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