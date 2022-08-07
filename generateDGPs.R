# toy simulation example in the spirit of Hirano & Imbens
toy.example <- function(n = 1000, m = 1000, 
                        ratio = 0.5, 
                        output.oracles = TRUE,
                        symetric.po = TRUE,
                        var.Y = 1,
                        noisier.var.X1 = FALSE,
                        prop.X1.RCT = 0.75,
                        prop.X1.Target = 0.3,
                        ATE.on.X1 = 10,
                        factor.var = 5){
  
  
  X.RCT <- rbinom(n = n, 1, prop.X1.RCT)
  
  # deal with situation where the randomness leaded to not all X on trial
  while(mean(X.RCT) == 0 | mean(X.RCT) == 1){
    print(paste0("Issue with mean(X.RCT): ", mean(X.RCT)))
    X.RCT <- rbinom(n = n, 1, prop.X1.RCT)
  }
  
  
  X.obs <- rbinom(n = m, 1, prop.X1.Target)

  # deal with situation where the randomness leaded to not all X on observational data
  while(mean(X.obs) == 0 | mean(X.obs) == 1){
    print(paste0("Issue with mean(X.Obs): ", mean(X.obs)))
    X.obs <- rbinom(n = m, 1, prop.X1.Target)
  }
  
  # random treatment assignment within the RCT / Bernoulli trial
  treat.assignment.in.RCT <-  rbinom(n, 1, ratio)
  
  # deal with situation where the randomness leaded to no overlap on trial
  while(mean(treat.assignment.in.RCT) == 0 | mean(treat.assignment.in.RCT) == 1){
    print(paste0("Issue with mean(treat.assignment.in.RCT): ", mean(treat.assignment.in.RCT)))
    treat.assignment.in.RCT <-  rbinom(n, 1, ratio)
  }
  
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

# this is an extension of the toy example with multiple covariates
simulation.multivariate.categorical.X <- function(n = 1000, m = 1000, 
                                                  ratio = 0.5, 
                                                  prop.X1.RCT = 0.75,
                                                  prop.X1.Target = 0.3,
                                                  output.oracles = F){
  
  
  
  # X1 -- the treatment effect modifier
  X1.RCT <- rbinom(n = n, 1, prop.X1.RCT)
  X1.obs <- rbinom(n = m, 1, prop.X1.Target)
  
  X2.RCT <- rnorm(n = n, mean = 0, sd = 2)
  X2.RCT <- floor(X2.RCT)
  X2.obs <- rnorm(n = m, mean = 0, sd = 2)
  X2.obs <- floor(X2.obs)
  
  # random treatment assignment within the RCT / Bernoulli trial
  treat.assignment.in.RCT <-  rbinom(n, 1, ratio)
  
  output <- data.frame("X1" = c(X1.RCT, X1.obs),
                       "X2" = c(X2.RCT, X2.obs),
                       "S" = c(rep(1, n), rep(0, m)),
                       "A" = c(treat.assignment.in.RCT, rep(NA, m)))
  
  error_0 = rnorm(n = nrow(output), mean = 0, sd = 1)
  error_1 = rnorm(n = nrow(output), mean = 0, sd = 1)
  
  Y_0 <- ifelse(output$X1 == 1, 0, 0)
  output$Y_0 <- Y_0 + error_0
  Y_1 <- ifelse(output$X1 == 1, 20, 5) - 10*output$X2
  output$Y_1 <- Y_1 + error_1

  # observed outcome
  output$Y <- ifelse(output$S == 1, ifelse(output$A == 1, output$Y_1, output$Y_0), NA)
  
  if (!output.oracles){
    output <- output[, c("X1", "X2", "A", "Y", "S")]
  }
  
  
  return(output)
}


simulation.semi.synthetic <- function(n = 1000, m = 1000, ratio = 0.5, output.oracles = FALSE, extra.noise.on.high.ttt = FALSE, source.data = NULL, generate.associated.ground.truth = FALSE, non.shifted.treatment.effect.modifier = FALSE){
  
  # load source data
  if(is.null(source.data)){
    source.data <- load("./data/semi-synthetic-DGP.rds")
  }

  # sample from the two populations
  source.RCT <- total.with.overlap[total.with.overlap$S == 1,]
  source.Obs <- total.with.overlap[total.with.overlap$S == 0,]
  
  if(!generate.associated.ground.truth){
    RCT <- source.RCT[sample(1:nrow(source.RCT), n, replace = TRUE), ]
    Obs <- source.Obs[sample(1:nrow(source.Obs), m, replace = TRUE), ]
  } else {
    RCT <- source.RCT
    Obs <- source.Obs
  }
  
  if (non.shifted.treatment.effect.modifier){
    
    # add non shifted treatment effect modifier
    non.shifted.treatment.effect.modifier.RCT <- rnorm(n = n, mean = 0, sd = 2)
    non.shifted.treatment.effect.modifier.RCT <- floor(non.shifted.treatment.effect.modifier.RCT)
    non.shifted.treatment.effect.modifier.Obs <- rnorm(n = m, mean = 0, sd = 2)
    non.shifted.treatment.effect.modifier.Obs <- floor(non.shifted.treatment.effect.modifier.Obs)
    
    RCT$X.treatment.effect.modifier <- non.shifted.treatment.effect.modifier.RCT
    Obs$X.treatment.effect.modifier <- non.shifted.treatment.effect.modifier.Obs
  }
  
  
  total <- rbind(RCT, Obs)
  total <- as.data.frame(total)
  
  
  total$Glasgow.initial <- as.numeric(total$Glasgow.initial)
  
  # Outcome model
  baseline <- (10 - total$Glasgow.initial) - 5*total$gender
  
  cate <-  15*(6-total$time_to_treatment.categorized) + 3*(total$systolicBloodPressure.categorized-1)^2 
  
  if (non.shifted.treatment.effect.modifier){
    cate <- cate + 50*total$X.treatment.effect.modifier
  }
  
  total$Y_0 = baseline 
  total$Y_1 =  baseline + cate
  
  if(generate.associated.ground.truth){
    return(total)
    break
  }
  
  # add gaussian noise
  total$Y_0 = total$Y_0 + rnorm(n+m,  mean = 0, sd = 1)
  total$Y_1 =  total$Y_1 + rnorm(n+m,  mean = 0, sd = 1)
  
  if(extra.noise.on.high.ttt){
    
    # adding extra noise if treatment given too late
    extra.noise.Y_1 <- ifelse(total$time_to_treatment.categorized == 4 | total$time_to_treatment.categorized == 3, rnorm(1,  mean = 0, sd = 4), 0)
    
    total$Y_1 <- total$Y_1 + extra.noise.Y_1
  }
  
  
  
  # random treatment assignment within the RCT / Bernoulli trial
  treat.assignment.in.RCT <-  rbinom(n, 1, ratio)
  
  A = c(treat.assignment.in.RCT, rep(NA, m))
  total$A <- A
  
  # observed outcome
  total$Y <- ifelse(total$S == 1, ifelse(total$A == 1, total$Y_1, total$Y_0), NA)
  
  
  if(!output.oracles){
    
    # remove potential outcomes
    total <- total[, c(names(RCT), "A", "Y")]
    
  } 
  
  return(total)
  
}