difference.in.means <- function(dataframe, oracle.e.too = TRUE, estimand = "ATE"){
  rct <- dataframe[dataframe$S == 1,]
  Y = rct$Y
  A = rct$A
  
  if (oracle.e.too){
    e = 0.5
  } else {
    e = mean(rct$A)
  }
  
  # estimation
  if (estimand == "ATE"){
    estimate = mean(Y * (A/e - (1-A)/(1-e)))
  } else if (estimand == "Y1") {
    estimate = mean(Y *A/e)
  }  else if (estimand == "Y0") {
    estimate = mean(Y *(1-A)/(1-e))
  } else {
    print("Error in estimand, should be ATE, Y1, or Y0")
    break
  }
  
  return(estimate)
}


# toy example: ipsw estimating the ratio estimator with univariate and binary X
ipsw.univariate.and.categorical.X <- function(dataframe, 
                                              estimand = "ATE", 
                                              oracle.e = F, 
                                              oracle.pt = F, 
                                              oracle.pr = F){
  
  # extract RCT and relevant quantities
  rct <- dataframe[dataframe$S == 1,]
  Y = rct$Y
  A = rct$A
  X = rct$X
  
  if (oracle.e){
    e.hat.X1 = 0.5
    e.hat.X0 = 0.5
    
  } else {
    e.hat.X1 = mean(rct[rct$X == 1, "A"])
    e.hat.X0 = mean(rct[rct$X == 0, "A"])
  }
  
  
  e.hat <- ifelse(rct$X == 1, e.hat.X1, e.hat.X0)
  
  if (!oracle.pt & !oracle.pr){
    
    r.X.1 <- mean(dataframe[dataframe$S == 0, "X"]) / mean(dataframe[dataframe$S == 1, "X"])
    r.X.0 <- (1- mean(dataframe[dataframe$S == 0, "X"]))/ (1-mean(dataframe[dataframe$S == 1, "X"]))
    r.X <- ifelse(rct$X == 1, r.X.1, r.X.0)
    
  } else if (!oracle.pt & oracle.pr){
    
    if(!("pr" %in% colnames(dataframe)) ) {
      print("Error: should provide a data set with oracles quantities to proceed with option 'oracle.r'.")
      break
    }
    
    r.X.1 <- mean(dataframe[dataframe$S == 0, "X"]) / rct[rct$X == 1, "pr"][[1]]
    r.X.0 <- (1- mean(dataframe[dataframe$S == 0, "X"]))/   rct[rct$X == 0, "pr"][[1]]
    r.X <- ifelse(rct$X == 1, r.X.1, r.X.0)
    
  } else if  (oracle.pt & !oracle.pr){
    
    if(!("pt" %in% colnames(dataframe))) {
      print("Error: should provide a data set with oracles quantities to proceed with option 'oracle.r'.")
      break
    }
    
    r.X.1 <- rct[rct$X == 1, "pt"][[1]] / mean(dataframe[dataframe$S == 1, "X"])
    r.X.0 <-  rct[rct$X == 0, "pt"][[1]] / (1-mean(dataframe[dataframe$S == 1, "X"]))
    r.X <- ifelse(rct$X == 1, r.X.1, r.X.0)
    
  } else { # full oracle
    
    if(!("pt" %in% colnames(dataframe)) | !("pr" %in% colnames(dataframe)) ) {
      print("Error: should provide a data set with oracles quantities to proceed with option 'oracle.r'.")
      break
    }
    
    r.X <- rct$pt /  rct$pr
    
  }
  
  mean.Y1 = mean(A*Y*r.X/e.hat)
  mean.Y0 = mean((1-A)*Y*r.X/(1-e.hat))
  
  # estimation
  if (estimand == "ATE"){
    estimate = mean.Y1 - mean.Y0
  } else if (estimand == "Y1") {
    estimate = mean.Y1
  }  else if (estimand == "Y0") {
    estimate = mean.Y0
  } else {
    print("Error in estimand, should be ATE, Y1, or Y0")
    break
  }
  
  
  return(estimate)
}


ipsw.binned <- function(dataframe,
                        covariates_names_vector,
                        outcome_name = "Y",
                        treatment_name = "A",  
                        oracle.e = T, 
                        oracle.pt = F, 
                        oracle.pr = F,
                        oracle.pt.data = NULL,
                        oracle.pr.data = NULL){
  
  RCT <- dataframe[dataframe$S == 1,]
  n = nrow(RCT)
  
  Obs <- dataframe[dataframe$S == 0,]
  m = nrow(Obs)
  
  if(!oracle.pt){
    
    # remove oracle if any in RCT
    RCT = RCT[,!(names(RCT) %in% c("pt"))]
    
    # Obs - count proportion of each of the covariates
    pt.hat <-  Obs %>% 
      group_by(across(covariates_names_vector)) %>%
      summarise(pt = n()/m)
    
    # add the estimates in the table
    RCT <- merge(RCT, pt.hat, by = covariates_names_vector)
    
  }
  
  if(!oracle.pr){
    
    # remove oracle if any in RCT
    RCT = RCT[,!(names(RCT) %in% c("pr"))]
    
    # RCT - count proportion of each of the covariates
    pr.hat <-  RCT %>% 
      group_by(across(covariates_names_vector)) %>%
      summarise(pr = n()/n)
    
    # add the estimates in the table
    RCT <- merge(RCT, pr.hat, by = covariates_names_vector)
  }
  
  
  Y = RCT$Y
  A = RCT$A
  r <- RCT$pt / RCT$pr
  
  if (oracle.e){
    
    e = unique(RCT$e)[[1]]
    
  } else {
    print("careful, not implemented yet")
  }
  
  
  estimate <- Y*r*( (A/e) - (1-A)/(1-e))
  estimate <- sum(estimate)/n
  
  return(estimate)
}