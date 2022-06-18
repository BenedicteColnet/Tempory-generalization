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
                        oracle.pr.data = NULL,
                        oracle.e.data = 0.5){
  
  RCT <- dataframe[dataframe$S == 1,]
  n = nrow(RCT)
  
  Obs <- dataframe[dataframe$S == 0,]
  m = nrow(Obs)
  
  if(!oracle.pt){
    
    # Obs - count number of observations per observed categories
    pt <-  Obs %>% 
      group_by(across(covariates_names_vector)) %>%
      summarise(pt = n())
    
    # check if some categories are observed in trial but not in target
    trial_modalities <- unique(RCT[, covariates_names_vector])
    
    # for modalities only present in trial, pt will be filled with NA
    pt <- merge(pt, trial_modalities, by = covariates_names_vector, all.x = F, all.y = T)
    
    pt$pt <- ifelse(is.na(pt$pt), 1, pt$pt)
    
    m.effectif <- sum(pt$pt)
    pt$pt <- pt$pt/m.effectif
    
    
  } else if (oracle.pt){
    
    if(is.null(oracle.pt.data)){
      print("For oracle pt, provide oracle.pt.data")
      break
    }
    
    m_tot = sum(oracle.pt.data$count)
    pt <- oracle.pt.data[, c(covariates_names_vector, "count")]
    pt <- pt %>% group_by_at(covariates_names_vector) %>% summarize(pt = sum(count)/m_tot)
  } 
  
  # add the estimates in the RCT
  RCT <- merge(RCT, pt, by = covariates_names_vector)
  
  if(!oracle.pr){
    
    # RCT - count number of observations per observed categories
    pr <-  RCT %>% 
      group_by(across(covariates_names_vector)) %>%
      summarise(pr = n())
    
    # # check if some categories are observed in target but not in trial
    # target_modalities <- unique(Obs[, covariates_names_vector])
    # 
    # # for modalities only present in target, pr will be filled with NA
    # pr <- merge(pr, target_modalities, by = covariates_names_vector, all.x = F, all.y = T)
    # 
    # # assume there is at least one observation for each of those modalities
    # pr$pr <- ifelse(is.na(pr$pr), 0, pr$pr)
    
    pr$pr <- pr$pr/n
    
    
  } else if (oracle.pr){
    
    if(is.null(oracle.pr.data)){
      print("For oracle pr, provide oracle.pt.data")
      break
    }
    
    n_tot = sum(oracle.pr.data$count)
    pr <- oracle.pr.data[, c(covariates_names_vector, "count")]
    pr <- pr %>% group_by_at(covariates_names_vector) %>% summarize(pr = sum(count)/n_tot)
    
  } 
  
  # add the estimates in the RCT
  RCT <- merge(RCT, pr, by = covariates_names_vector)
  
  
  Y = RCT[, outcome_name]
  A = RCT[, treatment_name]
  r <- RCT$pt / RCT$pr
  
  if (oracle.e){
    
    eff = ifelse(RCT[, treatment_name] == 1, 1/oracle.e.data, (-1)/(1-oracle.e.data))
    
  } else {
    
    
    eff <- RCT[, c(covariates_names_vector)] %>% 
      group_by_at(covariates_names_vector) %>% 
      summarize(eff = mean(A)) # TODO: change for outcome name
    
    eff <- merge(RCT, eff, by = covariates_names_vector)
    eff$eff <- ifelse(eff[, treatment_name] == 1, 1/eff$eff, (-1)/(1-eff$eff))
    eff <- eff$eff
    
  }
  
  
  estimate <- Y*r*eff
  estimate <- sum(estimate)/n
  
  return(estimate)
}
