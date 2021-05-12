#FUNCTIONS_PROCESS MODULE
#This module contains functions for process based operations, including:
# Data partitions
# Calculating scores
# Calculating quantiles



AdjustedSplit = function(Full_Data){
  Full_Data = as.data.table(Full_Data)
  ## Environment 5
  set.seed(1)
  ## PARTITION
  
  ##RESET, REDEFINE PARTITIONS
  Full_Data[,Stage1 := as.numeric(AQ + DQ <= 41)]
  Full_Data[,Stage2 := as.numeric(AQ + DQ <= 41)]
  
  Full_Data[,Train1 := 0]
  Full_Data[,Train2 := 0]
  Full_Data[,Train3 := 0]
  Full_Data[,Train4 := 0]
  
  Full_Data[,Val1 := 0]
  Full_Data[,Val2 := 0]
  Full_Data[,Val3 := 0]
  Full_Data[,Val4 := 0]
  
  Full_Data[,Test1 := 0]
  Full_Data[,Test2 := 0]
  Full_Data[,Test3 := 0]
  Full_Data[,Test4 := 0]
  
  Full_Data[AQ + DQ > 41, Test3 := 1]
  Full_Data[AQ + DQ > 41, Test4 := 1]
  
  
  # Test_Inflation = copy(Full_Data)
  # 
  # moving_average = function(Data){
  #   meanVec = c()
  #   for (i in 1:(length(Data) - 3)){
  #     meanVec[i] = (Data[i + 3] + Data[i+2] + Data[i+1] + Data[i])/4
  #     
  #     
  #   }
  #   return (meanVec)
  #   
  # }
  # 
  # LossData = Full_Data[DQ == 6 & AQ + DQ <= 41,Loss]
  # 
  # meanVec = moving_average(as.vector(LossData))
  # 
  # plot(log(meanVec), type = "l", ylab = "Movign Average of Log Loss", main = "Moving Average of Losses", col = "red", lwd = 3)
  
  
  # Full_Data[AQ + DQ > 41, Test3 := 1]
  # 
  # Full_Data[,Latest := as.numeric(Stage1 & AQ + DQ > 21)]
  # 
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 0, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 1, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 2, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 3, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 4, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 5, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 6, Train3 := 1]
  # Full_Data[Latest == 1 & (AQ + 2*DQ) %% 8 == 7, Val3 := 1]
  # 
  # Full_Data[Latest == 0 & Stage1, Train3 := 1]
  
  #
  
  
  
  
  
  
  
  ##REGIME 2
  
  
  
  Full_Data[,Latest := as.numeric(Stage1 & AQ + DQ > 29)]
  
  valNum = 82
  testNum = 82
  
  LatestSize = nrow(Full_Data[Latest == 1,])
  
  vec1 = rep(0,LatestSize)
  TestInd = sample(c(1:LatestSize), testNum, replace = FALSE)
  
  vec1[TestInd] = 1
  
  Full_Data[Latest == 1, Test1 := vec1]
  
  
  
  
  LatestSize2 = nrow(Full_Data[Test1 == 0 & Latest,])
  
  
  
  vec1 = rep(0,LatestSize2)
  ValInd = sample(c(1:LatestSize2), valNum/2, replace = FALSE)
  
  vec1[ValInd] = 1
  
  Full_Data[Test1 == 0 & Latest, Val1 := vec1]
  
  
  LatestSize3 = nrow(Full_Data[Stage1 & Test1 == 0 & Val1 == 0,])
  
  vec1 = rep(0,LatestSize3)
  ValInd = sample(c(1:LatestSize3), valNum/2, replace = FALSE)
  
  vec1[ValInd] = 1
  
  Full_Data[Stage1 & Test1 == 0 & Val1 == 0, Val1 := vec1]
  
  
  
  
  
  Full_Data[Stage1 & Val1 == 0 & Test1 == 0, Train1 := 1]
  
  
  ##FIRST DONE
  
  ##NOW DO SECOND
  
  
  
  
  
  
  #
  
  
  
  Full_Data[Val1 == 1 & Test1 == 1,Train2 := 1]
  
  
  LatestSize = nrow(Full_Data[Latest == 1 & Train2 == 0,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), testNum, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Latest == 1 & Train2 == 0, Test2 := vec1]
  
  
  
  
  LatestSize = nrow(Full_Data[Latest == 1 & Train2 == 0 & Test2 == 0,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), valNum/2, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Latest == 1 & Train2 == 0 & Test2 == 0, Val2 := vec1]
  
  
  
  
  LatestSize = nrow(Full_Data[Stage2 == 1 & Train2 == 0 & Test2 == 0 & Val2 == 0,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), valNum/2, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Stage2 == 1 & Train2 == 0 & Test2 == 0 & Val2 == 0, Val2 := vec1]
  
  
  Full_Data[Stage2 == 1 & Val2 == 0 & Test2 == 0, Train2 := 1]
  
  
  
  
  ## NOW DOING 3 AND 4
  
  
  LatestSize = nrow(Full_Data[Latest == 1,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), valNum/2, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Latest == 1, Val3 := vec1]
  
  
  
  LatestSize = nrow(Full_Data[Stage1 == 1 & Val3 == 0,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), valNum/2, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Stage1 == 1 & Val3 == 0, Val3 := vec1]
  
  Full_Data[Val3 == 0 & Test3 == 0, Train3 := 1]
  
  
  
  ##4
  
  Full_Data[Val3 == 1, Train4 := 1]
  
  
  LatestSize = nrow(Full_Data[Latest == 1 & Train4 == 0,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), valNum/2, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Latest == 1 & Train4 == 0, Val4 := vec1]
  
  
  
  LatestSize = nrow(Full_Data[Stage1 == 1 & Val4 == 0 & Train4 == 0 ,])
  vec1 = rep(0,LatestSize)
  
  TestInd = sample(c(1:LatestSize), valNum/2, replace = FALSE)
  vec1[TestInd] = 1
  
  Full_Data[Stage1 == 1 & Val4 == 0 & Train4 == 0, Val4 := vec1]
  
  Full_Data[Val4 == 0 & Test4 == 0, Train4 := 1]
  return (Full_Data)
  
}






PoissonQuant = function(quantile, mean){
  
  initial = qpois(quantile, mean) 
  #print(paste0("Initial is ", initial))
  return (initial)
  actualQuant = ppois(initial , mean)
  #print(paste0("ActualQuant is ", actualQuant))
  
  if (actualQuant > quantile){
    nextRef = initial - 1
    
  } else if (actualQuant < quantile) {
    nextRef = initial + 1
  } else {
    return (1+initial)
  }
  #print(paste0("NextRef is ", nextRef))
  
  nextQuant = ppois(nextRef, mean)
  #print(paste0("NextQuant is ", nextQuant))
  initial = initial + 1
  nextRef = nextRef + 1
  
  
  #print((quantile*(initial - nextRef) - initial*nextQuant + nextRef*actualQuant)/(actualQuant - nextQuant))
  
  return (max(0,   (quantile*(initial - nextRef) - initial*nextQuant + nextRef*actualQuant)/(actualQuant - nextQuant)   )  )
  
}

PoissonQuant(0.25, 43000/288)

##DEFINE FUNCTIONS WHICH WILL CALCULATE THE TOTAL MEAN, LOG SCORE AND STANDARD DEVIATION OF THE AVERAGED FIT AMONG THE TRIALS
mean_function <- function(table, start, components,  trials){
  table1 <- as.data.frame(table)
  index = start
  table1$predMean = 0
  num_dist = trials*components
  for (i in 1:num_dist){
    table1$predMean =  table1$predMean + (table1[,index]/trials)*table1[,(index+1)]   
    index = index + 3
    
    
  }
  table1 <- as.data.table(table1)
  return (table1)
}


log_score_function <- function(table, start, components,  trials){
  table1 <- as.data.frame(table)
  index = start
  table1$logScore = 0
  num_dist = trials*components
  for (i in 1:num_dist){
    table1$logScore =  table1$logScore + (table1[,index]/trials)*dnorm(table1$Loss, table1[,(index+1)],table1[,(index+2)])
    index = index + 3
    
    
  }
  table1$logScore <- log(table1$logScore)
  table1 <- as.data.table(table1)
  return (table1)
}


sigma_function <- function(table, start, components,  trials){
  table1 <- as.data.frame(table)
  index = start
  table1$sigma = 0
  num_dist = trials*components
  for (i in 1:num_dist){
    table1$sigma =  table1$sigma + (table1[,index]/trials)*((table1[,index+1]^2) + (table1[,index+2]^2))
    index = index + 3
    
    
  }
  table1$sigma <- sqrt(table1$sigma - table1$predMean^2)
  table1 <- as.data.table(table1)
  return (table1)
}






quantile_prediction <- function(table, start, components, trials, quantile, tolerance, max_iter){
  table1 = as.data.frame(table)
  index = start
  table1$quantile_est = table1$predMean
  table1$jump = table1$sigma
  table1$quantile = 0
  table1$old_quantile = 0
  table1$tol = 1
  num_dist = trials*components
  index = start
  incomplete = abs(table1$tol) > tolerance
  for (i in 1:num_dist){
    alpha = table1[incomplete,index]
    mu =  table1[incomplete,index + 1]
    sigma =  table1[incomplete, index + 2]
    table1$quantile[incomplete] = table1$quantile[incomplete] + (alpha/trials)*pnorm(table1$quantile_est[incomplete], mu, sigma)
    index = index + 3
  }
  table1$old_quantile = table1$quantile
  table1$tol = (table1$quantile - quantile)
  table1$jump[table1$tol > 0] = -1*table1$jump[table1$tol > 0]
  ##INITIAL QUANTILE VALUES FILLED IN 
  ticker = 0
  ##START quantile search
  while (max(abs(table1$tol)) > tolerance && ticker <= max_iter){
    index = start
    incomplete = abs(table1$tol) > tolerance
    
    table1$quantile_est[incomplete] = table1$quantile_est[incomplete] + table1$jump[incomplete]
    table1$quantile[incomplete] = 0
    for (i in 1:num_dist){
      alpha = table1[incomplete,index]
      mu =  table1[incomplete,index + 1]
      sigma =  table1[incomplete, index + 2]
      table1$quantile[incomplete] = table1$quantile[incomplete] + (alpha/trials)*pnorm(table1$quantile_est[incomplete], mu, sigma)
      index = index + 3
    }
    ##NEW QUANTILE VALUES CALCULATED
    table1$tol[incomplete] = (table1$quantile[incomplete] - quantile)
    went_above = table1$old_quantile - quantile < 0 & table1$quantile - quantile > 0
    went_below = table1$old_quantile - quantile > 0 & table1$quantile - quantile < 0
    table1$jump[c(went_above & incomplete)] = -0.5 * table1$jump[c(went_above & incomplete)]
    table1$jump[c(went_below & incomplete)] = -0.5 * table1$jump[c(went_below & incomplete)]
    ticker = ticker + 1
    table1$old_quantile[incomplete] = table1$quantile[incomplete]
  }
  
  colnames(table1)[which(colnames(table1)== "quantile_est")] = paste0("MDN", quantile*100)
  
  table1 <- as.data.table(table1)
  
  table1[ ,`:=`(tol = NULL, quantile = NULL, old_quantile = NULL, jump = NULL)]
  return (table1)  
  
  
}


quantile_loss = function(table, quantile){
  table1 = as.data.frame(table)
  index = which(colnames(table1) == paste0("MDN", quantile*100))
  table1$quantile_loss = (table1$Loss - table1[,index])*(quantile - (table1$Loss < table1[,index]))  
  table1 = as.data.table(table1)
  colnames(table1)[which(colnames(table1) == "quantile_loss")] = paste0("MDNLoss",100*quantile)
  return (table1)
}










##DEFINE FUNCTIONS WHICH WILL CALCULATE THE TOTAL MEAN, LOG SCORE AND STANDARD DEVIATION OF THE AVERAGED FIT AMONG THE TRIALS
mean_function_log <- function(table, start, components,  trials){
  table1 <- as.data.frame(table)
  
  index = start
  table1$predMean = 0
  num_dist = trials*components
  for (i in 1:num_dist){
    alpha = table1[,index]
    mu =  table1[,index + 1]
    sigma =  table1[,index + 2]
    
    table1$predMean =  table1$predMean + (table1[,index]/trials)*exp(table1[,index + 1] + 0.5*table1[,index + 2]*table1[,index + 2])
    index = index + 3
    
    
  }
  table1 <- as.data.table(table1)
  return (table1)
}




log_score_function_log <- function(table, start, components,  trials){
  table1 <- as.data.frame(table)
  index = start
  table1$logScore = 0
  num_dist = trials*components
  for (i in 1:num_dist){
    alpha = table1[,index]
    mu =  table1[,index + 1]
    sigma =  table1[,index + 2]
    table1$logScore =  table1$logScore + (alpha/trials)*dlnorm(table1$Loss, mu,sigma)
    index = index + 3
    
    
  }
  table1$logScore <- log(table1$logScore)
  table1 <- as.data.table(table1)
  return (table1)
}


sigma_function_log <- function(table, start, components,  trials){
  table1 <- as.data.frame(table)
  index = start
  table1$sigma = 0
  num_dist = trials*components
  for (i in 1:num_dist){
    alpha = table1[,index]
    mu =  table1[,index + 1]
    sigma =  table1[,index + 2]
    table1$sigma =  table1$sigma + (alpha/trials)*(exp(sigma^2) )*exp(2*mu + sigma^2) 
    index = index + 3
    
    
  }
  table1$sigma <- sqrt(table1$sigma - table1$predMean^2)
  table1 <- as.data.table(table1)
  return (table1)
}












quantile_prediction_log <- function(table, start, components, trials, quantile, tolerance, max_iter){
  table1 = as.data.frame(table)
  index = start
  table1$quantile_est = table1$predMean
  table1$jump = table1$sigma
  table1$quantile = 0
  table1$old_quantile = 0
  table1$tol = 1
  num_dist = trials*components
  index = start
  incomplete = abs(table1$tol) > tolerance
  for (i in 1:num_dist){
    alpha = table1[incomplete,index]
    mu =  table1[incomplete,index + 1]
    sigma =  table1[incomplete, index + 2]
    table1$quantile[incomplete] = table1$quantile[incomplete] + (alpha/trials)*pnorm(table1$quantile_est[incomplete], mu, sigma)
    index = index + 3
  }
  table1$old_quantile = table1$quantile
  table1$tol = (table1$quantile - quantile)
  table1$jump[table1$tol > 0] = -1*table1$jump[table1$tol > 0]
  ##INITIAL QUANTILE VALUES FILLED IN 
  ticker = 0
  ##START quantile search
  while (max(abs(table1$tol)) > tolerance && ticker <= max_iter){
    index = start
    incomplete = abs(table1$tol) > tolerance
    
    table1$quantile_est[incomplete] = table1$quantile_est[incomplete] + table1$jump[incomplete]
    table1$quantile[incomplete] = 0
    for (i in 1:num_dist){
      alpha = table1[incomplete,index]
      mu =  table1[incomplete,index + 1]
      sigma =  table1[incomplete, index + 2]
      table1$quantile[incomplete] = table1$quantile[incomplete] + (alpha/trials)*pnorm(table1$quantile_est[incomplete], mu, sigma)
      index = index + 3
    }
    ##NEW QUANTILE VALUES CALCULATED
    table1$tol[incomplete] = (table1$quantile[incomplete] - quantile)
    went_above = table1$old_quantile - quantile < 0 & table1$quantile - quantile > 0
    went_below = table1$old_quantile - quantile > 0 & table1$quantile - quantile < 0
    table1$jump[c(went_above & incomplete)] = -0.5 * table1$jump[c(went_above & incomplete)]
    table1$jump[c(went_below & incomplete)] = -0.5 * table1$jump[c(went_below & incomplete)]
    ticker = ticker + 1
    table1$old_quantile[incomplete] = table1$quantile[incomplete]
  }
  table1$quantile_est = exp(table1$quantile_est)
  colnames(table1)[which(colnames(table1)== "quantile_est")] = paste0("MDN", quantile*100)
  
  table1 <- as.data.table(table1)
  
  table1[ ,`:=`(tol = NULL, quantile = NULL, old_quantile = NULL, jump = NULL)]
  return (table1)  
  
  
}









CreateEnsemble = function(trial1, trial2, trial3, trial4, trial5, components){
  col_names = c()
  num_dist = 5*components
  for (i in 1:num_dist){
    col_names = c(col_names, paste0(c("a","u","s"),i))
  }
  
  base <- trial1[,.(AQ, DQ, Loss)]
  
  
  t1 <- copy(trial1)
  
  if (components == 1){
    t1 = select(t1, c(a1, u1, s1))
  } else if (components == 2){
    t1 = select(t1, c(a1, u1, s1, a2, u2, s2))
    
  } else if (components == 3){
    t1 = select(t1, c(a1, u1, s1, a2, u2, s2, a3, u3, s3))
  } else if (components == 4){
    t1 = select(t1, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4))
    
    
  } else if (components == 5){
    t1 = select(t1, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4, a5, u5, s5))
    
    
  }
  
  
  t2 = copy(trial2)
  if (components == 1){
    t2 = select(t2, c(a1, u1, s1))
    colnames(t2) <- c("a2", "u2",  "s2")
    t2 = select(t2, c(a2, u2, s2))
  } else if (components == 2){
    t2 = select(t2, c(a1, a2, u1, u2, s1, s2))
    colnames(t2) <- c("a3", "a4", "u3", "u4",  "s3", "s4")
    t2 = select(t2, c(a3, u3, s3, a4, u4, s4))
  } else if (components == 3){
    t2 = select(t2, c(a1, u1, s1, a2, u2, s2, a3, u3, s3))
    colnames(t2) <- c("a4", "u4","s4", "a5", "u5", "s5", "a6", "u6","s6")
  } else if (components == 4){
    t2 = select(t2, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4))
    colnames(t2) <- c("a5", "u5","s5", "a6", "u6", "s6", "a7", "u7","s7", "a8", "u8", "s8")
  } else if (components == 5){
    t2 = select(t2, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4, a5, u5, s5))
    colnames(t2) <- col_names[16:30]
    
    
  }
  
  
  
  t3 = copy(trial3)
  if (components == 1){
    t3 = select(t3, c(a1, u1, s1))
    colnames(t3) <- c("a3", "u3", "s3")
  } else if (components == 2){
    t3 = select(t3, c(a1, a2, u1, u2, s1, s2))
    colnames(t3) <- c("a5", "a6", "u5", "u6", "s5", "s6")
    t3 = select(t3, c(a5, u5, s5, a6, u6, s6))
  } else if (components == 3){
    t3 = select(t3, c(a1, u1, s1, a2, u2, s2, a3, u3, s3))
    colnames(t3) <- c("a7", "u7", "s7", "a8", "u8", "s8", "a9", "u9", "s9")
    
  } else if (components == 4){
    t3 = select(t3, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4))
    colnames(t3) <- c("a9", "u9","s9", "a10", "u10", "s10", "a11", "u11","s11", "a12", "u12", "s12")
  } else if (components == 5){
    t3 = select(t3, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4, a5, u5, s5))
    colnames(t3) <- col_names[31:45]
    
    
  }
  
  
  
  t4 = copy(trial4)
  if (components == 1){
    t4 = select(t4, c(a1, u1, s1))
    colnames(t4) <- c("a4", "u4", "s4")
  }
  
  
  if (components == 2){
    t4 = select(t4, c(a1, a2, u1, u2, s1, s2))
    colnames(t4) <- c("a7", "a8", "u7", "u8", "s7", "s8")
    t4 = select(t4, c(a7, u7, s7, a8, u8, s8))
  } else if (components == 3){
    t4 = select(t4, c(a1, u1, s1, a2, u2, s2, a3, u3, s3))
    colnames(t4) <- c("a10", "u10", "s10", "a11", "u11", "s11", "a12", "u12", "s12")
    
  } else if (components == 4){
    t4 = select(t4, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4))
    colnames(t4) <- c("a13", "u13","s13", "a14", "u14", "s14", "a15", "u15","s15", "a16", "u16", "s16")
  } else if (components == 5){
    t4 = select(t4, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4, a5, u5, s5))
    colnames(t4) <- col_names[46:60]
    
    
  }
  
  
  t5 = copy(trial5)
  if (components == 1){
    t5 = select(t5, c(a1, u1, s1))
    colnames(t5) <- c("a5", "u5", "s5")
  }
  if (components == 2){
    t5 = select(t5, c(a1, a2, u1, u2, s1, s2))
    colnames(t5) <- c("a9", "a10", "u9", "u10", "s9", "s10")
    t5 = select(t5, c(a9, u9, s9, a10, u10, s10))
  } else if (components == 3){
    t5 = select(t5, c(a1, u1, s1, a2, u2, s2, a3, u3, s3))
    colnames(t5) <- c("a13", "u13", "s13", "a14", "u14", "s14", "a15", "u15", "s15")
    
  } else if (components == 4){
    t5 = select(t5, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4))
    colnames(t5) <- c("a17", "u17","s17", "a18", "u18", "s18", "a19", "u19","s19", "a20", "u20", "s20")
  } else if (components == 5){
    t5 = select(t5, c(a1, u1, s1, a2, u2, s2, a3, u3, s3, a4, u4, s4, a5, u5, s5))
    colnames(t5) <- col_names[61:75]
    
    
  }
  
  
  
  
  
  return (cbind(base, t1, t2, t3, t4, t5))
  
  
  
}












MeanSDNormal = function(Results, components, scale_vec){
  
  conv_table = copy(Results)
  
  conv_table[,u1:= u1*scale_vec[6] + scale_vec[5]]
  if (components > 1){
    conv_table[,u2:= u2*scale_vec[6] + scale_vec[5]]
  }
  if (components > 2){
    conv_table[,u3:= u3*scale_vec[6] + scale_vec[5]]
  }
  if (components > 3){
    conv_table[,u4:= u4*scale_vec[6] + scale_vec[5]]
  }
  
  if (components > 4){
    conv_table[,u5:= u5*scale_vec[6] + scale_vec[5]]
  }
  
  
  
  conv_table[,s1:= s1*scale_vec[6]]
  if (components > 1){
    conv_table[,s2:= s2*scale_vec[6]]
  }
  if (components > 2){
    conv_table[,s3:= s3*scale_vec[6]]
  }
  if (components > 3){
    conv_table[,s4:= s4*scale_vec[6]]
  }
  if (components > 4){
    conv_table[,s5:= s5*scale_vec[6]]
  }
  
  
  #Find mean and standard deviation of Mixture Density
  conv_table[,predMean := a1*u1]
  if (components > 1){
    conv_table[,predMean := predMean + a2*u2 ]
  }
  
  if (components > 2){
    conv_table[,predMean := predMean + a3*u3]
  }
  if (components > 3){
    conv_table[,predMean := predMean + a4*u4]
  }
  if (components > 4){
    conv_table[,predMean := predMean + a5*u5]
  }
  
  
  
  if (components == 1){
    conv_table[,sigma := s1]
    
  }
  
  
  if (components == 2){
    conv_table[,sigma := sqrt(a1*(s1^2 + u1^2) + a2*(s2^2 + u2^2) - predMean^2)]
  } else if (components == 3){
    conv_table[,sigma := sqrt(a1*(s1^2 + u1^2) + a2*(s2^2 + u2^2)+ a3*(s3^2 + u3^2) - predMean^2)]
    
  } else if (components == 4){
    conv_table[,sigma := sqrt(a1*(s1^2 + u1^2) + a2*(s2^2 + u2^2)+ a3*(s3^2 + u3^2)+ a4*(s4^2 + u4^2) - predMean^2)]
    
    
  } else if (components == 5){
    conv_table[,sigma := sqrt(a1*(s1^2 + u1^2) + a2*(s2^2 + u2^2)+ a3*(s3^2 + u3^2)+ a4*(s4^2 + u4^2) + a5*(s5^2 + u5^2) - predMean^2)]
    
  }
  
  
  return (conv_table)
  
}








MeanSDLog = function(Results, components){
  
  conv_table = as.data.table(Results)
  
  conv_table[,Loss := exp(Loss)]
  
  if (components >= 1){
    conv_table[,predMean := a1*exp(u1+0.5*s1^2)]
    conv_table[,sigma := a1*((exp(s1^2) - 1)*exp(2*u1 + s1^2) + exp(2*u1+s1^2))]
  }
  
  if (components >= 2){
    conv_table[,predMean := predMean + a2*exp(u2+0.5*s2^2)]
    conv_table[,sigma := sigma + a2*((exp(s2^2) - 1)*exp(2*u2 + s2^2) + exp(2*u2+s2^2))]
    
  }
  
  if (components >= 3){
    conv_table[,predMean := predMean +  a3*exp(u3+0.5*s3^2)]
    conv_table[,sigma := sigma + a3*((exp(s3^2) - 1)*exp(2*u3 + s3^2) + exp(2*u3+s3^2))]
    
  }
  if (components >= 4){
    conv_table[,predMean := predMean +  a4*exp(u4+0.5*s4^2)]
    conv_table[,sigma := sigma + a4*((exp(s4^2) - 1)*exp(2*u4 + s4^2) + exp(2*u4+s4^2))]
    
  }
  
  if (components >= 5){
    conv_table[,predMean := predMean +  a5*exp(u5+0.5*s5^2)]
    conv_table[,sigma := sigma + a5*((exp(s5^2) - 1)*exp(2*u5 + s5^2) + exp(2*u5+s5^2))]
    
  }
  
  
  conv_table[,sigma := sqrt(sigma - predMean^2)]
  
  return (conv_table)
  
}


# test_data = copy(conv_table)
# 
# test_data[,predMean := a1*exp(u1+0.5*s1^2)]
# test_data[,sigma := a1*((exp(s1^2) - 1)*exp(2*u1 + s1^2) + exp(2*u1+s1^2))]
# 
# 
# test_data[,sigma := sqrt(sigma - predMean^2)]
# 
# 
# exp(2.68^2)*exp(18.5+2.68^2)
# 
# 
# 
# sims = rnorm(100000, 9.28, 1)
# 
# mean(exp(sims))+sd(exp(sims))
# 
# quantiles = 0.01*c(1:99)
# 
# q_vec = c()
# 
# for (i in 1:length(quantiles)){
#   q_vec[i] = quantile(exp(sims), quantiles[i])
#   
#   
# }
# 
# plot(quantiles, q_vec)
# abline(h = mean(exp(sims))+sd(exp(sims)) )






DataSplit = function(Full_Data){
  Full_Data = as.data.table(Full_Data)
  
  Full_Data[,AQ := as.numeric(AQ)]
  Full_Data[,DQ := as.numeric(DQ)]
  Full_Data[,Loss := as.numeric(Loss)]
  
  
  ###Partition Data into Training, Val and Test sets (2 stage partition)
  
  Stage1_size <- 30 #number of AQs in first stage partition
  Val1_size <- 4 #number of calendar periods for validation (excluding corner cells)
  Full_Data[,Stage1 := as.numeric(AQ <= Stage1_size & DQ <= Stage1_size & (AQ + DQ <= 41))]
  Full_Data[,Train1 := as.numeric(Stage1 & (AQ + DQ <= Stage1_size - Val1_size + 1))]
  
  
  
  Full_Data[,Val1 := as.numeric((AQ + DQ <= Stage1_size + 1) & ! Train1)]
  Full_Data[,Test1 := as.numeric(Stage1 & !Train1 & ! Val1)]
  
  Full_Data[Val1 & DQ <= 3, Train1 := 1]
  Full_Data[Val1 & AQ <= 3, Train1 := 1]
  Full_Data[Train1 == 1, Val1 := 0]
  
  #Assign DQ2 and DQ3 validation points to earlier AQs to provide more training data for later DQs (heatmap will visualise)
  limit = Stage1_size - Val1_size - 3
  Vec2 = round(limit*c(1:4)/4)
  Vec3 = Vec2 - 3
  Full_Data[AQ %in% Vec2 & DQ == 2,Val1 := 1]
  Full_Data[AQ %in% Vec3 & DQ == 3,Val1 := 1]
  Full_Data[Val1 == 1, Train1 := 0]
  
  
  #Stage 2 Partition
  Stage2_size <- 36
  Val2_size <- 4
  
  Full_Data[,Stage2 := as.numeric(AQ <= Stage2_size & DQ <= Stage2_size & (AQ + DQ <= 41))]
  
  Full_Data[,Train2 := as.numeric(Stage2 & (AQ + DQ <= Stage2_size - Val2_size + 1))]
  
  Full_Data[,Val2 := as.numeric((AQ + DQ <= Stage2_size + 1) & ! Train2)]
  Full_Data[,Test2 := as.numeric(Stage2 & !Train2 & ! Val2)]
  
  Full_Data[Val2 & DQ <= 3, Train2 := 1]
  Full_Data[Val2 & AQ <= 3, Train2 := 1]
  Full_Data[Train2 == 1, Val2 := 0]
  
  limit = Stage2_size - Val2_size - 3
  Vec2 = round(limit*c(1:4)/4)
  Vec3 = Vec2 - 3
  Full_Data[AQ %in% Vec2 & DQ == 2,Val2 := 1]
  Full_Data[AQ %in% Vec3 & DQ == 3,Val2 := 1]
  Full_Data[Val2 == 1, Train2 := 0]
  
  
  
  #Full Data Partition (for running the final model on), called Partition 3 in this workspace
  
  Stage3_size <- 40
  Val3_size <- 4
  
  Full_Data[,Stage3 := as.numeric(AQ <= Stage3_size & DQ <= Stage3_size )]
  
  Full_Data[,Train3 := as.numeric(Stage3 & (AQ + DQ <= Stage3_size - Val3_size + 1))]
  
  Full_Data[,Val3 := as.numeric((AQ + DQ <= Stage3_size + 1) & ! Train3)]
  Full_Data[,Test3 := as.numeric(Stage3 & !Train3 & ! Val3)]
  
  Full_Data[Val3 & DQ <= 3, Train3 := 1]
  Full_Data[Val3 & AQ <= 3, Train3 := 1]
  Full_Data[Train3 == 1, Val3 := 0]
  
  
  ### REMOVE DQ40 
  #Full_Data[AQ == 1 & DQ ==  40, Train3 := 0]
  
  ###
  
  
  limit = Stage3_size - Val3_size - 3
  Vec2 = round(limit*c(1:4)/4)
  Vec3 = Vec2 - 3
  Full_Data[AQ %in% Vec2 & DQ == 2,Val3 := 1]
  Full_Data[AQ %in% Vec3 & DQ == 3,Val3 := 1]
  Full_Data[Val3 == 1, Train3 := 0]
  
  if (Adjusted_Partition){
    
    Full_Data = AdjustedSplit(Full_Data)
  }
  
  
  return (Full_Data)
  
}






Normalise = function(Full_Data){
  Full_Data= as.data.table(Full_Data)
  
  #AQ
  
  
  Full_Data[,AQMean := mean(Full_Data[AQ + DQ <= 41,AQ])]
  Full_Data[,AQStdev := sd(Full_Data[AQ + DQ <= 41,AQ])]
  
  #DQ
  
  
  Full_Data[,DQMean := mean(Full_Data[AQ + DQ <= 41,DQ])]
  Full_Data[,DQStdev := sd(Full_Data[AQ + DQ <= 41,DQ])]
  
  #Loss Normalisation
  
  
  Full_Data[,LossMean := mean(Full_Data[AQ + DQ <= 41,Loss])]
  Full_Data[,LossStdev := sd(Full_Data[AQ + DQ <= 41,Loss])]
  
  #Normalised columns
  
  
  Full_Data[,AQNorm := (AQ - AQMean)/AQStdev]
  Full_Data[,DQNorm := (DQ - DQMean)/DQStdev]
  Full_Data[,LossNorm := (Loss - LossMean)/LossStdev]
  
  ###CATEGORICAL MAPPING FOR RESNET
  Full_Data[,Categorical := 40*(DQ-1) + AQ - 1]
  
  return (Full_Data)
  
}





Save_Tr_Va_Te = function(Full_Data, output_directory){
  Tr1 <- select(Full_Data[Train1 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Tr1) <- c("AQ","DQ","Loss")
  
  Val1 <- select(Full_Data[Val1 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Val1) <- c("AQ","DQ","Loss")
  
  Test1 <- select(Full_Data[Test1 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Test1) <- c("AQ","DQ","Loss")
  
  Tr2 <- select(Full_Data[Train2 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Tr2) <- c("AQ","DQ","Loss")
  
  Val2 <- select(Full_Data[Val2 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Val2) <- c("AQ","DQ","Loss")
  
  Test2 <- select(Full_Data[Test2 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Test2) <- c("AQ","DQ","Loss")
  
  Tr3 <- select(Full_Data[Train3 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Tr3) <- c("AQ","DQ","Loss")
  
  Val3 <- select(Full_Data[Val3 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Val3) <- c("AQ","DQ","Loss")
  
  Test3 <- select(Full_Data[Test3 == 1,],AQNorm, DQNorm, LossNorm )
  colnames(Test3) <- c("AQ","DQ","Loss")
  
  
  if (Adjusted_Partition){
    Tr4 <- select(Full_Data[Train4 == 1,],AQNorm, DQNorm, LossNorm )
    colnames(Tr4) <- c("AQ","DQ","Loss")
    
    Val4 <- select(Full_Data[Val4 == 1,],AQNorm, DQNorm, LossNorm )
    colnames(Val4) <- c("AQ","DQ","Loss")
    
    Test4 <- select(Full_Data[Test4 == 1,],AQNorm, DQNorm, LossNorm )
    colnames(Test4) <- c("AQ","DQ","Loss")
    
    
    write.csv(Tr4, paste0(output_directory , "/Train4.csv"))
    write.csv(Val4, paste0(output_directory , "/Validation4.csv"))
    write.csv(Test4, paste0(output_directory , "/Test4.csv"))
    
    
  }
  
  
  
  
  write.csv(Tr1, paste0(output_directory , "/Train1.csv"))
  write.csv(Val1, paste0(output_directory , "/Validation1.csv"))
  write.csv(Test1, paste0(output_directory , "/Test1.csv"))
  
  write.csv(Tr2, paste0(output_directory , "/Train2.csv"))
  write.csv(Val2, paste0(output_directory , "/Validation2.csv"))
  write.csv(Test2, paste0(output_directory , "/Test2.csv"))
  
  write.csv(Tr3, paste0(output_directory , "/Train3.csv"))
  write.csv(Val3, paste0(output_directory , "/Validation3.csv"))
  write.csv(Test3, paste0(output_directory , "/Test3.csv"))
  
  
  
  
  
  
}





##Heatmap to visualise partitions

heatmap_set <- function(data, partition){
  if (partition == 1){
    
    ggplot(Full_Data, aes(AQ, DQ)) + geom_tile(aes(fill = Set1)) + scale_fill_manual(values=c("red", "green",  "dark green"))+
      coord_flip()+
      scale_x_reverse()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=20,face="bold"), plot.title = element_text(size = 24, face = "bold"),
            legend.title = element_text(size = 18),
            legend.text = element_text( size = 18))+
      ggtitle("Diagram of Partition 1")+ guides(fill=guide_legend(title="Partition"))
    
    
    
    
    
    
    
  } else if (partition == 2){
    
    ggplot(Full_Data, aes(AQ, DQ)) + geom_tile(aes(fill = Set2)) + scale_fill_manual(values=c("red", "green",  "dark green"))+
      coord_flip()+
      scale_x_reverse()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=20,face="bold"), plot.title = element_text(size = 24, face = "bold"),
            legend.title = element_text(size = 18),
            legend.text = element_text( size = 18))+
      ggtitle("Diagram of Partition 2")+ guides(fill=guide_legend(title="Partition"))
    
    
    
    
    
  } else if (partition == 3){
    
    ggplot(Full_Data, aes(AQ, DQ)) + geom_tile(aes(fill = Set3)) + scale_fill_manual(values=c("green",  "dark green"))+
      coord_flip()+
      scale_x_reverse()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=20,face="bold"), plot.title = element_text(size = 24, face = "bold"),
            legend.title = element_text(size = 18),
            legend.text = element_text( size = 18))+
      ggtitle("Diagram of Partition 3")+ guides(fill=guide_legend(title="Partition"))
    
    
    
    
    
    
  } else {
    
    ggplot(Full_Data, aes(AQ, DQ)) + geom_tile(aes(fill = Set4)) + scale_fill_manual(values=c("green",  "dark green"))+
      coord_flip()+
      scale_x_reverse()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=20,face="bold"), plot.title = element_text(size = 24, face = "bold"),
            legend.title = element_text(size = 18),
            legend.text = element_text( size = 18))+
      ggtitle("Diagram of Partition 4")+ guides(fill=guide_legend(title="Partition"))
    
    
    
    
  }
  
  
}











heatmap_zeros = function(Full_Data){
  Partition <- matrix(0,nrow = 40, ncol = 40)
  
  for (i in 1:40){
    for (j in 1:40){
      cell <- Full_Data[AQ == i,] 
      cell <- cell[DQ == j,]
      
      Partition[i,j] <- (cell$Loss == 0) 
      
    }
    
  }
  Partition <- as.data.table(Partition)
  AQ <- c(1:40)
  Partition <- cbind(AQ, Partition)
  colnames(Partition) <- c("AQ",c(1:40))
  
  
  Partition_melt <- reshape2::melt(Partition, id.vars = "AQ")
  
  colnames(Partition_melt) <- c("AQ","DQ","Set")
  
  
  ggplot(Partition_melt, aes(AQ, DQ, fill= Set)) + 
    geom_tile() +
    scale_fill_gradient(low="white", high="blue") 
  
  
  
}




