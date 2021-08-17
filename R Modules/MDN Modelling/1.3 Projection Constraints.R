#################################
# 1.3 PROJECTION CONSTRAINTS ####
#################################
# This module:
# - Adds upper and lower bounds to mean estimates in specified cells in the lower triangle
# - Splits the constrained cells evenly into training and validation sets
# - Saves new losses which include the constraints


#The code is replicated for each constrained environment




if (Environment == 4 && Model_Name == 'MDN'){
  #Load loss data
  Full_Data <- fread(paste0(output_directory, "/Full_Data.csv"))
  Full_Data <- as.data.table(Full_Data)
  
  #Add constraints - specify which cells need constraints
  AQs = c(30:40)
  DQs = c(38:40)
  table = expand.grid(AQs, DQs)
  colnames(table) = c("AQ","DQ")
  table = as.data.table(table)
  table = as.data.frame(table)
  lower = c()
  upper = c()
  
  #Loop through constrained cells, specify upper and lower bounds
  for (i in 1:nrow(table)){
    aq = table[i,1]
    dq = table[i,2]
    
    lower[i] = 0
    upper[i] = 250000
    
  }
  table = as.data.table(table)
  table[,Lower := lower]
  table[,Upper := upper]
  
  Constraints =  table
  
  Full_Data[,Constrained := 0]
  Full_Data[,Lower := 0]
  Full_Data[,Upper := 0]
  
  #Add constraint information to the triangle data
  for (i in 1:nrow(Constraints)){
    AQ_current = Constraints$AQ[i]
    DQ_current = Constraints$DQ[i]
    Lower_current = Constraints$Lower[i]
    Upper_current = Constraints$Upper[i]
    #Add identifier that the cell is constrained
    Full_Data[AQ == AQ_current & DQ == DQ_current, Constrained := 1]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Lower := Lower_current]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Upper := Upper_current]
    
  }
  
  #Normalise bounds
  scale_vec <- (c(mean(Full_Data$AQMean), mean(Full_Data$AQStdev), mean(Full_Data$DQMean), mean(Full_Data$DQStdev), mean(Full_Data$LossMean), mean(Full_Data$LossStdev)))
  
  Full_Data[,LowerNorm := 0]
  Full_Data[,UpperNorm := 0]
  
  Full_Data[Constrained == 1, LowerNorm := (Lower - scale_vec[5])/scale_vec[6]]
  Full_Data[Constrained == 1, UpperNorm := (Upper - scale_vec[5])/scale_vec[6]]
  
  
  #Re-write partition 1 and 2 (in the paper we only applied constraints when fitting the final model)
  #However, all data needs to have the same response structure
  column_names = c("AQ", "DQ", "Loss", "Constrained", "Lower","Upper")
  Tr1 <- select(Full_Data[Train1 == 1,],AQNorm, DQNorm, Categorical, LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr1) <- column_names
  
  Val1 <- select(Full_Data[Val1 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val1) <- column_names
  
  Test1 <- select(Full_Data[Test1 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test1) <- column_names
  
  Tr2 <- select(Full_Data[Train2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr2) <- column_names
  
  Val2 <- select(Full_Data[Val2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val2) <- column_names
  
  Test2 <- select(Full_Data[Test2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test2) <- column_names

  #Split constrained cells in half, 1 set for training (so the network learns them) and one set for validation (so the network stops training when the constraints are met)
  
  second_set = sample(c(1:nrow(Constraints)), nrow(Constraints)/2, replace = FALSE)
  splitter = rep(1, nrow(Constraints))
  splitter[second_set] = 2
  
  
  Full_Data[Constrained == 1, Splitter := splitter]
  
  Full_Data[Constrained == 1 & Splitter == 1, Train3 := 1]
  Full_Data[Constrained == 1 & Splitter == 2, Val3 := 1]
  
  
  
  
  Tr3 <- select(Full_Data[Train3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr3) <- column_names
  
  Val3 <- select(Full_Data[Val3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val3) <- column_names
  
  Test3 <- select(Full_Data[Test3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test3) <- column_names
  
  
  #Save updated training/validation/testing data
  write.csv(Tr1, paste0(output_directory , "/Train1.csv"))
  write.csv(Val1, paste0(output_directory , "/Validation1.csv"))
  write.csv(Test1, paste0(output_directory , "/Test1.csv"))
  
  write.csv(Tr2, paste0(output_directory , "/Train2.csv"))
  write.csv(Val2, paste0(output_directory , "/Validation2.csv"))
  write.csv(Test2, paste0(output_directory , "/Test2.csv"))
  
  write.csv(Tr3, paste0(output_directory , "/Train3.csv"))
  write.csv(Val3, paste0(output_directory , "/Validation3.csv"))
  write.csv(Test3, paste0(output_directory , "/Test3.csv"))
  
  if (Adjusted_Partition){
    Full_Data[Constrained == 1 & Splitter == 1, Train4 := 1]
    Full_Data[Constrained == 1 & Splitter == 2, Val4 := 1]
    Tr4 <- select(Full_Data[Train4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Tr4) <- column_names
    
    Val4 <- select(Full_Data[Val4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Val4) <- column_names
    
    Test4 <- select(Full_Data[Test4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Test4) <- column_names
    write.csv(Tr4, paste0(output_directory , "/Train4.csv"))
    write.csv(Val4, paste0(output_directory , "/Validation4.csv"))
    write.csv(Test4, paste0(output_directory , "/Test4.csv"))
    
  }
  
  fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))
  

}






if (Environment == 3 && Model_Name == 'ResMDN'){
 
  Full_Data <- fread(paste0(output_directory, "/Full_Data.csv"))
  Full_Data <- as.data.table(Full_Data)
  

  
  AQs = c(30:40)
  DQs = c(38:40)
  table = expand.grid(AQs, DQs)
  colnames(table) = c("AQ","DQ")
  table = as.data.table(table)
  table = as.data.frame(table)
  lower = c()
  upper = c()
  for (i in 1:nrow(table)){
    aq = table[i,1]
    dq = table[i,2]
    ccodp_est = Full_Data[AQ == aq & DQ == dq,ccODP]
    lower[i] = 0
    upper[i] = 150000
    
    
    
  }
  table = as.data.table(table)
  table[,Lower := lower]
  table[,Upper := upper]
  
  
  
  
  
  Constraints =  table
  
  Full_Data[,Constrained := 0]
  Full_Data[,Lower := 0]
  Full_Data[,Upper := 0]
  
  
  for (i in 1:nrow(Constraints)){
    AQ_current = Constraints$AQ[i]
    DQ_current = Constraints$DQ[i]
    Lower_current = Constraints$Lower[i]
    Upper_current = Constraints$Upper[i]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Constrained := 1]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Lower := Lower_current]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Upper := Upper_current]
    
  }
  
  
  
  scale_vec <- (c(mean(Full_Data$AQMean), mean(Full_Data$AQStdev), mean(Full_Data$DQMean), mean(Full_Data$DQStdev), mean(Full_Data$LossMean), mean(Full_Data$LossStdev)))
  
  Full_Data[,LowerNorm := 0]
  Full_Data[,UpperNorm := 0]
  
  Full_Data[Constrained == 1, LowerNorm := (Lower - scale_vec[5])/scale_vec[6]]
  Full_Data[Constrained == 1, UpperNorm := (Upper - scale_vec[5])/scale_vec[6]]
  
  
  
  
  
  
  
  
  column_names = c("AQ", "DQ",'Categorical', "Loss", "Constrained", "Lower","Upper")
  Tr1 <- select(Full_Data[Train1 == 1,],AQNorm, DQNorm, Categorical, LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr1) <- column_names
  
  Val1 <- select(Full_Data[Val1 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val1) <- column_names
  
  Test1 <- select(Full_Data[Test1 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test1) <- column_names
  
  Tr2 <- select(Full_Data[Train2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr2) <- column_names
  
  Val2 <- select(Full_Data[Val2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val2) <- column_names
  
  Test2 <- select(Full_Data[Test2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test2) <- column_names
  
  
  
 
  second_set = sample(c(1:nrow(Constraints)), nrow(Constraints)/2, replace = FALSE)
  splitter = rep(1, nrow(Constraints))
  splitter[second_set] = 2
  
  
  Full_Data[Constrained == 1, Splitter := splitter]
  
  Full_Data[Constrained == 1 & Splitter == 1, Train3 := 1]
  Full_Data[Constrained == 1 & Splitter == 2, Val3 := 1]
  
  
  
  
  Tr3 <- select(Full_Data[Train3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr3) <- column_names
  
  Val3 <- select(Full_Data[Val3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val3) <- column_names
  
  Test3 <- select(Full_Data[Test3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test3) <- column_names
  
  write.csv(Tr1, paste0(output_directory , "/Train1.csv"))
  write.csv(Val1, paste0(output_directory , "/Validation1.csv"))
  write.csv(Test1, paste0(output_directory , "/Test1.csv"))
  
  write.csv(Tr2, paste0(output_directory , "/Train2.csv"))
  write.csv(Val2, paste0(output_directory , "/Validation2.csv"))
  write.csv(Test2, paste0(output_directory , "/Test2.csv"))
  
  write.csv(Tr3, paste0(output_directory , "/Train3.csv"))
  write.csv(Val3, paste0(output_directory , "/Validation3.csv"))
  write.csv(Test3, paste0(output_directory , "/Test3.csv"))
  
  if (Adjusted_Partition){
    Full_Data[Constrained == 1 & Splitter == 1, Train4 := 1]
    Full_Data[Constrained == 1 & Splitter == 2, Val4 := 1]
    Tr4 <- select(Full_Data[Train4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Tr4) <- column_names
    
    Val4 <- select(Full_Data[Val4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Val4) <- column_names
    
    Test4 <- select(Full_Data[Test4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Test4) <- column_names
    write.csv(Tr4, paste0(output_directory , "/Train4.csv"))
    write.csv(Val4, paste0(output_directory , "/Validation4.csv"))
    write.csv(Test4, paste0(output_directory , "/Test4.csv"))
    
  }
  
  fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))
  
  
  
  
  
  
  
}










if (Environment == 2 && Model_Name == 'ResMDN'){
  Full_Data <- fread(paste0(output_directory, "/Full_Data.csv"))
  Full_Data <- as.data.table(Full_Data)
  
  

  
  AQs = c(30:40)
  DQs = c(38:40)

  table = expand.grid(AQs, DQs)
  colnames(table) = c("AQ","DQ")
  table = as.data.table(table)
  table = as.data.frame(table)
  lower = c()
  upper = c()
  for (i in 1:nrow(table)){
    aq = table[i,1]
    dq = table[i,2]
    lower[i] = 0
    upper[i] = 500000

    
  }
  table = as.data.table(table)
  table[,Lower := lower]
  table[,Upper := upper]
  
  
  
  
  
  Constraints =  table
  
  Full_Data[,Constrained := 0]
  Full_Data[,Lower := 0]
  Full_Data[,Upper := 0]
  
  
  for (i in 1:nrow(Constraints)){
    AQ_current = Constraints$AQ[i]
    DQ_current = Constraints$DQ[i]
    Lower_current = Constraints$Lower[i]
    Upper_current = Constraints$Upper[i]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Constrained := 1]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Lower := Lower_current]
    Full_Data[AQ == AQ_current & DQ == DQ_current, Upper := Upper_current]
    
  }
  
  
  
  scale_vec <- (c(mean(Full_Data$AQMean), mean(Full_Data$AQStdev), mean(Full_Data$DQMean), mean(Full_Data$DQStdev), mean(Full_Data$LossMean), mean(Full_Data$LossStdev)))
  
  Full_Data[,LowerNorm := 0]
  Full_Data[,UpperNorm := 0]
  
  Full_Data[Constrained == 1, LowerNorm := (Lower - scale_vec[5])/scale_vec[6]]
  Full_Data[Constrained == 1, UpperNorm := (Upper - scale_vec[5])/scale_vec[6]]
  
  
  
  
  
  
  
  
  column_names = c("AQ", "DQ",'Categorical', "Loss", "Constrained", "Lower","Upper")
  Tr1 <- select(Full_Data[Train1 == 1,],AQNorm, DQNorm, Categorical, LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr1) <- column_names
  
  Val1 <- select(Full_Data[Val1 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val1) <- column_names
  
  Test1 <- select(Full_Data[Test1 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test1) <- column_names
  
  Tr2 <- select(Full_Data[Train2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr2) <- column_names
  
  Val2 <- select(Full_Data[Val2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val2) <- column_names
  
  Test2 <- select(Full_Data[Test2 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test2) <- column_names
  
  
  
  second_set = sample(c(1:nrow(Constraints)), nrow(Constraints)/2, replace = FALSE)
  splitter = rep(1, nrow(Constraints))
  splitter[second_set] = 2
  
  
  Full_Data[Constrained == 1, Splitter := splitter]
  
  Full_Data[Constrained == 1 & Splitter == 1, Train3 := 1]
  Full_Data[Constrained == 1 & Splitter == 2, Val3 := 1]
  
  
  
  
  Tr3 <- select(Full_Data[Train3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Tr3) <- column_names
  
  Val3 <- select(Full_Data[Val3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Val3) <- column_names
  
  Test3 <- select(Full_Data[Test3 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
  colnames(Test3) <- column_names
  
  
  write.csv(Tr1, paste0(output_directory , "/Train1.csv"))
  write.csv(Val1, paste0(output_directory , "/Validation1.csv"))
  write.csv(Test1, paste0(output_directory , "/Test1.csv"))
  
  write.csv(Tr2, paste0(output_directory , "/Train2.csv"))
  write.csv(Val2, paste0(output_directory , "/Validation2.csv"))
  write.csv(Test2, paste0(output_directory , "/Test2.csv"))
  
  write.csv(Tr3, paste0(output_directory , "/Train3.csv"))
  write.csv(Val3, paste0(output_directory , "/Validation3.csv"))
  write.csv(Test3, paste0(output_directory , "/Test3.csv"))
  
  if (Adjusted_Partition){
    Full_Data[Constrained == 1 & Splitter == 1, Train4 := 1]
    Full_Data[Constrained == 1 & Splitter == 2, Val4 := 1]
    Tr4 <- select(Full_Data[Train4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Tr4) <- column_names
    
    Val4 <- select(Full_Data[Val4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Val4) <- column_names
    
    Test4 <- select(Full_Data[Test4 == 1,],AQNorm, DQNorm, Categorical,LossNorm,  Constrained, LowerNorm, UpperNorm )
    colnames(Test4) <- column_names
    write.csv(Tr4, paste0(output_directory , "/Train4.csv"))
    write.csv(Val4, paste0(output_directory , "/Validation4.csv"))
    write.csv(Test4, paste0(output_directory , "/Test4.csv"))
    
  }
  
  fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))
  
  
}
  
  
  
  
  
  



