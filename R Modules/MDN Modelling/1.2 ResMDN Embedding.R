###########################
# 1.2 ResMDN Embedding ####
###########################
# This module:
# - Fits a ccODP model to the triangle
# - Approximates the ccODP to a mixed Gaussian
# - Uses this approximation as the GLM backbone of the ResMDN


#Load triangle 
Full_Data <- fread(paste0(output_directory, "/Full_Data.csv"))
Full_Data <- as.data.table(Full_Data)
Data <- select(Full_Data, c(AQ, DQ, Loss, Stage1, Stage2, Stage3))
if (Distribution == "Log"){
  Data[,Loss := exp(Loss)]
  Full_Data[,Loss := exp(Loss)]
  print("Taken exponent!")
}
Data[,Loss := round(Loss)]

#Split data into upper (train) and lower (test) triangles, then fit a ccODP
Train = Data[AQ + DQ <= 41,]
glm_model <- glm(Loss ~ as.factor(AQ) + as.factor(DQ) - 1, data = Train, family = quasipoisson(link = "log"))

#Store alpha, beta and dispersion parameters
dispersion <- summary(glm_model)$dispersion

log_a <- as.numeric(glm_model$coefficients[1:40])
log_b <- as.numeric(c(0, glm_model$coefficients[41:79]))

#Adjust last accident parameter if too low
if (log_a[40] < mean(log_a)){
  log_a[40] = mean(log_a[37:39])
  
}
a_vec <- exp(log_a)
b_vec <- exp(log_b)

a_vec <- a_vec * sum(b_vec)
b_vec <- b_vec/sum(b_vec)

Full_Data[,ccODP := a_vec[AQ]*b_vec[DQ]]

#Save ccODP fit and reload
ccODP <- select(Full_Data, c(AQ, DQ, Loss, ccODP, Test = Test3, Stage1, Stage2, Stage3))
ccODP[,Dispersion := mean(dispersion)]
fwrite(ccODP, paste0(output_directory, "/ccODP.csv"))


if (Initialisation_Methodology == "Single Gaussian"){
#Single Gaussian
  ccODP = fread(paste0(output_directory, "/ccODP.csv"))

  ExtractNorms = fread(paste0(output_directory, "/Full_Data.csv"))
  first_index = which(colnames(ExtractNorms) == "AQMean")
  ExtractNorms = as.data.frame(ExtractNorms)
  Norms = ExtractNorms[1,c(first_index:ncol(ExtractNorms))]
#Create mapping by taking the mean and volatility of the ccODP
#and spreading the weights across the K components
  Mapping = copy(ccODP)
  Mapping = select(Mapping, c(AQ, DQ, ccODP, Dispersion))
  Mapping[,alpha := 0]
  if (components > 1){
    for (i in 1:(components - 1)){
      Parameters = select(Mapping, c( alpha))
      colnames(Parameters) = c(paste0("alpha", i+1))
      Mapping = cbind(Mapping, Parameters)
    }
  }
  
  Mapping[,mu := (ccODP - Norms$LossMean)/Norms$LossStdev]
  
  if (components > 1){
    for (i in 1:(components - 1)){
      Parameters = select(Mapping, c( mu))
      colnames(Parameters) = c(paste0("mu", i+1))
      Mapping = cbind(Mapping, Parameters)
    }
  }
  
  
  Mapping[,sigma := log((sqrt(ccODP*dispersion))/Norms$LossStdev)]
  
  if (components > 1){
    for (i in 1:(components - 1)){
      Parameters = select(Mapping, c( sigma))
      colnames(Parameters) = c(paste0("sigma", i+1))
      Mapping = cbind(Mapping, Parameters)
    }
  }
  
  
  Mapping = select(Mapping, -c(AQ, DQ, ccODP, Dispersion))

  fwrite(Mapping, paste0(output_directory, "/ResMDNMapping.csv"))
  
} else if (Initialisation_Methodology == "mixtools"){
#Sample from the ccODP distribution
#Approximate the samples to a Mixed Gaussian distribution
  ccODP = fread(paste0(output_directory, "/ccODP.csv"))
  
  
  ExtractNorms = fread(paste0(output_directory, "/Full_Data.csv"))
  ExtractNorms = as.data.frame(ExtractNorms)
  Norms = ExtractNorms[1,]
  meanLoss = Norms$LossMean
  stdevLoss = Norms$LossStdev
  
  samples = 1000 #no of samples of the ccODP
  
  ccODP <- as.data.frame(ccODP)
  
  for (i in 1:nrow(ccODP)){
    print(i)
    simulations <- dispersion*(rpois(samples, ccODP[i,"ccODP"]/dispersion) + runif(samples))
    approx = normalmixEM((simulations), k = components ,maxit = 1000, arbvar = TRUE, arbmean = TRUE)
    
    approx$lambda = log(approx$lambda)
    approx$mu = (approx$mu - meanLoss)/stdevLoss
    approx$sigma=  log(approx$sigma/stdevLoss)
    
    
    if (i == 1){
      table = c(as.vector(approx$lambda[order(approx$lambda)]), as.vector(approx$mu[order(approx$lambda)]), as.vector(approx$sigma[order(approx$lambda)]))
    } else {
      row = c(as.vector(approx$lambda[order(approx$lambda)]), as.vector(approx$mu[order(approx$lambda)]), as.vector(approx$sigma[order(approx$lambda)]))
      table = rbind(table, row)
    }
    
  }
  table <- as.data.table(table)
   for (i in 1:components){
    colnames(table)[i] = paste0("a", i)
    colnames(table)[i + components] = paste0("u", i)
    colnames(table)[i + 2*components] = paste0("s", i)
  }
  
  
  fwrite(table,paste0(output_directory, "/ResMDNMapping.csv") )
  
  
  
  
  
  
}


###Saving the new Training, Validation and Test data (adding categorical input)
Full_Data = fread(paste0(output_directory, "/Full_Data.csv"))


column_names = c("AQ", "DQ", "Categorical", "Loss")
Tr1 <- select(Full_Data[Train1 == 1,],AQNorm, DQNorm,Categorical,  LossNorm )
colnames(Tr1) <- column_names

Val1 <- select(Full_Data[Val1 == 1,],AQNorm, DQNorm,Categorical,  LossNorm )
colnames(Val1) <- column_names

Test1 <- select(Full_Data[Test1 == 1,],AQNorm, DQNorm, Categorical,LossNorm )
colnames(Test1) <- column_names

Tr2 <- select(Full_Data[Train2 == 1,],AQNorm, DQNorm, Categorical,LossNorm )
colnames(Tr2) <- column_names

Val2 <- select(Full_Data[Val2 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
colnames(Val2) <- column_names

Test2 <- select(Full_Data[Test2 == 1,],AQNorm, DQNorm, Categorical,LossNorm )
colnames(Test2) <- column_names

Tr3 <- select(Full_Data[Train3 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
colnames(Tr3) <- column_names

Val3 <- select(Full_Data[Val3 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
colnames(Val3) <- column_names

Test3 <- select(Full_Data[Test3 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
colnames(Test3) <- column_names

if (Adjusted_Partition){
  Tr4 <- select(Full_Data[Train4 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
  colnames(Tr4) <- column_names
  
  Val4 <- select(Full_Data[Val4 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
  colnames(Val4) <- column_names
  
  Test4 <- select(Full_Data[Test4 == 1,],AQNorm, DQNorm,Categorical, LossNorm )
  colnames(Test4) <- column_names
  
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






