###This module fits a ccODP model to the claims dataset, computing the RMSE and Log score of the resulting fit and comparing to the MDN model


##LOAD LOSS DATA 
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

a_vec <- exp(log_a)

b_vec <- exp(log_b)

a_vec <- a_vec * sum(b_vec)
b_vec <- b_vec/sum(b_vec)


Full_Data[,ccODP := a_vec[AQ]*b_vec[DQ]]

#Save ccODP results, Load MDN results and bind with GLM fit
ccODP <- select(Full_Data, c(AQ, DQ, Loss, ccODP, Test = Test3, Stage1, Stage2, Stage3))
ccODP[,Dispersion := mean(dispersion)]
fwrite(ccODP, paste0(output_directory, "/ccODP.csv"))

MDN = fread(paste0(output_directory,"/Full_Results.csv"))

Comparison <- cbind(ccODP, "MDN" = MDN$predMean , "sigmaMDN" = MDN$sigma)
plotComparison(Comparison, c(c(10,20,30,40)), horizon = 40)

fwrite(Comparison, paste0(output_directory, "/ModelComparisons.csv"))






#Find fit scores of ccODP
quantile_ccODP = function(Table, quantile){
  table1 = as.data.table(Table)
  
  table1[,quantile_est := 0]
  table1 = as.data.frame(table1)
  for (i in 1:nrow(table1)){
    table1$quantile_est[i] = table1$Dispersion[i]*PoissonQuant(quantile, table1$ccODP[i]/table1$Dispersion[i])
    
  }
  table1 = as.data.table(table1)
  
  colnames(table1)[which(colnames(table1) == "quantile_est")] = paste0("CC",quantile*100)
  return (table1)
  
}
quantile_loss_cc = function(Table, quantile){
  table1 = as.data.table(copy(Table))
  table1 = as.data.frame(table1)
  index = which(colnames(table1) == paste0("CC",100*quantile))
  table1$quantile_loss = (table1$Loss - table1[,index])*(quantile - (table1$Loss < table1[,index]))  
  table1 = as.data.table(table1)
  colnames(table1)[which(colnames(table1) == "quantile_loss")] = paste0("CCLoss",100*quantile)
  table1 = as.data.table(table1)
  return (table1)
  
}


ccODP[,log_score := log(dpois(floor(Loss/Dispersion), ccODP/Dispersion)/Dispersion)]
ccODP = quantile_ccODP(ccODP, 0.75)
ccODP = quantile_ccODP(ccODP, 0.995)
ccODP = quantile_loss_cc(ccODP, 0.75)
ccODP = quantile_loss_cc(ccODP, 0.995)

fwrite(ccODP, paste0(output_directory, "/ccODP.csv"))


ccODP_Score = data.frame(
  "Data" = Data_Name,
  "Model" = "ccODP",
  "RMSE" = sqrt(mean((ccODP[Test == 1, ccODP] - ccODP[Test == 1, Loss])^2)),
  "MeanLogScore" = mean(ccODP[Test == 1, log_score]),
  "QuantileScore75" = mean(ccODP[Test == 1, CCLoss75]),
  "QuantileScore99.5" = mean(ccODP[Test == 1, CCLoss99.5])
)

Final_Scores = rbind(Scores, ccODP_Score)
#Save the Final Scores f the MDN and ccODP models
fwrite(Final_Scores, paste0(output_directory, "/Final_Scores.csv"))



