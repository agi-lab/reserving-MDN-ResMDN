###########################
# 3.3 ccODP Comparison ####
###########################
# This module:
# - Fits a ccODP model to the triangle
# - Computes the RMSE, log score and quantile scores of the ccODP fit
# - Compares the ccODP to the MDN/ResMDN model

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
#Adjust the last accident parameter if it's too low
if (log_a[40] < mean(log_a)){
  log_a[40] = mean(log_a[37:39])
  
}

a_vec <- exp(log_a)
b_vec <- exp(log_b)

a_vec <- a_vec * sum(b_vec)
b_vec <- b_vec/sum(b_vec)

#Compute the mean
Full_Data[,ccODP := a_vec[AQ]*b_vec[DQ]]

#Save ccODP results, load MDN results and bind with ccODP to compare
ccODP <- select(Full_Data, c(AQ, DQ, Loss, ccODP, Test = Test3, Stage1, Stage2, Stage3))
ccODP[,Dispersion := mean(dispersion)]
fwrite(ccODP, paste0(output_directory, "/ccODP.csv"))

MDN = fread(paste0(output_directory,"/Full_Results.csv"))

Comparison <- cbind(ccODP, "MDN" = MDN$predMean , "sigmaMDN" = MDN$sigma)

#Plot the MDN alongside the ccODP
plotComparison(Comparison, c(c(10,20,30,40)), horizon = 40)

fwrite(Comparison, paste0(output_directory, "/ModelComparisons.csv"))






#Calculate RMSE, log score and quantile scores of the ccODP
ccODP[,log_score := log(dpois(floor(Loss/Dispersion), ccODP/Dispersion)/Dispersion)]
#Set the log score at a minimum of -50, to reduce the influence of low-volume data points
ccODP[log_score < -50, log_score := -50]
ccODP = quantile_ccODP(ccODP, 0.75)
ccODP = quantile_ccODP(ccODP, 0.95)
ccODP = quantile_loss_cc(ccODP, 0.75)
ccODP = quantile_loss_cc(ccODP, 0.95)

fwrite(ccODP, paste0(output_directory, "/ccODP.csv"))


ccODP_Score = data.frame(
  "Data" = Data_Name,
  "Model" = "ccODP",
  "RMSE" = sqrt(mean((ccODP[Test == 1, ccODP] - ccODP[Test == 1, Loss])^2)),
  "MeanLogScore" = mean(ccODP[Test == 1, log_score]),
  "MeanLogScore35" = mean(ccODP[Test == 1 & DQ <= 35, log_score]),
  "QuantileScore75" = mean(ccODP[Test == 1, CCLoss75]),
  "QuantileScore95" = mean(ccODP[Test == 1, CCLoss95])
)

#Save the quantitative Scores of the MDN and ccODP models
Final_Scores = rbind(Scores, ccODP_Score)
fwrite(Final_Scores, paste0(output_directory, "/Final_Scores.csv"))



