################################
# 3.1 MDN Output - Gaussian ####
################################
# This module:
# - Imports the MDN/ResMDN fit (for a Mixed Gaussian model) and assesses the model, performing the following processes:
# 1. Importing the MDN/ResMDN fit
# 2. Scaling back mean and sigma estimates (Loss was normalised in the pre-processing module)
# 3. Storing multiple trials (the model is run multiple times)
# 4. Averages fits from multiple runs
# 5. Assess the RMSE, Log Score and Quantile Scores of the model


#Import 5 fits from 5 runs
for (trial in 1:5){
  print(trial)
    Results = fread(paste0(output_directory, "/MDNResults", trial, "-",desired_mse_weight,".csv"))
 
  components = ncol(Results)/3 -1
  
  #Format of Results table is (alpha, mu, sigma)
  #Rename columns appropriately
  colnames(Results)[1:3] <- c("AQ", "DQ", "Loss")
  #start column index
  start = 4
  for (i in 1:components){
    colnames(Results)[start] = paste0("a",i)
    colnames(Results)[start + components] = paste0("u",i)
    colnames(Results)[start + 2*components] = paste0("s",i)
    start = start+1
    
  }
  
  Results <- as.data.table(Results)
  #Un-Normalise AQ, DQ, Loss
  #mu and sigma also unnormalised
  Full_Data = fread(paste0(output_directory, "/Full_Data.csv"))
  
  scale_vec <- (c(mean(Full_Data$AQMean), mean(Full_Data$AQStdev), mean(Full_Data$DQMean), mean(Full_Data$DQStdev), mean(Full_Data$LossMean), mean(Full_Data$LossStdev)))
  
  ResultsTable <- copy(Results)
  ResultsTable[,AQ := as.integer(round(AQ*scale_vec[2] + scale_vec[1]))]
  ResultsTable[,DQ := as.integer(round(DQ*scale_vec[4] + scale_vec[3]))]
  
  
  ResultsTable[,Loss := Loss*scale_vec[6] + scale_vec[5] ]
  
  
  
  ResultsTable = MeanSDNormal(ResultsTable, components = components, scale_vec = scale_vec)
  
  #Plot the results from a single run
  par(mfrow = c(2,2))
  AQs <- c(10,20,30,40)
  plotResults(ResultsTable, AQs)
  
  
  fwrite(ResultsTable, paste0(output_directory , "/Trial",trial,".csv"))

  trial = trial + 1
  
  
}




#Averaging fits from multiple runs

#Read 5 runs
trial1 = fread(paste0(output_directory,"/Trial",1,".csv" ))
trial2 = fread(paste0(output_directory,"/Trial",2,".csv" ))
trial3 = fread(paste0(output_directory,"/Trial",3,".csv" ))
trial4 = fread(paste0(output_directory,"/Trial",4,".csv" ))
trial5 = fread(paste0(output_directory,"/Trial",5,".csv" ))

#CreateEnsemble: Binds 5 runs together in single table
Full_Results = CreateEnsemble(trial1, trial2, trial3, trial4, trial5, components = components)
Full_Results = as.data.table(Full_Results)

#Save the combined runs
#Note, alphas haven't been divided by the number of trials conducted, this will be done later
fwrite(Full_Results, paste0(output_directory , "/Full_Results.csv"))





#Calculate the mean loss prediction, standard deviation, Absolute Error, log score and quantile scores
#Note: The final fitted distribution is the average of the 5 fits/runs

trials = 5

#start column index
start = 4

Full_Results <- mean_function(Full_Results, start, components, trials)
Full_Results <- log_score_function(Full_Results, start, components, trials)
Full_Results <- sigma_function(Full_Results, start, components, trials)
Full_Results <- Full_Results[,Test := as.numeric(AQ + DQ > 41)]
Full_Results[, AbsError := abs(Loss - predMean)]
Full_Results = quantile_prediction(Full_Results, start, components, trials, 0.75, 0.001, 100)
Full_Results = quantile_prediction(Full_Results, start, components, trials, 0.95, 0.001, 200)
Full_Results = quantile_loss(Full_Results, 0.75)
Full_Results = quantile_loss(Full_Results, 0.95)

#Set a minimum on the log likelihood of individual cells. 
#This reduces the influence of low-volume points in the later DQs
Full_Results[logScore < -50, logScore := -50]

#Save Full_Results, with quantitative metrics calculated
fwrite(Full_Results, paste0(output_directory, "/Full_Results.csv"))


#Record and save the accuracy of the ensemble model (RMSE and Log Score, Quantile Scores) in a data frame
Scores <- data.frame(
  "Data" = Data_Name,
  "Model" = Model_Name,
  "RMSE" = sqrt(mean(Full_Results[Test == 1, AbsError^2])),
  "MeanLogScore" = mean(Full_Results[Test == 1, logScore]),
  "MeanLogScore35" = mean(Full_Results[Test == 1 & DQ <= 35, logScore]),
  "QuantileScore75" = mean(Full_Results[Test == 1, `MDNLoss75`]),
  "QuantileScore95" = mean(Full_Results[Test == 1, `MDNLoss95`])
)

fwrite(Scores, paste0(output_directory, "/MDN_Scores.csv"))


#Plot the ensemble model
par(mfrow = c(2,2))

plotResults(Full_Results, c(10,20,30,40))





