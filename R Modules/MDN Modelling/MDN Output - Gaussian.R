##MD OUTPUT - GAUSSIAN MODULE
##This module imports the MDN fit and assesses the model, performing the following processes:
# 
# 1. Importing the MDN fit
# 2. Scaling back mean and sigma estimates (Loss was normalised in the pre-processing module)
# 3. Storing multiple trials (the MDN can be run multiple times)
# 4. Averages fit from multiple runs
# 5. Assess the RMSE and Log Score of the model

##READ TABLE OF FITTED MIXED DISTRIBUTION FROM PYTHON/R MDN

for (trial in 1:5){
  print(trial)
    Results = fread(paste0(output_directory, "/MDNResults", trial, "-",desired_mse_weight,".csv"))
  
  

  components = ncol(Results)/3 -1
  
  #Format of Results table is (alpha, mu, sigma)
  #Rename columns appropriately
  colnames(Results)[1:3] <- c("AQ", "DQ", "Loss")
  start = 4
  for (i in 1:components){
    colnames(Results)[start] = paste0("a",i)
    colnames(Results)[start+ components] = paste0("u",i)
    colnames(Results)[start+ 2*components] = paste0("s",i)
    start = start+1
    
  }
  
  Results <- as.data.table(Results)
  #Un-Normalise AQ, DQ, Loss
  #mu and sigma also unnormalised
  scale_vec <- (c(mean(Full_Data$AQMean), mean(Full_Data$AQStdev), mean(Full_Data$DQMean), mean(Full_Data$DQStdev), mean(Full_Data$LossMean), mean(Full_Data$LossStdev)))
  
  conv_table <- copy(Results)
  conv_table[,AQ := as.integer(round(AQ*scale_vec[2] + scale_vec[1]))]
  conv_table[,DQ := as.integer(round(DQ*scale_vec[4] + scale_vec[3]))]
  
  
  conv_table[,Loss := Loss*scale_vec[6] + scale_vec[5] ]
  
  
  
  conv_table = MeanSDNormal(conv_table, components = components, scale_vec = scale_vec)
  
  #Plot for now
  par(mfrow = c(2,2))
  ##HASHED OUT COMMANDS FOR STORING RESULTS OF DIFFERENT NETWORK RUNS
  #THESE RUNS WILL BE ENSEMBLED LATER ON
  
  
  #trial1 <- copy(conv_table)
  #trial2 <- copy(conv_table)
  #trial3 <- copy(conv_table)
  #trial4 <- copy(conv_table)
  #trial5 <- copy(conv_table)
  
  # reserve = copy(conv_table)
  # conv_table = reserve
  
  AQs <- c(10,20,30,40)
  
  plotResults(conv_table, AQs)
  
  if (trial == 1){
    trial1 <- as.data.table(copy(conv_table))
    fwrite(trial1, paste0(output_directory , "/Trial1.csv"))
    trial = trial + 1
  } else if (trial == 2){
    trial2 <- as.data.table(copy(conv_table))
    fwrite(trial2, paste0(output_directory, "/Trial2.csv"))
    trial = trial + 1
  } else if (trial == 3){
    trial3 <- as.data.table(copy(conv_table))
    fwrite(trial3, paste0(output_directory , "/Trial3.csv"))
    trial = trial + 1
    
  } else if (trial == 4){
    trial4 <- as.data.table(copy(conv_table))
    fwrite(trial4, paste0(output_directory , "/Trial4.csv"))
    trial = trial + 1
  } else if (trial == 5){
    trial5 <- as.data.table(copy(conv_table))
    fwrite(trial5, paste0(output_directory , "/Trial5.csv"))
    trial = trial + 1
    
  }
}


#Scores
##CBIND THE RESULTS FROM THE 5 TRIALS, to produce the Full_Results table


trial1 <- as.data.table(trial1)
trial2 <- as.data.table(trial2)
trial3 <- as.data.table(trial3)
trial4 <- as.data.table(trial4)
trial5 <- as.data.table(trial5)


Full_Results = CreateEnsemble(trial1, trial2, trial3, trial4, trial5, components = components)
Full_Results = as.data.table(Full_Results)
#SAVE THE TABLE WITH ALL TRIALS IN THE RELEVANT DIRECTORY
#Note, alphas haven't been divided by the number of trials conducted, this will be done later

fwrite(Full_Results, paste0(output_directory , "/Full_Results_pre.csv"))
Full_Results <- fread(paste0(output_directory , "/Full_Results_pre.csv"))
Full_Results <- as.data.table(Full_Results)






trial = 6

start = 4

#Calculate the mean loss prediction, standard deviation, log score and Absolute Error
Full_Results <- mean_function(Full_Results, start, components, trial - 1)
Full_Results <- log_score_function(Full_Results, start, components, trial - 1)
Full_Results <- sigma_function(Full_Results, start, components, trial - 1)
Full_Results <- Full_Results[,Test := as.numeric(AQ + DQ > 41)]
Full_Results[, AbsError := abs(Loss - predMean)]
Full_Results = quantile_prediction(Full_Results, start, components, trial - 1, 0.75, 0.001, 100)
Full_Results = quantile_prediction(Full_Results, start, components, trial - 1, 0.995, 0.001, 200)
Full_Results = quantile_loss(Full_Results, 0.75)
Full_Results = quantile_loss(Full_Results, 0.995)



#Rewrite Full_Results, with 
fwrite(Full_Results, paste0(output_directory, "/Full_Results.csv"))
##Record accuracy of the fit (RMSE and Log Score, will add quantile scores soon) in a data frame

Scores <- data.frame(
  "Data" = Data_Name,
  "Model" = Model_Name,
  "RMSE" = sqrt(mean(Full_Results[Test == 1, AbsError^2])),
  "MeanLogScore" = mean(Full_Results[Test == 1, logScore]),
  "QuantileScore75" = mean(Full_Results[Test == 1, `MDNLoss75`]),
  "QuantileScore99.5" = mean(Full_Results[Test == 1, `MDNLoss99.5`])
)

#####

par(mfrow = c(2,2))

plotResults(Full_Results, c(10,20,30,40))



fwrite(Scores, paste0(output_directory, "/MDN_Scores.csv"))


