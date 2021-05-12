
#trial = 1
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
  for (c in c(1:components)){
    Results[[paste0("s", c)]][Results[[paste0("s", c)]]>2] = 2
    
  }
  
  Results <- as.data.table(Results)
  #Un-Normalise AQ, DQ, Loss
  #mu and sigma also unnormalised
  scale_vec <- (c(mean(Full_Data$AQMean), mean(Full_Data$AQStdev), mean(Full_Data$DQMean), mean(Full_Data$DQStdev), mean(Full_Data$LossMean), mean(Full_Data$LossStdev)))
  conv_table <- copy(Results)
  conv_table[,AQ := as.integer(round(AQ*scale_vec[2] + scale_vec[1]))]
  conv_table[,DQ := as.integer(round(DQ*scale_vec[4] + scale_vec[3]))]
  
  conv_table[,Loss := Loss*mean(Full_Data$LossStdev) + mean(Full_Data$LossMean) ]
  
  

  
  conv_table = MeanSDNormal(conv_table, components = components, scale_vec = scale_vec)
  
  
  
  ###ANALYSING LOG DATA ON ITS OWN
  ########################
  log_analysis = copy(conv_table)
  #conv_table[,mean_coef_var := mean(sigma/abs(predMean))]
  par(mfrow = c(2,2))
  #Plot for now
  
  fwrite(log_analysis, paste0(output_directory, "/log_analysis.csv"))
  #trial1 <- copy(conv_table)
  #trial2 <- copy(conv_table)
  #trial3 <- copy(conv_table)
  # reserve = copy(conv_table)
  # conv_table = reserve
  
  plot_log = 1
  
  if (plot_log){
    
    AQs <- c(10,20,30,40)
    plotResults(log_analysis, AQs)
    
  }
  #trial = trial + 1
  
  
  
  
  
  ####################
  conv_table <- as.data.table(conv_table)
  
  conv_table = MeanSDLog(conv_table, components = components)
  
  
  
  
  par(mfrow = c(2,2))
  AQs <- c(10,20,30,40)
  plotResults(conv_table, AQs)
  conv_table = as.data.table(conv_table)
  
  
  if (trial == 1){
    trial1 <- as.data.table(copy(conv_table))
    fwrite(trial1, paste0(output_directory , "/Trial1.csv"))
    trial = trial + 1
  } else if (trial == 2){
    trial2 <- as.data.table(copy(conv_table))
    fwrite(trial2, paste0(output_directory , "/Trial2.csv"))
    trial = trial + 1
  } else if (trial == 3){
    trial3 <- as.data.table(copy(conv_table))
    fwrite(trial3, paste0(output_directory, "/Trial3.csv"))
    trial = trial + 1
    
  } else if (trial == 4){
    trial4 <- as.data.table(copy(conv_table))
    fwrite(trial4, paste0(output_directory , "/Trial4.csv"))
    trial = trial + 1
  } else if (trial == 5){
    trial5 <- as.data.table(copy(conv_table))
    fwrite(trial5, paste0(output_directory, "/Trial5.csv"))
    trial = trial + 1
    
  }
}

##CBIND THE RESULTS FROM THE 3 TRIALS, to produce the FullResults table

trial1 <- as.data.table(trial1)
trial2 <- as.data.table(trial2)
trial3 <- as.data.table(trial3)
trial4 <- as.data.table(trial4)
trial5 <- as.data.table(trial5)


Full_Results <- CreateEnsemble(trial1, trial2, trial3, trial4, trial5, components = components)
Full_Results = as.data.table(Full_Results)

#SAVE THE TABLE WITH ALL TRIALS IN THE RELEVANT DIRECTORY
#Note, alphas haven't been divided by the number of trials conducted, this will be done later

fwrite(Full_Results, paste0(output_directory , "/Full_Results_pre.csv"))
Full_Results <- fread(paste0(output_directory , "/Full_Results_pre.csv"))
Full_Results <- as.data.table(Full_Results)



######LOG FUNCTIONS

trial = 6

start = 4

#Calculate the mean loss prediction, standard deviation, log score and Absolute Error
Full_Results <- mean_function_log(Full_Results, start, components, trial - 1)
Full_Results <- log_score_function_log(Full_Results, start, components, trial - 1)
Full_Results <- sigma_function_log(Full_Results, start, components, trial - 1)
Full_Results <- Full_Results[,Test := as.numeric(AQ + DQ > 41)]
Full_Results[, AbsError := abs(Loss - predMean)]
Full_Results = quantile_prediction_log(Full_Results, start, components, trial - 1, 0.75, 0.001, 100)
Full_Results = quantile_prediction_log(Full_Results, start, components, trial - 1, 0.995, 0.001, 200)
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

