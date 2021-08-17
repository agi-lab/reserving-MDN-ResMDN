######################################
# 3.4 Empirical Mean and Margins #####
######################################
# This module:
# - Plots the MDN and ccODP's central estimates against the empirical mean based on hundreds of triangle simulations
# - Similarly plots the MDN and ccODP's risk margins against the empirical margins
# - Plots the MDN and ccODP's quantiles for incremental claims against the empirical quantiles



#Extract empirical information from the triangle_directory
Quantile_Info <- fread(paste0(triangle_directory , "/Quantile_Info.csv"))
Quantile_Info = Quantile_Info[order(DQ, AQ)]


#Load and combine the MDN and ccODP projections
MDN = fread(paste0(output_directory,"/Full_Results.csv"))
ccODP = fread(paste0(output_directory, "/ccODP.csv"))

All_Info = cbind(MDN, "ccODP" = ccODP$ccODP, "Dispersion" = ccODP$Dispersion)
fwrite(All_Info, paste0(output_directory, "/All_Info.csv"))




start = 4
trials = 5

#Calculate the desired quantiles of the MDN and ccODP fits
Quantiles = c(0.25,0.75,0.95, 0.995)


for (i in 1:length(Quantiles)){
  if (Distribution == "Log"){
    All_Info = quantile_prediction_log(All_Info, start, components, trial - 1,(Quantiles[i]), 0.001, 100)
  } else {
    All_Info = quantile_prediction(All_Info, start, components, trial - 1,(Quantiles[i]), 0.001, 100)

  }
  
  All_Info = quantile_ccODP(All_Info, as.numeric(Quantiles[i]))

  
}


#Combine the MDN, ccODP and empirical mean and quantiles
select(Quantile_Info, -c(AQ, DQ))

All_Info = cbind(All_Info, select(Quantile_Info, -c(AQ, DQ)))


fwrite(All_Info, paste0(output_directory,"/All_Info.csv"))
All_Info = fread(paste0(output_directory,"/All_Info.csv"))


#Plot the central estimates of incremental claims (MDN, ccODP and empirical)
par(mfrow = c(2,2))
plotReal(All_Info, c(10,20,30,40), horizon = 40)


#Plot the 75 and 95th quantiles of incremental claims (MDN, ccODP and empirical)
par(mfrow = c(2,2))
plotQuantiles(All_Info, c(10,20,30,40), 40, 0.75)
plotQuantiles(All_Info, c(10,20,30,40), 40, 0.95)


#Plot the risk margins of incremental claims (MDN, ccODP and empirical)
par(mfrow = c(2,2))
plotShape(All_Info, c(10,20,30, 40), horizon = 40)





