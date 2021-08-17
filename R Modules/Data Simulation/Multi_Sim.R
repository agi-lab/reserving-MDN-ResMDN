
Environment = 4



for (i in c(1:1000)){


Triangle <- setDT(reshape2::melt(fread(paste0("Datasets/Environment ",Environment,"/Table ", i, ".csv")), id.vars = "AQ"))

colnames(Triangle)<- c("AQ","DQ",paste0("Trial ", i))

Triangle = Triangle[order(AQ, DQ)]
if (i != 1){
Triangle[,AQ := NULL]
Triangle[,DQ := NULL]
}
if (i == 1){
  All_Runs = copy(Triangle)
} else {
  All_Runs = cbind(All_Runs, Triangle)
}



}









Quantiles = c(0.25,0.5, 0.75,0.95, 0.995)
base = setDT(expand.grid('AQ' = c(1:40), 'DQ' = c(1:40)))
base = base[order(AQ, DQ)]


no_trials = ncol(All_Runs) - 2





real_mean = function(Data, bindWith){
  table = as.data.frame(Data)
  table$RealMean = 0
  for (i in 1:nrow(table)){
    if (i %% 100 == 0){
      print(paste0("Mean: ", i))
    }
    vec = as.numeric(table[i,c(3:no_trials + 2)])
    table$RealMean[i] = mean(vec)
  }
  table = as.data.table(table)
  bindWith = cbind(bindWith, "RealMean" = table$RealMean)
  return (bindWith)
}


real_quantile = function(Data, bindWith, quantile){
  
  table = as.data.frame(Data)
  table$quantile_est = 0
  for (i in 1:nrow(table)){
    if (i %% 100 == 0){
      print(paste0("Quantile ", quantile, ": ", i))
    }
    vec = as.numeric(table[i,c(3:no_trials + 2)])
    vec = vec[order(vec)]
    table$quantile_est[i] = vec[quantile*no_trials]
  }
  #table = as.data.table(table)
  bindWith = cbind(bindWith, "quantile_est" = table$quantile_est)
  colnames(bindWith)[which(colnames(bindWith) == "quantile_est")] = paste0("Real",100*quantile)
  
  return (bindWith)
  
  
}

base = real_mean(All_Runs, base)

for (i in 1:length(Quantiles)){
  base = real_quantile(All_Runs,base, Quantiles[i])
  
  
}


fwrite(All_Runs, paste0("Datasets/Environment ",Environment,"/All_Runs.csv" ))

fwrite(base, paste0("Datasets/Environment ",Environment,"/Quantile_Info.csv" ))
  
  
