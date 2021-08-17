######################
# 2.4 MDN Fitting ####
######################
# This module:
# - Fits the final MDN model 5 times and saves results

#If the Adjusted_Partition is used, 2 partitions are used for fitting the final model
# - The first partition is used 3 times, while the second one is used twice
if (Adjusted_Partition){
  runs1 = 3
} else {
  runs1 = 5
}


for (i in 1:runs1){
  set_random_seed(i)
  MDNmodel = MDN(best_netl2, best_sigmal2, best_dropout, best_neurons, best_hidden, best_components)
  
  MDN_fit = MDNmodel %>% fit(x = x_train3,
                             y = y_train3,
                             epochs = 10000,
                             verbose = 0,
                             batch_size = dim(x_train3)[1],
                             validation_data = list(x_val3, y_val3),
                             callbacks = list(Early_Stopping)
  )
  print(paste0('MDN ', length(MDN_fit$params$epochs)))
  Predictions = MDNmodel %>% predict(x_all)
  
  
  
  plotTraining(MDN_fit)
  
  
  
  components = ncol(Predictions)/3
  
  MDNResults = cbind(cbind(x_all, y_all), Predictions)
  
  fwrite(as.data.table(MDNResults), paste0(output_directory, "/MDNResults", i, "-",mse_weight ,".csv"))
  
  
}




if (Adjusted_Partition){

for (i in ((runs1 + 1):5)){
  set_random_seed(i)
  MDNmodel = MDN(best_netl2, best_sigmal2, best_dropout, best_neurons, best_hidden, best_components)
  
  MDN_fit = MDNmodel %>% fit(x = x_train4,
                             y = y_train4,
                             epochs = 10000,
                             verbose = 0,
                             batch_size = dim(x_train4)[1],
                             validation_data = list(x_val4, y_val4),
                             callbacks = list(Early_Stopping)
  )
  
  Predictions = MDNmodel %>% predict(x_all)
  
  
  
  plotTraining(MDN_fit)
  
  
  
  components = ncol(Predictions)/3
  
  MDNResults = cbind(cbind(x_all, y_all), Predictions)
  
  fwrite(as.data.table(MDNResults), paste0(output_directory, "/MDNResults", i, "-",mse_weight ,".csv"))
  
  
}
  
  
}
