######################
# 2.5 MDN Fitting ####
######################
# This module:
# - Fits the final ResMDN model 5 times and saves results

if (Adjusted_Partition){
  runs = 3
} else {
  runs = 5
}


for (i in 1:runs){
  set_random_seed(i)
  MDNmodel = ResMDN(best_netl2, best_sigmal2, best_dropout, best_neurons, best_hidden, best_components)
  MDN_fit = MDNmodel %>% fit(x = list(x_train3, x_train3C),
                             y = y_train3,
                             epochs = 10000,
                             verbose = 0,
                             batch_size = dim(x_train3)[1],
                             validation_data = list(list(x_val3, x_val3C), y_val3),
                             callbacks = list(Early_Stopping)
  )
  
  Predictions = MDNmodel %>% predict(list(x_all, x_allC))
  print(paste0('ResMDN ', length(MDN_fit$params$epochs)))
  
 
  
  plotTraining(MDN_fit)
  
  
  
  components = ncol(Predictions)/3
  
  MDNResults = cbind(cbind(x_all, y_all), Predictions)
  
  fwrite(as.data.table(MDNResults), paste0(output_directory, "/MDNResults", i, "-",mse_weight ,".csv"))
  
  
}




if (Adjusted_Partition){
  
  for (i in 4:5){
    set_random_seed(i)
    MDNmodel = ResMDN(best_netl2, best_sigmal2, best_dropout, best_neurons, best_hidden, best_components)
    MDN_fit = MDNmodel %>% fit(x = list(x_train4, x_train4C),
                               y = y_train4,
                               epochs = 10000,
                               verbose = 0,
                               batch_size = dim(x_train4)[1],
                               validation_data = list(list(x_val4, x_val4C), y_val4),
                               callbacks = list(Early_Stopping)
    )
    
    Predictions = MDNmodel %>% predict(list(x_all, x_allC))
    
    
    
    plotTraining(MDN_fit)
    
    
    
    components = ncol(Predictions)/3
    
    MDNResults = cbind(cbind(x_all, y_all), Predictions)
    
    fwrite(as.data.table(MDNResults), paste0(output_directory, "/MDNResults", i, "-",mse_weight ,".csv"))
    
    
  }
  
  
}