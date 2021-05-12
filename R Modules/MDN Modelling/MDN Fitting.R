##MDN FITTING MODULE
# Fits 5 MDNs on the full triangle using the final model selected

for (i in 1:5){
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
  
  Predictions = MDNmodel %>% predict(x_all)
  
  
  
  plotTraining(MDN_fit)
  
  
  
  components = ncol(Predictions)/3
  
  MDNResults = cbind(cbind(x_all, y_all), Predictions)
  
  fwrite(as.data.table(MDNResults), paste0(output_directory, "/MDNResults", i, "-",mse_weight ,".csv"))
  
  
}

