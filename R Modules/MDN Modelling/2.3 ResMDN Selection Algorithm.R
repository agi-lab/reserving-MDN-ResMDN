#####################################
# 2.3 ResMDN Selection Algorithm ####
#####################################
# This module:
# - Tests different ResMDN designs and selects the best-performing one amongst the candidates
# - Similar to the 'HP Selection Algorithm' module, except it select a ResMDN model
# - Starts with an initial model, then sequentially updates the following parameters:
## - netL2, dropout rate, hidden layers, neurons

#The number of components is set beforehand, as the embedding dimensions depend on it



#Number of trials and epochs for :netl2, sigmal2, drop, nch steps
trials = c(5,5,5,5)
epochs = 10000

#Define initial conditions
initial_netl2 = 0
initial_sigmal2 = 0
best_sigmal2 = 0
initial_dropout = 0
initial_neurons = 60
initial_hidden = 2
initial_components = components


#Specify range of values to test for each hyper-parameter
netl2_range = c(0,0.0001,0.001,0.01)
dropout_range = c(0,0.1,0.2)
neurons_range = c(20,40,60,80,100)
hidden_range = c(1,2,3,4)






#NetL2 optimisation

for (i in 1:length(netl2_range)){
  netl2 = netl2_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[1]){
    start = Sys.time()
    set_random_seed(t)
    ResMDNmodel = ResMDN(netl2, initial_sigmal2, initial_dropout, initial_neurons, initial_hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train1, x_train1C),
                                     y = y_train1,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train1)[1],
                                     validation_data = list(list(x_val1, x_val1C), y_val1),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("NetL2 ", netl2, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test1, x_test1C), y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    ResMDNmodel = ResMDN(netl2, initial_sigmal2, initial_dropout, initial_neurons, initial_hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train2, x_train2C),
                                     y = y_train2,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train2)[1],
                                     validation_data = list(list(x_val2, x_val2C), y_val2),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("NetL2 ", netl2, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test2, x_test2C), y_test2, verbose = 0))
    Part2[t] = TestError
    
    
    end = Sys.time()
    print(paste0("Time taken: ", end - start))
  }
  
  TotalError = (length(y_test1)*mean(Part1[!is.nan(Part1)]) + length(y_test2)*mean(Part2[!is.nan(Part2)]))/(length(y_test1) + length(y_test2))
  print(paste0("NetL2 ", netl2, " Error: ", round(TotalError, 3)))
  Info = cbind("NetL2" = netl2, t(Part1), t(Part2), TotalError)
  Info = as.data.table(Info)
  
  if (i == 1){
    Results_Table = Info
  } else {
    Results_Table = rbind(Results_Table, Info)
  }
  
  fwrite((Results_Table), paste0(output_directory, "/NetL2.csv"))
}


#Selecting the best NetL2
best_netl2 = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),NetL2])



#Dropout rate optimisation

for (i in 1:length(dropout_range)){
  dropout = dropout_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[2]){
    start = Sys.time()
    set_random_seed(t)
    ResMDNmodel = ResMDN(best_netl2, best_sigmal2, dropout, as.integer(initial_neurons/(1 - dropout)), initial_hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train1, x_train1C),
                                     y = y_train1,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train1)[1],
                                     validation_data = list(list(x_val1, x_val1C), y_val1),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Dropout ", dropout, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test1, x_test1C), y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    ResMDNmodel = ResMDN(best_netl2, best_sigmal2, dropout, as.integer(initial_neurons/(1 - dropout)), initial_hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train2, x_train2C),
                                     y = y_train2,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train2)[1],
                                     validation_data = list(list(x_val2, x_val2C), y_val2),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Dropout ", dropout, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test2, x_test2C), y_test2, verbose = 0))
    Part2[t] = TestError
    
    
    end = Sys.time()
    print(paste0("Time taken: ", end - start))
  }
  
  TotalError = (length(y_test1)*mean(Part1[!is.nan(Part1)]) + length(y_test2)*mean(Part2[!is.nan(Part2)]))/(length(y_test1) + length(y_test2))
  print(paste0("Dropout ", dropout, " Error: ", round(TotalError, 3)))
  Info = cbind("Dropout" = dropout, t(Part1), t(Part2), TotalError)
  Info = as.data.table(Info)
  
  if (i == 1){
    Results_Table = Info
  } else {
    Results_Table = rbind(Results_Table, Info)
  }
  
  fwrite((Results_Table), paste0(output_directory, "/Dropout.csv"))
}


#Selecting the best dropout rate

best_dropout = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Dropout])



#Hidden layer optimisation

for (i in 1:length(hidden_range)){
  hidden = hidden_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[3]){
    start = Sys.time()
    set_random_seed(t)
    ResMDNmodel = ResMDN(best_netl2, best_sigmal2, best_dropout, as.integer(initial_neurons/(1 - best_dropout)), hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train1, x_train1C),
                                     y = y_train1,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train1)[1],
                                     validation_data = list(list(x_val1, x_val1C), y_val1),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Hidden ", hidden, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test1, x_test1C), y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    ResMDNmodel = ResMDN(best_netl2, best_sigmal2, best_dropout, as.integer(initial_neurons/(1 - best_dropout)), hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train2, x_train2C),
                                     y = y_train2,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train2)[1],
                                     validation_data = list(list(x_val2, x_val2C), y_val2),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Hidden ", hidden, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test2, x_test2C), y_test2, verbose = 0))
    Part2[t] = TestError
    
    
    end = Sys.time()
    print(paste0("Time taken: ", end - start))
  }
  
  TotalError = (length(y_test1)*mean(Part1[!is.nan(Part1)]) + length(y_test2)*mean(Part2[!is.nan(Part2)]))/(length(y_test1) + length(y_test2))
  print(paste0("Hidden ", hidden, " Error: ", round(TotalError, 3)))
  Info = cbind("Hidden" = hidden, t(Part1), t(Part2), TotalError)
  Info = as.data.table(Info)
  
  if (i == 1){
    Results_Table = Info
  } else {
    Results_Table = rbind(Results_Table, Info)
  }
  
  fwrite((Results_Table), paste0(output_directory, "/Hidden.csv"))
}


#Selecting the best number of hidden layers

best_hidden = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Hidden])



#Optimising the number of neurons

for (i in 1:length(neurons_range)){
  neurons = neurons_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[4]){
    start = Sys.time()
    set_random_seed(t)
    ResMDNmodel = ResMDN(best_netl2, best_sigmal2, best_dropout, neurons, best_hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train1, x_train1C),
                                     y = y_train1,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train1)[1],
                                     validation_data = list(list(x_val1, x_val1C), y_val1),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Neurons ", neurons, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test1, x_test1C), y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    ResMDNmodel = ResMDN(best_netl2, best_sigmal2, best_dropout, neurons, best_hidden, initial_components)
    ResMDN_fit = ResMDNmodel %>% fit(x = list(x_train2, x_train2C),
                                     y = y_train2,
                                     epochs = epochs,
                                     verbose = 0,
                                     batch_size = dim(x_train2)[1],
                                     validation_data = list(list(x_val2, x_val2C), y_val2),
                                     callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Neurons ", neurons, " trial ", t))
    plotTraining(ResMDN_fit)
    TestError = as.numeric( ResMDNmodel %>% evaluate(list(x_test2, x_test2C), y_test2, verbose = 0))
    Part2[t] = TestError
    
    
    end = Sys.time()
    print(paste0("Time taken: ", end - start))
  }
  
  TotalError = (length(y_test1)*mean(Part1[!is.nan(Part1)]) + length(y_test2)*mean(Part2[!is.nan(Part2)]))/(length(y_test1) + length(y_test2))
  print(paste0("Neurons ", neurons, " Error: ", round(TotalError, 3)))
  Info = cbind("Neurons" = neurons, t(Part1), t(Part2), TotalError)
  Info = as.data.table(Info)
  
  if (i == 1){
    Results_Table = Info
  } else {
    Results_Table = rbind(Results_Table, Info)
  }
  
  fwrite((Results_Table), paste0(output_directory, "/Neurons.csv"))
}


#Select the best number of neurons

best_neurons = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Neurons])



#Save the best performing model amongst the designs tested

Chosen_Model = data.frame(
  "NetL2" = best_netl2,
  "SigmaL2" = best_sigmal2,
  "Dropout" = best_dropout,
  "Neurons" = best_neurons,
  "Hidden" = best_hidden,
  "Components" = initial_components
  
)


fwrite((Chosen_Model), paste0(output_directory, "/Chosen_Model.csv"))




