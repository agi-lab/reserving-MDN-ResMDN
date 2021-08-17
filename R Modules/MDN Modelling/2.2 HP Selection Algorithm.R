#################################
# 2.2 HP Selection Algorithm ####
#################################
# This module:
# - Tests different MDN designs and selects the best-performing one amongst the candidates
# - Starts with an initial model, then sequentially updates the following parameters:
## - netL2, sigmaL2, dropout rate, hidden layers, neurons, components



##Define initial conditions

#Number of trials and epochs for :netl2, sigmal2, dropout, hidden layers, neurons-components 
trials = c(5,5,5,5,2)
#maximum epochs for each run
epochs = 10000

#Initial model
initial_netl2 = 0
initial_sigmal2 = 0
initial_dropout = 0
initial_neurons = 60
initial_hidden = 2
initial_components = 2


#Specify range of values to test for each hyper-parameter
#The number of components will increase so long as the error decreases
netl2_range = c(0,0.0001,0.001,0.01)
sigmal2_range = c(0,0.0001,0.001,0.01,0.1)
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
    MDNmodel = MDN(netl2, initial_sigmal2, initial_dropout, initial_neurons, initial_hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train1,
                               y = y_train1,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train1)[1],
                               validation_data = list(x_val1, y_val1),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("NetL2 ", netl2, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test1, y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    MDNmodel = MDN(netl2, initial_sigmal2, initial_dropout, initial_neurons, initial_hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train2,
                               y = y_train2,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train2)[1],
                               validation_data = list(x_val2, y_val2),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("NetL2 ", netl2, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test2, y_test2, verbose = 0))
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







#SigmaL2 optimisation
for (i in 1:length(sigmal2_range)){
  sigmal2 = sigmal2_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[2]){
    start = Sys.time()
    set_random_seed(t)
    MDNmodel = MDN(best_netl2, sigmal2, initial_dropout, initial_neurons, initial_hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train1,
                               y = y_train1,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train1)[1],
                               validation_data = list(x_val1, y_val1),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("SigmaL2 ", sigmal2, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test1, y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    MDNmodel = MDN(best_netl2, sigmal2, initial_dropout, initial_neurons, initial_hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train2,
                               y = y_train2,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train2)[1],
                               validation_data = list(x_val2, y_val2),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("SigmaL2 ", sigmal2, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test2, y_test2, verbose = 0))
    Part2[t] = TestError
    
    
    end = Sys.time()
    print(paste0("Time taken: ", end - start))
  }
  
  TotalError = (length(y_test1)*mean(Part1[!is.nan(Part1)]) + length(y_test2)*mean(Part2[!is.nan(Part2)]))/(length(y_test1) + length(y_test2))
  print(paste0("SigmaL2 ", sigmal2, " Error: ", round(TotalError, 3)))
  Info = cbind("SigmaL2" = sigmal2, t(Part1), t(Part2), TotalError)
  Info = as.data.table(Info)
  
  if (i == 1){
    Results_Table = Info
  } else {
    Results_Table = rbind(Results_Table, Info)
  }
  
  fwrite((Results_Table), paste0(output_directory, "/SigmaL2.csv"))
}


#Selecting the best sigmaL2

best_sigmal2 = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),SigmaL2])





#Dropout rate optimisation

for (i in 1:length(dropout_range)){
  dropout = dropout_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[3]){
    start = Sys.time()
    set_random_seed(t)
    MDNmodel = MDN(best_netl2, best_sigmal2, dropout, as.integer(initial_neurons/(1 - dropout)), initial_hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train1,
                               y = y_train1,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train1)[1],
                               validation_data = list(x_val1, y_val1),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Dropout ", dropout, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test1, y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    MDNmodel = MDN(best_netl2, best_sigmal2, dropout, as.integer(initial_neurons/(1 - dropout)), initial_hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train2,
                               y = y_train2,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train2)[1],
                               validation_data = list(x_val2, y_val2),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Dropout ", dropout, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test2, y_test2, verbose = 0))
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


###Select the best dropout rate

best_dropout = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Dropout])






#Hidden layer optimisation

for (i in 1:length(hidden_range)){
  hidden = hidden_range[i]
  Part1 = c()
  Part2 = c()
  
  for (t in 1:trials[4]){
    start = Sys.time()
    set_random_seed(t)
    MDNmodel = MDN(best_netl2, best_sigmal2, best_dropout, as.integer(initial_neurons/(1 - best_dropout)), hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train1,
                               y = y_train1,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train1)[1],
                               validation_data = list(x_val1, y_val1),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Hidden ", hidden, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test1, y_test1, verbose = 0))
    Part1[t] = TestError
    
    
    set_random_seed(t)
    
    
    MDNmodel = MDN(best_netl2, best_sigmal2, best_dropout, as.integer(initial_neurons/(1 - best_dropout)), hidden, initial_components)
    MDN_fit = MDNmodel %>% fit(x = x_train2,
                               y = y_train2,
                               epochs = epochs,
                               verbose = 0,
                               batch_size = dim(x_train2)[1],
                               validation_data = list(x_val2, y_val2),
                               callbacks = list(Early_Stopping, callback_terminate_on_naan())
    )
    print(paste0("Hidden ", hidden, " trial ", t))
    plotTraining(MDN_fit)
    TestError = as.numeric( MDNmodel %>% evaluate(x_test2, y_test2, verbose = 0))
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


###Choosing the best number of hidden layers

best_hidden = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Hidden])




#Optimising the distribution flexibility and number of neurons in each hidden layer


components = 1
models_tested = 0
stop = 0

while (!stop){
  
  components = components
  for (i in 1:length(neurons_range)){
    neurons = neurons_range[i]
    Part1 = c()
    Part2 = c()
    
    for (t in 1:trials[5]){
      start = Sys.time()
      set_random_seed(t)
      MDNmodel = MDN(best_netl2, best_sigmal2, best_dropout, neurons, best_hidden, components)
      MDN_fit = MDNmodel %>% fit(x = x_train1,
                                 y = y_train1,
                                 epochs = 1,
                                 verbose = 0,
                                 batch_size = dim(x_train1)[1],
                                 validation_data = list(x_val1, y_val1),
                                 callbacks = list(Early_Stopping)
      )
      if (is.nan(MDN_fit$metrics$loss[1]) || MDN_fit$metrics$loss[1] == Inf ){
        print("NaN Error")
        Part1[t] = NaN
      } else {
        MDN_fit = MDNmodel %>% fit(x = x_train1,
                                   y = y_train1,
                                   epochs = epochs,
                                   verbose = 0,
                                   batch_size = dim(x_train1)[1],
                                   validation_data = list(x_val1, y_val1),
                                   callbacks = list(Early_Stopping)
        )
        plotTraining(MDN_fit)
        TestError = as.numeric( MDNmodel %>% evaluate(x_test1, y_test1, verbose = 0))
        Part1[t] = TestError
      }
      print(paste0("Component ", components, " Neurons ", neurons, " trial ", t))
      
      
      
      set_random_seed(t)
      
      
      MDNmodel = MDN(best_netl2, best_sigmal2, best_dropout, neurons, best_hidden, components)
      MDN_fit = MDNmodel %>% fit(x = x_train2,
                                 y = y_train2,
                                 epochs = 1,
                                 verbose = 0,
                                 batch_size = dim(x_train2)[1],
                                 validation_data = list(x_val2, y_val2),
                                 callbacks = list(Early_Stopping)
      )
      if (is.nan(MDN_fit$metrics$loss[1]) || MDN_fit$metrics$loss[1] == Inf ){
        print("NaN Error")
        Part2[t] = NaN
      } else {
        MDN_fit = MDNmodel %>% fit(x = x_train2,
                                   y = y_train2,
                                   epochs = epochs,
                                   verbose = 0,
                                   batch_size = dim(x_train2)[1],
                                   validation_data = list(x_val2, y_val2),
                                   callbacks = list(Early_Stopping)
        )
        plotTraining(MDN_fit)
        TestError = as.numeric( MDNmodel %>% evaluate(x_test1, y_test1, verbose = 0))
        Part2[t] = TestError
      }
      print(paste0("Component ", components, " Neurons ", neurons, " trial ", t))
      
      
      end = Sys.time()
      print(paste0("Time taken: ", end - start))
    }
    
    TotalError = (length(y_test1)*mean(Part1[!is.nan(Part1)]) + length(y_test2)*mean(Part2[!is.nan(Part2)]))/(length(y_test1) + length(y_test2))
    print(paste0("Component ", components, " Neurons ", neurons, " Error: ", round(TotalError, 3)))
    Info = cbind("Components" = components, "Neurons" = neurons, t(Part1), t(Part2), TotalError)
    Info = as.data.table(Info)
    
    if (models_tested == 0){
      Results_Table = Info
    } else {
      Results_Table = rbind(Results_Table, Info)
    }
    models_tested = models_tested + 1
    fwrite((Results_Table), paste0(output_directory, "/Comp_Neurons.csv"))
  }
  
  best_components = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Components])
  
  if (best_components == components){
    components = components + 1
  } else {
    stop = 1
  }
  
}


best_neurons = as.numeric(Results_Table[TotalError == min(Results_Table$TotalError),Neurons])



#Save the best model design out of ones tested
Chosen_Model = data.frame(
  "NetL2" = best_netl2,
  "SigmaL2" = best_sigmal2,
  "Dropout" = best_dropout,
  "Neurons" = best_neurons,
  "Hidden" = best_hidden,
  "Components" = best_components
  
)


fwrite((Chosen_Model), paste0(output_directory, "/Chosen_Model.csv"))




