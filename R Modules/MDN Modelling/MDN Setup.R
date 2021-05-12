#install_keras()




library(keras)
#library(tensorflow)
#library(reticulate)


# set tensorflow random seed
set_random_seed = function(seed){
  seed_int = as.integer(seed)
  set.seed(seed_int)
  reticulate::py_set_seed(seed_int)
  tensorflow::tf$random$set_seed(seed_int)
  
}
set_random_seed(1)
#install_keras()

###LOAD DATA
output_directory = output_directory



Train1 <- fread(paste0(output_directory , "/Train1.csv"))
Validation1 <- fread(paste0(output_directory , "/Validation1.csv"))
Test1 <- fread(paste0(output_directory , "/Test1.csv"))


Train2 <- fread(paste0(output_directory , "/Train2.csv"))
Validation2 <- fread(paste0(output_directory , "/Validation2.csv"))
Test2 <- fread(paste0(output_directory , "/Test2.csv"))


Train3 <- fread(paste0(output_directory , "/Train3.csv"))
Validation3 <- fread(paste0(output_directory , "/Validation3.csv"))
Test3 <- fread(paste0(output_directory , "/Test3.csv"))

if (Adjusted_Partition){
  Train4 <- fread(paste0(output_directory , "/Train4.csv"))
  Validation4 <- fread(paste0(output_directory , "/Validation4.csv"))
  Test4 <- fread(paste0(output_directory , "/Test4.csv"))
  x_train4 = as.matrix(Train4[,.(AQ, DQ)])
  x_val4 = as.matrix(Validation4[,.(AQ, DQ)])
  
  
  
  y_train4 = as.matrix(Train4[,.(Loss)])
  y_val4 = as.matrix(Validation4[,.(Loss)])
  
}

x_train1 = as.matrix(Train1[,.(AQ, DQ)])
x_val1 = as.matrix(Validation1[,.(AQ, DQ)])
x_test1 = as.matrix(Test1[,.(AQ, DQ)])



y_train1 = as.matrix(Train1[,.(Loss)])
y_val1 = as.matrix(Validation1[,.(Loss)])
y_test1 = as.matrix(Test1[,.(Loss)])



x_train2 = as.matrix(Train2[,.(AQ, DQ)])
x_val2 = as.matrix(Validation2[,.(AQ, DQ)])
x_test2 = as.matrix(Test2[,.(AQ, DQ)])


y_train2 = as.matrix(Train2[,.(Loss)])
y_val2 = as.matrix(Validation2[,.(Loss)])
y_test2 = as.matrix(Test2[,.(Loss)])



x_train3 = as.matrix(Train3[,.(AQ, DQ)])
x_val3 = as.matrix(Validation3[,.(AQ, DQ)])



y_train3 = as.matrix(Train3[,.(Loss)])
y_val3 = as.matrix(Validation3[,.(Loss)])



Full_Data <- fread(paste0(output_directory , "/Full_Data.csv"))

x_all = as.matrix(Full_Data[,.(AQNorm, DQNorm)])

y_all = as.matrix(Full_Data[,.(LossNorm)])






###########
###DEFINE MDN
############




## CUSTOM LOSS FUNCTION: NEGATIVE LOG-LIKELIHOOD
GaussianPDF = function(y, alpha, mu, sigma){
  
  inv_sigma = 1/sigma
  exponent = (-0.5)*k_square((y - mu)/sigma)
  constant = alpha/(k_sqrt(2*pi*k_square(sigma)))
  return (((constant * k_exp(exponent)))   )
  
}


MeanGaussian = function(parameter){
  mean_est = 0
  c = dim(parameter)[[2]]/3
  for (i in 1:c){
    alpha = parameter[,(1*i):(1*i)]
    mu = parameter[,(c+i):(c+i)]
    mean_est = mean_est + alpha*mu
    
  }
  return (mean_est)
}





NLLcustom = function(y, parameter){
  K = backend()
  NLL_term = 0
  mean_est = 0
  c = dim(parameter)[[2]]/3
  for (i in 1:c){
    alpha = parameter[,(1*i):(1*i)]
    mu = parameter[,(c+i):(c+i)]
    sigma = parameter[,(2*c+i):(2*c+i)]
    NLL_term = NLL_term + GaussianPDF(y, alpha, mu, sigma)
    mean_est = mean_est + alpha*mu
    
  }
  NLL_term = -k_mean(k_log(NLL_term))
  MSE_term = k_mean(k_square(y - mean_est))
  
  final_output = nll_weight*(NLL_term) + mse_weight*MSE_term
  
  return (final_output)
  
}



PConst_Loss = function(y, parameter){
  
  Const = y[,(2:2)]
  Constrained = k_greater(Const,0)
  NonConstrained = k_less_equal(Const,0)
  
  
  
  y_Const = tensorflow::tf$boolean_mask(y, Constrained)
  
  y_NonConst = tensorflow::tf$boolean_mask(y, NonConstrained)
  
  parConst = tensorflow::tf$boolean_mask(parameter, Constrained)
  parNonConst = tensorflow::tf$boolean_mask(parameter, NonConstrained)
  
  y_loss = y_NonConst[,1:1]
  NLLterm = NLLcustom(y_loss, parNonConst)
  
  CEConst = MeanGaussian(parConst)
  
  Lower = y_Const[,3:3]
  Upper = y_Const[,4:4]
  LBDiff = (Lower) - CEConst
  UBDiff = CEConst - (Upper)
  
  LB = k_maximum(LBDiff,0.0)
  UB = k_maximum(UBDiff,0.0)
  
  
  
  
  Const_term = const_weight*(k_mean(LB) + k_mean(UB))
  return(Const_term + NLLterm)
}




MDNLoss = NLLcustom




##Define MDN

MDN = function(netl2, sigmal2, dropout_rate, neurons, no_hidden, components){
  input = layer_input( shape = c(2), dtype = 'float32')
  hidden = input
  hidden_left = no_hidden
  while (hidden_left > 0){
    hidden = hidden %>%
      layer_dense(units = neurons, activation = "sigmoid", kernel_regularizer = regularizer_l2(netl2) )%>%
      layer_dropout(dropout_rate)
    hidden_left = hidden_left - 1
  }
  alpha = hidden %>%
    layer_dense(units = components, activation = "softmax")
  
  mu = hidden %>%
    layer_dense(units = components)
  
  sigma = hidden %>%
    layer_dense(units = components, activation = "exponential", activity_regularizer = regularizer_l2(sigmal2))
  
  final = list(alpha, mu, sigma) %>% layer_concatenate()
  model = keras_model(input = input, outputs = final)
  model %>% compile(
    optimizer = 'adam',
    loss = MDNLoss
    
  )
  model
}





Early_Stopping = callback_early_stopping(
  monitor = "val_loss",
  min_delta = 0,
  patience = 1000,
  verbose = 1,
  mode = c("auto", "min", "max"),
  baseline = NULL,
  restore_best_weights = TRUE
)




plotTraining = function(MDNFit){
  if(!is.nan(mean(MDNFit$metrics$loss ))){
    if (max(MDNFit$metrics$loss, MDNFit$metrics$val_loss) == Inf){
      ylims = c(min(MDNFit$metrics$loss, MDNFit$metrics$val_loss),max(MDNFit$metrics$loss) )
    } else {
      ylims = c(min(MDNFit$metrics$loss, MDNFit$metrics$val_loss),max(MDNFit$metrics$loss, MDNFit$metrics$val_loss) )
    }
    plot(MDNFit$metrics$loss, type = "l", col = "blue", lwd = 2, ylim = ylims,
         main = "Plot of Training History", xlab = "Epochs", ylab = "Loss",
         cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
    lines(MDNFit$metrics$val_loss, col = "orange", lwd = 2)
    grid(nx = NULL, ny = NULL)
    legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "orange"), lwd = c(2,2))
  } else {
    print("NaN training")
  }
}






ResMDN = function(netl2, sigmal2, dropout_rate, neurons, no_hidden, components){
  inputNN = layer_input( shape = c(2), dtype = 'float32')
  hidden = inputNN
  hidden_left = no_hidden
  while (hidden_left > 0){
    hidden = hidden %>%
      layer_dense(units = neurons, activation = "sigmoid", kernel_regularizer = regularizer_l2(netl2) )%>%
      layer_dropout(dropout_rate)
    hidden_left = hidden_left - 1
  }
  alphaNN = hidden %>%
    layer_dense(units = components, kernel_initializer = 'zeros')
  
  muNN = hidden %>%
    layer_dense(units = components, kernel_initializer = 'zeros')
  
  sigmaNN = hidden %>%
    layer_dense(units = components, kernel_initializer = 'zeros')
  
  
  ##EMBEDDING
  
  inputEmbed = layer_input(shape = c(1), dtype = 'int32')
  
  Embedding = inputEmbed %>%
    layer_embedding(input_dim = 1600, output_dim = 3*components, input_length = 1,
                    #embeddings_initializer = list(Mapping),
                    # put weights into list and do not allow training
                    weights = list(Mapping),
                    trainable = FALSE)
  ##
  Embedding = k_reshape(Embedding, c(-1,3*components))
  alphaEmbed = Embedding[,1:components]
  #print(k_int_shape(Embedding))
  muEmbed = Embedding[,(components + 1):(2*components)]
  sigmaEmbed = Embedding[,(2*components + 1):(3*components + 1)]
  
  
  
  ##ADD NN TO EMBEDDED
  alphaAGG = list(alphaEmbed, alphaNN) %>% layer_add()
  muFinal = list(muEmbed, muNN) %>% layer_add()
  sigmaAGG = list(sigmaEmbed, sigmaNN) %>% layer_add()
  
  ##PASS FINAL ACTIVATIONS
  alphaFinal = activation_softmax(alphaAGG, axis = -1)
  sigmaFinal = activation_exponential(sigmaAGG)
  #layer_activity_regularization(sigmaFinal, l2 = sigmal2)
  
  final = list(alphaFinal, muFinal, sigmaFinal) %>% layer_concatenate()
  model = keras_model(inputs = c(inputNN, inputEmbed), outputs = final)
  model %>% compile(
    optimizer = 'adam',
    loss = MDNLoss
    
  )
  model
}



