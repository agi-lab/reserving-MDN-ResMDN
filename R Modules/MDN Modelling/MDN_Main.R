


library(data.table)
library(dplyr)
library(ggplot2)
library(keras)


source('R Modules/MDN Modelling/Functions_Plotting.R', echo=TRUE)
source('R Modules/MDN Modelling/Functions_Process.R', echo=TRUE)



#####################################
##DATA LOADING AND PROCESSING SECTION
#####################################



#SET VARIABLES
Environment = 1
Triangle_Number = 1
##Distribution: Type "Normal"/ "Log" to fit a Mixed Gaussian/Log-Gaussian respectively
Distribution = "Normal"  
Adjusted_Partition = 0
log_setzeros = 0






output_directory = paste0("Modelling Output/Environment ",Environment)
dir.create(file.path(output_directory), showWarnings = FALSE)
output_directory = paste0(output_directory,"/",Triangle_Number)
dir.create(file.path(output_directory), showWarnings = FALSE)
output_directory = paste0(output_directory, "/",Distribution)
dir.create(file.path(output_directory), showWarnings = FALSE)

Model_Name = "MDN"
Data_Name = paste0(Environment, ".", Triangle_Number, "_", Distribution)


triangle_directory = paste0("Datasets/Environment ",Environment )

source('R Modules/MDN Modelling/Data Input.R', echo=TRUE)





#####################
##MDN FITTING SECTION
#####################


mse_weight = 0
nll_weight = 1
const_weight = 1


source('R Modules/MDN Modelling/MDN Setup.R', echo=TRUE)



#To run the hyper-parameter selection algorithm, source the following script
source('R Modules/MDN Modelling/HP Selection Algorithm.R', echo=TRUE)


#To fit an MDN on the whole triangle, set the following hyper-parameters 
#and source the following script

best_netl2 = 0
best_sigmal2 = 0
best_dropout = 0
best_neurons = 60
best_hidden = 3
best_components = 2


#PROCESSING AND ANALYSING RESULTS
#Specify which mse_weight to analyse
desired_mse_weight = 0

if (Distribution == 'Normal'){
  source('R Modules/MDN Modelling/MDN Output - Gaussian.R', echo=TRUE)
  
} else if (Distribution == 'Log'){
  source('R Modules/MDN Modelling/MDN Output - Log Gaussian.R', echo=TRUE)
}



#COMPARING TO THE CCODP
source('R Modules/MDN Modelling/ccODP Comparison.R', echo=TRUE)


