###############################################################################
## MIXTURE DENSITY NETWORK PROJECT ############################################
###############################################################################
#------------------------------------------------------------------------------
###############################################################################
## MASTER SCRIPT ##############################################################
###############################################################################
#------------------------------------------------------------------------------

# This project was used in the modelling for the paper: Stochastic loss reserving with
# Mixture Density Networks.
# 
# This project fits Mixture Density Networks (MDNs) to loss triangles. It aims to
# address some of the gaps in neural network reserving and make it more practical.
# The main model implemented is the Mixture Density Network, which yielded accurate results
# when compared to the stochastic chain ladder - the cross-classified over-dispersed Poisson model
# (ccODP). 
# 
# We impement a GLM-MDN hybrid model, called the ResMDN, which features an interpretable 
# GLM backbone and restricts the MDN's modelling to the GLM's residuals. This hybrid model
# aims to produce more interpretable results and bridge the gap between GLMs and machine learning
# models. It is an adaptation of the Combined Actuarial Neural Network 
# (see Wuethrich & Merz (2019), Gabrielli et al (2020) and Poon (2019) for details)
# 
# We implement projection constraints, which allow the user to set upper and lower 
# bounds on the mean estimates of forecasts as desired. This allows more control over the
# MDN's forecasts and accommodates user judgement.
# 
# 
# This project is split into several sections, each section containing several scripts:
# 1. Data Loading and Processing: The loss triangle is loaded, split into training/validation/testing
# sets and normalised. The ResMDN's GLM embedding and projection constraints are set if desired
# 2. Model training: Once the triangle has been processed, the MDN is setup for fitting. 
# If desired, a hyper-parameter selection algorithm can test an array of models and choose
# the most accurate one, or a set model design could be provided. The final model is fit on the triangle,
# with results saved
# 3. Processing and comparing forecasts: The MDN/ResMDN's fit is visualised and scored, alongside the
# ccODP. The empirical mean and margins from hundreds of triangles will be used to visualise the MDN's accuracy 
# 4. Total Reserves: With the modelling done on incremental claims, the MDN's 
# fitted distribution of total reserves will be simulated and compared against the 
# empirical distribution
# 
# 
# The MDN was primarily tested on 40x40 simulated triangles using the SynthETIC package 
# (see Avanzi et al (2020) for details). For the data processing and model fitting,
# a single loss triangle without the lower triangle known is sufficient. However, 
# for scoring and model comparison, it is assumed that the lower triangle is known.
# Some forecast visualisation functions also rely on multiple triangles being simulated,
# in order to yield an empirical distribution of reserves. Nevertheless, this project
# provides all necessary triangles required to run through, and can easily be modified
# to accommodate practical reserving scenarios.


# I hope you enjoy using this workspace. Please contact me on muhammed.almudafer97@gmail.com
# for any queries/issues encountered with the project.

# Thank you
# Muhammed Al-Mudafer


#Load packages

library(data.table)
library(dplyr)
library(ggplot2)
library(keras) #See https://keras.rstudio.com/ for installation details
library(EnvStats)
library(mixtools) #optional for 'mixtools' ResMDN


#####################################
## LOAD FUNCTIONS ###################
#####################################

source('R Modules/MDN Modelling/0.1 Functions_Plotting.R', echo=TRUE)
source('R Modules/MDN Modelling/0.2 Functions_Process.R', echo=TRUE)


#####################################
## SET VARIABLES ####################
#####################################


#Environment: The paper uses Environments 1-4. Can be named arbitrarily, 
#as long as a corresponding triangles exists in Datasets/Environment X
Environment = 2

#Triangle_Number: The paper uses 50 triangles for each environment. 
Triangle_Number = 1

#With Environment and Triangle_Number specified, the input loss triangle's directory is specified as such:
#this directory can be set by the user
triangle_directory = paste0("Datasets/Environment ",Environment )

#Distribution: Type "Normal"/ "Log" to fit a Mixed Gaussian/Log-Gaussian respectively.
Distribution = "Normal"  

#Adjusted_Partition: adjusted training/testing partition includes data from the latest calendar periods in the training set
#See Appendix B of the paper
Adjusted_Partition = 0

#log_setzeros: when fitting a mixture log-Gaussian, 0 loss cells cannot be processed. Set a number
#for 0 loss cells when the log is taken
log_setzeros = 6

#Model_Name: Toggle between MDN/ResMDN
Model_Name = "ResMDN"

#PConst: Projection Constraints
PConst = 0  #Note: Projection constraints are only applied to training final models, not to hyper-parameter selection



#Toggles for the processes required:
# 1. HPSelection: Hyper-Parameter Selection. Will run the hyper-parameter tuning algorithm to select the model design
# 2. Training: With hyper-parameters supplied, models will be trained and predictions saved
# 3. Analysing: Given models have been trained, the model's fit will be processed, visualised and compared to the ccODP
# 4. TotalReserves: Given forecasts have been processed, simulations of total reserves will be made

#In the associated paper, HPSelection was applied on only the first triangle in each environment
#the model design for the other triangles followed the one set on the first
HPSelection = FALSE
Training = FALSE
Analysing = TRUE
TotalReserves = FALSE

#ResMDN VARIABLES

#Initialisation Methodology: This paper uses the "Single Gaussian" approach. The "mixtools" approach
# is available, which uses the mixtools package to more accurately approximate the ccODP using a 
# Mixed Gaussian.
#The number of components in the mixture must be decided now for the ResMDN, in order for the embedding to be 
#given the right dimensions. The ResMDN model design's number of components must equal the number of components in the embedding
if (Model_Name == "ResMDN"){
  Initialisation_Methodology = "Single Gaussian"
  components = 4
}


#HPSelection and Training variables:
#Specify the mse, negative log likelihood and projection constraints weights in the loss function
#The HPSelection algorithm does not alter these
if (Training || HPSelection){
  mse_weight = 4
  nll_weight = 1
  if (PConst){
    const_weight = 1
}



#To fit an MDN on the whole triangle, set the following hyper-parameters, which correspond to
#the weight regularisation, sigma activity regularisation, dropout rate, neurons in each layer, hidden layers and number of components
#Hyper-parameters are set here if the algorithm isn't to be run, else the model design is decided automatically after the algorithm is run
  if(!HPSelection){
      best_netl2 = 0
      best_sigmal2 = 0
      best_dropout = 0
      best_neurons = 20
      best_hidden = 2
      best_components = 4
  }
}


#Analysis variables
#With a trained model ready to analyse, set the mse weight of the loss function which the models were trained on
if (Analysing){
  desired_mse_weight = 4
}




#### END OF SETTING VARIABLES ####





#####################################
## SET OUTPUT DIRECTORY #############
#####################################

output_directory = paste0("Modelling Output/Environment ",Environment)
dir.create(file.path(output_directory), showWarnings = FALSE)
output_directory = paste0(output_directory,"/",Triangle_Number)
dir.create(file.path(output_directory), showWarnings = FALSE)
output_directory = paste0(output_directory, "/",Distribution)
dir.create(file.path(output_directory), showWarnings = FALSE)

Data_Name = paste0(Environment, ".", Triangle_Number, "_", Distribution)

if (Model_Name == "ResMDN"){
  output_directory = paste0(output_directory, "/ResMDN" )
  dir.create(file.path(output_directory), showWarnings = FALSE)
  
}
if (PConst){
  output_directory = paste0(output_directory, "/PConst" )
  dir.create(file.path(output_directory), showWarnings = FALSE)
  
}


############################################
## SECTION 1: DATA LOADING AND PROCESSING ##
############################################

#Load the triangle, normalise and partition into training/validation/testing sets
source('R Modules/MDN Modelling/1.1 Data Input.R', echo=TRUE)




### RESMDN

#Approximate the GLM backbone
if (Model_Name == "ResMDN"){
  source('R Modules/MDN Modelling/1.2 ResMDN Embedding.R', echo=TRUE)
  
}


## PROJECTION CONTROL

#Set the constraints and assign to training/validation
if (PConst){
  source('R Modules/MDN Modelling/1.3 Projection Constraints.R', echo=TRUE)
  
}

############################
## SECTION 2: MDN FITTING ##
############################


if (Training || HPSelection){
  #Define the MDN, loss function
  source('R Modules/MDN Modelling/2.1 MDN Setup.R', echo=TRUE)
}

if (HPSelection){
    
    #Run the hyper-parameter selection algorithm, takes about 8-12 hours
    if (Model_Name == "ResMDN"){
      source('R Modules/MDN Modelling/2.3 ResMDN Selection Algorithm.R', echo=TRUE)
    } else {
      source('R Modules/MDN Modelling/2.2 HP Selection Algorithm.R', echo=TRUE)
    }
    #Now the HP Selection algorithm is run, the final model is saved. Create the final model and train

    Chosen_Model = fread(paste0(output_directory, '/Chosen_Model.csv'))
    
    best_netl2 = Chosen_Model$NetL2
    best_sigmal2 = Chosen_Model$SigmaL2
    best_dropout = Chosen_Model$Dropout
    best_neurons = Chosen_Model$Neurons
    best_hidden = Chosen_Model$Hidden
    best_components = Chosen_Model$Components
}


if (Training){
  #Separate scripts for fitting the final model, for the MDN and ResMDN
  if (Model_Name == "ResMDN"){
    source('R Modules/MDN Modelling/2.5 ResMDN Fitting.R', echo=TRUE)
  } else {
    source('R Modules/MDN Modelling/2.4 MDN Fitting.R', echo=TRUE)
  }
}


##################################
## SECTION 3: ANALYSING RESULTS ##
##################################

#The final model has been run on the triangle, projections have been generated
#Results are analysed in this section and compared to the ccODP


#Separate results processing scripts depending on the choice of Distribution
if (Analysing){

if (Distribution == 'Normal'){
  source('R Modules/MDN Modelling/3.1 MDN Output - Gaussian.R', echo=TRUE)
  
} else if (Distribution == 'Log'){
  source('R Modules/MDN Modelling/3.2 MDN Output - Log Gaussian.R', echo=TRUE)
}



#Comparing to the ccODP
source('R Modules/MDN Modelling/3.3 ccODP Comparison.R', echo=TRUE)
  
#Compare the model against the empirical losses
source('R Modules/MDN Modelling/3.4 Empirical Mean and Margins.R', echo=TRUE)
}  


##############################################################
## SECTION 4: SIMULATING THE DISTRIBUTION OF TOTAL RESERVES ##
##############################################################
if(TotalReserves){
#Simulate total reserves (aggregate of the lower triangle)
  source('R Modules/MDN Modelling/4.1 Total Reserves.R', echo=TRUE)
}

