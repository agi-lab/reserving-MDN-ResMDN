##########################
# 4.1 Total Reserves #####
##########################
# This module:
# - Simulates total reserves (outstanding claims) in the lower triangle, as predicted by the MDN and ccODP
# - Uses these simulations to approximate a distribution of outstanding claims (OSC)
# - Plots the distribution against the empirical distribution
# - Calculates the quantiles of total reserves/OSC



set.seed(1)

#Load the MDN's fit
Full_Results = fread(paste0(output_directory,"/Full_Results.csv"))

Simulations = 200
start = 4
trials = 5


#For each AQ, DQ cell, simulate losses from the distribution fitted for that cell
rows = 0

for (i in 1:40){
  for (j in 1:40){
    
    if (i + j > 41){
      print(rows)
      SIM_VEC = Simulate_MG(Full_Results, start, trials, AQ_chosen = i, DQ_chosen = j, Simulations)
      final_vec = c(i,j,SIM_VEC)
      if (rows == 0){
        Sim_Table = final_vec
        
      } else {
        Sim_Table = rbind(Sim_Table, final_vec)
      }
      rows = rows+1
    }
    
  }
  
  
}

#Aggregate the individual cell simulations to create simulations of total reserves in the lower triangle
#Note: we assume losses are independent for different cells (AQ, DQ)
MDN_sim = c()

for (i in 3:(Simulations+2)){
  MDN_sim[i-2] = sum(Sim_Table[,i])
  
  
}


#Save MDN/ResMDN simulations
MDN_sim = as.data.table(MDN_sim)
fwrite(MDN_sim,paste0(output_directory,"/MDN_sim.csv") )






#ccODP simulations
set.seed(1)

#Load the ccODP fit
ccODP = fread(paste0(output_directory,"/ccODP.csv"))

#Simulate total reserves in the lower triangle
#Much simpler due to the additive property of the Poisson distribution
ccODP_sim = mean(ccODP$Dispersion)*rpois(Simulations, sum(ccODP[AQ + DQ > 41]$ccODP)/mean(ccODP$Dispersion))

#Calculate the actual total outstanding claims in the lower triangle
RealRes = sum(Full_Results[AQ + DQ > 41,Loss])




MDN_sim = as.data.frame(MDN_sim)[,1]



#Find the empirical distribution of total outstanding claims

#All_Runs is a combination of all triangles run for that environment
#Each triangle gives a total OSC value, which together provides the empirical distribution
All_Runs = fread(paste0(triangle_directory, "/All_Runs.csv"))
no_trials = ncol(All_Runs) - 2

LowerTri = All_Runs[AQ + DQ > 41,]
LowerTri = as.data.frame(LowerTri)

Real_sim = c()

for (i in 3:(no_trials+2)){
  Real_sim[i-2] = sum(LowerTri[,i])
}


#Plot the OSC distributions
plotReserves(MDN_sim, ccODP_sim, Real_sim)

#Distributions centred around their mean (to visualise the dispersion)
plotReserves(MDN_sim, ccODP_sim, Real_sim, centred = 1)


#Save simulations
fwrite(as.data.table(ccODP_sim),paste0(output_directory,"/ccODP_sim.csv")) 
fwrite(as.data.table(MDN_sim),paste0(output_directory,"/MDN_sim.csv")) 

fwrite(as.data.table(Real_sim),paste0(output_directory,"/RealReserves.csv")) 

#Save mean and quantile estimate of total reserves/OSC
#To be used in calculating quantitative metrics

#Central estimates
ActualR = sum(Full_Results[AQ + DQ > 41, Loss])
MDN_R = sum(Full_Results[AQ + DQ > 41, predMean])
ccODP_R = sum(ccODP[AQ + DQ > 41, ccODP])

#Probability density function (usually 0, hence not used in comparisons)
MDN_f = demp(ActualR, MDN_sim, discrete = FALSE)
ccODP_f = demp(ActualR, ccODP_sim, discrete = FALSE)

#75th quantile
MDN_75 = qemp(0.75, MDN_sim, discrete = FALSE)
MDN_95 = qemp(0.95, MDN_sim, discrete = FALSE)

#95th quantile
ccODP_75 = qemp(0.75, ccODP_sim, discrete = FALSE)
ccODP_95 = qemp(0.95, ccODP_sim, discrete = FALSE)


Reserve_est = rbind( ActualR , MDN_R, ccODP_R, MDN_f,  ccODP_f, MDN_75, MDN_95, ccODP_75,ccODP_95 )
fwrite(as.data.table(Reserve_est),paste0(output_directory,"/Reserve_estimates.csv") )



