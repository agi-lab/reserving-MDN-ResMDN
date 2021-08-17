#####################
# 1.1 DATA INPUT ####
#####################
# This module:
# - Reads in the loss triangle and converts it to a data table, with AQ and DQ and Loss columns
# - Normalises the data to feed into the MDN
# - Partitions the data into training, validation and testing sets


##Load Triangle
Data <- fread(paste0(triangle_directory , "/Table ", Triangle_Number, ".csv"))

#Take the log of data if fitting a mixed Log-Gaussian

if (Distribution == "Log"){ 
  Data = as.data.frame(Data)
  
  Data[Data <= 0] <- exp(log_setzeros)
  Data[,-1] = log(Data[,-1])
  min(Data)
}


Data = as.data.table(Data)


#Name columns and melt data into a data.table

Data_Melted <- reshape2::melt(Data, id.vars = "AQ")

colnames(Data_Melted)<- c("AQ","DQ","Loss")

#Plot incremental claims
par(mfrow = c(1,1))
plotClaims(Data, c(1,10,20,30,40))



#Split the triangle into training, validation and testing
#Note there are 3 or 4 different partitions, each with a separate training, validation and testing set
#There is no testing set for partitions 3 (and 4)
Full_Data <- as.data.table(Data_Melted)
Full_Data = DataSplit(Full_Data)

#Normalise the data (input and output), so that it has a mean of 0 and variance of 1
#This makes training more stable
Full_Data = Normalise(Full_Data)


##Partition and normalising complete, save as csv

fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))
Save_Tr_Va_Te(Full_Data, output_directory)
  


#Visualise partitions: set the View_Partitions variable to 1 to visualise 
View_Partitions = 0
if(View_Partitions){

Full_Data[Train1 == 1, Set1 := "Train"]
Full_Data[Val1 == 1, Set1 := "Validation"]
Full_Data[Test1 == 1, Set1 := "Test"]
#Full_Data[!Train1 & !Val1 & !Test1, Set1 := "Unused"]


Full_Data[Train2 == 1, Set2 := "Train"]
Full_Data[Val2 == 1, Set2 := "Validation"]
Full_Data[Test2 == 1, Set2 := "Test"]



Full_Data[Train3 == 1, Set3 := "Train"]
Full_Data[Val3 == 1, Set3 := "Validation"]
#Full_Data[Test3 == 1, Set3 := "Test"]


if (Adjusted_Partition){
  
  Full_Data[Train4 == 1, Set4 := "Train"]
  Full_Data[Val4 == 1, Set4 := "Validation"]
}


fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))





  heatmap_set(Full_Data, 1)
  # 
  heatmap_set(Full_Data, 2)
  # 
  heatmap_set(Full_Data, 3)
  if(Adjusted_Partition){
    heatmap_set(Full_Data, 4)
  }


}
































