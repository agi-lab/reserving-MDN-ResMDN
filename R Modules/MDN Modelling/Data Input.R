#############
# 1. DATA INPUT
#############
# This module reads in the data and convert it to a melted data table, with AP and DP columns

##LOAD DATASET
Data <- fread(paste0(triangle_directory , "/Table ", Triangle_Number, ".csv"))




#Log data if needed

if (Distribution == "Log"){ 
  Data = as.data.frame(Data)
  
  Data[Data == 0] <- exp(log_setzeros)
  Data[,-1] = log(Data[,-1])
  min(Data)
}


Data = as.data.table(Data)

par(mfrow = c(1,1))

#Name columns and melt data into a data.table
#colnames(Data)<- c("AQ",1:40)
Data_Melted <- reshape2::melt(Data, id.vars = "AQ")

colnames(Data_Melted)<- c("AQ","DQ","Loss")

plotClaims(Data, c(1,10,20,30,40))

#WORK WITH MELTED TABLE

Full_Data <- as.data.table(Data_Melted)



Full_Data = DataSplit(Full_Data)
Full_Data = Normalise(Full_Data)





##Partition Complete, save as csv

fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))


Save_Tr_Va_Te(Full_Data, output_directory)
  





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


if (Environment == 3){
  
  Full_Data[Train4 == 1, Set4 := "Train"]
  Full_Data[Val4 == 1, Set4 := "Validation"]
}


fwrite(Full_Data, paste0(output_directory , "/Full_Data.csv"))

#VISUALISE PARTITIONS
View_Partitions = 0

if(View_Partitions){

  heatmap_set(Full_Data, 1)
  # 
  heatmap_set(Full_Data, 2)
  # 
  heatmap_set(Full_Data, 3)
  if(Adjusted_Partition){
    heatmap_set(Full_Data, 4)
  }


}
































