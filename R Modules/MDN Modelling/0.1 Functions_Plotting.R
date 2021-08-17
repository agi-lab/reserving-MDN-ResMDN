#############################
# 0.1 FUNCTIONS_PLOTTING ####
#############################
# This module:
# - Designs functions used for plotting claim development, model forecasts, margins and total reserves




clab = 1.25
cmain = 1.5
caxis = 1

plotClaims = function(Data_table, AQs, Environment = 1){
  
  Data = as.data.frame(copy(Data_table))
  #par(mfrow = c(1,1))
  #Plotting claims Decay
  colours = c("red", "black", "purple", "orange", "blue", "green", "yellow")
  for (i in 1:length(AQs)){
    if (i == 1){
      plot(c(1:40),Data[AQs[i],c(2:41)], 
           cex.lab = 1.25, cex.main = cmain,cex.axis = caxis, type = "l",
           lwd = 2, lty = 2, col = "red", main = paste0("Environment ", Environment), xlab = "DQ",
           ylab = "Incremental Claims", ylim = c(0,max(Data[,c(2:41)])))
      lines(c(1:(41-AQs[i])), Data[AQs[i],c(2:(42 - AQs[i]))], lwd = 2, col = "red"   )
    } else if (i <= length(colours)){
      lines(c(1:(41-AQs[i])), Data[AQs[i],c(2:(42 - AQs[i]))], col = colours[i], lwd = 2) #solid line
      lines(c((41 - AQs[i]):40), Data[AQs[i],c((42 - AQs[i]):41)], col = colours[i], lwd = 2, lty = 2   )
    } else {
      lines(c(1:(41-AQs[i])), Data[AQs[i],c(2:(42 - AQs[i]))], col = paste0(i), lwd = 2) #solid line
      lines(c((41 - AQs[i]):40), Data[AQs[i],c((42 - AQs[i]):41)], col = paste0(i), lwd = 2, lty = 2   )
      
    }
  }
  grid(nx = NULL, ny = NULL)
  
  legend("topright", legend = paste0("AQ ", c(AQs)), col = colours, lty = 1, lwd = 2)
  
  
}



plotResults = function(Data, AQs){
  Replica = as.data.table(Data)
  
  for (i in 1:length(AQs)){
    Section <- Replica[AQ == AQs[i],]
    #plot(Section$DQ, Section$Loss, type = "l",
    #     cex.lab = clab, cex.main = cmain,col = "blue", main = paste0("AQ ", AQs[i]), lwd = 3, xlab = "DQ", ylab = "Loss", ylim = c(min(min(Section$predMean - Section$sigma), min(Section$Loss)),max(max(Section$predMean + Section$sigma), max(Section$Loss))  ))
    ylimit = c(0,max(Data$Loss))
    plot(Section$DQ, Section$Loss, type = "l",
         cex.lab = clab, cex.main = cmain,cex.axis = caxis,col = "blue", main = paste0("AQ ", AQs[i]), lwd = 0.5, xlab = "DQ", ylab = "Incremental Claims", ylim = c(0, max(Data$Loss)  ))
    
    rect(42 - AQs[i],ylimit[1],40,ylimit[2],col = "grey95")
    abline(v = 42 - AQs[i], lwd = 2, lty = 2, col = "darkblue")
    
    lines(Section[AQ + DQ >= 41, DQ], Section[AQ + DQ >= 41, Loss], col = "blue", lwd = 3)
    lines(Section[AQ + DQ <= 41, DQ], Section[AQ + DQ <= 41, Loss], col = "blue", lwd = 3)
    
    
    
    lines(Section$DQ, Section$predMean, col = "red", lwd = 0.5)
    
    lines(Section[AQ + DQ >= 41, DQ], Section[AQ + DQ >= 41, predMean], col = "red", lwd = 3)
    lines(Section[AQ + DQ <= 41, DQ], Section[AQ + DQ <= 41, predMean], col = "red", lwd = 3)
    
    
    lines(Section$DQ, Section$predMean+Section$sigma, lty = 2, lwd = 2)
    lines(Section$DQ, Section$predMean-Section$sigma, lty = 2, lwd = 2)
    grid(nx = NULL, ny = NULL)
    #abline(v = 41 - AQs[i], lwd = 3)
    
    #legend("topright", legend = c("Actual", "CE", "One SD", "Forecast"), col = c("blue", "red", "black", "black"), lty = c(1,1,2, 1), lwd = c(2,2,2,3))
  }
  
  rm(Section, Replica)
  
}



plotComparison = function(Comparison, AQs, horizon){
  Replica = as.data.table(copy(Comparison))
  
  Replica[,inHorizon := (AQ + DQ > 41 & AQ + DQ < 42 + horizon)]
  
  #par(mfrow = c(1,1))
  for (i in 1:length(AQs)){
    
    if (horizon >= 40){
      Section <- Replica[AQ == AQs[i],]
      
    } else {
      Section <- Replica[AQ == AQs[i] & inHorizon,]
      
    }
    minCC = min(Section$ccODP)
    maxCC = max(Section$ccODP)
    minMDN = min(Section$MDN - Section$sigmaMDN)
    maxMDN = max(Section$MDN + Section$sigmaMDN)
    minLoss = min(Section$Loss)
    maxLoss = max(Section$Loss)
    
    
    plot(Section$DQ, Section$Loss, type = "l",
         cex.lab = clab, cex.main = cmain,cex.axis = caxis
         ,col = "blue", main = paste0("AQ ", AQs[i]), lwd = 1, xlab= "DQ", ylab = "Incremental Claims"
         , ylim = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss)))
    ylimit = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss))
    rect(42 - AQs[i],ylimit[1],40,ylimit[2],col = "grey95")
    abline(v = 42 - AQs[i], lwd = 2, lty = 2, col = "darkblue")
    lines(Section$DQ, Section$Loss, lwd = 3, col = "blue")
    lines(Section$DQ, Section$MDN, col = "red", lwd = 3)
    #lines(Section$DQ, Section$RealMean, col = "black", lwd = 3)
    
    lines(Section$DQ, Section$ccODP, col = "green", lwd = 3)
    lines(Section$DQ, Section$MDN+Section$sigmaMDN, lty = 2, lwd = 2, col = "black")
    lines(Section$DQ, Section$MDN-Section$sigmaMDN, lty = 2, lwd = 2, col = "black")
    grid(nx = NULL, ny = NULL)
    #legend("topright", legend = c("Actual", "MDN CE", "MDN One SD", "ccODP"), col = c("blue", "red", "red", "green"), lty = c(1,1,2, 1), lwd = c(2,2,2,2))
    
  }
  
  
  rm(Section, Replica)
  
}





plotReal = function(Data, AQs, horizon){
  Replica = as.data.table(copy(Data))
  
  Replica[,inHorizon := (AQ + DQ > 41 & AQ + DQ < 42 + horizon)]
  
  #par(mfrow = c(1,1))
  for (i in 1:length(AQs)){
    
    if (horizon >= 40){
      Section <- Replica[AQ == AQs[i],]
      
    } else if (horizon == -1){
      Section <- Replica[AQ == AQs[i] & AQ + DQ <= 41,]
    } else {
      Section <- Replica[AQ == AQs[i] & inHorizon,]
      
    }
    minCC = min(Section$ccODP)
    maxCC = max(Section$ccODP)
    minMDN = min(Section$predMean - Section$sigma)
    maxMDN = max(Section$predMean + Section$sigma)
    minLoss = min(Section$Loss)
    maxLoss = max(Section$Loss)
    
    
    plot(Section$DQ, Section$predMean, type = "l", 
         cex.lab = clab, cex.main = cmain,cex.axis = caxis,col = "red", main = paste0("AQ ", AQs[i]), lwd = 3, xlab= "DQ", ylab = "Loss", ylim = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss)))
    lines(Section$DQ, Section$RealMean, col = "black", lwd = 4)
    lines(Section$DQ, Section$Loss, col = "blue", lwd = 2)
    lines(Section$DQ, Section$ccODP, col = "green", lwd = 3)
    lines(Section$DQ, Section$predMean+Section$sigma, lty = 2, lwd = 1, col = "red")
    lines(Section$DQ, Section$predMean-Section$sigma, lty = 2, lwd = 1, col = "red")
    grid(nx = NULL, ny = NULL)
    #legend("topright", legend = c("MDN CE","MDN 1SD",  "Actual", "ccODP", "Mean Claims"), col = c( "red","red", "blue", "green", "black"), lty = c(1,2,1, 1, 1), lwd = c(2,1,2,2, 3))
    
  }
  
  
  rm(Section, Replica)
  
  
  
  
  
}























plotShape = function(Data, AQs, horizon){
  Replica = as.data.table(copy(Data))
  
  Replica[,inHorizon := (AQ + DQ > 41 & AQ + DQ < 42 + horizon)]
  
  #par(mfrow = c(1,1))
  for (i in 1:length(AQs)){
    
    if (horizon >= 40){
      Section <- Replica[AQ == AQs[i],]
      
    } else if (horizon == -1){
      Section <- Replica[AQ == AQs[i] & AQ + DQ <= 41,]
    } else {
      Section <- Replica[AQ == AQs[i] & inHorizon,]
      
    }
    Section$CC25 = Section$CC25 - Section$ccODP
    Section$CC75 = Section$CC75 - Section$ccODP
    Section$CC95 = Section$CC95 - Section$ccODP
    
    Section$MDN25 = Section$MDN25 - Section$predMean
    Section$MDN75 = Section$MDN75 - Section$predMean
    Section$MDN95 = Section$MDN95 - Section$predMean
    
    Section$Real25 = Section$Real25 - Section$RealMean
    Section$Real75 = Section$Real75 - Section$RealMean
    Section$Real95 = Section$Real95 - Section$RealMean
    Section$RealMean = 0
    
    
    minCC = min(Section$CC25)
    maxCC = max(Section$CC75)
    minMDN = min(Section$MDN25)
    maxMDN = max(Section$MDN75)
    minLoss = min(Section$Real25)
    maxLoss = max(Section$Real75)
    ylimit = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss))
    
    plot(Section$DQ, Section$RealMean, type = "l", 
         cex.lab = clab, cex.main = cmain,cex.axis = caxis, col = "black", main = paste0("AQ ", AQs[i]), lwd = 3
         , xlab= "DQ", ylab = "Risk Margins", ylim = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss)))
    
    rect(42 - AQs[i],ylimit[1],40,ylimit[2],col = "grey95")
    abline(v = 42 - AQs[i], lwd = 2, lty = 2, col = "darkblue")
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,Real25], col = "black", lwd = 2, lty = 1)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,Real25], col = "black", lwd = 2, lty = 1)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,Real75], col = "black", lwd = 2, lty = 2)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,Real75], col = "black", lwd = 2, lty = 2)
    
    
    #lines(Section$DQ, Section$Real95, col = "black", lwd = 3, lty = 3)
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,MDN25], col = "red", lwd = 2, lty = 1)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,MDN25], col = "red", lwd = 2, lty = 1)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,MDN75], col = "red", lwd = 2, lty = 2)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,MDN75], col = "red", lwd = 2, lty = 2)
    
    
    #lines(Section$DQ, Section$MDN95, col = "red", lwd = 3, lty = 3)
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,CC25], col = "green3", lwd = 2, lty = 1)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,CC25], col = "green3", lwd = 2, lty = 1)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,CC75], col = "green3", lwd = 2, lty = 2)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,CC75], col = "green3", lwd = 2, lty = 2)
    #lines(Section$DQ, Section$CC95, col = "green", lwd = 3, lty = 3)
    grid(nx = NULL, ny = NULL)
    #legend("topright", legend = c("Simulated", "MDN", "ccODP", "25%", "75%", "95%"), col = c( "black", "red", "green", "black", "black"), lty = c(1,1,1,1,2, 3), lwd = c(2,2,2,2,2,2))
    #legend("topright", legend = c("Simulated", "MDN", "ccODP", "25%", "75%"), col = c( "black", "red", "green", "black"), lty = c(1,1,1,1,2), lwd = c(2,2,2,2,2))
    
  }
  
  
  
  rm(Section, Replica)
  
  
  
  
  
  
}



plotShape95 = function(Data, AQs, horizon){
  Replica = as.data.table(copy(Data))
  
  Replica[,inHorizon := (AQ + DQ > 41 & AQ + DQ < 42 + horizon)]
  
  #par(mfrow = c(1,1))
  for (i in 1:length(AQs)){
    
    if (horizon >= 40){
      Section <- Replica[AQ == AQs[i],]
      
    } else if (horizon == -1){
      Section <- Replica[AQ == AQs[i] & AQ + DQ <= 41,]
    } else {
      Section <- Replica[AQ == AQs[i] & inHorizon,]
      
    }
    Section$CC25 = Section$CC25 - Section$ccODP
    Section$CC75 = Section$CC75 - Section$ccODP
    Section$CC95 = Section$CC95 - Section$ccODP
    
    Section$MDN25 = Section$MDN25 - Section$predMean
    Section$MDN75 = Section$MDN75 - Section$predMean
    Section$MDN95 = Section$MDN95 - Section$predMean
    
    Section$Real25 = Section$Real25 - Section$RealMean
    Section$Real75 = Section$Real75 - Section$RealMean
    Section$Real95 = Section$Real95 - Section$RealMean
    Section$RealMean = 0
    
    
    minCC = min(Section$CC25)
    maxCC = max(Section$CC95)
    minMDN = min(Section$MDN25)
    maxMDN = max(Section$MDN95)
    minLoss = min(Section$Real25)
    maxLoss = max(Section$Real95)
    
    
    plot(Section$DQ, Section$RealMean, type = "l", 
         cex.lab = clab, cex.main = cmain,cex.axis = caxis, col = "black"
         , main = paste0("AQ ", AQs[i]), lwd = 3, xlab= "DQ", ylab = "Risk Margins"
         , ylim = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss)))
    ylimit = c(min(minCC, minMDN, minLoss), max(maxCC, maxMDN, maxLoss))
    rect(42 - AQs[i],ylimit[1],40,ylimit[2],col = "grey95")
    abline(v = 42 - AQs[i], lwd = 2, lty = 2, col = "darkblue")
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,Real25], col = "black", lwd = 2, lty = 1)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,Real25], col = "black", lwd = 2, lty = 1)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,Real75], col = "black", lwd = 2, lty = 2)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,Real75], col = "black", lwd = 2, lty = 2)
    
    
    #lines(Section$DQ, Section$Real95, col = "black", lwd = 3, lty = 3)
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,MDN25], col = "red", lwd = 2, lty = 1)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,MDN25], col = "red", lwd = 2, lty = 1)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,MDN75], col = "red", lwd = 2, lty = 2)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,MDN75], col = "red", lwd = 2, lty = 2)
    
    
    #lines(Section$DQ, Section$MDN95, col = "red", lwd = 3, lty = 3)
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,CC25], col = "green3", lwd = 2, lty = 1)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,CC25], col = "green3", lwd = 2, lty = 1)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,CC75], col = "green3", lwd = 2, lty = 2)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,CC75], col = "green3", lwd = 2, lty = 2)
    
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,Real95], col = "black", lwd = 2, lty = 3)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,Real95], col = "black", lwd = 2, lty = 3)
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,MDN95], col = "red", lwd = 2, lty = 3)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,MDN95], col = "red", lwd = 2, lty = 3)
    
    
    
    lines(Section[AQ + DQ <= 41,DQ], Section[AQ + DQ <= 41,CC95], col = "green3", lwd = 2, lty = 3)
    lines(Section[AQ + DQ >= 41,DQ], Section[AQ + DQ >= 41,CC95], col = "green3", lwd = 2, lty = 3)
    #lines(Section$DQ, Section$CC95, col = "green", lwd = 3, lty = 3)
    grid(nx = NULL, ny = NULL)
    #legend("topright", legend = c("Simulated", "MDN", "ccODP", "25%", "75%", "95%"), col = c( "black", "red", "green", "black", "black"), lty = c(1,1,1,1,2, 3), lwd = c(2,2,2,2,2,2))
    #legend("topright", legend = c("Simulated", "MDN", "ccODP", "25%", "75%"), col = c( "black", "red", "green", "black"), lty = c(1,1,1,1,2), lwd = c(2,2,2,2,2))
    
  }
  
  
  
  rm(Section, Replica)
  
  
  
  
  
  
}












plotQuantiles = function(Data, AQs, horizon, quantile){
  Replica = as.data.table(copy(Data))
  
  Replica[,inHorizon := (AQ + DQ > 41 & AQ + DQ < 42 + horizon)]
  
  #par(mfrow = c(1,1))
  for (i in 1:length(AQs)){
    
    if (horizon >= 40){
      Section <- Replica[AQ == AQs[i],]
      
    } else if (horizon == -1){
      Section <- Replica[AQ == AQs[i] & AQ + DQ <= 41,]
    } else {
      Section <- Replica[AQ == AQs[i] & inHorizon,]
      
    }
    if (quantile == 0.75){
      
      maxCC = max(Section$CC75)
      maxMDN = max(Section$MDN75)
      maxLoss = max(Section$Loss)
      
      maxReal = max(Section$Real75)
      plot(Section$DQ, Section$MDN75, type = "l", 
           cex.lab = clab, cex.main = cmain,cex.axis = caxis,col = "red"
           , main = paste0("AQ ", AQs[i]), lwd = 3, xlab= "DQ", ylab = "Incremental Claims"
           , ylim = c(0, max(maxCC, maxMDN, maxLoss, maxReal)))
      lines(Section$DQ, Section$Real75, col = "black", lwd = 4)
      lines(Section$DQ, Section$Loss, col = "blue", lwd = 2)
      lines(Section$DQ, Section$CC75, col = "green", lwd = 3)
    } else if (quantile == 0.995){
      maxCC = max(Section$CC99.5)
      maxMDN = max(Section$MDN99.5)
      maxLoss = max(Section$Loss)
      
      maxReal = max(Section$Real99.5)
      plot(Section$DQ, Section$MDN99.5, type = "l", 
           cex.lab = clab, cex.main = cmain,cex.axis = caxis,col = "red"
           , main = paste0("AQ ", AQs[i]), lwd = 3, xlab= "DQ", ylab = "Loss"
           , ylim = c(0, max(maxCC, maxMDN, maxLoss, maxReal)))
      lines(Section$DQ, Section$Real99.5, col = "black", lwd = 4)
      lines(Section$DQ, Section$Loss, col = "blue", lwd = 2)
      lines(Section$DQ, Section$CC99.5, col = "green", lwd = 3)
      
    }
    
    
    
    
    
    
    
    grid(nx = NULL, ny = NULL)
    #legend("topright", legend = c("MDN CE","MDN 1SD",  "Actual", "ccODP", "Mean Claims"), col = c( "red","red", "blue", "green", "black"), lty = c(1,2,1, 1, 1), lwd = c(2,1,2,2, 3))
    
  }
  
  
  rm(Section, Replica)
  
  
  
  
  
}





plotReserves = function(MDN_sim, ccODP_sim, Real_sim, centred = 0){
  if(centred){
    MDN_sim = MDN_sim - mean(MDN_sim)
    ccODP_sim = ccODP_sim - mean(ccODP_sim)
    Real_sim = Real_sim - mean(Real_sim)
  }
  xlimit = 1.2*c(min(MDN_sim, ccODP_sim, Real_sim) ,max(MDN_sim, ccODP_sim, Real_sim))
  ylimit = c(0,max(demp(MDN_sim, MDN_sim), demp(ccODP_sim, ccODP_sim), demp(Real_sim, Real_sim)) )        
  plot(density(as.numeric(as.data.frame(MDN_sim)[,1])), lwd = 2, 
       xlim = xlimit, ylim = ylimit,
       col = "red", main = 'Total OSC Distribution', xlab = 'Outstanding Claims', ylab = 'Density')
  lines(density(ccODP_sim), col = "green", lwd = 2)
  lines(density(Real_sim), lwd = 2)
  abline(v = RealRes)
}
