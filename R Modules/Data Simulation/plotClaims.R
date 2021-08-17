plotClaims = function(Data_table, AQs, Environment = 1){
  
  Data = as.data.frame(copy(Data_table))
  #par(mfrow = c(1,1))
  #Plotting claims Decay
  colours = c("red", "black", "purple", "orange", "blue", "green", "yellow")
  for (i in 1:length(AQs)){
    if (i == 1){
      plot(c(1:40),Data[AQs[i],c(2:41)], 
           cex.lab = 1.25, cex.main = 2,cex.axis = 1.25, type = "l",
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