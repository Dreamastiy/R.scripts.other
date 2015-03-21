sSide = 1000
data <- matrix(nrow = sSide, ncol=sSide)
N <- 1

maxLenStart <- 0
maxLenEnd <- 10
dataRed <- c()
n <- 1
trigger <- F
trigCol <- 0

for (iCol in 1:sSide)
{
     if (iCol >= maxLenEnd){
          maxLenStart <- maxLenEnd
          maxLenEnd <- maxLenEnd + 10 * n
          n <- n + 1
          trigger <- !trigger
     }
     
     if (trigger == 0){
          data[400,iCol] <- 1
     }
     else{
          data[400, iCol] <- 0
     }
}

for(iRow in 1:399){
     for(iCol in 1:sSide){
          data[iRow, iCol] <- 0
     } 
}  

for(iRow in 401:sSide){
     for(iCol in 1:sSide){
          if((iCol - 1 > 0) & (iCol+1 < 1001) & ( (data[iRow - 1, iCol + 1]==1)|(data[iRow - 1, iCol - 1]==1) )
          {
               data[iRow,iCol] <- 1
          }
          else{
               data[iRow,iCol] <- 0
          }
     }
}

while(maxLen < 1000){
     dataRed <- c(dataRed, maxLen+10*n)
     maxLen <- maxLen + 80*n
     n<-n+1
}

data[400,]<-

for(iRow in 1:sSide){
     for(iCol in 1:sSide){
          data[iRow, iCOl] <- 1
     } 
}



# par(mfrow(1,1))
par()
plot(data, pch = 16)
# plot(dataRed, pch=16, color='red')
?points(dataRed, y=rep(600, length(dataRed)), color='red')