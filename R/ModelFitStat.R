ModelFitStat <- function(Stattype,SSE,SST,n,nY,p,sigmaVal){
   if(Stattype=="SBC"){
     ICvalue <- n*log(SSE/n)+log(n)*p
   }
   if(Stattype=="AIC"){
     ICvalue <- n*log(SSE/n)+(2*p*nY*n+nY*(nY+1))/n-2/n+n+2
   }
   if(Stattype=="AICc"){
     ICvalue <- n*log(SSE/n)+n*(n+p)*nY/(n-p-nY-1)
   }
   if(Stattype=="CP"){
     ICvalue <- SSE/sigmaVal+2*p-n
   }
   if(Stattype=="HQ"){
     ICvalue <- n*log(SSE/n)+2*log(log(n))*p*nY/n
   }
   if(Stattype=="HQc"){
     ICvalue <- n*log(SSE*SSE/n)+2*log(log(n))*p*nY/(n-p-nY-1)
   }
   if(Stattype=="BIC"){
     ICvalue <- n*log(SSE/n)+2*(2+p)*(n*sigmaVal/SSE)-2*(n*sigmaVal/SSE)*(n*sigmaVal/SSE)
   }
   if(Stattype=="Rsq"){
     ICvalue <- 1-SSE/SST
   }
   if(Stattype=="adjRsq"){
     ICvalue <- 1-(SSE/SST)*(n-1)/(n-p)
   }
   return(ICvalue)
} 

