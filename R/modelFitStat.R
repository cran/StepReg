#' Fit Model Statistics
#'
#' Fit Model Statistics with least square or likelihood method to return an information criteria value 
#'
#' @param ic Information criteria, including AIC, AICc, BIC, CP, HQ, HQc, Rsq, adjRsq and SBC
#' 
#' @param fit Object of linear model or general linear model
#' 
#' @param method Method to calculate information criteria value, including 'LeastSquare' and 'Likelihood'
#' 
#' @param cox Compute model fit statistics for cox regression or not, where partial likelihood value will be used instead of the ordinary.
#' 
#' @references 
#' 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).
#' 
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' 
#' Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.
#' 
#' Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.
#' 
#' Hocking, R. R. (1976). A biometrics invited paper. the analysis and selection of variables in linear regression. Biometrics, 32(1), 1-49.
#' 
#' Hurvich, C. M., & Tsai, C. (1989). Regression and time series model selection in small samples. Biometrika, 76(2), 297-307.
#' 
#' Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.
#' 
#' Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.
#' 
#' Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.
#' 
#' Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.
#' 
#' Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model Selection. Regression and time series model selection /. World Scientific.
#' 
#' Pillai, K. C. S. (2006). Pillai's Trace. Encyclopedia of Statistical Sciences. John Wiley & Sons, Inc.
#' 
#' R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable selection in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.
#' 
#' Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.
#' 
#' Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
#' 
#' @examples
#' data(mtcars)
#' fit <- lm(mpg~wt+qsec+vs+am+gear+carb,data=mtcars)
#' modelFitStat("AIC",fit,"LeastSquare")
#' 
#' @author Junhui Li 
#' @export modelFitStat

modelFitStat <- function(ic,fit,method=c("LeastSquare","Likelihood"),cox=FALSE){
  method <- match.arg(method)
  if(method=="LeastSquare"){
    resMatrix <- as.matrix(fit$residuals)
    SSEmatrix <- t(resMatrix) %*% resMatrix
    SSE <- abs(det(SSEmatrix))
    p <- fit$rank
    n <- nrow(resMatrix)
    #yName <- rownames(attr(fit$terms,"factors"))[1]
    vars <- as.character(attr(fit$terms, "variables"))[-1]
    yName <- vars[attr(fit$terms, "response")]
    Y <- as.matrix(fit$model[,yName])
    nY <- ncol(Y)
    if(ic=="AIC"){
      PIC <- n*log(SSE/n)+2*p*nY+nY*(nY+1)+n
    }else if(ic=="AICc"){
      PIC <- n*log(SSE/n)+n*(n+p)*nY/(n-p-nY-1)
    }else if(ic=="CP"){
      PIC <- SSE/sigma(fit)+2*p-n
    }else if(ic=="HQ"){
      PIC <- n*log(SSE/n)+2*log(log(n))*p*nY/n
    }else if(ic=="HQc"){
      #PIC <- n*log(SSE*SSE/n)+2*log(log(n))*p*nY/(n-p-nY-1)
      PIC <- n*log(SSE/n)+2*log(log(n))*p*nY/(n-p-nY-1)
    }else if(ic=="BIC"){
      PIC <- n*log(SSE/n)+2*(2+p)*(n*sigma(fit)/SSE)-2*(n*sigma(fit)/SSE)*(n*sigma(fit)/SSE)
    }else if(ic=="Rsq"){
      #PIC <- 1-(SSE/SST)
      PIC <- summary(fit)$r.squared
    }else if(ic=="adjRsq"){
      #PIC <- 1-(SSE/SST)*(n-1)/(n-p)
      PIC <- summary(fit)$adj.r.squared
    }else if(ic=="SBC"){
      PIC <- n*log(SSE/n)+log(n)*p*nY
    }
  }else if(method=="Likelihood"){
    ll <- logLik(fit)[1]
    k <- attr(logLik(fit),"df")
    if(cox==TRUE){
      n <- fit$nevent
    }else{
      n <- nrow(fit$data)
    }
    if(ic=="IC(1)"){
      PIC <- -2*ll+k
    }else if(ic=="IC(3/2)"){
      PIC <- -2*ll+1.5*k
    }else if(ic=="SBC"){
      PIC <- -2*ll+k*log(n)
    }else if(ic=="AICc"){
      PIC <- -2*ll+2*k*(k+1)/(n-k-1)
    }else if(ic=="AIC"){
      PIC <- -2*ll+2*k
    }else if(ic=="HQ"){
      PIC <- -2*ll+2*k*log(log(n))/n
    }else if(ic=="HQc"){
      PIC <- -2*ll+2*k*log(log(n))/(n-k-2)
    }
  }
  return(PIC)
}

