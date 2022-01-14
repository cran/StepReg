#' Stepwise Cox Proportional Hazards Regression
#' 
#' Stepwise Cox regression analysis selects model based on information criteria and significant test with 'forward', 'backward', 'bidirection' and 'score' variable selection method.
#' 
#' @param formula Model formulae. The models fitted by the coxph functions are specified in a compact symbolic form. The basic structure of a formula is the tilde symbol (~) and at least one independent (righthand) variable. In most (but not all) situations, a single dependent (lefthand) variable is also needed. Thus we can construct a formula quite simple formula (y ~ x). Multiple independent variables by simply separating them with the plus (+) symbol (y ~ x1 + x2). Variables in the formula are removed with a minus(-) symbol (y ~ x1 - x2). One particularly useful feature is the . operator when modelling with lots of variables (y ~ .). The %in% operator indicates that the terms on its left are nested within those on the right. For example y ~ x1 + x2 %in% x1 expands to the formula y ~ x1 + x1:x2.
#' 
#' @param data Data set including dependent and independent variables to be analyzed
#' 
#' @param include Force the effects vector listed in the data to be included in all models. The selection methods are performed on the other effects in the data set
#' 
#' @param selection Model selection method including "forward", "backward", "bidirection" and 'score',forward selection starts with no effects in the model and adds effects, backward selection starts with all effects in the model and removes effects, while bidirection regression is similar to the forward method except that effects already in the model do not necessarily stay there, and score method requests best subset selection.
#' 
#' @param select Specify the criterion that uses to determine the order in which effects enter and leave at each step of the specified selection method including AIC, AICc, SBC, IC(1), IC(3/2), HQ, HQc and Significant Levels(SL)
#' 
#' @param sle Specify the significance level for entry, default is 0.15
#' 
#' @param sls Specify the significance level for staying in the model, default is 0.15
#' 
#' @param method Specify the method for tie handling. If there are no tied death times all the methods are equivalent. Nearly all Cox regression programs use the Breslow method by default, but not this one. The Efron approximation is used as the default here, it is more accurate when dealing with tied death times, and is as efficient computationally. The “exact partial likelihood is equivalent to a conditional logistic model, and is appropriate when the times are a small set of discrete values.
#' 
#' @param weights Numeric vector to provide a weight for each observation in the input data set. Note that weights should be ranged from 0 to 1, while negative numbers are forcibly converted to 0, and numbers greater than 1 are forcibly converted to 1. If you do not specify a weight vector, each observation has a default weight of 1.
#'
#' @param best Control the number of models displayed in the output, default is NULL which means all possible model will be displayed
#' 
#' @import survival
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
#' @author Junhui Li
#' 
#' @examples
#' lung <- survival::lung
#' my.data <- na.omit(lung)
#' my.data$status1 <- ifelse(my.data$status==2,1,0)
#' data <- my.data
#' formula = Surv(time, status1) ~ . - status
#' 
#' stepwiseCox(formula,
#' data,
#' include=NULL,
#' selection=c("bidirection"),
#' select="HQ",
#' method=c("efron"),
#' sle=0.15,
#' sls=0.15,
#' weights=NULL,
#' best=NULL)
#' 
#' @export stepwiseCox

stepwiseCox <- function(formula,
                        data,
                        include=NULL,
                        selection=c("forward","backward","bidirection","score"),
                        select=c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)"),
                        sle=0.15,
                        sls=0.15,
                        method=c("efron","breslow","exact"), 
                        weights=NULL,
                        best=NULL){
  selection <- match.arg(selection)
  select <- match.arg(select)
  method <- match.arg(method)
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula,data=data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm,"term.labels")
  if(is.character(include)){
    if(!all(include %in% xName)){
      stop("variable in include is not included formula or dataset")
    }else{
      includeName <- include
      mergeIncName <- paste0(includeName,collapse=" ")
    }
  }else if(is.null(include)){
    includeName <- include
    mergeIncName <- "NULL"
  }else{
    stop("include should be character vector indicating variable to be included in all models")
  }
  fmFull <- reformulate(c(xName),yName)
  fitFull <- coxph(fmFull,data=data, weights=weights,method=method)
  allVarClass <- attr(fitFull$terms,"dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class","variable")
  for(i in names(table(allVarClass))){
    classTable[names(table(allVarClass)) %in% i,2] <- paste0(names(allVarClass[allVarClass %in% i]),collapse=" ")
  }
  classTable$class <- paste0(classTable$class,":")
  ## detect multicollinearity
  if(any(allVarClass=="factor")){
    factVar <- names(which(allVarClass=="factor"))
    for(i in factVar){
      data[,i] <- as.factor(as.numeric(data[,i]))
    }
  }
  xMatrix <- as.matrix(data[,xName])
  qrXList <- qr(xMatrix,tol=1e-7)
  rank0 <- qrXList$rank
  pivot0 <- qrXList$pivot
  if(rank0 < length(pivot0)){
    mulcolX <- colnames(qrXList$qr)[pivot0[(rank0+1):length(pivot0)]]
    mulcolMergeName <- paste0(mulcolX,collapse=" ")
  }else{
    mulcolX <- NULL
    mulcolMergeName <- "NULL"
  }
  xName <- setdiff(xName,mulcolX)
  n <- nrow(data)
  result <- list()
  ModInf <- matrix(NA,8,1)
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,selection,select,sle,sle,method,mulcolMergeName),8,1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("","")
  ModInf[,1] <- c("Response Variable = ",
                  "Included Variable = ",
                  "Selection Method = ",
                  "Select Criterion = ",
                  "Entry Significance Level(sle) = ",
                  "Stay Significance Level(sls) = ",
                  "Multicollinearity Terms = ",
                  "Method = ")
  if(select=="SL"){
    if(selection=="forward"){
      ModInf <- ModInf[-6,]
    }else if(selection=="backward"){
      ModInf <- ModInf[-5,]
    }else if(selection=="score"){
      ModInf <- ModInf[-c(5:6),]
    }
  }else{
    ModInf <- ModInf[-c(5:6),]
  }
  rownames(ModInf) <- 1:nrow(ModInf)
  result$'Basic Information' <- ModInf
  result$'Variable Class' <- classTable
  if(selection=="score"){ #score
    bestSubSet <- NULL
    singSet <- matrix(NA,1,3)
    colnames(singSet) <- c("NumberOfVariables",select,"VariablesInModel")
    finalResult <- singSet
    if(length(includeName)!=0){
      fm <- reformulate(c(includeName), yName)
      fit <- coxph(fm,data=data, weights=weights,method=method)
      if(select=="SL"){
        #PIC <- summary(fit)[[sigMethod]][1]
        PIC <- fit$score
      }else{
        PIC <- modelFitStat(select,fit,"Likelihood",TRUE)
      }
      singSet[1,1:3] <- c(length(attr(fit$terms,"term.labels")),PIC,paste0(c(includeName),collapse=" "))
      includeSubSet <- singSet
      xCheck <- setdiff(xName,includeName)
    }else{
      includeSubSet <- NULL
      xCheck <- xName
    }
    for(nv in 1:length(xCheck)){
      subSet <- NULL
      comTable <- combn(xCheck,nv)
      for(ncom in 1:ncol(comTable)){
        comVar <- c(includeName,comTable[,ncom])
        fm <- reformulate(comVar, yName)
        fit <- coxph(fm,data = data,weights=weights,method=method)
        if(select=="SL"){
          PIC <- fit$score
        }else{
          PIC <- modelFitStat(select,fit,"Likelihood",TRUE)
        }
        singSet[1,1:3] <- c(attr(logLik(fit),"df"),PIC,paste0(comVar,collapse=" "))
        subSet <- rbind(subSet,singSet)
      }
      bestSubSet <- as.data.frame(subSet)
      bestSubSet[,2] <- as.numeric(bestSubSet[,2])
      if(select=="SL"){
        subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
      }else{
        subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
      }
      if(is.null(best)){
        nbest <- nrow(subResultSort)
      }else{
        if(nrow(subResultSort)<best){
          nbest <- nrow(subResultSort)
        }else{
          nbest <- best
        }
      }
      finalResult <- rbind(finalResult,subResultSort[1:nbest,])
    }
    finalResult <- finalResult[-1,]
    RegPIC <- rbind(includeSubSet,finalResult)
    rownames(RegPIC) <- c(1:nrow(RegPIC))
    result$Process <- RegPIC
  }else{ #forward # bidirection # backward
    subBestPoint <- data.frame(Step=numeric(),
                               EnteredEffect=character(),
                               RemovedEffect=character(),
                               DF=numeric(),
                               NumberIn=numeric(),
                               select=numeric())
    colnames(subBestPoint)[6] <- select
    bestPoint <- subBestPoint
    if(selection=="backward"){
      addVar <- FALSE
      xModel <- c(includeName,setdiff(xName,includeName))
      xResidual <- NULL
      fmFull <- reformulate(xModel, yName)
      fitFull <- coxph(fmFull,data=data,weights=weights,method=method)
      if(select=="SL"){
        PIC <- 1
      }else{
        PIC <- modelFitStat(select,fitFull,"Likelihood",TRUE)
      }
      k <- attr(logLik(fitFull),"df")
      bestPoint[1,-1] <- c("","","",k,PIC)
    }else{
      addVar <- TRUE
      xModel <- c(includeName)
      xResidual <- setdiff(xName,includeName)
      if(select=="SL"){
        PIC <- 1
      }else{
        PIC <- Inf
      }
      bestPoint[1,] <- c(0,"","",0,0,PIC)
      if(!is.null(includeName)){
        fmInt <- reformulate("0",yName)
        fitInt <- coxph(fmInt,data=data,weights=weights,method=method)
        fmInc <- reformulate(includeName,yName)
        fitInc <- coxph(fmInc,data=data,weights=weights,method=method)
        if(select=="SL"){
          PIC <- anova(fitInt,fitInc)[2,'P(>|Chi|)']
        }else{
          PIC <- modelFitStat(select,fitInc,"Likelihood",TRUE)
        }
        k <- attr(logLik(fitInc),"df")
        subBestPoint[1,-1] <- c(paste0(includeName,collapse=" "),"",anova(fitInt,fitInc)[2,'Df'],k,PIC)
        bestPoint <- rbind(bestPoint,subBestPoint)
      }
    }
    while(TRUE){
      if(addVar==TRUE){
        if(is.null(xModel)){
          xMod <- "0"
        }else{
          xMod <- xModel
        }
        fm0 <- reformulate(xMod, yName)
        fit0 <- coxph(fm0,data = data,weights=weights,method=method)
		if(length(xResidual)==0){
		  break
		}
        xResidualList <- as.list(xResidual)
        names(xResidualList) <- xResidual
        fm1 <- lapply(xResidualList,function(x){reformulate(c(xModel,x),yName)})
        fit1 <- lapply(fm1,function(x){coxph(x,data = data,weights=weights,method=method)})
        if(select=="SL"){
          threshold <- sle
          PICset <- sapply(fit1,function(x){anova(fit0,x)[2,'P(>|Chi|)']})
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PICset <- sapply(fit1,function(x){modelFitStat(select,x,"Likelihood",TRUE)})
        }
        mPIC <- min(PICset)
        minmaxVar <- names(which.min(PICset))
        minmaxFit1 <- fit1[[minmaxVar]]
        if(mPIC < threshold){
          indicator <- TRUE
          xModel <- append(xModel,minmaxVar)
          xResidual <- setdiff(xResidual,minmaxVar)
          k <- attr(logLik(minmaxFit1),"df")
          subBestPoint[1,-1] <- c(minmaxVar,"",anova(fit0,minmaxFit1)[2,'Df'],k,mPIC)
          bestPoint <- rbind(bestPoint,subBestPoint)
        }else{
          indicator <- FALSE
        }
      }else{
        fm1 <- reformulate(xModel,yName)
        fit1 <- coxph(fm1,data=data,weights=weights,method=method)
        xChcek <- setdiff(xModel,c(includeName))
        if(is.null(xChcek)){
          break
        }else if(length(xChcek)==1){
          fm0 <- list(reformulate("0",yName))
          names(fm0) <- xChcek
        }else{
          xChcekList <- as.list(xChcek)
          names(xChcekList) <- xChcek
          fm0 <- lapply(xChcekList,function(x){reformulate(setdiff(xModel,x),yName)})
        }
        fit0 <- lapply(fm0,function(x){coxph(x,data=data,weights=weights,method=method)})
        if(select=="SL"){
          threshold <- sls
          PIC <- sapply(fit0,function(x){anova(x,fit1)[2,'P(>|Chi|)']})
          mPIC <- max(PIC)
          minmaxVar <- names(which.max(PIC))
          if(mPIC > threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PIC <- sapply(fit0,function(x){modelFitStat(select,x,"Likelihood",TRUE)})
          mPIC <- min(PIC)
          minmaxVar <- names(which.min(PIC))
          if(mPIC < threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }
        if(indicator==TRUE){
          minmaxFit0 <- fit0[[minmaxVar]]
          xResidual <- append(xResidual,minmaxVar)
          xModel <- setdiff(xModel,minmaxVar)
          k <- attr(logLik(minmaxFit0),"df")
          subBestPoint[1,-1] <- c("",minmaxVar,anova(minmaxFit0)[2,'Df'],k,mPIC)
          bestPoint <- rbind(bestPoint,subBestPoint)
        }
      }
      ## change direction or stop for this while loop
      if(indicator==TRUE){
        if(selection=="bidirection"){
          if(addVar==TRUE){
            addVar <- FALSE
          }else{
            addVar <- TRUE
          }
          next
        }else{
          next
        }
      }else{
        if(selection=="bidirection" && addVar==TRUE){
          break
        }else if(selection=="bidirection" && addVar==FALSE){
          addVar <- TRUE
          next
        }else if(selection != "bidirecion"){
          break
        }
      }
    }#while
    if(selection!="backward"){
      bestPoint <- bestPoint[-1,]
      if(is.null(includeName)){
        nInc <- 0
      }else{
        nInc <- 1
      }
      if(nInc<nrow(bestPoint)){
        bestPoint[,1] <- c(rep(0,nInc),1:(nrow(bestPoint)-nInc))
      }
    }else{
      bestPoint[,1] <- c(1:nrow(bestPoint)-1)
    }
    
    lastModel <- reformulate(xModel,yName)
    lastFit <- coxph(lastModel,data,weights=weights,method=method)
    MLE <- coef(summary(lastFit))
    result$Process <- bestPoint
    result$Variables <- xModel
    result$Coefficients <- MLE
  }
  return(result)
}