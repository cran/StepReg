#' Stepwise Logistic Regression
#' 
#' Stepwise logistic regression analysis selects model based on information criteria and Wald or Score test with 'forward', 'backward', 'bidirection' and 'score' model selection method.
#' 
#' @param formula Model formulae. The models fitted by the glm functions are specified in a compact symbolic form. The basic structure of a formula is the tilde symbol (~) and at least one independent (righthand) variable. In most (but not all) situations, a single dependent (lefthand) variable is also needed. Thus we can construct a formula quite simple formula (y ~ x). Multiple independent variables by simply separating them with the plus (+) symbol (y ~ x1 + x2). Variables in the formula are removed with a minus(-) symbol (y ~ x1 - x2). One particularly useful feature is the . operator when modelling with lots of variables (y ~ .). The \%in\% operator indicates that the terms on its left are nested within those on the right. For example y ~ x1 + x2 \%in\% x1 expands to the formula y ~ x1 + x1:x2. A model with no intercept can be specified as y ~ x - 1 or y ~ x + 0 or y ~ 0 + x.
#' 
#' @param data Data set including dependent and independent variables to be analyzed
#' 
#' @param include Force the effects vector listed in the data to be included in all models. The selection methods are performed on the other effects in the data set
#' 
#' @param selection Model selection method including "forward", "backward", "bidirection" and 'score',forward selection starts with no effects in the model and adds effects, backward selection starts with all effects in the model and removes effects, while bidirection regression is similar to the forward method except that effects already in the model do not necessarily stay there, and score method requests best subset selection.
#' 
#' @param select Specify the criterion that uses to determine the order in which effects enter and leave at each step of the specified selection method including AIC, AICc, SBC, IC(1), IC(3/2), HQ, HQc and Significant Levels(SL)
#' 
#' @param sigMethod Specify the method of significant test for variable to be entered in the model. "Rao" and "LRT" cab be chosen for Rao's efficient score test and likelihood ratio test.
#' 
#' @param sle Specify the significance level for entry, default is 0.15
#' 
#' @param sls Specify the significance level for staying in the model, default is 0.15
#' 
#' @param weights Numeric vector to provide a weights for each observation in the input data set. Note that weights should be ranged from 0 to 1, while negative numbers are forcibly converted to 0, and numbers greater than 1 are forcibly converted to 1. If you do not specify a weights vector, each observation has a default weights of 1.
#'
#' @param best Control the number of models displayed in the output, default is NULL which means all possible model will be displayed
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
#' data(mtcars)
#' formula <- vs ~ .
#' stepwiseLogit(formula,
#'               data=mtcars,
#'               selection="bidirection",
#'               select="SL",
#'               sle=0.15,
#'               sls=0.15,
#'               sigMethod="Rao")
#' 
#' @keywords stepwise logistic regression
#'
#' @export

stepwiseLogit <- function(formula,
                          data,
                          include=NULL,
                          selection=c("forward","backward","bidirection","score"),
                          select=c("SL","AIC","AICc","SBC","HQ","HQc","IC(3/2)","IC(1)"),
                          sle=0.15,
                          sls=0.15,
                          sigMethod=c("Rao","LRT"),
                          weights=NULL,
                          best=NULL){
  selection <- match.arg(selection)
  select <- match.arg(select)
  sigMethod <- match.arg(sigMethod)
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula,data=data)
  vars <- as.character(attr(termForm, "variables"))[-1]
  yName <- vars[attr(termForm, "response")]
  xName <- attr(termForm,"term.labels")
  if(attr(termForm, "intercept")==0){
    intercept <- "0"
  }else{
    intercept <- "1"
  }
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
  #fit <- glm(formula,data=data, weights=weights, family="binomial")
  #https://stackoverflow.com/questions/8218196/object-not-found-error-when-passing-model-formula-to-another-function
  fmFull <- reformulate(c(intercept,xName),yName)
  fitFull <- glm(fmFull,data=data, weights=weights, family="binomial")
  allVarClass <- attr(fitFull$terms,"dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class","variable")
  for(i in names(table(allVarClass))){
    classTable[names(table(allVarClass)) %in% i,2] <- paste0(names(allVarClass[allVarClass %in% i]),collapse=" ")
  }
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
  ModInf <- matrix(NA,9,1)
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,selection,select,sle,sle,sigMethod,mulcolMergeName,intercept),9,1))
  ModInf <- data.frame(ModInf)
  colnames(ModInf) <- c("Paramters","Value")
  ModInf[,1] <- c("Response Variable",
                  "Included Variable",
                  "Selection Method",
                  "Select Criterion",
                  "Entry Significance Level(sle)",
                  "Stay Significance Level(sls)",
                  "Variable significance test",
                  "Multicollinearity Terms",
                  "Intercept")
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
  result$'Summary of Parameters' <- ModInf
  result$'Variables Type' <- classTable
  if(selection=="score"){ #score
    bestSubSet <- NULL
    singSet <- matrix(NA,1,3)
    colnames(singSet) <- c("NumberOfVariables",select,"VariablesInModel")
    finalResult <- singSet
    fmReduce <- reformulate(c(intercept), yName)
    fitReduce <- glm(fmReduce,data=data, weights=weights, family="binomial")
    if(length(includeName)!=0){
      fm <- reformulate(c(intercept,includeName), yName)
      #fit <- multinom(fm, data = YXdata, weights = weights)
      fit <- glm(fm,data=data, weights=weights, family="binomial")
      if(select=="SL"){
        PIC <- anova(fitReduce,fit,test="Rao")[2,"Rao"]
      }else{
        PIC <- modelFitStat(select,fit,"Likelihood")
      }
      singSet[1,1:3] <- c(fit$rank,PIC,paste0(c(intercept,includeName),collapse=" "))
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
        comVar <- c(intercept,includeName,comTable[,ncom])
        fm <- reformulate(comVar, yName)
        fit <- glm(fm,data = data,weights=weights,family="binomial")
        if(select=="SL"){
          PIC <- anova(fitReduce,fit, test="Rao")[2,"Rao"] 
        }else{
          PIC <- modelFitStat(select,fit,"Likelihood")
        }
        singSet[1,1:3] <- c(fit$rank,PIC,paste0(comVar,collapse=" "))
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
    result$'Process of Selection' <- RegPIC
    RegPIC[,2] %in% min(RegPIC[,2])
    if(select=="SL"){
      xModel <- unlist(strsplit(RegPIC[which.max(as.numeric(RegPIC[,2])),3]," "))
    }else{
      xModel <- unlist(strsplit(RegPIC[which.min(as.numeric(RegPIC[,2])),3]," "))
    }
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
      xModel <- c(intercept,includeName,setdiff(xName,includeName))
      xResidual <- NULL
      fmFull <- reformulate(xModel, yName)
      fitFull <- glm(fmFull,data=data,weights=weights,family="binomial")
      if(select=="SL"){
        PIC <- 1
      }else{
        PIC <- modelFitStat(select,fitFull,"Likelihood")
      }
      bestPoint[1,-1] <- c("","",fitFull$rank,fitFull$rank,PIC)
    }else{
      addVar <- TRUE
      xModel <- c(intercept,includeName)
      xResidual <- setdiff(xName,includeName)
      fmInt <- reformulate(intercept, yName)
      fitInt <- glm(fmInt,data=data,weights=weights,family="binomial")
      if(select=="SL"){
        PIC <- 1
      }else{
        if(intercept=="0"){
          PIC <- Inf
        }else{
          PIC <- modelFitStat(select,fitInt,"Likelihood")
        }
      }
      bestPoint[1,-1] <- c(intercept,"",fitInt$rank,fitInt$rank,PIC)
      if(!is.null(includeName)){
        fmInc <- reformulate(xModel, yName)
        fitInc <- glm(fmInc,data=data,weights=weights,family="binomial")
        if(select=="SL"){
          PIC <- anova(fitInt,fitInc,test=sigMethod)[2,'Pr(>Chi)']
        }else{
          PIC <- modelFitStat(select,fitInc,"Likelihood")
        }
        subBestPoint[1,-1] <- c(paste0(includeName,collapse=" "),"",anova(fitInt,fitInc)[2,'Df'],fitInc$rank,PIC)
        bestPoint <- rbind(bestPoint,subBestPoint)
      }
    }
    while(TRUE){
      if(addVar==TRUE){
        fm0 <- reformulate(xModel, yName)
        fit0 <- glm(fm0,data = data,weights=weights,family="binomial")
        if(length(xResidual)==0){
          break
        }
        xResidualList <- as.list(xResidual)
        names(xResidualList) <- xResidual
        fm1 <- lapply(xResidualList,function(x){reformulate(c(xModel,x),yName)})
        fit1 <- lapply(fm1,function(x){glm(x,data=data,weights=weights,family="binomial")})
        rank1 <- lapply(fit1,function(x){x$rank})
        mulColVar <- names(which(fit0$rank == rank1))
        if(length(mulColVar)>0){
          fit1 <- fit1[!names(fit1) %in% mulColVar]
        }
        if(select=="SL"){
          threshold <- sle
          PICset <- sapply(fit1,function(x){anova(fit0,x,test=sigMethod)[2,'Pr(>Chi)']})
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PICset <- sapply(fit1,function(x){modelFitStat(select,x,"Likelihood")})
        }
        mPIC <- min(PICset)
        minmaxVar <- names(which.min(PICset))
        minmaxFit1 <- fit1[[minmaxVar]]
        if(mPIC < threshold){
          indicator <- TRUE
          xModel <- append(xModel,minmaxVar)
          xResidual <- setdiff(xResidual,minmaxVar)
          subBestPoint[1,-1] <- c(minmaxVar,"",anova(fit0,minmaxFit1)[2,'Df'],minmaxFit1$rank,mPIC)
          bestPoint <- rbind(bestPoint,subBestPoint)
        }else{
          indicator <- FALSE
        }
      }else{
        fm1 <- reformulate(xModel,yName)
        fit1 <- glm(fm1,data=data,weights=weights,family="binomial")
        xChcek <- setdiff(xModel,c(intercept,includeName))
        if(is.null(xChcek)){
          break
        }
        xChcekList <- as.list(xChcek)
        names(xChcekList) <- xChcek
        fm0 <- lapply(xChcekList,function(x){reformulate(setdiff(xModel,x),yName)})
        fit0 <- lapply(fm0,function(x){glm(x,data=data,weights=weights,family="binomial")})
        if(select=="SL"){
          threshold <- sls
          PIC <- sapply(fit0,function(x){anova(x,fit1,test=sigMethod)[2,'Pr(>Chi)']})
          mPIC <- max(PIC)
          minmaxVar <- names(which.max(PIC))
          if(mPIC > threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }else{
          threshold <- as.numeric(bestPoint[nrow(bestPoint),6])
          PIC <- sapply(fit0,function(x){modelFitStat(select,x,"Likelihood")})
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
          subBestPoint[1,-1] <- c("",minmaxVar,anova(minmaxFit0)[2,'Df'],minmaxFit0$rank,mPIC)
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
      if(is.null(includeName)){
        nInc <- 0
      }else{
        nInc <- 1
      }
      if(1+nInc<nrow(bestPoint)){
        bestPoint[,1] <- c(rep(0,1+nInc),1:(nrow(bestPoint)-1-nInc))
      }
    }else{
      bestPoint[,1] <- c(1:nrow(bestPoint))
    }
    result$'Process of Selection' <- bestPoint
  }
  lastModel <- reformulate(xModel,yName)
  lastFit <- glm(lastModel,data=data,weights=weights,family="binomial")
  MLE <- coef(summary(lastFit))
  MLE <- data.frame(rownames(MLE),MLE)
  colnames(MLE) <- c("Variable","Estimate","StdError","t.value","P.value")
  variables <- as.data.frame(t(data.frame(xModel)))
  colnames(variables) <- paste0("variables",1:length(xModel))
  result$'Selected Varaibles' <- variables
  result$'Coefficients of the Selected Variables' <- MLE
  class(result) <- c("StepReg","list")
  return(result)
}
