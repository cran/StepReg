#' Stepwise Linear Model Regression
#' 
#' Stepwise linear regression analysis selects model based on information criteria and F or approximate F test with 'forward', 'backward', 'bidirection' and 'score' model selection method.
#' 
#' @param formula Model formulae. The models fitted by the lm functions are specified in a compact symbolic form. The basic structure of a formula is the tilde symbol (~) and at least one independent (righthand) variable. In most (but not all) situations, a single dependent (lefthand) variable is also needed. Thus we can construct a formula quite simple formula (y ~ x). Multiple independent variables by simply separating them with the plus (+) symbol (y ~ x1 + x2). Variables in the formula are removed with a minus(-) symbol (y ~ x1 - x2). One particularly useful feature is the . operator when modelling with lots of variables (y ~ .). The \%in\% operator indicates that the terms on its left are nested within those on the right. For example y ~ x1 + x2 \%in\% x1 expands to the formula y ~ x1 + x1:x2. A model with no intercept can be specified as y ~ x - 1 or y ~ x + 0 or y ~ 0 + x. Multivariate multiple regression can be specified as cbind(y1,y2) ~ x1 + x2.
#'
#' @param data Data set including dependent and independent variables to be analyzed
#'
#' @param include Force vector of effects name to be included in all models.
#' 
#' @param selection Model selection method including "forward", "backward", "bidirection" and 'score',forward selection starts with no effects in the model and adds effects, backward selection starts with all effects in the model and removes effects, while bidirection regression is similar to the forward method except that effects already in the model do not necessarily stay there, and score method requests specifies the best-subset selection method, which uses the branch-and-bound technique to efficiently search for subsets of model effects that best predict the response variable.
#' 
#' @param select Specify the criterion that uses to determine the order in which effects enter and leave at each step of the specified selection method including "AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SBC" and "SL".
#' 
#' @param sle Specify the significance level for entry, default is 0.15
#' 
#' @param sls Specify the significance level for staying in the model, default is 0.15
#' 
#' @param weights Numeric vector to provide a weight for each observation in the input data set. Note that weights should be ranged from 0 to 1, while negative numbers are forcibly converted to 0, and numbers greater than 1 are forcibly converted to 1. If you do not specify a weight vector, each observation has a default weight of 1.
#' 
#' @param multivarStat Statistic for multivariate regression analysis, including Wilks' lamda ("Wilks"), Pillai Trace ("Pillai"), Hotelling-Lawley's Trace ("Hotelling"), Roy's Largest Root ("Roy")
#'
#' @param best Control the number of models displayed in the output, default is NULL, which means all possible model will be displayed.
#' 
#' @references 
#' 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).
#' 
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' 
#' Dharmawansa, P. , Nadler, B. , & Shwartz, O. . (2014). Roy's largest root under rank-one alternatives:the complex valued case and applications. Statistics.
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
#' Pillai, K. . (1955). Some new test criteria in multivariate analysis. The Annals of Mathematical Statistics, 26(1), 117-121.
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
#' mtcars$yes <- mtcars$wt
#' formula <- cbind(mpg,drat) ~ . + 0
#' stepwise(formula=formula,
#'          data=mtcars,
#'          selection="bidirection",
#'          select="AIC")
#' @keywords stepwise regression
#' 
#' @importFrom utils combn
#' 
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms
#' 
#' @export
#' 
stepwise <- function(formula,
                     data,
                     include=NULL,
                     selection=c("forward","backward","bidirection","score"),
                     select=c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SL","SBC"),
                     sle=0.15,
                     sls=0.15,
                     multivarStat=c("Pillai","Wilks","Hotelling-Lawley","Roy"),
                     weights=NULL,
                     best=NULL){
  selection <- match.arg(selection)
  select <- match.arg(select)
  multivarStat <- match.arg(multivarStat)
  ## score and SL
  if(selection=="score" & select=="SL"){
    stop("select = 'SL' is not allowed when specifing selection = 'score'")
  }
  ## extract response, independent variable and intercept
  stopifnot(inherits(formula, "formula"))
  termForm <- terms(formula,data=data)
  #yName <- rownames(attr(termForm,"factors"))[1]
  #yName <- all.vars(formula)[1]
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
    includeName <- NULL
    mergeIncName <- "NULL"
  }else{
    stop("include should be character vector indicating variable to be included in all models")
  }
  if(!is.null(weights)){
    if(length(weights)==nrow(data)){
      weightData <- data*sqrt(weights)
    }else{
      stop("Variable length is different ('(weights)')")
    }
  }else{
    weightData <- data
  }

  lmFull <- lm(formula,data=weightData)
  allVarClass <- attr(lmFull$terms,"dataClasses")
  classTable <- as.data.frame(table(allVarClass))
  colnames(classTable) <- c("class","variable")
  for(i in names(table(allVarClass))){
    classTable[names(table(allVarClass)) %in% i,2] <- paste0(names(allVarClass[allVarClass %in% i]),collapse=" ")
  }
  ## detect multicollinearity
  if(any(allVarClass=="factor")){
    factVar <- names(which(allVarClass=="factor"))
    for(i in factVar){
      weightData[,i] <- as.factor(as.numeric(weightData[,i]))
    }
  }
  xMatrix <- as.matrix(weightData[,xName])
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
  Y <- as.matrix(lmFull$model[,yName])
  nY <- ncol(Y)
  nObs <- nrow(data)
  # get sigma for BIC and CP
  if(nY==1){
    approxF <- "F"
  }else{
    approxF <- multivarStat
    if(any(c(select)==c("BIC","CP","Rsq","adjRsq"))){
      stop("Can't specify 'BIC','CP','Rsq' or 'adjRsq' when using multivariate multiple regression")
    }
  }
  if((select=="CP" | select=='BIC') & lmFull$rank >= nObs){
    stop("'select' can't specify 'CP' or 'BIC' when variable number is greater than number of observation")
  }
  result <- list()
  ModInf <- matrix(NA,9,1)
  ModInf <- cbind(ModInf,matrix(c(yName,mergeIncName,selection,select,sle,sle,approxF,mulcolMergeName,intercept),9,1))
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
  if(selection=="score"){
    ## best subset model selection
    tempresult <- matrix(NA,1,4)
    colnames(tempresult) <- c("NoVariable","RankModel",select,"VariablesEnteredinModel")
    finalResult <- tempresult
    if(!is.null(includeName)){
      lmIncForm <- reformulate(c(intercept,includeName), yName)
      lmInc <- lm(lmIncForm,data=weightData)
      tempresult[1,c(1:4)] <- c(length(attr(lmInc$terms,"term.labels")),lmInc$rank,modelFitStat(select,lmInc,"LeastSquare"),paste(c(intercept,includeName),collapse=" "))
      finalResult <- rbind(finalResult,tempresult)
      checkX <- xName[!xName %in% includeName]
    }else{
      checkX <- xName
    }
    for(nv in 1:length(checkX)){
      comTable <- combn(length(checkX),nv)
      subSet <- NULL
      for(ncom in 1:ncol(comTable)){
        comVar <- c(intercept,includeName,checkX[comTable[,ncom]])
        tempFormula <- reformulate(comVar, yName)
        lmresult <- lm(tempFormula,data=weightData)
        tempresult[1,1:4] <- c(length(attr(lmresult$terms,"term.labels")),lmresult$rank,modelFitStat(select,lmresult,"LeastSquare"),paste(comVar,collapse=" "))
        subSet <- rbind(subSet,tempresult)
      }
      if(is.null(best)){
        nbest <- nrow(subSet)
      }else{
        if(nrow(subSet)>best){
          nbest <- best
        }else{
          nbest <- nrow(subSet)
        }
      }
      bestSubSet <- as.data.frame(subSet)
      bestSubSet[,2] <- as.numeric(bestSubSet[,2])
      if(select=="Rsq" | select=="adjRsq"){
        subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = TRUE),]
      }else{
        subResultSort <- bestSubSet[order(bestSubSet[,2],decreasing = FALSE),]
      }
      finalResult <- rbind(finalResult,subResultSort[1:nbest,])
    }
    finalResult <- finalResult[-1,]
    rownames(finalResult) <- 1:nrow(finalResult)
    result$'Process of Selection' <- finalResult
    if(select=="Rsq" | select=="adjRsq"){
      xModel <- unlist(strsplit(finalResult[which.max(as.numeric(finalResult[,3])),4]," "))
    }else{
      xModel <- unlist(strsplit(finalResult[which.min(as.numeric(finalResult[,3])),4]," "))
    }
  }else{
    subBestPoint <- data.frame(Step=numeric(),
                               EnteredEffect=character(),
                               RemovedEffect=character(),
                               DF=numeric(),
                               NumberEffectIn=numeric(),
                               NumberParmsIn=numeric(),
                               select=numeric())
    colnames(subBestPoint)[7] <- select
    bestPoint <- subBestPoint
    if(selection == "backward"){
      addIdx <- FALSE
      xModel <- c(intercept,includeName,setdiff(xName,includeName))
      xResidual <- NULL
      if (select == 'SL') {
        PIC <- 1
      }else{
        PIC <- modelFitStat(select,lmFull,"LeastSquare")
      }
      bestPoint[1,] <- c(0,"","","",length(attr(lmFull$terms,"term.labels")),lmFull$rank,PIC)
    }else{
      addIdx <- TRUE
      xModel <- c(intercept,includeName)
      xResidual <- setdiff(xName,includeName)
      fmInt <- reformulate(intercept, yName)
      fitInt <- lm(fmInt,data=weightData)
      if(select == 'SL') {
        PIC <- 1
      }else{
        if(intercept=='1'){
          PIC <- modelFitStat(select,fitInt,"LeastSquare")
        }else{
          if(select %in% c("Rsq","adjRsq")){
            PIC <- 0
          }else{
            PIC <- Inf
          }
        }
      }
      bestPoint[1,] <- c(0,intercept,"",fitInt$rank,length(attr(fitInt$terms,"term.labels")),fitInt$rank,PIC)
      if(!is.null(includeName)){
        fmInc <- reformulate(c(intercept,includeName),yName)
        fitInc <- lm(fmInc,data=weightData)
        if(select == 'SL') {
          PIC <- anova(fitInc,fitInt,test=approxF)[2,'Pr(>F)']
        }else{
          PIC <- modelFitStat(select,fitInc,"LeastSquare")
        }
        subBestPoint[1,] <- c(0,mergeIncName,"",anova(fitInc,fitInt,test=approxF)[2,'Df'],length(attr(fitInt$terms,"term.labels")),fitInc$rank,PIC)
        bestPoint <- rbind(bestPoint,subBestPoint)
      }
    }
    while(TRUE){
      fm0 <- reformulate(xModel,yName)
      lmAlt <- lm(fm0,data=weightData)
      if(addIdx==TRUE){
        xCheck <- xResidual
        if(length(xCheck)==0){
          break
        }
        xCheckList <- as.list(xCheck)
        names(xCheckList) <- xCheck
        fmX <- lapply(xCheckList, function(x){reformulate(c(xModel,x),yName)})
      }else{
        xCheck <- setdiff(xModel,c(intercept,includeName))
        if(length(xCheck)==0){
          break
        }
        xCheckList <- as.list(xCheck)
        names(xCheckList) <- xCheck
        fmX <- lapply(xCheckList,function(x){reformulate(setdiff(xModel,x),yName)})
      }
      fitX <- lapply(fmX,function(x){lm(x,data=weightData)})
      if(select=="SL"){
        PICset <- sapply(fitX,function(x){anova(x,lmAlt,test=approxF)[2,'Pr(>F)']})
        Fset <- sapply(fitX,function(x){anova(x,lmAlt,test=approxF)[2,'F']})
      }else{
        if(addIdx==FALSE & length(xCheck)==1 & intercept=="0"){
          PICset <- Inf
          names(PICset) <- xCheck
        }else{
          PICset <- sapply(fitX,function(x){modelFitStat(select,x,"LeastSquare")})
        }
      }
      if(select=="Rsq" | select=="adjRsq" | (select=="SL" & addIdx==FALSE)){
        PIC <- max(PICset)
        minmaxVar <- names(which.max(PICset))
        bestLm <- fitX[[minmaxVar]]
      }else{
        PIC <- min(PICset)
        minmaxVar <- names(which.min(PICset))
        bestLm <- fitX[[minmaxVar]]
        if(sum(PICset %in% PIC)>1 & select=="SL"){
          Fvalue <- max(Fset)
          minmaxVar <- names(which.max(Fset))
          bestLm <- fitX[[minmaxVar]]
          PIC <- PICset[minmaxVar]
        }
      }
      if(bestLm$rank==lmAlt$rank & addIdx==TRUE){
        break
      }else{
        if(select=='SL'){
          if(addIdx==FALSE){
            indicator <- PIC > sls
          }else{
            indicator <- PIC < sle
          }
        }else if(select=='Rsq' | select=='adjRsq'){
          indicator <- PIC > as.numeric(bestPoint[nrow(bestPoint),7])
        }else{
          indicator <- PIC <= as.numeric(bestPoint[nrow(bestPoint),7])
        }
        if(indicator==TRUE){
          #goodness of fit
          smr <- summary(bestLm)
          if(nY==1){
            f <- smr$fstatistic
            if(is.nan(f[1])){
              pval <- NaN
            }else{
              pval <- pf(f[1],f[2],f[3],lower.tail=F)
            }
          }else{
            for(ny in 1:nY){
              f <- smr[[ny]]$fstatistic
              if(is.nan(f[1])){
                pval <- NaN
              }else{
                pval <- pf(f[1],f[2],f[3],lower.tail=F)
              }
            }
          }
          if(is.nan(pval)==TRUE & (select!='Rsq' | select!='adjRsq')){
            break
          }
          if(addIdx==TRUE){
            xModel <- append(xModel,minmaxVar)
            xResidual <- setdiff(xResidual,minmaxVar)
            subBestPoint[1,] <- c(as.numeric(bestPoint[nrow(bestPoint),1])+1,minmaxVar,"",anova(lmAlt,bestLm,test=approxF)[2,'Df'],length(attr(bestLm$terms,"term.labels")),bestLm$rank,PIC)
          }else{
            xResidual <- append(xResidual,minmaxVar)
            xModel <- setdiff(xModel,minmaxVar)
            subBestPoint[1,] <- c(as.numeric(bestPoint[nrow(bestPoint),1])+1,"",minmaxVar,anova(bestLm,lmAlt,test=approxF)[2,'Df'],length(attr(bestLm$terms,"term.labels")),bestLm$rank,PIC)
          }
          bestPoint <- rbind(bestPoint,subBestPoint)
          
          if(selection == 'bidirection'){
            if(addIdx==FALSE){
              next
            }else{
              addIdx <- FALSE
              next
            }
          }else{
            next
          }
        }else{
          if(selection == 'bidirection' & addIdx==FALSE) {
            addIdx <- TRUE
            next
          }else{
            break
          }
        }
      }
    }#while
    bestPoint$DF <- abs(as.numeric(bestPoint$DF))
    bestPoint$DF[is.na(bestPoint$DF)] <- ""
    result$'Process of Selection' <- bestPoint
  }
  if(is.null(xModel)){
    parEst <- NULL
  }else{
    parEst <- summary(lm(reformulate(xModel,yName),data=weightData))
    parEstList <- list()
    if(nY>1){
      for(i in names(parEst)){
        subParEst <- parEst[[i]]$coefficients
        subParEst <- data.frame(rownames(subParEst),subParEst)
        colnames(subParEst) <- c("Variable","Estimate","StdError","t.value","P.value")
        parEstList[i] <- list(subParEst)
      }
    }else{
      subParEst <- parEst$coefficients
      subParEst <- data.frame(rownames(subParEst),subParEst)
      colnames(subParEst) <- c("Variable","Estimate","StdError","t.value","P.value")
      parEstList <- list(subParEst)
      names(parEstList) <- yName
    }
  }
  variables <- as.data.frame(t(data.frame(xModel)))
  colnames(variables) <- paste0("variables",1:length(xModel))
  result$'Selected Varaibles' <- variables
  result$'Coefficients of the Selected Variables' <- parEstList
  class(result) <- c("StepReg","list")
  return(result)
}
