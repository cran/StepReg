stepwise <- function(data,y,exclude=NULL,include=NULL,Class=NULL,weights=c(rep(1,nrow(data))),selection="bidirection",select="SBC",sle=0.15,sls=0.15,tolerance=1e-7,Trace="Pillai",Choose=NULL){
  ##selection
  if(!is.character(selection) | !selection %in% c("forward","bidirection","backward")){
    stop("'selection' must be one of 'forward','bidirection','backward'")
  }
  ##select and Choose
  if(!is.character(select) | !select %in% c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SL","SBC")){
    stop("'select' must be one of 'AIC','AICc','BIC','CP','HQ','HQc','Rsq','adjRsq','SL','SBC'")
  }
  if(!is.null(Choose)){
    if(!is.character(Choose) | !Choose %in% c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SBC")){
      stop("'Choose' must be one of 'AIC','AICc','BIC','CP','HQ','HQc','Rsq','adjRsq',NULL,'SBC'")
    }
  }
  ## weights
  if (!is.null(weights) & !is.numeric(weights)){
    stop("'weights' must be a numeric vector")
  }
  if(any(weights < 0) | any(weights > 1)){
    if(any(weights < 0)){
      warning("'weights' with negtive values are forcibly converted to 0")
      weights[weights < 0] <- 0
    }
    if(any(weights > 1)){
      warning("'weights' greater than 1 are forcibly converted to 1")
      weights[weights > 1] <- 1
    }
    
    warning("'weights' should be a numeric vector ranging from 0 to 1")
  }
  if(any(weights==0)){
    data <- data[which(weights!=0),]
    weights <- weights[weights != 0]
  }
  ## y 
  nameset <- colnames(data)
  if(is.numeric(y)){
    if(0 %in% y){
      stop("0 should be remove from 'y'")
    }else{
      Yname <- nameset[y]
    }
  }else if(is.character(y)){
    if(!all(y %in% nameset)){
      stop("'y' should be included in data set")
    }else{
      Yname <- y
    }
  }else{
    stop("'y' should be numeric or character vector")
  }
  nY <- length(Yname)
  Y <- as.matrix(data[,Yname])
  colnames(Y) <- Yname
  ## exclude
  if(is.numeric(exclude)){
    if(0 %in% exclude){
      stop("0 should be remove from exclude")
    }else{
      notXname <- colnames(data)[exclude]
    }
  }else if(is.character(exclude)){
    if(!all(exclude %in% nameset)){
      stop("'exclude' should be included in data set")
    }else{
      notXname <- exclude
    }
  }else if(is.null(exclude)){
    notXname <- exclude
  }else{
    stop("illegal 'exclude' variable")
  }
  X <- as.matrix(data[,!nameset %in% c(Yname,notXname)])
  
  if(nrow(Y) != nrow(X)){
    stop("The rows of y does not equals independent variable")
  }else{
    nObs <- nrow(X)
  }
  Xf <- X
  nX <- ncol(Xf)
  XName <- colnames(Xf)
  ## Class
  if(is.null(Class)){
    Classname <- NULL
  }else{
    if(length(Class) > 1){
      stop("class effect should be catalogy variable and should be not greater than one variable")
    }else{
      if(is.numeric(Class)){
        if(0 %in% Class){
          stop("0 should be remove from Class")
        }else{
          Classname <- colnames(data)[Class]
        }
      }else if(is.character(Class)){
        Classname <- Class
      }
    }
  }
  if(is.null(Classname)){
    NoClass <- 1
  }else{
    NestClass <- as.numeric(data[,Classname])
    NoClass <- nlevels(as.factor(NestClass))
    VecClass <- levels(as.factor(NestClass))
  }
  ## Total sum of squares corrected for the mean for the dependent variable
  Ysst <- round(Y,2)
  Yd <- Ysst-mean(Ysst)
  SST <- t(Yd) %*% (Yd)
  SST <- t(Yd*sqrt(weights)) %*% (Yd*sqrt(weights))
  SSTdet <- abs(det(SST))
  ## weighted data
  b <- matrix(1,nObs,1)*sqrt(weights)
  colnames(b) <- "Intercept"
  Y <- Y*sqrt(weights)
  Xf <- Xf*sqrt(weights)
  qrxf <- qr(Xf)
  ## continous to continous-nesting-class
  if(NoClass > 1){
    Xf1 <- NULL
    NoSub <- rep(0,NoClass+1)
    for (k in 1:NoClass){
      #k=1
      tempNo <- sum(NestClass %in% VecClass[k])
      NoSub[1+k] <- tempNo+NoSub[k]
      X_part <- matrix(0,tempNo,NoClass*nX)
      X_part[,seq(k,NoClass*nX,by=NoClass)] <- Xf[(NoSub[k]+1):NoSub[k+1],]
      Xf1 <- rbind(Xf1,X_part)
    }
    Xf <- Xf1
  }
  # get sigma for BIC and CP
  if(ncol(Xf)>nObs){
    sigmaVal <- 0
  }else{
    Ys <- Y/sqrt(weights)
    Xfs <- Xf/sqrt(weights)
    lmf <- lm(Ys~Xfs,weights = weights)
    sigmaVal <- sum(deviance(lmf)/df.residual(lmf))/nY
  }
  if(is.null(Choose)){
    if(select=="CP" & sigmaVal == 0){
      stop("The CP criterion cannot be used as sigma=0 for the full model")
    }
  }else{
    if((select=="CP" | Choose=="CP") & sigmaVal == 0){
      stop("The CP criterion cannot be used as sigma=0 for the full model")
    }
  }
  #include
  if(is.null(include)){
    includename <- NULL
  }else if(is.numeric(include)){
    includename <- nameset[include]
  }else if(is.character(include)){
    if(all(include %in% XName)){
      includename <- include
    }
  }
  ##
  if(length(notXname)*length(includename) != 0){
    if(any(notXname %in% includename)){
      stop("elements in exclude should not be listed in include")
    }
  }
  ##include intercept
  nInclude <- 1
  if(length(includename) != 0){
    includek <- which(XName %in% includename)
    nInclude <- c(1,includek+1) #modified by junhuili @ 20191012
    includeAll <- 0
    for(k in includek){
      includeAll <- append(includeAll,(NoClass*(k-1)+1):(k*NoClass))
    }
    b <- cbind(b,Xf[,includeAll])
  }
  nk <- length(includename)
  
  slcOpt <- list(serial = 'numeric', bestValue = 'numeric', bestPoint = 'numeric', enOrRe = 'logical', nVarIn = 'numeric')
  class(slcOpt) <- select
  if(!is.null(Choose)){
    chsOpt <- list(serial = 'numeric', bestValue = 'numeric', bestPoint = 'numeric', enOrRe = 'logical', nVarIn = 'numeric')
    class(chsOpt) <- Choose
  }
  
  # initial tempX for step0
  if(selection=="backward"){
    tmpX <- cbind(b,Xf)
    tmpX1 <- as.matrix(tmpX)
    qrX <- qr(tmpX1)
    p <- qrX$rank
    tmpX1 <- tmpX1[,qrX$pivot[1:p]]
    IdentityVec <- diag(nObs)
    tempSSEp <- tmpX1 %*% solve(t(tmpX1) %*% tmpX1) %*% t(tmpX1)
    SSEp <- t(Y)%*%(IdentityVec-tempSSEp)%*%Y
    SSEpSq <- t(SSEp)%*%SSEp
    SSESqdet <- abs(det(SSEpSq))
    SSEdet <- abs(det(SSEp))
    if(!is.null(Choose)){
      chsOpt$bestValue <- ModelFitStat(class(chsOpt),SSEdet,SSTdet,nObs,nY,p,sigmaVal)
    }
    # Calculate bestValue of select
    if (class(slcOpt) == 'SL') {
      slcOpt$bestValue <- 1
    }else{
      slcOpt$bestValue <- ModelFitStat(class(slcOpt),SSEdet,SSTdet,nObs,nY,p,sigmaVal)
    }
    slcOpt$serial <- 1
    slcOpt$bestPoint <- c(0:nX)
    slcOpt$nVarIn <- p
    if(p<nX){
      slcOpt$enOrRe <- c(rep(TRUE,p/NoClass),rep(FALSE,nX-p/NoClass))
    }else{
      slcOpt$enOrRe[1:(p/NoClass)] <- rep(TRUE,p/NoClass)
    }
  }else{
    tmpX <- b
    IdentityVec <- diag(nObs)
    for(k in 0:length(includename)){
      tmpX1 <- as.matrix(tmpX[,c(1:(k*NoClass+1))])
      qrX <- qr(tmpX1)
      p <- qrX$rank
      tmpX1 <- tmpX1[,qrX$pivot[1:p]]
      tempSSEp <- tmpX1 %*% solve(t(tmpX1) %*% tmpX1) %*% t(tmpX1)
      SSEp <- t(Y)%*%(IdentityVec-tempSSEp)%*%Y
      SSEpSq <- t(SSEp)%*%SSEp
      SSESqdet <- abs(det(SSEpSq))
      SSEdet <- abs(det(SSEp))
      if(!is.null(Choose)){
        chsOptbestValue <- ModelFitStat(class(chsOpt),SSEdet,SSTdet,nObs,nY,p,sigmaVal)
      }
      # Calculate bestValue of select
      if (class(slcOpt) == 'SL') {
        slcOptbestValue <- 1
      }else{
        slcOptbestValue <- ModelFitStat(class(slcOpt),SSEdet,SSTdet,nObs,nY,p,sigmaVal)
      }
      # Calculate select
      if(k!=0){
        slcOpt$serial <- slcOpt$serial + 1
        slcOpt$bestPoint[slcOpt$serial] <- which(XName %in% includename[k])
        slcOpt$bestValue[slcOpt$serial] <- slcOptbestValue
        slcOpt$enOrRe[slcOpt$serial] <- TRUE
        slcOpt$nVarIn[slcOpt$serial] <- p
        if (!is.null(Choose)){
          chsOpt$bestValue[slcOpt$serial] <- chsOptbestValue
        }
      }else{
        slcOpt$serial <- 1
        slcOpt$bestPoint <- 0
        slcOpt$enOrRe <- TRUE
        slcOpt$nVarIn <- p
        #forced Rsq=0 and adjRsq=0 with weighted data when Xf=b
        if(select=="Rsq" | select=="adjRsq"){
          slcOpt$bestValue <- 0
        }else{
          slcOpt$bestValue <- slcOptbestValue
        }
        if (!is.null(Choose)){
          #forced Rsq=0 and adjRsq=0 with weighted data when Xf=b
          if(Choose=="Rsq" | Choose=="adjRsq"){
            chsOpt$bestValue <- 0
          }else{
            chsOpt$bestValue <- chsOptbestValue
          }
        }
      }
    }
  }
  if(selection=="backward"){
    addVar <- FALSE
  }else{
    addVar <- TRUE	
  }
  varIn <- slcOpt$bestPoint

  while (TRUE) {
    findIn <- if (addVar == TRUE) FALSE else TRUE
    pointer <- if(addVar == TRUE) 1 else -1 #modified by junhuili @ 20190918
    p <- slcOpt$nVarIn[slcOpt$serial]
    addX <- varIn[-c(nInclude)]
    if(NoClass==1){
      if(length(addX) > 0){
        X0 <-	cbind(b,Xf[,addX])
        X1 <- Xf[,-addX]
        X0Name <- colnames(X0)[-nInclude]
        X1Name <- XName[-addX]
      }else{
        X0 <- b
        X1 <- Xf
        X1Name <- XName
      }
    }else{
      if (length(addX) > 0){
        MresdX <- NULL
        for(l in 1:length(addX)){
          temp <- ((addX[l]-1)*NoClass+1):(addX[l]*NoClass)
          MresdX <- append(MresdX,temp)
        }
        X0 <- cbind(b,Xf[,MresdX])
        X1 <- Xf[,-MresdX]
        X0Name <- XName[addX]
        X1Name <- XName[-addX]
      }else{
        X0 <- b
        X1 <- Xf
        X1Name <- XName
      }
    }
    X0 <- as.matrix(X0)
    X1 <- as.matrix(X1)
    stepvalue <- stepOne(findIn,NoClass,nObs,sigmaVal,tolerance,Trace,class(slcOpt),Y,X1,X0,nk,SSTdet)
    
    if(stepvalue$rank0==stepvalue$rank && findIn ==FALSE){
      break
    }else{
      if (class(slcOpt) == 'SL') {
        if (findIn == TRUE) {
          indicator <- stepvalue$PIC > log10(sls)
        } else {
          indicator <- stepvalue$PIC < log10(sle)
        }
      }else if(class(slcOpt) == 'Rsq' | class(slcOpt) == 'adjRsq'){
        indicator <- round(stepvalue$PIC,digits=7) > round(slcOpt$bestValue[slcOpt$serial],digits=7)
      }else{
        indicator <- round(stepvalue$PIC,digits=7) <= round(slcOpt$bestValue[slcOpt$serial],digits=7)
      }
      if (indicator == TRUE) {
        #goodness of fit
        SEQ <- stepvalue$SEQ
        if(SEQ>0){
          SEQclass <- pointer*((SEQ-1)*NoClass+1):(SEQ*NoClass)
          X01 <- cbind(X0,as.matrix(X1[,SEQclass]))
          X02 <- X01[,qr(X01)$pivot[1:qr(X01)$rank]]
          smr <- summary(lm(Y~X02))
          if(length(y)==1){
            f <- smr$fstatistic
            if(is.nan(f[1])){
              pval <- NaN
            }else{
              pval <- pf(f[1],f[2],f[3],lower.tail=F)
            }
          }else{
            for(ny in 1:length(y)){
              f <- smr[[ny]]$fstatistic
              if(is.nan(f[1])){
                pval <- NaN
              }else{
                pval <- pf(f[1],f[2],f[3],lower.tail=F)
              }
            }
          }
        }
        if(is.nan(pval)==TRUE && (class(slcOpt)!='Rsq' && class(slcOpt)!='adjRsq')){
          break
        }
        if(addVar == TRUE){
          Order <- which(XName %in% X1Name[stepvalue$SEQ])
          varIn <- append(varIn,Order)
        }else{
          Order <- which(XName %in% X0Name[stepvalue$SEQ])
          varIn <- varIn[!varIn %in% Order] #modified by junhuili @ 20190918
        }
        slcOpt$serial <- slcOpt$serial + 1
        slcOpt$bestPoint[slcOpt$serial] <- Order
        slcOpt$bestValue[slcOpt$serial] <- stepvalue$PIC
        slcOpt$enOrRe[slcOpt$serial] <- addVar
        slcOpt$nVarIn[slcOpt$serial] <- if (addVar == TRUE) p + 1 else p - 1
        if(!is.null(Choose)){
          chsOpt$bestValue[slcOpt$serial] <- ModelFitStat(class(chsOpt),stepvalue$SSE,SSTdet,nObs,nY,stepvalue$rank,sigmaVal)
        }
        if(selection == 'bidirection'){
          if(addVar == FALSE){
            next
          }else{
            addVar <- FALSE
            next
          }
        }else{
          next
        }
      }else{
        if(selection == 'bidirection' && addVar == FALSE) {
          addVar <- TRUE
          next
        }else{
          break
        }
      }
    }#valid
  }#while
  
  varName <- array(FALSE, slcOpt$serial)
  varName[1:(slcOpt$serial)] <- c('intercept',XName[slcOpt$bestPoint[1:slcOpt$serial]])
  if(!is.null(Choose)){
    process <- data.frame(Step = 0:(slcOpt$serial-1), VarName = varName, EnterModel = slcOpt$enOrRe[1:slcOpt$serial],
                          VarPosition = slcOpt$bestPoint[1:slcOpt$serial], VarNumber = slcOpt$nVarIn, Select = slcOpt$bestValue, Choose = chsOpt$bestValue)
  }else{
    process <- data.frame(Step = 0:(slcOpt$serial-1), VarName = varName, EnterModel = slcOpt$enOrRe[1:slcOpt$serial],
                          VarPosition = slcOpt$bestPoint[1:slcOpt$serial], VarNumber = slcOpt$nVarIn, Select = slcOpt$bestValue)
  }
  if(!is.null(Choose)){
    if(Choose=="Rsq" | Choose=="adjRsq"){
      optN <- which.max(process[(length(includename)+1):nrow(process),"Choose"])
    }else{
      optN <- which.min(process[(length(includename)+1):nrow(process),"Choose"])
    }
    sleres <- process[1:(length(includename)+optN),]
  }else{
    sleres <- process
  }
  resVar <- sleres[,2]
  if(selection=="backward"){
    resVar <- c("intercept",XName[!XName %in% resVar])
  }else if(selection=="bidirection"){
    Xtab <- table(resVar)
    resVar <- resVar[!resVar %in% names(Xtab)[Xtab%%2 == 0]]
  }
  results <- list(process,resVar)
  names(results) <- c("process","variate")
  return(results)
}