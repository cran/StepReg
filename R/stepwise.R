stepwise <- function(data,y,notX=NULL,include=NULL,Class=NULL,weights=c(rep(1,nrow(data))),selection="stepwise",select="SBC",sle=0.15,sls=0.15,tolerance=1e-7,Trace="Pillai",Choose="SBC"){
  ## weights
  if (!is.null(weights) && !is.numeric(weights)){
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
  ## notX
  if(is.numeric(notX)){
    if(0 %in% notX){
      stop("0 should be remove from notX")
    }else{
      notXname <- colnames(data)[notX]
    }
  }else if(is.character(notX)){
    if(!all(notX %in% nameset)){
      stop("'notX' should be included in data set")
    }else{
      notXname <- notX
    }
  }else if(is.null(notX)){
    notXname <- notX
  }else{
    stop("illegal 'notX' variable")
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
  ## weighted data
  Y <- Y*sqrt(weights)
  Xf <- Xf*sqrt(weights)
  ## continous to continous-nesting-class
  if(NoClass > 1){
    Xf1 <- NULL
    NoSub <- rep(0,NoClass+1)
    for (k in 1:NoClass){
      tempNo <- sum(NestClass %in% VecClass[k])
      NoSub[1+k] <- tempNo+NoSub[k]
      X_part <- matrix(0,tempNo,NoClass*nX)
      X_part[,seq(k,NoClass*nX,by=NoClass)] <- Xf[(NoSub[k]+1):NoSub[k+1],]
      Xf1 <- rbind(Xf1,X_part)
    }
    Xf <- Xf1
  }
  # get sigma from BIC
  if (select=="BIC"|Choose=="BIC"){
    if(ncol(Xf)>nObs){ 
      sigma <- 0
    }else{
      lmf <- lm(Y~Xf)
      sigma <- sum(deviance(lmf)/df.residual(lmf))/nY
    }
  }else{
    sigma <- 0
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
  nk <- length(includename)
  ##
  if(length(notXname)*length(includename) != 0){
    if(any(notXname %in% includename)){
      stop("elements in notX should not be included in include")
    }
  }
  ##
  b <- matrix(1,nObs,1)*sqrt(weights)
  if(length(includename) != 0){
    includek <- which(XName %in% includename)
    includeAll <- 0
    for(k in includek){
      includeAll <- append(includeAll,(NoClass*(k-1)+1):(k*NoClass))
    }
    b <- cbind(b,Xf[,includeAll])
    Xf <- Xf[,-includeAll]
  }
  
  tmpX <-b 
  nX <- nX - length(includename)
  slcOpt <- list(serial = 'numeric', bestValue = 'numeric', bestPoint = 'numeric', enOrRe = 'logical', nVarIn = 'numeric')
  class(slcOpt) <- select
  chsOpt <- list(serial = 'numeric', bestValue = 'numeric', bestPoint = 'numeric', enOrRe = 'logical', nVarIn = 'numeric')
  class(chsOpt) <- Choose
  I <- diag(nObs)
  #k=1
  for(k in 0:length(includename)){
    tmpX1 <- as.matrix(tmpX[,c(1:(k*NoClass+1))])
    qrX <- qr(tmpX1)
    p <- qrX$rank
    tmpX1 <- tmpX1[,qrX$pivot[1:p]]
    #lmresult <- lm(Y~tmpX1-1)
    #res <- residuals(lmresult)
    #SSEp <- res %*% res
    tempSSEp <- tmpX1 %*% solve(t(tmpX1) %*% tmpX1) %*% t(tmpX1)
    SSEp <- t(Y)%*%(I-tempSSEp)%*%Y
    SSEpSq <- t(SSEp)%*%SSEp
    SSESqdet <- abs(det(SSEpSq))
    SSEdet <- abs(det(SSEp))
    
    if(class(chsOpt)=="AIC"){
      chsOptbestValue <- nObs*log(SSEdet/nObs)+(2*p*nY*nObs+nY*(nY+1))/nObs-2/nObs+nObs+2
    }
    if(class(chsOpt)=="AICc"){
      chsOptbestValue <- nObs*log(SSEdet/nObs)+nObs*(nObs+p)*nY/(nObs-p-nY-1)
    }
    if(class(chsOpt)=="BIC"){
      chsOptbestValue <- nObs*log(SSEdet/nObs)+2*(2+p)*(nObs*sigma/SSEdet)-2*(nObs*sigma/SSEdet)^2
    }
    if(class(chsOpt)=="HQ"){
      chsOptbestValue <- log(SSEdet)+2*log(log(nObs))*p*nY/nObs
    }
    if(class(chsOpt)=="HQc"){
      chsOptbestValue <- log(SSESqdet)+2*log(log(nObs))*p*nY/(nObs-p-nY-1)
    }
    if(class(chsOpt)=="SBC"){
      chsOptbestValue <- nObs*log(SSEdet/nObs)+log(nObs)*p
    }
    # Calculate bestValue of select
    if (class(slcOpt) == 'SL') {
      slcOptbestValue <- 1
    }else{
      if(class(slcOpt)=="AIC"){
        slcOptbestValue <- nObs*log(SSEdet/nObs)+(2*p*nY*nObs+nY*(nY+1))/nObs-2/nObs+nObs+2
      }
      if(class(slcOpt)=="AICc"){
        slcOptbestValue <- nObs*log(SSEdet/nObs)+nObs*(nObs+p)*nY/(nObs-p-nY-1)
      }
      if(class(slcOpt)=="BIC"){
        slcOptbestValue <- nObs*log(SSEdet/nObs)+2*(2+p)*(nObs*sigma/SSEdet)-2*(nObs*sigma/SSEdet)^2
      }
      if(class(slcOpt)=="HQ"){
        slcOptbestValue <- log(SSEdet)+2*log(log(nObs))*p*nY/nObs
      }
      if(class(slcOpt)=="HQc"){
        slcOptbestValue <- log(SSESqdet)+2*log(log(nObs))*p*nY/(nObs-p-nY-1)
      }
      if(class(slcOpt)=="SBC"){
        slcOptbestValue <- nObs*log(SSEdet/nObs)+log(nObs)*p
      }
    }
    # Calculate select
    if(k!=0){
      slcOpt$serial <- slcOpt$serial + 1
      slcOpt$bestPoint[slcOpt$serial] <- which(XName %in% includename[k])
      slcOpt$bestValue[slcOpt$serial] <- slcOptbestValue
      slcOpt$enOrRe[slcOpt$serial] <- TRUE
      slcOpt$nVarIn[slcOpt$serial] <- p
      chsOpt$bestValue[slcOpt$serial] <- chsOptbestValue
    }else{
      slcOpt$serial <- 1
      slcOpt$bestPoint <- 0
      slcOpt$enOrRe <- TRUE
      slcOpt$nVarIn <- p
      slcOpt$bestValue <- slcOptbestValue
      chsOpt$bestValue <- chsOptbestValue
    }
  }
  
  addVar <- TRUE
  varIn <- rep(0,nX)
  # 4th. while loop for adding and deleting Independent variate-------------
  while (TRUE) {
    findIn <- if (addVar == TRUE) FALSE else TRUE
    pointer <- if(addVar == TRUE) 1 else -1
    p <- slcOpt$nVarIn[slcOpt$serial]
    addX <- which(varIn %in% 1)
    if (NoClass==1){
      if (length(addX) > 0){
        X0 <-  cbind(b,Xf[,addX])
        X1 <- Xf[,-addX]
        X1Name <- XName[-addX]
      }else{
        X0 <- b
        X1 <- Xf
        X1Name <- XName
      }
    }else {
      if (length(addX) > 0){
        MresdX <- NULL
        for(l in 1:length(addX)){
          temp <- ((addX[l]-1)*NoClass+1):(addX[l]*NoClass)
          MresdX <- append(MresdX,temp)
        }
        X0 <- cbind(b,Xf[,MresdX])
        X1 <- Xf[,-MresdX]
        X1Name <- XName[-addX]
      }else{
        X0 <- b
        X1 <- Xf
        X1Name <- XName
      }
    }
    X0 <- as.matrix(X0)
    X1 <- as.matrix(X1)
    stepvalue <- bestCandidate_RCpp(findIn,NoClass,nObs,sigma,tolerance,Trace,class(slcOpt),Y,X1,X0,nk)
    
    if(stepvalue$rank0==stepvalue$rank && findIn ==FALSE){
      break
    }else{
      if (class(slcOpt) == 'SL') {
        if (findIn == TRUE) {
          indicator <- stepvalue$PIc > log10(sls)
        } else {
          indicator <- stepvalue$PIc < log10(sle)
        }
      } else{
        indicator <- round(stepvalue$PIc,digits=7) <= round(slcOpt$bestValue[slcOpt$serial],digits=7)
      }
      if (indicator == TRUE) {
        if(addVar == TRUE){
          Order <- which(XName %in% X1Name[stepvalue$seq])
        }else{
          XfX0 <- which(varIn %in% 1)
          Order <- XfX0[stepvalue$seq]
        }
        slcOpt$serial <- slcOpt$serial + 1
        slcOpt$bestPoint[slcOpt$serial] <- Order + nk
        slcOpt$bestValue[slcOpt$serial] <- stepvalue$PIc
        slcOpt$enOrRe[slcOpt$serial] <- addVar
        slcOpt$nVarIn[slcOpt$serial] <- if (addVar == TRUE) p + 1 else p - 1
        if(class(chsOpt)=="SBC"){
          chsOpt$bestValue[slcOpt$serial] = nObs*log(stepvalue$SSE/nObs)+log(nObs)*stepvalue$rank
        }
        if(class(chsOpt)=="AIC"){
          chsOpt$bestValue[slcOpt$serial] = nObs*log(stepvalue$SSE/nObs)+(2*stepvalue$rank*nY*nObs+nY*(nY+1))/nObs-2/nObs+nObs+2
        }
        if(class(chsOpt)=="AICc"){
          chsOpt$bestValue[slcOpt$serial] = nObs*log(stepvalue$SSE/nObs)+nObs*(nObs+stepvalue$rank)*nY/(nObs-stepvalue$rank-nY-1)
        }
        if(class(chsOpt)=="HQ"){
          chsOpt$bestValue[slcOpt$serial] = nObs*log(stepvalue$SSE/nObs)+2*log(log(nObs))*stepvalue$rank*nY/nObs
        }
        if(class(chsOpt)=="HQc"){
          chsOpt$bestValue[slcOpt$serial] = nObs*log(stepvalue$SSE*stepvalue$SSE/nObs)+2*log(log(nObs))*stepvalue$rank*nY/(nObs-stepvalue$rank-nY-1)
        }
        if(class(chsOpt)=="BIC"){
          chsOpt$bestValue[slcOpt$serial] = nObs*log(stepvalue$SSE/nObs)+2*(2+stepvalue$rank)*(nObs*sigma/stepvalue$SSE)-2*(nObs*sigma/stepvalue$SSE)*(nObs*sigma/stepvalue$SSE)
        }
        varIn[Order] <- varIn[Order]+pointer 
        
        if(selection == 'forward') {
          next
        }else if (selection == 'stepwise') {
          if (addVar == FALSE) {
            next
          } else if (addVar == TRUE) {
            addVar <- FALSE
            next
          }
        }
      }else {
        if(selection == 'stepwise' && addVar == FALSE) {
          addVar <- TRUE
          next
        }else {
          break
        }
      }
    }#valid
  }#while
  
  varName <- array(FALSE, slcOpt$serial)
  varName[1:slcOpt$serial] <- c('intercept',XName[slcOpt$bestPoint])
  process <- data.frame(Step = 0:(slcOpt$serial-1), VarName = varName, EnterModel = slcOpt$enOrRe,
                        VarPosition = slcOpt$bestPoint, VarNumber = slcOpt$nVarIn, Select = slcOpt$bestValue, Choose = chsOpt$bestValue)
  chores <- process[1:(length(includename)+which.min(process[(length(includename)+1):nrow(process),"Choose"])),]
  remVar <- chores[chores[,3]=="FALSE",2]
  resVar <- chores[chores[,3]=="TRUE",2]
  model <- resVar
  if(length(remVar) > 0){
    for(i in remVar){
      resVar[which(resVar %in% i)[1]] <- NA
    }
    model <- resVar[!is.na(resVar)]
  }
  results <- list(process,model)
  names(results) <- c("process","variate")
  return(results)
}