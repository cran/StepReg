stepwiselogit <- function(data,y,exclude=NULL,include=NULL,selection="bidirection",select="SL",sle=0.15,sls=0.15){
  if(!selection %in% c("forward","backward","bidirection","score")){
    stop("selection should be one of 'forward','backward','bidirection','score'.")
  }
  if(selection=="score" & select=="SL"){
    stop("select parameter should not be 'SL' when selection parameter is 'score'.")
  }
  if(!select %in% c("AIC","AICc","IC(1)","IC(3/2)","SBC","SL")){
    stop("selection should be one of 'AIC','IC(1)','IC(3/2)','SBC','SL'.")
  }
  nameset <- colnames(data)
  ## y
  if (is.numeric(y)) {
    if (0 %in% y) {
      stop("0 should be remove from 'y'")
    }else{
      Yname <- nameset[y]
    }
  }else if (is.character(y)) {
    if (!all(y %in% nameset)) {
      stop("'y' should be included in data set")
    }else{
      Yname <- y
    }
  }else {
    stop("'y' should be numeric or character vector")
  }
  if(nlevels(as.factor(data[,Yname]))==1){
    stop("All observations have the same response. No statistics are computed.")
  }
  ## exclude
  if(is.numeric(exclude)){
    if(0 %in% exclude){
      stop("0 should be remove from exclude")
    }else{
      excludename <- colnames(data)[exclude]
    }
  }else if(is.character(exclude)){
    if(!all(exclude %in% nameset)){
      stop("'exclude' should be included in data set")
    }else{
      excludename <- exclude
    }
  }else if(is.null(exclude)){
    excludename <- exclude
  }else{
    stop("illegal 'exclude' variable")
  }
  #include
  if(is.null(include)){
    includename <- NULL
  }else if(is.numeric(include)){
    includename <- nameset[include]
  }else if(is.character(include)){
    if(all(include %in% nameset)){
      includename <- include
    }
  }
  if(length(excludename)*length(includename) != 0){
    if(any(excludename %in% includename)){
      stop("elements in exclude should not be listed in include")
    }
  }
  ## x set name
  Xname <- nameset[!nameset %in% c(Yname,excludename,includename)]
  ## convert x dataset to numeric
  Xdataset <- data[,c(Xname,includename)]
  if(!all(sapply(Xdataset, class)=="numeric")){
    n <- which(sapply(Xdataset, class)!="numeric")
    for(i in n){
      Xdataset[,i] <- as.numeric(Xdataset[,i])
    }
  }
  YXdata <- cbind(data[,Yname],Xdataset)
  colnames(YXdata)[1:length(Yname)] <- Yname
  
  result <- list()
  if(selection=="score"){ #score
    bestSubSet <- NULL
    subSet <- matrix(NA,1,3)
    colnames(subSet) <- c("NumberofVariables",select,"VariablesIncludedinModel")
    if(length(includename)!=0){
      fm <- paste(Yname,"~",paste0(includename,collapse = "+"),sep="")
      fit <- glm(fm,YXdata,family="binomial")
      PIC <- fit$aic
      k <- fit$rank
      if(select=="IC(1)"){
        PIC <- PIC-k
      }else if(select=="IC(3/2)"){
        PIC <- PIC-0.5*k
      }else if(select=="SBC"){
        PIC <- PIC+(log(nrow(YXdata))-2)*k
      }else if(select=="AICc"){
        PIC <- PIC+2*k*(k+1)/(nrow(YXdata)-k-1)
      }
      subSet[1,1] <- fit$rank
      subSet[1,2] <- PIC
      subSet[1,3] <- includename
      bestSubSet <- rbind(bestSubSet,subSet)
    }
    for(nv in 1:length(Xname)){
      comTable <- combn(length(Xname),nv)
      #ncom=1
      for(ncom in 1:ncol(comTable)){
        comVar <- c(includename,Xname[comTable[,ncom]])
        fm <- paste(Yname,"~",paste0(comVar,collapse = "+"),sep="")
        fit <- glm(fm,YXdata,family="binomial")
        PIC <- fit$aic
        k <- fit$rank
        if(select=="IC(1)"){
          PIC <- PIC-k
        }else if(select=="IC(3/2)"){
          PIC <- PIC-0.5*k
        }else if(select=="SBC"){
          PIC <- PIC+(log(nrow(YXdata))-2)*k
        }else if(select=="AICc"){
          PIC <- PIC+2*k*(k+1)/(nrow(YXdata)-k-1)
        }
        subSet[1,1] <- fit$rank
        subSet[1,2] <- round(PIC,4)
        subSet[1,3] <- paste0(comVar,collapse = "+")
        bestSubSet <- rbind(bestSubSet,subSet)
      }
    }
    result$RegressionModelsSelectedbyInformationCriterion <- bestSubSet
  }else{ #forward # bidirection # backward
    if(selection=="backward"){
      addVar <- FALSE
      Xmodel <- Xname
      Xresidual <- NULL
    }else{
      addVar <- TRUE
      Xmodel <- NULL
      Xresidual <- Xname
    }
    #Summary_of_Selection
    SoSset <- NULL
    nStep <- 0
    if(select=="SL"){
      nc <- 8
      testName <- c("Score.Chi","Wald.Chi","Pr>ChiSq")
    }else{
      nc <- 6
      testName <- select
    }
    SoS <- matrix(NA,1,nc)
    colnames(SoS) <- c("Step","EnteredEffect","RemovedEffect","DF","NumberIn",testName)
    while(TRUE){
      if(addVar==TRUE){
        FITi <- NULL
        comVar <- c(1,includename,Xmodel)
        fm <- paste(Yname,"~",paste0(comVar,collapse = "+"),sep="")
        fit <- glm(fm,YXdata,family="binomial")
        k <- fit$rank
        if(select=="SL"){
          threshold <- sle
          stlist <- scoretest(fit,as.matrix(YXdata[Xresidual]))
          stv <- stlist$score
          PIC <- stlist$pvalue
        }else{
          threshold <- fit$aic
          PIC <- NULL
          for(i in Xresidual){
            #i="cell"
            fmFwd <- paste(Yname,"~",paste0(c(comVar,i),collapse = "+"),sep="")
            fit <- glm(fmFwd,YXdata,family="binomial")
            PIC <- append(PIC,fit$aic)
          }
          names(PIC) <- Xresidual
          if(select=="IC(1)"){
            PIC <- PIC-fit$rank
            threshold <- threshold-k
          }else if(select=="IC(3/2)"){
            PIC <- PIC-0.5*fit$rank
            threshold <- threshold-0.5*k
          }else if(select=="SBC"){
            PIC <- PIC+(log(nrow(YXdata))-2)*fit$rank
            threshold <- threshold+(log(nrow(YXdata))-2)*k
          }else if(select=="AICc"){
            PIC <- PIC+2*fit$rank*(fit$rank+1)/(nrow(YXdata)-fit$rank-1)
            threshold <- threshold+2*k*(k+1)/(nrow(YXdata)-k-1)
          }
        }
        mPIC <- min(PIC)
        mP <- which.min(PIC)
        minmaxVar <- Xresidual[mP]
        if(mPIC < threshold){
          indicator <- TRUE
          Xmodel <- append(Xmodel,minmaxVar)
          Xresidual <- Xresidual[-mP]
          nStep <- nStep+1
          if(select=="SL"){
            SoS[1,] <- c(nStep,minmaxVar,"",1,length(c(Xmodel,includename)),round(stv[mP],4),"",round(mPIC,4))
          }else{
            SoS[1,] <- c(nStep,minmaxVar,"",1,length(c(Xmodel,includename)),round(mPIC,4))
          }
          SoSset <- rbind(SoSset,SoS)
        }else{
          indicator <- FALSE
        }
      }else{
        comVar <- c(1,includename,Xmodel)
        fm <- paste(Yname,"~",paste0(comVar,collapse = "+"),sep="")
        fit <- glm(fm,YXdata,family="binomial")
        k <- fit$rank
        if(select=="SL"){
          threshold <- sls
          FITi <- coef(summary(fit))
          PIC <- FITi[-(1:(length(includename)+1)),'Pr(>|z|)']
          mPIC <- max(PIC)
          mP <- which.max(PIC)
          if(mPIC > threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }else{
          threshold <- fit$aic
          PIC <- NULL
          if(length(Xmodel)==1){
            plus <- ""
          }else{
            plus <- "+"
          }
          for(i in Xmodel){
            comVarBwd <- Xmodel[!Xmodel %in% i]
            fmBwd <- paste(Yname,"~1",plus,paste0(comVarBwd,collapse = "+"),sep="")
            fit <- glm(fmBwd,YXdata,family="binomial")
            PIC <- append(PIC,fit$aic)
          }
          names(PIC) <- Xmodel
          if(select=="IC(1)"){
            PIC <- PIC-fit$rank
            threshold <- threshold-k
          }else if(select=="IC(3/2)"){
            PIC <- PIC-0.5*fit$rank
            threshold <- threshold-0.5*k
          }else if(select=="SBC"){
            PIC <- PIC+(log(nrow(YXdata))-2)*fit$rank
            threshold <- threshold+(log(nrow(YXdata))-2)*k
          }
          mPIC <- min(PIC)
          mP <- which.min(PIC)
          if(mPIC < threshold){
            indicator <- TRUE
          }else{
            indicator <- FALSE
          }
        }
        minmaxVar <- Xmodel[mP]
        if(indicator==TRUE){
          Xresidual <- append(Xresidual,Xmodel[mP])
          Xmodel <- Xmodel[-mP]
          nStep <- nStep+1
          if(select=="SL"){
            SoS[1,] <- c(nStep,"",minmaxVar,1,length(c(Xmodel,includename)),"",round(FITi[mP+1,3:4],4)) 
          }else{
            SoS[1,] <- c(nStep,"",minmaxVar,1,length(c(Xmodel,includename)),round(mPIC,4))
          }
          SoSset <- rbind(SoSset,SoS)
        }
      }
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
    }
    SoSset <- as.data.frame(SoSset)
    #Analysis of Maximum Likelihood Estimate
    fmodel <- paste(Yname,"~",paste0(c(includename,Xmodel),collapse = "+"),sep="")
    fitmodel <- glm(fmodel,YXdata,family="binomial")
    MLE <- round(coef(summary(fitmodel)),4)
    MLE <- cbind(rownames(MLE),MLE)
    colnames(MLE)[1] <- c("Parameter")
    MLE <- as.data.frame(MLE)
    result <- list()
    result$SummaryOfSelection <- SoSset
    result$AnalysisOfMaximumLikelihoodEstimate <- MLE
  }
  return(result)
}