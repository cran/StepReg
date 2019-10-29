bestsubset <- function(data,y,exclude=NULL,include=NULL,Class=NULL,weights=c(rep(1,nrow(data))),select="SBC",tolerance=1e-7,best=5){
	##select
	if(!is.character(select) | !select %in% c("AIC","AICc","BIC","CP","HQ","HQc","Rsq","adjRsq","SBC")){
		stop("'select' must be one of 'AIC','AICc','BIC','CP','HQ','HQc','Rsq','adjRsq','SBC'")
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
	X <- as.matrix(data[,!nameset %in% c(Yname,excludename)])
	
	if(nrow(Y) != nrow(X)){
		stop("The rows of y does not equals independent variable")
	}else{
		nObs <- nrow(X)		
	}
	
	Xf <- X
	nX <- ncol(Xf)
	XName <- colnames(Xf)
	Xfqr <- qr(Xf, tol = tolerance)
	if(Xfqr$rank < nX){
		cat("data removed duplicated variable due to multi-colinearility")
	}
	XNameMC <- XName[Xfqr$pivot[1:Xfqr$rank]]
	
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
	if(length(excludename)*length(includename) != 0){
		if(any(excludename %in% includename)){
			stop("elements in exclude should not be listed in include")
		}
	}
	## Total sum of squares corrected for the mean for the dependent variable
	Yd <- Y-mean(Y)
	SST <- t(Yd*sqrt(weights)) %*% (Yd*sqrt(weights))
	SSTdet <- abs(det(SST))
	
	b <- matrix(1,nObs,1)*sqrt(weights)
	colnames(b) <- "Intercept"
	Y <- Y*sqrt(weights)
	Xf <- Xf*sqrt(weights)
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
	wgtData <- cbind(b,Xf)
	
	# get sigma for BIC and CP
	if(ncol(Xf)>nObs){
		sigmaVal <- 0
	}else{
		Ys <- Y/sqrt(weights)
		Xfs <- Xf/sqrt(weights)
		lmf <- lm(Ys~Xfs,weights = weights)
		sigmaVal <- sum(deviance(lmf)/df.residual(lmf))/nY
	}
	if(select=="CP" & sigmaVal == 0){
		stop("The CP criterion cannot be used as sigma=0 for the full model")
	}
	
	## best subset model selection
	tempresult <- matrix(NA,1,3)
	colnames(tempresult) <- c("NumberofVariables",select,"VariablesIncludedinModel")
	bestResult <- tempresult
	XNameMCin <- XNameMC[!XNameMC %in% includename]
	if(length(includename)>0){
		whichin <- which(XNameMC %in% includename)
		if(NoClass>1){
			whichin <- sort(c(whichin*2,whichin*2+1))
		}
		lmresult <- lm(Y~wgtData[,c(1,whichin)]-1)
		lmresd <- as.matrix(lmresult$residuals)
		SSEmatrix <- t(lmresd)%*%lmresd
		SSEdet <- abs(det(SSEmatrix))
		tempresult[1,1] <- length(includename)
		tempresult[1,2] <- round(ModelFitStat(select,SSEdet,SSTdet,nObs,nY,lmresult$rank,sigmaVal),5)
		tempresult[1,3] <- paste(includename,collapse=" ")
		bestResult <- rbind(bestResult,tempresult)
	}
	for(nv in 1:length(XNameMCin)){
		comTable <- combn(length(XNameMCin),nv)
		for(ncom in 1:ncol(comTable)){
			comVar <- c(includename,XNameMCin[comTable[,ncom]])
			whichin <- which(XNameMC %in% comVar)
			if(NoClass>1){
				whichin <- sort(c(whichin*2,whichin*2+1))
			}
			lmresult <- lm(Y~wgtData[,c(1,whichin)]-1)
			lmresd <- as.matrix(lmresult$residuals)
			SSEmatrix <- t(lmresd)%*%lmresd
			SSEdet <- abs(det(SSEmatrix))
			tempresult[1,1] <- length(comVar)
			tempresult[1,2] <- round(ModelFitStat(select,SSEdet,SSTdet,nObs,nY,lmresult$rank,sigmaVal),5)
			tempresult[1,3] <- paste(comVar,collapse=" ")
			bestResult <- rbind(bestResult,tempresult)
		}
	}
	tempBest <- as.data.frame(bestResult[-1,])
	finalResult <- NULL
	for(nsub in sort(as.numeric(as.character(levels(tempBest[,1]))))){
		subResult <- subset(tempBest,tempBest[,1]==nsub)
		subResult[,select] <- as.numeric(as.character(subResult[,select]))
		if(select=='Rsq'|select=='adjRsq'){
			subResultSort <- subResult[order(subResult[,2],decreasing = TRUE),]
		}else{
			subResultSort <- subResult[order(subResult[,2]),]
		}
		if(nrow(subResultSort)<best){
			nbest <- nrow(subResultSort)
		}else{
			nbest <- best
		}
		finalResult <- rbind(finalResult,subResultSort[1:nbest,])
	}
	colnames(finalResult) <- colnames(tempresult)
	rownames(finalResult) <- c(1:nrow(finalResult))
	return(finalResult)
}