#' Prints from a StepReg object
#'
#' print.StepReg prints to console the from an object of class StepReg
#'
#' @param x each dataframe from outputlist
#' 
#' @param ... further paramters
#'
#' @return formatted dataframe
#' 
#' @importFrom purrr pmap_dfc pmap_chr
#' 
#' @importFrom stringr str_pad
#' 
#' @importFrom dplyr `%>%` mutate_if
#' 
#' @export
#' 
print.StepReg <- function(x, ...){
  for(i in 1:length(x)){
    y <- x[[i]]
    yName <- names(x)[i]
    if(is.data.frame(y)){
      outputStepReg(y,text=paste0("Table ",i,". ",yName))
    }else if(is.list(y)){
      for(j in 1:length(y)){
        ySubName <- paste(yName,"for",names(y)[j],sep=" ")
        suby <- y[[j]]
        outputStepReg(suby,text=paste0("Table ",i+j-1,". ",ySubName))
      }
    }
  }
}

outputStepReg <- function(x,text){
  x %>% mutate_if(is.factor, as.character) -> x
  if(nrow(x)==1){
    dfLen <- sapply(x,nchar)
  }else{
    dfLen <- apply(sapply(x,nchar),2,max)
  }
  nameLen <- nchar(colnames(x),keepNA =FALSE)
  lengths <- pmax(nameLen,dfLen)+2
  
  side <- rep("both",ncol(x))
  list(colnames(x),lengths,side) %>% pmap_chr(str_pad) -> dfHeader
  side <- rep("right",ncol(x))
  list(x,lengths,side) %>% pmap_dfc(str_pad) -> dataFrame
  cat(format(text, width = sum(lengths), justify = "centre"));cat("\n")
  cat(paste0(rep("\u2017",sum(lengths)),collapse=""));cat("\n")
  cat(paste0(dfHeader,collapse=""));cat("\n")
  cat(paste0(rep("\u2014",sum(lengths)),collapse=""));cat("\n")
  
  for(i in 1:nrow(dataFrame)){
    cat(paste0(dataFrame[i,],collapse=""),"\n")
  }
  cat(paste0(rep("\u2017",sum(lengths)),collapse=""));cat("\n");cat("\n")
}