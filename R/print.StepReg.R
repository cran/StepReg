#' Prints from a StepReg object
#'
#' print.StepReg prints to console the from an object of class StepReg
#'
#' @param x StepReg object
#' 
#' @param ... further parameters
#'
#' @return only print some dataframe
#' 
#' @export print.StepReg
#' 
#' @export
#' 
print.StepReg <- function(x, ...){
  nohid <- attr(x, "nonhidden")
  print(x[nohid])
}
