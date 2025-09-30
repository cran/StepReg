#' Print Stepwise Regression Results
#'
#' Displays the final model fit statistics from a StepReg object. This function provides
#' a concise summary of the selected model's performance metrics.
#'
#' @param x A StepReg object containing the results of stepwise regression analysis.
#' 
#' @param ... Additional arguments passed to the print method (currently not used).
#'
#' @return Invisibly returns the printed object. The function displays:
#'   \itemize{
#'     \item Final model fit statistics for each strategy and metric combination
#'   }
#' 
#' @details The print method provides a focused view of the final model's performance,
#' showing the selected variables and their corresponding fit statistics. This is useful
#' for quickly assessing the model's quality without the detailed step-by-step selection
#' process (which can be viewed using \code{\link{stepwise}}).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(mtcars)
#' 
#' # Run stepwise regression
#' formula <- mpg ~ .
#' result <- stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = "forward",
#'   metric = "AIC"
#' )
#' 
#' # Print final model statistics
#' result
#' }
#'
#' @seealso \code{\link{stepwise}} for creating StepReg objects
#' @seealso \code{\link{plot.StepReg}} for visualization of results
print.StepReg <- function(x, ...){
  nohid <- attr(x, "nonhidden")
  print(x[nohid])
}
