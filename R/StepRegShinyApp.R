#' Launch StepReg Shiny Application
#'
#' Launches an interactive Shiny application for performing stepwise regression analysis.
#' The application provides a user-friendly interface for data analysis, model selection,
#' and visualization of results.
#'
#' @details The application consists of two main steps:
#' 
#' \strong{Step 1: Data Preparation}
#' \itemize{
#'   \item Upload custom datasets or select from built-in examples
#'   \item Configure data import settings (headers, separators, quotes)
#'   \item View and modify variable types (numeric, factor, integer, character)
#'   \item Generate exploratory data visualizations
#' }
#' 
#' \strong{Step 2: Model Analysis}
#' \itemize{
#'   \item Select regression type:
#'     \itemize{
#'       \item Linear regression
#'       \item Logistic regression
#'       \item Cox proportional hazards
#'       \item Poisson regression
#'       \item Gamma regression
#'       \item Negative binomial regression
#'     }
#'   \item Choose dependent and independent variables
#'   \item Specify stepwise selection strategy:
#'     \itemize{
#'       \item Forward selection
#'       \item Backward elimination
#'       \item Bidirectional elimination
#'       \item Best subset selection
#'     }
#'   \item Set model selection criteria (AIC, BIC, etc.)
#'   \item Configure significance levels for variable entry/removal
#'   \item Generate comprehensive reports and visualizations
#' }
#'
#' @return Launches the Shiny application in the user's default web browser.
#'
#' @examples
#' \dontrun{
#' # Launch the StepReg Shiny application
#' StepRegShinyApp()
#' }
#'
#' @seealso \code{\link{stepwise}} for the core stepwise regression function
#' @seealso \code{\link{report}} for generating analysis reports
#' @seealso \code{\link{plot.StepReg}} for visualization functions
#'
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom shinyjs disable enable
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom summarytools dfSummary
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom cowplot plot_grid
#' @importFrom tidyr gather
#' @importFrom rmarkdown render
#' @importFrom shinythemes shinytheme
#' @importFrom shinycssloaders withSpinner
#' @importFrom grDevices pdf
#' @importFrom stats as.formula cor na.omit
#' @importFrom utils data read.table
#' @importFrom dplyr select all_of
#' @export
#'
StepRegShinyApp <- function() {
  runApp(appDir = system.file('shiny', package = 'StepReg'))
}
