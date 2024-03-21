#' StepReg Shiny App
#' 
#' StepRegShinyApp is a Shiny application designed for performing stepwise 
#' regression analysis. In Step 1, users can upload their dataset, configure 
#' settings such as header, separator, and quotes, and select variables for 
#' distribution plots. In Step 2, users can choose the regression type (linear, 
#' logit, cox, poisson, or gamma), select dependent and independent variables, 
#' specify stepwise strategy (forward, backward, bidirectional, or subset), and 
#' set various metrics for model selection. The app dynamically adjusts input 
#' options based on the chosen regression type. Additionally, users can specify 
#' significant levels for entry and stay in the stepwise process. Finally, they 
#' can run the analysis to obtain stepwise regression results and visualize them
#' through summary outputs and plots.
#' 
## update import here and require in utils simutaniously
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom shinyjs disable enable
#' @importFrom DT dataTableOutput datatable renderDataTable
#' @importFrom cowplot plot_grid
#' @importFrom summarytools dfSummary
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom tidyr gather
#' @importFrom GGally ggpairs
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
  runApp(appDir = system.file('shiny', package='StepReg'))
}
