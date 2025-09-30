#' Tobacco Leaf Chemical Composition Dataset
#'
#' A dataset containing chemical composition measurements from 25 tobacco leaf samples.
#' This dataset is commonly used for demonstrating multivariate regression analysis
#' and exploring relationships between chemical components and burn rate.
#'
#' @format A data frame with 25 rows and 9 variables:
#' \describe{
#'   \item{cigarette}{Numeric. Rate of cigarette burn in inches per 1000 seconds.}
#'   \item{sugar}{Numeric. Percentage of sugar content in the leaf.}
#'   \item{nicotine}{Numeric. Percentage of nicotine content.}
#'   \item{nitrogen}{Numeric. Percentage of nitrogen content.}
#'   \item{chlorine}{Numeric. Percentage of chlorine content.}
#'   \item{potassium}{Numeric. Percentage of potassium content.}
#'   \item{phosphorus}{Numeric. Percentage of phosphorus content.}
#'   \item{calcium}{Numeric. Percentage of calcium content.}
#'   \item{magnesium}{Numeric. Percentage of magnesium content.}
#' }
#'
#' @details This dataset is particularly useful for:
#' \itemize{
#'   \item Demonstrating multivariate regression analysis
#'   \item Studying relationships between chemical composition and burn rate
#'   \item Analyzing correlations between different chemical components
#'   \item Teaching statistical methods in agricultural research
#' }
#'
#' @source Anderson, R. L. and Bancroft, T. A. (1952), Statistical Theory in Research,
#' McGraw-Hill Book Company, Inc., New York, NY.
#'
#' @examples
#' # Load the dataset
#' data(tobacco)
#' 
#' # View the first few rows
#' head(tobacco)
#' 
#' # Summary statistics
#' summary(tobacco)
#' 
#' # Correlation analysis
#' cor(tobacco)
#'
#' @keywords datasets
#' @docType data
#' @usage data(tobacco)
"tobacco"