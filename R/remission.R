#' Leukemia Remission Dataset
#'
#' A dataset containing information about leukemia remission and associated risk factors.
#' This dataset is commonly used for demonstrating logistic regression analysis in medical research.
#'
#' @format A data frame with 27 observations and 7 variables:
#' \describe{
#'   \item{remiss}{Binary outcome variable indicating leukemia remission status:
#'     \itemize{
#'       \item 1 = Remission occurred
#'       \item 0 = No remission
#'     }
#'   }
#'   \item{cell}{Numeric. Cellularity of the marrow clot section (percentage)}
#'   \item{smear}{Numeric. Smear differential percentage of blasts}
#'   \item{infil}{Numeric. Percentage of absolute marrow leukemia cell infiltrate}
#'   \item{li}{Numeric. Percentage labeling index of the bone marrow leukemia cells}
#'   \item{blast}{Numeric. Absolute number of blasts in the peripheral blood}
#'   \item{temp}{Numeric. Highest temperature (in Fahrenheit) before treatment}
#' }
#'
#' @details This dataset is particularly useful for:
#' \itemize{
#'   \item Demonstrating logistic regression analysis
#'   \item Studying risk factors for leukemia remission
#'   \item Teaching medical statistics and predictive modeling
#' }
#'
#' @source Lee, E. T. (1974). "A Computer Program for Linear Logistic Regression Analysis."
#' Computer Programs in Biomedicine 4:80–92.
#'
#' @references
#' \itemize{
#'   \item Lee, E. T. (1974). "A Computer Program for Linear Logistic Regression Analysis."
#'   Computer Programs in Biomedicine 4:80–92.
#'   \item Penn State University Statistics Online. "Logistic Regression Example."
#'   \url{https://online.stat.psu.edu/stat501/book/export/html/1011}
#' }
#'
#' @examples
#' \dontrun{
#' # Load the dataset
#' data(remission)
#' 
#' # View first few rows
#' head(remission)
#' 
#' # Summary statistics
#' summary(remission)
#' 
#' # Run logistic regression
#' model <- glm(remiss ~ ., data = remission, family = binomial)
#' summary(model)
#' }
#'
#' @usage data(remission)
#' @name remission
#' @docType data
#' @keywords datasets
"remission"