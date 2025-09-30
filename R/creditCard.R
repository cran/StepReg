#' Credit Card Application Dataset
#'
#' A dataset containing credit history information for credit card applicants. This dataset is sourced from the AER package.
#'
#' @format A data frame with 1,319 observations and 12 variables:
#' \describe{
#'   \item{card}{Factor. Whether the credit card application was accepted (Yes/No)}
#'   \item{reports}{Numeric. Number of major derogatory reports on the applicant's credit history}
#'   \item{age}{Numeric. Age in years plus twelfths of a year (e.g., 30.5 represents 30 years and 6 months)}
#'   \item{income}{Numeric. Annual income in USD (divided by 10,000)}
#'   \item{share}{Numeric. Ratio of monthly credit card expenditure to yearly income}
#'   \item{expenditure}{Numeric. Average monthly credit card expenditure in USD}
#'   \item{owner}{Factor. Home ownership status (Yes/No)}
#'   \item{selfemp}{Factor. Self-employment status (Yes/No)}
#'   \item{dependents}{Numeric. Number of dependents}
#'   \item{months}{Numeric. Number of months living at current address}
#'   \item{majorcards}{Numeric. Number of major credit cards held}
#'   \item{active}{Numeric. Number of active credit accounts}
#' }
#'
#' @details This dataset is commonly used for credit risk analysis and modeling credit card approval decisions. 
#' It provides a comprehensive view of various factors that may influence credit card application outcomes.
#'
#' @source Greene, W.H. (2003). Econometric Analysis, 5th edition. Upper Saddle River, NJ: Prentice Hall.
#'
#' @seealso \code{\link[AER]{CreditCard}} for the original dataset in the AER package
#'
#' @examples
#' data(creditCard)
#' summary(creditCard)
#' @usage data(creditCard)
#' @name creditCard
#' @docType data
#' @keywords datasets
"creditCard"
