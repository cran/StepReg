#' remission
#'
#' A dataset containing the remission and 6 risk factors thought to be related to leukemia remission.
#'
#' \itemize{
#'   \item remiss Indicates whether cancer remission occurred. A value of 1 indicates occurrence, while 0 indicates non-occurrence.
#'   \item cell Cellularity of the marrow clot section
#'   \item smear Smear differential percentage of blasts
#'   \item infil Percentage of absolute marrow leukemia cell infiltrate
#'   \item li Percentage labeling index of the bone marrow leukemia cells
#'   \item blast The absolute number of blasts in the peripheral blood
#'   \item temp The highest temperature before the start of treatment 
#' }
#'
#' @name remission
#' 
#' @keywords datasets
#' 
#' @docType data
#' 
#' @usage data(remission)
#' 
#' @references 
#' Lee, E. T. (1974). “A Computer Program for Linear Logistic Regression Analysis.” Computer Programs in Biomedicine 4:80–92.
#' 
#' https://online.stat.psu.edu/stat501/book/export/html/1011
#' 
#' @format A data frame with 27 rows and 7 columns.
#' 
"remission"