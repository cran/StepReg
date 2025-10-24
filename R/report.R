#' Generate Stepwise Regression Report
#'
#' Creates formatted reports from StepReg objects in various document formats. This function
#' generates comprehensive reports containing all tables and results from the stepwise regression
#' analysis.
#'
#' @param x A StepReg object containing the results of stepwise regression analysis.
#' 
#' @param report_name Character. The name of the output report file(s) without extension.
#' 
#' @param format Character vector. The output format(s) for the report. Choose from:
#'   \itemize{
#'     \item "html" - Web page format (default)
#'     \item "docx" - Microsoft Word document
#'     \item "pptx" - Microsoft PowerPoint presentation
#'     \item "rtf" - Rich Text Format
#'   }
#'   Multiple formats can be specified simultaneously.
#'
#' @details The generated report includes:
#'   \itemize{
#'     \item Summary of model parameters and selection criteria
#'     \item Variable types and classifications
#'     \item Step-by-step selection process
#'     \item Final selected model and fit statistics
#'     \item Model coefficients and significance levels
#'   }
#'
#' @return Creates report file(s) in the specified format(s) in the current working directory.
#' The file name will be \code{report_name.format} (e.g., "myreport.html", "myreport.docx").
#'
#' @importFrom flextable save_as_html save_as_pptx save_as_rtf save_as_docx autofit flextable align
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Load leukemia remission data
#' data(remission)
#' 
#' # Run stepwise logistic regression
#' formula <- remiss ~ .
#' result <- stepwise(
#'   formula = formula,
#'   data = remission,
#'   type = "logit",
#'   strategy = c("forward", "bidirection"),
#'   metric = c("AIC", "BIC")
#' )
#' 
#' # Generate reports in multiple formats
#' report(
#'   x = result,
#'   report_name = "leukemia_analysis",
#'   format = c("html", "docx")
#' )
#' }
#'
#' @seealso \code{\link{stepwise}} for creating StepReg objects
#' @seealso \code{\link{plot.StepReg}} for visualization of results

report <- function(x, report_name, format = c('html', 'docx', 'rtf', 'pptx')) {
  format <- match.arg(format, several.ok = TRUE)
  results <- list()
  for (j in c("argument","variable")) {
    if(j == "argument") {
      j_name <- "Parameters and Values"
    } else if(j == "variable"){
      j_name <- "variable and Class"
    }
    results[j_name] <- list(process_table(x[[j]]))
  }
  
  overview_list <- x$overview
  for(i in names(overview_list)){
    overview_sub_list <- overview_list[[i]]
    for(j in names(overview_sub_list)){
      results[paste("Selection Overview: ",paste(i,j,sep="-"))] <- list(process_table(overview_sub_list[[j]]))
    }
  }
  results["Model Performance"] <- list(process_table(performance(x)))
  names(results) <- paste("Table", paste(1:length(results),names(results)),sep="")
  
  if(!is.null(report_name)) {
    if (any(c('html', 'docx', 'rtf', 'pptx') %in% format)) {
      for (i in format) {
        if (i %in% 'html') {
          save_as_html(values = results,
                       path = paste0(report_name, ".html"))
        } 
        if (i %in% 'docx') {
          save_as_docx(values = results,
                       path = paste0(report_name, ".docx"))
        } 
        if (i %in% 'rtf') {
          save_as_rtf(values=results,
                      path = paste0(report_name, ".rtf"))
        } 
        if (i %in% 'pptx') {
          save_as_pptx(values = results,
                       path = paste0(report_name, ".pptx"))
        }
        # if (i %in% 'xlsx') {
        #   wb <- createWorkbook()
        #   cs <- CellStyle(wb) + Font(wb, isBold = TRUE)
        #   #k=2
        #   for (k in 1:length(x)) {
        #     x[[k]] <- as.data.frame(x[[k]]) # tbl won't work properly when addDataFrame
        #     x[[k]] <- x[[k]] %>% mutate_if(is.numeric, as.character) # Inf will not display properly in excel, so convert numeric to character
        #     
        #     sheet <- createSheet(wb, paste0("table", k))
        #     # Add sheet title and content:
        #     addDataFrame(data.frame(Dataframe = names(x)[[k]]), sheet, startRow = 1, startColumn = 1, col.names = FALSE, row.names = FALSE, colnamesStyle = cs)
        #     addDataFrame(x[[k]], sheet, startRow = 2, row.names = FALSE, colnamesStyle = cs)
        #   }
        #   saveWorkbook(wb, file = paste0(report_name, ".xlsx"))
        # }
      }
    }
  }
}

process_table <- function(data) {
  align(autofit(flextable(as.data.frame(data))), align = "center", part = "all")
}