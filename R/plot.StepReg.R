#' Plots from a StepReg object
#'
#' plot.StepReg visualizes the variable selection procedure using a StepReg object
#'
#' @param x StepReg object
#' 
#' @param ... Not used
#'
#' @return line and point plot for summary of selection process.
#' 
#' @import ggplot2
#' 
#' @importFrom stringr str_starts
#' 
#' @importFrom cowplot plot_grid
#' 
#' @importFrom ggrepel geom_label_repel
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' formula <- mpg ~ . + 0
#' p <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = c("forward","bidirection","subset"),
#'               metric = c("AIC", "BIC"),
#'               best_n = 3)
#' plot(p)
#' }

plot.StepReg <- function(x, ...) {
  process_list <- x[which(str_starts(names(x), "Summary of selection process under"))]
  strategy_vec <- class(x)[!class(x) %in% c("StepReg","list")]
  plot_list <- list()
  #n="subset"
  for(n in strategy_vec) {
    process_table <- process_list[which(str_starts(names(process_list), paste0("Summary of selection process under ",n)))]
    if ("subset" == n) {
      process_table_reformat <- lapply(process_table, function(df) {
        df <- cbind(df,colnames(df)[2])
        colnames(df) <- c("Step", "Metric_value", "Variable", "Metric")
        return(df)
      })
    } else {
      process_table_reformat <- lapply(process_table, function(df) {
        df <- cbind(df,colnames(df)[ncol(df)])
        var_sym <- rep(NA, nrow(df))
        var_plus <- paste0("(+)", df[, colnames(df) %in% "Enter_effect"])
        index_plus <- which(!var_plus %in% "(+)")
        var_minus <- paste0("(-)", df[, colnames(df) %in% "Remove_effect"])
        index_minus <- which(!var_minus %in% "(-)")
        var_sym[index_plus] <- var_plus[index_plus]
        var_sym[index_minus] <- var_minus[index_minus]
        df <- cbind(df[,c(1,ncol(df)-1,ncol(df))],var_sym)
        colnames(df) <- c("Step", "Metric_value", "Metric", "Variable")
        return(df)
      })
    }
    names(process_table_reformat) <- NULL
    plot_data <- do.call(rbind, process_table_reformat)
    plot_data$Step <- as.factor(as.numeric(plot_data$Step))
    plot_data$Metric_value <- as.numeric(plot_data$Metric_value)
    
    p <- ggplot(data = plot_data) + 
      aes(x = .data$Step,
          y = .data$Metric_value, 
          label = .data$Variable,
          group = .data$Metric) + 
      geom_point(aes(color = .data$Metric)) + 
      geom_label_repel(label.size = 0.05,
                       aes(color = .data$Metric),
                       show.legend = FALSE) +
      labs(title = paste0("Summary of selection process under ",n)) + 
      theme_minimal()
    
    if ("subset" != n) { # check if stepwise or best_subset
      p <- p + geom_line(aes(linetype = .data$Metric, color = .data$Metric)) +  xlab("Step")
    } else{
      p <- p + xlab("Variable Number")
    }
    plot_list[n] <- list(p)
  }
  combined_plot <- plot_grid(plotlist = plot_list, ncol = 1)
  print(combined_plot)
}
