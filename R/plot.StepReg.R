#' Plots from a StepReg object
#'
#' plot.StepReg visualizes the variable selection procedure using a StepReg object
#'
#' @param x StepReg object
#' 
#' @param num_digits The number of digits to keep when rounding the results. 
#' Default is 6.
#' 
#' @param ... Not used
#'
#' @return A list of plots comprising the selection detail plot and selection 
#' summary plot for each strategy.
#' 
#' @import ggplot2
#' 
#' @importFrom stringr str_starts
#' 
#' @importFrom dplyr group_by filter
#' 
#' @importFrom ggrepel geom_label_repel
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' formula <- mpg ~ .
#' x <- stepwise(formula = formula,
#'               data = mtcars,
#'               type = "linear",
#'               strategy = c("forward","bidirection","backward"),
#'               metric = c("AIC","BIC","SL"))
#' plot(x)
#' }

plot.StepReg <- function(x, num_digits = 6, ...) {
  y <- x$Detail_selection_summary
  process_list <- x[which(str_starts(names(x), "Summary of selection process under"))]
  strategy_vec <- class(x)[!class(x) %in% c("StepReg","list","linear","logit","cox","gamma","negbin","poisson")]
  plot_list <- list()
  
  for(n in strategy_vec) {
    process_table <- process_list[which(str_starts(names(process_list), paste0("Summary of selection process under ",n)))]
    if (n == "subset") {
      process_table_reformat <- lapply(process_table, function(df) {
        df <- cbind(df,colnames(df)[2])
        colnames(df) <- c("Step", "MetricValue", "Variable", "Metric")
        return(df)
      })
    } else {
      process_table_reformat <- lapply(process_table, function(df) {
        df <- cbind(df,colnames(df)[ncol(df)])
        var_sym <- rep(NA, nrow(df))
        var_plus <- paste0("(+)", df[, colnames(df) %in% "EffectEntered"])
        index_plus <- which(!var_plus %in% "(+)")
        var_minus <- paste0("(-)", df[, colnames(df) %in% "EffectRemoved"])
        index_minus <- which(!var_minus %in% "(-)")
        var_sym[index_plus] <- var_plus[index_plus]
        var_sym[index_minus] <- var_minus[index_minus]
        df <- cbind(df[,c(1,ncol(df)-1,ncol(df))],var_sym)
        colnames(df) <- c("Step", "MetricValue", "Metric", "Variable")
        return(df)
      })
    }
    names(process_table_reformat) <- NULL
    plot_data <- do.call(rbind, process_table_reformat)
    plot_data$Step <- as.numeric(plot_data$Step)
    plot_data$MetricValue <- as.numeric(plot_data$MetricValue)
    
    if(n != "subset") {
      detail_selection <- y[y$strategy == n,]
      p1 <- plotStepwiseDetail(detail_selection, num_digits)
    } else {
      #From SAS description: For two models A and B, each having the same number of explanatory variables, model A is considered to be better than model B if the global score chi-square statistic for A exceeds that for B.
      plot_data <- plot_data %>%
        group_by(.data$Step, .data$Metric) %>%
        filter(ifelse(.data$Metric == "SL", .data$MetricValue == max(.data$MetricValue), .data$MetricValue == min(.data$MetricValue)))
      
      p1 <- plotSubsetDetail(plot_data)
    }
    
    ## make a dual y-axis with log10 transformed for 'SL' if 'SL' is selected
    if(n != 'subset') {
      if("SL" %in% plot_data$Metric) {
        plot_data[plot_data$Metric == "SL",]$MetricValue <- detail_selection[detail_selection$metric == "SL" & detail_selection$selected == "YES","value"]
        plot_data$MetricValue[plot_data$MetricValue %in% Inf] <- max(plot_data$MetricValue[!plot_data$MetricValue %in% Inf]) * 1.1
        a <- range(log10(plot_data[plot_data$Metric == "SL", ]$MetricValue))
        b <- range(plot_data[plot_data$Metric != "SL", ]$MetricValue)
        
        p2 <- plotStepwiseSummaryDualY(x, plot_data, a, b, n)
      } else {
        p2 <- plotStepwiseSummarySingleY(plot_data)
      }
    } else {
      p2 <- plotSubsetSummary(plot_data)
    }
    p2 <- p2 +
      scale_x_continuous(breaks = plot_data$Step) + 
      labs(title ="Selection process") + 
      theme_light()
    
    #p3 <- cowplot::plot_grid(p1, p2, ncol=1, rel_heights = c(1, 0.75))
    plot_list[[n]]["detail"] <- list(p1)
    plot_list[[n]]["summary"] <- list(p2)
  }
  return(plot_list)
}

plotStepwiseSummaryDualY <- function(x, df, a, b, n){
  if(all(df$Metric %in% "SL")) {
    p2 <- ggplot(df, aes(x = .data$Step, group = .data$Metric)) +
      geom_point(aes(y = (log10(.data$MetricValue) - a[1])/diff(a), color = .data$Metric)) + 
      geom_label_repel(aes(y = (log10(.data$MetricValue) - a[1])/diff(a), color = .data$Metric, label = .data$Variable),
                       label.size = 0.05,
                       show.legend = FALSE) + 
      scale_y_continuous(
        labels = function(x) sprintf("%.1e", 10^(x * diff(a) + a[1])),
        breaks = (pretty(log10(df$MetricValue)) - a[1])/diff(a), 
        name = "SL") + 
      geom_line(aes(y = (log10(.data$MetricValue) - a[1])/diff(a), color = .data$Metric))
  } else {
    p2 <- ggplot(df, aes(x = .data$Step, group = .data$Metric)) +
      geom_point(aes(y = ifelse(.data$Metric == "SL", (log10(.data$MetricValue) - a[1])/diff(a), (.data$MetricValue - b[1])/diff(b)), color = .data$Metric)) + 
      geom_label_repel(aes(y = ifelse(.data$Metric == "SL", (log10(.data$MetricValue) - a[1])/diff(a), (.data$MetricValue - b[1])/diff(b)), color = .data$Metric, label = .data$Variable),
                       label.size = 0.05,
                       show.legend = FALSE) + 
      scale_y_continuous(
        labels = function(x) sprintf("%.1e", 10^(x * diff(a) + a[1])),
        breaks = (pretty(log10(df$MetricValue)) - a[1])/diff(a), 
        name = "SL",
        sec.axis = sec_axis(~(diff(b) * . + b[1]), name = "Information Criteria")) + 
      geom_line(aes(y = ifelse(.data$Metric == "SL", (log10(.data$MetricValue) - a[1])/diff(a), (.data$MetricValue - b[1])/diff(b)), color = .data$Metric))
  }
  if(n == "forward") {
    sle <- x[[1]][which(x[[1]][,1] %in% "significance level for entry (sle)"), 2]
    p2 <- p2 + 
      geom_hline(yintercept=(log10(as.numeric(sle)) - a[1])/diff(a), linetype="dashed", color = "gray") + 
      geom_text(aes(0,(log10(as.numeric(sle)) - a[1])/diff(a), label = paste0("sle=",sle), vjust = -1), color = "gray")
  } else if (n == "backward") {
    sls <- x[[1]][which(x[[1]][,1] %in% "significance level for stay (sls)"), 2]
    p2 <- p2 + 
      geom_hline(yintercept=(log10(as.numeric(sls)) - a[1])/diff(a), linetype="dotdash", color = "gray") + 
      geom_text(aes(max(as.numeric(.data$Step)), (log10(as.numeric(sls)) - a[1])/diff(a), label = paste0("sls=",sls), vjust = -1), color = "gray")
  } else {
    sle <- x[[1]][which(x[[1]][,1] %in% "significance level for entry (sle)"), 2]
    sls <- x[[1]][which(x[[1]][,1] %in% "significance level for stay (sls)"), 2]
    p2 <- p2 +
      geom_hline(yintercept=(log10(as.numeric(sle)) - a[1])/diff(a), linetype="dashed", color = "gray") + 
      geom_text(aes(1.5,(log10(as.numeric(sle)) - a[1])/diff(a), label = paste0("sle=",sle), vjust = -1), color = "gray") + 
      geom_hline(yintercept=(log10(as.numeric(sls)) - a[1])/diff(a), linetype="dotdash", color = "gray") + 
      geom_text(aes(max(as.numeric(.data$Step)), (log10(as.numeric(sls)) - a[1])/diff(a), label = paste0("sls=",sls), vjust = -1), color = "gray")
  }
  p2 <- p2 + xlab("step")
  return(p2)
}

plotStepwiseSummarySingleY <- function(df){
  p2 <- ggplot(data = df) + 
    aes(x = .data$Step,
        y = .data$MetricValue, 
        label = .data$Variable,
        group = .data$Metric) + 
    geom_point(aes(color = .data$Metric)) + 
    geom_line(aes(linetype = .data$Metric, color = .data$Metric)) +
    geom_label_repel(label.size = 0.05,
                     aes(color = .data$Metric),
                     show.legend = FALSE) + 
    xlab("step")
}

plotSubsetSummary <- function(df) {
  p2 <- ggplot(data = df) + 
    aes(x = .data$Step,
        y = .data$MetricValue, 
        label = .data$Variable,
        group = .data$Metric) + 
    geom_point(aes(color = .data$Metric)) + 
    geom_line(aes(linetype = .data$Metric, color = .data$Metric)) +
    xlab("Variable Number")
  return(p2)
}

plotStepwiseDetail <- function(df, num_digits) {
  p1 <- ggplot(df, 
               aes(x = .data$step,
                   y = .data$variable)) +
    geom_tile(aes(fill = .data$selected), width = 0.99, height = 0.95, color = "black") +
    geom_text(aes(label = round(.data$value, num_digits)),
              color = "black",
              size = 2) +
    scale_fill_manual(values = c("YES" = "palegreen2", "NO" = "gray80")) +
    theme_light() + 
    scale_x_continuous(breaks = unique(df$step)) + 
    theme(axis.text.y = element_text(size = 8),
          strip.text = element_text(color = "black")) +  # Adjust text color in facet labels
    facet_wrap(~ .data$metric, ncol=1) + 
    theme(strip.background = element_rect(colour = "black", fill = "gray80")) +
    ggtitle("Metric values at each step")
  return(p1)
}

plotSubsetDetail <- function(plot_data) {
  #-------------------------
  #subset works for SL in logit(need to test)
  
  variable_list <- lapply(strsplit(plot_data$Variable, " "), function(x) x[x != ""])
  tile_df <- expand.grid(Variable = variable_list[[length(variable_list)]], Step = plot_data$Step)
  tile_df$Metric <- rep(plot_data$Metric, each = length(variable_list[[length(variable_list)]]))
  tile_df$Selected <- mapply(function(metric, step, variable) {
    df2_1 <- plot_data[plot_data$Metric == metric, ]
    df2_2 <- df2_1[df2_1$Step == step, ]
    any(variable %in% strsplit(df2_2$Variable, " ")[[1]])
  }, tile_df$Metric, tile_df$Step, tile_df$Variable)
  tile_df$Selected <- ifelse(tile_df$Selected, "YES", "NO")
  
  p1 <- ggplot(tile_df, aes(x = .data$Step, y = .data$Variable, fill = .data$Selected)) +
    geom_tile(width = 0.99, height = 0.95, color = "black") +
    scale_fill_manual(values = c("YES" = "palegreen2", "NO" = "gray80")) +
    labs(x = "step", y = "variable", title = "Variable selection in each step") +
    scale_x_continuous(breaks = plot_data$Step) + 
    xlab("Variable Number") +
    facet_wrap(~ .data$Metric, ncol=1) + 
    theme(strip.background = element_rect(colour = "black", fill = "gray80")) +
    ggtitle("Best subset model under each variable number")
}


