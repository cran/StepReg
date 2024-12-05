
require("shiny") || stop("unable to load shiny")
require("shinyjs") || stop("unable to load shinyjs")
require("StepReg") || stop("unable to load StepReg")
require("survival") || stop("unable to load survival")
require("MASS") || stop("unable to load MASS")
require("cowplot") || stop("unable to load cowplot")
require("stringr") || stop("unable to load stringr")
require("DT") || stop("unable to load DT")
require("shinythemes") || stop("unable to load shinythemes")
require("shinycssloaders") || stop("unable to laod shinycssloaders")
require("ggplot2") || stop("unable to load ggplot2")
require("dplyr") || stop("unable to load dplyr")
require("summarytools") || stop("unable to load summarytools")
require("ggcorrplot") || stop("unable to load ggcorrplot")
require("tidyr") || stop("unable to load tidyr")
#require("GGally") || stop("unable to load GGally")
require("ggrepel") || stop("unable to load ggrepel")
require("rmarkdown") || stop("unable to load rmarkdown")

createPlot <- function(plot_type_value, var_plot_value, data_value) {
  plot_type <- switch(
    plot_type_value,
    "Bar plot" = {
      # Create a bar plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(x = .data[[var]])) +
          geom_bar() +
          labs(title = paste("Bar Plot of", var), x = var)
      })
    },
    "Box plot" = {
      # Create a box plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(y = .data[[var]])) +
          geom_boxplot() +
          labs(title = paste("Box Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)})
    },
    "Correlation plot" = {
      # Create a Correlation plot for each selected variable
      corr <- round(cor(select(data_value, all_of(var_plot_value))), 1)
      p <- list(corr %>% ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower", lab = TRUE))
    },
    "Density plot" = {
      # Create a density plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(.data[[var]])) +
          geom_density() +
          labs(title = paste("Density Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Dot plot" = {
      # Create a dot plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(.data[[var]])) +
          geom_dotplot() +
          labs(title = paste("Dot Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Histogram" = {
      # Create a histogram plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(.data[[var]])) +
          geom_histogram() +
          labs(title = paste("Histogram Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "QQ plot" = {
      # Create a qq plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          ggplot(aes(sample = .data[[var]])) +
          stat_qq() + 
          stat_qq_line() +
          labs(title = paste("QQ Plot of", var), x = var) + 
          scale_y_continuous(labels = NULL)
      })
    },
    "Scatter and Line plot" = {
      # Create a scatter plot for each selected variable
      lapply(var_plot_value, function(var) {
        data_value %>% 
          select(all_of(var_plot_value)) %>% 
          gather(-{{var}}, key = "var", value = "value") %>%
          ggplot(aes(x = value, y = .data[[var]])) +
          geom_point() +
          stat_smooth() +
          facet_wrap(~ var, scales = "free") +
          theme_bw()
      })
    }
    # "Pairs plot" = {
    #   # Create a pairs plot
    #   data_value %>% 
    #     select(all_of(var_plot_value)) %>% 
    #     ggpairs
    # }
  )
  return(plot_type)
}

# prepData <- function(example_dataset, upload_file, header_value, sep_value, quote_value){
#   if (example_dataset != " ") {
#     # Read the selected example dataset
#     data(CreditCard, package = 'AER')
#     data(remission,package="StepReg")
#     survival::lung %>%
#       mutate(sex = factor(sex, levels = c(1,2))) %>% # make sex as factor
#       mutate(status = ifelse(status == 1, 0, 1)) %>% # recode status: 0 means cencored, 1 means dead
#       na.omit() -> lung# get rid of incomplete records
#     
#     
#     df <- switch(example_dataset,
#                  "base::mtcars" = mtcars,
#                  "StepReg::remission" = remission,
#                  "survival::lung" = lung,
#                  "AER::CreditCard" = CreditCard)
#   }
#   if (!is.null(upload_file)) {
#     # Read the uploaded file
#     df <- read.table(upload_file$datapath,
#                      header = header_value,
#                      sep = sep_value,
#                      quote = quote_value)
#   }
#   return(df)
# }