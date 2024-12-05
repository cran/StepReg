source("utils.R")

js <- "
$(document).ready(function() {
    $('.navbar .container-fluid .navbar-nav .dropdown .dropdown-menu').prepend(
        '<li><a href=\"https://cran.r-project.org/web/packages/StepReg/vignettes/StepReg.html\" target=\"_blank\">Tutorial</a></li>',
        '<li><a href=\"https://github.com/JunhuiLi1017/StepReg/issues/new\" target=\"_blank\">Report Bug</a></li>');
});
"

ui <- tagList(
  useShinyjs(),
  tags$script(HTML(js)),
  tags$head(
    tags$style(HTML("
      /* this will affect all the pre elements */
      pre {
        color: red;
        background-color: #5ef4fb;
      }
      /* this will affect only the pre elements under the class myclass */
      .textOutput pre {
        color: black;
        background-color: white;
      }"))
  ),
  navbarPage(
    title = tags$a(href = "https://cran.r-project.org/web/packages/StepReg/index.html", "StepReg"),
    theme = shinythemes::shinytheme("flatly"),
    
    # Define tabs within the navbar
    tabPanel(
      
      title = "File", 
  
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
          selectInput(
            "example_dataset",
            label = tags$span(
              "Select an example dataset", 
              tags$i(
                class = "glyphicon glyphicon-question-sign centered-icon", 
                style = "color:#0072B2;",
                title = paste(c(
                  "Recommended Regression Type and Dependent Variable for Each Dataset (Demo Purposes)",
                  "------------------------------------------------------",
                  "Dataset\t|\tType\t\t|\tResponse",
                  "------------------------------------------------------",
                  "mtcars\t|\tlinear\t|\tmpg",
                  "remission\t|\tlogit\t\t|\tremiss",
                  "lung\t\t|\tcox\t\t|\ttime and status",
                  "creditCard\t|\tpoisson\t|\treports",
                  "------------------------------------------------------"), collapse = "\n"
                )
              )
            ),
            choices = c(
              "",
              "mtcars", 
              "remission", 
              "lung",
              "creditCard")
          ),
          
            # Input: Select a file ----
          fileInput(
            "upload_file", 
            "Or upload your data",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
          

          # Input: Checkbox if file has header 
          checkboxInput("header", "Header", TRUE),
          
          # Input: Select separator ----
          radioButtons(
            "sep", 
            "Separator",
            choices = c(Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"),
            selected = ","),
          
          # Input: Select quotes ----
          radioButtons("quote", "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'")),
          uiOutput("colname_in_numeric"),
          uiOutput("colname_in_factor"),
          uiOutput("colname_in_integer"),
          uiOutput("colname_in_character"),
          actionButton("change_class", "Change class")
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
          
          tabsetPanel(
            
            tabPanel(
              "Data",
              div(style = "width: 100%;",
                  withSpinner(DT::dataTableOutput('tbl', width = 750))
                  )
            ), # Data dalam tabel
            tabPanel(
              "Summary",
              div(class="textOutput",
                  withSpinner(verbatimTextOutput("summary"))
                  )
            ),
            tabPanel(
              title = "Plot",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("plot_type",
                                "select plot type:",
                                choices = c("Bar plot",
                                            "Box plot",
                                            "Correlation plot",
                                            "Density plot",
                                            "Dot plot",
                                            #"Pairs plot",
                                            "Histogram",
                                            "QQ plot",
                                            "Scatter and Line plot"),
                                selected = "Correlation plot"),
                    
                    #h5("select variables for the Plot:"),
                    selectInput("var_plot", 
                                "Select variables for the Plot:", 
                                choices = "", 
                                multiple = TRUE),
                    tags$div(
                      style = "display: flex; justify-content: space-between;",
                      actionButton("make_plot", 
                                   "Run", 
                                   icon = icon("chart-line"), 
                                   style = "width: 80px; font-size: 10px;"),
                      downloadButton("downloadPlot", 
                                     "Save", 
                                     icon = icon("download"),
                                     style = "width: 80px; font-size: 10px;")
                    )
                  ),
                  mainPanel(
                    withSpinner(plotOutput("Plot"))
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Analyze",
      # p("To perform stepwise regression, you need to first specify the type of 
      #   stepwise procedure, which involves determining the dependent variables by 
      #   defining the scope of both dependent and independent variables. Next, 
      #   select one or more selection strategies and metrics to guide the stepwise 
      #   regression process."),
      sidebarLayout(
        sidebarPanel(
          # Select type (linear, logit, cox, poisson, gamma, or negbin)
          selectInput(
            "type",
            "Regression type:",
            choices = c("linear",
                        "logit",
                        "cox",
                        "poisson",
                        "gamma",
                        "negbin")
          ),
          # Select dependent variable
          conditionalPanel(
            condition = "input.type === 'cox'",
            selectInput("status", "Status variable:", choices = NULL),
            selectInput("time", "Time Variable:", choices = NULL)
          ),
          
          conditionalPanel(
            condition = "input.type === 'linear'",
            selectInput("dependent_linear", "Dependent variable:", choices = NULL, multiple = TRUE)
          ),
          
          conditionalPanel(
            condition = "input.type === 'logit' || input.type === 'poisson' || input.type === 'gamma' || input.type === 'negbin'",
            selectInput("dependent_glm", "Dependent variable:", choices = NULL)
          ),
          
          selectInput(
            "independent",
            "Independent Variables:",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "include", 
            "Include Variables:",
            choices = NULL,
            multiple = TRUE
          ),
          conditionalPanel(
            condition = "input.type !== 'cox'",
            checkboxInput(
              "intercept", 
              "Include Intercept",
              TRUE
            )
          ),
          
          # Select method (forward, backward, or both)
          selectInput(
            "strategy", 
            "Stepwise Strategy:",
            choices = c("forward", 
                        "backward",
                        "bidirection", 
                        "subset"),
            multiple = TRUE
          ),
          
          # Select metric
          conditionalPanel(
            condition = "input.type === 'linear' && input.dependent_linear.length == 1",
            selectInput(
              "metric_univariate_linear", 
              "Selection Metric:",
              choices = c("AIC", 
                          "AICc",
                          "BIC", 
                          "CP",
                          "HQ",
                          "IC(1)",
                          "IC(3/2)",
                          "SBC",
                          "SL",
                          "adjRsq"),
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.type === 'linear' && input.dependent_linear.length > 1",
            selectInput(
              "metric_multivariate_linear", 
              "Selection Metric:",
              choices = c("AIC", 
                          "AICc",
                          "HQ",
                          "SL"),
              multiple = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.type !== 'linear'",
            selectInput(
              "metric_glm_cox", 
              "Selection Metric:",
              choices = c("AIC", 
                          "AICc",
                          "HQ",
                          "IC(1)",
                          "IC(3/2)",
                          "SBC",
                          "SL"),
              multiple = TRUE
            )
          ),
          
          # Display sliderInput for significance level only when SL is selected
          conditionalPanel(
            condition = "input.type === 'linear' && input.metric_multivariate_linear.indexOf('SL') != -1",
            selectInput(
              "Approx_F", 
              label = "Approx F test statistic:",
              choices = c("Pillai", 
                          "Wilks", 
                          "Hotelling-Lawley", 
                          "Roy"),
              selected = "Pillai"
            )
          ),
          
          conditionalPanel(
            condition = "input.type === 'logit' || input.type === 'gamma' || input.type === 'poisson' || input.type === 'negbin'",
            selectInput(
              "glm_test", 
              label = "Test Method",
              choices = c("Rao", 
                          "LRT"),
              selected = "Rao"
            )
          ),
          
          conditionalPanel(
            condition = "input.type === 'cox'",
            selectInput(
              "cox_test", 
              label = "Test Method",
              choices = c('efron',
                          'breslow',
                          'exact'),
              selected = "efron"
            )
          ),
          
          # Display sliderInput for significance level only when SL is selected
          conditionalPanel(
            condition = "input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
            sliderInput(
              "sle", 
              label = tags$span(
                "significance level for entry", 
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon", 
                  style = "color:#0072B2;",
                  title = paste(c("Cut-off for 'SL' metric under forward and bidirection strategy. Depending on the type of regression, different tests will be employed to compute the corresponding P-values.",
                                "------------------------------------------------------",
                                "Type\t\t\t\t|\t\t\t\tTest",
                                "------------------------------------------------------",
                                "linear(univariate)\t|\tF test",
                                "linear(multivariate)\t|\tApprox F test",
                                "logit/poission/gamma\t|\tRao or LRT Chi-squared",
                                "cox\t\t\t\t|\tWald Chi-squared",
                                "------------------------------------------------------"), collapse = "\n")
                )
              ),
              min = 0, 
              max = 1,
              value = 0.05
            )
          ),
          
          conditionalPanel(
            condition = "input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
            sliderInput(
              "sls", 
              label = tags$span(
                "significance level for stay", 
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon", 
                  style = "color:#0072B2;",
                  title = paste(c("Cut-off for 'SL' metric under backward and bidirection strategy. Depending on the type of regression, different tests will be employed to compute the corresponding P-values.",
                                  "------------------------------------------------------",
                                  "Type\t\t\t\t|\t\t\t\tTest",
                                  "------------------------------------------------------",
                                  "linear(univariate)\t|\tF test",
                                  "linear(multivariate)\t|\tApprox F test",
                                  "logit/poission/gamma\t|\tWald Chi-squared",
                                  "cox\t\t\t\t|\tWald Chi-squared",
                                  "------------------------------------------------------"), collapse = "\n")
                )
              ),
              min = 0, 
              max = 1, 
              value = 0.05
            )
          ),
          
          conditionalPanel(
            condition = "input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
            sliderInput(
              "sle", 
              label = tags$span(
                "significance level for entry", 
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon", 
                  style = "color:#0072B2;",
                  title = paste(c("Cut-off for 'SL' metric under forward and bidirection strategy.. Depending on the type of regression, different tests will be employed to compute the corresponding P-values.",
                                  "------------------------------------------------------",
                                  "Type\t\t\t\t|\t\t\t\tTest",
                                  "------------------------------------------------------",
                                  "linear(univariate)\t|\tF test",
                                  "linear(multivariate)\t|\tApprox F test",
                                  "logit/poission/gamma\t|\tRao or LRT Chi-squared",
                                  "cox\t\t\t\t|\tWald Chi-squared",
                                  "------------------------------------------------------"), collapse = "\n")
                )
              ),
              min = 0, 
              max = 1,
              value = 0.05
            )
          ),
          
          conditionalPanel(
            condition = "input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type === 'linear'",
            sliderInput(
              "sls", 
              label = tags$span(
                "significance level for stay", 
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon", 
                  style = "color:#0072B2;",
                  title = paste(c("Cut-off for 'SL' metric under backward and bidirection strategy. Depending on the type of regression, different tests will be employed to compute the corresponding P-values.",
                                  "------------------------------------------------------",
                                  "Type\t\t\t\t|\t\t\t\tTest",
                                  "------------------------------------------------------",
                                  "linear(univariate)\t|\tF test",
                                  "linear(multivariate)\t|\tApprox F test",
                                  "logit/poission/gamma\t|\tWald Chi-squared",
                                  "cox\t\t\t\t|\tWald Chi-squared",
                                  "------------------------------------------------------"), collapse = "\n")
                )
              ), 
              min = 0, 
              max = 1, 
              value = 0.05)
          ),
          
          conditionalPanel(
            condition = "input.metric_glm_cox.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type !== 'linear'",
            sliderInput(
              "sle", 
              label = tags$span(
                "significance level for entry", 
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon", 
                  style = "color:#0072B2;",
                  title = paste(c("Cut-off for 'SL' metric under forward and bidirection strategy.. Depending on the type of regression, different tests will be employed to compute the corresponding P-values.",
                                  "------------------------------------------------------",
                                  "Type\t\t\t\t|\t\t\t\tTest",
                                  "------------------------------------------------------",
                                  "linear(univariate)\t|\tF test",
                                  "linear(multivariate)\t|\tApprox F test",
                                  "logit/poission/gamma\t|\tRao or LRT Chi-squared",
                                  "cox\t\t\t\t|\tWald Chi-squared",
                                  "------------------------------------------------------"), collapse = "\n")
                )
              ),
              min = 0, 
              max = 1,
              value = 0.05)
          ),
          
          conditionalPanel(
            condition = "input.metric_glm_cox.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1) && input.type !== 'linear'",
            sliderInput(
              "sls", 
              label = tags$span(
                "significance level for stay", 
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon", 
                  style = "color:#0072B2;",
                  title = paste(c("Cut-off for 'SL' metric under backward and bidirection strategy. Depending on the type of regression, different tests will be employed to compute the corresponding P-values.",
                                  "------------------------------------------------------",
                                  "Type\t\t\t\t|\t\t\t\tTest",
                                  "------------------------------------------------------",
                                  "linear(univariate)\t|\tF test",
                                  "linear(multivariate)\t|\tApprox F test",
                                  "logit/poission/gamma\t|\tWald Chi-squared",
                                  "cox\t\t\t\t|\tWald Chi-squared",
                                  "------------------------------------------------------"), collapse = "\n")
                )
              ),
              min = 0, 
              max = 1, 
              value = 0.05)
          ),
          tags$div(
            style = "display: flex; justify-content: space-between;",
            actionButton("run_analysis", 
                         "Run", 
                         icon = icon("chart-line"), 
                         style = "width: 130px; font-size: 12px;"),
            downloadButton("download", 
                           "Report", 
                           icon = icon("download"),
                           style = "width: 130px; font-size: 12px;")
          )
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Statistics",
              conditionalPanel(
                condition = "input.run_analysis",
                htmlOutput("selectionStatText")
              ),
              div(class="textOutput",
                  withSpinner(verbatimTextOutput("modelSelection"))
                  )
            ),
            
            tabPanel(
              title = "Visualization",
              fluidPage(
                fluidRow(
                  column(
                    width = 12,
                    sidebarPanel(
                      selectInput("strategy_plot",
                                  "Stepwise Strategy:",
                                  choices = c("")),
                      selectInput("relative_height",
                                  label = tags$span(
                                    "Relative height of plots", 
                                    tags$i(
                                      class = "glyphicon glyphicon-question-sign centered-icon", 
                                      style = "color:#0072B2;",
                                      title = paste(c(
                                        "Adjust the height of plot B relative to plot A"), collapse = "\n"
                                      )
                                    )
                                  ),
                                  choices = c(seq(0.2,1,0.2),seq(1.5,4,0.5)), 
                                  selected = 1),
                      downloadButton("download_process_plot",
                                     "Save",
                                     icon = icon("download"),
                                     style = "width: 80px; font-size: 10px;")
                      )
                   )
                ),
                fluidRow(
                  column(width = 12,
                         mainPanel(
                           conditionalPanel(
                             condition = "input.run_analysis",
                             htmlOutput("selectionPlotText")
                           ),
                           withSpinner(plotOutput("detail_plot"))
                         )
                  )
                )
              )
            ),
            
            tabPanel(
              "Model Vote",
              conditionalPanel(
                condition = "input.run_analysis",
                htmlOutput("modelVoteText")
              ),
              withSpinner(DT::dataTableOutput("modelVote"))
            )
          )
        )
      )
    ),
    navbarMenu(
      title = "Help",
      tabPanel(
        "Citation",
        tags$p("If you think 'StepReg' R package is helpful to your research, please cite:"),
        tags$ul(
          tags$li("Junhui Li (2024). StepReg: Stepwise Regression Analysis. R package version 1.5.6, https://CRAN.R-project.org/package=StepReg")
        )
      )
    )
  )
)
