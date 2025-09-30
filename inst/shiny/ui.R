source("utils.R")

js <- "
$(document).ready(function() {
    $('.navbar .container-fluid .navbar-nav .dropdown .dropdown-menu').prepend(
        '<li><a href=\"https://cran.r-project.org/web/packages/StepReg/vignettes/StepReg.html\" target=\"_blank\">Vignette</a></li>',
        '<li><a href=\"https://github.com/JunhuiLi1017/StepReg/issues/new\" target=\"_blank\">Report Bug</a></li>');
});
"

ui <- tagList(
  useShinyjs(),
  #tags$script(HTML(js)),
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
      }
      
      /* Stepwise regression themed styling */
      .navbar-default {
        background-color: #2c3e50;
        border-color: #2c3e50;
      }
      
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: #ecf0f1;
        font-size: 14px;
      }
      
      .navbar-default .navbar-brand:hover,
      .navbar-default .navbar-nav > li > a:hover {
        color: #3498db;
      }
      
      .navbar-default .navbar-nav > .open > a,
      .navbar-default .navbar-nav > .open > a:hover {
        background-color: #34495e;
        color: #ecf0f1;
      }
      
      /* Sidebar styling */
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
      }
      
      /* Button styling */
      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
        font-size: 12px;
      }
      
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
      }
      
      /* Formula input styling */
      .form-control:focus {
        border-color: #3498db;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }
      
      /* Table styling */
      .table {
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        font-size: 12px;
      }
      
      .table th {
        background-color: #3498db;
        color: white;
        border: none;
        font-size: 12px;
      }
      
      /* Info box styling */
      .info-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
      }
      
      /* Step indicator styling */
      .step-indicator {
        background-color: #e8f4fd;
        border-left: 4px solid #3498db;
        padding: 10px;
        margin: 10px 0;
        border-radius: 4px;
      }
      
      .step-indicator h5 {
        font-size: 13px;
        margin: 0 0 5px 0;
        font-weight: bold;
      }
      
      /* Metric selection styling */
      .metric-selection {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 10px;
        margin: 5px 0;
      }
      
      /* General font size adjustments */
      body {
        font-size: 12px;
      }
      
      h1, h2, h3, h4, h5, h6 {
        font-size: 14px;
      }
      
      h1 { font-size: 18px; }
      h2 { font-size: 16px; }
      h3 { font-size: 15px; }
      h4 { font-size: 14px; }
      h5 { font-size: 13px; }
      h6 { font-size: 12px; }
      
      /* Label styling */
      label {
        font-size: 12px;
        font-weight: normal;
      }
      
      /* Select input styling */
      .selectize-input {
        font-size: 12px;
      }
      
      .selectize-dropdown {
        font-size: 12px;
      }
      
      /* Text area styling */
      textarea {
        font-size: 12px;
      }
      
      /* Input styling */
      input {
        font-size: 12px;
      }
      
      /* Paragraph styling */
      p {
        font-size: 12px;
        line-height: 1.4;
      }
      
      /* List styling */
      ul, ol {
        font-size: 12px;
      }
      
      /* About section styling */
      .about-section h3 {
        font-size: 16px;
      }
      
      .about-section h4 {
        font-size: 14px;
      }
      
      .about-section p, .about-section li {
        font-size: 12px;
      }
    "))
  ),
  navbarPage(
    title = tags$a(href = "https://cran.r-project.org/web/packages/StepReg/index.html", "StepReg"),
    theme = shinythemes::shinytheme("cosmo"),
    
    # Define tabs within the navbar
    tabPanel(
      
      title = "Data", 
  
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
          actionButton("change_class", "Change variable types")
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
      "Stepwise",
      # p("To perform stepwise regression, you need to first specify the type of 
      #   stepwise procedure, which involves determining the dependent variables by 
      #   defining the scope of both dependent and independent variables. Next, 
      #   select one or more selection strategies and metrics to guide the stepwise 
      #   regression process."),
      sidebarLayout(
        sidebarPanel(
          tags$div(
            class = "step-indicator",
            tags$h5("Step 1: Select Regression Type"),
          ),
          
          # Select type (linear, logit, cox, poisson, gamma, or negbin)
          selectInput(
            "type",
            label = "",
            choices = c("linear",
                        "logit",
                        "cox",
                        "poisson",
                        "gamma",
                        "negbin")
          ),
          
          tags$div(
            class = "step-indicator",
            tags$h5("Step 2: Enter Formula"),
          ),
          uiOutput("formula_placeholder_help"),
          textAreaInput(
            "formula_input",
            label = "",
            placeholder = "Enter your formula here...",
            rows = 3
          ),
          uiOutput("include_input_ui"),
          
          tags$div(
            class = "step-indicator",
            tags$h5("Step 3: Choose Strategy")
          ),
          
          # Select method (forward, backward, or both)
          selectInput(
            "strategy", 
            label = "",
            choices = c("forward", 
                        "backward",
                        "bidirection", 
                        "subset"),
            multiple = TRUE
          ),
          
          tags$div(
            class = "step-indicator",
            tags$h5("Step 4: Select Metric")
          ),
          
          conditionalPanel(
            condition = "input.type === 'linear' && !input.formula_input.includes('cbind(')",
            selectInput(
              "metric_univariate_linear", 
              label = "",
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
            condition = "input.type === 'linear' && input.formula_input.includes('cbind(')",
            selectInput(
              "metric_multivariate_linear", 
              label = "",
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
              label = "",
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
            condition = "input.type === 'linear' && input.formula_input.includes('cbind(') && input.metric_multivariate_linear.indexOf('SL') != -1",
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
            condition = "input.type === 'linear' && !input.formula_input.includes('cbind(') && input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1)",
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
            condition = "input.type === 'linear' && !input.formula_input.includes('cbind(') && input.metric_univariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1)",
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
            condition = "input.type === 'linear' && input.formula_input.includes('cbind(') && input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('forward') != -1 || input.strategy.indexOf('bidirection') != -1)",
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
            condition = "input.type === 'linear' && input.formula_input.includes('cbind(') && input.metric_multivariate_linear.indexOf('SL') != -1 && (input.strategy.indexOf('backward') != -1 || input.strategy.indexOf('bidirection') != -1)",
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
            class = "step-indicator",
            tags$h5("Step 5: Data Splitting and Randomized Forward Selection")
          ),
          sliderInput(
            "test_ratio",
            label = tags$span(
              "Test ratio (holdout)",
              tags$i(
                class = "glyphicon glyphicon-question-sign centered-icon",
                style = "color:#0072B2;",
                title = paste(c(
                  "Proportion of data reserved for testing (0 ≤ r < 1).",
                  "Rows are randomly split into train/test using a fixed seed for reproducibility.",
                  "Set to 0 to disable holdout validation."), collapse = "\n")
              )
            ),
            min = 0,
            max = 0.99,
            value = 0,
            step = 0.05
          ),
          conditionalPanel(
            condition = "input.strategy.indexOf('forward') != -1",
            sliderInput(
              "feature_ratio",
              label = tags$span(
                "Feature ratio (forward only)",
                tags$i(
                  class = "glyphicon glyphicon-question-sign centered-icon",
                  style = "color:#0072B2;",
                  title = paste(c(
                    "Proportion of candidate features sampled uniformly at random",
                    "during each forward add step (0 < r ≤ 1). r = 1 disables sampling."), collapse = "\n")
                )
              ),
              min = 0.1,
              max = 1,
              value = 1,
              step = 0.1
            )
          ),
          
          tags$div(
            class = "step-indicator",
            tags$h5("Step 6: Run Analysis")
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
              "Model Voting",
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
      title = "About",
      tabPanel(
        "StepReg Package",
        tags$div(
          class = "about-section",
          style = "padding: 20px;",
          tags$h3("StepReg: Stepwise Regression Analysis"),
          tags$p(
            "Stepwise regression is a statistical technique used for model selection. This package streamlines stepwise regression analysis by supporting multiple regression types, incorporating popular selection strategies, and offering essential metrics. It enables users to apply multiple selection strategies and metrics in a single function call, visualize variable selection processes, and export results in various formats. StepReg supports advanced features including strata variables for Cox regression and continuous-nested-within-class effects for complex modeling scenarios. However, StepReg should not be used for statistical inference unless the variable selection process is explicitly accounted for, as it can compromise the validity of the results. This limitation does not apply when StepReg is used for prediction purposes. We validated StepReg's accuracy using public datasets within the SAS software environment."
          ),
          tags$h4("Key Features:"),
          tags$ul(
            tags$li("Multiple stepwise regression types: linear, logistic, Cox proportional hazards, Poisson, and Gamma regression"),
            tags$li("Multiple stepwise strategies: forward, backward, bidirectional, and subset selection"),
            tags$li("Various selection criteria: AIC, AICc, BIC, CP, HQ, SL, SBC, IC(1), IC(3/2), adjRsq"),
            tags$li("Multivariate multiple regression for linear regression"),
            tags$li("Cox regression with strata() function"),
            tags$li("Continuous-nested-within-class effects"),
            tags$li("Comprehensive visualization of selection process"),
            tags$li("Multiple report output formats: html, docx, pptx, and rtf"),
            tags$li("Model voting across different strategies and metrics")
          )
        )
      ),
      tabPanel(
        "Citation",
        tags$div(
          class = "about-section",
          style = "padding: 20px;",
          tags$h3("Citation"),
          tags$p("If you find StepReg useful in your research, please cite:"),
          tags$div(
            style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;",
            tags$p(
              paste0("Junhui Li et al. (2025). StepReg: Stepwise Regression Analysis. R package version ", packageVersion("StepReg"), ", "),
              tags$a(href = "https://CRAN.R-project.org/package=StepReg", "https://CRAN.R-project.org/package=StepReg")
            )
          ),
          tags$h4("BibTeX Entry:"),
          tags$pre(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-size: 11px;",
            paste0("@Manual{StepReg2025,
  title = {StepReg: Stepwise Regression Analysis},
  author = {Junhui Li, Kai Hu, Xiaohuan Lu, Sushmita N Nayak, Cesar Bautista Sotelo, Michael A Lodato, Wenxin Liu, Lihua Julie Zhu},
  year = {2025},
  note = {R package version ", packageVersion("StepReg"), "},
  url = {https://CRAN.R-project.org/package=StepReg},
}")
          )
        )
      ),
      tabPanel(
        "Contact & Support",
        tags$div(
          class = "about-section",
          style = "padding: 20px;",
          tags$h3("Get Help and Support"),
          tags$p("Need help with StepReg? Here are some resources:"),
          tags$ul(
            tags$li(tags$a(href = "https://cran.r-project.org/web/packages/StepReg/vignettes/StepReg.html", "Package Vignette"), " - Comprehensive tutorial and examples"),
            tags$li(tags$a(href = "https://github.com/JunhuiLi1017/StepReg/issues/new", "Report Issues"), " - Report bugs or request features"),
            tags$li(tags$a(href = "https://cran.r-project.org/web/packages/StepReg/index.html", "CRAN Page"), " - Official package page"),
            tags$li(tags$a(href = "https://github.com/JunhuiLi1017/StepReg", "GitHub Repository"), " - Source code and development")
          ),
          tags$h4("Package Information:"),
          tags$ul(
            tags$li(paste("Version:", packageVersion("StepReg"))),
            tags$li("License: MIT"),
            tags$li("Maintainer: Junhui Li (junhui.li11@umassmed.edu)"),
            tags$li("Dependencies: R (≥ 4.0.0)")
          )
        )
      )
    )
  )
)