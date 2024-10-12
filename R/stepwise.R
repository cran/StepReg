#' Main wrapper function for stepwise regression
#' 
#' Select optimal model using various stepwise regression strategies, e.g., Forward Selection, Backward Elimination, Bidirectional Elimination; meanwhile, it also supports Best Subset method. Four types of models are currently implemented: linear regression, logistic regression, Cox regression, Poisson, and Gamma regression. For selection criteria, a.k.a, stop rule, users can choose from AIC, AICc, BIC, HQ, Significant Level, and more.
#' 
#' @param formula (formula) The formula used for model fitting by defining the scope of dependent and independent variables. The formula takes the form of a '~' (tilde) symbol, with the response variable(s) on the left-hand side, and the predictor variable(s) on the right-hand side. The 'lm()' function uses this formula to fit a regression model. A formula can be as simple as 'y ~ x'. For multiple predictors, they must be separated by the '+' (plus) symbol, e.g. 'y ~ x1 + x2'. To include an interaction term between variables, use the ':' (colon) symbol: 'y ~ x1 + x1:x2'. Use the '.' (dot) symbol to indicate that all other variables in the dataset should be included as predictors, e.g. 'y ~ .'. In the case of multiple response variables (multivariate), the formula can be specified as 'cbind(y1, y2) ~ x1 + x2'. By default, an intercept term is always included in the models, to exclude it, include '0' or '- 1' in your formula: 'y ~ 0 + x1', 'y ~ x1 + 0', and 'y ~ x1 - 1'.
#' 
#' @param data (data.frame) A dataset consisting of predictor variable(s) and response variable(s).
#' 
#' @param type (character) The stepwise regression type. Choose from 'linear', 'logit', 'poisson', 'cox', 'gamma' and 'negbin'. Default is 'linear'. More information, see \href{https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html}{StepReg_vignettes}
#' 
#' @param include (NULL|character) A character vector specifying predictor variables that will always stay in the model. A subset of the predictors in the dataset.
#' 
#' @param strategy (character) The model selection strategy. Choose from 'forward', 'backward', 'bidirectional' and 'subset'. Default is 'forward'. More information, see \href{https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html}{StepReg_vignettes}
#' 
#' @param metric (character) The model selection criterion (model fit score). Used for the evaluation of the predictive performance of an intermediate model. Choose from 'AIC', 'AICc', 'BIC', 'CP', 'HQ', 'adjRsq', 'SL', 'SBC', 'IC(3/2)', 'IC(1)'. Default is 'AIC'. More information, see \href{https://CRAN.R-project.org/package=StepReg/vignettes/StepReg.html}{StepReg_vignettes}
#' 
#' @param sle (numeric) Significance Level to Enter. It is the statistical significance level that a predictor variable must meet to be included in the model. E.g. if 'sle = 0.05', a predictor with a P-value less than 0.05 will 'enter' the model. Default is 0.15.
#' 
#' @param sls (numeric) Significance Level to Stay. Similar to 'sle', 'sls' is the statistical significance level that a predictor variable must meet to 'stay' in the model. E.g. if 'sls = 0.1', a predictor that was previously included in the model but whose P-value is now greater than 0.1 will be removed.
#' 
#' @param tolerance (numeric)  A statistical measure used to assess multicollinearity in a multiple regression model. It is calculated as the proportion of the variance in a predictor variable that is not accounted for by the other predictor variables in the model. Default is 1e-07.
#' 
#' @param weight (numeric) A numeric vector specifying the coefficients assigned to the predictor variables. The magnitude of the weight reflects the degree to which each predictor variable contributes to the prediction of the response variable. The range of weight should be from 0 to 1. Values greater than 1 will be coerced to 1, and values less than 0 will be coerced to 0. Default is NULL, which means that all weight are set equal.
#' 
#' @param test_method_linear (character) Test method for multivariate linear regression analysis, choose from 'Pillai', 'Wilks', 'Hotelling-Lawley', 'Roy'. Default is 'Pillai'. For univariate regression, 'F-test' will be used. 
#' 
#' @param test_method_glm (character) Test method for logit, Poisson, Gamma, and negative binomial regression analysis, choose from 'Rao', 'LRT'. Default is 'Rao'. Only "Rao" is available for strategy = 'subset'.
#' 
#' @param test_method_cox (character) Test method for cox regression analysis, choose from 'efron', 'breslow', 'exact'. Default is 'efron'.
#' 
#' @param best_n (numeric(integer)) The number of models to be retained in the process output. Default is 3, indicating that only the top 3 best models with the same number of variables are displayed. If all models are displayed, set it to Inf.
#'  
#' @param num_digits (numeric(integer)) The number of digits to keep when rounding the results. Default is 6.
#' 
#' @references
#' 
#' Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable strategy in multivariable regression using sas/iml. , 07(i12).
#' 
#' Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.
#' 
#' Dharmawansa, P. , Nadler, B. , & Shwartz, O. . (2014). Roy's largest root under rank-one alternatives:the complex valued case and applications. Statistics.
#' 
#' Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.
#' 
#' Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.
#' 
#' Hocking, R. R. (1976). A biometrics invited paper. the analysis and strategy of variables in linear regression. Biometrics, 32(1), 1-49.
#' 
#' Hurvich, C. M., & Tsai, C. (1989). Regression and time series model strategy in small samples. Biometrika, 76(2), 297-307.
#' 
#' Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.
#' 
#' Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.
#' 
#' Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.
#' 
#' Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.
#' 
#' Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model strategy. Regression and time series model strategy /. World Scientific.
#' 
#' Pillai, K. . (1955). Some new test criteria in multivariate analysis. The Annals of Mathematical Statistics, 26(1), 117-121.
#' 
#' R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable strategy in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.
#' 
#' Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.
#' 
#' Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
#' 
#' @author Junhui Li, Kai Hu, Xiaohuan Lu
#' 
#' @return A list containing multiple tables will be returned.
#' \itemize{
#' \item Summary of arguments for model selection: Arguments used in the stepwise function, either default or user-supplied values.
#' \item Summary of variables in dataset: Variable names, types, and classes in dataset.
#' \item Summary of selection process under xxx(strategy) with xxx(metric): Overview of the variable selection process under specified strategy and metric.
#' \item Summary of coefficients for the selected model with xxx(dependent variable) under xxx(strategy) and xxx(metric): Coefficients for the selected models under specified strategy with metric. Please note that this table will not be generated for the strategy 'subset' when using the metric 'SL'.
#' }
#' 
#' @examples
#' ## perform multivariate linear stepwise regression with 'bidirection' 
#' ## strategy and 'AIC' stop rule, excluding intercept.
#' data(mtcars)
#' mtcars$yes <- mtcars$wt
#' formula <- cbind(mpg,drat) ~ . + 0
#' stepwise(formula = formula,
#'          data = mtcars,
#'          type = "linear",
#'          strategy = "bidirection",
#'          metric = "AIC")
#' ## perform linear stepwise regression with 'bidirection' strategy and 
#' ## "AIC","SBC","SL","AICc","BIC", and "HQ" stop rule.
#' formula <- mpg ~ . + 1
#' stepwise(formula = formula,
#'          data = mtcars,
#'          type = "linear",
#'          strategy = c("forward","bidirection"),
#'          metric = c("AIC","SBC","SL","AICc","BIC","HQ"))
#'
#' ## perform logit stepwise regression with 'forward' strategy and significance
#' ## level as stop rule.
#' data(remission)
#' formula <- remiss ~ .
#' stepwise(formula = formula,
#'          data = remission,
#'          type = "logit",
#'          strategy = "forward",
#'          metric = "SL",
#'          sle=0.05,
#'          sls=0.05)
#' @keywords stepwise regression
#' 
#' @importFrom survival coxph
#' @importFrom stringr str_replace
#' @importFrom utils combn
#' @importFrom dplyr %>% mutate_if mutate
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms deviance df.residual formula model.frame
#' @importFrom MASS glm.nb
#' 
#' @export

stepwise <- function(formula,
                     data,
                     type = c("linear", "logit", "cox", "poisson", "gamma", "negbin"),
                     include = NULL,
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                     sle = 0.15,
                     sls = 0.15,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     test_method_glm = c("Rao", "LRT"),
                     test_method_cox = c("efron", "breslow", "exact"),
                     tolerance = 1e-7,
                     weight = NULL,
                     best_n = 3,
                     num_digits = 6) {

  type <- match.arg(type)
  strategy <- match_multiple_args(strategy, c("forward", "backward", "bidirection", "subset"))
  metric <- match_multiple_args(metric, c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"))
  
  test_method_linear <- match.arg(test_method_linear)
  test_method_glm <- match.arg(test_method_glm)
  test_method_cox <- match.arg(test_method_cox)
  
  x_name_orig <- getXname(formula, data)
  y_name <- getYname(formula, data)
  intercept <- getIntercept(formula, data, type = type) # char type
  merged_include <- getMergedVar(include)
  model_raw <- getModel(data, type = type, intercept = intercept, x_name_orig, y_name, weight = weight, method = test_method_cox)
  if(type != "cox") {
    y_df <- as.matrix(model_raw$model[, y_name])
    n_y <- ncol(y_df)
  }else{
    n_y <- 1
  }
  sigma_value <- getSigmaFullModel(model_raw, type, n_y)
  
  validateUtils(formula = formula, data = data, type = type, include = include, strategy = strategy, metric = metric, sle = sle, sls = sls, sigma_value = sigma_value, test_method_linear = test_method_linear, test_method_glm = test_method_glm, test_method_cox = test_method_cox, tolerance = tolerance, weight = weight, best_n = best_n, n_y = n_y)
  test_method <- getTestMethod(data, model_raw, type, metric, n_y, test_method_linear, test_method_glm, test_method_cox)
  
  multico_x <- getMulticolX(data, x_name_orig, tolerance)
  merged_multico_x <- getMergedVar(multico_x)
  x_name <- setdiff(x_name_orig, multico_x)
  
  result <- list()
  ## table1
  table1_para_value <- getTable1SummaryOfParameters(data, type, x_name_orig, y_name, merged_multico_x, merged_include, strategy, metric, sle, sls, test_method, tolerance, intercept)
  result$'Summary of arguments for model selection' <- table1_para_value
  
  ## table2
  table2_class_table <- getTable2TypeOfVariables(model_raw)
  result$'Summary of variables in dataset' <- table2_class_table
  
  ## table3
  table3_list <- getTable3ProcessSummary(data, type, strategy, metric, sle, sls, weight, x_name, y_name, intercept, include, best_n, test_method, sigma_value, num_digits)
  table3 <- table3_list$table3
  vote_df <- table3_list$vote_df
  pic_df <- table3_list$pic_df
  final_variable <- table3_list$final_variable
  result <- append(result,table3)
  
  table4 <- getTable4CoefModel(type = type, strategy, metric, intercept, include, final_variable, y_name, n_y, data, weight, test_method_cox, num_digits)
  result <- append(result,table4)
  
  if(!is.null(vote_df)) {
    colnames(vote_df) <- c("model", "combination")
  }
  result[['Vote_df']] <- vote_df
  result[['Detail_selection_summary']] <-  pic_df
  class(result) <- c("StepReg", "list", strategy, type)
  return(result)
}
