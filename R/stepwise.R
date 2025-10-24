#' Stepwise Regression Model Selection
#'
#' Performs stepwise regression model selection using various strategies and selection criteria.
#' Supports multiple regression types including linear, logistic, Cox, Poisson, and Gamma regression.
#'
#' @param formula A formula object specifying the model structure:
#'   \itemize{
#'     \item Response variable(s) on left side of ~
#'     \item Predictor variable(s) on right side of ~
#'     \item Use + to separate multiple predictors
#'     \item Use * for main effect and interaction terms
#'     \item Use : for continuous-nested-within-class variable, make sure class variable is a factor variable, e.g. X:A or A:X means a continuous variable X nested within a factor variable A
#'     \item Use . to include all variables
#'     \item Use cbind() for multiple responses
#'     \item Use 0 or -1 to exclude intercept
#'     \item Use strata() to include strata variable for Cox regression
#'   }
#'
#' @param data A data frame containing the variables in the model
#'
#' @param type The type of regression model to fit:
#'   \itemize{
#'     \item "linear" - Linear regression (default)
#'     \item "logit" - Logistic regression
#'     \item "poisson" - Poisson regression
#'     \item "cox" - Cox proportional hazards regression
#'     \item "gamma" - Gamma regression
#'     \item "negbin" - Negative binomial regression
#'   }
#'
#' @param strategy The model selection strategy:
#'   \itemize{
#'     \item "forward" - Forward selection (default)
#'     \item "backward" - Backward elimination
#'     \item "bidirection" - Bidirectional elimination
#'     \item "subset" - Best subset selection
#'   }
#'
#' @param metric The model selection criterion:
#'   \itemize{
#'     \item "AIC" - Akaike Information Criterion (default)
#'     \item "AICc" - Corrected AIC
#'     \item "BIC" - Bayesian Information Criterion
#'     \item "CP" - Mallows' Cp
#'     \item "HQ" - Hannan-Quinn criterion
#'     \item "adjRsq" - Adjusted R-squared
#'     \item "SL" - Significance Level
#'     \item "SBC" - Schwarz Bayesian Criterion
#'     \item "IC(3/2)" - Information Criterion with penalty 3/2
#'     \item "IC(1)" - Information Criterion with penalty 1
#'   }
#'
#' @param sle Significance Level to Enter (default: 0.15). A predictor must have p-value < sle to enter the model.
#'
#' @param sls Significance Level to Stay (default: 0.15). A predictor must have p-value < sls to remain in the model.
#'
#' @param include Character vector of predictor variables that must be included in all models.
#'
#' @param tolerance Threshold for detecting multicollinearity (default: 1e-07). Lower values are more strict.
#'
#' @param weight Optional numeric vector of observation weights. Values are coerced to [0,1].
#'
#' @param test_method_linear Test method for multivariate linear regression:
#'   \itemize{
#'     \item "Pillai" (default)
#'     \item "Wilks"
#'     \item "Hotelling-Lawley"
#'     \item "Roy"
#'   }
#'   For univariate regression, F-test is used.
#' 
#' @param test_method_glm Test method for GLM models:
#'   \itemize{
#'     \item "Rao" (default)
#'     \item "LRT"
#'   }
#'   Only "Rao" available for subset strategy.
#'
#' @param test_method_cox Test method for Cox regression:
#'   \itemize{
#'     \item "efron" (default)
#'     \item "breslow"
#'     \item "exact"
#'   }
#'
#' @param best_n Maximum number of models to retain for each variable count (default: 3)
#' 
#' @param test_ratio Proportion of the dataset allocated for testing (e.g., 0.3, which means 30\% of the dataset is used for testing), with the remainder reserved for training, enabling train-test validation.
#' 
#' @param feature_ratio Proportion of candidate features sampled uniformly at random during forward selection (default = 1). This randomized selection helps identify the best variables while reducing the risk of overfitting, and is only valid when strategy is "forward".
#' 
#' @param seed Seed for random number generation (default: 123), this is only valid when test_ratio or feature_ratio is specified.
#' 
#' @param num_digits Number of decimal places to round results (default: 6)
#'
#' @return A StepReg class object, which is a structured list containing both the input specifications and the outcomes of the stepwise regression analysis. The key components of this object are detailed below, providing a comprehensive framework for model exploration and validation.
#'   \itemize{
#'     \item \code{argument} A data.frame containing the user-specified settings and parameters used in the analysis, including the initial formula, regression type, selection strategy, chosen metrics, significance levels (sle/sls), tolerance threshold, test method, and other control parameters.
#'     \item \code{variable} A data.frame containing information about all variables in the model, including variable names, data types (numeric, factor, etc.), and their roles (Dependent/Independent) in the model.
#'     \item \code{performance} A data.frame providing detailed performance metrics for the selected models across different strategies and metrics. For both training and test datasets (when test_ratio < 1), the output includes model-specific performance indicators:
#'       \itemize{
#'         \item \strong{For linear, poisson, gamma, and negative binomial regression:}
#'           \itemize{
#'             \item \code{adj_r2_train/adj_r2_test}: Adjusted R-squared measures the proportion of variance explained by the model, adjusted for the number of predictors. Values range from 0 to 1, with higher values indicating better model fit. A good model should have high adjusted R-squared on both training and test data, with minimal difference between them. Large differences suggest overfitting.
#'             \item \code{mse_train/mse_test}: Mean Squared Error measures the average squared difference between predicted and actual values. Lower values indicate better model performance. The test MSE should be close to training MSE; significantly higher test MSE suggests overfitting.
#'             \item \code{mae_train/mae_test}: Mean Absolute Error measures the average absolute difference between predicted and actual values. Lower values indicate better model performance. Like MSE, test MAE should be close to training MAE to avoid overfitting.
#'           }
#'         \item \strong{For logistic regression:}
#'           \itemize{
#'             \item \code{accuracy_train/accuracy_test}: Accuracy measures the proportion of correct predictions (true positives + true negatives) / total predictions. Values range from 0 to 1, with higher values indicating better classification performance. Test accuracy should be close to training accuracy; large differences suggest overfitting.
#'             \item \code{auc_train/auc_test}: Area Under the Curve measures the model's ability to distinguish between classes. Values range from 0.5 (random) to 1.0 (perfect discrimination). AUC > 0.7 is considered acceptable, > 0.8 is good, > 0.9 is excellent. Test AUC should be close to training AUC to avoid overfitting.
#'             \item \code{log_loss_train/log_loss_test}: Log Loss (logarithmic loss) penalizes confident wrong predictions more heavily. Lower values indicate better model performance. Values close to 0 are ideal. Test log loss should be close to training log loss; higher test log loss suggests overfitting.
#'           }
#'         \item \strong{For Cox regression:}
#'           \itemize{
#'             \item \code{c-index_train/c-index_test}: Concordance Index (C-index) measures the model's ability to correctly rank survival times. Values range from 0.5 (random) to 1.0 (perfect ranking). C-index > 0.7 is considered acceptable, > 0.8 is good, > 0.9 is excellent. Test C-index should be close to training C-index to avoid overfitting.
#'             \item \code{auc_hc}: Harrell's C-index for time-dependent AUC, measuring discrimination at specific time points. Higher values indicate better discrimination ability.
#'             \item \code{auc_uno}: Uno's C-index for time-dependent AUC, providing an alternative measure of discrimination that may be more robust to censoring patterns.
#'             \item \code{auc_sh}: Schemper and Henderson's C-index for time-dependent AUC, offering another perspective on model discrimination performance.
#'           }
#'       }
#'     \item \code{overview} A nested list organized by strategy and metric, containing step-by-step summaries of the model-building process. Each element shows which variables were entered or removed at each step along with the corresponding metric values (e.g., AIC, BIC, SBC).
#'     \item \code{detail} A nested list organized by strategy and metric, providing granular information about each candidate step. This includes which variables were tested, their evaluation statistics, p-values, and whether they were ultimately selected or rejected.
#'     \item \code{fitted model object within the strategy-specific list} A nested list object organized with a first layer representing the selection strategy (e.g., forward, backward, bidirection, subset) and a second layer representing the metric (e.g., AIC, BIC, SBC). For each strategy-metric combination, the function returns fitted model objects that can be further analyzed using S3 generic functions such as \code{summary()}, \code{anova()}, or \code{coefficients()}. These functions adapt to the model type (e.g., \code{coxph}, \code{lm}, \code{glm}) through call-specific methods. Specific statistics can be directly retrieved using the \code{$} operator, such as \code{result$forward$AIC$coefficients}. The level of detail in these analyses depends on the model type: the \CRANpkg{survival} package enriches \code{coxph} objects with detailed statistics including hazard ratios, standard errors, z-statistics, p-values, and likelihood ratio tests, while base R functions like \code{lm} and \code{glm} offer basic output with coefficients by default, requiring \code{summary()} or \code{anova()} to reveal standard errors, t-values, p-values, and R-squared values.
#'   }
#'
#' @examples
#' # Multivariate linear regression with bidirectional selection
#' data(mtcars)
#' formula <- cbind(mpg, drat) ~ . + 0
#' result1 <- stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = "bidirection",
#'   metric = "AIC"
#' )
#' 
#' summary(result1$bidirection$AIC)
#' anova(result1$bidirection$AIC)
#' coefficients(result1$bidirection$AIC)
#'
#' # Linear regression with multiple strategies and metrics
#' formula <- mpg ~ . + 1
#' result2 <- stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = c("forward", "bidirection"),
#'   metric = c("AIC", "SBC", "SL", "AICc", "BIC", "HQ")
#' )
#' 
#' summary(result2$forward$AIC)
#' anova(result2$forward$AIC)
#' coefficients(result2$forward$AIC)
#' 
#' # Logistic regression with significance level criteria
#' data(remission)
#' formula <- remiss ~ .
#' result3 <- stepwise(
#'   formula = formula,
#'   data = remission,
#'   type = "logit",
#'   strategy = "forward",
#'   metric = "SL",
#'   sle = 0.05,
#'   sls = 0.05
#' )
#' 
#' summary(result3$forward$SL)
#' anova(result3$forward$SL)
#' coefficients(result3$forward$SL)
#' 
#' # Linear regression with continuous-nested-within-class effects
#' mtcars$am <- factor(mtcars$am)
#' formula <- mpg ~ am + cyl + wt:am + disp:am + hp:am
#' result4 <- stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = "bidirection",
#'   metric = "AIC"
#' )
#' 
#' summary(result4$bidirection$AIC)
#' anova(result4$bidirection$AIC)
#' coefficients(result4$bidirection$AIC)
#' 
#' @references
#' \itemize{
#'   \item Alsubaihi et al. (2002) Variable strategy in multivariable regression using sas/iml
#'   \item Darlington (1968) Multiple regression in psychological research and practice
#'   \item Dharmawansa et al. (2014) Roy's largest root under rank-one alternatives
#'   \item Hannan & Quinn (1979) The determination of the order of an autoregression
#'   \item Hotelling (1992) The Generalization of Student's Ratio
#'   \item Hocking (1976) The analysis and strategy of variables in linear regression
#'   \item Hurvich & Tsai (1989) Regression and time series model strategy in small samples
#'   \item Judge (1985) The Theory and practice of econometrics
#'   \item Mallows (1973) Some comments on cp
#'   \item Mardia et al. (1979) Multivariate analysis
#'   \item Mckeon (1974) F approximations to the distribution of hotelling's t20
#'   \item Mcquarrie & Tsai (1998) Regression and Time Series Model strategy
#'   \item Pillai (1955) Some new test criteria in multivariate analysis
#'   \item Sparks et al. (1985) On variable strategy in multivariate regression
#'   \item Sawa (1978) Information criteria for discriminating among alternative regression models
#'   \item Schwarz (1978) Estimating the dimension of a model
#' }
#'
#' @author Junhui Li, Kai Hu, Xiaohuan Lu
#'
#' @keywords stepwise regression
#'
#' @importFrom survival coxph concordance Surv
#' @importFrom survAUC AUC.uno AUC.sh AUC.hc
#' @importFrom utils combn
#' @importFrom pROC auc roc
#' @importFrom stats anova coef glm lm logLik pf reformulate sigma terms deviance df.residual formula model.frame predict cor
#' @importFrom MASS glm.nb
#'
#' @export

stepwise <- function(formula,
                     data,
                     type = c("linear", "logit", "cox", "poisson", "gamma", "negbin"),
                     strategy = c("forward", "backward", "bidirection", "subset"),
                     metric = c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"),
                     sle = 0.15,
                     sls = 0.15,
                     include = NULL,
                     test_method_linear = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"),
                     test_method_glm = c("Rao", "LRT"),
                     test_method_cox = c("efron", "breslow", "exact"),
                     tolerance = 1e-7,
                     weight = NULL,
                     best_n = 3,
                     test_ratio = 0,
                     feature_ratio = 1,
                     seed = 123,
                     num_digits = 6) {
  type <- match.arg(type)
  strategy <- match_multiple_args(strategy, c("forward", "backward", "bidirection", "subset"))
  metric <- match_multiple_args(metric, c("AIC", "AICc", "BIC", "CP", "HQ", "adjRsq", "SL", "SBC", "IC(3/2)", "IC(1)"))
  
  test_method_linear <- match.arg(test_method_linear)
  test_method_glm <- match.arg(test_method_glm)
  test_method_cox <- match.arg(test_method_cox)

  if(test_ratio >= 1 | test_ratio < 0) {
    stop("test_ratio must be between 0 and 1")
  }
  if(feature_ratio > 1 | feature_ratio <= 0) {
    stop("feature_ratio must be between 0 and 1")
  }
  if(feature_ratio < 1) {
    set.seed(seed)
  }
  data_train <- data
  data_test <- NULL
  if(test_ratio > 0) {
    set.seed(seed)
    data_test <- data[sample(1:nrow(data), size = round(test_ratio * nrow(data), 0)), ]
    data_train <- data[sample(1:nrow(data), size = round((1 - test_ratio) * nrow(data), 0)), ]
  }

  x_name_orig <- getXname(formula, data_train)
  y_name <- getYname(formula, data_train)
  intercept <- getIntercept(formula, data_train, type = type) # char type
  merged_include <- getMergedVar(include)
  model_raw <- getModel(data_train, type = type, intercept = intercept, x_name_orig, y_name, weight = weight, method = test_method_cox)
  if(type != "cox") {
    y_df <- as.matrix(model_raw$model[, y_name])
    n_y <- ncol(y_df)
  }else{
    n_y <- 1
  }
  sigma_value <- getSigmaFullModel(model_raw, type, n_y)
  validateUtils(formula = formula, data = data_train, type = type, include = include, strategy = strategy, metric = metric, sle = sle, sls = sls, sigma_value = sigma_value, test_method_linear = test_method_linear, test_method_glm = test_method_glm, test_method_cox = test_method_cox, tolerance = tolerance, weight = weight, best_n = best_n, n_y = n_y)
  test_method <- getTestMethod(data_train, model_raw, type, metric, n_y, test_method_linear, test_method_glm, test_method_cox)
  
  multico_x <- getMulticolX(data_train, x_name_orig, tolerance)
  merged_multico_x <- getMergedVar(multico_x)
  x_name <- setdiff(x_name_orig, multico_x)
  
  result <- list()
  ## table1
  table1_para_value <- getTable1SummaryOfParameters(formula, data_train, type, x_name_orig, y_name, merged_multico_x, merged_include, strategy, metric, sle, sls, test_method, tolerance, intercept, test_ratio, feature_ratio, seed)
  result$argument <- table1_para_value
  
  ## table2
  table2_class_table <- getTable2TypeOfVariables(model_raw)
  result$variable <- table2_class_table
  
  ## table3
  table3_process <- getTable3ProcessSummary(data_train=data_train, data_test=data_test, type, strategy, metric, sle, sls, weight, x_name, y_name, intercept, include, best_n, test_method, sigma_value, num_digits, feature_ratio)
  x_final_model_metric <- table3_process$final_variable
  result <- append(result,table3_process[which(names(table3_process) != "final_variable")])
  
  ## table4
  table4_model <- getTable4ModelCall(type, intercept, include, x_final_model_metric, y_name, n_y, data_train, weight, test_method, num_digits)
  result <- append(result,table4_model)

  class(result) <- c("StepReg","list")
  attr(result, "nonhidden") <- strategy
  return(result)
}
