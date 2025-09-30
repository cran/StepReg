#' Model Performance Summary Across Different Selection Strategies
#'
#' Creates a summary table showing the performance of the selected models by different combinations of 
#' stepwise regression strategies and selection metrics.
#'
#' @param x A list object returned by the \code{stepwise()} function
#' 
#' @param ... Additional arguments (currently not used)
#'
#' @return A data frame where:
#'   \item{model}{The formula of each selected model}
#'   \item{strategy:metric}{Columns for each combination of strategy and metric used}
#'   \itemize{
#'     \item \strong{For linear, poisson, gamma, and negative binomial regression:}
#'       \itemize{
#'         \item \code{adj_r2_train/adj_r2_test}: Adjusted R-squared measures the proportion of variance explained by the model, adjusted for the number of predictors. Values range from 0 to 1, with higher values indicating better model fit. A good model should have high adjusted R-squared on both training and test data, with minimal difference between them. Large differences suggest overfitting.
#'         \item \code{mse_train/mse_test}: Mean Squared Error measures the average squared difference between predicted and actual values. Lower values indicate better model performance. The test MSE should be close to training MSE; significantly higher test MSE suggests overfitting.
#'         \item \code{mae_train/mae_test}: Mean Absolute Error measures the average absolute difference between predicted and actual values. Lower values indicate better model performance. Like MSE, test MAE should be close to training MAE to avoid overfitting.
#'       }
#'     \item \strong{For logistic regression:}
#'       \itemize{
#'         \item \code{accuracy_train/accuracy_test}: Accuracy measures the proportion of correct predictions (true positives + true negatives) / total predictions. Values range from 0 to 1, with higher values indicating better classification performance. Test accuracy should be close to training accuracy; large differences suggest overfitting.
#'         \item \code{auc_train/auc_test}: Area Under the Curve measures the model's ability to distinguish between classes. Values range from 0.5 (random) to 1.0 (perfect discrimination). AUC > 0.7 is considered acceptable, > 0.8 is good, > 0.9 is excellent. Test AUC should be close to training AUC to avoid overfitting.
#'         \item \code{log_loss_train/log_loss_test}: Log Loss (logarithmic loss) penalizes confident wrong predictions more heavily. Lower values indicate better model performance. Values close to 0 are ideal. Test log loss should be close to training log loss; higher test log loss suggests overfitting.
#'       }
#'     \item \strong{For Cox regression:}
#'       \itemize{
#'         \item \code{c-index_train/c-index_test}: Concordance Index (C-index) measures the model's ability to correctly rank survival times. Values range from 0.5 (random) to 1.0 (perfect ranking). C-index > 0.7 is considered acceptable, > 0.8 is good, > 0.9 is excellent. Test C-index should be close to training C-index to avoid overfitting.
#'         \item \code{auc_hc}: Harrell's C-index for time-dependent AUC, measuring discrimination at specific time points. Higher values indicate better discrimination ability.
#'         \item \code{auc_uno}: Uno's C-index for time-dependent AUC, providing an alternative measure of discrimination that may be more robust to censoring patterns.
#'         \item \code{auc_sh}: Schemper and Henderson's C-index for time-dependent AUC, offering another perspective on model discrimination performance.
#'       }
#'   }
#'   
  #' Each cell contains the performance of the model by the corresponding 
  #' strategy-metric combination. For the subset strategy with Information Criteria (IC), 
  #' only the single best model across all variable numbers is shown. This does not apply 
  #' to Significance Level (SL) since F/Rao statistics can only be compared between models 
  #' with the same number of variables.
#'
#' @examples
#' # Load example data
#' data(mtcars)
#' 
#' # Run stepwise regression with multiple strategies and metrics
#' formula <- mpg ~ .
#' result <- stepwise(
#'   formula = formula,
#'   data = mtcars,
#'   type = "linear",
#'   strategy = c("forward", "backward", "bidirection"),
#'   metric = c("AIC", "BIC")
#' )
#' 
#' # Get performance summary
#' performance(result)
#'
#' @export
#' 
performance <- function(x, ...){
  performance_df <- x[["performance"]]
  if("response" %in% colnames(performance_df)) {
    performance_df$model <- paste(performance_df$response, performance_df$model, sep=":")
  }
  performance_mat <- as.data.frame(performance_df)
  
  uniq_model <- unique(performance_mat[,1])
  performance_df <- as.data.frame(matrix("",length(uniq_model),ncol(performance_mat)))
  performance_df[,1] <- uniq_model
  for(i in 1:length(uniq_model)) {
    performance_df[i,2] <- paste(performance_mat[performance_mat[,"model"] %in% uniq_model[i],2], collapse = "; ")
    performance_df[i,3:ncol(performance_mat)] <- performance_mat[which(performance_mat[,"model"] %in% uniq_model[i])[1],3:ncol(performance_mat)]
  }
  colnames(performance_df) <- colnames(performance_mat)
  if("response" %in% colnames(performance_df)) {
    performance_df$model <- sub(".*cbind\\(", "cbind(", performance_df$model)
    last_col <- ncol(performance_df) - 1
  } else {
    last_col <- ncol(performance_df)
  }
  numeric_cols <- colnames(performance_df)[3:last_col]
  
  performance_df[, numeric_cols] <- as.data.frame(
    lapply(performance_df[, numeric_cols], as.numeric)
  )
  performance_df[, numeric_cols] <- round(performance_df[, numeric_cols], digits = 4)
  #class(vote_reform) <- c("StepReg")
  return(performance_df)
}