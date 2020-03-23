scoretest <- function(model, x){
  w <- model$weights
  r <- model$residuals
  #remove x/r/w with nagtive weights
  if (any(w <= 0)) {
    r <- r[w > 0]
    x <- x[w > 0]
    w <- w[w > 0]
  }
  w_sqrt <- sqrt(w)
  x_qr_resid <- qr.resid(model$qr, w_sqrt * x)
  w_sqrt_r <- w_sqrt * r
  z <- colSums(as.matrix(x_qr_resid * w_sqrt_r))/sqrt(colSums(as.matrix(x_qr_resid * x_qr_resid)))
  score <- z^2
  p <- pchisq(score, df=1, lower.tail=FALSE)
  result <- list(score,p)
  names(result) <- c("score","pvalue")
  return(result)
}
