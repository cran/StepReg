\name{scoretest}
\alias{scoretest}
\title{
Compute score test statistics and probability value for generalized linear model
}
\description{
This function can compute score test statistics and probability value for linear model by adding an independent variable.
}
\usage{
scoretest(model, x)
}

\arguments{
  \item{model}{
  Generalized linear model object
}
  \item{x}{
  Vector or matrix of independent variable
}
}
\references{
Lovison, G. . (2005). On rao score and pearson x2 statistics in generalized linear models. Statistical Papers, 46(4), 555-574.

Robert Gilchrist. (1982). Glim 82: proceedings of the international conference on generalised linear models. Lecture Notes in Statistics, 14(9), 3008-11.

Smyth, G. K. . (2003). Pearson's goodness of fit statistic as a score test statistic. Lecture Notes-Monograph Series, 40, 115-126.
}
\author{
Junhui Li
}
\examples{
  set.seed(1)
  yd <- data.frame(sample(c(0,1),30,replace=TRUE))
  colnames(yd) <- "remiss"
  set.seed(4)
  xd <- data.frame(matrix(c(round(rnorm(100,0,2),2),round(rnorm(140,2,4),2),
  sample(c(1,0),30,replace=TRUE),sample(1:80,30,replace=TRUE)),30,10))
  colnames(xd) <- c(paste("X",1:8,sep=""),"gender","age")
  yx <- cbind(yd,xd)
  y <- "remiss"
  comVar <- paste("X",1:3,sep="")
  fm <- paste(y,"~",paste0(comVar,collapse = "+"),sep="")
  model <- glm(fm,yx,family="binomial")
  scoretest(model, as.matrix(yx[,4]))
}
\keyword{score test}