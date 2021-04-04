\name{bestsubset}
\alias{bestsubset}
\title{
Best subset selection
}
\description{
This function uses information criteria to find a specified number of best models containing one, two, or three variables, and so on, up to the single model containing all of the explanatory variables.
}
\usage{
bestsubset(data, y, exclude = NULL, include = NULL, Class = NULL, 
weights = c(rep(1, nrow(data))),select = "SBC", tolerance = 1e-07, best = 5)
}

\arguments{
  \item{data}{
  Data set including dependent and independent variables to be analyzed
}
  \item{y}{
  A character or numeric vector indicating the subset of dependent variables
}
  \item{exclude}{
  A character or numeric vector indicating the subset of independent variables removed from datasets
}
  \item{include}{
  Forces the effects vector listed in the data to be included in all models. The selection methods are performed on the other effects in the data set
}
  \item{Class}{
  Class effect variable
}
  \item{weights}{
  The weights names numeric vector to provide a weight for each observation in the input data set. And note that weights should be ranged from 0 to 1, while negative numbers are forcibly converted to 0, and numbers greater than 1 are forcibly converted to 1. If you do not specify a weight vector, each observation has a default weight of 1.
}
  \item{select}{
  Specifies the criterion that uses to calculate all models including Akaike Information Criterion(AIC), the Corrected form of Akaike Information Criterion(AICc),Bayesian Information Criterion(BIC),Schwarz criterion(SBC),Hannan and Quinn Information Criterion(HQ), R-square statistic(Rsq), adjusted R-square statistic(adjRsq) and Mallows Cp statistic(CP)
}
  \item{tolerance}{
  Tolerance value for multicollinearity, default is 1e-7 
}
  \item{best}{
  Controls the number of models displayed in the output, default is 5
}
}
\references{
Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).

Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.

Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.

Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.

Hocking, R. R. (1976). A biometrics invited paper. the analysis and selection of variables in linear regression. Biometrics, 32(1), 1-49.

Hurvich, C. M., & Tsai, C. (1989). Regression and time series model selection in small samples. Biometrika, 76(2), 297-307.

Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.

Mallows, C. L. (1973). Some comments on cp. Technometrics, 15(4), 661-676.

Mardia, K. V., Kent, J. T., & Bibby, J. M. (1979). Multivariate analysis. Mathematical Gazette, 37(1), 123-131.

Mckeon, J. J. (1974). F approximations to the distribution of hotelling's t20. Biometrika, 61(2), 381-383.

Mcquarrie, A. D. R., & Tsai, C. L. (1998). Regression and Time Series Model Selection. Regression and time series model selection /. World Scientific.

Pillai, K. C. S. (2006). Pillai's Trace. Encyclopedia of Statistical Sciences. John Wiley & Sons, Inc.

R.S. Sparks, W. Zucchini, & D. Coutsourides. (1985). On variable selection in multivariate regression. Communication in Statistics- Theory and Methods, 14(7), 1569-1587.

Sawa, T. (1978). Information criteria for discriminating among alternative regression models. Econometrica, 46(6), 1273-1291.

Schwarz, G. (1978). Estimating the dimension of a model. Annals of Statistics, 6(2), pags. 15-18.
}
\author{
Junhui Li
}
\examples{
set.seed(4)
dfY <- data.frame(matrix(c(rnorm(20,0,2),c(rep(1,10),rep(2,10)),rnorm(20,2,3)),20,3))
colnames(dfY) <- paste("Y",1:3,sep="")
dfX <- data.frame(matrix(c(rnorm(100,0,2),rnorm(100,2,1)),20,10))
colnames(dfX) <- paste("X",1:10,sep="")
yx <- cbind(dfY,dfX)
bestsubset(yx,y="Y1",exclude="Y3",include="Y2",Class="Y2",
weights=c(rep(0.5,2),rep(1,18)),select="SBC",tolerance=1e-7,best=5)
}
\keyword{best subset selection}