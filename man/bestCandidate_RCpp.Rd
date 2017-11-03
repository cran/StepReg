\name{bestCandidate_RCpp}
\alias{bestCandidate_RCpp}
\title{
	Obtain one best candidate variable
}
\description{
	Get best candidate variable with forward or backward direction in only one step
}
\usage{
	bestCandidate_RCpp(findIn, p, n, sigma, tolerance, Ftrace, criteria, Y,X1, X0, k)
}

\arguments{
  \item{findIn}{
	Logical value, if FALSE then add independent variable to regression model, otherwise remove independent variable from regression model
}
  \item{p}{
	The number of independent variable entered in regression
}
  \item{n}{
	The sample size
}
  \item{sigma}{
	Pure error variance from full regressoin model for Bayesian information criterion(BIC)
}
  \item{tolerance}{
	Tolerance value for multicollinearity
}
  \item{Ftrace}{
	Statistic of multivariate regression including Wilks` lambda, Pillai trace and Hotelling-lawley trace
}
  \item{criteria}{
	Information criterion including AIC, AICc, BIC, SBC, HQ, HQc and SL
}
  \item{Y}{
	Data set for dependent variable
}
  \item{X1}{
	Data set for independent variables not in regression model
}
  \item{X0}{
	Data set for independent variables entered in regression model
}
  \item{k}{
	Forces the first k effects entered in regression model, and the selection methods are performed on the other effects in the data set
}
}
\details{
	This function can compute probability value or information criteria statistics with multivariate and univariate regression using least square method
}
\value{
	\item{PIc }{P value or Information Criteria statistic value}
	\item{seq }{Pointer for independent variable enter or eliminate }
	\item{SSE }{Maximum or minimum of SSE }
	\item{RkCh }{Rank changed or not }
}
\references{
Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).

Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.

Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.

Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.

Hurvich, C. M., & Tsai, C. (1989). Regression and time series model selection in small samples. Biometrika, 76(2), 297-307.

Judge, & GeorgeG. (1985). The Theory and practice of econometrics /-2nd ed. The Theory and practice of econometrics /. Wiley.

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
\keyword{stepwise regression}
