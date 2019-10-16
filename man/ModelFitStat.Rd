\name{ModelFitStat}
\alias{ModelFitStat}
\title{
	calculate model fit statistics
}
\description{
	calculate model fit statistics adjRsq, AIC, AICc, BIC, CP, HQ, HQc, Rsq and SBC 
}
\usage{
ModelFitStat(Stattype,SSE,SST,n,nY,p,sigmaVal)
}
\arguments{
  \item{Stattype}{
  Model fit statistics adjRsq, AIC, AICc, BIC, CP, HQ, HQc, Rsq and SBC
}
  \item{SSE}{
  Sum of squares of error
}
  \item{SST}{
  Total sum of squares corrected for the mean for the dependent variable
}
  \item{n}{
  Number of observation
}
  \item{nY}{
  Number of dependent variable
}
  \item{p}{
  Number of independent variable in the model including the intercept
}
  \item{sigmaVal}{
  Estimate of pure error variance from fitting the full model
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
Junhui Li Xiaohuan Lu
}
\keyword{model fit statistics}