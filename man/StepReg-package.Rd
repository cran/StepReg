\name{StepReg-package}
\alias{StepReg-package}
\alias{StepReg}
\docType{package}
\title{
  Stepwise Regression Analysis
}
\description{
  Stepwise regression analysis for variable selection can be used to get the best candidate final regression model with the forward selection, backward elimination and bidirectional elimination approaches. Best subset selection fit a separate least squares regression for each possible combination of all predictors. Both the above two procedures in this package can use weighted data to get best regression model in univariate regression and multivariate regression analysis(Alsubaihi, A. A., (2002) <doi:10.18637/jss.v007.i12>). And continuous variables nested within class effect is also considered in both two procedures. Also stepwise logistic regression in this package can performed with binary dependent variable(Agresti, A. (1984) <doi:10.1002/9780470594001> and Agresti, A. (2014) <doi:10.1007/978-3-642-04898-2_161>). A widely used selection criteria are available which includes Akaike information criterion(Darlington, R. B. (1968) <doi:10.1037/h0025471>, Judge, G. G. (1985) <doi:10.2307/1391738>), corrected Akaike information criterion(Hurvich, C. M., and Tsai, C. (1989) <doi:10.1093/biomet/76.2.297>), Bayesian information criterion(Sawa, T. (1978) <doi:10.2307/1913828>, Judge, G. G. (1985) <doi:10.2307/1391738>), Mallows Cp statistic(Mallows, C. L. (1973) <doi:10.1080/00401706.1995.10484370>, Hocking, R. R. (1976) <doi:10.2307/2529336>), Hannan and Quinn information criterion(Hannan, E. J. and Quinn, B. G. (1979) <doi:10.1111/j.2517-6161.1979.tb01072.x>, Mcquarrie, A. D. R. and Tsai, C. L. (1998) <doi:10.1142/3573>), corrected Hannan and Quinn information criterion(Mcquarrie, A. D. R. and Tsai, C. L. (1998) <doi:10.1142/3573>), Schwarz criterion(Schwarz, G. (1978) <doi:10.1214/aos/1176344136>, Judge, G. G. (1985) <doi:10.2307/1391738>), adjusted R-square statistic(Darlington, R. B. (1968) <doi:10.1037/h0025471>, Judge, G. G. (1985) <doi:10.2307/1391738>) and significance levels(Mckeon, J. J. (1974) <doi:10.1093/biomet/61.2.381>, Harold Hotelling. (1992) <doi:10.1007/978-1-4612-0919-5_4>, Pillai, K. C. S. (2006) <doi:10.1002/0471667196.ess1965.pub2>), where multicollinearity can be detected with checking tolerance value.
}
\details{
\tabular{ll}{
Package: \tab StepReg\cr
Type: \tab Package\cr
Version: \tab 1.4.0\cr
Date: \tab 2020-2-22\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Junhui Li, Xiaohuan Lu, Kun Cheng,Wenxin Liu

Maintainer: Junhui Li <junhuili@cau.edu.cn>
}
\references{
Agresti, A. (1984), Analysis of Ordinal Categorical Data, New York: John Wiley & Sons.

Agresti, A. (2014), Categorical Data Analysis, New York: John Wiley & Sons.

Alsubaihi, A. A., Leeuw, J. D., and Zeileis, A. (2002). Variable selection in multivariable regression using sas/iml. , 07(i12).

Darlington, R. B. (1968). Multiple regression in psychological research and practice. Psychological Bulletin, 69(3), 161.

Hannan, E. J., & Quinn, B. G. (1979). The determination of the order of an autoregression. Journal of the Royal Statistical Society, 41(2), 190-195.

Harold Hotelling. (1992). The Generalization of Student's Ratio. Breakthroughs in Statistics. Springer New York.

Hocking, R. R. (1976). A biometrics invited paper. the analysis and selection of variables in linear regression. Biometrics, 32(1), 1-49.

Hosmer, D. W., Jr. and Lemeshow, S. (2000), Applied Logistic Regression, 2nd Edition, New York: John Wiley & Sons.

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
\keyword{ package }
