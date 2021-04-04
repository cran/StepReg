\name{stepwise}
\alias{stepwise}
\title{
  Stepwise Regression
}
\description{
  Stepwise regression analysis can be performed  with univariate and multivariate based on information criteria specified, which includes 'forward', 'backward' and 'bidirection' direction model selection method. Also continuous variables nested within class effect and weighted stepwise are considered.
}
\usage{
stepwise(data, y, exclude, include, Class, weights, selection,
    select, sle, sls, tolerance, Trace, Choose)
}
\arguments{
  \item{data}{
  Data set including dependent and independent variables to be analyzed
}
  \item{y}{
  A character or numeric vector indicating the subset of dependent variables
}
  \item{exclude}{
  A character or numeric vector indicating the subset of independent variables removed from stepwise regression analysis
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
  \item{selection}{
  Model selection method including "forward", "backward" and "bidirection",forward selection starts with no effects in the model and adds effects, backward selection starts with all effects in the model and removes effects, while bidirection regression is similar to the forward method except that effects already in the model do not necessarily stay there
}
  \item{select}{
  Specifies the criterion that uses to determine the order in which effects enter and/or leave at each step of the specified selection method including Akaike Information Criterion(AIC), the Corrected form of Akaike Information Criterion(AICc),Bayesian Information Criterion(BIC),Schwarz criterion(SBC),Hannan and Quinn Information Criterion(HQ), R-square statistic(Rsq), adjusted R-square statistic(adjRsq), Mallows Cp statistic(CP) and Significant Levels(SL)
}
  \item{sle}{
  Specifies the significance level for entry
}
  \item{sls}{
  Specifies the significance level for staying in the model
}
  \item{tolerance}{
  Tolerance value for multicollinearity, default is 1e-7 
}
  \item{Trace}{
  Statistic for multivariate regression analysis, including Wilks' lamda ("Wilks"), Pillai Trace ("Pillai") and Hotelling-Lawley's Trace ("Hotelling")
}
  \item{Choose}{
  Chooses from the list of models at the steps of the selection process the model that yields the best value of the specified criterion. If the optimal value of the specified criterion occurs for models at more than one step, then the model with the smallest number of parameters is chosen. Choose method includes AIC, AICc, BIC, HQ, HQc, SBC,Rsq, adjRsq, CP and NULL, if you do not specify the Choose option, then the model selected is the model at the final step in the selection process
}
}
\details{
Multivariate regression and univariate regression can be detected by parameter 'y', where numbers of elements in 'y' is more than 1, then multivariate regression is carried out otherwise univariate regreesion
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

tol <- 1e-7
Trace <- "Pillai"
sle <- 0.15
sls <- 0.15
# weights vector
w0 <- c(rep(0.5,2),rep(1,18))
w2 <- c(rep(0.5,3),rep(1,14),0.5,1,0.5)

#univariate regression with select = 'SBC' &  choose = 'AIC' and select = 'CP' & choose = NULL 
#without forced effect and continuous variable nested in class effect

stepwise(yx, y="Y1", exclude="Y3", include=NULL, Class=NULL,w0,
selection="backward", select="SBC", sle, sls, tol, Trace, Choose='AIC')
stepwise(yx, y="Y1",exclude="Y3", include=NULL, Class=NULL, w0,
selection="bidirection", select="CP", sle, sls, tol, Trace, NULL)

#univariate regression with select='AICc'  & choose='HQc' and select='BIC' & choose = NULL 
#with forced effect and continuous variable nested in class effect 
stepwise(yx, y="Y1", exclude="Y3", include="Y2", Class="Y2", w2,
selection="forward", select='AICc', sle, sls, tol, Trace, 'HQc')
stepwise(yx, y="Y1", exclude="Y3", include="Y2", Class="Y2", w2,
selection="bidirection", 'BIC', sle, sls, tol, Trace, NULL)

#multivariate regression with select='HQ'  & choose='BIC'
#with forced effect and continuous variable nested in class effect
stepwise(yx, y=c("Y1","Y3"), exclude=NULL, include="Y2", Class="Y2", w2,
selection="bidirection", select='HQ', sle, sls, tol, Trace, 'BIC')
}
\keyword{stepwise regression}
