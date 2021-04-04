\name{stepwiselogit}
\alias{stepwiselogit}
\title{
  Stepwise Logistic Regression
}
\description{
  Stepwise logistic regression analysis selects model based on information criteria and Wald or Score test with 'forward', 'backward', 'bidirection' and 'score' model selection method.
}
\usage{
  stepwiselogit(data, y, exclude = NULL, include = NULL, selection = "bidirection",
  select = "SL", sle = 0.15, sls = 0.15)
}
\arguments{
  \item{data}{
  Data set including dependent and independent variables to be analyzed
}
  \item{y}{
  A character or numeric vector indicating the subset of dependent variables
}
  \item{exclude}{
  A character or numeric vector indicating the subset of independent variables removed from logstic stepwise regression analysis
}
  \item{include}{
  Forces the effects vector listed in the data to be included in all models. The selection methods are performed on the other effects in the data set
}
  \item{selection}{
  Model selection method including "forward", "backward", "bidirection" and 'score',forward selection starts with no effects in the model and adds effects, backward selection starts with all effects in the model and removes effects, while bidirection regression is similar to the forward method except that effects already in the model do not necessarily stay there, and score method uses information criteria to find a specified number of best models containing one, two, or three variables, and so on, up to the single model containing all of the explanatory variables.
}
  \item{select}{
  Specifies the criterion that uses to determine the order in which effects enter andr leave at each step of the specified selection method including Akaike Information Criterion(AIC), the Corrected form of Akaike Information Criterion(AICc),Schwarz criterion(SBC), and Significant Levels(SL)
}
  \item{sle}{
  Specifies the significance level for entry, default is 0.15
}
  \item{sls}{
  Specifies the significance level for staying in the model, default is 0.15
}
}
\value{
  \item{RegressionModelsSelectedbyInformationCriterion }{summary of regression models selected by information criterion}
  \item{SummaryOfSelection }{summary of selection process}
  \item{AnalysisOfMaximumLikelihoodEstimate }{analysis of maximum likelihood estimate for the selected model}
  \item{GoodnessOfTeste }{Hosmer and Lemeshow goodness of fit (GOF) test}
}
\references{
  Agresti, A. (1984), Analysis of Ordinal Categorical Data, New York: John Wiley & Sons.

  Agresti, A. (2014), Categorical Data Analysis, New York: John Wiley & Sons.

  Hurvich, C. M. and Tsai, C.-L. (1993), A Corrected Akaike Information Criterion for Vector Autoregressive Model Selection, Journal of Time Series Analysis, 14, 271-279

  Hosmer, D. W., Jr. and Lemeshow, S. (2000), Applied Logistic Regression, 2nd Edition, New York: John Wiley & Sons.
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
  stepwiselogit(yx,y,selection="bidirection",select="IC(3/2)")
}
\keyword{stepwise logistic regression}