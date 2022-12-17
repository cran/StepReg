## ----library------------------------------------------------------------------
#install.package("StepReg")
library(StepReg)

## -----------------------------------------------------------------------------
formula <- mpg ~ .
sForwAIC <- stepwise(formula=formula,
         data=mtcars,
         selection="forward",
         select="AIC")
sForwAIC

## -----------------------------------------------------------------------------
formula <- mpg ~ .
sBidiSL <- stepwise(formula=formula,
         data=mtcars,
         selection="bidirection",
         select="SL",
         sle=0.15,
         sls=0.15)
sBidiSL

## -----------------------------------------------------------------------------
#formula <- mpg ~ . -1
formula <- mpg ~ . + 0
sBackSBC <- stepwise(formula=formula,
                     data=mtcars,
                     selection="backward",
                     select="SBC")
sBackSBC

## -----------------------------------------------------------------------------
formula <- cbind(mpg,drat) ~ cyl+disp+hp+wt+vs+am
stepwise(formula=formula,
        data=mtcars,
        include='wt',
        selection="score",
        select="AICc")

## ----warning=FALSE------------------------------------------------------------
formula <- am ~ .
stepwiseLogit(formula=formula,
              data=mtcars,
              selection="forward",
              select="AIC")

## ----warning=FALSE------------------------------------------------------------
formula <- am ~ .
stepwiseLogit(formula=formula,
              data=mtcars,
              selection="score",
              select="SL",
              best=3)

## ----warning=FALSE------------------------------------------------------------
lung <- survival::lung
my.data <- na.omit(lung)
my.data$status1 <- ifelse(my.data$status==2,1,0)
data <- my.data
formula = Surv(time, status1) ~ . - status 

stepwiseCox(formula=formula,
  data=my.data,
  selection="forward",
  select="IC(1)")

## ----warning=FALSE------------------------------------------------------------
formula = Surv(time, status1) ~ . - status 
stepwiseCox(formula=formula,
  data=my.data,
  selection="score",
  select="SL",
  best=3)

## ----sessionInfo, echo=FALSE--------------------------------------------------
sessionInfo()

