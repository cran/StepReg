## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(comment = NA)

## ----message = FALSE----------------------------------------------------------
library(StepReg)
data(mtcars)
formula <- mpg ~ .
res <- stepwise(formula = formula,
                data = mtcars,
                type = "linear",
                include = c("qsec"),
                strategy = "bidirection",
                metric = c("AIC"))

## ----message = FALSE----------------------------------------------------------
res

## ----message = FALSE----------------------------------------------------------
summary(res$bidirection$AIC)

## ----message = FALSE----------------------------------------------------------
plot(res, strategy = "bidirection", process = "overview")
plot(res, strategy = "bidirection", process = "details")

## ----eval = FALSE-------------------------------------------------------------
# report(res, report_name = "path_to/demo_res", format = "html")

## ----echo = FALSE-------------------------------------------------------------
library(knitr)
library(kableExtra)

Regression <- c("linear", "logit", "cox", "poisson", "gamma")
Reponse <- c("continuous", "binary", "time-to-event", "count", "continuous and positively skewed")
df <- data.frame(Regression, Reponse)

kable(df, format = "html", caption = 'Common regression categories') %>% kable_styling()

## ----echo = FALSE-------------------------------------------------------------
Strategy <- c("Forward Selection", "Backward Elimination", "Bidirectional Elimination", "Best Subsets")
Description <- c("In forward selection, the algorithm starts with an empty model (no predictors) and adds in variables one by one. Each step tests the addition of every possible predictor by calculating a pre-selected metric. Add the variable (if any) whose inclusion leads to the most statistically significant fit improvement. Repeat this process until more predictors no longer lead to a statistically better fit.",
                "In backward elimination, the algorithm starts with a full model (all predictors) and deletes variables one by one. Each step test the deletion of every possible predictor by calculating a pre-selected metric. Delete the variable (if any) whose loss leads to the most statistically significant fit improvement. Repeat this process until less predictors no longer lead to a statistically better fit.",
                "Bidirectional elimination is essentially a forward selection procedure combined with backward elimination at each iteration. Each iteration starts with a forward selection step that adds in predictors, followed by a round of backward elimination that removes predictors. Repeat this process until no more predictors are added or excluded.",
                "Stepwise algorithms add or delete one predictor at a time and output a single model without evaluating all candidates. Therefore, it is a relatively simple procedure that only produces one model. In contrast, the *Best Subsets* algorithm calculates all possible models and output the best-fitting models with one predictor, two predictors, etc., for users to choose from.")
df <- data.frame(Strategy, Description)

kable(df, format = "html", caption = 'Model selection strategy') %>% kable_styling()

## ----echo = FALSE-------------------------------------------------------------
Statistic <- c(
"${n}$",
"${p}$",
"${q}$",
"$\\sigma^2$",
"${SST}$",
"${SSE}$",
"$\\text{LL}$",
"${|  |}$",
"$\\ln()$")

Meanings <- c(
"Sample Size",
"Number of parameters including the intercept",
"Number of dependent variables",
"Estimate of pure error variance from fitting the full model",
"Total sum of squares corrected for the mean for the dependent variable, which is a numeric value for UMR and a matrix for multivariate regression",
"Error sum of squares, which is a numeric value for UMR and a matrix for multivariate regression",
"The natural logarithm of likelihood",
"The determinant function",
"The natural logarithm")

kable_styling(kable(data.frame(Statistic,Meanings),format = "html", align='l', escape = F, caption = 'Statistics in selection metric'))

## ----echo = FALSE-------------------------------------------------------------
Abbreviation <- c("", "AIC", "AICc", "BIC", "Cp", "HQ", "IC(1)", "IC(3/2)", "SBC", "SL", "adjRsq")
Definition <- c("",
                "Akaike’s Information Criterion",
                "Corrected Akaike’s Information Criterion",
                "Sawa Bayesian Information Criterion",
                "Mallows’ Cp statistic",
                "Hannan and Quinn Information Criterion",
                "Information Criterion with Penalty Coefficient Set to 1",
                "Information Criterion with Penalty Coefficient Set to 3/2",
                "Schwarz Bayesian Information Criterion",
                "Significance Level (pvalue)",
                "Adjusted R-square statistic")

Formula_in_Linear <- c("linear",
                       "$n\\ln\\left(\\frac{|\\text{SSE}|}{n}\\right) + 2pq + n + q(q+1)$ <br>[@Hurvich_Tsai_1989; @Al-Subaihi_2002]$^1$",
                       "$n\\ln\\left(\\frac{|\\text{SSE}|}{n}\\right) + \\frac{nq(n+p)}{n-p-q-1}$ <br>[@Hurvich_Tsai_1989; @Bedrick_Tsai_1994]$^2$",
                       "$n\\ln\\left(\\frac{SSE}{n}\\right) + 2(p+2)o - 2o^2, o = \\frac{n\\sigma^2}{SSE}$ <br>[@Sawa_1978; @Judge_1985] <br>not available for MMR",
                       "$\\frac{SSE}{\\sigma^2} + 2p - n$ <br> [@Mallows_1973; @Hocking_1976] <br>not available for MMR",
                       "$n\\ln\\left(\\frac{|\\text{SSE}|}{n}\\right) + 2pq\\ln(\\ln(n))$ <br>[@Hannan_Quinn_1979; @McQuarrie_Tsai_1998; @Hurvich_Tsai_1989]",
                       "$n\\ln\\left(\\frac{|\\text{SSE}|}{n}\\right) + p$ <br>[@Nelder_Wedderburn_1972; @Smith_Spiegelhalter_1980] not available for MMR",
                       "$n\\ln\\left(\\frac{|\\text{SSE}|}{n}\\right) + \\frac{3}{2}p$ <br>[@Smith_Spiegelhalter_1980] <br>not available for MMR",
                       "$n\\ln\\left(\\frac{|\\text{SSE}|}{n}\\right) + pq \\ln(n)$ <br>[@Hurvich_Tsai_1989; @Schwarz_1978; @Judge_1985; @Al-Subaihi_2002] <br>not available for MMR",
                       "$\\textit{F test}$ for UMR and $\\textit{Approximate F test}$ for MMR",
                       "$1 - \\frac{(n-1)(1-R^2)}{n-p}$, <br> where $R^2=1 - \\frac{SSE}{SST}$ <br>[@Darlington_1968; @Judge_1985] <br>not available for MMR")

Formula_in_Logit_Cox_Poisson_Gamma <- c("logit, cox, poisson and gamma",
                                        "$-2\\text{LL} + 2p$ <br>[@Darlington_1968; @Judge_1985]",
                                        "$-2\\text{LL} + \\frac{n(n+p)}{n-p-2}$ <br>[@Hurvich_Tsai_1989]",
                                        "not available",
                                        "not available",
                                        "$-2\\text{LL} + 2p\\ln(\\ln(n))$ <br>[@Hannan_Quinn_1979]",
                                        "$-2\\text{LL} + p$ <br>[@Nelder_Wedderburn_1972; @Smith_Spiegelhalter_1980]",
                                        "$-2\\text{LL} + \\frac{3}{2}p$ <br>[@Smith_Spiegelhalter_1980]",
                                        "$-2\\text{LL} + p\\ln(n)$ <br>[@Schwarz_1978; @Judge_1985]",
                                        "Forward: LRT and Rao Chi-square test (logit, poisson, gamma); LRT (cox); <br><br>Backward: Wald test",
                                        "not available")
df <- data.frame(Abbreviation, Definition, Formula_in_Linear, Formula_in_Logit_Cox_Poisson_Gamma)
colnames(df) <- c("Abbreviation","Definition","Formula","")

kable(df, format = "html", align = "l", 
      booktabs = TRUE, escape = F, 
      caption = 'Abbreviation, Definition, and Formula of the Selection Metric for Linear, Logit, Cox, Possion, and Gamma regression') %>%
  footnote(number = c("Unsupported AIC formula (which does not affect the selection process as it only differs by constant additive and multiplicative factors):\n
                      $AIC=n\\ln\\left(\\frac{SSE}{n}\\right) + 2p$ [@Darlington_1968; @Judge_1985]", 
                      "Unsupported AICc formula (which does not affect the selection process as it only differs by constant additive and multiplicative factors):\n
                      $AICc=\\ln\\left(\\frac{SSE}{n}\\right) + 1 + \\frac{2(p+1)}{n-p-2}$ [@McQuarrie_Tsai_1998]")) %>%
  kable_styling() %>%
  column_spec(3, width = "0.5in") %>%
  column_spec(4, width = "0.4in")

## ----eval = FALSE-------------------------------------------------------------
# report(res, report_name = "results", format = c("xlsx", "docx"))

## ----message = FALSE----------------------------------------------------------
data(mtcars)

formula <- mpg ~ .
res1 <- stepwise(formula = formula,
                 data = mtcars,
                 type = "linear",
                 include = c("disp", "cyl"),
                 strategy = "forward",
                 metric = "AIC")
res1

## ----plot_res1, warning = FALSE-----------------------------------------------
plot_list <- list()
plot_list[["forward"]][["details"]] <- plot(res1, process = "details")
plot_list[["forward"]][["overview"]] <- plot(res1, process = "overview")
cowplot::plot_grid(plotlist = plot_list$forward, ncol = 1)

## ----eval = FALSE-------------------------------------------------------------
# formula <- mpg ~ . + 0

## ----eval = FALSE-------------------------------------------------------------
# formula <- mpg ~ . - 1

## ----eval = FALSE-------------------------------------------------------------
# formula <- mpg ~ cyl + disp + hp + wt + vs + am + 0

## ----eval = FALSE-------------------------------------------------------------
# formula <- mpg ~ . - 1 - disp - wt

## ----message = FALSE, warning = FALSE-----------------------------------------
formula <- mpg ~ .
res2 <- stepwise(formula = formula,
                 data = mtcars,
                 type = "linear",
                 strategy = c("forward", "backward"),
                 metric = c("AIC", "BIC", "SL"),
                 sle = 0.05,
                 sls = 0.05)
res2

## ----message = FALSE, warning = FALSE, fig.width=9, fig.height=12-------------
plot_list <- setNames(
  lapply(c("forward", "backward"),function(i){
    setNames(
      lapply(c("details","overview"),function(j){
        plot(res2,strategy=i,process=j)
    }),
    c("details","overview")
    )
  }),
  c("forward", "backward")
)

cowplot::plot_grid(plotlist = plot_list$forward, ncol = 1, rel_heights = c(2, 1))
cowplot::plot_grid(plotlist = plot_list$backward, ncol = 1, rel_heights = c(2, 1))

## -----------------------------------------------------------------------------
formula <- cbind(mpg, drat) ~ . + 0
res3 <- stepwise(formula = formula,
                 data = mtcars,
                 type = "linear",
                 strategy = "bidirection",
                 metric = c("AIC", "HQ"))
res3

plot_list <- setNames(
  lapply(c("bidirection"),function(i){
    setNames(
      lapply(c("details","overview"),function(j){
        plot(res3,strategy=i,process=j)
    }),
    c("details","overview")
    )
  }),
  c("bidirection")
)

cowplot::plot_grid(plotlist = plot_list$bidirection, ncol = 1, rel_heights = c(2, 1))

## ----message = FALSE----------------------------------------------------------
data(remission)

formula <- remiss ~ .
res4 <- stepwise(formula = formula,
                 data = remission,
                 type = "logit",
                 include= "cell",
                 strategy = "forward",
                 metric = "AIC")
res4

plot_list <- setNames(
  lapply(c("forward"),function(i){
    setNames(
      lapply(c("details","overview"),function(j){
        plot(res4,strategy=i,process=j)
    }),
    c("details","overview")
    )
  }),
  c("forward")
)
cowplot::plot_grid(plotlist = plot_list$forward, ncol = 1, rel_heights = c(2, 1))

## ----message = FALSE----------------------------------------------------------
data(remission)

formula <- remiss ~ . + 0
res5 <- stepwise(formula = formula,
                  data = remission,
                  type = "logit",
                  strategy = "subset",
                  metric = "SBC",
                  best_n = 3)
res5

plot_list <- setNames(
  lapply(c("subset"),function(i){
    setNames(
      lapply(c("details","overview"),function(j){
        plot(res5,strategy=i,process=j)
    }),
    c("details","overview")
    )
  }),
  c("subset")
)
cowplot::plot_grid(plotlist = plot_list$subset, ncol = 1, rel_heights = c(2, 1))

## ----message = FALSE----------------------------------------------------------
library(dplyr)
library(survival)
# Preprocess:
lung <- survival::lung %>%
  mutate(sex = factor(sex, levels = c(1, 2))) %>% # make sex as factor
  na.omit() # get rid of incomplete records

formula  =  Surv(time, status) ~ .
res6 <- stepwise(formula = formula,
                 data = lung,
                 type = "cox",
                 strategy = "forward",
                 metric = "AICc")
res6

plot_list <- setNames(
  lapply(c("forward"),function(i){
    setNames(
      lapply(c("details","overview"),function(j){
        plot(res6,strategy=i,process=j)
    }),
    c("details","overview")
    )
  }),
  c("forward")
)
cowplot::plot_grid(plotlist = plot_list$forward, ncol = 1, rel_heights = c(2, 1))

## ----message = FALSE----------------------------------------------------------
data(creditCard)

formula  = reports ~ .
res7 <- stepwise(formula = formula,
                 data = creditCard,
                 type = "poisson",
                 strategy = "forward",
                 metric = "SL",
                 sle = 0.05)
summary(res7$forward$SL)

## ----message = FALSE----------------------------------------------------------
plot_list <- setNames(
  lapply(c("forward"),function(i){
    setNames(
      lapply(c("details","overview"),function(j){
        plot(res7,strategy=i,process=j)
      }),
      c("details","overview")
    )
  }),
  c("forward")
)
cowplot::plot_grid(plotlist = plot_list$forward, ncol = 1, rel_heights = c(2, 1))

## ----eval = FALSE-------------------------------------------------------------
# library(StepReg)
# StepRegShinyApp()

## ----sessionInfo, echo = FALSE------------------------------------------------
sessionInfo()

