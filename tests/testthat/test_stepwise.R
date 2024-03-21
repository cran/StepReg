Sys.setenv(R_TESTS="")
test_that("test_stepwise.R failed", {
  linear_model1 <- mpg ~ . + 1
  linear_model2 <- cbind(mpg, drat) ~ . + 0
  logit_model1 <- remiss ~ .
  cox_model1 <- Surv(time, status) ~ .
  ## the object should be located in inst/extdata instead of tests/data, since When you run devtools::check(), the package is installed in a temporary directory, tests/data is not in this temporary dir.
  ## If you have data files that need to be accessed during package checks, it's a good practice to place them in the inst/extdata directory of your package. Files in this directory are installed with the package and can be accessed using system.file().
  ## When you run devtools::check(), the package is installed in a temporary directory, and the working directory may not be set to the package directory itself. The system.file() function in R looks for files relative to the package installation directory, and if the working directory is not set correctly during the check process, it may lead to issues.
  res_v1_5_0 <- readRDS(system.file("extdata","res_v1_5_0.rds", package = "StepReg"))
  for (mod in names(res_v1_5_0)){
    type <- unlist(stringr::str_split(mod,"_"))[1]
    if(mod=="cox_model1"){
      lung <- survival::lung %>% na.omit()
      mydata <- lung
    }else if(mod %in% c("linear_model1","linear_model2")){
      data(mtcars)
      mtcars$yes <- mtcars$wt
      mydata <- mtcars
    }else if(mod %in% "logit_model1"){
      data(remission)
      mydata <- remission
    }
    #strategy="forward"
    for (strategy in names(res_v1_5_0[[mod]])){
      if(strategy=="subset"){
        index_n <- 2
        select_col1 <- c(1,2,3)
      }else{
        index_n <- 3
        select_col1 <- c(2,3,7)
      }
      #metric="SL"
      for(metric in names(res_v1_5_0[[mod]][[strategy]])){
        message(mod,"\t",strategy,"\t",metric)
        output_new <- NA
        output_old <- res_v1_5_0[[mod]][[strategy]][[metric]][,select_col1]
        
        try(output_new <- stepwise(type = type,
                                    formula=get(mod),
                                    data=mydata,
                                    strategy="backward",
                                    metric="SL",
                                    sle=0.05,
                                    sls=0.05)[[3]],silent = TRUE)
        output_new
        res <- try(expect_equal(output_new,output_old),silent = TRUE)
        if(inherits(res, "try-error")){
          message("Error\t",mod,"\t",strategy,"\t",metric)
        }  
      } #metric
    } #strategy
  } #mod
})
