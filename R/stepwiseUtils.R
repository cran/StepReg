# Stepwise helper functions
# 
# @author Junhui Li, Kai Hu, Xiaohuan Lu

match_multiple_args <- function(value, choice){
	value <- sapply(value, function(x) match.arg(x, choice))
	names(value) <- NULL
	value
}

getXname <- function(formula, data) {
	term_form <- terms(formula, data = data)
	x_name <- attr(term_form, "term.labels")
	strata_check <- grepl("strata\\(", x_name)
	if(any(strata_check)) {
		strata_var <- gsub("strata\\((.*)\\)", "\\1", x_name[strata_check])
		x_name <- x_name[!strata_check]
		x_name <- x_name[!x_name %in% strata_var]
	}
	continuous_check <- grepl(":", x_name)
	if(any(continuous_check)) {
		cont_class_var <- x_name[continuous_check]
		back_var <- gsub(".*:", "", cont_class_var)
		front_var <- gsub(":.*", "", cont_class_var)
		## make sure front_var and back_var is a factor variable at least
		if(!all(sapply(data[, back_var], is.factor) | sapply(data[, front_var], is.factor))) {
			index <- !(sapply(data[, back_var], is.factor) | sapply(data[, front_var], is.factor))
			stop("For continuous-nested-within-class variable ",cont_class_var[index],", make sure one of the variables is a factor variable!")
		}
	}
	return(x_name)
}

getYname <- function(formula, data) {
	term_form <- terms(formula, data = data)
	vars <- as.character(attr(term_form, "variables"))[ -1 ]
	y_name <- vars[attr(term_form, "response")]
	return(y_name)
}

getIntercept <- function(formula, data, type) {
	term_form <- terms(formula, data = data)
	x_name <- attr(term_form, "term.labels")
	if(type != 'cox') {
		if(attr(term_form, "intercept") == 0) {
			intercept <- "0"
		}else{
			intercept <- "1"
		}
	}else{ # for 'cox', we need to set intercept to NULL if no strata() in formula
		if(any(grepl("strata\\(", x_name))) {
			intercept <- x_name[grepl("strata\\(", x_name)]
		}else{
			intercept <- '0'
		}
	}
	return(intercept)
}

getMergedVar <- function(include) {
	# obtain a single string concatenating all include variables by space
	if(is.character(include)) {
		merge_include_name <- paste0(include, collapse = " ")
	}else if(is.null(include)) {
		merge_include_name <- "NULL"
	}
	return(merge_include_name)
}

getModel <- function(data, type, intercept, x_name, y_name, weight, method = c("efron", "breslow", "exact")) {
	formula_raw <- reformulate(c(intercept, x_name), y_name)
	if(type == 'linear') {
		## cannot perform multivariate multiple regression in glm() function
		#lm_raw <- glm(formula_raw, data = data, weights = weights, family = "gaussian")
		model_raw <- do.call("lm", list(formula_raw, data = quote(data), weights = weight))
	}else if(type == "logit") {
		model_raw <- do.call("glm", list(formula_raw, data = quote(data), weights = weight, family = "binomial"))
	}else if(type == "poisson") {
		model_raw <- do.call("glm", list(formula_raw, data = quote(data), weights = weight, family = "poisson"))
	}else if(type == "gamma") {
		model_raw <- do.call("glm", list(formula_raw, data = quote(data), weights = weight, family = "Gamma"))
	}else if(type == 'cox') {
		## "method" is only used for cox regression
		method <- match.arg(method)
		model_raw <- do.call("coxph", list(formula_raw, data = quote(data), weights = weight, method = method))
	}else if(type == "negbin") {
		model_raw <- do.call("glm.nb", list(formula_raw, data = quote(data), weights = weight))
	}
	return(model_raw)
}

getSigmaFullModel <- function(lmf, type, n_y) {
	if(type == "linear") {
		if(lmf$rank >= nrow(as.data.frame(lmf$residuals))) {
			sigma_value <- 0
		}else{ 
			sigma_value <- sum(deviance(lmf)/df.residual(lmf))/n_y
		}
	}else{
		sigma_value <- 0
	}
	return(sigma_value)
}

getMulticolX <- function(data, x_name, tolerance) {
	## expand the continuous-nested-within-class variable to two variables
	continuous_check <- grepl(":", x_name)
	if(any(continuous_check)) {
		cont_class_var <- x_name[continuous_check]
		front_var <- unique(gsub(":.*", "", cont_class_var))
		back_var <- unique(gsub(".*:", "", cont_class_var))
		x_col_name <- c(x_name, front_var, back_var)
	}else{
		x_col_name <- x_name
	}
	x_name_var <- x_col_name[x_col_name %in% colnames(data)]
	x_matrix <- as.matrix(data[, x_name_var])
	x_matrix_num <- apply(x_matrix, 2, function(x) {
		if(is.character(x)) as.numeric(as.factor(x)) else as.numeric(x)
	})
	qrx_list <- qr(x_matrix_num, tol = tolerance)
	rank0 <- qrx_list$rank
	pivot0 <- qrx_list$pivot
	if(rank0 < length(pivot0)) {
		multico_x <- colnames(qrx_list$qr)[pivot0[(rank0	+	1) : length(pivot0)]]
	multico_x <- x_name[x_col_name %in% multico_x]
	}else{
		multico_x <- "NULL"
	}
	return(multico_x) # return multicollineared x_names
}

getTestMethod <- function(data, model_raw, type, metric, n_y, test_method_linear, test_method_glm, test_method_cox) {
	if(type == "linear") {
		n_obs <- nrow(data)
		# get sigma for BIC and CP
		if(n_y == 1) {
			test_method <- "F"
		}else{
			test_method <- test_method_linear
		}
	}else if(type == "logit" | type == "poisson" | type == "gamma" | type == "negbin") {
		test_method <- test_method_glm
	}else if(type == "cox") {
		test_method <- test_method_cox
	}
	return(test_method)
}

# For "subset", "forward", "backward", "bidirection": each model needs to calculate PIC. 
# Fit Model Statistics
#
# Fit Model Statistics with least square or likelihood method to return an information criteria value 
#
# @param metric Information criteria, including AIC, AICc, BIC, CP, HQ, HQc, adjRsq and SBC
# 
# @param fit Object of linear model or general linear model
# 
# @param type "linear", "cox", "logit", "poisson", "gamma" and "negbin": to calculate information criteria value; for "linear", the "Least Square" method will be used; for others, "Maximum Likelyhood" method will be used.
# 
# @param sigma_value Sigma value for calculation of 'BIC' and 'CP'

getModelFitStat <- function(metric = c("AIC", "AICc", "BIC", "CP", "HQ", "HQc", "adjRsq", "SBC", "IC(3/2)", "IC(1)"), fit, type = c("linear", "logit", "poisson", "cox", "gamma", "negbin"), sigma_value) {
	# "LeastSquare" is for linear; "Likelihood" is for cox and logit; cox and logit are essentially the same except for sample size calculation.
	if (type == "linear") {
		resMatrix <- as.matrix(fit$residuals)
		SSEmatrix <- t(resMatrix) %*% resMatrix
		SSE <- abs(det(SSEmatrix))
		p <- fit$rank
		n <- nrow(resMatrix)
		#yName <- rownames(attr(fit$terms, "factors"))[1]
		vars <- as.character(attr(fit$terms, "variables"))[ -1 ]
		yName <- vars[attr(fit$terms, "response")]
		Y <- as.matrix(fit$model[, yName])
		nY <- ncol(Y)
		if(metric == "AIC") {
			PIC <- n*log(SSE/n) + 2*p*nY + nY*(nY + 1) + n
		}else if(metric == "AICc") {
			PIC <- n*log(SSE/n) + n*(n + p)*nY/(n - p - nY - 1)
		}else if(metric == "CP") {
			PIC <- SSE/sigma_value + 2*p - n
		}else if(metric == "HQ") {
			#PIC <- n*log(SSE/n) + 2*log(log(n))*p*nY/n
			PIC <- n*log(SSE/n) + 2*log(log(n))*p*nY
		}else if(metric == "HQc") {
			#PIC <- n*log(SSE*SSE/n) + 2*log(log(n))*p*nY/(n - p - nY - 1)
			PIC <- n*log(SSE/n) + 2*log(log(n))*p*nY*n/(n - p - nY - 1)
		}else if(metric == "IC(1)") {
			PIC <- n*log(SSE/n) + p*nY + nY*(nY + 1) + n
		}else if(metric == "IC(3/2)") {
			PIC <- n*log(SSE/n) + 1.5*p*nY + nY*(nY + 1) + n
		}else if(metric == "BIC") {
			PIC <- n*log(SSE/n) + 2*(2 + p)*(n*sigma_value/SSE) - 2*(n*sigma_value/SSE)*(n*sigma_value/SSE)
		#}#else if(metric == "Rsq") {
			#PIC <- 1 - (SSE/SST)
			#PIC <- summary(fit)$r.squared
		}else if(metric == "adjRsq") {
			#PIC <- 1 - (SSE/SST)*(n - 1)/(n - p)
			PIC <- summary(fit)$adj.r.squared
		}else if(metric == "SBC") {
			PIC <- n*log(SSE/n) + log(n)*p*nY
		}
	} else if (type %in% c("logit", "poisson", "cox", "gamma", "negbin")) {
		ll <- logLik(fit)[1]
		p <- attr(logLik(fit), "df")
		if (type == "cox") {
			n <- fit$nevent
		} else {
			n <- nrow(fit$data)
		}
		if(metric == "IC(1)") {
			PIC <- -2*ll + p
		}else if(metric == "IC(3/2)") {
			PIC <- -2*ll + 1.5*p
		}else if(metric == "SBC") {
			PIC <- -2*ll + p*log(n)
		}else if(metric == "AICc") {
			#PIC <- -2*ll + 2*p*(p + 1)/(n - p - 1)
			PIC <- -2*ll + n*(n + p)/(n - p - 2)
		}else if(metric == "AIC") {
			PIC <- -2*ll + 2*p
		}else if(metric == "HQ") {
			PIC <- -2*ll + 2*p*log(log(n))
		}else if(metric == "HQc") {
			PIC <- -2*ll + 2*p*n*log(log(n))/(n - p - 2)
		}
	}
	return(PIC)
}

getInitialSubSet <- function(data, type, metric, y_name, intercept, include, weight, test_method, sigma_value) {
	# obtain the initial model information: if no include variable, return NULL, otherwise return a matrix containing columns of "NumberOfVariables", metric, and "VariablesInModel"
	# metric refers to PIC method, e.g. AIC, BIC, etc. for "logit" and "cox" type, if metric is "SL", the PIC is calculated differently
	initial_process_table <- NULL
	if (length(include) != 0) {
		initial_process_table <- data.frame(NumberOfVariables=numeric(),
																				metric=numeric(),
																				VariablesInModel=character())
		colnames(initial_process_table) <- c("NumberOfVariables", metric, "VariablesInModel")
		x_fit <- getModel(data = data, type = type, intercept = intercept, x_name = c(intercept, include), y_name = y_name, weight = weight, method = test_method)

		if(metric == "SL") {
			if(type == "cox") {
				pic_set <- x_fit$score
			}else {
				fit_reduce <- switch(type,
														 "logit"	 = glm(reformulate(intercept, y_name), data = data, weights = weight, family = "binomial"),
														 "poisson" = glm(reformulate(intercept, y_name), data = data, weights = weight, family = "poisson"),
														 "gamma"	 = glm(reformulate(intercept, y_name), data = data, weights = weight, family = "Gamma"),
														 "negbin"	= glm.nb(reformulate(intercept, y_name), data = data, weights = weight),
														 "linear"	= lm(reformulate(intercept, y_name), data = data, weights = weight)
														 
				)
				f_pic_vec <- getAnovaStat(add_or_remove = "add", include = include, fit_reduced = fit_reduce, fit_full = x_fit, type = type, test_method = test_method)
				pic_set <- f_pic_vec[1]
			}
		}else{
			pic_set <- getModelFitStat(metric, x_fit, type, sigma_value)
		}
		initial_process_table[1, 1:3] <- c(as.numeric(intercept) + length(include), pic_set, paste(c(intercept, include), collapse = " "))
	}
	return(initial_process_table)
}

getFinalSubSet <- function(data, type, metric, x_notin_model, initial_process_table, y_name, include, weight, intercept, best_n = Inf, test_method, sigma_value) {
	process_table <- initial_process_table
	#nv=8
	for (nv in 1:length(x_notin_model)) {
		com_table <- as.data.frame(combn(x_notin_model, nv))
		n_test <- ncol(com_table)
		com_var <- apply(com_table, 2, paste, collapse = " ")
		sub_process_table <- matrix(rep(c(nv + length(include) + as.numeric(intercept), NA), each = n_test), n_test, 2)
		com_var_df <- cbind(paste(c(intercept, include), collapse = " "), data.frame(com_var))
		com_var_set <- apply(com_var_df, 1, paste, collapse = " ")
		sub_process_table <- data.frame(sub_process_table, c(com_var_set))
		colnames(sub_process_table) <- c("NumberOfVariables", metric, "VariablesInModel")
		colnames(com_table) <- com_var_set
		x_test_list <- as.list(com_table)
		x_name_list <- lapply(x_test_list, function(x) {c(intercept, include, x)})
		x_fit_list <- lapply(x_name_list, function(x) {getModel(data = data, type = type, intercept = intercept, x_name = x, y_name = y_name, weight = weight, method = test_method)})
		
		if(metric == "SL") {
			if(type == "cox") {
				pic_set <- sapply(x_fit_list, function(x) {x$score})
			}else {
				fit_reduce <- switch(type,
														 "logit"	 = glm(reformulate(intercept, y_name), data = data, weights = weight, family = "binomial"),
														 "poisson" = glm(reformulate(intercept, y_name), data = data, weights = weight, family = "poisson"),
														 "gamma"	 = glm(reformulate(intercept, y_name), data = data, weights = weight, family = "Gamma"),
														 "negbin"	= glm.nb(reformulate(intercept, y_name), data = data, weights = weight),
														 "linear"	= lm(reformulate(intercept, y_name), data = data, weights = weight)
				)
				f_pic_vec <- sapply(x_fit_list, function(x) {getAnovaStat(add_or_remove = "add", include = include, fit_reduced = fit_reduce, fit_full = x, type = type, test_method = test_method)})
				pic_set <- f_pic_vec[1, ]
			}
		}else{
			pic_set <- sapply(x_fit_list, function(x) {getModelFitStat(metric, x, type, sigma_value)})
		}
		sub_process_table[, 2] <- pic_set
		
		if (metric %in% c("SL", "adjRsq")) {
			# "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
			decreasing = TRUE
		} else{
			decreasing = FALSE
		}
		sub_process_table_sort <- sub_process_table[order(sub_process_table[, 2], decreasing = decreasing), ]
		if(nrow(sub_process_table_sort) < best_n) {
			best_n_model <- nrow(sub_process_table_sort)
		}else{
			best_n_model <- best_n
		}
		process_table <- rbind(process_table, sub_process_table_sort[1:best_n_model, ])
	}
	return(process_table)
}

getXNameSelected <- function(process_table, metric) {
	if (metric %in% c("SL", "adjRsq")) {
		# "Rsq" and "adjRsq" are for type "linear"; "SL" is for type "logit" and "cox"
		x_name_selected <- unlist(strsplit(process_table[which.max(as.numeric(process_table[, metric])), 3], " "))
	} else{
		x_name_selected <- unlist(strsplit(process_table[which.min(as.numeric(process_table[, metric])), 3], " "))
	}
	x_name_selected <- x_name_selected[!x_name_selected %in% ""]
	return(x_name_selected)
}

getSubsetWrapper <- function(data, type, metric, x_name, y_name, intercept, include, weight, best_n, test_method, sigma_value) {
	# a wrapper to obtain x_name_selected
	## obtain initial model info
	initial_process_table <- getInitialSubSet(data, type, metric, y_name, intercept, include, weight = weight, test_method, sigma_value)
	
	## obtain final model info
	x_notin_model <- setdiff(x_name, include)
	process_table <- getFinalSubSet(data, type, metric, x_notin_model, initial_process_table, y_name, include, weight = weight, intercept, best_n, test_method, sigma_value)
	
	## add rownames to sort the variables in process_table when output
	rownames(process_table) <- c(1:nrow(process_table))
	return(process_table)
}

formatTable <- function(tbl, tbl_name = "Test") {
	tbl_list <- list(tbl)
	names(tbl_list) <- tbl_name
	class(tbl_list) <- "StepReg"
	return(tbl_list)
}

getTable1SummaryOfParameters <- function(formula, data, type, x_name, y_name, merged_multico_x, 
																				 merged_include, strategy, metric, sle, sls, 
																				 test_method, tolerance, intercept, test_ratio, feature_ratio, seed) {
	# generate: table1: Summary of Parameters
	table_1_summary_of_parameters <- data.frame(
		Parameter = c("initial formula",
									"regression type",
									"selection strategy", 
									"stepwise metric", 
									"significance level for entry (sle)", 
									"significance level for stay (sls)",
									"included variable",
									"test method", 
									"tolerance of multicollinearity", 
									"multicollinearity variable", 
									"intercept",
									"test ratio",
									"feature ratio",
									"seed"),
		Value = c(paste0(deparse(formula), collapse = ""),
							type,
							paste0(strategy, collapse=" & "), 
							paste0(metric, collapse=" & "), 
							sle, 
							sls, 
							merged_include, 
							test_method, 
							tolerance, 
							merged_multico_x, 
							intercept,
							test_ratio,
							feature_ratio,
							seed)
	)
	if(type == 'cox') {
		# "intercept" is not relevant
		table_1_summary_of_parameters <- table_1_summary_of_parameters[-11, ]
	}
	# get rid of unrelevant variables from table 1:
	nrow_sls <- which(table_1_summary_of_parameters$Parameter %in% "significance level for stay (sls)")
	nrow_sle <- which(table_1_summary_of_parameters$Parameter %in% "significance level for entry (sle)")
	nrow_test_method <- which(table_1_summary_of_parameters$Parameter %in% "test method")
	if(any(metric %in% "SL")) {
		if(any(strategy == "bidirection") | (any(strategy == "forward") & any(strategy == "backward"))) {
			table_1_summary_of_parameters <- table_1_summary_of_parameters
		}else if(any(strategy == "forward") & (!any(strategy == "bidirection") & !any(strategy == "backward"))) {
			table_1_summary_of_parameters <- table_1_summary_of_parameters[-nrow_sls, ]
		}else if(any(strategy == "backward") & (!any(strategy == "bidirection") & !any(strategy == "forward"))) {
			table_1_summary_of_parameters <- table_1_summary_of_parameters[-nrow_sle, ]
		}else{
			table_1_summary_of_parameters <- table_1_summary_of_parameters[-c(nrow_sle,nrow_sls), ]
		}
	}else if(!any(metric %in% "SL") & type != 'cox') {
		table_1_summary_of_parameters <- table_1_summary_of_parameters[-c(nrow_sls,nrow_sle,nrow_test_method), ]
	}
	return(table_1_summary_of_parameters)
}

getTable2TypeOfVariables <- function(model) {
	x_name <- getXname(formula(model), model.frame(model))
	y_name <- getYname(formula(model), model.frame(model))
	
	class_var <- attr(model$terms, "dataClasses")
	class_table <- matrix(c(names(class_var), class_var), ncol=2)
	table2_class_table <- cbind(class_table,NA)
	table2_class_table[class_table[, 1] %in% y_name, 3] <- "Dependent"
	table2_class_table[!class_table[, 1] %in% y_name, 3] <- "Independent"
	colnames(table2_class_table) <- c("Variable_name", "Variable_class", "Variable_type")
	rownames(table2_class_table) <- NULL
	return(as.data.frame(table2_class_table))
}

#note1: test_method_linear should be 'F' for univariate and c(“Pillai”, “Wilks”, “Hotelling-Lawley”, “Roy”) for multivariates, so cant use summary() to get p value for multivariates.
#note2: for cox regression, no matter what the parameter is in test="x" of anova(), always return the same p value. 
# 1) Wald test p value can only obtained from summary(). 
# 2) cannot select 'Rao' or 'LRT' in anova for effect entered.
#note3: for logit/poison/gamma regression, 
#1) Wald test p value obtained from summary(), since we dont know if test='chisq' in anova is for wald test. 
#2) can select 'Rao' or 'LRT' in anova for effect entered.
#3) for factor effect in formula, xname1(for example sex) in formula is different with ones(for example sex1 and sex2) in summary(), we need to keep the min p value of paste("set",c(1:2)), and then rename it with 'sex'.
#getAnovaStat(fit_reduced = x_fit_list[[1]], fit_full = fit_x_in_model, type = type, test_method = test_method)
getAnovaStat <- function(add_or_remove = "add", intercept, include, fit_reduced, fit_full, type, test_method) {
	if (type == "linear") {
		ptype <- 'Pr(>F)'
		if(test_method %in% c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")) {
			stattype <- 'approx F'
		}else{
			stattype <- 'F'
		}
	} else if (type == "logit" | type == "poisson" | type == "gamma" | type == "negbin") {
		if(add_or_remove == "add") {
			if(test_method == "Rao") {
				stattype <- "Rao"
			}else if(test_method == "LRT") {
				stattype <- "Deviance"
			}
			ptype <- 'Pr(>Chi)'
		}else{
			stattype <- "z value"
			ptype <- 'Pr(>|z|)'
		}
	} else if (type == "cox") {
		# need to update for wald test
		if(add_or_remove == "add") {
			test_method <- ""
			ptype <- c('P(>|Chi|)', 'Pr(>|Chi|)')
			stattype <- "Chisq"
		}else{
			stattype <- "z"
			ptype <- 'Pr(>|z|)'
		}
	}
	if(type != "linear" & add_or_remove == "remove") { #wald test for logit and cox with add_or_remove = "remove"
		stat_table <- coef(summary(fit_full))
		stat_table[, stattype] <- stat_table[, stattype]^2
		coef_name <- rownames(stat_table)
		ptype <- colnames(stat_table)[colnames(stat_table) %in% ptype]
		xlevels <- fit_full$xlevels
		
		if(intercept == "1") { # remove intercept for scanning all other variables' p value
			if(nrow(stat_table) == 2) {
				stat_table <- stat_table[-1, , drop = FALSE]
			} else {
				stat_table <- stat_table[-1,]
			}
		}
		if(length(xlevels) == 0) {
			stat_table <- as.data.frame(stat_table)
			stat_table$Var <- rownames(stat_table)
		} else {
			coef_name_levels <- unlist(lapply(names(xlevels), function(i) {
				paste(i,xlevels[[i]],sep="")
			}))
			names(coef_name_levels) <- rep(names(xlevels),sapply(xlevels,length))
			#coef_name_include <- c(coef_name_levels[names(coef_name_levels) %in% include], include)
			#stat_table_no_include <- stat_table[which(!coef_name %in% coef_name_include), , drop = FALSE]
			#coef_name_parent <- sapply(rownames(stat_table_no_include), function(i){
			coef_name_parent <- sapply(rownames(stat_table), function(i) {
				if(i %in% coef_name_levels) {
					names(coef_name_levels)[coef_name_levels %in% i]
				} else {
					i
				}
			})
			stat_table <- as.data.frame(stat_table)
			stat_table$Var <- coef_name_parent
		}
		
		pic_set <- stat_table[, ptype]
		#names(pic_set) <- rownames(stat_table)
		names(pic_set) <- stat_table$Var
		statistics <- stat_table[, stattype]
		#names(statistics) <- rownames(stat_table)
		names(statistics) <- stat_table$Var
		#maxPVar <- rownames(stat_table)[which.max(pic_set)]
		pic <- list(pic_set[!names(pic_set) %in% include])
		statistics <- list(statistics[!names(statistics) %in% include])
		# maxPVar <- names(which.max(pic_set))
		# statistics <- stat_table[maxPVar, stattype]
		# pic <- stat_table[maxPVar, ptype]
	} else {
		stat_table <- anova(fit_reduced, fit_full, test = test_method)
		ptype <- names(stat_table)[names(stat_table) %in% ptype]
		statistics <- stat_table[2, stattype]
		pic <- stat_table[2, ptype]
	}
	return(c("statistics" = statistics, "pic" = pic))
}

## get pic based on model fit, it needs fit_reduced and fit_full for SL and only fit_formula for other metrics
## used in stepwise in step0: SL:0, 1, inf	non - SL:
## fit_fm<- fit_intercept
getInitStepModelStat <- function(fit_intercept, fit_fm, type, strategy, metric, intercept, include, test_method, sigma_value) {
	if(metric == "SL") {
		if(!is.null(include)) {
			if(all(include %in% attr(fit_fm$terms, "term.labels")) & strategy != "backward") {
				#add_or_remove, intercept, fit_reduced, fit_full, type, test_method
				f_pic_vec <- getAnovaStat(add_or_remove = "add", include = include, fit_reduced = fit_intercept, fit_full = fit_fm, type = type, test_method = test_method)
				pic <- f_pic_vec["pic"]
			}else{
				pic <- 1
			}
		}else{
			pic <- 1
		}
	}else{
		if(strategy == "backward") {
			pic <- getModelFitStat(metric, fit_fm, type, sigma_value)
		}else{
			if(!is.null(include)) {
				if(all(include %in% attr(fit_fm$terms, "term.labels"))) {
					pic <- getModelFitStat(metric, fit_fm, type, sigma_value)
				}else{
					if(intercept == '1') {
						pic <- getModelFitStat(metric, fit_fm, type, sigma_value)
					}else{
						if(metric %in% c("adjRsq") & type == "linear") {
							pic <- 0
						}else{
							pic <- Inf
						}
					}
				}
			}else{
				if(intercept == '1') {
					pic <- getModelFitStat(metric, fit_fm, type, sigma_value)
				}else{
					if(metric %in% c("adjRsq") & type == "linear") {
						pic <- 0
					}else{
						pic <- Inf
					}
				}
			}
		}
	}
	return(pic)
}

#return 3 num for IC and remove 1st of 3 num for SL
getNumberEffect <- function(fit, type) {
	if(type == "linear") {
		vec <- c(length(attr(fit$terms, "term.labels")) + attr(fit$terms,"intercept"), fit$rank)
	}else if(type == "logit" | type == "poisson" | type == "gamma" | type == "negbin") {
		vec <- c(fit$rank, fit$rank)
	}else if(type == "cox") {
		vec <- c(attr(logLik(fit), "df"), attr(logLik(fit), "df"))
	}
	return(vec)
}

initialProcessTable <- function(metric) {
	sub_init_process_table <- data.frame(Step = numeric(), 
															 EffectEntered = character(), 
															 EffectRemoved = character(), 
															 NumberEffect = numeric(), 
															 NumberParams = numeric(), 
															 metric = numeric())
	colnames(sub_init_process_table)[ncol(sub_init_process_table)] <- metric
	#colnames(sub_init_process_table)[ncol(sub_init_process_table)] <- ifelse(metric == "SL", "PValue", metric)
	return(sub_init_process_table)
}

getInitialStepwise <- function(data, type, strategy, metric, intercept, include, x_name, y_name, weight, test_method, sigma_value) {
	sub_init_process_table <- initialProcessTable(metric)
	process_table <- sub_init_process_table
	if(strategy == "backward") {
		add_or_remove <- "remove"
		#the order of variable in x_in_model will affect pic calculation.
		x_in_model <- c(setdiff(x_name, include))
		x_notin_model <- NULL
		fit_full <- getModel(data = data, type = type, intercept = intercept, x_name = c(include, x_in_model), y_name = y_name, weight = weight, method = test_method)
		pic <- getInitStepModelStat(fit_intercept = NULL, fit_fm = fit_full, type = type, strategy = strategy, metric = metric, intercept = intercept, include = include, test_method = test_method, sigma_value)
		num_eff_para_in <- getNumberEffect(fit = fit_full, type = type)
		process_table[1, ] <- c(rep("", 3), num_eff_para_in, pic)
	}else{
		add_or_remove <- "add"
		x_in_model <- NULL
		x_notin_model <- setdiff(x_name, include)
		#x_notin_model <- x_notin_model[sample(1:length(x_notin_model), size = round(feature_ratio * length(x_notin_model), 0))]
		## for intercept
		fit_intercept <- getModel(data = data, type = type, intercept = intercept, x_name = NULL, y_name = y_name, weight = weight, method = test_method)
		pic <- getInitStepModelStat(fit_intercept = fit_intercept, fit_fm = fit_intercept, type = type, strategy = strategy, metric = metric, intercept = intercept, include = include, test_method = test_method, sigma_value)
		num_eff_para_in <- getNumberEffect(fit = fit_intercept, type = type)
		process_table[1, ] <- c("", intercept, "", num_eff_para_in, pic)
		## for include
		if(!is.null(include)) {
			fit_include <- getModel(data = data, type = type, intercept = intercept, x_name = include, y_name = y_name, weight = weight, method = test_method)
			pic <- getInitStepModelStat(fit_intercept = fit_intercept, fit_fm = fit_include, type = type, strategy = strategy, metric = metric, intercept = intercept, include = include, test_method = test_method, sigma_value)
			num_eff_para_in <- getNumberEffect(fit = fit_include, type = type)
			#sub_init_process_table[1, ] <- c("", paste0(include, collapse = " "), "", abs(anova(fit_include, fit_intercept)[2, 'Df']), num_eff_para_in[-1], pic)
			sub_init_process_table[1, ] <- c("", paste0(include, collapse = " "), "", num_eff_para_in, pic)
			process_table <- rbind(process_table, sub_init_process_table)
		}
	}
	process_table$Step <- c(1:nrow(process_table))
	return(list("add_or_remove" = add_or_remove, "x_in_model" = x_in_model, "x_notin_model" = x_notin_model, "process_table" = process_table))
}

getCandStepModel <- function(add_or_remove, data, type, metric, weight, y_name, x_in_model, x_notin_model, intercept, include, test_method, sigma_value, feature_ratio) {
	fit_x_in_model <- getModel(data = data, type = type, intercept = intercept, x_name = c(include, x_in_model), y_name = y_name, weight = weight, method = test_method)
	BREAK <- FALSE
	if(add_or_remove == "add") {
		x_test <- x_notin_model[sample(1:length(x_notin_model), size = round(feature_ratio * length(x_notin_model), 0))]
	}else{
		x_test <- x_in_model
	}
	if(length(x_test) == 0) {
		BREAK <- TRUE
	}
	x_test_list <- as.list(x_test)
	names(x_test_list) <- x_test

	if(BREAK == FALSE) {
		if(add_or_remove == "add") {
			x_name_list <- lapply(x_test_list, function(x) {c(x_in_model, x)})
		}else{
			x_name_list <- lapply(x_test_list, function(x) {setdiff(x_in_model, x)})
		}
		x_fit_list <- lapply(x_name_list, function(x) {getModel(data = data, type = type, intercept = intercept, x_name = c(include, x), y_name = y_name, weight = weight, method = test_method)})
		
		if(metric == "SL") {
			if(type != "linear" & add_or_remove == "remove") {
				f_pic_vec <- getAnovaStat(add_or_remove = "remove", intercept = intercept, include = include, fit_full = fit_x_in_model, type = type, test_method = test_method)
				pic_set <- f_pic_vec[[2]]
				f_set <- f_pic_vec[[1]]
				# var_set <- f_pic_vec[3]
				# names(pic_set) <- var_set
				# names(f_set) <- var_set
			}else{
				f_pic_vec <- sapply(x_fit_list, function(x) {getAnovaStat(add_or_remove = add_or_remove, include = include, fit_reduced = x, fit_full = fit_x_in_model, type = type, test_method = test_method)})
				pic_set <- f_pic_vec[2, ]
				f_set <- f_pic_vec[1, ]
				names(pic_set) <- colnames(f_pic_vec)
				names(f_set) <- colnames(f_pic_vec)
			}
		} else {
			if(add_or_remove == "remove" & length(x_test) == 1 & intercept == "0") {
				if(metric == 'adjRsq'){
					pic_set <- 0
				} else {
					pic_set <- Inf
				}
				names(pic_set) <- x_test
			}else{
				pic_set <- sapply(x_fit_list, function(x) {getModelFitStat(metric, x, type, sigma_value)})
			}
		}
		if(metric == "adjRsq" | (metric == "SL" & add_or_remove == "remove")) {
			pic <- max(pic_set)
			minmax_var <- names(which.max(pic_set))
			best_candidate_model <- x_fit_list[[minmax_var]]
		} else {
			pic <- min(pic_set)
			minmax_var <- names(which.min(pic_set))
			best_candidate_model <- x_fit_list[[minmax_var]]
			if(sum(pic_set %in% pic) > 1 & metric == "SL") {
				Fvalue <- max(f_set)
				minmax_var <- names(which.max(f_set))
				best_candidate_model <- x_fit_list[[minmax_var]]
				pic <- pic_set[minmax_var]
			}
		}
		if(type != "cox") {
			if(best_candidate_model$rank == fit_x_in_model$rank & add_or_remove == "add") {
				BREAK <- TRUE
			}
		}
		return(list("pic" = pic, "minmax_var" = minmax_var, "best_candidate_model" = best_candidate_model, "BREAK" = BREAK, "pic_list" = pic_set))
	}else{
		return(list("BREAK" = BREAK))
	}
}

getGoodnessFit <- function(best_candidate_model, y_name, metric) {
	smr <- summary(best_candidate_model)
	n_y <- ncol(as.matrix(best_candidate_model$model[, y_name]))
	if(n_y == 1) {
		f <- smr$fstatistic
		t <- smr
		if(is.nan(f[1])) {
			pval <- NaN
		}else{
			pval <- pf(f[1], f[2], f[3], lower.tail = F)
		}
	}else{
		for(ny in 1:n_y) {
			f <- smr[[ny]]$fstatistic
			if(is.nan(f[1])) {
				pval <- NaN
			}else{
				pval <- pf(f[1], f[2], f[3], lower.tail = F)
			}
		}
	}
	if(is.nan(pval) == TRUE & (metric != 'adjRsq')) {
		BREAK <- TRUE
	}else{
		BREAK <- FALSE
	}
	return(BREAK)
}

checkEnterOrRemove <- function(add_or_remove, best_candidate_model, type, metric, sle, sls, y_name, pic, process_table) {
	if(metric == 'SL') {
		if(add_or_remove == "remove") {
			indicator <- pic > sls
		}else{
			indicator <- pic < sle
		}
	}else if(metric == 'adjRsq') {
		indicator <- pic > as.numeric(process_table[nrow(process_table), 6])
	}else{
		indicator <- pic <= as.numeric(process_table[nrow(process_table), 6])
	}
	if(indicator == TRUE & type == "linear" & (metric != "adjRsq")) {
		if(best_candidate_model$rank != 0) {
			BREAK <- getGoodnessFit(best_candidate_model, y_name, metric)
		}
	}else{
		BREAK <- FALSE
	}
	return(c("indicator" = indicator, "BREAK" = BREAK))
}

updateXinModel <- function(add_or_remove, indicator, best_candidate_model, type, metric, BREAK, pic, x_in_model, x_notin_model, process_table, minmax_var, pic_list) {
	sub_init_process_table <- initialProcessTable(metric)
	if(indicator == TRUE & BREAK == FALSE) {
		if(add_or_remove == "add") {
			x_in_model <- append(x_in_model, minmax_var)
			x_notin_model <- setdiff(x_notin_model, minmax_var)
			sub_init_process_table[1, ] <- c(as.numeric(process_table[nrow(process_table), 1]) + 1, minmax_var, "", getNumberEffect(fit = best_candidate_model, type), pic)
		}else{
			x_notin_model <- append(x_notin_model, minmax_var)
			x_in_model <- setdiff(x_in_model, minmax_var)
			sub_init_process_table[1, ] <- c(as.numeric(process_table[nrow(process_table), 1]) + 1, "", minmax_var, getNumberEffect(fit = best_candidate_model, type), pic)
		}
		process_table <- rbind(process_table, sub_init_process_table)
		#process_table[nrow(process_table), 1] <- as.numeric(process_table[1, nrow(process_table) - 1])	+	1
		pic_set <- unlist(pic_list)
	}else{
		BREAK <- TRUE
		pic_set <- NULL
	}
	return(list("BREAK" = BREAK, "process_table" = process_table, "x_in_model" = x_in_model, "x_notin_model" = x_notin_model, "pic_set" = pic_set))
}

getFinalStepModel <- function(add_or_remove, data, type, strategy, metric, sle, sls, weight, y_name, x_in_model, x_notin_model, intercept, include, process_table, test_method, sigma_value, feature_ratio) {
	pic_df <- NULL
	while(TRUE) {
		out_cand_stepwise <- getCandStepModel(add_or_remove, data, type, metric, weight = weight, y_name, x_in_model, x_notin_model, intercept, include, test_method, sigma_value, feature_ratio)
		BREAK <- out_cand_stepwise$BREAK
		minmax_var <- out_cand_stepwise$minmax_var
		if(BREAK == TRUE) {
			break
		}
		best_candidate_model <- out_cand_stepwise$best_candidate_model
		pic <- out_cand_stepwise$pic
		pic_list <- list(sort(out_cand_stepwise$pic_list))
		
		out_check <- checkEnterOrRemove(add_or_remove, best_candidate_model, type, metric, sle, sls, y_name, pic, process_table)
		indicator <- out_check["indicator"]
		BREAK <- out_check["BREAK"]
		if(BREAK == TRUE) {
			break
		}
		
		out_updateX <- updateXinModel(add_or_remove, indicator, best_candidate_model, type, metric, BREAK, pic, x_in_model, x_notin_model, process_table, minmax_var, pic_list)
		x_in_model <- out_updateX$x_in_model
		x_notin_model <- out_updateX$x_notin_model
		process_table <- out_updateX$process_table
		pic_set <- out_updateX$pic_set
		if(!is.null(pic_set)) {
			pic_df <- rbind(pic_df,data.frame(strategy, metric, step = process_table[nrow(process_table),1], "variable" = names(pic_set), "value" = pic_set))
		}
		rownames(pic_df) <- NULL
		# stop stepwise infinite loops
		# enter remove
		# x1
		#			 x1(stop here)
		#			 
		# enter remove		 
		#			 x1(stop here)
		# x1
		if(nrow(process_table) > 1) {
			last2_step <- process_table[nrow(process_table) -1 , ]
			last1_step <- process_table[nrow(process_table), ]
			if(last2_step[, 2] != "" & last2_step[, 2] == last1_step[, 3]) {
				break
			}else if(last2_step[, 3] == last1_step[, 2] & last1_step[, 2] != "") {
				process_table <- process_table[ - nrow(process_table), ]
				break
			}
		}
		
		if(indicator == TRUE) {
			if(strategy == 'bidirection') {
				if(add_or_remove == "remove") {
					next
				}else{
					add_or_remove <- "remove"
					next
				}
			}else{
				next
			}
		}else{
			if(strategy == 'bidirection' & add_or_remove == "remove") {
				add_or_remove <- "add"
				next
			}else{
				break
			}
		}
	}
	
	if(type == "cox" && strategy != "backward") {
		process_table <- process_table[-1, ]
		process_table$Step <- as.numeric(process_table$Step) - 1
	}
	return(list("process_table" = process_table, "x_in_model" = x_in_model, "x_notin_model" = x_notin_model, "pic_df" = pic_df))
}

getStepwiseWrapper <- function(data, type, strategy, metric, sle, sls, weight, x_name, y_name, intercept, include, test_method, sigma_value, feature_ratio) {
	#fit_full <- getModel(data = data, type = type, intercept = intercept, x_name = c(include, x_name), y_name = y_name, weight = weight, method = test_method)
	out_init_stepwise <- getInitialStepwise(data, type = type, strategy, metric, intercept, include, x_name, y_name, weight = weight, test_method = test_method, sigma_value)
	add_or_remove <- out_init_stepwise$add_or_remove
	x_in_model <- out_init_stepwise$x_in_model
	x_notin_model <- out_init_stepwise$x_notin_model
	process_table <- out_init_stepwise$process_table
	rownames(process_table) <- NULL
	pic_df_init <- data.frame(strategy, metric, process_table[,c(1:2,6)])
	colnames(pic_df_init)[c(3:5)] <- c("step","variable","value")
	## get final stepwise model
	out_final_stepwise <- getFinalStepModel(add_or_remove, data, type = type, strategy, metric, sle, sls, weight = weight, y_name, x_in_model, x_notin_model, intercept, include, process_table, test_method, sigma_value, feature_ratio)
	
	if(type == "cox") {
		if(strategy == "backward"){
			Selection <- rep("Remove",nrow(pic_df_init))
		} else {
			pic_df_init <- pic_df_init[-1,]
			out_final_stepwise$pic_df$step <- as.numeric(out_final_stepwise$pic_df$step) - 1
			if(nrow(pic_df_init) > 0){
				Selection <- rep("Entry",nrow(pic_df_init))
				pic_df_init$step <- pic_df_init$step - 1
			} else {
				Selection <- NULL
			}
		}
	} else {
		if(strategy == "backward"){
			Selection <- rep("Remove",nrow(pic_df_init))
		} else {
			Selection <- rep("Entry",nrow(pic_df_init))
		}
	}
	for (i in out_final_stepwise$process_table$Step) {
		sub_process_table <- out_final_stepwise$process_table[out_final_stepwise$process_table$Step %in% i,]
		sub_pic_df <- out_final_stepwise$pic_df[out_final_stepwise$pic_df$step %in% i,]
		if(!is.null(sub_pic_df) && nrow(sub_pic_df) > 0){
			sub_var <- sub_process_table[,c(2,3)][!sub_process_table[,c(2,3)] %in% ""]
			if(colnames(sub_var) == "EffectEntered"){
				selected_index <- "Entry"
			} else if(colnames(sub_var) == "EffectRemoved") {
				selected_index <- "Remove"
			}
			
			sub_pic_df$value[sub_pic_df$variable %in% sub_var] <- selected_index
			sub_pic_df$value[!sub_pic_df$variable %in% sub_var] <- "No"
			Selection <- append(Selection,sub_pic_df$value)
		}
	}
	
	pic_df <- rbind(pic_df_init,out_final_stepwise$pic_df)
	pic_df$value <- as.numeric(pic_df$value)
	pic_df$step <- as.numeric(pic_df$step)
	pic_df$Selection <- Selection

	out_final_stepwise$pic_df <- pic_df
	return(out_final_stepwise)
}

getTable3ProcessSummary <- function(data_train, data_test, type, strategy, metric, sle, sls, weight, x_name, y_name, intercept, include, best_n, test_method, sigma_value, num_digits, feature_ratio) {
	overview_table_metric <- list()
	x_final_model_metric <- list()
	detail <- list()
	model_performance_df <- NULL
	overview <- list()
	
	for(stra in strategy) {
		for(met in metric) {
			if(stra == "subset") {
				overview_table <- getSubsetWrapper(data_train, type = type, met, x_name, y_name, intercept, include, weight = weight, best_n, test_method, sigma_value)
				if(met != "SL"){
					x_final_model <- getXNameSelected(overview_table,met)
				}
			} else {
				out_final_stepwise <- getStepwiseWrapper(data_train, type = type, stra, met, sle, sls, weight = weight, x_name, y_name, intercept, include, test_method, sigma_value, feature_ratio)
				detail[[stra]][[met]] <- out_final_stepwise$pic_df
				overview_table <- out_final_stepwise$process_table
				remove_col <- NULL
				if(stra == "forward") {
					remove_col <- "EffectRemoved"
				} else if(stra == "backward") {
					remove_col <- "EffectEntered"
				}
				overview_table <- overview_table[,!colnames(overview_table) %in% remove_col]
				if(all(overview_table[,"NumberEffect"] == overview_table[,"NumberParams"])) {
					overview_table <- overview_table[,!colnames(overview_table) %in% "NumberEffect"]
				}
				x_final_model <- c(intercept, include, out_final_stepwise$x_in_model)
			}
			#overview_table[,met] <- overview_table[,met] %>% as.numeric() %>% round(num_digits) %>% as.character()
			overview_table[,met] <- as.numeric(overview_table[,met])
			# Apply round to numeric columns, then convert to character
			temp_table <- overview_table
			numeric_cols <- sapply(temp_table, is.numeric)
			temp_table[numeric_cols] <- lapply(temp_table[numeric_cols], round, num_digits)
			temp_table[numeric_cols] <- lapply(temp_table[numeric_cols], as.character)
			overview[[stra]][[met]] <- temp_table # to keep digits as we expected, convert numeric to character for html output.
			
			if(!(stra == "subset" & met == "SL")) {
				model_train <- getModel(data_train, type, intercept, c(x_final_model[!x_final_model %in% intercept]), y_name, weight, method = test_method)
				x_final_model_metric[[stra]][[met]] <- x_final_model
				if(type == "cox") {
				  model_performance <- cox_performance(data_train, data_test, stra, met, model_train, y_name, weight)
				} else if(type == "linear" | type == "gamma" |type == "negbin" | type == "poisson"){
				  model_performance <- lm_performance(data_train, data_test, type, stra, met, model_train, y_name, weight)
				} else if(type == "logit"){
				  model_performance <- logit_performance(data_train, data_test, stra, met, model_train, y_name, weight)
				}
			}
			model_performance_df <- rbind(model_performance_df ,model_performance)
		}
	}
	return(list('final_variable' = x_final_model_metric, 'performance' = model_performance_df, 'overview' = overview, "detail" = detail))
}

getTable4ModelCall <- function(type, intercept, include, x_final_model_metric, y_name, n_y, data, weight, test_method, num_digits) {
	table4_model_call <- list()
	for(stra in names(x_final_model_metric)) {
		x_final_model_strategy <- x_final_model_metric[[stra]]
		for(met in names(x_final_model_strategy)) {
			x_in_model <- x_final_model_strategy[[met]]
			table4_model_call[[stra]][[met]] <- getModel(data, type, intercept = NULL, x_name = c(x_in_model), y_name, weight = weight,	method = test_method)
		}
	}
	return(table4_model_call)
}

cox_performance <- function(data_train, data_test, strategy, metric, model_train, y_name, weight) {	
	# 1. Concordance Index
	cindex_train <- concordance(model_train)$concordance
	
	# Test data operations with error handling
	test_results <- tryCatch({
		y_vars <- sub("Surv\\((.*)\\)", "\\1", y_name)
		time_var <- trimws(strsplit(y_vars, ",")[[1]][1])
		status_var <- trimws(strsplit(y_vars, ",")[[1]][2])
		surv_obj <- Surv(data_test[,time_var], data_test[,status_var])
		lp_test <- predict(model_train, newdata = data_test, weights=weight)
		cindex_test <- concordance(surv_obj ~ lp_test, data = data_test)$concordance
		
		# 3. Time-Dependent AUC
		data <- rbind(data_train, data_test)
		times <- seq(min(data[,time_var]), max(data[,time_var]), by = round((max(data[,time_var]) - min(data[,time_var]))/100))
		lp_train <- predict(model_train, weights=weight)
		lp_test <- predict(model_train, newdata=data_test, weights=weight)
		Surv_rsp_train <- Surv(data_train[,time_var], data_train[,status_var])
		Surv_rsp_test <- Surv(data_test[,time_var], data_test[,status_var])
		
		auc_uno <- AUC.uno(Surv_rsp_train, Surv_rsp_test, lp_test, times)
		auc_sh <- AUC.sh(Surv_rsp_train, Surv_rsp_test, lp_train, lp_test, times)
		auc_hc <- AUC.hc(Surv_rsp_train, Surv_rsp_test, lp_test, times)
		
		list(cindex_test = cindex_test, auc_hc = auc_hc$iauc, auc_uno = auc_uno$iauc, auc_sh = auc_sh$iauc)
	}, error = function(e) {
		# Return NA values if test data operations fail
		list(cindex_test = NA, auc_hc = NA, auc_uno = NA, auc_sh = NA)
	})
	
	# 2. Integrated Brier Score (requires pec package)
	# Install if needed: install.packages("pec")
	# brier_train <- pec(list(model_train), formula = Surv(time, status) ~ 1, data = data_train, 
	#                    exact = TRUE, cens.model = "cox", splitMethod = "none")
	# brier_test <- pec(list(model_train), formula = Surv(time, status) ~ 1, data = data_test, 
	#                   exact = TRUE, cens.model = "cox", splitMethod = "none")
	# cat("Integrated Brier Score (Training):", mean(brier_train$AppErr), "\n")
	# cat("Integrated Brier Score (Test):", mean(brier_test$AppErr), "\n")
	
	model_performance <- data.frame(deparse1(model_train$formula), paste0(strategy,":",metric),
								   cindex_train, test_results$cindex_test, 
								   test_results$auc_hc, test_results$auc_uno, test_results$auc_sh)
	colnames(model_performance) <- c("model", "strategy:metric", "c-index_train", "c-index_test", "auc_hc", "auc_uno", "auc_sh")
	model_performance <- model_performance[,c(1:4,7)]
	return(model_performance)
}



lm_performance <- function(data_train, data_test, type, strategy, metric, model_train, y_name, weight) {	
	pred_train <- predict(model_train, newdata=data_train, weights=weight)

	if(is.matrix(pred_train)) {
		y_name <- colnames(pred_train)
	} 
	# Actual values
	actual_train <- data_train[,y_name]
	if(type == "linear"){
	  model_train_summary <- summary(model_train)
	} else {
	  model_train_summary <- model_train
	}
	
	# Test data operations with error handling
	test_results <- tryCatch({
		pred_test <- predict(model_train, newdata = data_test, weights=weight)
		actual_test <- data_test[,y_name]
		
		if(is.matrix(pred_train)) {
		  mse_test <- colMeans((actual_test - pred_test)^2)
		  rmse_test <- sqrt(mse_test)
		  mae_test <- colMeans(abs(actual_test - pred_test))
		  r2_test <- diag(cor(actual_test, pred_test)^2)
		  # n_test <- nrow(data_test)
		  # coef_df <- coef(model_train)
		  # p_test <- sum(!rownames(coef_df) %in% "(Intercept)")
		  # adj_r2_test <- 1 - ((1 - r2_test) * (n_test - 1) / (n_test - p_test - 1))
		} else {
		  mse_test <- mean((actual_test - pred_test)^2)
		  rmse_test <- sqrt(mse_test)
		  mae_test <- mean(abs(actual_test - pred_test))
		  if(type == "linear") {
		    r2_test <- cor(actual_test, pred_test)^2
		    # n_test <- nrow(data_test)
		    # coef_df <- coef(summary(model_train))
		    # p_test <- sum(!rownames(coef_df) %in% "(Intercept)")
		    # adj_r2_test <- 1 - ((1 - r2_test) * (n_test - 1) / (n_test - p_test - 1))
		  } else {
		    r2_test <- NA
		  }
		}
		list(pred_test = pred_test, actual_test = actual_test, mse_test = mse_test, 
			 rmse_test = rmse_test, mae_test = mae_test, r2_test = r2_test)
	}, error = function(e) {
		# Return NA values if test data operations fail
		list(pred_test = NA, actual_test = NA, mse_test = NA, 
			 rmse_test = NA, mae_test = NA, r2_test = NA)
	})
  
	if(is.matrix(pred_train)) {
	  mse_train <- colMeans((model_train$residuals)^2)
	  rmse_train <- sqrt(mse_train)
	  mae_train <- colMeans(abs(model_train$residuals))
	  #r2_train <- model_train_summary$adj.r.squared # cannot access adj.r.squared of two response
	  r2_train <- NULL
	  for(i in 1:dim(pred_train)[2]) {
	    r2_train <- append(r2_train, model_train_summary[[i]]$r.squared)
	  }
	  names(r2_train) <- y_name
	} else {
	  mse_train <- mean((model_train_summary$residuals)^2)
	  rmse_train <- sqrt(mse_train)
	  mae_train <- mean(abs(model_train_summary$residuals))
	  if(type == "linear") {
	    r2_train <- model_train_summary$r.squared
	  } else {
	    r2_train <- NA
	  }
	}

	model_performance <- data.frame(deparse1(model_train$call$formula), paste0(strategy,":",metric), 
								   r2_train, test_results$r2_test, 
								   mse_train, test_results$mse_test, 
								   mae_train, test_results$mae_test)
	if(is.matrix(pred_train)) {
	  model_performance$response <- y_name
	  colnames(model_performance) <- c("model", "strategy:metric", "r2_train", "r2_test", "mse_train", "mse_test", "mae_train", "mae_test", "response")
	} else {
	  colnames(model_performance) <- c("model", "strategy:metric", "r2_train", "r2_test", "mse_train", "mse_test", "mae_train", "mae_test")
	}
	#if(type != "linear") {
	  model_performance <- model_performance[,-c(3:4)]
	#}
	return(model_performance)
}



logit_performance <- function(data_train, data_test, strategy, metric, model_train, y_name, weight) {	
	# Predictions
	pred_prob_train <- predict(model_train, newdata=data_train, type = "response", weights=weight)
	
	# Test data operations with error handling
	test_results <- tryCatch({
		pred_prob_test <- predict(model_train, newdata = data_test, type = "response", weights=weight)
		pred_class_test <- ifelse(pred_prob_test > 0.5, 0, 1)
		actual_test <- data_test[,y_name]
		conf_mat_test <- table(Predicted = pred_class_test, Actual = actual_test)
		accuracy_test <- sum(diag(conf_mat_test)) / sum(conf_mat_test)
		auc_test <- tryCatch({
			auc(roc(actual_test, pred_prob_test))
		}, error = function(e) {
			NA  # Return NA if AUC calculation fails
		})
		log_loss_test <- -mean(actual_test * log(pred_prob_test + 1e-15) + (1 - actual_test) * log(1 - pred_prob_test + 1e-15))
		
		list(pred_prob_test = pred_prob_test, pred_class_test = pred_class_test, 
			 actual_test = actual_test, conf_mat_test = conf_mat_test, 
			 accuracy_test = accuracy_test, auc_test = auc_test, log_loss_test = log_loss_test)
	}, error = function(e) {
		# Return NA values if test data operations fail
		list(pred_prob_test = NA, pred_class_test = NA, 
			 actual_test = NA, conf_mat_test = NA, 
			 accuracy_test = NA, auc_test = NA, log_loss_test = NA)
	})
	
	pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

	# Actual values
	actual_train <- data_train[,y_name]

	# Metrics
	conf_mat_train <- table(Predicted = pred_class_train, Actual = actual_train)
	accuracy_train <- sum(diag(conf_mat_train)) / sum(conf_mat_train)
	
	# Calculate AUC with error handling
	auc_train <- tryCatch({
		auc(roc(actual_train, pred_prob_train))
	}, error = function(e) {
		NA  # Return NA if AUC calculation fails
	})
	
	log_loss_train <- -mean(actual_train * log(pred_prob_train + 1e-15) + (1 - actual_train) * log(1 - pred_prob_train + 1e-15))

	model_performance <- data.frame(deparse1(model_train$formula), paste0(strategy,":",metric), 
								   accuracy_train, test_results$accuracy_test, 
								   auc_train, test_results$auc_test, 
								   log_loss_train, test_results$log_loss_test)
	colnames(model_performance) <- c("model", "strategy:metric", "accuracy_train", "accuracy_test", "auc_train", "auc_test", "log_loss_train", "log_loss_test")
	return(model_performance)
}