######################################################################################
#                                                                                    #
# -- RPACT utilities --                                                              #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 25-09-2018                                                                   #
# Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD                             #
# Licensed under "GNU Lesser General Public License" version 3                       #
# License text can be found here: https://www.r-project.org/Licenses/LGPL-3          #
#                                                                                    #
# RPACT company website: https://www.rpact.com                                       #
# RPACT package website: https://www.rpact.org                                       #
#                                                                                    #
# Contact us for information about our services: info@rpact.com                      #
#                                                                                    #
######################################################################################

globalVariables(".parallelComputingCluster") 
globalVariables(".parallelComputingCaseNumbers") 
globalVariables(".parallelComputingArguments") 

.parallelComputingCluster <- NULL
.parallelComputingCaseNumbers <- NULL
.parallelComputingArguments <- NULL

.createParallelComputingCluster <- function() {
	if (!is.null(.parallelComputingCluster)) {
		return(TRUE)
	}
	
	if (requireNamespace("parallel", quietly = TRUE)) {
		startTime <- Sys.time()
		cores <- parallel::detectCores(logical = FALSE)
		if (is.na(cores) || cores < 2) {
			return(FALSE)
		}
		
		tryCatch({	
			.parallelComputingCluster <<- parallel::makeCluster(cores)
			.logProgress("Parallel computing cluster created with " + cores + " cores", startTime = startTime)
			return(TRUE)
		}, error = function(e) {
			.logWarn("Failed to create parallel computing cluster", e)
		})
	}
	
	return(FALSE)
}

.areEqualVectors <- function(v1, v2, tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	if (is.null(v1) || is.null(v2)) {
		return(FALSE)
	}
	
	if (length(v1) != length(v2)) {
		return(FALSE)
	}
	
	if (length(v1) == 0) {
		return(TRUE)
	}
	
	vec1 <- v1
	vec2 <- v2
	vec1[is.na(vec1)] <- -99999999999999
	vec2[is.na(vec2)] <- -99999999999999
	d <- nchar(as.character(1 / tolerance)) - 1
	vec1 <- round(vec1, d)
	vec2 <- round(vec2, d)
	
	return(sum(vec1 == vec2) == length(vec1))
}

#.areEqualVectors(c(0.152206629, 0.165328755, 0.002777922, NA), c(0.152206631, 0.165328753, 0.002777917, NA), tolerance = 1e-08)

.toCapitalized <- function(x, ignoreBlackList = FALSE) {
	if (is.null(x) || is.na(x) || !is.character(x)) {
		return(x)
	}
	
	if (!ignoreBlackList) {
		if (x %in% c("pi1", "pi2")) {
			return(x)
		}
	}
	
	s <- strsplit(x, " ")[[1]]
	s <- paste0(toupper(substring(s, 1,1)), substring(s, 2))
	wordsToExclude <- c("And", "The", "Of", "Or")
	s[s %in% wordsToExclude] <- tolower(s[s %in% wordsToExclude])
	s <- paste(s, collapse=" ")
	return(s)
}

.equalsRegexpIgnoreCase <- function(x, pattern) {
	x <- tolower(x)
	pattern <- tolower(pattern)
	result <- grep(pattern, x)
	return(sum(result) > 0)
}

# 
# @title 
# Get Optional Argument
# 
# @description 
# Returns the value of an optional argument if it exists.
#
# @param optionalArgumentName the name of the optional argument.
# 
# @details 
# Internal function.
# 
# @return the value of the optional argument if it exists; NULL otherwise.
# 
# @examples
# 
# f = function(...) {
# 	print(.getOptionalArgument("x", ...))
# }
# 
# > f(x = 1)
# [1] 1
# 
# > f(y = 1)
# NULL
# 
# @keywords internal
#
.getOptionalArgument <- function(optionalArgumentName, ...) {
	args <- list(...)
	if (optionalArgumentName %in% names(args)) {
		return(args[[optionalArgumentName]])
	}
	
	return(NULL)
}

.isUndefinedArgument <- function(arg) {		
	if (missing(arg) || is.null(arg)) {
		return(TRUE)
	}
	
	tryCatch({	
		if (length(arg) == 0) {
			return(TRUE)
		}
		
		if (length(arg) > 1) {
			return(FALSE)
		}
	}, error = function(e) {
		paramName <- deparse(substitute(arg))
		.logWarn("Failed to execute '.isUndefinedArgument(%s)' ('%s' is an instance of class '%s'): %s", 
			paramName, paramName, class(arg), e)
	})
	
	return(is.na(arg))
}

.isDefinedArgument <- function(arg, argumentExistsValidationEnabled = TRUE) {	
	paramName <- deparse(substitute(arg))
	if (argumentExistsValidationEnabled && 
			length(grep("\\$|\\[|\\]", paramName)) == 0 && !exists(paramName)) {	
		tryCatch({	
			if (missing(arg) || is.null(arg)) {
				return(FALSE)
			}
		}, error = function(e) {
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
				"the object '", paramName, "' has not been defined anywhere. ",
				"Please define it first, e.g., run '", paramName, " <- 1'")
		})
	}
	
	if (missing(arg) || is.null(arg)) {
		return(FALSE)
	}
	
	tryCatch({	
		if (length(arg) == 0) {
			return(FALSE)
		}
		
		if (length(arg) > 1) {
			return(TRUE)
		}
	}, error = function(e) {
		paramName <- deparse(substitute(arg))
		.logWarn("Failed to execute '.isDefinedArgument(%s)' ('%s' is an instance of class '%s'): %s", 
			paramName, paramName, class(arg), e)
	})
	
	return(!is.na(arg))
	
}

.arrayToString <- function(a, separator = ", ", vectorLookAndFeelEnabled = FALSE, encapsulate = FALSE) {	
	if (missing(a) || is.null(a) || length(a) == 0) {
		return("NULL")
	}
	
	if (length(a) == 1 && is.na(a)) {
		return("NA")
	}
	
	if (encapsulate) {
		a <- paste0("'", a, "'")
	}
		
	if (!vectorLookAndFeelEnabled) {
		return(paste(a, collapse = separator))
	}
	
	return(paste0("c(", paste(a, collapse = separator), ")"))
}

.listToString <- function(a, separator = ", ", listLookAndFeelEnabled = FALSE, encapsulate = FALSE) {	
	if (missing(a) || is.null(a) || length(a) == 0) {
		return("NULL")
	}
	
	if (length(a) == 1 && is.na(a)) {
		return("NA")
	}
	
	result <- ""
	for (name in names(a)) {
		value <- a[[name]]
		if (encapsulate) {
			value <- paste0("'", value, "'")
		}
		entry <- paste(name, "=", value)
		if (nchar(result) > 0) {
			result <- paste(result, entry, sep = ", ")
		} else {
			result <- entry
		}
	}
	
	if (!listLookAndFeelEnabled) {
		return(result)
	}
	
	return(paste0("list(", result, ")"))
}

# 
# @title 
# Set Seed
# 
# @description 
# Sets the seed, generates it if \code{is.na(seed) == TRUE} and returns it.
#
# @param seed the seed to set.
# 
# @details 
# Internal function.
# 
# @return the (generated) seed.
# 
# @examples
# 
# .setSeed(12345)
# 
# mySeed <- .setSeed()
# 
# @keywords internal
#
.setSeed <- function(seed = NA_real_) {
	if (!is.null(seed) && !is.na(seed)) {
		set.seed(seed)
		return(seed)
	}
		
	if (exists(".Random.seed") && length(.Random.seed) > 0) {
		seed <- .Random.seed[length(.Random.seed)]
	} else {
		seed <- round(stats::runif(1) * 1e8)
	}
	
	.logDebug("Set seed to %s", seed)
	
	tryCatch({	
		set.seed(seed)
	}, error = function(e) {
		.logError("Failed to set seed to '%s' (%s): %s", seed, class(seed), e)
		seed <- NA_real_
		traceback()
	})
	
	invisible(seed)
}

.getInputForZeroOutputInsideTolerance <- function(input, output, tolerance = .Machine$double.eps^0.25) {
	if (is.null(tolerance) || is.na(tolerance)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'tolerance' must be a valid double")
	}
	
	if (tolerance < 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'tolerance' (", tolerance, ") must be >= 0")
	}
	
	if (is.null(input)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'input' must be a valid double or NA")
	}
	
	if (is.null(output) || is.na(output)) {
		return(NA_real_)
	}
	
	if (abs(output) <= tolerance) {
		return(input)
	}
	
	return(NA_real_)
}

.getInputProducingZeroOutput <- function(input1, output1, input2, output2, 
		tolerance = .Machine$double.eps^0.25) {
	
	if ((is.na(output1) || is.null(output1))
			&& (is.na(output2) || is.null(output2))) {
		return(NA_real_)
	}
	
	if (is.na(output1) || is.null(output1)) {
		return(.getInputForZeroOutputInsideTolerance(input2, output2, tolerance))
	}
	
	if (is.na(output2) || is.null(output2)) {
		return(.getInputForZeroOutputInsideTolerance(input1, output1, tolerance))
	}
	
	if (abs(output1) <= abs(output2) && !is.na(input1)) {
		return(.getInputForZeroOutputInsideTolerance(input1, output1, tolerance))
	}
	
	return(.getInputForZeroOutputInsideTolerance(input2, output2, tolerance))
}

# 
# @title 
# Get One Dimensional Root
# 
# @description 
# Searches and returns the one dimensional root of a function using \code{uniroot}.
#
# @param acceptResultsOutOfTolerance if \code{TRUE}, results will be accepted in any case; 
#        if \code{FALSE}, \code{NA_real_} will be returned in case of tolerance discrepancy
# 
# @details 
# Internal function.
# 
# @return the root.
# 
# @keywords internal
#
.getOneDimensionalRoot <- function(
	f,
	...,
	lower,
	upper,
	tolerance = .Machine$double.eps^0.25,
	acceptResultsOutOfTolerance = FALSE,
	suppressWarnings = FALSE) {
	
	.assertIsSingleNumber(lower, "lower")
	.assertIsSingleNumber(upper, "upper")
	.assertIsSingleNumber(tolerance, "tolerance")	
	
	resultLower <- f(lower, ...)
	resultUpper <- f(upper, ...)
	result <- .getInputProducingZeroOutput(lower, resultLower, upper, resultUpper, tolerance)
	if (!is.na(result)) {	
		return(result)
	}
	
	unirootResult <- NULL
	tryCatch({	
		unirootResult <- stats::uniroot(f = f, lower = lower, upper = upper, 
			tol = tolerance, trace = 2, extendInt = "no", ...)
	}, warning = function(w) {
		.logWarn("uniroot(f, lower = %s, upper = %s, tol = %s) produced a warning: %s", 
			lower, upper, tolerance, w)
	}, error = function(e) {
		msg <- "Failed to run uniroot(f, lower = %s, upper = %s, tol = %s): %s"
		if (C_LOG_LEVEL == C_LOG_LEVEL_DEBUG) {
			.logError(msg, lower, upper, tolerance, e)
		} else {
			.logWarn(msg, lower, upper, tolerance, e)
		}
	})

	if (is.null(unirootResult)) {
		direction <- ifelse(f(lower) < f(upper), 1, -1)
		if (is.na(direction)) {
			return(NA_real_)
		}
		
		return(.getOneDimensionalRootBisectionMethod(f = f, 
			lower = lower, upper = upper, tolerance = tolerance, 
			acceptResultsOutOfTolerance = acceptResultsOutOfTolerance, direction = direction,
			suppressWarnings = suppressWarnings))
	}
	
	if (is.infinite(unirootResult$f.root) || abs(unirootResult$f.root) > max(tolerance * 100, 1e-07)) {		
		if (!acceptResultsOutOfTolerance) {
			if (!suppressWarnings) {
				warning("NA returned because root search by 'uniroot' produced a function result (", 
					unirootResult$f.root, ") that differs from target 0 ", 
					"(tolerance is ", tolerance, ", last function argument was ", unirootResult$root, ")", 
					call. = FALSE)
			}
			return(NA_real_)
		} else if (!suppressWarnings) {
			warning("Root search by 'uniroot' produced a function result (", unirootResult$f.root, ") ", 
				"that differs from target 0 ", 
				"(tolerance is ", tolerance, ", last function argument was ", unirootResult$root, ")", 
				call. = FALSE)
		}
	}
	
	return(unirootResult$root)
}

#
# @title 
# Get One Dimensional Root Bisection Method
# 
# @description 
# Searches and returns the one dimensional root of a function using the bisection method.
# 
# @param acceptResultsOutOfTolerance if \code{TRUE}, results will be accepted in any case; 
#        if \code{FALSE}, \code{NA_real_} will be returned in case of tolerance discrepancy
# 
# @details 
# Internal function.
# 
# @keywords internal
# 
.getOneDimensionalRootBisectionMethod <- function(
		f, ..., lower, upper, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT, 
		acceptResultsOutOfTolerance = FALSE,
		maxSearchIterations = 50,
		direction = 0,
		suppressWarnings = FALSE) {
	
	lowerStart <- lower
	upperStart <- upper
	
	if (direction == 0) {
		direction <- ifelse(f(lower) < f(upper), 1, -1)
	}

	.logTrace("Start special root search: lower = %s, upper = %s, tolerance = %s, direction = %s", 
		lower, upper, tolerance, direction)
		
	precision <- 1
	while (precision > tolerance) {
		argument <- (lower + upper) / 2
		result <- f(argument)
		
		.logTrace("Root search step: f(%s, lower = %s, upper = %s, direction = %s) = %s", 
			argument, lower, upper, direction, result)
		
		ifelse(result * direction < 0, lower <- argument, upper <- argument)
		
		maxSearchIterations <- maxSearchIterations - 1
		if (maxSearchIterations < 0) {
			if (!suppressWarnings) {
				warning("Root search via 'bisection' stopped: maximum number of search iterations reached. ",
					"Check if lower and upper search bounds were calculated correctly", 
					call. = FALSE)	
			}
			.plotMonotoneFunctionRootSearch(f, lowerStart, upperStart)
			return(NA_real_)
		}
		
		precision <- upper - lower
	}
	
	if (is.infinite(result) || abs(result) > max(tolerance * 100, 1e-07)) { # 0.01) { # tolerance * 20
		.plotMonotoneFunctionRootSearch(f, lowerStart, upperStart)
	
		if (!acceptResultsOutOfTolerance) {
			if (!suppressWarnings) {
				warning("NA returned because root search via 'bisection' produced a function result (", 
					result, ") that differs from target 0 ", 
					"(tolerance is ", tolerance, ", last function argument was ", argument, ")", 
					call. = FALSE)
			}
			return(NA_real_)
		} else if (!suppressWarnings) {
			warning("Root search via 'bisection' produced a function result (", result, ") ",
				"that differs from target 0 ", 
				"(tolerance is ", tolerance, ", last function argument was ", argument, ")", 
				call. = FALSE)
		}
	}
	
	return(argument)
}

.plotMonotoneFunctionRootSearch <- function(f, lowerStart, upperStart) {
	if (C_LOG_LEVEL != C_LOG_LEVEL_TRACE) {
		return()
	}
	
	values <- c()
	params <- seq(from = lowerStart, to = upperStart, by = (upperStart - lowerStart) / 20)
	for (i in params) {
		values <- c(values, f(i))
	}
	graphics::plot(params, values)
}

.getTextLineWithLineBreak <- function(line, lineBreakIndex) {
	index <- .getSpaceIndex(line, lineBreakIndex)
	if (index == -1) {
		return(line)
	}
	
	a <- substr(line, 0, index - 1)
	b <- substr(line, index + 1, nchar(line))
	return(paste0(a, "\n", b))
}

.getSpaceIndex <- function(line, lineBreakIndex) {
	if (nchar(line) <= lineBreakIndex) {
		return(-1)
	}
	
	if (regexpr('\\n', line) > 0) {
		return(-1)
	}
	
	len <- nchar(line)
	lineSplit <- strsplit(line, "")[[1]]
	for (i in (len/2):length(lineSplit)) {
		char <- lineSplit[i]
		if (char == " ") {
			return(i)
		}
	}
	return(-1)
}

.getRelativeFigureOutputPath <- function(subDir = NULL) {
	if (is.null(subDir)) {
		today <- Sys.Date()
		subDir <- format(today, format="%Y-%m-%d")
	}
	
	figPath <- getwd()
	subDirs <- c("_examples", "output", "figures", subDir)
	for (s in subDirs) {
		figPath <- file.path(figPath, s)
		if (!file.exists(figPath)) {
			dir.create(figPath, showWarnings = FALSE)
		}  
	}
	 
	return(figPath)
}

# @title 
# Save Last Plot
# 
# @description 
# Saves the last plot to a PNG file located in '[getwd()]/figures/[current date]/[filename].png'.
# 
# @param filename The filename (without extension!).
# 
# @details 
# This is a wrapper function that creates a output path and uses \code{ggsave} to save the last plot.
# 
# @examples
# 
# # saveLastPlot('my_plot') 
# 
# @keywords internal
#
saveLastPlot <- function(filename, outputPath = .getRelativeFigureOutputPath()) {
	.assertGgplotIsInstalled()
	
	path <- file.path(outputPath, paste0(filename, ".png"))
	ggplot2::ggsave(filename = path, 
		plot = ggplot2::last_plot(), device = NULL, path = NULL,
		scale = 1.2, width = 16, height = 15, units = "cm", dpi = 600, limitsize = TRUE)
	
	cat("Last plot was saved to '", path, "'\n")
}

.isFirstValueGreaterThanSecondValue <- function(firstValue, secondValue) {
	if (is.null(firstValue) || is.na(firstValue) || length(firstValue) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'firstValue' (", firstValue, ") must be a valid numeric value")
	}
	if (is.null(secondValue) || is.na(secondValue) || length(secondValue) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'secondValue' (", secondValue, ") must be a valid numeric value")
	}
	return(firstValue > secondValue)
}

.isFirstValueSmallerThanSecondValue <- function(firstValue, secondValue) {
	if (is.null(firstValue) || is.na(firstValue) || length(firstValue) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'firstValue' (", firstValue, ") must be a valid numeric value")
	}
	if (is.null(secondValue) || is.na(secondValue) || length(secondValue) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'secondValue' (", secondValue, ") must be a valid numeric value")
	}
	return(firstValue < secondValue)
}

.logBase <- function(s, ..., logLevel) {
	if (length(list(...)) > 0) {
		cat(paste0("[", logLevel, "]"), sprintf(s, ...), "\n")
	} else {
		cat(paste0("[", logLevel, "]"), s, "\n")
	}
}

.logTrace <- function(s, ...) {
	if (C_LOG_LEVEL == C_LOG_LEVEL_TRACE) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_TRACE)
	}
}

.logDebug <- function(s, ...) {
	if (C_LOG_LEVEL %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_DEBUG)
	}
}

.logInfo <- function(s, ...) {
	if (C_LOG_LEVEL %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, C_LOG_LEVEL_INFO)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_INFO)
	}
}

.logWarn <- function(s, ...) {
	if (C_LOG_LEVEL %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_WARN)
	}
}

.logError <- function(s, ...) {
	if (C_LOG_LEVEL %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN, C_LOG_LEVEL_ERROR)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_ERROR)
	}
}

.logProgress <- function(s, ..., startTime) {
	if (!(C_LOG_LEVEL %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN, 
			C_LOG_LEVEL_ERROR, C_LOG_LEVEL_PROGRESS))) {
		return()
	}
	
	time <- Sys.time() - startTime	
	timeStr <- paste0("[", round(as.numeric(time), 4), " secs]")
	if (length(list(...)) > 0) {
		cat(paste0("[", C_LOG_LEVEL_PROGRESS, "]"), sprintf(s, ...), timeStr, "\n")
	} else {
		cat(paste0("[", C_LOG_LEVEL_PROGRESS, "]"), s, timeStr, "\n")
	}
}

##
## -- Design utilities
##

.getValidatedFutilityBounds <- function(design, kMaxLowerBound = 1, writeToDesign = TRUE) {
	.assertIsTrialDesignInverseNormalOrGroupSequential(design)
	return(.getValidatedFutilityBoundsOrAlpha0Vec(design = design, parameterName = "futilityBounds", 
			defaultValue = C_FUTILITY_BOUNDS_DEFAULT, kMaxLowerBound = kMaxLowerBound,
			writeToDesign = writeToDesign))
}

.getValidatedAlpha0Vec <- function(design, kMaxLowerBound = 1, writeToDesign = TRUE) {
	.assertIsTrialDesignFisher(design)
	return(.getValidatedFutilityBoundsOrAlpha0Vec(design = design, parameterName = "alpha0Vec", 
			defaultValue = C_ALPHA_0_VEC_DEFAULT, kMaxLowerBound = kMaxLowerBound,
			writeToDesign = writeToDesign))
}

.getValidatedFutilityBoundsOrAlpha0Vec <- function(design, parameterName, defaultValue, 
	kMaxLowerBound, writeToDesign) {
	
	kMaxUpperBound <- ifelse(.isTrialDesignFisher(design), C_KMAX_UPPER_BOUND_FISHER, C_KMAX_UPPER_BOUND)
	if (.isDefinedArgument(design[[parameterName]]) && .isDefinedArgument(design$kMax)) {
		if (.isTrialDesignFisher(design)) {
			.assertIsValidAlpha0Vec(design[[parameterName]], kMax = design$kMax, 
				kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
		} else {
			.assertAreValidFutilityBounds(design[[parameterName]], kMax = design$kMax, 
				kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
		}
	}
	
	if (writeToDesign) {
		.setParameterType(design, parameterName, C_PARAM_USER_DEFINED)
	}
	
	if (.isUndefinedArgument(design$informationRates) && .isUndefinedArgument(design[[parameterName]])) { 
		if (writeToDesign) {
			if (.setKMaxToDefaultIfUndefined(design, writeToDesign) || design$kMax == C_KMAX_DEFAULT) {
				.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
			} else {
				.setParameterType(design, parameterName, C_PARAM_DERIVED)
			}
		}
		
		return(rep(defaultValue, design$kMax - 1))
	}
	
	if (.isDefinedArgument(design$informationRates) && .isUndefinedArgument(design[[parameterName]])) {
		if (writeToDesign) {
			if (.isUndefinedArgument(design$kMax)) {
				.setKMax(design, kMax = length(design$informationRates))
			}
			.setParameterType(design, parameterName, ifelse(design$kMax == C_KMAX_DEFAULT, 
					C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED))
		}
		return(rep(defaultValue, design$kMax - 1))
	}
	
	if (.isUndefinedArgument(design$informationRates) && 
			.isDefinedArgument(design[[parameterName]], argumentExistsValidationEnabled = FALSE)) {	
		if (writeToDesign) {
			.setKMax(design, kMax = length(design[[parameterName]]) + 1)
			if (.isDefaultVector(design[[parameterName]], rep(defaultValue, design$kMax - 1))) {
				.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
			}
		}
		
		if (.isBetaSpendingDesignWithDefinedFutilityBounds(design, parameterName, writeToDesign)) {
			return(rep(defaultValue, design$kMax - 1))
		}
		
		return(design[[parameterName]])
	}
	
	if (writeToDesign) {
		.setKMax(design, kMax = length(design[[parameterName]]) + 1)
		if (.isDefaultVector(design[[parameterName]], rep(defaultValue, design$kMax - 1))) {
			.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
		}
	}
	
	if (.isTrialDesignFisher(design)) {
		.assertIsValidAlpha0Vec(design[[parameterName]], kMax = design$kMax, 
			kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	} else {
		.assertAreValidFutilityBounds(design[[parameterName]], kMax = design$kMax, 
			kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	}
	
	if (.isBetaSpendingDesignWithDefinedFutilityBounds(design, parameterName, writeToDesign)) {
		return(rep(defaultValue, design$kMax - 1))
	}
	
	return(design[[parameterName]])
}

.isBetaSpendingDesignWithDefinedFutilityBounds <- function(design, parameterName, writeToDesign) {
	if (.isTrialDesignFisher(design) || !isBetaSpendingDesignType(design$typeBetaSpending)) {
		return(FALSE)
	}
	
	if (design$.getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
		warning(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
			"binding futility bounds specified in '", parameterName, "' are ignored", 
			call. = FALSE)
	}
	else if (design$.getParameterType(parameterName) == C_PARAM_GENERATED) {
		return(FALSE)
	}
	
	if (writeToDesign) {
		.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
	}
	return(TRUE)
}

.setParameterType <- function(design, parameterName, parameterType) {
	if (is.null(design)) {
		return()
	}
	
	design$.setParameterType(parameterName, parameterType)
}

.setKMax <- function(design, kMax) {
	design$kMax <- as.integer(kMax)
	.setParameterType(design, "kMax", C_PARAM_DERIVED)
	invisible(kMax)
}

.getValidatedInformationRates <- function(design, kMaxLowerBound = 1, writeToDesign = TRUE) {
	
	kMaxUpperBound <- ifelse(.isTrialDesignFisher(design), C_KMAX_UPPER_BOUND_FISHER, C_KMAX_UPPER_BOUND)
	if (.isDefinedArgument(design$informationRates) && .isDefinedArgument(design$kMax)) {
		.assertAreValidInformationRates(informationRates = design$informationRates, 
			kMax = design$kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	}
	
	.setParameterType(design, "informationRates", C_PARAM_USER_DEFINED)
	
	if (.isTrialDesignFisher(design)) {
		futilityBounds <- design$alpha0Vec
	} else {
		futilityBounds <- design$futilityBounds
	}
	
	if (.isUndefinedArgument(design$informationRates) && .isUndefinedArgument(futilityBounds)) { 
		if (writeToDesign) {
			if (.setKMaxToDefaultIfUndefined(design, writeToDesign) || design$kMax == C_KMAX_DEFAULT) {
				.setParameterType(design, "informationRates", C_PARAM_DEFAULT_VALUE)
			} else {
				.setParameterType(design, "informationRates", C_PARAM_DERIVED)
			}
		}
		return((1:design$kMax) / design$kMax)
	}
	
	if (.isDefinedArgument(design$informationRates) && .isUndefinedArgument(futilityBounds)) {
		if (writeToDesign) {
			.setKMax(design, kMax = length(design$informationRates))
			if (.isDefaultVector(design$informationRates, (1:design$kMax) / design$kMax)) {
				.setParameterType(design, "informationRates", C_PARAM_DEFAULT_VALUE)
			}
		}
		.assertAreValidInformationRates(informationRates = design$informationRates, 
			kMax = design$kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
		return(design$informationRates)
	}
	
	if (.isUndefinedArgument(design$informationRates) && 
			.isDefinedArgument(futilityBounds, argumentExistsValidationEnabled = FALSE)) {
		if (writeToDesign) {
			if (.isUndefinedArgument(design$kMax)) {
				.setKMax(design, kMax = length(futilityBounds) + 1)
			}
			.setParameterType(design, "informationRates", ifelse(design$kMax == C_KMAX_DEFAULT, 
					C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED))
		}
		return((1:design$kMax) / design$kMax)
	}
	
	if (writeToDesign) {
		.setKMax(design, kMax = length(design$informationRates))
		if (.isDefaultVector(design$informationRates, (1:design$kMax) / design$kMax)) {
			.setParameterType(design, "informationRates", C_PARAM_DEFAULT_VALUE)
		}
	}
	
	.assertAreValidInformationRates(informationRates = design$informationRates, 
		kMax = design$kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	
	return(design$informationRates)
}

.setKMaxToDefaultIfUndefined <- function(design, writeToDesign = TRUE) {
	if (writeToDesign && .isUndefinedArgument(design$kMax)) {
		design$kMax <- C_KMAX_DEFAULT
		design$.setParameterType("kMax", C_PARAM_DEFAULT_VALUE)
		return(TRUE)
	}
	return(FALSE)
}

.isDefaultVector <- function(x, default) {
	if (length(x) != length(default)) {
		return(FALSE)
	}
	
	return(sum(x == default) == length(x))
}

.validateAlphaAndBeta <- function(design) {
	.assertDesignParameterExists(design, "alpha", C_ALPHA_DEFAULT)
	.assertDesignParameterExists(design, "beta", C_BETA_DEFAULT)
	.assertIsValidAlphaAndBeta(alpha = design$alpha, beta = design$beta)
}

.validateUserAlphaSpending <- function(design) {
	.assertIsTrialDesign(design)
	
	.assertDesignParameterExists(design, "userAlphaSpending", NA_real_)
	
	design$.setParameterType("userAlphaSpending", C_PARAM_USER_DEFINED)
	
	if ((design$isUserDefinedParameter("informationRates") || 
			(design$isDefaultParameter("informationRates") && !design$isUserDefinedParameter("kMax"))) &&
		length(design$informationRates) != length(design$userAlphaSpending)) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"length of 'userAlphaSpending' (%s) must be equal to length of 'informationRates' (%s)"), 
				length(design$userAlphaSpending), length(design$informationRates)))
	}
	
	if (length(design$userAlphaSpending) != design$kMax) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"length of 'userAlphaSpending' (%s) must be equal to 'kMax' (%s)"), 
				length(design$userAlphaSpending), design$kMax))
	}
	
	.validateUserAlphaSpendingLength(design)
	
	if (.isUndefinedArgument(design$alpha)) {
		design$alpha <- design$userAlphaSpending[design$kMax]
		design$.setParameterType("alpha", ifelse(design$alpha == C_ALPHA_DEFAULT, 
				C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED))
	}
	
	.assertIsValidAlpha(design$alpha)
	
	if (design$kMax > 1 && (design$userAlphaSpending[1] < 0 || design$userAlphaSpending[design$kMax] > design$alpha ||
			any(design$userAlphaSpending[2:design$kMax] - design$userAlphaSpending[1:(design$kMax - 1)] < 0))) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
					"'userAlphaSpending' = %s must be a vector that satisfies the following condition: ", 
					"0 <= alpha_1 <= .. <= alpha_%s <= alpha = %s"), 
				.arrayToString(design$userAlphaSpending, vectorLookAndFeelEnabled = TRUE), 
				design$kMax, design$alpha))
	}
}

.validateUserBetaSpending <- function(design) {
	.assertIsTrialDesign(design)
	
	.assertDesignParameterExists(design, "userBetaSpending", NA_real_)
	
	design$.setParameterType("userBetaSpending", C_PARAM_USER_DEFINED)
	
	if ((design$isUserDefinedParameter("informationRates") || 
			(design$isDefaultParameter("informationRates") && !design$isUserDefinedParameter("kMax"))) &&
		length(design$informationRates) != length(design$userBetaSpending)) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"length of 'userBetaSpending' (%s) must be equal to length of 'informationRates' (%s)"), 
				length(design$userBetaSpending), length(design$informationRates)))
	}
	
	if (length(design$userBetaSpending) != design$kMax) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"length of 'userBetaSpending' (%s) must be equal to 'kMax' (%s)"), 
				length(design$userBetaSpending), design$kMax))
	}
	
	if (length(design$userBetaSpending) < 2 || length(design$userBetaSpending) > C_KMAX_UPPER_BOUND) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
					"length of 'userBetaSpending' (%s) is out of bounds [2; %s]"), 
				length(design$userBetaSpending), C_KMAX_UPPER_BOUND))
	}
	
	if (.isUndefinedArgument(design$beta)) {
		design$beta <- design$userBetaSpending[design$kMax]
		design$.setParameterType("beta", ifelse(design$beta == C_BETA_DEFAULT, 
				C_PARAM_DEFAULT_VALUE, C_PARAM_DERIVED))
	}
	
	.assertIsValidBeta(beta = design$beta, alpha = design$alpha)
	
	if (design$kMax > 1 && (design$userBetaSpending[1] < 0 || design$userBetaSpending[design$kMax] > design$beta ||
			any(design$userBetaSpending[2:design$kMax] - design$userBetaSpending[1:(design$kMax - 1)] < 0))) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
					"'userBetaSpending' = %s must be a vector that satisfies the following condition: ", 
					"0 <= beta_1 <= .. <= beta_%s <= beta = %s"), 
				.arrayToString(design$userBetaSpending, vectorLookAndFeelEnabled = TRUE), 
				design$kMax, design$beta))
	}
}

.validateUserAlphaSpendingLength <- function(design) {
	if (length(design$userAlphaSpending) < 1 || length(design$userAlphaSpending) > C_KMAX_UPPER_BOUND) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
					"length of 'userAlphaSpending' (%s) is out of bounds [1; %s]"), 
				length(design$userAlphaSpending), C_KMAX_UPPER_BOUND))
	}
}

.setKmaxBasedOnAlphaSpendingDefintion <- function(design) {
	
	if (.isTrialDesignFisher(design)) {
		if (design$method != C_FISHER_METHOD_USER_DEFINED_ALPHA) {
			return()
		}
	} else {
		if (design$typeOfDesign != C_TYPE_OF_DESIGN_AS_USER) {
			return()
		}
	}
	
	if (.isDefinedArgument(design$kMax)) {
		return()
	}
	
	if (.isUndefinedArgument(design$userAlphaSpending)) {
		return()
	}
	
	if (.isDefinedArgument(design$informationRates)) {
		return()
	}
	
	if (.isTrialDesignFisher(design)) {
		if (.isDefinedArgument(design$alpha0Vec)) {
			return()
		}
	} else {
		if (.isDefinedArgument(design$futilityBounds)) {
			return()
		}
	}
	
	.validateUserAlphaSpendingLength(design)
	
	.setKMax(design, kMax = length(design$userAlphaSpending))
}

