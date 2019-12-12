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

#' @include f_core_constants.R
NULL

utils::globalVariables(".parallelComputingCluster") 
utils::globalVariables(".parallelComputingCaseNumbers") 
utils::globalVariables(".parallelComputingArguments") 

.parallelComputingCluster <- NULL
.parallelComputingCaseNumbers <- NULL
.parallelComputingArguments <- NULL

#'
#' @title
#' Set Log Level
#'
#' @description
#' Sets the \code{rpact} log level. 
#' 
#' @param logLevel The new log level to set. Can be one of
#'        "PROGRESS", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "DISABLED". 
#' 
#' @details
#' This function is intended for debugging purposes only.
#'
#' @export
#' 
#' @keywords internal
#' 
#' @examples 
#' 
#' \dontrun{
#' setLogLevel("DEBUG")
#' }
#'
setLogLevel <- function(logLevel = c("PROGRESS", "ERROR", "WARN", 
		"INFO", "DEBUG", "TRACE", "DISABLED")) {
	
	logLevel <- match.arg(logLevel)
	
	if (!is.character(logLevel) || !(logLevel %in% c(
			C_LOG_LEVEL_TRACE,
			C_LOG_LEVEL_DEBUG,
			C_LOG_LEVEL_INFO,
			C_LOG_LEVEL_WARN,
			C_LOG_LEVEL_ERROR,
			C_LOG_LEVEL_PROGRESS,
			C_LOG_LEVEL_DISABLED))) {
		
		stop("Illegal argument: 'logLevel' must be one of ",
			"c(", paste(paste0("'", c(C_LOG_LEVEL_TRACE, 
						C_LOG_LEVEL_DEBUG, 
						C_LOG_LEVEL_INFO, 
						C_LOG_LEVEL_WARN, 
						C_LOG_LEVEL_ERROR, 
						C_LOG_LEVEL_PROGRESS, 
						C_LOG_LEVEL_DISABLED), "'"), collapse = ", "), ")")
	}
	
	Sys.setenv("RPACT_LOG_LEVEL" = logLevel)
}

#'
#' @title
#' Get Log Level
#'
#' @description
#' Returns the current \code{rpact} log level. 
#' 
#' @details
#' This function is intended for debugging purposes only.
#'
#' @export
#' 
#' @keywords internal
#' 
#' @examples 
#' 
#' \dontrun{
#' getLogLevel()
#' }
#'
getLogLevel <- function() {
	logLevel <- Sys.getenv("RPACT_LOG_LEVEL")
	if (logLevel == "") {
		logLevel <- C_LOG_LEVEL_PROGRESS
		Sys.setenv("RPACT_LOG_LEVEL" = logLevel)
	}
	return(logLevel)
}

#'
#' @title
#' Reset Log Level
#'
#' @description
#' Resets the \code{rpact} log level. 
#' 
#' @details
#' This function is intended for debugging purposes only.
#'
#' @export
#' 
#' @keywords internal
#' 
#' @examples 
#' 
#' \dontrun{
#' resetLogLevel()
#' }
#'
resetLogLevel <- function() {
	setLogLevel(C_LOG_LEVEL_PROGRESS)
}

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
	s <- paste0(toupper(substring(s, 1, 1)), substring(s, 2))
	wordsToExclude <- c("And", "The", "Of", "Or")
	s[s %in% wordsToExclude] <- tolower(s[s %in% wordsToExclude])
	s <- paste(s, collapse = " ")
	return(s)
}

.firstCharacterToUpperCase <- function(x) {
	substr(x, 1, 1) <- toupper(substr(x, 1, 1))
	return(x)
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

.arrayToString <- function(x, separator = ", ", 
		vectorLookAndFeelEnabled = FALSE, 
		encapsulate = FALSE,
		digits = 3) {
		
	if (digits < 0) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'digits' (", digits, ") must be >= 0")
	}
		
	if (missing(x) || is.null(x) || length(x) == 0) {
		return("NULL")
	}
	
	if (length(x) == 1 && is.na(x)) {
		return("NA")
	}
	
	if (!is.numeric(x) && !is.character(x) && !is.logical(x) && !is.integer(x)) {
		return(class(x))
	}
	
	if (is.numeric(x)) {
		if (digits > 0) {
			indices <- which(!is.na(x) & abs(x) >= 10^-digits)
		} else {
			indices <- which(!is.na(x))
		}
		x[indices] <- round(x[indices], digits)
	}
	
	if (encapsulate) {
		x <- paste0("'", x, "'")
	}
		
	if (!vectorLookAndFeelEnabled) {
		return(paste(x, collapse = separator))
	}
	
	return(paste0("c(", paste(x, collapse = separator), ")"))
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
		if (is.na(as.integer(seed))) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'seed' must be a valid integer")
		}
		
		set.seed(seed = seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
		return(seed)
	}
		
	if (exists(".Random.seed") && length(.Random.seed) > 0) {
		seed <- .Random.seed[length(.Random.seed)]
	} else {
		seed <- round(stats::runif(1) * 1e8)
	}
	
	.logDebug("Set seed to %s", seed)
	
	tryCatch({	
		set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion")
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
		if (getLogLevel() == C_LOG_LEVEL_DEBUG) {
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
					"(lower = ", lower, ", upper = ", upper, ", tolerance = ", tolerance, 
					", last function argument was ", unirootResult$root, ")", 
					call. = FALSE)
			}
			return(NA_real_)
		} else if (!suppressWarnings) {
			warning("Root search by 'uniroot' produced a function result (", unirootResult$f.root, ") ", 
				"that differs from target 0 ", 
				"(lower = ", lower, ", upper = ", upper, ", tolerance = ", tolerance, 
				", last function argument was ", unirootResult$root, ")", 
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
	while (!is.na(precision) && precision > tolerance) {
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
	if (getLogLevel() != C_LOG_LEVEL_TRACE) {
		return(invisible())
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
		subDir <- format(Sys.Date(), format="%Y-%m-%d")
	}
	figPath <- file.path(getwd(), "_examples", "output", "figures", subDir)
	if (!dir.exists(figPath)) {
		dir.create(figPath, showWarnings = FALSE, recursive = TRUE)
	}  
	return(figPath)
}

# @title 
# Save Last Plot
# 
# @description 
# Saves the last plot to a PNG file located in 
# '[getwd()]/_examples/output/figures/[current date]/[filename].png'.
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
	
	if (!grepl("\\.png$", filename)) {
		filename <- paste0(filename, ".png")
	}
	
	path <- file.path(outputPath, filename)
	ggplot2::ggsave(filename = path, 
		plot = ggplot2::last_plot(), device = NULL, path = NULL,
		scale = 1.2, width = 16, height = 15, units = "cm", dpi = 600, limitsize = TRUE)
	
	cat("Last plot was saved to '", path, "'\n")
}

.isFirstValueGreaterThanSecondValue <- function(firstValue, secondValue) {
	if (is.null(firstValue) || length(firstValue) != 1 || is.na(firstValue)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'firstValue' (", firstValue, ") must be a valid numeric value")
	}
	if (is.null(secondValue) || length(secondValue) != 1 || is.na(secondValue)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'secondValue' (", secondValue, ") must be a valid numeric value")
	}
	return(firstValue > secondValue)
}

.isFirstValueSmallerThanSecondValue <- function(firstValue, secondValue) {
	if (is.null(firstValue) || length(firstValue) != 1 || is.na(firstValue)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'firstValue' (", firstValue, ") must be a valid numeric value")
	}
	if (is.null(secondValue) || length(secondValue) != 1 || is.na(secondValue)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'secondValue' (", secondValue, ") must be a valid numeric value")
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
	if (getLogLevel() == C_LOG_LEVEL_TRACE) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_TRACE)
	}
}

.logDebug <- function(s, ...) {
	if (getLogLevel() %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_DEBUG)
	}
}

.logInfo <- function(s, ...) {
	if (getLogLevel() %in% c(C_LOG_LEVEL_TRACE, 
			C_LOG_LEVEL_DEBUG, C_LOG_LEVEL_INFO)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_INFO)
	}
}

.logWarn <- function(s, ...) {
	if (getLogLevel() %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, 
			C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_WARN)
	}
}

.logError <- function(s, ...) {
	if (getLogLevel() %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, 
			C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN, C_LOG_LEVEL_ERROR)) {
		.logBase(s, ..., logLevel = C_LOG_LEVEL_ERROR)
	}
}

.logProgress <- function(s, ..., startTime) {
	if (!(getLogLevel() %in% c(C_LOG_LEVEL_TRACE, C_LOG_LEVEL_DEBUG, 
			C_LOG_LEVEL_INFO, C_LOG_LEVEL_WARN, 
			C_LOG_LEVEL_ERROR, C_LOG_LEVEL_PROGRESS))) {
		return(invisible())
	}
	#return(invisible())
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
	
	parameterValues <- design[[parameterName]]
		
	kMaxUpperBound <- ifelse(.isTrialDesignFisher(design), C_KMAX_UPPER_BOUND_FISHER, C_KMAX_UPPER_BOUND)
	if (.isDefinedArgument(parameterValues) && .isDefinedArgument(design$kMax)) {
		if (.isTrialDesignFisher(design)) {
			.assertIsValidAlpha0Vec(parameterValues, kMax = design$kMax, 
				kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
		} else {
			.assertAreValidFutilityBounds(parameterValues, kMax = design$kMax, 
				kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
		}
	}
	
	if (design$sided == 2 && .isDefinedArgument(parameterValues) && any(na.omit(parameterValues) != defaultValue)) {
		warning("'", parameterName, "' (", .arrayToString(parameterValues), 
			") will be ignored because the design is two-sided", call. = FALSE)
	}
	
	if (writeToDesign) {
		.setParameterType(design, parameterName, C_PARAM_USER_DEFINED)
	}
	
	if (.isUndefinedArgument(design$informationRates) && .isUndefinedArgument(parameterValues)) { 
		if (writeToDesign) {
			if (.setKMaxToDefaultIfUndefined(design, writeToDesign) || design$kMax == C_KMAX_DEFAULT) {
				.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
			} else {
				.setParameterType(design, parameterName, C_PARAM_DERIVED)
			}
		}
		
		return(rep(defaultValue, design$kMax - 1))
	}
	
	if (.isDefinedArgument(design$informationRates) && .isUndefinedArgument(parameterValues)) {
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
			.isDefinedArgument(parameterValues, argumentExistsValidationEnabled = FALSE)) {	
		if (writeToDesign) {
			.setKMax(design, kMax = length(parameterValues) + 1)
			if (.isDefaultVector(parameterValues, rep(defaultValue, design$kMax - 1))) {
				.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
			}
		}
		
		if (.isBetaSpendingDesignWithDefinedFutilityBounds(design, parameterName, writeToDesign)) {
			return(rep(defaultValue, design$kMax - 1))
		}
		
		return(parameterValues)
	}
	
	if (writeToDesign) {
		.setKMax(design, kMax = length(parameterValues) + 1)
		if (.isDefaultVector(parameterValues, rep(defaultValue, design$kMax - 1))) {
			.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
		}
	}
	
	if (.isTrialDesignFisher(design)) {
		.assertIsValidAlpha0Vec(parameterValues, kMax = design$kMax, 
			kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	} else {
		.assertAreValidFutilityBounds(parameterValues, kMax = design$kMax, 
			kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	}
	
	if (.isBetaSpendingDesignWithDefinedFutilityBounds(design, parameterName, writeToDesign)) {
		return(rep(defaultValue, design$kMax - 1))
	}
	
	return(parameterValues)
}

.isBetaSpendingDesignWithDefinedFutilityBounds <- function(design, parameterName, writeToDesign) {
	if (.isTrialDesignFisher(design) || !.isBetaSpendingDesignType(design$typeBetaSpending)) {
		return(FALSE)
	}
	
	if (design$.getParameterType(parameterName) == C_PARAM_USER_DEFINED) {
		warning("'", parameterName, "' (", .arrayToString(design[[parameterName]]), 
			") will be ignored because it will be calculated", call. = FALSE)
	}
	else if (design$.getParameterType(parameterName) == C_PARAM_GENERATED) {
		return(FALSE)
	}
	
	if (writeToDesign) {
		.setParameterType(design, parameterName, C_PARAM_DEFAULT_VALUE)
	}
	return(TRUE)
}

.setParameterType <- function(parameterSet, parameterName, parameterType) {
	if (is.null(parameterSet)) {
		return(invisible())
	}
	
	parameterSet$.setParameterType(parameterName, parameterType)
}

.setValueAndParameterType <- function(parameterSet, parameterName, value, defaultValue,
		notApplicableIfNA = FALSE) {
		
	.assertIsParameterSetClass(parameterSet, "parameterSet")
	
	if (is.null(parameterSet)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'parameterSet' must be not null")
	}
	
	if (!(parameterName %in% names(parameterSet$getRefClass()$fields()))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", class(parameterSet), "' does not contain a field with name '", parameterName, "'")
	}
	
	parameterSet[[parameterName]] <- value
	
	if (notApplicableIfNA && all(is.na(value))) {
		parameterSet$.setParameterType(parameterName, C_PARAM_NOT_APPLICABLE)
	} else if (!is.null(value) && length(value) == length(defaultValue) && (
			(all(is.na(value)) && all(is.na(value) == is.na(defaultValue))) ||
			(!is.na(all(value == defaultValue)) && all(value == defaultValue))
			)) {
		parameterSet$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
	} else {
		parameterSet$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
	}
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
			return(invisible())
		}
	} else {
		if (design$typeOfDesign != C_TYPE_OF_DESIGN_AS_USER) {
			return(invisible())
		}
	}
	
	if (.isDefinedArgument(design$kMax)) {
		return(invisible())
	}
	
	if (.isUndefinedArgument(design$userAlphaSpending)) {
		return(invisible())
	}
	
	if (.isDefinedArgument(design$informationRates)) {
		return(invisible())
	}
	
	if (.isTrialDesignFisher(design)) {
		if (.isDefinedArgument(design$alpha0Vec)) {
			return(invisible())
		}
	} else {
		if (.isDefinedArgument(design$futilityBounds)) {
			return(invisible())
		}
	}
	
	.validateUserAlphaSpendingLength(design)
	
	.setKMax(design, kMax = length(design$userAlphaSpending))
}

.skipTestifDisabled <- function() {
	if (!isTRUE(.isCompleteUnitTestSetEnabled()) && 
			base::requireNamespace("testthat", quietly = TRUE)) {
		testthat::skip("Test is disabled")
	}
}

.getNumberOfZeroesDirectlyAfterDecimalSeparator <- function(x) {
	zeroCounter <- 0
	startEnabled <- FALSE
	x <- round(x, 15)
	x <- sprintf("%.15f", x)
	for (i in 1:nchar(x)) {
		num <- substring(x, i, i)
		if (num == ".") {
			startEnabled <- TRUE
		}
		else if (startEnabled) {
			if (num == "0") {
				zeroCounter <- zeroCounter + 1
			} else {
				return(zeroCounter)
			}
		}
	}
	return(zeroCounter)
}

.getNextHigherValue <- function(x) {
	.assertIsNumericVector(x, "x")
	values <- c()
	for (value in x) {
		value <- round(value, 15)
		values <- c(values, 1 / 10^.getNumberOfZeroesDirectlyAfterDecimalSeparator(value))
	}
	return(values)
}

#' @title 
#' Test Package
# 
#' @description
#' This function allows the installed package \code{rpact} to be tested.
#' 
#' @param outDir The output directory where all test results shall be saved.
#'     By default the current working directory is used.
#' @param completeUnitTestSetEnabled If \code{TRUE} (default) all existing unit tests will
#'     be executed; a subset of all unit tests will be used otherwise.
#' @param types The type(s) of tests to be done. Can be one or more of
#'     \code{c("tests", "examples", "vignettes")}. Default is "tests" only.
#' @param sourceDirectory An optional directory to look for \code{.save} files.
#' 
#' @details 
#' This function creates the subdirectory \code{rpact-tests} in the specified output directory
#' and copies all unit test files of the package to this newly created directory.
#' Then the function runs all tests (or a subset of all tests if 
#' \code{completeUnitTestSetEnabled} is \code{FALSE}) using 
#' \code{\link[tools]{testInstalledPackage}}.
#' The test results will be saved to the text file \code{testthat.Rout} that can be found
#' in the subdirectory \code{rpact-tests}.
#' 
#' @keywords internal
#'
#' @export 
#' 
#' @examples 
#' 
#' \dontrun{
#' testPackage()
#' }
#' 
testPackage <- function(outDir = ".", ..., completeUnitTestSetEnabled = TRUE, 
		types = "tests", sourceDirectory = NULL) {
	
	.assertTestthatIsInstalled()
		
	if (!dir.exists(outDir)) {
		stop("Test output directory '", outDir, "' does not exist")
	}
	
	startTime <- Sys.time()

	Sys.setenv("LANGUAGE" = "EN")
	on.exit(Sys.unsetenv("LANGUAGE"))

	temp <- .isCompleteUnitTestSetEnabled()
	on.exit(Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = temp), add = TRUE)
	Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = completeUnitTestSetEnabled)
	
	setLogLevel(C_LOG_LEVEL_DISABLED)
	on.exit(resetLogLevel(), add = TRUE)

	if (.isCompleteUnitTestSetEnabled()) {
		cat("Run all tests. Please wait...\n")
		cat("Have a big coffee - it will take 5 minutes or more.\n")
	} else {
		cat("Run a subset of all tests. Please wait...\n")
		cat("Have a little coffee - it will take a minute or more.\n")
	}
	
	tools::testInstalledPackage(pkg = "rpact", outDir = outDir, types = types, srcdir = sourceDirectory)
	
	if (outDir == ".") {
		outDir <- getwd()
	}
	outDir <- file.path(outDir, "rpact-tests")
	
	endTime <- Sys.time()
	timeTotal <- as.numeric(endTime - startTime) * 60
	cat("Total runtime for testing:", floor(timeTotal / 60), "minutes and", 
		((round(timeTotal / 60, 2) - floor(timeTotal / 60)) * 60), "seconds\n")
	cat("Test results were written to directory '", outDir, "' (see file 'testthat.Rout')\n", sep = "")
	
	invisible(.isCompleteUnitTestSetEnabled())
}

.isCompleteUnitTestSetEnabled <- function() {
	completeUnitTestSetEnabled <- as.logical(Sys.getenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED"))
	if (is.na(completeUnitTestSetEnabled)) {
		completeUnitTestSetEnabled <- FALSE
		Sys.setenv("RPACT_COMPLETE_UNIT_TEST_SET_ENABLED" = completeUnitTestSetEnabled)
	}
	return(isTRUE(completeUnitTestSetEnabled))
}

.getVariedParameterVectorByValue <- function(variedParameter) {
	return((variedParameter[2] -  variedParameter[1]) / C_VARIED_PARAMETER_SEQUENCE_LENGTH_DEFAULT)
}

.getVariedParameterVector <- function(variedParameter, variedParameterName) {
	if (is.null(variedParameter) || length(variedParameter) != 2 || any(is.na(variedParameter))) {
		return(variedParameter)
	}
	
	minValue <- variedParameter[1]
	maxValue <- variedParameter[2]
	if (minValue == maxValue) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", variedParameterName, "' with length 2 must contain minimum != maximum (", 
			minValue, " == ", maxValue , ")")
	}
	by <- .getVariedParameterVectorByValue(variedParameter)
	variedParameter <- seq(minValue, maxValue, by)
	return(variedParameter)
}

.getVariedParameterVectorSeqCommand <- function(variedParameter) {
	return(paste0("seq(", round(variedParameter[1], 4), ", ", round(variedParameter[2], 4), ", ",
			round(.getVariedParameterVectorByValue(variedParameter), 6),")"))
}

.getNumberOfSubjects1 <- function(numberOfSubjects, allocationRatioPlanned) {
	return((numberOfSubjects * allocationRatioPlanned) / (allocationRatioPlanned + 1))
}

.getNumberOfSubjects2 <- function(numberOfSubjects, allocationRatioPlanned) {
	return(numberOfSubjects / (allocationRatioPlanned + 1))
}

.fillWithNAs <- function(x, kMax) {
	if (length(x) >= kMax) {
		return(x)
	}
	
	x[(length(x) + 1):kMax] <- NA_real_
	return(x)
}

#' @title 
#' Print Citation
# 
#' @description
#' How to cite \code{rpact} and \code{R} in publications.
#' 
#' @param inclusiveR If \code{TRUE} (default) the information on how to cite the base R system in publications will be added.
#' 
#' @details 
#' This function shows how to cite \code{rpact} and \code{R} (\code{inclusiveR = TRUE}) in publications.
#' 
#' @keywords internal
#'
#' @export 
#' 
#' @examples 
#' 
#' printCitation()
#' 
printCitation <- function(inclusiveR = TRUE) {
	if (inclusiveR) {
		citR <- capture.output(print(citation("base"), bibtex = FALSE))
		indices <- which(citR == "")
		indices <- indices[indices != 1 & indices != length(citR)]
		if (length(indices) > 1) {
			index <- indices[length(indices)]
			citR <- citR[1:min(index, length(citR))]
		}
		cat("\n", trimws(paste(citR, collapse = "\n")), "\n", sep = "")
	}
	print(citation("rpact"), bibtex = FALSE)
}

