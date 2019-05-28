######################################################################################
#                                                                                    #
# -- Time classes --                                                                 #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 19-02-2019                                                                   #
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

C_REGEXP_GREATER_OR_EQUAL <- ">= ?"
C_REGEXP_SMALLER <- "< ?"
C_REGEXP_SMALLER_OR_EQUAL <- "<= ?"
C_REGEXP_DECIMAL_NUMBER <- "\\d*(\\.{1}\\d*)?"

TimeDefinition <- setRefClass("TimeDefinition",
	contains = "ParameterSet",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			.parameterNames <<- C_PARAMETER_NAMES
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		
		.getRegexpFromTo = function(..., from, to, fromPrefix = "", toPrefix = "") {
			return(paste0("(^ *", fromPrefix, from, " *- *", toPrefix, to, " *$)"))
		},
		
		.getRegexpSmallerThan = function() {
			return(paste0("(^ *", C_REGEXP_SMALLER, C_REGEXP_DECIMAL_NUMBER, " *$)"))
		},
		
		.getRegexpDecimalNumber = function() {
			return(paste0("(^ *", C_REGEXP_DECIMAL_NUMBER, " *$)"))
		},
		
		.getRegexpGreaterOrEqualThan = function() {
			return(paste0("(^ *", C_REGEXP_GREATER_OR_EQUAL, C_REGEXP_DECIMAL_NUMBER, " *$)"))
		},
		
		.getRegexpDecimalRangeStart = function() {
			return(.getRegexpFromTo(from = "0", to = C_REGEXP_DECIMAL_NUMBER, toPrefix = C_REGEXP_SMALLER))
		},
		
		.getRegexpDecimalRange = function() {
			return(.getRegexpFromTo(from = C_REGEXP_DECIMAL_NUMBER, to = C_REGEXP_DECIMAL_NUMBER, 
					toPrefix = C_REGEXP_SMALLER))
		},
		
		.getRegexpDecimalRangeEnd = function() {
			return(.getRegexpFromTo(from = C_REGEXP_DECIMAL_NUMBER, to = "(Inf|x|\\?)", 
					toPrefix = paste0("(", C_REGEXP_SMALLER, " *)?")))
		},
		
		.getRegexpDecimalRangeFiniteEnd = function() {
			return(.getRegexpFromTo(from = C_REGEXP_DECIMAL_NUMBER, to = C_REGEXP_DECIMAL_NUMBER, 
					toPrefix = "<=? ?"))
		},
		
		.getRegexpOr = function(...) {
			args <- list(...)
			if (length(args) == 0) {
				return("")
			}
			
			if (length(args) == 1) {
				return(args[[1]])
			}
			
			return(paste(unlist(args, recursive = FALSE, use.names = FALSE), collapse = "|"))
		},
		
		.validateTimePeriod = function(timePeriod, i, n, accrualTimeMode = FALSE) {
			calculateLastAccrualTimeEnabled = FALSE
			if (i == 1) {
				if (!grepl(.getRegexpOr(.getRegexpSmallerThan(), .getRegexpDecimalRangeStart()), 
					timePeriod, perl = TRUE)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"the name of the first region must have the format ", 
						"\"<time\" or \"0 - <time\", e.g., \"<5\" or \"0 - <5\"")
				}
				if (grepl(.getRegexpSmallerThan(), timePeriod, perl = TRUE)) {
					timePeriod <- sub("^ *< *", "0 - <", timePeriod)
				}
			}
			else if (i == n) {
				if (accrualTimeMode) {
					if (!grepl(.getRegexpOr(.getRegexpDecimalNumber(), 
							.getRegexpGreaterOrEqualThan(), .getRegexpDecimalRangeEnd(), 
							.getRegexpDecimalRangeFiniteEnd()), 
							timePeriod, perl = TRUE)) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"the name of the last region must have the format \"time\", ", 
							"\">=time\", \"time - Inf\" or \"time1 - <=time2\", ",
							"e.g., \"20\", \">=20\" or \"20 - Inf\" or \"20 - <=30\"")
					}
					if (grepl(.getRegexpOr(.getRegexpGreaterOrEqualThan(), .getRegexpDecimalRangeEnd()), 
							timePeriod, perl = TRUE)) {
						calculateLastAccrualTimeEnabled <- TRUE
					}
					timePeriod <- gsub("([Inf >=\\?x]*)|-", "", timePeriod)
				} else {
					if (!grepl(.getRegexpOr(.getRegexpGreaterOrEqualThan(), .getRegexpDecimalRangeEnd()), 
						timePeriod, perl = TRUE)) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"the name of the last region must have the format ", 
							"\">=time\" or \"time - Inf\", e.g., \">=20\" or \"20 - Inf\"")
					}
				}
			}
			else {
				if (!grepl(.getRegexpDecimalRange(), timePeriod, perl = TRUE)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"the name of the inner regions must have the format \"time_1 - <time_2\", e.g., \"5 - <20\"")
				}
			}
			
			if (accrualTimeMode) {
				return(list(timePeriod = timePeriod, calculateLastAccrualTimeEnabled = calculateLastAccrualTimeEnabled))
			}
			
			return(timePeriod)
		}
	)
)

#' @title
#' Get Piecewise Survival Time
#' 
#' @description 
#' Returns a \code{PiecewiseSurvivalTime} object that contains the all relevant parameters 
#' of an exponential survival time cumulative distribution function.
#' 
#' @param piecewiseSurvivalTime A vector that specifies the time intervals for the piecewise 
#'        definition of the exponential survival time cumulative distribution function (see details). 
#' @param lambda1 The assumed hazard rate in the treatment group, there is no default.
#'        lambda1 can also be used to define piecewise exponentially distributed survival times 
#'        (see details). 	 
#' @param lambda2 The assumed hazard rate in the reference group, there is no default.
#'  	  lambda2 can also be used to define piecewise exponentially distributed survival times 
#'        (see details).
#' @param pi1 The assumed event rate in the treatment group, default is \code{seq(0.4, 0.6, 0.1)}.
#' @param pi2 The assumed event rate in the control group, default is 0.2.
#' @param hazardRatio The vector of hazard ratios under consideration. 
#'        If the event or hazard rates in both treatment groups are defined, the hazard ratio needs 
#'        not to be specified as it is calculated. 
#' @param eventTime The assumed time under which the event rates are calculated, default is \code{12}. 
#' @param kappa The shape parameter of the Weibull distribution, default is \code{1}. 
#'        The Weibull distribution cannot be used for the piecewise definition of 
#'        the survival time distribution. 
#'        Note that the parameters \code{shape} and \code{scale} in \code{\link[stats]{Weibull}} 
#'        are equivalent to \code{kappa} and \code{1 / lambda}, respectively, in rpact.
#' @param delayedResponseAllowed If \code{TRUE}, delayed response is allowed;
#'        otherwise it will be validatet that the definition is not delayed, default is \code{FALSE}.
#' @param ... Ensures that all arguments after \code{piecewiseSurvivalTime} are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' 
#' \code{piecewiseSurvivalTime} 
#' The first element of this vector must be equal to \code{0}. \code{piecewiseSurvivalTime} can also 
#' be a list that combines the definition of the time intervals and hazard rates in the reference group. 
#' The definition of the survival time in the treatment group is obtained by the specification 
#' of the hazard ratio (see examples for details).
#' 
#' @return Returns a \code{\link{PiecewiseSurvivalTime}} object.
#' 
#' @export
#' 
#' @examples 
#' 
#' pwst <- getPiecewiseSurvivalTime(lambda2 = 0.5, hazardRatio = 0.8)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(lambda2 = 0.5, lambda1 = 0.4)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(pi2 = 0.5, hazardRatio = 0.8)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(pi2 = 0.5, pi1 = 0.4)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(pi1 = 0.3)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
#'     lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
#'     lambda2 = c(0.025, 0.04, 0.015), 
#'     lambda1 = c(0.025, 0.04, 0.015) * 0.8)
#' pwst
#' 
#' pwst <- getPiecewiseSurvivalTime(list(
#'     "0 - <6"   = 0.025, 
#'     "6 - <9"   = 0.04, 
#'     "9 - <15"  = 0.015, 
#'     "15 - <21" = 0.01, 
#'     ">=21"     = 0.007), hazardRatio = 0.75)
#' pwst
#' 
#' \donttest{
#' 
#' # The object created by getPiecewiseSurvivalTime() can be used directly in getSampleSizeSurvival():
#' getSampleSizeSurvival(piecewiseSurvivalTime = pwst)
#' 
#' # The object created by getPiecewiseSurvivalTime() can be used directly in getPowerSurvival():
#' getPowerSurvival(piecewiseSurvivalTime = pwst, 
#'     maxNumberOfEvents = 40, maxNumberOfSubjects = 100)
#' 
#' }
#'  
getPiecewiseSurvivalTime <- function(piecewiseSurvivalTime = NA_real_, 
		...,
		lambda1 = NA_real_, 
		lambda2 = NA_real_,
		hazardRatio = NA_real_,
		pi1 = NA_real_,
		pi2 = NA_real_,
#		median1 = NA_real_,
#		median2 = NA_real_,
		eventTime = C_EVENT_TIME_DEFAULT,
		kappa = 1,
		delayedResponseAllowed = FALSE) {
		
	.warnInCaseOfUnknownArguments(functionName = "getPiecewiseSurvivalTime", ..., 
		ignore = c(".pi1Default"))
	
	if (inherits(piecewiseSurvivalTime, "PiecewiseSurvivalTime") ||
			inherits(piecewiseSurvivalTime, "TrialDesignPlanSurvival")) {
		.warnInCaseOfUnusedArgument(lambda1, "lambda1", NA_real_, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(lambda2, "lambda2", NA_real_, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(hazardRatio, "hazardRatio", NA_real_, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(pi1, "pi1", NA_real_, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(pi2, "pi2", NA_real_, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(eventTime, "eventTime", C_EVENT_TIME_DEFAULT, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(kappa, "kappa", 1, "getPiecewiseSurvivalTime")
		.warnInCaseOfUnusedArgument(delayedResponseAllowed, "delayedResponseAllowed", FALSE, "getPiecewiseSurvivalTime")
	}
	
	if (inherits(piecewiseSurvivalTime, "PiecewiseSurvivalTime")) {
		return(piecewiseSurvivalTime)
	}
	
	if (inherits(piecewiseSurvivalTime, "TrialDesignPlanSurvival")) {
		return(piecewiseSurvivalTime$.piecewiseSurvivalTime)
	}
	
	.assertIsValidLambda(lambda1, 1)
	.assertIsValidLambda(lambda2, 2)
	.assertIsNumericVector(hazardRatio, "hazardRatio", naAllowed = TRUE)
	.assertIsNumericVector(pi1, "pi1", naAllowed = TRUE)
	.assertIsSingleNumber(pi2, "pi2", naAllowed = TRUE)
#	.assertIsNumericVector(median1, "median1", naAllowed = TRUE)
#	.assertIsSingleNumber(median2, "median2", naAllowed = TRUE)
	.assertIsSingleNumber(eventTime, "eventTime", naAllowed = TRUE)
	.assertIsValidKappa(kappa)
	.assertIsSingleLogical(delayedResponseAllowed, "delayedResponseAllowed")
	
	return(PiecewiseSurvivalTime(piecewiseSurvivalTime = piecewiseSurvivalTime, 
			lambda1 = lambda1, 
			lambda2 = lambda2,
			hazardRatio = hazardRatio,
			pi1 = pi1,
			pi2 = pi2,
#			median1 = median1,
#			median2 = median2,
			eventTime = eventTime,
			kappa = kappa,
			delayedResponseAllowed = delayedResponseAllowed,
			...))
}

#' @title
#' Get Accrual Time
#' 
#' @description 
#' Returns a \code{AccrualTime} object that contains the accrual time and the accrual intensity.
#' 
#' @param accrualTime The assumed accrual time for the study, default is \code{c(0,12)} (see details).
#' @param accrualIntensity A vector of accrual intensities, default is the relative 
#'        intensity \code{0.1} (see details).
#' @param maxNumberOfSubjects The maximum number of subjects.
#' @param ... Ensures that all arguments after \code{accrualTime} are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' 
#' \code{accrualTime} can also be used to define a non-constant accrual over time. 
#' For this, \code{accrualTime} needs to be a vector that defines the accrual intervals and
#' \code{accrualIntensity} needs to be specified. The first element of \code{accrualTime} must be equal to 0.\cr 
#' \code{accrualTime} can also be a list that combines the definition of the accrual time and 
#' accrual intensity \code{accrualIntensity} (see below and examples for details). 
#' If the length of \code{accrualTime} and the length of \code{accrualIntensity} are 
#' the same (i.e., the end of accrual is undefined), \code{maxNumberOfPatients > 0} needs to 
#' be specified and the end of accrual is calculated.	
#' 
#' \code{accrualIntensity} needs to be defined if a vector of \code{accrualTime} is specified.\cr
#' If the length of \code{accrualTime} and the length of \code{accrualIntensity} are the same 
#' (i.e., the end of accrual is undefined), \code{maxNumberOfPatients > 0} needs to be specified 
#' and the end of accrual is calculated.	
#' In that case, \code{accrualIntensity} is given by the number of subjects per time unit.\cr
#' If the length of \code{accrualTime} equals the length of \code{accrualIntensity - 1}   
#' (i.e., the end of accrual is defined), \code{maxNumberOfPatients} is calculated. 
#' In that case, \code{accrualIntensity} defines the intensity how subjects enter the trial. 
#' For example, \code{accrualIntensity = c(1,2)} specifies that in the second accrual interval 
#' the intensity is doubled as compared to the first accrual interval. The actual accrual intensity 
#' is calculated for the calculated  \code{maxNumberOfPatients}.
#' 
#' @return Returns a \code{\link{AccrualTime}} object.
#' 
#' @export
#' 
#' @examples 
#' 
#' \donttest{ 
#' 
#' # Case 1
#' 
#' # > End of accrual, absolute accrual intensity and `maxNumberOfSubjects` are given, 
#' # > `followUpTime`** shall be calculated. 
#' 
#' ## Example: vector based definition
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6, 30), 
#'     accrualIntensity = c(22, 33), maxNumberOfSubjects = 924) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6"   = 22,
#'     "6 - <=30" = 33), 
#'     maxNumberOfSubjects = 924) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' 
#' 
#' # Case 2
#' 
#' # > End of accrual, relative accrual intensity and `maxNumberOfSubjects` are given, 
#' # > absolute accrual intensity* and `followUpTime`** shall be calculated. 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6, 30), 
#'     accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6"   = 0.22,
#'     "6 - <=30" = 0.33), 
#'     maxNumberOfSubjects = 1000) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' 
#' 
#' # Case 3
#' 
#' # > End of accrual and absolute accrual intensity are given, 
#' # > `maxNumberOfSubjects`* and `followUpTime`** shall be calculated. 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33)) 
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6"   = 22,
#'     "6 - <=30" = 33)) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' 
#' 
#' # Case 4
#' 
#' # > End of accrual, relative accrual intensity and `followUpTime` are given, 
#' # > absolute accrual intensity** and `maxNumberOfSubjects`** shall be calculated. 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33)) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6"   = 0.22,
#'     "6 - <=30" = 0.33)) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' 
#' 
#' # Case 5
#' 
#' # > `maxNumberOfSubjects` and absolute accrual intensity are given, 
#' # > absolute accrual intensity*, end of accrual* and `followUpTime`** shall be calculated 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6), 
#'     accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6" = 22,
#'     "6"      = 33), 
#'     maxNumberOfSubjects = 1000) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' 
#' 
#' # Case 6 (not possible)
#' 
#' # > `maxNumberOfSubjects` and relative accrual intensity are given, 
#' # > absolute accrual intensity[x], end of accrual* and `followUpTime`** shall be calculated 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6), 
#'     accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6" = 0.22,
#'     "6"      = 0.33), 
#'     maxNumberOfSubjects = 1000) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' # Case 6 is not allowed and therefore an error will be shown:
#' 
#' tryCatch({	
#'     getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2)
#' }, error = function(e) {
#'     print(e$message)
#' })
#' 
#' 
#' # Case 7
#' 
#' # > `followUpTime` and absolute accrual intensity are given,  
#' # > end of accrual** and `maxNumberOfSubjects`** shall be calculated 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33)) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6" = 22,
#'     "6"      = 33)) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' getSampleSizeSurvival(accrualTime = accrualTime, 
#'     pi1 = 0.4, pi2 = 0.2, followUpTime = 6)
#' 
#' 
#' # Case 8 (not possible)
#' 
#' # > `followUpTime` and relative accrual intensity are given,  
#' # > absolute accrual intensity[x], end of accrual and `maxNumberOfSubjects` shall be calculated 
#' 
#' ## Example: vector based definition 
#' 
#' accrualTime <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33)) 
#' accrualTime
#' 
#' 
#' ## Example: list based definition
#' 
#' accrualTime <- getAccrualTime(list(
#'     "0 - <6" = 0.22,
#'     "6"      = 0.33)) 
#' accrualTime
#' 
#' 
#' ## Example: how to use accrual time object
#' 
#' # Case 8 is not allowed and therefore an error will be shown:
#' 
#' tryCatch({	
#'     getSampleSizeSurvival(accrualTime = accrualTime, pi1 = 0.4, pi2 = 0.2, followUpTime = 6)
#' }, error = function(e) {
#'     print(e$message)
#' })
#' 
#' 
#' # How to show accrual time details
#' 
#' # You can use a sample size or power object as argument for function `getAccrualTime`:
#' 
#' sampleSize <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
#'     lambda2 = 0.05, hazardRatio = 0.8, followUpTime = 6)
#' sampleSize
#' accrualTime <- getAccrualTime(sampleSize)
#' accrualTime
#' 
#' }
#' 
getAccrualTime <- function(accrualTime = NA_real_, 
		...,
		accrualIntensity = NA_real_, 
		maxNumberOfSubjects = NA_real_) {
	
	.warnInCaseOfUnknownArguments(functionName = "getAccrualTime", ..., ignore = "showWarnings")
	
	if (inherits(accrualTime, "AccrualTime") ||
			inherits(accrualTime, "TrialDesignPlanSurvival")) {
		.warnInCaseOfUnusedArgument(accrualIntensity, "accrualIntensity", NA_real_, "getAccrualTime")
		.warnInCaseOfUnusedArgument(maxNumberOfSubjects, "maxNumberOfSubjects", NA_real_, "getAccrualTime")
	}
	
	if (inherits(accrualTime, "AccrualTime")) {
		return(accrualTime)
	}
	
	if (inherits(accrualTime, "TrialDesignPlanSurvival")) {
		return(accrualTime$.accrualTime)
	}
	
	.assertIsNumericVector(accrualIntensity, "accrualIntensity", naAllowed = TRUE)
	.assertIsSingleNumber(maxNumberOfSubjects, "maxNumberOfSubjects", naAllowed = TRUE)
	
	args <- list(...)
	showWarnings <- args[["showWarnings"]]
	if (is.null(showWarnings) || !is.logical(showWarnings)) {
		showWarnings <- TRUE
	}
	
	return(AccrualTime(accrualTime = accrualTime, 
			accrualIntensity = accrualIntensity, 
			maxNumberOfSubjects = maxNumberOfSubjects, 
			showWarnings = showWarnings))
}

#' 
#' @name PiecewiseSurvivalTime
#' 
#' @title
#' Piecewise Exponential Survival Time
#' 
#' @description 
#' Class for definition of piecewise survival times.
#' 
#' @details
#' \code{PiecewiseSurvivalTime} is a class for definition of piecewise survival times.
#' 
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
PiecewiseSurvivalTime <- setRefClass("PiecewiseSurvivalTime",
	contains = "TimeDefinition",
	fields = list(
		.logDebugEnabled = "logical",
		.pi1Default = "numeric",
		piecewiseSurvivalTime = "numeric",
		lambda1 = "numeric",
		lambda2 = "numeric",
		hazardRatio = "numeric",
		pi1 = "numeric",
		pi2 = "numeric",
		median1 = "numeric", 
		median2 = "numeric", 
		eventTime = "numeric",
		kappa = "numeric",
		piecewiseSurvivalEnabled = "logical",
		delayedResponseAllowed = "logical",
		delayedResponseEnabled = "logical"
	),
	methods = list(
		initialize = function(piecewiseSurvivalTime = NA_real_, 
				...,
				lambda1 = NA_real_, 
				lambda2 = NA_real_,
				hazardRatio = NA_real_,
				pi1 = NA_real_,
				pi2 = NA_real_,
				median1 = NA_real_,
				median2 = NA_real_,
				eventTime = C_EVENT_TIME_DEFAULT,
				kappa = 1,
				delayedResponseAllowed = FALSE) {
			
			callSuper(piecewiseSurvivalTime = NA_real_,
				lambda1 = lambda1,
				lambda2 = lambda2,
				hazardRatio = hazardRatio, 
				pi1 = pi1, 
				pi2 = pi2, 
				median1 = median1, 
				median2 = median2, 
				eventTime = eventTime, 
				kappa = kappa,
				delayedResponseAllowed = delayedResponseAllowed, ...)
			
			if (length(piecewiseSurvivalTime) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piecewiseSurvivalTime' must be defined (set to NA_real_ if not applicable)")
			}
			
			args <- list(...)
			if (!is.null(args[[".pi1Default"]])) {
				.pi1Default <<- args[[".pi1Default"]]
			}
			
			.logDebugEnabled <<- FALSE
			piecewiseSurvivalEnabled <<- FALSE
			delayedResponseEnabled <<- FALSE
			
			.setParameterType("piecewiseSurvivalTime", C_PARAM_NOT_APPLICABLE)
			.setParameterType("lambda1", ifelse(length(lambda1) == 1 && is.na(lambda1), 
					C_PARAM_NOT_APPLICABLE, C_PARAM_USER_DEFINED))
			.setParameterType("lambda2", C_PARAM_NOT_APPLICABLE)
			
			.setParameterType("piecewiseSurvivalEnabled", C_PARAM_GENERATED)
			.setParameterType("delayedResponseEnabled", ifelse(isTRUE(delayedResponseAllowed), 
					C_PARAM_GENERATED, C_PARAM_NOT_APPLICABLE))
			.setParameterType("delayedResponseAllowed", ifelse(isTRUE(delayedResponseAllowed), 
					C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE))
			.setParameterType("pi1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
			.setParameterType("eventTime", ifelse(length(eventTime) == 1 && is.na(eventTime), 
					C_PARAM_NOT_APPLICABLE, 
					ifelse(eventTime == C_EVENT_TIME_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)))
			.setParameterType("kappa", ifelse(length(kappa) == 1 && !is.na(kappa) && kappa == 1,
					C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			
			.init(piecewiseSurvivalTime)
		},
		
		.asDataFrame = function() {
			data <- data.frame(
				piecewiseSurvivalTime = piecewiseSurvivalTime,
				lambda1 = lambda1,
				lambda2 = lambda2
			)
			rownames(data) <- as.character(1:nrow(data))
			colnames(data) <- c("Start time", 
				C_PARAMETER_NAMES["lambda1"], # Hazard rate (1) 
				C_PARAMETER_NAMES["lambda2"]) # Hazard rate (2)
			return(data)
		},
		
		.logDebug = function(...) {
			if (!.logDebugEnabled) {
				return(invisible())
			}
			cat("[DEBUG] ", ..., "\n", sep = "")
		},
		
		.isPiBased = function() {
			return(!.isLambdaBased())
		},
		
		.isLambdaBased = function(minNumberOfLambdas = 2) {
			if (.getParameterType("lambda2") == C_PARAM_USER_DEFINED) {
				if (length(lambda2) >= minNumberOfLambdas && !any(is.na(lambda2))) {
					return(TRUE)
				}
			}
			
			return((length(pi1) == 0 || any(is.na(pi1))) && (length(pi2) == 0 || any(is.na(pi2))))
		},
		
		show = function(showType = 1) {
			.show(showType = showType, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, consoleOutputEnabled = TRUE) {
			'Method for automatically printing piecewise survival time objects'
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the piecewise survival time object of class",
					methods::classLabel(class(.self)), ":\n\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat("Piecewise exponential survival times:\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				if (!piecewiseSurvivalEnabled) {
					.cat("  Piecewise exponential survival is disabled.\n\n", consoleOutputEnabled = consoleOutputEnabled)
				} else if (length(piecewiseSurvivalTime) == 1) {
					.cat("  At all times:", lambda2[1], "\n\n", consoleOutputEnabled = consoleOutputEnabled)
				} else {
					piecewiseSurvivalTimeStr <- format(piecewiseSurvivalTime)
					lambda2Str <- format(lambda2)
					for (i in 1:length(piecewiseSurvivalTime)) {
						if (i < length(piecewiseSurvivalTime)) {
							.cat("  ", piecewiseSurvivalTimeStr[i], " - <", 
								piecewiseSurvivalTimeStr[i + 1], ": ",
								lambda2Str[i], "\n", sep ="",
								consoleOutputEnabled = consoleOutputEnabled)
						} else {
							.cat("  ", rep(" ", 2 + max(nchar(piecewiseSurvivalTimeStr))), 
								">=", piecewiseSurvivalTimeStr[i], ": ",
								lambda2Str[i], "\n", sep ="",
								consoleOutputEnabled = consoleOutputEnabled)
						}
						
					}
					if (delayedResponseEnabled) {
						.cat("Delayed response is enabled.\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
				
				.cat("Details:\n\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Generated parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				
			}
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			s <- "piecewise survival time"
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		},
		
		isDelayedResponseEnabled = function() {
			return(delayedResponseEnabled)
		},
		
		isPiecewiseSurvivalEnabled = function() {
			if (length(piecewiseSurvivalTime) == 0) {
				return(FALSE)
			}
			
			if (length(piecewiseSurvivalTime) == 1 && is.na(piecewiseSurvivalTime)) {
				return(FALSE)
			}
			
			return(TRUE)
		},
		
		.initFromList = function(pwSurvTimeList) {
			if (!is.list(pwSurvTimeList)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'piecewiseSurvivalTime' must be a list")
			}
			
			if (length(pwSurvTimeList) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piecewiseSurvivalTime' must contain at least one entry")
			}
			
			if (!all(is.na(lambda2))) {
				warning("'lambda2' (", .arrayToString(lambda2), 
					") will be ignored because 'piecewiseSurvivalTime' is a list", call. = FALSE)
			}
			
			pwSurvStartTimes <- c(0)
			pwSurvLambda2 <- c()
			pwSurvTimeNames <- names(pwSurvTimeList)
			for (i in 1:length(pwSurvTimeNames)) {
				timePeriod <- pwSurvTimeNames[i]
				lambdaValue <- pwSurvTimeList[[timePeriod]]
				.assertIsSingleNumber(lambdaValue, paste0("pwSurvLambda[", i, "]"))

				timePeriod <- .validateTimePeriod(timePeriod, i = i, n = length(pwSurvTimeNames))
				
				if (i < length(pwSurvTimeNames)) {
					parts <- strsplit(timePeriod, "- *(< *)?", perl = TRUE)[[1]]
					if (length(parts) != 2) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"all regions (", timePeriod, ") must have the format ", 
							"\"time_1 - <time_2\", e.g., \"3 - <6\"")
					}
					intervalBoundary <- as.numeric(trimws(parts[2]))
					pwSurvStartTimes <- c(pwSurvStartTimes, intervalBoundary)
				}
				pwSurvLambda2 <- c(pwSurvLambda2, lambdaValue)
			}
			
			piecewiseSurvivalTime <<- pwSurvStartTimes
			.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
			if (length(hazardRatio) == 1 && !is.na(hazardRatio)) {
				lambda1 <<- pwSurvLambda2 * hazardRatio^(1 / kappa)
				.setParameterType("lambda1", C_PARAM_GENERATED)
			} else if (length(hazardRatio) > 1 && delayedResponseAllowed) {
				warning("Only the first 'hazardRatio' (", hazardRatio[1], 
					") was used for piecewise survival time definition ",
					"(use a loop over the function to simulate different hazard ratios)", 
					call. = FALSE)
				hazardRatio <<- hazardRatio[1]
				lambda1 <<- pwSurvLambda2 * hazardRatio^(1 / kappa)
				.setParameterType("lambda1", C_PARAM_GENERATED)
			} else {
				lambda1 <<- NA_real_
				.setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
			}
			
			lambda2 <<- pwSurvLambda2
			.setParameterType("lambda2", C_PARAM_USER_DEFINED)
			
			piecewiseSurvivalEnabled <<- !identical(piecewiseSurvivalTime, 0)
		},
		
		.init = function(pwSurvTime) {
			
			# case 1: lambda1 and lambda2 = NA
			if (length(pwSurvTime) == 1 && (is.na(pwSurvTime) || is.numeric(pwSurvTime)) && 
					length(lambda1) == 1 && is.na(lambda1) && 
					length(lambda2) == 1 && is.na(lambda2)) {
					
				.logDebug(".init, case 1: lambda1 and lambda2 = NA")
					
				if (!any(is.na(hazardRatio))) {
					.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
				}
				
				if (!is.na(pwSurvTime)) {
					warning("'piecewiseSurvivalTime' (", pwSurvTime, ") will be ignored")
				}
				
				if (is.na(pi2)) {
					if (!is.na(median2)) {
						.logDebug(".init: calculate pi2 to by median2")
						pi2 <<- getPiByMedian(median2, eventTime, kappa = kappa) 
						.setParameterType("pi2", C_PARAM_GENERATED)
					} else {
						.logDebug(".init: set pi2 to default")
						pi2 <<- 0.2
						.setParameterType("pi2", ifelse(pi2 == 0.2, 
								C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
					}
				} else {
					.assertIsSingleNumber(pi2, "pi2")
					if (!any(is.na(median2))) {
						warning("'median2' (", .arrayToString(median2), ") will be ignored")
						median2 <<- NA_real_
					}
				}
				
				hazardRatioCalculationEnabled <- TRUE
				if (all(is.na(pi1))) {
					if (length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
						.logDebug(".init: calculate pi1 by pi2 and hazardRatio")
						pi1 <<- getPiByLambda(
							getLambdaByPi(pi2, eventTime, kappa = kappa) * 
							hazardRatio^(1 / kappa), eventTime, kappa = kappa)
						.setParameterType("pi1", C_PARAM_GENERATED)
						.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
						hazardRatioCalculationEnabled <- FALSE
					} else {
						.logDebug(".init: set pi1 to default")
						if (!is.null(.pi1Default) && is.numeric(.pi1Default) &&
								length(.pi1Default) > 0) {
							pi1 <<- .pi1Default
						} else {
							pi1 <<- C_PI_1_SAMPLE_SIZE_DEFAULT
						}
						.setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
					}
				} else {
					.assertIsNumericVector(pi1, "pi1")
					if (!any(is.na(median1))) {
						.logDebug(".init: set median1 to NA")
						warning("'median1' (", .arrayToString(median1), ") will be ignored")
						median1 <<- NA_real_
					}
				}
				
				if (hazardRatioCalculationEnabled) {
					if (length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
						warning("'hazardRatio' (", .arrayToString(hazardRatio), 
							") will be ignored because it will be calculated", call. = FALSE)
					}

					.logDebug(".init: calculate hazardRatio by pi1 and pi2")
					hazardRatio <<- getHazardRatioByPi(pi1, pi2, eventTime, kappa = kappa)
					.setParameterType("hazardRatio", C_PARAM_GENERATED)
				}
				
				if (length(pi1) > 0 && !any(is.na(pi2))) {
					pi1Default <- C_PI_1_SAMPLE_SIZE_DEFAULT
					if (!is.null(.pi1Default) && is.numeric(.pi1Default) &&
							length(.pi1Default) > 0) {
						pi1Default <- .pi1Default
					} 
					if (identical(pi1, pi1Default)) {
						.setParameterType("pi1", C_PARAM_DEFAULT_VALUE)
					} else if (hazardRatioCalculationEnabled) {
						.setParameterType("pi1", C_PARAM_USER_DEFINED)
					}
				}
				
				if (length(pi2) == 1 && !is.na(pi2)) {
					if (length(eventTime) == 1 && !is.na(eventTime)) {
						lambda2 <<- getLambdaByPi(pi2, eventTime, kappa = kappa)
						.setParameterType("lambda2", C_PARAM_GENERATED)
					}
					
					if (length(pi1) == 1 && is.na(pi1) && !any(is.na(hazardRatio))) {
						pi1 <<- getPiByLambda(getLambdaByPi(
							pi2, eventTime, kappa = kappa) * hazardRatio^(1 / kappa), 
							eventTime, kappa = kappa)
						.setParameterType("pi1", C_PARAM_GENERATED)
					}
					if (length(pi1) > 0 && !any(is.na(pi1)) && 
							length(eventTime) == 1 && !is.na(eventTime)) {
						lambda1 <<- getLambdaByPi(pi1, eventTime, kappa = kappa)
						.setParameterType("lambda1", C_PARAM_GENERATED)
					}
				}
				
				.initMedian()
				return(invisible())
			}
			
			if (is.list(pwSurvTime)) {
				.assertIsValidHazardRatioVector(hazardRatio)
				.initFromList(pwSurvTime)
				.initHazardRatio()
				if (!piecewiseSurvivalEnabled) {
					.initPi()
					.initMedian()
				}
			}
			else if (delayedResponseAllowed && length(lambda2) == 1 && 
					!is.na(lambda2) && length(hazardRatio) > 0) {
					
				.logDebug(".init, case 2: delayedResponseAllowed") 
					
				piecewiseSurvivalEnabled <<- FALSE
				piecewiseSurvivalTime <<- 0
				.initPi()
				.initHazardRatio()
				.initMedian()
			}
			else if (!is.numeric(pwSurvTime)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piecewiseSurvivalTime' must be a list or a numeric vector")
			} 
			else {
				piecewiseSurvivalTime <<- pwSurvTime
				if ((all(is.na(piecewiseSurvivalTime)) || identical(piecewiseSurvivalTime, 0)) && 
						length(lambda2) == 1 && !is.na(lambda2)) {
					.logDebug(".init, case 3: piecewise survival is disabled") 
					piecewiseSurvivalTime <<- 0
					.setParameterType("piecewiseSurvivalTime", C_PARAM_DEFAULT_VALUE)
					piecewiseSurvivalEnabled <<- FALSE
				} else {
					.logDebug(".init, case 3: piecewise survival is enabled")
					if (all(is.na(piecewiseSurvivalTime))) {
						stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
							"'piecewiseSurvivalTime' must be specified")
					}
					.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
					piecewiseSurvivalEnabled <<- TRUE
				}
				.initHazardRatio()
				.initPi()
				.initMedian()
			}
			
			.validateInitialization()
		},
		
		.initMedian = function() {
			if (length(eventTime) == 1 && !is.na(eventTime)) {
				if (length(pi1) > 0 && !all(is.na(pi1))) {
					median1 <<- getMedianByPi(pi1, eventTime, kappa = kappa)
					.setParameterType("median1", C_PARAM_GENERATED)
				}
				if (length(pi2) == 1 && !is.na(pi2)) {
					median2 <<- getMedianByPi(pi2, eventTime, kappa = kappa)
					.setParameterType("median2", C_PARAM_GENERATED)
				}
			} else {
				if (length(lambda1) > 0 && !all(is.na(lambda1))) {
					median1 <<- getMedianByLambda(lambda1, kappa = kappa)
					.setParameterType("median1", C_PARAM_GENERATED)
				}
				if (length(lambda2) == 1 && !is.na(lambda2)) {
					median2 <<- getMedianByLambda(lambda2, kappa = kappa)
					.setParameterType("median2", C_PARAM_GENERATED)
				}
			}
		},
		
		.initPi = function() {
			.logDebug(".initPi: lambda2 is defined")
			
			if (length(eventTime) != 1 || is.na(eventTime)) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"'eventTime' must be specified to calculate 'pi2' by 'lambda2'")
			}
			
			if (length(lambda2) == 0 || any(is.na(lambda2))) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
					"'lambda2' must be defined before .initPi() can be called")
			}
			
			.setParameterType("lambda2", C_PARAM_USER_DEFINED)
			.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
			.setParameterType("eventTime", C_PARAM_USER_DEFINED)
			
			if (piecewiseSurvivalEnabled && length(hazardRatio) > 1) {
				return(invisible())
			}
			
			pi2Calulated <- getPiByLambda(lambda2, eventTime, kappa = kappa)
			if (!is.na(pi2) && !isTRUE(all.equal(pi2, pi2Calulated))) {
				warning("'pi2' (", pi2, ") will be ignored", call. = FALSE)
			}
			pi2 <<- pi2Calulated
			.setParameterType("pi2", C_PARAM_GENERATED)
			
			if (length(lambda1) == 0 || any(is.na(lambda1))) {
				if (length(hazardRatio) > 0 && !any(is.na(hazardRatio))) {
					.logDebug(".initPi: calculate lambda1 by hazardRatio")
					lambda1 <<- lambda2 * hazardRatio^(1 / kappa)
					.setParameterType("lambda1", C_PARAM_GENERATED)
				} else if (length(lambda1) == 0) {
					lambda1 <<- NA_real_
				} else if (delayedResponseAllowed) {
					.setParameterType("lambda1", C_PARAM_USER_DEFINED)
				}
			}
			
			if (length(lambda1) > 0 && !all(is.na(lambda1))) {
				.logDebug(".initPi: calculate p1 by lambda1")
				pi1Calulated <- getPiByLambda(lambda1, eventTime, kappa = kappa)
				if (!all(is.na(pi1)) && !isTRUE(all.equal(pi1, pi1Calulated))) {
					warning("'pi1' (", .arrayToString(pi1), ") will be ignored", call. = FALSE)
				}
				pi1 <<- pi1Calulated
				.setParameterType("pi1", C_PARAM_GENERATED)
			}
		},
		
		.initHazardRatio = function() {
			.logDebug(".initHazardRatio")
			
			if (!is.null(hazardRatio) && length(hazardRatio) > 0 && !all(is.na(hazardRatio))) {
				if ((length(lambda1) == 1 && is.na(lambda1)) ||
						.getParameterType("lambda1") == C_PARAM_GENERATED) {
					.setParameterType("hazardRatio", C_PARAM_USER_DEFINED)
					return(invisible())
				}
				
				warning("'hazardRatio' (", .arrayToString(hazardRatio), 
					") will be ignored because it will be calculated", call. = FALSE)
			}
			
			if (any(is.na(lambda2))) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda2' must be specified")
			}
			
			if (any(is.na(lambda1))) {
				if (any(is.na(hazardRatio))) {
					stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
						"'hazardRatio' or 'lambda1' must be specified")
				}
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda1' must be specified")
			}
			
			.setParameterType("lambda1", C_PARAM_USER_DEFINED)
			
			hr <- unique(round(lambda1 / lambda2, 8)^kappa)
			if (length(hr) != 1) {
				if (delayedResponseAllowed) {
					hazardRatio <<- (lambda1 / lambda2)^kappa
					.setParameterType("hazardRatio", C_PARAM_GENERATED)
					delayedResponseEnabled <<- TRUE
					return(invisible())
				} else {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'hazardRatio' can only be calculated if 'unique(lambda1 / lambda2)' ",
						"result in a single value; current result = ", 
						.arrayToString(round(hr, 4), vectorLookAndFeelEnabled = TRUE), 
						" (delayed response is not allowed)")
				}
				
			}
			
			hazardRatio <<- hr[1]
			.setParameterType("hazardRatio", C_PARAM_GENERATED)
		},
		
		.validateInitialization = function() {
			if (length(piecewiseSurvivalTime) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piecewiseSurvivalTime' must contain at least one urvival start time")
			}
			
			if (any(is.na(piecewiseSurvivalTime))) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piecewiseSurvivalTime' must contain valid survival start times")
			}
			
			if (piecewiseSurvivalTime[1] != 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"the first value of 'piecewiseSurvivalTime' must be 0")
			}
			
			if (length(piecewiseSurvivalTime) != length(lambda2)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"length of 'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime), 
					") must be equal to length of 'lambda2' (", 
					length(lambda2), ")")
			}
			
			.assertValuesAreStrictlyIncreasing(piecewiseSurvivalTime, "piecewiseSurvivalTime")
			
			if ((length(lambda1) != 1 || is.na(lambda1)) && 
					.getParameterType("lambda1") != C_PARAM_GENERATED) {
				if (length(hazardRatio) == 1 && !is.na(hazardRatio)) {
					lambda1 <<- lambda2 * hazardRatio^(1 / kappa)
					.setParameterType("lambda1", C_PARAM_GENERATED)
				} else if (length(hazardRatio) > 1 && delayedResponseAllowed && 
						!is.na(hazardRatio[1])) {
					if (!delayedResponseEnabled && .isLambdaBased()) {
						warning("Only the first 'hazardRatio' (", hazardRatio[1], 
							") was used for piecewise survival time definition", call. = FALSE)
						hazardRatio <<- hazardRatio[1]
						lambda1 <<- lambda2 * hazardRatio^(1 / kappa)
						.setParameterType("lambda1", C_PARAM_GENERATED)
					}
				} else if (!delayedResponseEnabled) {
					if (length(lambda1) > 1) {
						warning("'lambda1' (", .arrayToString(lambda1), 
							") will be ignored", call. = FALSE)
					}
					lambda1 <<- NA_real_
					.setParameterType("lambda1", C_PARAM_NOT_APPLICABLE)
				}
			}
			else if (length(hazardRatio) == 1 && !is.na(hazardRatio) && 
					length(lambda1) > 0 && !any(is.na(lambda1)) &&
					length(lambda2) > 0 && !any(is.na(lambda2))) {
				target <- lambda2 * hazardRatio^(1 / kappa)
				if (length(lambda1) > 0 && !all(is.na(lambda1)) && 
						!isTRUE(all.equal(target, lambda1))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'lambda1' (", .arrayToString(lambda1), ") ",
						"is not as expected (", .arrayToString(target), ") for given hazard ratio ", hazardRatio)
				}
			}
			
			if (piecewiseSurvivalEnabled && !(length(lambda1) == 1 && is.na(lambda1)) && 
					length(piecewiseSurvivalTime) != length(lambda1)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"length of 'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime), 
					") must be equal to length of 'lambda1' (", 
					length(lambda1), ")")
			}
		}
	)
)

#' 
#' @name AccrualTime
#' 
#' @title
#' Accrual Time
#' 
#' @description 
#' Class for definition of accrual time and accrual intensity.
#' 
#' @details
#' \code{AccrualTime} is a class for definition of accrual time and accrual intensity.
#' 
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
AccrualTime <- setRefClass("AccrualTime",
	contains = "TimeDefinition",
	fields = list(
		.showWarnings = "logical",
		endOfAccrualIsUserDefined = "logical",
		followUpTimeMustBeUserDefined = "logical",
		maxNumberOfSubjectsIsUserDefined = "logical",
		maxNumberOfSubjectsCanBeCalculatedDirectly = "logical",
		absoluteAccrualIntensityEnabled = "logical",
		
		accrualTime = "numeric",
		accrualIntensity = "numeric",
		accrualIntensityRelative = "numeric",
		maxNumberOfSubjects = "numeric",
		remainingTime = "numeric",
		piecewiseAccrualEnabled = "logical"
	),
	methods = list(
		initialize = function(accrualTime = NA_real_, 
			...,
			accrualIntensity = NA_real_, 
			maxNumberOfSubjects = NA_real_,
			showWarnings = TRUE) {
			callSuper(accrualTime = NA_real_,
				accrualIntensity = accrualIntensity,
				maxNumberOfSubjects = maxNumberOfSubjects, 
				.showWarnings = showWarnings, ...)
			
			endOfAccrualIsUserDefined <<- NA
			followUpTimeMustBeUserDefined <<- NA
			maxNumberOfSubjectsIsUserDefined <<- NA
			maxNumberOfSubjectsCanBeCalculatedDirectly <<- TRUE
			absoluteAccrualIntensityEnabled <<- NA
			.setParameterType("endOfAccrualIsUserDefined", C_PARAM_GENERATED)
			.setParameterType("followUpTimeMustBeUserDefined", C_PARAM_GENERATED)
			.setParameterType("maxNumberOfSubjectsIsUserDefined", C_PARAM_GENERATED)
			.setParameterType("maxNumberOfSubjectsCanBeCalculatedDirectly", C_PARAM_GENERATED)
			.setParameterType("absoluteAccrualIntensityEnabled", C_PARAM_GENERATED)
			
			accrualIntensityRelative <<- NA_real_
			.setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)
			remainingTime <<- NA_real_
			
			.init(accrualTime)
			.initAccrualIntensityAbsolute()
			.validateFormula()
		},
		
		.asDataFrame = function() {
			accrualIntensityTemp <- accrualIntensity
			if (!all(is.na(accrualIntensityRelative))) {
				accrualIntensityTemp <- accrualIntensityRelative
			}
			if (length(accrualIntensityTemp) + 1 == length(accrualTime)) {
				accrualIntensityTemp <- c(accrualIntensityTemp, NA_real_)
			}
			data <- data.frame(
				accrualTime = accrualTime,
				accrualIntensity = accrualIntensityTemp
			)
			rownames(data) <- as.character(1:nrow(data))
			colnames(data) <- c("Start time", 
				C_PARAMETER_NAMES["accrualIntensity"])
			return(data)
		},
		
		show = function(showType = 1) {
			.show(showType = showType, consoleOutputEnabled = TRUE)
		},
		
		.isAbsoluteAccrualIntensity = function(x) {
			return(!.isRelativeAccrualIntensity(x))
		},
		
		
		.isRelativeAccrualIntensity = function(x) {
			return(all(x < 1))
		},
		
		.show = function(showType = 1, consoleOutputEnabled = TRUE) {
			'Method for automatically printing accrual time objects'
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the accrual time object of class",
					methods::classLabel(class(.self)), ":\n\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat("Accrual time and intensity:\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				if (!isAccrualTimeEnabled()) {
					.cat("  Accrual time is disabled.\n", consoleOutputEnabled = consoleOutputEnabled)
				} else if (length(accrualTime) == 1) {
					.cat("  At all times:", accrualIntensity[1], "\n", consoleOutputEnabled = consoleOutputEnabled)
				} else {
					accrualTimeStr <- format(accrualTime)
					accrualIntensityStr <- format(accrualIntensity)
					for (i in 1:length(accrualTime)) {
						prefix <- ifelse(i == length(accrualTime) - 1, "<=", " <")
						suffix <- ""
						if (!maxNumberOfSubjectsIsUserDefined) {
							suffix <- " "
						}
						if (i < length(accrualTime)) {
							.cat("  ", accrualTimeStr[i], " - ", prefix, accrualTimeStr[i + 1], suffix, ": ",
								accrualIntensityStr[i], "\n",
								consoleOutputEnabled = consoleOutputEnabled)
						}
						else if (!maxNumberOfSubjectsIsUserDefined) {
							.cat("  ", accrualTimeStr[i], " - <=[?]: ",
								accrualIntensityStr[i], "\n",
								consoleOutputEnabled = consoleOutputEnabled)
						}
						
					}
					.cat("", consoleOutputEnabled = consoleOutputEnabled)
				}
				.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				
				if (isAccrualTimeEnabled()) {
					.showFormula(consoleOutputEnabled = consoleOutputEnabled)
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
					
					.showCase(consoleOutputEnabled = consoleOutputEnabled)
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
				
				.cat("Details:\n\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Generated parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
			}
		},
		
		.getFormula = function() {
			s <- ""
			for (i in 1:length(accrualTime)) {
				if (i < length(accrualTime)) {
					s <- paste0(s, (round(accrualTime[i + 1], 4) - round(accrualTime[i], 4)), 
						" * ", round(accrualIntensity[i], 4))
					if (!absoluteAccrualIntensityEnabled) {
						s <- paste0(s, " * c ")
					}
					if (i < length(accrualIntensity)) {
						s <- paste0(s, " + ")
					}
				}
			}
			return(s)
		},
		
		.validateFormula = function() {
			if (is.na(maxNumberOfSubjects) || length(accrualTime) != length(accrualIntensity) + 1) {
				return(invisible())
			}
			
			numberOfSubjects <- 0
			for (i in 1:length(accrualTime)) {
				if (i < length(accrualTime)) {
					numberOfSubjects <- numberOfSubjects + 
						(accrualTime[i + 1] - accrualTime[i]) * accrualIntensity[i]
				}
			}
			if (!isTRUE(all.equal(numberOfSubjects, maxNumberOfSubjects, tolerance = 1e-03)) && 
					.isAbsoluteAccrualIntensity(accrualIntensity)) {
				stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"'maxNumberOfSubjects' (", maxNumberOfSubjects, ") disagrees with ",
					"the defined accrual time and intensity: ",
					.getFormula(), " = ", numberOfSubjects)
			}
		},
		
		.showFormula = function(consoleOutputEnabled) {
			.cat("Formula:\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
			.cat("  ", consoleOutputEnabled = consoleOutputEnabled)
			.cat("maxNumberOfSubjects = ", consoleOutputEnabled = consoleOutputEnabled)
			if (!is.na(maxNumberOfSubjects)) {
				.cat(maxNumberOfSubjects, " = ", consoleOutputEnabled = consoleOutputEnabled)
			}
			.cat(.getFormula(), consoleOutputEnabled = consoleOutputEnabled)
			if (length(accrualTime) == length(accrualIntensity)) {
				.cat("(x - ", accrualTime[length(accrualTime)], ") * ", 
					accrualIntensity[length(accrualIntensity)], 
					", where 'x' is the unknown last accrual time",
					consoleOutputEnabled = consoleOutputEnabled)
				if (!absoluteAccrualIntensityEnabled) {
					.cat(" and 'c' a constant factor", consoleOutputEnabled = consoleOutputEnabled)
				}
			} else if (!absoluteAccrualIntensityEnabled) {
				.cat(", where 'c' is a constant factor", consoleOutputEnabled = consoleOutputEnabled)
			}
			.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
		},
		
		.getCase = function() {
			
		},
		
		.showCase = function(consoleOutputEnabled = TRUE) {
			
			caseIsAllowed <- TRUE
			
			prefix <- "  "
			
			# Case 1
			# example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33), 
			#          maxNumberOfSubjects = 1000)
			if (endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined && 
					absoluteAccrualIntensityEnabled) {
				.cat("Case (#1):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "End of accrual, absolute accrual intensity and 'maxNumberOfSubjects' are given, ",
					" 'followUpTime'** shall be calculated.\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), ",
					"accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000)\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 2
			# example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33), 
			#          maxNumberOfSubjects = 1000)
			else if (endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined && 
					!absoluteAccrualIntensityEnabled) {
				.cat("Case (#2):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "End of accrual, relative accrual intensity and 'maxNumberOfSubjects' are given, ",
					"absolute accrual intensity* and 'followUpTime'** shall be calculated.\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), ",
					"accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000)\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 3
			# example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))
			else if (endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
					absoluteAccrualIntensityEnabled) {
				.cat("Case (#3):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "End of accrual and absolute accrual intensity are given, ",
					"'maxNumberOfSubjects'* and 'followUpTime'** shall be calculated.\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33))\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 4
			# example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))
			else if (endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined &&
					!absoluteAccrualIntensityEnabled) {
				.cat("Case (#4):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "End of accrual, relative accrual intensity and 'followUpTime' are given, ",
					"absolute accrual intensity** and 'maxNumberOfSubjects'** shall be calculated.\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33))\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 5
			# example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33), 
			#          maxNumberOfSubjects = 1000)
			else if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
					absoluteAccrualIntensityEnabled) {
				.cat("Case (#5):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "'maxNumberOfSubjects' and absolute accrual intensity are given, ",
					"end of accrual* and 'followUpTime'** shall be calculated\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), ",
					"accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000)\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 6
			# example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33), 
			#          maxNumberOfSubjects = 1000)
			else if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
					!absoluteAccrualIntensityEnabled) {
				caseIsAllowed <- FALSE
				.cat("Case (#6):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "'maxNumberOfSubjects' and relative accrual intensity are given, ",
					"absolute accrual intensity[x], end of accrual* and 'followUpTime'** shall be calculated\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), ",
					"accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000)\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 7
			# example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))
			else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined && 
					followUpTimeMustBeUserDefined && absoluteAccrualIntensityEnabled) {
				.cat("Case (#7):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "'followUpTime' and absolute accrual intensity are given, ", 
					"end of accrual** and 'maxNumberOfSubjects'** shall be calculated\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33))\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			# Case 8
			# example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))
			else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined && 
					followUpTimeMustBeUserDefined && !absoluteAccrualIntensityEnabled) {
				caseIsAllowed <- FALSE
				.cat("Case (#8):\n", sep = "", heading = 1, consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "'followUpTime' and relative accrual intensity are given, ", 
					"absolute accrual intensity[x], end of accrual and 'maxNumberOfSubjects' shall be calculated\n", 
					consoleOutputEnabled = consoleOutputEnabled)
				.cat(prefix, "Example: getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33))\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}

			.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
			if (!caseIsAllowed) {
				.cat(prefix, "[x] Cannot be calculated.\n", 
					consoleOutputEnabled = consoleOutputEnabled)
			}			
			.cat(prefix, "(*) Can be calculated directly.\n", 
				consoleOutputEnabled = consoleOutputEnabled)
			.cat(prefix, "(**) Cannot be calculated directly but with ",
				"'getSampleSizeSurvival' or 'getPowerSurvival'.\n", 
				consoleOutputEnabled = consoleOutputEnabled)
		},
		
		.validate = function() {
			# Case 6
			if (!endOfAccrualIsUserDefined && maxNumberOfSubjectsIsUserDefined &&
					!absoluteAccrualIntensityEnabled) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"the calulation of 'followUpTime' for given 'maxNumberOfSubjects' ",
					"and relative accrual intensities (< 1) ",
					"can only be done if end of accrual is defined")
			}
			
			# Case 8
			else if (!endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined && 
					followUpTimeMustBeUserDefined && !absoluteAccrualIntensityEnabled) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"the calulation of 'maxNumberOfSubjects' for given 'followUpTime' ",
					"and relative accrual intensities (< 1) ",
					"can only be done if end of accrual is defined")
			}
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			s <- "accrual time"
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		},
		
		.getAccrualTimeWithoutLeadingZero = function() {
			if (length(accrualTime) <= 1) {
				return(NA_real_)
			}
			
			return(accrualTime[2:length(accrualTime)])
		},
		
		isAccrualTimeEnabled = function() {
			if (length(accrualTime) == 0) {
				return(FALSE)
			}
			
			if (length(accrualTime) == 1 && is.na(accrualTime)) {
				return(FALSE)
			}
			
			return(TRUE)
		},
		
		.initFromList = function(accrualTimeList) {
			if (!is.list(accrualTimeList)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must be a list")
			}
			
			if (length(accrualTimeList) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must contain at least one entry")
			}
			
			if (.showWarnings && !all(is.na(accrualIntensity))&& (length(accrualIntensity) != 1 || 
					accrualIntensity != C_ACCRUAL_INTENSITY_DEFAULT)) { 
				warning("'accrualIntensity' (", .arrayToString(accrualIntensity), 
					") will be ignored because 'accrualTime' is a list", call. = FALSE)
			}
			
			accrualTime <<- numeric(0)
			accrualIntensity <<- numeric(0)
			timeRegions <- names(accrualTimeList)
			calculateLastAccrualTimeEnabled <- FALSE
			accrualTime <<- c(accrualTime, 0)
			for (i in 1:length(timeRegions)) {
				timePeriod <- timeRegions[i]
				accrualTimeValue <- accrualTimeList[[timePeriod]]
				.assertIsSingleNumber(accrualTimeValue, paste0("accrualTime[", i, "]"))
				
				settings <- .validateTimePeriod(timePeriod, i = i, n = length(timeRegions), accrualTimeMode = TRUE)
				timePeriod <- settings$timePeriod
				calculateLastAccrualTimeEnabled <- settings$calculateLastAccrualTimeEnabled
				
				if (i < length(timeRegions)) {
					parts <- strsplit(timePeriod, "- *(< *)?", perl = TRUE)[[1]]
					if (length(parts) != 2) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"all regions (", timePeriod, ") must have the format ", 
							"\"time_1 - <time_2\", e.g., \"3 - <6\"")
					}
					accrualTime <<- c(accrualTime, as.numeric(trimws(parts[2])))
				} else {
					parts <- strsplit(timePeriod, " *< *", perl = TRUE)[[1]]
					if (length(parts) == 2) {
						accrualTime <<- c(accrualTime, as.numeric(trimws(parts[2])))
					}
				}
				accrualIntensity <<- c(accrualIntensity, accrualTimeValue)
			}
			
			.setParameterType("accrualTime", C_PARAM_USER_DEFINED)
			.setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)
			
			return(calculateLastAccrualTimeEnabled = calculateLastAccrualTimeEnabled)
		},
		
		.initAccrualIntensityAbsolute = function() {
			if (is.null(maxNumberOfSubjects) || length(maxNumberOfSubjects) != 1 || 
					is.na(maxNumberOfSubjects) || maxNumberOfSubjects == 0) {
				return(invisible())
			}
			
			if (length(accrualTime) >= 2 && length(accrualTime) == length(accrualIntensity) + 1 &&
					!any(is.na(accrualTime)) && !any(is.na(accrualIntensity))) {
					
				len <- length(accrualIntensity) 
				accrualIntensityAbsolute <- maxNumberOfSubjects / sum((accrualTime[2:(len + 1)] - 
					accrualTime[1:len]) * accrualIntensity) * accrualIntensity 
				if (!isTRUE(all.equal(accrualIntensityAbsolute, accrualIntensity, tolerance = 1e-06)) &&
						!isTRUE(all.equal(accrualIntensityAbsolute, 0, tolerance = 1e-06))) {
					
					.validateAccrualTimeAndIntensity()
						
					if (.isAbsoluteAccrualIntensity(accrualIntensity) && 
							.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED) {
						stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
							"'maxNumberOfSubjects' (", maxNumberOfSubjects, ") disagrees with ",
							"the defined accrual time and intensity: ",
							.getFormula(), " = ", .getSampleSize())
					}
						
					accrualIntensityRelative <<- accrualIntensity
					accrualIntensity <<- accrualIntensityAbsolute
					.setParameterType("accrualIntensity", C_PARAM_GENERATED)
					.setParameterType("accrualIntensityRelative", C_PARAM_USER_DEFINED)
				}
			}
		},
		
		.isNoPiecewiseAccrualTime = function(accrualTimeArg) {
			if (length(accrualTimeArg) == 0 || any(is.na(accrualTimeArg)) || 
					!all(is.numeric(accrualTimeArg))) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'accrualTimeArg' must a be valid numeric vector")
			}
			
			if (length(accrualTimeArg) == 1) {
				return(TRUE)
			}
			
			if (length(accrualTimeArg) == 2 && accrualTimeArg[1] == 0) {
				return(TRUE)
			}

			return(FALSE)
		},
		
		.init = function(accrualTimeArg) {
			
			if (length(accrualTimeArg) == 0) {
				return(invisible())
			}
			
			if (length(accrualTimeArg) == 1 && is.numeric(accrualTimeArg) && is.na(accrualTimeArg)) {
				accrualTimeArg <- C_ACCRUAL_TIME_DEFAULT
			}
			
			calculateLastAccrualTimeEnabled <- FALSE
			if (is.list(accrualTimeArg)) {
				calculateLastAccrualTimeEnabled <- .initFromList(accrualTimeArg)
			}
			else if (is.numeric(accrualTimeArg)) {
				
				.assertIsNumericVector(accrualTimeArg, "accrualTime")
				if (length(accrualIntensity) > 1) {
					.assertIsNumericVector(accrualIntensity, "accrualIntensity")
				}
				
				if (.isNoPiecewiseAccrualTime(accrualTimeArg) &&
						(length(accrualIntensity) == 0 || is.null(accrualIntensity) || 
						all(is.na(accrualIntensity)) || 
						all(accrualIntensity == C_ACCRUAL_INTENSITY_DEFAULT))) {
					
					accrualTimeArg <- accrualTimeArg[length(accrualTimeArg)]
					accrualTime <<- c(0L, accrualTimeArg)
					.setParameterType("accrualTime", ifelse(
							identical(as.integer(accrualTime), C_ACCRUAL_TIME_DEFAULT), 
							C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
					
					accrualIntensity <<- C_ACCRUAL_INTENSITY_DEFAULT
					.setParameterType("accrualIntensity", C_PARAM_DEFAULT_VALUE)
					
					.setParameterType("maxNumberOfSubjects", 
						ifelse(length(maxNumberOfSubjects) == 1 && is.na(maxNumberOfSubjects), 
							C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
					
					endOfAccrualIsUserDefined <<- length(accrualTime) == length(accrualIntensity) + 1
					maxNumberOfSubjectsIsUserDefined <<- 
						.getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED
					followUpTimeMustBeUserDefined <<- !endOfAccrualIsUserDefined && 
						!maxNumberOfSubjectsIsUserDefined
					absoluteAccrualIntensityEnabled <<- FALSE
					
					if (maxNumberOfSubjectsIsUserDefined) {
						accrualIntensity <<- maxNumberOfSubjects / accrualTime[length(accrualTime)]
						.setParameterType("accrualIntensity", C_PARAM_GENERATED)
					}
					
					return(invisible())
				}
				
				accrualTime <<- accrualTimeArg
				if (length(accrualTime) == 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'accrualTime' must contain at least one time value")
				}
				
				if (accrualTime[1] != 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"the first value of 'accrualTime' (", .arrayToString(accrualTime), ") must be 0")
				}
				
				.setParameterType("accrualTime", ifelse(
						identical(as.integer(accrualTime), C_ACCRUAL_TIME_DEFAULT), 
						C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
				.setParameterType("accrualIntensity", C_PARAM_USER_DEFINED)
				
			} else {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' must be a list or a numeric vector")
			}
			
			absoluteAccrualIntensityEnabled <<- .isAbsoluteAccrualIntensity(accrualIntensity)
			if (is.null(maxNumberOfSubjects) || length(maxNumberOfSubjects) == 0 ||
					any(is.na(maxNumberOfSubjects))) {
				if (length(accrualTime) != length(accrualIntensity) + 1 || 
						!absoluteAccrualIntensityEnabled) {
					maxNumberOfSubjectsCanBeCalculatedDirectly <<- FALSE
				}
				
				.setParameterType("maxNumberOfSubjects", C_PARAM_NOT_APPLICABLE)
				
			} else {
				if (!(length(accrualTime) %in% c(length(accrualIntensity), 
						length(accrualIntensity) + 1))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"length of 'accrualTime' (", length(accrualTime), 
						") must be equal to length of 'accrualIntensity' if the last 'accrualTime' ",
						"shall be calculated ",
						"based on 'maxNumberOfSubjects' or length of 'accrualIntensity' (", 
						length(accrualIntensity), ") + 1 otherwise")
				}
				if (length(accrualTime) == length(accrualIntensity)) {
					calculateLastAccrualTimeEnabled <- TRUE
				}
				
				.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
			}
			
			endOfAccrualIsUserDefined <<- length(accrualTime) == length(accrualIntensity) + 1
			
			if (calculateLastAccrualTimeEnabled) {
				.calculateRemainingTime()
			} else if (maxNumberOfSubjectsCanBeCalculatedDirectly) {
				if (length(accrualTime) == 1) {
					if (length(maxNumberOfSubjects) > 0 && !is.na(maxNumberOfSubjects) && 
							maxNumberOfSubjects > 0 && maxNumberOfSubjects < accrualIntensity[1]) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"'maxNumberOfSubjects' (", maxNumberOfSubjects, ") ",
							"must be >= ", accrualIntensity[1], " ('accrualIntensity')")
					}
					remainingTime <<- accrualTime
					.setParameterType("remainingTime", C_PARAM_USER_DEFINED)
				} else if (length(accrualTime) > 1) {
					sampleSize <- .getSampleSize()
					if (length(maxNumberOfSubjects) > 0 && !is.na(maxNumberOfSubjects) && 
							maxNumberOfSubjects > 0 && maxNumberOfSubjects < sampleSize) {
						if (length(accrualIntensity) == 1) {
							.setParameterType("maxNumberOfSubjects", C_PARAM_USER_DEFINED)
							accrualTime <<- 0
							.calculateRemainingTime()
						} else {
							if (length(accrualTime) == length(accrualIntensity) + 1 && 
									.isAbsoluteAccrualIntensity(accrualIntensity)) {
								stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
									"'maxNumberOfSubjects' (", maxNumberOfSubjects, ") disagrees with ",
									"the defined accrual time and intensity: ",
									.getFormula(), " = ", sampleSize)
							} else {
								stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjects' (", 
									maxNumberOfSubjects, ") ", "must be >= ", sampleSize)
							}
						}
					} else {
						if ((length(maxNumberOfSubjects) != 1 || is.na(maxNumberOfSubjects)) &&
								.isAbsoluteAccrualIntensity(accrualIntensity)) {
							maxNumberOfSubjects <<- sampleSize
							.setParameterType("maxNumberOfSubjects", C_PARAM_GENERATED)
						}
						remainingTime <<- accrualTime[length(accrualTime)] - accrualTime[length(accrualTime) - 1]
						.setParameterType("remainingTime", C_PARAM_GENERATED)
					}
				}
			}
			
			.validateInitialization()
			
			maxNumberOfSubjectsIsUserDefined <<- .getParameterType("maxNumberOfSubjects") == C_PARAM_USER_DEFINED
			followUpTimeMustBeUserDefined <<- !endOfAccrualIsUserDefined && !maxNumberOfSubjectsIsUserDefined
		},
		
		.getSampleSize = function() {
			if (length(accrualTime) < 2) {
				return(0)
			}
			
			sampleSize <- 0
			for (i in 2:length(accrualTime)) {
				time <- accrualTime[i] - accrualTime[i - 1]
				sampleSize <- sampleSize + time * accrualIntensity[i - 1]
			}
			return(sampleSize)
		},
		
		.getValuesAfterDecimalPoint = function(x) {
			values <- c()
			for (value in x) {
				baseLevel <- value - floor(value)
				if (baseLevel == 0) {
					baseLevel <- 1
				}
				values <- c(values, baseLevel)
			}
			return(values)
		},
		
		.getBaseLevel = function(x) {
			return(min(.getValuesAfterDecimalPoint(x[x > 0])))
		},
		
		.calcSampleSize = function() {
			if (length(accrualTime) <= 1) {
				return(0)
			}
			
			sampleSize <- 0
			for (i in 2:length(accrualTime)) {
				time <- accrualTime[i] - accrualTime[i - 1]
				sampleSize <- sampleSize + time * accrualIntensity[i - 1]
				if (sampleSize >= maxNumberOfSubjects && 
						length(accrualTime) == length(accrualIntensity)) {
					
					i2 <- i
					if (length(accrualTime) == length(accrualIntensity) + 1) {
						i2 <- i - 1
					}
					
					if (.showWarnings) {
						
						n1 <- length(accrualTime) - i + 1
						if (length(accrualTime) == length(accrualIntensity)) {
							n1 <- n1 - 1
						}
						
						if (n1 == 1) {
							warning("Last accrual time value (", 
								accrualTime[length(accrualTime)], ") ignored", call. = FALSE)
						} else if (n1 > 1) {
							warning("Last ", n1, " accrual time values (", 
								.arrayToString(accrualTime[(length(accrualTime) - n1 + 1):length(accrualTime)]), 
								") ignored", call. = FALSE)
						}
						
						n2 <- length(accrualIntensity) - i2 + 1
						if (n2 == 1) {
							warning("Last accrual intensity value (", 
								accrualIntensity[length(accrualIntensity)], ") ignored", call. = FALSE)
						} else if (n2 > 1) {
							warning("Last ", n2, " accrual intensity values (", 
								.arrayToString(accrualIntensity[i2:length(accrualIntensity)]), 
								") ignored", call. = FALSE)
						}
					}
					accrualTime <<- accrualTime[1:(i - 1)]
					accrualIntensity <<- accrualIntensity[1:(i2 - 1)]
					sampleSize <- 0
					if (length(accrualTime) > 1) {
						sampleSize <- .getSampleSize()
					}
					return(sampleSize)
				}
			}
			return(sampleSize)
		},
		
		.calculateRemainingTime = function() {
			.assertIsValidMaxNumberOfSubjects(maxNumberOfSubjects)
			
			if (length(accrualIntensity) == 1) {
				lastAccrualIntensity <- accrualIntensity[1]
				remainingSubjects <- maxNumberOfSubjects
			} else {
				sampleSize <- .calcSampleSize()
				remainingSubjects <- maxNumberOfSubjects - sampleSize
				if (remainingSubjects < 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'maxNumberOfSubjects' (", maxNumberOfSubjects, ") ",
						"is too small for the defined accrual time (minimum = ", sampleSize, ")")
				}
			}
			
			lastAccrualIntensity <- accrualIntensity[length(accrualIntensity)]
			remainingTime <<- remainingSubjects / lastAccrualIntensity
			.setParameterType("remainingTime", C_PARAM_GENERATED)
			accrualTime <<- c(accrualTime, accrualTime[length(accrualTime)] + remainingTime)
			if (any(accrualTime < 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjects' (", maxNumberOfSubjects, ") ",
					"is too small for the defined accrual time")
			}
		},
		
		.validateAccrualTimeAndIntensity = function() {
			if ((length(accrualTime) >= 2 && any(accrualTime[2:length(accrualTime)] < 0))) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'accrualTime' (", .arrayToString(accrualTime), ") must be > 0")
			}
			
			.assertValuesAreStrictlyIncreasing(accrualTime, "accrualTime")
			
			if ((length(accrualTime) > 1) && any(accrualIntensity < 0)) { 
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'accrualIntensity' (", .arrayToString(accrualIntensity), ") must be >= 0")
			}
			
			if (length(accrualIntensity) == 1 && !is.na(accrualIntensity) &&
					accrualIntensity == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"at least one 'accrualIntensity' value must be > 0")
			}
			
			if (length(accrualIntensity) > 0 && accrualIntensity[1] == 0) {
				warning("It makes no sense to start 'accrualIntensity' (", 
					.arrayToString(accrualIntensity), ") with 0")
			}
			
			if (sum(.isAbsoluteAccrualIntensity(accrualIntensity)) > 0 && 
					sum(.isRelativeAccrualIntensity(accrualIntensity)) > 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
					"the values of 'accrualIntensity' (", .arrayToString(accrualIntensity), ") ", 
					"must exclusive absolute or relative, i.e. all (= 0 || >= 1) or all (< 1)")
			}
		},
		
		.validateInitialization = function() {
			.validateAccrualTimeAndIntensity()
			
			piecewiseAccrualEnabled <<- !.isNoPiecewiseAccrualTime(accrualTime)
		}
	)
)


