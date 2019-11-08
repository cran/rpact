######################################################################################
#                                                                                    #
# -- RPACT design utilities --                                                       #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 26-02-2019                                                                   #
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

#' @include f_core_assertions.R
NULL

# This function generates the piecewise exponential survival function or (if kappa != 1) a Weibull cdf 
.getPiecewiseExponentialDistributionSingleTime <- function(
		time, piecewiseLambda, piecewiseSurvivalTime = NA_real_, kappa) {

	if (length(piecewiseLambda) == 1) {
		if (kappa <= 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'kappa' (", kappa, ") must be > 0")
		}
		
		return(pweibull(time, kappa, scale = 1 / piecewiseLambda, lower.tail = TRUE, log.p = FALSE))
	} 
	
	if (length(piecewiseSurvivalTime) != length(piecewiseLambda)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"length of 'piecewiseSurvivalTime' (", .arrayToString(piecewiseSurvivalTime), 
			") must be equal to length of 'piecewiseLambda' (", .arrayToString(piecewiseLambda), ")")
	}
	
	piecewiseSurvivalTime <- .getPiecewiseExpStartTimesWithoutLeadingZero(piecewiseSurvivalTime)
	
	if (kappa != 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"Weibull distribution cannot be used for piecewise survival definition")
	}
	
	len <- length(piecewiseSurvivalTime)
	for (i in 1:len) {
		if (time <= piecewiseSurvivalTime[i]) {
			if (i == 1) {
				return(1 - exp(-(piecewiseLambda[1] * time)))
			} 
			y <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
			if (i > 2) {
				y <- y + sum(piecewiseLambda[2:(i - 1)] * 
					(piecewiseSurvivalTime[2:(i - 1)] -	piecewiseSurvivalTime[1:(i - 2)]))
			}
			y <- y + piecewiseLambda[i] * (time - piecewiseSurvivalTime[i - 1])
			return(1 - exp(-y))
		}
	}
	if (len == 1) {
		y <- piecewiseLambda[1] * piecewiseSurvivalTime[1] + 
			piecewiseLambda[len + 1] * (time - piecewiseSurvivalTime[len])
	} else {
		y <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
			sum(piecewiseLambda[2:len] * (piecewiseSurvivalTime[2:len] - 
			piecewiseSurvivalTime[1:(len - 1)])) +
			piecewiseLambda[len + 1] * (time - piecewiseSurvivalTime[len])
	}
	return(1 - exp(-y))
}

.getPiecewiseExponentialSingleQuantile <- function(
		quantile, piecewiseLambda, piecewiseSurvivalTime, kappa) {
	
	if (length(piecewiseLambda) == 1)	{
		if (kappa <= 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
					"kappa needs to a positive number")
		}
		return((-log(1 - quantile))^(1 / kappa) / piecewiseLambda[1])
	}
	
	if (kappa != 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"Weibull distribution cannot be used for piecewise survival definition")
	}
	
	cdfValues <- .getPiecewiseExponentialDistribution(piecewiseSurvivalTime, 
		piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda, kappa = 1)
	cdfValues <- cdfValues[2:length(cdfValues)] # use values without a leading 0
	
	piecewiseSurvivalTime <- .getPiecewiseExpStartTimesWithoutLeadingZero(piecewiseSurvivalTime)
	
	len <- length(piecewiseSurvivalTime)
	for (i in 1:len) {
		if (quantile <= cdfValues[i]) {
			if (i == 1) {
				return(-log(1 - quantile) / piecewiseLambda[1])
			} 
			y <- piecewiseLambda[1] * piecewiseSurvivalTime[1]
			if (i > 2) {
				y <- y + sum(piecewiseLambda[2:(i - 1)] * 
					(piecewiseSurvivalTime[2:(i - 1)] - piecewiseSurvivalTime[1:(i - 2)]))
			}
			return(piecewiseSurvivalTime[i - 1] - (log(1 - quantile) + y) / piecewiseLambda[i])
		}
	}
	
	if (len == 1) {
		return(piecewiseSurvivalTime[1] - (log(1 - quantile) + piecewiseLambda[1] * 
			piecewiseSurvivalTime[1]) / piecewiseLambda[2])		
	} 
	
	y <- piecewiseLambda[1] * piecewiseSurvivalTime[1] +
		sum(piecewiseLambda[2:len] * (piecewiseSurvivalTime[2:len] - 
		piecewiseSurvivalTime[1:(len - 1)])) 

	return(piecewiseSurvivalTime[len] - (log(1 - quantile) + y) / piecewiseLambda[len + 1])
	
}

.getPiecewiseExponentialDistribution <- function(time, piecewiseLambda, piecewiseSurvivalTime, kappa) {
	if (length(time) == 1 && length(piecewiseSurvivalTime) == 1 && 
			identical(time, piecewiseSurvivalTime) && length(piecewiseLambda) > 1) {
			
		result <- c()
		for (lambda in piecewiseLambda) {
			result <- c(result, .getPiecewiseExponentialDistributionSingleTime(
				time, lambda, piecewiseSurvivalTime, kappa))
		}
		return(result)
	}
	
	result <- c()
	for (timeValue in time) {
		result <- c(result, .getPiecewiseExponentialDistributionSingleTime(
				timeValue, piecewiseLambda, piecewiseSurvivalTime, kappa))
	}
	return(result)
}

.getPiecewiseExponentialSettings <- function(..., piecewiseSurvivalTime = NA_real_, 
		piecewiseLambda = NA_real_, kappa = 1) {
	
	if (!all(is.na(piecewiseLambda)) && is.list(piecewiseSurvivalTime)) {
		stop("'piecewiseSurvivalTime' needs to be a numeric vector and not a list ", 
			"because 'piecewiseLambda' (", piecewiseLambda, ") is defined separately")
	}
	
	if (any(is.na(piecewiseSurvivalTime))) {
		.assertIsSingleNumber(kappa, "kappa")
	}
	
	if (length(piecewiseLambda) == 1 && !is.na(piecewiseLambda) && 
			length(piecewiseSurvivalTime) > 0 && !all(is.na(piecewiseSurvivalTime))) {
		warning("Argument 'piecewiseSurvivalTime' will be ignored because ",
			"length of 'piecewiseLambda' is 1", call. = FALSE)	
	}

	setting <- PiecewiseSurvivalTime(
		piecewiseSurvivalTime = piecewiseSurvivalTime, 
		lambda2 = piecewiseLambda, 
		hazardRatio = 1, kappa = kappa,
		delayedResponseAllowed = FALSE)
	
	return(list(piecewiseSurvivalTime = setting$piecewiseSurvivalTime,
			piecewiseLambda = setting$lambda2))
}

#' 
#' @title
#' The Piecewise Exponential Distribution
#'
#' @description
#' Distribution function, quantile function and random number generation for the 
#' piecewise exponential distribution. 
#' 
#' @param t,time Vector of time values.
#' @param q,quantile Vector of quantiles.
#' @param n Number of observations.
#' @param s,piecewiseSurvivalTime Vector of start times defining the "time pieces".
#' @param lambda,piecewiseLambda Vector of lambda values (hazard rates) corresponding to the start times.
#' @param kappa The kappa value. Is needed for the specification of the Weibull distribution. 
#'        In this case, no piecewise definition is possible, i.e., 
#'        only lambda and kappa need to be specified.
#'        This function is equivalent to pweibull(t, kappa, 1 / lambda) of the R core system, i.e., 
#'        the scale parameter is 1 / 'hazard rate'.
#'        For example, getPiecewiseExponentialDistribution(time = 130, 
#'        piecewiseLambda = 0.01, kappa = 4.2) and 
#'        pweibull(q = 130, shape = 4.2, scale = 1 /0.01) provide the sample result. 
#' @param ... Ensures that all arguments after \code{time} are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details
#' 
#' \code{getPiecewiseExponentialDistribution} (short: \code{ppwexp}), 
#' \code{getPiecewiseExponentialQuantile} (short: \code{qpwexp}), and 
#' \code{getPiecewiseExponentialRandomNumbers} (short: \code{rpwexp}) provide 
#' probabilities, quantiles, and random numbers according to a piecewise 
#' exponential or a Weibull distribution.
#' The piecewise definition is performed through a vector of 
#' starting times (\code{piecewiseSurvivalTime}) and a vector of hazard rates (\code{piecewiseLambda}).
#' You can also use a list that defines the starting times and piecewise 
#' lambdas together and define piecewiseSurvivalTime as this list.
#' The list needs to have the form, for example, 
#' piecewiseSurvivalTime <- list(
#'     "0 - <6"   = 0.025, 
#'     "6 - <9"   = 0.04, 
#'     "9 - <15"  = 0.015, 
#'     ">=15"      = 0.007) 
#' For the Weibull case, you can also specify a shape parameter kappa in order to 
#' calculated probabilities, quantiles, or random numbers.
#' In this case, no piecewise definition is possible, i.e., only piecewiseLambda and 
#' kappa need to be specified. 
#' 
#' @examples
#' 
#' # Calculate probabilties for a range of time values for a 
#' # piecewise exponential distribution with hazard rates 
#' # 0.025, 0.04, 0.015, and 0.007 in the intervals 
#' # [0, 6), [6, 9), [9, 15), [15,Inf), respectively,
#' # and re-return the time values: 
#' piecewiseSurvivalTime <- list(
#'     "0 - <6"   = 0.025, 
#'     "6 - <9"   = 0.04, 
#'     "9 - <15"  = 0.015, 
#'     ">=15"     = 0.01)
#' y <- getPiecewiseExponentialDistribution(seq(0, 150, 15), 
#'     piecewiseSurvivalTime = piecewiseSurvivalTime)
#' getPiecewiseExponentialQuantile(y, 
#'     piecewiseSurvivalTime = piecewiseSurvivalTime)
#' 
#' @name utilitiesForPiecewiseExponentialDistribution
#' 
NULL

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
getPiecewiseExponentialDistribution <- function(time, ..., 
		piecewiseSurvivalTime = NA_real_, piecewiseLambda = NA_real_, kappa = 1) {
	
	.warnInCaseOfUnknownArguments(functionName = "getPiecewiseExponentialDistribution", ...)
	.assertIsNumericVector(time, "time")
	if (any(time < 0))  {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"time needs to be a non-negative number")
	}
	
	settings <- .getPiecewiseExponentialSettings(piecewiseSurvivalTime = piecewiseSurvivalTime,
		piecewiseLambda = piecewiseLambda, kappa = kappa)	
	
	return(.getPiecewiseExponentialDistribution(time = time, 
		piecewiseSurvivalTime = settings$piecewiseSurvivalTime, 
		piecewiseLambda = settings$piecewiseLambda, kappa = kappa))
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
ppwexp <- function(t, ..., s = NA_real_, lambda = NA_real_, kappa = 1) {
	getPiecewiseExponentialDistribution(time = t, 
		piecewiseSurvivalTime = s, piecewiseLambda = lambda, kappa = kappa, ...)
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
getPiecewiseExponentialQuantile <- function(quantile, ..., 
		piecewiseSurvivalTime = NA_real_, piecewiseLambda = NA_real_, kappa = 1) {
	
	.warnInCaseOfUnknownArguments(functionName = "getPiecewiseExponentialQuantile", ...)
	.assertIsNumericVector(quantile, "quantile")
	if (any(quantile < 0) || any(quantile > 1))  {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"quantile needs to be within [0; 1]")
	}
	
	settings <- .getPiecewiseExponentialSettings(piecewiseSurvivalTime = piecewiseSurvivalTime,
		piecewiseLambda = piecewiseLambda, kappa = kappa)
	
	result <- c()
	for (quantileValue in quantile) {
		result <- c(result, .getPiecewiseExponentialSingleQuantile(quantileValue, 
			piecewiseSurvivalTime = settings$piecewiseSurvivalTime, 
			piecewiseLambda = settings$piecewiseLambda, kappa))
	}
	return(result)
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
qpwexp <- function(q, ..., s = NA_real_, lambda = NA_real_, kappa = 1) {
	getPiecewiseExponentialQuantile(quantile = q, 
		piecewiseSurvivalTime = s, piecewiseLambda = lambda, kappa = kappa, ...)
}

.getPiecewiseExponentialRandomNumbersFast <- function(n, piecewiseSurvivalTime, piecewiseLambda) {
	result <- rexp(n, rate = piecewiseLambda[1])
	if (length(piecewiseSurvivalTime) > 1) {
		for (i in 2:length(piecewiseSurvivalTime)) {
			result <- ifelse(result < piecewiseSurvivalTime[i], 
				result, piecewiseSurvivalTime[i] + rexp(n, rate = piecewiseLambda[i]))
		}
	}
	result
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
getPiecewiseExponentialRandomNumbers <- function(n, ..., 
		piecewiseSurvivalTime = NA_real_, piecewiseLambda = NA_real_, kappa = 1) {
		
	.warnInCaseOfUnknownArguments(functionName = "getPiecewiseExponentialRandomNumbers", ...)
	.assertIsSingleInteger(n, "n", validateType = FALSE)
	if (n <= 0)  {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"n needs to be a positive integer.")
	}
	
	settings <- .getPiecewiseExponentialSettings(piecewiseSurvivalTime = piecewiseSurvivalTime,
		piecewiseLambda = piecewiseLambda, kappa = kappa)
	
	if (kappa == 1) {
		return(.getPiecewiseExponentialRandomNumbersFast(n, 
			piecewiseSurvivalTime = settings$piecewiseSurvivalTime, 
			piecewiseLambda = settings$piecewiseLambda))
	}
	
	randomQuantiles <- runif(n, 0, 1)
	result <- c()
	for (quantile in randomQuantiles) {
		result <- c(result, .getPiecewiseExponentialSingleQuantile(quantile, 
			piecewiseSurvivalTime = settings$piecewiseSurvivalTime, 
			piecewiseLambda = settings$piecewiseLambda, kappa = kappa))
	}
	return(result)
}

#' @rdname utilitiesForPiecewiseExponentialDistribution
#' @export
rpwexp <- function(n, ..., s = NA_real_, lambda = NA_real_, kappa = 1) {
	getPiecewiseExponentialRandomNumbers(n = n, 
		piecewiseSurvivalTime = s, piecewiseLambda = lambda, kappa = kappa, ...)
}

#' 
#' @title
#' Survival Helper Functions for Conversion of Pi, Lambda, Median
#'
#' @description
#' Functions to convert pi, lambda and median values into each other. 
#' 
#' @param piValue,pi1,pi2,lambda,median Value that shall be converted.
#' @param eventTime The assumed time under which the event rates 
#'        are calculated, default is \code{12}.
#' @param kappa The scale parameter of the Weibull distribution, default is \code{1}. 
#'        The Weibull distribution cannot be used for the piecewise
#' 		  definition of the survival time distribution.
#' 
#' @details
#' Can be used, e.g., to convert median values into pi or lambda values for usage in
#' \code{\link{getSampleSizeSurvival}} or \code{\link{getPowerSurvival}}.
#' 
#' @name utilitiesForSurvivalTrials
#' 
NULL

#' @rdname utilitiesForSurvivalTrials
#' @export
getLambdaByPi <- function(piValue, eventTime = C_EVENT_TIME_DEFAULT, kappa = 1) {
	.assertIsValidPi(piValue, "pi")
	.assertIsValidKappa(kappa)
	for (value in piValue) {
		if (value > 1 - 1e-15 && value < 1 + 1e-15) {
			stop("'pi' must be != 1")
		}
	}
	return((-log(1 - piValue))^(1 / kappa) / eventTime)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getLambdaByMedian <- function(median, kappa = 1) {
	.assertIsValidKappa(kappa)
	return(log(2)^(1 / kappa) / median)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getHazardRatioByPi <- function(pi1, pi2, eventTime = C_EVENT_TIME_DEFAULT, kappa = 1) {
	.assertIsValidPi(pi1, "pi1")
	.assertIsValidPi(pi2, "pi2")
	.assertIsValidKappa(kappa)
	return((getLambdaByPi(pi1, eventTime, kappa) / getLambdaByPi(pi2, eventTime, kappa))^kappa)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getPiByLambda <- function(lambda, eventTime = C_EVENT_TIME_DEFAULT, kappa = 1) {
	.assertIsValidLambda(lambda)
	.assertIsValidKappa(kappa)
	return(1 - exp(-(lambda * eventTime)^kappa))
}

# alternative: return(1 - exp(-(log(2)^(1 / kappa) / median * eventTime)^kappa))
#' @rdname utilitiesForSurvivalTrials
#' @export
getPiByMedian <- function(median, eventTime = C_EVENT_TIME_DEFAULT, kappa = 1) {
	.assertIsValidKappa(kappa)
	return(1 - 2^(-(eventTime / median)^kappa))
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getMedianByLambda <- function(lambda, kappa = 1) {
	.assertIsValidLambda(lambda)
	.assertIsValidKappa(kappa)
	return(log(2)^(1 / kappa) / lambda)
}

#' @rdname utilitiesForSurvivalTrials
#' @export
getMedianByPi <- function(piValue, eventTime = C_EVENT_TIME_DEFAULT, kappa = 1) {
	.assertIsValidPi(piValue, "piValue")
	getMedianByLambda(getLambdaByPi(piValue, eventTime, kappa), kappa)
}



