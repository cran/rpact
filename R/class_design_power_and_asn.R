######################################################################################
#                                                                                    #
# -- Power and average sample number result classes --                               #
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


#' 
#' @name PowerAndAverageSampleNumberResult
#' 
#' @title
#' Power and Average Sample Number Result
#' 
#' @description
#' Class for power and average sample number (ASN) results.
#' 
#' @details 
#' This object can not be created directly; use \code{getPowerAndAverageSampleNumber} 
#' with suitable arguments to create it.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
PowerAndAverageSampleNumberResult <- setRefClass("PowerAndAverageSampleNumberResult",
	contains = "ParameterSet",
	fields = list(
		.design = "TrialDesign",
		nMax = "numeric", 
		theta = "numeric",
		averageSampleNumber = "numeric", 
		calculatedPower = "numeric", 
		overallEarlyStop = "numeric", 
		earlyStop = "matrix", 
		overallReject = "numeric", 
		rejectPerStage = "matrix", 
		overallFutility = "numeric",
		futilityPerStage = "matrix"
	),
	methods = list(
		initialize = function(design, theta = seq(-1, 1, 0.05), nMax = 100L, ...) {
			callSuper(.design = design, theta = theta, nMax = nMax, ...)
			theta <<- .assertIsValidThetaRange(thetaRange = theta, thetaAutoSeqEnabled = FALSE)
			.initPowerAndAverageSampleNumber(design = design, theta = .self$theta, nMax = nMax)			
			.parameterNames <<- .getParameterNames(design)
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		
		clone = function() {
			return(PowerAndAverageSampleNumberResult(design = .self$.design, theta = .self$theta, nMax = .self$nMax))
		},
		
		show = function(showType = 1, digits = NA_integer_) {
			.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
			'Method for automatically printing a power and average sample size (ASN) result'
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the power and average sample size (ASN) object:\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat("Power and average sample size (ASN):\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Output",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				if (.design$kMax > 1) {
					.cat("Legend:\n", heading = 2,
						consoleOutputEnabled = consoleOutputEnabled)
					if (.design$kMax > 1) {
						.cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
			}
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			s <- "power and average sample size (ASN)"
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		},
		
		.initPowerAndAverageSampleNumber = function(design, theta = C_POWER_ASN_THETA_DEFAULT, 
				nMax = C_NA_MAX_DEFAULT) {
			.assertIsTrialDesignInverseNormalOrGroupSequential(design)			
			.assertIsValidSidedParameter(design$sided)
			
			if (nMax <= 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'nMax' must be an integer > 0")
			} 
			
			.setParameterType("nMax", ifelse(nMax == C_NA_MAX_DEFAULT, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED)) 
			
			thetaIsDefault <- length(theta) == length(C_POWER_ASN_THETA_DEFAULT) &&
				sum(theta == C_POWER_ASN_THETA_DEFAULT) == length(theta)
			.setParameterType("theta", ifelse(thetaIsDefault, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			
			kMax <- design$kMax
			
			# initialization
			numberOfThetas <- length(theta)
			
			averageSampleNumber <<- rep(NA_real_, numberOfThetas) 
			.setParameterType("averageSampleNumber", C_PARAM_GENERATED)
			
			calculatedPower <<- rep(NA_real_, numberOfThetas)
			.setParameterType("calculatedPower", C_PARAM_GENERATED)
			
			earlyStop <<- matrix(NA_real_, kMax, numberOfThetas)
			.setParameterType("earlyStop", C_PARAM_GENERATED)
			
			rejectPerStage <<- matrix(NA_real_, kMax, numberOfThetas)
			.setParameterType("rejectPerStage", C_PARAM_GENERATED)
			
			futilityPerStage <<- matrix(NA_real_, kMax - 1, numberOfThetas)
			.setParameterType("futilityPerStage", C_PARAM_GENERATED)
			
			rowNames <- paste("stage =", c(1:kMax))
			rownames(earlyStop) <<- rowNames
			rownames(rejectPerStage) <<- rowNames
			if (kMax > 1) {
				rownames(futilityPerStage) <<- rowNames[1:(kMax - 1)]
			}
			
			for (i in 1:numberOfThetas) {
				result <- .getPowerAndAverageSampleNumber(kMax = design$kMax, 
					informationRates = design$informationRates, 
					futilityBounds = design$futilityBounds, criticalValues = design$criticalValues, 
					sided = design$sided, theta = theta[i], nMax)
				
				averageSampleNumber[i] <<- result$averageSampleNumber
				calculatedPower[i] <<- result$calculatedPower
				earlyStop[1:(kMax - 1), i] <<- result$earlyStop[1:(kMax - 1)]
				rejectPerStage[, i] <<- result$rejectPerStage[1:kMax]
				futilityPerStage[, i] <<- result$futilityPerStage[1:(kMax - 1)]
			}
			
			overallEarlyStop <<- .getOverallParameter(earlyStop)
			.setParameterType("overallEarlyStop", C_PARAM_GENERATED)
			
			overallReject <<- .getOverallParameter(rejectPerStage)
			.setParameterType("overallReject", C_PARAM_GENERATED)
			
			overallFutility <<- .getOverallParameter(futilityPerStage)
			.setParameterType("overallFutility", C_PARAM_GENERATED)
		},
		
		.getPowerAndAverageSampleNumberByDesign = function(design, theta, nMax) {
			if (.isTrialDesignFisher(design)) {
				futilityBounds <- design$alpha0Vec
			} else {
				futilityBounds <- design$futilityBounds
			}
			return(.getPowerAndAverageSampleNumber(kMax = design$kMax, 
					informationRates = design$informationRates, futilityBounds = futilityBounds, 
					criticalValues = design$criticalValues, sided = design$sided, 
					theta = theta, nMax = nMax))
		},
		
		.getPowerAndAverageSampleNumber = function(kMax, informationRates, futilityBounds, 
			criticalValues, sided, theta, nMax) {
			
			if (sided == 2) {
				decisionMatrix <- matrix(c(-criticalValues - theta * sqrt(nMax * informationRates), 
						criticalValues - theta * sqrt(nMax * informationRates)), nrow = 2, byrow = TRUE)
			} else {
				shiftedFutilityBounds <- futilityBounds - theta * sqrt(nMax * informationRates[1:(kMax - 1)])
				shiftedFutilityBounds[futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT
				decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
						criticalValues - theta * sqrt(nMax * informationRates)), nrow = 2, byrow = TRUE)
			}
			
			probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
			.averageSampleNumber <- nMax - sum((probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + 
						probs[1, 1:(kMax - 1)]) * 
					(informationRates[kMax] - informationRates[1:(kMax - 1)]) * nMax)
			
			.futilityPerStage <- rep(NA_real_, kMax)
			if (sided == 2) {
				.calculatedPower <- sum(probs[3, 1:kMax] - probs[2, 1:kMax] + probs[1, 1:kMax])
				.rejectPerStage <- probs[3, 1:kMax] - probs[2, 1:kMax] + probs[1, 1:kMax]
			} else {
				.calculatedPower <- sum(probs[3, 1:kMax] - probs[2, 1:kMax])
				.rejectPerStage <- probs[3, 1:kMax] - probs[2, 1:kMax]
				if (kMax > 1) {
					.futilityPerStage <- probs[1, 1:(kMax - 1)]
				} 	
			}
			
			.earlyStop <- rep(NA_real_, kMax)
			if (kMax > 1) {
				.earlyStop <- probs[3, 1:(kMax - 1)] - probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]
			}
			
			return(list(
					averageSampleNumber = .averageSampleNumber,
					calculatedPower = .calculatedPower,
					earlyStop = .earlyStop,
					rejectPerStage = .rejectPerStage,
					futilityPerStage = .futilityPerStage
				))
		},
		
		.getOverallParameter = function(parameter) {
			if (is.null(parameter) || length(parameter) == 0) {
				return(rep(NA_real_, length(theta)))
			}
			
			overallParameter <- parameter
			overallParameter[is.na(overallParameter)] <- 0
			overallParameter <- colSums(overallParameter)			
			return(overallParameter)
		}
	)
)

#'
#' @name PowerAndAverageSampleNumberResult_as.data.frame
#' 
#' @title
#' Coerce Power And Average Sample Number Result to a Data Frame
#'
#' @description
#' Returns the \code{PowerAndAverageSampleNumberResult} as data frame.
#' 
#' @details
#' Coerces the object to a data frame.
#' 
#' @export
#' 
#' @keywords internal
#' 
as.data.frame.PowerAndAverageSampleNumberResult <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = FALSE, includeAllParameters = FALSE, ...) {
	
	parameterNames <- x$.getVisibleFieldNames()
	parameterNames <- parameterNames[parameterNames != "nMax"]	
	dataFrame <- x$.getAsDataFrame(parameterNames = parameterNames, 
		niceColumnNamesEnabled = niceColumnNamesEnabled, includeAllParameters = includeAllParameters,
		tableColumnNames = .getTableColumnNames(design = x$.design))
	return(dataFrame)
}

