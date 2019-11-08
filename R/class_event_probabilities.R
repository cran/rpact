######################################################################################
#                                                                                    #
# -- Event probabilities classes --                                                  #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 06-05-2019                                                                   #
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
#' @name EventProbabilities
#' 
#' @title
#' Event Probabilities
#' 
#' @description 
#' Class for definition of event probabilities.
#' 
#' @details
#' \code{EventProbabilities} is a class for definition of event probabilities.
#' 
#' @importFrom methods new
#'
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' @include class_time.R
#' 
#' @keywords internal
#' 
EventProbabilities <- setRefClass("EventProbabilities",
	contains = "ParameterSet",
	fields = list(
		.piecewiseSurvivalTime = "PiecewiseSurvivalTime",
		.accrualTime = "AccrualTime",
		time = "numeric", 
		accrualTime = "numeric", 
		accrualIntensity = "numeric", 
		kappa = "numeric", 
		piecewiseSurvivalTime = "numeric", 
		lambda1 = "numeric",
		lambda2 = "numeric", 
		allocationRatioPlanned = "numeric", 
		hazardRatio = "numeric",
		dropoutRate1 = "numeric", 
		dropoutRate2 = "numeric", 
		dropoutTime = "numeric",
		maxNumberOfSubjects = "numeric",
		overallEventProbabilities = "numeric",
		eventProbabilities1 = "numeric",
		eventProbabilities2 = "numeric"
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
			
			.parameterNames <<- C_PARAMETER_NAMES
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		
		show = function(showType = 1, digits = NA_integer_) {
			.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
			'Method for automatically printing event probabilities objects'
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the event probabilities object of class ",
					methods::classLabel(class(.self)), ":\n\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat("Event probabilities at given time:\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Time and output",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				
				.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
				.cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)
				
				.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
			}
		}
	)
)

#' 
#' @name NumberOfSubjects
#' 
#' @title
#' Number Of Subjects
#' 
#' @description 
#' Class for definition of number of subjects results.
#' 
#' @details
#' \code{NumberOfSubjects} is a class for definition of number of subjects results.
#' 
#' @importFrom methods new
#'
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' @include class_time.R
#' 
#' @keywords internal
#' 
NumberOfSubjects <- setRefClass("NumberOfSubjects",
	contains = "ParameterSet",
	fields = list(
		.accrualTime = "AccrualTime",
		time = "numeric", 
		accrualTime = "numeric", 
		accrualIntensity = "numeric", 
		maxNumberOfSubjects = "numeric",
		numberOfSubjects = "numeric"
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
			
			.parameterNames <<- C_PARAMETER_NAMES
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		
		show = function(showType = 1, digits = NA_integer_) {
			.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
			'Method for automatically printing number of subjects objects'
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the number of subjects object of class ",
					methods::classLabel(class(.self)), ":\n\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat("Number of recruited subjects at given time:\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Time and output",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				
				.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
			}
		}
	)
)

