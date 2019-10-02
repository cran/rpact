######################################################################################
#                                                                                    #
# -- Stage results classes --                                                        #
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

.getStageResultsClassNames <- function() {
	return(c("StageResultsMeans", 
		"StageResultsRates",
		"StageResultsSurvival",
		"StageResultsMeansMultiArmed", 
		"StageResultsRatesMultiArmed",
		"StageResultsSurvivalMultiArmed"))
}

#' 
#' @name StageResults
#' 
#' @title
#' Basic Stage Results
#' 
#' @description 
#' Basic class for stage results.
#' 
#' @details
#' \code{StageResults} is the basic class for \code{StageResultsMeans}, 
#' \code{StageResultsRates}, and \code{StageResultsSurvival}.
#' 
#' @field testStatistics The stage-wise test statistics.
#' @field pValues The stage-wise p-values.
#' @field combInverseNormal The inverse normal test.
#' @field combFisher The Fisher's combination test.
#' @field effectSizes The effect sizes for different designs.
#' @field testActions The action drawn from test result.
#' @field weightsFisher The weights for Fisher's combination test.
#' @field weightsInverseNormal The weights for inverse normal statistic.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' @include class_core_plot_settings.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResults <- setRefClass("StageResults",
	contains = "ParameterSet",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.dataInput = "Dataset",
		stages = "integer",
		pValues = "numeric", 
		weightsFisher = "numeric", 
		weightsInverseNormal = "numeric", 
		thetaH0 = "numeric", 
		direction = "character" 
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
		},
		
		init = function(design, dataInput) {
			.design <<- design
			.dataInput <<- dataInput
			
			.plotSettings <<- PlotSettings()
			if (!missing(design)) {
				stages <<- c(1:design$kMax)
				if (design$kMax == C_KMAX_DEFAULT) {
					.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
				} else {
					.setParameterType("stages", C_PARAM_USER_DEFINED)
				}
				
				.parameterNames <<- .getParameterNames(design)
			}
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
			
			.setParameterType("pValues", ifelse(
				.isMultiArmed(), C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED))
			.setParameterType("thetaH0", ifelse(
				identical(thetaH0, C_THETA_H0_MEANS_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			.setParameterType("direction", ifelse(
				identical(direction, C_DIRECTION_UPPER), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		show = function(showType = 1) {
			.show(showType = showType, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, consoleOutputEnabled = TRUE) {
			'Method for automatically printing stage results'
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the stage results object of class ",
					methods::classLabel(class(.self)), ":\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {		
				.cat(.toString(startWithUpperCase = TRUE), ":\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
#				.showParametersOfOneGroup(.getDerivedParameters(), "Derived from user defined parameters",
#					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Output",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
			}
		},
		
		isDirectionUpper = function() {
			return(direction == C_DIRECTION_UPPER)
		},
		
		.isMultiArmed = function() {
			return(grepl("multi", tolower(class(.self))))
		},
		
		.getParametersToShow = function() {
			return(c("stages"))
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			prefix <- paste(ifelse(startWithUpperCase, "Stage results of", "stage results of"))
			if (class(.self) == "StageResultsMeans") {
				return(paste(prefix, "means"))
			}
			
			if (class(.self) == "StageResultsMeansMultiArmed") {
				return(paste(prefix, "multi-armed means"))
			}
			
			if (class(.self) == "StageResultsRates") {
				return(paste(prefix, "rates"))
			}
			
			if (class(.self) == "StageResultsRatesMultiArmed") {
				return(paste(prefix, "multi-armed rates"))
			}
			
			if (class(.self) == "StageResultsSurvival") {
				return(paste(prefix, "survival data"))
			}
			
			if (class(.self) == "StageResultsSurvivalMultiArmed") {
				return(paste(prefix, "multi-armed survival"))
			}
			
			return("unknown stage results")
		},
		
		getDataInput = function() {
			return(.dataInput)
		},
		
		getNumberOfGroups = function() {
			return(.dataInput$getNumberOfGroups())
		},
		
		isOneSampleDataset = function() {
			return(getNumberOfGroups() == 1)
		},
		
		isTwoSampleDataset = function() {
			return(getNumberOfGroups() == 2)
		},
		
		isDatasetMeans = function() {
			return(.dataInput$isDatasetMeans())
		},
		
		isDatasetRates = function() {
			return(.dataInput$isDatasetRates())
		},
		
		isDatasetSurvival = function() {
			return(.dataInput$isDatasetSurvival())
		},
		
		getNumberOfStages = function() {
			if (.isMultiArmed()) {
				return(max(ncol(stats::na.omit(effectSizes)), 
						ncol(stats::na.omit(separatePValues))))
			}
			return(max(length(stats::na.omit(effectSizes)), 
				length(stats::na.omit(pValues))))
		}
	)
)

#' 
#' @name StageResultsMeans
#' 
#' @title
#' Stage Results of Means
#' 
#' @description 
#' Class for stage results of means.
#' 
#' @details
#' This object can not be created directly; use \code{getStageResults} 
#' with suitable arguments to create the stage results of a dataset of means.
#' 
#' @field testStatistics The stage-wise test statistics.
#' @field pValues The stage-wise p-values.
#' @field combInverseNormal The inverse normal test.
#' @field combFisher The Fisher's combination test.
#' @field effectSizes The effect sizes for different designs.
#' @field testActions The action drawn from test result.
#' @field weightsFisher The weights for Fisher's combination test.
#' @field weightsInverseNormal The weights for inverse normal statistic.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResultsMeans <- setRefClass("StageResultsMeans",
	contains = "StageResults",
	fields = list(
		combInverseNormal = "numeric", 
		combFisher = "numeric", 
		overallTestStatistics = "numeric", 
		overallPValues = "numeric",
		effectSizes = "numeric",
		testStatistics = "numeric",
		overallMeans = "numeric",
		overallMeans1 = "numeric", 
		overallMeans2 = "numeric", 
		overallStDevs = "numeric",
		overallStDevs1 = "numeric", 
		overallStDevs2 = "numeric", 
		overallSampleSizes = "numeric",
		overallSampleSizes1 = "numeric",
		overallSampleSizes2 = "numeric",
		equalVariances = "logical",
		normalApproximation = "logical" 
	),
	methods = list(
		initialize = function(design, dataInput, ..., equalVariances = TRUE, normalApproximation = FALSE) {
			callSuper(.design = design, .dataInput = dataInput, ...,  
				equalVariances = equalVariances, normalApproximation = normalApproximation)
			init(design = design, dataInput = dataInput)
			
			for (param in c(
					"weightsFisher", 
					"weightsInverseNormal", 
					"combFisher", 
					"combInverseNormal")) {
				.setParameterType(param, C_PARAM_NOT_APPLICABLE)
			}
			
			for (param in .getParametersToShow()) {
				if (.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
					.setParameterType(param, C_PARAM_GENERATED)
				}
			}
			
			.setParameterType("equalVariances", ifelse(
					identical(equalVariances, TRUE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			.setParameterType("normalApproximation", ifelse(
					identical(normalApproximation, FALSE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
		},
		.getParametersToShow = function() {
			parametersToShow <- c(
				"stages",
				"overallTestStatistics", 
				"overallPValues"
			)
			if (.dataInput$getNumberOfGroups() == 1) {
				parametersToShow <- c(parametersToShow,
					"overallMeans", 
					"overallStDevs",
					"overallSampleSizes"
				)
			}
			else if (.dataInput$getNumberOfGroups() == 2) {
				parametersToShow <- c(parametersToShow,
					"overallMeans1", 
					"overallMeans2", 
					"overallStDevs1", 
					"overallStDevs2",
					"overallSampleSizes1",
					"overallSampleSizes2"
				)

			}
			parametersToShow <- c(parametersToShow,	
				"testStatistics", 
				"pValues", 
				"effectSizes"
			)
			if (.isTrialDesignInverseNormal(.design)) {
				parametersToShow <- c(parametersToShow,			
					"combInverseNormal", 
					"weightsInverseNormal"
				)
			}
			else if (.isTrialDesignFisher(.design)) {
				parametersToShow <- c(parametersToShow,
					"combFisher", 
					"weightsFisher"
				)
			}
			parametersToShow <- c(parametersToShow, 
				"thetaH0",
				"direction",
				"normalApproximation"
			)
			if (.dataInput$getNumberOfGroups() == 2) {
				parametersToShow <- c(parametersToShow, 
					"equalVariances"
				)
			}
			return(parametersToShow)
		}
	)
)

#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResultsMeansMultiArmed <- setRefClass("StageResultsMeansMultiArmed",
	contains = "StageResults",
	fields = list(
		stage = "integer",
		combInverseNormal = "matrix", 
		combFisher = "matrix", 
		overallTestStatistics = "matrix", 
		overallStDevs = "matrix",
		overallPValues = "matrix",
		testStatistics = "matrix",
		separatePValues = "matrix",
		effectSizes = "matrix",
		singleStepAdjustedPValues = "matrix",
		intersectionTest = "character",
		varianceOption = "character",
		normalApproximation = "logical",
		directionUpper = "logical" 
	),
	methods = list(
		initialize = function(design, dataInput, ..., varianceOption = C_VARIANCES_OPTION_DEFAULT, 
				normalApproximation = FALSE) {
			callSuper(.design = design, .dataInput = dataInput, ...,  
				varianceOption = varianceOption, normalApproximation = normalApproximation)
			init(design = design, dataInput = dataInput)
			
			for (param in c("singleStepAdjustedPValues",
					"weightsFisher", 
					"weightsInverseNormal", 
					"combFisher", 
					"combInverseNormal")) {
				.setParameterType(param, C_PARAM_NOT_APPLICABLE)
			}
			
			for (param in .getParametersToShow()) {
				if (.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
					.setParameterType(param, C_PARAM_GENERATED)
				}
			}
			
			.setParameterType("varianceOption", ifelse(
					identical(varianceOption, C_VARIANCES_OPTION_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			.setParameterType("normalApproximation", ifelse(
					identical(normalApproximation, FALSE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			.setParameterType("directionUpper", ifelse(
					identical(directionUpper, C_DIRECTION_UPPER_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
		},
		.getParametersToShow = function() {
			parametersToShow <- c(
				"stages",
				"thetaH0",
				"direction",
				"normalApproximation",
				"directionUpper",
				"varianceOption",
				
				"overallTestStatistics", 
				"overallPValues",
				"overallStDevs",
				"testStatistics", 
				"separatePValues", 
				"effectSizes",
				"singleStepAdjustedPValues"
			)
			if (.isTrialDesignInverseNormal(.design)) {
				parametersToShow <- c(parametersToShow,			
					"combInverseNormal", 
					"weightsInverseNormal"
				)
			}
			else if (.isTrialDesignFisher(.design)) {
				parametersToShow <- c(parametersToShow,
					"combFisher", 
					"weightsFisher"
				)
			}
			return(parametersToShow)
		}
	)
)

#' 
#' @name StageResultsRates
#' 
#' @title
#' Stage Results of Rates
#' 
#' @description 
#' Class for stage results of rates.
#' 
#' @details
#' This object can not be created directly; use \code{getStageResults} 
#' with suitable arguments to create the stage results of a dataset of rates.
#' 
#' @field testStatistics The stage-wise test statistics.
#' @field pValues The stage-wise p-values.
#' @field combInverseNormal The inverse normal test.
#' @field combFisher The Fisher's combination test.
#' @field effectSizes The effect sizes for different designs.
#' @field testActions The action drawn from test result.
#' @field weightsFisher The weights for Fisher's combination test.
#' @field weightsInverseNormal The weights for inverse normal statistic.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResultsRates <- setRefClass("StageResultsRates",
	contains = "StageResults",
	fields = list(
		combInverseNormal = "numeric", 
		combFisher = "numeric", 
		overallTestStatistics = "numeric", 
		overallPValues = "numeric",
		effectSizes = "numeric",
		testStatistics = "numeric",
		overallEvents = "numeric",
		overallEvents1 = "numeric", 
		overallEvents2 = "numeric",
		overallSampleSizes = "numeric",
		overallSampleSizes1 = "numeric",
		overallSampleSizes2 = "numeric",
		normalApproximation = "logical" 
	),
	methods = list(
		initialize = function(design, dataInput, ..., normalApproximation = TRUE) {
			callSuper(.design = design, .dataInput = dataInput, ...,  
				normalApproximation = normalApproximation)
			init(design = design, dataInput = dataInput)
			
			for (param in c(
					"weightsFisher", 
					"weightsInverseNormal", 
					"combFisher", 
					"combInverseNormal")) {
				.setParameterType(param, C_PARAM_NOT_APPLICABLE)
			}
			
			for (param in .getParametersToShow()) {
				if (.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
					.setParameterType(param, C_PARAM_GENERATED)
				}
			}
			
			.setParameterType("normalApproximation", ifelse(
					identical(normalApproximation, TRUE), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
		},
		.getParametersToShow = function() {
			parametersToShow <- c(
				"stages",
				"overallTestStatistics", 
				"overallPValues"
			)
			if (.dataInput$getNumberOfGroups() == 1) {
				parametersToShow <- c(parametersToShow,
					"overallEvents",
					"overallSampleSizes"
				)

			}
			else if (.dataInput$getNumberOfGroups() == 2) {
				parametersToShow <- c(parametersToShow,
					"overallEvents1", 
					"overallEvents2",
					"overallSampleSizes1",
					"overallSampleSizes2"
				)
			}
			parametersToShow <- c(parametersToShow,	
				"testStatistics", 
				"pValues", 
				"effectSizes"
			)
			if (.isTrialDesignInverseNormal(.design)) {
				parametersToShow <- c(parametersToShow,				
					"combInverseNormal", 
					"weightsInverseNormal"
				)
			}
			else if (.isTrialDesignFisher(.design)) {
				parametersToShow <- c(parametersToShow,			
					"combFisher", 
					"weightsFisher"
				)
			}
			parametersToShow <- c(parametersToShow, 
				"thetaH0",
				"direction",
				"normalApproximation"
			)
			return(parametersToShow)
		}
	)
)

#' 
#' @name StageResultsSurvival
#' 
#' @title
#' Stage Results of Survival Data
#' 
#' @description 
#' Class for stage results survival data.
#' 
#' @details
#' This object can not be created directly; use \code{getStageResults} 
#' with suitable arguments to create the stage results of a dataset of survival data.
#' 
#' @field testStatistics The stage-wise test statistics.
#' @field pValues The stage-wise p-values.
#' @field combInverseNormal The inverse normal test.
#' @field combFisher The Fisher's combination test.
#' @field effectSizes The effect sizes for different designs.
#' @field testActions The action drawn from test result.
#' @field weightsFisher The weights for Fisher's combination test.
#' @field weightsInverseNormal The weights for inverse normal statistic.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResultsSurvival <- setRefClass("StageResultsSurvival",
	contains = "StageResults",
	fields = list(
		combInverseNormal = "numeric", 
		combFisher = "numeric", 
		overallPValues = "numeric",
		effectSizes = "numeric",
		overallLogRanks = "numeric",
		overallEvents = "numeric", 
		overallAllocationRatios = "numeric",
		events = "numeric",
		allocationRatios = "numeric",
		logRanks = "numeric"
	),
	methods = list(
		initialize = function(design, dataInput, ...) {
			callSuper(.design = design, .dataInput = dataInput, ...)
			init(design = design, dataInput = dataInput)
			
			for (param in c(
					"weightsFisher", 
					"weightsInverseNormal", 
					"combFisher", 
					"combInverseNormal")) {
				.setParameterType(param, C_PARAM_NOT_APPLICABLE)
			}
			
			for (param in .getParametersToShow()) {
				if (.getParameterType(param) == C_PARAM_TYPE_UNKNOWN) {
					.setParameterType(param, C_PARAM_GENERATED)
				}
			}
		},
		.getParametersToShow = function() {
			parametersToShow <- c(
				"stages",
				"overallLogRanks",
				"overallPValues", 
				"overallEvents",
				"overallAllocationRatios", 
				"events", 
				"allocationRatios",
				"logRanks", 
				"pValues", 
				"overallPValues",
				"effectSizes"
			)
			if (.isTrialDesignInverseNormal(.design)) {
				parametersToShow <- c(parametersToShow,				
					"combInverseNormal", 
					"weightsInverseNormal"
				)
			}
			else if (.isTrialDesignFisher(.design)) {
				parametersToShow <- c(parametersToShow,			
					"combFisher", 
					"weightsFisher"
				)
			}
			parametersToShow <- c(parametersToShow, 
				"thetaH0",
				"direction"
			)
			return(parametersToShow)
		}
	)
)

#'
#' @name StageResults_names
#' 
#' @title
#' The Names of a Stage Results object
#'
#' @description
#' Function to get the names of a \code{StageResults} object.
#' 
#' @details
#' Returns the names of stage results that can be accessed by the user.
#'
#' @export
#' 
#' @keywords internal
#' 
names.StageResults <- function(x) {
	return(x$.getParametersToShow())
}

#'
#' @name StageResults_as.data.frame
#' 
#' @title
#' Coerce Stage Results to a Data Frame
#'
#' @description
#' Returns the \code{StageResults} as data frame.
#' 
#' @details
#' Coerces the stage results to a data frame.
#' 
#' @export
#' 
#' @keywords internal
#'  
as.data.frame.StageResults <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = FALSE, 
		includeAllParameters = FALSE, type = 1, ...) {
	
	if (type == 1) {
		parametersToShow <- x$.getParametersToShow()
		
		return(x$.getAsDataFrame(parameterNames = parametersToShow, 
				niceColumnNamesEnabled = niceColumnNamesEnabled, includeAllParameters = includeAllParameters,
				tableColumnNames = .getTableColumnNames(design = x$.design)))
	}
	
	kMax <- length(x$stages)
	group1 <- rep(1, kMax)
	group2 <- rep(2, kMax)
	empty <- rep(NA_real_, kMax)
	stageResults <- data.frame(
		Stage = c(x$stages, x$stages),
		Group = c(group1, group2),
		"Overall Mean" = c(x$overallMeans1, x$overallMeans2),
		"Overall StDev" = c(x$overallStDevs1, x$overallStDevs2),
		"Overall test statistics" = c(x$overallTestStatistics, empty),
		"Overall p-value" = c(x$overallPValues, empty),
		"Overall StdDev" = c(x$overallStDevs, empty),
		"Test statistic" = c(x$testStatistics, empty),
		"p-value" = c(x$pValues, empty),
		"Comb Inverse Normal" = c(x$combInverseNormal, empty),
		"Comb Fisher" = c(x$combFisher, empty),
		"Weights Fisher" = c(x$weightsFisher, empty),
		"Weights Inverse Normal" = c(x$weightsInverseNormal, empty),
		row.names = row.names,
		...
	)
	stageResults <- stageResults[with(stageResults, order(Stage, Group)), ]
	return(stageResults)
}

#'
#' @title
#' Stage Results Plotting
#' 
#' @description
#' Plots the conditional power together with the likelihood function. 
#' 
#' @param x The stage results at given stage, obtained from \code{getStageResults} or \code{getAnalysisResults}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param stage The stage number (optional). Default: total number of existing stages in the data input
#'        used to create the stage results.
#' @param nPlanned The sample size planned for the subsequent stages. 
#'        It should be a vector with length equal to the remaining stages and is the 
#'        overall sample size in the two treatment groups if two groups are considered.   
#' @param allocationRatioPlanned The allocation ratio for two treatment groups planned for 
#'        the subsequent stages, the default value is 1. 
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title.
#' @param palette The palette, default is \code{"Set1"}.
#' @param showSource If \code{TRUE}, the parameter names of the object will 
#'        be printed which were used to create the plot; that may be, e.g., 
#'        useful to check the values or to create own plots with \code{\link[graphics]{plot}}.
#' @param legendPosition The position of the legend. 
#' By default (\code{NA_integer_}) the algorithm tries to find a suitable position. 
#' Choose one of the following values to specify the position manually:
#' \itemize{
#'   \item \code{0}: legend position outside plot
#'   \item \code{1}: legend position left top
#'   \item \code{2}: legend position left center
#'   \item \code{3}: legend position left bottom
#'   \item \code{4}: legend position right top
#'   \item \code{5}: legend position right center
#'   \item \code{6}: legend position right bottom
#' }
#' @param type The plot type (default = 1). Note that at the moment only one type 
#'        (the conditional power plot) is available.
#' @param ... Optional \code{ggplot2} arguments. Furthermore the following arguments can be defined:
#' \itemize{
#' \item \code{thetaRange}: A range of assumed effect sizes if testing means or a survival design was specified. 
#' 			Additionally, if testing means was selected, an assumed standard deviation can be specified (default is 1).
#' \item \code{piRange}: A range of assumed rates pi1 to calculate the conditional power. 
#' 		  Additionally, if a two-sample comparison was selected, pi2 can be specified (default is the value from 
#'        \code{getAnalysisResults}). 
#' \item \code{directionUpper}: The direction of one-sided testing. 
#'        Default is \code{directionUpper = TRUE} which means that larger values of the 
#'        test statistics yield smaller p-values.
#' \item \code{thetaH0}: The null hypothesis value, default is 0 for the normal and the binary case, 
#'        it is 1 for the survival case.      
#'        For testing a rate in one sample, a value thetaH0 in (0,1) has to be specified for 
#'        defining the null hypothesis H0: pi = thetaH0.
#' }
#' 
#' @details
#' Generic function to plot all kinds of stage results.
#' The conditional power is calculated only if effect size and sample size is specified. 
#' 
#' @export
#' 
#' @examples 
#' 
#' design <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
#'     informationRates = c(0.2, 0.5, 0.8, 1), 
#'     typeOfDesign = "WT", deltaWT = 0.25)
#' 
#' dataExample <- getDataset(
#'     n = c(20, 30, 30),
#'     means = c(50, 51, 55),
#'     stDevs = c(130, 140, 120)
#' )
#' 
#' stageResults <- getStageResults(design, dataExample, thetaH0 = 20)
#' 
#' if (require(ggplot2)) plot(stageResults, nPlanned = c(30), thetaRange = c(0, 100))
#'
plot.StageResults <- function(x, y, ..., type = 1L,
		nPlanned, stage = x$getNumberOfStages(),
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
		main = NA_character_, xlab = NA_character_, ylab = NA_character_,
		legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_, 
		showSource = FALSE) {
	
	.assertGgplotIsInstalled()
	.assertIsStageResults(x)
	.assertIsValidLegendPosition(legendPosition)
	
	if (x$.isMultiArmed()) {
		plotData <- .getConditionalPowerPlotMeansMultiArmed(stageResults = x, stage = stage, 
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...)
	} else {
		plotData <- .getConditionalPowerPlot(stageResults = x, nPlanned = nPlanned, stage = stage,
			allocationRatioPlanned = allocationRatioPlanned, ...)
	}
	
	yParameterName1 <- "Conditional power"
	yParameterName2 <- "Likelihood"
	
	if (x$.isMultiArmed()) {
		numberOfTreatments <- nrow(x$testStatistics)
		
		treatmentArmsToShow <- as.integer(list(...)[["treatmentArms"]])
		if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 || 
				is.na(treatmentArmsToShow) || !is.numeric(treatmentArmsToShow)) {
			treatmentArmsToShow <- 1L:as.integer(numberOfTreatments)
		}
		data <- data.frame(
			xValues = numeric(0),
			yValues = numeric(0),
			categories = character(0)
		)
		for (treatmentArm in treatmentArmsToShow) {
			legend1 <- ifelse(length(treatmentArmsToShow) == 1, yParameterName1,
				paste0(yParameterName1, " (", treatmentArm, ")"))
			legend2 <- ifelse(length(treatmentArmsToShow) == 1, yParameterName2,
				paste0(yParameterName2, " (", treatmentArm, ")"))
			if (all(is.na(plotData$condPowerValues[treatmentArm, ]))) {
				data <- rbind(data, data.frame(
					xValues = plotData$xValues,
					yValues = plotData$likelihoodValues[treatmentArm, ],
					categories = rep(legend2, length(plotData$xValues))
				)) 
			} else {
				data <- rbind(data, data.frame(
					xValues = c(plotData$xValues, plotData$xValues),
					yValues = c(plotData$condPowerValues[treatmentArm, ], 
						plotData$likelihoodValues[treatmentArm, ]),
					categories = c(rep(legend1, length(plotData$xValues)), 
						rep(legend2, length(plotData$xValues)))
				)) 
			}
		}
	} else {
		data <- data.frame(
			xValues = c(plotData$xValues, plotData$xValues),
			yValues = c(plotData$condPowerValues, plotData$likelihoodValues),
			categories = c(rep(yParameterName1, length(plotData$xValues)), 
				rep(yParameterName2, length(plotData$xValues)))
		)
	}
	
	p <- ggplot2::ggplot(data, ggplot2::aes(x = data$xValues, y = data$yValues, 
		colour = factor(data$categories)))
	
	p <- x$getPlotSettings()$setTheme(p)
	p <- x$getPlotSettings()$hideGridLines(p)
	
	# set main title
	mainTitle <- ifelse(is.na(main), plotData$main, main)
	p <- x$getPlotSettings()$setMainTitle(p, mainTitle, subtitle = plotData$sub)
	
	# set legend
	if (is.na(legendPosition)) {
		legendPosition <- C_POSITION_LEFT_TOP
	}
	p <- x$getPlotSettings()$setLegendPosition(p, legendPosition = legendPosition)
	p <- x$getPlotSettings()$setLegendBorder(p)
	p <- x$getPlotSettings()$setLegendTitle(p, legendTitle)
	p <- x$getPlotSettings()$setLegendLabelSize(p)
	
	# set axes labels
	p <- x$getPlotSettings()$setAxesLabels(p, 
		xAxisLabel = plotData$xlab, yAxisLabel1 = plotData$ylab, 
		xlab = xlab, ylab = ylab)
	
	# plot lines and points
	p <- x$getPlotSettings()$plotValues(p, plotPointsEnabled = FALSE, pointBorder = 1)
	
	p <- x$getPlotSettings()$setAxesAppearance(p)
	p <- x$getPlotSettings()$setColorPalette(p, palette)
	p <- x$getPlotSettings()$enlargeAxisTicks(p)
	
	companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
	if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
		companyAnnotationEnabled <- FALSE
	}
	
	p <- x$getPlotSettings()$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)
	
	# start plot generation
	return(p)
}

