######################################################################################
#                                                                                    #
# -- Analysis result classes --                                                       #
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
#' @name AnalysisResults
#' 
#' @title
#' Basic Class for Analysis Results
#' 
#' @description
#' A basic class for analysis results.
#' 
#' @details 
#' \code{AnalysisResults} is the basic class for 
#' \itemize{
#'   \item \code{\link{AnalysisResultsFisher}}, 
#'   \item \code{\link{AnalysisResultsGroupSequential}}, and 
#'   \item \code{\link{AnalysisResultsInverseNormal}}.
#' }
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_analysis_stage_results.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'  
AnalysisResults <- setRefClass("AnalysisResults",
	contains = "ParameterSet",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.dataInput = "Dataset",
		.stageResults = "StageResults",
		stages = "integer",
		informationRates = "numeric",
		criticalValues = "numeric",
		futilityBounds = "numeric",
		alphaSpent = "numeric", 
		stageLevels = "numeric",
		effectSizes = "numeric",
		testStatistics = "numeric",
		pValues	 = "numeric",
		testActions = "character",
		thetaH0 = "numeric",
		thetaH1 = "numeric",
		assumedStDev = "numeric",
		conditionalRejectionProbabilities = "numeric",
		nPlanned = "numeric",
		allocationRatioPlanned = "numeric",
		pi1 = "numeric",
		pi2 = "numeric",
		conditionalPower = "numeric",
		repeatedConfidenceIntervalLowerBounds = "numeric",
		repeatedConfidenceIntervalUpperBounds = "numeric",
		repeatedPValues = "numeric",
		finalStage = "integer",
		finalPValues = "numeric",	
		finalConfidenceIntervalLowerBounds = "numeric",
		finalConfidenceIntervalUpperBounds = "numeric",
		medianUnbiasedEstimates = "numeric",
		normalApproximation = "logical",
		equalVariances = "logical",
		directionUpper = "logical"
	),
	methods = list(
	
		initialize = function(design, dataInput, ...) {
			callSuper(.design = design, .dataInput = dataInput, ...)
			
			.plotSettings <<- PlotSettings()
			.parameterNames <<- .getParameterNames(design)
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
			
			finalStage <<- NA_integer_
			stages <<- c(1:design$kMax)
			if (design$kMax == C_KMAX_DEFAULT) {
				.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
			} else {
				.setParameterType("stages", C_PARAM_USER_DEFINED)
			}
			
			informationRates <<- design$informationRates
			criticalValues <<- design$criticalValues
			if (.isTrialDesignFisher(design)) {
				futilityBounds <<- design$alpha0Vec
			} else {
				futilityBounds <<- design$futilityBounds
			}
			alphaSpent <<- design$alphaSpent
			stageLevels <<- design$stageLevels
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		show = function(showType = 1) {
			.show(showType = showType, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, consoleOutputEnabled = TRUE) {
			'Method for automatically printing analysis result objects'	
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the analysis results object of class ",
					methods::classLabel(class(.self)), ":\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {				
				.showParametersOfOneGroup(parameters = .getParametersToShow(), 
					title = .toString(startWithUpperCase = TRUE), orderByParameterName = FALSE,
					consoleOutputEnabled = consoleOutputEnabled)
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
			}
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			prefix <- paste(ifelse(startWithUpperCase, "Analysis results", "analysis results"))
			if (class(.self) == "AnalysisResultsGroupSequential") {
				return(paste(prefix, "(group sequential design)"))
			}
			
			if (class(.self) == "AnalysisResultsInverseNormal") {
				return(paste(prefix, "(inverse normal design)"))
			}
			
			if (class(.self) == "AnalysisResultsFisher") {
				return(paste(prefix, "(Fisher design)"))
			}
			
			return("unknown analysis results")
		},
		
		.getParametersToShow = function() {
	
			parametersToShow <- c(
				"stages",
				"informationRates",
				"criticalValues",
				"futilityBounds",
				"alphaSpent",
				"stageLevels",
				"effectSizes",
				"testStatistics",
				"pValues"
			)
			
			if (.isTrialDesignGroupSequential(.design)) {
				parametersToShow <- c(parametersToShow, 
					"overallTestStatistics",
					"overallPValues")	
			} else {
				parametersToShow <- c(parametersToShow, 
					"combinationTestStatistics")
			}

			parametersToShow <- c(parametersToShow, 
				"testActions",
				"thetaH0",
				"conditionalRejectionProbabilities",
				"nPlanned",
				"allocationRatioPlanned"
			)
			
			if (.dataInput$isDatasetRates()) {
				if (.dataInput$getNumberOfGroups() == 1) {
					parametersToShow <- c(parametersToShow, "pi1")
				} else {
					parametersToShow <- c(parametersToShow, "pi1", "pi2")	
				}
			} else {
				parametersToShow <- c(parametersToShow, "thetaH1")
			}
			if (.dataInput$isDatasetMeans()) {
				parametersToShow <- c(parametersToShow, "assumedStDev")	
			}
			
			if (.isTrialDesignFisher(.design) && length(conditionalPowerSimulated) > 0 &&
					(length(conditionalPowerSimulated) != 1 || conditionalPowerSimulated != -1)) {
				parametersToShow <- c(parametersToShow, "conditionalPowerSimulated")
			} else {
				parametersToShow <- c(parametersToShow, "conditionalPower")
			}
			
			parametersToShow <- c(parametersToShow,
				"repeatedConfidenceIntervalLowerBounds",
				"repeatedConfidenceIntervalUpperBounds",
				"repeatedPValues",
				"finalStage",
				"finalPValues",	
				"finalConfidenceIntervalLowerBounds",
				"finalConfidenceIntervalUpperBounds",
				"medianUnbiasedEstimates"
			)
			
			return(parametersToShow)
		},
		
		getNumberOfStages = function() {
			return(max(length(stats::na.omit(effectSizes)), 
					length(stats::na.omit(testStatistics)),
					length(stats::na.omit(pValues))))
		},
		
		getDataInput = function() {
			return(.dataInput)
		}
	)
)

#'
#' @name AnalysisResults_as.data.frame
#' 
#' @title
#' Coerce AnalysisResults to a Data Frame
#'
#' @description
#' Returns the \code{\link{AnalysisResults}} object as data frame.
#' 
#' @details
#' Coerces the analysis results to a data frame.
#' 
#' @export
#' 
#' @keywords internal
#'  
as.data.frame.AnalysisResults <- function(x, row.names = NULL, optional = FALSE, ...) {
	parametersToShow <- x$.getParametersToShow()
	parametersToShow <- parametersToShow[!(parametersToShow %in% c(
					"finalStage", "allocationRatioPlanned", "thetaH0", "thetaH1", "pi1", "pi2"
	))]
	return(x$.getAsDataFrame(parameterNames = parametersToShow, 
			tableColumnNames = .getTableColumnNames(design = x$.design)))
}

#'
#' @name AnalysisResults_names
#' 
#' @title
#' The Names of a Analysis Results object
#'
#' @description
#' Function to get the names of a \code{\link{AnalysisResults}} object.
#' 
#' @details
#' Returns the names of a analysis results that can be accessed by the user.
#'
#' @export
#' 
#' @keywords internal
#' 
names.AnalysisResults <- function(x) {
	return(x$.getVisibleFieldNames())
}

#' 
#' @name AnalysisResultsGroupSequential
#' 
#' @title
#' Analysis Results Group Sequential
#' 
#' @description
#' Class for analysis results results based on a group sequential design.
#' 
#' @details 
#' This object can not be created directly; use \code{\link{getAnalysisResults}} 
#' with suitable arguments to create the analysis results of a group sequential design.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
AnalysisResultsGroupSequential <- setRefClass("AnalysisResultsGroupSequential",
	contains = "AnalysisResults",
	fields = list(
		overallTestStatistics = "numeric",
		overallPValues = "numeric"
	)
)

#' 
#' @name AnalysisResultsInverseNormal
#' 
#' @title
#' Analysis Results Inverse Normal
#' 
#' @description
#' Class for analysis results results based on an inverse normal design.
#' 
#' @details 
#' This object can not be created directly; use \code{\link{getAnalysisResults}} 
#' with suitable arguments to create the analysis results of a inverse normal design.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
AnalysisResultsInverseNormal <- setRefClass("AnalysisResultsInverseNormal",
	contains = "AnalysisResults",
	fields = list(
		combinationTestStatistics = "numeric"
	)
)

#' 
#' @name AnalysisResultsFisher
#' 
#' @title
#' Analysis Results Fisher
#' 
#' @description
#' Class for analysis results based on a Fisher design.
#' 
#' @details 
#' This object can not be created directly; use \code{\link{getAnalysisResults}} 
#' with suitable arguments to create the analysis results of a Fisher design.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_analysis_dataset.R
#' @include class_design.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
AnalysisResultsFisher <- setRefClass("AnalysisResultsFisher",
	contains = "AnalysisResults",
	fields = list(
		conditionalPowerSimulated = "numeric",
		combinationTestStatistics = "numeric"
	),
	methods = list(
		initialize = function(design, dataInput, ...) {
			callSuper(design = design, dataInput = dataInput, ...)
			conditionalPowerSimulated <<- -1
		}
	)
)

.getAnalysisResultsPlotArguments <- function(x, nPlanned = NA_real_,
		stage = x$getNumberOfStages(), allocationRatioPlanned = NA_real_) {
		
	if (all(is.na(nPlanned))) {
		nPlanned <- stats::na.omit(x$nPlanned)
		stage <- x$.design$kMax - length(nPlanned)
	}
	else if (is.na(stage)) {
		stage <- x$getNumberOfStages()
	}
	
	if (is.na(allocationRatioPlanned)) {
		allocationRatioPlanned <- x$allocationRatioPlanned
	}
	
	return(list(
		stageResults = x$.stageResults,
		nPlanned = nPlanned,
		stage = stage,
		allocationRatioPlanned = allocationRatioPlanned
	))
}

#'
#' @title
#' Analysis Results Plotting
#' 
#' @description
#' Plots the conditional power together with the likelihood function. 
#' 
#' @param x The analysis results at given stage, obtained from \code{\link{getAnalysisResults}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param nPlanned The sample size planned for the subsequent stages. 
#'        It should be a vector with length equal to the remaining stages and is the 
#'        overall sample size in the two treatment groups if two groups are considered.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input
#'        used to create the analysis results.
#' @param allocationRatioPlanned The allocation ratio n1/n2 for two treatment groups planned for 
#'        the subsequent stages, the default value is 1.
#' @param main The main title, default is \code{"Dataset"}.
#' @param xlab The x-axis label, default is \code{"Stage"}.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title, default is \code{""}.
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
#' @param type The plot type (default = 1). Note that at the moment only one type (the conditional power plot) is available.
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
#' The conditional power is calculated only if effect size and sample size is specified. 
#' 
#' @export
#' 
#' @examples 
#' 
#' design <- getDesignGroupSequential(kMax = 2)
#' 
#' dataExample <- getDataset(
#'     n = c(20, 30),
#' 	   means = c(50, 51),
#' 	   stDevs = c(130, 140)
#' )
#' 
#' result <- getAnalysisResults(design = design, 
#' 	   dataInput = dataExample, thetaH0 = 20, 
#' 	   nPlanned = c(30), thetaH1 = 1.5, stage = 1) 
#' 
#' if (require(ggplot2)) plot(result, thetaRange = c(0, 100))
#'
plot.AnalysisResults <- function(x, y, ..., type = 1L,
		nPlanned = NA_real_, stage = x$getNumberOfStages(),
		allocationRatioPlanned = NA_real_,
		main = NA_character_, xlab = NA_character_, ylab = NA_character_,
		legendTitle = "", palette = "Set1", legendPosition = NA_integer_, 
		showSource = FALSE) {
		
	if (showSource) {
		warning("'showSource' = TRUE is not implemented yet for class ", class(x))
	}
	
	.assertIsValidLegendPosition(legendPosition = legendPosition)
	plotArgs <- .getAnalysisResultsPlotArguments(x = x, nPlanned = nPlanned, 
		stage = stage, allocationRatioPlanned = allocationRatioPlanned)	
	nPlanned <- plotArgs$nPlanned
	stage <- plotArgs$stage
	allocationRatioPlanned <- plotArgs$allocationRatioPlanned
	
	if (x$getDataInput()$isDatasetMeans()) {
		assumedStDev <- .getOptionalArgument("assumedStDev", ...)
		if (is.null(assumedStDev)) {
			assumedStDev <- x$assumedStDev
			return(plot.StageResults(x = x$.stageResults, y = y, 
				nPlanned = nPlanned, stage = stage, main = main, xlab = xlab, ylab = ylab,
				legendTitle = legendTitle, palette = palette, legendPosition = legendPosition, 
				type = type, assumedStDev = assumedStDev, 
				allocationRatioPlanned = allocationRatioPlanned, ...))
		}
	}
	else if (x$getDataInput()$isDatasetRates()) {
		pi2 <- .getOptionalArgument("pi2", ...)
		if (is.null(pi2)) {
			pi2 <- x$pi2
			return(plot.StageResults(x = x$.stageResults, y = y, 
				nPlanned = nPlanned, stage = stage, main = main, xlab = xlab, ylab = ylab,
				legendTitle = legendTitle, palette = palette, legendPosition = legendPosition, 
				type = type, pi2 = pi2, allocationRatioPlanned = allocationRatioPlanned, ...))
		}
	}

	plot.StageResults(x = x$.stageResults, y = y, 
		nPlanned = nPlanned, stage = stage, allocationRatioPlanned = allocationRatioPlanned, 
		main = main, xlab = xlab, ylab = ylab,
		legendTitle = legendTitle, palette = palette, legendPosition = legendPosition, 
		type = type, ...)
}
