######################################################################################
#                                                                                    #
# -- Simulation result classes --                                                    #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.1.0                                                                #
# Date: 13-05-2019                                                                   #
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
#' @name SimulationResults
#' 
#' @title
#' Class for Simulation Results
#' 
#' @description
#' A class for simulation results.
#' 
#' @details 
#' \code{SimulationResults} is the basic class for 
#' \itemize{
#'   \item \code{\link{SimulationResultsMeans}}, 
#'   \item \code{\link{SimulationResultsRates}}, and 
#'   \item \code{\link{SimulationResultsSurvival}}.
#' }
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResults <- setRefClass("SimulationResults",
	contains = "ParameterSet",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.data = "data.frame",
		.rawData = "data.frame",
		.showStatistics = "logical"
	),
	methods = list(
		
		initialize = function(design, ...) {
			callSuper(.design = design, ...)
			
			.showStatistics <<- TRUE
			.plotSettings <<- PlotSettings()
			.parameterNames <<- .getParameterNames(design, .self)
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		setShowStatistics = function(showStatistics) {
			.assertIsSingleLogical(showStatistics, "showStatistics")
			.showStatistics <<- showStatistics
		},
		
		show = function(showType = 1, showStatistics = TRUE) {
			.show(showType = showType, consoleOutputEnabled = TRUE, showStatistics = showStatistics)
		},
		
		.show = function(showType = 1, consoleOutputEnabled = TRUE, showStatistics = TRUE) {
			'Method for automatically printing simulation result objects'	
			.resetCat()
			if (showType == 3) {
				parameterList <- .getSimpleBoundarySummary(.self)
				for (parameterName in names(parameterList)) {
					.cat(parameterName, ":", parameterList[[parameterName]], "\n",
						consoleOutputEnabled = consoleOutputEnabled)
				}
			}
			else if (showType == 2) {
				.cat("Technical summary of the simulation results object of class ",
					methods::classLabel(class(.self)), ":\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat(.toString(startWithUpperCase = TRUE), ":\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Results",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				
				## statistics of simulated data
				if (showStatistics && .showStatistics) {
					
					.cat("Simulated data:\n", consoleOutputEnabled = consoleOutputEnabled)
					if (inherits(.self, "SimulationResultsMeans")) {
						params <- c(
							"numberOfSubjects",
							"testStatistic")
					}
					else if (inherits(.self, "SimulationResultsRates")) {
						params <- c(
							"numberOfSubjects",
							"testStatistic")
					}
					else if (inherits(.self, "SimulationResultsSurvival")) {
						params <- c(
							"analysisTime",
							"numberOfSubjects",
							"eventsPerStage1",
							"eventsPerStage2",
							"eventsPerStage",
							"testStatistic",
							"logRankStatistic",
							"hazardRatioEstimateLR")
					}
					
					if (!all(is.na(conditionalPowerAchieved)) && 
							any(!is.na(conditionalPowerAchieved)) && 
							any(na.omit(conditionalPowerAchieved) != 0)) {
						params <- c(params, "conditionalPowerAchieved")
					}
					
					stages <- sort(unique(.self$.data$stageNumber))
					
					if (inherits(.self, "SimulationResultsMeans")) {
						levelName <- "alternative"
					} else  {
						levelName <- "pi1"
					}
					
					levels <- unique(.self$.data[[levelName]])
					if (length(levels) > 1 && !any(is.na(levels))) {
						levels <- sort(levels)
					} else {
						levels <- NA_real_
					}
					
					for (parameterName in params) {
						paramCaption <- .parameterNames[[parameterName]]
						if (is.null(paramCaption)) {
							paramCaption <- paste0("%", parameterName, "%")
						}
						
						for (stage in stages) {
							for (levelValue in levels) {
								.catStatisticsLine(stage, parameterName, paramCaption,
									levelValue = levelValue, levelName = levelName, 
									consoleOutputEnabled = consoleOutputEnabled)
							}
						}
					}
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
				
				twoGroupsEnabled <- !inherits(.self, "SimulationResultsMeans")
				if (.design$kMax > 1 || twoGroupsEnabled) {
					.cat("Legend:\n", heading = 2, consoleOutputEnabled = consoleOutputEnabled)
					if (twoGroupsEnabled) {
						.cat("  (i): values of treatment arm i\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					if (.design$kMax > 1) {
						.cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
					}
					.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				}
			}
		},
		
		.catStatisticsLine = function(stage, parameterName, paramCaption,
				levelValue, levelName, consoleOutputEnabled) {
				
			if (stage == 1 && parameterName == "conditionalPowerAchieved") {
				return(invisible())
			}
				
			postfix <- paste0("[", stage, "]")
			if (!is.na(levelValue)) {
				if (inherits(.self, "SimulationResultsMeans")) {
					levelName <- "alternative"
				} else  {
					levelName <- "pi1"
				}
				postfix <- paste0(postfix, ", ", levelName, " = ", round(levelValue, 4))
				paramValue <- .self$.data[[parameterName]][
					.self$.data$stageNumber == stage & .self$.data[[levelName]] == levelValue]
			} else {
				paramValue <- .self$.data[[parameterName]][
					.self$.data$stageNumber == stage]
			}
			
			variableNameFormatted <- formatVariableName(name = paramCaption, 
				n = .getNChar(), prefix = "", postfix = postfix)
			
			if (!is.null(paramValue) && length(paramValue) > 0 && is.numeric(paramValue)) {
				paramValueFormatted <- paste0("median [range]: ", round(stats::median(paramValue), 3), 
					" [", paste(round(base::range(paramValue), 3), collapse = " - "), "]; ", 
					"mean +/-sd: ", round(base::mean(paramValue), 3), " +/-", round(stats::sd(paramValue), 3))
			} else {
				paramValueFormatted <- "median [range]: NA [NA - NA]; mean +/sd: NA +/-NA"
			}
			output <- paste(variableNameFormatted, paramValueFormatted, "\n")
			if (!grepl(": median \\[range\\]: NA \\[NA - NA\\]", output)) {
				.cat(output, consoleOutputEnabled = consoleOutputEnabled)
			}
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			s <- "simulation"
			
			if (inherits(.self, "SimulationResultsMeans")) {
				s <- paste(s, "of means")
			}
			else if (inherits(.self, "SimulationResultsRates")) {
				s <- paste(s, "of rates")
			}
			else if (inherits(.self, "SimulationResultsSurvival")) {
				s <- paste(s, "of survival data")
			} 
			else {
				s <- paste(s, "results")
			}
			
			if (.isTrialDesignGroupSequential(.design)) {
				s <- paste(s, "(group sequential design)")
			}
			else if (.isTrialDesignInverseNormal(.design)) {
				s <- paste(s, "(inverse normal design)")
			}
			else if (.isTrialDesignFisher(.design)) {
				s <- paste(s, "(Fisher design)")
			}
			else {
				s <- paste("unknown", s)
			}
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		}
	)
)

#' 
#' @name SimulationResultsMeans
#' 
#' @title
#' Class for Simulation Results Means
#' 
#' @description
#' A class for simulation results means.
#' 
#' @details 
#' Use \code{\link{getSimulationMeans}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsMeans <- setRefClass("SimulationResultsMeans",
	contains = "SimulationResults",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.data = "data.frame",
		.rawData = "data.frame",
		.showStatistics = "logical",
		
		alternative = "numeric",
		effect = "numeric",
		stDev = "numeric",
		groups = "integer",
		allocationRatioPlanned = "numeric",		
		directionUpper = "logical",
		thetaH0 = "numeric", 
		meanRatio = "logical",
		
		iterations = "matrix",  
		sampleSizes = "matrix", 
		rejectPerStage = "matrix", 
		overallReject = "numeric", 
		futilityPerStage = "matrix", 
		futilityStop = "numeric",
		earlyStop = "numeric",
		expectedNumberOfSubjects = "numeric", 
		plannedSubjects = "numeric", 
		minNumberOfSubjectsPerStage = "numeric",
		maxNumberOfSubjectsPerStage = "numeric", 
		conditionalPower = "numeric", 
		thetaH1 = "numeric", 
		maxNumberOfIterations = "integer", 
		conditionalPowerAchieved = "matrix", 
		
		seed = "numeric"
	),
	methods = list(
		
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
				"effect",
				"iterations", 
				"sampleSizes", 
				"eventsNotAchieved", 
				"expectedNumberOfSubjects", 
				"rejectPerStage", 
				"overallReject", 
				"futilityPerStage", 
				"futilityStop", 
				"earlyStop",
				"analysisTime", 
				"studyDuration")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsRates
#' 
#' @title
#' Class for Simulation Results Rates
#' 
#' @description
#' A class for simulation results rates.
#' 
#' @details 
#' Use \code{\link{getSimulationRates}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsRates <- setRefClass("SimulationResultsRates",
	contains = "SimulationResults",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.data = "data.frame",
		.rawData = "data.frame",
		.showStatistics = "logical",
		
		pi1 = "numeric",
		pi2 = "numeric",
		effect = "numeric",		
		groups = "integer",
		allocationRatioPlanned = "numeric",		
		directionUpper = "logical",
		thetaH0 = "numeric", 
		riskRatio = "logical",
		pi1H1 = "numeric",
		pi2H1 = "numeric",
		
		iterations = "matrix",  
		sampleSizes = "matrix", 
		rejectPerStage = "matrix", 
		overallReject = "numeric", 
		futilityPerStage = "matrix", 
		futilityStop = "numeric", 
		earlyStop = "numeric",		
		expectedNumberOfSubjects = "numeric", 
		plannedSubjects = "numeric", 
		minNumberOfSubjectsPerStage = "numeric",
		maxNumberOfSubjectsPerStage = "numeric", 
		conditionalPower = "numeric", 
		thetaH1 = "numeric", 
		maxNumberOfIterations = "integer", 
		conditionalPowerAchieved = "matrix", 
		
		seed = "numeric"
	),
	methods = list(
		
		initialize = function(design, ...) {
			callSuper(design = design, ...)
			
			for (generatedParam in c(
				"effect",					
				"iterations", 
				"sampleSizes",
				"eventsNotAchieved", 
				"expectedNumberOfSubjects", 
				"rejectPerStage", 
				"overallReject", 
				"futilityPerStage", 
				"futilityStop", 
				"earlyStop",
				"analysisTime", 
				"studyDuration")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
		}
	)
)

#' 
#' @name SimulationResultsSurvival
#' 
#' @title
#' Class for Simulation Results Survival
#' 
#' @description
#' A class for simulation results survival.
#' 
#' @details 
#' Use \code{\link{getSimulationSurvival}} to create an object of this type.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include class_design.R
#' @include f_core_constants.R
#' @include class_time.R
#' @include f_simulation_survival.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SimulationResultsSurvival <- setRefClass("SimulationResultsSurvival",
	contains = "SimulationResults",
	fields = list(
		.piecewiseSurvivalTime = "PiecewiseSurvivalTime",
		.accrualTime = "AccrualTime",
		
		maxNumberOfSubjects = "numeric",
		accrualTime = "numeric", 
		accrualIntensity = "numeric",
		plannedEvents = "numeric", 
		pi1 = "numeric",
		pi2 = "numeric",
		median1 = "numeric", 
		median2 = "numeric", 
		directionUpper = "logical",
		dropoutRate1 = "numeric",
		dropoutRate2 = "numeric",
		dropoutTime = "numeric",
		eventTime = "numeric",
		thetaH0 = "numeric", 
		allocation1 = "numeric",
		allocation2 = "numeric",
		minNumberOfEventsPerStage = "numeric",
		maxNumberOfEventsPerStage = "numeric", 
		conditionalPower = "numeric", 
		thetaH1 = "numeric", 
		maxNumberOfIterations = "integer", 
		kappa = "numeric",
		piecewiseSurvivalTime = "numeric", 
		lambda1 = "numeric",
		lambda2 = "numeric",
		
		hazardRatio = "numeric",
		iterations = "matrix", 
		analysisTime = "matrix",
		studyDuration = "numeric",
		eventsPerStage = "matrix", 
		expectedNumberOfEvents = "numeric", 
		eventsNotAchieved = "matrix",
		numberOfSubjects = "matrix", 
		numberOfSubjects1 = "matrix",
		numberOfSubjects2 = "matrix",
		expectedNumberOfSubjects = "numeric", 
		rejectPerStage = "matrix",
		overallReject = "numeric",		
		futilityPerStage = "matrix",
		futilityStop = "numeric", 
		earlyStop = "numeric",
		conditionalPowerAchieved = "matrix", 
		
		seed = "numeric"
	),
	methods = list(
		
		initialize = function(design, ...) {
			callSuper(design = design, ...)
						
			for (generatedParam in c(
					"hazardRatio", 
					"iterations", 
					"eventsPerStage",
					"expectedNumberOfEvents", 
					"eventsNotAchieved", 
					"numberOfSubjects", 
					"expectedNumberOfSubjects", 
					"rejectPerStage", 
					"overallReject", 
					"futilityPerStage", 
					"futilityStop", 
					"earlyStop",
					"analysisTime", 
					"studyDuration")) {
				.setParameterType(generatedParam, C_PARAM_GENERATED)
			}
			.setParameterType("numberOfSubjects1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("numberOfSubjects2", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
		}
	)
)

.assertIsValidVariedParameterVectorForSimulationResultsPlotting <- function(simulationResults, plotType) {
	if (inherits(simulationResults, "SimulationResultsMeans")) {
		if (is.null(simulationResults$alternative) || 
				is.na(simulationResults$alternative) || 
				length(simulationResults$alternative) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available if 'alternative' with length > 1 is defined")
		}
	}
	else if (inherits(simulationResults, "SimulationResultsRates")) {
		if (is.null(simulationResults$pi1) || 
				is.na(simulationResults$pi1) || 
				length(simulationResults$pi1) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available if 'pi1' with length > 1 is defined")
		}
	}
	else if (inherits(simulationResults, "SimulationResultsSurvival")) {
		if (is.null(simulationResults$hazardRatio) || 
				is.na(simulationResults$hazardRatio) || 
				length(simulationResults$hazardRatio) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available if 'hazardRatio' with length > 1 is defined")
		}
		if (length(simulationResults$hazardRatio) != length(simulationResults$overallReject)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available for delayed piecewise survival")
		}
	}
}

.plotSimulationResults <- function(simulationResults, designMaster, type = 5L, main = NA_character_, 
		xlab = NA_character_, ylab = NA_character_, palette = "Set1",
		theta = seq(-1, 1, 0.02), plotPointsEnabled = NA, 
		legendPosition = NA_integer_, showSource = FALSE, simulationResultsName = NA_character_, ...) {
	
	.assertGgplotIsInstalled()
	.assertIsSimulationResults(simulationResults) 
	.assertIsValidLegendPosition(legendPosition)
	theta <- .assertIsValidThetaRange(thetaRange = theta)
	
	survivalEnabled <- inherits(simulationResults, "SimulationResultsSurvival")
	meansEnabled <- inherits(simulationResults, "SimulationResultsMeans")
	
	if (survivalEnabled) {
		nMax <- simulationResults$expectedNumberOfEvents[1] # use first value for plotting
	} else {
		nMax <- simulationResults$expectedNumberOfSubjects[1] # use first value for plotting
	}
	
	if (type %in% c(1:4)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
			") is not available for simulation results")
	}
	
	if (!survivalEnabled && type %in% c(10:14)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
			") is only available for survival simulation results")
	}
	
	variedParameters <- logical(0)
	
	if (is.na(plotPointsEnabled)) { 
		plotPointsEnabled <- FALSE
	}
	
	showSourceHint <- ""
	
	if (type == 5) { # Power and Stopping Probabilities 
		
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)

		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Overall Power and Early Stopping")
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (survivalEnabled) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		} 
		
		yParameterNames <- c("overallReject", "earlyStop", "futilityStop")

		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_RIGHT_CENTER
		}
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	} 
	
	else if (type == 6) { # Average Sample Size / Average Event Number
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		
		if (is.na(main)) {
			titlePart <- ifelse(survivalEnabled, "Number of Events", "Number of Subjects")
			items <- PlotSubTitleItems(title = paste0("Expected ", titlePart, " and Power / Early Stop"))
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (survivalEnabled) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		} 
		
		if (survivalEnabled) {
			yParameterNames <- "expectedNumberOfEvents"
			expectedNumberOfEvents <- simulationResults[["expectedNumberOfEvents"]]
			if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
				yParameterNames <- "observedEventsH1" 
			}
		} else {
			yParameterNames <- "expectedNumberOfSubjects"
		}
		yParameterNames <- c(yParameterNames, "overallReject", "earlyStop") 
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_LEFT_CENTER
		}

		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 7) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Overall Power")
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		if (survivalEnabled) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		}
		
		yParameterNames <- "overallReject"
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_RIGHT_CENTER
		}
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 8) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Overall Early Stopping")
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		if (survivalEnabled) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		}
		
		yParameterNames <- c("earlyStop", "futilityStop")
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_LEFT_CENTER
		}
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 9) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type) 
		
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = ifelse(survivalEnabled,
					"Expected Number of Events", "Expected Number of Subjects"))
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		if (survivalEnabled) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		}
		
		if (survivalEnabled) {
			yParameterNames <- "expectedNumberOfEvents"
		} else {
			yParameterNames <- "expectedNumberOfSubjects"
		}
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 10) { # Study Duration
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Study Duration")
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		xParameterName <- "hazardRatio"
		yParameterNames <- "studyDuration"
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 11) {
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Expected Number of Subjects")
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		xParameterName <- "hazardRatio"
		yParameterNames <- "expectedNumberOfSubjects" 
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 12) { # Analysis Time
		.assertIsValidVariedParameterVectorForSimulationResultsPlotting(simulationResults, type)
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Analysis Times")
			.addPlotSubTitleItems(simulationResults, designMaster, items, type)
			main <- items$toQuote()
		}
		
		xParameterName <- "hazardRatio"
		yParameterNames <- "analysisTime"
		
		data <- NULL
		for (k in 1:designMaster$kMax) {
			part <- data.frame(
				categories = rep(k, length(simulationResults$hazardRatio)),
				xValues = simulationResults$hazardRatio,
				yValues = simulationResults$analysisTime[k, ]
			)
			if (is.null(data)) {
				data <- part
			} else {
				data <- rbind(data, part)
			}
		}
		
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_LEFT_CENTER
		}
		
		.showPlotSourceInformation(objectName = simulationResultsName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
		
		return(.plotDataFrame(data, mainTitle = main, 
			xlab = NA_character_, ylab = NA_character_, xAxisLabel = "Hazard Ratio",
			yAxisLabel1 = "Analysis Time", yAxisLabel2 = NA_character_, 
			plotPointsEnabled = TRUE, legendTitle = "Stage",
			legendPosition = legendPosition, sided = designMaster$sided))
	}
	
	else if (type == 13 || type == 14) { # Cumulative Distribution Function / Survival function
		return(.plotSurvivalFunction(simulationResults, designMaster = designMaster, type = type, main = main, 
			xlab = xlab, ylab = ylab, palette = palette,
			legendPosition = legendPosition, showSource = showSource))
	} else {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 5, 6,..., 14")	
	}
	
	return(.plotParameterSet(parameterSet = simulationResults, designMaster = designMaster, 
		xParameterName = xParameterName,
		yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
		palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
		legendPosition = legendPosition, variedParameters = variedParameters, 
		qnormAlphaLineEnabled = (type != 2), ratioEnabled = TRUE, ...))
}

#'
#' @title
#' Simulation Results Plotting
#' 
#' @param x The simulation results, obtained from \cr
#'        \code{\link{getSimulationSurvival}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param palette The palette, default is \code{"Set1"}.
#' @param theta A vector of theta values.
#' @param plotPointsEnabled If \code{TRUE}, additional points will be plotted.
#' @param showSource If \code{TRUE}, the parameter names of the object will 
#'        be printed which were used to create the plot; that may be, e.g., 
#'        useful to check the values or to create own plots with \code{\link[graphics]{plot}}.
#' @param legendPosition The position of the legend. 
#' By default (\code{NA_integer_}) the algorithm tries to find a suitable position. 
#' Choose one of the following values to specify the position manually:
#' \itemize{
#'   \item \code{-1}: no legend will be shown
#'   \item \code{NA}: the algorithm tries to find a suitable position
#'   \item \code{0}: legend position outside plot
#'   \item \code{1}: legend position left top
#'   \item \code{2}: legend position left center
#'   \item \code{3}: legend position left bottom
#'   \item \code{4}: legend position right top
#'   \item \code{5}: legend position right center
#'   \item \code{6}: legend position right bottom
#' }
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Boundaries' plot
#'   \item \code{2}: creates a 'Boundaries Effect Scale' plot
#'   \item \code{3}: creates a 'Boundaries p Values Scale' plot
#'   \item \code{4}: creates a 'Type One Error Spending' plot
#'   \item \code{5}: creates a 'Sample Size' or 'Overall Power and Early Stopping' plot
#'   \item \code{6}: creates a 'Number of Events' or 'Sample Size' plot
#'   \item \code{7}: creates an 'Overall Power' plot
#'   \item \code{8}: creates an 'Overall Early Stopping' plot
#'   \item \code{9}: creates an 'Expected Number of Events' or 'Expected Sample Size' plot
#'   \item \code{10}: creates a 'Study Duration' plot
#'   \item \code{11}: creates an 'Expected Number of Subjects' plot
#'   \item \code{12}: creates an 'Analysis Times' plot
#'   \item \code{13}: creates a 'Cumulative Distribution Function' plot
#'   \item \code{14}: creates a 'Survival Function' plot
#' }
#' @param ... Optional \code{ggplot2} arguments.
#' 
#' @description
#' Plots simulation results.
#' 
#' @details
#' Generic function to plot all kinds of simulation results.
#' 
#' @export
#'
plot.SimulationResults = function(x, y, main = NA_character_,
		xlab = NA_character_, ylab = NA_character_, type = 1, palette = "Set1",
		theta = seq(-1, 1, 0.01), plotPointsEnabled = NA, 
		legendPosition = NA_integer_, showSource = FALSE, ...) {
	
	fCall = match.call(expand.dots = FALSE)
	simulationResultsName <- as.character(fCall$x)[1]
	
	.plotSimulationResults(simulationResults = x, designMaster = x$.design, 
		main = main, xlab = xlab, ylab = ylab, type = type,
		palette = palette, theta = theta, plotPointsEnabled = plotPointsEnabled, 
		legendPosition = legendPosition, showSource = showSource,
		simulationResultsName = simulationResultsName, ...)
}

#'
#' @title
#' Get Simulation Data
#' 
#' @description
#' Returns the aggregated simulation data.
#' 
#' @param x An \code{SimulationResults} object created by \code{\link{getSimulationMeans}}, 
#'  \code{\link{getSimulationRates}}, or \code{\link{getSimulationSurvival}}.
#'
#' @details
#' This data are the base for creation of the small statistics in the simulation results output.
#'  
#' @keywords internal
#' 
#' @export
#' 
getData <- function(x) {
	if (!inherits(x, "SimulationResults")) { #  or 'Dataset'
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'x' must be a 'SimulationResults' object; for example, use getSimulationSurvival() to create one")
	}
	
	return(x$.data)
}

.getAggregatedDataByIterationNumber <- function(rawData, iterationNumber) {
	subData <- rawData[rawData$iterationNumber == iterationNumber, ]
	
	eventsPerStage1 <- sum(subData$event[subData$treatmentGroup == 1])
	eventsPerStage2 <- sum(subData$event[subData$treatmentGroup == 2])
	
	return(data.frame(
		iterationNumber = iterationNumber,
		stageNumber = subData$stopStage[1],
		analysisTime = max(subData$observationTime),
		numberOfSubjects = nrow(subData),
		eventsPerStage1 = eventsPerStage1,
		eventsPerStage2 = eventsPerStage2,
		eventsPerStage = eventsPerStage1 + eventsPerStage2
	))
}

.getAggregatedData <- function(rawData) {
	iterationNumbers <- sort(unique(rawData$iterationNumber))
	data <- NULL
	for (iterationNumber in iterationNumbers) {
		row <- .getAggregatedDataByIterationNumber(rawData, iterationNumber)
		if (is.null(data)) {
			data <- row
		} else {
			data <- rbind(data, row)
		}
	}
	return(data)
}

#'
#' @title
#' Get Simulation Raw Data
#' 
#' @description
#' Returns the raw data which was generated randomly for simulation.
#' 
#' @param x An \code{SimulationResults} object created by \code{\link{getSimulationSurvival}}.
#' @param aggregate If \code{TRUE} the raw data will be aggregated similar to
#'        the result of \code{\link{getData}}, default is \code{FALSE}.
#'
#' @details
#' This function works only if \code{\link{getSimulationSurvival}} was called
#' with a \code{maxNumberOfRawDatasetsPerStage > 0} (default is \code{0}).
#'  
#' @keywords internal
#' 
#' @export
#' 
getRawData <- function(x, aggregate = FALSE) {
	if (!inherits(x, "SimulationResultsSurvival")) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'x' must be a 'SimulationResultsSurvival' object; use getSimulationSurvival() to create one")
	}
	
	rawData <- x$.rawData
	if (is.null(rawData) || ncol(rawData) == 0 || nrow(rawData) == 0) {
		stop("Simulation results contain no raw data; ",
			"choose a 'maxNumberOfRawDatasetsPerStage' > 0, e.g., ", 
			"getSimulationSurvival(..., maxNumberOfRawDatasetsPerStage = 1)")
	}
	
	if (!aggregate) {
		return(rawData)
	}
	
	return(.getAggregatedData(rawData))
}
