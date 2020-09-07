#:#
#:#  *Stage results classes*
#:# 
#:#  This file is part of the R package rpact: 
#:#  Confirmatory Adaptive Clinical Trial Design and Analysis
#:# 
#:#  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
#:#  Licensed under "GNU Lesser General Public License" version 3
#:#  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
#:# 
#:#  RPACT company website: https://www.rpact.com
#:#  rpact package website: https://www.rpact.org
#:# 
#:#  Contact us for information about our services: info@rpact.com
#:# 
#:#  File version: $Revision: 3581 $
#:#  Last changed: $Date: 2020-09-03 08:58:34 +0200 (Do, 03 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

.getStageResultsClassNames <- function() {
	return(c("StageResultsMeans", 
		"StageResultsRates",
		"StageResultsSurvival",
		"StageResultsMultiArmMeans", 
		"StageResultsMultiArmRates",
		"StageResultsMultiArmSurvival"))
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
		stage = "integer",
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
			
			.setParameterType("stage", C_PARAM_NOT_APPLICABLE)
			
			.setParameterType("pValues", ifelse(
				.isMultiArm(), C_PARAM_NOT_APPLICABLE, C_PARAM_GENERATED))
			.setParameterType("thetaH0", ifelse(
				identical(thetaH0, C_THETA_H0_MEANS_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			.setParameterType("direction", ifelse(
				identical(direction, C_DIRECTION_UPPER), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		show = function(showType = 1, digits = NA_integer_) {
			.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
			'Method for automatically printing stage results'
			.resetCat()
			if (showType == 2) {
				callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
			} else {		
				.cat(.toString(startWithUpperCase = TRUE), ":\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
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
		
		.isMultiArm = function() {
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
			
			if (class(.self) == "StageResultsMultiArmMeans") {
				return(paste(prefix, "multi-armed means"))
			}
			
			if (class(.self) == "StageResultsRates") {
				return(paste(prefix, "rates"))
			}
			
			if (class(.self) == "StageResultsMultiArmRates") {
				return(paste(prefix, "multi-armed rates"))
			}
			
			if (class(.self) == "StageResultsSurvival") {
				return(paste(prefix, "survival data"))
			}
			
			if (class(.self) == "StageResultsMultiArmSurvival") {
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
			if (.isMultiArm()) {
				if (inherits(.self, "StageResultsMultiArmRates")) {
					return(max(ncol(stats::na.omit(testStatistics)), 
						ncol(stats::na.omit(separatePValues))))
				}
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
#' This object cannot be created directly; use \code{getStageResults} 
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
StageResultsMultiArmMeans <- setRefClass("StageResultsMultiArmMeans",
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
		initialize = function(design, dataInput, ..., varianceOption = C_VARIANCE_OPTION_DEFAULT, 
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
				identical(varianceOption, C_VARIANCE_OPTION_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
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
				#"directionUpper",
				"varianceOption", 
				"intersectionTest",
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
#' This object cannot be created directly; use \code{getStageResults} 
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

#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResultsMultiArmRates <- setRefClass("StageResultsMultiArmRates",
		contains = "StageResults",
		fields = list(
			stage = "integer",
			piControl = "matrix",
			piTreatments = "matrix",
			combInverseNormal = "matrix", 
			combFisher = "matrix", 
			overallTestStatistics = "matrix", 
			overallPValues = "matrix",
			testStatistics = "matrix",
			separatePValues = "matrix",
			singleStepAdjustedPValues = "matrix",
			intersectionTest = "character",
			normalApproximation = "logical",
			directionUpper = "logical" 
		),
		methods = list(
				initialize = function(design, dataInput, ..., 
						normalApproximation = FALSE) {
					callSuper(.design = design, .dataInput = dataInput, ...,  
							normalApproximation = normalApproximation)
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
						#"directionUpper",
						"piControl",
						"piTreatments", 
						"intersectionTest",
						"overallTestStatistics", 
						"overallPValues",
						"overallStDevs",
						"testStatistics", 
						"separatePValues", 
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
#' @name StageResultsSurvival
#' 
#' @title
#' Stage Results of Survival Data
#' 
#' @description 
#' Class for stage results survival data.
#' 
#' @details
#' This object cannot be created directly; use \code{getStageResults} 
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


#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_analysis_dataset.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
StageResultsMultiArmSurvival <- setRefClass("StageResultsMultiArmSurvival",
		contains = "StageResults",
		fields = list(
			stage = "integer",
			combInverseNormal = "matrix", 
			combFisher = "matrix", 
			overallTestStatistics = "matrix", 
			overallPValues = "matrix",
			testStatistics = "matrix",
			separatePValues = "matrix",
			effectSizes = "matrix",
			singleStepAdjustedPValues = "matrix",
			intersectionTest = "character",
			directionUpper = "logical" 
		),
		methods = list(
				initialize = function(design, dataInput, ..., 
						normalApproximation = FALSE) {
					callSuper(.design = design, .dataInput = dataInput, ...)
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
					
					.setParameterType("directionUpper", ifelse(
						identical(directionUpper, C_DIRECTION_UPPER_DEFAULT), C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
				},
				.getParametersToShow = function() {
					parametersToShow <- c(
						"stages",
						"thetaH0",
						"direction",
						#"directionUpper", 
						"intersectionTest",
						"overallTestStatistics", 
						"overallPValues",
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
#' @name StageResults_names
#' 
#' @title
#' Names of a Stage Results Object
#'
#' @description
#' Function to get the names of a \code{\link{StageResults}} object.
#' 
#' @param x A \code{\link{StageResults}} object.
#' 
#' @details
#' Returns the names of stage results that can be accessed by the user.
#' 
#' @template return_names
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
#' @param x A \code{\link{StageResults}} object.
#' @inheritParams param_niceColumnNamesEnabled
#' @inheritParams param_includeAllParameters
#' @inheritParams param_three_dots
#' 
#' @details
#' Coerces the stage results to a data frame.
#' 
#' @template return_dataframe
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

.getTreatmentArmsToShow <- function(x, ...) {
	dataInput <- x
	if (!inherits(dataInput, "Dataset")) {
		dataInput <- x[[".dataInput"]]
	}
	if (is.null(dataInput) || !inherits(dataInput, "Dataset")) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to get 'dataInput' from ", class(x))
	}
	
	numberOfTreatments <- dataInput$getNumberOfGroups()
	if (numberOfTreatments > 1) {
		validComparisons <- 1L:as.integer(numberOfTreatments - 1)
	} else {
		validComparisons <- 1L
	}
	
	treatmentArmsToShow <- .getOptionalArgument("treatmentArms", ...)
	if (!is.null(treatmentArmsToShow)) {
		treatmentArmsToShow <- as.integer(na.omit(treatmentArmsToShow))
	}
	if (is.null(treatmentArmsToShow) || length(treatmentArmsToShow) == 0 || 
		all(is.na(treatmentArmsToShow)) || !is.numeric(treatmentArmsToShow)) {
		treatmentArmsToShow <- validComparisons
	} else if (!all(treatmentArmsToShow %in% validComparisons)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'treatmentArms' (", 
			.arrayToString(treatmentArmsToShow), ") must be a vector ",
			"containing one or more values of ", .arrayToString(validComparisons))
	}
	treatmentArmsToShow <- sort(unique(treatmentArmsToShow))
	return(treatmentArmsToShow)
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
#' @inheritParams param_stage
#' @inheritParams param_nPlanned
#' @inheritParams param_allocationRatioPlanned
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title.
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_legendPosition
#' @param type The plot type (default = 1). Note that at the moment only one type 
#'        (the conditional power plot) is available.
#' @param ... Optional \link[=param_three_dots_plot]{plot arguments}. Furthermore the following arguments can be defined:
#' \itemize{
#' \item \code{thetaRange}: A range of assumed effect sizes if testing means or a survival design was specified. 
#' 			Additionally, if testing means was selected, an assumed standard deviation can be specified (default is 1).
#' \item \code{piTreatmentRange}: A range of assumed rates pi1 to calculate the conditional power. 
#' 		  Additionally, if a two-sample comparison was selected, pi2 can be specified (default is the value from 
#'        \code{getAnalysisResults}). 
#' \item \code{directionUpper}: Specifies the direction of the alternative, 
#'        only applicable for one-sided testing; default is \code{TRUE}
#'        which means that larger values of the test statistics yield smaller p-values.
#' \item \code{\link[=param_thetaH0]{thetaH0}}: The null hypothesis value, default is 0 for the normal and the binary case, 
#'        it is 1 for the survival case.      
#'        For testing a rate in one sample, a value thetaH0 in (0,1) has to be specified for 
#'        defining the null hypothesis H0: pi = thetaH0.
#' }
#' 
#' @details
#' Generic function to plot all kinds of stage results.
#' The conditional power is calculated only if effect size and sample size is specified. 
#' 
#' @template return_object_ggplot
#' 
#' @examples 
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
#' @export
#' 
plot.StageResults <- function(x, y, ..., type = 1L,
		nPlanned, allocationRatioPlanned = 1, # C_ALLOCATION_RATIO_DEFAULT
		main = NA_character_, xlab = NA_character_, ylab = NA_character_,
		legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_, 
		showSource = FALSE) {
	
	fCall = match.call(expand.dots = FALSE)
		
	.assertGgplotIsInstalled()
	.assertIsStageResults(x)
	.assertIsValidLegendPosition(legendPosition)
	.assertIsValidAllocationRatioPlanned(allocationRatioPlanned, x$.dataInput$getNumberOfGroups())
	#.assertIsSingleInteger(type, "type", naAllowed = FALSE, validateType = FALSE)
	.stopInCaseOfIllegalStageDefinition2(...)
	
	if (x$.design$kMax == 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "cannot plot stage results of a fixed design")
	}
	
	if (!is.logical(showSource) || isTRUE(showSource)) {
		stageResultsName <- .getOptionalArgument("stageResultsName", ...)
		if (is.null(stageResultsName)) {
			stageResultsName <- deparse(fCall$x)
		}
		cat("Source data of the plot:\n")
		cat("  Use getConditionalPower(..., addPlotData = TRUE) to create the data.\n", sep = "")
		cat("Simple plot command example:\n", sep = "")
		
		cmd <- paste0("condPow <- getConditionalPower(", stageResultsName,
			", nPlanned = ", .arrayToString(nPlanned, vectorLookAndFeelEnabled = TRUE))
		if (allocationRatioPlanned != C_ALLOCATION_RATIO_DEFAULT) {
			cmd <- paste0(cmd, ", allocationRatioPlanned = ", allocationRatioPlanned)
		}
		if (grepl("Means|Survival", class(x))) {
			cmd <- paste0(cmd, ", thetaRange = seq(0, 1, 0.1)")
		}
		else if (grepl("Rates", class(x))) {
			cmd <- paste0(cmd, ", piTreatmentRange <- seq(0, 1, 0.1)")
		}
		cmd <- paste0(cmd, ", addPlotData = TRUE)")
		
		cat("  ", cmd, "\n", sep = "")
		cat("  plotData <- condPow$.plotData # get plot data list\n", sep = "")
		cat("  plotData # show plot data list\n", sep = "")
		cat("  plot(plotData$xValues, plotData$condPowerValues)\n", sep = "")
		cat("  plot(plotData$xValues, plotData$likelihoodValues)\n", sep = "")
	}
	
	plotData <- .getConditionalPowerPlot(stageResults = x, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, ...)

	yParameterName1 <- "Conditional power"
	yParameterName2 <- "Likelihood"
	
	if (x$.isMultiArm()) {
		treatmentArmsToShow <- .getTreatmentArmsToShow(x, ...) 
		data <- data.frame(
			xValues = numeric(0),
			yValues = numeric(0),
			categories = character(0)
		)
		for (treatmentArm in treatmentArmsToShow) {
			legend1 <- ifelse(length(treatmentArmsToShow) == 1, yParameterName1,
				paste0(yParameterName1, " (", treatmentArm, " vs control)"))
			legend2 <- ifelse(length(treatmentArmsToShow) == 1, yParameterName2,
				paste0(yParameterName2, " (", treatmentArm, " vs control)"))
			
			treatmentArmIndices <- which(plotData$treatmentArms == treatmentArm)
			
			if (all(is.na(plotData$condPowerValues[treatmentArmIndices]))) {
				if (!all(is.na(plotData$likelihoodValues[treatmentArmIndices]))) {
					data <- rbind(data, data.frame(
						xValues = plotData$xValues[treatmentArmIndices],
						yValues = plotData$likelihoodValues[treatmentArmIndices],
						categories = rep(legend2, length(plotData$xValues[treatmentArmIndices]))
					)) 
				}
			} else {
				data <- rbind(data, data.frame(
					xValues = c(plotData$xValues[treatmentArmIndices], 
						plotData$xValues[treatmentArmIndices]),
					yValues = c(plotData$condPowerValues[treatmentArmIndices], 
						plotData$likelihoodValues[treatmentArmIndices]),
					categories = c(rep(legend1, length(plotData$xValues[treatmentArmIndices])), 
						rep(legend2, length(plotData$xValues[treatmentArmIndices])))
				)) 
			}
		}
	} else {
		if (all(is.na(plotData$condPowerValues))) {
			legendPosition <- -1
			data <- data.frame(
				xValues = plotData$xValues,
				yValues = plotData$likelihoodValues,
				categories = rep(yParameterName2, length(plotData$xValues))
			)
		} else {
			data <- data.frame(
				xValues = c(plotData$xValues, plotData$xValues),
				yValues = c(plotData$condPowerValues, plotData$likelihoodValues),
				categories = c(rep(yParameterName1, length(plotData$xValues)), 
					rep(yParameterName2, length(plotData$xValues)))
			)
		}
	}
	
	main <- ifelse(is.na(main), C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD, main)
	ylab <- ifelse(is.na(ylab), C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD, ylab)
	
	if (is.na(legendTitle)) {
		legendTitle <- "Parameter"
	}
	
	return(.createPlotObject(x, data = data, plotData = plotData, main = main, xlab = xlab, ylab = ylab,
		legendTitle = legendTitle, palette = palette, legendPosition = legendPosition))
}

.createPlotObject <- function(x, ..., data, plotData,
		main = NA_character_, xlab = NA_character_, ylab = NA_character_,
		legendTitle = NA_character_, palette = "Set1", legendPosition = NA_integer_) {
		
	ciModeEnabled <- !is.null(data[["lower"]]) && !is.null(data[["upper"]])
	
	if (!ciModeEnabled) {
		p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["xValues"]], y = .data[["yValues"]], 
				colour = factor(.data[["categories"]]), 
				linetype = factor(.data[["categories"]])))
	} else {
		p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[["xValues"]], y = .data[["yValues"]],
				colour = factor(.data[["categories"]])))
	}
	
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
	if (!ciModeEnabled) {
		p <- x$getPlotSettings()$plotValues(p, plotPointsEnabled = FALSE, pointBorder = 1)
		n <- length(unique(data$categories)) / 2
		if (n > 1) {
			lineTypeValues <- rep(1:2, n)
			colorTypes <- sort(rep(1:n, 2))
			for (i in c(1, 3)) {
				colorTypes[colorTypes >= i] <- colorTypes[colorTypes >= i] + 1
			}
			p <- p + ggplot2::scale_color_manual(name = legendTitle, values = colorTypes) 
			p <- p + ggplot2::scale_linetype_manual(name = legendTitle, values = lineTypeValues)
		} else {
			colorValues = c(2, 4)
			if (!x$.isMultiArm()) {
				colorValues = c(2, 2) # use only one color
			}
			p <- p + ggplot2::scale_color_manual(name = legendTitle, values = colorValues)
			p <- p + ggplot2::scale_linetype_manual(name = legendTitle, values = c(1, 2))
		}
			
		p <- x$getPlotSettings()$setAxesAppearance(p)
		p <- x$getPlotSettings()$enlargeAxisTicks(p)
	}
	
	# plot confidence intervall
	if (ciModeEnabled) {
		pd <- ggplot2::position_dodge(0.15)
	
		p <- p + ggplot2::geom_errorbar(data = data, 
			ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), 
			width = 0.15, position = pd, size = 0.8)
		p <- p + ggplot2::geom_line(position = pd, linetype = "longdash")
		p <- p + ggplot2::geom_point(position = pd, size = 2.0)
		
		stage <- unique(data$xValues)
		kMax <- list(...)[["kMax"]]
		if (length(stage) == 1 && !is.null(kMax)) {
			stages <- 1:kMax
			p <- p + ggplot2::scale_x_continuous(breaks = stages)
		}
	}
	
	companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
	if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
		companyAnnotationEnabled <- FALSE
	}
	
	p <- x$getPlotSettings()$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)

	# start plot generation
	return(p)
}

