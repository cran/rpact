######################################################################################
#                                                                                    #
# -- Trial design plan classes --                                                    #
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
#' @include f_design_utilities.R
NULL

C_VARIABLE_DESIGN_PLAN_PARAMETERS <- c("lambda1", "pi1", "median1", "alternative", "hazardRatio")

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS <- list(
	normalApproximation = FALSE, 
	meanRatio = FALSE, 
	thetaH0 = 0, 
	alternative = seq(0.2, 1, 0.2), 
	stDev = 1, 
	groups = 2L, 
	allocationRatioPlanned = 1
)

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES <- list(
	normalApproximation = TRUE, 
	riskRatio = FALSE, 
	thetaH0 = 0, 
	pi1 = seq(0.4, 0.6, 0.1), 
	pi2 = 0.2, 
	groups = 2L, 
	allocationRatioPlanned = 1
)

C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL <- list(
	typeOfComputation = "Schoenfeld", 
	thetaH0 = 1, 
	pi2 = 0.2, 
	pi1 = seq(0.4, 0.6, 0.1), 
	allocationRatioPlanned = 1, 
	accountForObservationTimes = NA, 
	eventTime = 12, 
	accrualTime = C_ACCRUAL_TIME_DEFAULT, 
	accrualIntensity = C_ACCRUAL_INTENSITY_DEFAULT, 
	kappa = 1, 
	piecewiseSurvivalTime = NA_real_, 
	lambda2 = NA_real_,	
	lambda1 = NA_real_,
	followUpTime = C_FOLLOW_UP_TIME_DEFAULT, 
	maxNumberOfSubjects = 0, 
	dropoutRate1 = 0, 
	dropoutRate2 = 0, 
	dropoutTime = 12
)

#' 
#' @name TrialDesignPlan
#' 
#' @title
#' Basic Trial Design Plan
#' 
#' @description 
#' Basic class for trial design plans.
#' 
#' @details
#' \code{TrialDesignPlan} is the basic class for 
#' \itemize{
#'   \item \code{TrialDesignPlanMeans}, 
#'   \item \code{TrialDesignPlanRates}, and 
#'   \item \code{TrialDesignPlanSurvival}.
#' }
#' 
#' @include f_core_constants.R
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_plot.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignPlan <- setRefClass("TrialDesignPlan",
	contains = "ParameterSet",
	fields = list(
		.plotSettings = "PlotSettings",
		.design = "TrialDesign",
		.objectType = "character" # "sampleSize" or "power"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(.design = design, ...)
			
			.plotSettings <<- PlotSettings()
			.parameterNames <<- .getParameterNames(design, .self)
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
						
			if (.isTrialDesignPlanMeans(.self)) {
				defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS
			}
			else if (.isTrialDesignPlanRates(.self)) {
				defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES
			}
			else if (.isTrialDesignPlanSurvival(.self)) {
				defaultValueList <- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL
			}
			for (parameterName in .getVisibleFieldNames()) {
				defaultValue <- defaultValueList[[parameterName]]
				existingValue <- .self[[parameterName]]
				if (all(is.na(existingValue))) {
					.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
				}
				else if (!is.null(defaultValue) && length(defaultValue) == length(existingValue) &&
						!any(is.na(defaultValue)) && !any(is.na(existingValue)) && 
						sum(defaultValue == existingValue) == length(defaultValue)) {
					.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
				} else {
					.setParameterType(parameterName, C_PARAM_USER_DEFINED)
				}
			}
			.setParameterType("optimumAllocationRatio", C_PARAM_NOT_APPLICABLE)
		},
		
		.setSampleSizeObject = function(objectType) {
			if (length(objectType) == 0 || !(objectType %in% c("sampleSize", "power"))) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' (", objectType, 
					") must be specified as 'sampleSize' or 'power'")
			}
			.objectType <<- objectType
		},
		
		.isSampleSizeObject = function() {
			if (length(.objectType) == 0 || !(.objectType %in% c("sampleSize", "power"))) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' must be specified as 'sampleSize' or 'power'")
			}
			return(.objectType == "sampleSize")
		},
		
		.isPowerObject = function() {
			if (length(.objectType) == 0 || !(.objectType %in% c("sampleSize", "power"))) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.objectType' must be specified as 'sampleSize' or 'power'")
			}
			return(.objectType == "power")
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		show = function(showType = 1) {
			.show(showType = showType, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, consoleOutputEnabled = TRUE) {
			'Method for automatically printing trial plan objects'
			.resetCat()
			if (showType == 3) {
				parameterList <- .getSimpleBoundarySummary(.self)
				for (parameterName in names(parameterList)) {
					.cat(parameterName, ":", parameterList[[parameterName]], "\n",
						consoleOutputEnabled = consoleOutputEnabled)
				}
			}
			else if (showType == 2) {
				.cat("Technical summary of the design plan object of class ",
					methods::classLabel(class(.self)), ":\n\n", sep = "", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.cat("Design plan parameters and output for ", .toString(), ":\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
				
				designParametersToShow <- c(".design$alpha")
				if (.objectType == "sampleSize" || (inherits(.self, "TrialDesignPlanSurvival") &&
						.isBetaSpendingDesignType(.design$typeBetaSpending))) {
					designParametersToShow <- c(designParametersToShow, ".design$beta")
				}
				if (.objectType == "sampleSize"  && !is.null(.design$sided) && 
					!is.na(.design$sided) && .design$sided == 2) {
					designParametersToShow <- c(designParametersToShow, ".design$twoSidedPower")
				}
				designParametersToShow <- c(designParametersToShow, ".design$sided")
				.showParametersOfOneGroup(designParametersToShow, "Design parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Sample size and output",
					orderByParameterName = FALSE, consoleOutputEnabled = consoleOutputEnabled)
				
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				
				if (inherits(.self, "TrialDesignPlanSurvival") || groups == 2 || .design$kMax > 1) {
					.cat("Legend:\n", heading = 2,
						consoleOutputEnabled = consoleOutputEnabled)
					if (inherits(.self, "TrialDesignPlanSurvival") || groups == 2) {
						.cat("  (i): values of treatment arm i\n", 
							consoleOutputEnabled = consoleOutputEnabled)
					}
					if (.design$kMax > 1) {
						.cat("  [k]: values at stage k\n", consoleOutputEnabled = consoleOutputEnabled)
					}
				}

				.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
			}
		},
		
		getAlpha = function() {
			return(.design$alpha)
		},
		
		getBeta = function() {
			if (.isTrialDesignInverseNormalOrGroupSequential(.design)) {
				return(.design$beta)
			}
			return(NA_real_)
		},
		
		getSided = function() {
			return(.design$sided)
		},
		
		getTwoSidedPower = function() {
			if (.isTrialDesignInverseNormalOrGroupSequential(.design)) {
				return(.design$twoSidedPower)
			}
			return(NA)
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			if (.isTrialDesignPlanMeans(.self)) {
				s <- "means"
			}
			else if (.isTrialDesignPlanRates(.self)) {
				s <- "rates"
			}
			else if (.isTrialDesignPlanSurvival(.self)) {
				s <- "survival data"
			}
			else  {
				s <- paste0("unknown data class '", class(.self), "'")
			}
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		}
	)
)

#'
#' @name TrialDesignPlan_as.data.frame
#' 
#' @title
#' Coerce Trial Design Plan to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesignPlan} as data frame.
#' 
#' @details
#' Coerces the design plan to a data frame.
#' 
#' @export
#' 
#' @keywords internal
#'  
as.data.frame.TrialDesignPlan <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = TRUE, includeAllParameters = FALSE, ...) {	
	return(x$.getAsDataFrame(parameterNames = NULL, 
			niceColumnNamesEnabled = niceColumnNamesEnabled, includeAllParameters = includeAllParameters))
}

#' 
#' @name TrialDesignPlanMeans
#' 
#' @title
#' Trial Design Plan Means
#' 
#' @description
#' Trial design plan for means.
#' 
#' @details
#' This object can not be created directly; use \code{\link{getSampleSizeMeans}} 
#' with suitable arguments to create a design plan for a dataset of means. 
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignPlanMeans <- setRefClass("TrialDesignPlanMeans",	
	contains = "TrialDesignPlan",
	
	fields = list(
		normalApproximation = "logical", 
		meanRatio = "logical", 
		thetaH0 = "numeric", 
		alternative = "numeric", 
		stDev = "numeric", 
		groups = "numeric", 
		allocationRatioPlanned = "numeric",
		optimumAllocationRatio = "logical",
		directionUpper = "logical",
		
		nFixed = "numeric",
		nFixed1 = "numeric", 
		nFixed2 = "numeric", 
		
		informationRates = "matrix",		
		maxNumberOfSubjects = "numeric",					
		maxNumberOfSubjects1 = "numeric",
		maxNumberOfSubjects2 = "numeric",
		numberOfSubjects = "matrix",
		numberOfSubjects1 = "matrix",
		numberOfSubjects2 = "matrix",
		expectedNumberOfSubjectsH0 = "numeric",
		expectedNumberOfSubjectsH01 = "numeric",
		expectedNumberOfSubjectsH1 = "numeric",
		
		effect = "numeric",
		expectedNumberOfSubjects = "numeric", 
		rejectPerStage = "matrix",
		overallReject = "numeric",		
		futilityPerStage = "matrix",
		futilityStop = "numeric",
		earlyStop = "numeric",
		
		criticalValuesEffectScale = "matrix",
		criticalValuesEffectScaleLower = "matrix",
		criticalValuesEffectScaleUpper = "matrix",
		criticalValuesPValueScale = "matrix",
		futilityBoundsEffectScale = "matrix",
		futilityBoundsPValueScale = "matrix"
	),
	
	methods = list(
		initialize = function(...,
				normalApproximation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["normalApproximation"]], 
				meanRatio = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["meanRatio"]], 
				thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["thetaH0"]], 
				alternative = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["alternative"]], 
				stDev = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["stDev"]], 
				groups = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["groups"]], 
				allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_MEANS[["allocationRatioPlanned"]]) {
			callSuper(..., 
				normalApproximation = normalApproximation, 
				meanRatio = meanRatio, 
				thetaH0 = thetaH0, 
				alternative = alternative,
				stDev = stDev, 
				groups = groups, 
				allocationRatioPlanned = allocationRatioPlanned
				)
			
			visibleFieldNames <- .getVisibleFieldNames()
			startIndex <- which(visibleFieldNames == "directionUpper")
			for (i in startIndex:length(visibleFieldNames)) {
				.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
			}
			
			if (groups == 1) {
				.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
				.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
			}
			
			.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)
			
			.setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
			.setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
			.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
		},
		
		clone = function(alternative = NA_real_) {
			alternativeTemp <- alternative
			if (any(is.na(alternative))) {
				alternativeTemp <- .self$alternative	
			}
			if (.objectType == "sampleSize") {
				return(getSampleSizeMeans(design = .self$.design, 
					normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"), 
					meanRatio = .self$meanRatio, #.getParameterValueIfUserDefinedOrDefault("meanRatio"), 
					thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"), 
					alternative = alternativeTemp, 
					stDev = .self$.getParameterValueIfUserDefinedOrDefault("stDev"), 
					groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"), 
					allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")))
			} else {
				return(getPowerMeans(design = .self$.design, 
					normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"), 
					meanRatio = .self$meanRatio, #.getParameterValueIfUserDefinedOrDefault("meanRatio"), 
					thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"), 
					alternative = alternativeTemp, 
					stDev = .self$.getParameterValueIfUserDefinedOrDefault("stDev"), 
					directionUpper = .self$.getParameterValueIfUserDefinedOrDefault("directionUpper"), 
					maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"), 
					groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"), 
					allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")))
			}
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			callSuper(showType)
		}
	)
)

#' 
#' @name TrialDesignPlanRates
#' 
#' @title
#' Trial Design Plan Rates
#' 
#' @description
#' Trial design plan for rates.
#' 
#' @details
#' This object can not be created directly; use \code{\link{getSampleSizeRates}} 
#' with suitable arguments to create a design plan for a dataset of rates. 
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignPlanRates <- setRefClass("TrialDesignPlanRates",	
	contains = "TrialDesignPlan",
	
	fields = list(
		normalApproximation = "logical", 
		riskRatio = "logical", 
		thetaH0 = "numeric", 
		pi1 = "numeric", 
		pi2 = "numeric", 
		groups = "numeric", 
		allocationRatioPlanned = "numeric",
		optimumAllocationRatio = "logical",
		directionUpper = "logical",
		
		nFixed = "numeric",
		nFixed1 = "numeric", 
		nFixed2 = "numeric", 
		
		informationRates = "matrix",		
		maxNumberOfSubjects = "numeric",					
		maxNumberOfSubjects1 = "numeric",
		maxNumberOfSubjects2 = "numeric",
		numberOfSubjects = "matrix",
		numberOfSubjects1 = "matrix",
		numberOfSubjects2 = "matrix",
		expectedNumberOfSubjectsH0 = "numeric",
		expectedNumberOfSubjectsH01 = "numeric",
		expectedNumberOfSubjectsH1 = "numeric",
		
		effect = "numeric",
		expectedNumberOfSubjects = "numeric",
		rejectPerStage = "matrix",
		overallReject = "numeric",
		futilityPerStage = "matrix",		
		futilityStop = "numeric",
		earlyStop = "numeric",
		
		criticalValuesEffectScale = "matrix",
		criticalValuesEffectScaleLower = "matrix",
		criticalValuesEffectScaleUpper = "matrix",
		criticalValuesPValueScale = "matrix",
		futilityBoundsEffectScale = "matrix",
		futilityBoundsPValueScale = "matrix"
	),
	
	methods = list(
		initialize = function(...,
				normalApproximation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["normalApproximation"]], 
				riskRatio = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["riskRatio"]], 
				thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["thetaH0"]], 
				pi1 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["pi1"]], 
				pi2 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["pi2"]], 
				groups = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["groups"]], 
				allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_RATES[["allocationRatioPlanned"]]) {
			callSuper(..., 
				normalApproximation = normalApproximation, 
				riskRatio = riskRatio, 
				thetaH0 = thetaH0, 
				pi1 = pi1,
				pi2 = pi2, 
				groups = groups, 
				allocationRatioPlanned = allocationRatioPlanned)
			
			visibleFieldNames <- .getVisibleFieldNames()
			startIndex <- which(visibleFieldNames == "directionUpper")
			for (i in startIndex:length(visibleFieldNames)) {
				.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
			}
			
			if (groups == 1) {
				.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
				.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
			}
			
			.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)
			
			.setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
			.setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
			.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
		},
		
		clone = function(pi1 = NA_real_) {
			pi1Temp <- pi1
			if (any(is.na(pi1))) {
				pi1Temp <- .self$pi1	
			}
			if (.objectType == "sampleSize") {
				return(getSampleSizeRates(design = .self$.design, 
					normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"), 
					riskRatio = .self$riskRatio, #.getParameterValueIfUserDefinedOrDefault("riskRatio"), 
					thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"), 
					pi1 = pi1Temp, 
					pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"), 
					groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"), 
					allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")))
			} else {
				return(getPowerRates(design = .self$.design, 
					normalApproximation = .self$.getParameterValueIfUserDefinedOrDefault("normalApproximation"), 
					riskRatio = .self$riskRatio, #.getParameterValueIfUserDefinedOrDefault("riskRatio"), 
					thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"), 
					pi1 = pi1Temp, 
					pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"), 
					directionUpper = .self$.getParameterValueIfUserDefinedOrDefault("directionUpper"), 
					maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"), 
					groups = .self$.getParameterValueIfUserDefinedOrDefault("groups"), 
					allocationRatioPlanned = .self$.getParameterValueIfUserDefinedOrDefault("allocationRatioPlanned")))
			}
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			callSuper(showType)
		}
	)
)

#' 
#' @name TrialDesignPlanSurvival
#' 
#' @title
#' Trial Design Plan Survival
#' 
#' @description
#' Trial design plan for survival data.
#' 
#' @details
#' This object can not be created directly; use \code{\link{getSampleSizeSurvival}} 
#' with suitable arguments to create a design plan for a dataset of survival data. 
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include class_design_set.R
#' @include class_time.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignPlanSurvival <- setRefClass("TrialDesignPlanSurvival",	
	contains = "TrialDesignPlan",
	fields = list(
		.piecewiseSurvivalTime = "PiecewiseSurvivalTime",
		.accrualTime = "AccrualTime",
		typeOfComputation = "character", 
		thetaH0 = "numeric", 
		directionUpper = "logical",
		pi1 = "numeric", 
		pi2 = "numeric", 
		median1 = "numeric", 
		median2 = "numeric", 
		lambda1 = "numeric",
		lambda2 = "numeric",
		hazardRatio = "numeric", 
		maxNumberOfSubjects = "numeric",
		maxNumberOfSubjects1 = "numeric",
		maxNumberOfSubjects2 = "numeric",
		maxNumberOfEvents = "numeric",
		allocationRatioPlanned = "numeric", 
		optimumAllocationRatio = "logical",
		accountForObservationTimes = "logical",
		eventTime = "numeric",
		accrualTime = "numeric",
		totalAccrualTime = "numeric",
		accrualIntensity = "numeric", 
		accrualIntensityRelative = "numeric",
		kappa = "numeric", 
		piecewiseSurvivalTime = "numeric",
		followUpTime = "numeric",
		dropoutRate1 = "numeric",
		dropoutRate2 = "numeric",
		dropoutTime = "numeric",
		
		omega = "numeric",
		calculateFollowUpTime = "logical", 
		eventsFixed = "numeric",
		nFixed = "numeric",
		nFixed1 = "numeric", 
		nFixed2 = "numeric",
		
		informationRates = "matrix",
		analysisTime = "matrix", 
		studyDurationH1 = "numeric", 
		studyDuration = "numeric",
		maxStudyDuration = "numeric",
		eventsPerStage = "matrix", 		
		expectedEventsH0 = "numeric",
		expectedEventsH01 = "numeric",
		expectedEventsH1 = "numeric",	
		expectedNumberOfEvents = "numeric",
		numberOfSubjects = "matrix",
		numberOfSubjects1 = "matrix",
		numberOfSubjects2 = "matrix",		
		expectedNumberOfSubjectsH1 = "numeric",
		expectedNumberOfSubjects = "numeric", 
		
		rejectPerStage = "matrix",
		overallReject = "numeric",
		futilityPerStage = "matrix",
		futilityStop = "numeric",		
		earlyStop = "numeric",
		
		criticalValuesEffectScale = "matrix",
		criticalValuesEffectScaleLower = "matrix",
		criticalValuesEffectScaleUpper = "matrix",
		criticalValuesPValueScale = "matrix",
		futilityBoundsEffectScale = "matrix",
		futilityBoundsPValueScale = "matrix"
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
	
			visibleFieldNames <- .getVisibleFieldNames()
			startIndex <- which(visibleFieldNames == "hazardRatio")
			for (i in startIndex:length(visibleFieldNames)) {
				.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
			}
			
			.setParameterType("maxNumberOfSubjects", C_PARAM_NOT_APPLICABLE)
			.setParameterType("maxNumberOfSubjects1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("maxNumberOfSubjects2", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median1", C_PARAM_NOT_APPLICABLE)
			.setParameterType("median2", C_PARAM_NOT_APPLICABLE)
			.setParameterType("accountForObservationTimes", C_PARAM_NOT_APPLICABLE)
			.setParameterType("omega", C_PARAM_NOT_APPLICABLE)
			.setParameterType("maxStudyDuration", C_PARAM_NOT_APPLICABLE)
			.setParameterType("accrualIntensityRelative", C_PARAM_NOT_APPLICABLE)
			
			.setParameterType("criticalValuesEffectScale", C_PARAM_NOT_APPLICABLE)
			.setParameterType("criticalValuesEffectScaleLower", C_PARAM_NOT_APPLICABLE)
			.setParameterType("criticalValuesEffectScaleUpper", C_PARAM_NOT_APPLICABLE)
			
			# set default values	
			for (parameterName in c("eventTime", "accrualTime", "accrualIntensity", 
					"kappa", "piecewiseSurvivalTime", "lambda1", "lambda2", 
					"followUpTime", "dropoutTime")) {
				.setDefaultValue(parameterName)
			}
		},
		
		clone = function(hazardRatio = NA_real_, pi1 = NA_real_) {
			hr <- NA_real_
			if (.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
				hr <- hazardRatio
				if (any(is.na(hazardRatio))) {
					hr <- .self$hazardRatio
				}
			}
			pi1Temp <- NA_real_
			if (.getParameterType("pi1") == C_PARAM_USER_DEFINED) {
				pi1Temp <- pi1
				if (any(is.na(pi1))) {
					pi1Temp <- .self$pi1	
				}
			}
			accrualTimeTemp <- .self$.getParameterValueIfUserDefinedOrDefault("accrualTime")
			if (!is.null(accrualTimeTemp) && length(accrualTimeTemp) > 0 && 
					!all(is.na(accrualTimeTemp)) && accrualTimeTemp[1] != 0) {
				accrualTimeTemp <- c(0, accrualTimeTemp)
			}
			accrualIntensityTemp <- .self$.getParameterValueIfUserDefinedOrDefault("accrualIntensity")
			if (all(is.na(accrualIntensityTemp))) {
				accrualIntensityTemp <- C_ACCRUAL_INTENSITY_DEFAULT
			}
			if (.objectType == "sampleSize") {
				return(getSampleSizeSurvival(design = .self$.design, 
					typeOfComputation = .self$.getParameterValueIfUserDefinedOrDefault("typeOfComputation"), 
					thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"), 
					pi1 = pi1Temp, 
					pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"), 
					allocationRatioPlanned = .self$allocationRatioPlanned, 
					accountForObservationTimes = .self$.getParameterValueIfUserDefinedOrDefault("accountForObservationTimes"), 
					eventTime = .self$eventTime, 
					accrualTime = accrualTimeTemp, 
					accrualIntensity = accrualIntensityTemp, 
					kappa = .self$kappa, 
					piecewiseSurvivalTime = .self$.getParameterValueIfUserDefinedOrDefault("piecewiseSurvivalTime"), 
					lambda2 = .self$.getParameterValueIfUserDefinedOrDefault("lambda2"),
					lambda1 = .self$.getParameterValueIfUserDefinedOrDefault("lambda1"),
					followUpTime = .self$.getParameterValueIfUserDefinedOrDefault("followUpTime"), 
					maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"), 
					dropoutRate1 = .self$dropoutRate1, 
					dropoutRate2 = .self$dropoutRate2, 
					dropoutTime = .self$dropoutTime, 
					hazardRatio = hr))
			} else {
				directionUpperTemp <- directionUpper
				if (length(directionUpperTemp) > 1) {
					directionUpperTemp <- directionUpperTemp[1]
				}
				return(getPowerSurvival(design = .self$.design, 
					typeOfComputation = .self$.getParameterValueIfUserDefinedOrDefault("typeOfComputation"), 
					thetaH0 = .self$.getParameterValueIfUserDefinedOrDefault("thetaH0"), 
					pi1 = pi1Temp, 
					pi2 = .self$.getParameterValueIfUserDefinedOrDefault("pi2"), 
					directionUpper = directionUpperTemp,
					allocationRatioPlanned = .self$allocationRatioPlanned, 
					eventTime = .self$eventTime, 
					accrualTime = accrualTimeTemp, 
					accrualIntensity = accrualIntensityTemp, 
					kappa = .self$kappa, 
					piecewiseSurvivalTime = .self$.getParameterValueIfUserDefinedOrDefault("piecewiseSurvivalTime"), 
					lambda2 = .self$.getParameterValueIfUserDefinedOrDefault("lambda2"), 
					lambda1 = .self$.getParameterValueIfUserDefinedOrDefault("lambda1"), 
					hazardRatio = hr,
					maxNumberOfSubjects = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfSubjects"), 
					maxNumberOfEvents = .self$.getParameterValueIfUserDefinedOrDefault("maxNumberOfEvents"),
					dropoutRate1 = .self$dropoutRate1, 
					dropoutRate2 = .self$dropoutRate2, 
					dropoutTime = .self$dropoutTime))
			}
		},
		
		.setDefaultValue = function(argumentName) {
			if (is.null(.self[[argumentName]]) || all(is.na(.self[[argumentName]])))  {
				.self[[argumentName]] <<- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[[argumentName]]
				.setParameterType(argumentName, C_PARAM_DEFAULT_VALUE)
			}
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			callSuper(showType)
		}, 
		
		.warnInCaseArgumentExists = function(argument, argumentName) {
			if (!all(is.na(argument)) && any(argument > 0)) {
				warning(sprintf("Specified '%s' (%s) not taken into account", 
					argumentName, .arrayToString(argument)), call. = FALSE)
			}
		}
	)
)

#'
#' @name TrialDesignPlanSurvival_summary
#' 
#' @title
#' Trial Design Plan Survival Set Summary
#'
#' @description
#' Displays a summary of \code{TrialDesignPlanSurvival} object.
#' 
#' @details
#' Summarizes the parameters and results of a survival design.
#' 
#' @export
#' 
#' @keywords internal
#' 
summary.TrialDesignPlanSurvival <- function(object, ..., type = 1) {
	if (type == 1) {
		return(invisible(summary.ParameterSet(object = object, ..., type = type)))
	} 
	
	object$.cat("This output summarizes the ", object$.toString(), " specification.\n\n", heading = 1)
	
	object$show()
	object$.cat("\n")
	
	object$.piecewiseSurvivalTime$show()
	object$.cat("\n")
	
	object$.accrualTime$show()
	object$.cat("\n")
	
	object$show(showType = 2)
	object$.cat("\n")
	
	object$.cat(object$.toString(startWithUpperCase = TRUE), " table:\n", heading = 1)
	parametersToShow <- object$.getParametersToShow()
	parametersToShow <- parametersToShow[parametersToShow != "stages" & parametersToShow != "stage"]
	for (parameter in parametersToShow) {
		if (length(object[[parameter]]) == 1) {
			parametersToShow <- parametersToShow[parametersToShow != parameter]
		}
	}
	object$.printAsDataFrame(parameterNames = parametersToShow)
	
	invisible(object)
}

.addPlotSubTitleItems <- function(designPlan, designMaster, items, type) {
	
	if (type %in% c(1, 3, 4)) {
		return(invisible())
	}
	
	if (.isTrialDesignPlanMeans(designPlan)) {
		
		nMax <- designPlan$maxNumberOfSubjects[1] # use first value for plotting
		
		if  (!(type %in% c(5))) {
			items$add("N", round(nMax, 1), "max")
		}
		
		if  ((type %in% c(5)) && !(items$title == "Sample Size")) {
			items$add("N", round(nMax, 1), "max")
		}	
		
		if (designPlan$meanRatio) {
			items$add("coefficient of variation", designPlan$stDev) 
		} else { 
			items$add("standard deviation", designPlan$stDev)
		}

		if (designPlan$groups == 1) {
			if  (type %in% c(2,(5:9))) {
				items$add("H0: mu", designPlan$thetaH0)
				items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
			}
		}	else {
			if  (type %in% c(2,(5:9))) {
				if (designPlan$meanRatio) {
					items$add("H0: mean ratio", designPlan$thetaH0) 
				} else {
					items$add("H0: mean difference", designPlan$thetaH0)
				}
				items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
			}
		}
		
	} else if (.isTrialDesignPlanRates(designPlan)) {
		
		nMax <- designPlan$maxNumberOfSubjects[1] # use first value for plotting
		
		if  (!(type %in% c(5))){
			items$add("N", round(nMax, 1), "max")
		}
		
		if  ((type %in% c(5)) && !(items$title == "Sample Size")) {
			items$add("N", round(nMax, 1), "max")
		}	
		
		if (designPlan$groups == 2 && !(type %in% c(3, 4)) && 
				length(designPlan$pi2) == 1 && !is.na(designPlan$pi2)) { 
			items$add("pi", designPlan$pi2, 2)
		}
		
		if (designPlan$groups == 1) {
			if  (type %in% c(2,(5:9))) {
				items$add("H0: pi", designPlan$thetaH0)
			}	
		} else {
			if  (type %in% c(2,(5:9))) {
				if (designPlan$riskRatio) {
					items$add("H0: risk ratio", designPlan$thetaH0) 
				} else {
					items$add("H0: risk difference", designPlan$thetaH0)
				}
				items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
			}
		}
		
	} else if (.isTrialDesignPlanSurvival(designPlan)) {
		
		if (designPlan$.isPowerObject() && !(type %in% (13:14))) {
			items$add("maximum number of events", designPlan$maxNumberOfEvents[1])
		}
		if (type %in% (10:12)) {
			items$add("maximum number of subjects", designPlan$maxNumberOfSubjects[1])
		}	
		if (type %in% c(2,(5:12))) {
			items$add("H0: hazard ratio", designPlan$thetaH0)
			items$add("allocation ratio", round(designPlan$allocationRatioPlanned, 2))
		}
	}
}

.assertIsValidVariedParameterVectorForPlotting <- function(designPlan, plotType) {
	if (.isTrialDesignPlanMeans(designPlan)) {
		if (is.null(designPlan$alternative) || is.na(designPlan$alternative) || 
				length(designPlan$alternative) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available if 'alternative' with length > 1 is defined")
		}
	} else if (.isTrialDesignPlanRates(designPlan)) {
		if (is.null(designPlan$pi1) || is.na(designPlan$pi1) || 
				length(designPlan$pi1) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available if 'pi1' with length > 1 is defined")
		}
	} else if (.isTrialDesignPlanSurvival(designPlan)) {
		if (is.null(designPlan$hazardRatio) || is.na(designPlan$hazardRatio) || 
				length(designPlan$hazardRatio) <= 1) { 
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "plot type ", plotType, 
				" is only available if 'hazardRatio' with length > 1 is defined")
		}
	}
}

.plotTrialDesignPlan <- function(designPlan, designMaster, type = 1L, main = NA_character_, 
		xlab = NA_character_, ylab = NA_character_, palette = "Set1",
		theta = seq(-1, 1, 0.02), plotPointsEnabled = NA, 
		legendPosition = NA_integer_, showSource = FALSE, designPlanName = NA_character_, ...) {
	
	.assertGgplotIsInstalled()
	.assertIsTrialDesignPlan(designPlan) 
	.assertIsValidLegendPosition(legendPosition)
	theta <- .assertIsValidThetaRange(thetaRange = theta)
	
	nMax <- ifelse(.isTrialDesignPlanSurvival(designPlan), designPlan$maxNumberOfEvents[1], 
		designPlan$maxNumberOfSubjects[1]) # use first value for plotting
	
	plotSettings <- designPlan$.plotSettings
	
	if (designMaster$kMax == 1 && (type %in% c(1:4))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
			") is not available for 'kMax' = 1")
	}
	
	if (designPlan$.isSampleSizeObject()) {
		if (.isTrialDesignPlanSurvival(designPlan)) {
			if (!(type %in% c(1:5, 13, 14))) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
					") is not allowed; must be 1, 2, 3, 4, 5, 13 or 14")
			}
		} else {
			if (!(type %in% c(1:5))) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, 
					") is not allowed; must be 1, 2, 3, 4, 5")
			}
		}
	}
	
	if (is.na(plotPointsEnabled)) { 
		plotPointsEnabled <- type < 4
	}
	
	ratioEnabled <- (.isTrialDesignPlanSurvival(designPlan) || 
		(.isTrialDesignPlanMeans(designPlan) && designPlan$meanRatio) || 
		(.isTrialDesignPlanRates(designPlan) && designPlan$riskRatio))
	
	variedParameters <- logical(0)
	
	showSourceHint <- ""
	if (type %in% c(5:12)) {
		if (.isTrialDesignPlanMeans(designPlan) && length(designPlan$alternative) == 2 &&
				designPlan$.getParameterType("alternative") == C_PARAM_USER_DEFINED) {
			if (showSource) {
				showSourceHint <- .getVariedParameterHint(designPlan$alternative, "alternative")
			}
			designPlan <- designPlan$clone(alternative = 
				.getVariedParameterVector(designPlan$alternative, "alternative"))
		}
		else if ((.isTrialDesignPlanRates(designPlan) || .isTrialDesignPlanSurvival(designPlan)) && 
				length(designPlan$pi1) == 2 &&
				designPlan$.getParameterType("pi1") == C_PARAM_USER_DEFINED) {
			if (showSource) {
				showSourceHint <- .getVariedParameterHint(designPlan$pi1, "pi1")
			}
			designPlan <- designPlan$clone(pi1 = 
				.getVariedParameterVector(designPlan$pi1, "pi1"))
		}
		else if (.isTrialDesignPlanSurvival(designPlan) && length(designPlan$hazardRatio) == 2 &&
				designPlan$.getParameterType("hazardRatio") == C_PARAM_USER_DEFINED) {
			if (showSource) {
				showSourceHint <- .getVariedParameterHint(designPlan$hazardRatio, "hazardRatio")
			}
			designPlan <- designPlan$clone(hazardRatio = 
				.getVariedParameterVector(designPlan$hazardRatio, "hazardRatio"))
		}
	}
	
	if (type == 1) { # Boundary plot
		if (.isTrialDesignPlanSurvival(designPlan)) {
			
			main <- ifelse(is.na(main), "Boundaries Z Scale", main)

			if (designMaster$sided == 1) {
				designPlan <- data.frame(
					eventsPerStage = designPlan$eventsPerStage[, 1],
					criticalValues = designMaster$criticalValues,
					futilityBounds = c(designMaster$futilityBounds, designMaster$criticalValues[designMaster$kMax])
				)
			} else {
				designPlan <- data.frame(
					eventsPerStage = designPlan$eventsPerStage[, 1],
					criticalValues = designMaster$criticalValues,
					criticalValuesMirrored = -designMaster$criticalValues
				)
			}
			
			xParameterName <- "eventsPerStage"
			if (designMaster$sided == 1) {
				if (any(designMaster$futilityBounds > -6)) {
					yParameterNames <- c("futilityBounds", "criticalValues")
				} else {
					yParameterNames <- "criticalValues"
				}
			} else {
				yParameterNames <- c("criticalValues", "criticalValuesMirrored")
			}
			
			if (is.na(legendPosition)) {
				legendPosition <- C_POSITION_RIGHT_TOP
			}
			
			.showPlotSourceInformation(objectName = designPlanName, 
				xParameterName = xParameterName, yParameterNames = yParameterNames, 
				hint = showSourceHint, nMax = nMax,
				showSource = showSource)
			
		} else {
			designSet <- TrialDesignSet(design = designMaster, singleDesign = TRUE)
			return(.plotTrialDesignSet(x = designSet, y = NULL, main = main, 
					xlab = xlab, ylab = ylab, type = type,
					palette = palette, theta = theta, nMax = nMax, 
					plotPointsEnabled = plotPointsEnabled, legendPosition = legendPosition, 
					designSetName = designPlanName, ...))
		}
	}
	
	else if (type == 2) { # Effect Scale Boundary plot
		reducedParam <- .warnInCaseOfUnusedValuesForPlotting(designPlan)
		
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Boundaries Effect Scale")
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			if (!is.null(reducedParam)) {
				items$add(reducedParam$title, reducedParam$value, reducedParam$subscript)
			}
			main <- items$toQuote()
		}
		
		if (is.na(ylab)) {
			if (.isTrialDesignPlanMeans(designPlan)) {
				if (designPlan$groups == 1) {
					ylab <- "Mean"
				} else if (!designPlan$meanRatio) {
					ylab <- "Mean Difference"
				} else {
					ylab <- "Mean Ratio"
				}
			} else if (.isTrialDesignPlanRates(designPlan)) {
				if (designPlan$groups == 1) {
					ylab <- "Rate"
				} else if (!designPlan$riskRatio) {
					ylab <- "Rate Difference"
				} else {
					ylab <- "Risk Ratio"
				}
			} else if (.isTrialDesignPlanSurvival(designPlan)) {
				ylab <- "Hazard Ratio"
			}
		}
			
		yParameterNamesSrc <- c()
		if (designMaster$sided == 1) {
			data <- data.frame(
				criticalValues = designPlan$criticalValuesEffectScale[, 1],
				futilityBounds = c(designPlan$futilityBoundsEffectScale[, 1], 
					designPlan$criticalValuesEffectScale[designMaster$kMax, 1])
			)
			yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleUpper[, 1]")
			yParameterNamesSrc <- c(yParameterNamesSrc, "futilityBoundsEffectScale[, 1]")
		} else {
			data <- data.frame(
				criticalValues = designPlan$criticalValuesEffectScaleUpper[, 1],
				criticalValuesMirrored = designPlan$criticalValuesEffectScaleLower[, 1]
			)
			yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleUpper[, 1]")
			yParameterNamesSrc <- c(yParameterNamesSrc, "criticalValuesEffectScaleLower[, 1]")
		}
		
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "eventsPerStage"
			data <- cbind(data.frame(eventsPerStage = designPlan$eventsPerStage[, 1]), data)
		} else {
			xParameterName <- "informationRates"
			data <- cbind(data.frame(informationRates = designMaster$informationRates), data)
		}
		if (designMaster$sided == 1) {
			if (any(designMaster$futilityBounds > -6)) {
				yParameterNames <- c("futilityBounds", "criticalValues")
			} else {
				yParameterNames <- "criticalValues"
			}
		} else {
			yParameterNames <- c("criticalValues", "criticalValuesMirrored") 
		}
		
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_RIGHT_TOP
		}
		
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_RIGHT_TOP
		}
		
		designPlan <- data
		
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	} 
	
	else if (type == 3) { # Stage Levels
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Boundaries p Values Scale")
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "eventsPerStage"
			yParameterNames <- "stageLevels"
			designPlan <- data.frame(
				eventsPerStage = designPlan$eventsPerStage[, 1],
				stageLevels = designMaster$stageLevels
			)
			xParameterNameSrc <- "eventsPerStage[, 1]"
			yParameterNamesSrc <- ".design$stageLevels"
		} else {
			xParameterName <- "informationRates"
			yParameterNames <- "stageLevels"
			designPlan <- TrialDesignSet(design = designMaster, singleDesign = TRUE)
			xParameterNameSrc <- ".design$informationRates"
			yParameterNamesSrc <- ".design$stageLevels"
		}
		
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	} 
	
	else if (type == 4) { # Alpha Spending
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Type One Error Spending")
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			main <- items$toQuote()
		}
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "eventsPerStage"
			yParameterNames <- "alphaSpent"
			designPlan <- data.frame(
				eventsPerStage = designPlan$eventsPerStage[, 1],
				alphaSpent = designMaster$alphaSpent
			)
			xParameterNameSrc <- "eventsPerStage[, 1]"
			yParameterNamesSrc <- ".design$alphaSpent"
		} else {
			xParameterName <- "informationRates"
			yParameterNames <- "alphaSpent"
			designPlan <- TrialDesignSet(design = designMaster, singleDesign = TRUE)
			xParameterNameSrc <- ".design$informationRates"
			yParameterNamesSrc <- ".design$alphaSpent"
		}
		plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), FALSE, plotPointsEnabled)
		
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterNameSrc, 
			yParameterNames = yParameterNamesSrc, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	} 
	
	else if (type == 5) { # Power and Stopping Probabilities 
		
		.assertIsValidVariedParameterVectorForPlotting(designPlan, type)
		
		if (designPlan$.isSampleSizeObject()) { 
			if (is.na(main)) {
				items <- PlotSubTitleItems(title = "Sample Size")
				.addPlotSubTitleItems(designPlan, designMaster, items, type)
				main <- items$toQuote()
			}
			
			yAxisScalingEnabled <- TRUE
			
			if (.isTrialDesignPlanMeans(designPlan)) {
				xParameterName <- "alternative"
				yParameterNames <- c("nFixed", "maxNumberOfSubjects", "expectedNumberOfSubjectsH1") 
				if (is.na(ylab)) {
					ylab <- "Sample Size"
				}
				yAxisScalingEnabled <- FALSE
				if (is.na(legendPosition)) {
					legendPosition <- C_POSITION_RIGHT_TOP
				}
				yParameterNamesSrc <- yParameterNames
				
			} else if (.isTrialDesignPlanRates(designPlan)) {
				xParameterName <- "pi1"
				yParameterNames <- c("nFixed", "maxNumberOfSubjects", "expectedNumberOfSubjectsH1") 
				if (is.na(ylab)) {
					ylab <- "Sample Size"
				}
				yAxisScalingEnabled <- FALSE
				if (is.na(legendPosition)) {
					legendPosition <- C_POSITION_RIGHT_TOP
				}
				yParameterNamesSrc <- yParameterNames
				
			} else if (.isTrialDesignPlanSurvival(designPlan)) {
				designPlan <- data.frame(
					hazardRatio = designPlan$hazardRatio,
					eventsFixed = designPlan$eventsFixed,
					maxNumberOfEvents = designPlan$eventsPerStage[designMaster$kMax, ],
					expectedEventsH1 = designPlan$expectedEventsH1
				)
				xParameterName <- "hazardRatio"
				yParameterNames <- c("eventsFixed", "maxNumberOfEvents", "expectedEventsH1")
				if (is.na(ylab)) {
					ylab <- "# Events"
				}
				
				if (is.na(legendPosition)) {
					legendPosition <- C_POSITION_LEFT_TOP
				}
				yParameterNamesSrc <- c("eventsFixed", 
					paste0("eventsPerStage[", designMaster$kMax, ", ]"), "expectedEventsH1")
			}
			
			.showPlotSourceInformation(objectName = designPlanName, 
				xParameterName = xParameterName, 
				yParameterNames = yParameterNamesSrc,
				hint = showSourceHint, nMax = nMax,
				showSource = showSource)
			
			return(.plotParameterSet(parameterSet = designPlan, designMaster = designMaster, 
					xParameterName = xParameterName,
					yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
					palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
					legendPosition = legendPosition, variedParameters = variedParameters, 
					qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE, ...))
		} else {
			if (is.na(main)) {
				items <- PlotSubTitleItems(title = "Overall Power and Early Stopping")
				.addPlotSubTitleItems(designPlan, designMaster, items, type)
				main <- items$toQuote()
			}
			if (.isTrialDesignPlanSurvival(designPlan)) {
				xParameterName <- "hazardRatio"
			} else {
				xParameterName <- "effect"
			}
			yParameterNames <- c("overallReject", "futilityStop", "earlyStop")
			
			if (.isTrialDesignPlanRates(designPlan)) {
				if (is.na(ylab)) {
					ylab <- ""
				}
				if (is.na(legendPosition)) {
					legendPosition <- C_POSITION_LEFT_TOP
				}
				if (is.null(list(...)[["ylim"]])) {
					ylim <- c(0, 1)
					return(.plotParameterSet(parameterSet = designPlan, designMaster = designMaster, 
						xParameterName = xParameterName,
						yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
						palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
						legendPosition = legendPosition, variedParameters = variedParameters, 
						qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE, ylim = ylim, ...))
				} else {
					return(.plotParameterSet(parameterSet = designPlan, designMaster = designMaster, 
						xParameterName = xParameterName,
						yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
						palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
						legendPosition = legendPosition, variedParameters = variedParameters, 
						qnormAlphaLineEnabled = FALSE, yAxisScalingEnabled = FALSE, ...))
				}
			} else {
				if (is.na(legendPosition)) {
					legendPosition <- C_POSITION_RIGHT_CENTER
				}
			}
			
			.showPlotSourceInformation(objectName = designPlanName, 
				xParameterName = xParameterName, 
				yParameterNames = yParameterNames, 
				hint = showSourceHint, nMax = nMax,
				showSource = showSource)
		}
	} 
	
	else if (type == 6) { # Average Sample Size / Average Event Number
		.assertIsValidVariedParameterVectorForPlotting(designPlan, type) 
		
		if (is.na(main)) {
			titlePart <- ifelse(.isTrialDesignPlanSurvival(designPlan), "Number of Events", "Sample Size")
			items <- PlotSubTitleItems(title = paste0("Expected ", titlePart, " and Power / Early Stop"))
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "hazardRatio"
			yParameterNames <- "expectedNumberOfEvents"
			expectedNumberOfEvents <- designPlan[["expectedNumberOfEvents"]]
			if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
				yParameterNames <- "expectedEventsH1" 
			}
			yParameterNames <- c(yParameterNames, "overallReject", "earlyStop") # overallReject = power
			if (is.na(legendPosition)) {
				legendPosition <- C_POSITION_RIGHT_CENTER
			}
		} else {
			xParameterName <- "effect"
			yParameterNames <- c("expectedNumberOfSubjects", "overallReject", "earlyStop") # overallReject = power
		}
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 7) {
		.assertIsValidVariedParameterVectorForPlotting(designPlan, type) 
		
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Overall Power")
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		}
		yParameterNames <- "overallReject"
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_RIGHT_CENTER
		}
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 8) {
		.assertIsValidVariedParameterVectorForPlotting(designPlan, type) 
		
		if (is.na(main)) {
			items <- PlotSubTitleItems(title = "Overall Early Stopping")
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "hazardRatio"
		} else {
			xParameterName <- "effect"
		}
		yParameterNames <-  c("earlyStop", "futilityStop")
		if (is.na(legendPosition)) {
			legendPosition <- C_POSITION_RIGHT_CENTER
		}
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (type == 9) {
		.assertIsValidVariedParameterVectorForPlotting(designPlan, type) 
		
		if (is.na(main)) {
			if (.isTrialDesignPlanSurvival(designPlan)) {
				items <- PlotSubTitleItems(title = "Expected Number of Events")
			} else {
				items <- PlotSubTitleItems(title = "Expected Sample Size")
			}
			.addPlotSubTitleItems(designPlan, designMaster, items, type)
			main <- items$toQuote()
		}
		
		if (.isTrialDesignPlanSurvival(designPlan)) {
			xParameterName <- "hazardRatio"
			yParameterNames <- "expectedNumberOfEvents"
			expectedNumberOfEvents <- designPlan[["expectedNumberOfEvents"]]
			if (is.null(expectedNumberOfEvents) || length(expectedNumberOfEvents) == 0) {
				yParameterNames <- c("expectedEventsH0", "expectedEventsH1")
				if (is.na(legendPosition)) {
					legendPosition <- C_POSITION_RIGHT_CENTER
				}
			}
		} else {
			xParameterName <- "effect"
			yParameterNames <- "expectedNumberOfSubjects"
		}
		.showPlotSourceInformation(objectName = designPlanName, 
			xParameterName = xParameterName, 
			yParameterNames = yParameterNames, 
			hint = showSourceHint, nMax = nMax,
			showSource = showSource)
	}
	
	else if (.isTrialDesignPlanSurvival(designPlan)) {
		
		if (type == 10) { # Study Duration
			.assertIsValidVariedParameterVectorForPlotting(designPlan, type)
			if (is.na(main)) {
				items <- PlotSubTitleItems(title = "Study Duration")
				.addPlotSubTitleItems(designPlan, designMaster, items, type)
				main <- items$toQuote()
			}
			xParameterName <- "hazardRatio"
			yParameterNames <- "studyDuration"
			.showPlotSourceInformation(objectName = designPlanName, 
				xParameterName = xParameterName, 
				yParameterNames = yParameterNames, 
				hint = showSourceHint, nMax = nMax,
				showSource = showSource)
		}
		
		else if (type == 11) {
			.assertIsValidVariedParameterVectorForPlotting(designPlan, type)
			if (is.na(main)) {
				items <- PlotSubTitleItems(title = "Expected Number of Subjects")
				.addPlotSubTitleItems(designPlan, designMaster, items, type)
				main <- items$toQuote()
			}
			xParameterName <- "hazardRatio"
			yParameterNames <- "expectedNumberOfSubjects" 
			.showPlotSourceInformation(objectName = designPlanName, 
				xParameterName = xParameterName, 
				yParameterNames = yParameterNames, 
				hint = showSourceHint, nMax = nMax,
				showSource = showSource)
		}
		
		else if (type == 12) { # Analysis Time
			.assertIsValidVariedParameterVectorForPlotting(designPlan, type)
			if (is.na(main)) {
				items <- PlotSubTitleItems(title = "Analysis Times")
				.addPlotSubTitleItems(designPlan, designMaster, items, type)
				main <- items$toQuote()
			}
			
			xParameterName <- "hazardRatio"
			yParameterNames <- "analysisTime"
			
			data <- NULL
			for (k in 1:designMaster$kMax) {
				part <- data.frame(
					categories = rep(k, length(designPlan$hazardRatio)),
					xValues = designPlan$hazardRatio,
					yValues = designPlan$analysisTime[k, ]
				)
				if (is.null(data)) {
					data <- part
				} else {
					data <- rbind(data, part)
				}
			}
			
			.showPlotSourceInformation(objectName = designPlanName, 
				xParameterName = xParameterName, 
				yParameterNames = yParameterNames, 
				hint = showSourceHint,
				showSource = showSource)
			
			return(.plotDataFrame(data, mainTitle = main, 
					xlab = NA_character_, ylab = NA_character_, xAxisLabel = "Hazard Ratio",
					yAxisLabel1 = "Analysis Time", yAxisLabel2 = NA_character_, 
					plotPointsEnabled = TRUE, legendTitle = "Stage",
					legendPosition = legendPosition, sided = designMaster$sided, ...))
		}
		
		else if (type == 13 || type == 14) { # Cumulative Distribution Function / Survival function
			return(.plotSurvivalFunction(designPlan, designMaster = designMaster, type = type, main = main, 
				xlab = xlab, ylab = ylab, palette = palette,
				legendPosition = legendPosition, showSource = showSource, ...))
		}
		
		else {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 14")	
		}
	} 
	
	else {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 9")	
	}
	
	return(.plotParameterSet(parameterSet = designPlan, designMaster = designMaster, 
			xParameterName = xParameterName,
			yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
			palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
			legendPosition = legendPosition, variedParameters = variedParameters, 
			qnormAlphaLineEnabled = (type != 2), ratioEnabled = ratioEnabled,
			plotSettings = plotSettings, ...))
}

# Cumulative Distribution Function / Survival function
.plotSurvivalFunction <- function(designPlan, ..., designMaster, type = 1L, main = NA_character_, 
		xlab = NA_character_, ylab = NA_character_, palette = "Set1",
		legendPosition = NA_integer_, showSource = FALSE) {
		
	if (is.null(designPlan$piecewiseSurvivalTime) || 
		length(designPlan$piecewiseSurvivalTime) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'piecewiseSurvivalTime' must be specified")
	}
	
	lambda1 <- designPlan[["lambda1"]]
	lambda2 <- designPlan[["lambda2"]]
	if (is.null(lambda2) || length(lambda2) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'lambda2' must be specified")
	}
	
	if (is.null(designPlan$kappa) || length(designPlan$kappa) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'kappa' must be specified")
	}
	
	if (is.null(designPlan$hazardRatio) || length(designPlan$hazardRatio) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'hazardRatio' must be specified")
	}
	
	piecewiseSurvivalEnabled <- designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled
	
	if (is.na(main)) {
		if (type == 13) {
			items <- PlotSubTitleItems(title = "Cumulative Distribution Function")
		} else {
			items <- PlotSubTitleItems(title = "Survival Function")
		}
		.addPlotSubTitleItems(designPlan, designMaster, items, type)
		if (!piecewiseSurvivalEnabled) {
			if (designPlan$.piecewiseSurvivalTime$.isLambdaBased(minNumberOfLambdas = 1)) {
				items$add("lambda", designPlan$lambda1[1], 1)
				items$add("lambda", designPlan$lambda2, 2)
			} else {
				items$add("pi", designPlan$pi1[1], 1)
				items$add("pi", designPlan$pi2, 2)
			}
		} else if (length(designPlan$hazardRatio) == 1) {
			items$add("Hazard Ratio", designPlan$hazardRatio[1])
		}
		main <- items$toQuote()
	}
	
	if (!piecewiseSurvivalEnabled || (length(designPlan$piecewiseSurvivalTime) == 1 && 
			designPlan$piecewiseSurvivalTime[1] == 0)) {
		timeTo <- max(designPlan$analysisTime[designMaster$kMax, ])
	} else {
		timeTo <- max(designPlan$piecewiseSurvivalTime)
	}
	if (is.na(timeTo) || !is.numeric(timeTo) || is.infinite(timeTo)) {
		#warning("Unable to determine upper bound of time values", call. = FALSE)
		timeTo <- 0
	}
	
	timeValues <- seq(0, timeTo + 10, 0.1)
	
	data <- data.frame(
		time = timeValues,
		lambdaGroup1 = rep(-1, length(timeValues)),
		lambdaGroup2 = rep(-1, length(timeValues)),
		survival1 = rep(-1, length(timeValues)),
		survival2 = rep(-1, length(timeValues)),
		survivalGroup1 = rep(-1, length(timeValues)),
		survivalGroup2 = rep(-1, length(timeValues))
	)
	
	if (piecewiseSurvivalEnabled) {
		data$survival2 <- .getPiecewiseExponentialDistribution(timeValues, 
			lambda2, designPlan$piecewiseSurvivalTime, designPlan$kappa)
		
		if (!is.null(lambda1) && !is.na(lambda1) && 
				length(lambda1) == length(lambda2)) {
			data$survival1 <- .getPiecewiseExponentialDistribution(timeValues, 
				lambda1, designPlan$piecewiseSurvivalTime, designPlan$kappa)
		} else {
			.warnInCaseOfUnusedValuesForPlottingSurvival(designPlan$hazardRatio)
			data$survival1 <- data$survival2 * designPlan$hazardRatio[1]
		}
	} else {
		if (designPlan$.piecewiseSurvivalTime$.isLambdaBased(minNumberOfLambdas = 1)) {
			if (length(designPlan$lambda1) > 1) {
				warning("Only the first 'lambda1' (", designPlan$lambda1[1], ") was used for plotting", call. = FALSE)
			}
		} else {
			.warnInCaseOfUnusedValuesForPlottingRates(designPlan$pi1)
		}
		
		lambda2 <- (-log(1 - designPlan$pi2))^(1/designPlan$kappa) / designPlan$eventTime
		lambda1 <- (-log(1 - designPlan$pi1[1]))^(1/designPlan$kappa) / designPlan$eventTime
		
		data$survival2 <- .getPiecewiseExponentialDistribution(timeValues, 
			lambda2, 0, designPlan$kappa)
		data$survival1 <- .getPiecewiseExponentialDistribution(timeValues, 
			lambda1, 0, designPlan$kappa)
	}
	
	# two groups: 1 = treatment, 2 = control
	if (type == 14) {
		data$survival1 <- 1 - data$survival1
		data$survival2 <- 1 - data$survival2
	}
	
	if (piecewiseSurvivalEnabled) {
		data$lambdaGroup2 <- .getLambdaStepFunction(timeValues, 
			designPlan$piecewiseSurvivalTime, lambda2)
		if (length(lambda1) == 1) {
			if (!is.na(lambda1)) {
				data$lambdaGroup1 <- rep(lambda1, length(data$lambdaGroup2))
			} else {
				data$lambdaGroup1 <- data$lambdaGroup2 * designPlan$hazardRatio[1]
			}
		} else {
			data$lambdaGroup1 <- .getLambdaStepFunction(timeValues, 
				designPlan$piecewiseSurvivalTime, lambda1)
		}
	} else {
		data$lambdaGroup2 <- .getLambdaStepFunction(timeValues, 0, lambda2)
		data$lambdaGroup1 <- .getLambdaStepFunction(timeValues, 0, lambda1)
	}
	
	scalingBaseValues1 <- na.omit(c(data$survival1, data$survival2))
	scalingBaseValues2 <- na.omit(c(data$lambdaGroup1, data$lambdaGroup2))
	scalingFactor <- max(scalingBaseValues1) / max(.getNextHigherValue(scalingBaseValues2)) 
	data2 <- data.frame(
		categories = c(
			rep("Treatm. piecew. exp.", nrow(data)),
			rep("Control piecew. exp.", nrow(data)),
			rep("Treatm. piecew. lambda", nrow(data)),
			rep("Control piecew. lambda", nrow(data))
		),
		xValues = rep(data$time, 4),
		yValues = c(
			data$survival1,
			data$survival2,
			data$lambdaGroup1 * scalingFactor, 
			data$lambdaGroup2 * scalingFactor
		)
	)
	
	if (is.na(legendPosition)) {
		if (type == 13) {
			legendPosition <- C_POSITION_LEFT_TOP
		} else {
			legendPosition <- C_POSITION_RIGHT_TOP
		}
	}
	
	if (is.na(palette) || palette == "Set1") {
		palette <- "Paired"
	}
	
	if (type == 13) {
		yAxisLabel1 <- "Cumulative Distribution Function"
	} else {
		yAxisLabel1 <- "Survival Function"
	}
	
	if (showSource) {
		warning("'showSource' = TRUE is not supported yet for plot type ", type, call. = FALSE)
	}
	
	return(.plotDataFrame(data2, mainTitle = main, 
			xlab = xlab, ylab = ylab, xAxisLabel = "Time",
			yAxisLabel1 = yAxisLabel1, yAxisLabel2 = "Lambda", 
			plotPointsEnabled = FALSE, legendTitle = NA_character_,
			legendPosition = legendPosition, scalingFactor1 = 1, 
			scalingFactor2 = scalingFactor, palette = palette, sided = designMaster$sided))
}

.warnInCaseOfUnusedValuesForPlottingMeans <- function(alternative) {
	if (length(alternative) > 1) {
		warning("Only the first 'alternative' (", round(alternative[1], 3), 
			") was used for plotting", call. = FALSE)
		return(list(title = "alternative", value = alternative[1], subscript = NA_character_))
	} 
	return(NULL)
}

.warnInCaseOfUnusedValuesForPlottingRates <- function(pi1) {
	if (length(pi1) > 1) {
		warning("Only the first 'pi1' (", round(pi1[1], 3), 
			") was used for plotting", call. = FALSE)
		return(list(title = "pi", value = pi1[1], subscript = "1"))
	}
	return(NULL)
}

.warnInCaseOfUnusedValuesForPlottingSurvival <- function(hazardRatio) {
	if (length(hazardRatio) > 1) {
		warning("Only the first 'hazardRatio' (", round(hazardRatio[1], 3), 
			") was used for plotting", call. = FALSE)
		return(list(title = "hazardRatio", value = hazardRatio[1], subscript = NA_character_))
	}
	return(NULL)
}

.warnInCaseOfUnusedValuesForPlotting <- function(designPlan) {
	if (.isTrialDesignPlanMeans(designPlan) && designPlan$.isSampleSizeObject()) {
		return(.warnInCaseOfUnusedValuesForPlottingMeans(designPlan$alternative))
	}
	if (.isTrialDesignPlanRates(designPlan) && designPlan$.isSampleSizeObject()) {
		return(.warnInCaseOfUnusedValuesForPlottingRates(designPlan$pi1))
	}
	if (.isTrialDesignPlanSurvival(designPlan) && designPlan$.isSampleSizeObject()) {
		return(.warnInCaseOfUnusedValuesForPlottingSurvival(designPlan$hazardRatio))
	}
	return(NULL)
}

#'
#' @title
#' Trial Design Plan Plotting
#' 
#' @param x The trial design plan, obtained from \cr
#'        \code{\link{getSampleSizeMeans}}, \cr
#'        \code{\link{getSampleSizeRates}}, \cr
#'        \code{\link{getSampleSizeSurvival}}, \cr
#'        \code{\link{getPowerMeans}}, \cr
#'        \code{\link{getPowerRates}} or \cr
#'        \code{\link{getPowerSurvival}}.
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
#' Plots a trial design plan.
#' 
#' @details
#' Generic function to plot all kinds of trial design plans.
#' 
#' @export
#'
plot.TrialDesignPlan = function(x, y, main = NA_character_,
		xlab = NA_character_, ylab = NA_character_, type = ifelse(x$.design$kMax == 1, 5, 1), palette = "Set1",
		theta = seq(-1, 1, 0.01), plotPointsEnabled = NA, 
		legendPosition = NA_integer_, showSource = FALSE, ...) {
		
	fCall = match.call(expand.dots = FALSE)
	designPlanName <- as.character(fCall$x)[1]
	
	nMax <- list(...)[["nMax"]]
	if (!is.null(nMax)) {
		warning(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'nMax' (", nMax, 
			") will be ignored because it will be taken from design plan")
	}
	
	.plotTrialDesignPlan(designPlan = x, designMaster = x$.design, 
		main = main, xlab = xlab, ylab = ylab, type = type,
		palette = palette, theta = theta, plotPointsEnabled = plotPointsEnabled, 
		legendPosition = legendPosition, showSource = showSource,
		designPlanName = designPlanName, ...)
}


