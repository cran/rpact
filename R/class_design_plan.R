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
	groups = 2, 
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
	accrualTime = 12, 
	followUpTime = 24, 
	maxNumberOfPatients = 0, 
	dropOutRate1 = 0, 
	dropOutRate2 = 0, 
	dropOutTime = 12
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
#' @name TrialDesignPlan_initialize
#' Initializes the object.
#' 
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignPlan <- setRefClass("TrialDesignPlan",
	contains = "ParameterSet",
	fields = list(
		.design = "TrialDesign"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(.design = design, ...)
			
			.parameterNames <<- C_PARAMETER_NAMES 
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
				if (sum(is.na(existingValue)) == length(existingValue)) {
					.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
				}
				else if (!is.null(defaultValue) && length(defaultValue) == length(existingValue) &&
						!is.na(defaultValue) && !is.na(existingValue) && 
						sum(defaultValue == existingValue) == length(defaultValue)) {
					.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
				} else {
					.setParameterType(parameterName, C_PARAM_USER_DEFINED)
				}
			}
			.setParameterType("alpha", C_PARAM_DERIVED)
			.setParameterType("beta", C_PARAM_DERIVED)
			.setParameterType("sided", C_PARAM_DERIVED)
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			if (showType == 2) {
				cat("Technical summary of the design plan object of class",
					methods::classLabel(class(.self)), ":\n\n", sep = "")
				.showAllParameters()
				.showParameterTypeDescription()
			} else {
				cat("Design plan parameters and output for ", .toString(), ":\n\n", sep = "")
				.showParametersOfOneGroup(c(".design$alpha", ".design$beta", ".design$sided", ".design$twoSidedPower"), "Design parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Sample size and output",
					orderByParameterName = FALSE)
				.showUnknownParameters()
				twoGroupsEnabled <- class(.self) == "TrialDesignPlanSurvival" || groups > 1
				if (.design$kMax > 1 || twoGroupsEnabled) {
					cat("Legend:\n")
					if (twoGroupsEnabled) {
						cat("  (i): values of treatment group i\n")
					}
					if (.design$kMax > 1) {
						cat("  [k]: values at stage k\n")
					}
					cat("\n")
				}
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
				return("means")
			}
			
			if (.isTrialDesignPlanRates(.self)) {
				return("rates")
			}
			
			if (.isTrialDesignPlanSurvival(.self)) {
				return("survival data")
			}
			
			return(paste0("unknown data class '", class(.self), "'"))
		}
	)
)

#'
#' @name TrialDesignPlan_print
#' 
#' @title
#' Print Trial Design Plan
#'
#' @description
#' \code{print} prints its \code{TrialDesignPlan} argument and returns it invisibly (via \code{invisible(x)}).
#' 
#' @details
#' Prints the parameters and results of a design plan.
#' 
#' @export
#' 
#' @keywords internal
#' 
print.TrialDesignPlan <- function(x, ...) {
	cat(x$.toString(startWithUpperCase = TRUE), "table:\n")
	parametersToShow <- x$.getParametersToShow()
	parametersToShow <- parametersToShow[parametersToShow != "stages" & parametersToShow != "stage"]
	for (parameter in parametersToShow) {
		if (length(x[[parameter]]) == 1) {
			parametersToShow <- parametersToShow[parametersToShow != parameter]
		}
	}
	x$.printAsDataFrame(parameterNames = parametersToShow)
	invisible(x)
}

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
#' @name TrialDesignPlanMeans_initialize
#' Initializes the object.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
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
		
		nFixed = "numeric",
		nFixed1 = "numeric", 
		nFixed2 = "numeric", 
		
		informationRates = "matrix",		
		maxNumberOfPatients = "numeric",					
		numberOfPatients = "matrix",
		numberOfPatientsGroup1 = "matrix",
		numberOfPatientsGroup2 = "matrix",
		expectedPatientsH0 = "numeric",
		expectedPatientsH01 = "numeric",
		expectedPatientsH1 = "numeric"
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
			
			.validateInput()
			
			visibleFieldNames <- .getVisibleFieldNames()
			startIndex <- which(visibleFieldNames == "nFixed1")
			for (i in startIndex:length(visibleFieldNames)) {
				.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
			}
			
			if (groups == 1) {
				.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
				.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
			}
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			callSuper(showType)
		}, 
		
		.validateInput = function() {
			.assertIsTrialDesignInverseNormalOrGroupSequential(.design)
			.assertIsValidAlphaAndBeta(.design$alpha, .design$beta)	
			.assertIsValidSidedParameter(.design$sided)
			.assertIsValidStandardDeviation(stDev)
			.assertIsValidGroupsParameter(groups)
			
			if (.design$sided == 1 && any(alternative - thetaH0 <= 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"alternative (", .arrayToString(alternative - thetaH0), ") is not allowed to be non-positive")
			}
			
			if (any(alternative - thetaH0 == 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"alternative (", .arrayToString(alternative - thetaH0), ") is not allowed to be zero")
			}
			
			if (groups == 2) {
				
				if (.design$sided == 2 && ((thetaH0 != 0 && !meanRatio) || (thetaH0 != 1 && meanRatio))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"two-sided case is implemented only for superiority testing")
				}
				
				if (allocationRatioPlanned < 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"allocation ratio (", allocationRatioPlanned, ") is not allowed to be negative")
				}
				
				if (.design$sided == 1 && meanRatio && thetaH0 <= 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "mean ratio is not allowed to be negative or zero")
				}
			}
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
#' @name TrialDesignPlanRates_initialize
#' Initializes the object.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
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
		
		nFixed = "numeric",
		nFixed1 = "numeric", 
		nFixed2 = "numeric", 
		
		informationRates = "matrix",		
		maxNumberOfPatients = "numeric",					
		numberOfPatients = "matrix",
		numberOfPatientsGroup1 = "matrix",
		numberOfPatientsGroup2 = "matrix",
		expectedPatientsH0 = "numeric",
		expectedPatientsH01 = "numeric",
		expectedPatientsH1 = "numeric"
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
			
			.validateInput()
			
			visibleFieldNames <- .getVisibleFieldNames()
			startIndex <- which(visibleFieldNames == "nFixed1")
			for (i in startIndex:length(visibleFieldNames)) {
				.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
			}
			
			if (groups == 1) {
				.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
				.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
			}
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			callSuper(showType)
		}, 
		
		.validateInput = function() {
			.assertIsTrialDesignInverseNormalOrGroupSequential(.design)
			.assertIsValidAlphaAndBeta(.design$alpha, .design$beta)	
			.assertIsValidSidedParameter(.design$sided)
			.assertIsValidGroupsParameter(groups)			
			
			if (groups == 1) {
				
				if (any(pi1 <= 0) || any(pi1 >= 1)) {
					stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
						"probability 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)")
				}
				
				if (thetaH0 >= 1 || thetaH0 <= 0) {
					stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, "'thetaH0' (", thetaH0, ") is out of bounds (0; 1)")
				}
				
				if (!normalApproximation && .design$sided == 2) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
						"exact sample size calculation not available for two-sided testing")
				}
			}
			
			else if (groups == 2) {
				
				if (any(pi1 <= 0) || any(pi1 >= 1)) {
					stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
						"probability 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)")
				}
				
				if (any(pi2 <= 0) || any(pi2 >= 1)) {
					stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
						"probability 'pi2' (", .arrayToString(pi2), ") is out of bounds (0; 1)")
				}
				
				if (.design$sided == 2 && ((thetaH0 != 0 && !riskRatio) || (thetaH0 != 1 && riskRatio))) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "two-sided case is implemented only for superiority testing")
				}
				
				if (!normalApproximation) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "only normal approximation case is implemented for two groups")
				}
				
				if (allocationRatioPlanned < 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"allocation ratio (", allocationRatioPlanned, ") is not allowed to be negative")
				}
				
				if (riskRatio && thetaH0 <= 0) { 
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "null hypothesis risk ratio is not allowed be negative or zero")			
				}
			}
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
#' @name TrialDesignPlanSurvival_initialize
#' Initializes the object.
#'
#' @include class_core_parameter_set.R
#' @include class_design.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignPlanSurvival <- setRefClass("TrialDesignPlanSurvival",	
	contains = "TrialDesignPlan",
	fields = list(
		typeOfComputation = "character", 
		thetaH0 = "numeric", 
		pi1 = "numeric", 
		pi2 = "numeric", 
		allocationRatioPlanned = "numeric", 
		accountForObservationTimes = "logical",
		eventTime = "numeric",
		accrualTime = "numeric",
		followUpTime = "numeric",
		maxNumberOfPatients = "numeric",
		dropOutRate1 = "numeric",
		dropOutRate2 = "numeric",
		dropOutTime = "numeric",
		
		hazardRatio = "numeric", 
		omega = "numeric",
		calculateFollowUpTime = "logical", 
		eventsFixed = "numeric",
		nFixed = "numeric",
		nFixed1 = "numeric", 
		nFixed2 = "numeric",
		
		informationRates = "matrix",
		analysisTimes = "matrix", 
		eventsOverStages = "matrix", 		
		numberOfPatients = "matrix",
		numberOfPatientsGroup1 = "matrix",
		numberOfPatientsGroup2 = "matrix",		
		expectedEventsH0 = "numeric",
		expectedEventsH01 = "numeric",
		expectedEventsH1 = "numeric",	
		expectedNumberOfPatientsH1 = "numeric",
		studyDurationH1 = "numeric"
	),
	methods = list(
		initialize = function(...,
				typeOfComputation = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["typeOfComputation"]],  
				thetaH0 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["thetaH0"]], 
				pi1 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["pi1"]], 
				pi2 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["pi2"]], 
				allocationRatioPlanned = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["allocationRatioPlanned"]], 
				accountForObservationTimes = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["accountForObservationTimes"]],
				eventTime = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["eventTime"]], 
				accrualTime = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["accrualTime"]], 
				followUpTime = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["followUpTime"]], 
				maxNumberOfPatients = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["maxNumberOfPatients"]], 
				dropOutRate1 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["dropOutRate1"]], 
				dropOutRate2 = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["dropOutRate2"]], 
				dropOutTime = C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[["dropOutTime"]]
			) {
			callSuper(..., 
				typeOfComputation = typeOfComputation, 
				thetaH0 = thetaH0, 
				pi1 = pi1,
				pi2 = pi2, 
				allocationRatioPlanned = allocationRatioPlanned,
				accountForObservationTimes = accountForObservationTimes,
				eventTime = eventTime,
				accrualTime = accrualTime,
				followUpTime = followUpTime,
				maxNumberOfPatients = maxNumberOfPatients,
				dropOutRate1 = dropOutRate1,
				dropOutRate2 = dropOutRate2,
				dropOutTime = dropOutTime
			)

			.validateInput()
	
			visibleFieldNames <- .getVisibleFieldNames()
			startIndex <- which(visibleFieldNames == "hazardRatio")
			for (i in startIndex:length(visibleFieldNames)) {
				.setParameterType(visibleFieldNames[i], C_PARAM_NOT_APPLICABLE)
			}
			
			if (!.self$accountForObservationTimes) {
				for (p in c("eventTime", "accrualTime", "dropOutRate1", "dropOutRate2", 
						"dropOutTime", "followUpTime", "analysisTimes")) {
					.setParameterType("meanRatio", C_PARAM_NOT_APPLICABLE)
				}
			}
			
			.setParameterType("maxNumberOfPatients", C_PARAM_NOT_APPLICABLE)
			
			# set default values	
			.setDefaultValue("eventTime")
			.setDefaultValue("accrualTime")
			.setDefaultValue("followUpTime")
			.setDefaultValue("dropOutTime")
		},
		
		.setDefaultValue = function(argumentName) {
			if (is.na(.self[[argumentName]]))  {
				.self[[argumentName]] <<- C_TRIAL_DESIGN_PLAN_DEFAULT_VALUES_SURVIVAL[[argumentName]]
				.setParameterType(argumentName, C_PARAM_DEFAULT_VALUE)
			}
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial plan objects'
			callSuper(showType)
		}, 
		
		.validateInput = function() {
			.assertIsTrialDesignInverseNormalOrGroupSequential(.design)
			.assertIsValidAlphaAndBeta(.design$alpha, .design$beta)
			.assertIsValidSidedParameter(.design$sided)
			
			if (any(pi1 <= 0) || any(pi1 >= 1)) {
				stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
					"event rate 'pi1' (", .arrayToString(pi1), ") is out of bounds (0; 1)")
			}
			
			if (any(pi2 <= 0) || any(pi2 >= 1)) {
				stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
					"event rate 'pi2' (", .arrayToString(pi2), ") is out of bounds (0; 1)")
			}
			
			if (.design$sided == 2 && thetaH0 != 1) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "two-sided case is implemented only for superiority testing")
			}
			
			if (!(typeOfComputation %in% c("Schoenfeld", "Freedman", "HsiehFreedman"))) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"computation type ('", typeOfComputation, "') must be one of the following: ", 
					"'Schoenfeld', 'Freedman', or 'HsiehFreedman' ")
			}
			
			if (typeOfComputation != "Schoenfeld" && thetaH0 != 1) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"Freedman test calculation is possible only for superiority testing (thetaH0 != 1)")
			}
			
			if (allocationRatioPlanned < 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'allocationRatioPlanned' (", allocationRatioPlanned, ") is not allowed to be negative")
			}
			
			if (maxNumberOfPatients > 0) {
				if (!is.na(accountForObservationTimes) && !accountForObservationTimes) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'accountForObservationTimes' must be TRUE because 'maxNumberOfPatients' is > 0")
				}
				
				.setParameterType("accountForObservationTimes", 
					ifelse(is.na(accountForObservationTimes) || !accountForObservationTimes, 
						C_PARAM_GENERATED, C_PARAM_USER_DEFINED))
				accountForObservationTimes <<- TRUE
			} else {
				if (is.na(accountForObservationTimes)) {
					accountForObservationTimes <<- FALSE
				}
				.setParameterType("accountForObservationTimes", 
					ifelse(!accountForObservationTimes, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
			}
			
			if (accountForObservationTimes) {
				if (!is.na(dropOutTime) && dropOutTime <= 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dropOutTime' (", dropOutTime, ") must be > 0")
				}
				
				if (dropOutRate1 < 0 || dropOutRate1 >= 1) {
					stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
						"'dropOutRate1' (", dropOutRate1, ") is out of bounds [0; 1)")
				}
				
				if (dropOutRate2 < 0 || dropOutRate2 >= 1) {
					stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
						"'dropOutRate2' (", dropOutRate2, ") is out of bounds [0; 1)")
				}
				
				if (!is.na(eventTime) && eventTime <= 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'eventTime' (", eventTime, ") must be > 0")
				}
				
				if (!is.na(accrualTime) && accrualTime <= 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' (", accrualTime, ") must be > 0")
				}
				
				if (!is.na(accrualTime) && !is.na(followUpTime) && accrualTime + followUpTime <= 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the sum of accrual time and follow-up time (", 
						(accrualTime + followUpTime), ") must be > 0")
				}
			} else {
				.validateArgument(accrualTime, "accrualTime")
				.validateArgument(eventTime, "eventTime")
				.validateArgument(dropOutTime, "dropOutTime")
				.validateArgument(dropOutRate1, "accrualTime")
				.validateArgument(dropOutRate2, "accrualTime")
				.validateArgument(maxNumberOfPatients, "maxNumberOfPatients")
			}
			
			if (maxNumberOfPatients > 0) {	
				if (!is.na(followUpTime)) {
					warning("Follow-up time will be calculated, value entered (", 
						followUpTime, ") is not taken into account", call. = FALSE)
				}
				
				if (allocationRatioPlanned == 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"determination of optimal allocation ratio not possible ", 
						"if 'maxNumberOfPatients' > 0, i.e., follow-up time should be calculated ",
						"(please specify an 'allocationRatioPlanned' != 0)")
				}
			}	
		},
		
		.validateArgument = function(argument, argumentName) {
			if (!is.na(argument) && argument > 0) {
				warning(sprintf("Specified '%s' (%s) not taken into account", argumentName, argument), 
					call. = FALSE)
			}
		}
	)
)
