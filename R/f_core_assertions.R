######################################################################################
#                                                                                    #
# -- RPACT assertions --                                                             #
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

.stopWithWrongDesignMessage <- function(design) {
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'design' must be an instance of ", .arrayToString(
			.getTrialDesignClassNames(), vectorLookAndFeelEnabled = FALSE), " (is '", class(design), "')")
}

.isParameterSet <- function(x) {
	return(isS4(x) && inherits(x, "ParameterSet"))
}

.assertIsParameterSetClass <- function(x, objectName = "x") {
	if (!.isParameterSet(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", objectName, "' (", class(x), ") must be a S4 class which inherits from class 'ParameterSet' ")
	}
}

.assertIsTrialDesignSet <- function(x, objectName = "x") {
	if (!.isTrialDesignSet(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'designSet' must be an instance of 'TrialDesignSet' (is '", class(x), "')")
	}
}

.isTrialDesignSet <- function(x) {
	return(class(x) == "TrialDesignSet")
}

.isTrialDesignGroupSequential <- function(design) {
	return(class(design) == C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL)
}

.isTrialDesignInverseNormal <- function(design) {
	return(class(design) == C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL)
}

.isTrialDesignFisher <- function(design) {
	return(class(design) == C_CLASS_NAME_TRIAL_DESIGN_FISHER)
}

.isTrialDesignInverseNormalOrGroupSequential <- function(design) {
	return(.isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design))
}

.isTrialDesign <- function(design) {
	return(.isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design) || 
			.isTrialDesignFisher(design))
}

.isTrialDesignPlanMeans <- function(designPlan) {
	return(class(designPlan) == "TrialDesignPlanMeans")
}

.isTrialDesignPlanRates <- function(designPlan) {
	return(class(designPlan) == "TrialDesignPlanRates")
}

.isTrialDesignPlanSurvival <- function(designPlan) {
	return(class(designPlan) == "TrialDesignPlanSurvival")
}

.isTrialDesignPlan <- function(designPlan) {
	return(.isTrialDesignPlanMeans(designPlan) || 
			.isTrialDesignPlanRates(designPlan) || 
			.isTrialDesignPlanSurvival(designPlan))
}

.assertIsTrialDesignPlan <- function(designPlan) {
	if (!.isTrialDesignPlan(designPlan)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'designPlan' must be an instance of 'TrialDesignPlan' (is '", class(designPlan), "')")
	}
}

.assertIsTrialDesign <- function(design) {
	if (!.isTrialDesign(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'design' must be an instance of ", .arrayToString(
				.getTrialDesignClassNames(), vectorLookAndFeelEnabled = FALSE), " (is '", class(design), "')")
	}
}

.assertIsTrialDesignInverseNormal <- function(design) {
	if (!.isTrialDesignInverseNormal(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignInverseNormal' (is '", class(design), "')")
	}
}

.assertIsTrialDesignFisher <- function(design) {
	if (!.isTrialDesignFisher(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignFisher' (is '", class(design), "')")
	}
}

.assertIsTrialDesignGroupSequential <- function(design) {
	if (!.isTrialDesignGroupSequential(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignGroupSequential' (is '", class(design), "')")
	}
}

.assertIsTrialDesignInverseNormalOrGroupSequential <- function(design) {
	if (!.isTrialDesignInverseNormalOrGroupSequential(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignInverseNormal' or 'TrialDesignGroupSequential' (is '", 
			class(design), "')")
	}
}

.isSimulationResults <- function(simulationResults) {
	return(inherits(simulationResults, "SimulationResults"))
}

.assertIsSimulationResults <- function(simulationResults) {
	if (!.isSimulationResults(simulationResults)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'simulationResults' must be an instance of SimulationResults (is '", class(simulationResults), "')")
	}
}

.isStageResults <- function(stageResults) {
	return(.isStageResultsMeans(stageResults) || .isStageResultsRates(stageResults) || 
			.isStageResultsSurvival(stageResults))
}

.isStageResultsMeans <- function(stageResults) {
	return(class(stageResults) == "StageResultsMeans")
}

.isStageResultsRates <- function(stageResults) {
	return(class(stageResults) == "StageResultsRates")
}

.isStageResultsSurvival <- function(stageResults) {
	return(class(stageResults) == "StageResultsSurvival")
}

.assertIsStageResults <- function(stageResults) {
	if (!.isStageResults(stageResults)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stageResults' must be an instance of ", .arrayToString(
				.getStageResultsClassNames(), vectorLookAndFeelEnabled = FALSE), " (is '", class(stageResults), "')")
	}
}

.assertIsInClosedInterval <- function(x, xName, ..., lower, upper, naAllowed = FALSE) {
	.warnInCaseOfUnknownArguments(functionName = ".assertIsInClosedInterval", ...)
	if (naAllowed && all(is.na(x))) {
		return(invisible())
	}
	
	if (length(x) > 1 && any(is.na(x))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", xName, "' (", .arrayToString(x), ") must be a valid numeric vector or a single NA")
	}
	
	if (is.null(upper) || is.na(upper)) {
		if (any(x < lower)) {
			prefix <- ifelse(length(x) > 1, "each value of ", "")
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, prefix,
				"'", xName, "' (", .arrayToString(x), ") must be >= ", lower)
		}
	}
	else if (any(x < lower) || any(x > upper)) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
			"'", xName, "' (", .arrayToString(x), ") is out of bounds [", lower, "; ", upper, "]")
	}
}

.assertIsInOpenInterval <- function(x, xName, lower, upper, naAllowed = FALSE) {
	if (naAllowed && all(is.na(x))) {
		return(invisible())
	}
	
	if (length(x) > 1 && any(is.na(x))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", xName, "' (", .arrayToString(x), ") must be a valid numeric vector or a single NA")
	}
	
	if (is.null(upper) || is.na(upper)) {
		if (any(x <= lower)) {
			prefix <- ifelse(length(x) > 1, "each value of ", "")
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, prefix,
				"'", xName, "' (", .arrayToString(x), ") must be > ", lower)
		}
	}
	else if (any(x <= lower) || any(x >= upper)) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
			"'", xName, "' (", .arrayToString(x), ") is out of bounds (", lower, "; ", upper, ")")
	}
}

.assertIsValidDataInput <- function(dataInput, design = NULL, stage = NULL) {
	.assertIsDataset(dataInput)
	if (!is.null(design)) {
		.assertIsTrialDesign(design)
	}
	
	stages <- dataInput$stages
	l1 <- length(stages)
	for (fieldName in dataInput$.getVisibleFieldNames()) {
		l2 <- length(dataInput[[fieldName]])
		if (fieldName != "stages" && l1 != l2) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, 
				"all parameters must have the same length ('stage' has length ", l1,
				", '", fieldName, "' has length ", l2, ")")
		}
	}
	
	if (!is.null(stage)) {
		if (dataInput$getNumberOfGroups() == 1) {
			if (.isDatasetMeans(dataInput) ) {
				if (any(dataInput$getStDevsUpTo(stage) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all standard deviations must be > 0")
				} 
				if (any(dataInput$getSampleSizesUpTo(stage) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				} 
			} 
			else if (.isDatasetRates(dataInput) ) {
				if (any(dataInput$getEventsUpTo(stage) < 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be >= 0")
				}
				if (any(dataInput$getSampleSizesUpTo(stage) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				}
				if (any(dataInput$getEventsUpTo(stage) > dataInput$getSampleSizesUpTo(stage))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be <= corresponding sample size")
				}
			}
		}
		else if (dataInput$getNumberOfGroups() == 2) {
			if (.isDatasetMeans(dataInput) ) {

				if (any(dataInput$getStDevsUpTo(stage, 1) <= 0) || any(dataInput$getStDevsUpTo(stage, 2) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all standard deviations must be > 0")
				} 
				if (any(dataInput$getSampleSizesUpTo(stage, 1) <= 0) || 
						any(dataInput$getSampleSizesUpTo(stage, 2) <= 0)	) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				} 
			} 
			else if (.isDatasetRates(dataInput) ) {
				if (any(dataInput$getEventsUpTo(stage, 1) < 0) || 
						any(dataInput$getEventsUpTo(stage, 2) < 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be >= 0")
				}
				if (any(dataInput$getSampleSizesUpTo(stage, 1) <= 0)  || 
						any(dataInput$getSampleSizesUpTo(stage, 2) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				}
				if (any(dataInput$getEventsUpTo(stage, 1) > dataInput$getSampleSizesUpTo(stage, 1)) ||  
						any(dataInput$getEventsUpTo(stage, 2) > dataInput$getSampleSizesUpTo(stage, 2))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be <= corresponding sample size")
				}
			}
		}
		
		if (.isDatasetSurvival(dataInput) ) {
			if (any(dataInput$getOverallEventsUpTo(stage) < 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all overall events must be >= 0")
			}
			
			if (any(dataInput$getOverallAllocationRatiosUpTo(stage) <= 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all overall allocation ratios must be > 0")
			}
		}
	}
	
	if (!is.null(design)) {
		numberOfStages <- length(unique(stats::na.omit(stages)))
		kMax <- design$kMax
		if (numberOfStages > kMax) {
			s <- numberOfStages - kMax
			plural <- ifelse(s == 1, "", "s")
			warning(sprintf(paste0("The data of the last %s in the dataset will be ",
					"ignored because the design has specified kMax = %s"), 
				ifelse(s == 1, "stage", paste0(s, " stages")), kMax, kMax), call. = FALSE)
		}
		else if (numberOfStages < kMax) {
			dataInput$.fillWithNAs(kMax)
		}
	}
	
	invisible(dataInput)
}

.assertIsDataset <- function(dataInput) {
	if (!.isDataset(dataInput)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' must be an instance of class ",
			"'DatasetMeans', 'DatasetRates' or 'DatasetSurvival' (is '", class(dataInput), "')")
	}	
}

.assertIsDatasetMeans <- function(dataInput) {
	if (!.isDatasetMeans(dataInput = dataInput)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' must be an instance of class ",
			"'DatasetMeans' (is '", class(dataInput), "')")
	}	
}

.assertIsDatasetRates <- function(dataInput) {
	if (!.isDatasetRates(dataInput = dataInput)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' must be an instance of class ",
			"'DatasetRates' (is '", class(dataInput), "')")
	}	
}

.assertIsDatasetSurvival <- function(dataInput) {
	if (!.isDatasetSurvival(dataInput = dataInput)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' must be an instance of class ",
			"'DatasetSurvival' (is '", class(dataInput), "')")
	}	
}

.isDataset <- function(dataInput) {
	return(.isDatasetMeans(dataInput) || .isDatasetRates(dataInput) || .isDatasetSurvival(dataInput))
}

.isDatasetMeans <- function(dataInput) {
	return(class(dataInput) == "DatasetMeans")
}

.isDatasetRates <- function(dataInput) {
	return(class(dataInput) == "DatasetRates")
}

.isDatasetSurvival <- function(dataInput) {
	return(class(dataInput) == "DatasetSurvival")
}

.assertIsNumericVector <- function(x, argumentName, naAllowed = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, 
			"' must be a valid numerical value or vector")
	}
	
	if ((!naAllowed && any(is.na(x))) || !is.numeric(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(x), ") must be a valid numerical value or vector")
	}
}

.assertIsIntegerVector <- function(x, argumentName, naAllowed = FALSE, validateType = TRUE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, 
			"' must be a valid integer value or vector")
	}
	
	if (naAllowed && all(is.na(x))) {
		return(invisible())
	}
	
	if ((!naAllowed && any(is.na(x))) || (validateType && !is.integer(x)) || 
			(!validateType && any(as.integer(x) != x))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(x), ") must be a valid integer value or vector")
	}
}

.assertIsSingleLogical <- function(x, argumentName, naAllowed = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid single logical value")
	}
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single logical value")
	}
	
	if ((!naAllowed && is.na(x)) || !is.logical(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", x, ") must be a valid single logical value")
	}
}

.assertIsSingleNumber <- function(x, argumentName, naAllowed = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid single numerical value")
	}
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single numerical value")
	}
	
	if ((!naAllowed && is.na(x)) || !is.numeric(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", x, ") must be a valid single numerical value")
	}
}


.assertIsSingleInteger <- function(x, argumentName, naAllowed = FALSE, validateType = TRUE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid single integer value")
	}
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single integer value")
	}
	
	if ((!naAllowed && is.na(x)) || (validateType && !is.integer(x)) || (!validateType && as.integer(x) != x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", x, ") must be a valid single integer value")
	}
}

.assertIsSinglePositiveInteger <- function(x, argumentName, naAllowed = FALSE, validateType = TRUE) {
	if (missing(x) || is.null(x) || length(x) == 0 || x <= 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a positive single integer value")
	}
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
				.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single integer value")
	}
	
	if ((!naAllowed && is.na(x)) || (validateType && !is.integer(x)) || (!validateType && as.integer(x) != x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", x, ") must be a positive single integer value")
	}
}

.assertIsCharacter <- function(x, argumentName) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid character value")
	}
	
	if (!is.character(x)) {
		stop(sprintf(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'%s' must be a valid character (is an instance of class '%s')", argumentName, class(x)))
	}
}

.assertDesignParameterExists <- function(design, parameterName, defaultValue) {
	if (missing(design)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'design' must be defined")
	}
	
	if (missing(parameterName)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'parameterName' must be defined")
	}
	
	if (missing(defaultValue)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'defaultValue' must be defined")
	}
	
	value <- design[[parameterName]]
	if (is.null(value) || length(value) == 0 || all(is.na(value))) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "parameter '", parameterName, 
			"' must be specified in design")
	}
	
	if (is.null(defaultValue) || length(defaultValue) == 0 || all(is.na(defaultValue))) {
		return(invisible())
	}
	
	if (all(value == defaultValue)) {
		design$.setParameterType(parameterName, C_PARAM_DEFAULT_VALUE)
	} else {
		design$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
	}
}

.designParameterExists <- function(design, parameterName) {
	if (missing(design)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'design' must be defined")
	}
	
	if (missing(parameterName)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'parameterName' must be defined")
	}
	
	value <- design[[parameterName]]
	if (is.null(value)) {
		return(FALSE)
	}
	
	if (length(value) > 1) {
		return(sum(is.na(value)) < length(value))
	}
	
	return(!is.na(value))
}

.assertIsOptimizationCriterion <- function(x) {
	if (!.isOptimizationCriterion(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"optimization criterion must be one of the following: ", .printOptimizationCriterion())
	}
}

.assertIsValidAlpha <- function(alpha) {
	.assertIsSingleNumber(alpha, "alpha")
	
	if (alpha < 1e-06 || alpha >= 0.5) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
			"'alpha' (", alpha, ") is out of bounds [1e-06; 0.5)")
	}
}

.assertIsValidKappa <- function(kappa) {
	.assertIsSingleNumber(kappa, "kappa")
	.assertIsInOpenInterval(kappa, "kappa", lower = 0, upper = NULL)
}

.assertIsValidLambda <- function(lambda, lambdaNumber = 0) {
	argumentName <- "lambda"
	if (lambdaNumber >= 1) {
		argumentName <- paste0("lambda", lambdaNumber)
	} 
	.assertIsNumericVector(lambda, argumentName, naAllowed = TRUE)
	if (all(is.na(lambda))) {
		return(invisible())
	}
	
	if (any(is.na(lambda))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(lambda), ") must be a valid numeric vector")
	}
	
	.assertIsInClosedInterval(lambda, argumentName, lower = 0, upper = NULL)
	if (all(lambda == 0)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(lambda), ") not allowed: ",
			"at least one lambda value must be > 0")
	}
}

.assertIsValidFollowUpTime <- function(followUpTime) {
	if (is.null(followUpTime) || length(followUpTime) == 0 || is.na(followUpTime)) {
		return(invisible())
	}
	
	.assertIsSingleNumber(followUpTime, "followUpTime", naAllowed = TRUE)
	if (followUpTime < 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'followUpTime' (", followUpTime, ") must be >= 0")
	}
}

.assertIsValidAccrualTime <- function(accrualTime) {
	.assertIsNumericVector(accrualTime, "accrualTime", naAllowed = TRUE)
	
	if (is.null(accrualTime) || length(accrualTime) == 0 || all(is.na(accrualTime))) {
		return(invisible())
	}
	
	if (any(accrualTime < 0)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'accrualTime' (", 
			.arrayToString(accrualTime), ") must be >= 0")
	}
}

.assertIsValidMaxNumberOfSubjects <- function(maxNumberOfSubjects) {
	.assertIsSingleNumber(maxNumberOfSubjects, "maxNumberOfSubjects")
	.assertIsInClosedInterval(maxNumberOfSubjects, "maxNumberOfSubjects", lower = 1, upper = NULL)
}

.assertIsValidStandardDeviation <- function(stDev) {
	.assertIsSingleNumber(stDev, "stDev")
	
	if (stDev <= 0) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, "standard deviation 'stDev' (", stDev, ") must be > 0")
	}
}

.assertIsValidBeta <- function(beta, alpha) {
	.assertIsSingleNumber(beta, "beta")
	.assertIsSingleNumber(alpha, "alpha")
	
	if (beta < 1e-04 || beta >= 1 - alpha) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
			"'beta' (", beta, ") is out of bounds [1e-04; ", (1 - alpha), "); ",
			"condition: 1e-05 <= alpha < 1 - beta <= 1 - 1e-04")
	}
}

.assertIsValidAlphaAndBeta <- function(alpha, beta) {
	.assertIsValidAlpha(alpha)
	.assertIsValidBeta(beta, alpha)
}

.assertIsValidStage <- function(stage, kMax) {
	if (stage < 1 || stage > kMax) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
			"'stage' (", stage, ") is out of bounds [1; ", kMax, "]")
	}
}

.assertIsValidIterationsAndSeed <- function(iterations, seed, zeroIterationsAllowed = TRUE) {
	if (is.null(iterations) || length(iterations) == 0 || !is.numeric(iterations)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'iterations' must be a valid integer value")
	}
	
	if (zeroIterationsAllowed) {
		if (iterations < 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'iterations' (", iterations, ") must be >= 0")
		}
		
	} else {
		if (iterations < 1) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'iterations' (", iterations, ") must be > 0")
		}
		
	}
	
	if (is.null(seed) || length(seed) == 0 || (!is.na(seed) && !is.numeric(seed))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'seed' (", seed, ") must be a valid integer value")
	}
}

.assertIsValidLegendPosition <- function(legendPosition) {
	if (is.null(legendPosition) || length(legendPosition) == 0 || 
			(!is.na(legendPosition) && !is.numeric(legendPosition))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'legendPosition' (", legendPosition, ") must be a valid integer value")
	}
	
	if (!is.na(legendPosition) && (legendPosition < -1 || legendPosition > 6)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'legendPosition' (", 
			legendPosition, ") must be an integer between -1 and 6")
	}
}

.assertIsValidKMax <- function(kMax, kMaxLowerBound = 1, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
	if (missing(kMax)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'kMax' must be defined")
	}
	
	if (is.null(kMax) || length(kMax) == 0 || sum(is.na(kMax)) > 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'kMax' is invalid")
	}
	
	if (kMax == Inf || kMax == -Inf) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
					"'kMax' (%s) is out of bounds [%s; %s]"), kMax, kMaxLowerBound, kMaxUpperBound))
	}
	
	if (kMax != as.integer(kMax)) {
		stop(sprintf(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'kMax' (%s) must be a valid integer (is an instance of class '%s')", kMax, class(kMax)))
	}
	
	if (kMax < kMaxLowerBound || kMax > kMaxUpperBound) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
				"'kMax' (%s) is out of bounds [%s; %s]"), kMax, kMaxLowerBound, kMaxUpperBound))
	}
}

.assertAreValidInformationRates <- function(informationRates, kMax = length(informationRates), 
		kMaxLowerBound = 1, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
		
	if (length(informationRates) < kMaxLowerBound) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
			"length of 'informationRates' (%s) is out of bounds [%s; %s]"),
			length(informationRates), kMaxLowerBound, 
			ifelse(kMax >= kMaxLowerBound && kMax < C_KMAX_UPPER_BOUND, kMax, C_KMAX_UPPER_BOUND)))
	}
	
	.assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	
	if (length(informationRates) != kMax) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"length of 'informationRates' (%s) must be equal to 'kMax' (%s)"),
				length(informationRates), kMax))
	}
	
	if (length(informationRates) > kMaxUpperBound) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
					"length of 'informationRates' (%s) is out of bounds [%s; %s]"),
				length(informationRates), kMaxLowerBound, kMax))
	}
	
	if (kMax == 1) {
		return(invisible())
	}
	
	.assertValuesAreAreInsideBounds("informationRates", informationRates, 
		0, 1, lowerBoundInclusive = FALSE)
	
	if (min(informationRates) <= 0 || max(informationRates) > 1 || 
			any(informationRates[2 : kMax] <= informationRates[1 : (kMax - 1)])) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'informationRates' (%s) ", 
				"must be strictly increasing: 0 < x_1 < .. < x_%s <= 1"), 
				.arrayToString(informationRates, vectorLookAndFeelEnabled = FALSE), kMax))
	}
}

.assertValuesAreAreInsideBounds <- function(parameterName, values, lowerBound, upperBound,
		lowerBoundInclusive = TRUE, upperBoundInclusive = TRUE) {
	lower <- min(values)
	upper <- max(values)
	lowerInvalid <- ifelse(lowerBoundInclusive, lower < lowerBound, lower <= lowerBound)
	upperInvalid <- ifelse(upperBoundInclusive, upper > upperBound, upper >= upperBound)
	if (lowerInvalid || upperInvalid) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
				"'%s' (%s) is out of bounds %s%s; %s%s"), 
				parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE),
				ifelse(lowerBoundInclusive, "[", "("), lowerBound,
				upperBound, ifelse(upperBoundInclusive, "]", ")")))
	}
}

.assertValuesAreStrictlyIncreasing <- function(values, parameterName) {
	len <- length(values)
	if (len <= 1) {
		return(invisible())
	}
	
	if (any(values[2 : len] <= values[1 : (len - 1)])) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ", 
			"must be strictly increasing: x_1 < .. < x_%s"), 
			parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE), len))
	}
}

.assertValuesAreMonotoneIncreasing <- function(values, parameterName) {
	len <- length(values)
	if (len <= 1) {
		return(invisible())
	}
	
	if (any(values[2 : len] < values[1 : (len - 1)])) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ", 
			"must be increasing: x_1 <= .. <= x_%s"), 
			parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE), len))
	}
}

.assertAreValidFutilityBounds <- function(futilityBounds, kMax = length(futilityBounds) + 1, 
		kMaxLowerBound = 1, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
		
	if (length(futilityBounds) < kMaxLowerBound - 1) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
			"length of 'futilityBounds' (%s) is out of bounds [%s; %s]"),
			length(futilityBounds), kMaxLowerBound - 1, 
			ifelse(kMax >= kMaxLowerBound && kMax < C_KMAX_UPPER_BOUND, kMax - 1, C_KMAX_UPPER_BOUND - 1)))
	}
	
	.assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	
	if (length(futilityBounds) != kMax - 1) {
		stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
			"length of 'futilityBounds' (", length(futilityBounds), 
			") must be equal to 'kMax' (", kMax, ") - 1")
	}
	
	.assertValuesAreAreInsideBounds("futilityBounds", futilityBounds, -6, 6)
}

.assertIsValidAlpha0Vec <- function(alpha0Vec, kMax = length(alpha0Vec) - 1, 
		kMaxLowerBound = 1, kMaxUpperBound = C_KMAX_UPPER_BOUND) {
		
	if (length(alpha0Vec) < kMaxLowerBound - 1) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS,
			"length of 'alpha0Vec' (%s) is out of bounds [%s; %s]"),
			length(alpha0Vec), kMaxLowerBound - 1, kMax - 1))
	}
	
	.assertIsValidKMax(kMax, kMaxLowerBound = kMaxLowerBound, kMaxUpperBound = kMaxUpperBound)
	
	if (length(alpha0Vec) != kMax - 1) {
		stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS,
			"length of 'alpha0Vec' (", length(alpha0Vec), 
			") must be equal to 'kMax' (", kMax, ") - 1")
	}
	
	.assertValuesAreAreInsideBounds("alpha0Vec", alpha0Vec, 0, 1, lowerBoundInclusive = FALSE)
}

.assertIsValidSidedParameter <- function(sided) {
	if (sided != 1 && sided != 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'sided' (", sided, ") must be 1 or 2")
	} 
}

.assertIsValidGroupsParameter <- function(groups) {
	if (groups != 1 && groups != 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'groups' (", groups, ") must be 1 or 2")
	} 
}


.assertIsValidForLogarithmization <- function(valueList) {
	if (getLogLevel() %in% c(C_LOG_LEVEL_PROGRESS, C_LOG_LEVEL_DISABLED)) {
		return(invisible())
	}
	
	if (missing(valueList)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'valueList' must be defined")
	}
	
	if (!is.list(valueList) || length(valueList) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'valueList' must be a valid list")
	}
	
	for (index in 1:length(valueList)) {
		value <- valueList[[index]]
		if (is.null(value) || is.na(value) || !is.numeric(value) || value < 0) {
			paramName <- names(valueList)[index]
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
				"logarithmization of '", paramName, "' (", value, ") produces NaN")
		}
	}
}

.allArgumentsAreNotNull <- function(...) {
	args <- list(...)
	naCounter <- 0
	for (arg in args) {
		if (!is.null(arg)) {
			naCounter <- naCounter + sum(is.na(arg))
		}
	}
	return(naCounter == 0)
}

.assertAssociatedArgumentsAreDefined <- function(...) {
	.associatedArgumentsAreDefined(..., warningOnlyEnabled = FALSE)
}

.associatedArgumentsAreDefined <- function(..., warningOnlyEnabled = TRUE) {
	if (.allArgumentsAreNotNull(...)) {
		return(TRUE)
	}
	
	args <- list(...)
	args <- args[args != "warningOnlyEnabled" & !is.null(args)]
	argNames <- names(args)
	if (sum(argNames == "") > 0) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "each argument must have a name defined, e.g. a = a")
	}
	
	definedArguments <- c()
	undefinedArguments <- c()
	for (i in 1:length(args)) {
		arg <- args[i]
		argName <- argNames[i]
		if (missing(arg) || (!is.null(arg) && sum(is.na(arg)) > 0)) {
			undefinedArguments <- c(undefinedArguments, argName)
		} else {
			definedArguments <- c(definedArguments, argName)
		}
	}
	if (length(undefinedArguments) > 0 && length(definedArguments) > 0) {
		message <- paste0(.arrayToString(undefinedArguments, encapsulate = TRUE),
			" ", ifelse(warningOnlyEnabled,"should","must"), 
			" be defined because ", .arrayToString(definedArguments, encapsulate = TRUE),
			ifelse(length(definedArguments) > 1, " are", " is"), " defined")
		if (warningOnlyEnabled) {
			warning(C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS, message, call. = FALSE)
			return(FALSE)
		} else {
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, message)
		}
	}
	
	invisible(length(definedArguments) == length(args))
}

.assertIsValidNPlanned <- function(nPlanned, kMax, stage) {
	if (length(nPlanned) != kMax - stage) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			sprintf(paste0("'nPlanned' (%s) is invalid: ", 
				"length must be equal to 'kMax' (%s) - 'stage' (%s)"),
				.arrayToString(nPlanned), kMax, stage))
	}
	
	if (sum(is.na(nPlanned)) > 0 || sum(nPlanned <= 0) > 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			sprintf(paste0("'nPlanned' (%s) is invalid: ", 
				"all values must be > 0"),
				.arrayToString(nPlanned)))
	}
}

.isValidNPlanned <- function(nPlanned, kMax, stage) {
	if (missing(nPlanned)) {
		warning("'nPlanned' is missing", call. = FALSE)
		return(FALSE)
	} 
	if (!any(is.na(nPlanned))) {
		if ((length(nPlanned) != kMax - stage)) {
			warning(sprintf(paste0("'nPlanned' (%s) will be ignored: ", 
				"length must be equal to 'kMax' (%s) - 'stage' (%s)"),
				.arrayToString(nPlanned), kMax, stage), call. = FALSE)
			return(FALSE)
		}
		
		if (sum(is.na(nPlanned)) > 0 || sum(nPlanned <= 0) > 0) {
			warning(sprintf(paste0("'nPlanned' (%s) will be ignored: ", 
						"all values must be > 0"),
					.arrayToString(nPlanned)), call. = FALSE)
			return(FALSE)
		}
	}
	return(TRUE)
}

.warnInCaseOfUnknownArguments <- function(..., functionName, ignore = c()) {
	args <- list(...)
	if (length(args) == 0) {
		return(invisible())
	}
	
	argNames <- names(args)
	for (i in 1:length(args)) {
		arg <- args[i]
		argName <- ifelse(is.null(argNames[i]), paste0("%param", i, "%"), argNames[i])
		if (!(argName %in% ignore)) {
			warning("Argument unknown in ", functionName, "(...): '", 
				argName, "' = ", arg, " will be ignored", call. = FALSE)
		}
	}
}

.warnInCaseOfUnusedArgument <- function(arg, argName, defaultValue, functionName) {
	if (!identical(arg, defaultValue)) {
		warning("Unused argument in ", functionName, "(...): '", 
			argName, "' = ", arg, " will be ignored", call. = FALSE)
	}
}

.assertIsDefined <- function(parameter, parameterName) {
	if (is.null(parameter) || any(is.na(parameter))) {
		stop("'", parameterName, "' must be defined")
	}
}

.isTrialDesignWithValidFutilityBounds <- function(design) {
	if (is.null(design) || !.isTrialDesignInverseNormalOrGroupSequential(design)) {
		return(FALSE)
	}
	
	futilityBounds <- design[["futilityBounds"]]
	if (is.null(futilityBounds)) {
		return(FALSE)
	}
	
	if (length(futilityBounds) == 0 || sum(is.na(futilityBounds)) == design$kMax) {
		return(FALSE)
	}
	
	return(sum(futilityBounds == C_FUTILITY_BOUNDS_DEFAULT) == 0)
}

.assertGgplotIsInstalled <- function() {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("Package \"ggplot2\" is needed for this function to work. Please install and load it.",
			call. = FALSE)
	}
}

.assertRcppIsInstalled <- function() {
	if (!requireNamespace("Rcpp", quietly = TRUE)) {
		stop("Package \"Rcpp\" is needed for this function to work. Please install and load it.",
			call. = FALSE)
	}
}

.assertTestthatIsInstalled <- function() {
	if (!requireNamespace("testthat", quietly = TRUE)) {
		stop("Package \"testthat\" is needed for this function to work. Please install and load it.",
			call. = FALSE)
	}
}

.assertIsValidThetaH0 <- function(thetaH0, ..., endpoint = c("means", "rates", "survival"), 
		groups, ratioEnabled = FALSE) {
		
	.warnInCaseOfUnknownArguments(functionName = ".assertIsValidThetaH0", ...)
		
	if (is.na(thetaH0)) {
		return(invisible())
	}
	
	if (!is.numeric(thetaH0)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'thetaH0' must be a valid numeric value")
	}
	
	endpoint <- match.arg(endpoint)
	if (endpoint == "means" || endpoint == "rates") {
		if (groups == 2 && ratioEnabled) {
			if (thetaH0 <= 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'thetaH0' (", thetaH0, ") must be > 0")
			}
			return(invisible())
		}
	}
	
	if (endpoint == "rates") {
		if (groups == 1) {
			if (thetaH0 <= 0 || thetaH0 >= 1) {
				stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
					"'thetaH0' (", thetaH0, ") is out of bounds (0; 1) or not specified") 
			}
		} else {
			if (thetaH0 <= -1 || thetaH0 >= 1) {
				stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
					"'thetaH0' (", thetaH0, ") is out of bounds (-1; 1)") 
			}
		}
	}
	else if (endpoint == "survival") {
		if (thetaH0 <= 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'thetaH0' (", thetaH0, ") must be > 0")
		}
	}
}

.assertIsValidThetaH0DataInput <- function(thetaH0, dataInput) {
	if (.isDatasetRates(dataInput)) {
		endpoint <- "rates"
	} else if (.isDatasetSurvival(dataInput)) {
		endpoint <- "survival"
	} else {
		endpoint <- "means"
	}
	.assertIsValidThetaH0(thetaH0, endpoint = endpoint, groups = dataInput$getNumberOfGroups())
}

.assertIsValidThetaRange <- function(..., thetaRange, thetaAutoSeqEnabled = TRUE, 
		survivalDataEnabled = FALSE) {	
	if (length(thetaRange) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'thetaRange' must be a vector with two entries defining minimum and maximum ",
			"or a sequence of values with length > 2")
	}
	else if (length(thetaRange) == 2 && thetaAutoSeqEnabled) {
		minValue <- thetaRange[1]
		maxValue <- thetaRange[2]
		if (survivalDataEnabled) {
			.assertIsValidHazardRatio(minValue, "thetaRange[1]")
			.assertIsValidHazardRatio(maxValue, "thetaRange[2]")
		}
		if (minValue == maxValue) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'thetaRange' with length 2 must contain minimum != maximum (", 
				minValue, " == ", maxValue , ")")
		}
		by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
		thetaRange <- seq(minValue, maxValue, by)
	}
	
	invisible(thetaRange)
}

.assertIsValidPiRange <- function(..., piRange, piAutoSeqEnabled = TRUE) {	
	if (length(piRange) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'piRange' must be a vector with two entries defining minimum and maximum ",
			"or a sequence of values with length > 2")
	}
	else if (length(piRange) == 2) {
		if (piAutoSeqEnabled) {
			minValue <- piRange[1]
			maxValue <- piRange[2]
			.assertIsValidPi(minValue, "piRange[1]")
			.assertIsValidPi(maxValue, "piRange[2]")
			if (minValue == maxValue) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piRange' with length 2 must contain minimum != maximum (", 
					minValue, " == ", maxValue , ")")
			}
			by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
			piRange <- seq(minValue, maxValue, by)
		}
	}
	
	invisible(piRange)
}

.assertIsValidPi <- function(piValue, piName) {
	if (is.null(piValue) || length(piValue) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", piName, "' must be a valid numeric value")
	}
	
	if (all(is.na(piValue))) {
		return(invisible())
	}
	
	if (!is.numeric(piValue) || any(is.na(piValue))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", piName, "' (", .arrayToString(piValue), ") must be a valid numeric value")
	}
	
	if (any(piValue <= 0) || any(piValue >= 1)) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
			"'", piName, "' (", .arrayToString(piValue), ") is out of bounds (0; 1)") 
	}
}

.assertIsValidPi1 <- function(pi1, stageResults = NULL, stage = NULL) {

	if (is.na(pi1) && !is.null(stageResults) && !is.null(stage)) {
		if (stageResults$isOneSampleDataset()) {
			pi1 <- stageResults$overallEvents[stage]/stageResults$overallSampleSizes[stage]
		} else {
			pi1 <- stageResults$overallEvents1[stage]/stageResults$overallSampleSizes1[stage]
		}
	}
	.assertIsInClosedInterval(pi1, "pi1", lower = 0, upper = 1)

	invisible(pi1)
}

.assertIsValidPi2 <- function(pi2, stageResults = NULL, stage = NULL) {
	
	if (is.na(pi2) && !is.null(stageResults) && !is.null(stage)) {
		pi2 <- stageResults$overallEvents2[stage]/stageResults$overallSampleSizes2[stage]
	}
	
	.assertIsInClosedInterval(pi2, "pi2", lower = 0, upper = 1)
	
	invisible(pi2)
}


.assertIsValidAllocationRatioPlanned <- function(allocationRatioPlanned, numberOfGroups) {
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
	
	.assertIsInClosedInterval(allocationRatioPlanned, "allocationRatioPlanned", lower = 0.001, upper = 1000)
	
	if (allocationRatioPlanned != C_ALLOCATION_RATIO_DEFAULT && numberOfGroups == 1) {
		warning("Planned allocation ratio ", allocationRatioPlanned, " will be ignored ",
			"because the dataset has only one group", call. = FALSE)
	}
}

.assertIsValidAllocationRatioPlannedSampleSize <- function(allocationRatioPlanned, maxNumberOfSubjects = NA_real_) {
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
	if (allocationRatioPlanned < 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'allocationRatioPlanned' (", allocationRatioPlanned, ") is not allowed to be negative")
	}
	if (length(maxNumberOfSubjects) > 0 && !is.na(maxNumberOfSubjects) && 
			maxNumberOfSubjects > 0 && allocationRatioPlanned == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"determination of optimal allocation ratio not possible ", 
			"if 'maxNumberOfSubjects' > 0, i.e., follow-up time should be calculated ",
			"(please specify an 'allocationRatioPlanned' > 0)")
	}
}

.assertIsValidThetaH1 <- function(thetaH1, stageResults = NULL, stage = NULL) {
	if (is.na(thetaH1) && !is.null(stageResults) && !is.null(stage)) {
		thetaH1 <- stageResults$effectSizes[stage]
	}

	.assertIsSingleNumber(thetaH1, "thetaH1")
	
	invisible(thetaH1)
}

.assertIsValidAssumedStDev <- function(assumedStDev, stageResults = NULL, stage = NULL) {
	if (is.na(assumedStDev) && !is.null(stageResults) && !is.null(stage)) {
		assumedStDev <- stageResults$overallStDevs[stage]
	}

	.assertIsSingleNumber(assumedStDev, "assumedStDev")
	
	if (assumedStDev <= 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'assumedStDev' (", assumedStDev, ") must be > 0")
	}
	
	invisible(assumedStDev)
}

.isValidValueOrVector <- function(x) {
	if (is.null(x) || length(x) == 0) {
		return(FALSE)
	}
	
	return(!any(is.na(x)))
}

.assertIsValidHazardRatio <- function(hazardRatio, thetaH0) {

	.assertIsNumericVector(hazardRatio, "hazardRatio")
	
	if (any(hazardRatio == thetaH0)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"alternative not correctly specified: ", 
				"each hazard ratio (", 
				.arrayToString(hazardRatio[1:min(length(hazardRatio), 10)]), 
				") must be unequal to 'thetaH0' (", thetaH0, ")")
	}
	
}


.assertIsValidHazardRatioVector <- function(hazardRatio) {
	.assertIsNumericVector(hazardRatio, "hazardRatio")
	if (any(hazardRatio <= 0)) {
		if (length(hazardRatio) == 1) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'hazardRatio' (", hazardRatio ,") must be > 0")
		} else {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "each 'hazardRatio' (", 
				.arrayToString(hazardRatio[1:min(length(hazardRatio), 10)]), 
				") must be > 0")
		}
	}
}

.assertIsValidDirectionUpper <- function(directionUpper, sided, objectType = c("power", "sampleSize")) {

	objectType <- match.arg(objectType)
	
	.assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = TRUE)
	
	if (objectType == "power") {
		if (sided == 1 && is.na(directionUpper)) {
			directionUpper <- TRUE
		}
		if (sided == 2 && !is.na(directionUpper)) {
			warning("'directionUpper' will be ignored because it ",
				"is not applicable for 'sided' = 2", call. = FALSE)
		}
	} else if (is.na(directionUpper)) {
		directionUpper <- TRUE
	}
	
	return(directionUpper)
}

.assertIsValidFunction <- function(fun, ..., funArgName = "fun", 
	expectedArguments = NULL, expectedFunction = NULL, 
	identical = FALSE, validateThreeDots = TRUE) {
	
	fCall = match.call(expand.dots = FALSE)
	
	if (is.null(expectedArguments) && is.null(expectedFunction)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'expectedArguments' or 'expectedFunction' must be not NULL")
	}
	
	if (!is.function(fun)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'", funArgName, "' must be a function")
	}
	
	functionName <- as.character(fCall$fun)
	if (is.null(functionName) || functionName == funArgName) {
		functionName <- "function"
	}
	
	argNames <- methods::formalArgs(fun)
	if (!is.null(expectedArguments)) {
		argNamesExpected <-expectedArguments
	} else {
		if (!is.function(expectedFunction)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"'expectedFunction' must be a function")
		}
		argNamesExpected <- methods::formalArgs(expectedFunction)
	}
	if (validateThreeDots) {
		if (!("..." %in% argNames)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'", funArgName, "' must contain the three-dots argument '...', e.g., ",
				funArgName, " = ", functionName, "(", .arrayToString(argNames), ", ...)")
		}
	}
	argNames <- argNames[argNames != "..."]
	argNamesExpected <- argNamesExpected[argNamesExpected != "..."]
	
	for (argName in argNames) {
		if (argName != "..." && !(argName %in% argNamesExpected)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"the argument '", argName, "' in '", funArgName, 
				"' (", functionName, ") is not allowed. ",
				"Use one or more of the following arguments: ", .arrayToString(argNamesExpected))
		}
	}
	
	if (identical) {
		for (argNameExpected in argNamesExpected) {
			if (argNameExpected != "..." && !(argNameExpected %in% argNames)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'", funArgName, "' (", functionName, ") must contain ",
					"an argument with name '", argNameExpected, "'")
			}
		}
		return(invisible())
	}
	
	counter <- 0
	unusedArgs <- c()
	for (argNameExpected in argNamesExpected) {
		if (argNameExpected %in% argNames) {
			counter <- counter + 1
		} else {
			unusedArgs <- c(unusedArgs, argNameExpected)
		}
	}
	
	if (counter == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", funArgName, "' (", functionName, ") must contain at ",
			"least one of the following arguments: ", 
			.arrayToString(argNamesExpected))
	}
	
	if (length(unusedArgs) > 0) {
		warning("Note that the following arguments can optionally be used in '", 
			funArgName, "' (", functionName, "): \n", 
			.arrayToString(unusedArgs), call. = FALSE)
	}
}

.assertIsValidMinNumberOfSubjectsPerStage <- function(
		parameterValues, parameterName, plannedSubjects, conditionalPower, kMax) {
		
	if (kMax == 1 || is.na(conditionalPower)) {
		return(invisible(NA_real_))
	}
	
	.assertIsNumericVector(parameterValues, parameterName, naAllowed = TRUE)
	
	if (length(parameterValues) == 0 || (length(parameterValues) == 1 && is.na(parameterValues))) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
			"'", parameterName, "' must be defined ",
			"because 'conditionalPower' is defined")
	}
	
	if (length(parameterValues) != kMax) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", parameterName, "' (", 
			.arrayToString(parameterValues), ") must have length ", kMax)
	}
	
	if (any(is.na(parameterValues[2:length(parameterValues)]))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", parameterName, "' (", 
			.arrayToString(parameterValues), ") must contain valid numeric values")
	}
	
	if (!is.na(parameterValues[1]) && parameterValues[1] != plannedSubjects[1]) {
		warning("First value of '", parameterName, "' (", parameterValues[1], ") will be ignored")
	}
	
	parameterValues[1] <- plannedSubjects[1]
	
	.assertIsInClosedInterval(parameterValues, parameterName, lower = 1, upper = NULL)
	
	return(invisible(parameterValues))
}

