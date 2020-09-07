#:#
#:#  *Core assertions*
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
#:#  File version: $Revision: 3585 $
#:#  Last changed: $Date: 2020-09-03 15:27:08 +0200 (Do, 03 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

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

.isTrialDesignConditionalDunnett <- function(design) {
	return(class(design) == C_CLASS_NAME_TRIAL_DESIGN_CONDITIONAL_DUNNETT)
}

.isTrialDesignInverseNormalOrGroupSequential <- function(design) {
	return(.isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design))
}

.isTrialDesignInverseNormalOrFisher <- function(design) {
	return(.isTrialDesignInverseNormal(design) || .isTrialDesignFisher(design))
}

.isTrialDesign <- function(design) {
	return(.isTrialDesignInverseNormal(design) || .isTrialDesignGroupSequential(design) || 
			.isTrialDesignFisher(design) || .isTrialDesignConditionalDunnett(design))
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

.assertIsTrialDesignConditionalDunnett <- function(design) {
	if (!.isTrialDesignConditionalDunnett(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'design' must be an instance of class 'TrialDesignConditionalDunnett' (is '", class(design), "')")
	}
}

.assertIsTrialDesignInverseNormalOrGroupSequential <- function(design) {
	if (!.isTrialDesignInverseNormalOrGroupSequential(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignInverseNormal' or 'TrialDesignGroupSequential' (is '", 
			class(design), "')")
	}
}

.assertIsTrialDesignInverseNormalOrFisher <- function(design) {
	if (!.isTrialDesignInverseNormalOrFisher(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignInverseNormal' or 'TrialDesignFisher' (is '", 
			class(design), "')")
	}
}

.assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett <- function(design) {
	if (!.isTrialDesignInverseNormalOrFisher(design) && !.isTrialDesignConditionalDunnett(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignInverseNormal', ",
			"'TrialDesignFisher', or 'TrialDesignConditionalDunnett' (is '", 
			class(design), "')")
	}
}

.assertIsTrialDesignInverseNormalOrGroupSequentialOrFisher <- function(design) {
	if (!.isTrialDesignInverseNormalOrGroupSequential(design) && !.isTrialDesignFisher(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'design' must be an instance of class 'TrialDesignInverseNormal', 'TrialDesignGroupSequential', ",
			"or 'TrialDesignFisher' (is '", class(design), "')")
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
	return(.isStageResultsMeans(stageResults) || 
			.isStageResultsRates(stageResults) || 
			.isStageResultsSurvival(stageResults) ||
			.isStageResultsMeansMultiArm(stageResults) || 
			.isStageResultsRatesMultiArm(stageResults) || 
			.isStageResultsSurvivalMultiArm(stageResults))
}

.isStageResultsMeans <- function(stageResults) {
	return(class(stageResults) == "StageResultsMeans")
}

.isStageResultsMeansMultiArm <- function(stageResults) {
	return(class(stageResults) == "StageResultsMultiArmMeans")
}

.isStageResultsRates <- function(stageResults) {
	return(class(stageResults) == "StageResultsRates")
}

.isStageResultsRatesMultiArm <- function(stageResults) {
	return(class(stageResults) == "StageResultsMultiArmRates")
}
.isStageResultsSurvival <- function(stageResults) {
	return(class(stageResults) == "StageResultsSurvival")
}

.isStageResultsSurvivalMultiArm <- function(stageResults) {
	return(class(stageResults) == "StageResultsMultiArmSurvival")
}

.assertIsStageResults <- function(stageResults) {
	if (!.isStageResults(stageResults)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stageResults' must be an instance of ", 
			.arrayToString(.getStageResultsClassNames(), vectorLookAndFeelEnabled = FALSE), 
			" (is '", class(stageResults), "')")
	}
}

.assertIsInClosedInterval <- function(x, xName, ..., lower, upper, naAllowed = FALSE) {
	.warnInCaseOfUnknownArguments(functionName = ".assertIsInClosedInterval", ...)
	if (naAllowed && all(is.na(x))) {
		return(invisible())
	}
	
	if (!naAllowed && length(x) > 1 && any(is.na(x))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", xName, "' (", .arrayToString(x), ") must be a valid numeric vector or a single NA")
	}
	
	if (is.null(upper) || is.na(upper)) {
		if (any(x < lower, na.rm = TRUE)) {
			prefix <- ifelse(length(x) > 1, "each value of ", "")
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, prefix,
				"'", xName, "' (", .arrayToString(x), ") must be >= ", lower)
		}
	}
	else if (any(x < lower, na.rm = TRUE) || any(x > upper, na.rm = TRUE)) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
			"'", xName, "' (", .arrayToString(x), ") is out of bounds [", lower, "; ", upper, "]")
	}
}

.assertIsInOpenInterval <- function(x, xName, lower, upper, naAllowed = FALSE) {
	if (naAllowed && all(is.na(x))) {
		return(invisible())
	}
	
	if (!naAllowed && length(x) > 1 && any(is.na(x))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", xName, "' (", .arrayToString(x), ") must be a valid numeric vector or a single NA")
	}
	
	if (is.null(upper) || is.na(upper)) {
		if (any(x <= lower, na.rm = TRUE)) {
			prefix <- ifelse(length(x) > 1, "each value of ", "")
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, prefix,
				"'", xName, "' (", .arrayToString(x), ") must be > ", lower)
		}
	}
	else if (any(x <= lower, na.rm = TRUE) || any(x >= upper, na.rm = TRUE)) {
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
				if (any(na.omit(dataInput$getStDevsUpTo(stage)) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all standard deviations must be > 0")
				} 
				if (any(na.omit(dataInput$getSampleSizesUpTo(stage)) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				} 
			} 
			else if (.isDatasetRates(dataInput) ) {
				if (any(na.omit(dataInput$getEventsUpTo(stage)) < 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be >= 0")
				}
				if (any(na.omit(dataInput$getSampleSizesUpTo(stage)) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				}
				if (any(na.omit(dataInput$getEventsUpTo(stage)) > na.omit(dataInput$getSampleSizesUpTo(stage)))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be <= corresponding sample size")
				}
			}
		}
		else if (dataInput$getNumberOfGroups() == 2) {
			if (.isDatasetMeans(dataInput) ) {
				if (any(na.omit(dataInput$getStDevsUpTo(stage, 1)) <= 0) || 
						any(na.omit(dataInput$getStDevsUpTo(stage, 2)) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all standard deviations must be > 0")
				} 
				if (any(na.omit(dataInput$getSampleSizesUpTo(stage, 1)) <= 0) || 
						any(na.omit(dataInput$getSampleSizesUpTo(stage, 2)) <= 0)	) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				} 
			} 
			else if (.isDatasetRates(dataInput) ) {
				if (any(na.omit(dataInput$getEventsUpTo(stage, 1)) < 0) || 
						any(na.omit(dataInput$getEventsUpTo(stage, 2)) < 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be >= 0")
				}
				if (any(na.omit(dataInput$getSampleSizesUpTo(stage, 1)) <= 0)  || 
						any(na.omit(dataInput$getSampleSizesUpTo(stage, 2)) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all sample sizes must be > 0")
				}
				if (any(na.omit(dataInput$getEventsUpTo(stage, 1)) > na.omit(dataInput$getSampleSizesUpTo(stage, 1))) ||  
						any(na.omit(dataInput$getEventsUpTo(stage, 2)) > na.omit(dataInput$getSampleSizesUpTo(stage, 2)))) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all events must be <= corresponding sample size")
				}
			}
		}
		
		if (.isDatasetSurvival(dataInput) ) {
			if (any(na.omit(dataInput$getOverallEventsUpTo(stage)) < 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT, "all overall events must be >= 0")
			}
			
			if (any(na.omit(dataInput$getOverallAllocationRatiosUpTo(stage)) <= 0)) {
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

.assertIsNumericVector <- function(x, argumentName, naAllowed = FALSE, noDefaultAvailable = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, 
			"' must be a valid numeric value or vector")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if ((!naAllowed && any(is.na(x))) || !is.numeric(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(x), ") must be a valid numeric value or vector")
	}
}

.assertIsIntegerVector <- function(x, argumentName, naAllowed = FALSE, validateType = TRUE, noDefaultAvailable = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)		
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, 
			"' must be a valid integer value or vector")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if (naAllowed && all(is.na(x))) {
		return(invisible())
	}
	
	if (!is.numeric(x) || (!naAllowed && any(is.na(x))) || (validateType && !is.integer(x)) || 
			(!validateType && any(as.integer(na.omit(x)) != na.omit(x)))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(x), ") must be a valid integer value or vector")
	}
}

.assertIsLogicalVector <- function(x, argumentName, naAllowed = FALSE, noDefaultAvailable = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)		
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid logical value or vector")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if ((!naAllowed && all(is.na(x))) || !is.logical(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", x, ") must be a valid logical value or vector")
	}
}

.assertIsNoDefault <- function(x, argumentName, noDefaultAvailable, checkNA = FALSE) {
	if (noDefaultAvailable && (!checkNA || all(is.na(x)))) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be specified, there is no default value")
	}
}

.assertIsSingleLogical <- function(x, argumentName, naAllowed = FALSE, noDefaultAvailable = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)		
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a single logical value")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single logical value")
	}
	
	if ((!naAllowed && is.na(x)) || !is.logical(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			ifelse(isS4(x), class(x), x), ") must be a single logical value")
	}
}

.assertIsSingleNumber <- function(x, argumentName, naAllowed = FALSE, noDefaultAvailable = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)			
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid numeric value")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single numeric value")
	}
	
	if ((!naAllowed && is.na(x)) || !is.numeric(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			ifelse(isS4(x), class(x), x), ") must be a valid numeric value")
	}
}

.assertIsSingleInteger <- function(x, argumentName, naAllowed = FALSE, 
		validateType = TRUE, noDefaultAvailable = FALSE) {
	.assertIsSinglePositiveInteger(x = x, argumentName = argumentName, 
		naAllowed = naAllowed, validateType = validateType, 
		mustBePositive = FALSE, noDefaultAvailable = noDefaultAvailable)
}

.assertIsSinglePositiveInteger <- function(x, argumentName, ..., 
		naAllowed = FALSE, validateType = TRUE, mustBePositive = TRUE, noDefaultAvailable = FALSE) {
		
	prefix <- ifelse(mustBePositive, "single positive ", "single ")
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)		
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
			"'", argumentName, "' must be a ", prefix, "integer value")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), 
			" must be a ", prefix, "integer value")
	}
	
	if (!is.numeric(x) || (!naAllowed && is.na(x)) || (validateType && !is.integer(x)) || 
			(!validateType && !is.na(x) && as.integer(x) != x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", argumentName, "' (", ifelse(isS4(x), class(x), x), ") must be a ", prefix, "integer value")
	}
	
	if (mustBePositive && !is.na(x) && x <= 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'", argumentName, "' (", ifelse(isS4(x), class(x), x), ") must be a ", prefix, "integer value")
	}
}

.assertIsSingleCharacter <- function(x, argumentName, naAllowed = FALSE, noDefaultAvailable = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = FALSE)		
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid character value")
	}
	
	.assertIsNoDefault(x, argumentName, noDefaultAvailable, checkNA = TRUE)
	
	if (length(x) > 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' ", 
			.arrayToString(x, vectorLookAndFeelEnabled = TRUE), " must be a single character value")
	}
	
	if (!is.character(x)) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'%s' must be a valid character value (is an instance of class '%s')"), argumentName, class(x)))
	}
	
	if (!naAllowed && is.na(x)) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'%s' (NA) must be a valid character value"), argumentName))
	}
}

.assertIsCharacter <- function(x, argumentName, naAllowed = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
			"'", argumentName, "' must be a valid character value or vector")
	}
	
	if (!all(is.character(x))) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'%s' must be a valid character value or vector ",
			"(is an instance of class '%s')"), argumentName, class(x)))
	}
	
	if (!naAllowed && any(is.na(x))) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'%s' (%s) must be a valid character value (NA is not allowed)"), 
			argumentName, .arrayToString(x)))
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
		design$.setParameterType(parameterName, C_PARAM_USER_DEFINED)
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
	
	.assertValuesAreInsideBounds("informationRates", informationRates, 
		0, 1, lowerBoundInclusive = FALSE)
	
	if (min(informationRates) <= 0 || max(informationRates) > 1 || 
	any(informationRates[2:kMax] <= informationRates[1:(kMax - 1)])) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'informationRates' (%s) ", 
			"must be strictly increasing: 0 < x_1 < .. < x_%s <= 1"), 
			.arrayToString(informationRates, vectorLookAndFeelEnabled = FALSE), kMax))
	}
}

.assertValuesAreInsideBounds <- function(parameterName, values, lowerBound, upperBound,
		lowerBoundInclusive = TRUE, upperBoundInclusive = TRUE) {
	lower <- min(values)
	upper <- max(values)
	lowerInvalid <- ifelse(lowerBoundInclusive, lower < lowerBound, lower <= lowerBound)
	upperInvalid <- ifelse(upperBoundInclusive, upper > upperBound, upper >= upperBound)
	if (!is.na(lowerInvalid)) {
		if (lowerInvalid || upperInvalid) {
			stop(sprintf(paste0(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
					"'%s' (%s) is out of bounds %s%s; %s%s"), 
					parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE),
					ifelse(lowerBoundInclusive, "[", "("), lowerBound,
					upperBound, ifelse(upperBoundInclusive, "]", ")")))
		}
	}	
}

.assertContainsNoNas <- function(values, parameterName) {
	if (any(is.na(values))) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ", 
			"must contain valid numeric values (NA is not allowed)"), 
			parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE)))
	}
}

.assertContainsOnlyNasAtTheEnd <- function(values, parameterName) {
	if (length(values) <= 1) {
		return(invisible())
	}
	
	for (i in length(values):2) {
		if (!is.na(values[i]) && is.na(values[i - 1])) {
			stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ", 
				"must contain valid numeric values (NAs are only allowed at the end of the vector)"), 
				parameterName, .arrayToString(values, vectorLookAndFeelEnabled = FALSE)))
		}
	}
}

.assertValuesAreStrictlyIncreasing <- function(values, parameterName, endingNasAllowed = FALSE) {
	len <- length(values)
	if (len <= 1) {
		return(invisible())
	}
	
	if (!endingNasAllowed) {
		.assertContainsNoNas(values, parameterName)
	}
	
	.assertContainsOnlyNasAtTheEnd(values, parameterName)
	
	valuesTemp <- values
	values <- na.omit(values)
	len <- length(values)
	if (len <= 1) {
		return(invisible())
	}
	
	if (any(values[2:len] <= values[1:(len - 1)])) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ", 
			"must be strictly increasing: x_1 < .. < x_%s"), 
			parameterName, .arrayToString(valuesTemp, vectorLookAndFeelEnabled = FALSE), len))
	}
}

.assertValuesAreMonotoneIncreasing <- function(values, parameterName, endingNasAllowed = FALSE) { 
	len <- length(values)
	if (len <= 1) {
		return(invisible())
	}
	
	if (!endingNasAllowed) {
		.assertContainsNoNas(values, parameterName)
	}
	
	.assertContainsOnlyNasAtTheEnd(values, parameterName)
	
	valuesTemp <- values
	values <- na.omit(values)
	len <- length(values)
	if (len <= 1) {
		return(invisible())
	}
	
	if (any(values[2:len] < values[1:(len - 1)])) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'%s' (%s) ", 
			"must be increasing: x_1 <= .. <= x_%s"), 
			parameterName, .arrayToString(valuesTemp, vectorLookAndFeelEnabled = FALSE), len))
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
	
	.assertValuesAreInsideBounds("futilityBounds", futilityBounds, -6, 6)
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
	
	.assertValuesAreInsideBounds("alpha0Vec", alpha0Vec, 0, 1, lowerBoundInclusive = FALSE)
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
	args <- NULL
	tryCatch(expr = {	
		args <- list(...)
	}, error = function(e) {
		stop(simpleError(paste0(C_EXCEPTION_TYPE_MISSING_ARGUMENT, e$message), call = e$call))
	})
	
	if (.allArgumentsAreNotNull(...)) {
		return(invisible(TRUE))
	}
	
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
			" ", ifelse(warningOnlyEnabled, "should", "must"), 
			" be defined because ", .arrayToString(definedArguments, encapsulate = TRUE),
			ifelse(length(definedArguments) > 1, " are", " is"), " defined")
		if (warningOnlyEnabled) {
			warning(C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS, message, call. = FALSE)
			return(FALSE)
		} else {
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, message)
		}
	}
	
	return(invisible(length(definedArguments) == length(args)))
}

.assertIsValidNPlanned <- function(nPlanned, kMax, stage, ..., required = TRUE) {
	if (is.null(nPlanned) || (length(nPlanned) > 0 && all(is.na(nPlanned)))) { 
		if (!required) {
			return(invisible())
		}
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'nPlanned' must be specified")
	}
	
	if (length(nPlanned) != kMax - stage) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			sprintf(paste0("'nPlanned' (%s) is invalid: ", 
				"length must be equal to %s (kMax - stage = %s - %s)"),
				.arrayToString(nPlanned), kMax - stage, kMax, stage))
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
				"length must be equal to %s (kMax - stage = %s - %s)"),
				.arrayToString(nPlanned), kMax - stage, kMax, stage), call. = FALSE)
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

.warnInCaseOfUnknownArguments <- function(..., functionName, ignore = c(), 
		numberOfAllowedUnnamedParameters = 0) {
	args <- list(...)
	if (length(args) == 0) {
		return(invisible())
	}
	
	if (numberOfAllowedUnnamedParameters > 0) {
		ignore <- c(ignore, paste0("%param", 1:numberOfAllowedUnnamedParameters, "%"))
	}
	ignore <- c(ignore, "showWarnings")
	argNames <- names(args)
	for (i in 1:length(args)) {
		arg <- args[[i]]
		argName <- ifelse(is.null(argNames[i]) || argNames[i] == "", 
			ifelse(inherits(arg, "StageResults"), "stageResultsName", paste0("%param", i, "%")), 
			argNames[i])
		if (!(argName %in% ignore) && !grepl("^\\.", argName)) {
			if (isS4(arg) || is.environment(arg)) {
				arg <- class(arg)
			}
			warning("Argument unknown in ", functionName, "(...): '", 
				argName, "' = ", arg, " will be ignored", call. = FALSE)
		}
	}
}

.warnInCaseOfUnusedArgument <- function(arg, argName, defaultValue, functionName) {
	if (!identical(arg, defaultValue)) {
		warning("Unused argument in ", functionName, "(...): '", 
			argName, "' = ", .arrayToString(arg, vectorLookAndFeelEnabled = (length(arg) > 1), maxLength = 10), 
			" will be ignored", call. = FALSE)
	}
}

.assertIsDefined <- function(parameter, parameterName) {
	if (is.null(parameter) || any(is.na(parameter))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", parameterName, "' must be defined")
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

.isTrialDesignWithValidAlpha0Vec <- function(design) {
	if (is.null(design) || !.isTrialDesignFisher(design)) {
		return(FALSE)
	}
	
	alpha0Vec <- design[["alpha0Vec"]]
	if (is.null(alpha0Vec)) {
		return(FALSE)
	}
	
	alpha0Vec <- na.omit(alpha0Vec)
	if (length(alpha0Vec) == 0 || all(is.na(alpha0Vec))) {
		return(FALSE)
	}
	
	return(sum(alpha0Vec == C_ALPHA_0_VEC_DEFAULT) == 0)
}

.assertPackageIsInstalled <- function(packageName) {
	if (!requireNamespace(packageName, quietly = TRUE)) {
		stop("Package \"", packageName, "\" is needed for this function to work. ", 
			"Please install and load it", call. = FALSE)
	}
}

.assertGgplotIsInstalled <- function() {
	.assertPackageIsInstalled("ggplot2")
}

.assertRcppIsInstalled <- function() {
	.assertPackageIsInstalled("Rcpp")
}

.assertTestthatIsInstalled <- function() {
	.assertPackageIsInstalled("testthat")
}

.assertMnormtIsInstalled <- function() {
	.assertPackageIsInstalled("mnormt")
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

.assertIsValidThetaRange <- function(..., thetaRange, thetaAutoSeqEnabled = TRUE, survivalDataEnabled = FALSE) {	
	if (is.null(thetaRange) || (thetaAutoSeqEnabled && length(thetaRange) <= 1) || 
			any(is.na(thetaRange))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'thetaRange' (", .arrayToString(thetaRange), ") must be a vector ", 
			"with two entries defining minimum and maximum ",
			"or a sequence of numeric values with length > 2")
	}
	else if (length(thetaRange) == 2 && thetaAutoSeqEnabled) {
		minValue <- thetaRange[1]
		maxValue <- thetaRange[2]
		if (survivalDataEnabled) {
			.assertIsValidHazardRatio(minValue, "thetaRange[1]")
			.assertIsValidHazardRatio(maxValue, "thetaRange[2]")
		}
		if (minValue >= maxValue) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'thetaRange' with length 2 must contain minimum < maximum (", 
				minValue, " >= ", maxValue , ")")
		}
		by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
		thetaRange <- seq(minValue, maxValue, by)
	}
	
	invisible(thetaRange)
}

.assertIsValidPiTreatmentRange <- function(..., piTreatmentRange, piAutoSeqEnabled = TRUE) {	
	if (is.null(piTreatmentRange) || (piAutoSeqEnabled && length(piTreatmentRange) <= 1) || 
			any(is.na(piTreatmentRange))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'piTreatmentRange' (", .arrayToString(piTreatmentRange), ") must be a vector ", 
			"with two entries defining minimum and maximum ",
			"or a sequence of numeric values with length > 2")
	}
	else if (length(piTreatmentRange) == 2) {
		if (piAutoSeqEnabled) {
			minValue <- piTreatmentRange[1]
			maxValue <- piTreatmentRange[2]
			if (minValue == 0) {
				minValue <- 0.00000001
			}
			if (maxValue == 1) {
				maxValue <- 0.99999999
			}
			.assertIsValidPi(minValue, "piTreatmentRange[1]")
			.assertIsValidPi(maxValue, "piTreatmentRange[2]")
			if (minValue >= maxValue) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'piTreatmentRange' with length 2 must contain minimum < maximum (", 
					minValue, " >= ", maxValue , ")")
			}
			by <- (maxValue - minValue) / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT
			piTreatmentRange <- seq(minValue, maxValue, by)
		}
	}
	
	invisible(piTreatmentRange)
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
	
	if (any(piValue <= -1e-16) || any(piValue >= 1 + 1e-16)) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
			"'", piName, "' (", .arrayToString(piValue), ") is out of bounds (0; 1) or event time too long") 
	}
}

.assertIsValidPi1 <- function(pi1, stageResults = NULL, stage = NULL) {
	if (is.na(pi1) && !is.null(stageResults) && !is.null(stage)) {
		if (stageResults$isOneSampleDataset()) {
			pi1 <- stageResults$overallEvents[stage] / stageResults$overallSampleSizes[stage]
		} else {
			pi1 <- stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage]
		}
	}
	.assertIsInClosedInterval(pi1, "pi1", lower = 0, upper = 1)
	invisible(pi1)
}

.assertIsValidPi2 <- function(pi2, stageResults = NULL, stage = NULL) {
	if (is.na(pi2) && !is.null(stageResults) && !is.null(stage)) {
		pi2 <- stageResults$overallEvents2[stage] / stageResults$overallSampleSizes2[stage]
	}
	.assertIsInClosedInterval(pi2, "pi2", lower = 0, upper = 1)
	invisible(pi2)
}

.assertIsValidAllocationRatioPlanned <- function(allocationRatioPlanned, numberOfGroups) {
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
	.assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
	if (allocationRatioPlanned != C_ALLOCATION_RATIO_DEFAULT && numberOfGroups == 1) {
		warning("Planned allocation ratio ", allocationRatioPlanned, " will be ignored ",
			"because the dataset has only one group", call. = FALSE)
	}
}

.assertIsValidAllocationRatioPlannedSampleSize <- function(allocationRatioPlanned, maxNumberOfSubjects = NA_real_) {
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
	if (allocationRatioPlanned < 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'allocationRatioPlanned' (", allocationRatioPlanned, ") must be >= 0")
	}
	if (length(maxNumberOfSubjects) > 0 && !is.na(maxNumberOfSubjects) && 
			maxNumberOfSubjects > 0 && allocationRatioPlanned == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"determination of optimal allocation ratio not possible ", 
			"if explicit or implicit 'maxNumberOfSubjects' (", maxNumberOfSubjects, 
			") > 0, i.e., follow-up time should be calculated ",
			"(please specify an 'allocationRatioPlanned' > 0)")
	}
}

.assertIsValidThetaH1 <- function(thetaH1, stageResults = NULL, stage = NULL, ..., results = NULL) {
	if (is.na(thetaH1) && !is.null(stageResults) && !is.null(stage)) {
		thetaH1 <- stageResults$effectSizes[stage]
		if (!is.null(results)) {
			results$.setParameterType("thetaH1", C_PARAM_GENERATED)
		}
	}
	.assertIsSingleNumber(thetaH1, "thetaH1")
	invisible(thetaH1)
}

.assertIsValidAssumedStDev <- function(assumedStDev, stageResults = NULL, stage = NULL, ..., results = NULL) {
	if (is.na(assumedStDev) && !is.null(stageResults) && !is.null(stage)) {
		assumedStDev <- stageResults$overallStDevs[stage]
		if (!is.null(results)) {
			results$.setParameterType("assumedStDev", C_PARAM_GENERATED)
		}
	}

	.assertIsSingleNumber(assumedStDev, "assumedStDev")
	
	if (assumedStDev <= 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'assumedStDev' (", assumedStDev, ") must be > 0")
	}
	
	invisible(assumedStDev)
}

.assertIsValidThetaH1ForMultiArm <- function(thetaH1, stageResults = NULL, stage = NULL, ..., results = NULL) {
	if (!is.null(stageResults) && all(is.na(thetaH1)) && !is.null(stage)) {
		thetaH1 <- stageResults$effectSizes[, stage]
		if (!is.null(results)) {
			results$.setParameterType("thetaH1", C_PARAM_GENERATED)
		}
	}
		
	.assertIsNumericVector(thetaH1, "thetaH1", naAllowed = TRUE)
	
	invisible(thetaH1)
}

.assertIsValidAssumedStDevForMultiArm <- function(assumedStDev, stageResults = NULL, stage = NULL, ..., results = NULL) {
	if (!is.null(stageResults) && all(is.na(assumedStDev)) && !is.null(stage)) {
		assumedStDev <- stageResults$overallStDevs[, stage]
		if (!is.null(results)) {
			results$.setParameterType("assumedStDevs", C_PARAM_GENERATED)
		}
	}
	
	.assertIsNumericVector(assumedStDev, "assumedStDev", naAllowed = TRUE)
	
	if (any(assumedStDev <= 0, na.rm = TRUE)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"'assumedStDev' (", .arrayToString(assumedStDev), ") must be > 0")
	}
	
	invisible(assumedStDev)
}

.assertIsValidPiTreatmentsForMultiArm <- function(piTreatments, stageResults = NULL, stage = NULL, ..., results = NULL) {
	if (!is.null(stageResults) && all(is.na(piTreatments)) && !is.null(stage)) {
		piTreatments <- stageResults$piTreatments[, stage]
		if (!is.null(results)) {
			results$.setParameterType("piTreatments", C_PARAM_GENERATED)
		}
	}
	.assertIsNumericVector(piTreatments, "piTreatments", naAllowed = TRUE)
	.assertIsInClosedInterval(piTreatments, "piTreatments", lower = 0, upper = 1, naAllowed = TRUE)
	invisible(piTreatments)
}

.assertIsValidPiControlForMultiArm <- function(piControl, stageResults = NULL, stage = NULL, ..., results = NULL) {
	if (is.na(piControl) && !is.null(stageResults) && !is.null(stage)) {
		piControl <- stageResults$piControl[,stage]
		if (!is.null(results)) {
			results$.setParameterType("piControl", C_PARAM_GENERATED)
		}
	}
	.assertIsNumericVector(piControl, "piControl", naAllowed = TRUE)
	.assertIsInClosedInterval(piControl, "piControl", lower = 0, upper = 1)
	invisible(piControl)
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

.assertIsValidDirectionUpper <- function(directionUpper, sided, 
		objectType = c("power", "sampleSize"), userFunctionCallEnabled = FALSE) {
		
	objectType <- match.arg(objectType)
	
	.assertIsSingleLogical(directionUpper, "directionUpper", naAllowed = TRUE)
	
	if (objectType == "power") {
		if (sided == 1 && is.na(directionUpper)) {
			directionUpper <- TRUE
		}
		if (userFunctionCallEnabled && sided == 2 && !is.na(directionUpper)) {
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
		identical = FALSE, validateThreeDots = TRUE, showUnusedArgumentsMessage = FALSE) {
	
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
		argNamesExpected <- expectedArguments
	} else if (!is.null(expectedFunction)) {
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
	
	if (length(argNamesExpected) <= 1 && length(argNames) == length(argNamesExpected)) {
		return(invisible())
	}
	
	for (argName in argNames) {
		if (argName != "..." && !(argName %in% argNamesExpected)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"the argument '", argName, "' in '", funArgName, 
				"' (", functionName, ") is not allowed.\n",
				"Use one or more than one of the following arguments:\n ", .arrayToString(argNamesExpected))
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
	
	if (showUnusedArgumentsMessage && length(unusedArgs) > 0) {
		message("Note that the following arguments can optionally be used in '", 
			funArgName, "' (", functionName, "): \n", 
			.arrayToString(unusedArgs), call. = FALSE)
	}
}

.assertIsValidThreshold <- function(threshold, activeArms) {
	.assertIsNumericVector(threshold, "threshold", naAllowed = TRUE)
	if ((length(threshold) != 1) && (length(threshold) != activeArms)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'threshold' (", .arrayToString(threshold), 
				") must be a single value or a vector of length ", activeArms)
	}
}

.assertIsValidNumberOfSubjectsPerStage <- function(
		parameterValues, parameterName, plannedSubjects, 
		conditionalPower, calcSubjectsFunction, kMax, 
		endpoint = c("means", "rates", "survival"), calcSubjectsFunctionEnabled = TRUE) {
		
	endpoint <- match.arg(endpoint)
		
	if (kMax == 1) {
		.ignoreParameterIfNotUsed("conditionalPower", 
			conditionalPower, kMax > 1, "design is fixed ('kMax' = 1)")
		return(invisible(NA_real_))
	}

	.assertIsNumericVector(parameterValues, parameterName, naAllowed = TRUE)
	
	calcSubjectsFunctionName <- ifelse(endpoint == "survival", "calcEventsFunction", "calcSubjectsFunction")
	
	if (is.na(conditionalPower) && is.null(calcSubjectsFunction)) {
		if (length(parameterValues) != 1 || !is.na(parameterValues)) {
			if (calcSubjectsFunctionEnabled) {
				warning("'", parameterName, "' (", .arrayToString(parameterValues), ") ",
					"will be ignored because neither 'conditionalPower' nor '", 
					calcSubjectsFunctionName, "' is defined", call. = FALSE)
			} else {
				warning("'", parameterName, "' (", .arrayToString(parameterValues), ") ",
					"will be ignored because 'conditionalPower' is not defined", call. = FALSE)
			}
		}
		return(invisible(NA_real_))
	}
	
	if (!is.na(conditionalPower) && length(parameterValues) == 0 || 
			(length(parameterValues) == 1 && is.na(parameterValues))) {
		if (calcSubjectsFunctionEnabled) {
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
				"'", parameterName, "' must be defined ",
				"because 'conditionalPower' or '", calcSubjectsFunctionName, "' is defined")
		} else {
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
				"'", parameterName, "' must be defined ",
				"because 'conditionalPower' is defined")
		}
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
		warning("First value of '", parameterName, "' (", parameterValues[1], ") will be ignored", call. = FALSE)
	}
	
	parameterValues[1] <- plannedSubjects[1]
	
	.assertIsInClosedInterval(parameterValues, parameterName, lower = 1, upper = NULL)
	
	return(invisible(parameterValues))
}

.assertIsValidMaxNumberOfSubjects <- function(maxNumberOfSubjects, naAllowed = FALSE) {
	.assertIsSingleNumber(maxNumberOfSubjects, "maxNumberOfSubjects", naAllowed = naAllowed)
	.assertIsInClosedInterval(maxNumberOfSubjects, "maxNumberOfSubjects", lower = 1, upper = NULL, naAllowed = naAllowed)
}

.assertAreSuitableInformationRates <- function(design, dataInput, stage) {
	
	if (!.isTrialDesignGroupSequential(design) || stage == 1) {
		return(invisible())
	}

	param <- NA_character_
	if (dataInput$isDatasetSurvival()) {
		if (any(abs(design$informationRates[2:stage] - dataInput$getOverallEventsUpTo(stage)[2:stage] /
				dataInput$getOverallEventsUpTo(1) * design$informationRates[1]) > 
				C_ACCEPT_DEVIATION_INFORMATIONRATES)) {
			param <- "events"
		}
	} else {
		if (dataInput$getNumberOfGroups() == 1) {	
			if (any(abs(design$informationRates[2:stage] - 
					dataInput$getOverallSampleSizesUpTo(stage)[2:stage] /
					dataInput$getOverallSampleSizesUpTo(1) * design$informationRates[1]) > 
					C_ACCEPT_DEVIATION_INFORMATIONRATES)) {
				param <- "sample sizes"
			}
		}
		else if (dataInput$getNumberOfGroups() == 2) {	
			if (any(abs(design$informationRates[2:stage] - 
						dataInput$getOverallSampleSizesUpTo(stage)[2:stage] /
						dataInput$getOverallSampleSizesUpTo(1) * design$informationRates[1]) > 
						C_ACCEPT_DEVIATION_INFORMATIONRATES) ||
					any(abs(design$informationRates[2:stage] - 
						dataInput$getOverallSampleSizesUpTo(stage,2)[2:stage] / 
						dataInput$getOverallSampleSizesUpTo(1,2)*design$informationRates[1]) > 
						C_ACCEPT_DEVIATION_INFORMATIONRATES)) {
				param <- "sample sizes"
			}
		}
	}
	if (!is.na(param)) {
		warning("Observed ", param, " not according to specified information rates in ", 
			"group sequential design. ", 
			"Test procedure might not control Type I error rate", call. = FALSE)
	}
}

.isMultiArmDataset <- function(dataInput) {
	return(inherits(dataInput, "Dataset") && dataInput$getNumberOfGroups() > 2)
}

.isMultiArmStageResults <- function(stageResults) {
	return(grepl("MultiArm", class(stageResults)))
}

.isMultiArmAnalysisResults <- function(analysisResults) {
	return(inherits(analysisResults, "AnalysisResultsMultiArm"))
}

.isMultiArmSimulationResults <- function(simulationResults) {
	return(inherits(simulationResults, "SimulationResults") && grepl("MultiArm", class(simulationResults)))
}

.assertIsStageResultsMultiArm <- function(stageResults) {
	if (!inherits(stageResults, "StageResults")) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'stageResults' must be a multi-arm stage results object (is ", class(stageResults), ")")
	}
	
	if (!.isMultiArmStageResults(stageResults)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'stageResults' must be a multi-arm object (is ", class(stageResults), ")")
	}
}

.assertIsStageResultsNonMultiArm <- function(stageResults) {
	if (inherits(stageResults, "StageResults") && .isMultiArmStageResults(stageResults)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'stageResults' must be a non-multi-arm object (is ", class(stageResults), ")")
	}
	
	allowedClasses <- c(
		"StageResultsMeans", 
		"StageResultsRates",
		"StageResultsSurvival")
	if (!(class(stageResults) %in% allowedClasses)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stageResults' must be an instance of ", 
			.arrayToString(allowedClasses, vectorLookAndFeelEnabled = FALSE), 
			" (is '", class(stageResults), "')")
	}
}

.assertIsDatasetNonMultiArm <- function(dataInput) {
	if (.isMultiArmDataset(dataInput)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'dataInput' must be a non-multi-arm dataset (has ", dataInput$getNumberOfGroups(), " treatment arms)")
	}
}

.assertIsAnalysisResults <- function(analysisResults) {
	if (!inherits(analysisResults, "AnalysisResults")) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'analysisResults' must be a valid 'AnalysisResults' object ", 
			" (is '", class(analysisResults), "')")
	}
}

.isValidIntersectionTest <- function(intersectionTest) {
	return(!is.null(intersectionTest) && length(intersectionTest) == 1 && !is.na(intersectionTest) && 
		is.character(intersectionTest) && intersectionTest %in% C_INTERSECTION_TESTS)
}

.getCorrectedIntersectionTestIfNecessary <- function(design, intersectionTest, userFunctionCallEnabled = TRUE) {
	.assertIsCharacter(intersectionTest, "intersectionTest")
	intersectionTest <- intersectionTest[1]
	if (.isTrialDesignConditionalDunnett(design) && intersectionTest != "Dunnett") {
		if (userFunctionCallEnabled) {
			message <- paste0("Intersection test '", intersectionTest, "' ")
			if (!.isValidIntersectionTest(intersectionTest)) {
				message <- paste0(message, "is invalid, ")
			}
			message <- paste0(message, "will be ignored")
			message <- paste0(message, ifelse(!.isValidIntersectionTest(intersectionTest), ", ", " "))
			message <- paste0(message, "and 'Dunnett' will be used instead ",
				"because conditional Dunnett test was specified as design")
			warning(message, call. = FALSE)
		}
		intersectionTest <- "Dunnett"
	}
	return(intersectionTest)
}

.assertIsValidIntersectionTest <- function(design, intersectionTest) {
	.assertIsCharacter(intersectionTest, "intersectionTest")
	intersectionTest <- intersectionTest[1]
	if (!.isValidIntersectionTest(intersectionTest)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'intersectionTest' (", intersectionTest, ") must be one of ",
			.arrayToString(C_INTERSECTION_TESTS, encapsulate = TRUE))
	}
	if (.isTrialDesignConditionalDunnett(design) && intersectionTest != C_INTERSECTION_TEST_DUNNETT) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "intersection test ('", intersectionTest, "') must be 'Dunnett' ",
			"because conditional Dunnett test was specified as design")
	}
}

.ignoreParameterIfNotUsed <- function(paramName, paramValue, requirementLogical, requirementFailedReason,
		prefix = NA_character_) {
	if (all(is.na(paramValue)) || requirementLogical) {
		return(paramValue)
	}
	
	if (is.na(prefix) || trimws(prefix) == "") {
		prefix <- ""
	} else {
		prefix <- paste0(trimws(prefix), " ")
	}
	
	warning(prefix, "'", paramName, "' (", .arrayToString(paramValue), ") will be ignored because ", 
		requirementFailedReason, call. = FALSE)
	return(NA_real_)
}

#
# This is a workaround for the following  R core bug:
#
# rCoreBugDemonstration <- function(stageX, ...) {
# 	result <- list(...); result$stageX <- stageX; return(result)
# }
# # bug: stage will be removed, stageX gets the value of stage
# rCoreBugDemonstration("A", stage = 1) 
# # everything works as expected
# rCoreBugDemonstration("A", state = 1) 
#
.stopInCaseOfIllegalStageDefinition <- function(stageResults, ...) {
	stage <- list(...)[["stage"]]
	if (is.null(stage) && is.numeric(stageResults) && stageResults %in% 1L:C_KMAX_UPPER_BOUND) {
		stage <- stageResults
	}
	if (!is.null(stage)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'stage' (", stage, ") can only be defined in getStageResults() or getAnalysisResults()")
	}
}

.stopInCaseOfIllegalStageDefinition2 <- function(...) {
	forbiddenStage <- .getOptionalArgument("stage", ...)
	if (!is.null(forbiddenStage)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'stage' (", forbiddenStage, ") can only be defined in getStageResults() or getAnalysisResults()")
	}
}

.assertIsValidTolerance <- function(tolerance) {
	.assertIsSingleNumber(tolerance, "tolerance")
	if (tolerance > 0.1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'tolerance' (", tolerance, ") must be <= 0.1")
	}
}

.isValidVarianceOption <- function(varianceOption) {
	return(!is.null(varianceOption) && length(varianceOption) == 1 && !is.na(varianceOption) && 
			is.character(varianceOption) && varianceOption %in% C_VARIANCE_OPTIONS)
}

.assertIsValidVarianceOption <- function(design, varianceOption) {
	if (!.isValidVarianceOption(varianceOption)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'varianceOption' should be one of ",
			.arrayToString(C_VARIANCE_OPTIONS, encapsulate = TRUE))
	}
	if (.isTrialDesignConditionalDunnett(design) && varianceOption != C_VARIANCE_OPTION_DUNNETT) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"variance option ('", varianceOption, "') must be '", C_VARIANCE_OPTION_DUNNETT, "' ",
			"because conditional Dunnett test was specified as design")
	}
}

.assertIsValidSummaryIntervalFormat <- function(intervalFormat) {
	.assertIsSingleCharacter(intervalFormat, "intervalFormat") # "[%s; %s]"
	if (!grepl("^[^%]*%s[^%]*%s[^%]*$",  intervalFormat)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'intervalFormat' (", intervalFormat, ") has an invalid format; ",
			"the control character %s must appear exactly twice; ",
			"to change it use 'options(\"rpact.summary.intervalFormat\" = \"[%s; %s]\")'")
	}
}

.isSpecialPlotShowSourceArgument <- function(showSource) {
	return(is.character(showSource) && showSource %in% C_PLOT_SHOW_SOURCE_ARGUMENTS)
}

.assertIsValidTypeOfSelection <- function(typeOfSelection, rValue, epsilonValue, activeArms) {
	.assertIsCharacter(typeOfSelection, "typeOfSelection")
	typeOfSelection <- typeOfSelection[1]
	if (typeOfSelection == "rbest") {
		typeOfSelection <- "rBest"
	}
	if (!(typeOfSelection %in% C_TYPES_OF_SELECTION)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'typeOfSelection' (", typeOfSelection, ") must be one of ",
			.arrayToString(C_TYPES_OF_SELECTION, encapsulate = TRUE))
	}
	
	if (typeOfSelection == "rBest") {
		.assertIsSingleNumber(rValue, "rValue", naAllowed = FALSE, noDefaultAvailable = TRUE)
		if (activeArms == 1) {
			warning("'typeOfSelection' (\"", typeOfSelection, "\") will be ignored because 'activeArms' = 1", call. = FALSE)
		}
		else if (rValue > activeArms) {
			warning("'rValue' (", rValue, ") is larger than activeArms (", activeArms, ") and will be ignored", call. = FALSE)
		}
	} else if (!is.na(rValue)) {
		warning("'rValue' (", rValue, ") will be ignored because 'typeOfSelection' != \"rBest\"", call. = FALSE)
	}
	
	if (typeOfSelection == "epsilon") {
		.assertIsSingleNumber(epsilonValue, "epsilonValue", naAllowed = FALSE, noDefaultAvailable = TRUE)
	} else if (!is.na(epsilonValue)) {
		warning("'epsilonValue' (", epsilonValue, ") will be ignored because 'typeOfSelection' != \"epsilon\"", call. = FALSE)
	}
	
	return(typeOfSelection)
}

.assertIsValidSuccessCriterion <- function(successCriterion) {
	.assertIsCharacter(successCriterion, "successCriterion")
	successCriterion <- successCriterion[1]
	if (!(successCriterion %in% C_SUCCESS_CRITERIONS)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'successCriterion' (", successCriterion, ") must be one of ",
			.arrayToString(C_SUCCESS_CRITERIONS, encapsulate = TRUE))
	}
	return(successCriterion)
}

.assertIsValidEffectMeasure <- function(effectMeasure) {
	.assertIsCharacter(effectMeasure, "effectMeasure")
	effectMeasure <- effectMeasure[1]
	if (!(effectMeasure %in% C_EFFECT_MEASURES)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'effectMeasure' (", effectMeasure, ") must be one of ",
			.arrayToString(C_EFFECT_MEASURES, encapsulate = TRUE))
	}
	return(effectMeasure)
}

.assertIsValidMatrix <- function(x, argumentName, naAllowed = FALSE) {
	if (missing(x) || is.null(x) || length(x) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'", argumentName, "' must be a valid matrix")
	}
	
	if (!is.matrix(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", class(x), ") must be a valid matrix")
	}
	
	if (!naAllowed && any(is.na(x))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", .arrayToString(x), ") must not contain NA's")
	}
	
	if (!is.numeric(x)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'", argumentName, "' (", 
			.arrayToString(x), ") must be a valid numeric matrix")
	}
}

.assertIsValidTypeOfShape <- function(typeOfShape) {
	.assertIsCharacter(typeOfShape, "typeOfShape")
	typeOfShape <- typeOfShape[1]
	if (!(typeOfShape %in% C_TYPES_OF_SHAPE)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'typeOfShape' (", typeOfShape, ") must be one of ",
			.arrayToString(C_TYPES_OF_SHAPE, encapsulate = TRUE))
	}
	return(typeOfShape)
}

.assertIsValidEffectMatrixMeans <- function(typeOfShape, effectMatrix, muMaxVector, gED50, gMax, slope) {
	
	if (typeOfShape == "userDefined") {
		.assertIsValidMatrix(effectMatrix, "effectMatrix", naAllowed = FALSE)
		
		.assertIsNumericVector(muMaxVector, "muMaxVector", naAllowed = TRUE)
		if (!all(is.na(muMaxVector)) && !identical(muMaxVector, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT)) {
			warning("'muMaxVector' (", .arrayToString(muMaxVector), 
				") will be ignored because it will be set to first column of 'effectMatrix'", call. = FALSE)
		}
	} else if (!is.null(effectMatrix)) {
		warning("'effectMatrix' will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	if (typeOfShape == "sigmoidEmax") {
		.assertIsNumericVector(muMaxVector, "muMaxVector", naAllowed = FALSE, noDefaultAvailable = TRUE)
		.assertIsSingleNumber(gED50, "gED50", naAllowed = FALSE, noDefaultAvailable = TRUE)
		effectMatrix <- matrix(muMaxVector, nrow = length(muMaxVector), ncol = 1) %*% 
			matrix((1:gMax)^slope / (gED50^slope + (1:gMax)^slope), nrow = 1, ncol = gMax)
		return(effectMatrix)
	}
	
	if (!is.null(gED50) && !is.na(gED50)) {
		warning("'gED50' (", gED50, ") will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	if (typeOfShape == "linear") {
		.assertIsNumericVector(muMaxVector, "muMaxVector", naAllowed = FALSE, noDefaultAvailable = TRUE)
		effectMatrix <- matrix(muMaxVector, nrow = length(muMaxVector), ncol = 1) %*% 
			matrix((1:gMax) / gMax, nrow = 1, ncol = gMax)
	}
	
	if (!is.null(slope) && !is.na(slope) && slope != 1) {
		warning("'slope' (", slope, ") will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	return(effectMatrix)
}

.assertIsValidEffectMatrixRates <- function(typeOfShape, effectMatrix, piMaxVector, piControl, gED50, gMax, slope) {
	
	if (typeOfShape == "userDefined") {
		.assertIsValidMatrix(effectMatrix, "effectMatrix", naAllowed = FALSE)
		.assertIsInOpenInterval(effectMatrix, "effectMatrix", 0, 1, naAllowed = FALSE)
		
		.assertIsNumericVector(piMaxVector, "piMaxVector", naAllowed = TRUE)
		if (!all(is.na(piMaxVector)) && !identical(piMaxVector, C_PI_1_DEFAULT)) {
			warning("'piMaxVector' (", .arrayToString(piMaxVector), 
				") will be ignored because it will be set to first column of 'effectMatrix'", call. = FALSE)
		}
		
	} else if (!is.null(effectMatrix)) {
		warning("'effectMatrix' will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	if (typeOfShape == "sigmoidEmax") {
		.assertIsNumericVector(piMaxVector, "piMaxVector", naAllowed = FALSE, noDefaultAvailable = TRUE)
		.assertIsInOpenInterval(piMaxVector, "piMaxVector", 0, 1, naAllowed = FALSE)
		.assertIsSingleNumber(gED50, "gED50", naAllowed = FALSE, noDefaultAvailable = TRUE)
		effectMatrix <- matrix(piMaxVector, nrow = length(piMaxVector), ncol = 1) %*% 
			matrix((1:gMax)^slope / (gED50^slope + (1:gMax)^slope), nrow = 1, ncol = gMax)
		return(effectMatrix)
	}
	
	if (!is.null(gED50) && !is.na(gED50)) {
		warning("'gED50' (", gED50, ") will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}

	if (typeOfShape == "linear") {
		.assertIsNumericVector(piMaxVector, "piMaxVector", naAllowed = FALSE, noDefaultAvailable = TRUE)
		.assertIsInOpenInterval(piMaxVector, "piMaxVector", 0, 1, naAllowed = FALSE)
		.assertIsSingleNumber(piControl, "piControl", naAllowed = FALSE, noDefaultAvailable = TRUE)
		.assertIsInOpenInterval(piControl, "piControl", 0, 1, naAllowed = FALSE)
		effectMatrix <- piControl + matrix(piMaxVector - piControl, nrow = length(piMaxVector), ncol = 1) %*% 
			matrix((1:gMax) / gMax, nrow = 1, ncol = gMax)
	}
	
	if (!is.null(slope) && !is.na(slope) && slope != 1) {
		warning("'slope' (", slope, ") will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	return(effectMatrix)
}

.assertIsValidEffectMatrixSurvival <- function(typeOfShape, effectMatrix, omegaMaxVector, gED50, gMax, slope) {
	
	if (typeOfShape == "userDefined") {
		.assertIsValidMatrix(effectMatrix, "effectMatrix", naAllowed = FALSE)
		.assertIsInOpenInterval(effectMatrix, "effectMatrix", 0, NULL, naAllowed = FALSE)
		
		.assertIsNumericVector(omegaMaxVector, "omegaMaxVector", naAllowed = TRUE)
		if (!all(is.na(omegaMaxVector)) && !identical(omegaMaxVector, C_RANGE_OF_HAZARD_RATIOS_DEFAULT)) {
			warning("'omegaMaxVector' (", .arrayToString(omegaMaxVector), 
				") will be ignored because it will be set to first column of 'effectMatrix'", call. = FALSE)
		}
	}  else if (!is.null(effectMatrix)) {
		warning("'effectMatrix' will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	} 
		
	if (typeOfShape == "sigmoidEmax") {
		.assertIsNumericVector(omegaMaxVector, "omegaMaxVector", naAllowed = FALSE, noDefaultAvailable = TRUE)
		.assertIsInOpenInterval(omegaMaxVector, "omegaMaxVector", 0, NULL, naAllowed = FALSE)
		.assertIsSingleNumber(gED50, "gED50", naAllowed = FALSE, noDefaultAvailable = TRUE)
		effectMatrix <- matrix(omegaMaxVector - 1, nrow = length(omegaMaxVector), ncol = 1) %*% 
			matrix((1:gMax)^slope / (gED50^slope + (1:gMax)^slope), nrow = 1, ncol = gMax) + 1
		
		return(effectMatrix)
	}
	
	if (!is.null(gED50) && !is.na(gED50)) {
		warning("'gED50' (", gED50, ") will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	if (typeOfShape == "linear") {
		.assertIsNumericVector(omegaMaxVector, "omegaMaxVector", naAllowed = FALSE, noDefaultAvailable = TRUE)
		.assertIsInOpenInterval(omegaMaxVector, "omegaMaxVector", 0, NULL, naAllowed = FALSE)
		effectMatrix <- matrix(omegaMaxVector - 1, nrow = length(omegaMaxVector), ncol = 1) %*% 
			matrix((1:gMax) / gMax, nrow = 1, ncol = gMax) + 1
	}
	
	if (!is.null(slope) && !is.na(slope) && slope != 1) {
		warning("'slope' (", slope, ") will be ignored because 'typeOfShape' is defined as '", typeOfShape, "'", call. = FALSE)	
	}
	
	return(effectMatrix)
}

.assertIsValidPlannedSubjects <- function(plannedSubjects, kMax) {
	
	.assertIsIntegerVector(plannedSubjects, "plannedSubjects", validateType = FALSE)
	if (length(plannedSubjects) != kMax) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'plannedSubjects' (", .arrayToString(plannedSubjects), 
			") must have length 'kMax' (", kMax, ")")
	}
	.assertIsInClosedInterval(plannedSubjects, "plannedSubjects", lower = 1, upper = NULL)
	.assertValuesAreStrictlyIncreasing(plannedSubjects, "plannedSubjects")
}

