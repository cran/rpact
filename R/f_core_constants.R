######################################################################################
#                                                                                    #
# -- RPACT constants --                                                              #
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

C_COMPLETE_UNIT_TEST_SET_ENABLED <- FALSE

C_LOG_LEVEL_TRACE <- "TRACE"
C_LOG_LEVEL_DEBUG <- "DEBUG"
C_LOG_LEVEL_INFO <- "INFO"
C_LOG_LEVEL_WARN <- "WARN"
C_LOG_LEVEL_ERROR <- "ERROR"
C_LOG_LEVEL_PROGRESS <- "PROGRESS"
C_LOG_LEVEL_DISABLED <- ""

C_LOG_LEVEL <- C_LOG_LEVEL_PROGRESS

# the ratio of the circumference of a circle to its diameter
C_CIRCLE_CONSTANT_PI <- base::pi # 3.1415926535897931

# used in 'class_core_plot_settings.R'
C_POSITION_OUTSIDE_PLOT <- 0
C_POSITION_LEFT_TOP <- 1
C_POSITION_LEFT_CENTER <- 2
C_POSITION_LEFT_BOTTOM <- 3
C_POSITION_RIGHT_TOP <- 4
C_POSITION_RIGHT_CENTER <- 5
C_POSITION_RIGHT_BOTTOM <- 6

C_DESIGN_TOLERANCE_DEFAULT <- 1e-08
C_CONST_NEWTON_COTES <- 15
C_TWO_SIDED_POWER_DEFAULT <- FALSE
C_BINDING_FUTILITY_DEFAULT <- FALSE
C_BINDING_FUTILITY_FISHER_DEFAULT <- TRUE
C_CONST_BOUND_HP_DEFAULT <- 3
C_ALPHA_DEFAULT <- 0.025
C_BETA_DEFAULT <- 0.2
C_KMAX_DEFAULT <- 3L # L <- integer literal
C_KMAX_UPPER_BOUND <- 10L
C_KMAX_UPPER_BOUND_FISHER <- 6L

C_NA_MAX_DEFAULT <- 100L
C_POWER_ASN_THETA_DEFAULT <- seq(-1, 1, 0.02)

C_ANALYSIS_TOLERANCE_DEFAULT <- 1e-06
C_ANALYSIS_TOLERANCE_FISHER_DEFAULT <- 1e-14

C_UPPER_BOUNDS_DEFAULT <- 8
C_FUTILITY_BOUNDS_DEFAULT <- -6
C_ALPHA_0_VEC_DEFAULT <- 1
C_THETA_H0_MEANS_DEFAULT <- 0
C_THETA_H0_RATES_DEFAULT <- 0
C_THETA_H0_SURVIVAL_DEFAULT <- 1
C_ALLOCATION_RATIO_DEFAULT <- 1
C_DIRECTION_UPPER_DEFAULT <- TRUE
C_NORMAL_APPROXIMATION_MEANS_DEFAULT <- FALSE
C_NORMAL_APPROXIMATION_RATES_DEFAULT <- TRUE
C_EQUAL_VARIANCES_DEFAULT <- TRUE
C_ITERATIONS_DEFAULT <- 10000

C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT <- 50

C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL <- "TrialDesignGroupSequential"
C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL <- "TrialDesignInverseNormal"
C_CLASS_NAME_TRIAL_DESIGN_FISHER <- "TrialDesignFisher"

.getTrialDesignClassNames <- function() {
	return(c(C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL, 
			C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
			C_CLASS_NAME_TRIAL_DESIGN_FISHER))
}

C_EXCEPTION_TYPE_RUNTIME_ISSUE = "Runtime exception: "
C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT = "Illegal argument: "
C_EXCEPTION_TYPE_ILLEGAL_DATA_INPUT = "Illegal data input: "
C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS = "Conflicting arguments: "
C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS = "Argument out of bounds: "
C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS = "Argument length out of bounds: "
C_EXCEPTION_TYPE_INDEX_OUT_OF_BOUNDS = "Index out of bounds: "
C_EXCEPTION_TYPE_MISSING_ARGUMENT = "Missing argument: "
C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS = "Incomplete associated arguments: "

C_DIRECTION_LOWER = "lower"
C_DIRECTION_UPPER = "upper"

# 
# Constants used in 'parameters.R'
# 
C_PARAM_USER_DEFINED <- "u"
C_PARAM_DEFAULT_VALUE <- "d"
C_PARAM_GENERATED <- "g"
C_PARAM_DERIVED <- ">"
C_PARAM_NOT_APPLICABLE <- "."
C_PARAM_TYPE_UNKNOWN <- "?"

# 
# Constants used in 'core_group_sequential_design.R'
# 
# Type of design is one of the following: 
# O'Brien & Fleming, 
# Pocock, 
# Wang & Tsiatis Delta class, 
# Haybittle & Peto, 
# Optimum design within Wang & Tsiatis class, 
# Pocock type alpha spending, 
# O'Brien & Fleming type alpha spending, 
# Kim & DeMets alpha spending, 
# Hwang, Shi & DeCani alpha spending, 
# user defined alpha spending
#
C_TYPE_OF_DESIGN_OF <- "OF" # O'Brien & Fleming
C_TYPE_OF_DESIGN_P <- "P"   # Pocock, 
C_TYPE_OF_DESIGN_WT <- "WT" # Wang & Tsiatis Delta class
C_TYPE_OF_DESIGN_HP <- "HP" # Haybittle & Peto
C_TYPE_OF_DESIGN_WT_OPTIMUM <- "WToptimum" # Optimum design within Wang & Tsiatis class
C_TYPE_OF_DESIGN_AS_P <- "asP"      # Pocock type alpha spending
C_TYPE_OF_DESIGN_AS_OF <- "asOF"    # O'Brien & Fleming type alpha spending
C_TYPE_OF_DESIGN_AS_KD <- "asKD"    # Kim & DeMets alpha spending
C_TYPE_OF_DESIGN_AS_HSD <- "asHSD"  # Hwang, Shi & DeCani alpha spending
C_TYPE_OF_DESIGN_AS_USER <- "asUser" # user defined alpha spending
C_DEFAULT_TYPE_OF_DESIGN <- C_TYPE_OF_DESIGN_OF # the default type of design

getDesignTypes <- function() {
	return(c(
		C_TYPE_OF_DESIGN_OF,
		C_TYPE_OF_DESIGN_P,
		C_TYPE_OF_DESIGN_WT,
		C_TYPE_OF_DESIGN_HP,
		C_TYPE_OF_DESIGN_WT_OPTIMUM,
		C_TYPE_OF_DESIGN_AS_P,
		C_TYPE_OF_DESIGN_AS_OF,
		C_TYPE_OF_DESIGN_AS_KD,
		C_TYPE_OF_DESIGN_AS_HSD,
		C_TYPE_OF_DESIGN_AS_USER
	))
}

printDesignTypes <- function() {
	.arrayToString(getDesignTypes(), encapsulate = TRUE)
}

isAlphaSpendingDesignType <- function(typeOfDesign, userDefinedAlphaSpendingIncluded = TRUE) {
	if (userDefinedAlphaSpendingIncluded && typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
		return(TRUE)
	}
	
	return(typeOfDesign %in% c(C_TYPE_OF_DESIGN_AS_P, C_TYPE_OF_DESIGN_AS_OF, 
					C_TYPE_OF_DESIGN_AS_KD,C_TYPE_OF_DESIGN_AS_HSD))
}


# 
# Type of beta spending design is one of the following: 
# Pocock type beta spending, 
# O'Brien & Fleming type beta spending, 
# Kim & DeMets beta spending, 
# Hwang, Shi & DeCani beta spending, 
# user defined beta spending
# "none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"
C_TYPE_OF_DESIGN_BS_NONE <- "none" 
C_TYPE_OF_DESIGN_BS_P <- "bsP"       # Pocock type beta spending
C_TYPE_OF_DESIGN_BS_OF <- "bsOF"     # O'Brien & Fleming type beta spending
C_TYPE_OF_DESIGN_BS_KD <- "bsKD"     # Kim & DeMets beta spending
C_TYPE_OF_DESIGN_BS_HSD <- "bsHSD"   # Hwang, Shi & DeCani beta spending
C_TYPE_OF_DESIGN_BS_USER <- "bsUser" # user defined beta spending

getBetaSpendingDesignTypes <- function() {
	return(c(
		C_TYPE_OF_DESIGN_BS_NONE,
		C_TYPE_OF_DESIGN_BS_P,
		C_TYPE_OF_DESIGN_BS_OF,
		C_TYPE_OF_DESIGN_BS_KD,
		C_TYPE_OF_DESIGN_BS_HSD,
		C_TYPE_OF_DESIGN_BS_USER
	))
}

printBetaSpendingDesignTypes <- function() {
	.arrayToString(getBetaSpendingDesignTypes(), encapsulate = TRUE)
}

isBetaSpendingDesignType <- function(typeOfDesign, userDefinedBetaSpendingIncluded = TRUE, noneIncluded = FALSE) {
	if (userDefinedBetaSpendingIncluded && typeOfDesign == C_TYPE_OF_DESIGN_BS_USER) {
		return(TRUE)
	}
	
	if (noneIncluded && typeOfDesign == C_TYPE_OF_DESIGN_BS_NONE) {
		return(TRUE)
	}
	
	return(typeOfDesign %in% c(		
		C_TYPE_OF_DESIGN_BS_P,
		C_TYPE_OF_DESIGN_BS_OF,
		C_TYPE_OF_DESIGN_BS_KD,
		C_TYPE_OF_DESIGN_BS_HSD
	))
}

##
## -------------------------------------------
##

C_OPTIMIZATION_CRITERION_ASNH1 <- "ASNH1"
C_OPTIMIZATION_CRITERION_ASNIFH1 <- "ASNIFH1"
C_OPTIMIZATION_CRITERION_ASN_SUM <- "ASNsum"
C_OPTIMIZATION_CRITERION_DEFAULT <- C_OPTIMIZATION_CRITERION_ASNH1

getOptimizationCriterions <- function() {
	return(c(
		C_OPTIMIZATION_CRITERION_ASNH1,
		C_OPTIMIZATION_CRITERION_ASNIFH1,
		C_OPTIMIZATION_CRITERION_ASN_SUM
	))
}

printOptimizationCriterion <- function() {
	.arrayToString(getOptimizationCriterions(), encapsulate = TRUE)
}

isOptimizationCriterion <- function(x) {
	return(x %in% getOptimizationCriterions())
}

##
## -------------------------------------------
##

C_FISHER_METHOD_FULL_ALPHA <- "fullAlpha"
C_FISHER_METHOD_EQUAL_ALPHA <- "equalAlpha"
C_FISHER_METHOD_NO_INTERACTION <- "noInteraction"
C_FISHER_METHOD_USER_DEFINED_ALPHA <- "userDefinedAlpha"
C_FISHER_METHOD_DEFAULT <- C_FISHER_METHOD_EQUAL_ALPHA

getFisherMethods <- function() {
	return(c(
		C_FISHER_METHOD_FULL_ALPHA,
		C_FISHER_METHOD_EQUAL_ALPHA,
		C_FISHER_METHOD_NO_INTERACTION,
		C_FISHER_METHOD_USER_DEFINED_ALPHA
	))
}

printFisherMethods <- function() {
	.arrayToString(getFisherMethods(), encapsulate = TRUE)
}

isFisherMethod <- function(method) {
	return(method %in% getFisherMethods())
}

##
## -------------------------------------------
##

C_PARAMETER_NAMES <- list(
	iterations = "Iterations",
	seed = "Seed",
	
	groups = "Treatment groups",
	stages = "Stages",
	sampleSizes = "Sample sizes",
	means = "Means",
	stDevs = "Standard deviations",
	events = "Events",	
	overallEvents = "Overall events",
	overallAllocationRatios = "Overall allocation ratios",
	overallLogRanks = "Overall logranks",
	
	bindingFutility = "Binding futility",
	constantBoundsHP = "Haybittle Peto constants",
	
	kMax = "Maximum number of stages",
	alpha = "Significance level",
	finalStage = "Final stage",
	informationRates = "Information rates",
	criticalValues = "Critical values",
	stageLevels = "Stage levels", 
	alphaSpent = "Cumulative alpha spending",
	tolerance = "Tolerance",
	method = "Method",
	alpha0Vec = "Alpha_0",
	scale = "Scale",
	nonStochasticCurtailment = "Non stochastic curtailment",
	simAlpha = "Simulated alpha",
	beta = "Type II error rate",
	betaSpent = "Cumulative beta spending",
	sided = "Test",
	futilityBounds = "Futility bounds (binding)",
	futilityBoundsNonBinding = "Futility bounds (non-binding)",
	typeOfDesign = "Type of design",
	deltaWT = "Delta for Wang & Tsiatis Delta class",
	optimizationCriterion = "Optimization criterion for optimum design within Wang & Tsiatis class",
	gammaA = "Parameter for alpha spending function",
	gammaB = "Parameter for beta spending function",
	typeBetaSpending = "Type of beta spending",
	userAlphaSpending = "User defined alpha spending",
	userBetaSpending = "User defined beta spending",
	probs = "Exit probabilities" ,
	power = "Power",
	theta = "Effect",
	direction = "Direction",
	normalApproximation = "Normal approximation",
	equalVariances = "Equal variances",
	
	nFixed = "N fixed",
	shift = "Shift",
	inflationFactor = "Inflation factor",
	information = "Informations",
	rejectionProbabilities = "Rejection probabilities",
	futilityProbabilities = "Futility probabilities",
	averageSampleNumber1 = "Expected reduction under H1",
	averageSampleNumber01 = "Expected reduction under a value between H0 and H1",
	averageSampleNumber0 = "Expected reduction under H0",
	
	allocationRatioPlanned = "Planned allocation ratio",
	thetaH0 = "Theta H0", # Effect
	thetaH1 = "Assumed effect",
	assumedStDev = "Assumed standard deviation",
	pi1 = "pi_1",
	pi2 = "pi_2",
	nPlanned = "Planned sample size",
	
	effectSizes = "Effect sizes",
	testStatistics = "Test statistics",
	pValues = "p-values",
	combinationTestStatistics = "Combination test statistics",
	testActions = "Actions",
	conditionalPower = "Conditional power",
	conditionalPowerSimulated = "Conditional power (simulated)",
	conditionalRejectionProbabilities = "CRP",
	repeatedConfidenceIntervalLowerBounds = "RCIs (lower)",
	repeatedConfidenceIntervalUpperBounds = "RCIs (upper)",
	repeatedPValues = "Repeated p-values",
	finalPValues = "Final p-value",
	finalConfidenceIntervalLowerBounds = "Final CIs (lower)",
	finalConfidenceIntervalUpperBounds = "Final CIs (upper)",
	medianUnbiasedEstimates = "Median unbiased estimate",
	
	overallSampleSizes = "Overall sample sizes",
	overallSampleSizes1 = "Overall sample sizes (1)",
	overallSampleSizes2 = "Overall sample sizes (2)",
	overallTestStatistics = "Overall test statistics", 
	overallPValues = "Overall p-values", 
	overallMeans = "Overall means", 
	overallMeans1 = "Overall means (1)", 
	overallMeans2 = "Overall means (2)", 
	overallStDevs1 = "Overall standard deviations (1)", 
	overallStDevs2 = "Overall standard deviations (2)", 
	overallStDevs = "Overall standard deviations", 
	testStatistics = "Test statistics", 
	combInverseNormal = "Inverse Normal Combination", 
	combFisher = "Fisher Combination", 
	weightsFisher = "Weights Fisher", 
	weightsInverseNormal = "Weights Inverse Normal",
	
	overallLogRanks = "Overall logranks",
	overallEvents = "Overall events",
	overallEvents1 = "Overall events (1)",
	overallEvents2 = "Overall events (2)",
	overallAllocationRatios = "Overall allocation ratios",
	events = "Events",
	allocationRatios = "Allocation ratios",
	logRanks = "Log ranks",
	
	nMax = "N_max",
	averageSampleNumber = "Average sample sizes (ASN)",
	calculatedPower = "Power",
	earlyStop = "Early stop",
	rejectPerStage = "Reject per stage",
	futilityPerStage = "Futility per stage",
	overallEarlyStop = "Overall early stop",
	overallRejectPerStage = "Overall reject per stage",
	overallFutilityPerStage = "Overall futility per stage",
	
	riskRatio = "Risk ratio",
	meanRatio = "Mean ratio",
	alternative = "Alternatives",
	stDev = "Standard deviation",
	nFixed1 = "N fixed (1)",
	nFixed2 = "N fixed (2)",
	
	maxNumberOfPatients = "Maximum number of patients",					
	numberOfPatients = "Number of patients",
	numberOfPatientsGroup1 = "Number of patients (1)",
	numberOfPatientsGroup2 = "Number of patients (2)",
	expectedPatientsH0 = "Expected patients H0",
	expectedPatientsH01 = "Expected patients H0/H1",
	expectedPatientsH1 = "Expected patients H1",
	
	omega = "Probabilities of an event",
	hazardRatio = "Hazard ratio",
	
	typeOfComputation = "Type of computation",
	accountForObservationTimes = "Account for observation times",
	eventTime = "Event time",
	accrualTime = "Accrual time",
	followUpTime = "Follow up time",
	dropOutRate1 = "Drop-out rate (1)",
	dropOutRate2 = "Drop-out rate (2)",
	dropOutTime = "Drop-out time",
	calculateFollowUpTime = "Calculate follow up time",
	eventsFixed = "Events fixed",
	expectedEventsH0 = "Expected events H0",
	expectedEventsH01 = "Expected events H0/H1",
	expectedEventsH1 = "Expected events H1",
	
	analysisTimes = "Analysis times", 
	studyDurationH1 = "Study duration H1",
	eventsOverStages = "Events over stages", 
	expectedNumberOfPatientsH1 = "Expected number of patients H1",
	
	twoSidedPower = "Two-sided power"
)

.getParameterNames <- function(design = NULL) {
	parameterNames <- C_PARAMETER_NAMES
	if (!is.null(design) && !is.na(design$bindingFutility) && !design$bindingFutility) {
		parameterNames$futilityBounds <- C_PARAMETER_NAMES[["futilityBoundsNonBinding"]]
	}
	return(parameterNames)
}

C_TABLE_COLUMN_NAMES <- list(
	iterations = "Iterations",
	seed = "Seed",
	
	groups = "Treatment group",
	stages = "Stage",
	sampleSizes = "Sample size",
	means = "Mean",
	stDevs = "Standard deviation",
	events = "Event",	
	overallEvents = "Overall event",
	overallAllocationRatios = "Overall allocation ratio",
	overallLogRanks = "Overall log rank",
	overallMeans = "Overall mean",
	
	bindingFutility = "Binding futility",
	constantBoundsHP = "Haybittle Peto constant",
	
	kMax = "Maximum number of stages",
	alpha = "Significance level",
	finalStage = "Final stage",
	informationRates = "Information rate",
	criticalValues = "Critical value",
	stageLevels = "Stage level", 
	alphaSpent = "Cumulative alpha spending",
	tolerance = "Tolerance",
	method = "Method",
	alpha0Vec = "Alpha_0",
	scale = "Scale",
	nonStochasticCurtailment = "Non stochastic curtailment",
	simAlpha = "Simulated alpha",
	beta = "Type II error rate",
	betaSpent = "Cumulative beta spending",
	sided = "Test",
	futilityBounds = "Futility bound (binding)",
	futilityBoundsNonBinding = "Futility bound (non-binding)",
	typeOfDesign = "Type of design",
	deltaWT = "Delta (Wang & Tsiatis)",
	optimizationCriterion = "Optimization criterion (Wang & Tsiatis)",
	gammaA = "Parameter for alpha spending function",
	gammaB = "Parameter for beta spending function",
	typeBetaSpending = "Type of beta",
	userAlphaSpending = "User defined alpha spending",
	userBetaSpending = "User defined beta spending",
	probs = "Internal calculation probabilities" ,
	power = "Power",
	theta = "Effect",
	direction = "Direction",
	normalApproximation = "Normal approximation",
	equalVariances = "Equal variance",

	assumedStDev = "Assumed standard deviation",
	
	nFixed = "N fixed",
	shift = "Shift",
	inflationFactor = "Inflation factor",
	information = "Information",
	rejectionProbabilities = "Rejection probability",
	futilityProbabilities = "Futility probability",
	averageSampleNumber1 = "Expected reduction under H1",
	averageSampleNumber01 = "Expected reduction under a value between H0 and H1",
	averageSampleNumber0 = "Expected reduction under H0",
	
	nPlanned = "Planned sample size",
	
	stages = "Stage",
	effectSizes = "Effect size",
	testStatistics = "Test statistic",
	pValues = "p-value",
	combinationTestStatistics = "Combination test statistic",
	testActions = "Action",
	conditionalPower = "Conditional power",
	conditionalPowerSimulated = "Conditional power (simulated)",
	conditionalRejectionProbabilities = "CRP",
	repeatedConfidenceIntervalLowerBounds = "RCI (lower)",
	repeatedConfidenceIntervalUpperBounds = "RCI (upper)",
	repeatedPValues = "Repeated p-value",
	finalPValues = "Final p-value",
	finalConfidenceIntervalLowerBounds = "Final CI (lower)",
	finalConfidenceIntervalUpperBounds = "Final CI (upper)",
	medianUnbiasedEstimates = "Median unbiased estimate",
	
	overallSampleSizes = "Overall sample size",
	overallSampleSizes1 = "Overall sample size (1)",
	overallSampleSizes2 = "Overall sample size (2)",
	overallTestStatistics = "Overall test statistic", 
	overallPValues = "Overall p-value", 
	overallMeans1 = "Overall mean (1)", 
	overallMeans2 = "Overall mean (2)", 
	overallStDevs1 = "Overall standard deviation (1)", 
	overallStDevs2 = "Overall standard deviation (2)", 
	overallStDevs = "Overall standard deviation", 
	testStatistics = "Test statistic", 
	combInverseNormal = "Inverse Normal Combination", 
	combFisher = "Fisher Combination", 
	weightsFisher = "Weight Fisher", 
	weightsInverseNormal = "Weight Inverse Normal",
	
	overallLogRanks = "Overall log rank",
	overallEvents = "Overall event",
	overallEvents1 = "Overall event (1)",
	overallEvents2 = "Overall event (2)",
	overallAllocationRatios = "Overall allocation ratio",
	events = "Event",
	allocationRatios = "Allocation ratio",
	logRanks = "Log rank",
	
	nMax = "N_max",
	averageSampleNumber = "Average sample size (ASN)",
	calculatedPower = "Power",
	earlyStop = "Early stop",
	rejectPerStage = "Reject per stage",
	futilityPerStage = "Futility per stage",
	overallEarlyStop = "Overall early stop",
	overallRejectPerStage = "Overall reject per stage",
	overallFutilityPerStage = "Overall futility per stage",
	
	riskRatio = "Risk ratio",
	meanRatio = "Mean ratio",
	alternative = "Alternative",
	stDev = "Standard deviation",
	nFixed1 = "N fixed (1)",
	nFixed2 = "N fixed (2)",
	
	maxNumberOfPatients = "Max # patients",					
	numberOfPatients = "# patients",
	numberOfPatientsGroup1 = "# patients (1)",
	numberOfPatientsGroup2 = "# patients (2)",
	expectedPatientsH0 = "Expected patients H0",
	expectedPatientsH01 = "Expected patients H0/H1",
	expectedPatientsH1 = "Expected patients H1",
	
	omega = "Probability of an event",
	hazardRatio = "Hazard ratio",
	
	typeOfComputation = "Type of computation",
	accountForObservationTimes = "Account for observation times",
	eventTime = "Event time",
	accrualTime = "Accrual time",
	followUpTime = "Follow up time",
	dropOutRate1 = "Drop-out rate (1)",
	dropOutRate2 = "Drop-out rate (2)",
	dropOutTime = "Drop-out time",
	calculateFollowUpTime = "Calculate follow up time",
	eventsFixed = "Event fixed",
	expectedEventsH0 = "Expected event H0",
	expectedEventsH01 = "Expected event H0/H1",
	expectedEventsH1 = "Expected event H1",
	
	analysisTimes = "Analysis time", 
	studyDurationH1 = "Study duration H1",
	eventsOverStages = "Events over stages", 
	expectedNumberOfPatientsH1 = "Expected number of patients H1",
	
	twoSidedPower = "Two-sided power"
)

.getTableColumnNames <- function(design = NULL) {
	tableColumnNames <- C_TABLE_COLUMN_NAMES
	if (!is.null(design) && !is.na(design$bindingFutility) && !design$bindingFutility) {
		tableColumnNames$futilityBounds <- C_TABLE_COLUMN_NAMES[["futilityBoundsNonBinding"]]
	}
	return(tableColumnNames)
}

C_PARAMETER_FORMAT_FUNCTIONS <- list(
	means = "formatMeans",	
	stDevs = "formatStDevs",
	stDev = "formatStDevs",
	assumedStDev = "formatStDevs",
	overallAllocationRatios = "formatRatios",
	overallLogRanks = "formatTestStatistics",
	
	alpha = "formatProbabilities",
	informationRates = "formatRates",
	stageLevels = "formatProbabilities", 
	alphaSpent = "formatProbabilities",
	alpha0Vec = "formatProbabilities",
	simAlpha = "formatProbabilities",
	criticalValues = "formatFisherCriticalValues", #  will be set in class TrialDesignFisher
	criticalValues = "formatGroupSequentialCriticalValues", # will be set in class TrialDesignGroupSequential
	betaSpent = "formatProbabilities",
	futilityBounds = "formatGroupSequentialCriticalValues",
	alpha0Vec = "formatFisherCriticalValues",
	
	constantBoundsHP = "formatGroupSequentialCriticalValues",
	
	nFixed = "formatSampleSizes",
	nFixed1 = "formatSampleSizes",
	nFixed2 = "formatSampleSizes",
	shift = "formatProbabilities",
	inflationFactor = "formatProbabilities",
	information = "formatProbabilities",
	power = "formatProbabilities",
	rejectionProbabilities = "formatProbabilities",
	futilityProbabilities = "formatFutilityProbabilities",
	probs = "formatProbabilities",	
	averageSampleNumber1 = "formatProbabilities",
	averageSampleNumber01 = "formatProbabilities",
	averageSampleNumber0 = "formatProbabilities",

	effectSizes = "formatMeans",
	testStatistics = "formatTestStatistics",
	pValues = "formatPValues",
	combinationTestStatistics = "formatTestStatistics",
	conditionalPower = "formatConditionalPower", 
	conditionalPowerSimulated = "formatConditionalPower", 
	conditionalRejectionProbabilities = "formatProbabilities",
	repeatedConfidenceIntervalLowerBounds = "formatStDevs",
	repeatedConfidenceIntervalUpperBounds = "formatStDevs",
	repeatedPValues = "formatRepeatedPValues", 
	finalPValues = "formatPValues",
	finalConfidenceIntervalLowerBounds = "formatStDevs",
	finalConfidenceIntervalUpperBounds = "formatStDevs",
	medianUnbiasedEstimates = "formatStDevs",

	overallTestStatistics = "formatTestStatistics", 
	overallPValues = "formatPValues", 
	overallMeans = "formatMeans", 
	overallMeans1 = "formatMeans", 
	overallMeans2 = "formatMeans", 
	overallStDevs1 = "formatStDevs", 
	overallStDevs2 = "formatStDevs", 
	overallStDevs = "formatStDevs", 
	testStatistics = "formatTestStatistics", 
	combInverseNormal = "formatTestStatistics", 
	combFisher = "formatFisherTestStatistics",

	weightsFisher = "formatRates",
	weightsInverseNormal = "formatRates", 
	overallLogRanks = "formatTestStatistics", 
	logRanks = "formatTestStatistics", 
	
	theta = "formatDouble",
	averageSampleNumber = "formatDouble",
	calculatedPower = "formatProbabilities",
	earlyStop = "formatDouble",
	rejectPerStage = "formatDouble",
	futilityPerStage = "formatDouble",
	overallEarlyStop = "formatDouble",
	overallRejectPerStage = "formatDouble",
	overallFutilityPerStage = "formatDouble",
	
	maxNumberOfPatients = "formatSampleSizes",					
	numberOfPatients = "formatSampleSizes",
	numberOfPatientsGroup1 = "formatSampleSizes",
	numberOfPatientsGroup2 = "formatSampleSizes",
	expectedPatientsH0 = "formatSampleSizes",
	expectedPatientsH01 = "formatSampleSizes",
	expectedPatientsH1 = "formatSampleSizes",
	
	omega = "formatRates",
	hazardRatio = "formatRates",
	
	eventTime = "formatDouble",
	accrualTime = "formatDouble",
	followUpTime = "formatDouble",
	dropOutRate1 = "formatRates",
	dropOutRate2 = "formatRates",
	dropOutTime = "formatDouble",
	eventsFixed = "formatSampleSizes",
	expectedEventsH0 = "formatSampleSizes",
	expectedEventsH01 = "formatSampleSizes",
	expectedEventsH1 = "formatSampleSizes",
	analysisTimes = "formatDouble", 
	studyDurationH1 = "formatDouble",
	eventsOverStages = "formatSampleSizes", 
	expectedNumberOfPatientsH1 = "formatSampleSizes"
)

.getParameterFormatFunctions <- function() {
	return(C_PARAMETER_FORMAT_FUNCTIONS)
}


