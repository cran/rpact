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

C_LOG_LEVEL_TRACE <- "TRACE"
C_LOG_LEVEL_DEBUG <- "DEBUG"
C_LOG_LEVEL_INFO <- "INFO"
C_LOG_LEVEL_WARN <- "WARN"
C_LOG_LEVEL_ERROR <- "ERROR"
C_LOG_LEVEL_PROGRESS <- "PROGRESS"
C_LOG_LEVEL_DISABLED <- "DISABLED"

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
C_VARIED_PARAMETER_SEQUENCE_LENGTH_DEFAULT <- 30

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
# Constants used in 'f_simulation_survival.R'
# 
C_PI_2_DEFAULT <- 0.2
C_PI_1_DEFAULT <- seq(0.2, 0.5, 0.1)
C_PI_1_SAMPLE_SIZE_DEFAULT <- c(0.4, 0.5, 0.6)
C_DROP_OUT_RATE_1_DEFAULT <- 0
C_DROP_OUT_RATE_2_DEFAULT <- 0
C_DROP_OUT_TIME_DEFAULT <- 12L
C_EVENT_TIME_DEFAULT <- 12L
C_ALLOCATION_1_DEFAULT <- 1
C_ALLOCATION_2_DEFAULT <- 1
C_MAX_ITERATIONS_DEFAULT <- 10L
C_MAX_SIMULATION_ITERATIONS_DEFAULT <- 1000L
C_ACCRUAL_TIME_DEFAULT <- c(0L, 12L)
C_ACCRUAL_INTENSITY_DEFAULT <- 0.1
C_FOLLOW_UP_TIME_DEFAULT <- 6L

# 
# Additional constants used in 'f_design_sample_size_calculator.R'
# 

C_ALTERNATIVE_DEFAULT <- seq(0.2, 1, 0.2)
C_ALTERNATIVE_POWER_SIMULATION_DEFAULT <- seq(0, 1, 0.2)
C_STDEV_DEFAULT <- 1

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

.getDesignTypes <- function() {
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

.printDesignTypes <- function() {
	.arrayToString(.getDesignTypes(), encapsulate = TRUE)
}

.isAlphaSpendingDesignType <- function(typeOfDesign, userDefinedAlphaSpendingIncluded = TRUE) {
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

.getBetaSpendingDesignTypes <- function() {
	return(c(
		C_TYPE_OF_DESIGN_BS_NONE,
		C_TYPE_OF_DESIGN_BS_P,
		C_TYPE_OF_DESIGN_BS_OF,
		C_TYPE_OF_DESIGN_BS_KD,
		C_TYPE_OF_DESIGN_BS_HSD,
		C_TYPE_OF_DESIGN_BS_USER
	))
}

.printBetaSpendingDesignTypes <- function() {
	.arrayToString(.getBetaSpendingDesignTypes(), encapsulate = TRUE)
}

.isBetaSpendingDesignType <- function(typeOfDesign, userDefinedBetaSpendingIncluded = TRUE, noneIncluded = FALSE) {
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

.getOptimizationCriterions <- function() {
	return(c(
		C_OPTIMIZATION_CRITERION_ASNH1,
		C_OPTIMIZATION_CRITERION_ASNIFH1,
		C_OPTIMIZATION_CRITERION_ASN_SUM
	))
}

.printOptimizationCriterion <- function() {
	.arrayToString(.getOptimizationCriterions(), encapsulate = TRUE)
}

.isOptimizationCriterion <- function(x) {
	return(x %in% .getOptimizationCriterions())
}

##
## -------------------------------------------
##

C_FISHER_METHOD_FULL_ALPHA <- "fullAlpha"
C_FISHER_METHOD_EQUAL_ALPHA <- "equalAlpha"
C_FISHER_METHOD_NO_INTERACTION <- "noInteraction"
C_FISHER_METHOD_USER_DEFINED_ALPHA <- "userDefinedAlpha"
C_FISHER_METHOD_DEFAULT <- C_FISHER_METHOD_EQUAL_ALPHA

.getFisherMethods <- function() {
	return(c(
		C_FISHER_METHOD_FULL_ALPHA,
		C_FISHER_METHOD_EQUAL_ALPHA,
		C_FISHER_METHOD_NO_INTERACTION,
		C_FISHER_METHOD_USER_DEFINED_ALPHA
	))
}

.printFisherMethods <- function() {
	.arrayToString(.getFisherMethods(), encapsulate = TRUE)
}

.isFisherMethod <- function(method) {
	return(method %in% .getFisherMethods())
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
	overallEvents = "Overall events",
	overallAllocationRatios = "Overall allocation ratios",
	overallLogRanks = "Overall logranks",
	
	bindingFutility = "Binding futility",
	constantBoundsHP = "Haybittle Peto constants",
	
	kMax = "Maximum number of stages",
	alpha = "Significance level",
	finalStage = "Final stage",
	informationRates = "Information rates", # DOTO remove ending 's' (plural)?
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
	
	shift = "Shift",
	inflationFactor = "Inflation factor",
	information = "Informations",
	rejectionProbabilities = "Rejection probabilities",
	futilityProbabilities = "Futility probabilities",
	averageSampleNumber1 = "Ratio expected vs fixed sample size under H1",
	averageSampleNumber01 = "Ratio expected vs fixed sample size under a value between H0 and H1",
	averageSampleNumber0 = "Ratio expected vs fixed sample size under H0",
	
	allocationRatioPlanned = "Planned allocation ratio",
	thetaH0 = "Theta H0", 
	thetaH1 = "Assumed effect",
	assumedStDev = "Assumed standard deviation",
	pi1 = "pi (1)",
	pi2 = "pi (2)",
	pi1H1 = "pi (1) under H1",
	pi2H1 = "pi (2) under H1",
	nPlanned = "Planned sample size",
	
	effectSizes = "Effect sizes",
	testStatistics = "Test statistics",
	pValues = "p-values",
	combinationTestStatistics = "Combination test statistics",
	testActions = "Actions",
	conditionalPower = "Conditional power",
	conditionalPowerAchieved = "Cond. power (achieved)",
	conditionalPowerSimulated = "Cond. power (simulated)",
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
	combInverseNormal = "Inverse normal combination", 
	combFisher = "Fisher combination", 
	weightsFisher = "Weights Fisher", 
	weightsInverseNormal = "Weights inverse normal",
	
	overallLogRanks = "Overall logranks",
	overallEvents = "Overall number of events",
	overallEvents1 = "Overall number of events (1)",
	overallEvents2 = "Overall number of events (2)",
	overallAllocationRatios = "Overall allocation ratios",
	events = "Number of events",
	allocationRatios = "Allocation ratios",
	logRanks = "Log ranks",
	
	nMax = "N_max",
	averageSampleNumber = "Average sample sizes (ASN)",
	calculatedPower = "Power",
	earlyStop = "Early stop",
	rejectPerStage = "Reject per stage",
	futilityPerStage = "Futility stop per stage",
	overallEarlyStop = "Overall Early stop",
	overallReject = "Overall reject",
	overallFutility = "Overall futility",
	
	riskRatio = "Risk ratio",
	meanRatio = "Mean ratio",
	alternative = "Alternatives",
	stDev = "Standard deviation",
	nFixed = "Number of subjects fixed",
	nFixed1 = "Number of subjects fixed (1)",
	nFixed2 = "Number of subjects fixed (2)",
	
	maxNumberOfSubjects = "Maximum number of subjects",					
	maxNumberOfSubjects1 = "Maximum number of subjects (1)",					
	maxNumberOfSubjects2 = "Maximum number of subjects (2)",					
	numberOfSubjects = "Number of subjects",
	numberOfSubjects1 = "Number of subjects (1)",
	numberOfSubjects2 = "Number of subjects (2)",
	expectedNumberOfSubjectsH0 = "Expected number of subjects under H0",
	expectedNumberOfSubjectsH01 = "Expected number of subjects under H0/H1",
	expectedNumberOfSubjectsH1 = "Expected number of subjects under H1",
	expectedNumberOfSubjects = "Expected number of subjects",
	
	omega = "Probability of an event",
	hazardRatio = "Hazard ratio",
	
	typeOfComputation = "Type of computation",
	accountForObservationTimes = "Account for observation times",
	eventTime = "Event time",
	accrualTime = "Accrual time",
	totalAccrualTime = "Total accrual time",
	remainingTime = "Remaining time",
	followUpTime = "Follow up time",
	dropoutRate1 = "Drop-out rate (1)",
	dropoutRate2 = "Drop-out rate (2)",
	dropoutTime = "Drop-out time",
	calculateFollowUpTime = "Calculate follow up time",
	eventsFixed = "Number of events fixed",
	expectedEventsH0 = "Expected number of events under H0",
	expectedEventsH01 = "Expected number of events under H0/H1",
	expectedEventsH1 = "Expected number of events under H1",
	
	analysisTime = "Analysis times", 
	studyDurationH1 = "Expected study duration under H1",
	eventsPerStage = "Number of events by stage", 
	expectedNumberOfSubjectsH1 = "Expected number of subjects under H1",
	
	twoSidedPower = "Two-sided power",
	
	plannedEvents = "Planned events",
	plannedSubjects = "Planned subjects",
	minNumberOfEventsPerStage = "Minimum number of events per stage",
	maxNumberOfEventsPerStage = "Maximum number of events per stage",
	minNumberOfSubjectsPerStage = "Minimum number of subjects per stage",
	maxNumberOfSubjectsPerStage = "Maximum number of subjects per stage",
	accrualIntensity = "Accrual intensity",
	accrualIntensityRelative = "Accrual intensity (relative)",
	maxNumberOfIterations = "Maximum number of iterations",
	allocation1 = "Allocation 1",
	allocation2 = "Allocation 2",
	expectedNumberOfEvents = "Expected number of events",
	expectedNumberOfEventsPerStage = "Expected number of events by stage",
	eventsNotAchieved = "Events not achieved",
	subjects = "Subjects",
	overallReject = "Overall reject",
	futilityStop = "Futility stop",
	studyDuration = "Expected study duration",
	maxStudyDuration = "Maximal study duration",
	directionUpper = "Direction upper",
	piecewiseSurvivalTime = "Piecewise survival times",
	lambda2 = "Lambda (2)",
	lambda1 = "Lambda (1)",
	kappa = "Kappa",
	
	earlyStopPerStage = "Early stop per stage",
	effect = "Effect",
	maxNumberOfEvents = "Maximum number of events",
	
	criticalValuesEffectScale = "Critical values (effect scale)",
	criticalValuesEffectScaleLower = "Lower critical values (effect scale)",
	criticalValuesEffectScaleUpper = "Upper critical values (effect scale)",
	criticalValuesPValueScale = "Local one-sided significance levels",
	".design$stageLevels" = "Local one-sided significance levels",
	futilityBoundsEffectScale = "Futility bounds (effect scale)",
	futilityBoundsPValueScale = "Futility bounds (1-sided p-value scale)",
	
	analysisTime = "Analysis time",
	eventsPerStage1 = "Observed # events by stage (1)",
	eventsPerStage2 = "Observed # events by stage (2)",
	testStatistic = "Test statistic",
	logRankStatistic = "Log-rank statistic",
	hazardRatioEstimateLR = "Hazard ratio estimate LR",
	
	delayedResponseAllowed = "Delayed response allowed",
	delayedResponseEnabled = "Delayed response enabled",
	piecewiseSurvivalEnabled = "Piecewise exponential survival enabled",
	
	median1 = "Median (1)",
	median2 = "Median (2)",
	
	eventsPerStage = "Observed number of events by stage", 
	expectedNumberOfEvents = "Observed number of events",
	expectedNumberOfSubjects = "Observed number of subjects",
	
	endOfAccrualIsUserDefined = "End of accrual is user defined",
	followUpTimeMustBeUserDefined = "Follow-up time must be user defined",
	maxNumberOfSubjectsIsUserDefined = "Max number of subjects is user defined",
	maxNumberOfSubjectsCanBeCalculatedDirectly = "Max number of subjects can be calculated directly",
	absoluteAccrualIntensityEnabled = "Absolute accrual intensity is enabled",
	
	time = "Time",
	overallEventProbabilities = "Overall event probabilities",
	eventProbabilities1 = "Event probabilities (1)",
	eventProbabilities2 = "Event probabilities (2)"
)

.getParameterNames <- function(design = NULL, designPlan = NULL) {
	parameterNames <- C_PARAMETER_NAMES
	
	if (!is.null(design) && !is.na(design$bindingFutility) && !design$bindingFutility) {
		parameterNames$futilityBounds <- C_PARAMETER_NAMES[["futilityBoundsNonBinding"]]
	}
	
	if (!is.null(designPlan) && inherits(designPlan, "TrialDesignPlanSurvival") &&
			!is.null(designPlan$.piecewiseSurvivalTime) &&
		designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
		parameterNames$lambda2 <- "Piecewise survival lambda (2)"
		parameterNames$lambda1 <- "Piecewise survival lambda (1)"
	}
	
	if (!is.null(designPlan) && 
			inherits(designPlan, "TrialDesignPlanSurvival") && 
			identical(designPlan$.design$kMax, 1L)) {
		parameterNames$maxNumberOfEvents <- "Number of events"
	}

	if (!is.null(designPlan) && inherits(designPlan, "TrialDesignPlan") && 
			identical(designPlan$.design$kMax, 1L)) {
		parameterNames$studyDuration <- "Study duration"
	}
	
	if (!is.null(designPlan) && 
			(inherits(designPlan, "TrialDesignPlanMeans") || 
				inherits(designPlan, "SimulationResultsMeans")) && 
			isTRUE(designPlan$meanRatio)) {
		parameterNames$stDev <- "Coefficient of variation"
	}
	
	if (!is.null(design) && class(design) != "TrialDesign" && design$sided == 2) {
		parameterNames$criticalValuesPValueScale <- "Local two-sided significance levels"
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
	overallEvents = "Overall event",
	overallAllocationRatios = "Overall allocation ratio",
	overallLogRanks = "Overall log rank",
	overallMeans = "Overall mean",
	
	bindingFutility = "Binding futility",
	constantBoundsHP = "Haybittle Peto constant",
	
	kMax = "Maximum # stages",
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
	typeBetaSpending = "Type of beta spending",
	userAlphaSpending = "User defined alpha spending",
	userBetaSpending = "User defined beta spending",
	probs = "Internal calculation probabilities" ,
	power = "Power",
	theta = "Effect",
	direction = "Direction",
	normalApproximation = "Normal approximation",
	equalVariances = "Equal variance",

	assumedStDev = "Assumed standard deviation",
	
	shift = "Shift",
	inflationFactor = "Inflation factor",
	information = "Information",
	rejectionProbabilities = "Rejection probability",
	futilityProbabilities = "Futility probability",
	averageSampleNumber1 = "Ratio expected vs fixed sample size under H1",
	averageSampleNumber01 = "Ratio expected vs fixed sample size under a value between H0 and H1",
	averageSampleNumber0 = "Ratio expected vs fixed sample size under H0",
	
	allocationRatioPlanned = "Planned allocation ratio",
	thetaH0 = "Theta H0", # Effect
	thetaH1 = "Assumed effect",
	assumedStDev = "Assumed standard deviation",
	pi1 = "pi (1)",
	pi2 = "pi (2)",
	pi1H1 = "pi (1) under H1",
	pi2H1 = "pi (2) under H1",
	nPlanned = "Planned sample size",
	
	stages = "Stage",
	effectSizes = "Effect size",
	testStatistics = "Test statistic",
	pValues = "p-value",
	combinationTestStatistics = "Combination test statistic",
	testActions = "Action",
	conditionalPower = "Conditional power",
	conditionalPowerAchieved = "Cond. power (achieved)",
	conditionalPowerSimulated = "Cond. power (simulated)",
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
	overallEvents = "Overall # events",
	overallEvents1 = "Overall # events (1)",
	overallEvents2 = "Overall #events (2)",
	overallAllocationRatios = "Overall allocation ratio",
	events = "# events", 
	allocationRatios = "Allocation ratio",
	logRanks = "Log rank",
	
	nMax = "N_max",
	averageSampleNumber = "Average sample size (ASN)",
	calculatedPower = "Power",
	earlyStop = "Early stop",
	rejectPerStage = "Reject per stage",
	futilityPerStage = "Futility stop per stage",
	overallEarlyStop = "Overall Early stop",
	overallReject = "Overall reject",
	overallFutility = "Overall futility",
	
	riskRatio = "Risk ratio",
	meanRatio = "Mean ratio",
	alternative = "Alternative",
	stDev = "Standard deviation",
	nFixed = "# subjects fixed",
	nFixed1 = "# subjects fixed (1)",
	nFixed2 = "# subjects fixed (2)",
	
	maxNumberOfSubjects = "Max # subjects",					
	maxNumberOfSubjects1 = "Max # subjects (1)",					
	maxNumberOfSubjects2 = "Max # subjects (2)",					
	numberOfSubjects = "# subjects",
	numberOfSubjects1 = "# subjects (1)",
	numberOfSubjects2 = "# subjects (2)",
	expectedNumberOfSubjectsH0 = "Expected # subjects under H0",
	expectedNumberOfSubjectsH01 = "Expected # subjects under H0/H1",
	expectedNumberOfSubjectsH1 = "Expected # subjects under H1",
	expectedNumberOfSubjects = "Expected # subjects",
	
	omega = "Probability of an event",
	hazardRatio = "Hazard ratio",
	
	typeOfComputation = "Type of computation",
	accountForObservationTimes = "Account for observation times",
	eventTime = "Event time",
	accrualTime = "Accrual time",
	totalAccrualTime = "Total accrual time",
	remainingTime = "Remaining time",
	followUpTime = "Follow up time",
	dropoutRate1 = "Drop-out rate (1)",
	dropoutRate2 = "Drop-out rate (2)",
	dropoutTime = "Drop-out time",
	calculateFollowUpTime = "Calculate follow up time",
	eventsFixed = "# events fixed",
	expectedEventsH0 = "Expected # events under H0",
	expectedEventsH01 = "Expected # events under H0/H1",
	expectedEventsH1 = "Expected # events under H1",
	
	analysisTime = "Analysis time", 
	eventsPerStage1 = "Observed # events by stage (1)",
	eventsPerStage2 = "Observed # events by stage (2)",
	studyDurationH1 = "Expected study duration H1",
	eventsPerStage = "# events by stage", 
	expectedNumberOfSubjectsH1 = "Expected # subjects H1",
	
	twoSidedPower = "Two-sided power",
	
	plannedEvents = "Required planned events",
	plannedSubjects = "Required planned subjects",
	minNumberOfEventsPerStage = "Minimum # additional events per stage",
	maxNumberOfEventsPerStage = "Maximum # additional events per stage",
	minNumberOfSubjectsPerStage = "Minimum # of additional subjects per stage",
	maxNumberOfSubjectsPerStage = "Maximum # of additional subjects per stage",
	accrualIntensity = "Accrual intensity",
	accrualIntensityRelative = "Accrual intensity (relative)",
	maxNumberOfIterations = "Maximum # iterations",
	allocation1 = "Allocation 1",
	allocation2 = "Allocation 2",
	expectedNumberOfEvents = "Expected # events",
	expectedNumberOfEventsPerStage = "Expected # events by stage",
	eventsNotAchieved = "Events not achieved",
	subjects = "Subjects",
	futilityStop = "Futility stop",
	studyDuration = "Expected study duration",
	maxStudyDuration = "Maximal study duration",
	directionUpper = "Direction upper",
	piecewiseSurvivalTime = "Piecewise survival times",
	lambda2 = "Lambda (2)",
	lambda1 = "Lambda (1)",
	kappa = "Kappa",
	
	earlyStopPerStage = "Early stop per stage",
	effect = "Effect",
	maxNumberOfEvents = "Maximum # events",
	
	criticalValuesEffectScale = "Critical value (effect scale)",
	criticalValuesEffectScaleLower = "Lower critical value (effect scale)",
	criticalValuesEffectScaleUpper = "Upper critical value (effect scale)",
	criticalValuesPValueScale = "Local one-sided significance level",
	".design$stageLevels" = "Local one-sided significance level",
	futilityBoundsEffectScale = "Futility bound (effect scale)",
	futilityBoundsPValueScale = "Futility bound (1-sided p-value scale)",
	
	delayedResponseAllowed = "Delayed response allowed",
	delayedResponseEnabled = "Delayed response enabled",
	piecewiseSurvivalEnabled = "Piecewise exponential survival enabled",
	
	median1 = "Median (1)",
	median2 = "Median (2)",
	
	eventsPerStage = "Observed number of events by stage", 
	expectedNumberOfEvents = "Observed number of events",
	expectedNumberOfSubjects = "Observed number of subjects",
	
	endOfAccrualIsUserDefined = "End of accrual is user defined",
	followUpTimeMustBeUserDefined = "Follow-up time must be user defined",
	maxNumberOfSubjectsIsUserDefined = "Max number of subjects is user defined",
	maxNumberOfSubjectsCanBeCalculatedDirectly = "Max number of subjects can be calculated directly",
	absoluteAccrualIntensityEnabled = "Absolute accrual intensity is enabled",
	
	time = "Time",
	overallEventProbabilities = "Overall event probability",
	eventProbabilities1 = "Event probability (1)",
	eventProbabilities2 = "Event probability (2)"
)

.getTableColumnNames <- function(design = NULL, designPlan = NULL) {
	tableColumnNames <- C_TABLE_COLUMN_NAMES
	
	if (!is.null(design) && !is.na(design$bindingFutility) && !design$bindingFutility) {
		tableColumnNames$futilityBounds <- C_TABLE_COLUMN_NAMES[["futilityBoundsNonBinding"]]
	}
	
	if (!is.null(designPlan) && inherits(designPlan, "TrialDesignPlanSurvival") &&
			!is.null(designPlan$.piecewiseSurvivalTime) &&
			designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
		tableColumnNames$lambda2 = "Piecewise survival lambda (2)"
		tableColumnNames$lambda1 = "Piecewise survival lambda (1)"
	}
	
	if (!is.null(designPlan) && 
			inherits(designPlan, "TrialDesignPlanSurvival") && 
			identical(designPlan$.design$kMax, 1L)) {
		tableColumnNames$maxNumberOfEvents <- "Number of events"
	}
	
	if (!is.null(designPlan) && inherits(designPlan, "TrialDesignPlan") && 
			identical(designPlan$.design$kMax, 1L)) {
		tableColumnNames$studyDuration <- "Study duration"
	}
	
	if (!is.null(designPlan) && 
			(inherits(designPlan, "TrialDesignPlanMeans") || 
				inherits(designPlan, "SimulationResultsMeans")) && 
			isTRUE(designPlan$meanRatio)) {
		tableColumnNames$stDev <- "Coefficient of variation"
	}
	
	if (!is.null(design) && class(design) != "TrialDesign" && design$sided == 2) {
		tableColumnNames$criticalValuesPValueScale <- "Local two-sided significance level"
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
	allocationRatioPlanned = "formatRatios",
	
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
	thetaH1 = "formatMeans",
	testStatistics = "formatTestStatistics",
	pValues = "formatPValues",
	combinationTestStatistics = "formatTestStatistics",
	conditionalPower = "formatConditionalPower", 
	conditionalPowerAchieved = "formatConditionalPower",
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
	overallReject = "formatDouble",
	overallFutility = "formatDouble",
	
	maxNumberOfSubjects = "formatSampleSizes",	
	maxNumberOfSubjects1 = "formatSampleSizes",	
	maxNumberOfSubjects2 = "formatSampleSizes",	
	maxNumberOfEvents = "formatSampleSizes",
	numberOfSubjects = "formatSampleSizes",
	numberOfSubjects1 = "formatSampleSizes",
	numberOfSubjects2 = "formatSampleSizes",
	expectedNumberOfSubjectsH0 = "formatSampleSizes",
	expectedNumberOfSubjectsH01 = "formatSampleSizes",
	expectedNumberOfSubjectsH1 = "formatSampleSizes",
	expectedNumberOfSubjects = "formatSampleSizes",
	
	omega = "formatRates",
	hazardRatio = "formatRates",
	
	pi1 = "formatRates",
	pi2 = "formatRates",
	pi1H1 = "formatRates",
	pi2H1 = "formatRates",
	piecewiseSurvivalTime = "formatTime",
	lambda2 = "formatRates",
	lambda1 = "formatRates",
	
	eventTime = "formatDouble",
	accrualTime = "formatTime",
	totalAccrualTime = "formatTime",
	remainingTime = "formatTime",
	followUpTime = "formatTime",
	dropoutRate1 = "formatRates",
	dropoutRate2 = "formatRates",
	dropoutTime = "formatTime",
	eventsFixed = "formatSampleSizes",
	expectedEventsH0 = "formatSampleSizes",
	expectedEventsH01 = "formatSampleSizes",
	expectedEventsH1 = "formatSampleSizes",
	analysisTime = "formatTime", 
	studyDurationH1 = "formatDurations",
	eventsPerStage = "formatSampleSizes", 
	expectedNumberOfSubjectsH1 = "formatSampleSizes",
	
	events = "formatSampleSizes",
	expectedNumberOfEvents = "formatSampleSizes",
	expectedNumberOfEventsPerStage = "formatSampleSizes",
	eventsNotAchieved = "formatDouble",
	subjects = "formatSampleSizes",
	futilityStop = "formatSimulationOutput",
	studyDuration = "formatDurations",
	maxStudyDuration = "formatDurations",
	
	earlyStopPerStage = "formatDouble",
	effect = "formatDouble",
	
	criticalValuesEffectScale = "formatGroupSequentialCriticalValues",
	criticalValuesEffectScaleLower = "formatGroupSequentialCriticalValues",
	criticalValuesEffectScaleUpper = "formatGroupSequentialCriticalValues",
	criticalValuesPValueScale = "formatProbabilities",
	futilityBoundsEffectScale = "formatGroupSequentialCriticalValues",
	futilityBoundsPValueScale = "formatProbabilities",
	
	median1 = "formatRatesDynamic",
	median2 = "formatRatesDynamic",
	
	accrualIntensity = "formatAccrualIntensities",
	accrualIntensityRelative = "formatAccrualIntensities",
	
	eventsPerStage = "formatSampleSizes", 
	expectedNumberOfEvents = "formatSampleSizes",
	expectedNumberOfSubjects = "formatSampleSizes",
	
	time = "formatTime",
	overallEventProbabilities = "formatProbabilities",
	eventProbabilities1 = "formatProbabilities",
	eventProbabilities2 = "formatProbabilities"
)

.getParameterFormatFunctions <- function() {
	return(C_PARAMETER_FORMAT_FUNCTIONS)
}


