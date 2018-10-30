######################################################################################
#                                                                                    #
# -- Sample size calculator --                                                       #
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

#' @title
#' Get Sample Size Means
#' 
#' @description 
#' Returns the sample size for testing means in one and two samples.
#'
#' @param design The trial design.
#' @param normalApproximation If \code{normalApproximation = TRUE} is specified, the variance is 
#'        assumed to be known, default is FALSE.
#' @param meanRatio If \code{meanRatio = TRUE} is specified, the sample size for 
#'        one-sided testing of H0: mu1/mu2 = thetaH0 is calculated, default is FALSE.
#' @param thetaH0 The null hypothesis value. For one-sided testing, a value != 0 
#'        (or a value != 1 for testing the mean ratio) can be specified, default is 0.
#' @param alternative The alternative hypothesis value. This can be a vector of assumed 
#'        alternatives, default is seq(0.1, 2, 0.2). 
#' @param stDev The standard deviation, default is 1. If \code{meanRatio = TRUE} 
#'        is specified, stDev defines the coefficient of variation sigma/mu2. 
#' @param groups The number of treatment groups (1 or 2), default is 2.
#' @param allocationRatioPlanned The planned allocation ratio for a two treatment groups 
#'        design, default is 1. If \code{allocationRatioPlanned = 0} is entered, 
#' 		  the optimal allocation ratio yielding the smallest overall sample size is determined.
#' @param ... Ensures that all arguments are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' At given design the function calculates the sample size for testing means.
#' In a two treatment groups design, additionally, an allocation ratio = n1Fixed/n2Fixed can be specified. 
#' A null hypothesis value thetaH0 != 0 for testing the difference of two means or  
#' thetaH0 != 1 for testing the ratio of two means can be specified.
#' 
#' @return Returns a \code{TrialDesignPlanMeans} object.
#'
#' @export
#' 
#' @examples
#' 
#' # Calculate sample sizes n1Fixed, n2Fixed, and 
#' # nFixed for a range of alternative values:
#' getSampleSizeMeans(getDesignGroupSequential(alpha = 0.025, sided = 1), 
#'     groups = 2, alternative = seq(0.1, 2, 0.2), 
#'     normalApproximation = FALSE, allocationRatioPlanned = 2)
#' 
getSampleSizeMeans <- function(design, ..., normalApproximation = FALSE, meanRatio = FALSE, 
		thetaH0 = 0, alternative = seq(0.2, 1, 0.2), 
		stDev = 1, groups = 2, allocationRatioPlanned = 1) {
	
	.assertIsTrialDesign(design)
	.warnInCaseOfUnknownArguments(functionName = "getSampleSizeMeans", ...)
	.warnInCaseOfTwoSidedPowerArgument(...)
	
	designPlan <- TrialDesignPlanMeans(
		design = design, 
		normalApproximation = normalApproximation, 
		meanRatio = meanRatio, 
		thetaH0 = thetaH0, 
		alternative = alternative, 
		stDev = stDev, 
		groups = groups, 
		allocationRatioPlanned = allocationRatioPlanned
	)
	return(.getSampleSize(designPlan))
}

.warnInCaseOfTwoSidedPowerArgument <- function(...) {
	if ("twoSidedPower" %in% names(list(...))) {
		warning("Note that 'twoSidedPower' can only be defined in 'design'", call. = FALSE)
	}
}

#' @title
#' Get Sample Size Rates 
#' 
#' @description 
#' Returns the sample size for testing rates in one and two samples.
#' 
#' @param design The trial design.
#' @param normalApproximation If \code{normalApproximation = FALSE} is specified, the sample size 
#'        for the case of one treatment group is calculated exactly using the binomial distribution. 
#'        default is TRUE.
#' @param riskRatio If \code{riskRatio = TRUE} is specified, the sample size for one-sided 
#'        testing of H0: pi1/pi2 = thetaH0 is calculated, default is FALSE. 
#' @param thetaH0 The null hypothesis value. For one-sided testing, a value != 0 
#'        (or != 1 for testing the risk ratio pi1/pi2) can be specified, default is 0.
#' @param pi1 The assumed probability in the treatment group if two treatment groups 
#'        are considered, or the alternative probability for a one treatment group design, 
#'        default is seq(0.4, 0.6, 0.1).
#' @param pi2 The assumed probability in the control group if two treatment groups are considered, default is 0.2. 
#' @param groups The number of treatment groups (1 or 2), default is 2.
#' @param allocationRatioPlanned The planned allocation ratio for a two treatment groups design. \cr
#'        If \code{allocationRatioPlanned = 0} is entered, the optimal allocation ratio yielding the 
#'        smallest overall sample size is determined, default is 1.
#' @param ... Ensures that all arguments are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' At given design the function calculates the sample size for testing rates.
#' In a two treatment groups design, additionally, an allocation ratio = n1Fixed/n2Fixed can be specified. 
#' If a null hypothesis value thetaH0 != 0 for testing the difference of two rates 
#' thetaH0 != 1 for testing the risk ratio is specified, the sample size 
#' formula according to Farrington & Manning (Statistics in Medicine, 1990) is used.
#' 
#' @return Returns a \code{TrialDesignPlanRates} object.
#'
#' @export
#' 
#' @examples
#' 
#' # Calculate sample sizes n1Fixed, n2Fixed, nFixed, and the optimum 
#' # allocation ratios for a range of pi1 values when testing 
#' # H0: pi1 - pi2 = -0.1:
#' getSampleSizeRates(getDesignGroupSequential(alpha = 0.025, beta = 0.2, 
#'     sided = 1), groups = 2, thetaH0 = -0.1, pi1 = seq(0.4, 0.55, 0.025), 
#'     pi2 = 0.4, normalApproximation = TRUE, allocationRatioPlanned = 0)
#' 
#' # Calculate sample sizes n1Fixed, n2Fixed, nFixed, and the optimum 
#' # allocation ratios for a range of pi2 values when testing 
#' # H0: pi1 / pi2 = 1.25:
#' getSampleSizeRates(getDesignGroupSequential(alpha = 0.025, beta = 0.2, 
#'     sided = 1), groups = 2, riskRatio = TRUE, thetaH0 = 1.25, pi1 = 0.3, 
#'     pi2 = 0.3, normalApproximation = TRUE, allocationRatioPlanned = 1)
#' 
getSampleSizeRates <- function(design, ..., normalApproximation = TRUE, riskRatio = FALSE, 
		thetaH0 = 0, pi1 = seq(0.4, 0.6, 0.1), pi2 = 0.2, groups = 2, allocationRatioPlanned = 1) {
	
	.assertIsTrialDesign(design)
	.warnInCaseOfUnknownArguments(functionName = "getSampleSizeRates", ...)
	.warnInCaseOfTwoSidedPowerArgument(...)
		
	designPlan <- TrialDesignPlanRates(
		design = design, 
		normalApproximation = normalApproximation, 
		riskRatio = riskRatio, 
		thetaH0 = thetaH0, 
		pi1 = pi1, 
		pi2 = pi2, 
		groups = groups, 
		allocationRatioPlanned = allocationRatioPlanned
	)
	return(.getSampleSize(designPlan))
}

#' @title
#' Get Sample Size Survival 
#' 
#' @description 
#' Returns the sample size for testing the hazard ratio in a two treatment groups survival design. 
#'
#' @param design The trial design.
#' @param typeOfComputation Three options are available: "Schoenfeld", "Freedman", "HsiehFreedman", 
#'        the default is "Schoenfeld". For details, see Hsieh (Statistics in Medicine, 1992).
#' @param thetaH0 The null hypothesis value. The default value is 1. For one-sided testing, 
#'        a bound for testing H0: log(1 - pi1)/log(1 - pi2) = thetaH0 != 1 can be specified.
#' @param pi1 The assumed event rate in the treatment group, default is seq(0.4, 0.6, 0.1).
#' @param pi2 The assumed event rate in the control group, default is 0.2.
#' @param allocationRatioPlanned The planned allocation ratio, default is 1. 
#'        If \code{allocationRatioPlanned = 0} is entered, the optimal allocation ratio yielding the 
#'        smallest number of patients is determined.
#' @param accountForObservationTimes If \code{accountForObservationTimes = TRUE}, the number of 
#'        patients is calculates assuming assuming specific accrual and follow-up time. 
#'        The formula of Kim & Tsiatis (Biometrics, 1990) is used to calculated the expected 
#'        number of events under the alternative (see also Lakatos & Lan, Statistics in Medicine, 1992).
#' @param eventTime The assumed time under which the event rates are calculated 
#'        (need to be specified if \code{accountForObservationTimes = TRUE}).
#' @param accrualTime The assumed accrual time for the study (need to be specified if \cr 
#'        \code{accountForObservationTimes = TRUE}). 
#' @param followUpTime The assumed (additional) follow-up time for the study 
#'        (need to be specified if \code{accountForObservationTimes = TRUE}). 
#'        The total study duration is accrualTime + followUpTime.  
#' @param dropOutRate1 The assumed drop-out rate in the control group, default is 0. 
#' @param dropOutRate2 The assumed drop-out rate in the treatment group, default is 0.
#' @param dropOutTime The assumed time for drop-out rates in the control and the 
#'        treatment group, default is 12. 
#' @param maxNumberOfPatients For \code{accountForObservationTimes = TRUE}, if \cr
#'        \code{maxNumberOfPatients > 0} is specified, the follow-up time for the required 
#'        number of events is determined.   
#' @param ... Ensures that all arguments are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' At given design the function calculates the number of events and an estimate for the 
#' necessary number of patients for testing the 
#' hazard ratio log(1 - p1)/log(1 - p2) with no interim stages. 
#' Accordingly, an event probability omega is calculated.
#' It also calculates the time when the required events are expected under the given 
#' assumptions (assuming exponentially distributed survival times). 
#' Furthermore, an allocation ratio = n1Fixed/n2Fixed can be specified. 
#' 
#' @return Returns a \code{TrialDesignPlanSurvival} object.
#'
#' @export
#' 
#' @examples
#' 
#' # Calculate the number of events and number of patients calculated with 
#' # the Schoenfeld formula.
#' getSampleSizeSurvival(getDesignGroupSequential(alpha = 0.025, beta = 0.2, 
#'     sided = 1), thetaH0 = 1, pi1 = 0.6, pi2 = 0.9, 
#'     allocationRatioPlanned = 2, typeOfComputation = "Schoenfeld")
#' 
#' # Calculate analysis times, number of aevent, and number of patients 
#' # under specified event, accrual, followup, and dropout time and event 
#' # and dropout rates.
#' getSampleSizeSurvival(getDesignGroupSequential(alpha = .025, sided = 1), 
#'     pi1 = c(0.25, 0.3, 0.35), pi2 = 0.4, allocationRatioPlanned = 0, 
#'     typeOfComputation = "Schoenfeld", accountForObservationTimes = TRUE,
#'     eventTime = 12, accrualTime = 6, followUpTime = 12, 
#'     maxNumberOfPatients = 0, dropOutRate1 = 0.15, dropOutRate2 = 0.1, 
#'     dropOutTime = 24)
#' 
getSampleSizeSurvival <- function(design, ..., typeOfComputation = "Schoenfeld", 
		thetaH0 = 1, pi2 = 0.2, pi1 = seq(0.4, 0.6, 0.1), 
		allocationRatioPlanned = 1, accountForObservationTimes = NA, 
		eventTime = NA_real_, accrualTime = NA_real_, followUpTime = NA_real_, maxNumberOfPatients = 0, 
		dropOutRate1 = 0, dropOutRate2 = 0, dropOutTime = NA_real_) {
	
	.assertIsTrialDesign(design)
	.warnInCaseOfUnknownArguments(functionName = "getSampleSizeSurvival", ...)
	.warnInCaseOfTwoSidedPowerArgument(...)
		
	designPlan <- TrialDesignPlanSurvival(
		design = design, 
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
	return(.getSampleSize(designPlan))
}

.setFollowUpTime <- function(designPlan, sampleSizeObject, userDefinedMaxNumberOfPatientsEnabled) {
	if (designPlan$accountForObservationTimes) {
		followUpTimeDefined <- designPlan$followUpTime
		if (userDefinedMaxNumberOfPatientsEnabled) {
			designPlan$followUpTime = sampleSizeObject$followUpTime
		} else {
			designPlan$followUpTime = sampleSizeObject$followUpTime[1]
		}
		designPlan$.setParameterType("followUpTime", 
			ifelse(designPlan$maxNumberOfPatients > 0 && followUpTimeDefined != designPlan$followUpTime, 
				C_PARAM_GENERATED, C_PARAM_USER_DEFINED))
		if (designPlan$maxNumberOfPatients > 0 && userDefinedMaxNumberOfPatientsEnabled) {
			designPlan$.setParameterType("maxNumberOfPatients", C_PARAM_USER_DEFINED)
		}
		if (all(designPlan$followUpTime == 24)) {
			designPlan$.setParameterType("followUpTime", C_PARAM_DEFAULT_VALUE)
		}
	} else {
		designPlan$.setParameterType("followUpTime", 
			ifelse(designPlan$maxNumberOfPatients > 0, C_PARAM_GENERATED, C_PARAM_DEFAULT_VALUE))
		if (all(designPlan$followUpTime == 24)) {
			designPlan$.setParameterType("followUpTime", C_PARAM_DEFAULT_VALUE)
		}
	}
}

.getSampleSize <- function(designPlan) {
	
	userDefinedMaxNumberOfPatientsEnabled <- length(designPlan$maxNumberOfPatients) > 1 || 
		designPlan$maxNumberOfPatients != 0

	if (.isTrialDesignPlanMeans(designPlan) || .isTrialDesignPlanRates(designPlan)) {
		if (.isTrialDesignPlanMeans(designPlan)) {
			sampleSizeFixed <- .getSampleSizeFixedMeans(
				alpha = designPlan$getAlpha(), 
				beta = designPlan$getBeta(), 
				sided = designPlan$getSided(), 
				twoSidedPower = designPlan$getTwoSidedPower(), 
				normalApproximation = designPlan$normalApproximation, 
				meanRatio = designPlan$meanRatio, 
				thetaH0 = designPlan$thetaH0, 
				alternative = designPlan$alternative, 
				stDev = designPlan$stDev, 
				groups = designPlan$groups, 
				allocationRatioPlanned = designPlan$allocationRatioPlanned)
		} else {
			sampleSizeFixed <- .getSampleSizeFixedRates(
				alpha = designPlan$getAlpha(), 
				beta = designPlan$getBeta(), 
				sided = designPlan$getSided(), 
				normalApproximation = designPlan$normalApproximation, 
				riskRatio = designPlan$riskRatio, 
				thetaH0 = designPlan$thetaH0, 
				pi1 = designPlan$pi1, 
				pi2 = designPlan$pi2, 
				groups = designPlan$groups, 
				allocationRatioPlanned = designPlan$allocationRatioPlanned)
		}

		# Fixed
		designPlan$nFixed <- sampleSizeFixed$nFixed
		designPlan$.setParameterType("nFixed", C_PARAM_GENERATED)
		if (designPlan$groups == 2) {
			designPlan$nFixed1 <- sampleSizeFixed$n1Fixed
			designPlan$nFixed2 <- sampleSizeFixed$n2Fixed
			designPlan$.setParameterType("nFixed1", C_PARAM_GENERATED)
			designPlan$.setParameterType("nFixed2", C_PARAM_GENERATED)
		}
		
		if (!is.null(sampleSizeFixed$allocationRatioPlanned) && (length(designPlan$allocationRatioPlanned) != length(sampleSizeFixed$allocationRatioPlanned) ||
				sum(designPlan$allocationRatioPlanned == sampleSizeFixed$allocationRatioPlanned) != length(designPlan$allocationRatioPlanned))) {
			designPlan$allocationRatioPlanned = sampleSizeFixed$allocationRatioPlanned
			designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
		}
		
		# Sequential
		if (designPlan$.design$kMax > 1) {
			designCharacteristics <- getDesignCharacteristics(designPlan$.design)
			if (.isTrialDesignPlanMeans(designPlan)) {
				sampleSizeSequential <- .getSampleSizeSequentialMeans(sampleSizeFixed, designCharacteristics)
			} else {
				sampleSizeSequential <- .getSampleSizeSequentialRates(sampleSizeFixed, designCharacteristics)
			}
			
			designPlan$informationRates = sampleSizeSequential$informationRates
			designPlan$maxNumberOfPatients = sampleSizeSequential$maxNumberOfPatients
			designPlan$numberOfPatients = sampleSizeSequential$numberOfPatients
			if (designPlan$groups == 2) {
				designPlan$numberOfPatientsGroup1 = sampleSizeSequential$numberOfPatientsGroup1
				designPlan$numberOfPatientsGroup2 = sampleSizeSequential$numberOfPatientsGroup2
			}
			designPlan$expectedPatientsH0 = sampleSizeSequential$expectedPatientsH0
			designPlan$expectedPatientsH01 = sampleSizeSequential$expectedPatientsH01
			designPlan$expectedPatientsH1 = sampleSizeSequential$expectedPatientsH1
			
			designPlan$.setParameterType("informationRates", C_PARAM_GENERATED)
			designPlan$.setParameterType("maxNumberOfPatients", C_PARAM_GENERATED)
			designPlan$.setParameterType("numberOfPatients", C_PARAM_GENERATED)
			if (designPlan$groups == 2) {
				designPlan$.setParameterType("numberOfPatientsGroup1", C_PARAM_GENERATED)
				designPlan$.setParameterType("numberOfPatientsGroup2", C_PARAM_GENERATED)
			}
			designPlan$.setParameterType("expectedPatientsH0", C_PARAM_GENERATED)
			designPlan$.setParameterType("expectedPatientsH01", C_PARAM_GENERATED)
			designPlan$.setParameterType("expectedPatientsH1", C_PARAM_GENERATED)
		}
			
		return(designPlan)
	}
	
	if (.isTrialDesignPlanSurvival(designPlan)) {
		
		sampleSizeFixed <- .getSampleSizeFixedSurvival(
			alpha = designPlan$getAlpha(), 
			beta = designPlan$getBeta(), 
			sided = designPlan$getSided(), 	
			typeOfComputation = designPlan$typeOfComputation, 
			thetaH0 = designPlan$thetaH0, 
			pi1 = designPlan$pi1,
			pi2 = designPlan$pi2, 
			allocationRatioPlanned = designPlan$allocationRatioPlanned,
			accountForObservationTimes = designPlan$accountForObservationTimes,
			eventTime = designPlan$eventTime,
			accrualTime = designPlan$accrualTime,
			followUpTime = designPlan$followUpTime,
			maxNumberOfPatients = designPlan$maxNumberOfPatients,
			dropOutRate1 = designPlan$dropOutRate1,
			dropOutRate2 = designPlan$dropOutRate2,
			dropOutTime = designPlan$dropOutTime)
		
		# Fixed	
		designPlan$hazardRatio = sampleSizeFixed$hazardRatio
		designPlan$omega = sampleSizeFixed$omega
		designPlan$calculateFollowUpTime = sampleSizeFixed$calculateFollowUpTime
		designPlan$eventsFixed = sampleSizeFixed$eventsFixed
		designPlan$maxNumberOfPatients = sampleSizeFixed$maxNumberOfPatients
		designPlan$nFixed1 = sampleSizeFixed$nFixed1
		designPlan$nFixed2 = sampleSizeFixed$nFixed2
		designPlan$nFixed <- designPlan$nFixed1 + designPlan$nFixed2
		
		designPlan$.setParameterType("hazardRatio", C_PARAM_GENERATED)
		designPlan$.setParameterType("omega", C_PARAM_GENERATED)
		designPlan$.setParameterType("calculateFollowUpTime", 
			ifelse(designPlan$calculateFollowUpTime == FALSE, C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
		designPlan$.setParameterType("eventsFixed", C_PARAM_GENERATED)
		designPlan$.setParameterType("nFixed1", C_PARAM_GENERATED)
		designPlan$.setParameterType("nFixed2", C_PARAM_GENERATED)
		designPlan$.setParameterType("nFixed", C_PARAM_GENERATED)
		
		.setFollowUpTime(designPlan, sampleSizeFixed, userDefinedMaxNumberOfPatientsEnabled) 
		
		if (length(designPlan$allocationRatioPlanned) != length(sampleSizeFixed$allocationRatioPlanned) ||
			sum(designPlan$allocationRatioPlanned == sampleSizeFixed$allocationRatioPlanned) != length(designPlan$allocationRatioPlanned)) {
			designPlan$allocationRatioPlanned = sampleSizeFixed$allocationRatioPlanned
			designPlan$.setParameterType("allocationRatioPlanned", C_PARAM_GENERATED)
		}
		
		# Sequential
		if (designPlan$.design$kMax > 1) {
			designCharacteristics <- getDesignCharacteristics(designPlan$.design)
			sampleSizeSequential <- .getSampleSizeSequentialSurvival(sampleSizeFixed, designCharacteristics)
			
			designPlan$informationRates = sampleSizeSequential$informationRates
			designPlan$numberOfPatients = sampleSizeSequential$numberOfPatients
			designPlan$numberOfPatientsGroup1 = sampleSizeSequential$numberOfPatientsGroup1
			designPlan$numberOfPatientsGroup2 = sampleSizeSequential$numberOfPatientsGroup2
			designPlan$expectedEventsH0 = sampleSizeSequential$expectedEventsH0
			designPlan$expectedEventsH01 = sampleSizeSequential$expectedEventsH01
			designPlan$expectedEventsH1 = sampleSizeSequential$expectedEventsH1
			designPlan$eventsOverStages = sampleSizeSequential$eventsOverStages
			
			designPlan$.setParameterType("informationRates", C_PARAM_GENERATED)
			designPlan$.setParameterType("numberOfPatients", C_PARAM_GENERATED)
			designPlan$.setParameterType("numberOfPatientsGroup1", C_PARAM_GENERATED)
			designPlan$.setParameterType("numberOfPatientsGroup2", C_PARAM_GENERATED)
			designPlan$.setParameterType("expectedEventsH0", C_PARAM_GENERATED)
			designPlan$.setParameterType("expectedEventsH01", C_PARAM_GENERATED)
			designPlan$.setParameterType("expectedEventsH1", C_PARAM_GENERATED)
			designPlan$.setParameterType("eventsOverStages", C_PARAM_GENERATED)
			
			if (sampleSizeFixed$accountForObservationTimes) {
				designPlan$analysisTimes = sampleSizeSequential$analysisTimes
				designPlan$expectedNumberOfPatientsH1 = sampleSizeSequential$expectedNumberOfPatientsH1
				designPlan$studyDurationH1 = sampleSizeSequential$studyDurationH1
				
				designPlan$.setParameterType("analysisTimes", C_PARAM_GENERATED)
				designPlan$.setParameterType("expectedNumberOfPatientsH1", C_PARAM_GENERATED)
				designPlan$.setParameterType("studyDurationH1", C_PARAM_GENERATED)
			}
			
			.setFollowUpTime(designPlan, sampleSizeSequential, userDefinedMaxNumberOfPatientsEnabled) 
		}
		
		return(designPlan)
	}
	
	stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "unknown trial plan class '", class(designPlan), "'")
}

.getSampleSizeFixedMeans <- function(..., alpha = 0.025, beta = 0.2, sided = 1, 
		twoSidedPower = C_TWO_SIDED_POWER_DEFAULT,
		normalApproximation = FALSE, meanRatio = FALSE, 
		thetaH0 = 0, alternative = seq(0.2, 1, 0.2), 
		stDev = 1, groups = 2, allocationRatioPlanned = 1) {
	
	nFixed <- rep(NA_real_, length(alternative)) 
	
	for (i in 1:length(alternative)) {
		theta <- alternative[i]
		
		if (groups == 1) {
			
			if (sided == 1 || !twoSidedPower) {
				if (normalApproximation == FALSE) {
					up <- 2
					while (stats::pt(stats::qt(1 - alpha / sided, up - 1), max(0.001, up - 1), 
							sqrt(up) * (theta - thetaH0) / stDev) > beta) {
						up <- 2*up 
					}
					nFixed[i] <- .getOneDimensionalRoot(
						function(n) {
							return(stats::pt(stats::qt(1 - alpha / sided, max(0.001, n - 1)), 
											max(0.001, n - 1), sqrt(n)*(theta - thetaH0)/stDev) - beta)
						}, lower = 0.001, upper = up, tolerance = 1E-4
					)
				} else {
					nFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2/((theta - thetaH0)/stDev)^2
				}
			} else {
				up <- 2
				while (stats::pt(stats::qt(1 - alpha/2, max(0.001, up - 1)), max(0.001, up - 1), 
						sqrt(up)*(theta - thetaH0)/stDev) - 
						stats::pt(-stats::qt(1 - alpha/2, max(0.001, up - 1)), 
						max(0.001, up - 1), sqrt(up)*(theta - thetaH0) / stDev) > beta) {
					up <- 2 * up 
				}
				if (normalApproximation == FALSE) {
					nFixed[i] <- .getOneDimensionalRoot(
						function(n) {
							return(stats::pt(stats::qt(1 - alpha/2, max(0.001, n - 1)), max(0.001, n - 1), 
								sqrt(n)*(theta - thetaH0)/stDev) - 
								stats::pt(-stats::qt(1 - alpha/2, max(0.001, n - 1)), 
								max(0.001, n - 1), sqrt(n) * (theta - thetaH0)/stDev) - beta)
						}, lower = 0.001, upper = up, tolerance = 1E-4
					)
				} else {
					nFixed[i] <- .getOneDimensionalRoot(
						function(n) {
							return(stats::pnorm(stats::qnorm(1 - alpha/2) - sqrt(n)*(theta - thetaH0)/stDev) - 
								stats::pnorm(-stats::qnorm(1 - alpha/2) - sqrt(n)*(theta - thetaH0) / stDev) - beta)
						}, lower = 0.001, upper = up, tolerance = 1E-4
					)
				}
			}
		}
		
		else if (groups == 2) {
			
			if (sided == 1 || !twoSidedPower) {
				if (!meanRatio) { 
					# allocationRatioPlanned = 0 provides optimum sample size					
					if (allocationRatioPlanned == 0) {
						allocationRatioPlanned <- 1
					}
					if (normalApproximation == FALSE) {
						up <- 2
						while (stats::pt(stats::qt(1 - alpha / sided, up * (1 + allocationRatioPlanned) - 2), 
								up * (1 + allocationRatioPlanned) - 2, 
								sqrt(up) * sqrt(allocationRatioPlanned / (1 + allocationRatioPlanned)) * 
								(theta - thetaH0)/stDev) > beta) {
							up <- 2 * up 
						}
						n2Fixed <- .getOneDimensionalRoot(
							function(x) {
								return(stats::pt(stats::qt(1 - alpha / sided, max(0.001, x * (1 + allocationRatioPlanned) - 2)), 
									max(0.001, x * (1 + allocationRatioPlanned) - 2), 
									sqrt(x) * sqrt(allocationRatioPlanned / 
									(1 + allocationRatioPlanned)) * (theta - thetaH0)/stDev) - beta)
							}, lower = 0.001, upper = up, tolerance = 1E-4
						)
						nFixed[i] <- n2Fixed*(1 + allocationRatioPlanned)
					} else {
						nFixed[i] <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * 
							(stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2 / ((theta - thetaH0)/stDev)^2
					}
				} else {	
					
					# allocationRatioPlanned = 0 provides optimum sample size					
					if (allocationRatioPlanned == 0) {
						allocationRatioPlanned <- 1/thetaH0
					}
					if (!normalApproximation) {
						up <- 2
						while (stats::pt(stats::qt(1 - alpha / sided, up*(1 + allocationRatioPlanned) - 2), 
								up * (1 + allocationRatioPlanned) - 2, 
								sqrt(up * allocationRatioPlanned / (1 + allocationRatioPlanned * thetaH0^2)) * 
								(theta - thetaH0) / stDev) > beta) {
							up <- 2 * up 
						}
						n2Fixed <- .getOneDimensionalRoot(
							function(n2) {
								return(stats::pt(stats::qt(1 - alpha / sided, max(0.001, n2 * (1 + allocationRatioPlanned) - 2)), 
									max(0.001, n2 * (1 + allocationRatioPlanned) - 2), 
									sqrt(n2 * allocationRatioPlanned / 
									(1 + allocationRatioPlanned * thetaH0^2)) * 
									(theta - thetaH0) / stDev) - beta)
							}, lower = 0.001, upper = up, tolerance = 1E-4
						)
						nFixed[i] <- n2Fixed * (1 + allocationRatioPlanned)
					} else {
						nFixed[i] <- (1 + 1/allocationRatioPlanned + thetaH0^2 * 
							(1 + allocationRatioPlanned)) * 
							(stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2 / ((theta - thetaH0) / stDev)^2
					}
				}
			} else {
				if (!normalApproximation) {
					if (allocationRatioPlanned == 0) {
						allocationRatioPlanned <- 1
					}
					up <- 2
					while (stats::pt(stats::qt(1 - alpha/2, max(0.001, up * (1 + allocationRatioPlanned) - 2)), 
							max(0.001, up * (1 + allocationRatioPlanned) - 2), 
							sqrt(up) * sqrt(allocationRatioPlanned / (1 + allocationRatioPlanned)) * 
							(theta - thetaH0)/stDev) - stats::pt(-stats::qt(1 - alpha/2, 
							up * (1 + allocationRatioPlanned) - 2), up * (1 + allocationRatioPlanned) - 2, 
							sqrt(up) * sqrt(allocationRatioPlanned / (1 + allocationRatioPlanned)) * 
							(theta - thetaH0) / stDev) > beta) {
						up <- 2*up 
					}
					n2Fixed <- .getOneDimensionalRoot(
						function(n2) {
							return(stats::pt(stats::qt(1 - alpha / 2, max(0.001, n2 * (1 + allocationRatioPlanned) - 2)), 
								max(0.001, n2*(1 + allocationRatioPlanned) - 2), 
								sqrt(n2) * sqrt(allocationRatioPlanned / (1 + allocationRatioPlanned)) * 
									(theta - thetaH0) / stDev) - stats::pt(-stats::qt(1 - alpha / 2, 
									max(0.001, n2 * (1 + allocationRatioPlanned) - 2)), 
								max(0.001, n2 * (1 + allocationRatioPlanned) - 2), 
								sqrt(n2) * sqrt(allocationRatioPlanned/(1 + allocationRatioPlanned)) * 
									(theta - thetaH0) / stDev) - beta)
						}, lower = 0.001, upper = up, tolerance = 1E-4
					)
					nFixed[i] <- n2Fixed*(1 + allocationRatioPlanned)
					
				} else {
					
					up <- 2
					while (stats::pnorm(stats::qnorm(1 - alpha / 2) - sqrt(up / 4) * (theta - thetaH0) / stDev) - 
							stats::pnorm(-stats::qnorm(1 - alpha / 2) - sqrt(up / 4) * (theta - thetaH0) / stDev) > beta) {
						up <- 2 * up 
					}
					
					nFixed[i] <- (1 + allocationRatioPlanned)^2 / (4 * allocationRatioPlanned) * 
						.getOneDimensionalRoot(
							function(n) {
								return(stats::pnorm(stats::qnorm(1 - alpha / 2) - sqrt(n / 4) * (theta - thetaH0) / stDev) - 
									stats::pnorm(-stats::qnorm(1 - alpha / 2) - sqrt(n / 4) * (theta - thetaH0) / stDev) - beta)
							}, lower = 0.001, upper = up, tolerance = 1E-4
						)
				}
			}
		}
	}
	
	if (groups == 1) {
		nFixed <- round(nFixed, 1)
		return(list(alpha = alpha, 
			beta = beta, 
			sided = sided, 
			groups = groups, 
			thetaH0 = thetaH0, 
			alternative = alternative, 
			stDev = stDev, 
			normalApproximation = normalApproximation, 
			nFixed = nFixed)
		)
	}
	if (groups == 2) {
		n1Fixed <- nFixed*allocationRatioPlanned / (1 + allocationRatioPlanned)
		n2Fixed <- n1Fixed / allocationRatioPlanned 
		n1Fixed <- round(n1Fixed, 1)
		n2Fixed <- round(n2Fixed, 1)
		nFixed <- round(nFixed, 1)
		return(list(alpha = alpha, 
			beta = beta, 
			sided = sided, 
			groups = groups, 
			allocationRatioPlanned = allocationRatioPlanned, 
			thetaH0 = thetaH0, 
			meanRatio = meanRatio, 
			alternative = alternative, 
			stDev = stDev, 
			normalApproximation = normalApproximation, 
			n1Fixed = n1Fixed, 
			n2Fixed = n2Fixed, 
			nFixed = nFixed)
		)
	}
}

.getSampleSizeSequentialMeans <- function(fixedSampleSize, designCharacteristics) {
	
	kMax <- designCharacteristics$.design$kMax
	numberOfPatients <- matrix(NA_real_, kMax, length(fixedSampleSize$alternative))
	numberOfPatientsGroup1 <- matrix(NA_real_, kMax, length(fixedSampleSize$alternative))
	numberOfPatientsGroup2 <- matrix(NA_real_, kMax, length(fixedSampleSize$alternative))
	maxNumberOfPatients  <- rep(NA_real_, length(fixedSampleSize$alternative))
	expectedPatientsH0  <- rep(NA_real_, length(fixedSampleSize$alternative))
	expectedPatientsH01  <- rep(NA_real_, length(fixedSampleSize$alternative))
	expectedPatientsH1  <- rep(NA_real_, length(fixedSampleSize$alternative))
	
	informationRates <- designCharacteristics$information/designCharacteristics$shift
	
	for (i in (1:length(fixedSampleSize$alternative))) {
		
		maxNumberOfPatients[i] <- fixedSampleSize$nFixed[i] * designCharacteristics$inflationFactor
		
		numberOfPatients[, i] <- maxNumberOfPatients[i] * 
			c(informationRates[1], (informationRates[2:kMax] - informationRates[1:(kMax - 1)])) 
		
		expectedPatientsH0[i] <- designCharacteristics$averageSampleNumber0 * fixedSampleSize$nFixed[i]
		expectedPatientsH01[i] <- designCharacteristics$averageSampleNumber01 * fixedSampleSize$nFixed[i]
		expectedPatientsH1[i] <- designCharacteristics$averageSampleNumber1 * fixedSampleSize$nFixed[i]
		
		if (fixedSampleSize$groups == 2) {
			if (length(fixedSampleSize$allocationRatioPlanned) > 1) {
				allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned[i]
			} else {
				allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned
			}
			numberOfPatientsGroup1[, i] <- numberOfPatients[, i] * allocationRatioPlanned / (1 + allocationRatioPlanned)
			numberOfPatientsGroup2[, i] <- numberOfPatients[, i] /	(1 + allocationRatioPlanned)
		}
	}
	
	if (fixedSampleSize$groups == 1) {
		return(list(alpha = fixedSampleSize$alpha, 
			beta = fixedSampleSize$beta, 
			sided = fixedSampleSize$sided, 
			groups = fixedSampleSize$groups, 
			thetaH0 = fixedSampleSize$thetaH0, 
			alternative = fixedSampleSize$alternative, 
			stDev = fixedSampleSize$stDev, 
			normalApproximation = fixedSampleSize$normalApproximation,
			informationRates = matrix(informationRates, ncol = 1),
			maxNumberOfPatients = maxNumberOfPatients,
			numberOfPatients = numberOfPatients,
			expectedPatientsH0 = expectedPatientsH0,
			expectedPatientsH01 = expectedPatientsH01,
			expectedPatientsH1 = expectedPatientsH1
		))
	} else {
		return(list(alpha = fixedSampleSize$alpha, 
			beta = fixedSampleSize$beta, 
			sided = fixedSampleSize$sided, 
			groups = fixedSampleSize$groups,
			allocationRatioPlanned = fixedSampleSize$allocationRatioPlanned,
			thetaH0 = fixedSampleSize$thetaH0, 
			alternative = fixedSampleSize$alternative, 
			stDev = fixedSampleSize$stDev, 
			normalApproximation = fixedSampleSize$normalApproximation,
			meanRatio = fixedSampleSize$meanRatio, 
			informationRates = matrix(informationRates, ncol = 1),		
			maxNumberOfPatients = maxNumberOfPatients,					
			numberOfPatients = numberOfPatients,
			numberOfPatientsGroup1 = numberOfPatientsGroup1,
			numberOfPatientsGroup2 = numberOfPatientsGroup2,
			expectedPatientsH0 = expectedPatientsH0,
			expectedPatientsH01 = expectedPatientsH01,
			expectedPatientsH1 = expectedPatientsH1
		))
	}	
}

# 
# @title 
# Get Farrington Manning Values
# 
# @description 
# Calculates and returns the maximum likelihood estimates under H0.
# 
# @details 
# Calculation of maximum likelihood estimates under H0: 
# pi1 - pi2 = theta0 or H0: pi1 / pi2 = theta0
# 
# @references 
# Farrington & Manning (1990)
# Wassmer (2003)
# 
# @keywords internal
# 
.getFarringtonManningValues <- function(rate1, rate2, theta0, allocation, method = "diff") {
	
	if (method != "diff") {
		a <- 1 + 1/allocation
		b <- -((1 + rate2/allocation) * theta0  + 1/allocation + rate1)
		c <-  (rate1 + rate2/allocation) * theta0
		ml1 <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)
		ml2 <- ml1 / theta0
		return(list(theta0 = theta0, method = method, ml1 = ml1, ml2 = ml2))
	}
	
	if (theta0 == 0) {
		ml1 <- (allocation * rate1 + rate2) / (1 + allocation)
		ml2 <- ml1
		return(list(theta0 = theta0, method = method, ml1 = ml1, ml2 = ml2))
	}
	
	a <- 1 + 1/allocation
	b <- -(1 + 1/allocation + rate1 + rate2/allocation + theta0 * (1/allocation + 2))
	c <- theta0^2 + theta0 * (2 * rate1 + 1/allocation + 1) + rate1 + rate2/allocation
	d <- -theta0 * (1 + theta0) * rate1
	
	v <- b^3 / (3 * a)^3 - b * c / (6 * a^2) + d / (2 * a)
	if (v == 0) {
		u <- sqrt(b^2 / (3 * a)^2 - c / (3 * a))
		w <- acos(-1) / 2
	} else {
		u <- sign(v) * sqrt(b^2 / (3 * a)^2 - c / (3 * a))
		w <- 1 / 3 * (acos(-1) + acos(v / u^3))
	}
	
	ml1 <- min(max(0, 2 * u * cos(w) - b / (3 * a)), 1)
	ml2 <- min(max(0, ml1 - theta0), 1)
	
	return(list(theta0 = theta0, method = method, ml1 = ml1, ml2 = ml2))
}

.getSampleSizeFixedRates <- function(..., alpha = 0.025, beta = 0.2, sided = 1, 
		normalApproximation = TRUE, riskRatio = FALSE, 
		thetaH0 = 0, pi1 = seq(0.4, 0.6, 0.1), pi2 = 0.2, 
		groups = 2, allocationRatioPlanned = 1) {
		
	if (groups == 1) {
		
		nFixed <- rep(NA_real_, length(pi1))
		
		for (i in 1:length(pi1)) {		
			if (normalApproximation) {
				nFixed[i] <- (stats::qnorm(1 - alpha / sided)*sqrt(thetaH0*(1 - thetaH0)) + 
					stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i])))^2 /
					(pi1[i] - thetaH0)^2
			} else {
				ifelse(pi1[i] > thetaH0, lower.tail <- FALSE, lower.tail <- TRUE) 
				iterations <- 1					
				if (lower.tail) {
					nup <- 2
					while ((stats::pbinom(stats::qbinom(alpha, nup, thetaH0, lower.tail = lower.tail) - 1, nup, pi1[i], lower.tail = lower.tail) < 1 - beta) && (iterations <= 50)) {
						nup <- 2*nup
						iterations <- iterations + 1
					}
					if (iterations > 50) {
						nFixed[i] <- Inf
					} else {
						prec <- 2
						nlow <- 2
						while (prec > 1) {
							nFixed[i] <- round((nlow + nup)/2)
							ifelse(stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail) - 1, 
								nFixed[i], pi1[i], 
								lower.tail = lower.tail) < 1 - beta, 
								nlow <- nFixed[i], nup <- nFixed[i])
							prec <- nup - nlow
						}
						if(stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail) - 1, 
							nFixed[i], pi1[i], lower.tail = lower.tail) < 1 - beta) {
							nFixed[i] <- nFixed[i] + 1
						}
					}
				} else {
					nup <- 2
					while ((stats::pbinom(stats::qbinom(alpha, nup, thetaH0, lower.tail = lower.tail), 
							nup, pi1[i], lower.tail = lower.tail) < 1 - beta) && (iterations <= 50)) {
						nup <- 2 * nup
						iterations <- iterations + 1						
					}
					if (iterations > 50) {
						nFixed[i] <- Inf
					} else {
						prec <- 2
						nlow <- 2
						while (prec > 1) {
							nFixed[i] <- round((nlow + nup)/2)
							ifelse(stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail), 
								nFixed[i], pi1[i], lower.tail = lower.tail) < 1 - beta, 
								nlow <- nFixed[i], nup <- nFixed[i])
							prec <- nup - nlow
						}
						if(stats::pbinom(stats::qbinom(alpha, nFixed[i], thetaH0, lower.tail = lower.tail), 
								nFixed[i], pi1[i], lower.tail = lower.tail) < 1 - beta) {
							nFixed[i] <- nFixed[i] + 1
						}
					}
				}
			}
		}	
		nFixed <- round(nFixed, 1)
		
		return(list(alpha = alpha, 
			beta = beta, 
			sided = sided, 
			groups = groups, 
			thetaH0 = thetaH0, 
			pi1 = pi1, 
			normalApproximation = normalApproximation, 
			nFixed = nFixed
		))
	}
	
	if (groups == 2) {
		
		n1Fixed <- rep(NA_real_, length(pi1)) 
		n2Fixed <- rep(NA_real_, length(pi1))
		nFixed <- rep(NA_real_, length(pi1))
		if (allocationRatioPlanned == 0) {
			allocationRatioPlannedVec <- rep(NA_real_, length(pi1))
		}
		
		for (i in 1:length(pi1)) {
			if (!riskRatio) { 
				# allocationRatioPlanned = 0 provides optimum sample size
				if (allocationRatioPlanned == 0) {
					allocationRatioPlannedVec[i] <- stats::optimize(function(x) { 
						fm <- .getFarringtonManningValues(pi1[i], pi2, thetaH0, x, method = "diff")
						n1 <- (stats::qnorm(1 - alpha / sided)*sqrt(fm$ml1*(1 - fm$ml1) + fm$ml2*(1 - fm$ml2) * x) + 
							stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i]) + pi2*(1 - pi2) * x))^2 /
							(pi1[i] - pi2 - thetaH0)^2
						return((1 + x)/x*n1)	
					}, interval = c(0, 5), tol = 0.0001)$minimum
					fm <- .getFarringtonManningValues(pi1[i], pi2, thetaH0, 
						allocationRatioPlannedVec[i], method = "diff")
					n1Fixed[i] <- (stats::qnorm(1 - alpha / sided)*sqrt(fm$ml1*(1 - fm$ml1) + 
						fm$ml2*(1 - fm$ml2) * allocationRatioPlannedVec[i]) + 
						stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i]) + pi2*(1 - pi2) * 
						allocationRatioPlannedVec[i]))^2 /
						(pi1[i] - pi2 - thetaH0)^2
				} else {
					fm <- .getFarringtonManningValues(pi1[i], pi2, thetaH0, allocationRatioPlanned, method = "diff")
					n1Fixed[i] <- (stats::qnorm(1 - alpha / sided)*sqrt(fm$ml1*(1 - fm$ml1) + 
						fm$ml2*(1 - fm$ml2) * allocationRatioPlanned) + 
						stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i]) + pi2*(1 - pi2) * allocationRatioPlanned))^2 /
						(pi1[i] - pi2 - thetaH0)^2
				}	
			} else {	
				if (allocationRatioPlanned == 0) {
					# allocationRatioPlanned = 0 provides optimum sample size					
					allocationRatioPlannedVec[i] <- stats::optimize(function(x) { 
						fm <- .getFarringtonManningValues(pi1[i], pi2 , thetaH0, x, method = "ratio")
						n1 <- (stats::qnorm(1 - alpha / sided)*sqrt(fm$ml1*(1 - fm$ml1) + fm$ml2*(1 - fm$ml2)*x*thetaH0^2) + 
							stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i]) + pi2*(1 - pi2)*x*thetaH0^2))^2 /
							(pi1[i] - thetaH0*pi2)^2
						return((1 + x)/x*n1)	
					}, interval = c(0, 5), tol = 0.0001)$minimum
					fm <- .getFarringtonManningValues(pi1[i], pi2, thetaH0, 
						allocationRatioPlannedVec[i], method = "ratio")
					n1Fixed[i] <- (stats::qnorm(1 - alpha / sided)*sqrt(fm$ml1*(1 - fm$ml1) + 
						fm$ml2*(1 - fm$ml2)*allocationRatioPlannedVec[i]*thetaH0^2) + 
						stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i]) + pi2*(1 - pi2) * 
						allocationRatioPlannedVec[i]*thetaH0^2))^2 /
						(pi1[i] - thetaH0*pi2)^2
				} else {
					fm <- .getFarringtonManningValues(pi1[i], pi2, thetaH0, allocationRatioPlanned, method = "ratio")
					n1Fixed[i] <- (stats::qnorm(1 - alpha / sided)*sqrt(fm$ml1*(1 - fm$ml1) + 
						fm$ml2*(1 - fm$ml2)*allocationRatioPlanned*thetaH0^2) + 
						stats::qnorm(1 - beta)*sqrt(pi1[i]*(1 - pi1[i]) + pi2*(1 - pi2) * 
						allocationRatioPlanned*thetaH0^2))^2 /
						(pi1[i] - thetaH0*pi2)^2
				}	
			}
		}
		if (allocationRatioPlanned == 0) {
			allocationRatioPlanned <- round(allocationRatioPlannedVec, 3)
		}
		
		n2Fixed <- n1Fixed / allocationRatioPlanned
		nFixed <- n1Fixed + n2Fixed
		
		n1Fixed <- round(n1Fixed, 1)
		n2Fixed <- round(n2Fixed, 1)
		nFixed <- round(nFixed, 1)
		
		return(list(alpha = alpha, 
			beta = beta, 
			sided = sided, 
			groups = groups, 
			allocationRatioPlanned = allocationRatioPlanned, 
			thetaH0 = thetaH0, 					
			pi1 = pi1, 
			pi2 = pi2, 
			normalApproximation = normalApproximation, 
			riskRatio = riskRatio, 
			n1Fixed = n1Fixed, 
			n2Fixed = n2Fixed, 
			nFixed = nFixed
		))
	}
}


.getSampleSizeSequentialRates <- function(fixedSampleSize, designCharacteristics) {
	
	kMax <- designCharacteristics$.design$kMax
	numberOfPatients <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
	numberOfPatientsGroup1 <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
	numberOfPatientsGroup2 <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
	maxNumberOfPatients  <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedPatientsH0  <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedPatientsH01  <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedPatientsH1  <- rep(NA_real_, length(fixedSampleSize$pi1))
	
	informationRates <- designCharacteristics$information/designCharacteristics$shift
	
	for (i in (1:length(fixedSampleSize$pi1))) {
		
		maxNumberOfPatients[i] <- fixedSampleSize$nFixed[i]*designCharacteristics$inflationFactor
		
		numberOfPatients[, i] <- maxNumberOfPatients[i]*c(informationRates[1], 
			(informationRates[2:kMax] - informationRates[1:(kMax - 1)])) 
		
		expectedPatientsH0[i] <- designCharacteristics$averageSampleNumber0 * fixedSampleSize$nFixed[i]
		expectedPatientsH01[i] <- designCharacteristics$averageSampleNumber01 * fixedSampleSize$nFixed[i]
		expectedPatientsH1[i] <- designCharacteristics$averageSampleNumber1 * fixedSampleSize$nFixed[i]
		
		if (fixedSampleSize$groups == 2) {
			if (length(fixedSampleSize$allocationRatioPlanned) > 1) {
				allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned[i]
			} else {
				allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned
			}
			numberOfPatientsGroup1[, i] <- numberOfPatients[, i] * allocationRatioPlanned / 
				(1 + allocationRatioPlanned)
			numberOfPatientsGroup2[, i] <- numberOfPatients[, i] /	(1 + allocationRatioPlanned)
		}
	}
	
	if (fixedSampleSize$groups == 1) {
		return(list(alpha = fixedSampleSize$alpha, 
			beta = fixedSampleSize$beta, 
			sided = fixedSampleSize$sided, 
			groups = fixedSampleSize$groups, 
			thetaH0 = fixedSampleSize$thetaH0, 
			pi1 = fixedSampleSize$pi1, 
			normalApproximation = fixedSampleSize$normalApproximation,
			informationRates = matrix(informationRates, ncol = 1),
			maxNumberOfPatients = maxNumberOfPatients,
			numberOfPatients = numberOfPatients,
			expectedPatientsH0 = expectedPatientsH0,
			expectedPatientsH01 = expectedPatientsH01,
			expectedPatientsH1 = expectedPatientsH1
		))
	} else {
		return(list(alpha = fixedSampleSize$alpha, 
			beta = fixedSampleSize$beta, 
			sided = fixedSampleSize$sided, 
			groups = fixedSampleSize$groups,
			allocationRatioPlanned = fixedSampleSize$allocationRatioPlanned,
			thetaH0 = fixedSampleSize$thetaH0, 
			pi1 = fixedSampleSize$pi1,
			pi2 = fixedSampleSize$pi2,
			normalApproximation = fixedSampleSize$normalApproximation,
			riskRatio = fixedSampleSize$riskRatio, 
			informationRates = matrix(informationRates, ncol = 1),		
			maxNumberOfPatients = maxNumberOfPatients,					
			numberOfPatients = numberOfPatients,
			numberOfPatientsGroup1 = numberOfPatientsGroup1,
			numberOfPatientsGroup2 = numberOfPatientsGroup2,
			expectedPatientsH0 = expectedPatientsH0,
			expectedPatientsH01 = expectedPatientsH01,
			expectedPatientsH1 = expectedPatientsH1
		))
	}	
}

.getEventProbabilities <- function(time, accrualTime, lambda, phi, allocationRatioPlanned) {
	if (time < accrualTime) {
		eventProbs <- lambda / (lambda + phi)*
				(time / accrualTime - (1 - exp(-(lambda + phi)*time))/(accrualTime*(lambda + phi)))
	} else {
		eventProbs <- lambda / (lambda + phi)*
				(1 - exp(-(lambda + phi)*time)*(exp((lambda + phi)*accrualTime) - 1)/
					(accrualTime*(lambda + phi)));
	}
	return((allocationRatioPlanned*eventProbs[1] + eventProbs[2])/(1 + allocationRatioPlanned))
}

.getSampleSizeFixedSurvival <- function(..., alpha = 0.025, beta = 0.2, sided = 1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi2 = 0.2, pi1 = seq(0.4, 0.6, 0.1), 
		allocationRatioPlanned = 1, accountForObservationTimes = FALSE, 
		eventTime = 12, accrualTime = 12, followUpTime = 24, maxNumberOfPatients = 0, 
		dropOutRate1 = 0, dropOutRate2 = 0, dropOutTime = 12) {
	
	eventsFixed <- rep(NA_real_, length(pi1)) # number of events 
	nFixed <- rep(NA_real_, length(pi1))	  # number of patients
	omega <- rep(NA_real_, length(pi1))		  # probability of an event 
	
	hazardRatio <- log(1 - pi1) / log(1 - pi2)	# hazard ratios vector 
	
	if (sided == 1 && any(hazardRatio - thetaH0 == 0)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"alternative not correctly specified ", 
			"(for one-sided case each hazard ratio (", .arrayToString(hazardRatio), 
			") must be unequal to 'thetaH0' (", thetaH0, ")")
	}
	
	if (allocationRatioPlanned == 0) {
		allocationRatioPlannedVec <- rep(NA_real_, length(pi1))
	}
	
	if (maxNumberOfPatients > 0) {
		timeVec <- rep(NA_real_, length(pi1))
	}
	
	calculateFollowUpTime <- FALSE
	
	for (i in 1:length(pi1)) {
		
		lambda = -c(log(1 - pi1[i]), log(1 - pi2)) / eventTime 
		phi <-  -c(log(1 - dropOutRate1), log(1 - dropOutRate2)) / dropOutTime
		
		if (maxNumberOfPatients == 0) {
			
			if (allocationRatioPlanned == 0) {
				# allocationRatioPlanned = 0 provides optimum sample size	
				allocationRatioPlannedVec[i] <- stats::optimize(function(x) { 
					if (typeOfComputation == "Schoenfeld") {
						numberEvents <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2 / 
							(log(hazardRatio[i]) - log(thetaH0))^2*(1 + x)^2/x
					}
					if (typeOfComputation == "Freedman") {
						numberEvents <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
							(1 + hazardRatio[i]*x)^2/(1 - hazardRatio[i])^2/x
					}
					if (typeOfComputation == "HsiehFreedman") {
						numberEvents <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
							(1 + hazardRatio[i])^2/(1 - hazardRatio[i])^2*(1 + x)^2/(4*x)
					}
					if (!accountForObservationTimes) {
						probEvent <- (x*pi1[i] + pi2)/(1 + x)
					} else {
						probEvent <- .getEventProbabilities(accrualTime + followUpTime, accrualTime, lambda, phi, x)
					}	
					return(numberEvents/probEvent)	
				}, interval = c(0, 5), tol = 0.0001)$minimum
				
				if (typeOfComputation == "Schoenfeld") {
					eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2/(log(hazardRatio[i]) - log(thetaH0))^2*
						(1 + allocationRatioPlannedVec[i])^2/allocationRatioPlannedVec[i]
				}
				if (typeOfComputation == "Freedman") {
					eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
						(1 + hazardRatio[i]*allocationRatioPlannedVec[i])^2/(1 - hazardRatio[i])^2 / 
						allocationRatioPlannedVec[i]
				}
				if (typeOfComputation == "HsiehFreedman") {
					eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
						(1 + hazardRatio[i])^2/(1 - hazardRatio[i])^2*
						(1 + allocationRatioPlannedVec[i])^2/(4*allocationRatioPlannedVec[i])
				}
				if (!accountForObservationTimes) {
					omega[i] <- (allocationRatioPlannedVec[i]*pi1[i] + pi2)/(1 + allocationRatioPlannedVec[i])
				} else {
					omega[i] <- .getEventProbabilities(accrualTime + followUpTime, 
						accrualTime, lambda, phi, allocationRatioPlannedVec[i])
				} 
				
				
			} else {
				
				if (typeOfComputation == "Schoenfeld") {
					eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2/(log(hazardRatio[i]) - 
						log(thetaH0))^2 * (1 + allocationRatioPlanned)^2/allocationRatioPlanned
				}
				if (typeOfComputation == "Freedman") {
					eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
						(1 + hazardRatio[i]*allocationRatioPlanned)^2/(1 - hazardRatio[i])^2 / 
						allocationRatioPlanned
				}
				if (typeOfComputation == "HsiehFreedman") {
					eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
						(1 + hazardRatio[i])^2/(1 - hazardRatio[i])^2*
						(1 + allocationRatioPlanned)^2/(4*allocationRatioPlanned)
				}
				
				if (!accountForObservationTimes) {
					omega[i] <- (allocationRatioPlanned*pi1[i] + pi2)/(1 + allocationRatioPlanned)
				} else {
					omega[i] <- .getEventProbabilities(accrualTime + followUpTime, accrualTime, 
						lambda, phi, allocationRatioPlanned)
				}
				
			}
			
			nFixed[i] <- eventsFixed[i]/omega[i]
			
		} else {
			
			calculateFollowUpTime <- TRUE
			
			if (typeOfComputation == "Schoenfeld") {
				eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2/(log(hazardRatio[i]) - 
					log(thetaH0))^2 * (1 + allocationRatioPlanned)^2/allocationRatioPlanned
			}
			if (typeOfComputation == "Freedman") {
				eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
						(1 + hazardRatio[i]*allocationRatioPlanned)^2/(1 - hazardRatio[i])^2/allocationRatioPlanned
			}
			if (typeOfComputation == "HsiehFreedman") {
				eventsFixed[i] <- (stats::qnorm(1 - alpha / sided) + stats::qnorm(1 - beta))^2*
						(1 + hazardRatio[i])^2/(1 - hazardRatio[i])^2*
						(1 + allocationRatioPlanned)^2/(4*allocationRatioPlanned)
			}
			
			nFixed[i] <- maxNumberOfPatients
			if (eventsFixed[i] > maxNumberOfPatients) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					sprintf(paste0("the number of patients (%s) is smaller than the number ",
							"of events (%s) at stage %s"), 
						maxNumberOfPatients, eventsFixed[i], i))
			}
			
			up <- 2
			iterate <- 1
			while (eventsFixed[i]/.getEventProbabilities(up, accrualTime, lambda, phi, allocationRatioPlanned) > 
					maxNumberOfPatients) {
				up <- 2*up
				iterate <- iterate + 1
				if (iterate > 50) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"the number of patients is too small to reach maximum number of events ", 
						"(presumably due to drop-out rates)")
				}	
			}
			
			timeVec[i] <- .getOneDimensionalRoot(function(designCharacteristics) {
				eventsFixed[i]/.getEventProbabilities(designCharacteristics, accrualTime, lambda, phi, allocationRatioPlanned) - maxNumberOfPatients
			}, lower = 0, upper = up, tolerance = 1E-6)
			omega[i] <- .getEventProbabilities(timeVec[i], accrualTime, lambda, phi, allocationRatioPlanned)
		}	
	}	
	
	if (allocationRatioPlanned == 0) {
		allocationRatioPlanned <- round(allocationRatioPlannedVec, 3)
	}
	
	if (maxNumberOfPatients > 0) {
		followUpTime <- round(timeVec - accrualTime, 3)
	}
	
	eventsFixed <- round(eventsFixed, 1)
	nFixed <- round(nFixed, 1)
	n2Fixed <- nFixed/(1 + allocationRatioPlanned)
	n1Fixed <- n2Fixed*allocationRatioPlanned
	
	eventRatio <- allocationRatioPlanned * pi1 / pi2
	
	#nFixed <- ceiling(nFixed)
	
	if (!accountForObservationTimes) {
		return(list(alpha = alpha, 
			beta = beta, 
			sided = sided, 
			typeOfComputation = typeOfComputation, 
			accountForObservationTimes = accountForObservationTimes, 
			allocationRatioPlanned = allocationRatioPlanned,
			eventRatio = eventRatio,			
			thetaH0 = thetaH0, 
			pi1 = pi1,
			pi2 = pi2,
			hazardRatio = hazardRatio, 
			omega = omega, 
			calculateFollowUpTime = calculateFollowUpTime,
			eventsFixed = eventsFixed, 
			maxNumberOfPatients = nFixed, 
			nFixed1 = n1Fixed, 
			nFixed2 = n2Fixed
		))
	} else {
		return(list(alpha = alpha, 
			beta = beta, 
			sided = sided, 
			typeOfComputation = typeOfComputation, 
			accountForObservationTimes = accountForObservationTimes, 
			allocationRatioPlanned = allocationRatioPlanned, 
			eventRatio = eventRatio,
			thetaH0 = thetaH0, 
			pi1 = pi1,
			pi2 = pi2,
			hazardRatio = hazardRatio, 
			omega = omega, 
			eventTime = eventTime, 
			accrualTime = accrualTime, 
			followUpTime = followUpTime, 
			dropOutRate1 = dropOutRate1, 
			dropOutRate2 = dropOutRate2, 
			dropOutTime = dropOutTime,
			calculateFollowUpTime = calculateFollowUpTime,
			eventsFixed = eventsFixed, 
			maxNumberOfPatients = nFixed, 
			nFixed1 = n1Fixed, 
			nFixed2 = n2Fixed
		))
	}
}


.getSampleSizeSequentialSurvival <- function(fixedSampleSize, designCharacteristics) {
	
	kMax <- designCharacteristics$.design$kMax
	eventsOverStages <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
	analysisTimes <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
	numberOfPatients <- matrix(NA_real_, kMax, length(fixedSampleSize$pi1))
	followUpTime <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedEventsH0 <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedEventsH01 <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedEventsH1 <- rep(NA_real_, length(fixedSampleSize$pi1))
	expectedNumberOfPatientsH1 <- rep(NA_real_, length(fixedSampleSize$pi1))
	studyDurationH1 <- rep(NA_real_, length(fixedSampleSize$pi1))
	
	informationRates <- designCharacteristics$information/designCharacteristics$shift
	
	for (i in (1:length(fixedSampleSize$pi1))) {
		
		lambda = -c(log(1 - fixedSampleSize$pi1[i]), log(1 - fixedSampleSize$pi2)) / fixedSampleSize$eventTime 
		phi <-  -c(log(1 - fixedSampleSize$dropOutRate1), 
			log(1 - fixedSampleSize$dropOutRate2)) / fixedSampleSize$dropOutTime
		
		eventsOverStages[, i] <- fixedSampleSize$eventsFixed[i] * informationRates * 
			designCharacteristics$inflationFactor		
		
		if (!fixedSampleSize$accountForObservationTimes) {
			if (length(fixedSampleSize$allocationRatioPlanned) > 1) {
				allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned[i]
			} else {
				allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned
			}
			omega <- (allocationRatioPlanned*fixedSampleSize$pi1[i] + fixedSampleSize$pi2) / 
				(1 + allocationRatioPlanned)
			
			numberOfPatients[kMax, i] <- eventsOverStages[kMax, i] / omega
			
		} else {	
			
			if (fixedSampleSize$calculateFollowUpTime) {
				
				if (eventsOverStages[kMax, i] > fixedSampleSize$maxNumberOfPatients[i]) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						sprintf(paste0("the number of patients (%s) is smaller than the number ",
							"of events (%s) at stage %s"), 
							fixedSampleSize$maxNumberOfPatients[i], eventsOverStages[kMax, i], i))
				}
				
				up <- 2
				iterate <- 1
				while (eventsOverStages[kMax, i]/.getEventProbabilities(up, fixedSampleSize$accrualTime, 
						lambda, phi, fixedSampleSize$allocationRatioPlanned) > 
						fixedSampleSize$maxNumberOfPatients[i]) {
					up <- 2 * up
					iterate <- iterate + 1
					if (iterate > 50) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"the number of patients is too small to reach maximum number of events ", 
							"(presumably due to drop-out rates)")
					}	
				}
				
				totalTime <- .getOneDimensionalRoot(function(x) {
					eventsOverStages[kMax, i]/.getEventProbabilities(x, fixedSampleSize$accrualTime, lambda, phi, 
							fixedSampleSize$allocationRatioPlanned) - fixedSampleSize$maxNumberOfPatients[i]
				}, lower = 0, upper = up, tolerance = 1E-6)
				followUpTime[i] <- totalTime - fixedSampleSize$accrualTime
				
				#   Analysis times		
				for (j in (1:(kMax - 1))) {          
					analysisTimes[j, i] <- .getOneDimensionalRoot(function(x) {
						eventsOverStages[j, i]/fixedSampleSize$maxNumberOfPatients[i] - 
							.getEventProbabilities(x, fixedSampleSize$accrualTime, 
							lambda, phi, fixedSampleSize$allocationRatioPlanned)
					}, lower = 0, upper = totalTime, tolerance = 1E-6)
				}
				analysisTimes[kMax, i] <- totalTime
				
				numberOfPatients[, i] <- pmin(analysisTimes[, i] * 
					fixedSampleSize$maxNumberOfPatients[i]/fixedSampleSize$accrualTime, 
					rep(1, kMax)*fixedSampleSize$maxNumberOfPatients[i])
				
			} else {
				
				followUpTime[i] <- fixedSampleSize$followUpTime

				if (length(fixedSampleSize$allocationRatioPlanned) > 1) {
					allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned[i]
				} else {
					allocationRatioPlanned <- fixedSampleSize$allocationRatioPlanned
				}
				omega <- .getEventProbabilities(fixedSampleSize$accrualTime + fixedSampleSize$followUpTime, 
					fixedSampleSize$accrualTime, lambda, phi, allocationRatioPlanned)
				numberOfPatients[kMax, i] <- eventsOverStages[kMax, i] / omega
				
				#   Analysis times		
				for (j in (1:(kMax - 1))) {          
					analysisTimes[j, i] <- .getOneDimensionalRoot(function(x) {
						eventsOverStages[j, i]/numberOfPatients[kMax, i] - 
							.getEventProbabilities(x, fixedSampleSize$accrualTime, lambda, phi, 
							allocationRatioPlanned)
					}, lower = 0, upper = fixedSampleSize$accrualTime + fixedSampleSize$followUpTime, tolerance = 1E-6)
				}
				analysisTimes[kMax, i] <- fixedSampleSize$accrualTime + fixedSampleSize$followUpTime
				
				numberOfPatients[, i] <- pmin(analysisTimes[, i]*numberOfPatients[kMax, i] / 
					fixedSampleSize$accrualTime, rep(1, kMax)*numberOfPatients[kMax, i])				
			}
			
			stoppingProbs <- designCharacteristics$rejectionProbabilities + 
				c(designCharacteristics$futilityProbabilities, 0) 

			if (all(is.na(designCharacteristics$futilityProbabilities))) {
				warning("Expected number of patients H1 and study duration H1 cannot be calculated because ",
					"the futility probabilities are not applicable for the specified design", call. = FALSE)
			}
			
			stoppingProbs[kMax] <- 1 - sum(stoppingProbs[1:(kMax - 1)])
			
			studyDurationH1[i] <- analysisTimes[, i] %*% stoppingProbs  

			expectedNumberOfPatientsH1[i] <- numberOfPatients[, i] %*% stoppingProbs 	
			
		}	
		
		expectedEventsH0[i] <- designCharacteristics$averageSampleNumber0 * fixedSampleSize$eventsFixed[i]
		expectedEventsH01[i] <- designCharacteristics$averageSampleNumber01 * fixedSampleSize$eventsFixed[i]
		expectedEventsH1[i] <- designCharacteristics$averageSampleNumber1 * fixedSampleSize$eventsFixed[i]
		
		numberOfPatientsGroup2 <- numberOfPatients/(1 + fixedSampleSize$allocationRatioPlanned)
		numberOfPatientsGroup1 <- numberOfPatientsGroup2*fixedSampleSize$allocationRatioPlanned
		
	}
	
	if (!fixedSampleSize$accountForObservationTimes) {
		return(list(alpha = fixedSampleSize$alpha, 
			beta = fixedSampleSize$beta, 
			sided = fixedSampleSize$sided, 
			informationRates = matrix(informationRates, ncol = 1), 
			typeOfComputation = fixedSampleSize$typeOfComputation, 
			accountForObservationTimes = fixedSampleSize$accountForObservationTimes, 
			allocationRatioPlanned = fixedSampleSize$allocationRatioPlanned,
			eventRatio = fixedSampleSize$eventRatio,			
			thetaH0 = fixedSampleSize$thetaH0, 
			pi1 = fixedSampleSize$pi1,
			pi2 = fixedSampleSize$pi2,
			hazardRatio = fixedSampleSize$hazardRatio, 
			omega = fixedSampleSize$omega,
			calculateFollowUpTime = fixedSampleSize$calculateFollowUpTime,
			eventsOverStages = eventsOverStages, 
			numberOfPatients = matrix(numberOfPatients[kMax,], nrow = 1),
			numberOfPatientsGroup1 = numberOfPatientsGroup1,
			numberOfPatientsGroup2 = numberOfPatientsGroup2,
			expectedEventsH0 = expectedEventsH0,
			expectedEventsH01 = expectedEventsH01,
			expectedEventsH1 = expectedEventsH1
		))
	} else {
		return(list(alpha = fixedSampleSize$alpha, 
			beta = fixedSampleSize$beta, 
			sided = fixedSampleSize$sided,
			informationRates = matrix(informationRates, ncol = 1),						
			typeOfComputation = fixedSampleSize$typeOfComputation, 
			accountForObservationTimes = fixedSampleSize$accountForObservationTimes, 
			allocationRatioPlanned = fixedSampleSize$allocationRatioPlanned, 
			eventRatio = fixedSampleSize$eventRatio, 
			thetaH0 = fixedSampleSize$thetaH0, 
			pi1 = fixedSampleSize$pi1,
			pi2 = fixedSampleSize$pi2,
			hazardRatio = fixedSampleSize$hazardRatio, 
			omega = fixedSampleSize$omega,
			eventTime = fixedSampleSize$eventTime, 
			accrualTime = fixedSampleSize$accrualTime, 
			dropOutRate1 = fixedSampleSize$dropOutRate1, 
			dropOutRate2 = fixedSampleSize$dropOutRate2, 
			dropOutTime = fixedSampleSize$dropOutTime,
			calculateFollowUpTime = fixedSampleSize$calculateFollowUpTime,
			followUpTime = followUpTime,
			analysisTimes = analysisTimes, 
			eventsOverStages = eventsOverStages, 
			numberOfPatients = numberOfPatients, 
			numberOfPatientsGroup1 = numberOfPatientsGroup1,
			numberOfPatientsGroup2 = numberOfPatientsGroup2,
			expectedEventsH0 = expectedEventsH0,
			expectedEventsH01 = expectedEventsH01,
			expectedEventsH1 = expectedEventsH1,
			expectedNumberOfPatientsH1 = expectedNumberOfPatientsH1,
			studyDurationH1 = studyDurationH1				
		))
	}
}

