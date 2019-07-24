##################################################################################################
#                                                                                                #
# -- Simulation of continous data with group sequential and combination test --                  #
#			                                                                                     #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. 			 #
#                                                                                    			 # 
# File version: 1.0.0                                                                			 #
# Date: 21-04-2019                                                                   			 #
# Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD                             			 #
# Licensed under "GNU Lesser General Public License" version 3                       			 #
# License text can be found here: https://www.r-project.org/Licenses/LGPL-3          			 #
#                                                                                    			 #
# RPACT company website: https://www.rpact.com                                       			 #
# RPACT package website: https://www.rpact.org                                       			 #
#                                                                                    			 #
# Contact us for information about our services: info@rpact.com                      			 #
#                                                                                    			 #
##################################################################################################

.getTestStatisticsMeans <- function(
		designNumber,
		informationRates,
		groups,	meanRatio, 
		thetaH0, allocationRatioPlanned,
		sampleSizesPerStage, 
		testStatisticsPerStage) {
	
	stage <- length(sampleSizesPerStage)

	overallTestStatistic <- sqrt(sampleSizesPerStage) %*% testStatisticsPerStage / 
			sqrt(sum(sampleSizesPerStage))
	pValuesSeparate <- 1 - pnorm(testStatisticsPerStage[stage])	
	
	if (designNumber == 1L) {
		value <- overallTestStatistic

	} else if (designNumber == 2L) {
		if (stage == 1) {
			value <- testStatisticsPerStage[1]
		} else {
			value <- (sqrt(informationRates[1]) * testStatisticsPerStage[1] + 
				sqrt(informationRates[2:stage] - informationRates[1:(stage - 1)]) %*% 
				testStatisticsPerStage[2:stage]) / sqrt(informationRates[stage])
		}	
	
	} else if (designNumber == 3L) {
		weightFisher <- rep(NA_real_, stage)
		weightFisher[1] <- 1
		if (stage > 1) {
			weightFisher[2:stage]  <- sqrt(informationRates[2:stage] - 
				informationRates[1:(stage-1)]) / sqrt(informationRates[1])
		}	
		value <- prod((1 - pnorm(testStatisticsPerStage[1:stage]))^weightFisher[1:stage])			
	}	
	
	if (groups == 1) {
		effectEstimate <- overallTestStatistic / sqrt(sum(sampleSizesPerStage))
	} else { 
		if (!meanRatio) {
			effectEstimate <- overallTestStatistic /
					sqrt(allocationRatioPlanned * sum(sampleSizesPerStage)) * 
					(1 + allocationRatioPlanned)
		} else {
			effectEstimate <- overallTestStatistic /
					sqrt(allocationRatioPlanned*sum(sampleSizesPerStage)) *
					sqrt((1 + allocationRatioPlanned) * 
					(1 + thetaH0^2 * allocationRatioPlanned)) 
		} 
	}
	
	return(list(value = value,
	   overallTestStatistic = overallTestStatistic,
	   effectEstimate = effectEstimate, 
	   pValuesSeparate = pValuesSeparate))
}


.getSimulationMeansStageSubjects <- function(..., stage, 
		meanRatio, thetaH0, groups, plannedSubjects, 
		allocationRatioPlanned,
		minNumberOfSubjectsPerStage,
		maxNumberOfSubjectsPerStage,
		sampleSizesPerStage,
		thetaStandardized,		
		conditionalPower, 
		conditionalCriticalValue) {
		
	if (is.na(conditionalPower)) {
		return(plannedSubjects[stage] - plannedSubjects[stage - 1]) 
	} 
	
	mult <- 1
	if (groups == 2) {
		thetaH0 <- ifelse(meanRatio, thetaH0, 1)
		mult <- 1 + 1 /	allocationRatioPlanned + thetaH0^2 * (1 + allocationRatioPlanned)
	}
	
	stageSubjects <- (max(0, conditionalCriticalValue + stats::qnorm(conditionalPower)))^2 * mult / 
		(max(1e-12, thetaStandardized))^2
	
	stageSubjects <- min(max(minNumberOfSubjectsPerStage[stage], stageSubjects), 
		maxNumberOfSubjectsPerStage[stage])
	
	return(stageSubjects)
}


.getSimulationStepMeans <- function(..., 
		k, 
		kMax,
		designNumber,
		informationRates,
		futilityBounds,
		alpha0Vec,
		criticalValues,
		meanRatio, 
		thetaH0,
		alternative,
		stDev,
		groups,
		plannedSubjects,		
		directionUpper,	
		allocationRatioPlanned,
		minNumberOfSubjectsPerStage,
		maxNumberOfSubjectsPerStage, 
		conditionalPower, 
		thetaH1,
		effectEstimate,
		sampleSizesPerStage,
		testStatisticsPerStage,
		testStatistic,
		calcSubjectsFunction) {
	
	stageSubjects <- plannedSubjects[1]
	
	# perform event size recalculation for stages 2,..., kMax
	simulatedConditionalPower <- 0
	if (k > 1) {

		# used effect size is either estimated from test statistic or pre-fixed		
		if (is.na(thetaH1)) {
			thetaStandardized <- effectEstimate	
		} else {
			thetaStandardized <- (thetaH1 - thetaH0) / stDev
		}
		if (!directionUpper) {
			thetaStandardized <- -thetaStandardized
		}
		
		# conditional critical value to reject the null hypotheses at the next stage of the trial
		if (designNumber == 3L) {
			conditionalCriticalValue <- stats::qnorm(1 - (criticalValues[k] / 
				testStatistic$value)^(1 / sqrt((informationRates[k] - 
				informationRates[k - 1]) / informationRates[1])))
		} else {						
			conditionalCriticalValue <- (criticalValues[k] * 
				sqrt(informationRates[k]) - testStatistic$value * sqrt(informationRates[k - 1])) /
				sqrt(informationRates[k] - informationRates[k - 1])
		}

		stageSubjects <- calcSubjectsFunction(
			stage = k, 
			meanRatio = meanRatio, 
			thetaH0 = thetaH0, 
			groups = groups, 
			plannedSubjects = plannedSubjects,
			sampleSizesPerStage = sampleSizesPerStage,
			allocationRatioPlanned = allocationRatioPlanned,
			minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
			maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage, 
			conditionalPower = conditionalPower, 
			thetaStandardized = thetaStandardized, 
			conditionalCriticalValue = conditionalCriticalValue)
		
		# calculate conditional power for computed stageSubjects 
		if (groups == 1) {
			thetaStandardized <- thetaStandardized 
		} else {
			if (!meanRatio) {
				thetaStandardized <- thetaStandardized * sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned)
			} else {
				thetaStandardized <- thetaStandardized * sqrt(allocationRatioPlanned) / 
					sqrt((1 + allocationRatioPlanned) * (1 + thetaH0 * allocationRatioPlanned))
			}
		}
		simulatedConditionalPower <- 
			1 - stats::pnorm(conditionalCriticalValue - thetaStandardized * sqrt(stageSubjects))										
	}
	
	if (groups == 1) {
		testResult <- (2 * directionUpper - 1) * 
			stats::rnorm(1, (alternative - thetaH0) / stDev * sqrt(stageSubjects))
	} else { 
		if (!meanRatio) {
			testResult <- (2 * directionUpper - 1) * 
				stats::rnorm(1, (alternative - thetaH0) / stDev *
				sqrt(allocationRatioPlanned * stageSubjects) / (1 + allocationRatioPlanned))
		} else {
			testResult <- (2 * directionUpper - 1) * 
				stats::rnorm(1, (alternative - thetaH0) / stDev * 
				sqrt(allocationRatioPlanned * stageSubjects) / 
				sqrt((1 + allocationRatioPlanned) * (1 + thetaH0^2 * allocationRatioPlanned)))
		} 
	}
	
	sampleSizesPerStage <- c(sampleSizesPerStage, stageSubjects)					
	testStatisticsPerStage <- c(testStatisticsPerStage, testResult)
	
	testStatistic <- .getTestStatisticsMeans(designNumber, 
		informationRates, groups, meanRatio, thetaH0, allocationRatioPlanned, sampleSizesPerStage, testStatisticsPerStage)

	effectEstimate <- testStatistic$effectEstimate
	
	simulatedRejections <- 0
	simulatedFutilityStop <- 0
	trialStop <- FALSE
	if (k == kMax){
		trialStop <- TRUE
	}
	if (designNumber <= 2) {
		if (testStatistic$value >= criticalValues[k]) {
			simulatedRejections <- 1
			trialStop <- TRUE
		} 
		# add small number to avoid ties
		if (k < kMax && testStatistic$value <= futilityBounds[k]) {
			simulatedFutilityStop <- 1
			trialStop <- TRUE
		}
	} else {
		if (testStatistic$value <= criticalValues[k]) {
			simulatedRejections <- 1
			trialStop <- TRUE
		} 
		if (k < kMax && testStatistic$pValuesSeparate >= alpha0Vec[k]) {
			simulatedFutilityStop <- 1
			trialStop <- TRUE
		}
	}
	
	
	if (!directionUpper){
		effectEstimate <- -effectEstimate
	}	
	
	return(list(
		trialStop = trialStop,
		sampleSizesPerStage = sampleSizesPerStage,
		testStatisticsPerStage = testStatisticsPerStage,
		testStatistic = testStatistic,
		effectEstimate = effectEstimate,  
		simulatedSubjects = stageSubjects,
		simulatedRejections = simulatedRejections,
		simulatedFutilityStop = simulatedFutilityStop,
		simulatedConditionalPower = simulatedConditionalPower
	))
}

#' @title
#' Get Simulation Means 
#' 
#' @description 
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size 
#' for testing means in a one or two treatment groups testing situation. 
#'
#' @param design The trial design. If no trial design is specified, a fixed sample size design is used. 
#' 		  In this case, \code{alpha}, \code{beta}, and \code{sided} can be directly entered as argument.  
#' @param groups The number of treatment groups (1 or 2), default is \code{2}.
#' @param meanRatio If \code{meanRatio = TRUE} is specified, the design characteristics for 
#'        one-sided testing of H0: mu1/mu2 = thetaH0 are simulated, default is \code{FALSE}.
#' @param thetaH0 The null hypothesis value. For one-sided testing, a value != 0 
#'        (or a value != 1 for testing the mean ratio) can be specified, default is 
#'        \code{0} or \code{1} for difference and ratio testing, respectively.
#' @param alternative The alternative hypothesis value. This can be a vector of assumed 
#'        alternatives, default is \code{seq(0,1,0.2)}. 
#' @param directionUpper Specifies the direction of the alternative, only applicable 
#'        for one-sided testing, default is \code{TRUE}.
#' @param allocationRatioPlanned The planned allocation ratio for a two treatment groups 
#'        design, default is \code{1}. 
#' @param plannedSubjects \code{plannedSubjects} is a vector of length \code{kMax} (the number of stages of the design) 
#' 	      that determines the number of cumulated (overall) subjects when the interim stages are planned.
#' @param minNumberOfSubjectsPerStage When performing a data driven sample size recalculation, 
#' 		  the vector with length kMax \code{minNumberOfSubjectsPerStage} determines the 
#'        minimum number of subjects per stage (i.e., not cumulated), the first element 
#'        is not taken into account.   
#' @param maxNumberOfSubjectsPerStage When performing a data driven sample size recalculation, 
#' 	      the vector with length kMax \code{maxNumberOfSubjectsPerStage} determines the maximum number 
#'        of subjects per stage (i.e., not cumulated), the first element is not taken into account.
#' @param conditionalPower The conditional power for the subsequent stage under which the sample size recalculation is performed.
#' @param thetaH1 If specified, the value of the alternative under which the conditional power calculation is performed.
#' @param stDev The standard deviation under which the conditional power calculation is performed, default is 1. 
#'        If \code{meanRatio = TRUE} is specified, stDev defines the coefficient of variation sigma/mu2. 
#' @param maxNumberOfIterations The number of simulation iterations.
#' @param calcSubjectsFunction Optionally, a function can be entered that defines the way of performing the sample size
#' 		  recalculation. By default, sample size recalulation is performed with conditional power with specified
#' 		  \code{minNumberOfSubjectsPerStage} and \code{maxNumberOfSubjectsPerStage} (see details
#' 		  and examples).
#' @param seed The seed to reproduce the simulation, default is a random seed.
#' @param ... Ensures that all arguments are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' At given design the function simulates the power, stopping probabilities, conditional power, and expected 
#' sample size at given number of subjects and parameter configuration. 
#' Additionally, an allocation ratio = n1/n2 can be specified where n1 and n2 are the number 
#' of subjects in the two treatment groups.
#' 
#' calcSubjectsFunction\cr 
#' This function returns the number of subjects at given conditional power and conditional Type I error rate for specified 
#' testing situation. The function might depend on variables \code{stage}, \code{meanRatio}, \code{thetaH0}, \code{groups}, 
#' \code{plannedSubjects}, \code{sampleSizesPerStage}, \code{directionUpper}, \code{allocationRatioPlanned}, 
#' \code{minNumberOfSubjectsPerStage}, 
#' \code{maxNumberOfSubjectsPerStage}, \code{conditionalPower}, \code{conditionalCriticalValue}, 
#' \code{thetaStandardized}.
#' The function has to obtain the three-dots arument '...' (see examples). 
#' 
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#' 
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable 
#' the output of the aggregated simulated data.\cr
#' 
#' Example 1: \cr
#' \code{simulationResults <- getSimulationMeans(plannedSubjects = 40)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#' 
#' Example 2: \cr
#' \code{simulationResults <- getSimulationMeans(plannedSubjects = 40)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#' 
#' \code{\link{getData}} can be used to get the aggregated simulated data from the 
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{alternative}: The alternative hypothesis value.
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the 
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise. 
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision, 
#'         depends on which design was chosen (group sequential, inverse normal, or Fishers combination test).  
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#' 			the considered stage is taken into account.
#'   \item \code{effectEstimate}: Standardized overall simulated effect estimate. 
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.  
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for 
#' 			selected sample size and effect. The effect is either estimated from the data or can be
#' 			user defined with \code{thetaH1}.   
#' }
#'  
#' @return Returns a  \code{\link{SimulationResultsMeans}} object.
#' 
#' @export 
#' 
#' @examples
#'
#' # Fixed sample size with minimum required definitions, 
#' # alternative = c(0, 1, 2, 3, 4), standard deviation = 5  
#' getSimulationMeans(getDesignGroupSequential(), alternative = 40, 
#'     stDev = 50, plannedSubjects = c(20, 40, 60), thetaH1 = 60, 
#'     maxNumberOfIterations = 50)
#' 
#' \donttest{
#' 
#' # Increase number of simulation iterations and compare results 
#' # with power calculator using normal approximation 
#' getSimulationMeans(alternative = 0:4, stDev = 5, 
#'     plannedSubjects = 40, maxNumberOfIterations = 50)
#' getPowerMeans(alternative = 0:4, stDev = 5, 
#'     maxNumberOfSubjects = 40, normalApproximation = TRUE)
#' 
#' # Do the same for a three-stage O'Brien&Fleming inverse 
#' # normal group sequential design with non-binding futility stops
#' designIN <- getDesignInverseNormal(typeOfDesign = "OF", futilityBounds = c(0, 0))
#' x <- getSimulationMeans(designIN, alternative = c(0:4), stDev = 5, 
#'     plannedSubjects = c(20, 40, 60), maxNumberOfIterations = 1000)
#' getPowerMeans(designIN, alternative = 0:4, stDev = 5, 
#'     maxNumberOfSubjects = 60, normalApproximation = TRUE)
#' 
#' # Assess power and average sample size if a sample size increase is foreseen 
#' # at conditional power 80% for each subsequent stage based on observed overall 
#' # effect and specified minNumberOfSubjectsPerStage and
#' # maxNumberOfSubjectsPerStage
#' getSimulationMeans(designIN, alternative = 0:4, stDev = 5, 
#'     plannedSubjects = c(20, 40, 60), 
#'     minNumberOfSubjectsPerStage = c(20, 20, 20), 
#'     maxNumberOfSubjectsPerStage = c(80, 80, 80),
#'     conditionalPower = 0.8,	
#' 	   maxNumberOfIterations = 50)
#' 
#' # Do the same under the assumption that a sample size increase only takes 
#' # place at the first interim. The sample size for the third stage is set equal 
#' # to the second stage sample size.
#' mySampleSizeCalculationFunction <- function(..., stage, 
#'         minNumberOfSubjectsPerStage,
#'         maxNumberOfSubjectsPerStage,
#'         sampleSizesPerStage,
#'         conditionalPower,
#'         conditionalCriticalValue,
#'         thetaStandardized) {
#'     if (stage == 2) {
#'         stageSubjects <- 4 * (max(0, conditionalCriticalValue + 
#'             stats::qnorm(conditionalPower)))^2 / (max(1e-12, thetaStandardized))^2
#'         stageSubjects <- min(max(minNumberOfSubjectsPerStage[stage], 
#'             stageSubjects), maxNumberOfSubjectsPerStage[stage])
#'     } else {
#'         stageSubjects <- sampleSizesPerStage[stage - 1]
#'     }
#'     return(stageSubjects)
#' }
#' getSimulationMeans(designIN, alternative = 2:4, stDev = 5, 
#'     plannedSubjects = c(20, 40, 60), 
#'     minNumberOfSubjectsPerStage = c(20, 20, 20), 
#'     maxNumberOfSubjectsPerStage = c(40, 160, 160),
#'     conditionalPower = 0.8, 
#'     calcSubjectsFunction = mySampleSizeCalculationFunction, 
#'     maxNumberOfIterations = 50)
#' 
#' }
#' 
getSimulationMeans <- function(
		design = NULL, ...,
		groups = 2L,
		meanRatio = FALSE, 
		thetaH0 = ifelse(meanRatio, 1, 0),
		alternative = C_ALTERNATIVE_POWER_SIMULATION_DEFAULT,
		stDev = C_STDEV_DEFAULT,
		plannedSubjects = NA_real_,		
		directionUpper = C_DIRECTION_UPPER_DEFAULT,	
		allocationRatioPlanned = NA_real_,
		minNumberOfSubjectsPerStage = NA_real_,
		maxNumberOfSubjectsPerStage = NA_real_, 
		conditionalPower = NA_real_, 
		thetaH1 = NA_real_,
		maxNumberOfIterations = C_MAX_SIMULATION_ITERATIONS_DEFAULT, 
		seed = NA_real_,
		calcSubjectsFunction = NULL) {
		
	if (is.null(design)) {
		design <- .getDefaultDesignForSampleSizeCalculations(...)
	} else {
		.assertIsTrialDesign(design)
		.warnInCaseOfUnknownArguments(functionName = "getSimulationMeans", ...)
		.warnInCaseOfTwoSidedPowerArgument(...)
	}
	.assertIsSingleLogical(directionUpper, "directionUpper")
	.assertIsSingleNumber(thetaH0, "thetaH0")
	if (meanRatio) {
		.assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
	}
	.assertIsNumericVector(alternative, "alternative", naAllowed = FALSE)
	.assertIsNumericVector(minNumberOfSubjectsPerStage, 
		"minNumberOfSubjectsPerStage", naAllowed = TRUE)
	.assertIsNumericVector(maxNumberOfSubjectsPerStage, 
		"maxNumberOfSubjectsPerStage", naAllowed = TRUE)
	.assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
	.assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
	.assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE)
	if (meanRatio) {
		.assertIsInOpenInterval(thetaH1, "thetaH1", 0, NULL, naAllowed = TRUE)
	}
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
	.assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, NULL, naAllowed = TRUE)	
	.assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
	.assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
	.assertIsValidStandardDeviation(stDev)
	if (is.null(calcSubjectsFunction)) {
		calcSubjectsFunction <- .getSimulationMeansStageSubjects
	}
	.assertIsValidFunction(fun = calcSubjectsFunction, 
		funArgName = "calcSubjectsFunction",
		expectedFunction = .getSimulationMeansStageSubjects)
	
	if (design$sided == 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"only one-sided case is implemented for the simulation design")
	}
	
	if (groups == 1L) {
		if (isTRUE(meanRatio)) {
			warning("'meanRatio' (", meanRatio, ") will be ignored ", 
				"because it is not applicable for 'groups' = 1", call. = FALSE)
		}
		if (!is.na(allocationRatioPlanned)) {   
			warning("'allocationRatioPlanned' (", allocationRatioPlanned, 
				") will be ignored because it is not applicable for 'groups' = 1", call. = FALSE)
		}	
	} else if (is.na(allocationRatioPlanned)) {
		allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
	}

	simulationResults <- SimulationResultsMeans(design, meanRatio = meanRatio)
	
	minNumberOfSubjectsPerStage <- .assertIsValidMinNumberOfSubjectsPerStage(minNumberOfSubjectsPerStage, 
		"minNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, design$kMax) 
	maxNumberOfSubjectsPerStage <- .assertIsValidMinNumberOfSubjectsPerStage(maxNumberOfSubjectsPerStage, 
		"maxNumberOfSubjectsPerStage", plannedSubjects, conditionalPower, design$kMax) 
	
	if (!is.na(conditionalPower)) {
		if (design$kMax > 1) {
			if (any(maxNumberOfSubjectsPerStage - minNumberOfSubjectsPerStage < 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfSubjectsPerStage' (", 
					.arrayToString(maxNumberOfSubjectsPerStage), 
					") must be not smaller than minNumberOfSubjectsPerStage' (", 
					.arrayToString(minNumberOfSubjectsPerStage), ")")
			}
			.setValueAndParameterType(simulationResults, "minNumberOfSubjectsPerStage",
				minNumberOfSubjectsPerStage, NA_real_)
			.setValueAndParameterType(simulationResults, "maxNumberOfSubjectsPerStage", 
				maxNumberOfSubjectsPerStage, NA_real_)
		} else {
			warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
		}
	} else {
		if (length(minNumberOfSubjectsPerStage) != 1 || 
				!is.na(minNumberOfSubjectsPerStage)) {
			warning("'minNumberOfSubjectsPerStage' (", 
				.arrayToString(minNumberOfSubjectsPerStage), ") ",
				"will be ignored because no 'conditionalPower' is defined", call. = FALSE)
			simulationResults$minNumberOfSubjectsPerStage <- NA_real_
		}
		if (length(maxNumberOfSubjectsPerStage) != 1 || 
				!is.na(maxNumberOfSubjectsPerStage)) {
			warning("'maxNumberOfSubjectsPerStage' (", 
				.arrayToString(maxNumberOfSubjectsPerStage), ") ",
				"will be ignored because no 'conditionalPower' is defined", call. = FALSE)
			simulationResults$maxNumberOfSubjectsPerStage <- NA_real_
		}
	}
	
	.assertIsIntegerVector(plannedSubjects, "plannedSubjects", validateType = FALSE)
	if (length(plannedSubjects) != design$kMax) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'plannedSubjects' (", .arrayToString(plannedSubjects), 
			") must have length ", design$kMax)
	}
	.assertIsInClosedInterval(plannedSubjects, "plannedSubjects", lower = 1, upper = NULL)
	.assertValuesAreStrictlyIncreasing(plannedSubjects, "plannedSubjects")
	.assertIsSingleLogical(directionUpper, "directionUpper")
	
	seed <- .setSeed(seed)
	
	effect <- alternative - thetaH0 
	simulationResults$effect <- effect
	
	.setValueAndParameterType(simulationResults, "meanRatio", meanRatio, FALSE)
	.setValueAndParameterType(simulationResults, "thetaH0", thetaH0, ifelse(meanRatio, 1, 0))
	.setValueAndParameterType(simulationResults, "alternative", alternative, C_ALTERNATIVE_POWER_SIMULATION_DEFAULT)
	.setValueAndParameterType(simulationResults, "stDev", stDev, C_STDEV_DEFAULT)
	.setValueAndParameterType(simulationResults, "groups", as.integer(groups), 2L)
	.setValueAndParameterType(simulationResults, "allocationRatioPlanned", 
		allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
	.setValueAndParameterType(simulationResults, "plannedSubjects", 
		plannedSubjects, NA_real_)
	.setValueAndParameterType(simulationResults, "directionUpper", 
		directionUpper, C_DIRECTION_UPPER_DEFAULT)
	.setValueAndParameterType(simulationResults, "minNumberOfSubjectsPerStage", 
		minNumberOfSubjectsPerStage, NA_real_, notApplicableIfNA = TRUE)
	.setValueAndParameterType(simulationResults, "maxNumberOfSubjectsPerStage", 
		maxNumberOfSubjectsPerStage, NA_real_, notApplicableIfNA = TRUE)
	.setValueAndParameterType(simulationResults, "conditionalPower", 
		conditionalPower, NA_real_, notApplicableIfNA = TRUE)
	.setValueAndParameterType(simulationResults, "thetaH1", 
		thetaH1, NA_real_, notApplicableIfNA = TRUE)
	.setValueAndParameterType(simulationResults, "maxNumberOfIterations", 
		as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT)
	.setValueAndParameterType(simulationResults, "seed", seed, NA_real_)
	
	if (.isTrialDesignGroupSequential(design)) {
		designNumber <- 1L
	} else if (.isTrialDesignInverseNormal(design)) {
		designNumber <- 2L
	} else if (.isTrialDesignFisher(design)) {
		designNumber <- 3L
	}
	
	if (.isTrialDesignFisher(design)) {
		alpha0Vec <- design$alpha0Vec
		futilityBounds <- rep(NA_real_, design$kMax - 1)
	} else {
		alpha0Vec <- rep(NA_real_, design$kMax - 1)
		futilityBounds <- design$futilityBounds
	}
	informationRates <- design$informationRates
	criticalValues <- design$criticalValues
	kMax <- design$kMax
	cols <- length(alternative)
	sampleSizes <- matrix(0, kMax, cols)
	rejectPerStage <- matrix(0, kMax, cols)
	overallReject <- rep(0, cols)
	futilityPerStage <- matrix(0, kMax - 1, cols)
	futilityStop <- rep(0, cols)
	iterations <- matrix(0, kMax, cols)
	expectedNumberOfSubjects <- rep(0, cols)
	conditionalPowerAchieved <- matrix(NA_real_, kMax, cols)	
	
	len <- length(alternative) * maxNumberOfIterations * kMax
	dataIterationNumber <- rep(NA_real_, len)
	dataStageNumber <- rep(NA_real_, len)
	dataAlternative <- rep(NA_real_, len)
	dataEffect <- rep(NA_real_, len)
	dataNumberOfSubjects <- rep(NA_real_, len)
	dataRejectPerStage <- rep(NA_real_, len)
	dataFutilityPerStage <- rep(NA_real_, len)
	dataTestStatisticsPerStage <- rep(NA_real_, len)
	dataTestStatistic <- rep(NA_real_, len)
	dataTrialStop <- rep(NA, len)
	dataConditionalPowerAchieved <- rep(NA_real_, len)
	dataEffectEstimate <- rep(NA_real_, len)
	if (designNumber == 3L) {
		dataPValuesSeparate <- rep(NA_real_, len)
	}
	
	index <- 1
	for (i in 1:length(alternative)) {
		
		simulatedSubjects <- rep(0, kMax)
		simulatedOverallSubjects <- rep(0, kMax)
		simulatedRejections <- rep(0, kMax)
		simulatedFutilityStop <- rep(0, kMax - 1)
		simulatedOverallSubjects <- 0
		simulatedConditionalPower <- rep(0, kMax)
		
		for (j in 1:maxNumberOfIterations) { 
			trialStop <- FALSE
			sampleSizesPerStage <- c()
			testStatisticsPerStage <- c()
			testStatistic <- NULL
			effectEstimate <- NULL
			
			for (k in 1:kMax) {
				
				if (!trialStop) {
					stepResult <- .getSimulationStepMeans(
						k = k, 
						kMax = kMax,
						designNumber = designNumber,
						informationRates = informationRates,
						futilityBounds = futilityBounds,
						alpha0Vec = alpha0Vec,
						criticalValues = criticalValues,
						meanRatio = meanRatio, 
						thetaH0 = thetaH0,
						alternative = alternative[i],
						stDev = stDev,
						groups = groups,
						plannedSubjects = plannedSubjects,		
						directionUpper = directionUpper,	
						allocationRatioPlanned = allocationRatioPlanned,
						minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
						maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage, 
						conditionalPower = conditionalPower, 
						thetaH1 = thetaH1,
						effectEstimate = effectEstimate,
						sampleSizesPerStage = sampleSizesPerStage,
						testStatisticsPerStage = testStatisticsPerStage,
						testStatistic = testStatistic,
						calcSubjectsFunction = calcSubjectsFunction) 
					
					trialStop <- stepResult$trialStop
					sampleSizesPerStage <- stepResult$sampleSizesPerStage
					testStatisticsPerStage <- stepResult$testStatisticsPerStage
					testStatistic <- stepResult$testStatistic
					simulatedSubjectsStep <- stepResult$simulatedSubjects
					simulatedRejectionsStep <- stepResult$simulatedRejections
					simulatedFutilityStopStep <- stepResult$simulatedFutilityStop
					simulatedConditionalPowerStep <- NA_real_
					effectEstimate <- stepResult$effectEstimate
					if (k > 1) {
						simulatedConditionalPowerStep <- stepResult$simulatedConditionalPower
					}
					
					iterations[k, i] <- iterations[k, i] + 1
					simulatedSubjects[k] <- simulatedSubjects[k] + simulatedSubjectsStep
					simulatedRejections[k] <- simulatedRejections[k] + simulatedRejectionsStep
					if (k < kMax) {
						simulatedFutilityStop[k] <- simulatedFutilityStop[k] + simulatedFutilityStopStep
					}
					simulatedConditionalPower[k] <- 
						simulatedConditionalPower[k] + simulatedConditionalPowerStep
					
					dataIterationNumber[index] <- j
					dataStageNumber[index] <- k
					dataAlternative[index] <- alternative[i]
					dataEffect[index] <- effect[i]
					dataNumberOfSubjects[index] <- simulatedSubjectsStep
					dataRejectPerStage[index] <- simulatedRejectionsStep
					dataFutilityPerStage[index] <- simulatedFutilityStopStep
					dataTestStatistic[index] <- testStatistic$value
					dataTestStatisticsPerStage[index] <- testStatisticsPerStage[k]
					dataTrialStop[index] <- trialStop
					dataConditionalPowerAchieved[index] <- simulatedConditionalPowerStep
					dataEffectEstimate[index] <- effectEstimate
					if (designNumber == 3L) {
						dataPValuesSeparate[index] <- testStatistic$pValuesSeparate
					}
					index <- index + 1
				}
			}
		}
		
		simulatedOverallSubjects <- sum(simulatedSubjects[1:k])
		
		sampleSizes[, i]  <- simulatedSubjects / iterations[, i]
		rejectPerStage[, i] <- simulatedRejections / maxNumberOfIterations
		overallReject[i] <- sum(simulatedRejections / maxNumberOfIterations)
		futilityPerStage[, i] <- simulatedFutilityStop / maxNumberOfIterations
		futilityStop[i] <- sum(simulatedFutilityStop / maxNumberOfIterations)
		expectedNumberOfSubjects[i] <- simulatedOverallSubjects / maxNumberOfIterations 	
		if (kMax > 1) {
			conditionalPowerAchieved[2:kMax, i] <- 
				simulatedConditionalPower[2:kMax] / iterations[2:kMax, i]
		}
	}

	simulationResults$iterations <- iterations
	simulationResults$sampleSizes <- sampleSizes
	simulationResults$rejectPerStage <- rejectPerStage
	simulationResults$overallReject <- overallReject
	simulationResults$futilityPerStage <- futilityPerStage
	simulationResults$futilityStop <- futilityStop
	if (kMax > 1) {
		if (length(alternative) == 1){
			simulationResults$earlyStop <- sum(futilityPerStage) + sum(rejectPerStage[1:(kMax - 1)])
		} else {	
			if (kMax > 2) {
				rejectPerStageColSum <- colSums(rejectPerStage[1:(kMax - 1), ])
			} else {
				rejectPerStageColSum <- rejectPerStage[1, ]
			}
			simulationResults$earlyStop <- colSums(futilityPerStage) + rejectPerStageColSum
		}
	} else {
		simulationResults$earlyStop <- rep(0, length(alternative))
	}	
	
	simulationResults$expectedNumberOfSubjects <- expectedNumberOfSubjects
	simulationResults$conditionalPowerAchieved <- conditionalPowerAchieved
	
	if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
		simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
	}
	
	data <- data.frame(
		iterationNumber = dataIterationNumber,
		stageNumber = dataStageNumber,
		alternative = dataAlternative,
		effect = dataEffect,
		numberOfSubjects = dataNumberOfSubjects,
		rejectPerStage = dataRejectPerStage,
		futilityPerStage = dataFutilityPerStage,
		testStatistic = dataTestStatistic,
		testStatisticsPerStage = dataTestStatisticsPerStage,
		effectEstimate = dataEffectEstimate,
		trialStop = dataTrialStop,		
		conditionalPowerAchieved = round(dataConditionalPowerAchieved,6)
	)
	if (designNumber == 3L) {
		data$pValue <- dataPValuesSeparate
	} 
	data <- data[!is.na(data$alternative), ]
	
	simulationResults$.data <- data
	
	return(simulationResults)
}

