######################################################################################
#                                                                                    #
# -- Analysis of rates with group sequential and combination test --                 #
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

# @title
# Get Analysis Results Rates
# 
# @description
# Returns an analysis result object.  
#
# @param design The trial design.
# 
# @return Returns a \code{AnalysisResultsRates} object.
# 
# @keywords internal
# 
getAnalysisResultsRates <- function(design, ...) {
	if (.isTrialDesignGroupSequential(design)) {
		return(.getAnalysisResultsRatesGroupSequential(design, ...))
	}
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getAnalysisResultsRatesInverseNormal(design, ...))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getAnalysisResultsRatesFisher(design, ...))
	}
	
	.stopWithWrongDesignMessage(design)
}

.getAnalysisResultsRatesInverseNormal <- function(design, ...,
		dataInput, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, pi1 = NA_real_, pi2 = NA_real_, nPlanned = NA_real_, 
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	.assertIsTrialDesignInverseNormal(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.warnInCaseOfUnknownArguments(functionName = ".getAnalysisResultsRatesInverseNormal", 
		ignore = c("stage"), ...)
	
	results <- AnalysisResultsInverseNormal(design = design, dataInput = dataInput)
	
	.getAnalysisResultsRates(results = results, design = design, dataInput = dataInput, 
		stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation, 
		thetaH0 = thetaH0, pi1 = pi1, pi2 = pi2, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, 
		tolerance = tolerance)
	
	return(results)
}

.getAnalysisResultsRatesGroupSequential <- function(design, ...,
		dataInput, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,  
		thetaH0 = C_THETA_H0_RATES_DEFAULT, pi1 = NA_real_, pi2 = NA_real_, nPlanned = NA_real_, 
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	.assertIsTrialDesignGroupSequential(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.warnInCaseOfUnknownArguments(functionName = ".getAnalysisResultsRatesGroupSequential", 
		ignore = c("stage"), ...)
	
	results <- AnalysisResultsGroupSequential(design = design, dataInput = dataInput)
	
	.getAnalysisResultsRates(results = results, design = design, dataInput = dataInput, 
		stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation, 
		thetaH0 = thetaH0, pi1 = pi1, pi2 = pi2, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, 
		tolerance = tolerance)
	
	return(results)
}

.getAnalysisResultsRatesFisher <- function(design, ...,
		dataInput, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, pi1 = NA_real_, pi2 = NA_real_, nPlanned = NA_real_, 
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT, 
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
	
	.assertIsTrialDesignFisher(design)
	.assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.warnInCaseOfUnknownArguments(functionName = ".getAnalysisResultsRatesFisher", 
		ignore = c("stage"), ...)
	
	results <- AnalysisResultsFisher(design = design, dataInput = dataInput)
	
	.getAnalysisResultsRates(results = results, design = design, dataInput = dataInput, 
		stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation, 
		thetaH0 = thetaH0, pi1 = pi1, pi2 = pi2, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, 
		tolerance = tolerance,
		iterations = iterations, seed = seed)

	return(results)
}

#
# The following parameters will be taken from 'design':
# stages, informationRates, criticalValues, futilityBounds, alphaSpent, stageLevels	
#
.getAnalysisResultsRates <- function(results, design, dataInput, stage, 
		directionUpper, normalApproximation, thetaH0, pi1, pi2,
		nPlanned, allocationRatioPlanned, tolerance,
		iterations, seed) {
	
	startTime <- Sys.time()
	stageResults <- getStageResultsRates(design = design, dataInput = dataInput, stage = stage, 
		thetaH0 = thetaH0, directionUpper = directionUpper, 
		normalApproximation = normalApproximation)
	results$.stageResults <- stageResults
	.logProgress("Stage results calculated", startTime = startTime)
	
	.assertIsValidAllocationRatioPlanned(allocationRatioPlanned, dataInput$getNumberOfGroups())
	.assertIsValidPi(pi1, "pi1")
	.assertIsValidPi(pi2, "pi2")
	
	results$directionUpper <- directionUpper
	results$normalApproximation <- normalApproximation
	results$allocationRatioPlanned <- allocationRatioPlanned
	results$thetaH0 <- thetaH0
	results$pi1 <- pi1
	results$pi2 <- pi2
	results$nPlanned <- nPlanned
	while (length(results$nPlanned) < design$kMax) {
		results$nPlanned <- c(NA_real_, results$nPlanned)
	}
	
	# effect size
	results$effectSizes <- stageResults$effectSizes
	
	# test statistic
	results$testStatistics <- stageResults$testStatistics
	
	# p-value
	results$pValues <- stageResults$pValues
	
	# combined test statistic and test action
	if (.isTrialDesignInverseNormal(design)) {
		results$combinationTestStatistics <- stageResults$combInverseNormal
	} else if (.isTrialDesignGroupSequential(design)) {
		results$overallTestStatistics <- stageResults$overallTestStatistics
		results$overallPValues <- stageResults$overallPValues
	}		
	else if (.isTrialDesignFisher(design)) {
		results$combinationTestStatistics <- stageResults$combFisher
	}	
	
	#  test actions 
	results$testActions <- getTestActions(design = design, stageResults = stageResults, stage = stage)
	
	# conditional power
	startTime <- Sys.time()
	if (.isTrialDesignFisher(design)) {
		conditionalPowerList <- .getConditionalPowerRates(design = design, stageResults = stageResults, 
			stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			pi1 = pi1, pi2 = pi2, iterations = iterations, seed = seed)
		if (conditionalPowerList$simulated) {
			results$conditionalPowerSimulated <- conditionalPowerList$conditionalPower
		} else {
			results$conditionalPower <- conditionalPowerList$conditionalPower
			results$conditionalPowerSimulated <- numeric(0)
		}
	} else {
		results$conditionalPower <- .getConditionalPowerRates(design = design, stageResults = stageResults, 
			stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			pi1 = pi1, pi2 = pi2)$conditionalPower
	}
	.logProgress("Conditional power calculated", startTime = startTime)	
	
	# CRP - conditional rejection probabilities
	startTime <- Sys.time()
	results$conditionalRejectionProbabilities <- getConditionalRejectionProbabilities(
		design = design, stageResults = stageResults, stage = stage)
	.logProgress("Conditional rejection probabilities (CRP) calculated", startTime = startTime)
	
	# RCI - repeated confidence interval
	startTime <- Sys.time()
	repeatedConfidenceIntervals <- getRepeatedConfidenceIntervalsRates(
		design = design, dataInput = dataInput, stage = stage, 
		normalApproximation = normalApproximation, tolerance = tolerance)
	results$repeatedConfidenceIntervalLowerBounds <- repeatedConfidenceIntervals[1, ]
	results$repeatedConfidenceIntervalUpperBounds <- repeatedConfidenceIntervals[2, ]
	.logProgress("Repeated confidence interval calculated", startTime = startTime)
	
	# repeated p-value
	startTime <- Sys.time()
	results$repeatedPValues <- getRepeatedPValues(design = design, 
		stageResults = stageResults, stage = stage, tolerance = tolerance)
	.logProgress("Repeated p-values calculated", startTime = startTime)
	
	# final p-value
	startTime <- Sys.time()
	finalPValue <- getFinalPValue(design = design, stageResults = stageResults, stage = stage) 
	results$finalPValues <- .getVectorWithFinalValueAtFinalStage(kMax = design$kMax, 
		finalValue = finalPValue$pFinal, finalStage = finalPValue$finalStage)
	results$finalStage <- finalPValue$finalStage
	.logProgress("Final p-value calculated", startTime = startTime)
	
	# final confidence interval & median unbiased estimate
	startTime <- Sys.time()
	finalConfidenceIntervals <- getFinalConfidenceIntervalRates(design = design, dataInput = dataInput, 
		thetaH0 = thetaH0, stage = stage, directionUpper = directionUpper, 
		normalApproximation = normalApproximation, tolerance = tolerance)
	.logProgress("Final confidence interval calculated", startTime = startTime)
	
	if (!is.null(finalConfidenceIntervals)) {
		finalStage <- finalConfidenceIntervals$finalStage
		results$finalConfidenceIntervalLowerBounds <- .getVectorWithFinalValueAtFinalStage(kMax = design$kMax, 
			finalValue = finalConfidenceIntervals$finalConfidenceInterval[1], finalStage = finalStage)
		results$finalConfidenceIntervalUpperBounds <- .getVectorWithFinalValueAtFinalStage(kMax = design$kMax, 
			finalValue = finalConfidenceIntervals$finalConfidenceInterval[2], finalStage = finalStage)
		results$medianUnbiasedEstimates <- .getVectorWithFinalValueAtFinalStage(kMax = design$kMax, 
			finalValue = finalConfidenceIntervals$medianUnbiased, finalStage = finalStage)
	}
	
	return(results)
}

# @title
# Get Stage Results Rates
# 
# @description  
# Returns a stage results object.
#
# @param design the trial design.
# 
# @return Returns a \code{StageResultsRates} object.
# 
# @keywords internal
# 
getStageResultsRates <- function(..., design, dataInput, thetaH0 = NA_real_, 
		directionUpper = TRUE, normalApproximation = TRUE) {
	
	.assertIsDatasetRates(dataInput)
	.assertIsValidThetaH0(thetaH0, dataInput)
	.warnInCaseOfUnknownArguments(functionName = "getStageResultsRates", ignore = c("stage"), ...)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)

	effectSizes <- rep(NA_real_, design$kMax)
	
	if (dataInput$getNumberOfGroups() == 1) {
		if (is.na(thetaH0)) {
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "'thetaH0' must be defined")
		}
		
		if (normalApproximation) {
			overallTestStatistics <- c((dataInput$getOverallEventsUpTo(stage) / 
				dataInput$getOverallSampleSizesUpTo(stage) - thetaH0) / 
				sqrt(thetaH0 * (1 - thetaH0)) * 
				sqrt(dataInput$getOverallSampleSizesUpTo(stage)), rep(NA_real_, design$kMax - stage))  
			if (directionUpper) {
				overallPValues <- 1 - stats::pnorm(overallTestStatistics)
			} else {	
				overallPValues <- stats::pnorm(overallTestStatistics)
			}
		} else {
			overallTestStatistics <- rep(NA_real_, design$kMax)  
			if (directionUpper) {
				overallPValues <- stats::pbinom(dataInput$getOverallEventsUpTo(stage) - 1, 
					dataInput$getOverallSampleSizesUpTo(stage), thetaH0, lower.tail = FALSE) 
			} else {
				overallPValues <- stats::pbinom(dataInput$getOverallEventsUpTo(stage), 
					dataInput$getOverallSampleSizesUpTo(stage), thetaH0, lower.tail = TRUE)
			}
			overallTestStatistics <- stats::qnorm(1 - overallPValues)
		}
		effectSizes[1:stage] <- dataInput$getOverallEventsUpTo(stage) / 
			dataInput$getOverallSampleSizesUpTo(stage) 
	}
	
	if (dataInput$getNumberOfGroups() == 2) {	
		if (is.na(thetaH0)) {
			thetaH0 <- C_THETA_H0_RATES_DEFAULT
		}
		
		overallEvents1 <- dataInput$getOverallEvents(1)
		overallEvents2 <- dataInput$getOverallEvents(2)
		
		overallTestStatistics <- rep(NA_real_, design$kMax)
		overallPValues <- rep(NA_real_, design$kMax)
		
		for (k in 1:stage) {
			if (normalApproximation) {
				if (thetaH0 == 0) {
					if ((overallEvents1[k] + overallEvents2[k] == 0) || 
							(overallEvents1[k] + overallEvents2[k] == 
							sum(dataInput$getSampleSizesUpTo(k, 1)) + 
							sum(dataInput$getSampleSizesUpTo(k, 2)))) {
						overallTestStatistics[k] <- 0
					} else {	
						overallRateH0 <-  (overallEvents1[k] + overallEvents2[k]) / 
							(sum(dataInput$getSampleSizesUpTo(k, 1)) + sum(dataInput$getSampleSizesUpTo(k, 2)))  
						overallTestStatistics[k] <- 
							(overallEvents1[k]/sum(dataInput$getSampleSizesUpTo(k, 1)) - 
							overallEvents2[k]/sum(dataInput$getSampleSizesUpTo(k, 2)) - thetaH0) /
							sqrt(overallRateH0 * (1 - overallRateH0) * 
							(1 / sum(dataInput$getSampleSizesUpTo(k, 1)) + 
							1 / sum(dataInput$getSampleSizesUpTo(k, 2)))) 
					}
				} else {
					y <- .getFarringtonManningValues(overallEvents1[k] / sum(dataInput$getSampleSizesUpTo(k, 1)), 
						overallEvents2[k] /	sum(dataInput$getSampleSizesUpTo(k, 2)), 
						thetaH0, sum(dataInput$getSampleSizesUpTo(k, 1)) / 
							sum(dataInput$getSampleSizesUpTo(k, 2)), "diff")
					
					overallTestStatistics[k] <- 
						(overallEvents1[k] / sum(dataInput$getSampleSizesUpTo(k, 1)) - 
						overallEvents2[k]/sum(dataInput$getSampleSizesUpTo(k, 2)) - thetaH0) /
						sqrt(y$ml1 * (1 - y$ml1) / sum(dataInput$getSampleSizesUpTo(k, 1)) + 
						y$ml2 * (1 - y$ml2) / sum(dataInput$getSampleSizesUpTo(k, 2)))
				}			
				
				if (directionUpper) {
					overallPValues[k] <- 1 - stats::pnorm(overallTestStatistics[k])
				} else { 
					overallPValues[k] <- stats::pnorm(overallTestStatistics[k])
				}
			} else {
				
				if (thetaH0 != 0) {
					stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, "thetaH0 must be equal 0 for performing Fisher's exact test")
				}
				
				if (directionUpper) {
					overallPValues[k] <- stats::phyper(overallEvents1[k] - 1, 
						overallEvents1[k] + overallEvents2[k], 
						sum(dataInput$getSampleSizesUpTo(k, 1)) + sum(dataInput$getSampleSizesUpTo(k, 2)) - 
							overallEvents1[k] - overallEvents2[k],
						sum(dataInput$getSampleSizesUpTo(k, 1)),
						lower.tail = FALSE)
					
				} else {
					overallPValues[k] <- stats::phyper(overallEvents1[k], 
						overallEvents1[k] + overallEvents2[k], 
						sum(dataInput$getSampleSizesUpTo(k, 1)) + sum(dataInput$getSampleSizesUpTo(k, 2)) - 
							overallEvents1[k] - overallEvents2[k],
						sum(dataInput$getSampleSizesUpTo(k, 1)),
						lower.tail = TRUE) 
				}
				overallTestStatistics <- stats::qnorm(1 - overallPValues)
			}
		}
		effectSizes[1:stage] <- overallEvents1[1:stage]/cumsum(dataInput$getSampleSizesUpTo(stage,1)) - 
			overallEvents2[1:stage]/cumsum(dataInput$getSampleSizesUpTo(stage,2)) 						
	}
	
	# calculation of stagewise test statistics and combination tests
	testStatistics <- rep(NA_real_, design$kMax)
	pValues <- rep(NA_real_, design$kMax)
	combInverseNormal <- rep(NA_real_, design$kMax)
	combFisher <- rep(NA_real_, design$kMax)
	weightsInverseNormal <- .getWeighsInverseNormal(design) 
	weightsFisher <- .getWeighsFisher(design)
	for (k in 1:stage) {
		
		if (dataInput$getNumberOfGroups() == 1) {
			if (normalApproximation) {
				# stage-wise test statistics
				testStatistics[k] <- (dataInput$getEvent(k) / dataInput$getSampleSize(k) - thetaH0) / 
					sqrt(thetaH0 * (1 - thetaH0)) * sqrt(dataInput$getSampleSize(k)) 
				pValues[k] <- 1 - stats::pnorm(testStatistics[k])
				if (!directionUpper) {
					pValues[k] <- 1 - pValues[k] 
				}
			} else {
				testStatistics[k] <- NA_real_
				if (directionUpper) {
					pValues[k] <- stats::pbinom(dataInput$getEvent(k) - 1, dataInput$getSampleSize(k), 
						thetaH0, lower.tail = FALSE) 
				} else {
					pValues[k] <- stats::pbinom(dataInput$getEvent(k), dataInput$getSampleSize(k), 
						thetaH0, lower.tail = TRUE)
				}
			}
		}
		
		if (dataInput$getNumberOfGroups() == 2) {
			if (normalApproximation) {
				# stage-wise test statistics
				if (thetaH0 == 0) {
					if ((dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2) == 0) || 
							(dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2) == 
								dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2))) {
						testStatistics[k] <- 0
					} else {	
						rateH0 <- (dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2)) / 
							(dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2))  
						testStatistics[k] <- 
							(dataInput$getEvent(k, 1)/dataInput$getSampleSize(k, 1) - 
							dataInput$getEvent(k, 2)/dataInput$getSampleSize(k, 2) - thetaH0) /
							sqrt(rateH0 * (1 - rateH0) * 
							(1 / dataInput$getSampleSize(k, 1) + 1 / dataInput$getSampleSize(k, 2))) 
					}
				} else {
					y <- .getFarringtonManningValues(dataInput$getEvent(k, 1)/dataInput$getSampleSize(k, 1), 
						dataInput$getEvent(k, 2)/dataInput$getSampleSize(k, 2),	thetaH0, 
						dataInput$getSampleSize(k, 1)/dataInput$getSampleSize(k, 2), "diff")
					
					testStatistics[k] <- (dataInput$getEvent(k, 1)/dataInput$getSampleSize(k, 1) - 
						dataInput$getEvent(k, 2)/dataInput$getSampleSize(k, 2) - thetaH0) /
						sqrt(y$ml1 * (1 - y$ml1) / dataInput$getSampleSize(k, 1) + 
						y$ml2 * (1 - y$ml2) / dataInput$getSampleSize(k, 2))
				}	
				
				if (directionUpper) {
					pValues[k] <- 1 - stats::pnorm(testStatistics[k])
				} else { 
					pValues[k] <- stats::pnorm(testStatistics[k])
				}
			} else {
				testStatistics[k] <- NA_real_
				if (directionUpper) {
					pValues[k] <- stats::phyper(dataInput$getEvent(k, 1) - 1, 
						dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2),
						dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2) - 
						dataInput$getEvent(k, 1) - dataInput$getEvent(k, 2), 
						dataInput$getSampleSize(k, 1),
						lower.tail = FALSE) 
					
				} else {
					pValues[k] <- stats::phyper(dataInput$getEvent(k, 1), 
						dataInput$getEvent(k, 1) + dataInput$getEvent(k, 2),
						dataInput$getSampleSize(k, 1) + dataInput$getSampleSize(k, 2) - 
						dataInput$getEvent(k, 1) - dataInput$getEvent(k, 2), 
						dataInput$getSampleSize(k, 1),
						lower.tail = TRUE)
				}
			}
		}
		
		# inverse normal test
		combInverseNormal[k] <- (weightsInverseNormal[1:k] %*% stats::qnorm(1 - pValues[1:k])) / 
			sqrt(sum(weightsInverseNormal[1:k]^2)) 
		
		# Fisher combination test
		combFisher[k] <- prod(pValues[1:k]^weightsFisher[1:k]) 		
	}

	direction <- ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER)
	if (dataInput$getNumberOfGroups() == 1) {
		return(StageResultsRates(
			design = design,
			dataInput = dataInput,
			overallTestStatistics = overallTestStatistics, 
			overallPValues = overallPValues,
			effectSizes = effectSizes,			
			overallEvents = dataInput$getOverallEvents(1),
			overallSampleSizes = dataInput$getOverallSampleSizesUpTo(stage, 1),
			testStatistics = testStatistics, 
			pValues = pValues, 
			combInverseNormal = combInverseNormal, 
			combFisher = combFisher, 
			weightsInverseNormal = weightsInverseNormal, 
			weightsFisher = weightsFisher, 
			thetaH0 = thetaH0, 
			direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER), 
			normalApproximation = normalApproximation
		))
	}
	
	if (dataInput$getNumberOfGroups() == 2) {
		return(StageResultsRates(
			design = design,
			dataInput = dataInput,
			overallTestStatistics = overallTestStatistics, 
			overallPValues = overallPValues, 
			effectSizes = effectSizes,
			overallEvents1 = dataInput$getOverallEvents(1), 
			overallEvents2 = dataInput$getOverallEvents(2), 
			overallSampleSizes1 = dataInput$getOverallSampleSizesUpTo(stage, 1),
			overallSampleSizes2 = dataInput$getOverallSampleSizesUpTo(stage, 2),
			testStatistics = testStatistics, 
			pValues = pValues, 
			combInverseNormal = combInverseNormal, 
			combFisher = combFisher, 
			weightsInverseNormal = weightsInverseNormal, 
			weightsFisher = weightsFisher, 
			thetaH0 = thetaH0, 
			direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER), 
			normalApproximation = normalApproximation
		))
	}
}

# 
#  Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Rates
#
getRepeatedConfidenceIntervalsRates <- function(design, ...) {
	
	if (.isTrialDesignGroupSequential(design)) {
		return(.getRepeatedConfidenceIntervalsRatesGroupSequential(design = design, ...))
	}
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getRepeatedConfidenceIntervalsRatesInverseNormal(design = design, ...))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getRepeatedConfidenceIntervalsRatesFisher(design = design, ...))
	}
	
	.stopWithWrongDesignMessage(design)
}

.getRootThetaRates <- function(..., design, dataInput, stage, directionUpper, normalApproximation, 
		firstParameterName, secondValue, tolerance, acceptResultsOutOfTolerance) {

	if (dataInput$getNumberOfGroups() == 2) {
		thetaLow <- -1 + tolerance
	} else {
		thetaLow <- tolerance
	}	
	thetaUp <- 1 - tolerance
	
	if (dataInput$getNumberOfGroups() == 1 && !normalApproximation) {
		acceptResultsOutOfTolerance <- FALSE
	}	
	
	result <- .getOneDimensionalRoot(
		function(theta) {
			stageResults <- getStageResultsRates(design = design, dataInput = dataInput, 
				stage = stage, thetaH0 = theta, directionUpper = directionUpper, 
				normalApproximation = normalApproximation)
			firstValue <- stageResults[[firstParameterName]][stage]
			if (.isTrialDesignGroupSequential(design)) {
				firstValue <- stats::qnorm(1 - firstValue)
			}
			return(firstValue - secondValue)
		}, lower = thetaLow, upper = thetaUp, tolerance = tolerance, 
			acceptResultsOutOfTolerance = acceptResultsOutOfTolerance)
	
	return(result)
	
}	

.getRepeatedConfidenceIntervalsRates <- function(..., design, dataInput, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		directionUpper = TRUE, tolerance = C_ANALYSIS_TOLERANCE_DEFAULT, firstParameterName) {
	
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidStage(stage, design$kMax)
		
	if (!normalApproximation && dataInput$getNumberOfGroups() == 2) {
		normalApproximation <- TRUE
		warning("Repeated confidence intervals will be calculated under the normal approximation", call. = FALSE)
	}	
	
	futilityCorr <- rep(NA_real_, design$kMax) # necessary for adjustment for binding futility boundaries
	
	criticalValues <- design$criticalValues
	
	if (.isTrialDesignFisher(design)) {
		bounds <- design$alpha0Vec
		border <- C_ALPHA_0_VEC_DEFAULT
		conditionFunction <- .isFirstValueSmallerThanSecondValue	 	
	} else {
		bounds <- design$futilityBounds
		border <- C_FUTILITY_BOUNDS_DEFAULT
		conditionFunction <- .isFirstValueGreaterThanSecondValue 		
	}
	
	repeatedConfidenceIntervals <- matrix(NA_real_, 2, design$kMax)
	for (k in (1:stage)) {
		startTime <- Sys.time()		
		
		# finding upper and lower RCI limits through root function
		
		if (dataInput$getNumberOfGroups() == 1) {
			
			if (dataInput$overallEvents[k] == 0){
				repeatedConfidenceIntervals[1, k] <- 0
			} else {
				repeatedConfidenceIntervals[1, k] <- .getRootThetaRates(
				design = design, dataInput = dataInput, stage = k, 
				directionUpper = TRUE, normalApproximation = normalApproximation, 
				firstParameterName = firstParameterName, secondValue = criticalValues[k], tolerance = tolerance,
				acceptResultsOutOfTolerance = TRUE)
			}

			if (dataInput$overallEvents[k] == dataInput$overallSampleSizes[k]){
				repeatedConfidenceIntervals[2, k] <- 1
			} else {
				repeatedConfidenceIntervals[2, k] <- .getRootThetaRates(
				design = design, dataInput = dataInput, stage = k, 
				directionUpper = FALSE, normalApproximation = normalApproximation, 
				firstParameterName = firstParameterName, secondValue = criticalValues[k], tolerance = tolerance,
				acceptResultsOutOfTolerance = TRUE)
			}
		}
		
		if (dataInput$getNumberOfGroups() == 2) {

			repeatedConfidenceIntervals[1, k] <- .getRootThetaRates(
						design = design, dataInput = dataInput, stage = k, 
						directionUpper = TRUE, normalApproximation = normalApproximation, 
						firstParameterName = firstParameterName, secondValue = criticalValues[k], tolerance = tolerance,
						acceptResultsOutOfTolerance = TRUE)

			repeatedConfidenceIntervals[2, k] <- .getRootThetaRates(
						design = design, dataInput = dataInput, stage = k, 
						directionUpper = FALSE, normalApproximation = normalApproximation, 
						firstParameterName = firstParameterName, secondValue = criticalValues[k], tolerance = tolerance,
						acceptResultsOutOfTolerance = TRUE)
		}
		
		
		# adjustment for binding futility bounds
		if (k > 1 && conditionFunction(bounds[k - 1], border) && design$bindingFutility) {		
	
			parameterName <- ifelse(.isTrialDesignFisher(design), "pValues", firstParameterName)
			
			futilityCorr[k] <- .getRootThetaRates(
				design = design, dataInput = dataInput, stage = k - 1, 
				directionUpper = directionUpper, normalApproximation = normalApproximation, 
				firstParameterName = parameterName, secondValue = bounds[k - 1], tolerance = tolerance,
				acceptResultsOutOfTolerance = TRUE)
			
			if (directionUpper) {
				repeatedConfidenceIntervals[1, k] <- min(min(futilityCorr[2:k]), repeatedConfidenceIntervals[1, k])
			} else {
				repeatedConfidenceIntervals[2, k] <- max(max(futilityCorr[2:k]), repeatedConfidenceIntervals[2, k])
			}	
			
		}
		.logProgress("Repeated confidence interval of stage %s calculated", startTime = startTime, k)
	}
	
	if (!is.na(repeatedConfidenceIntervals[1, k]) && !is.na(repeatedConfidenceIntervals[2, k]) &&
			repeatedConfidenceIntervals[1, k] > repeatedConfidenceIntervals[2, k]){
		repeatedConfidenceIntervals[, k] <- rep(NA_real_, 2)
	}
	
	return(repeatedConfidenceIntervals)
}

# 
# RCIs based on group sequential method	
# 
.getRepeatedConfidenceIntervalsRatesGroupSequential <- function(..., design, dataInput,   
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, directionUpper = TRUE, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
		
	.warnInCaseOfUnknownArguments(functionName = 
		".getRepeatedConfidenceIntervalsRatesGroupSequential", ignore = c("stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsRates(design = design, dataInput = dataInput, 
		normalApproximation = normalApproximation, directionUpper = directionUpper, 
		firstParameterName = "overallPValues", tolerance = tolerance, ...))
}

# 
# RCIs based on inverse normal combination test	
#
.getRepeatedConfidenceIntervalsRatesInverseNormal <- function(..., design, dataInput, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, directionUpper = TRUE,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
		
	.warnInCaseOfUnknownArguments(functionName = 
		".getRepeatedConfidenceIntervalsRatesInverseNormal", ignore = c("stage"), ...)

	return(.getRepeatedConfidenceIntervalsRates(design = design, dataInput = dataInput,   
		normalApproximation = normalApproximation, directionUpper = directionUpper, 
		firstParameterName = "combInverseNormal", tolerance = tolerance, ...))
}

# 
# RCIs based on Fisher's combination test
#
.getRepeatedConfidenceIntervalsRatesFisher <- function(..., design, dataInput,  
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, directionUpper = TRUE,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
		
	.warnInCaseOfUnknownArguments(functionName = 
		".getRepeatedConfidenceIntervalsRatesFisher", ignore = c("stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsRates(design = design, dataInput = dataInput,   
		normalApproximation = normalApproximation, directionUpper = directionUpper, 
		firstParameterName = "combFisher", tolerance = tolerance, ...))
}

#
# Calculation of conditional power based on group sequential method
#
.getConditionalPowerRatesGroupSequential <- function(..., design, stageResults,  
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, pi1, pi2) {
	
	.assertIsTrialDesignGroupSequential(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerRatesGroupSequential", 
		ignore = c("stage"), ...)
	
	kMax <- design$kMax	
	conditionalPower <- rep(NA_real_, kMax)
	weights <- stageResults$weightsInverseNormal
	informationRates <- design$informationRates
	
	nPlanned <- c(rep(NA, stage), nPlanned)	
	
	if (stage == kMax) {
		.logDebug("Conditional power will be calculated only for subsequent stages ", 
				"(stage = ", stage, ", kMax = ", design$kMax, ")")
		return(list(
			nPlanned = nPlanned,
			conditionalPower = conditionalPower 
		))
	}
	
	criticalValuesInverseNormal <- design$criticalValues
	
	condError <- getConditionalRejectionProbabilities(design = design, 
		stageResults = stageResults, stage = stage)[stage] 
	
	if (stageResults$isOneSampleDataset()) {
		adjustment <- stats::qnorm(1 - condError) * (1 - sqrt(stageResults$thetaH0 * (1 - stageResults$thetaH0)) / 
						sqrt(pi1*(1 - pi1))) / sqrt(sum(nPlanned[(stage + 1) : kMax]))
		if (stageResults$direction == "upper") {
			thetaH1 <- (pi1 - stageResults$thetaH0) / sqrt(pi1*(1 - pi1)) + adjustment
		} else {
			thetaH1 <- -(pi1 - stageResults$thetaH0) / sqrt(pi1*(1 - pi1)) + adjustment
		}
	} 	
	
	if (stageResults$isTwoSampleDataset()) {
		x <- .getFarringtonManningValues(pi1, pi2, stageResults$thetaH0, allocationRatioPlanned)
		adjustment <- stats::qnorm(1 - condError) * (1 - 
			sqrt(x$ml1 * (1 - x$ml1) + allocationRatioPlanned * x$ml2 * (1 - x$ml2)) / 
			sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2))) * 
			(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * 
			sum(nPlanned[(stage + 1) : kMax]))
		if (stageResults$direction == "upper") {
			thetaH1 <- (pi1 - pi2 - stageResults$thetaH0) / 
				sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * 
				(1 - pi2)) * sqrt(1 + allocationRatioPlanned) + adjustment
		} else {
			thetaH1 <- -(pi1 - pi2 - stageResults$thetaH0) / sqrt(pi1 * (1 - pi1) + 
				allocationRatioPlanned * pi2 * (1 - pi2)) * sqrt(1 + allocationRatioPlanned) + adjustment
		}	
	}
	
	if (stageResults$isTwoSampleDataset()) {
		.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
		nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	}	
		
	# shifted decision region for use in getGroupSeqProbs 
	# Group Sequential Method
	shiftedDecisionRegion <- criticalValuesInverseNormal[(stage + 1):kMax] * 
		sqrt(sum(weights[1:stage]^2) + 	cumsum(weights[(stage + 1):kMax]^2)) / 
		sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
		stats::qnorm(1 - stageResults$overallPValues[stage]) * sqrt(sum(weights[1:stage]^2)) / 
		sqrt(cumsum(weights[(stage + 1):kMax]^2)) - 
		thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):kMax]) *	weights[(stage + 1):kMax]) / 
		sqrt(cumsum(weights[(stage + 1):kMax]^2))
	
	if (stage == kMax - 1) {
		shiftedFutilityBounds <- c()
	} else {
		shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] * 
			sqrt(sum(weights[1:stage]^2) + 	cumsum(weights[(stage + 1):(kMax - 1)]^2)) / 
			sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - 
			stats::qnorm(1 - stageResults$overallPValues[stage]) * sqrt(sum(weights[1:stage]^2)) / 
			sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - 
			thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) / 
			sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
	}
	
	# scaled information for use in getGroupSeqProbs
	scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) / 
			(1 - informationRates[stage])
	
	if (design$sided == 2) {
		decisionMatrix <- (matrix(c(-shiftedDecisionRegion, shiftedDecisionRegion), nrow = 2, byrow = TRUE))
		probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
			informationRates = scaledInformation)
		conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
	} else {
		decisionMatrix <- (matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
			shiftedDecisionRegion), nrow = 2, byrow = TRUE))
		probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
			informationRates = scaledInformation)
		conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
	}
	
	if (stageResults$isTwoSampleDataset()) {
		nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	}	
		
	return(list(nPlanned = nPlanned,
		conditionalPower = conditionalPower
	))	
}

#
# Calculation of conditional power based on inverse normal method
#
.getConditionalPowerRatesInverseNormal <- function(..., design, stageResults, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		pi1, pi2) {
	
	.assertIsTrialDesignInverseNormal(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerRatesInverseNormal", 
		ignore = c("stage"), ...)
	
	kMax <- design$kMax	
	conditionalPower <- rep(NA_real_, kMax)
	weights <- stageResults$weightsInverseNormal
	informationRates <- design$informationRates
	
	nPlanned <- c(rep(NA,stage), nPlanned)
	
	if (stage == kMax) {
		.logDebug("Conditional power will be calculated only for subsequent stages ", 
				"(stage = ", stage, ", kMax = ", design$kMax, ")")
		return(list(
			nPlanned = nPlanned,
			conditionalPower = conditionalPower 
		))
	}
	
	criticalValuesInverseNormal <- design$criticalValues

	# Shifted decision region for use in getGroupSeqProbs 
	# Inverse normal method
	condError <- getConditionalRejectionProbabilities(design = design, 
		stageResults = stageResults, stage = stage)[stage] 
	
	if (stageResults$isOneSampleDataset()) {
		adjustment <- stats::qnorm(1 - condError) * (1 - sqrt(stageResults$thetaH0 * (1 - stageResults$thetaH0)) / 
						sqrt(pi1*(1 - pi1))) / sqrt(sum(nPlanned[(stage + 1) : kMax]))
		if (stageResults$direction == "upper") {
			thetaH1 <- (pi1 - stageResults$thetaH0) / sqrt(pi1*(1 - pi1)) + adjustment
		} else {
			thetaH1 <- -(pi1 - stageResults$thetaH0) / sqrt(pi1*(1 - pi1)) + adjustment
		}
	} 	
	
	if (stageResults$isTwoSampleDataset()) {
		x <- .getFarringtonManningValues(pi1, pi2, stageResults$thetaH0, allocationRatioPlanned)
		adjustment <- stats::qnorm(1 - condError) * (1 - 
					sqrt(x$ml1 * (1 - x$ml1) + allocationRatioPlanned * x$ml2 * (1 - x$ml2)) / 
					sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2))) * 
					(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1) : kMax]))
		if (stageResults$direction == "upper") {
			thetaH1 <- (pi1 - pi2 - stageResults$thetaH0) / sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2)) * sqrt(1 + allocationRatioPlanned) + adjustment
		} else {
			thetaH1 <- -(pi1 - pi2 - stageResults$thetaH0) / sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2)) * sqrt(1 + allocationRatioPlanned) + adjustment
		}	
	} 	
	
	if (stageResults$isTwoSampleDataset()) {
		.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
		nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	}	
		
	shiftedDecisionRegion <- criticalValuesInverseNormal[(stage + 1):kMax] * 
		sqrt(sum(weights[1:stage]^2) + 	cumsum(weights[(stage + 1):kMax]^2)) / 
		sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
		c(weights[1:stage] %*% stats::qnorm(1 - stageResults$pValues[1:stage])) / 
		sqrt(cumsum(weights[(stage + 1):kMax]^2)) - 
		thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):kMax]) *	weights[(stage + 1):kMax]) / 
		sqrt(cumsum(weights[(stage + 1):kMax]^2))
	
	if (stage == kMax - 1) {
		shiftedFutilityBounds <- c()
	} else {
		shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] * 
			sqrt(sum(weights[1:stage]^2) + 	cumsum(weights[(stage + 1):(kMax - 1)]^2)) / 
			sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - 
			c(weights[1:stage] %*% stats::qnorm(1 - stageResults$pValues[1:stage])) / 
			sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - 
			thetaH1 * cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) / 
			sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
	}
	
	# Scaled information for use in getGroupSeqProbs
	scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) / 
			(1 - informationRates[stage])
	
	if (design$sided == 2) {
		decisionMatrix <- (matrix(c(-shiftedDecisionRegion, shiftedDecisionRegion), nrow = 2, byrow = TRUE))
		probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
			informationRates = scaledInformation)
		conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
	} else {
		decisionMatrix <- (matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, shiftedDecisionRegion), 
			nrow = 2, byrow = TRUE))
		probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
			informationRates = scaledInformation)
		conditionalPower[(stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
	}
	
	if (stageResults$isTwoSampleDataset()) {
		nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	}	
	
	return(list(
		nPlanned = nPlanned,
		conditionalPower = conditionalPower
	))	
}

#
# Calculation of conditional power based on Fisher combination test
#
.getConditionalPowerRatesFisher <- function(..., design, stageResults, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		pi1, pi2, iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
	
	.assertIsTrialDesignFisher(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerRatesFisher", 
		ignore = c("stage"), ...)
	
	kMax <- design$kMax
	conditionalPower <- rep(NA_real_, kMax)
	seed <- .setSeed(seed)
	simulated <- FALSE
	
	nPlanned <- c(rep(NA,stage), nPlanned)
	
	condError <- getConditionalRejectionProbabilities(design = design, 
		stageResults = stageResults, stage = stage)[stage] 
	
	if (stageResults$isOneSampleDataset()) {
		adjustment <- stats::qnorm(1 - condError) * (1 - sqrt(stageResults$thetaH0 * (1 - stageResults$thetaH0)) / 
						sqrt(pi1*(1 - pi1))) / sqrt(sum(nPlanned[(stage + 1) : kMax]))
		if (stageResults$direction == "upper") {
			thetaH1 <- (pi1 - stageResults$thetaH0) / sqrt(pi1*(1 - pi1)) + adjustment
		} else {
			thetaH1 <- -(pi1 - stageResults$thetaH0) / sqrt(pi1*(1 - pi1)) + adjustment
		}
	} 	
	
	if (stageResults$isTwoSampleDataset()) {
		x <- .getFarringtonManningValues(pi1, pi2, stageResults$thetaH0, allocationRatioPlanned)
		adjustment <- stats::qnorm(1 - condError) * (1 - 
			sqrt(x$ml1 * (1 - x$ml1) + allocationRatioPlanned * x$ml2 * (1 - x$ml2)) / 
			sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2))) * 
			(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * 
				sum(nPlanned[(stage + 1) : kMax]))
		if (stageResults$direction == "upper") {
			thetaH1 <- (pi1 - pi2 - stageResults$thetaH0) / 
				sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2)) * 
				sqrt(1 + allocationRatioPlanned) + adjustment
		} else {
			thetaH1 <- -(pi1 - pi2 - stageResults$thetaH0) / 
				sqrt(pi1 * (1 - pi1) + allocationRatioPlanned * pi2 * (1 - pi2)) * 
				sqrt(1 + allocationRatioPlanned) + adjustment
		}	
	} 	
	
	if (stageResults$isTwoSampleDataset()) {
		.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")
		nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	}		
	
	criticalValues <- design$criticalValues
	weightsFisher <- stageResults$weightsFisher
	pValues <- stageResults$pValues
	
	if (stage < kMax - 1) {
		for (k in (stage + 1):kMax) {
			reject <- 0
			for (i in 1:iterations) {
				reject <- reject + .getRejectValueConditionalPowerFisher(
					kMax, alpha0Vec = design$alpha0Vec, criticalValues, weightsFisher, 
					pValues, k, thetaH1, stage, nPlanned)
			}
			conditionalPower[k] <- reject / iterations
		}
		simulated <- TRUE
	}
	
	if (stage == kMax - 1) {
		divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
		result <- 1 - (criticalValues[kMax] / divisor)^(1/weightsFisher[kMax])
		if (result <= 0 || result >= 1) {
			warning("Could not calculate conditional power for stage ", kMax, call. = FALSE)
			conditionalPower[kMax] <- NA_real_
		} else {
			conditionalPower[kMax] <- 1 - stats::pnorm(stats::qnorm(result) - thetaH1 * sqrt(nPlanned[kMax]))				
		}
	}
	
	if (stageResults$isTwoSampleDataset()) {
		nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	}	
		
	return(list(
		nPlanned = nPlanned, 
		conditionalPower = conditionalPower, 
		iterations = iterations, 
		seed = seed,
		simulated = simulated
	))	
}

.getConditionalPowerRates <- function(..., design, stageResults,  
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, pi1, pi2) {
		
	.assertIsValidPi(pi1, "pi1")
	.assertIsValidPi(pi2, "pi2")
			
	if (stageResults$isOneSampleDataset()) {
		if (!.associatedArgumentsAreDefined(nPlanned = nPlanned, pi1 = pi1)) {
			return(list(conditionalPower = rep(NA_real_, design$kMax), simulated = FALSE))
		}
		pi2 <- 0
	} else {
		if (!.associatedArgumentsAreDefined(nPlanned = nPlanned, pi1 = pi1, pi2 = pi2)) {
			return(list(conditionalPower = rep(NA_real_, design$kMax), simulated = FALSE))
		}
	}
	
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	
	if (!.isValidNPlanned(nPlanned = nPlanned, kMax = design$kMax, stage = stage)) {
		return(list(conditionalPower = rep(NA_real_, design$kMax), simulated = FALSE))
	}
	
	if (.isTrialDesignGroupSequential(design)) {
		return(.getConditionalPowerRatesGroupSequential(..., 
			design = design, stageResults = stageResults, nPlanned = nPlanned, 
			allocationRatioPlanned = allocationRatioPlanned, pi1 = pi1, pi2 = pi2))
	}
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getConditionalPowerRatesInverseNormal(..., 
			design = design, stageResults = stageResults, nPlanned = nPlanned, 
			allocationRatioPlanned = allocationRatioPlanned, pi1 = pi1, pi2 = pi2))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getConditionalPowerRatesFisher(..., 
			design = design, stageResults = stageResults, nPlanned = nPlanned, 
			allocationRatioPlanned = allocationRatioPlanned, pi1 = pi1, pi2 = pi2))
	}
	
	.stopWithWrongDesignMessage(design)
}

.getConditionalPowerPlotRates <- function(..., design, stageResults, stage, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, piRange, pi2) {
			
	if (stageResults$isOneSampleDataset()) {
		if (!.associatedArgumentsAreDefined(nPlanned = nPlanned, piRange = piRange)) {
			warning("You must specify a planned sample size (nPlanned) and ", 
				"a range of rates (piRange)", call. = FALSE)	
		}
		pi2 <- NA_real_
	} else {
		if (!.associatedArgumentsAreDefined(nPlanned = nPlanned, pi2 = pi2, piRange = piRange)) {
			warning("You must specify a planned sample size (nPlanned), ", 
				"a control rate (pi2), and a range of treatment rates (piRange)", call. = FALSE)
		}
	}
	
	.assertIsValidAllocationRatioPlanned(allocationRatioPlanned, 
		stageResults$getDataInput()$getNumberOfGroups())
	.assertIsValidPi(pi2, "pi2")
	piRange <- .assertIsValidPiRange(piRange = piRange)

	condPowerValues <- rep(NA, length(piRange)) 
	likelihoodValues <- rep(NA, length(piRange))	
	
	if (stageResults$isOneSampleDataset()) {
		mu <- stageResults$effectSizes[stage]
		stdErr <- sqrt(stageResults$effectSizes[stage] * (1 -  stageResults$effectSizes[stage]) / 
				stageResults$overallSampleSizes[stage])
		
		for (i in seq(along = piRange)) {
			if (.isTrialDesignGroupSequential(design)) {
				condPowerValues[i] <- .getConditionalPowerRatesGroupSequential(..., 
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, 
					pi1 = piRange[i], pi2 = pi2)$conditionalPower[design$kMax]
			}
			
			if (.isTrialDesignInverseNormal(design)) {
				condPowerValues[i] <- .getConditionalPowerRatesInverseNormal(..., 
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, 
					pi1 = piRange[i], pi2 = pi2)$conditionalPower[design$kMax]
			}
			
			if (.isTrialDesignFisher(design)) {
				condPowerValues[i] <- .getConditionalPowerRatesFisher(..., 
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, 
					pi1 = piRange[i], pi2 = pi2)$conditionalPower[design$kMax]
			}
			likelihoodValues[i] <- stats::dnorm(piRange[i], mu, stdErr) / stats::dnorm(0, 0, stdErr)
		}
		
	}
	if (stageResults$isTwoSampleDataset()) {
		mu <- stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage]
		stdErr <- sqrt(stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage] * 
			(1 -  stageResults$overallEvents1[stage] / stageResults$overallSampleSizes1[stage]) / 
			stageResults$overallSampleSizes1[stage])
		
		for (i in seq(along = piRange)) {
			if (.isTrialDesignGroupSequential(design)) {
				condPowerValues[i] <- .getConditionalPowerRatesGroupSequential(..., 
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, 
					pi1 = piRange[i], pi2 = pi2)$conditionalPower[design$kMax]
			}
			
			if (.isTrialDesignInverseNormal(design)) {
				condPowerValues[i] <- .getConditionalPowerRatesInverseNormal(..., 
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned,  
					pi1 = piRange[i], pi2 = pi2)$conditionalPower[design$kMax]
			}
			
			if (.isTrialDesignFisher(design)) {
				condPowerValues[i] <- .getConditionalPowerRatesFisher(..., 
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned,  
					pi1 = piRange[i], pi2 = pi2)$conditionalPower[design$kMax]
			}
			likelihoodValues[i] <- stats::dnorm(piRange[i], mu, stdErr) / stats::dnorm(0, 0, stdErr)
		}
	}
	
	
	if (stageResults$isOneSampleDataset()) {
		subTitle <- paste0("Stage = ", stage, ", # of remaining patients = ", sum(nPlanned))
	} else {
		subTitle <- paste0("Stage = ", stage, ", # of remaining patients = ", sum(nPlanned), 
			", pi2 = ", pi2, ", allocation ratio = ", allocationRatioPlanned)
	}
	
	return(list(
		xValues = piRange,
		condPowerValues = condPowerValues,
		likelihoodValues = likelihoodValues,
		main = "Conditional Power Plot with Likelihood",
		xlab = "pi1",
		ylab = "Conditional power / Likelihood",
		sub = subTitle
	))
}	

#
# Calculation of final confidence interval 
# based on group sequential test without SSR (general case).
# 
.getFinalConfidenceIntervalRatesGroupSequential <- function(design, dataInput, stage, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	stageResults <- getStageResultsRates(design = design, dataInput = dataInput, stage = stage, 
		thetaH0 = thetaH0, directionUpper = directionUpper, normalApproximation = normalApproximation)	
	
	finalConfidenceIntervalGeneral <- rep(NA_real_, 2)
	medianUnbiasedGeneral <- NA_real_
	
	stageGroupSeq <- .getStageGroupSeq(design, stageResults, stage)
	
	finalStage <- min(stageGroupSeq, design$kMax)
	
	# early stopping or at end of study
	if (stageGroupSeq < design$kMax || stage == design$kMax) { 
		if (stageGroupSeq == 1) {
			finalConfidenceIntervalGeneral[1] <- stageResults$overallTestStatistics[1] - 
				stats::qnorm(1 - design$alpha / design$sided)
			finalConfidenceIntervalGeneral[2] <- stageResults$overallTestStatistics[1] + 
				stats::qnorm(1 - design$alpha / design$sided)
			medianUnbiasedGeneral <- stageResults$overallTestStatistics[1]
			
			if (dataInput$getNumberOfGroups() == 1) {
				finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral / 
						sqrt(stageResults$overallSampleSizes[1])
				medianUnbiasedGeneral <- medianUnbiasedGeneral / 
						sqrt(stageResults$overallSampleSizes[1])	
			} else {
				finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral * 
						sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage])
				medianUnbiasedGeneral <- medianUnbiasedGeneral * 
						sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage])
			}			
			
		} else {	
			finalConfidenceIntervalGeneral[1] <- .getDecisionMatrixRoot(design = design, 
				stage = finalStage, stageResults = stageResults, tolerance = tolerance, 
				firstParameterName = "overallPValues",
				case = "finalConfidenceIntervalGeneralLower")
			
			finalConfidenceIntervalGeneral[2] <- .getDecisionMatrixRoot(design = design, 
				stage = finalStage, stageResults = stageResults, tolerance = tolerance, 
				firstParameterName = "overallPValues",
				case = "finalConfidenceIntervalGeneralUpper")
			
			medianUnbiasedGeneral <- .getDecisionMatrixRoot(design = design, 
				stage = finalStage, stageResults = stageResults, tolerance = tolerance,  
				firstParameterName = "overallPValues",
				case = "medianUnbiasedGeneral")
		}
	}	

	if (is.na(finalConfidenceIntervalGeneral[1]) && (stageGroupSeq > 1)) {
		finalStage <- NA_integer_
	}	
	
	finalConfidenceInterval <- rep(NA_real_, 2)
	medianUnbiased <- NA_real_
	
	if (!is.na(finalStage)) {
		#  Retransformation  
		if (dataInput$getNumberOfGroups() == 1) {
			stErrRates <- sqrt(stageResults$overallEvents[finalStage]/stageResults$overallSampleSizes[finalStage] * 
				(1 - stageResults$overallEvents[finalStage]/stageResults$overallSampleSizes[finalStage])) /
				sqrt(stageResults$overallSampleSizes[finalStage])
		} else {
			stErrRates <- sqrt(stageResults$overallEvents1[finalStage]/stageResults$overallSampleSizes1[finalStage] *
				(1 - stageResults$overallEvents1[finalStage]/stageResults$overallSampleSizes1[finalStage])/
				stageResults$overallSampleSizes1[finalStage] +
				stageResults$overallEvents2[finalStage]/stageResults$overallSampleSizes2[finalStage] *
				(1 - stageResults$overallEvents2[finalStage]/stageResults$overallSampleSizes2[finalStage])/
				stageResults$overallSampleSizes2[finalStage])
		}			
		
		directionUpperSign <- ifelse(directionUpper, 1, -1)
		
		if (stageGroupSeq == 1) {

			#finalConfidenceInterval[1] <- stageResults$effectSizes[1] - stats::qnorm(1 - design$alpha / design$sided) * stErrRates
			#finalConfidenceInterval[2] <- stageResults$effectSizes[1] + stats::qnorm(1 - design$alpha / design$sided) * stErrRates
			
			finalConfidenceInterval[1] <- .getRootThetaRates(
					design = design, dataInput = dataInput, stage = 1, 
					directionUpper = TRUE, normalApproximation = TRUE, 
					firstParameterName = "overallPValues", secondValue = stats::qnorm(1 - design$alpha/design$sided), tolerance = tolerance,
					acceptResultsOutOfTolerance = TRUE)
			
			finalConfidenceInterval[2] <- .getRootThetaRates(
					design = design, dataInput = dataInput, stage = 1, 
					directionUpper = FALSE, normalApproximation = TRUE, 
					firstParameterName = "overallPValues", secondValue = stats::qnorm(1 - design$alpha/design$sided), tolerance = tolerance,
					acceptResultsOutOfTolerance = TRUE)
			
			medianUnbiased  <- stageResults$effectSizes[1]
			
		} else {	
			
			if (dataInput$getNumberOfGroups() == 1) {
				finalConfidenceInterval[1] <- finalConfidenceIntervalGeneral[1] * 
								sqrt(stageResults$overallSampleSizes[finalStage]) *
								stErrRates +  directionUpperSign * thetaH0  
				finalConfidenceInterval[2] <- finalConfidenceIntervalGeneral[2] *
								sqrt(stageResults$overallSampleSizes[finalStage]) *
								stErrRates + directionUpperSign * thetaH0
				medianUnbiased <- medianUnbiasedGeneral * sqrt(stageResults$overallSampleSizes[finalStage]) *  
								stErrRates + directionUpperSign * thetaH0	
						
			} else {
				
				finalConfidenceInterval[1] <- finalConfidenceIntervalGeneral[1] / 
								sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage]) *  
								stErrRates +  directionUpperSign * thetaH0
				finalConfidenceInterval[2] <- finalConfidenceIntervalGeneral[2] /
								sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage]) *  
								stErrRates + directionUpperSign * thetaH0 
				medianUnbiased <- medianUnbiasedGeneral /
								sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage]) *  
								stErrRates + directionUpperSign * thetaH0
			}			
		}
	}	
	
	if (!directionUpper) {
		medianUnbiasedGeneral = -medianUnbiasedGeneral
		finalConfidenceIntervalGeneral = -finalConfidenceIntervalGeneral
		if (stageGroupSeq > 1){
			medianUnbiased = -medianUnbiased
			finalConfidenceInterval = -finalConfidenceInterval
		}	
	}
	
	finalConfidenceIntervalGeneral = sort(finalConfidenceIntervalGeneral)
	finalConfidenceInterval = sort(finalConfidenceInterval)
	if (dataInput$getNumberOfGroups() == 1) {
		finalConfidenceInterval[1] <- max(0, finalConfidenceInterval[1])
		finalConfidenceInterval[2] <- min(1, finalConfidenceInterval[2])
	} else {
		finalConfidenceInterval[1] <- max(-1, finalConfidenceInterval[1])
		finalConfidenceInterval[2] <- min(1, finalConfidenceInterval[2])
	}	
	
	return(list(
		finalStage = finalStage,
		medianUnbiasedGeneral = medianUnbiasedGeneral,
		finalConfidenceIntervalGeneral = finalConfidenceIntervalGeneral, 
		medianUnbiased = medianUnbiased,
		finalConfidenceInterval = finalConfidenceInterval
	))
}

#
# Calculation of final confidence interval 
# based on inverse normal method, only valid for kMax <= 2 or no SSR.
# 
.getFinalConfidenceIntervalRatesInverseNormal <- function(design, dataInput, stage, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {

	stageResults <- getStageResultsRates(design = design, dataInput = dataInput, stage = stage, 
		thetaH0 = thetaH0, directionUpper = directionUpper, normalApproximation = normalApproximation)	
	
	finalConfidenceIntervalGeneral <- rep(NA_real_, 2)
	medianUnbiasedGeneral <- NA_real_
	
	stageInverseNormal <- .getStageInverseNormal(design, stageResults, stage)
	finalStage <- min(stageInverseNormal, design$kMax)
	
	# Early stopping or at end of study
	if (stageInverseNormal < design$kMax || stage == design$kMax) { 
		
		if (stageInverseNormal == 1) {
			finalConfidenceIntervalGeneral[1] <- stageResults$combInverseNormal[1] - 
				stats::qnorm(1 - design$alpha / design$sided)
			finalConfidenceIntervalGeneral[2] <- stageResults$combInverseNormal[1] + 
				stats::qnorm(1 - design$alpha / design$sided)
			medianUnbiasedGeneral <- stageResults$combInverseNormal[1]
			
			if (dataInput$getNumberOfGroups() == 1) {
				finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral / 
						sqrt(stageResults$overallSampleSizes[1])
				medianUnbiasedGeneral <- medianUnbiasedGeneral / 
						sqrt(stageResults$overallSampleSizes[1])	
			} else {
				finalConfidenceIntervalGeneral <- finalConfidenceIntervalGeneral * 
						sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage])
				medianUnbiasedGeneral <- medianUnbiasedGeneral * 
						sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage])
			}			
			
		} else {
			
			if (design$kMax > 2) {
				warning("Calculation of final confidence interval performed for kMax = ", design$kMax, 
						" (for kMax > 2, it is theoretically shown that it is valid only if no sample size change was performed)", call. = FALSE)
			}
			
			finalConfidenceIntervalGeneral[1] <- .getDecisionMatrixRoot(design = design, 
				stage = finalStage, stageResults = stageResults, tolerance = tolerance, 
				firstParameterName = "combInverseNormal",
				case = "finalConfidenceIntervalGeneralLower")
			
			finalConfidenceIntervalGeneral[2] <- .getDecisionMatrixRoot(design = design, 
				stage = finalStage, stageResults = stageResults, tolerance = tolerance, 
				firstParameterName = "combInverseNormal",
				case = "finalConfidenceIntervalGeneralUpper")
			
			medianUnbiasedGeneral <- .getDecisionMatrixRoot(design = design, 
				stage = finalStage, stageResults = stageResults, tolerance = tolerance,  
				firstParameterName = "combInverseNormal",
				case = "medianUnbiasedGeneral")
		}
	}	
	
	if (is.na(finalConfidenceIntervalGeneral[1]) && (stageInverseNormal > 1)) {
		finalStage <- NA_integer_
	}	
	
	finalConfidenceInterval <- rep(NA_real_, 2)
	medianUnbiased <- NA_real_
	
	if (!is.na(finalStage)) {
		# Retransformation  
		if (dataInput$getNumberOfGroups() == 1) {
			stErrRates <- sqrt(stageResults$overallEvents[finalStage]/stageResults$overallSampleSizes[finalStage] * 
				(1 - stageResults$overallEvents[finalStage]/stageResults$overallSampleSizes[finalStage])) /
				sqrt(stageResults$overallSampleSizes[finalStage])
		} else {
			stErrRates <- sqrt(stageResults$overallEvents1[finalStage]/stageResults$overallSampleSizes1[finalStage] *
				(1 - stageResults$overallEvents1[finalStage]/stageResults$overallSampleSizes1[finalStage])/
				stageResults$overallSampleSizes1[finalStage] +
				stageResults$overallEvents2[finalStage]/stageResults$overallSampleSizes2[finalStage] *
				(1 - stageResults$overallEvents2[finalStage]/stageResults$overallSampleSizes2[finalStage])/
				stageResults$overallSampleSizes2[finalStage])
		}			
		
		directionUpperSign <- ifelse(directionUpper, 1, -1)
		
		if (stageInverseNormal == 1) {
			
			#finalConfidenceInterval[1] <- stageResults$effectSizes[1] - stats::qnorm(1 - design$alpha / design$sided) * stErrRates
			#finalConfidenceInterval[2] <- stageResults$effectSizes[1] + stats::qnorm(1 - design$alpha / design$sided) * stErrRates

			finalConfidenceInterval[1] <- .getRootThetaRates(
					design = design, dataInput = dataInput, stage = 1, 
					directionUpper = TRUE, normalApproximation = T, 
					firstParameterName = "combInverseNormal", secondValue = stats::qnorm(1 - design$alpha/design$sided), tolerance = tolerance,
					acceptResultsOutOfTolerance = TRUE)
			
			finalConfidenceInterval[2] <- .getRootThetaRates(
					design = design, dataInput = dataInput, stage = 1, 
					directionUpper = FALSE, normalApproximation = T, 
					firstParameterName = "combInverseNormal", secondValue = stats::qnorm(1 - design$alpha/design$sided), tolerance = tolerance,
					acceptResultsOutOfTolerance = TRUE)

			medianUnbiased  <- stageResults$effectSizes[1]
			
		} else {	
			
			if (dataInput$getNumberOfGroups() == 1) {
				finalConfidenceInterval[1] <- finalConfidenceIntervalGeneral[1] * 
					sqrt(stageResults$overallSampleSizes[finalStage]) *
					stErrRates +  directionUpperSign * thetaH0  
				finalConfidenceInterval[2] <- finalConfidenceIntervalGeneral[2] *
					sqrt(stageResults$overallSampleSizes[finalStage]) *
					stErrRates + directionUpperSign * thetaH0
				medianUnbiased <- medianUnbiasedGeneral * sqrt(stageResults$overallSampleSizes[finalStage]) *  
					stErrRates + directionUpperSign * thetaH0	
			} else {
				finalConfidenceInterval[1] <- finalConfidenceIntervalGeneral[1] / 
					sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage]) *  
					stErrRates +  directionUpperSign * thetaH0
				finalConfidenceInterval[2] <- finalConfidenceIntervalGeneral[2] /
					sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage]) *  
					stErrRates + directionUpperSign * thetaH0 
				medianUnbiased <- medianUnbiasedGeneral /
					sqrt(1/stageResults$overallSampleSizes1[finalStage] + 1/stageResults$overallSampleSizes2[finalStage]) *  
					stErrRates + directionUpperSign * thetaH0
			}			
		}
	}	
	
	if (!directionUpper){
		medianUnbiasedGeneral = -medianUnbiasedGeneral
		finalConfidenceIntervalGeneral = -finalConfidenceIntervalGeneral
		if (stageInverseNormal > 1){
			medianUnbiased = -medianUnbiased
			finalConfidenceInterval = -finalConfidenceInterval
		}	
	}

	finalConfidenceIntervalGeneral = sort(finalConfidenceIntervalGeneral)
	finalConfidenceInterval = sort(finalConfidenceInterval)

	if (dataInput$getNumberOfGroups() == 1) {
		finalConfidenceInterval[1] <- max(0, finalConfidenceInterval[1])
		finalConfidenceInterval[2] <- min(1, finalConfidenceInterval[2])
	} else {
		finalConfidenceInterval[1] <- max(-1, finalConfidenceInterval[1])
		finalConfidenceInterval[2] <- min(1, finalConfidenceInterval[2])
	}	
	
	return(list(
		finalStage = finalStage,
		medianUnbiasedGeneral = medianUnbiasedGeneral,
		finalConfidenceIntervalGeneral = finalConfidenceIntervalGeneral, 
		medianUnbiased = medianUnbiased,
		finalConfidenceInterval = finalConfidenceInterval
	))
}

#
# Calculation of final confidence interval 	
# based on Fisher combination test, only valid for kMax <= 2.
#
.getFinalConfidenceIntervalRatesFisher <- function(design, dataInput, stage, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	stageResults <- getStageResultsRates(design = design, dataInput = dataInput, stage = stage, 
		thetaH0 = thetaH0, directionUpper = directionUpper, normalApproximation = normalApproximation)	
	
	finalConfidenceInterval <- rep(NA_real_, 2)
	medianUnbiased <- NA_real_
	
	stageFisher <- .getStageFisher(design, stageResults, stage)
	
	finalStage <- min(stageFisher, design$kMax)
	
	# Early stopping or at end of study
	if (stageFisher < design$kMax || stage == design$kMax) {
		
		warning("Calculation of final confidence interval for Fisher's design not implemented yet.", call. = FALSE)
		return(list(finalStage = NA_integer_ , medianUnbiased = NA_real_, 
						finalConfidenceInterval = rep(NA_real_, design$kMax)))
		
	}
	
	return(list(
		finalStage = finalStage,
		medianUnbiased = medianUnbiased, 
		finalConfidenceInterval = finalConfidenceInterval
	))
}

getFinalConfidenceIntervalRates <- function(..., design, dataInput, 
		thetaH0 = NA_real_, directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) { 
	
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidStage(stage, design$kMax)
	.assertIsValidThetaH0(thetaH0, dataInput)
	.warnInCaseOfUnknownArguments(functionName = "getFinalConfidenceIntervalRates", ignore = c("stage"), ...)
	
	if (is.na(thetaH0)) {
		thetaH0 <- C_THETA_H0_RATES_DEFAULT
	}
	
	if (.isTrialDesignGroupSequential(design)) {
		return(.getFinalConfidenceIntervalRatesGroupSequential(
			design = design, dataInput = dataInput, stage = stage, thetaH0 = thetaH0, 
			directionUpper = directionUpper, normalApproximation = normalApproximation,
			tolerance = tolerance))
	}
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getFinalConfidenceIntervalRatesInverseNormal(
			design = design, dataInput = dataInput, stage = stage, thetaH0 = thetaH0, 
			directionUpper = directionUpper, normalApproximation = normalApproximation, 
			tolerance = tolerance))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getFinalConfidenceIntervalRatesFisher(
			design = design, dataInput = dataInput, stage = stage, thetaH0 = thetaH0, 
			directionUpper = directionUpper, normalApproximation = normalApproximation, 
			tolerance = tolerance))
	}
	
	.stopWithWrongDesignMessage(design)
}

