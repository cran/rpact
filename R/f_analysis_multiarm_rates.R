#:#
#:#  *Analysis of Rates in multi-armed designs with adaptive test* 
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
#:#  File version: $Revision: 3635 $
#:#  Last changed: $Date: 2020-09-14 13:31:28 +0200 (Mo, 14 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

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
.getAnalysisResultsRatesMultiArm <- function(..., design, dataInput) {
	if (.isTrialDesignInverseNormal(design)) {
		return(.getAnalysisResultsRatesInverseNormalMultiArm(design = design, 
				dataInput = dataInput, ...))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getAnalysisResultsRatesFisherMultiArm(design = design, 
				dataInput = dataInput, ...))
	}
	
	if (.isTrialDesignConditionalDunnett(design)) {
		return(.getAnalysisResultsRatesConditionalDunnettMultiArm(design = design, 
				dataInput = dataInput, ...))
	}

	.stopWithWrongDesignMessage(design)
}

.getAnalysisResultsRatesInverseNormalMultiArm <- function(...,
		design, dataInput,
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, piTreatments = NA_real_, 
		piControl = NA_real_, nPlanned = NA_real_, 
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	.assertIsTrialDesignInverseNormal(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
	.warnInCaseOfUnknownArguments(functionName = ".getAnalysisResultsRatesInverseNormalMultiArm", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	results <- AnalysisResultsMultiArmInverseNormal(design = design, dataInput = dataInput)
	
	results <- .getAnalysisResultsRatesMultiArmAll(results = results, design = design, dataInput = dataInput, 
		intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper, 
		normalApproximation = normalApproximation, 
		thetaH0 = thetaH0, piTreatments = piTreatments, piControl = piControl, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, 
		tolerance = tolerance)

	return(results)
}

.getAnalysisResultsRatesFisherMultiArm <- function(...,
		design, dataInput, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, 
		piTreatments = NA_real_, piControl = NA_real_, nPlanned = NA_real_, 
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT, 
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
	
	.assertIsTrialDesignFisher(design)
	.assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
	.warnInCaseOfUnknownArguments(functionName = ".getAnalysisResultsRatesFisherMultiArm", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	results <- AnalysisResultsMultiArmFisher(design = design, dataInput = dataInput)
	.setValueAndParameterType(results, "iterations", as.integer(iterations), C_ITERATIONS_DEFAULT)
	.setValueAndParameterType(results, "seed", seed, NA_real_)
	
	results <- .getAnalysisResultsRatesMultiArmAll(results = results, design = design, dataInput = dataInput, 
		intersectionTest = intersectionTest, stage = stage, directionUpper = directionUpper, 
		normalApproximation = normalApproximation, 
		thetaH0 = thetaH0, piTreatments = piTreatments, piControl = piControl, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, 
		tolerance = tolerance,
		iterations = iterations, seed = seed)
	
	return(results)
}

.getAnalysisResultsRatesConditionalDunnettMultiArm <- function(..., 
		design, dataInput, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,		
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		thetaH0 = C_THETA_H0_RATES_DEFAULT, piTreatments = NA_real_, piControl = NA_real_, nPlanned = NA_real_, 
		allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	.assertIsTrialDesignConditionalDunnett(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
	.warnInCaseOfUnknownArguments(functionName = ".getAnalysisResultsRatesConditionalDunnettMultiArm", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	results <- AnalysisResultsConditionalDunnett(design = design, dataInput = dataInput) 
	
	results <- .getAnalysisResultsRatesMultiArmAll(results = results, design = design, 
		dataInput = dataInput, intersectionTest = intersectionTest,
		stage = stage, directionUpper = directionUpper, normalApproximation = normalApproximation, 
		thetaH0 = thetaH0, piTreatments = piTreatments, piControl = piControl, nPlanned = nPlanned, 
		allocationRatioPlanned = allocationRatioPlanned, 
		tolerance = tolerance,
		iterations = iterations, seed = seed)
	
	return(results)
}

.getAnalysisResultsRatesMultiArmAll <- function(..., results, design, dataInput, intersectionTest, stage, 
		directionUpper, normalApproximation, thetaH0, piTreatments, piControl, nPlanned, allocationRatioPlanned, 
		tolerance, iterations, seed) {
	
	startTime <- Sys.time()
	
	intersectionTest <- .getCorrectedIntersectionTestIfNecessary(design, intersectionTest)
	
	stageResults <- .getStageResultsRatesMultiArm(design = design, dataInput = dataInput, 
			intersectionTest = intersectionTest, stage = stage, 
			thetaH0 = thetaH0, directionUpper = directionUpper, 
			normalApproximation = normalApproximation)
	results$.stageResults <- stageResults
	.logProgress("Stage results calculated", startTime = startTime)
	gMax <- nrow(stageResults$testStatistics)
	
	.assertIsValidAllocationRatioPlanned(allocationRatioPlanned, dataInput$getNumberOfGroups())

	piControl <- .assertIsValidPiControlForMultiArm(piControl, stageResults, stage, results = results)
	piTreatments <- .assertIsValidPiTreatmentsForMultiArm(piTreatments, stageResults, stage, results = results)
	
	.setValueAndParameterType(results, "intersectionTest", intersectionTest, C_INTERSECTION_TEST_MULTIARMED_DEFAULT)
	.setValueAndParameterType(results, "directionUpper", directionUpper, C_DIRECTION_UPPER_DEFAULT)
	.setValueAndParameterType(results, "normalApproximation", normalApproximation, C_NORMAL_APPROXIMATION_MEANS_DEFAULT)
	.setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
	.setValueAndParameterType(results, "thetaH0", thetaH0, C_THETA_H0_MEANS_DEFAULT)
	if (results$.getParameterType("piControl") %in% c(C_PARAM_TYPE_UNKNOWN, C_PARAM_NOT_APPLICABLE)) {
		.setValueAndParameterType(results, "piControl", matrix(piControl, ncol = 1), matrix(rep(NA_real_, gMax), ncol = 1))
	} else {
		results$piControl <- matrix(piControl, ncol = 1)
	}
	if (results$.getParameterType("piTreatments") == C_PARAM_TYPE_UNKNOWN) {
		.setValueAndParameterType(results, "piTreatments", matrix(piTreatments, ncol = 1), matrix(rep(NA_real_, gMax), ncol = 1))
	} else {
		results$piTreatments <- matrix(piTreatments, ncol = 1)
	}
	
	.setValueAndParameterType(results, "nPlanned", nPlanned, NA_real_)
	while (length(results$nPlanned) < design$kMax) {
		results$nPlanned <- c(NA_real_, results$nPlanned)
	}
	if (design$kMax == 1) {
		results$.setParameterType("nPlanned", C_PARAM_NOT_APPLICABLE)
	}
	
	startTime <- Sys.time()
	if (!.isTrialDesignConditionalDunnett(design)) {
		results$.closedTestResults <- getClosedCombinationTestResults(stageResults = stageResults)
	} else {
		results$.closedTestResults <- getClosedConditionalDunnettTestResults(
			stageResults = stageResults, design = design, stage = stage)
	}
	.logProgress("Closed test calculated", startTime = startTime)
	
	if (design$kMax > 1) {
		
		# conditional power
		startTime <- Sys.time()
		if (.isTrialDesignFisher(design)) {
			conditionalPowerResults <- .getConditionalPowerRatesMultiArm(stageResults = stageResults, 
				stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
				piTreatments = piTreatments, piControl = piControl, iterations = iterations, seed = seed)
			if (conditionalPowerResults$simulated) {
				results$conditionalPowerSimulated <- conditionalPowerResults$conditionalPower
				results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
				results$.setParameterType("conditionalPowerSimulated", C_PARAM_GENERATED)
			} else {
				results$conditionalPower <- conditionalPowerResults$conditionalPower
				results$conditionalPowerSimulated <- matrix(numeric(0))
				results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
				results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
			}
		} else {
			conditionalPowerResults <- .getConditionalPowerRatesMultiArm(stageResults = stageResults, 
				stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
				piTreatments = piTreatments, piControl = piControl)
			results$conditionalPower <- conditionalPowerResults$conditionalPower 
			results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
		}
		results$.conditionalPowerResults <- conditionalPowerResults
		.logProgress("Conditional power calculated", startTime = startTime)
		
		# CRP - conditional rejection probabilities
		startTime <- Sys.time()
		results$conditionalRejectionProbabilities <- .getConditionalRejectionProbabilitiesMultiArm(
			stageResults = stageResults, stage = stage)
		results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_GENERATED)
		.logProgress("Conditional rejection probabilities (CRP) calculated", startTime = startTime)
	} else {
		results$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
		results$.setParameterType("conditionalPowerSimulated", C_PARAM_NOT_APPLICABLE)
		results$.setParameterType("conditionalRejectionProbabilities", C_PARAM_NOT_APPLICABLE)
	}
	
	# RCI - repeated confidence interval
	repeatedConfidenceIntervals <- .getRepeatedConfidenceIntervalsRatesMultiArm(
		design = design, dataInput = dataInput, 
		intersectionTest = intersectionTest, stage = stage, 
		normalApproximation = normalApproximation, tolerance = tolerance)
	results$repeatedConfidenceIntervalLowerBounds <- 
		matrix(rep(NA_real_, gMax * design$kMax), nrow = gMax, ncol = design$kMax) 
	results$repeatedConfidenceIntervalUpperBounds <- results$repeatedConfidenceIntervalLowerBounds
	for (k in 1:design$kMax) {
		for (treatmentArm in 1:gMax) {
			results$repeatedConfidenceIntervalLowerBounds[treatmentArm, k] <- repeatedConfidenceIntervals[treatmentArm, 1, k]
			results$repeatedConfidenceIntervalUpperBounds[treatmentArm, k]  <- repeatedConfidenceIntervals[treatmentArm, 2, k]
		}
	}
	results$.setParameterType("repeatedConfidenceIntervalLowerBounds", C_PARAM_GENERATED)
	results$.setParameterType("repeatedConfidenceIntervalUpperBounds", C_PARAM_GENERATED)
	
	# repeated p-value
	if (design$kMax > 1) {	
		startTime <- Sys.time()
		results$repeatedPValues <- .getRepeatedPValuesMultiArm(stageResults = stageResults, tolerance = tolerance)	
		results$.setParameterType("repeatedPValues", C_PARAM_GENERATED)
		.logProgress("Repeated p-values calculated", startTime = startTime)
	}
	
	return(results)
}

.getStageResultsRatesMultiArm <- function(..., design, dataInput,
		thetaH0 = C_THETA_H0_RATES_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		calculateSingleStepAdjusted = FALSE,
		userFunctionCallEnabled = FALSE) {	
	
	.assertIsTrialDesign(design)
	.assertIsDatasetRates(dataInput)
	.assertIsValidThetaH0DataInput(thetaH0, dataInput)
	.assertIsValidDirectionUpper(directionUpper, design$sided)
	.assertIsSingleLogical(normalApproximation, "normalApproximation")
	.assertIsValidIntersectionTest(design, intersectionTest)
	.assertIsSingleLogical(calculateSingleStepAdjusted, "calculateSingleStepAdjusted")
	.warnInCaseOfUnknownArguments(functionName = ".getStageResultsRatesMultiArm", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
	gMax <- dataInput$getNumberOfGroups() - 1
	kMax <- design$kMax
	
	if (.isTrialDesignConditionalDunnett(design)) {
		if (normalApproximation == FALSE) {
			if (userFunctionCallEnabled) {
				warning("'normalApproximation' was set to TRUE ",
					"because conditional Dunnett test was specified as design", call. = FALSE)
			}
			normalApproximation <- TRUE
		}
	}
	
	intersectionTest <- .getCorrectedIntersectionTestIfNecessary(design, intersectionTest, userFunctionCallEnabled)
	
	.assertIsValidIntersectionTest(design, intersectionTest)

	stageResults <- StageResultsMultiArmRates(
		design = design,
		dataInput = dataInput,
		intersectionTest = intersectionTest,
		thetaH0 = thetaH0, 
		direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER), 
		normalApproximation = normalApproximation, 
		directionUpper = directionUpper,
		stage = stage
	)
	
	piControl <- matrix(rep(NA_real_, kMax), 1, kMax)
	piTreatments <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	testStatistics <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	overallTestStatistics <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	separatePValues <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	overallPValues <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)	
	dimnames(testStatistics) = list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
	dimnames(overallTestStatistics) = list(paste("arm ", 1:gMax, sep = ""), 
		paste("stage ", (1:kMax), sep = ""))
	dimnames(separatePValues) = list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
	dimnames(overallPValues) = list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
	
	for (k in 1:stage) {
		
		piControl[1, k] <- dataInput$getOverallEvents(stage = k, group = gMax + 1) / 
			dataInput$getOverallSampleSizes(stage = k, group = gMax + 1)
		
		for (g in 1:gMax) {
			
			piTreatments[g, k] <- dataInput$getOverallEvents(stage = k, group = g) / 
				dataInput$getOverallSampleSizes(stage = k, group = g)
			
			actEv <- dataInput$getEvents(stage = k, group = g)
			ctrEv <- dataInput$getEvents(stage = k, group = gMax + 1)
			actN <- dataInput$getSampleSize(stage = k, group = g)
			ctrN <- dataInput$getSampleSize(stage = k, group = gMax + 1)
					
			if (normalApproximation) {
				if (thetaH0 == 0) {
					if (!is.na(actEv)) {
						if ((actEv + ctrEv == 0) || 
								(actEv + ctrEv == actN + ctrN)) {
							testStatistics[g, k] <- 0
						} else {	
							rateH0 <-  (actEv + ctrEv) / (actN + ctrN)  
							testStatistics[g, k] <- (actEv/actN - ctrEv/ctrN - thetaH0) /
									sqrt(rateH0 * (1 - rateH0) * (1 / actN + 1 / ctrN)) 
						}
					}
				} else {
					y <- .getFarringtonManningValues(rate1 = actEv / actN, 
						rate2 = ctrEv /	ctrN, theta = thetaH0, allocation = actN / ctrN, method = "diff")
					
					testStatistics[g, k] <- 
							(actEv / actN - ctrEv/ctrN - thetaH0) /
							sqrt(y$ml1 * (1 - y$ml1) / actN + y$ml2 * (1 - y$ml2) / ctrN)
				}			
				
				if (directionUpper) {
					separatePValues[g, k] <- 1 - stats::pnorm(testStatistics[g, k])
				} else { 
					separatePValues[g, k] <- stats::pnorm(testStatistics[g, k])
				}
				
			} else {
				
				if (thetaH0 != 0) {
					stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
						"'thetaH0' (", thetaH0, ") must be 0 to perform Fisher's exact test")
				}
				
				if (directionUpper) {
					separatePValues[g, k] <- stats::phyper(actEv - 1, 
							actEv + ctrEv, 
							actN + ctrN - actEv - ctrEv,
							actN,
							lower.tail = FALSE)
					
				} else {
					separatePValues[g, k] <- stats::phyper(actEv, 
							actEv + ctrEv, 
							actN + ctrN - actEv - ctrEv,
							actN,
							lower.tail = TRUE) 
				}
				if (directionUpper) {
					testStatistics <- stats::qnorm(1 - separatePValues)
				} else {
					testStatistics <- -stats::qnorm(1 - separatePValues)
				}	
			}
			
			# overall test statistics
			actEv <- dataInput$getOverallEvents(stage = k, group = g)
			ctrEv <- dataInput$getOverallEvents(stage = k, group = gMax + 1)
			actN <- dataInput$getOverallSampleSize(stage = k, group = g)
			ctrN <- dataInput$getOverallSampleSize(stage = k, group = gMax + 1)
			
			if (normalApproximation) {
				if (thetaH0 == 0) {
					if (!is.na(actEv)) {
						if ((actEv + ctrEv == 0) || 
								(actEv + ctrEv == actN + ctrN)) {
							overallTestStatistics[g, k] <- 0
						} else {	
							overallRateH0 <-  (actEv + ctrEv) / (actN + ctrN)  
							overallTestStatistics[g, k] <- (actEv/actN - ctrEv/ctrN - thetaH0) /
								sqrt(overallRateH0 * (1 - overallRateH0) * (1 / actN +	1 / ctrN)) 
						}
					}
				} else {
					y <- .getFarringtonManningValues(rate1 = actEv / actN, rate2 = ctrEv /	ctrN, 
						theta = thetaH0, allocation = actN / ctrN, method = "diff")
					
					overallTestStatistics[g, k] <- 
						(actEv / actN - ctrEv/ctrN - thetaH0) /
						sqrt(y$ml1 * (1 - y$ml1) / actN + y$ml2 * (1 - y$ml2) / ctrN)
				}			
				
				if (directionUpper) {
					overallPValues[g, k] <- 1 - stats::pnorm(overallTestStatistics[g, k])
				} else { 
					overallPValues[g, k] <- stats::pnorm(overallTestStatistics[g, k])
				}
			} else {
				if (thetaH0 != 0) {
					stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
						"'thetaH0' (", thetaH0, ") must be 0 to perform Fisher's exact test")
				}
				
				if (directionUpper) {
					overallPValues[g, k] <- stats::phyper(actEv - 1, 
							actEv + ctrEv, 
							actN + ctrN - actEv - ctrEv,
							actN,
							lower.tail = FALSE)
					
				} else {
					overallPValues[g, k] <- stats::phyper(actEv, 
							actEv + ctrEv, 
							actN + ctrN - actEv - ctrEv,
							actN,
							lower.tail = TRUE) 
				}
				if (directionUpper) {
					overallTestStatistics <- stats::qnorm(1 - overallPValues)
				} else {
					overallTestStatistics <- -stats::qnorm(1 - overallPValues)
				}	
			}
		}
	}
	
	# Calculation of single stage adjusted p-Values and overall test statistics
	# for determination of RCIs 
	if (calculateSingleStepAdjusted) {
		singleStepAdjustedPValues <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
		combInverseNormal <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
		combFisher <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
		
		if (.isTrialDesignInverseNormal(design)) {	
			weightsInverseNormal <- .getWeightsInverseNormal(design)
		}	
		if (.isTrialDesignFisher(design)) {
			weightsFisher <- .getWeightsFisher(design) 
		}
		for (k in 1:stage) {
			selected <- sum(!is.na(separatePValues[, k]))
			sampleSizesSelected <- as.numeric(na.omit(
				dataInput$getSampleSizes(stage = k, group = -(gMax + 1))))
			sigma <- sqrt(sampleSizesSelected / 
				(sampleSizesSelected + dataInput$getSampleSize(k, gMax + 1))) %*% 
				sqrt(t(sampleSizesSelected / (sampleSizesSelected + 
								dataInput$getSampleSize(k, gMax + 1)))) 
			diag(sigma) <- 1
			for (g in 1:gMax) {
				if ((intersectionTest == "Bonferroni") || (intersectionTest == "Simes")) {
					if (.isTrialDesignGroupSequential(design)) {
						overallPValues[g, k] <- min(1, overallPValues[g, k]*selected)
					} else {
						singleStepAdjustedPValues[g, k] <- min(1, separatePValues[g, k]*selected)
					}	
				} else if (intersectionTest == "Sidak") {
					if (.isTrialDesignGroupSequential(design)) {
						overallPValues[g, k] <- 1 - (1 - overallPValues[g, k])^selected
					} else {	
						singleStepAdjustedPValues[g, k] <- 1 - (1 - separatePValues[g, k])^selected
					}	
				} else if (intersectionTest == "Dunnett") {
					if (!is.na(testStatistics[g, k])) {
						df <- NA_real_
						singleStepAdjustedPValues[g, k] <- 1 - .getMultivariateDistribution(
							type = "normal",
							upper = ifelse(directionUpper, testStatistics[g, k], -testStatistics[g, k]),  
							sigma = sigma, df = df)
					}
				}
				if (.isTrialDesignInverseNormal(design)) {	
					combInverseNormal[g, k] <- (weightsInverseNormal[1:k] %*% 
						stats::qnorm(1 - singleStepAdjustedPValues[g,1:k])) / 
						sqrt(sum(weightsInverseNormal[1:k]^2))
				}
				else if (.isTrialDesignFisher(design)) {	
					combFisher[g, k] <- prod(singleStepAdjustedPValues[g, 1:k]^weightsFisher[1:k])
				}
			}
		}
		
		stageResults$piControl <- piControl
		stageResults$piTreatments <- piTreatments
		stageResults$overallTestStatistics <- overallTestStatistics 
		stageResults$overallPValues <- overallPValues 
		stageResults$testStatistics <- testStatistics 
		stageResults$separatePValues <- separatePValues
		stageResults$singleStepAdjustedPValues <- singleStepAdjustedPValues
		stageResults$.setParameterType("singleStepAdjustedPValues", C_PARAM_GENERATED)
		
		if (.isTrialDesignFisher(design)) {	
			stageResults$combFisher <- combFisher 
			stageResults$.setParameterType("combFisher", C_PARAM_GENERATED)
			
			stageResults$weightsFisher <- weightsFisher
			stageResults$.setParameterType("weightsFisher", C_PARAM_GENERATED)
		}
		else if (.isTrialDesignInverseNormal(design)) {
			stageResults$combInverseNormal <- combInverseNormal 
			stageResults$.setParameterType("combInverseNormal", C_PARAM_GENERATED)
			
			stageResults$weightsInverseNormal <- weightsInverseNormal
			stageResults$.setParameterType("weightsInverseNormal", C_PARAM_GENERATED)
		}
		
	} else {
		stageResults$piControl <- piControl
		stageResults$piTreatments <- piTreatments
		stageResults$overallTestStatistics <- overallTestStatistics
		stageResults$overallPValues <- overallPValues
		stageResults$testStatistics <- testStatistics
		stageResults$separatePValues <- separatePValues
	}
	
	return(stageResults)
}


.getRootThetaRatesMultiArm <- function(..., design, dataInput, treatmentArm, stage, 
		directionUpper, normalApproximation, intersectionTest,
		thetaLow, thetaUp, firstParameterName, secondValue, tolerance) {
	
	result <- .getOneDimensionalRoot( 
		function(theta) {
			stageResults <- .getStageResultsRatesMultiArm(design = design, dataInput = dataInput, 
				stage = stage, thetaH0 = theta, directionUpper = directionUpper, 
				intersectionTest = intersectionTest, normalApproximation = normalApproximation, 
				calculateSingleStepAdjusted = TRUE)
			firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
			if (.isTrialDesignGroupSequential(design)) {
				firstValue <- stats::qnorm(1 - firstValue)
			}
			return(firstValue - secondValue)
		}, lower = thetaLow, upper = thetaUp, tolerance = tolerance,
		callingFunctionInformation = ".getRootThetaRatesMultiArm"
	)
	return(result)
}


.getRepeatedConfidenceIntervalsRatesMultiArmAll <- function(..., 
		design, dataInput,  
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT, 
		firstParameterName) {
		
	.assertIsValidIntersectionTest(design, intersectionTest)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput, design = design)
	
	stageResults <- .getStageResultsRatesMultiArm(design = design, dataInput = dataInput, 
			stage = stage, thetaH0 = 0, directionUpper = directionUpper, 
			intersectionTest = intersectionTest, normalApproximation = normalApproximation, 
			calculateSingleStepAdjusted = FALSE)

	gMax <- dataInput$getNumberOfGroups() - 1
	repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, design$kMax))	

	# Confidence interval for second stage when using conditional Dunnett test
	if (.isTrialDesignConditionalDunnett(design)) {
		for (treatmentArm in 1:gMax) {
			if (!is.na(stageResults$testStatistics[treatmentArm, 2])) {
				
				thetaLow <- -1
				thetaUp <- 1
				
				iteration <- 50
				prec <- 1
				while (prec > tolerance) {
					theta <- (thetaLow + thetaUp) / 2
					stageResults <- .getStageResultsRatesMultiArm(design = design, dataInput = dataInput, 
						stage = stage, thetaH0 = theta, directionUpper = TRUE, 
						intersectionTest = intersectionTest, normalApproximation = TRUE, 
						calculateSingleStepAdjusted = FALSE)
					conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
						design = design, stageResults = stageResults, treatmentArm = treatmentArm)
					ifelse(conditionalDunnettSingleStepRejected, thetaLow <- theta, thetaUp <- theta)					
					ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
					iteration <- iteration - 1 
				}
				repeatedConfidenceIntervals[treatmentArm, 1, 2] <- theta 
				
				thetaLow <- -1
				thetaUp <- 1
							
				iteration <- 50
				prec <- 1
				while (prec > tolerance) {
					theta <- (thetaLow + thetaUp) / 2
					stageResults <- .getStageResultsRatesMultiArm(design = design, dataInput = dataInput, 
						stage = stage, thetaH0 = theta, directionUpper = FALSE, 
						intersectionTest = intersectionTest, normalApproximation = TRUE, 
						calculateSingleStepAdjusted = FALSE)
					conditionalDunnettSingleStepRejected <- .getConditionalDunnettTestForCI(
						design = design, stageResults = stageResults, treatmentArm = treatmentArm)
					ifelse(conditionalDunnettSingleStepRejected, thetaUp <- theta, thetaLow <- theta)					
					ifelse(iteration > 0, prec <- thetaUp - thetaLow, prec <- 0)
					iteration <- iteration - 1
				}
				repeatedConfidenceIntervals[treatmentArm, 2, 2] <- theta 
				
				if (!is.na(repeatedConfidenceIntervals[treatmentArm, 1, 2]) && 
						!is.na(repeatedConfidenceIntervals[treatmentArm, 2, 2]) &&
						repeatedConfidenceIntervals[treatmentArm, 1, 2] > repeatedConfidenceIntervals[treatmentArm, 2, 2]) {
					repeatedConfidenceIntervals[treatmentArm, , 2] <- rep(NA_real_, 2)
				}
			}
		}
		
		
	} else {
		# Repeated onfidence intervals when using combination tests
	
		if (intersectionTest == "Hierarchical") {
			warning("Repeated confidence intervals not available for ", 
				"'intersectionTest' = \"Hierarchical\"", call. = FALSE)
			return(repeatedConfidenceIntervals)
		} 
		
		if (.isTrialDesignFisher(design)) {
			bounds <- design$alpha0Vec
			border <- C_ALPHA_0_VEC_DEFAULT
			criticalValues <- design$criticalValues
			conditionFunction <- .isFirstValueSmallerThanSecondValue		
		} else if (.isTrialDesignInverseNormal(design)) {
			bounds <- design$futilityBounds
			border <- C_FUTILITY_BOUNDS_DEFAULT
			criticalValues <- design$criticalValues
			conditionFunction <- .isFirstValueGreaterThanSecondValue		
		} 
		
		# necessary for adjustment for binding futility boundaries
		futilityCorr <- rep(NA_real_, design$kMax) 
		
		stages <- (1:stage)
		for (k in stages) {
			startTime <- Sys.time()
			for (g in 1:gMax) {
				if (!is.na(stageResults$testStatistics[g, k])) {
					thetaLow <- -1 + tolerance
					thetaUp <- 1 - tolerance
					# finding upper and lower RCI limits through root function
					repeatedConfidenceIntervals[g, 1, k] <- .getRootThetaRatesMultiArm(design = design, 
						dataInput = dataInput, treatmentArm = g, stage = k, directionUpper = TRUE, 
						normalApproximation = normalApproximation, 
						thetaLow = thetaLow, thetaUp = thetaUp, 
						intersectionTest = intersectionTest, firstParameterName = firstParameterName, 
						secondValue = criticalValues[k], tolerance = tolerance) 
					
					repeatedConfidenceIntervals[g, 2, k] <- .getRootThetaRatesMultiArm(design = design, 
						dataInput = dataInput, treatmentArm = g, stage = k, directionUpper = FALSE, 
						normalApproximation = normalApproximation, 
						thetaLow = thetaLow, thetaUp = thetaUp, 
						intersectionTest = intersectionTest, firstParameterName = firstParameterName, 
						secondValue = criticalValues[k], tolerance = tolerance) 
					
					# adjustment for binding futility bounds	
					if (k > 1 && conditionFunction(bounds[k - 1], border) & design$bindingFutility) {
						
						parameterName <- ifelse(.isTrialDesignFisher(design), 
							"singleStepAdjustedPValues", firstParameterName)
						
						futilityCorr[k] <- .getRootThetaRatesMultiArm(design = design, dataInput = dataInput, 
							treatmentArm = g, stage = k - 1, directionUpper = directionUpper, 
							normalApproximation = normalApproximation, 
							thetaLow = thetaLow, thetaUp = thetaUp, 
							intersectionTest = intersectionTest, firstParameterName = parameterName, 
							secondValue = bounds[k - 1], tolerance = tolerance)
						
						if (directionUpper) {
							repeatedConfidenceIntervals[g, 1, k] <- min(min(futilityCorr[2:k]), 
								repeatedConfidenceIntervals[g, 1, k])
						} else {
							repeatedConfidenceIntervals[g, 2, k] <- max(max(futilityCorr[2:k]), 
								repeatedConfidenceIntervals[g, 2, k])
						}	
					}
					
					if (!is.na(repeatedConfidenceIntervals[g, 1, k]) && 
						!is.na(repeatedConfidenceIntervals[g, 2, k]) &&
						repeatedConfidenceIntervals[g, 1, k] > repeatedConfidenceIntervals[g, 2, k]) {
						repeatedConfidenceIntervals[g, , k] <- rep(NA_real_, 2)
					}
				}
			}
			.logProgress("Repeated confidence intervals for stage %s calculated", startTime = startTime, k)		
		}
	}
	
	return(repeatedConfidenceIntervals)
}

# 
# RCIs based on inverse normal combination test	
#
.getRepeatedConfidenceIntervalsRatesMultiArmInverseNormal <- function(..., 
		design, dataInput, 
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	if (!normalApproximation) {
		message("Repeated confidence intervals is calculated under the normal approximation")
		normalApproximation <- TRUE
	}	
	
	.warnInCaseOfUnknownArguments(functionName = 
		".getRepeatedConfidenceIntervalsRatesMultiArmInverseNormal", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsRatesMultiArmAll(design = design, dataInput = dataInput, 
		normalApproximation = normalApproximation, 
		directionUpper = directionUpper, intersectionTest = intersectionTest,
		tolerance = tolerance, firstParameterName = "combInverseNormal", ...))
}

# 
# RCIs based on Fisher's combination test	
#
.getRepeatedConfidenceIntervalsRatesMultiArmFisher <- function(..., 
		design, dataInput,     
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	if (!normalApproximation) {
		message("Repeated confidence intervals will be calculated under the normal approximation")
		normalApproximation <- TRUE
	}	
	
	.warnInCaseOfUnknownArguments(functionName = 
		".getRepeatedConfidenceIntervalsRatesMultiArmFisher", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsRatesMultiArmAll(design = design, dataInput = dataInput, 
		normalApproximation = normalApproximation, 
		directionUpper = directionUpper, intersectionTest = intersectionTest,
		tolerance = tolerance, firstParameterName = "combFisher", ...))
}

# 
# CIs based on conditional Dunnett test	
#
.getRepeatedConfidenceIntervalsRatesMultiArmConditionalDunnett <- function(..., 
		design, dataInput,     
		normalApproximation = C_NORMAL_APPROXIMATION_RATES_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		intersectionTest = C_INTERSECTION_TEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
	
	.warnInCaseOfUnknownArguments(functionName = 
		".getRepeatedConfidenceIntervalsRatesMultiArmConditionalDunnett", 
		ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design), "stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsRatesMultiArmAll(design = design, dataInput = dataInput, 
		normalApproximation = normalApproximation, 
		directionUpper = directionUpper, intersectionTest = intersectionTest,
		tolerance = tolerance, firstParameterName = "condDunnett", ...))
	
}

# 
#  Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Rates
#
.getRepeatedConfidenceIntervalsRatesMultiArm <- function(..., design) {
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getRepeatedConfidenceIntervalsRatesMultiArmInverseNormal(design = design, ...))
	}
	if (.isTrialDesignFisher(design)) {
		return(.getRepeatedConfidenceIntervalsRatesMultiArmFisher(design = design, ...))
	}
	if (.isTrialDesignConditionalDunnett(design)) {
		return(.getRepeatedConfidenceIntervalsRatesMultiArmConditionalDunnett(design = design, ...))
	}
	.stopWithWrongDesignMessage(design)
}

# 
#  Calculation of conditional power for Rates
#
.getConditionalPowerRatesMultiArm <- function(..., stageResults, stage = stageResults$stage	, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		piTreatments = NA_real_, piControl = NA_real_, useAdjustment = TRUE, 
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {

	design <- stageResults$.design
	gMax <- nrow(stageResults$testStatistics)	
	
	if (.isTrialDesignConditionalDunnett(design)) {
		kMax <- 2
	} else {
		kMax <- design$kMax
	}
	
	results <- ConditionalPowerResultsMultiArmRates(
		.design = design,
		.stageResults = stageResults,
		piControl = piControl,
		piTreatments = piTreatments, 
		nPlanned = nPlanned,
		allocationRatioPlanned = allocationRatioPlanned)
	
	if (any(is.na(nPlanned))) {	
		return(results)
	}
	
	.assertIsValidStage(stage, kMax)
	if (stage == kMax) {
		.logDebug("Conditional power will be calculated only for subsequent stages ", 
			"(stage = ", stage, ", kMax = ", kMax, ")")
		return(results)
	}
	
	if (!.isValidNPlanned(nPlanned = nPlanned, kMax = kMax, stage = stage)) {
		return(results)
	}
	
	.assertIsValidNPlanned(nPlanned, kMax, stage)
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")	
	.assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
	
	piControl <- .assertIsValidPiControlForMultiArm(piControl, stageResults, stage, results = results)
	piTreatments <- .assertIsValidPiTreatmentsForMultiArm(piTreatments, stageResults, stage, results = results)
	
	if ((length(piTreatments) != 1) && (length(piTreatments) != gMax)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			sprintf(paste0("length of 'piTreatments' (%s) ",
					"must be equal to 'gMax' (%s) or 1"), .arrayToString(piTreatments), gMax))
	}
	
	if (length(piControl) != 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			sprintf(paste0("length of 'piControl' (%s) must be equal to 1"), .arrayToString(piControl)))
	}
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getConditionalPowerRatesMultiArmInverseNormal(results = results,
			design = design, stageResults = stageResults, stage = stage, 
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			piControl = piControl,
			piTreatments = piTreatments, ...))
	}
	else if (.isTrialDesignFisher(design)) {
		return(.getConditionalPowerRatesMultiArmFisher(results = results,
			design = design, stageResults = stageResults,  stage = stage,
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			useAdjustment = useAdjustment, 
			piControl = piControl,
			piTreatments = piTreatments, 
			iterations = iterations, seed = seed, ...))
	}
	else if (.isTrialDesignConditionalDunnett(design)) {
		return(.getConditionalPowerRatesMultiArmConditionalDunnett(results = results,
			design = design, stageResults = stageResults, stage = stage, 
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			piControl = piControl,
			piTreatments = piTreatments, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
		"'design' must be an instance of TrialDesignInverseNormal, TrialDesignFisher, ",
		"or TrialDesignConditionalDunnett")
}

#
# Calculation of conditional power based on inverse normal method
#
.getConditionalPowerRatesMultiArmInverseNormal <- function(..., results, design, stageResults, stage,
		allocationRatioPlanned, nPlanned, piTreatments, piControl) {
	
	.assertIsTrialDesignInverseNormal(design)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerRatesMultiArmInverseNormal", ...)
	
	kMax <- design$kMax	
	gMax <- nrow(stageResults$testStatistics)	
	weights <- .getWeightsInverseNormal(design)
	informationRates <- design$informationRates
	
	nPlanned <- c(rep(NA_real_, stage), nPlanned)	
	
	condError <- .getConditionalRejectionProbabilitiesMultiArm(design = design, stageResults = stageResults)[,stage]

	ml <- (allocationRatioPlanned * piTreatments + piControl) / (1 + allocationRatioPlanned)	
	adjustment <- stats::qnorm(1 - condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) / 
		sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControl * (1 - piControl))) * 
		(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):kMax]))

	.setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
	results$.setParameterType("piControl", C_PARAM_DEFAULT_VALUE)
	if (length(piTreatments) == 1) {
		piTreatments <- rep(piTreatments, gMax)
		results$.setParameterType("piTreatments", C_PARAM_GENERATED)
	} else {
		results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
	}
	
	if (stageResults$directionUpper) {
		standardizedEffect <- (piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) + 
			allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
	} else {
		standardizedEffect <- -(piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) + 
			allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
	}
	
	nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	
	ctr <- .performClosedCombinationTest(stageResults = stageResults)
	criticalValues <- design$criticalValues	
	
	for (g in 1:gMax) {
		
		if (!is.na(ctr$separatePValues[g, stage])) {
			# shifted decision region for use in getGroupSeqProbs 
			# Inverse Normal Method
			shiftedDecisionRegionUpper <- criticalValues[(stage + 1):kMax] * 
				sqrt(sum(weights[1:stage]^2) + cumsum(weights[(stage + 1):kMax]^2)) / 
				sqrt(cumsum(weights[(stage + 1):kMax]^2)) -
				min(ctr$overallAdjustedTestStatistics[ctr$indices[,g] == 1,stage], na.rm = TRUE) *
				sqrt(sum(weights[1:stage]^2)) / 
				sqrt(cumsum(weights[(stage + 1):kMax]^2)) - standardizedEffect[g] * 
				cumsum(sqrt(nPlanned[(stage + 1):kMax]) * weights[(stage + 1):kMax]) / 
				sqrt(cumsum(weights[(stage + 1):kMax]^2))
			if (stage == kMax - 1) {
				shiftedFutilityBounds <- c()
			} else {
				shiftedFutilityBounds <- design$futilityBounds[(stage + 1):(kMax - 1)] * 
					sqrt(sum(weights[1:stage]^2) + 	cumsum(weights[(stage + 1):(kMax - 1)]^2)) / 
					sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - 
					min(ctr$overallAdjustedTestStatistics[ctr$indices[,g] == 1,stage], na.rm = TRUE) *
					sqrt(sum(weights[1:stage]^2)) / 
					sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2)) - standardizedEffect[g] * 
					cumsum(sqrt(nPlanned[(stage + 1):(kMax - 1)]) * weights[(stage + 1):(kMax - 1)]) / 
					sqrt(cumsum(weights[(stage + 1):(kMax - 1)]^2))
			}
			
			# scaled information for use in getGroupSeqProbs
			scaledInformation <- (informationRates[(stage + 1):kMax] - informationRates[stage]) / 
				(1 - informationRates[stage])
			
			decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
				shiftedDecisionRegionUpper), nrow = 2, byrow = TRUE)
			
			probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
				informationRates = scaledInformation)
			
			results$conditionalPower[g, (stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
		}	
	}	
	nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	results$nPlanned <- nPlanned
	results$.setParameterType("nPlanned", C_PARAM_GENERATED)
	
	results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
	
	results$piTreatments <- piTreatments
	results$piControl <- piControl
	return(results)
}

#
# Calculation of conditional power based on Fisher's combination test
#
.getConditionalPowerRatesMultiArmFisher <- function(..., results, design, stageResults, stage,  
		allocationRatioPlanned, nPlanned, piTreatments, piControl, useAdjustment = TRUE,
		iterations, seed) {
	
	.assertIsTrialDesignFisher(design)
	.assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerRatesMultiArmFisher", ...)

	kMax <- design$kMax	
	gMax <- nrow(stageResults$testStatistics)
	criticalValues <- design$criticalValues
	weightsFisher <- .getWeightsFisher(design) 
	
	results$iterations <- as.integer(iterations) 
	results$.setParameterType("iterations", C_PARAM_USER_DEFINED)
	results$.setParameterType("seed", ifelse(is.na(seed), C_PARAM_GENERATED, C_PARAM_USER_DEFINED))
	results$seed <- .setSeed(seed)
	results$simulated <- FALSE
	results$.setParameterType("simulated", C_PARAM_DEFAULT_VALUE)
	
	nPlanned <- c(rep(NA_real_, stage), nPlanned)
	
	if (useAdjustment) {
		condError <- .getConditionalRejectionProbabilitiesMultiArm(design = design, stageResults = stageResults, 
			iterations = iterations, seed = seed)[, stage]
		
		ml <- (allocationRatioPlanned * piTreatments + piControl) / (1 + allocationRatioPlanned)
		
		adjustment <- stats::qnorm(1 - condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) / 
			sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControl * (1 - piControl))) * 
			(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):kMax]))
	} else {
		adjustment <- 0
	}	
	
	.setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
	if (length(piTreatments) == 1) {
		piTreatments <- rep(piTreatments, gMax)
		results$.setParameterType("piTreatments", C_PARAM_GENERATED)
	} else {
		results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
	}
	
	if (stageResults$directionUpper) {
		standardizedEffect <- (piTreatments - piControl) / sqrt(piTreatments * (1 - piTreatments) + 
			allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
	} else {
		standardizedEffect <- -(piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) + 
			allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
	}	
	
	nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned	
	
	ctr <- .performClosedCombinationTest(stageResults = stageResults)
	for (g in 1:gMax) {
		if (!is.na(ctr$separatePValues[g, stage])) {
			if (gMax == 1) {
				pValues <- ctr$adjustedStageWisePValues[ctr$indices[,g] == 1,][1:stage]
			} else {
				pValues <- ctr$adjustedStageWisePValues[ctr$indices[,g] == 1,][which.max(
					ctr$overallAdjustedTestStatistics[ctr$indices[,g] == 1,stage]), 1:stage]
			}	
			if (stage < kMax - 1) {		
				for (k in (stage + 1):kMax) {
					reject <- 0
					for (i in 1:iterations) {
						reject <- reject + .getRejectValueConditionalPowerFisher(
							kMax = kMax, alpha0Vec = design$alpha0Vec, 
							criticalValues = criticalValues, weightsFisher = weightsFisher, 
							pValues = pValues, currentKMax = k, thetaH1 = standardizedEffect[g], 
							stage = stage, nPlanned = nPlanned)
					}
					results$conditionalPower[g, k] <- reject / iterations
				}
				results$simulated <- TRUE
				results$.setParameterType("simulated", C_PARAM_GENERATED)
			}
			else if (stage == kMax - 1) {
				divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
				result <- 1 - (criticalValues[kMax] / divisor)^(1/weightsFisher[kMax])
				
				if (result <= 0 || result >= 1) {
					warning("Calculation not possible: could not calculate conditional power for stage ", kMax, call. = FALSE)
					results$conditionalPower[g, kMax] <- NA_real_
				} else {
					results$conditionalPower[g, kMax] <- 1 - stats::pnorm(stats::qnorm(result) - 
						standardizedEffect[g] * sqrt(nPlanned[kMax]))				
				}
			}
		}
	}	
	
	nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	results$nPlanned <- nPlanned
	results$.setParameterType("nPlanned", C_PARAM_GENERATED)
	
	results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
	
	results$piTreatments <- piTreatments
	results$piControl <- piControl
	return(results)	
}

#
# Calculation of conditional power based on conditional Dunnett test
#
.getConditionalPowerRatesMultiArmConditionalDunnett <- function(..., results, design, stageResults, stage,  
		allocationRatioPlanned, nPlanned, piTreatments, piControl) {
	
	.assertIsTrialDesignConditionalDunnett(design)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerRatesMultiArmConditionalDunnett", 
		ignore = c("intersectionTest"), ...)

	if (stage > 1) {
		warning("Conditional power is only calculated for the first (interim) stage", call. = FALSE)
	}	

	gMax <- nrow(stageResults$testStatistics)	
	nPlanned <- c(rep(NA_real_, stage), nPlanned)	
	condError <- .getConditionalRejectionProbabilitiesMultiArm(design = design, stageResults = stageResults)[, 2]
	
	ml <- (allocationRatioPlanned * piTreatments + piControl) / (1 + allocationRatioPlanned)	
	adjustment <- stats::qnorm(1 - condError) * (1 - sqrt(ml * (1 - ml) * (1 + allocationRatioPlanned)) / 
		sqrt(piTreatments * (1 - piTreatments) + allocationRatioPlanned * piControl * (1 - piControl))) * 
		(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * sum(nPlanned[(stage + 1):2]))

	.setValueAndParameterType(results, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
	if (length(piTreatments) == 1) {
		piTreatments <- rep(piTreatments, gMax)
		results$.setParameterType("piTreatments", C_PARAM_GENERATED)
	} else {
		results$.setParameterType("piTreatments", C_PARAM_DEFAULT_VALUE)
	}

	nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	if (stageResults$directionUpper) {
		standardizedEffect <- (piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) + 
			allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
	} else {
		standardizedEffect <- -(piTreatments - piControl - stageResults$thetaH0) / sqrt(piTreatments * (1 - piTreatments) + 
			allocationRatioPlanned * piControl * (1 - piControl)) * sqrt(1 + allocationRatioPlanned) + adjustment
	}
	
	ctr <- .getClosedConditionalDunnettTestResults(stageResults = stageResults, design = design, stage = stage)
	
	for (g in 1:gMax) {
		if (!is.na(ctr$separatePValues[g, stage])) {
			results$conditionalPower[g, 2] <- 1 - 
				stats::pnorm(stats::qnorm(1 - min(ctr$conditionalErrorRate[ctr$indices[,g] == 1,
				stage], na.rm = TRUE)) - standardizedEffect[g] * sqrt(nPlanned[2]))
		}	
	}	
	
	nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	results$nPlanned <- nPlanned
	results$.setParameterType("nPlanned", C_PARAM_GENERATED)
	
	results$.setParameterType("conditionalPower", C_PARAM_GENERATED)
	
	results$piTreatments <- piTreatments
	results$piControl <- piControl
	return(results)
}

#
# Calculation of conditional power and likelihood values for plotting the graph
#
.getConditionalPowerLikelihoodRatesMultiArm <- function(..., stageResults, stage, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		piTreatmentRange, piControl = NA_real_,
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_) {
	
	.associatedArgumentsAreDefined(nPlanned = nPlanned, piTreatmentRange = piTreatmentRange)
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")	
	.assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, C_ALLOCATION_RATIO_MAXIMUM)
	
	design <- stageResults$.design
	kMax <- design$kMax
	gMax <- nrow(stageResults$testStatistics)	
	intersectionTest <- stageResults$intersectionTest
	
	piControl <- .assertIsValidPiControlForMultiArm(piControl, stageResults, stage)
	if (length(piControl) != 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"length of 'piControl' (", .arrayToString(piControl), ") must be equal to 1")
	}
	
	piTreatmentRange <- .assertIsValidPiTreatmentRange(piTreatmentRange = piTreatmentRange)

	treatmentArms <- numeric(gMax * length(piTreatmentRange))
	effectValues <- numeric(gMax * length(piTreatmentRange))
	condPowerValues <- numeric(gMax * length(piTreatmentRange))
	likelihoodValues <- numeric(gMax * length(piTreatmentRange))
	
	stdErr <- sqrt(stageResults$piTreatments[,stage] * (1 - stageResults$piTreatments[,stage])) /
		 sqrt(stageResults$.dataInput$getOverallSampleSizes(stage = stage, group = (1:gMax)))
  
	 results <- ConditionalPowerResultsMultiArmRates(
		 .design = design,
		 .stageResults = stageResults,
		 piControl = piControl,
		 nPlanned = nPlanned,
		 allocationRatioPlanned = allocationRatioPlanned)
	 
	j <- 1
  	for (i in seq(along = piTreatmentRange)) {
		for (g in (1:gMax)) {
			treatmentArms[j] <- g
			effectValues[j] <- piTreatmentRange[i]
			
			if (.isTrialDesignInverseNormal(design)) {
				condPowerValues[j] <- .getConditionalPowerRatesMultiArmInverseNormal(results = results,
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, 
					piControl = piControl,
					piTreatments = piTreatmentRange[i])$conditionalPower[g, kMax]
			}
			else if (.isTrialDesignFisher(design)) {
				condPowerValues[j] <- .getConditionalPowerRatesMultiArmFisher(results = results,
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, useAdjustment = FALSE,
					piControl = piControl,
					piTreatments = piTreatmentRange[i], 
					iterations = iterations, seed = seed)$conditionalPower[g, kMax]
			}
			else if (.isTrialDesignConditionalDunnett(design)) {
				condPowerValues[j] <- .getConditionalPowerRatesMultiArmConditionalDunnett(results = results,
					design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
					allocationRatioPlanned = allocationRatioPlanned, 
					piControl = piControl,
					piTreatments = piTreatmentRange[i])$conditionalPower[g, 2]
			}
			
			likelihoodValues[j] <- stats::dnorm(piTreatmentRange[i], stageResults$piTreatments[g,stage], stdErr[g]) / 
				stats::dnorm(0, 0, stdErr[g])
			j <- j + 1
		}
	}	
	
	assumedPiControlPrint <- paste0("(", .arrayToString(sprintf("%.3f", piControl), encapsulate = FALSE), ")")
	
	subTitle <- paste0("Intersection test = ", intersectionTest, 
		", stage = ", stage, ", # of remaining subjects = ", 
		sum(nPlanned), ", control rate = ", sprintf("%.3f", piControl) , ", allocation ratio = ", allocationRatioPlanned)
	
	return(list(
		treatmentArms = treatmentArms,			
		xValues = effectValues,
		condPowerValues = condPowerValues,
		likelihoodValues = likelihoodValues,
		main = C_PLOT_MAIN_CONDITIONAL_POWER_WITH_LIKELIHOOD,
		xlab = "Treatment rate",
		ylab = C_PLOT_YLAB_CONDITIONAL_POWER_WITH_LIKELIHOOD,
		sub = subTitle
	))
}

