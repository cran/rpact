######################################################################################
#                                                                                    #
# -- Analysis of means in multi-armed designs with adaptive test  --                 #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 19-09-2019                                                                   #
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

#
# WARNING:
# This is a alpha test implementation of multiarmed functions.
# The code is not validated!
#

.getIndicesOfClosedHypothesesSystem <- function(gMax) {
	indices <- as.matrix(expand.grid(rep(list(1:0), gMax)))[1:(2^gMax - 1), ]
	if (gMax == 1) {
		indices <- as.matrix(indices)
	}
	return(indices)
}

.getMultivariateDistribution <- NULL
#.getMultivariateDistribution <- function(type = c("normal", "t", "quantile"), ..., upper, sigma, 
#		df = NA_real_, alpha = NA_real_) {
#	
#	.assertPackageIsInstalled("mnormt")
#	type <- match.arg(type)
#	if (type == "normal") {				
#		return(mnormt::sadmvn(lower = -Inf, upper = upper, mean = 0, varcov = sigma))
#	} else if (type == "t") {
#		return(mnormt::sadmvt(lower = -Inf, upper = upper, mean = 0, S = sigma, df = df))
#	} else if (type == "quantile") {
#		return(.getOneDimensionalRoot(
#			function(x) {
#				return(mnormt::pmnorm(x, varcov = sigma) - (1 - alpha)) 
#			}, 
#			lower = -8, 
#			upper = 8, 
#			tolerance = 1e-08
#		))
#	}
#}

.performClosedCombinationTest <- function(design, stageResults, intersectionTest,
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	dataInput <- stageResults$.dataInput
	stage <- stageResults$stage
	gMax <- dataInput$getNumberOfGroups() - 1
	kMax <- design$kMax
	indices <- .getIndicesOfClosedHypothesesSystem(gMax = gMax)
	
	adjustedStageWisePValues <- matrix(rep(NA_real_, kMax * (2^gMax - 1)), 2^gMax - 1, kMax)
	adjustedOverallPValues <- matrix(rep(NA_real_, kMax * (2^gMax - 1)), 2^gMax - 1, kMax)
	overallAdjustedTestStatistics <- matrix(rep(NA_real_, kMax * (2^gMax - 1)), 2^gMax - 1, kMax)
	rejected <- matrix(rep(NA, gMax * kMax), gMax, kMax)
	
	colnames(adjustedStageWisePValues) = paste("stage ", (1:kMax), sep = "")
	colnames(overallAdjustedTestStatistics) = paste("stage ", (1:kMax), sep = "")
	dimnames(rejected) = list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:kMax), sep = ""))
	
	rejectedIntersections <- matrix(rep(FALSE, stage * nrow(indices)), nrow(indices), stage)
	rejectedIntersectionsBefore <- matrix(rep(FALSE, nrow(indices)), nrow(indices), 1)
	
	if (.isTrialDesignFisher(design)) {
		weightsFisher <- .getWeightsFisher(design) 
	} else {
		weightsInverseNormal <- .getWeightsInverseNormal(design)
	}
	
	for (k in 1:stage) {
		for (i in 1:(2^gMax - 1)) {
			if (!all(is.na(stageResults$separatePValues[indices[i, ] == 1, k]))) {
				if (intersectionTest == "Dunnett") {
					sampleSizesSelected <- as.numeric(na.omit(
						dataInput$getSampleSizes(stage = k, group = 1:gMax)[indices[i, ] == 1]))
					
					sigma <- sqrt(sampleSizesSelected / (sampleSizesSelected + 
						dataInput$getSampleSizes(stage = k, group = gMax + 1))) %*% 
						sqrt(t(sampleSizesSelected / (sampleSizesSelected + 
						dataInput$getSampleSizes(stage = k, group = gMax + 1)))) 
					diag(sigma) <- 1
					maxTestStatistic <- max(stageResults$testStatistics[indices[i, ] == 1, k], 
						na.rm = TRUE)
					df <- NA_real_
					if (!stageResults$normalApproximation) {
						df <- sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
					}
					adjustedStageWisePValues[i, k] <- 1 - multivariateDistributionFunction(
						type = ifelse(stageResults$normalApproximation, "normal", "t"),
						upper = maxTestStatistic, sigma = sigma, df = df)
				}
				
				#  Bonferroni adjusted p-values
				else if (intersectionTest == "Bonferroni") {
					adjustedStageWisePValues[i, k] <- min(c(sum(indices[i, 
						!is.na(stageResults$separatePValues[, k])]) * 
						min(stageResults$separatePValues[indices[i, ] == 1, k], na.rm = TRUE), 1))
				}	
				
				#  Simes adjusted p-values
				else if (intersectionTest == "Simes") {
					adjustedStageWisePValues[i, k] <-  min(sum(indices[i, 
						!is.na(stageResults$separatePValues[, k])]) / 
						(1:sum(indices[i,!is.na(stageResults$separatePValues[, k])])) * 
						sort(stageResults$separatePValues[indices[i, ] == 1, k]))
				}
				
				#  Sidak adjusted p-values
				else if (intersectionTest == "Sidak") {
					adjustedStageWisePValues[i, k] <- 1 - (1 - 
						min(stageResults$separatePValues[indices[i, ] == 1, k], na.rm = TRUE)) ^ 
						sum(indices[i,!is.na(stageResults$separatePValues[, k])])
				}
				
				#  Hierarchically ordered hypotheses
				else if (intersectionTest == "Hierarchical") {
					separatePValues <- stageResults$separatePValues
					separatePValues[is.na(separatePValues[,1:stage])] <- 1
					adjustedStageWisePValues[i, k] <- separatePValues[min(which(indices[i, ] == 1)), k]
				}
				
				if (.isTrialDesignFisher(design)) {
					overallAdjustedTestStatistics[i, k] <- 
						prod(adjustedStageWisePValues[i,1:k]^weightsFisher[1:k])
				} else {
					overallAdjustedTestStatistics[i, k] <- 
						(weightsInverseNormal[1:k] %*% stats::qnorm(1 - adjustedStageWisePValues[i,1:k])) / 
						sqrt(sum(weightsInverseNormal[1:k]^2)) 
				}
			}
		}
		
		if (.isTrialDesignFisher(design)) {
			rejectedIntersections[, k] <- (overallAdjustedTestStatistics[, k] <= design$criticalValues[k])
		} else {
			rejectedIntersections[, k] <- (overallAdjustedTestStatistics[, k] >= design$criticalValues[k])
		}	
		rejectedIntersections[is.na(rejectedIntersections[, k]), k] <- FALSE
		
		rejectedIntersections[, k] <- rejectedIntersections[, k] | rejectedIntersectionsBefore 
		rejectedIntersectionsBefore <- matrix(rejectedIntersections[, k], ncol = 1)
		
		for (j in 1:gMax) {
			rejected[j, k] <- all(rejectedIntersections[indices[, j] == 1, k])
		}
	}
	
	return(list(
		separatePValues	= stageResults$separatePValues,
		indices = indices,			
		adjustedStageWisePValues = adjustedStageWisePValues,
		overallAdjustedTestStatistics =	overallAdjustedTestStatistics,
		rejected = rejected,
		rejectedIntersections = rejectedIntersections
	))
}

getClosedCombinationTestResults <- function(design, stageResults, 
		multivariateDistributionFunction = .getMultivariateDistribution) {
		
	.assertIsValidMultivariateDistributionFunctionDefined(multivariateDistributionFunction)
		
	intersectionTest <- stageResults$intersectionTest
	
	if ((intersectionTest == "Dunnett") && !(stageResults$varianceOption == "overallPooled") && 
		!stageResults$normalApproximation) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"Dunnett t test can only be performed with overall variance estimation.")	
	}
	ctr <- .performClosedCombinationTest(design, stageResults, intersectionTest = intersectionTest,
		multivariateDistributionFunction = multivariateDistributionFunction)
	return(list(
		.design = design,	
		intersectionTest = intersectionTest,			
		indices = ctr$indices,			
		adjustedStageWisePValues = ctr$adjustedStageWisePValues,
		overallAdjustedTestStatistics =	ctr$overallAdjustedTestStatistics,
		rejected = ctr$rejected,
		rejectedIntersections = ctr$rejectedIntersections
	))
}

.assertIsValidMultivariateDistributionFunctionDefined <- function(multivariateDistributionFunction) {
	if (is.null(multivariateDistributionFunction) || !is.function(multivariateDistributionFunction)) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
			"'multivariateDistributionFunction' must define a ",
			"function(type = c(\"normal\", \"t\", \"quantile\"), upper, sigma, df, alpha)")
	}
}

getStageResultsMeansMultiArmed <- function(..., design, dataInput,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		varianceOption = C_VARIANCES_OPTION_DEFAULT,
		intersectionTest = C_INTERSECTIONTEST_MULTIARMED_DEFAULT,
		calculateSingleStepAdjusted = FALSE,
		multivariateDistributionFunction = .getMultivariateDistribution) {	
	
	.assertIsTrialDesign(design)
	.assertIsDatasetMeans(dataInput)
	.assertIsValidThetaH0DataInput(thetaH0, dataInput)
	.warnInCaseOfUnknownArguments(functionName = "getStageResultsMeansMultiArmed", 
		ignore = c("stage"), ...)
	.assertIsValidMultivariateDistributionFunctionDefined(multivariateDistributionFunction)
	
	if (.isTrialDesignGroupSequential(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"group sequential design cannot be used for designs with treatment arm selection")
	}	
	
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	gMax <- dataInput$getNumberOfGroups() - 1
	
	if (.isTrialDesignConditionalDunnett(design)) {
		kMax <- 2
		if (normalApproximation == FALSE) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"specify 'normalApproximation = TRUE' for conditional Dunnett test")
		}
		if (intersectionTest != C_INTERSECTIONTEST_MULTIARMED_DEFAULT) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"'intersectionTest other than Dunnett cannot be specified for conditional Dunnett test")
		}
	} else {
		kMax <- design$kMax
	}
	
	stageResults <- StageResultsMeansMultiArmed(
		design = design,
		dataInput = dataInput,
		intersectionTest = intersectionTest,
		thetaH0 = thetaH0, 
		direction = ifelse(directionUpper, C_DIRECTION_UPPER, C_DIRECTION_LOWER), 
		normalApproximation = normalApproximation, 
		directionUpper = directionUpper,
		varianceOption = varianceOption,
		stage = stage
	)
	
	effectSizes <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	overallStDevs <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
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
		if (varianceOption == "overallPooled") {
			stDev <- sqrt(sum((dataInput$getSampleSizes(stage = k) - 1) * 
				dataInput$getStDevs(stage = k)^2, na.rm = TRUE) /
				sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE))
			overallStDevForTest <- sqrt(sum((dataInput$getOverallSampleSizes(stage = k) - 1) * 
				dataInput$getOverallStDevs(stage = k)^2, na.rm = TRUE) /
				sum(dataInput$getOverallSampleSizes(stage = k) - 1, na.rm = TRUE))
		}
		for (g in 1:gMax) {
			effectSizes[g, k] <- dataInput$getOverallMeans(stage = k, group = g) - 
				dataInput$getOverallMeans(stage = k, group = gMax + 1)
			
			overallStDevs[g, k] <- sqrt(sum((dataInput$getOverallSampleSize(stage = k, 
				group = c(g, gMax + 1)) - 1) * 
				dataInput$getOverallStDev(stage = k, group = c(g, gMax + 1))^2, na.rm = TRUE) /
				sum(dataInput$getOverallSampleSize(stage = k, group = c(g, gMax + 1)) - 1))
			
			if (varianceOption == "pairwisePooled") {
				stDev <- sqrt(sum((dataInput$getSampleSizes(stage = k, group = c(g, gMax + 1)) - 1) * 
					dataInput$getStDevs(stage = k, group = c(g, gMax + 1))^2, na.rm = TRUE) /
					sum(dataInput$getSampleSizes(stage = k, group = c(g, gMax + 1)) - 1))
				overallStDevForTest <- overallStDevs[g, k]
			}				
			
			if (varianceOption == "notPooled") {
				testStatistics[g, k] <- (dataInput$getMeans(stage = k, group = g) - 
					dataInput$getMeans(stage = k, group = gMax + 1) - thetaH0) / 
					sqrt(dataInput$getStDevs(stage = k, group = g)^2 / 
					dataInput$getSampleSizes(stage = k, group = g) + 
					dataInput$getStDevs(stage = k, group = gMax + 1)^2 / 
					dataInput$getSampleSizes(stage = k, group = gMax + 1))
				overallTestStatistics[g, k] <- (dataInput$getOverallMeans(stage = k, group = g) - 
					dataInput$getOverallMeans(stage = k, group = gMax + 1) - thetaH0) / 
					sqrt(dataInput$getOverallStDevs(stage = k, group = g)^2 / 
					dataInput$getOverallSampleSizes(stage = k, group = g) + 
					dataInput$getOverallStDevs(stage = k, group = gMax + 1)^2 / 
					dataInput$getOverallSampleSizes(stage = k, group = gMax + 1))
			} else {
				testStatistics[g, k] <- (dataInput$getMeans(stage = k, group = g) - 
					dataInput$getMeans(stage = k, group = gMax + 1) - thetaH0) / stDev /
					sqrt(1 / dataInput$getSampleSizes(stage = k, group = g) + 1 / 
					dataInput$getSampleSizes(stage = k, group = gMax + 1))
				overallTestStatistics[g, k] <- (dataInput$getOverallMeans(stage = k, group = g) - 
					dataInput$getOverallMeans(stage = k, group = gMax + 1) - thetaH0) / 
					overallStDevForTest /
					sqrt(1 / dataInput$getOverallSampleSizes(stage = k, group = g) + 1 / 
					dataInput$getOverallSampleSizes(stage = k, group = gMax + 1))
			}
			
			if (normalApproximation) {
				separatePValues[g, k] <- 1 - stats::pnorm(testStatistics[g, k])
				overallPValues[g, k] <- 1 - stats::pnorm(overallTestStatistics[g, k])
			} else {
				if (varianceOption == "overallPooled") {
					separatePValues[g, k] <- 1 - stats::pt(testStatistics[g, k], 
					sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE))
					overallPValues[g, k] <- 1 - stats::pt(overallTestStatistics[g, k], 
					sum(dataInput$getOverallSampleSizes(stage = k) - 1, na.rm = TRUE))
					
				} else if (varianceOption == "pairwisePooled") {
					separatePValues[g, k] <- 1 - stats::pt(testStatistics[g, k], 
						sum(dataInput$getSampleSizes(stage = k, group = c(g, gMax + 1)) - 1))
					overallPValues[g, k] <- 1 - stats::pt(overallTestStatistics[g, k], 
						sum(dataInput$getOverallSampleSizes(stage = k, group = c(g, gMax + 1)) - 1))
					
				} else if (varianceOption == "notPooled") {
					u <- dataInput$getStDevs(stage = k, group = g)^2 / 
						dataInput$getSampleSizes(stage = k, group = g) / 
						(dataInput$getStDevs(stage = k, group = g)^2 / 
						dataInput$getSampleSizes(stage = k, group = g) + 
						dataInput$getStDevs(stage = k, group = gMax + 1)^2 / 
						dataInput$getSampleSizes(stage = k, group = gMax + 1))
					separatePValues[g, k] <- 1 - stats::pt(testStatistics[g, k], 
						1 / (u^2 / (dataInput$getSampleSizes(stage = k, group = g) - 1) + 
						(1 - u)^2 / (dataInput$getSampleSizes(stage = k, group = gMax + 1) - 1)))
					u <- dataInput$getOverallStDevs(stage = k, group = g)^2 / 
						dataInput$getOverallSampleSizes(stage = k, group = g) / 
						(dataInput$getOverallStDevs(stage = k, group = g)^2 / 
						dataInput$getOverallSampleSizes(stage = k, group = g) + 
						dataInput$getOverallStDevs(stage = k, group = gMax + 1)^2 / 
						dataInput$getOverallSampleSizes(stage = k, group = gMax + 1))
					overallPValues[g, k] <- 1 - stats::pt(overallTestStatistics[g, k], 
						1 / (u^2 / (dataInput$getOverallSampleSizes(stage = k, group = g) - 1) + 
						(1 - u)^2 / (dataInput$getOverallSampleSizes(stage = k, group = gMax + 1) - 1)))
				}	
			}
			if (!directionUpper) {
				separatePValues[g, k] <- 1 - separatePValues[g, k]
				overallPValues[g, k] <- 1 - overallPValues[g, k]
				testStatistics[g, k] <- -testStatistics[g, k]
				overallTestStatistics[g, k] <- -overallTestStatistics[g, k]
			}
		}
	}
	
	# Calculation of single stage adjusted p-Values and overall test statistics
	# for determination of RCIs and computation of conditional power
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
						if (!normalApproximation) {
							df <- sum(dataInput$getSampleSizes(stage = k) - 1, na.rm = TRUE)
						}
						singleStepAdjustedPValues[g, k] <- 1 - multivariateDistributionFunction(
							type = ifelse(normalApproximation, "normal", "t"),
							upper = testStatistics[g, k], sigma = sigma, df = df)
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
		
		stageResults$overallTestStatistics <- overallTestStatistics 
		stageResults$overallPValues <- overallPValues 
		stageResults$effectSizes <- effectSizes
		stageResults$overallStDevs <- overallStDevs
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
		stageResults$overallTestStatistics <- overallTestStatistics
		stageResults$overallPValues <- overallPValues
		stageResults$effectSizes <- effectSizes
		stageResults$overallStDevs <- overallStDevs
		stageResults$testStatistics <- testStatistics
		stageResults$separatePValues <- separatePValues
	}
	
	return(stageResults)
}

#
# Repeated p-values for multi-armed designs
#
getRepeatedPValuesMultiArmed <- function(design, stageResults, ..., 
		multivariateDistributionFunction = .getMultivariateDistribution) {
		
	.assertIsValidMultivariateDistributionFunctionDefined(multivariateDistributionFunction)
	.warnInCaseOfUnknownArguments(functionName = "getRepeatedPValuesMultiArmed", ignore = c("stage"), ...)
	
	gMax <- nrow(stageResults$testStatistics)	
	
	if (.isTrialDesignConditionalDunnett(design)) {
		kMax <- 2
	} else {
		kMax <- design$kMax
	}	
	
	if (.isTrialDesignInverseNormal(design)) {
		if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
			warning("Repeated p-values not available for 'typeOfDesign' = '", 
				C_TYPE_OF_DESIGN_AS_USER, "'", call. = FALSE)
			return(rep(NA_real_, gMax))
		}
		if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
			warning("Repeated p-values not available for 'typeOfDesign' = '", 
				C_TYPE_OF_DESIGN_HP, "'", call. = FALSE)
			return(rep(NA_real_, gMax))
		}
		if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
			warning("Repeated p-values not available for 'typeOfDesign' = '", 
				C_TYPE_OF_DESIGN_WT_OPTIMUM, "'", call. = FALSE)
			return(rep(NA_real_, gMax))
		}
	}
	
	if (.isTrialDesignFisher(design)) {
		if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
			warning("Repeated p-values not available for 'method' = '", 
				C_FISHER_METHOD_USER_DEFINED_ALPHA, "'", call. = FALSE)
			return(rep(NA_real_, gMax))
		}
	}	
	
	intersectionTest <- stageResults$intersectionTest
	stage <- stageResults$stage
	repeatedPValues <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax) 
	
	startTime <- Sys.time()
	if (.isTrialDesignConditionalDunnett(design)) {
		if (stage == 1 || stage > 2) {
			warning("Repeated p-values can only be calculated for the second stage", call. = FALSE)
			return(rep(NA_real_, gMax))
		}
		for (g in 1:gMax) {
			if (!is.na(stageResults$testStatistics[g, 2])) {	
				prec <- 1
				lower <- 1E-6
				upper <- 0.5
				maxSearchIterations <- 30
				while (prec > 1E-6 && maxSearchIterations >= 0) {
					alpha <- (lower + upper)/2
					ctr <- getConditionalDunnettTest(getDesignConditionalDunnett(
						alpha = alpha, informationAtInterim = 
						design$informationAtInterim, 
						secondStageConditioning = design$secondStageConditioning), 
						stageResults = stageResults, stage = 2)
					ifelse(ctr$rejected[g, 2], upper <- alpha, lower <- alpha)
					prec <- upper - lower
					maxSearchIterations <- maxSearchIterations - 1
				}
				repeatedPValues[g, 2] <- upper
			}	
		}
		return(repeatedPValues)
	} 
	
	for (k in 1:stage) {
		for (g in 1:gMax) {
			if (!is.na(stageResults$testStatistics[g, k])) {	
				prec <- 1
				lower <- 1E-6
				if (.isTrialDesignFisher(design)) {
					upper <- 0.5
				} else {	
					if (design$bindingFutility) {
						upper <- min(0.5, 1 - stats::pnorm(max(design$futilityBounds)))
					} else {
						upper <- 0.5
					}
				}
				maxSearchIterations <- 30
				while (prec > 1E-6 && maxSearchIterations >= 0) {
					alpha <- (lower + upper)/2
					if (.isTrialDesignFisher(design)) {
						designAlpha <- .getDesignFisher(kMax = kMax, alpha = alpha, 
							method = design$method, alpha0Vec = design$alpha0Vec, 
							sided = design$sided, bindingFutility = design$bindingFutility, 
							informationRates = design$informationRates)	
					} else {	
						designAlpha <- .getDesignInverseNormal(kMax = kMax, 
							alpha = alpha, typeOfDesign = design$typeOfDesign, deltaWT = design$deltaWT, 
							gammaA = design$gammaA,	futilityBounds = design$futilityBounds, 
							sided = design$sided, bindingFutility = design$bindingFutility, 
							informationRates = design$informationRates)
					} 
					ctr <- .performClosedCombinationTest(designAlpha, stageResults, 
						intersectionTest = intersectionTest,
						multivariateDistributionFunction = multivariateDistributionFunction)
					ifelse(ctr$rejected[g, k], upper <- alpha, lower <- alpha)
					prec <- upper - lower
					maxSearchIterations <- maxSearchIterations - 1
				}
				repeatedPValues[g, k] <- upper
			}	
		}
		.logProgress("Repeated p-values for stage %s calculated", startTime = startTime, k)		
	}	
	return(repeatedPValues)
}

# 
#  Calculation of lower and upper limits of repeated confidence intervals (RCIs) for Means
#
getRepeatedConfidenceIntervalsMeansMultiArmed <- function(design, ...) {
	if (.isTrialDesignInverseNormal(design)) {
		return(.getRepeatedConfidenceIntervalsMeansMultiArmedInverseNormal(design = design, ...))
	}
	if (.isTrialDesignFisher(design)) {
		return(.getRepeatedConfidenceIntervalsMeansMultiArmedFisher(design = design, ...))
	}
	if (.isTrialDesignConditionalDunnett(design)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"Repeated confidence intervals for conditional Dunnett test not available")
	}
	.stopWithWrongDesignMessage(design)
}

.getRootThetaMeansMultiArmed <- function(design, dataInput, treatmentArm, stage, 
		directionUpper, normalApproximation, varianceOption, intersectionTest,
		thetaLow, thetaUp, firstParameterName, secondValue, tolerance, 
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	result <- .getOneDimensionalRoot( 
		function(theta) {
			stageResults <- getStageResultsMeansMultiArmed(design = design, dataInput = dataInput, 
				stage = stage, thetaH0 = theta, directionUpper = directionUpper, 
				intersectionTest = intersectionTest, normalApproximation = normalApproximation, 
				varianceOption = varianceOption, calculateSingleStepAdjusted = TRUE,
				multivariateDistributionFunction = multivariateDistributionFunction)
			firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
			if (.isTrialDesignGroupSequential(design)) {
				firstValue <- stats::qnorm(1 - firstValue)
			}
			if (.isTrialDesignConditionalDunnett(design)) {
				ctr <- getConditionalDunnettTest(design = design, stageResults = stageResults)
				secondValue <-  min(ctr$conditionalErrorRate[ctr$indices[,treatmentArm] == 1,1], 
				na.rm = TRUE) 
			}	
			return(firstValue - secondValue)
		}, lower = thetaLow, upper = thetaUp, tolerance = tolerance
	)
	return(result)
}

.getUpperLowerThetaMeansMultiArmed <- function(design, dataInput, theta, treatmentArm, stage, 
		directionUpper,	normalApproximation, varianceOption, conditionFunction, intersectionTest,
		firstParameterName, secondValue, multivariateDistributionFunction = .getMultivariateDistribution) {
	
	stageResults <- getStageResultsMeansMultiArmed(design = design, dataInput = dataInput, 
		stage = stage, thetaH0 = theta, directionUpper = directionUpper, 
		intersectionTest = intersectionTest, normalApproximation = normalApproximation, 
		varianceOption = varianceOption, calculateSingleStepAdjusted = TRUE,
		multivariateDistributionFunction = multivariateDistributionFunction)
	
	firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
	maxSearchIterations <- 30
	while (conditionFunction(secondValue, firstValue)) {
		theta <- 2 * theta
		stageResults <- getStageResultsMeansMultiArmed(design = design, dataInput = dataInput, 
			stage = stage, thetaH0 = theta, directionUpper = directionUpper, 
			intersectionTest = intersectionTest, normalApproximation = normalApproximation, 
			varianceOption = varianceOption, calculateSingleStepAdjusted = TRUE,
			multivariateDistributionFunction = multivariateDistributionFunction)
		
		firstValue <- stageResults[[firstParameterName]][treatmentArm, stage]
		maxSearchIterations <- maxSearchIterations - 1
		if (maxSearchIterations < 0) {
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
				sprintf(paste0("Failed to find theta (k = %s, firstValue = %s, ", 
					"secondValue = %s, levels(firstValue) = %s, theta = %s)"), 
					stage, stageResults[[firstParameterName]][treatmentArm, stage], secondValue, 
					firstValue, theta))
		}
	}
	
	return(theta)
}

.getRepeatedConfidenceIntervalsMeansMultiArmed <- function(..., 
		design, dataInput,  
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		varianceOption = C_VARIANCES_OPTION_DEFAULT, 
		intersectionTest = C_INTERSECTIONTEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT, 
		firstParameterName, multivariateDistributionFunction = .getMultivariateDistribution) {
	
	kMax <- design$kMax
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidStage(stage, kMax)
	
	# necessary for adjustment for binding futility boundaries
	futilityCorr <- rep(NA_real_, kMax) 
	gMax <- dataInput$getNumberOfGroups() - 1
	repeatedConfidenceIntervals <- array(NA_real_, dim = c(gMax, 2, kMax))	
	
	if (intersectionTest == "Hierarchical") {
		warning("Repeated confidence intervals not available for ", 
			"'intersectionTest' = \"Hierarchical\"", call. = FALSE)
		return(repeatedConfidenceIntervals)
	} 
	
	stageResults <- getStageResultsMeansMultiArmed(design = design, dataInput = dataInput, 
		stage = stage, thetaH0 = 0, directionUpper = directionUpper, 
		intersectionTest = intersectionTest, normalApproximation = normalApproximation, 
		varianceOption = varianceOption, calculateSingleStepAdjusted = FALSE,
		multivariateDistributionFunction = multivariateDistributionFunction)
	
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
	startTime <- Sys.time()
	
	stages <- (1:stage)
	for (k in stages) {
		for (g in 1:gMax) {
			if (!is.na(stageResults$testStatistics[g, k])) {
				
				# finding maximum upper and minimum lower bounds for RCIs 	
				thetaLow <- .getUpperLowerThetaMeansMultiArmed(design = design, dataInput = dataInput, 
					theta = -1, treatmentArm = g, stage = k, directionUpper = TRUE, 
					normalApproximation, varianceOption, conditionFunction = conditionFunction, 
					intersectionTest = intersectionTest, firstParameterName = firstParameterName, 
					secondValue = criticalValues[k], 
					multivariateDistributionFunction = multivariateDistributionFunction) 
				
				thetaUp <- .getUpperLowerThetaMeansMultiArmed(design = design, dataInput = dataInput, 
					theta = 1, treatmentArm = g, stage = k, directionUpper = FALSE, 
					normalApproximation, varianceOption, conditionFunction = conditionFunction, 
					intersectionTest = intersectionTest, firstParameterName = firstParameterName, 
					secondValue = criticalValues[k], 
					multivariateDistributionFunction = multivariateDistributionFunction)
				
				# finding upper and lower RCI limits through root function
				repeatedConfidenceIntervals[g, 1, k] <- .getRootThetaMeansMultiArmed(design = design, 
					dataInput = dataInput, treatmentArm = g, stage = k, directionUpper = TRUE, 
					normalApproximation, varianceOption, thetaLow, thetaUp, 
					intersectionTest = intersectionTest, firstParameterName = firstParameterName, 
					secondValue = criticalValues[k], tolerance = tolerance,
					multivariateDistributionFunction = multivariateDistributionFunction) 
				
				repeatedConfidenceIntervals[g, 2, k] <- .getRootThetaMeansMultiArmed(design = design, 
					dataInput = dataInput, treatmentArm = g, stage = k, directionUpper = FALSE, 
					normalApproximation, varianceOption, thetaLow, thetaUp, 
					intersectionTest = intersectionTest, firstParameterName = firstParameterName, 
					secondValue = criticalValues[k], tolerance = tolerance,
					multivariateDistributionFunction = multivariateDistributionFunction) 
				
				# adjustment for binding futility bounds	
				if (k > 1 && conditionFunction(bounds[k - 1], border) & design$bindingFutility) {
					
					parameterName <- ifelse(.isTrialDesignFisher(design), 
						"singleStepAdjustedPValues", firstParameterName)
					
					#  Calculate new lower and upper bounds
					if (directionUpper) {
						thetaLow <- .getUpperLowerThetaMeansMultiArmed(design = design, 
							dataInput = dataInput, 
							theta = -1, treatmentArm = g, stage = k - 1, directionUpper = TRUE, 
							normalApproximation, varianceOption, conditionFunction = conditionFunction, 
							intersectionTest = intersectionTest, firstParameterName = parameterName, 
							secondValue = bounds[k - 1], 
							multivariateDistributionFunction = multivariateDistributionFunction)
					} else {
						thetaUp <- .getUpperLowerThetaMeansMultiArmed(design = design, 
							dataInput = dataInput, 
							theta = 1, treatmentArm = g, stage = k - 1, directionUpper = FALSE, 
							normalApproximation, varianceOption, conditionFunction = conditionFunction, 
							intersectionTest = intersectionTest, firstParameterName = parameterName, 
							secondValue = bounds[k - 1], 
							multivariateDistributionFunction = multivariateDistributionFunction)
					}
					
					futilityCorr[k] <- .getRootThetaMeansMultiArmed(design = design, dataInput = dataInput, 
						treatmentArm = g, stage = k - 1, directionUpper = directionUpper, 
						normalApproximation, varianceOption, thetaLow, thetaUp, 
						intersectionTest = intersectionTest, firstParameterName = parameterName, 
						secondValue = bounds[k - 1], tolerance = tolerance,
						multivariateDistributionFunction = multivariateDistributionFunction)
					
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
	
	return(list(	  
		normalApproximation = normalApproximation, 
		varianceOption = varianceOption, 
		intersectionTest = intersectionTest,
		repeatedConfidenceIntervals = repeatedConfidenceIntervals)
	)
}

# 
# RCIs based on inverse normal combination test	
#
.getRepeatedConfidenceIntervalsMeansMultiArmedInverseNormal <- function(..., 
		design, dataInput, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		varianceOption = C_VARIANCES_OPTION_DEFAULT,  
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		intersectionTest = C_INTERSECTIONTEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	.warnInCaseOfUnknownArguments(functionName = 
			".getRepeatedConfidenceIntervalsMeansMultiArmedInverseNormal", ignore = c("stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsMeansMultiArmed(design = design, dataInput = dataInput, 
		normalApproximation = normalApproximation, varianceOption = varianceOption, 
		directionUpper = directionUpper, intersectionTest = intersectionTest,
		tolerance = tolerance, firstParameterName = "combInverseNormal", 
		multivariateDistributionFunction = multivariateDistributionFunction, ...))
}

# 
# RCIs based on Fisher's combination test	
#
.getRepeatedConfidenceIntervalsMeansMultiArmedFisher <- function(..., 
		design, dataInput,     
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		varianceOption = C_VARIANCES_OPTION_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		intersectionTest = C_INTERSECTIONTEST_MULTIARMED_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_DEFAULT,
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	.warnInCaseOfUnknownArguments(functionName = 
			".getRepeatedConfidenceIntervalsMeansMultiArmedFisher", ignore = c("stage"), ...)
	
	return(.getRepeatedConfidenceIntervalsMeansMultiArmed(design = design, dataInput = dataInput, 
		normalApproximation = normalApproximation, varianceOption = varianceOption, 
		directionUpper = directionUpper, intersectionTest = intersectionTest,
		tolerance = tolerance, firstParameterName = "combFisher", 
		multivariateDistributionFunction = multivariateDistributionFunction, ...))
}

getConditionalPowerMeansMultiArmed <- function(..., design, stageResults, stage = NA_integer_, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		thetaH1 = NA_real_, assumedStDevs = NA_real_,
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_,
		multivariateDistributionFunction = .getMultivariateDistribution) {
		
	.assertIsValidMultivariateDistributionFunctionDefined(multivariateDistributionFunction)
	
	if (.isTrialDesignConditionalDunnett(design)) {
		kMax <- 2
	} else {
		kMax <- design$kMax
	}	
	if (any(is.na(nPlanned))) {			
		return(list(conditionalPower = rep(NA_real_, kMax), simulated = FALSE))
	}
	
	if (is.na(stage)) {
		stage <- stageResults$stage	
	}
	.assertIsValidStage(stage, kMax)
	if (stage == kMax) {
		.logDebug("Conditional power will be calculated only for subsequent stages ", 
			"(stage = ", stage, ", kMax = ", kMax, ")")
		return(list(conditionalPower = rep(NA_real_, kMax), simulated = FALSE))
	}
	
	if (!.isValidNPlanned(nPlanned = nPlanned, kMax = kMax, stage = stage)) {
		return(list(conditionalPower = rep(NA_real_, kMax), simulated = FALSE))
	}
	
	.assertIsValidNPlanned(nPlanned, kMax, stage)
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")	
	assumedStDevs <- .assertIsValidAssumedStDevForMultiArm(assumedStDevs, stageResults, stage)
	thetaH1 <- .assertIsValidThetaH1ForMultiArm(thetaH1, stageResults, stage)
	gMax <- nrow(stageResults$testStatistics)
	
	if (length(thetaH1) == 1) {
		thetaH1 <- rep(thetaH1, gMax)
	}	
	if (length(assumedStDevs) == 1) {
		assumedStDevs <- rep(assumedStDevs, gMax)
	}	
	if ((length(thetaH1) != gMax) || (length(assumedStDevs) != gMax)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			sprintf(paste0("Length of thetaH1 and/or assumedStDevs is invalid: ", 
				"length must be equal to 'gMax' (%s) or 1"), gMax))
	}
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getConditionalPowerMeansMultiArmedInverseNormal(
			design = design, stageResults = stageResults, stage = stage, 
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			thetaH1 = thetaH1, 
			assumedStDevs = assumedStDevs,
			multivariateDistributionFunction = multivariateDistributionFunction, ...))
	}
	else if (.isTrialDesignFisher(design)) {
		return(.getConditionalPowerMeansMultiArmedFisher(
			design = design, stageResults = stageResults,  stage = stage,
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			thetaH1 = thetaH1, 
			assumedStDevs = assumedStDevs, 
			iterations = iterations, seed = seed,
			multivariateDistributionFunction = multivariateDistributionFunction, ...))
	}
	else if (.isTrialDesignConditionalDunnett(design)) {
		return(.getConditionalPowerMeansMultiArmedConditionalDunnett(
			design = design, stageResults = stageResults, stage = stage, 
			nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
			thetaH1 = thetaH1, 
			assumedStDevs = assumedStDevs, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
		"'design' must be an instance of TrialDesignInverseNormal, TrialDesignFisher, ",
		"or TrialDesignConditionalDunnett")
}

#
# Calculation of conditional power based on inverse normal method
#
.getConditionalPowerMeansMultiArmedInverseNormal <- function(..., design, stageResults, stage,
		allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs, 
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	.assertIsTrialDesignInverseNormal(design)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerMeansMultiArmedInverseNormal", 
		ignore = c("stage"), ...)
	
	kMax <- design$kMax	
	gMax <- nrow(stageResults$testStatistics)	
	conditionalPower <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	weights <- .getWeightsInverseNormal(design)
	informationRates <- design$informationRates
	nPlanned <- c(rep(NA_real_, stage), nPlanned)	
	nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	
	if (stageResults$directionUpper) {
		standardizedEffect <- (thetaH1 - stageResults$thetaH0) / assumedStDevs
	} else {
		standardizedEffect <- -(thetaH1 - stageResults$thetaH0) / assumedStDevs
	}
	intersectionTest <- stageResults$intersectionTest
	ctr <- .performClosedCombinationTest(design, stageResults, intersectionTest = intersectionTest,
		multivariateDistributionFunction = multivariateDistributionFunction)
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
			
			conditionalPower[g, (stage + 1):kMax] <- cumsum(probs[3, ] - probs[2, ])
		}	
	}	
	nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	
	return(list(
		thetaH1 = thetaH1,
		assumedStDevs = assumedStDevs, 
		nPlanned = nPlanned,
		conditionalPower = conditionalPower
	))	
}

#
# Calculation of conditional power based on Fisher's combination test
#
.getConditionalPowerMeansMultiArmedFisher <- function(..., design, stageResults, stage,  
		allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs,
		iterations, seed, multivariateDistributionFunction = .getMultivariateDistribution) {
	
	.assertIsTrialDesignFisher(design)
	.assertIsValidIterationsAndSeed(iterations, seed, zeroIterationsAllowed = FALSE)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerMeansMultiArmedFisher", 
		ignore = c("stage"), ...)
	kMax <- design$kMax	
	gMax <- nrow(stageResults$testStatistics)
	criticalValues <- design$criticalValues
	weightsFisher <- .getWeightsFisher(design) 
	intersectionTest <- stageResults$intersectionTest
	
	conditionalPower <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	seed <- .setSeed(seed)
	simulated <- FALSE
	if (stageResults$directionUpper) {
		standardizedEffect <- (thetaH1 - stageResults$thetaH0) / assumedStDevs
	} else {
		standardizedEffect <- -(thetaH1 - stageResults$thetaH0) / assumedStDevs
	}
	nPlanned <- c(rep(NA_real_, stage), nPlanned)
	nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	ctr <- .performClosedCombinationTest(design, stageResults, intersectionTest = intersectionTest,
		multivariateDistributionFunction = multivariateDistributionFunction)
	
	for (g in 1:gMax) {
		if (!is.na(ctr$separatePValues[g, stage])) {
			pValues <- ctr$adjustedStageWisePValues[ctr$indices[,g] == 1,][which.max(
					ctr$overallAdjustedTestStatistics[ctr$indices[,g] == 1,stage]), 1:stage]
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
					conditionalPower[g, k] <- reject / iterations
				}
				simulated <- TRUE
			}
			else if (stage == kMax - 1) {
				divisor <- prod(pValues[1:(kMax - 1)]^weightsFisher[1:(kMax - 1)])
				result <- 1 - (criticalValues[kMax] / divisor)^(1/weightsFisher[kMax])
				
				if (result <= 0 || result >= 1) {
					warning("Could not calculate conditional power for stage ", kMax, call. = FALSE)
					conditionalPower[g, kMax] <- NA_real_
				} else {
					conditionalPower[g, kMax] <- 1 - stats::pnorm(stats::qnorm(result) - 
							standardizedEffect[g] * sqrt(nPlanned[kMax]))				
				}
			}
		}
	}	
	nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	
	return(list(
		thetaH1 = thetaH1,
		assumedStDevs = assumedStDevs, 
		nPlanned = nPlanned, 
		conditionalPower = conditionalPower, 
		iterations = iterations, 
		seed = seed,
		simulated = simulated
	))	
}

#
# Calculation of conditional power based on conditional Dunnett test
#
.getConditionalPowerMeansMultiArmedConditionalDunnett <- function(..., design, stageResults, stage,  
		allocationRatioPlanned, nPlanned, thetaH1, assumedStDevs) {
	
	.assertIsTrialDesignConditionalDunnett(design)
	.warnInCaseOfUnknownArguments(functionName = ".getConditionalPowerMeansMultiArmedConditionalDunnett", 
		ignore = c("stage", "intersectionTest"), ...)
	
	kMax <- 2	
	gMax <- nrow(stageResults$testStatistics)	
	conditionalPower <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	nPlanned <- c(rep(NA_real_, stage), nPlanned)	
	nPlanned <- allocationRatioPlanned / (1 + allocationRatioPlanned)^2 * nPlanned
	
	if (stageResults$directionUpper) {
		standardizedEffect <- (thetaH1 - stageResults$thetaH0) / assumedStDevs
	} else {
		standardizedEffect <- -(thetaH1 - stageResults$thetaH0) / assumedStDevs
	}
	ctr <- getConditionalDunnettTest(design, stageResults, stage = stage)
	
	for (g in 1:gMax) {
		if (!is.na(ctr$separatePValues[g, stage])) {
			conditionalPower[g, 2] <- 1 - 
				stats::pnorm(stats::qnorm(1 - min(ctr$conditionalErrorRate[ctr$indices[,g] == 1,
				stage], na.rm = TRUE)) - standardizedEffect[g] * sqrt(nPlanned[2]))
		}	
	}	
	nPlanned <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned * nPlanned
	
	return(list(
		thetaH1 = thetaH1,
		assumedStDevs = assumedStDevs, 
		nPlanned = nPlanned,
		conditionalPower = conditionalPower
	))	
}

.getConditionalPowerPlotMeansMultiArmed <- function(..., stageResults, stage, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		thetaRange, assumedStDevs = NA_real_,
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_,
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	if (!.associatedArgumentsAreDefined(nPlanned = nPlanned, thetaRange = thetaRange)) {
		warning("A planned sample size (nPlanned) and ", 
			"a range of effect sizes (thetaRange) must be specified", call. = FALSE)
	}
	
	design <- stageResults$.design
	
	if (.isTrialDesignConditionalDunnett(design)) {
		kMax <- 2
	} else {
		kMax <- design$kMax
	}	
	
	intersectionTest <- stageResults$intersectionTest
	
	gMax <- nrow(stageResults$testStatistics)	
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned")	
	assumedStDevs <- .assertIsValidAssumedStDevForMultiArm(assumedStDevs, stageResults, stage)
	
	if (length(assumedStDevs) == 1) {
		assumedStDevs <- rep(assumedStDevs, gMax)
	}	
	thetaRange <- .assertIsValidThetaH1ForMultiArm(thetaH1 = thetaRange)
	condPowerValues <- matrix(rep(NA_real_, gMax * length(thetaRange)), gMax, length(thetaRange))
	likelihoodValues <- matrix(rep(NA_real_, gMax * length(thetaRange)), gMax, length(thetaRange))
	stdErr <- stageResults$overallStDevs[, stage] * 
		sqrt(1 / stageResults$.dataInput$getOverallSampleSizes(stage = stage, group = gMax + 1) + 
		1 / stageResults$.dataInput$getOverallSampleSizes(stage = stage, group = (1:gMax)))
	
	for (i in seq(along = thetaRange)) {
		
		if (.isTrialDesignInverseNormal(design)) {
			condPowerValues[, i] <- .getConditionalPowerMeansMultiArmedInverseNormal(
				design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
				allocationRatioPlanned = allocationRatioPlanned, 
				thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs,
				multivariateDistributionFunction = multivariateDistributionFunction)$conditionalPower[, kMax]
		}
		else if (.isTrialDesignFisher(design)) {
			condPowerValues[, i] <- .getConditionalPowerMeansMultiArmedFisher(
				design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
				allocationRatioPlanned = allocationRatioPlanned, 
				thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs, 
				iterations = iterations, seed = seed,
				multivariateDistributionFunction = multivariateDistributionFunction)$conditionalPower[, kMax]
		}
		else if (.isTrialDesignConditionalDunnett(design)) {
			condPowerValues[, i] <- .getConditionalPowerMeansMultiArmedConditionalDunnett(
				design = design, stageResults = stageResults, stage = stage, nPlanned = nPlanned, 
				allocationRatioPlanned = allocationRatioPlanned, 
				thetaH1 = thetaRange[i], assumedStDevs = assumedStDevs,
				multivariateDistributionFunction = multivariateDistributionFunction)$conditionalPower[, 2]
		}
		
		likelihoodValues[, i] <- stats::dnorm(thetaRange[i], stageResults$effectSizes[,stage], stdErr) / 
			stats::dnorm(0, 0, stdErr)
	}	
	
	assumedStDevsPrint <- paste0("(", .arrayToString(sprintf("%.1f", assumedStDevs), encapsulate = FALSE), ")")
	
	subTitle <- paste0("Intersection test = ", intersectionTest, 
		", Stage = ", stage, ", # of remaining subjects = ", 
		sum(nPlanned), ", std = ", assumedStDevsPrint, ", allocation ratio = ", allocationRatioPlanned)
	
	return(list(
		xValues = thetaRange,
		condPowerValues = condPowerValues,
		likelihoodValues = likelihoodValues,
		main = "Conditional Power Plot with Likelihood",
		xlab = "Effect size",
		ylab = "Conditional power / Likelihood",
		sub = subTitle
	))
}	

getPlotMeansMultiArmed <- function(..., stageResults, stage, 
		nPlanned, allocationRatioPlanned = C_ALLOCATION_RATIO_DEFAULT, 
		thetaRange, assumedStDevs = NA_real_,
		iterations = C_ITERATIONS_DEFAULT, seed = NA_real_, showArms = NA_real_,
		multivariateDistributionFunction = .getMultivariateDistribution) {
	
	z <- .getConditionalPowerPlotMeansMultiArmed(
		stageResults = stageResults, stage = stage, 
		nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, 
		thetaRange = thetaRange, assumedStDevs = assumedStDevs, iterations = iterations, seed = seed,
		multivariateDistributionFunction = multivariateDistributionFunction)
	
	if (any(is.na(showArms))) {
		showArms <- (1:nrow(stageResults$testStatistics))
	} 	
	graphics::plot(x = z$xValues,y = z$condPowerValues[showArms[1],], type = "l", 
		xlab = z$xlab, main = z$main, sub = z$sub, 
		ylab = z$ylab, ylim = c(0,1), lwd = 2, col = "Black")
	
	for (g in showArms) {
		graphics::lines(z$xValues,z$condPowerValues[g,], lwd = 2, lty = 1, col = g)
		graphics::lines(z$xValues,z$likelihoodValues[g,], lwd = 2, lty = 2, col = g)
	}
}	

#
# Conditional Dunnett test
#
getConditionalDunnettTest <- function(design = getDesignConditionalDunnett(), ...,
		stageResults, stage = NA_integer_, multivariateDistributionFunction = .getMultivariateDistribution) {
	
	.assertIsValidMultivariateDistributionFunctionDefined(multivariateDistributionFunction)
		
	kMax <- 2
	gMax <- nrow(stageResults$testStatistics)
	if (is.na(stage)) {
		stage <- stageResults$stage	
	}
	.assertIsValidStage(stage, kMax)
	
	informationAtInterim <- design$informationAtInterim
	secondStageConditioning <- design$secondStageConditioning
	alpha <- design$alpha
	if (length(informationAtInterim) == 1) {
		informationAtInterim <- rep(informationAtInterim, gMax)
	}
	frac1 <- stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 & 
		stageResults$.dataInput$groups <= gMax] /
		(stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 & 
		stageResults$.dataInput$groups <= gMax] +
		stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 1 & 
					stageResults$.dataInput$groups == (gMax + 1)])
	if (stage == 2) {
		frac2 <- stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 & 
			stageResults$.dataInput$groups <= gMax] /
			(stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 & 
			stageResults$.dataInput$groups <= gMax] +
			stageResults$.dataInput$sampleSizes[stageResults$.dataInput$stages == 2 & 
			stageResults$.dataInput$groups == (gMax + 1)])
	}	
	indices <- .getIndicesOfClosedHypothesesSystem(gMax = gMax)
	
	conditionalErrorRate <- matrix(rep(NA_real_, 2 * (2^gMax - 1)), 2^gMax - 1, 2)
	secondStagePValues <- matrix(rep(NA_real_, 2 * (2^gMax - 1)), 2^gMax - 1, 2)
	rejected <- matrix(rep(FALSE, gMax * 2), gMax, 2)
	
	colnames(conditionalErrorRate) = paste("stage ", (1:2), sep = "")
	colnames(secondStagePValues) = paste("stage ", (1:2), sep = "")
	dimnames(rejected) = list(paste("arm ", 1:gMax, sep = ""), paste("stage ", (1:2), sep = ""))
	rejectedIntersections <- matrix(rep(FALSE, stage * nrow(indices)), nrow(indices), stage)
	
	for (i in 1:(2^gMax - 1)) {
		zeta <- sqrt(frac1[indices[i,] == 1])
		sigma <- zeta %*% t(zeta)
		diag(sigma) <- 1
		crit <- multivariateDistributionFunction(type = "quantile", 
			upper = NA_real_, sigma = sigma, alpha = alpha)
		
		integrand <- function(x) {
			innerProduct <- 1
			for (g in (1:gMax)) {
				if (indices[i,g] == 1) {
					innerProduct <- innerProduct * stats::pnorm(((crit - 
						sqrt(informationAtInterim[g]) * stageResults$testStatistics[g, 1] + 
						sqrt(1 - informationAtInterim[g]) * sqrt(frac1[g]) * x)) / 
						sqrt((1 - informationAtInterim[g]) * (1 - frac1[g])))
				}
			}				
			return(innerProduct * dnorm(x))
		}
		conditionalErrorRate[i, 1] <- 1 - integrate(integrand, lower = -Inf, upper = Inf)$value			
		if (stage == 2) {
			if (!all(is.na(stageResults$separatePValues[indices[i, ] == 1, 2]))) {
				
				if (secondStageConditioning) {
					maxOverallTestStatistic <- max(
						stageResults$overallTestStatistics[indices[i, ] == 1, 2], na.rm = TRUE)
					integrand <- function(x) {
						innerProduct <- 1
						for (g in (1:gMax)) {
							if ((indices[i,g] == 1) && !is.na(stageResults$overallTestStatistics[g, 2])) {
								innerProduct <- innerProduct * 
									stats::pnorm(((maxOverallTestStatistic - 
									sqrt(informationAtInterim[g]) * 
									stageResults$testStatistics[g,1] + 
									sqrt(1 - informationAtInterim[g]) * sqrt(frac2[g]) * x)) / 
									sqrt((1 - informationAtInterim[g]) * (1 - frac2[g])))
							}
						}				
						return(innerProduct * dnorm(x))
					}
					secondStagePValues[i, 2] <- 1 - integrate(integrand, lower = -Inf, upper = Inf)$value
					
				} else {	
					maxTestStatistic <- max(stageResults$testStatistics[indices[i, ] == 1, 2], na.rm = TRUE)				
					integrand <- function(x) {
						innerProduct <- 1
						for (g in (1:gMax)) {
							if ((indices[i,g] == 1) && !is.na(stageResults$separatePValues[g, 2])) {
								innerProduct <- innerProduct * 
								stats::pnorm(((maxTestStatistic + sqrt(frac2[g])*x)) / sqrt(1 - frac2[g]))
							}
						}				
						return(innerProduct * dnorm(x))
					}
					secondStagePValues[i, 2] <- 1 - integrate(integrand, lower = -Inf, upper = Inf)$value
				}	
			}
		}
	}
	
	if (stage == 2) {
		rejectedIntersections[, 2] <- (secondStagePValues[,2] <= conditionalErrorRate[,1])
		for (j in 1:gMax) {
			rejected[j, 2] <- all(rejectedIntersections[indices[, j] == 1, 2])
		}
	}
	
	return(list(
		design = design,		
		informationAtInterim = informationAtInterim, 
		indices = indices,
		secondStageConditioning = secondStageConditioning,
		separatePValues	= stageResults$separatePValues,
		conditionalErrorRate = conditionalErrorRate,			
		secondStagePValues	= secondStagePValues,
		rejected = rejected,
		rejectedIntersections = rejectedIntersections
	))
}	


