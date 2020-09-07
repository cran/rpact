
#:#
#:#  *Simulation of multiarm design with time to event data*
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
#:#  File version: $Revision: 3594 $
#:#  Last changed: $Date: 2020-09-04 14:53:13 +0200 (Fr, 04 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

#' @include f_simulation_multiarm.R
NULL

.getSimulationSurvivalMultiArmStageEvents <- function(..., 
		stage,  
		directionUpper,
		conditionalPower, 
		conditionalCriticalValue, 
		plannedEvents, 
		allocationRatioPlanned,
		selectedArms,
		thetaH1,
		overallEffects, 
		minNumberOfEventsPerStage, 
		maxNumberOfEventsPerStage) {
	
	gMax <- nrow(overallEffects)
	if (!is.na(conditionalPower)) {
		if (any(selectedArms[1:gMax, stage + 1], na.rm = TRUE)) {
			
			if (is.na(thetaH1)) {
				if (directionUpper) {
					thetaStandardized <- log(max(min(overallEffects[selectedArms[1:gMax, stage + 1], stage], na.rm = TRUE), 1 + 1E-7))
				} else {
					thetaStandardized <- log(min(max(overallEffects[selectedArms[1:gMax, stage + 1], stage], na.rm = TRUE), 1 - 1E-7))
				}	
			} else {
				thetaStandardized <- log(thetaH1)
			}
			
			if (conditionalCriticalValue[stage] > 8) {
				newEvents <- maxNumberOfEventsPerStage[stage + 1]
			} else {	
				newEvents <- (1 + allocationRatioPlanned)^2 / allocationRatioPlanned *
						(max(0, conditionalCriticalValue[stage] + stats::qnorm(conditionalPower), na.rm = TRUE))^2 / thetaStandardized^2
				newEvents <- min(max(minNumberOfEventsPerStage[stage + 1], newEvents), maxNumberOfEventsPerStage[stage + 1])
			}	
		} else {
			newEvents <- 0
		}
	} else {
		newEvents <- plannedEvents[stage + 1] - plannedEvents[stage]
	}
	return(newEvents)
}

.getSimulatedStageSurvivalMultiArm <- function(..., 
		design, 
		directionUpper, 
		omegaVector, 
		plannedEvents, 
		typeOfSelection, 
		effectMeasure, 
		adaptations, 
		epsilonValue, 
		rValue, 
		threshold, 
		allocationRatioPlanned, 
		minNumberOfEventsPerStage, 
		maxNumberOfEventsPerStage, 
		conditionalPower, 
		thetaH1, 
		calcEventsFunction, 
		selectArmsFunction) {
	
	kMax <- length(plannedEvents)	
	gMax <- length(omegaVector)	
	simSurvival <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	overallEffects <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	eventsPerStage <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	testStatistics <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	overallTestStatistics <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	separatePValues <- matrix(rep(NA_real_, gMax * kMax), gMax, kMax)
	conditionalCriticalValue <- rep(NA_real_, kMax - 1) 
	conditionalPowerPerStage <- rep(NA_real_, kMax)
	selectedArms <- matrix(rep(FALSE, gMax* kMax), gMax, kMax)
	selectedArms[, 1] <- TRUE
	adjustedPValues <- rep(NA_real_, kMax)
	
	if (.isTrialDesignFisher(design)) {
		weights  <- .getWeightsFisher(design)
	} else if (.isTrialDesignInverseNormal(design)) {
		weights <- .getWeightsInverseNormal(design)
	}	
	
	for (k in (1:kMax)) {
		 
		for (treatmentArm in (1:gMax)) {
			if (selectedArms[treatmentArm, k]) {
				
				if (k == 1) {
					eventsPerStage[treatmentArm, k] <- plannedEvents[k]
				} else {
					eventsPerStage[treatmentArm, k] <- plannedEvents[k] - plannedEvents[k - 1]
				}	
				
				if (eventsPerStage[treatmentArm, k] > 0) {
					testStatistics[treatmentArm, k] <- (2*directionUpper - 1)*rnorm(1, log(omegaVector[treatmentArm]) * sqrt(eventsPerStage[treatmentArm, k]) * 
								sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned), 1)
				}

				overallTestStatistics[treatmentArm, k] <- sqrt(eventsPerStage[treatmentArm, 1:k]) %*% testStatistics[treatmentArm, 1:k] / 
								sqrt(sum(eventsPerStage[treatmentArm, 1:k]))
			
				overallEffects[treatmentArm, k] <- exp((2*directionUpper - 1)*overallTestStatistics[treatmentArm, k] * 
								(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned) /
								sqrt(sum(eventsPerStage[treatmentArm, 1:k])))
						
				separatePValues[treatmentArm, k] <- 1 - stats::pnorm(testStatistics[treatmentArm, k])
				
			}	
		}
		if (k < kMax) {
			if (colSums(selectedArms)[k] == 0) {
				break
			}
				
			# Bonferroni adjustment
			adjustedPValues[k] <- min(min(separatePValues[, k], na.rm = TRUE)*(colSums(selectedArms)[k]), 1 - 1e-7)   # Bonferroni adjustment

			# conditional critical value to reject the null hypotheses at the next stage of the trial
			if (.isTrialDesignConditionalDunnett(design)) {
				conditionalCriticalValue[k] <- (stats::qnorm(1 - design$alpha) - stats::qnorm(1 - adjustedPValues[k]) * sqrt(design$informationAtInterim)) /
						sqrt(1 - design$informationAtInterim)
			} else {
				if (.isTrialDesignFisher(design)) {
					conditionalCriticalValue[k] <- stats::qnorm(1 - min((design$criticalValues[k + 1] / 
												prod(adjustedPValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 1 - 1e-7))
				} else {						
					conditionalCriticalValue[k] <- (design$criticalValues[k + 1] * sqrt(design$informationRates[k + 1]) - 
							stats::qnorm(1 - adjustedPValues[1:k])%*%weights[1:k]) / 
							sqrt(design$informationRates[k + 1] - design$informationRates[k])
				}
			}
			
			if (adaptations[k]) {
				
				if (effectMeasure == "testStatistic") {
					selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(overallTestStatistics[, k], 
										typeOfSelection, epsilonValue, rValue, threshold, selectArmsFunction, survival = TRUE))				
				} else if (effectMeasure == "effectDifference") {
					selectedArms[, k + 1] <- (selectedArms[, k] & .selectTreatmentArms(overallEffects[, k], 
										typeOfSelection, epsilonValue, rValue, threshold, selectArmsFunction, survival = TRUE))
				}
				
				newEvents <- calcEventsFunction(
					stage = k, 
					directionUpper = directionUpper,
					conditionalPower = conditionalPower, 
					conditionalCriticalValue = conditionalCriticalValue, 
					plannedEvents = plannedEvents, 
					allocationRatioPlanned = allocationRatioPlanned,
					selectedArms = selectedArms,
					thetaH1 = thetaH1,
					overallEffects = overallEffects, 
					minNumberOfEventsPerStage = minNumberOfEventsPerStage, 
					maxNumberOfEventsPerStage = maxNumberOfEventsPerStage)
				
				if (is.null(newEvents) || length(newEvents) != 1 || !is.numeric(newEvents) || is.na(newEvents)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"'calcEventsFunction' returned an illegal or undefined result (", newEvents,"); ", 
							"the output must be a single numeric value")
				}
				eventsToStage <- eventsPerStage[1, 1]
				if (k > 1) {
					for (i in (2:k)) {
						eventsToStage <- eventsToStage + min(eventsPerStage[, i], na.rm = TRUE) 
					}
				}	
				if (!is.na(conditionalPower) || !is.null(calcEventsFunction)) {
					plannedEvents[(k + 1):kMax] <- eventsToStage + cumsum(rep(newEvents, kMax - k))
				}	
						
			} else {
				selectedArms[, k + 1] <- selectedArms[, k]
			}
			
			if (is.na(thetaH1)) {
				if (directionUpper) {
					thetaStandardized <- log(max(min(overallEffects[selectedArms[1:gMax, k], k], na.rm = TRUE), 1 + 1E-7))
				} else {
					thetaStandardized <- log(min(max(overallEffects[selectedArms[1:gMax, k], k], na.rm = TRUE), 1 - 1E-7))
				}	
			} else {
				thetaStandardized <- log(thetaH1)
			}
			thetaStandardized <- (2*directionUpper - 1)*thetaStandardized
			
			conditionalPowerPerStage[k] <- 1 - stats::pnorm(conditionalCriticalValue[k] - 
							thetaStandardized * sqrt(plannedEvents[k + 1] - plannedEvents[k]) * 
							sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned))
		}	
	}
	
	return(list(eventsPerStage = eventsPerStage, 
					allocationRatioPlanned = allocationRatioPlanned, 
					overallEffects = overallEffects, 
					testStatistics = testStatistics, 
					overallTestStatistics = overallTestStatistics, 
					separatePValues = separatePValues, 
					conditionalCriticalValue = conditionalCriticalValue, 
					conditionalPowerPerStage = conditionalPowerPerStage,
					selectedArms = selectedArms
			))
}

#' 
#' @title 
#' Get Simulation Multi-Arm Survival
#' 
#' @description 
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size 
#' for testing survival in a multi-arm treatment groups testing situation. In contrast to \code{getSimulationSurvival()} (where survival times are simulated), normally
#' distributed logrank test statistics are simulated. 
#'
#' @param omegaMaxVector Range of hazard ratios with highest response for \code{"linear"} and \code{"sigmoidEmax"} model, default is \code{seq(1, 2.6, 0.4)}.
#' @inheritParams param_intersectionTest 
#' @inheritParams param_typeOfSelection
#' @inheritParams param_effectMeasure 
#' @inheritParams param_adaptations
#' @inheritParams param_threshold
#' @inheritParams param_effectMatrix
#' @inheritParams param_activeArms
#' @inheritParams param_successCriterion
#' @inheritParams param_typeOfShape
#' @inheritParams param_typeOfSelection
#' @inheritParams param_design_with_default 
#' @inheritParams param_directionUpper
#' @inheritParams param_allocationRatioPlanned
#' @inheritParams param_minNumberOfEventsPerStage  
#' @inheritParams param_maxNumberOfEventsPerStage
#' @inheritParams param_conditionalPowerSimulation
#' @inheritParams param_thetaH1 
#' @inheritParams param_plannedEvents 
#' @inheritParams param_maxNumberOfIterations
#' @inheritParams param_calcEventsFunction
#' @inheritParams param_selectArmsFunction
#' @inheritParams param_rValue
#' @inheritParams param_epsilonValue
#' @inheritParams param_gED50
#' @inheritParams param_slope
#' @inheritParams param_seed
#' @inheritParams param_three_dots
#' @inheritParams param_showStatistics
#' 
#' @details 
#' At given design the function simulates the power, stopping probabilities, selection probabilities, and expected 
#' sample size at given number of subjects, parameter configuration, and treatment arm selection rule in the multi-arm situation. 
#' An allocation ratio can be specified referring to the ratio of number of subjects in the active treatment groups as compared to the control group.
#'  
#' The definition of \code{thetaH1} makes only sense if \code{kMax} > 1
#' and if \code{conditionalPower}, \code{minNumberOfEventsPerStage}, and 
#' \code{maxNumberOfEventsPerStage} (or \code{calcEventsFunction}) are defined.
#' 
#' \code{calcEventsFunction}\cr 
#' This function returns the number of events at given conditional power and conditional critical value for specified 
#' testing situation. The function might depend on the variables 
#' \code{stage}, 
#' \code{selectedArms}, 
#' \code{plannedEvents}, 
#' \code{directionUpper}, 
#' \code{allocationRatioPlanned}, 
#' \code{minNumberOfEventsPerStage}, 
#' \code{maxNumberOfEventsPerStage}, 
#' \code{conditionalPower}, 
#' \code{conditionalCriticalValue}, and 
#' \code{overallEffects}. 
#' The function has to contain the three-dots argument '...' (see examples).
#' 
#' @template return_object_simulation_results
#' @template how_to_get_help_for_generics
#' 
#' @template examples_get_simulation_multiarm_survival
#'  
#' @export
#' 
getSimulationMultiArmSurvival <- function(
		design                    = NULL, ..., 
		activeArms                = 3L, # C_ACTIVE_ARMS_DEFAULT 
		effectMatrix              = NULL, 
		typeOfShape               = c("linear", "sigmoidEmax", "userDefined"), # C_TYPE_OF_SHAPE_DEFAULT
		omegaMaxVector            = seq(1, 2.6, 0.4), # C_RANGE_OF_HAZARD_RATIOS_DEFAULT
		gED50                     = NA_real_, 
		slope                     = 1, 
		intersectionTest          = c("Dunnett", "Bonferroni", "Simes", "Sidak", "Hierarchical"), # C_INTERSECTION_TEST_MULTIARMED_DEFAULT 
		directionUpper            = TRUE, # C_DIRECTION_UPPER_DEFAULT
		adaptations               = NA, 
		typeOfSelection           = c("best", "rBest", "epsilon", "all", "userDefined"), # C_TYPE_OF_SELECTION_DEFAULT
		effectMeasure             = c("effectDifference", "testStatistic"), # C_EFFECT_MEASURE_DEFAULT
		successCriterion          = c("all", "atLeastOne"), # C_SUCCESS_CRITERION_DEFAULT
		epsilonValue              = NA_real_, 
		rValue                    = NA_real_, 
		threshold                 = -Inf, 
		plannedEvents             = NA_real_, 
		allocationRatioPlanned    = NA_real_, 
		minNumberOfEventsPerStage = NA_real_, 
		maxNumberOfEventsPerStage = NA_real_, 
		conditionalPower          = NA_real_, 
		thetaH1                   = NA_real_, 
		maxNumberOfIterations     = 1000L, # C_MAX_SIMULATION_ITERATIONS_DEFAULT
		seed                      = NA_real_,
		calcEventsFunction        = NULL,
		selectArmsFunction        = NULL,
		showStatistics            = TRUE) {
	
	if (is.null(design)) {
		design <- .getDefaultDesign(..., type = "simulation", multiArmEnabled = TRUE)
		.warnInCaseOfUnknownArguments(functionName = "getSimulationMultiArmSurvival", 
			ignore = c(.getDesignArgumentsToIgnoreAtUnknownArgumentCheck(design, multiArmEnabled = TRUE), "showStatistics"), ...)
	} else {
		.assertIsTrialDesignInverseNormalOrFisherOrConditionalDunnett(design)
		.warnInCaseOfUnknownArguments(functionName = "getSimulationMultiArmSurvival", ignore = "showStatistics", ...)
		.warnInCaseOfTwoSidedPowerArgument(...)
	}	
		
	simulationResults <- .createSimulationResultsMultiArmObject(
		design                      = design,  
		activeArms                  = activeArms,
		effectMatrix                = effectMatrix,
		typeOfShape                 = typeOfShape,
		muMaxVector                 = muMaxVector,    # means only
		piMaxVector                 = piMaxVector,    # rates only
		piControl                   = piControl,      # rates only
		omegaMaxVector              = omegaMaxVector, # survival only
		gED50                       = gED50,
		slope                       = slope,
		intersectionTest            = intersectionTest,
		stDev                       = stDev,          # means only
		directionUpper              = directionUpper, # rates + survival only
		adaptations                 = adaptations,
		typeOfSelection             = typeOfSelection,
		effectMeasure               = effectMeasure,
		successCriterion            = successCriterion,
		epsilonValue                = epsilonValue,
		rValue                      = rValue,
		threshold                   = threshold,
		plannedSubjects             = plannedSubjects, # means + rates only
		plannedEvents               = plannedEvents,   # survival only
		allocationRatioPlanned      = allocationRatioPlanned,
		minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage, # means + rates only
		maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage, # means + rates only
		minNumberOfEventsPerStage   = minNumberOfEventsPerStage,   # survival only
		maxNumberOfEventsPerStage   = maxNumberOfEventsPerStage,   # survival only
		conditionalPower            = conditionalPower,
		thetaH1                     = thetaH1,         # means + survival only
		piH1                        = piH1,            # rates only
		piControlH1                 = piControlH1,     # rates only
		maxNumberOfIterations       = maxNumberOfIterations,
		seed                        = seed,
		calcSubjectsFunction        = calcSubjectsFunction, # means + rates only
		calcEventsFunction          = calcEventsFunction,   # survival only
		selectArmsFunction          = selectArmsFunction, 
		showStatistics              = showStatistics,
		endpoint                    = "survival")
		
	design                      <- simulationResults$.design
	successCriterion            <- simulationResults$successCriterion
	effectMeasure               <- simulationResults$effectMeasure
	adaptations                 <- simulationResults$adaptations
	gMax                        <- activeArms
	kMax                        <- simulationResults$.design$kMax
	intersectionTest            <- simulationResults$intersectionTest
	typeOfSelection             <- simulationResults$typeOfSelection
	effectMatrix                <- t(simulationResults$effectMatrix)
	omegaMaxVector              <- simulationResults$omegaMaxVector # survival only
	thetaH1                     <- simulationResults$thetaH1        # means + survival only
	plannedEvents               <- simulationResults$plannedEvents  # survival only
	conditionalPower            <- simulationResults$conditionalPower
	minNumberOfEventsPerStage   <- simulationResults$minNumberOfEventsPerStage # survival only
	maxNumberOfEventsPerStage   <- simulationResults$maxNumberOfEventsPerStage # survival only
	allocationRatioPlanned      <- simulationResults$allocationRatioPlanned
	calcEventsFunction          <- simulationResults$calcEventsFunction
	
	indices <- .getIndicesOfClosedHypothesesSystemForSimulation(gMax = gMax)
	
	if (.isTrialDesignConditionalDunnett(design)) {
		criticalValuesDunnett <- .getCriticalValuesDunnettForSimulation(alpha = design$alpha, indices = indices, 
				allocationRatioPlanned = allocationRatioPlanned)
	}
	
	cols <- length(omegaMaxVector)
	
	simulatedSelections <- array(0, dim = c(kMax, cols, gMax))
	simulatedRejections <- array(0, dim = c(kMax, cols, gMax))
	simulatedNumberOfActiveArms <- matrix(0, cols*kMax, nrow = kMax, ncol = cols)
	simulatedEventsPerStage <- array(0, dim = c(kMax, cols, gMax))
	simulatedSuccessStopping <- matrix(0, cols*kMax, nrow = kMax, ncol = cols)
	simulatedFutilityStopping <- matrix(0, cols*(kMax - 1), nrow = kMax - 1, ncol = cols)
	simulatedConditionalPower <- matrix(0, cols*kMax, nrow = kMax, ncol = cols)
	simulatedRejectAtLeastOne <- rep(0, cols)
	expectedNumberOfEvents <- rep(0, cols)
	iterations <- matrix(0, kMax, cols)
	
	len <- maxNumberOfIterations * kMax * gMax * cols 
	
	dataIterationNumber <- rep(NA_real_, len)
	dataStageNumber <- rep(NA_real_, len)
	dataArmNumber <- rep(NA_real_, len)
	dataAlternative <- rep(NA_real_, len)
	dataEffect <- rep(NA_real_, len)
	dataNumberOfEvents <- rep(NA_real_, len)
	dataRejectPerStage <- rep(NA, len)
	dataFutilityStop <- rep(NA_real_, len)
	dataSuccessStop <- rep(NA, len)
	dataFutilityStop <- rep(NA, len)
	dataTestStatistics <- rep(NA_real_, len)
	dataConditionalCriticalValue <- rep(NA_real_, len)
	dataConditionalPowerAchieved <- rep(NA_real_, len)
	dataEffectEstimate <- rep(NA_real_, len)
	dataPValuesSeparate <- rep(NA_real_, len)

	index <- 1
	
	for (i in (1:cols)) {
		
		for (j in (1:maxNumberOfIterations)) { 
			
			stageResults <- .getSimulatedStageSurvivalMultiArm(
				design                    = design, 
				directionUpper            = directionUpper, 
				omegaVector               = effectMatrix[i, ], 
				plannedEvents             = plannedEvents, 
				typeOfSelection           = typeOfSelection, 
				effectMeasure             = effectMeasure, 
				adaptations               = adaptations, 
				epsilonValue              = epsilonValue, 
				rValue                    = rValue, 
				threshold                 = threshold, 
				allocationRatioPlanned    = allocationRatioPlanned, 
				minNumberOfEventsPerStage = minNumberOfEventsPerStage, 
				maxNumberOfEventsPerStage = maxNumberOfEventsPerStage, 
				conditionalPower          = conditionalPower, 
				thetaH1                   = thetaH1, 
				calcEventsFunction        = calcEventsFunction, 
				selectArmsFunction        = selectArmsFunction)
			
			if (.isTrialDesignConditionalDunnett(design)) {
				closedTest <- .performClosedConditionalDunnettTestForSimulation(stageResults = stageResults, 
					design = design, indices = indices, 
					criticalValuesDunnett = criticalValuesDunnett, successCriterion = successCriterion)
			} else {
				closedTest <- .performClosedCombinationTestForSimulation(stageResults = stageResults, 
					design = design, indices = indices, 
					intersectionTest = intersectionTest, successCriterion = successCriterion)
			}	
			
			rejectAtSomeStage <- FALSE
			rejectedArmsBefore <- rep(FALSE, gMax)
			
			for (k in 1:kMax) {
				
				simulatedRejections[k, i, ] <- simulatedRejections[k, i, ] + (closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)
				simulatedSelections[k, i, ] <- simulatedSelections[k, i, ] + closedTest$selectedArms[, k]
				
				simulatedNumberOfActiveArms[k, i] <- simulatedNumberOfActiveArms[k, i] + sum(closedTest$selectedArms[, k])
				
				if (!any(is.na(closedTest$successStop))) {
					simulatedSuccessStopping[k, i] <- simulatedSuccessStopping[k, i] + closedTest$successStop[k]
				}	 
				
				if ((kMax > 1) && (k < kMax)) {
					if (!any(is.na(closedTest$futilityStop))) {
						simulatedFutilityStopping[k, i] <- simulatedFutilityStopping[k, i] + closedTest$futilityStop[k]
					}
					if (!closedTest$successStop[k] && !closedTest$futilityStop[k]) {
						simulatedConditionalPower[k + 1, i] <- simulatedConditionalPower[k + 1, i] + stageResults$conditionalPowerPerStage[k]
					}
				}	
				
				iterations[k, i] <- iterations[k, i] + 1
				
				for (g in 1:gMax) {
					
					if (!is.na(stageResults$eventsPerStage[g, k])) {
						simulatedEventsPerStage[k, i, g] <- simulatedEventsPerStage[k, i, g] + stageResults$eventsPerStage[g,k]
					}
					dataIterationNumber[index] <- j
					dataStageNumber[index] <- k
					dataArmNumber[index] <- g
					dataAlternative[index] <- omegaMaxVector[i]
					dataEffect[index] <- effectMatrix[i, g]
					dataNumberOfEvents[index] <- round(stageResults$eventsPerStage[g, k], 1)
					dataRejectPerStage[index] <- closedTest$rejected[g, k]
					dataTestStatistics[index] <- stageResults$testStatistics[g, k]
					dataSuccessStop[index] <- closedTest$successStop[k]
					if (k < kMax) {
						dataFutilityStop[index] <- closedTest$futilityStop[k]
						dataConditionalCriticalValue[index] <-  stageResults$conditionalCriticalValue[k]
						dataConditionalPowerAchieved[index + 1] <- stageResults$conditionalPowerPerStage[k]
					}	
					dataEffectEstimate[index] <- stageResults$overallEffects[g, k]
					dataPValuesSeparate[index] <- closedTest$separatePValues[g, k]
					
					index <- index + 1
				}
				
				if (!rejectAtSomeStage && any(closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore)) {
					simulatedRejectAtLeastOne[i] <- simulatedRejectAtLeastOne[i] + 1
					rejectAtSomeStage <- TRUE 
				}
				
				if ((k < kMax) && (closedTest$successStop[k] || closedTest$futilityStop[k])) {
					#  rejected hypotheses remain rejected also in case of early stopping
					simulatedRejections[(k + 1):kMax, i, ] <- simulatedRejections[(k + 1):kMax, i, ] + 
							matrix((closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore), 
									kMax - k, gMax, byrow = TRUE)
					break
				} 
				
				rejectedArmsBefore <- closedTest$rejected[, k] & closedTest$selectedArms[1:gMax, k] | rejectedArmsBefore
				
			}
		}	
		
		
		simulatedEventsPerStage[is.na(simulatedEventsPerStage)] <- 0
		
		simulatedEventsPerStage[, i, ] <- simulatedEventsPerStage[, i, ] / iterations[, i]
		
		if (kMax > 1) {
			
			simulatedRejections[2:kMax, i, ] <- simulatedRejections[2:kMax, i, ] - simulatedRejections[1:(kMax - 1), i, ]
			
			stopping <- cumsum(simulatedSuccessStopping[1:(kMax - 1), i] + simulatedFutilityStopping[,i])/maxNumberOfIterations
			
			expectedNumberOfEvents[i] <- sum(simulatedEventsPerStage[1, i, ] + t(1 - stopping)%*%simulatedEventsPerStage[2:kMax, i, ])
			
		} else {
			
			expectedNumberOfEvents[i] <- sum(simulatedEventsPerStage[1, i, ])
		}
	}
	
	simulatedConditionalPower[1,] <- NA_real_
	if (kMax > 1) {
		simulatedConditionalPower[2:kMax,] <- simulatedConditionalPower[2:kMax, ] / iterations[2:kMax, ]	
	}
	simulationResults$rejectAtLeastOne <- simulatedRejectAtLeastOne / maxNumberOfIterations
	simulationResults$numberOfActiveArms <- simulatedNumberOfActiveArms / iterations
	
	simulationResults$selectedArms <- simulatedSelections / maxNumberOfIterations
	simulationResults$rejectedArmsPerStage <- simulatedRejections / maxNumberOfIterations
	simulationResults$successPerStage <- simulatedSuccessStopping / maxNumberOfIterations
	simulationResults$futilityPerStage <- simulatedFutilityStopping / maxNumberOfIterations
	simulationResults$futilityStop <- base::colSums(simulatedFutilityStopping / maxNumberOfIterations) 
	if (kMax > 1) {
		simulationResults$earlyStop <- colSums(simulationResults$futilityPerStage + simulationResults$successPerStage[1:(kMax - 1), ])
		simulationResults$conditionalPowerAchieved <- simulatedConditionalPower	
	}
	
	simulationResults$eventsPerStage <- .convertStageWiseToOverallValues(simulatedEventsPerStage)
	simulationResults$expectedNumberOfEvents <- expectedNumberOfEvents
	simulationResults$iterations <- iterations
	
	if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
		simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
	}
	
	if (any(simulationResults$rejectedArmsPerStage < 0)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "internal error, simulation not possible due to numerical overflow")
	}

	data <- data.frame(
		iterationNumber = dataIterationNumber,
		stageNumber = dataStageNumber,
		armNumber = dataArmNumber,
		omegaMax = dataAlternative,
		effect = dataEffect,
		numberOfEvents = dataNumberOfEvents,
		effectEstimate = dataEffectEstimate,
		testStatistics = dataTestStatistics,
		pValue = dataPValuesSeparate,
		conditionalCriticalValue = round(dataConditionalCriticalValue,6),
		conditionalPowerAchieved = round(dataConditionalPowerAchieved,6),
		rejectPerStage = dataRejectPerStage,
		successStop = dataSuccessStop,
		futilityPerStage = dataFutilityStop
	)
	
	data <- data[!is.na(data$effectEstimate), ]
	simulationResults$.data <- data		
	
	return(simulationResults)
}
	
