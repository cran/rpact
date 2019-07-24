##################################################################################################
#                                                                                                #
# -- Simulation of binary data with group sequential and combination test --                     #
#			                                                                                     #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. 			 #
#                                                                                    			 # 
# File version: 1.0.0                                                                			 #
# Date: 09-05-2019                                                                   			 #
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


.getTestStatisticsRates <- function(..., designNumber, informationRates, groups, 
		riskRatio, thetaH0, directionUpper, eventsPerStage, sampleSizesPerStage, 
		testStatisticsPerStage) {

	stage <- ncol(sampleSizesPerStage)
	
	if (groups == 1) {
		stagewiseRates <- eventsPerStage[, stage] / sampleSizesPerStage[, stage]
		overallRates <-	sum(eventsPerStage[, 1:stage]) / sum(sampleSizesPerStage[, 1:stage])
	} else {
		if (stage == 1) {
			stagewiseRates <- eventsPerStage / sampleSizesPerStage
			overallRates <-	eventsPerStage / sampleSizesPerStage
		} else {
			stagewiseRates <- eventsPerStage[, stage] / sampleSizesPerStage[, stage]
			overallRates <-	rowSums(eventsPerStage[, 1:stage]) / rowSums(sampleSizesPerStage[, 1:stage])
		}
	}
	
	if (designNumber == 1L) {
		
		n1 <- sum(sampleSizesPerStage[1, ])
		r1 <- sum(eventsPerStage[1, ]) / n1
		if (groups == 1) {
			value <- (r1 - thetaH0) / sqrt(thetaH0 * (1 - thetaH0)) * sqrt(n1)
		} else {
			n2 <- sum(sampleSizesPerStage[2, ])
			r2 <- sum(eventsPerStage[2, ]) / n2
			if (!riskRatio) {
				if (r1 - r2 - thetaH0 == 0) {
					value <- 0
				} else {
					fm <- .getFarringtonManningValuesDiff(r1, r2, thetaH0, n1 / n2)
					value <- (r1 - r2 - thetaH0) / 
						sqrt( fm[1] * (1 -  fm[1]) / n1 + fm[2] * (1 - fm[2]) / n2)
				}	
			} else {
				if (r1 - r2 * thetaH0 == 0) {
					value <- 0
				} else {
					fm <- .getFarringtonManningValuesRatio(r1, r2, thetaH0, n1 / n2)
					value <- (r1 - r2 * thetaH0) / 
						sqrt( fm[1] * (1 -  fm[1]) / n1 + thetaH0^2 * fm[2] * (1 - fm[2]) / n2)
				}	
			}	
		}
		
		value <- (2 * directionUpper - 1) * value
		pValuesSeparate <- NA_real_
		testStatisticsPerStage <- NA_real_

	} else {
		
		if (stage == 1) {
			n1 <- sampleSizesPerStage[1, 1]
			r1 <- eventsPerStage[1, 1] / n1
			if (groups == 1) {
				testStatisticsPerStage <- (2 * directionUpper - 1) * 
					(r1 - thetaH0) / sqrt(thetaH0 * (1 - thetaH0)) * sqrt(n1)
			} else {
				n2 <- sampleSizesPerStage[2, 1]
				r2 <- eventsPerStage[2, 1] / n2
				if (!riskRatio) {
					if (r1 - r2 - thetaH0 == 0) {
						testStatisticsPerStage <- 0
					} else {
						fm <- .getFarringtonManningValuesDiff(r1, r2, thetaH0, n1 / n2)
						testStatisticsPerStage <- (2 * directionUpper - 1) * 
							(r1 - r2 - thetaH0) / sqrt( fm[1] * (1 -  fm[1]) / 
							n1 + fm[2] * (1 - fm[2]) / n2)
					} 
				} else {
					if (r1 - r2 * thetaH0 == 0) {
						testStatisticsPerStage <- 0
					} else {
						fm <- .getFarringtonManningValuesRatio(r1, r2, thetaH0, n1 / n2)
						testStatisticsPerStage <- (2 * directionUpper - 1) * 
							(r1 - r2 * thetaH0) / sqrt( fm[1] * (1 -  fm[1]) / 
							n1 + thetaH0^2 * fm[2] * (1 - fm[2]) / n2)
					}
				}
			}
			
		} else {
			
			n1 <- sampleSizesPerStage[1, stage]
			r1 <- eventsPerStage[1, stage] / n1
			if (groups == 1) {
				testStatisticsPerStage <- c(testStatisticsPerStage, 
					(2 * directionUpper - 1) * (r1 - thetaH0) / 
					sqrt(thetaH0 * (1 - thetaH0)) * sqrt(n1))
			} else {
				n2 <- sampleSizesPerStage[2, stage]
				r2 <- eventsPerStage[2, stage] / n2
				if (!riskRatio) {
					if (r1 - r2 - thetaH0 == 0) {
						testStatisticsPerStage <- c(testStatisticsPerStage, 0)
					} else {
						fm <- .getFarringtonManningValuesDiff(r1, r2, thetaH0, n1 / n2)
						testStatisticsPerStage <- c(testStatisticsPerStage, 
							(2 * directionUpper - 1) * (r1 - r2 - thetaH0) / 
							sqrt( fm[1] * (1 -  fm[1]) / n1 + fm[2] * (1 - fm[2]) / n2))
					} 
				} else {
					if (r1 - r2 * thetaH0 == 0) {
						testStatisticsPerStage <- c(testStatisticsPerStage, 0)
					} else {
						fm <- .getFarringtonManningValuesRatio(r1, r2, thetaH0, n1 / n2)
						testStatisticsPerStage <- c(testStatisticsPerStage, 
							(2 * directionUpper - 1) * (r1 - r2 * thetaH0) / 
							sqrt( fm[1] * (1 -  fm[1]) / n1 + thetaH0^2 * fm[2] * (1 - fm[2]) / n2))
					}
				}
			}
		}
		
		if (designNumber == 2L) {
			if (stage == 1) {
				value <- testStatisticsPerStage
			} else {		
				value <- (sqrt(informationRates[1]) * testStatisticsPerStage[1] + 
					sqrt(informationRates[2:stage] - informationRates[1:(stage - 1)]) %*% 
					testStatisticsPerStage[2:stage]) / sqrt(informationRates[stage])
			}	
		} else if (designNumber == 3L) {
			if (stage == 1) {
				value <- 1 - pnorm(testStatisticsPerStage)
			} else {
				weightFisher <- rep(NA_real_, stage)
				weightFisher[1] <- 1
				weightFisher[2:stage]  <- sqrt(informationRates[2:stage] - 
					informationRates[1:(stage-1)]) / sqrt(informationRates[1])
				value <- prod((1 - pnorm(testStatisticsPerStage[1:stage]))^weightFisher[1:stage])
			}
		}
		
		pValuesSeparate <- 1 - pnorm(testStatisticsPerStage)
	}
	
	return(list(value = value,
		stagewiseRates = stagewiseRates,
		overallRates = overallRates,	
		sampleSizesPerStage = sampleSizesPerStage,  
		testStatisticsPerStage = testStatisticsPerStage,
		pValuesSeparate = pValuesSeparate))
}


.getSimulationRatesStageSubjects <- function(..., stage, 
		riskRatio, 
		thetaH0, 
		groups, 
		plannedSubjects, 
		directionUpper,
		allocationRatioPlanned,
		minNumberOfSubjectsPerStage,
		maxNumberOfSubjectsPerStage,
		sampleSizesPerStage,
		conditionalPower,
		conditionalCriticalValue,
		overallRate,
		farringtonManningValue1,
		farringtonManningValue2) {
	
	if (is.na(conditionalPower)) {
		return(plannedSubjects[stage] - plannedSubjects[stage - 1]) 
	} 
 
	if (groups == 1) {
		stageSubjects <- 
			(max(0, conditionalCriticalValue * sqrt(thetaH0 * (1 - thetaH0)) + 
				stats::qnorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]))))^2 / 
				(max(1e-12, (2 * directionUpper - 1) * (overallRate[1] - thetaH0)))^2
	} else {
		mult <- 1
		corr <- thetaH0
		if (riskRatio) {
			mult <- thetaH0
			corr <- 0
		}
		stageSubjects <- (1 + 1 / allocationRatioPlanned) *	(max(0, conditionalCriticalValue * 
			sqrt(farringtonManningValue1 * (1 - farringtonManningValue1) + 
			farringtonManningValue2 * (1 - farringtonManningValue2) * allocationRatioPlanned * mult^2) + 
			stats::qnorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]) + overallRate[2] * 
			(1 - overallRate[2]) * allocationRatioPlanned * mult^2)))^2 /
			(max(1e-12,	(2 * directionUpper - 1) * (overallRate[1] - mult * overallRate[2] - corr)))^2
	}
	stageSubjects <- ceiling(min(max(minNumberOfSubjectsPerStage[stage], 
  		stageSubjects), maxNumberOfSubjectsPerStage[stage]))

	return(stageSubjects)

}

.getSimulationStepRates <- function(..., 
		k, 
		kMax,
		designNumber,
		informationRates,
		futilityBounds,
		alpha0Vec,
		criticalValues,
		riskRatio, 
		thetaH0,
		pi1,
		pi2,
		groups,
		plannedSubjects,		
		directionUpper,	
		allocationRatioPlanned,
		minNumberOfSubjectsPerStage,
		maxNumberOfSubjectsPerStage, 
		conditionalPower, 
		pi1H1,
		pi2H1,
		sampleSizesPerStage,
		eventsPerStage,
		testStatisticsPerStage,
		testStatistic,
		calcSubjectsFunction) {
	
	stageSubjects <- plannedSubjects[k] 
	
	# perform event size recalculation for stages 2,..., kMax
	simulatedConditionalPower <- 0
	if (k > 1) {
		
		# used effect size is either estimated from test statistic or pre-fixed
		if (is.na(pi1H1)) {
			overallRate <- testStatistic$overallRate
		} else {
			if (groups == 1) { 
				overallRate <- pi1H1
			} else {
				overallRate <- c(pi1H1, pi2H1)
			}	
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
		
		if (groups == 2) {
			if (!riskRatio) {
				fm <- .getFarringtonManningValuesDiff(overallRate[1], overallRate[2], 
					thetaH0, allocationRatioPlanned)
			} else {
				fm <- .getFarringtonManningValuesRatio(overallRate[1], overallRate[2], 
					thetaH0, allocationRatioPlanned)
			}
		}	
		
		stageSubjects <- calcSubjectsFunction(
			stage = k, 
			riskRatio = riskRatio, 
			thetaH0 = thetaH0, 
			groups = groups, 
			plannedSubjects = plannedSubjects, 
			directionUpper = directionUpper,
			allocationRatioPlanned = allocationRatioPlanned,
			minNumberOfSubjectsPerStage = minNumberOfSubjectsPerStage,
			maxNumberOfSubjectsPerStage = maxNumberOfSubjectsPerStage, 
			sampleSizesPerStage = sampleSizesPerStage,
			conditionalPower = conditionalPower, 
			overallRate = overallRate, 
			conditionalCriticalValue = conditionalCriticalValue,
			farringtonManningValue1 = fm[1],
			farringtonManningValue2 = fm[2])
			
		# calculate conditional power for selected stageSubjects
		if (groups == 1) {
			if (overallRate[1] == 0) overallRate[1] <- 1e-6
			if (overallRate[1] == 1) overallRate[1] <- 1 - 1e-6
			if (overallRate[1] * (1 - overallRate[1]) == 0) {
				theta <- 0
			} else  {
				theta <- (overallRate[1] - thetaH0) / sqrt(overallRate[1] * (1 - overallRate[1])) + 
					sign(overallRate[1] - thetaH0) * conditionalCriticalValue * 
					(1 -  sqrt(thetaH0 * (1 - thetaH0) / (overallRate[1] * (1 - overallRate[1])))) / 
					sqrt(stageSubjects)
			}
		} else {
			if (overallRate[1] * (1 - overallRate[1]) + overallRate[2] * (1 - overallRate[2]) == 0) {
				theta <- 0
			} else {
				if (!riskRatio) {
					theta <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) * (
						(overallRate[1] - overallRate[2] - thetaH0) * sqrt(1 + allocationRatioPlanned) /
						sqrt(overallRate[1] * (1 - overallRate[1]) + allocationRatioPlanned * 
						overallRate[2]*(1 - overallRate[2])) + 
						sign(overallRate[1] - overallRate[2] - thetaH0) * conditionalCriticalValue *
						(1 - sqrt(fm[1] * (1 - fm[1]) + allocationRatioPlanned * 
						fm[2] * (1 - fm[2])) / sqrt(overallRate[1] * (1 - overallRate[1]) + 
						allocationRatioPlanned * overallRate[2] * (1 - overallRate[2]))) *
						(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * stageSubjects))
				} else {
					theta <- sqrt(allocationRatioPlanned) / (1 + allocationRatioPlanned) * (
						(overallRate[1] - thetaH0 * overallRate[2]) * sqrt(1 + allocationRatioPlanned) /
						sqrt(overallRate[1] * (1 - overallRate[1]) + allocationRatioPlanned * 
						thetaH0^2 * overallRate[2] * (1 - overallRate[2])) + 
						sign(overallRate[1] - thetaH0 * overallRate[2]) * conditionalCriticalValue *
						(1 - sqrt(fm[1] * (1 - fm[1]) + allocationRatioPlanned * thetaH0^2 * 
						fm[2] * (1 - fm[2])) / sqrt(overallRate[1] * (1 - overallRate[1]) + 
						allocationRatioPlanned * thetaH0^2 * overallRate[2] * (1 - overallRate[2]))) *
						(1 + allocationRatioPlanned) / sqrt(allocationRatioPlanned * stageSubjects))
				}
			}	
		}
		if (!directionUpper) {
			theta <- -theta
		}
		simulatedConditionalPower <- 
			1 - stats::pnorm(conditionalCriticalValue - theta * sqrt(stageSubjects))
		
	}
	if (groups == 1) {
		n1 <- stageSubjects 
		eventsPerStage <- cbind(eventsPerStage, matrix(c(stats::rbinom(1, n1, pi1)), nrow = 1))
		sampleSizesPerStage <- cbind(sampleSizesPerStage, matrix(n1, nrow = 1))
	} else {
		n1 <- ceiling(allocationRatioPlanned * stageSubjects / (1 + allocationRatioPlanned))
		n2 <- stageSubjects - n1
		eventsPerStage <- cbind(eventsPerStage, 
			matrix(c(stats::rbinom(1, n1, pi1), stats::rbinom(1, n2, pi2)), nrow = 2))
		sampleSizesPerStage <- cbind(sampleSizesPerStage, matrix(c(n1, n2), nrow = 2))
	}	
	
	testStatistic <- .getTestStatisticsRates(designNumber = designNumber, 
		informationRates = informationRates, groups = groups, riskRatio = riskRatio, 
		thetaH0 = thetaH0, directionUpper = directionUpper,  eventsPerStage = eventsPerStage, 
		sampleSizesPerStage = sampleSizesPerStage, testStatisticsPerStage = testStatisticsPerStage)
	
	testStatisticsPerStage <- c(testStatisticsPerStage, testStatistic$testStatisticsPerStage[k])
	
	simulatedRejections <- 0
	simulatedFutilityStop <- 0
	trialStop <- FALSE
	if (k == kMax) {
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
	
	return(list(
		trialStop = trialStop,
		sampleSizesPerStage = sampleSizesPerStage,
		eventsPerStage = eventsPerStage,
		testStatisticsPerStage = testStatisticsPerStage,
		testStatistic = testStatistic,
		simulatedSubjects = stageSubjects,
		simulatedRejections = simulatedRejections,
		simulatedFutilityStop = simulatedFutilityStop,
		simulatedConditionalPower = simulatedConditionalPower
	))
}

#' @title
#' Get Simulation Rates 
#' 
#' @description 
#' Returns the simulated power, stopping probabilities, conditional power, and expected sample size for 
#' testing rates in a one or two treatment groups testing situation. 
#'
#' @param design The trial design. If no trial design is specified, a fixed sample size design is used. 
#' 		  In this case, \code{alpha}, \code{beta}, and \code{sided} can be directly entered as argument.  
#' @param groups The number of treatment groups (1 or 2), default is \code{2}.
#' @param riskRatio If \code{riskRatio = TRUE} is specified, the design characteristics for 
#'        one-sided testing of H0: pi1/pi2 = thetaH0 are simulated, default is \code{FALSE}.
#' @param thetaH0 The null hypothesis value. For one-sided testing, a value != 0 
#'        (or a value != 1 for testing the mean ratio) can be specified, default is 
#'        \code{0} or \code{1} for difference and ratio testing, respectively.
#' @param pi1 The assumed probability in the active treatment group if two treatment groups 
#'        are considered, or the alternative probability for a one treatment group design, 
#'        default is \code{seq(0.2,0.5,0.1)}.
#' @param pi2 The assumed probability in the reference group if two treatment groups are considered, default is \code{0.2}. 
#' @param directionUpper Specifies the direction of the alternative, only applicable 
#'        for one-sided testing, default is \code{TRUE}.
#' @param allocationRatioPlanned The planned allocation ratio for a two treatment groups 
#'        design, default is \code{1}. 
#' @param plannedSubjects \code{plannedSubjects} is a vector of length kMax (the number of stages of the design) 
#' 	      that determines the number of cumulated (overall) subjects when the interim stages are planned.
#' @param minNumberOfSubjectsPerStage When performing a data driven sample size recalculation, 
#' 		  the vector with length kMax \code{minNumberOfSubjectsPerStage} determines the 
#'        minimum number of subjects per stage (i.e., not cumulated), the first element 
#'        is not taken into account.   
#' @param maxNumberOfSubjectsPerStage When performing a data driven sample size recalculation, 
#' 	      the vector with length kMax \code{maxNumberOfSubjectsPerStage} determines the maximum number 
#'        of subjects per stage (i.e., not cumulated), the first element is not taken into account.
#' @param conditionalPower The conditional power for the subsequent stage under which the sample size recalculation is performed.
#' @param pi1H1 If specified, the assumed probability in the active treatment group if two treatment groups 
#'        are considered, or the assumed probability for a one treatment group design, for which the conditional 
#' 		  power was calculated.	 
#' @param pi2H1 If specified, the assumed probability in the reference group if two treatment groups 
#'        are considered, for which the conditional power was calculated, default is \code{0.2}.  
#' @param maxNumberOfIterations The number of simulation iterations.
#' @param calcSubjectsFunction Optionally, a function can be entered that defines the way of performing the sample size
#' 		  recalculation. By default, sample size recalulation is performed with conditional power and specified
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
#' testing situation. The function might depend on variables \code{stage}, \code{riskRatio}, \code{thetaH0}, \code{groups}, 
#' \code{plannedSubjects}, \code{directionUpper}, \code{allocationRatioPlanned}, \code{minNumberOfSubjectsPerStage}, 
#' \code{maxNumberOfSubjectsPerStage}, \code{sampleSizesPerStage}, \code{conditionalPower}, 
#' \code{conditionalCriticalValue}, \code{overallRate}, \code{farringtonManningValue1}, and \code{farringtonManningValue2}.
#' The function has to obtain the three-dots arument '...' (see examples).  
#' 
#'  
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following parameters: median [range]; mean +/-sd\cr
#' 
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable 
#' the output of the aggregated simulated data.\cr
#' 
#' Example 1: \cr
#' \code{simulationResults <- getSimulationRates(plannedSubjects = 40)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#' 
#' Example 2: \cr
#' \code{simulationResults <- getSimulationRates(plannedSubjects = 40)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#' 
#' \code{\link{getData}} can be used to get the aggregated simulated data from the 
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{pi1}: The assumed or derived event rate in the treatment group (if available).
#'   \item \code{pi2}: The assumed or derived event rate in the control group (if available).
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the 
#'         (interim) analysis takes place.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise. 
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision, 
#'         depends on which design was chosen (group sequential, inverse normal, 
#'         or Fisher combination test)'
#'   \item \code{testStatisticsPerStage}: The test statistic for each stage if only data from
#' 			the considered stage is taken into account. 
#'   \item \code{overallRates1}: The overall rate in treatment group 1.
#'   \item \code{overallRates2}: The overall rate in treatment group 2.
#'   \item \code{stagewiseRates1}: The stagewise rate in treatment group 1.
#'   \item \code{stagewiseRates2}: The stagewise rate in treatment group 2.
#'   \item \code{sampleSizesPerStage1}: The stagewise sample size in treatment group 1.
#'   \item \code{sampleSizesPerStage2}: The stagewise sample size in treatment group 2.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.  
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for 
#' 			selected sample size and effect. The effect is either estimated from the data or can be
#' 			user defined with \code{pi1H1} and \code{pi2H1}.   
#' }
#'  
#' @return Returns a \code{\link{SimulationResultsRates}} object.
#' 
#' @export 
#' 
#' @examples
#' 
#' # Fixed sample size with minimum required definitions, pi1 = (0.3,0.4,0.5, 0.6) and pi2 = 0.3
#' getSimulationRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = 120, maxNumberOfIterations = 50)
#' 
#' \donttest{
#' 
#' # Increase number of simulation iterations and compare results with power calculator
#' getSimulationRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = 120, maxNumberOfIterations = 50)
#' getPowerRates(pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, maxNumberOfSubjects = 120)
#' 
#' # Do the same for a two-stage Pocock inverse normal group sequential 
#' # design with non-binding futility stops
#' designIN <- getDesignInverseNormal(typeOfDesign = "P", futilityBounds = c(0))
#' getSimulationRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = c(40, 80), maxNumberOfIterations = 50)
#' getPowerRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, maxNumberOfSubjects = 80)
#' 
#' # Assess power and average sample size if a sample size reassessment is 
#' # foreseen at conditional power 80% for the subsequent stage (decrease and increase) 
#' # based on observed overall rates and specified minNumberOfSubjectsPerStage 
#' # and maxNumberOfSubjectsPerStage
#' 
#' # Do the same under the assumption that a sample size increase only takes place 
#' # if the rate difference exceeds the value 0.1 at interim. For this, the sample 
#' # size recalculation method needs to be redefined:  
#' mySampleSizeCalculationFunction <- function(..., stage,
#'         plannedSubjects,
#'         minNumberOfSubjectsPerStage,
#'         maxNumberOfSubjectsPerStage,
#'         conditionalPower,
#'         conditionalCriticalValue,
#'         overallRate) {
#'     if (overallRate[1] - overallRate[2] < 0.1) {
#'         return(plannedSubjects[stage] - plannedSubjects[stage - 1]) 
#'     } else {
#'         rateUnderH0 <- (overallRate[1] + overallRate[2]) / 2 
#'         stageSubjects <- 2 * (max(0, conditionalCriticalValue * 
#'             sqrt(2 * rateUnderH0 * (1 - rateUnderH0)) + 
#'             stats::qnorm(conditionalPower) * sqrt(overallRate[1] * 
#'             (1 - overallRate[1]) + overallRate[2] * (1 - overallRate[2]))))^2 /
#'             (max(1e-12,	(overallRate[1] - overallRate[2])))^2
#'         stageSubjects <- ceiling(min(max(
#'             minNumberOfSubjectsPerStage[stage], 
#'             stageSubjects), maxNumberOfSubjectsPerStage[stage]))
#'         return(stageSubjects)
#'     }	
#' }
#' getSimulationRates(designIN, pi1 = seq(0.3, 0.6, 0.1), pi2 = 0.3, 
#'     plannedSubjects = c(40, 80), minNumberOfSubjectsPerStage = c(40, 20), 
#'     maxNumberOfSubjectsPerStage = c(40, 160), conditionalPower = 0.8, 
#'     calcSubjectsFunction = mySampleSizeCalculationFunction, maxNumberOfIterations = 50)
#' 
#' }
#' 
getSimulationRates <- function(design = NULL, ...,
		groups = 2L,
		riskRatio = FALSE,
		thetaH0 = ifelse(riskRatio, 1, 0),		
		pi1 = C_PI_1_DEFAULT,
		pi2 = NA_real_,
		plannedSubjects = NA_real_,		
		directionUpper = C_DIRECTION_UPPER_DEFAULT,	
		allocationRatioPlanned = NA_real_,
		minNumberOfSubjectsPerStage = NA_real_,
		maxNumberOfSubjectsPerStage = NA_real_, 
		conditionalPower = NA_real_, 
		pi1H1 = NA_real_,
		pi2H1 = 0.2,
		maxNumberOfIterations = C_MAX_SIMULATION_ITERATIONS_DEFAULT, 
		seed = NA_real_, 
		calcSubjectsFunction = NULL) {
	
	if (is.null(design)) {
		design <- .getDefaultDesignForSampleSizeCalculations(...)
	} else {
		.assertIsTrialDesign(design)
		.warnInCaseOfUnknownArguments(functionName = "getSimulationRates", ...)
		.warnInCaseOfTwoSidedPowerArgument(...)
	}
	.assertIsSingleLogical(directionUpper, "directionUpper")
	.assertIsSingleNumber(thetaH0, "thetaH0")
	if (groups == 1) {
		.assertIsInOpenInterval(thetaH0, "thetaH0", 0, 1, naAllowed = FALSE)
	} else {
		if (riskRatio) {
			.assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
		} else {	
			.assertIsInOpenInterval(thetaH0, "thetaH0", -1, 1, naAllowed = TRUE)
		}
	}	 
	.assertIsNumericVector(pi1, "pi1", naAllowed = FALSE)
	.assertIsInOpenInterval(pi1, "pi1", 0, 1, naAllowed = FALSE)
	.assertIsNumericVector(pi2, "pi2", naAllowed = TRUE)
	.assertIsInOpenInterval(pi2, "pi2", 0, 1, naAllowed = TRUE)
	.assertIsNumericVector(minNumberOfSubjectsPerStage, 
		"minNumberOfSubjectsPerStage", naAllowed = TRUE)
	.assertIsNumericVector(maxNumberOfSubjectsPerStage, 
		"maxNumberOfSubjectsPerStage", naAllowed = TRUE)
	.assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
	.assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
	.assertIsSingleNumber(pi1H1, "pi1H1", naAllowed = TRUE)
	.assertIsInOpenInterval(pi1H1, "pi1H1", 0, 1, naAllowed = TRUE)	
	.assertIsSingleNumber(pi2H1, "pi2H1", naAllowed = TRUE)
	.assertIsInOpenInterval(pi2H1, "pi2H1", 0, 1, naAllowed = TRUE)
	.assertIsSingleNumber(allocationRatioPlanned, "allocationRatioPlanned", naAllowed = TRUE)
	.assertIsInOpenInterval(allocationRatioPlanned, "allocationRatioPlanned", 0, NULL, naAllowed = TRUE)	
	.assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
	.assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
	if (is.null(calcSubjectsFunction)) {
		calcSubjectsFunction <- .getSimulationRatesStageSubjects
	}
	.assertIsValidFunction(fun = calcSubjectsFunction, 
		funArgName = "calcSubjectsFunction",
		expectedFunction = .getSimulationRatesStageSubjects)
	
	if (design$sided == 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"only one-sided case is implemented for the simulation design")
	}
	
	seed <- .setSeed(seed)
	
	simulationResults <- SimulationResultsRates(design)
	
	.setValueAndParameterType(simulationResults, "pi2", pi2, NA_real_)
	.setValueAndParameterType(simulationResults, "allocationRatioPlanned", 
		allocationRatioPlanned, NA_real_)
	if (groups == 1) {
		if (isTRUE(riskRatio)) {
			warning("'riskRatio' (", riskRatio, ") will be ignored ", 
				"because it is not applicable for 'groups' = 1", call. = FALSE)
		}
		
		if (!is.na(allocationRatioPlanned)) {   
			warning("'allocationRatioPlanned' (", allocationRatioPlanned, 
				") will be ignored because it is not applicable for 'groups' = 1", call. = FALSE)
			simulationResults$allocationRatioPlanned <- NA_real_
		}
		simulationResults$.setParameterType("allocationRatioPlanned", C_PARAM_NOT_APPLICABLE)
		
		if (!is.na(pi2)) {   
			warning("'pi2' (", pi2, 
				") will be ignored because it is not applicable for 'groups' = 1", call. = FALSE)
			simulationResults$pi2 <- NA_real_
		}
		simulationResults$.setParameterType("pi2", C_PARAM_NOT_APPLICABLE)
	} else {
		if (is.na(allocationRatioPlanned)) {
			allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
			simulationResults$allocationRatioPlanned <- allocationRatioPlanned
			simulationResults$.setParameterType("allocationRatioPlanned", C_PARAM_DEFAULT_VALUE)
		}
		if (is.na(pi2)) {
			pi2 <- C_PI_2_DEFAULT
			simulationResults$pi2 <- pi2
			simulationResults$.setParameterType("pi2", C_PARAM_DEFAULT_VALUE)
		}	
	}	
	
	if (groups ==1) {
		effect <- pi1 - thetaH0
	} else  {
		if (riskRatio) {
			effect <- pi1 / pi2 - thetaH0
		} else {
			effect <- pi1 - pi2 - thetaH0
		}	
	}	
	simulationResults$effect <- effect
	
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
			"'plannedSubjects' (", .arrayToString(plannedSubjects), ") must have length ", design$kMax)
	}
	.assertIsInClosedInterval(plannedSubjects, "plannedSubjects", lower = 1, upper = NULL)
	.assertValuesAreStrictlyIncreasing(plannedSubjects, "plannedSubjects")

	.setValueAndParameterType(simulationResults, "riskRatio", riskRatio, FALSE)
	.setValueAndParameterType(simulationResults, "thetaH0", thetaH0, ifelse(riskRatio, 1, 0))
	.setValueAndParameterType(simulationResults, "pi1", pi1, C_PI_1_DEFAULT)
	.setValueAndParameterType(simulationResults, "groups", as.integer(groups), 2L)
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
	.setValueAndParameterType(simulationResults, "pi1H1", 
		pi1H1, NA_real_, notApplicableIfNA = TRUE)
	.setValueAndParameterType(simulationResults, "pi2H1", 
		pi2H1, 0.2, notApplicableIfNA = TRUE)
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
	cols <- length(pi1)
	sampleSizes <- matrix(0, kMax, cols)
	rejectPerStage <- matrix(0, kMax, cols)
	overallReject <- rep(0, cols)
	futilityPerStage <- matrix(0, kMax - 1, cols)
	futilityStop <- rep(0, cols)
	iterations <- matrix(0, kMax, cols)
	expectedNumberOfSubjects <- rep(0, cols)
	conditionalPowerAchieved <- matrix(NA_real_, kMax, cols)
	
	len <- length(pi1) * maxNumberOfIterations * kMax
	dataIterationNumber <- rep(NA_real_, len)
	dataStageNumber <- rep(NA_real_, len)
	dataPi1 <- rep(NA_real_, len)
	dataPi2 <- rep(pi2, len)
	dataNumberOfSubjects <- rep(NA_real_, len)
	dataRejectPerStage <- rep(NA_real_, len) 
	dataFutilityPerStage <- rep(NA_real_, len) 
	dataTestStatistic <- rep(NA_real_, len)
	dataTestStatisticsPerStage = rep(NA_real_, len)
	dataOverallRates1 <- rep(NA_real_, len)
	dataOverallRates2 <- rep(NA_real_, len)
	dataStagewiseRates1 <- rep(NA_real_, len)
	dataStagewiseRates2 <- rep(NA_real_, len)
	dataSampleSizesPerStage1 <- rep(NA_real_, len)
	dataSampleSizesPerStage2 <- rep(NA_real_, len)
	dataTrialStop <- rep(NA, len)
	dataConditionalPowerAchieved <- rep(NA_real_, len)
	if (designNumber != 1L) {
		dataPValuesSeparate <- rep(NA_real_, len)
	}
	
	index <- 1
	for (i in 1:length(pi1)) {
		simulatedSubjects <- rep(0, kMax)
		simulatedOverallSubjects <- rep(0, kMax)
		simulatedRejections <- rep(0, kMax)
		simulatedFutilityStop <- rep(0, kMax - 1)
		simulatedOverallSubjects <- 0
		simulatedConditionalPower <- rep(0, kMax)
		
		for (j in 1:maxNumberOfIterations) { 
			trialStop <- FALSE
			sampleSizesPerStage <- matrix(rep(numeric(0), 2), nrow = groups)
			eventsPerStage <- matrix(rep(numeric(0), 2), nrow = groups)
			testStatisticsPerStage <- c()
			testStatistic <- NULL
			
			for (k in 1:kMax) {
				if (!trialStop) {
					stepResult <- .getSimulationStepRates(
						k = k, 
						kMax = kMax,
						designNumber = designNumber,
						informationRates = informationRates,
						futilityBounds = futilityBounds,
						alpha0Vec = alpha0Vec,
						criticalValues = criticalValues,
						riskRatio = riskRatio, 
						thetaH0 = thetaH0,
						pi1 = pi1[i],
						pi2 = pi2,
						groups = groups,
						plannedSubjects = plannedSubjects,		
						directionUpper = directionUpper,	
						allocationRatioPlanned = allocationRatioPlanned,
						minNumberOfSubjectsPerStage =
							minNumberOfSubjectsPerStage,
						maxNumberOfSubjectsPerStage =
							maxNumberOfSubjectsPerStage, 
						conditionalPower = conditionalPower, 
						pi1H1 = pi1H1,
						pi2H1 = pi2H1,
						sampleSizesPerStage = sampleSizesPerStage,
						eventsPerStage = eventsPerStage,
						testStatisticsPerStage = testStatisticsPerStage,
						testStatistic = testStatistic,
						calcSubjectsFunction = calcSubjectsFunction) 
					
					trialStop <- stepResult$trialStop
					sampleSizesPerStage <- stepResult$sampleSizesPerStage
					eventsPerStage <- stepResult$eventsPerStage
					testStatisticsPerStage <- stepResult$testStatisticsPerStage
					testStatistic <- stepResult$testStatistic
					
					simulatedSubjectsStep <- stepResult$simulatedSubjects
					simulatedRejectionsStep <- stepResult$simulatedRejections
					simulatedFutilityStopStep <- stepResult$simulatedFutilityStop
					simulatedConditionalPowerStep <- NA_real_
					if (k > 1) {
						simulatedConditionalPowerStep <- stepResult$simulatedConditionalPower
					}
					
					iterations[k, i] <- iterations[k, i] + 1
					simulatedSubjects[k] <- simulatedSubjects[k] + simulatedSubjectsStep
					simulatedRejections[k] <- simulatedRejections[k] + simulatedRejectionsStep
					if (k < kMax) {
						simulatedFutilityStop[k] <- simulatedFutilityStop[k] + simulatedFutilityStopStep
					}
					simulatedConditionalPower[k] <- simulatedConditionalPower[k] + 
						simulatedConditionalPowerStep

					dataIterationNumber[index] <- j
					dataStageNumber[index] <- k
					dataPi1[index] <- pi1[i]
					dataNumberOfSubjects[index] <- simulatedSubjectsStep
					dataRejectPerStage[index] <- simulatedRejectionsStep
					dataFutilityPerStage[index] <- simulatedFutilityStopStep
					dataTestStatistic[index] <- testStatistic$value
					dataTestStatisticsPerStage[index] <- testStatisticsPerStage[k]
					dataOverallRates1[index] <- testStatistic$overallRates[1]
					dataStagewiseRates1[index] <- testStatistic$stagewiseRates[1]
					dataSampleSizesPerStage1[index] <- testStatistic$sampleSizesPerStage[1]					
					if (length(testStatistic$stagewiseRates) > 1) {
						dataOverallRates2[index] <- testStatistic$overallRates[2]
						dataStagewiseRates2[index] <- testStatistic$stagewiseRates[2]
						dataSampleSizesPerStage2[index] <- testStatistic$sampleSizesPerStage[2]
					} else {
						dataStagewiseRates2[index] <- NA_real_
						dataOverallRates2[index] <- NA_real_
						dataSampleSizesPerStage2[index] <- NA_real_
					}
					dataTrialStop[index] <- trialStop
					dataConditionalPowerAchieved[index] <- simulatedConditionalPowerStep
					if (designNumber != 1L) {
						dataPValuesSeparate[index] <- testStatistic$pValuesSeparate[k]
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
		if (length(pi1) == 1) {
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
		simulationResults$earlyStop <- rep(0, length(pi1))
	}	
	simulationResults$expectedNumberOfSubjects <- expectedNumberOfSubjects
	simulationResults$conditionalPowerAchieved <- conditionalPowerAchieved
	
	if (!all(is.na(simulationResults$conditionalPowerAchieved))) {
		simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
	}
	
	data <- data.frame(
		iterationNumber = dataIterationNumber,
		stageNumber = dataStageNumber,
		pi1 = dataPi1,
		pi2 = dataPi2,
		numberOfSubjects = dataNumberOfSubjects,
		rejectPerStage = dataRejectPerStage,
		futilityPerStage = dataFutilityPerStage,
		testStatistic = dataTestStatistic,
		testStatisticsPerStage = dataTestStatisticsPerStage,
		overallRates1 = dataOverallRates1,
		overallRates2 = dataOverallRates2,
		stagewiseRates1 = dataStagewiseRates1,
		stagewiseRates2 = dataStagewiseRates2,
		sampleSizesPerStage1 = dataSampleSizesPerStage1,
		sampleSizesPerStage2 = dataSampleSizesPerStage2,
		trialStop = dataTrialStop,
		conditionalPowerAchieved = round(dataConditionalPowerAchieved, 6)
	)
	if (designNumber == 3L) {
		data$pValue <- dataPValuesSeparate
	}
	data <- data[!is.na(data$pi1), ]
	
	simulationResults$.data <- data
	
	return(simulationResults)
}

