######################################################################################
#                                                                                    #
# -- Analysis functions --                                                           #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.1                                                                #
# Date: 03-12-2018                                                                   #
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
#' Get Analysis Results
#' 
#' @description  
#' Calculates and returns the analysis results for the specified design and data.    
#' @param design The trial design.
#' @param dataInput The summary data used for calculating the test results. 
#'        This is either an element of \code{DatasetMeans}, of \code{DatasetRates}, or of \code{DatasetSurvival}.
#'        For more information see \code{details} below.
#' @param directionUpper The direction of one-sided testing. 
#'        Default is \code{directionUpper = TRUE} which means that larger values of the 
#'        test statistics yield smaller p-values.
#' @param thetaH0 The null hypothesis value, default is 0 for the normal and the binary case, 
#'        it is 1 for the survival case.      
#'        For testing a rate in one sample, a value thetaH0 in (0, 1) has to be specified for 
#'        defining the null hypothesis H0: pi = thetaH0.\cr
#' 		  For non-inferiority designs, this is the non-inferiority bound. 	
#' @param nPlanned The sample size planned for the subsequent stages. 
#'        It should be a vector with length equal to the remaining stages and is the 
#'        overall sample size in the two treatment groups if two groups are considered.   
#' @param ... Further arguments to be passed to methods (cp. separate functions in See Also), e.g.,
#' \describe{
#'   \item{stage}{The stage number (optional). Default: total number of existing stages in the data input.}
#'   \item{allocationRatioPlanned}{The allocation ratio n1/n2 for two treatment groups planned for 
#'       the subsequent stages, the default value is 1.} 
#'   \item{thetaH1 and assumedStDev or pi1, pi2}{The assumed effect size or assumed rates to calculate the 
#'       conditional power. Depending on the type of dataset, either thetaH1 (means and survival) 
#'       or pi1, pi2 (rates) can be specified. Additionally, if testing means is specified, 
#'       an assumed standard deviation can be specified, default is 1.}
#'   \item{normalApproximation}{The type of computation of the p-values. Default is FALSE for 
#'       testing means (i.e., the t test is used) and TRUE for testing rates and the hazard ratio. 
#'       For testing rates, if \cr
#'       \code{normalApproximation = FALSE} is specified, the binomial test 
#'       (one sample) or the test of Fisher (two samples) is used for calculating the p-values.
#'       In the survival setting, \cr
#'       \code{normalApproximation = FALSE} has no effect.}  
#'   \item{equalVariances}{The type of t test. For testing means in two treatment groups, either 
#'       the t test assuming that the variances are equal or the t test without assuming this, 
#'       i.e., the test of Welch-Satterthwaite is calculated, default is \code{equalVariances = TRUE}.}
#'   \item{iterations}{Iterations for simulating the power for Fisher's combination test. 
#'       If the power for more than one remaining stages is to be determined for 
#'       Fisher's combination test, it is estimated via simulation with specified \cr 
#'       \code{iterations}, the default value is 10000.}
#'   \item{seed}{Seed for simulating the power for Fisher's combination test. 
#'       See above, default is a random seed.}
#' }
#' 
#' @details
#' Given a design and a dataset, at given stage the function calculates the test results 
#' (effect sizes, stage-wise test statistics and p-values, overall p-values and test statistics, 
#' conditional rejection probability (CRP), conditional power, Repeated Confidence Intervals (RCIs), 
#' repeated overall p-values, and final stage p-values, median unbiased effect estimates, 
#' and final confidence intervals. \cr
#' 
#' \code{dataInput} is either an element of \code{DatasetMeans}, of \code{DatasetRates}, or of 
#' \code{DatasetSurvival} and should be created with the function \code{\link{getDataset}}.
#' 
#' @section Note:
#' The conditional power is calculated only if effect size and sample size 
#' is specified. Median unbiased effect estimates and confidence intervals are calculated if 
#' a group sequential design or an inverse normal combination test design was chosen, i.e., it is not applicable 
#' for Fisher's p-value combination test design. 
#' 
#' A final stage p-value for Fisher's combination test is calculated only if a two-stage design was chosen.
#' For Fisher's combination test, the conditional power for more than one remaining stages is estimated via simulation.     
#' 
#' @return Returns an \code{\link{AnalysisResults}} object.
#'
#' @export
#' 
#' @seealso 
#' Alternatively the analysis results can be calculated separately using one of the following functions:
#' \itemize{
#'   \item \code{\link{getTestActions}},
#'   \item \code{\link{getConditionalPower}},
#'   \item \code{\link{getConditionalRejectionProbabilities}},
#'   \item \code{\link{getRepeatedConfidenceIntervals}},
#'   \item \code{\link{getRepeatedPValues}},
#'   \item \code{\link{getFinalConfidenceInterval}},
#'   \item \code{\link{getFinalPValue}}.
#' }
#'  
#' @examples
#' 
#' \donttest{
#' 
#' design <- getDesignGroupSequential()
#' dataMeans <- getDataset(
#'     n = c(10,10),
#'     means = c(1.96,1.76),
#'     stDevs = c(1.92,2.01))
#' getAnalysisResults(design, dataMeans)
#' 
#' }
#' 
getAnalysisResults <- function(
	design, dataInput, ..., 
	directionUpper = C_DIRECTION_UPPER_DEFAULT, 
	thetaH0 = NA_real_, 
	nPlanned = NA_real_) {
	
	.assertIsTrialDesign(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
	.assertIsValidStage(stage, design$kMax)
	.assertIsValidThetaH0DataInput(thetaH0, dataInput)
	.assertAreSuitableInformationRates(design, dataInput, stage = stage)
	
	if (design$kMax < 2) stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "getAnalysisResults only available for design with interim stage(s)")	
	
	if (dataInput$isDatasetMeans()) {
		if (is.na(thetaH0)) {
			thetaH0 = C_THETA_H0_MEANS_DEFAULT
		}
		return(.getAnalysisResultsMeans(design = design, dataInput = dataInput, 
				directionUpper = directionUpper, thetaH0 = thetaH0, nPlanned = nPlanned, ...))
	}
	
	if (dataInput$isDatasetRates()) {
		if (is.na(thetaH0)) {
			thetaH0 = C_THETA_H0_RATES_DEFAULT
		}
		return(.getAnalysisResultsRates(design = design, dataInput = dataInput,  
				directionUpper = directionUpper, thetaH0 = thetaH0, nPlanned = nPlanned, ...))
	}
	
	if (dataInput$isDatasetSurvival()) {
		if (is.na(thetaH0)) {
			thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT
		}
		return(.getAnalysisResultsSurvival(design = design, dataInput = dataInput, 
				directionUpper = directionUpper, thetaH0 = thetaH0, nPlanned = nPlanned, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", class(dataInput), "' is not implemented yet")
}

#' @title
#' Get Stage Results
#' 
#' @description  
#' Returns summary statistics and p-values for a given data set and a given design.
#'
#' @param design The trial design.
#' @param dataInput The summary data used for calculating the test results. 
#'        This is either an element of \code{DatasetMeans}, of \code{DatasetRates}, or of \code{DatasetSurvival}. 
#'        See \code{\link{getDataset}}.
#' @param ... Further (optional) arguments to be passed:
#' \describe{
#'   \item{stage}{The stage number (optional). Default: total number of existing stages in the data input.}
#'   \item{thetaH0}{The null hypothesis value, default is 0 for the normal and the binary case, 
#'       it is 1 for the survival case.      
#'       For testing a rate in one sample, a value thetaH0 in (0, 1) has to be specified for 
#'       defining the null hypothesis H0: pi = thetaH0. \cr
#' 		 For non-inferiority designs, this is the non-inferiority bound. } 
#'   \item{thetaH1 and assumedStDev or pi1, pi2}{The assumed effect size or assumed rates to calculate the 
#'       conditional power. Depending on the type of dataset, either thetaH1 (means and survival) 
#'       or pi1, pi2 (rates) can be specified. Additionally, if testing means is specified, 
#'       an assumed standard deviation can be specified, default is 1.}
#'   \item{normalApproximation}{The type of computation of the p-values. Default is FALSE for 
#'       testing means (i.e., the t test is used) and TRUE for testing rates and the hazard ratio. 
#'       For testing rates, if \cr 
#'       \code{normalApproximation = FALSE} is specified, the binomial test 
#'       (one sample) or the test of Fisher (two samples) is used for calculating the p-values.
#'       In the survival setting, \cr
#'       \code{normalApproximation = FALSE} has no effect.}  
#'   \item{equalVariances}{The type of t test. For testing means in two treatment groups, either 
#'       the t test assuming that the variances are equal or the t test without assuming this, 
#'       i.e., the test of Welch-Satterthwaite is calculated, default is \code{equalVariances = TRUE}.}
#'   \item{directionUpper}{The direction of one-sided testing. 
#'       Default is \code{directionUpper = TRUE} which means that larger values of the 
#'       test statistics yield smaller p-values.}
#' }
#' 
#' @details
#' Calculates and returns the stage results of the specified design and data input at the specified stage.
#' 
#' @return Returns a \code{\link{StageResults}} object.
#'
#' @export
#' 
#' @examples
#' 
#' design <- getDesignInverseNormal()
#' dataRates <- getDataset(
#'     n1 = c(10,10),
#'     n2 = c(20,20),
#'     events1 = c(8,10),
#'     events2 = c(10,16))
#' getStageResults(design, dataRates)
#' 
getStageResults <- function(design, dataInput, ...) {
	
	.assertIsTrialDesign(design)
	
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
	.assertIsValidStage(stage, design$kMax)
	
	if (dataInput$isDatasetMeans()) {
		return(.getStageResultsMeans(design = design, dataInput = dataInput, ...))
	}
	
	if (dataInput$isDatasetRates()) {
		return(.getStageResultsRates(design = design, dataInput = dataInput, ...))
	}
	
	if (dataInput$isDatasetSurvival()) {
		return(.getStageResultsSurvival(design = design, dataInput = dataInput, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", class(dataInput), "' is not supported")
}

.getStageFromOptionalArguments <- function(..., dataInput) {
	stage <- .getOptionalArgument("stage", ...)
	if (is.null(stage)) {
		.assertIsDataset(dataInput)
		stage <- dataInput$getNumberOfStages()
	}
	return(as.integer(stage))
}

#' 
#' @title
#' Get Test Actions
#'
#' @description
#' Returns test actions.
#' 
#' @param design The trial design.
#' @param stageResults The results at given stage, obtained from \code{\link{getStageResults}}.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#' 
#' @details
#' Returns the test actions of the specified design and stage results at the specified stage.
#' 
#' @export
#' 
#' @keywords internal
#' 
getTestActions <- function(design, stageResults, ...) {
	
	.assertIsTrialDesign(design)
	.assertIsStageResults(stageResults)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	
	.warnInCaseOfUnknownArguments(functionName = "getTestActions", ignore = c("stage"), ...)
	
	testActions <- rep(NA_character_, design$kMax)
	if (.isTrialDesignInverseNormal(design)) {
		for (k in 1 : stage) {
			if (design$sided == 1) {
				if (k < design$kMax) { 
					if (stageResults$combInverseNormal[k] > design$criticalValues[k]) {
						testActions[k] <- "reject and stop"
					}
					else if (stageResults$combInverseNormal[k] < design$futilityBounds[k]) {
						testActions[k] <- "accept and stop"
					} else {
						testActions[k] <- "continue"
					}
				} else {
					if (stageResults$combInverseNormal[k] > design$criticalValues[k]) {
						testActions[k] <- "reject"
					} else {
						testActions[k] <- "accept"
					}
				}
			}
			if (design$sided == 2) {
				if (k < design$kMax) { 
					if (abs(stageResults$combInverseNormal[k]) > design$criticalValues[k]) {
						testActions[k] <- "reject and stop"
					} else {
						testActions[k] <- "continue"
					}
				} else {
					if (abs(stageResults$combInverseNormal[k]) > design$criticalValues[k]) {
						testActions[k] <- "reject"
					} else {
						testActions[k] <- "accept"
					}
				}
			}
		}
	}
	else if (.isTrialDesignGroupSequential(design)) {
		for (k in 1 : stage) {
			if (design$sided == 1) {
				if (k < design$kMax) { 
					if (stats::qnorm(1 - stageResults$overallPValues[k]) > design$criticalValues[k]) {
						testActions[k] <- "reject and stop"
					}
					else if (stats::qnorm(1 - stageResults$overallPValues[k]) < design$futilityBounds[k]) {
						testActions[k] <- "accept and stop"
					} else {
						testActions[k] <- "continue"
					}
				} else {
					if (stats::qnorm(1 - stageResults$overallPValues[k]) > design$criticalValues[k]) {
						testActions[k] <- "reject"
					} else {
						testActions[k] <- "accept"
					}
				}
			}
			if (design$sided == 2) {
				if (k < design$kMax) { 
					if (abs(stats::qnorm(1 - stageResults$overallPValues[k])) > design$criticalValues[k]) {
						testActions[k] <- "reject and stop"
					} else {
						testActions[k] <- "continue"
					}
				} else {
					if (abs(stats::qnorm(1 - stageResults$overallPValues[k])) > design$criticalValues[k]) {
						testActions[k] <- "reject"
					} else {
						testActions[k] <- "accept"
					}
				}
			}
		}
	} 
	else if (.isTrialDesignFisher(design)) {
		for (k in 1 : stage) {		
			if (design$sided == 1) {
				if (k < design$kMax) { 
					if (stageResults$combFisher[k] < design$criticalValues[k]) {
						testActions[k] <- "reject and stop"
					}
					else if (stageResults$pValues[k] > design$alpha0Vec[k]) {
						testActions[k] <- "accept and stop"
					} else {
						testActions[k] <- "continue"
					}
				} else {
					if (stageResults$combFisher[k] < design$criticalValues[k]) {
						testActions[k] <- "reject"
					} else {
						testActions[k] <- "accept"
					}
				}
			}	
			if (design$sided == 2) {
				if (k < design$kMax) { 
					if (min(stageResults$combFisher[k], 1 - stageResults$combFisher[k]) < design$criticalValues[k]) {
						testActions[k] <- "reject and stop"
					} else {
						testActions[k] <- "continue"
					}
				} else {
					if (min(stageResults$combFisher[k], 1 - stageResults$combFisher[k]) < design$criticalValues[k]) {
						testActions[k] <- "reject"
					} else {
						testActions[k] <- "accept"
					}
				}
			}	
		}
	}
	return(testActions)
}

#' 
#' @title 
#' Get Repeated Confidence Intervals
#' 
#' @description
#' Calculates and returns the lower and upper limit of the repeated confidence intervals of the trial.
#'  
#' @param design The trial design.
#' @param dataInput The summary data used for calculating the test results. 
#' This is either an element of \code{DatasetMeans}, of \code{DatasetRates}, or of \code{DatasetSurvival}. 
#' See \code{\link{getDataset}}.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#'  
#' @details
#' The repeated confidence interval at a given stage of the trial contains the parameter values that are not rejected using the specified sequential design.
#' It can be calculated at each stage of the trial and can thus be used as a monitoring tool. 
#' 
#' The repeated confidence intervals are provided up to the specified stage.
#' 
#' @export
#' 
#' @keywords internal
#'
getRepeatedConfidenceIntervals <- function(design, dataInput, ...) {
	
	.assertIsTrialDesign(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
	.assertIsValidStage(stage, design$kMax)
	
	if (dataInput$isDatasetMeans()) {
		return(.getRepeatedConfidenceIntervalsMeans(
				design = design, dataInput = dataInput, ...))
	}
	
	if (dataInput$isDatasetRates()) {
		return(.getRepeatedConfidenceIntervalsRates(
				design = design, dataInput = dataInput, ...))
	}
	
	if (dataInput$isDatasetSurvival()) {
		return(.getRepeatedConfidenceIntervalsSurvival(
				design = design, dataInput = dataInput, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", class(dataInput), "' is not implemented yet")
}

#' 
#' @title 
#' Get Conditional Power
#' 
#' @description 
#' Calculates and returns the conditional power.
#' 
#' @param design The trial design.
#' @param stageResults The results at given stage, obtained from \code{\link{getStageResults}}.
#' @param nPlanned The sample size planned for the subsequent stages.
#'        It should be a vector with length equal to the remaining stages and is the 
#'        overall sample size in the two treatment groups if two groups are considered.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#' @param allocationRatioPlanned The allocation ratio for two treatment groups planned for 
#'        the subsequent stages, the default value is 1. 
#' @param thetaH1 or pi1, pi2 Assumed effect sizes or assumed rates pi1 to calculate the 
#'        conditional power. Depending on the type of dataset, either thetaH1 (means and survival) 
#'        or pi1, pi2 (rates) needs to be specified. 
#' 		  Additionally, if testing means is specified, an assumed standard (\code{assumedStDev}) 
#' 		  deviation can be specified, default is 1.
#' @param iterations Iterations for simulating the power for Fisher's combination test. 
#' If the power for more than one remaining stages is to be determined for Fisher's combination test, 
#' it is estimated via simulation with specified \code{iterations}, the default value is 10000.  
#' @param seed Seed for simulating the power for Fisher's combination test. See above, default is a random seed.
#' 
#' @details 
#' The conditional power is calculated only if effect size and sample size is specified.
#' 
#' For Fisher's combination test, the conditional power for more than one remaining stages is 
#' estimated via simulation.     
#' 
#' @export
#' 
#' @seealso 
#' \code{\link{plot.StageResults}} or \code{\link{plot.AnalysisResults}} for plotting the conditional power.
#' 
#' @keywords internal
#' 
getConditionalPower <- function(design, stageResults, ..., nPlanned) {
	
	.assertIsTrialDesign(design)
	.assertIsStageResults(stageResults)
	
	if (stageResults$isDatasetMeans()) {
		return(.getConditionalPowerMeans(design = design, stageResults = stageResults,
				nPlanned = nPlanned, ...))
	}
	
	if (stageResults$isDatasetRates()) {
		return(.getConditionalPowerRates(design = design, stageResults = stageResults,
				nPlanned = nPlanned, ...))
	}
	
	if (stageResults$isDatasetSurvival()) {
		return(.getConditionalPowerSurvival(design = design, stageResults = stageResults,
				nPlanned = nPlanned, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", 
		class(stageResults$.dataInput), "' is not implemented yet")
}


.getConditionalPowerPlot <- function(..., 
	stageResults, nPlanned, stage = stageResults$getDataInput()$getNumberOfStages(),
	allocationRatioPlanned = NA_real_) {
	
	.assertIsStageResults(stageResults)
	design <- stageResults$.design
	.assertIsValidStage(stage, design$kMax)
	.assertIsValidNPlanned(nPlanned, design$kMax, stage)
	if (is.na(allocationRatioPlanned)) {
		allocationRatioPlanned <- C_ALLOCATION_RATIO_DEFAULT
	}
	
	if (stageResults$isDatasetMeans()) {
		return(.getConditionalPowerPlotMeans(design = design, stageResults = stageResults,
				stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...))
	}
	
	if (stageResults$isDatasetRates()) {
		return(.getConditionalPowerPlotRates(design = design, stageResults = stageResults,
				stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...))
	}
	
	if (stageResults$isDatasetSurvival()) {
		return(.getConditionalPowerPlotSurvival(design = design, stageResults = stageResults,
				stage = stage, nPlanned = nPlanned, allocationRatioPlanned = allocationRatioPlanned, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", 
		class(stageResults$.dataInput), "' is not implemented yet")
}

#' 
#' @title 
#' Get Repeated P Values
#' 
#' @description
#' Calculates the repeated p-values for given test results.
#' 
#' @param design The trial design.
#' @param stageResults The results at given stage, obtained from \code{\link{getStageResults}}.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#'  
#' @details
#' The repeated p-value at a given stage of the trial is defined as the smallest 
#' significance level under which at given test design the test results
#' obtain rejection of the null hypothesis. It can be calculated at each 
#' stage of the trial and can thus be used as a monitoring tool. 
#' 
#' The repeated p-values are provided up to the specified stage.
#' 
#' @export
#' 
#' @keywords internal
#' 
getRepeatedPValues <- function(design, stageResults, ...) {
	
	.assertIsTrialDesign(design)
	.assertIsStageResults(stageResults)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	
	if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
		if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
			warning("Repeated p-values not available for 'typeOfDesign' = '", 
				C_TYPE_OF_DESIGN_AS_USER, "'", call. = FALSE)
			return(rep(NA_real_, design$kMax))
		}
		if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
			warning("Repeated p-values not available for 'typeOfDesign' = '", 
				C_TYPE_OF_DESIGN_WT_OPTIMUM, "'", call. = FALSE)
			return(rep(NA_real_, design$kMax))
		}
	}	
	
	if (.isTrialDesignFisher(design)) {
		if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
			warning("Repeated p-values not available for 'method' = '", 
				C_FISHER_METHOD_USER_DEFINED_ALPHA, "'", call. = FALSE)
			return(rep(NA_real_, design$kMax))
		}
	}	
	
	if (.isTrialDesignInverseNormal(design)) {
		return(.getRepeatedPValuesInverseNormal(design = design, 
				stageResults = stageResults, ...))
	}
	
	if (.isTrialDesignGroupSequential(design)) {
		return(.getRepeatedPValuesGroupSequential(design = design, 
				stageResults = stageResults, ...))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getRepeatedPValuesFisher(design = design, 
				stageResults = stageResults, ...))
	}
	
	.stopWithWrongDesignMessage(design)
}

#
# Get final p-value based on inverse normal method
#
.getFinalPValueInverseNormalOrGroupSequential <- function(..., design, stageResults) {
	
	.assertIsTrialDesignInverseNormalOrGroupSequential(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getFinalPValueInverseNormalOrGroupSequential", 
		ignore = c("stage"), ...)
	
	if (.isTrialDesignInverseNormal(design)) {
		stageInverseNormalOrGroupSequential <- .getStageInverseNormal(design, stageResults, stage)
	} else {
		stageInverseNormalOrGroupSequential <- .getStageGroupSeq(design, stageResults, stage)
	}
	finalStage <- min(stageInverseNormalOrGroupSequential, design$kMax)
	
	# Early stopping or at end of study
	if (stageInverseNormalOrGroupSequential < design$kMax || stage == design$kMax) { 
		
		if (stageInverseNormalOrGroupSequential == 1) {
			
			pFinal <- stageResults$pValues[1]
			
		} else {
			
			if (design$bindingFutility){
				if (.isTrialDesignInverseNormal(design)) {
					decisionMatrix <- matrix(c(design$futilityBounds[1:(finalStage - 1)], C_FUTILITY_BOUNDS_DEFAULT, 
							c(design$criticalValues[1:(finalStage - 1)], stageResults$combInverseNormal[finalStage])), 
						nrow = 2, byrow = TRUE)
				} else {
					decisionMatrix <- matrix(c(design$futilityBounds[1:(finalStage - 1)], C_FUTILITY_BOUNDS_DEFAULT, 
							c(design$criticalValues[1:(finalStage - 1)], stats::qnorm(1 - stageResults$overallPValues[finalStage]))), 
						nrow = 2, byrow = TRUE)
				}
			} else {
				if (.isTrialDesignInverseNormal(design)) {
					decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT,finalStage), 
							c(design$criticalValues[1:(finalStage - 1)], stageResults$combInverseNormal[finalStage])), 
						nrow = 2, byrow = TRUE)
				} else {
					decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT,finalStage), 
							c(design$criticalValues[1:(finalStage - 1)], stats::qnorm(1 - stageResults$overallPValues[finalStage]))), 
						nrow = 2, byrow = TRUE)
				}
			}
			
			probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
				informationRates = design$informationRates[1:finalStage])
			pFinal <- sum(probs[3, ] - probs[2, ])
			
			if (design$sided == 2){
				if (stageInverseNormalOrGroupSequential == 1) {
					
					pFinalOtherDirection <- 1 - stageResults$pValues[1] 
					
				} else {
					if (.isTrialDesignInverseNormal(design)) {
						decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT,finalStage), 
								c(design$criticalValues[1:(finalStage - 1)], -stageResults$combInverseNormal[finalStage])), 
							nrow = 2, byrow = TRUE)
					} else {
						decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT,finalStage), 
								c(design$criticalValues[1:(finalStage - 1)], -stats::qnorm(1 - stageResults$overallPValues[finalStage]))), 
							nrow = 2, byrow = TRUE)
					}
					probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
						informationRates = design$informationRates[1:finalStage])
					
					pFinalOtherDirection <- sum(probs[3, ] - probs[2, ])
				}
				
				pFinal <- 2*min(pFinal, pFinalOtherDirection)
			}
		}
		
		return(list(finalStage = finalStage, pFinal = pFinal))
		
	}
	
	return(list(finalStage = NA_integer_, pFinal = NA_real_))
}

# 
# Returns the weights for inverse normal statistic
# 
.getWeightsInverseNormal <- function(design) {
	weights <- rep(NA, design$kMax) 
	weights[1] <- sqrt(design$informationRates[1])
	if (design$kMax == 1) {
		return(weights)
	}
	
	weights[2:design$kMax] <- sqrt(design$informationRates[2:design$kMax] - 
			design$informationRates[1:(design$kMax - 1)]) 
	return(weights)
}

# 
# Returns the weights for Fisher's combination test statistic
# 
.getWeightsFisher <- function(design) {
	weights <- rep(NA, design$kMax) 
	weights[1] <- 1
	if (design$kMax == 1) {
		return(weights)
	}
	
	weights[2:design$kMax] <- sqrt((design$informationRates[2:design$kMax] - 
				design$informationRates[1:(design$kMax - 1)]) / design$informationRates[1]) 
	return(weights)
}

# 
# Returns the stage when using the inverse normal combination test
# 
.getStageInverseNormal <- function(design, stageResults, stage) {
	
	for (k in 1:stage) {
		if (stageResults$combInverseNormal[k] >= design$criticalValues[k]) {
			return(k)
		}
		if (design$sided == 2){
			if (stageResults$combInverseNormal[k] <= -design$criticalValues[k]) {
				return(k)
			}
		}
		
		if (design$bindingFutility && k < design$kMax && stageResults$combInverseNormal[k] <= 
			design$futilityBounds[k]) {
			return(k)
		}
	}
	
	# no early stopping
	return(as.integer(stage + design$kMax))
}

# 
# Returns the stage when using the group sequential test
# 
.getStageGroupSeq <- function(design, stageResults, stage) {
	
	for (k in 1:stage) {
		
		if (stats::qnorm(1 - stageResults$overallPValues[k]) >= design$criticalValues[k]) {
			return(k)
		}
		if (design$sided == 2){
			if (stats::qnorm(1 - stageResults$overallPValues[k]) <= -design$criticalValues[k]) {
				return(k)
			}
		}
		
		if (design$bindingFutility && k < design$kMax && 
			stats::qnorm(max(1e-8,1 - stageResults$overallPValues[k])) <= design$futilityBounds[k]) {
			return(k)
		}
	}
	
	# no early stopping
	return(as.integer(stage + design$kMax)) 
}

# 
# Returns the stage when using Fisher's combination test
# 
.getStageFisher <- function(design, stageResults, stage) {
	
	for (k in 1:stage) {
		if (stageResults$combFisher[k] <= design$criticalValues[k]) {
			return(k)
		}
		if (design$sided == 2){
			if (1 - stageResults$combFisher[k] <= design$criticalValues[k]) {
				return(k)
			}
		}
		
		if (design$bindingFutility && k < design$kMax && stageResults$pValues[k] >= design$alpha0Vec[k]) {
			return(k)
		}
	}
	
	# no early stopping
	return(as.integer(stage + design$kMax)) 
}

# @title
# q function
# 
# @description
# Function for calculating the final p-value for two-stage design with Fisher's combination test
# and its use for calculating confidence intervals, see Wassmer & Brannath, p. 192 and Brannath et al. (2002), p. 241. 
# Formula generalized for arbitrary weight in combination test.
#
.getQFunctionResult <- function(..., design, stageResults, theta, infRate) {
	
	alpha1 <- design$criticalValues[1]
	alpha0 <- design$alpha0Vec[1]
	if (!design$bindingFutility || (design$sided == 2)) alpha0 <- 1
	weightForFisher <- stageResults$weightsFisher[2]
	
	if (theta != 0) {
		alpha1Adj <- ifelse(alpha1 <= 0, 0,
			1 - stats::pnorm(stats::qnorm(1 - alpha1) - theta / stageResults$overallStDevs[1] * infRate[1]))
	} else {
		alpha1Adj <- alpha1
	}
	
	if (is.na(alpha1Adj)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to calculate 'alpha1Adj'")
	}
	
	if (theta != 0) {
		alpha0Adj <- ifelse(alpha0 >= 1, 1,
			1 - stats::pnorm(stats::qnorm(1 - alpha0) - theta / stageResults$overallStDevs[1] * infRate[1]))
	} else {
		alpha0Adj <- alpha0
	} 
	
	if (is.na(alpha0Adj)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to calculate 'alpha0Adj'")
	}
	
	if (stageResults$pValues[1] <= alpha1Adj || stageResults$pValues[1] >= alpha0Adj) { 
		return(stageResults$pValues[1])
	}
	
	if (weightForFisher == 1) {
		return(max(alpha1Adj, stageResults$pValues[1] * stageResults$pValues[2]) + stageResults$pValues[1] * 
				stageResults$pValues[2] * (log(alpha0Adj) - log(max(alpha1Adj, 
							stageResults$pValues[1] * stageResults$pValues[2]))))
	} 
	
	return(max(alpha1Adj, stageResults$pValues[1] * stageResults$pValues[2]^weightForFisher) + 
			weightForFisher / (weightForFisher - 1) * stageResults$pValues[1]^(1 / weightForFisher) * 
			stageResults$pValues[2] * (alpha0Adj^(1 - 1 / weightForFisher) - 
				max(alpha1Adj, stageResults$pValues[1] * stageResults$pValues[2]^weightForFisher)^(1 - 1 / 
					weightForFisher)))
}

#
# Get final p-value based on Fisher combination test
#
.getFinalPValueFisher <- function(..., design, stageResults) {
	
	.assertIsTrialDesignFisher(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getFinalPValueFisher", 
		ignore = c("stage"), ...)
	
	stageFisher <- .getStageFisher(design, stageResults, stage)
	finalStage <- min(stageFisher, design$kMax)
	
	# Early stopping or at end of study
	if (stageFisher < design$kMax || stage == design$kMax) { 
		if (stageFisher == 1) {
			
			pFinal <- stageResults$pValues[1] 
			
		} else {
			if (design$kMax > 2) {
				warning("Final p-value cannot be calculated for kMax = ", design$kMax, " ",
					"because the function for Fisher's design is implemented only for kMax <= 2", call. = FALSE)
				return(list(finalStage = NA_integer_, pFinal = NA_real_))
			}
			
			# Final p-value for kMax = 2  
			pFinal <- .getQFunctionResult(design = design, stageResults = stageResults, 
				theta = 0, infRate = 0)
		}
		
		if (design$sided == 2){
			if (stageFisher == 1) {
				
				pFinalOtherDirection <- 1 - stageResults$pValues[1] 
				
			} else {
				
				stageResults$pValues <- 1 - stageResults$pValues 
				pFinalOtherDirection <- .getQFunctionResult(design = design, stageResults = stageResults, 
					theta = 0, infRate = 0)
				stageResults$pValues <- 1 - stageResults$pValues				
			}
			
			# Final p-value for kMax = 2
			pFinal <- 2*min(pFinal, pFinalOtherDirection)
		}
		
		return(list(finalStage = finalStage, pFinal = pFinal))
		
	}
	
	return(list(finalStage = NA_integer_, pFinal = NA_real_))
}

#' 
#' @title 
#' Get Final P Value
#' 
#' @description
#' Returns the final p-value for given stage results.
#' 
#' @param design The trial design.
#' @param stageResults The results at given stage, obtained from \code{\link{getStageResults}}.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#'  
#' @details 
#' The calculation of the final p-value is based on the stagewise ordering of the sample space. 
#' This enables the calculation for both the non-adaptive and the adaptive case.
#' For Fisher's combination test, it is available for \code{kMax = 2} only.  
#' 
#' @export
#' 
#' @keywords internal
#' 
getFinalPValue <- function(design, stageResults, ...) {
	
	.assertIsTrialDesign(design)
	.assertIsStageResults(stageResults)
	
	if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
		return(.getFinalPValueInverseNormalOrGroupSequential(design = design, 
				stageResults = stageResults, ...))
	}
	
	if (.isTrialDesignFisher(design)) {
		return(.getFinalPValueFisher(design = design, stageResults = stageResults, ...))
	}
	
	.stopWithWrongDesignMessage(design)
}

.getVectorWithFinalValueAtFinalStage <- function(kMax, finalValue, finalStage) {
	v <- rep(NA_real_, kMax)
	if (is.null(finalValue) || is.na(finalValue) || 
		is.null(finalStage) || is.na(finalStage) ||
		finalStage < 1 || finalStage > kMax) {
		return(v)
	}
	
	v[finalStage] <- finalValue	
	return(v)
}

#' @title
#' Get Final Confidence Interval
#' 
#' @description  
#' Returns the final confidence interval for the parameter of interest. 
#' It is based on the prototype case, i.e., the test for testing a mean for 
#' normally distributed variables. 
#'
#' @param design The trial design.
#' @param dataInput The data input.
#' @param stage The stage number.
#' @param thetaH0 The null hypothesis value, default is 0 for the normal and the binary case, 
#'        it is 1 for the survival case.      
#'        For testing a rate in one sample, a value \code{thetaH0} in (0,1) has to be specified for 
#'        defining the null hypothesis H0: pi= thetaH0. \cr
#' 		  For non-inferiority designs, this is the non-inferiority bound. 	
#' @param directionUpper The direction of one-sided testing. 
#'        Default is \code{directionUpper = TRUE} which means that larger values of the 
#'        test statistics yield smaller p-values.
#' @param normalApproximation The type of computation of the p-values. Default is FALSE for 
#'        testing means (i.e., the t test is used) and TRUE for testing rates and the hazard ratio. 
#'        For testing rates, if \code{normalApproximation = FALSE} is specified, the binomial test 
#'        (one sample) or the test of Fisher (two samples) is used for calculating the p-values.
#'        In the survival setting \code{normalApproximation = FALSE} has no effect.    
#' @param equalVariances The type of t test. For testing means in two treatment groups, either 
#'        the t test assuming that the variances are equal or the t test without assuming this, 
#'        i.e., the test of Welch-Satterthwaite is calculated, default is \code{equalVariances = TRUE}.
#' 
#' @details 
#' Depending on \code{design} and \code{dataInput} the final confidence interval and median unbiased estimate 
#' that is based on the stagewise ordering of the sample space will be calculated and returned.
#' Additionally, a non-standardized ("general") version is provided, use the standard deviation to obtain 
#' the confidence interval for the parameter of interest.  
#' 
#' @return Returns a \code{list} containing 
#' \itemize{
#'   \item \code{finalStage}, 
#'   \item \code{medianUnbiased}, 
#'   \item \code{finalConfidenceInterval}, 
#'   \item \code{medianUnbiasedGeneral}, and 
#'   \item \code{finalConfidenceIntervalGeneral}.
#' }
#' 
#' @export
#' 
#' @examples
#' 
#' design <- getDesignInverseNormal(kMax = 2)
#' data <- getDataset(
#'     n = c(20, 30),
#'     means = c(50, 51),
#'     stDevs = c(130, 140)
#' )
#' getFinalConfidenceInterval(design, dataInput = data)
#'
#' # Results in:
#' #
#' # $finalStage
#' # [1] 2
#' # 
#' # $medianUnbiasedGeneral
#' # [1] 0.3546145
#' # 
#' # $finalConfidenceIntervalGeneral
#' # [1] 0.06967801 0.63468553
#' # 
#' # $medianUnbiased
#' # [1] 47.7787
#' # 
#' # $finalConfidenceInterval
#' # [1]  9.388012 85.513851'
#' 
#' @keywords internal
#'
getFinalConfidenceInterval <- function(design, dataInput, ...) {
	
	.assertIsTrialDesign(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = dataInput)
	.assertIsValidDataInput(dataInput = dataInput, design = design, stage = stage)
	.assertIsValidStage(stage, design$kMax)
	
	if (design$bindingFutility){
		warning("Two-sided final confidence bounds are not appropriate, ", 
			"use one-sided version (i.e., one bound) only.", call. = FALSE)
	}
	
	if (dataInput$isDatasetMeans()) {
		return(.getFinalConfidenceIntervalMeans(
				design = design, dataInput = dataInput, ...))
	}
	
	if (dataInput$isDatasetRates()) {
		return(.getFinalConfidenceIntervalRates(
				design = design, dataInput = dataInput, ...))
	}
	
	if (dataInput$isDatasetSurvival()) {
		return(.getFinalConfidenceIntervalSurvival(
				design = design, dataInput = dataInput, ...))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'dataInput' type '", class(dataInput), "' is not implemented yet")
}

#
# Get repeated p-values based on group sequential test
#
.getRepeatedPValuesGroupSequential <- function(..., design, stageResults, tolerance = NA_real_) {
	
	.assertIsTrialDesignInverseNormalOrGroupSequential(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getRepeatedPValuesGroupSequential", ignore = c("stage"), ...)
	
	if (is.na(tolerance)) {
		tolerance <- C_ANALYSIS_TOLERANCE_DEFAULT
	}
	
	repeatedPValues <- rep(NA_real_, design$kMax)
	
	if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP && stage == design$kMax) {
		
		if (!is.na(stageResults$overallPValues[design$kMax]) && 
			stats::qnorm(1 - stageResults$overallPValues[design$kMax]) == Inf) {
			repeatedPValues[design$kMax] <- tolerance
		} else {
			startTime <- Sys.time()
			lower <- .getDesignGroupSequential(kMax = design$kMax,  
				sided = design$sided, 
				informationRates = design$informationRates, 
				typeOfDesign = C_TYPE_OF_DESIGN_HP, 
				futilityBounds = design$futilityBounds,
				bindingFutility = design$bindingFutility)$alphaSpent[design$kMax - 1] + tolerance
			if (design$bindingFutility){
				upper <- min(0.5, 1 - stats::pnorm(max(design$futilityBounds)))
			} else {
				upper <- 0.5
			}	
			repeatedPValues[design$kMax] <- .getOneDimensionalRootBisectionMethod(
				f = function(level) {
					y <- .getDesignGroupSequential(kMax = design$kMax, alpha = level, 
						sided = design$sided, 
						informationRates = design$informationRates, 
						typeOfDesign = C_TYPE_OF_DESIGN_HP, 
						futilityBounds = design$futilityBounds,
						bindingFutility = design$bindingFutility)
					if (design$sided == 2) {
						return(y$criticalValues[design$kMax] - 
								abs(stats::qnorm(1 - stageResults$overallPValues[design$kMax])))
					}
					
					return(y$criticalValues[design$kMax] - 
							stats::qnorm(1 - stageResults$overallPValues[design$kMax]))
					
				}, lower = lower, upper = upper, 
				tolerance = tolerance, direction = -1,
				acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE
			)
		}
	} else {
		
		for (k in 1:stage) {		
			if (!is.na(stageResults$overallPValues[k]) && stats::qnorm(1 - stageResults$overallPValues[k]) == Inf) {
				repeatedPValues[k] <- tolerance
			} else {
				startTime <- Sys.time()
				if (design$bindingFutility){
					upper <- min(0.5, 1 - stats::pnorm(max(design$futilityBounds)))
				} else {
					upper <- 0.5
				}	
				repeatedPValues[k] <- .getOneDimensionalRootBisectionMethod(
					f = function(level) {
						y <- .getDesignGroupSequential(kMax = design$kMax, alpha = level, 
							sided = design$sided, 
							informationRates = design$informationRates, 
							typeOfDesign = design$typeOfDesign, 
							deltaWT = design$deltaWT, 
							gammaA = design$gammaA,
							futilityBounds = design$futilityBounds,
							bindingFutility = design$bindingFutility)
						if (design$sided == 2) {
							return(y$criticalValues[k] - abs(stats::qnorm(1 - stageResults$overallPValues[k])))
						}
						
						return(y$criticalValues[k] - stats::qnorm(1 - stageResults$overallPValues[k]))
						
					}, lower = tolerance, upper = upper, 
					tolerance = tolerance, direction = -1,
					acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE
				)
				.logProgress("Overall repeated p-values of stage %s calculated", startTime = startTime, k)
			}
		}
	}
	
	return(repeatedPValues)
	
}

#
# Get repeated p-values based on inverse normal method
#
.getRepeatedPValuesInverseNormal <- function(..., design, stageResults, 
	tolerance = NA_real_) {
	
	.assertIsTrialDesignInverseNormalOrGroupSequential(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getRepeatedPValuesInverseNormal", ignore = c("stage"), ...)
	
	if (is.na(tolerance)) {
		tolerance <- C_ANALYSIS_TOLERANCE_DEFAULT
	}
	
	repeatedPValues <- rep(NA_real_, design$kMax)
	
	if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP && stage == design$kMax) {
		if (!is.na(stageResults$combInverseNormal[design$kMax]) && 
			stageResults$combInverseNormal[design$kMax] == Inf) {
			repeatedPValues[design$kMax] <- tolerance
		} else {
			startTime <- Sys.time()
			lower <- .getDesignGroupSequential(kMax = design$kMax,  
				sided = design$sided, 
				informationRates = design$informationRates, 
				typeOfDesign = C_TYPE_OF_DESIGN_HP, 
				futilityBounds = design$futilityBounds,
				bindingFutility = design$bindingFutility)$alphaSpent[design$kMax - 1] + tolerance
			if (design$bindingFutility){
				upper <- min(0.5, 1 - stats::pnorm(max(design$futilityBounds)))
			} else {
				upper <- 0.5
			}	
			repeatedPValues[design$kMax] <- .getOneDimensionalRootBisectionMethod(
				f = function(level) {
					y <- .getDesignGroupSequential(kMax = design$kMax, 
						alpha = level, 
						sided = design$sided, 
						informationRates = design$informationRates, 
						typeOfDesign = C_TYPE_OF_DESIGN_HP, 
						futilityBounds = design$futilityBounds,
						bindingFutility = design$bindingFutility)
					if (design$sided == 2) {
						return(y$criticalValues[design$kMax] - 
								abs(stageResults$combInverseNormal[design$kMax]))
					}
					
					return(y$criticalValues[design$kMax] - stageResults$combInverseNormal[design$kMax])
					
				}, lower = lower, upper = upper, 
				tolerance = tolerance, direction = -1,
				acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE
			)
		}
	} else {
		
		for (k in 1:stage) {		
			
			if (!is.na(stageResults$combInverseNormal[k]) && (stageResults$combInverseNormal[k] == Inf)) {
				repeatedPValues[k] <- tolerance
			} else {
				startTime <- Sys.time()
				if (design$bindingFutility){
					upper <- min(0.5, 1 - stats::pnorm(max(design$futilityBounds)))
				} else {
					upper <- 0.5
				}	
				repeatedPValues[k] <- .getOneDimensionalRootBisectionMethod(
					f = function(level) {
						y <- .getDesignGroupSequential(kMax = design$kMax, 
							alpha = level, 
							sided = design$sided, 
							informationRates = design$informationRates, 
							typeOfDesign = design$typeOfDesign, 
							deltaWT = design$deltaWT, 
							gammaA = design$gammaA,
							futilityBounds = design$futilityBounds,
							bindingFutility = design$bindingFutility)
						if (design$sided == 2) {
							return(y$criticalValues[k] - abs(stageResults$combInverseNormal[k]))
						}
						
						return(y$criticalValues[k] - stageResults$combInverseNormal[k])
						
					}, lower = tolerance, upper = upper, 
					tolerance = tolerance, direction = -1,
					acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE
				)
				.logProgress("Overall repeated p-values of stage %s calculated", startTime = startTime, k)
			}
		}
	}
	
	return(repeatedPValues)
}

#
# Get repeated p-values based on Fisher combination test
#
.getRepeatedPValuesFisher <- function(..., design, stageResults, tolerance = NA_real_) {
	
	.assertIsTrialDesignFisher(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = ".getRepeatedPValuesFisher", ignore = c("stage"), ...)
	
	if (is.na(tolerance)) {
		tolerance <- C_ANALYSIS_TOLERANCE_DEFAULT
	}
	
	repeatedPValues <- rep(NA_real_, design$kMax)
	
	for (k in 1:stage) {
		if (!is.na(stageResults$combFisher[k]) && (stageResults$combFisher[k] == 0)) {
			repeatedPValues[k] <- tolerance
		} else {
			startTime <- Sys.time()
			repeatedPValues[k] <- .getOneDimensionalRootBisectionMethod(
				f = function(level) {
					y <- .getDesignFisher(kMax = design$kMax, 
						alpha = level,
						sided = design$sided,
						informationRates = design$informationRates,
						alpha0Vec = design$alpha0Vec,
						bindingFutility = design$bindingFutility, 
						method = design$method)
					if (design$sided == 2){
						combFisherNegStagek <- prod((1 - 
									stageResults$pValues[1:k])^stageResults$weightsFisher[1:k])
						return(y$criticalValues[k] - min(stageResults$combFisher[k],combFisherNegStagek))
					}	
					return(y$criticalValues[k] - stageResults$combFisher[k])
					
				},
				lower = tolerance, upper = 0.5, tolerance = tolerance, direction = 1,
				acceptResultsOutOfTolerance = TRUE, suppressWarnings = TRUE
			)
			.logProgress("Overall repeated p-values of stage %s calculated", startTime = startTime, k)
		}
	}
	
	return(repeatedPValues)
}

.getRejectValueConditionalPowerFisher <- function(kMax, alpha0Vec, 
	criticalValues, weightsFisher, pValues, currentKMax, thetaH1, stage, nPlanned) {
	
	pValues <- c(pValues[1:stage], 1 - stats::pnorm(stats::rnorm(kMax - stage, 
				thetaH1 * sqrt(nPlanned[(stage + 1):currentKMax]))))
	
	for (j in 1:currentKMax) {
		reject <- .getRejectValueFisherForOneStage(kMax = currentKMax, alpha0Vec, criticalValues, 
			weightsFisher, stage = j, pValues)
		if (reject >= 0) {
			return(reject)
		}
	}
	
	return(0)
}

.getRejectValueFisherForOneStage <- function(kMax, alpha0Vec, criticalValues, weightsFisher, stage, pValues) {
	
	if (stage < kMax && pValues[stage] >= alpha0Vec[stage]) {
		return(0)
	}
	
	p <- prod(pValues[1:stage]^weightsFisher[1:stage])
	if (is.na(p)) {
		stop("Calculation of 'p' failed for stage ", stage,
			" ('pValues' = ", .arrayToString(pValues), ", 'weightsFisher' = ", .arrayToString(weightsFisher), ")")
	}
	if (is.na(criticalValues[stage])) {
		stop("No critical value available for stage ", stage, 
			" ('criticalValues' = ", .arrayToString(criticalValues), ")")
	}
	
	if (p < criticalValues[stage]) {
		return(1)
	}
	return(-1)
}

.getRejectValueCrpFisher <- function(kMax, alpha0Vec, criticalValues, weightsFisher, k, stageResults) {
	
	pValues <- c(stageResults$pValues[1:k], stats::runif(kMax - k))
	
	for (stage in 1:kMax) {
		reject <- .getRejectValueFisherForOneStage(kMax = kMax, alpha0Vec = alpha0Vec, 
			criticalValues = criticalValues, weightsFisher = weightsFisher, stage = stage, 
			pValues = pValues)
		if (reject >= 0) {
			return(reject)
		}
	}
	
	return(0)
}


#
# Get CRP based on inverse normal or group sequential method
#
.getConditionalRejectionProbabilitiesInverseNormalorGroupSequential <- function(
	..., design, stageResults) {	
	
	.assertIsTrialDesignInverseNormalOrGroupSequential(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = 
			".getConditionalRejectionProbabilitiesInverseNormalorGroupSequential", ignore = c("stage"), ...)
	
	kMax <- design$kMax
	
	criticalValues <- design$criticalValues
	informationRates <- design$informationRates
	weights <- stageResults$weightsInverseNormal
	futilityBounds <- design$futilityBounds
	
	conditionalRejectionProbabilities <- rep(NA_real_, kMax)
	for (k in 1:min(kMax - 1, stage)) {
		
		if (.isTrialDesignInverseNormal(design)) {
			# Shifted decision region for use in getGroupSeqProbs
			shiftedDecision <- criticalValues[(k + 1):kMax] * sqrt(sum(weights[1:k]^2) + 
						cumsum(weights[(k + 1):kMax]^2)) / sqrt(cumsum(weights[(k + 1):kMax]^2)) - 
				as.vector(weights[1:k] %*% stats::qnorm(1 - stageResults$pValues[1:k])) / 	
				sqrt(cumsum(weights[(k + 1):kMax]^2))
			
			if (k == kMax - 1) {
				shiftedFutilityBounds <- c()
			} else {
				shiftedFutilityBounds <- futilityBounds[(k + 1):(kMax - 1)] * 
					sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):(kMax - 1)]^2)) / 
					sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2)) -
					as.vector(weights[1:k] %*% stats::qnorm(1 - stageResults$pValues[1:k])) /
					sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2))
			}
		} else {
			# Shifted decision region for use in getGroupSeqProbs
			shiftedDecision <- criticalValues[(k + 1):kMax] * 
				sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):kMax]^2)) / 
				sqrt(cumsum(weights[(k + 1):kMax]^2)) - 
				stats::qnorm(1 - stageResults$overallPValues[k]) * sqrt(sum(weights[1:k]^2)) /
				sqrt(cumsum(weights[(k + 1):kMax]^2))
			
			if (k == kMax - 1) {
				shiftedFutilityBounds <- c()
			} else {
				shiftedFutilityBounds <- futilityBounds[(k + 1):(kMax - 1)] * 
					sqrt(sum(weights[1:k]^2) + cumsum(weights[(k + 1):(kMax - 1)]^2)) / 
					sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2)) -
					stats::qnorm(1 - stageResults$overallPValues[k]) * sqrt(sum(weights[1:k]^2)) /
					sqrt(cumsum(weights[(k + 1):(kMax - 1)]^2))
			}
		}
		
		# Scaled information for use in getGroupSeqProbs
		scaledInformation <- (informationRates[(k + 1):kMax] - informationRates[k]) / 
			(1 - informationRates[k])
		
		if (design$sided == 2) {
			decisionMatrix <- matrix(c(-shiftedDecision, shiftedDecision), nrow = 2, byrow = TRUE)
			probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, informationRates = scaledInformation)
			crp <- sum(probs[3, ] - probs[2, ] + probs[1, ])
		} else {
			if (design$bindingFutility){
				decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, shiftedDecision), 
					nrow = 2, byrow = TRUE)
			} else {
				decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - k), shiftedDecision), 
					nrow = 2, byrow = TRUE)
			}
			probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
				informationRates = scaledInformation)
			crp <- sum(probs[3, ] - probs[2, ])
		}
		conditionalRejectionProbabilities[k] <- crp
	}
	
	if (design$bindingFutility){
		for (k in (1:min(kMax - 1, stage))){
			if (.isTrialDesignInverseNormal(design)) {
				if (stageResults$combInverseNormal[k] <= futilityBounds[k]) conditionalRejectionProbabilities[k:stage] <- 0
			} else {
				if (stats::qnorm(1 - stageResults$overallPValues[k]) <= futilityBounds[k]) conditionalRejectionProbabilities[k:stage] <- 0
			}
		}	
	}
	
	return(conditionalRejectionProbabilities)	
}

#
# Get CRP based on Fisher combination test
#
.getConditionalRejectionProbabilitiesFisher <- function(..., design, stageResults) {
	
	.assertIsTrialDesignFisher(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.warnInCaseOfUnknownArguments(functionName = 
			".getConditionalRejectionProbabilitiesFisher", ignore = c("stage"), ...)
	
	kMax <- design$kMax
	if (kMax == 1) {
		return(NA_real_)
	}
	
	criticalValues <- design$criticalValues
	weights <- stageResults$weightsFisher	
	if (design$bindingFutility){
		alpha0Vec <- design$alpha0Vec
	} else {
		alpha0Vec <- rep(1, kMax - 1)
	}
	
	conditionalRejectionProbabilities <- rep(NA_real_, kMax)
	for (k in (1:min(kMax - 1, stage))) {		
		if (prod(stageResults$pValues[1:k]^weights[1:k]) <= criticalValues[k]){
			conditionalRejectionProbabilities[k] <- 1
		} else {
			if (k < kMax - 1){
				conditionalRejectionProbabilities[k] <- .getFisherCombinationSize(kMax - k, 
					alpha0Vec[(k + 1):(kMax - 1)], (criticalValues[(k + 1):kMax] / 
							prod(stageResults$pValues[1:k]^weights[1:k]))^(1 / weights[k + 1]), 
					weights[(k + 2):kMax] / weights[k + 1])
			} else {
				conditionalRejectionProbabilities[k] <- (criticalValues[kMax]/ 
						prod(stageResults$pValues[1:k]^weights[1:k]))^(1 / weights[kMax])
			}
		}
	}
	
	if (design$bindingFutility){
		for (k in (1:min(kMax - 1, stage))){
			if (stageResults$pValues[k] > alpha0Vec[k]) conditionalRejectionProbabilities[k:stage] <- 0
		}	
	}
	
	conditionalRejectionProbabilities[conditionalRejectionProbabilities >= 1] <- 1
	
	conditionalRejectionProbabilities[conditionalRejectionProbabilities < 0] <- NA_real_
	
	return(conditionalRejectionProbabilities)	
}

#
# Get CRP based on Fisher combination test, tested through simulation
#
.getConditionalRejectionProbabilitiesFisherSimulated <- function(
	..., design, stageResults, iterations = 0, seed = NA_real_) {
	
	.assertIsTrialDesignFisher(design)
	stage <- .getStageFromOptionalArguments(..., dataInput = stageResults$getDataInput())
	.assertIsValidStage(stage, design$kMax)
	.assertIsValidIterationsAndSeed(iterations, seed)
	.warnInCaseOfUnknownArguments(functionName = 
			".getConditionalRejectionProbabilitiesFisherSimulated", ignore = c("stage"), ...)
	
	kMax <- design$kMax
	criticalValues <- design$criticalValues
	alpha0Vec <- design$alpha0Vec
	weightsFisher <- stageResults$weightsFisher	
	
	crpFisherSimulated <- rep(NA_real_, kMax)
	if (iterations > 0) {
		seed = .setSeed(seed)	
		if (kMax >= 2) {
			for (k in 1:min(kMax - 1, stage)) {
				reject <- 0
				for (i in 1:iterations) {
					reject <- reject + .getRejectValueCrpFisher(kMax = kMax, 
						alpha0Vec = alpha0Vec, criticalValues = criticalValues, 
						weightsFisher = weightsFisher, k = k, stageResults = stageResults)
				}
				crpFisherSimulated[k] <- reject / iterations
			}
		} else {
			warning("Simulation of CRP Fisher stopped: 'kMax' must be >= 2", call. = FALSE)
		}
	}
	
	return(list(
			crpFisherSimulated = crpFisherSimulated,
			iterations = iterations,
			seed = seed
		))	
}

#' 
#' @title
#' Get Conditional Rejection Probabilities
#' 
#' @description
#' Calculates the conditional rejection probabilities (CRP) for given test results.
#' 
#' @param design The trial design.
#' @param stageResults The results at given stage, obtained from \code{\link{getStageResults}}.
#' @param stage The stage number (optional). Default: total number of existing stages in the data input.
#' 
#' @details
#' The conditional rejection probability is the probability, under H0, to reject H0 
#' in one of the subsequent (remaining) stages.
#' The probability is calculated using the specified design. For testing rates and the 
#' survival design, the normal approximation is used, i.e., it is calculated with the 
#' use of the prototype case testing a mean for normally distributed data with known variance.
#' 
#' The conditional rejection probabilities are provided up to the specified stage.
#' 
#' For Fisher's combination test, you can check the validity of the CRP calculation via simulation.   
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @examples
#' 
#' x <- getDesignFisher(kMax = 3, informationRates = c(0.1,0.8,1))
#' y <- getDataset(n = c(40,40), events = c(20,22))
#' getConditionalRejectionProbabilities(x, getStageResults(x, y, thetaH0 = 0.4))
#' # provides 
#' # [1] 0.0216417 0.1068607        NA
#' 
getConditionalRejectionProbabilities <- function(design, stageResults, ...) {
	
	.assertIsTrialDesign(design)
	.assertIsStageResults(stageResults)
	
	if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
		return(.getConditionalRejectionProbabilitiesInverseNormalorGroupSequential(
				design = design, stageResults = stageResults, ...))
	}
	
	if (.isTrialDesignFisher(design)) {
		iterations <- .getOptionalArgument("iterations", ...)
		if (!is.null(iterations) && iterations > 0) {
			return(.getConditionalRejectionProbabilitiesFisherSimulated(
					design = design, stageResults = stageResults, ...))
		}
		
		return(.getConditionalRejectionProbabilitiesFisher(
				design = design, stageResults = stageResults, ...))
	}
	
	.stopWithWrongDesignMessage(design)
}

.getDecisionMatrixRoot <- function(design, stage, stageResults, tolerance, firstParameterName, case) {
	
	firstValue <- stageResults[[firstParameterName]][stage]
	if (.isTrialDesignGroupSequential(design)) {
		firstValue <- stats::qnorm(1 - firstValue)
	}
	
	if (firstValue >= 8){
		return(NA_real_)
	}  
	
	result <- .getOneDimensionalRoot(
		function(theta) {
			
			if (design$bindingFutility){
				row1part1 <- design$futilityBounds[1:(stage - 1)]
			} else {
				row1part1 <- rep(C_FUTILITY_BOUNDS_DEFAULT, stage - 1)
			} 	
			row1part2 <- C_FUTILITY_BOUNDS_DEFAULT
			row2part1 <- design$criticalValues[1:(stage - 1)]
			row2part2 <- firstValue
			
			if (.isTrialDesignGroupSequential(design)){
				
				if (stageResults$isDatasetSurvival()) {
					row1part3 <- theta * sqrt(design$informationRates[1:stage] / 
								design$informationRates[stage]) *
						sqrt(stageResults$overallEvents[stage])
				} else {	
					
					if (stageResults$isOneSampleDataset()){
						row1part3 <- theta * sqrt(design$informationRates[1:stage] / 
									design$informationRates[stage]) *
							sqrt(stageResults$overallSampleSizes[stage])
					}
					
					if (stageResults$isTwoSampleDataset()){
						row1part3 <- theta * sqrt(design$informationRates[1:stage] / 
									design$informationRates[stage]) /
							sqrt(1/stageResults$overallSampleSizes1[stage] + 1 / 
									stageResults$overallSampleSizes2[stage])		
					}
				}	
			}
			
			if (.isTrialDesignInverseNormal(design)){
				
				if (stageResults$isDatasetSurvival()) {
					events <- stageResults$getDataInput()$getEventsUpTo(stage)
					adjInfRate <- cumsum(stageResults$weightsInverseNormal[1:stage] * sqrt(events[1:stage])) /
						sqrt(cumsum(stageResults$weightsInverseNormal[1:stage]^2))
					
				} else {	
					
					if (stageResults$isOneSampleDataset()){
						sampleSizes <- stageResults$getDataInput()$getSampleSizesUpTo(stage)
						adjInfRate <- cumsum(stageResults$weightsInverseNormal[1:stage] * 
									sqrt(sampleSizes[1:stage])) /
							sqrt(cumsum(stageResults$weightsInverseNormal[1:stage]^2))
					}
					
					if (stageResults$isTwoSampleDataset()){
						sampleSizes1 <- stageResults$getDataInput()$getSampleSizesUpTo(stage, 1)
						sampleSizes2 <- stageResults$getDataInput()$getSampleSizesUpTo(stage, 2)
						adjInfRate <- cumsum(stageResults$weightsInverseNormal[1:stage] / 
									sqrt(1 / sampleSizes1[1:stage] + 1 / sampleSizes2[1:stage])) /
							sqrt(cumsum(stageResults$weightsInverseNormal[1:stage]^2)) 
					}
				}	
				row1part3 <- theta * adjInfRate  
			}
			row2part3 <- row1part3
			
			row1 <- c(row1part1, row1part2) - row1part3
			row2 <- c(row2part1, row2part2) - row2part3
			
			decisionMatrix <- matrix(c(row1, row2), nrow = 2, byrow = TRUE)
			
			probs <- .getGroupSequentialProbabilities(decisionMatrix = decisionMatrix, 
				informationRates = design$informationRates[1:stage])
			
			if (case == "finalConfidenceIntervalGeneralLower") {
				return(sum(probs[3, ] - probs[2, ]) - design$alpha / design$sided)
			}
			else if (case == "finalConfidenceIntervalGeneralUpper") {
				return(1 - sum(probs[3, ] - probs[2, ]) - design$alpha / design$sided)
			}
			else if (case == "medianUnbiasedGeneral") {
				return(sum(probs[3, ] - probs[2, ]) - 0.50)
			}
			else {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'case' = '", case, "' is not implemented")
			}
		}, 
		lower = -8, 
		upper = 8, 
		tolerance = tolerance
	)
}


