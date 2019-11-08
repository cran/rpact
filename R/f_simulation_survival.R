######################################################################################
#                                                                                    #
# -- Simulation of survival data with group sequential and combination test --       #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.1                                                                #
# Date: 11-12-2018                                                                   #
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

#' @include class_simulation_results.R
NULL

#' @title
#' Get Simulation Survival 
#' 
#' @description 
#' Returns the analysis times, power, stopping probabilities, conditional power, and expected sample size 
#' for testing the hazard ratio in a two treatment groups survival design. 
#'
#' @param design The trial design. If no trial design is specified, a fixed sample size design is used. 
#' 		  In this case, \code{alpha}, \code{beta}, \code{twoSidedPower}, 
#'        and \code{sided} can be directly entered as argument.  
#' @param thetaH0 The null hypothesis value. The default value is \code{1}. For one-sided testing, 
#'        a bound for testing H0: hazard ratio = thetaH0 != 1 can be specified.
#' @param directionUpper Specifies the direction of the alternative, only applicable 
#'        for one-sided testing, default is \code{TRUE}.
#' @param pi1 The assumed event rate in the treatment group, default is \code{seq(0.2,0.5,0.1)}.
#' @param pi2 The assumed event rate in the control group, default is 0.2.
#' @param lambda1 The assumed hazard rate in the treatment group, there is no default.
#'        lambda1 can also be used to define piecewise exponentially distributed survival times 
#'        (see details). 	 
#' @param lambda2 The assumed hazard rate in the reference group, there is no default.
#'  	  lambda2 can also be used to define piecewise exponentially distributed survival times 
#'        (see details).
#' @param median1 The assumed median survival time in the treatment group, there is no default.
#' @param median2 The assumed median survival time in the reference group, there is no default.
#' @param hazardRatio The vector of hazard ratios under consideration. 
#'        If the event or hazard rates in both treatment groups are defined, the hazard ratio needs 
#'        not to be specified as it is calculated. 
#' @param piecewiseSurvivalTime A vector that specifies the time intervals for the piecewise 
#'        definition of the exponential survival time cumulative distribution function (see details). 
#' @param kappa The scale parameter of the Weibull distribution, default is \code{1}. 
#'        The Weibull distribution cannot be used for the piecewise
#' 		  definition of the survival time distribution.    
#'        Note that the parameters \code{shape} and \code{scale} in \code{\link[stats]{Weibull}} 
#'        are equivalent to \code{kappa} and \code{1 / lambda}, respectively, in rpact.
#' @param allocation1 The number how many subjects are assigned to treatment 1 in a 
#'        subsequent order, default is \code{1}  
#' @param allocation2 The number how many subjects are assigned to treatment 2 in a 
#'        subsequent order, default is \code{1}
#' @param eventTime The assumed time under which the event rates are calculated, default is \code{12}. 
#' @param accrualTime The assumed accrual time for the study, default is \code{12} 
#'        (see \code{\link{getAccrualTime}}).
#' @param accrualIntensity A vector of accrual intensities, default is the relative 
#'        intensity \code{0.1} (see \code{\link{getAccrualTime}}).
#' @param dropoutRate1 The assumed drop-out rate in the treatment group, default is \code{0}. 
#' @param dropoutRate2 The assumed drop-out rate in the control group, default is \code{0}.
#' @param dropoutTime The assumed time for drop-out rates in the control and the 
#'        treatment group, default is \code{12}. 
#' @param maxNumberOfSubjects \code{maxNumberOfSubjects > 0} needs to be specified.
#' 		  If accrual time and accrual intensity is specified, this will be calculated.
#' @param plannedEvents \code{plannedEvents} is a vector of length kMax 
#'        (the number of stages of the design) with increasing numbers
#' 	      that determines the number of cumulated (overall) events when the interim stages are planned.
#' @param minNumberOfEventsPerStage When performing a data driven sample size recalculation, 
#' 		  the vector with length kMax \code{minNumberOfEventsPerStage} determines the 
#'        minimum number of events per stage (i.e., not cumulated), the first element 
#'        is not taken into account.   
#' @param maxNumberOfEventsPerStage When performing a data driven sample size recalculation, 
#' 	      the vector with length kMax \code{maxNumberOfEventsPerStage} determines the maximum number 
#'        of events per stage (i.e., not cumulated), the first element is not taken into account.
#' @param conditionalPower The conditional power for the subsequent stage under which the sample size recalculation is performed.
#' @param thetaH1 If specified, the value of the hazard ratio under which the conditional power calculation is performed.  
#' @param maxNumberOfIterations The number of simulation iterations.
#' @param maxNumberOfRawDatasetsPerStage The number of raw datasets per stage that shall 
#'        be extracted and saved as \code{\link[base]{data.frame}}, default is \code{0}. 
#'        \code{\link{getRawData}} can be used to get the extracted raw data from the object. 
#' @param longTimeSimulationAllowed Logical that indicates whether long time simulations
#'        that consumes more than 30 seconds are allowed or not, default is \code{FALSE}.
#' @param seed The seed to reproduce the simulation, default is a random seed.
#' @param ... Ensures that all arguments are be named and 
#'        that a warning will be displayed if unknown arguments are passed.
#' 
#' @details 
#' At given design the function simulates the power, stopping probabilities, conditional power, and expected 
#' sample size at given number of events, number of subjects, and parameter configuration. 
#' It also simulates the time when the required events are expected under the given 
#' assumptions (exponentially, piecewise exponentially, or Weibull distributed survival times 
#' and constant or non-constant piecewise accrual). 
#' Additionally, integers \code{allocation1} and \code{allocation2} can be specified that determine the number allocated
#' to treatment group 1 and treatment group 2, respectively. 
#'  
#' The formula of Kim & Tsiatis (Biometrics, 1990) 
#' is used to calculated the expected number of events under the alternative 
#' (see also Lakatos & Lan, Statistics in Medicine, 1992). These formulas are generalized 
#' to piecewise survival times and non-constant piecewise accrual over time.\cr
#' 
#' \code{piecewiseSurvivalTime} 
#' The first element of this vector must be equal to \code{0}. \code{piecewiseSurvivalTime} can also 
#' be a list that combines the definition of the time intervals and hazard rates in the reference group. 
#' The definition of the survival time in the treatment group is obtained by the specification 
#' of the hazard ratio (see examples for details).
#' 
#' Note that \code{numberOfSubjects}, \code{numberOfSubjects1}, and \code{numberOfSubjects2} in the output
#' are expected number of subjects.
#' 
#' @section Simulation Data:
#' The summary statistics "Simulated data" contains the following 
#' parameters: median [range]; mean +/-sd\cr
#' 
#' \code{$show(showStatistics = FALSE)} or \code{$setShowStatistics(FALSE)} can be used to disable 
#' the output of the aggregated simulated data.\cr
#' 
#' Example 1: \cr
#' \code{simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 100, plannedEvents = 30)} \cr
#' \code{simulationResults$show(showStatistics = FALSE)}\cr
#' 
#' Example 2: \cr
#' \code{simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 100, plannedEvents = 30)} \cr
#' \code{simulationResults$setShowStatistics(FALSE)}\cr
#' \code{simulationResults}\cr
#' 
#' \code{\link{getData}} can be used to get the aggregated simulated data from the 
#' object as \code{\link[base]{data.frame}}. The data frame contains the following columns:
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stageNumber}: The stage.
#'   \item \code{pi1}: The assumed or derived event rate in the treatment group.
#'   \item \code{pi2}: The assumed or derived event rate in the control group.
#'   \item \code{hazardRatio}: The hazard ratio under consideration (if available).
#'   \item \code{analysisTime}: The analysis time.
#'   \item \code{numberOfSubjects}: The number of subjects under consideration when the 
#'         (interim) analysis takes place.
#'   \item \code{eventsPerStage1}: The observed number of events per stage 
#'         in treatment group 1.
#'   \item \code{eventsPerStage2}: The observed number of events per stage 
#'         in treatment group 2.
#'   \item \code{eventsPerStage}: The observed number of events per stage 
#'         in both treatment groups.
#'   \item \code{rejectPerStage}: 1 if null hypothesis can be rejected, 0 otherwise. 
#'   \item \code{futilityPerStage}: 1 if study should be stopped for futility, 0 otherwise.
#'   \item \code{eventsNotAchieved}: 1 if number of events could not be reached with 
#'         observed number of subjects, 0 otherwise.
#'   \item \code{testStatistic}: The test statistic that is used for the test decision, 
#'         depends on which design was chosen (group sequential, inverse normal, 
#'         or Fisher combination test)'  
#'   \item \code{logRankStatistic}: Z-score statistic which corresponds to a one-sided 
#'         log-rank test at considered stage. 
#'   \item \code{hazardRatioEstimateLR}: The estimated hazard ratio, derived from the 
#'         log-rank statistic.
#'   \item \code{trialStop}: \code{TRUE} if study should be stopped for efficacy or futility or final stage, \code{FALSE} otherwise.  
#'   \item \code{conditionalPowerAchieved}: The conditional power for the subsequent stage of the trial for 
#' 			selected sample size and effect. The effect is either estimated from the data or can be
#' 			user defined with \code{thetaH1}.   

#' }
#' 
#' @section Raw Data:
#' \code{\link{getRawData}} can be used to get the simulated raw data from the 
#' object as \code{\link[base]{data.frame}}. Note that \code{getSimulationSurvival} 
#' must called before with \code{maxNumberOfRawDatasetsPerStage} > 0.
#' The data frame contains the following columns: 
#' \enumerate{
#'   \item \code{iterationNumber}: The number of the simulation iteration.
#'   \item \code{stopStage}: The stage of stopping.
#'   \item \code{subjectId}: The subject id (increasing number 1, 2, 3, ...)
#'   \item \code{accrualTime}: The accrual time, i.e., the time when the subject entered the trial.
#'   \item \code{treatmentGroup}: The treatment group number (1 or 2).
#'   \item \code{survivalTime}: The survival time of the subject.
#'   \item \code{dropoutTime}: The dropout time of the subject (may be \code{NA}).
#'   \item \code{observationTime}: The specific observation time.
#'   \item \code{timeUnderObservation}: The time under observation is defined as follows:\cr
#'         if (event == TRUE) {\cr
#'             timeUnderObservation <- survivalTime;\cr
#'         } else if (dropoutEvent == TRUE) {\cr
#'             timeUnderObservation <- dropoutTime;\cr
#'         } else {\cr
#'             timeUnderObservation <- observationTime - accrualTime;\cr
#'         }
#'   \item \code{event}: \code{TRUE} if an event occurred; \code{FALSE} otherwise.
#'   \item \code{dropoutEvent}: \code{TRUE} if an dropout event occurred; \code{FALSE} otherwise. 
#' }
#' 
#' @return Returns a \code{\link{SimulationResultsSurvival}} object.
#' 
#' @export 
#' 
#' @examples
#' 
#' # Fixed sample size with minimum required definitions, pi1 = (0.3,0.4,0.5,0.6) and 
#' # pi2 = 0.3 at event time 12, and accrual time 24 
#' getSimulationSurvival(pi1 = seq(0.3,0.6,0.1), pi2 = 0.3, eventTime = 12, 
#'     accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' 
#' \donttest{
#' 
#' # Increase number of simulation iterations 
#' getSimulationSurvival(pi1 = seq(0.3,0.6,0.1), pi2 = 0.3, eventTime = 12, 
#'     accrualTime = 24, plannedEvents = 40, maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' 
#' # Determine necessary accrual time with default settings if 200 subjects and 
#' # 30 subjects per time unit can be recruited 
#' getSimulationSurvival(plannedEvents = 40, accrualTime = 0, 
#'     accrualIntensity = 30, maxNumberOfSubjects = 200, maxNumberOfIterations = 50)
#' 
#' # Determine necessary accrual time with default settings if 200 subjects and 
#' # if the first 6 time units 20 subjects per time unit can be recruited, 
#' # then 30 subjects per time unit 
#' getSimulationSurvival(plannedEvents = 40, accrualTime = c(0, 6), 
#'     accrualIntensity = c(20, 30), maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' 
#' # Determine maximum number of Subjects with default settings if the first 
#' # 6 time units 20 subjects per time unit can be recruited, and after 
#' # 10 time units 30 subjects per time unit
#' getSimulationSurvival(plannedEvents = 40, accrualTime = c(0, 6, 10), 
#'     accrualIntensity = c(20, 30), maxNumberOfIterations = 50)
#' 
#' # Specify accrual time as a list
#' at <- list(
#' 	   "0 - <6"  = 20,
#' 	   "6 - Inf" = 30)
#' getSimulationSurvival(plannedEvents = 40, accrualTime = at, 
#'     maxNumberOfSubjects = 200, maxNumberOfIterations = 50)
#' 
#' # Specify accrual time as a list, if maximum number of subjects need to be calculated
#' at <- list(
#' 	   "0 - <6"   = 20,
#' 	   "6 - <=10" = 30)
#' getSimulationSurvival(plannedEvents = 40, accrualTime = at, maxNumberOfIterations = 50)
#' 
#' # Specify effect size for a two-stage group sequential design with O'Brien & Fleming boundaries.
#' # Effect size is based on event rates at specified event time, directionUpper = FALSE 
#' # needs to be specified because it should be shown that hazard ratio < 1
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#' 	   pi1 = 0.2, pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), 
#' 	   maxNumberOfSubjects = 200, directionUpper = FALSE, maxNumberOfIterations = 50)
#' 
#' # As above, but with a three-stage O'Brien and Flemming design with 
#' # specified information rates, note that planned events consists of integer values
#' d3 <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
#' getSimulationSurvival(design = d3, pi1 = 0.2, pi2 = 0.3, eventTime = 24, 
#' 	   plannedEvents = round(d3$informationRates * 40), 
#' 	   maxNumberOfSubjects = 200, directionUpper = FALSE, 
#'     maxNumberOfIterations = 50)
#' 
#' # Effect size is based on event rate at specified event time for the reference group and 
#' # hazard ratio, directionUpper = FALSE needs to be specified because it should be shown 
#' # that hazard ratio < 1
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, 
#' 	   pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
#' 	   directionUpper = FALSE, maxNumberOfIterations = 50)
#' 
#' # Effect size is based on hazard rate for the reference group and 
#' # hazard ratio, directionUpper = FALSE needs to be specified because 
#' # it should be shown that hazard ratio < 1
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     hazardRatio = 0.5, lambda2 = 0.02, plannedEvents = c(20, 40), 
#'     maxNumberOfSubjects = 200, directionUpper = FALSE, 
#'     maxNumberOfIterations = 50) 
#' 
#' # Specification of piecewise exponential survival time and hazard ratios, 
#' # note that in getSimulationSurvival only on hazard ratio is used
#' # in the case that the survival time is piecewise expoential
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#' 	   piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#' 	   hazardRatio = 1.5, plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' 
#' pws <- list(
#'     "0 - <5"  = 0.01,	
#' 	   "5 - <10" = 0.02,	
#' 	   ">=10"    = 0.04)
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#' 	   piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), 
#' 	   plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' 
#' # Specification of piecewise exponential survival time for both treatment arms  
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#' 	   piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#' 	   lambda1 = c(0.015, 0.03, 0.06), plannedEvents = c(20, 40), 
#' 	   maxNumberOfSubjects = 200, maxNumberOfIterations = 50)
#' 
#' # Specification of piecewise exponential survival time as a list, 
#' # note that in getSimulationSurvival only on hazard ratio 
#' # (not a vector) can be used
#' pws <- list(
#'     "0 - <5"  = 0.01,
#' 	   "5 - <10" = 0.02,
#' 	   ">=10"    = 0.04)
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#' 	   piecewiseSurvivalTime = pws, hazardRatio = 1.5, 
#' 	   plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
#'     maxNumberOfIterations = 50)
#' 
#' # Specification of piecewise exponential survival time and delayed effect 
#' # (response after 5 time units)  
#' getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
#'     piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
#'     lambda1 = c(0.01, 0.02, 0.06), plannedEvents = c(20, 40), 
#'     maxNumberOfSubjects = 200, maxNumberOfIterations = 50)
#' 
#' # Specify effect size based on median survival times
#' getSimulationSurvival(median1 = 5, median2 = 3, plannedEvents = 40, 
#' 	   maxNumberOfSubjects = 200, directionUpper = FALSE, 
#'     maxNumberOfIterations = 50)
#' 
#' # Specify effect size based on median survival 
#' # times of Weibull distribtion with kappa = 2
#' getSimulationSurvival(median1 = 5, median2 = 3, kappa = 2, 
#' 	   plannedEvents = 40, maxNumberOfSubjects = 200, 
#'     directionUpper = FALSE, maxNumberOfIterations = 50)
#' 
#' # Perform recalculation of number of events based on conditional power for a 
#' # three-stage design with inverse normal combination test, where the conditional power 
#' # is calculated under the specified effect size thetaH1 = 1.3 and up to a four-fold 
#' # increase in originally planned sample size (number of events) is allowed
#' # Note that the first value in minNumberOfEventsPerStage and 
#' # maxNumberOfEventsPerStage is arbitrary, i.e., it has no effect.
#' dIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))
#' 
#' resultsWithSSR1 <- getSimulationSurvival(design = dIN, 
#'     hazardRatio = seq(1, 1.6, 0.1), 
#'     pi2 = 0.3, conditionalPower = 0.8, thetaH1 = 1.3, 
#'     plannedEvents = c(58, 102, 146), 
#' 	   minNumberOfEventsPerStage = c(58, 44, 44), 
#'     maxNumberOfEventsPerStage = 4 * c(58, 44, 44),
#' 	   maxNumberOfSubjects = 800, maxNumberOfIterations = 50)
#' resultsWithSSR1
#' 
#' # If thetaH1 is unspecified, the observed hazard ratio estimate 
#' # (calculated from the log-rank statistic) is used for performing the 
#' # recalculation of the number of events
#' resultsWithSSR2 <- getSimulationSurvival(design = dIN, 
#'     hazardRatio = seq(1, 1.6, 0.1), 
#' 	   pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
#' 	   minNumberOfEventsPerStage = c(58, 44, 44), 
#'     maxNumberOfEventsPerStage = 4 * c(58, 44, 44),
#' 	   maxNumberOfSubjects = 800, maxNumberOfIterations = 50)
#' resultsWithSSR2
#' 
#' # Compare it with design without event size recalculation
#' resultsWithoutSSR <- getSimulationSurvival(design = dIN, 
#'     hazardRatio = seq(1, 1.6, 0.1), pi2 = 0.3, 
#'     plannedEvents = c(58, 102, 145), maxNumberOfSubjects = 800, 
#' 	   maxNumberOfIterations = 50)
#' resultsWithoutSSR$overallReject
#' resultsWithSSR1$overallReject
#' resultsWithSSR2$overallReject
#' 
#' # Confirm that event size racalcuation increases the Type I error rate, 
#' # i.e., you have to use the combination test 
#' dGS <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
#' resultsWithSSRGS <- getSimulationSurvival(design = dGS, hazardRatio = seq(1), 
#' 	   pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 145), 
#' 	   minNumberOfEventsPerStage = c(58, 44, 44), 
#'     maxNumberOfEventsPerStage = 4 * c(58, 44, 44),
#' 	   maxNumberOfSubjects = 800, maxNumberOfIterations = 50)
#' resultsWithSSRGS$overallReject
#' 
#' # Set seed to get reproduceable results
#' 
#' identical(
#' 	   getSimulationSurvival(plannedEvents = 40, maxNumberOfSubjects = 200, 
#'         seed = 99)$analysisTime,
#' 	   getSimulationSurvival(plannedEvents = 40, maxNumberOfSubjects = 200, 
#'         seed = 99)$analysisTime
#' )
#' 
#' }
#' 
getSimulationSurvival <- function(design = NULL, ...,
		thetaH0 = C_THETA_H0_SURVIVAL_DEFAULT,
		directionUpper = C_DIRECTION_UPPER_DEFAULT,	
		pi1 = NA_real_,
		pi2 = NA_real_,
		lambda1 = NA_real_, 
		lambda2 = NA_real_,
		median1 = NA_real_,	
		median2 = NA_real_, 
		hazardRatio = NA_real_,
		kappa = 1,	
		piecewiseSurvivalTime = NA_real_,
		allocation1 = C_ALLOCATION_1_DEFAULT,
		allocation2 = C_ALLOCATION_2_DEFAULT,
		eventTime = C_EVENT_TIME_DEFAULT,	
		accrualTime = C_ACCRUAL_TIME_DEFAULT,
		accrualIntensity = C_ACCRUAL_INTENSITY_DEFAULT,
		dropoutRate1 = C_DROP_OUT_RATE_1_DEFAULT, 
		dropoutRate2 = C_DROP_OUT_RATE_2_DEFAULT, 
		dropoutTime = C_DROP_OUT_TIME_DEFAULT,
		maxNumberOfSubjects = NA_real_,		
		plannedEvents = NA_real_, 
		minNumberOfEventsPerStage = NA_real_,
		maxNumberOfEventsPerStage = NA_real_, 
		conditionalPower = NA_real_, 
		thetaH1 = NA_real_,
		maxNumberOfIterations = C_MAX_SIMULATION_ITERATIONS_DEFAULT, 
		maxNumberOfRawDatasetsPerStage = 0,
		longTimeSimulationAllowed = FALSE,
		seed = NA_real_) {
		
	.assertRcppIsInstalled()
	
	if (is.null(design)) {
		design <- .getDefaultDesignForSampleSizeCalculations(...)
		.warnInCaseOfUnknownArguments(functionName = "getSimulationSurvival", 
			ignore = c("alpha", "beta", "sided", "twoSidedPower"), ...)
	} else {
		.assertIsTrialDesign(design)
		.warnInCaseOfUnknownArguments(functionName = "getSimulationSurvival", ...)
		.warnInCaseOfTwoSidedPowerArgument(...)
	}
	
	.assertIsSingleLogical(directionUpper, "directionUpper")
	.assertIsSingleNumber(thetaH0, "thetaH0")
	.assertIsInOpenInterval(thetaH0, "thetaH0", 0, NULL, naAllowed = TRUE)
	.assertIsNumericVector(minNumberOfEventsPerStage, 
		"minNumberOfEventsPerStage", naAllowed = TRUE)
	.assertIsNumericVector(maxNumberOfEventsPerStage, 
		"maxNumberOfEventsPerStage", naAllowed = TRUE)
	.assertIsSingleNumber(conditionalPower, "conditionalPower", naAllowed = TRUE)
	.assertIsInOpenInterval(conditionalPower, "conditionalPower", 0, 1, naAllowed = TRUE)
	.assertIsSingleNumber(thetaH1, "thetaH1", naAllowed = TRUE)
	.assertIsInOpenInterval(thetaH1, "thetaH1", 0, NULL, naAllowed = TRUE)
	.assertIsSinglePositiveInteger(maxNumberOfIterations, "maxNumberOfIterations", validateType = FALSE)
	.assertIsSingleNumber(seed, "seed", naAllowed = TRUE)
	.assertIsNumericVector(lambda1, "lambda1", naAllowed = TRUE)
	.assertIsNumericVector(lambda2, "lambda2", naAllowed = TRUE)
	.assertIsSinglePositiveInteger(allocation1, "allocation1", validateType = FALSE)
	.assertIsSinglePositiveInteger(allocation2, "allocation2", validateType = FALSE)
	if (!is.na(thetaH1) && is.na(conditionalPower)) {
		warning("'thetaH1' (", thetaH1, ") ",
			"will be ignored because no 'conditionalPower' is defined", call. = FALSE)
	}
	
	if (design$sided == 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"Only one-sided case is implemented for the survival simulation design")
	}
	
	if (!all(is.na(lambda2)) && !all(is.na(lambda1)) && length(lambda2) != length(lambda1)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "length of 'lambda2' (", length(lambda2), 
			") must be equal to length of 'lambda1' (", length(lambda1), ")")
	}
	
	if (all(is.na(lambda2)) && !all(is.na(lambda1))) {
		warning("'lambda1' (", .arrayToString(lambda1), ") will be ignored ", 
			"because 'lambda2' (", .arrayToString(lambda2), ") is undefined", call. = FALSE)
		lambda1 <- NA_real_
	}
	
	if (!all(is.na(lambda2)) && is.list(piecewiseSurvivalTime)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'piecewiseSurvivalTime' needs to be a numeric vector and not a list ", 
			"because 'lambda2' (", .arrayToString(lambda2), ") is defined separately")
	}
	
	simulationResults <- SimulationResultsSurvival(design)
	
	if (is.na(conditionalPower)) {
		if (length(minNumberOfEventsPerStage) != 1 || 
				!is.na(minNumberOfEventsPerStage)) {
			warning("'minNumberOfEventsPerStage' (", 
				.arrayToString(minNumberOfEventsPerStage), ") ",
				"will be ignored because no 'conditionalPower' is defined", call. = FALSE)
		}
		if (length(maxNumberOfEventsPerStage) != 1 || 
				!is.na(maxNumberOfEventsPerStage)) {
			warning("'maxNumberOfEventsPerStage' (", 
				.arrayToString(maxNumberOfEventsPerStage), ") ",
				"will be ignored because no 'conditionalPower' is defined", call. = FALSE)
		}
	}
	
	minNumberOfEventsPerStage <- .assertIsValidMinNumberOfSubjectsPerStage(minNumberOfEventsPerStage, 
		"minNumberOfEventsPerStage", plannedEvents, conditionalPower, design$kMax) 
	maxNumberOfEventsPerStage <- .assertIsValidMinNumberOfSubjectsPerStage(maxNumberOfEventsPerStage, 
		"maxNumberOfEventsPerStage", plannedEvents, conditionalPower, design$kMax) 
	
	if (!is.na(conditionalPower)) {
		if (design$kMax > 1) {
			if (any(maxNumberOfEventsPerStage - minNumberOfEventsPerStage < 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'maxNumberOfEventsPerStage' (", 
					.arrayToString(maxNumberOfEventsPerStage), 
					") must be not smaller than minNumberOfEventsPerStage' (", 
					.arrayToString(minNumberOfEventsPerStage), ")")
			}
			.setValueAndParameterType(simulationResults, "minNumberOfEventsPerStage", 
				minNumberOfEventsPerStage, NA_real_)
			.setValueAndParameterType(simulationResults, "maxNumberOfEventsPerStage", 
				maxNumberOfEventsPerStage, NA_real_)
		} else {
			warning("'conditionalPower' will be ignored for fixed sample design", call. = FALSE)
		}
	} else {
		simulationResults$minNumberOfEventsPerStage <- NA_real_
		simulationResults$maxNumberOfEventsPerStage <- NA_real_
		simulationResults$.setParameterType("minNumberOfEventsPerStage", C_PARAM_NOT_APPLICABLE)
		simulationResults$.setParameterType("maxNumberOfEventsPerStage", C_PARAM_NOT_APPLICABLE)
		simulationResults$.setParameterType("conditionalPower", C_PARAM_NOT_APPLICABLE)
	}
	
	accrualSetup <- getAccrualTime(accrualTime = accrualTime, 
		accrualIntensity = accrualIntensity, maxNumberOfSubjects = maxNumberOfSubjects)
	if (is.na(accrualSetup$maxNumberOfSubjects)) {
		if (identical(accrualIntensity, 1L)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"choose a 'accrualIntensity' > 1 or define 'maxNumberOfSubjects'")
		}
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'maxNumberOfSubjects' must be defined")
	}
	
	simulationResults$.accrualTime <- accrualSetup
	
	accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
	simulationResults$maxNumberOfSubjects <- accrualSetup$maxNumberOfSubjects
	simulationResults$.setParameterType("maxNumberOfSubjects", 
		accrualSetup$.getParameterType("maxNumberOfSubjects"))
	
	simulationResults$accrualTime <- accrualSetup$.getAccrualTimeWithoutLeadingZero()
	simulationResults$.setParameterType("accrualTime", accrualSetup$.getParameterType("accrualTime"))
	
	simulationResults$accrualIntensity <- accrualSetup$accrualIntensity
	simulationResults$.setParameterType("accrualIntensity", 
		accrualSetup$.getParameterType("accrualIntensity"))
	
	.assertIsIntegerVector(plannedEvents, "plannedEvents", validateType = FALSE)
	if (length(plannedEvents) != design$kMax) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'plannedEvents' (", .arrayToString(plannedEvents), ") must have length ", design$kMax)
	}
	.assertIsInClosedInterval(plannedEvents, "plannedEvents", lower = 1, upper = NULL)
	.assertValuesAreStrictlyIncreasing(plannedEvents, "plannedEvents")
	simulationResults$plannedEvents <- plannedEvents
	simulationResults$.setParameterType("plannedEvents", C_PARAM_USER_DEFINED)
	
	pwsTimeObject <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = piecewiseSurvivalTime, 
		lambda2 = lambda2, lambda1 = lambda1, median1 = median1, median2 = median2,
		hazardRatio = hazardRatio, pi1 = pi1, pi2 = pi2, eventTime = eventTime, kappa = kappa,
		delayedResponseAllowed = TRUE, .pi1Default = C_PI_1_DEFAULT)
	
	simulationResults$.piecewiseSurvivalTime <- pwsTimeObject
	simulationResults$hazardRatio <- pwsTimeObject$hazardRatio
	simulationResults$.setParameterType("hazardRatio", pwsTimeObject$.getParameterType("hazardRatio"))
	
	if (pwsTimeObject$.isLambdaBased()) {	
		simulationResults$piecewiseSurvivalTime <- pwsTimeObject$piecewiseSurvivalTime
		simulationResults$.setParameterType("piecewiseSurvivalTime", C_PARAM_USER_DEFINED)
		
		simulationResults$lambda2 <- pwsTimeObject$lambda2
		simulationResults$.setParameterType("lambda2", pwsTimeObject$.getParameterType("lambda2"))
		lambdaVec2 <- simulationResults$lambda2
		
		simulationResults$lambda1 <- pwsTimeObject$lambda1
		simulationResults$.setParameterType("lambda1", pwsTimeObject$.getParameterType("lambda1"))
		
		if (any(is.na(pwsTimeObject$lambda1))) {
			.assertIsValidHazardRatioVector(pwsTimeObject$hazardRatio) 
			.setValueAndParameterType(simulationResults, "hazardRatio", 
				pwsTimeObject$hazardRatio, NA_real_)
			numberOfResults <- length(simulationResults$hazardRatio)
			lambdaVec1 <- simulationResults$lambda2 * pwsTimeObject$hazardRatio
		} else {
			.setValueAndParameterType(simulationResults, "hazardRatio", 
				pwsTimeObject$hazardRatio, NA_real_)
			numberOfResults <- 1
			lambdaVec1 <- pwsTimeObject$lambda1
		}
		
		.warnInCaseOfDefinedPiValue(simulationResults, "pi1")
		.warnInCaseOfDefinedPiValue(simulationResults, "pi2")
		simulationResults$pi1 <- pwsTimeObject$pi1
		simulationResults$pi2 <- pwsTimeObject$pi2
		simulationResults$.setParameterType("pi1", pwsTimeObject$.getParameterType("pi1"))
		simulationResults$.setParameterType("pi2", pwsTimeObject$.getParameterType("pi2"))

		cdfValues1 <- .getPiecewiseExponentialDistribution(
			pwsTimeObject$piecewiseSurvivalTime, lambdaVec1, 
			pwsTimeObject$piecewiseSurvivalTime, kappa = kappa) 
		cdfValues2 <- .getPiecewiseExponentialDistribution(
			pwsTimeObject$piecewiseSurvivalTime, lambdaVec2, 
			pwsTimeObject$piecewiseSurvivalTime, kappa = kappa) 
		
		if (length(cdfValues1) == 1) {
			cdfValues1 <- NA_real_ 
			cdfValues2 <- NA_real_
		} else {
			cdfValues1 <- cdfValues1[2:length(cdfValues1)] # use values without a leading 0 
			cdfValues2 <- cdfValues2[2:length(cdfValues2)]
		}
		
		pi1 <- NA_real_
		pi2 <- NA_real_
		
	} else {
		numberOfResults <- .initDesignPlanSurvivalByPiecewiseSurvivalTimeObject(
			simulationResults, pwsTimeObject)
		pi1 <- simulationResults$pi1
		pi2 <- simulationResults$pi2
		simulationResults$piecewiseSurvivalTime <- NA_real_
		lambdaVec1 <- NA_real_ 
		lambdaVec2 <- NA_real_ 
		cdfValues1 <- NA_real_
		cdfValues2 <- NA_real_
	}

	numberOfSimStepsTotal <- numberOfResults * maxNumberOfIterations * 
		accrualSetup$maxNumberOfSubjects
	maxNumberOfSimStepsTotal <- 10 * 100000 * 100
	if (numberOfSimStepsTotal > maxNumberOfSimStepsTotal) {
		if (!longTimeSimulationAllowed) {
			stop("Simulation stopped because long time simulation is disabled ",
				"and the defined number of single simulation steps (", numberOfSimStepsTotal,
				") is larger than the threshold ", maxNumberOfSimStepsTotal, ". ",
				"Set 'longTimeSimulationAllowed = TRUE' to enable simulations ",
				"that take a long time (> 30 sec)")
		}
		
		cat("Note that the simulation may take a long time because", 
			sprintf("%.0f", numberOfSimStepsTotal), 
			"single simulation steps must be calculated")
	}
	
	.setValueAndParameterType(simulationResults, "directionUpper", 
		directionUpper, C_DIRECTION_UPPER_DEFAULT)
	.setValueAndParameterType(simulationResults, "dropoutRate1", dropoutRate1, C_DROP_OUT_RATE_1_DEFAULT)
	.setValueAndParameterType(simulationResults, "dropoutRate2", dropoutRate2, C_DROP_OUT_RATE_2_DEFAULT)
	.setValueAndParameterType(simulationResults, "dropoutTime", dropoutTime, C_DROP_OUT_TIME_DEFAULT)
	.setValueAndParameterType(simulationResults, "eventTime", eventTime, C_EVENT_TIME_DEFAULT)
	.setValueAndParameterType(simulationResults, "thetaH0", thetaH0, C_THETA_H0_SURVIVAL_DEFAULT)
	.setValueAndParameterType(simulationResults, "allocation1", allocation1, C_ALLOCATION_1_DEFAULT)
	.setValueAndParameterType(simulationResults, "allocation2", allocation2, C_ALLOCATION_2_DEFAULT)
	allocationRatioPlanned <- allocation1 / allocation2
	.setValueAndParameterType(simulationResults, "allocationRatioPlanned", allocationRatioPlanned, C_ALLOCATION_RATIO_DEFAULT)
	.setValueAndParameterType(simulationResults, "conditionalPower", conditionalPower, NA_real_)
	if (!is.na(thetaH0) && !is.na(thetaH1) && thetaH0 != 1) {
		thetaH1 <- thetaH1 / thetaH0
		.setValueAndParameterType(simulationResults, "thetaH1", thetaH1, NA_real_)
		simulationResults$.setParameterType("thetaH1", C_PARAM_GENERATED)
	} else {
		.setValueAndParameterType(simulationResults, "thetaH1", thetaH1, NA_real_)
	}
	if (is.na(conditionalPower)) {
		simulationResults$.setParameterType("thetaH1", C_PARAM_NOT_APPLICABLE)
	}
	.setValueAndParameterType(simulationResults, "kappa", kappa, 1)
	.setValueAndParameterType(simulationResults, "maxNumberOfIterations", 
		as.integer(maxNumberOfIterations), C_MAX_SIMULATION_ITERATIONS_DEFAULT)
	.setValueAndParameterType(simulationResults, "seed", .setSeed(seed), NA_real_)
	if (is.na(seed)) {
		simulationResults$.setParameterType("seed", C_PARAM_DEFAULT_VALUE)
	}
	
	phi <- -c(log(1 - dropoutRate1), log(1 - dropoutRate2)) / dropoutTime
	
	densityIntervals <- accrualTime
	if (length(accrualTime) > 1) {
		densityIntervals[2:length(accrualTime)] <- 
			accrualTime[2:length(accrualTime)] - accrualTime[1:(length(accrualTime) - 1)]
	}
	densityVector <- accrualSetup$accrualIntensity / sum(densityIntervals * accrualSetup$accrualIntensity)
	
	accrualTimeValue <- cumsum(rep(1 / (densityVector * accrualSetup$maxNumberOfSubjects),
		round(densityVector * densityIntervals * 
		accrualSetup$maxNumberOfSubjects)))[1:accrualSetup$maxNumberOfSubjects]
	
	# to avoid last value to be NA_real_
	i <- accrualSetup$maxNumberOfSubjects
	while (is.na(accrualTimeValue[i])) {
		accrualTimeValue[i] <- accrualTime[length(accrualTime)]
		i <- i - 1
	}    

	treatmentGroup <- rep(c(rep(1, allocation1), rep(2, allocation2)), 
		ceiling(accrualSetup$maxNumberOfSubjects / 
		(allocation1 + allocation2)))[1:accrualSetup$maxNumberOfSubjects]
	
	if (.isTrialDesignFisher(design)) {
		alpha0Vec <- design$alpha0Vec
		futilityBounds <- rep(NA_real_, design$kMax - 1)
	} else {
		alpha0Vec <- rep(NA_real_, design$kMax - 1)
		futilityBounds <- design$futilityBounds
	}
	
	if (.isTrialDesignGroupSequential(design)) {
		designNumber <- 1L
	} else if (.isTrialDesignInverseNormal(design)) {
		designNumber <- 2L
	} else if (.isTrialDesignFisher(design)) {
		designNumber <- 3L
	}
	
	resultData <- getSimulationSurvivalCpp(
		designNumber,
		design$kMax,
		design$sided,
		design$criticalValues,
		design$informationRates,
		conditionalPower,
		plannedEvents,
		thetaH1,
		minNumberOfEventsPerStage,
		maxNumberOfEventsPerStage,
		directionUpper,
		allocation1,
		allocation2,
		accrualTimeValue,
		treatmentGroup,
		thetaH0,
		futilityBounds,
		alpha0Vec,
		pi1,
		pi2,
		eventTime,
		.getPiecewiseExpStartTimesWithoutLeadingZero(pwsTimeObject$piecewiseSurvivalTime), 
		cdfValues1, 
		cdfValues2,
		lambdaVec1,
		lambdaVec2, 
		phi,
		accrualSetup$maxNumberOfSubjects,
		maxNumberOfIterations, 
		maxNumberOfRawDatasetsPerStage,
		kappa)
	
	overview <- resultData$overview
	if (length(overview) == 0 || nrow(overview) == 0) {
		stop("No simulation results calculated")
	}
	
	n <- nrow(overview)
	overview <- cbind(
		design = rep(sub("^TrialDesign", "", class(design)), n),
		overview)
	
	if (pwsTimeObject$.isPiBased() && 
			pwsTimeObject$.getParameterType("hazardRatio") != C_PARAM_USER_DEFINED) {
		simulationResults$hazardRatio <- matrix(overview$hazardRatio, nrow = design$kMax)[1, ]
	}
	simulationResults$iterations <- matrix(as.integer(overview$iterations), nrow = design$kMax)
	if (!is.null(overview$eventsPerStage)) {
		simulationResults$eventsPerStage <- matrix(overview$eventsPerStage, nrow = design$kMax)
	}
	simulationResults$eventsNotAchieved <- matrix(overview$eventsNotAchieved, nrow = design$kMax)
	simulationResults$numberOfSubjects <- matrix(overview$numberOfSubjects, nrow = design$kMax)
	
	simulationResults$numberOfSubjects1 <- 
		.getNumberOfSubjects1(simulationResults$numberOfSubjects, allocationRatioPlanned)
	simulationResults$numberOfSubjects2 <- 
		.getNumberOfSubjects2(simulationResults$numberOfSubjects, allocationRatioPlanned)
	if (allocationRatioPlanned != 1) {
		simulationResults$.setParameterType("numberOfSubjects1", C_PARAM_GENERATED)
		simulationResults$.setParameterType("numberOfSubjects2", C_PARAM_GENERATED)
	}
	
	if (design$kMax > 1) {
		simulationResults$rejectPerStage <- matrix(overview$rejectPerStage, nrow = design$kMax)
	}
	simulationResults$overallReject <- matrix(overview$overallReject, nrow = design$kMax)[1, ]
	
	if (!all(is.na(overview$conditionalPowerAchieved))) {
		simulationResults$conditionalPowerAchieved <- matrix(
			overview$conditionalPowerAchieved, nrow = design$kMax)
		simulationResults$.setParameterType("conditionalPowerAchieved", C_PARAM_GENERATED)
	}
	
	if (design$kMax == 1) {
		simulationResults$.setParameterType("numberOfSubjects", C_PARAM_NOT_APPLICABLE)
		simulationResults$.setParameterType("eventsPerStage", C_PARAM_NOT_APPLICABLE)
	}

	if (design$kMax > 1) {
		if (numberOfResults == 1) {
			simulationResults$futilityPerStage <- matrix(
				overview$futilityPerStage[1:(design$kMax - 1)], nrow = design$kMax - 1)
		} else {
			simulationResults$futilityPerStage <- matrix(matrix(
				overview$futilityPerStage, nrow = design$kMax)[1:(design$kMax - 1), ], 
				nrow = design$kMax - 1)
		}
	}
	if (design$kMax > 1) {
		simulationResults$futilityStop <- matrix(overview$futilityStop, nrow = design$kMax)[1, ]
		simulationResults$earlyStop <- simulationResults$futilityStop + 
			simulationResults$overallReject - simulationResults$rejectPerStage[design$kMax, ]
	} else {
		simulationResults$futilityStop <- rep(0, numberOfResults)
		simulationResults$earlyStop <- rep(0, numberOfResults)
	}
	
	simulationResults$analysisTime <- matrix(overview$analysisTime, nrow = design$kMax)
	simulationResults$studyDuration <- matrix(overview$studyDuration, nrow = design$kMax)[1, ]
	
	if (design$kMax > 1) {
		subData <- simulationResults$rejectPerStage[1:(design$kMax - 1), ] + 
			simulationResults$futilityPerStage
		pStop <- rbind(subData, 1 - colSums(subData))
		
		numberOfSubjects <- simulationResults$numberOfSubjects
		numberOfSubjects[is.na(numberOfSubjects)] <- 0
		simulationResults$expectedNumberOfSubjects <- 
			diag(t(numberOfSubjects) %*% pStop)
		
		if (nrow(simulationResults$eventsPerStage) > 0 &&
				ncol(simulationResults$eventsPerStage) > 0) {
			eventsPerStage <- simulationResults$eventsPerStage
			eventsPerStage[is.na(eventsPerStage)] <- 0
			simulationResults$expectedNumberOfEvents <- 
				diag(t(eventsPerStage) %*% pStop)
		}
	} else {
		simulationResults$expectedNumberOfSubjects <- 
			as.numeric(simulationResults$numberOfSubjects)
		if (nrow(simulationResults$eventsPerStage) > 0 &&
				ncol(simulationResults$eventsPerStage) > 0) {
			simulationResults$expectedNumberOfEvents <- 
				as.numeric(simulationResults$eventsPerStage)
		}
	}
	
	data <- resultData$data[!is.na(resultData$data$iterationNumber), ]
	
	data$trialStop <- (data$rejectPerStage == 1 | data$futilityPerStage == 1 | 
		data$stageNumber == design$kMax)
	
	if (!is.null(data$eventsPerStage)) {
		if (directionUpper) {
			data$hazardRatioEstimateLR <- exp(data$logRankStatistic * 
					(1 + allocation1 / allocation2) / sqrt(allocation1 / allocation2 * 
							data$eventsPerStage))
		} else {
			data$hazardRatioEstimateLR <- exp(-data$logRankStatistic * 
					(1 + allocation1 / allocation2) / sqrt(allocation1 / allocation2 * 
							data$eventsPerStage))
		}
	}

	simulationResults$.data <- data
	
	stages <- 1:design$kMax
	rawData <- resultData$rawData
	if (!is.null(rawData) && nrow(rawData) > 0 && ncol(rawData) > 0) {
		rawData <- rawData[!is.na(rawData$iterationNumber), ]
	}
	if (!is.null(rawData) && nrow(rawData) > 0 && ncol(rawData) > 0) {
		stopStageNumbers <- rawData$stopStage
		missingStageNumbers <- c()
		if (length(stopStageNumbers) > 0) {
			stopStageNumbers <- order(unique(stopStageNumbers))
			missingStageNumbers <- stages[!which(stages %in% stopStageNumbers)]
		} else {
			missingStageNumbers <- stages
		}
		if (length(missingStageNumbers) > 0) {
			warning("Could not get rawData (individual results) for stages ", 
				.arrayToString(missingStageNumbers), call. = FALSE)
		}
	} else {
		rawData <- data.frame(
			iterationNumber = numeric(0),
			stopStage = numeric(0),
			pi1 = numeric(0),
			pi2 = numeric(0),
			subjectId = numeric(0),
			accrualTime = numeric(0),
			treatmentGroup = numeric(0),
			survivalTime = numeric(0),
			dropoutTime = numeric(0),
			observationTime = numeric(0),
			timeUnderObservation = numeric(0),
			event = logical(0),
			dropoutEvent = logical(0),
			censorIndicator = numeric(0)
		)
		if (maxNumberOfRawDatasetsPerStage > 0) {
			warning("Could not get rawData (individual results) for stages ", 
				.arrayToString(stages), call. = FALSE)
		}
	}
	
	if (pwsTimeObject$.isLambdaBased() || length(pi1) < 2) {
		rawData <- rawData[, !(colnames(rawData) %in% c("pi1", "pi2"))]
	}
	
	# Remove censorIndicator because it will not be calculated yet
	rawData <- rawData[, colnames(rawData) != "censorIndicator"]
	
	simulationResults$.rawData <- rawData
	
	return(simulationResults)
}


