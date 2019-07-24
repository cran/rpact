######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 23 July 2019, 11:45:44                                                       #
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

context("Testing the analysis survival functionality for the group sequential design")


test_that("'getAnalysisResults' for a group sequential design and survival data", {
	design1 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample1 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design1, dataExample1, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0, 0, 0))
	expect_equal(x1$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(2.9294137, 2.0393455, 2.9359555, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(1.52, 0.3951648, 2.7453772, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.064255488, 0.34636063, 0.003022069, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$thetaH0, 1)
	expect_equal(x1$thetaH1, 2.9359555, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.044563047, 0.46900287, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.58958009, 1.2243899, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(17.013382, 7.0540547, 7.0401059, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.19438137, 0.0054226276, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.013371274, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 1.1294538, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 5.6956842, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 2.632639, NA_real_), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$overallTestStatistics, c(1.52, 1.38, 2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.064255488, 0.083793322, 0.0018658133, NA_real_), tolerance = 1e-07)

	.skipTestifDisabled()

	x2 <- getAnalysisResults(design1, dataExample1, stage = 2, nPlanned = c(20,40), 
		allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(0, 0, 0))
	expect_equal(x2$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(2.9294137, 2.0393455, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(1.52, 0.3951648, 2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.064255488, 0.34636063, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 1)
	expect_equal(x2$thetaH1, 2)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.07432319, 0.044563047, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 20, 40))
	expect_equal(x2$allocationRatioPlanned, 2)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.24122422, 0.76137238), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.58958009, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(17.013382, 7.0540547, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.16918725, 0.19438137, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, TRUE)
	expect_equal(x2$overallTestStatistics, c(1.52, 1.38, 2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x2$overallPValues, c(0.064255488, 0.083793322, 0.0018658133, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(1, 2.5, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.042054884, 0.058920703, 0.079860688, 0.10500347, 0.13429067, 0.16748187, 0.20417526, 0.24383962, 0.28585263, 0.32954089, 0.37421781, 0.41921675, 0.46391757, 0.50776612, 0.55028679, 0.59108872, 0.62986668, 0.6663978, 0.70053535, 0.73220037, 0.76137238, 0.78807962, 0.81238956, 0.83439998, 0.85423087, 0.87201737, 0.88790373, 0.90203829, 0.91456948, 0.92564264, 0.93539766), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

	x3 <- getAnalysisResults(design1, dataExample1, thetaH0 = 0.95, stage = 2, 
		nPlanned = c(20, 40), allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2, 3, 4))
	expect_equal(x3$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(0, 0, 0))
	expect_equal(x3$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(2.9294137, 2.0393455, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(1.52, 0.3951648, 2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x3$pValues, c(0.055631748, 0.32167521, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$thetaH0, 0.95, tolerance = 1e-07)
	expect_equal(x3$thetaH1, 2)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.082607165, 0.055558825, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_, 20, 40))
	expect_equal(x3$allocationRatioPlanned, 2)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.32497202, 0.83762717), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.58958009, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(17.013382, 7.0540547, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.15076802, 0.16617365, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, TRUE)
	expect_equal(x3$directionUpper, TRUE)
	expect_equal(x3$overallTestStatistics, c(1.5925397, 1.479329, 3.0381114, NA_real_), tolerance = 1e-07)
	expect_equal(x3$overallPValues, c(0.055631748, 0.069526198, 0.0011903296, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x3, thetaRange = seq(1, 2.5, 0.05))

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.073284723, 0.098867221, 0.12923781, 0.16416019, 0.20317699, 0.24565174, 0.29082221, 0.33785714, 0.3859101, 0.43416582, 0.48187662, 0.52838782, 0.57315275, 0.61573859, 0.65582473, 0.69319563, 0.72772995, 0.75938755, 0.7881956, 0.81423493, 0.83762717, 0.85852313, 0.87709283, 0.89351704, 0.9079804, 0.92066609, 0.93175171, 0.94140636, 0.94978864, 0.95704547, 0.96331147), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "Hazard ratio")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

})

test_that("'getAnalysisResults' for a group sequential design and survival data ('directionUpper' reversed)", {

	.skipTestifDisabled()

	design2 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample2 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = -c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design2, dataExample2, directionUpper = FALSE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0, 0, 0))
	expect_equal(x1$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.34136523, 0.49035339, 0.34060461, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(-1.52, -0.3951648, -2.7453772, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.064255488, 0.34636063, 0.003022069, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$thetaH0, 1)
	expect_equal(x1$thetaH1, 0.34060461, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.044563047, 0.46900287, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14176244, 0.14204332, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.6961224, 0.81673327, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.19438137, 0.0054226276, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.013371274, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.17557153, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.88538374, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.379847, NA_real_), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$overallTestStatistics, c(-1.52, -1.38, -2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.064255488, 0.083793322, 0.0018658133, NA_real_), tolerance = 1e-07)

	x2 <- getAnalysisResults(design2, dataExample2, thetaH0 = 1.1, stage = 2, 
		nPlanned = c(20, 40), allocationRatioPlanned = 0.5, thetaH1 = 0.5, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(0, 0, 0))
	expect_equal(x2$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.34136523, 0.49035339, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(-1.52, -0.3951648, -2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.048983658, 0.3010969, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 1.1, tolerance = 1e-07)
	expect_equal(x2$thetaH1, 0.5, tolerance = 1e-07)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.090339948, 0.066890003, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 20, 40))
	expect_equal(x2$allocationRatioPlanned, 0.5, tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.40494574, 0.88883511), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14176244, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.6961224, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.13608528, 0.14422583, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$overallTestStatistics, c(-1.6547889, -1.5645674, -3.1566305, NA_real_), tolerance = 1e-07)
	expect_equal(x2$overallPValues, c(0.048983658, 0.058842192, 0.00079801722, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(0.4, 1, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.97858552, 0.94519604, 0.88883511, 0.81002306, 0.71447863, 0.61071863, 0.50731205, 0.41100476, 0.32600179, 0.25411912, 0.19536859, 0.14863199, 0.11223419), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.92517582, 0.98626675, 0.99928862, 0.9755955, 0.92648393, 0.86161675, 0.78854281, 0.71277663, 0.63811141, 0.56698955, 0.50084781, 0.44040564, 0.38589113), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 0.5")

})

context("Testing the analysis survival functionality for the inverse normal design")


test_that("'getAnalysisResults' for an inverse normal design and survival data", {
	design3 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample3 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design3, dataExample3, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0, 0, 0))
	expect_equal(x1$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(2.9294137, 2.0393455, 2.9359555, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(1.52, 0.3951648, 2.7453772, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.064255488, 0.34636063, 0.003022069, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$thetaH0, 1)
	expect_equal(x1$thetaH1, 2.9359555, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.042056716, 0.36917623, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.5816096, 1.1345596, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(17.013382, 6.9683119, 6.6631754, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.20216143, 0.010091808, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.014307783, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 1.121428, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 5.6413216, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 2.6253218, NA_real_), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$combinationTestStatistics, c(1.52, 1.354226, 2.6907652, NA_real_), tolerance = 1e-07)

	x2 <- getAnalysisResults(design3, stage = 2, nPlanned = c(20,40), 
		allocationRatioPlanned = 2, thetaH1 = 2, dataExample3, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(0, 0, 0))
	expect_equal(x2$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(2.9294137, 2.0393455, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(1.52, 0.3951648, 2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.064255488, 0.34636063, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 1)
	expect_equal(x2$thetaH1, 2)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.07432319, 0.042056716, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 20, 40))
	expect_equal(x2$allocationRatioPlanned, 2)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.24122422, 0.76137238), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.5816096, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(17.013382, 6.9683119, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.16918725, 0.20216143, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, TRUE)
	expect_equal(x2$combinationTestStatistics, c(1.52, 1.354226, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(1, 2.5, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.042054884, 0.058920703, 0.079860688, 0.10500347, 0.13429067, 0.16748187, 0.20417526, 0.24383962, 0.28585263, 0.32954089, 0.37421781, 0.41921675, 0.46391757, 0.50776612, 0.55028679, 0.59108872, 0.62986668, 0.6663978, 0.70053535, 0.73220037, 0.76137238, 0.78807962, 0.81238956, 0.83439998, 0.85423087, 0.87201737, 0.88790373, 0.90203829, 0.91456948, 0.92564264, 0.93539766), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

	x3 <- getAnalysisResults(design3, dataExample3, thetaH0 = 0.95, stage = 2, 
		nPlanned = c(20,40), allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2, 3, 4))
	expect_equal(x3$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(0, 0, 0))
	expect_equal(x3$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(2.9294137, 2.0393455, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(1.52, 0.3951648, 2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x3$pValues, c(0.055631748, 0.32167521, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$thetaH0, 0.95, tolerance = 1e-07)
	expect_equal(x3$thetaH1, 2)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.082607165, 0.052483916, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_, 20, 40))
	expect_equal(x3$allocationRatioPlanned, 2)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.32497202, 0.83762717), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.5816096, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(17.013382, 6.9683119, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.15076802, 0.17323655, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, TRUE)
	expect_equal(x3$directionUpper, TRUE)
	expect_equal(x3$combinationTestStatistics, c(1.5925397, 1.4534998, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x3, thetaRange = seq(1, 2.5, 0.05))

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.073284723, 0.098867221, 0.12923781, 0.16416019, 0.20317699, 0.24565174, 0.29082221, 0.33785714, 0.3859101, 0.43416582, 0.48187662, 0.52838782, 0.57315275, 0.61573859, 0.65582473, 0.69319563, 0.72772995, 0.75938755, 0.7881956, 0.81423493, 0.83762717, 0.85852313, 0.87709283, 0.89351704, 0.9079804, 0.92066609, 0.93175171, 0.94140636, 0.94978864, 0.95704547, 0.96331147), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "Hazard ratio")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

})

test_that("'getAnalysisResults' for an inverse normal design and survival data ('directionUpper' reversed)", {

	.skipTestifDisabled()

	design4 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample4 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = -c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design4, dataExample4, directionUpper = FALSE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0, 0, 0))
	expect_equal(x1$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.34136523, 0.49035339, 0.34060461, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(-1.52, -0.3951648, -2.7453772, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.064255488, 0.34636063, 0.003022069, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$thetaH0, 1)
	expect_equal(x1$thetaH1, 0.34060461, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.042056716, 0.36917623, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14350678, 0.15007853, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.7193664, 0.88139925, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.20216143, 0.010091808, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.014307783, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.17726343, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.89172021, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.38090568, NA_real_), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$combinationTestStatistics, c(1.52, 1.354226, 2.6907652, NA_real_), tolerance = 1e-07)

	x2 <- getAnalysisResults(design4, dataExample4, thetaH0 = 1.1, stage = 2, 
		nPlanned = c(20, 40), allocationRatioPlanned = 0.5, thetaH1 = 0.5, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.4878815, 2.4031352, 2.3549063, 2.2955206), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(0, 0, 0))
	expect_equal(x2$alphaSpent, c(0.0064253267, 0.012720859, 0.01826361, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0064253267, 0.0081275893, 0.0092636882, 0.010851653), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.34136523, 0.49035339, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(-1.52, -0.3951648, -2.9, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.048983658, 0.3010969, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 1.1, tolerance = 1e-07)
	expect_equal(x2$thetaH1, 0.5, tolerance = 1e-07)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.090339948, 0.063249751, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 20, 40))
	expect_equal(x2$allocationRatioPlanned, 0.5, tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.40494574, 0.88883511), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14350678, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.7193664, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.13608528, 0.15066694, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$combinationTestStatistics, c(1.6547889, 1.5386907, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(0.4, 1, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.97858552, 0.94519604, 0.88883511, 0.81002306, 0.71447863, 0.61071863, 0.50731205, 0.41100476, 0.32600179, 0.25411912, 0.19536859, 0.14863199, 0.11223419), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.92517582, 0.98626675, 0.99928862, 0.9755955, 0.92648393, 0.86161675, 0.78854281, 0.71277663, 0.63811141, 0.56698955, 0.50084781, 0.44040564, 0.38589113), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 0.5")

})

context("Testing the analysis survival functionality for the Fisher design")


test_that("'getAnalysisResults' for a Fisher design and 'bindingFutility = TRUE'", {
	.skipTestifDisabled()

	design5 <- getDesignFisher(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), alpha0Vec = c(0.6,0.5,0.4), bindingFutility = TRUE)

	dataExample5 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = c(1.52, 1.38, 2)
	)

	x1 <- getAnalysisResults(design5, dataExample5, thetaH1 = 2, allocationRatioPlanned = 2, 
		nPlanned = 50, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(0.012419362, 0.0016809245, 0.00029441484, 1.8548902e-05), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0.6, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(x1$alphaSpent, c(0.012419362, 0.018937437, 0.022642761, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.012419362, 0.012419362, 0.012419362, 0.012419362), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(2.9294137, 2.0393455, 2.1017732, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(1.52, 0.3951648, 1.450056, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.064255488, 0.34636063, 0.073521457, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$thetaH0, 1)
	expect_equal(x1$thetaH1, 2)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.046367462, 0.024190775, 0.042101664, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, 50))
	expect_equal(x1$allocationRatioPlanned, 2)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.72028527), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.59937028, 0.5945604, 0.81409304, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(14.31747, 6.9389819, 5.3768854, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.10915739, 0.16855974, 0.081195715, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$combinationTestStatistics, c(0.064255488, 0.022255572, 0.0016362621, NA_real_), tolerance = 1e-07)

})

test_that("'getAnalysisResults' for a Fisher design and 'bindingFutility = TRUE' ('directionUpper' reversed)", {

	.skipTestifDisabled()

	design6 <- getDesignFisher(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), alpha0Vec = c(0.6,0.5,0.4), bindingFutility = TRUE)

	dataExample6 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = -c(1.52, 1.38, 2)
	)

	x1 <- getAnalysisResults(design6, dataExample6, thetaH1 = 0.5, allocationRatioPlanned = 0.5, 
		nPlanned = 50, directionUpper = FALSE)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(0.012419362, 0.0016809245, 0.00029441484, 1.8548902e-05), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0.6, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(x1$alphaSpent, c(0.012419362, 0.018937437, 0.022642761, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.012419362, 0.012419362, 0.012419362, 0.012419362), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.34136523, 0.49035339, 0.47578874, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(-1.52, -0.3951648, -1.450056, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.064255488, 0.34636063, 0.073521457, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$thetaH0, 1)
	expect_equal(x1$thetaH1, 0.5, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.046367462, 0.024190775, 0.042101664, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, 50))
	expect_equal(x1$allocationRatioPlanned, 0.5, tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.72028527), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.069844861, 0.14411336, 0.18598127, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.6684179, 1.6819149, 1.2283608, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.10915739, 0.16855974, 0.081195715, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$combinationTestStatistics, c(0.064255488, 0.022255572, 0.0016362621, NA_real_), tolerance = 1e-07)

})

