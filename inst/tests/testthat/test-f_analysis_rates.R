######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 27 May 2019, 12:53:29                                                        #
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

context("Testing the analysis rates functionality for one treatment")


test_that("'getAnalysisResults' for a group sequential design and one treatment", {
	design1 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1), 
		futilityBounds = c(-0.5, 0, 0.5),	typeOfDesign = "asKD", gammaA = 2.8)

	dataExample1 <- getDataset(
		n = c(8, 10, 9, 11), 
		events = c(4, 5, 5, 6)
	)

	x1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = FALSE, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x1$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$pValues, c(0.11381531, 0.078126907, 0.16572571, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.055828724, 0.21032099, 0.60291694, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, NA_real_)
	expect_equal(x1$pi2, NA_real_)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.1766668, 0.29544407, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.8233332, 0.73635572, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.26917981, 0.015800491, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.0089457853, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.32991006, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.70969307, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.51904357, NA_real_), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, FALSE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$overallTestStatistics, c(1.2064848, 2.0674098, 2.4192811), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.11381531, 0.01934778, 0.0077756083), tolerance = 1e-07)

	.skipTestifDisabled()

	x2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = TRUE, directionUpper = FALSE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(-1.6329932, -1.8257419, -1.3471506, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.051235217, 0.033944577, 0.088965863, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x2$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.088079629, 0.32476642, 0.83593758, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, NA_real_)
	expect_equal(x2$pi2, NA_real_)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21639861, 0.31742335, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78360139, 0.71378821, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.10104164, 0.0056362503, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, 3)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, 0.0042246203, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.29660132, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.68026724, NA_real_), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.48659273, NA_real_), tolerance = 1e-07)
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$overallTestStatistics, c(-1.6329932, -2.4494897, -2.7777778, NA_real_), tolerance = 1e-07)
	expect_equal(x2$overallPValues, c(0.051235217, 0.0071529392, 0.0027366018, NA_real_), tolerance = 1e-07)

	x3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2, 3, 4))
	expect_equal(x3$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x3$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$pValues, c(0.11381531, 0.078126907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.055828724, 0.21032099, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x3$allocationRatioPlanned, 1)
	expect_equal(x3$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x3$pi2, NA_real_)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.6918414, 0.87964625), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.1766668, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.8233332, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.26917981, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, FALSE)
	expect_equal(x3$directionUpper, FALSE)
	expect_equal(x3$overallTestStatistics, c(1.2064848, 2.0674098), tolerance = 1e-07)
	expect_equal(x3$overallPValues, c(0.11381531, 0.01934778), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x3, piRange = seq(0.45, 0.75, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.94525609, 0.87964625, 0.77671901, 0.6376454, 0.47357888, 0.30528352, 0.15917802), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18")

	x4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x4' with expected results
	##
	expect_equal(x4$stages, c(1, 2, 3, 4))
	expect_equal(x4$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x4$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x4$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x4$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testStatistics, c(-1.6329932, -1.8257419, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$pValues, c(0.051235217, 0.033944577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x4$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x4$conditionalRejectionProbabilities, c(0.088079629, 0.32476642, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x4$allocationRatioPlanned, 1)
	expect_equal(x4$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x4$pi2, NA_real_)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.85377193, 0.95011174), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21639861, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78360139, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.49999905, 0.10104164, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$normalApproximation, TRUE)
	expect_equal(x4$directionUpper, FALSE)
	expect_equal(x4$overallTestStatistics, c(-1.6329932, -2.4494897, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$overallPValues, c(0.051235217, 0.0071529392, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x4, piRange = seq(0.45, 0.75, 0.05))

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.98086022, 0.95011174, 0.89224094, 0.79890753, 0.66697034, 0.50241609, 0.32350374), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18")

	x5 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = FALSE, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x5' with expected results
	##
	expect_equal(x5$stages, c(1, 2, 3, 4))
	expect_equal(x5$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x5$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x5$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x5$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x5$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x5$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x5$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$pValues, c(0.11381531, 0.078126907, 0.048927307, NA_real_), tolerance = 1e-07)
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x5$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x5$conditionalRejectionProbabilities, c(0.055828724, 0.21032099, 0.85300796, NA_real_), tolerance = 1e-07)
	expect_equal(x5$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$allocationRatioPlanned, 1)
	expect_equal(x5$pi1, NA_real_)
	expect_equal(x5$pi2, NA_real_)
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.1766668, 0.29544407, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.8233332, 0.73635572, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedPValues, c(0.49999905, 0.26917981, 0.0050506954, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalStage, 3)
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, 0.003964958, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.32244641, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.70667629, NA_real_), tolerance = 1e-07)
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.51656189, NA_real_), tolerance = 1e-07)
	expect_equal(x5$normalApproximation, FALSE)
	expect_equal(x5$directionUpper, TRUE)
	expect_equal(x5$overallTestStatistics, c(1.2064848, 2.0674098, 2.8135397), tolerance = 1e-07)
	expect_equal(x5$overallPValues, c(0.11381531, 0.01934778, 0.0024499668), tolerance = 1e-07)

	x6 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = TRUE, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x6' with expected results
	##
	expect_equal(x6$stages, c(1, 2, 3, 4))
	expect_equal(x6$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x6$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x6$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x6$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x6$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x6$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x6$testStatistics, c(1.6329932, 1.8257419, 2.116951, NA_real_), tolerance = 1e-07)
	expect_equal(x6$pValues, c(0.051235217, 0.033944577, 0.017132004, NA_real_), tolerance = 1e-07)
	expect_equal(x6$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x6$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x6$conditionalRejectionProbabilities, c(0.088079629, 0.32476642, 0.96903431, NA_real_), tolerance = 1e-07)
	expect_equal(x6$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$allocationRatioPlanned, 1)
	expect_equal(x6$pi1, NA_real_)
	expect_equal(x6$pi2, NA_real_)
	expect_equal(x6$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21639861, 0.31742335, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78360139, 0.71378821, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedPValues, c(0.49999905, 0.10104164, 0.0013294657, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalStage, 3)
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, 0.0023857966, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.34941079, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.74332995, NA_real_), tolerance = 1e-07)
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.55110366, NA_real_), tolerance = 1e-07)
	expect_equal(x6$normalApproximation, TRUE)
	expect_equal(x6$directionUpper, TRUE)
	expect_equal(x6$overallTestStatistics, c(1.6329932, 2.4494897, 3.2222222, NA_real_), tolerance = 1e-07)
	expect_equal(x6$overallPValues, c(0.051235217, 0.0071529392, 0.00063600219, NA_real_), tolerance = 1e-07)

	x7 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = TRUE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x7' with expected results
	##
	expect_equal(x7$stages, c(1, 2, 3, 4))
	expect_equal(x7$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x7$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x7$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x7$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x7$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x7$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$pValues, c(0.11381531, 0.078126907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x7$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x7$conditionalRejectionProbabilities, c(0.055828724, 0.21032099, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x7$allocationRatioPlanned, 1)
	expect_equal(x7$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x7$pi2, NA_real_)
	expect_equal(x7$conditionalPower, c(NA_real_, NA_real_, 0.6918414, 0.87964625), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.1766668, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.8233332, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedPValues, c(0.49999905, 0.26917981, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$finalStage, NA_integer_)
	expect_equal(x7$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$normalApproximation, FALSE)
	expect_equal(x7$directionUpper, TRUE)
	expect_equal(x7$overallTestStatistics, c(1.2064848, 2.0674098), tolerance = 1e-07)
	expect_equal(x7$overallPValues, c(0.11381531, 0.01934778), tolerance = 1e-07)

	plotData3 <- testGetAnalysisResultsPlotData(x7, piRange = seq(0.25, 0.55, 0.05))

	##
	## Comparison of the results of list object 'plotData3' with expected results
	##
	expect_equal(plotData3$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData3$condPowerValues, c(0.15917802, 0.30528352, 0.47357888, 0.6376454, 0.77671901, 0.87964625, 0.94525609), tolerance = 1e-07)
	expect_equal(plotData3$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData3$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData3$xlab, "pi1")
	expect_equal(plotData3$ylab, "Conditional power / Likelihood")
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 18")

	x8 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = TRUE) 

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x8' with expected results
	##
	expect_equal(x8$stages, c(1, 2, 3, 4))
	expect_equal(x8$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x8$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x8$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x8$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x8$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x8$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$testStatistics, c(1.6329932, 1.8257419, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$pValues, c(0.051235217, 0.033944577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x8$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x8$conditionalRejectionProbabilities, c(0.088079629, 0.32476642, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x8$allocationRatioPlanned, 1)
	expect_equal(x8$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x8$pi2, NA_real_)
	expect_equal(x8$conditionalPower, c(NA_real_, NA_real_, 0.85377193, 0.95011174), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21639861, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78360139, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedPValues, c(0.49999905, 0.10104164, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$finalStage, NA_integer_)
	expect_equal(x8$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$normalApproximation, TRUE)
	expect_equal(x8$directionUpper, TRUE)
	expect_equal(x8$overallTestStatistics, c(1.6329932, 2.4494897, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$overallPValues, c(0.051235217, 0.0071529392, NA_real_, NA_real_), tolerance = 1e-07)

	plotData4 <- testGetAnalysisResultsPlotData(x8, piRange = seq(0.25, 0.55, 0.05))

	##
	## Comparison of the results of list object 'plotData4' with expected results
	##
	expect_equal(plotData4$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData4$condPowerValues, c(0.32350374, 0.50241609, 0.66697034, 0.79890753, 0.89224094, 0.95011174, 0.98086022), tolerance = 1e-07)
	expect_equal(plotData4$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData4$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData4$xlab, "pi1")
	expect_equal(plotData4$ylab, "Conditional power / Likelihood")
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 18")

})

test_that("'getAnalysisResults' for an inverse sequential design and one treatment", {

	.skipTestifDisabled()

	design2 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1),
		futilityBounds = c(-0.5, 0, 0.5), typeOfDesign = "asKD", gammaA = 2.8)

	dataExample2 <- getDataset(
		n = c(8, 10, 9, 11), # cumsum, overall n = (8, 18, 27, 38)
		events = c(4, 5, 5, 6) # cumsum, overall events = (4, 9, 14, 20)
	)

	x1 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = FALSE, directionUpper = FALSE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x1$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$pValues, c(0.11381531, 0.078126907, 0.16572571, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, 0.28098687, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, NA_real_)
	expect_equal(x1$pi2, NA_real_)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, 0.26858957, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, 0.76870152, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.43799317, 0.045574143, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, FALSE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$combinationTestStatistics, c(1.2064848, 1.8556383, 1.9988727, NA_real_), tolerance = 1e-07)

	.skipTestifDisabled()

	x2 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = TRUE, directionUpper = FALSE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(-1.6329932, -1.8257419, -1.3471506, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.051235217, 0.033944577, 0.088965863, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x2$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, 0.78413538, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, NA_real_)
	expect_equal(x2$pi2, NA_real_)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, 0.31861038, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, 0.72001941, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.1020964, 0.0075111702, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, 3)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, 0.0050707339, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.30413229, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.68870859, NA_real_), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.49547717, NA_real_), tolerance = 1e-07)
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$combinationTestStatistics, c(1.6329932, 2.445695, 2.6819469, NA_real_), tolerance = 1e-07)

	x3 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2, 3, 4))
	expect_equal(x3$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x3$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$pValues, c(0.11381531, 0.078126907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x3$allocationRatioPlanned, 1)
	expect_equal(x3$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x3$pi2, NA_real_)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.69921202, 0.88465983), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.43799317, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, FALSE)
	expect_equal(x3$directionUpper, FALSE)
	expect_equal(x3$combinationTestStatistics, c(1.2064848, 1.8556383, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x3, piRange = seq(0.45, 0.75, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.94793138, 0.88465983, 0.78396384, 0.64581102, 0.48045808, 0.30888816, 0.15917802), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18")

	x4 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = FALSE) 

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x4' with expected results
	##
	expect_equal(x4$stages, c(1, 2, 3, 4))
	expect_equal(x4$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x4$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x4$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x4$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testStatistics, c(-1.6329932, -1.8257419, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$pValues, c(0.051235217, 0.033944577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x4$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x4$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x4$allocationRatioPlanned, 1)
	expect_equal(x4$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x4$pi2, NA_real_)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.85385983, 0.95015898), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.49999905, 0.1020964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$normalApproximation, TRUE)
	expect_equal(x4$directionUpper, FALSE)
	expect_equal(x4$combinationTestStatistics, c(1.6329932, 2.445695, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x4, piRange = seq(0.45, 0.75, 0.05))

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.98088099, 0.95015898, 0.89232288, 0.79901831, 0.66708346, 0.50248974, 0.32350374), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18")

	x5 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = FALSE, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x5' with expected results
	##
	expect_equal(x5$stages, c(1, 2, 3, 4))
	expect_equal(x5$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x5$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x5$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x5$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x5$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x5$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x5$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$pValues, c(0.11381531, 0.078126907, 0.048927307, NA_real_), tolerance = 1e-07)
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x5$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x5$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, 0.6508521, NA_real_), tolerance = 1e-07)
	expect_equal(x5$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$allocationRatioPlanned, 1)
	expect_equal(x5$pi1, NA_real_)
	expect_equal(x5$pi2, NA_real_)
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, 0.26858957, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, 0.76870152, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedPValues, c(0.49999905, 0.43799317, 0.013282796, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalStage, 3)
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, 0.007752129, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.29554194, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.67875285, NA_real_), tolerance = 1e-07)
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.48769645, NA_real_), tolerance = 1e-07)
	expect_equal(x5$normalApproximation, FALSE)
	expect_equal(x5$directionUpper, TRUE)
	expect_equal(x5$combinationTestStatistics, c(1.2064848, 1.8556383, 2.4826398, NA_real_), tolerance = 1e-07)

	x6 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = TRUE, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x6' with expected results
	##
	expect_equal(x6$stages, c(1, 2, 3, 4))
	expect_equal(x6$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x6$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x6$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x6$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x6$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x6$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x6$testStatistics, c(1.6329932, 1.8257419, 2.116951, NA_real_), tolerance = 1e-07)
	expect_equal(x6$pValues, c(0.051235217, 0.033944577, 0.017132004, NA_real_), tolerance = 1e-07)
	expect_equal(x6$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x6$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x6$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, 0.96959663, NA_real_), tolerance = 1e-07)
	expect_equal(x6$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$allocationRatioPlanned, 1)
	expect_equal(x6$pi1, NA_real_)
	expect_equal(x6$pi2, NA_real_)
	expect_equal(x6$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, 0.31861038, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, 0.72001941, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedPValues, c(0.49999905, 0.1020964, 0.0013103922, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalStage, 3)
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, 0.002378519, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.3437363, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.73847376, NA_real_), tolerance = 1e-07)
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.54446903, NA_real_), tolerance = 1e-07)
	expect_equal(x6$normalApproximation, TRUE)
	expect_equal(x6$directionUpper, TRUE)
	expect_equal(x6$combinationTestStatistics, c(1.6329932, 2.445695, 3.2262779, NA_real_), tolerance = 1e-07)

	x7 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = TRUE) 

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x7' with expected results
	##
	expect_equal(x7$stages, c(1, 2, 3, 4))
	expect_equal(x7$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x7$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x7$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x7$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x7$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x7$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$pValues, c(0.11381531, 0.078126907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x7$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x7$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x7$allocationRatioPlanned, 1)
	expect_equal(x7$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x7$pi2, NA_real_)
	expect_equal(x7$conditionalPower, c(NA_real_, NA_real_, 0.69921202, 0.88465983), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedPValues, c(0.49999905, 0.43799317, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$finalStage, NA_integer_)
	expect_equal(x7$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$normalApproximation, FALSE)
	expect_equal(x7$directionUpper, TRUE)
	expect_equal(x7$combinationTestStatistics, c(1.2064848, 1.8556383, NA_real_, NA_real_), tolerance = 1e-07)

	plotData3 <- testGetAnalysisResultsPlotData(x7, piRange = seq(0.25, 0.55, 0.05))

	##
	## Comparison of the results of list object 'plotData3' with expected results
	##
	expect_equal(plotData3$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData3$condPowerValues, c(0.15917802, 0.30888816, 0.48045808, 0.64581102, 0.78396384, 0.88465983, 0.94793138), tolerance = 1e-07)
	expect_equal(plotData3$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData3$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData3$xlab, "pi1")
	expect_equal(plotData3$ylab, "Conditional power / Likelihood")
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 18")

	x8 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = TRUE) 

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x8' with expected results
	##
	expect_equal(x8$stages, c(1, 2, 3, 4))
	expect_equal(x8$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x8$criticalValues, c(3.4542176, 2.9219298, 2.2448554, 2.0471908), tolerance = 1e-07)
	expect_equal(x8$futilityBounds, c(-0.5, 0, 0.5), tolerance = 1e-07)
	expect_equal(x8$alphaSpent, c(0.00027594593, 0.0019217991, 0.013384186, 0.025), tolerance = 1e-07)
	expect_equal(x8$stageLevels, c(0.00027594593, 0.00173935, 0.012388708, 0.020319679), tolerance = 1e-07)
	expect_equal(x8$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$testStatistics, c(1.6329932, 1.8257419, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$pValues, c(0.051235217, 0.033944577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x8$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x8$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x8$allocationRatioPlanned, 1)
	expect_equal(x8$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x8$pi2, NA_real_)
	expect_equal(x8$conditionalPower, c(NA_real_, NA_real_, 0.85385983, 0.95015898), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedPValues, c(0.49999905, 0.1020964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$finalStage, NA_integer_)
	expect_equal(x8$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$normalApproximation, TRUE)
	expect_equal(x8$directionUpper, TRUE)
	expect_equal(x8$combinationTestStatistics, c(1.6329932, 2.445695, NA_real_, NA_real_), tolerance = 1e-07)

	plotData4 <- testGetAnalysisResultsPlotData(x8, piRange = seq(0.25, 0.55, 0.05))

	##
	## Comparison of the results of list object 'plotData4' with expected results
	##
	expect_equal(plotData4$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData4$condPowerValues, c(0.32350374, 0.50248974, 0.66708346, 0.79901831, 0.89232288, 0.95015898, 0.98088099), tolerance = 1e-07)
	expect_equal(plotData4$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData4$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData4$xlab, "pi1")
	expect_equal(plotData4$ylab, "Conditional power / Likelihood")
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 18")

})

test_that("'getAnalysisResults' for a Fisher design and one treatment", {

	design3 <- getDesignFisher(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1))

	dataExample3 <- getDataset(
		n = c(8, 10, 9, 11), # cumsum, overall n = (8, 18, 27, 38)
		events = c(4, 5, 5, 6) # cumsum, overall events = (4, 9, 14, 20)
	)

	x1 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = FALSE, 
		directionUpper = FALSE, iterations = 1000, seed = 123)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(1, 1, 1))
	expect_equal(x1$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$pValues, c(0.11381531, 0.078126907, 0.16572571, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, 0.018233808, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, NA_real_)
	expect_equal(x1$pi2, NA_real_)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, 0.23521677, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, 0.79971589, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.23393398, 0.11483365, 0.11050779, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, FALSE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$combinationTestStatistics, c(0.11381531, 0.008892038, 0.00069992563, NA_real_), tolerance = 1e-07)

	.skipTestifDisabled()

	x2 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = TRUE, 
		directionUpper = FALSE, iterations = 1000, seed = 123)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(1, 1, 1))
	expect_equal(x2$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(-1.6329932, -1.8257419, -1.3471506, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.051235217, 0.033944577, 0.088965863, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x2$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.051264101, 0.1206033, 1, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, NA_real_)
	expect_equal(x2$pi2, NA_real_)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.18175814, 0.2424364, 0.28642867, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.81824186, 0.7575636, 0.7496417, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.11554509, 0.032131177, 0.024656293, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$combinationTestStatistics, c(0.051235217, 0.0017391578, 5.6795832e-05, NA_real_), tolerance = 1e-07)

	x3 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5, normalApproximation = FALSE, 
		directionUpper = FALSE, iterations = 1000, seed = 123) 

	##
	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2, 3, 4))
	expect_equal(x3$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(1, 1, 1))
	expect_equal(x3$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$pValues, c(0.11381531, 0.078126907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$thetaH0, 0.75, tolerance = 1e-07)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x3$allocationRatioPlanned, 1)
	expect_equal(x3$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x3$pi2, NA_real_)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.23393398, 0.11483365, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, FALSE)
	expect_equal(x3$directionUpper, FALSE)
	expect_equal(x3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.522, 0.672), tolerance = 1e-07)
	expect_equal(x3$combinationTestStatistics, c(0.11381531, 0.008892038, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x3, piRange = seq(0.25, 0.55, 0.05))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.9991, 0.988, 0.9637, 0.9018, 0.8082, 0.6784, 0.5175), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18")

	x4 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = FALSE, 
		directionUpper = TRUE, iterations = 1000, seed = 123)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x4' with expected results
	##
	expect_equal(x4$stages, c(1, 2, 3, 4))
	expect_equal(x4$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x4$futilityBounds, c(1, 1, 1))
	expect_equal(x4$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x4$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$pValues, c(0.11381531, 0.078126907, 0.048927307, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x4$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x4$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, 0.10237226, NA_real_), tolerance = 1e-07)
	expect_equal(x4$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$allocationRatioPlanned, 1)
	expect_equal(x4$pi1, NA_real_)
	expect_equal(x4$pi2, NA_real_)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, 0.23521677, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, 0.79971589, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.23393398, 0.11483365, 0.040061917, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$normalApproximation, FALSE)
	expect_equal(x4$directionUpper, TRUE)
	expect_equal(x4$combinationTestStatistics, c(0.11381531, 0.008892038, 0.00012466571, NA_real_), tolerance = 1e-07)

	x5 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = TRUE, 
		directionUpper = TRUE, iterations = 1000, seed = 123)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x5' with expected results
	##
	expect_equal(x5$stages, c(1, 2, 3, 4))
	expect_equal(x5$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x5$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x5$futilityBounds, c(1, 1, 1))
	expect_equal(x5$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x5$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x5$effectSizes, c(0.5, 0.5, 0.51851852, NA_real_), tolerance = 1e-07)
	expect_equal(x5$testStatistics, c(1.6329932, 1.8257419, 2.116951, NA_real_), tolerance = 1e-07)
	expect_equal(x5$pValues, c(0.051235217, 0.033944577, 0.017132004, NA_real_), tolerance = 1e-07)
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x5$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x5$conditionalRejectionProbabilities, c(0.051264101, 0.1206033, 1, NA_real_), tolerance = 1e-07)
	expect_equal(x5$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$allocationRatioPlanned, 1)
	expect_equal(x5$pi1, NA_real_)
	expect_equal(x5$pi2, NA_real_)
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.18175814, 0.2424364, 0.28642867, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.81824186, 0.7575636, 0.7496417, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedPValues, c(0.11554509, 0.032131177, 0.0055275316, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalStage, NA_integer_)
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$normalApproximation, TRUE)
	expect_equal(x5$directionUpper, TRUE)
	expect_equal(x5$combinationTestStatistics, c(0.051235217, 0.0017391578, 5.527981e-06, NA_real_), tolerance = 1e-07)

	x6 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, normalApproximation = FALSE, 
		directionUpper = TRUE, iterations = 1000, seed = 123) 

	##
	## Comparison of the results of AnalysisResultsFisher object 'x6' with expected results
	##
	expect_equal(x6$stages, c(1, 2, 3, 4))
	expect_equal(x6$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x6$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x6$futilityBounds, c(1, 1, 1))
	expect_equal(x6$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x6$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x6$effectSizes, c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$pValues, c(0.11381531, 0.078126907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x6$thetaH0, 0.25, tolerance = 1e-07)
	expect_equal(x6$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$nPlanned, c(NA_real_, NA_real_, 12, 6))
	expect_equal(x6$allocationRatioPlanned, 1)
	expect_equal(x6$pi1, 0.5, tolerance = 1e-07)
	expect_equal(x6$pi2, NA_real_)
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedPValues, c(0.23393398, 0.11483365, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalStage, NA_integer_)
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$normalApproximation, FALSE)
	expect_equal(x6$directionUpper, TRUE)
	expect_equal(x6$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.522, 0.672), tolerance = 1e-07)
	expect_equal(x6$combinationTestStatistics, c(0.11381531, 0.008892038, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x6, piRange = seq(0.25, 0.55, 0.05))

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.0422, 0.1045, 0.2175, 0.3551, 0.5294, 0.6784, 0.8077), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18")

})

test_that("'getAnalysisResults' produces the correct exact tests and final CIs", {

	.skipTestifDisabled()

	dataExample4 <- getDataset(
		n1 = c(10, 80),
		n2 = c(15, 100),
		events1 = c(8, 54),
		events2 = c(6, 45)
	)

	design4 <- getDesignGroupSequential(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x1 <- getAnalysisResults(design4, dataExample4, thetaH0 = 0, stage = 2, directionUpper = TRUE, 
		normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2))
	expect_equal(x1$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, -6)
	expect_equal(x1$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.4, 0.24541063), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(NA_real_, NA_real_))
	expect_equal(x1$pValues, c(0.057571679, 0.0019934481), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "reject"))
	expect_equal(x1$thetaH0, 0)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.093545976, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, NA_real_)
	expect_equal(x1$pi2, NA_real_)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.23265736, 0.10906229), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.78187776, 0.37165341), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.19076123, 0.00035290512), tolerance = 1e-07)
	expect_equal(x1$finalStage, 2)
	expect_equal(x1$finalPValues, c(NA_real_, 0.0010527587), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.08946214), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.35871085), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, 0.2258854), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, FALSE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$overallTestStatistics, c(1.5754901, 3.3869257), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.057571679, 0.00035340268), tolerance = 1e-07)

	design5 <- getDesignInverseNormal(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x2 <- getAnalysisResults(design5, dataExample4, thetaH0 = 0, stage = 2, directionUpper = TRUE, 
		normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2))
	expect_equal(x2$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, -6)
	expect_equal(x2$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.4, 0.24541063), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(NA_real_, NA_real_))
	expect_equal(x2$pValues, c(0.057571679, 0.0019934481), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "reject"))
	expect_equal(x2$thetaH0, 0)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.093545976, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, NA_real_)
	expect_equal(x2$pi2, NA_real_)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.23265736, 0.11944223), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.78187776, 0.38794979), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.19076123, 0.00053410288), tolerance = 1e-07)
	expect_equal(x2$finalStage, 2)
	expect_equal(x2$finalPValues, c(NA_real_, 0.0012242304), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.088125224), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.36146576), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, 0.2258396), tolerance = 1e-07)
	expect_equal(x2$normalApproximation, FALSE)
	expect_equal(x2$directionUpper, TRUE)
	expect_equal(x2$combinationTestStatistics, c(1.5754901, 3.2718402), tolerance = 1e-07)

	design6 <- getDesignFisher(kMax = 2, alpha = 0.025, method = "fullAlpha", 
		informationRates = c(0.3, 1))

	x3 <- getAnalysisResults(design6, dataExample4, thetaH0 = 0, stage = 2, directionUpper = TRUE, 
		normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2))
	expect_equal(x3$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(0.00076737019, 0.00076737019), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, 1)
	expect_equal(x3$alphaSpent, c(0.00076737019, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.00076737019, 0.025), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(0.4, 0.24541063), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(NA_real_, NA_real_))
	expect_equal(x3$pValues, c(0.057571679, 0.0019934481), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "reject"))
	expect_equal(x3$thetaH0, 0)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.059209424, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x3$allocationRatioPlanned, 1)
	expect_equal(x3$pi1, NA_real_)
	expect_equal(x3$pi2, NA_real_)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.22999653, 0.10478651), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.78083174, 0.37635443), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.33766302, 0.00088314698), tolerance = 1e-07)
	expect_equal(x3$finalStage, 2)
	expect_equal(x3$finalPValues, c(NA_real_, 0.0015832283), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, FALSE)
	expect_equal(x3$directionUpper, TRUE)
	expect_equal(x3$combinationTestStatistics, c(0.057571679, 4.3180464e-06), tolerance = 1e-07)

})

context("Testing the analysis rates functionality for two treatments")


test_that("'getAnalysisResults' for a group sequential design and two treatments", {
	.skipTestifDisabled()

	design7 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
			typeOfDesign = "WT", deltaWT = 0.25, informationRates = c(0.2, 0.4, 0.8, 1), 
			futilityBounds = c(0, 0.5, 0.8), bindingFutility = T)

	dataExample5 <- getDataset(
		n1 = c(17, 23, 22),
		n2 = c(18, 20, 19),
		events1 = c(11, 12, 17),
		events2 = c(5, 10, 7)
	)

	x1 <- getAnalysisResults(design7, dataExample5, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.0522063, 2.5665893, 2.1582358, 2.0411334), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x1$alphaSpent, c(0.0011358297, 0.0058207373, 0.017395547, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.0011358297, 0.0051352086, 0.015454753, 0.020618787), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.014194417, 0.44344359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$thetaH0, 0)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.19002543, 0.11795654, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, 60, 30))
	expect_equal(x1$allocationRatioPlanned, 2)
	expect_equal(x1$pi1, 0.8, tolerance = 1e-07)
	expect_equal(x1$pi2, 0.4, tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.95912173, 0.99561789), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.10866984, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.44094175, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.083297284, 0.1166436, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$overallTestStatistics, c(2.1918708, 1.5920411, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.014194417, 0.055687735, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x1, piRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)  			

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.39134298, 0.56404834, 0.72892392, 0.85852169, 0.94050627, 0.98087239, 0.99561789), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883, 0.081561833, 0.015871623), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2")

	# reversed "directionUpper"

	x2 <- getAnalysisResults(design7, dataExample5, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.0522063, 2.5665893, 2.1582358, 2.0411334), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.0011358297, 0.0058207373, 0.017395547, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0011358297, 0.0051352086, 0.015454753, 0.020618787), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.98580558, 0.55655641, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("accept and stop", "accept and stop", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 0)
	expect_equal(x2$conditionalRejectionProbabilities, c(0, 0, NA_real_, NA_real_))
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 60, 30))
	expect_equal(x2$allocationRatioPlanned, 0.5, tolerance = 1e-07)
	expect_equal(x2$pi1, 0.4, tolerance = 1e-07)
	expect_equal(x2$pi2, 0.8, tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 6.6613381e-16, 6.6613381e-16), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.10866984, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.44094175, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.21185459, 0.21185459, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, 1)
	expect_equal(x2$finalPValues, c(0.98580558, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(0.03932898, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(0.62730993, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(0.36928105, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$overallTestStatistics, c(2.1918708, 1.5920411, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$overallPValues, c(0.98580558, 0.94431227, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x2, piRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)  				

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 1, 1), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(1.003982e-05, 0.00017609247, 0.0020513476, 0.015871623, 0.081561833, 0.27837883, 0.63105765), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5")

})

test_that("'getAnalysisResults' for an inverse design and two treatments", {

	.skipTestifDisabled()

	design8 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
			typeOfDesign = "WT", deltaWT = 0.25, informationRates = c(0.2, 0.4, 0.8, 1), 
			futilityBounds = c(0, 0.5, 0.8), bindingFutility = T)

	dataExample6 <- getDataset(
		n1 = c(17, 23, 22),
		n2 = c(18, 20, 19),
		events1 = c(11, 12, 17),
		events2 = c(5, 10, 7)
	)

	x1 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0.0, stage = 2,  nPlanned = c(30,30), 
		pi2 = 0.2, pi1 = 0.4, directionUpper = T)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.0522063, 2.5665893, 2.1582358, 2.0411334), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x1$alphaSpent, c(0.0011358297, 0.0058207373, 0.017395547, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.0011358297, 0.0051352086, 0.015454753, 0.020618787), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.014194417, 0.44344359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$thetaH0, 0)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.19002543, 0.12887611, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, 30, 30))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, 0.4, tolerance = 1e-07)
	expect_equal(x1$pi2, 0.2, tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.42200962, 0.67359244), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.10230475, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.44728185, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.083297284, 0.10825489, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$combinationTestStatistics, c(2.1918708, 1.6504685, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x1, piRange = seq(0.4, 0.7, 0.05), nPlanned = c(30,30))

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.67359244, 0.79683049, 0.88832834, 0.9471853, 0.97926376, 0.99357391, 0.99853781), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.081561833, 0.27837883, 0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1")

	x2 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0.0, stage = 2,  nPlanned = c(30,30), 
		pi2 = 0.2, pi1 = 0.4, directionUpper = T)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.0522063, 2.5665893, 2.1582358, 2.0411334), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.0011358297, 0.0058207373, 0.017395547, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0011358297, 0.0051352086, 0.015454753, 0.020618787), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.014194417, 0.44344359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 0)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.19002543, 0.12887611, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 30, 30))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, 0.4, tolerance = 1e-07)
	expect_equal(x2$pi2, 0.2, tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.42200962, 0.67359244), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.10230475, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.44728185, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.083297284, 0.10825489, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, TRUE)
	expect_equal(x2$combinationTestStatistics, c(2.1918708, 1.6504685, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x2, piRange = seq(0.4, 0.7, 0.05), nPlanned = c(30, 30))

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.67359244, 0.79683049, 0.88832834, 0.9471853, 0.97926376, 0.99357391, 0.99853781), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.081561833, 0.27837883, 0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1")

	plotData3 <- testGetAnalysisResultsPlotData(x2, piRange = seq(0.4, 0.7, 0.05))

	##
	## Comparison of the results of list object 'plotData3' with expected results
	##
	expect_equal(plotData3$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData3$condPowerValues, c(0.67359244, 0.79683049, 0.88832834, 0.9471853, 0.97926376, 0.99357391, 0.99853781), tolerance = 1e-07)
	expect_equal(plotData3$likelihoodValues, c(0.081561833, 0.27837883, 0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883), tolerance = 1e-07)
	expect_equal(plotData3$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData3$xlab, "pi1")
	expect_equal(plotData3$ylab, "Conditional power / Likelihood")
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1")

	x3 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2, 3, 4))
	expect_equal(x3$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(3.0522063, 2.5665893, 2.1582358, 2.0411334), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x3$alphaSpent, c(0.0011358297, 0.0058207373, 0.017395547, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.0011358297, 0.0051352086, 0.015454753, 0.020618787), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$pValues, c(0.014194417, 0.44344359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$thetaH0, 0)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.19002543, 0.12887611, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_, 60, 30))
	expect_equal(x3$allocationRatioPlanned, 2)
	expect_equal(x3$pi1, 0.8, tolerance = 1e-07)
	expect_equal(x3$pi2, 0.4, tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.95920074, 0.9956319), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.10230475, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.44728185, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.083297284, 0.10825489, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, TRUE)
	expect_equal(x3$directionUpper, TRUE)
	expect_equal(x3$combinationTestStatistics, c(2.1918708, 1.6504685, NA_real_, NA_real_), tolerance = 1e-07)

	plotData4 <- testGetAnalysisResultsPlotData(x3, piRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)  			

	##
	## Comparison of the results of list object 'plotData4' with expected results
	##
	expect_equal(plotData4$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07)
	expect_equal(plotData4$condPowerValues, c(0.39156853, 0.5643629, 0.72924187, 0.85876179, 0.94064025, 0.98092555, 0.9956319), tolerance = 1e-07)
	expect_equal(plotData4$likelihoodValues, c(0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883, 0.081561833, 0.015871623), tolerance = 1e-07)
	expect_equal(plotData4$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData4$xlab, "pi1")
	expect_equal(plotData4$ylab, "Conditional power / Likelihood")
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2")

	# reversed "directionUpper"

	x4 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x4' with expected results
	##
	expect_equal(x4$stages, c(1, 2, 3, 4))
	expect_equal(x4$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(3.0522063, 2.5665893, 2.1582358, 2.0411334), tolerance = 1e-07)
	expect_equal(x4$futilityBounds, c(0, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x4$alphaSpent, c(0.0011358297, 0.0058207373, 0.017395547, 0.025), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.0011358297, 0.0051352086, 0.015454753, 0.020618787), tolerance = 1e-07)
	expect_equal(x4$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$pValues, c(0.98580558, 0.55655641, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testActions, c("accept and stop", "accept and stop", NA_character_, NA_character_))
	expect_equal(x4$thetaH0, 0)
	expect_equal(x4$conditionalRejectionProbabilities, c(0, 0, NA_real_, NA_real_))
	expect_equal(x4$nPlanned, c(NA_real_, NA_real_, 60, 30))
	expect_equal(x4$allocationRatioPlanned, 0.5, tolerance = 1e-07)
	expect_equal(x4$pi1, 0.4, tolerance = 1e-07)
	expect_equal(x4$pi2, 0.8, tolerance = 1e-07)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 6.6613381e-16, 6.6613381e-16), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.10230475, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.44728185, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.21185459, 0.21185459, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, 1)
	expect_equal(x4$finalPValues, c(0.98580558, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(0.03932898, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(0.62730993, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$medianUnbiasedEstimates, c(0.36928105, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$normalApproximation, TRUE)
	expect_equal(x4$directionUpper, FALSE)
	expect_equal(x4$combinationTestStatistics, c(-2.1918708, -1.6504685, NA_real_, NA_real_), tolerance = 1e-07)

	plotData5 <- testGetAnalysisResultsPlotData(x4, piRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)  		

	##
	## Comparison of the results of list object 'plotData5' with expected results
	##
	expect_equal(plotData5$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07)
	expect_equal(plotData5$condPowerValues, c(6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 1, 1), tolerance = 1e-07)
	expect_equal(plotData5$likelihoodValues, c(1.003982e-05, 0.00017609247, 0.0020513476, 0.015871623, 0.081561833, 0.27837883, 0.63105765), tolerance = 1e-07)
	expect_equal(plotData5$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData5$xlab, "pi1")
	expect_equal(plotData5$ylab, "Conditional power / Likelihood")
	expect_equal(plotData5$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5")

})

test_that("'getAnalysisResults' for a Fisher design and two treatments", {

	.skipTestifDisabled()

	design9 <- getDesignFisher(kMax = 4, alpha = 0.025, method = "equalAlpha", 
		informationRates = c(0.2, 0.4, 0.8, 1))

	dataExample7 <- getDataset(
			n1 = c(17, 23, 22),
			n2 = c(18, 20, 19),
			events1 = c(11, 12, 17),
			events2 = c(5, 10, 7)
	)

	x1 <- getAnalysisResults(design9, dataExample7, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2, 3, 4))
	expect_equal(x1$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, c(1, 1, 1))
	expect_equal(x1$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.014194417, 0.44344359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$thetaH0, 0)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.13898608, 0.050808351, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_, 60, 30))
	expect_equal(x1$allocationRatioPlanned, 2)
	expect_equal(x1$pi1, 0.8, tolerance = 1e-07)
	expect_equal(x1$pi2, 0.4, tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.023853561, -0.068378457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.66428984, 0.40418869, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.035427069, 0.088523734, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.9129, 0.9784), tolerance = 1e-07)
	expect_equal(x1$combinationTestStatistics, c(0.014194417, 0.0062944232, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x1, piRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)  			

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.2057, 0.3564, 0.5056, 0.689, 0.8294, 0.9263, 0.9766), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883, 0.081561833, 0.015871623), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2")

	# reversed "directionUpper"

	x2 <- getAnalysisResults(design9, dataExample7, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2, 3, 4))
	expect_equal(x2$informationRates, c(0.2, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(0.00979594, 0.001278498, 5.8066063e-05, 1.276231e-05), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(1, 1, 1))
	expect_equal(x2$alphaSpent, c(0.00979594, 0.01571, 0.022184266, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00979594, 0.00979594, 0.00979594, 0.00979594), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.36928105, 0.18026316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(2.1918708, 0.14224412, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.98580558, 0.55655641, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$thetaH0, 0)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.0056634595, 0.0023089469, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_, 60, 30))
	expect_equal(x2$allocationRatioPlanned, 0.5, tolerance = 1e-07)
	expect_equal(x2$pi1, 0.4, tolerance = 1e-07)
	expect_equal(x2$pi2, 0.8, tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.023853561, -0.068378457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.66428984, 0.40418869, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.5638, 0.7946), tolerance = 1e-07)
	expect_equal(x2$combinationTestStatistics, c(0.98580558, 0.54865641, NA_real_, NA_real_), tolerance = 1e-07)

	plotData2 <- testGetAnalysisResultsPlotData(x2,piRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5) 				

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.9986, 0.9908, 0.9652, 0.9014, 0.7908, 0.6483, 0.47), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(1.003982e-05, 0.00017609247, 0.0020513476, 0.015871623, 0.081561833, 0.27837883, 0.63105765), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5")

})

test_that("'getAnalysisResults' produces the correct exact tests and final CIs", {

	.skipTestifDisabled()

	dataExample8 <- getDataset(
		n2 = c(10, 80),
		n1 = c(15, 100),
		events2 = c(8, 54),
		events1 = c(6, 45)
	)

	design10 <- getDesignGroupSequential(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x1 <- getAnalysisResults(design10, dataExample8, thetaH0 = 0, stage = 2, directionUpper = FALSE, 
		normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2))
	expect_equal(x1$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, -6)
	expect_equal(x1$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(-0.4, -0.24541063), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(NA_real_, NA_real_))
	expect_equal(x1$pValues, c(0.057571679, 0.0019934481), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "reject"))
	expect_equal(x1$thetaH0, 0)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.093545976, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, NA_real_)
	expect_equal(x1$pi2, NA_real_)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.78187776, -0.37165341), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.23265736, -0.10906229), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.19076123, 0.00035290512), tolerance = 1e-07)
	expect_equal(x1$finalStage, 2)
	expect_equal(x1$finalPValues, c(NA_real_, 0.0010527587), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.35871085), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, -0.08946214), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, -0.2258854), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, FALSE)
	expect_equal(x1$directionUpper, FALSE)
	expect_equal(x1$overallTestStatistics, c(1.5754901, 3.3869257), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.057571679, 0.00035340268), tolerance = 1e-07)

	design11 <- getDesignInverseNormal(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x2 <- getAnalysisResults(design11, dataExample8, thetaH0 = 0, stage = 2, directionUpper = FALSE, 
		normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2))
	expect_equal(x2$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, -6)
	expect_equal(x2$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(-0.4, -0.24541063), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(NA_real_, NA_real_))
	expect_equal(x2$pValues, c(0.057571679, 0.0019934481), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "reject"))
	expect_equal(x2$thetaH0, 0)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.093545976, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, NA_real_)
	expect_equal(x2$pi2, NA_real_)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.78187776, -0.38794979), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.23265736, -0.11944223), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.19076123, 0.00053410288), tolerance = 1e-07)
	expect_equal(x2$finalStage, 2)
	expect_equal(x2$finalPValues, c(NA_real_, 0.0012242304), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.36146576), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, -0.088125224), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, -0.2258396), tolerance = 1e-07)
	expect_equal(x2$normalApproximation, FALSE)
	expect_equal(x2$directionUpper, FALSE)
	expect_equal(x2$combinationTestStatistics, c(1.5754901, 3.2718402), tolerance = 1e-07)

	design12 <- getDesignFisher(kMax = 2, alpha = 0.025, method = "fullAlpha", 
		informationRates = c(0.3, 1))

	x3 <- getAnalysisResults(design12, dataExample8, thetaH0 = 0, stage = 2, directionUpper = FALSE, 
		normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2))
	expect_equal(x3$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(0.00076737019, 0.00076737019), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, 1)
	expect_equal(x3$alphaSpent, c(0.00076737019, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.00076737019, 0.025), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(-0.4, -0.24541063), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(NA_real_, NA_real_))
	expect_equal(x3$pValues, c(0.057571679, 0.0019934481), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "reject"))
	expect_equal(x3$thetaH0, 0)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.059209424, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x3$allocationRatioPlanned, 1)
	expect_equal(x3$pi1, NA_real_)
	expect_equal(x3$pi2, NA_real_)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.78083174, -0.37635443), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.22999653, -0.10478651), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.33766302, 0.00088314698), tolerance = 1e-07)
	expect_equal(x3$finalStage, 2)
	expect_equal(x3$finalPValues, c(NA_real_, 0.0015832283), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	expect_equal(x3$normalApproximation, FALSE)
	expect_equal(x3$directionUpper, FALSE)
	expect_equal(x3$combinationTestStatistics, c(0.057571679, 4.3180464e-06), tolerance = 1e-07)

})

test_that("'getAnalysisResults' produes the correct non-inferiority results for a group sequential design", {

	.skipTestifDisabled()

	design13 <- getDesignGroupSequential(kMax = 2, alpha = 0.025, 
			typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	dataExample9 <- getDataset(
			n1 = c(10, 80),
			n2 = c(15, 100),
			events1 = c(8, 54),
			events2 = c(6, 45)
	)

	x1 <- getAnalysisResults(design13, dataExample9, thetaH0 = -0.1, stage = 2, directionUpper = TRUE, 
		normalApproximation = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	##
	expect_equal(x1$stages, c(1, 2))
	expect_equal(x1$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x1$futilityBounds, -6)
	expect_equal(x1$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x1$effectSizes, c(0.4, 0.24541063), tolerance = 1e-07)
	expect_equal(x1$testStatistics, c(2.4676423, 4.366446), tolerance = 1e-07)
	expect_equal(x1$pValues, c(0.0068003075, 6.3142236e-06), tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "reject"))
	expect_equal(x1$thetaH0, -0.1, tolerance = 1e-07)
	expect_equal(x1$conditionalRejectionProbabilities, c(0.2311149, NA_real_), tolerance = 1e-07)
	expect_equal(x1$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x1$allocationRatioPlanned, 1)
	expect_equal(x1$pi1, NA_real_)
	expect_equal(x1$pi2, NA_real_)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.23265736, 0.10906229), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.78187776, 0.37165341), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.067077549, 1.9536724e-06), tolerance = 1e-07)
	expect_equal(x1$finalStage, 2)
	expect_equal(x1$finalPValues, c(NA_real_, 0.00072814991), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.046389254), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.3577016), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, 0.2183453), tolerance = 1e-07)
	expect_equal(x1$normalApproximation, TRUE)
	expect_equal(x1$directionUpper, TRUE)
	expect_equal(x1$overallTestStatistics, c(2.4676423, 4.9460155), tolerance = 1e-07)
	expect_equal(x1$overallPValues, c(0.0068003075, 3.7873958e-07), tolerance = 1e-07)

	x2 <- getAnalysisResults(design13, dataExample9, thetaH0 = -0.1, stage = 1, nPlanned = 40, 
		pi1 = 0.45, pi2 = 0.4, directionUpper = TRUE, normalApproximation = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	##
	expect_equal(x2$stages, c(1, 2))
	expect_equal(x2$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, -6)
	expect_equal(x2$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x2$effectSizes, c(0.4, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testStatistics, c(2.4676423, NA_real_), tolerance = 1e-07)
	expect_equal(x2$pValues, c(0.0068003075, NA_real_), tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", NA_character_))
	expect_equal(x2$thetaH0, -0.1, tolerance = 1e-07)
	expect_equal(x2$conditionalRejectionProbabilities, c(0.2311149, NA_real_), tolerance = 1e-07)
	expect_equal(x2$nPlanned, c(NA_real_, 40))
	expect_equal(x2$allocationRatioPlanned, 1)
	expect_equal(x2$pi1, 0.45, tolerance = 1e-07)
	expect_equal(x2$pi2, 0.4, tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, 0.59014508), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.23265736, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.78187776, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.067077549, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	expect_equal(x2$normalApproximation, TRUE)
	expect_equal(x2$directionUpper, TRUE)
	expect_equal(x2$overallTestStatistics, c(2.4676423, NA_real_), tolerance = 1e-07)
	expect_equal(x2$overallPValues, c(0.0068003075, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetAnalysisResultsPlotData(x2, piRange = seq(0.25, 0.7, 0.05)) 

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.13978279, 0.2311149, 0.34247666, 0.46554605, 0.59014508, 0.70618885, 0.80546789, 0.88295965, 0.937434, 0.97121381), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(7.8444044e-05, 0.00040464517, 0.0017853782, 0.006737947, 0.021750359, 0.060054668, 0.14183016, 0.2865048, 0.4950359, 0.73161563), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 1, # of remaining subjects = 40, pi2 = 0.4, allocation ratio = 1")

	# non-inferiority, reversed "directionUpper"

	x3 <- getAnalysisResults(design13, dataExample9, thetaH0 = 0.1, stage = 2, directionUpper = FALSE, 
		normalApproximation = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	##
	expect_equal(x3$stages, c(1, 2))
	expect_equal(x3$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, -6)
	expect_equal(x3$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x3$effectSizes, c(0.4, 0.24541063), tolerance = 1e-07)
	expect_equal(x3$testStatistics, c(1.4985437, 1.6883572), tolerance = 1e-07)
	expect_equal(x3$pValues, c(0.93300397, 0.95432866), tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "accept"))
	expect_equal(x3$thetaH0, 0.1, tolerance = 1e-07)
	expect_equal(x3$conditionalRejectionProbabilities, c(0.00043165085, NA_real_), tolerance = 1e-07)
	expect_equal(x3$nPlanned, c(NA_real_, NA_real_))
	expect_equal(x3$allocationRatioPlanned, 1)
	expect_equal(x3$pi1, NA_real_)
	expect_equal(x3$pi2, NA_real_)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.23265736, 0.10906229), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.78187776, 0.37165341), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.49999905), tolerance = 1e-07)
	expect_equal(x3$finalStage, 2)
	expect_equal(x3$finalPValues, c(NA_real_, 0.9819019), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.10906703), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.37282539), tolerance = 1e-07)
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, 0.24094623), tolerance = 1e-07)
	expect_equal(x3$normalApproximation, TRUE)
	expect_equal(x3$directionUpper, FALSE)
	expect_equal(x3$overallTestStatistics, c(1.4985437, 2.0947166), tolerance = 1e-07)
	expect_equal(x3$overallPValues, c(0.93300397, 0.9819019), tolerance = 1e-07)

	x4 <- getAnalysisResults(design13, dataExample9, thetaH0 = 0.1, stage = 1, nPlanned = 40, 
		pi1 = 0.4, pi2 = 0.45, directionUpper = FALSE, normalApproximation = TRUE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'x4' with expected results
	##
	expect_equal(x4$stages, c(1, 2))
	expect_equal(x4$informationRates, c(0.3, 1), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(3.1833546, 1.9666792), tolerance = 1e-07)
	expect_equal(x4$futilityBounds, -6)
	expect_equal(x4$alphaSpent, c(0.00072789603, 0.025), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.00072789603, 0.024610104), tolerance = 1e-07)
	expect_equal(x4$effectSizes, c(0.4, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testStatistics, c(1.4985437, NA_real_), tolerance = 1e-07)
	expect_equal(x4$pValues, c(0.93300397, NA_real_), tolerance = 1e-07)
	expect_equal(x4$testActions, c("continue", NA_character_))
	expect_equal(x4$thetaH0, 0.1, tolerance = 1e-07)
	expect_equal(x4$conditionalRejectionProbabilities, c(0.00043165085, NA_real_), tolerance = 1e-07)
	expect_equal(x4$nPlanned, c(NA_real_, 40))
	expect_equal(x4$allocationRatioPlanned, 1)
	expect_equal(x4$pi1, 0.4, tolerance = 1e-07)
	expect_equal(x4$pi2, 0.45, tolerance = 1e-07)
	expect_equal(x4$conditionalPower, c(NA_real_, 0.009129264), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(-0.23265736, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.78187776, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.49999905, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	expect_equal(x4$normalApproximation, TRUE)
	expect_equal(x4$directionUpper, FALSE)
	expect_equal(x4$overallTestStatistics, c(1.4985437, NA_real_), tolerance = 1e-07)
	expect_equal(x4$overallPValues, c(0.93300397, NA_real_), tolerance = 1e-07)

})

