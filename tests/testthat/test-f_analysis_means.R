######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 11 September 2019, 16:24:16                                                  #
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

context("Testing the analysis means functionality for one treatment")


test_that("'getAnalysisResults' for group sequential design and a dataset of one mean per stage (bindingFutility = TRUE)", {
	dataExample1 <- getDataset(
		n = c(20, 30, 30),
		means = c(0.45, 0.51, 0.45) * 100,
		stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design1 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults1 <- getStageResults(design1, dataExample1, thetaH0 = 10, stage = 2)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	##
	expect_equal(stageResults1$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(45, 48.6, 47.25), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(130, 134.76601, 128.66279), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(20, 50))
	expect_equal(stageResults1$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)

	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	plotData1 <- testGetStageResultsPlotData(stageResults1, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData1$condPowerValues, c(0.20492816, 0.31007642, 0.43512091, 0.5683138, 0.6950205, 0.80243295, 0.88343665, 0.93770927, 0.96998259, 0.98700232, 0.99495733, 0.99825113, 0.99945881, 0.9998508, 0.9999634), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "Effect size")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 50, std = 100")

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result1 <- getAnalysisResults(design = design1, dataInput = dataExample1, 
				nPlanned = 30, thetaH1 = 50, assumedStDev = 100, thetaH0 = 10)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	##
	expect_equal(result1$stages, c(1, 2, 3, 4))
	expect_equal(result1$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result1$criticalValues, c(2.4958485, 2.328709, 2.2361766, 2.1727623), tolerance = 1e-07)
	expect_equal(result1$futilityBounds, c(0.5244, 0.5244, 0.5244), tolerance = 1e-07)
	expect_equal(result1$alphaSpent, c(0.0062828133, 0.013876673, 0.02015684, 0.025), tolerance = 1e-07)
	expect_equal(result1$stageLevels, c(0.0062828133, 0.0099372444, 0.012670104, 0.014899106), tolerance = 1e-07)
	expect_equal(result1$effectSizes, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(result1$testStatistics, c(1.2040366, 1.6040446, 1.5975241, NA_real_), tolerance = 1e-07)
	expect_equal(result1$pValues, c(0.12168078, 0.059770605, 0.060494785, NA_real_), tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result1$thetaH0, 10)
	expect_equal(result1$thetaH1, 50)
	expect_equal(result1$assumedStDev, 100)
	expect_equal(result1$conditionalRejectionProbabilities, c(0.054544013, 0.20492816, 0.51388905, NA_real_), tolerance = 1e-07)
	expect_equal(result1$nPlanned, c(NA_real_, NA_real_, NA_real_, 30))
	expect_equal(result1$allocationRatioPlanned, 1)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.98698326), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-35.118855, 2.7165066, 14.460593, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(125.11886, 94.483493, 80.039407, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.18164628, 0.051710415, 0.012085356, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, 3)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, 0.016174263, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 13.147855, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 77.032138, NA_real_), tolerance = 1e-07)
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 44.987662, NA_real_), tolerance = 1e-07)
	expect_equal(result1$normalApproximation, FALSE)
	expect_equal(result1$equalVariances, TRUE)
	expect_equal(result1$directionUpper, TRUE)
	expect_equal(result1$overallTestStatistics, c(1.2040366, 2.025312, 2.5895142, NA_real_), tolerance = 1e-07)
	expect_equal(result1$overallPValues, c(0.12168078, 0.02415027, 0.0057194973, NA_real_), tolerance = 1e-07)

})

test_that("'getAnalysisResults' for inverse normal and Fisher designs and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestifDisabled()

	dataExample1 <- getDataset(
		n = c(20, 30, 30),
		means = c(0.45, 0.51, 0.45) * 100,
		stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design2 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults2 <- getStageResults(design2, dataExample1, thetaH0 = 10, stage = 2)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	##
	expect_equal(stageResults2$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans, c(45, 48.6, 47.25), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs, c(130, 134.76601, 128.66279), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes, c(20, 50))
	expect_equal(stageResults2$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(1.1666257, 1.9256836, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)

	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	plotData2 <- testGetStageResultsPlotData(stageResults2, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData2$condPowerValues, c(0.18776792, 0.28883478, 0.41147918, 0.5447391, 0.67401995, 0.78575942, 0.87165951, 0.93031941, 0.96586805, 0.98497137, 0.99406923, 0.99790729, 0.999341, 0.99981509, 0.99995383), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "Effect size")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 50, std = 100")

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result2 <- getAnalysisResults(design = design2, dataInput = dataExample1, 
			nPlanned = 30, thetaH1 = 50, assumedStDev = 100, thetaH0 = 10)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	##
	expect_equal(result2$stages, c(1, 2, 3, 4))
	expect_equal(result2$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result2$criticalValues, c(2.4958485, 2.328709, 2.2361766, 2.1727623), tolerance = 1e-07)
	expect_equal(result2$futilityBounds, c(0.5244, 0.5244, 0.5244), tolerance = 1e-07)
	expect_equal(result2$alphaSpent, c(0.0062828133, 0.013876673, 0.02015684, 0.025), tolerance = 1e-07)
	expect_equal(result2$stageLevels, c(0.0062828133, 0.0099372444, 0.012670104, 0.014899106), tolerance = 1e-07)
	expect_equal(result2$effectSizes, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(result2$testStatistics, c(1.2040366, 1.6040446, 1.5975241, NA_real_), tolerance = 1e-07)
	expect_equal(result2$pValues, c(0.12168078, 0.059770605, 0.060494785, NA_real_), tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result2$thetaH0, 10)
	expect_equal(result2$thetaH1, 50)
	expect_equal(result2$assumedStDev, 100)
	expect_equal(result2$conditionalRejectionProbabilities, c(0.054544013, 0.18776792, 0.47147471, NA_real_), tolerance = 1e-07)
	expect_equal(result2$nPlanned, c(NA_real_, NA_real_, NA_real_, 30))
	expect_equal(result2$allocationRatioPlanned, 1)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.98296857), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-35.118855, 1.5735511, 13.58964, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(125.11886, 94.865725, 80.385626, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.18164628, 0.056608473, 0.014183052, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, 3)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, 0.016754234, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 13.011822, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 82.848073, NA_real_), tolerance = 1e-07)
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 45.714272, NA_real_), tolerance = 1e-07)
	expect_equal(result2$normalApproximation, FALSE)
	expect_equal(result2$equalVariances, TRUE)
	expect_equal(result2$directionUpper, TRUE)
	expect_equal(result2$combinationTestStatistics, c(1.1666257, 1.9256836, 2.4675727, NA_real_), tolerance = 1e-07)

	design3 <- getDesignFisher(kMax = 4, alpha = 0.025, alpha0Vec = rep(0.4, 3), bindingFutility = TRUE)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults3 <- getStageResults(design3, dataExample1, thetaH0 = 10, stage = 2)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	##
	expect_equal(stageResults3$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(45, 48.6, 47.25), tolerance = 1e-07)
	expect_equal(stageResults3$overallStDevs, c(130, 134.76601, 128.66279), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(20, 50))
	expect_equal(stageResults3$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$combFisher, c(0.12168078, 0.007272934, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1, 1, 1))

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result3 <- getAnalysisResults(design = design3, dataInput = dataExample1, thetaH0 = 10, 
		nPlanned = 30, thetaH1 = 50, assumedStDev = 100)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	##
	expect_equal(result3$stages, c(1, 2, 3, 4))
	expect_equal(result3$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result3$criticalValues, c(0.013928445, 0.0019196833, 0.00034092609, 6.8425459e-05), tolerance = 1e-07)
	expect_equal(result3$futilityBounds, c(0.4, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(result3$alphaSpent, c(0.013928445, 0.020373842, 0.0235151, 0.025), tolerance = 1e-07)
	expect_equal(result3$stageLevels, c(0.013928445, 0.013928445, 0.013928445, 0.013928445), tolerance = 1e-07)
	expect_equal(result3$effectSizes, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(result3$testStatistics, c(1.2040366, 1.6040446, 1.5975241, NA_real_), tolerance = 1e-07)
	expect_equal(result3$pValues, c(0.12168078, 0.059770605, 0.060494785, NA_real_), tolerance = 1e-07)
	expect_equal(result3$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result3$thetaH0, 10)
	expect_equal(result3$thetaH1, 50)
	expect_equal(result3$assumedStDev, 100)
	expect_equal(result3$conditionalRejectionProbabilities, c(0.029249394, 0.067046868, 0.15552139, NA_real_), tolerance = 1e-07)
	expect_equal(result3$nPlanned, c(NA_real_, NA_real_, NA_real_, 30))
	expect_equal(result3$allocationRatioPlanned, 1)
	expect_equal(result3$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.88057256), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-24.226675, 0.014834887, 8.7947814, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(114.22668, 96.713521, 85.125684, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.165096, 0.068572907, 0.029926287, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$normalApproximation, FALSE)
	expect_equal(result3$equalVariances, TRUE)
	expect_equal(result3$directionUpper, TRUE)
	expect_equal(result3$combinationTestStatistics, c(0.12168078, 0.007272934, 0.00043997458, NA_real_), tolerance = 1e-07)

})

test_that("'getAnalysisResults' for different designs and a dataset of one mean per stage (bindingFutility = FALSE)", {

	.skipTestifDisabled()

	dataExample2 <- getDataset(
		n = c(20, 30, 30),
		means = c(0.45, 0.51, 0.45) * 100,
		stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design4 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	stageResults1 <- getStageResults(design4, dataExample2, thetaH0 = 10, stage = 2)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	##
	expect_equal(stageResults1$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(45, 48.6, 47.25), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(130, 134.76601, 128.66279), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(20, 50))
	expect_equal(stageResults1$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)

	plotData1 <- testGetStageResultsPlotData(stageResults1, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	##
	## Comparison of the results of list object 'plotData1' with expected results
	##
	expect_equal(plotData1$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData1$condPowerValues, c(0.17749108, 0.27572975, 0.39647686, 0.52937537, 0.65998377, 0.77434444, 0.86340967, 0.9250277, 0.96285863, 0.98345513, 0.99339288, 0.99764031, 0.99924778, 0.9997863, 0.99994597), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData1$xlab, "Effect size")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 50, std = 100")

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result1 <- getAnalysisResults(design = design4, dataInput = dataExample2, thetaH0 = 10)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	##
	expect_equal(result1$stages, c(1, 2, 3, 4))
	expect_equal(result1$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result1$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result1$futilityBounds, c(-6, -6, -6))
	expect_equal(result1$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result1$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result1$effectSizes, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(result1$testStatistics, c(1.2040366, 1.6040446, 1.5975241, NA_real_), tolerance = 1e-07)
	expect_equal(result1$pValues, c(0.12168078, 0.059770605, 0.060494785, NA_real_), tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result1$thetaH0, 10)
	expect_equal(result1$thetaH1, 47.25, tolerance = 1e-07)
	expect_equal(result1$assumedStDev, 128.66279, tolerance = 1e-07)
	expect_equal(result1$conditionalRejectionProbabilities, c(0.046837862, 0.17749108, 0.46585158, NA_real_), tolerance = 1e-07)
	expect_equal(result1$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$allocationRatioPlanned, 1)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-37.7517, 1.3684534, 13.520683, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(127.7517, 95.831547, 80.979317, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.28074785, 0.063917079, 0.013597508, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, 3)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, 0.014875116, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 13.605112, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 73.306215, NA_real_), tolerance = 1e-07)
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 44.286284, NA_real_), tolerance = 1e-07)
	expect_equal(result1$normalApproximation, FALSE)
	expect_equal(result1$equalVariances, TRUE)
	expect_equal(result1$directionUpper, TRUE)
	expect_equal(result1$overallTestStatistics, c(1.2040366, 2.025312, 2.5895142, NA_real_), tolerance = 1e-07)
	expect_equal(result1$overallPValues, c(0.12168078, 0.02415027, 0.0057194973, NA_real_), tolerance = 1e-07)

	design5 <- getDesignInverseNormal(kMax = 4, alpha = 0.025,  typeOfDesign = "WT", deltaWT = 0.4)

	##
	## Comparison of the results of TrialDesignInverseNormal object 'design5' with expected results
	##
	expect_equal(design5$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(design5$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(design5$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)

	stageResults2 <- getStageResults(design5, dataExample2, thetaH0 = 10, stage = 2)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	##
	expect_equal(stageResults2$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans, c(45, 48.6, 47.25), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs, c(130, 134.76601, 128.66279), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes, c(20, 50))
	expect_equal(stageResults2$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(1.1666257, 1.9256836, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)

	plotData2 <- testGetStageResultsPlotData(stageResults2, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	##
	## Comparison of the results of list object 'plotData2' with expected results
	##
	expect_equal(plotData2$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData2$condPowerValues, c(0.16190673, 0.25578292, 0.37352456, 0.50571691, 0.63820191, 0.75647342, 0.85036726, 0.91657302, 0.95799593, 0.98097594, 0.99227321, 0.99719262, 0.99908938, 0.99973673, 0.99993225), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power Plot with Likelihood")
	expect_equal(plotData2$xlab, "Effect size")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 50, std = 100")

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticNormalCombinationTest}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCIOneMean}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	result2 <- getAnalysisResults(design = design5, dataInput = dataExample2, thetaH0 = 10)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	##
	expect_equal(result2$stages, c(1, 2, 3, 4))
	expect_equal(result2$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result2$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result2$futilityBounds, c(-6, -6, -6))
	expect_equal(result2$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result2$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result2$effectSizes, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(result2$testStatistics, c(1.2040366, 1.6040446, 1.5975241, NA_real_), tolerance = 1e-07)
	expect_equal(result2$pValues, c(0.12168078, 0.059770605, 0.060494785, NA_real_), tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result2$thetaH0, 10)
	expect_equal(result2$thetaH1, 47.25, tolerance = 1e-07)
	expect_equal(result2$assumedStDev, 128.66279, tolerance = 1e-07)
	expect_equal(result2$conditionalRejectionProbabilities, c(0.046837862, 0.16190673, 0.42383694, NA_real_), tolerance = 1e-07)
	expect_equal(result2$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$allocationRatioPlanned, 1)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-37.7517, 0.20066782, 12.631309, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(127.7517, 96.240714, 81.345632, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.28074785, 0.070627118, 0.016069426, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, 3)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, 0.015631623, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 13.353451, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 73.21831, NA_real_), tolerance = 1e-07)
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 44.191393, NA_real_), tolerance = 1e-07)
	expect_equal(result2$normalApproximation, FALSE)
	expect_equal(result2$equalVariances, TRUE)
	expect_equal(result2$directionUpper, TRUE)
	expect_equal(result2$combinationTestStatistics, c(1.1666257, 1.9256836, 2.4675727, NA_real_), tolerance = 1e-07)

	design6 <- getDesignFisher(kMax = 4, alpha = 0.025)

	##
	## Comparison of the results of TrialDesignFisher object 'design6' with expected results
	##
	expect_equal(design6$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(design6$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(design6$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(design6$scale, c(1, 1, 1))
	expect_equal(design6$nonStochasticCurtailment, FALSE)

	stageResults3 <- getStageResults(design6, dataExample2, thetaH0 = 10, stage = 2)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	##
	expect_equal(stageResults3$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(45, 48.6, 47.25), tolerance = 1e-07)
	expect_equal(stageResults3$overallStDevs, c(130, 134.76601, 128.66279), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(20, 50))
	expect_equal(stageResults3$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$combFisher, c(0.12168078, 0.007272934, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1, 1, 1))

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result3 <- getAnalysisResults(design = design6, dataInput = dataExample2, stage = 2, 
		thetaH0 = 10, nPlanned = c(30, 20), thetaH1 = 50, assumedStDev = 100, 
		iterations = 800, seed = 31082018)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	##
	expect_equal(result3$stages, c(1, 2, 3, 4))
	expect_equal(result3$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result3$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(result3$futilityBounds, c(1, 1, 1))
	expect_equal(result3$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(result3$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(result3$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$thetaH0, 10)
	expect_equal(result3$thetaH1, 50)
	expect_equal(result3$assumedStDev, 100)
	expect_equal(result3$conditionalRejectionProbabilities, c(0.026695414, 0.053938868, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$nPlanned, c(NA_real_, NA_real_, 30, 20))
	expect_equal(result3$allocationRatioPlanned, 1)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-28.274837, -2.3519587, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(118.27484, 99.090567, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.23830752, 0.094039775, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$normalApproximation, FALSE)
	expect_equal(result3$equalVariances, TRUE)
	expect_equal(result3$directionUpper, TRUE)
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.6175, 0.84875), tolerance = 1e-07)
	expect_equal(result3$combinationTestStatistics, c(0.12168078, 0.007272934, NA_real_, NA_real_), tolerance = 1e-07)

})

context("Testing the analysis means functionality for two treatments")


test_that("'getAnalysisResults' for a Fisher design and a dataset of two means per stage", {
	.skipTestifDisabled()

	# note: if third stage value of means1 (4.5) increases, lower bound of RCI does not increase
	design7 <- getDesignFisher(kMax = 4, alpha = 0.025)

	##
	## Comparison of the results of TrialDesignFisher object 'design7' with expected results
	##
	expect_equal(design7$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(design7$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(design7$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(design7$scale, c(1, 1, 1))
	expect_equal(design7$nonStochasticCurtailment, FALSE)

	dataExample3 <- getDataset(
		n1 = c(23, 13, 22, 13),
		n2 = c(22, 11, 22, 11),				
		means1 = c(2.7, 1.5, 4.5, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	# @refFS[Formula]{fs:testStatisticTwoMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:definitionRCIwithFutilityFisherCombination} 
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result <- getAnalysisResults(design = design7, dataInput = dataExample3, equalVariances = TRUE, directionUpper = TRUE)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result' with expected results
	##
	expect_equal(result$stages, c(1, 2, 3, 4))
	expect_equal(result$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(result$futilityBounds, c(1, 1, 1))
	expect_equal(result$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(result$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(result$effectSizes, c(170, 123.33333, 197.37931, 188.47418), tolerance = 1e-07)
	expect_equal(result$testStatistics, c(4.552582, 0.42245245, 4.9350374, 2.8165036), tolerance = 1e-07)
	expect_equal(result$pValues, c(2.1583718e-05, 0.33839752, 6.5708867e-06, 0.0050256902), tolerance = 1e-07)
	expect_equal(result$testActions, c("reject and stop", "reject and stop", "reject and stop", "reject"))
	expect_equal(result$thetaH0, 0)
	expect_equal(result$thetaH1, 188.47418, tolerance = 1e-07)
	expect_equal(result$assumedStDev, 192.76382, tolerance = 1e-07)
	expect_equal(result$conditionalRejectionProbabilities, c(1, 1, 1, NA_real_))
	expect_equal(result$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$allocationRatioPlanned, 1)
	expect_equal(result$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$repeatedConfidenceIntervalLowerBounds, c(80.389809, 58.773337, 126.21876, 121.44462), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalUpperBounds, c(259.61019, 232.56315, 252.86796, 238.01813), tolerance = 1e-07)
	expect_equal(result$repeatedPValues, c(6.2988707e-05, 0.00026325991, 1.9536724e-06, 1.9536724e-06), tolerance = 1e-07)
	expect_equal(result$finalStage, 1)
	expect_equal(result$finalPValues, c(2.1583718e-05, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalConfidenceIntervalLowerBounds, c(96.812108, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalConfidenceIntervalUpperBounds, c(243.18789, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result$medianUnbiasedEstimates, c(170, NA_real_, NA_real_, NA_real_))
	expect_equal(result$normalApproximation, FALSE)
	expect_equal(result$equalVariances, TRUE)
	expect_equal(result$directionUpper, TRUE)
	expect_equal(result$combinationTestStatistics, c(2.1583718e-05, 7.3038765e-06, 4.7992944e-11, 2.4119767e-13), tolerance = 1e-07)

})

test_that("'getAnalysisResults' for a group sequential design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestifDisabled()

	dataExample4 <- getDataset(
		n1 = c(23, 13, 22, 13),
		n2 = c(22, 11, 22, 11),				
		means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design8 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	##
	## Comparison of the results of TrialDesignGroupSequential object 'design8' with expected results
	##
	expect_equal(design8$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(design8$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(design8$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)

	# @refFS[Formula]{fs:testStatisticTwoMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result1 <- getAnalysisResults(design = design8, dataInput = dataExample4, equalVariances = TRUE,
		stage = 2, nPlanned = c(15, 15), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	##
	expect_equal(result1$stages, c(1, 2, 3, 4))
	expect_equal(result1$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result1$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result1$futilityBounds, c(-6, -6, -6))
	expect_equal(result1$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result1$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result1$effectSizes, c(70, 59.444444), tolerance = 1e-07)
	expect_equal(result1$testStatistics, c(1.8745926, 0.42245245, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$pValues, c(0.033826026, 0.33839752, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result1$thetaH0, 0)
	expect_equal(result1$thetaH1, 130)
	expect_equal(result1$assumedStDev, 100)
	expect_equal(result1$conditionalRejectionProbabilities, c(0.12319684, 0.060559169, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$nPlanned, c(NA_real_, NA_real_, 15, 15))
	expect_equal(result1$allocationRatioPlanned, 2)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, 0.67921715, 0.95627008), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -38.955154, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(170.18532, 157.84404, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.10782416, 0.16254779, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$normalApproximation, FALSE)
	expect_equal(result1$equalVariances, TRUE)
	expect_equal(result1$directionUpper, TRUE)
	expect_equal(result1$overallTestStatistics, c(1.8745926, 1.4830004, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$overallPValues, c(0.033826026, 0.071381585, NA_real_, NA_real_), tolerance = 1e-07)

	# @refFS[Formula]{fs:testStatisticTwoMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result4 <- getAnalysisResults(design = design8, dataInput = dataExample4, equalVariances = TRUE,
		stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'result4' with expected results
	##
	expect_equal(result4$stages, c(1, 2, 3, 4))
	expect_equal(result4$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result4$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result4$futilityBounds, c(-6, -6, -6))
	expect_equal(result4$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result4$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result4$effectSizes, c(70, 59.444444, 55.310345), tolerance = 1e-07)
	expect_equal(result4$testStatistics, c(1.8745926, 0.42245245, 0.7710996, NA_real_), tolerance = 1e-07)
	expect_equal(result4$pValues, c(0.033826026, 0.33839752, 0.22248223, NA_real_), tolerance = 1e-07)
	expect_equal(result4$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result4$thetaH0, 0)
	expect_equal(result4$thetaH1, 130)
	expect_equal(result4$assumedStDev, 100)
	expect_equal(result4$conditionalRejectionProbabilities, c(0.12319684, 0.060559169, 0.040934114, NA_real_), tolerance = 1e-07)
	expect_equal(result4$nPlanned, c(NA_real_, NA_real_, NA_real_, 15))
	expect_equal(result4$allocationRatioPlanned, 2)
	expect_equal(result4$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.73680191), tolerance = 1e-07)
	expect_equal(result4$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -38.955154, -25.969325, NA_real_), tolerance = 1e-07)
	expect_equal(result4$repeatedConfidenceIntervalUpperBounds, c(170.18532, 157.84404, 136.59001, NA_real_), tolerance = 1e-07)
	expect_equal(result4$repeatedPValues, c(0.10782416, 0.16254779, 0.12132816, NA_real_), tolerance = 1e-07)
	expect_equal(result4$finalStage, NA_integer_)
	expect_equal(result4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$normalApproximation, FALSE)
	expect_equal(result4$equalVariances, TRUE)
	expect_equal(result4$directionUpper, TRUE)
	expect_equal(result4$overallTestStatistics, c(1.8745926, 1.4830004, 1.5863394, NA_real_), tolerance = 1e-07)
	expect_equal(result4$overallPValues, c(0.033826026, 0.071381585, 0.057753539, NA_real_), tolerance = 1e-07)

	# @refFS[Formula]{fs:testStatisticTwoMeansEqualVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterEqualVariances}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result7 <- getAnalysisResults(design = design8, dataInput = dataExample4, equalVariances = TRUE,
		stage = 4, nPlanned = numeric(0), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'result7' with expected results
	##
	expect_equal(result7$stages, c(1, 2, 3, 4))
	expect_equal(result7$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result7$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result7$futilityBounds, c(-6, -6, -6))
	expect_equal(result7$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result7$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result7$effectSizes, c(70, 59.444444, 55.310345, 72.41784), tolerance = 1e-07)
	expect_equal(result7$testStatistics, c(1.8745926, 0.42245245, 0.7710996, 2.8165036), tolerance = 1e-07)
	expect_equal(result7$pValues, c(0.033826026, 0.33839752, 0.22248223, 0.0050256902), tolerance = 1e-07)
	expect_equal(result7$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result7$thetaH0, 0)
	expect_equal(result7$thetaH1, 130)
	expect_equal(result7$assumedStDev, 100)
	expect_equal(result7$conditionalRejectionProbabilities, c(0.12319684, 0.060559169, 0.040934114, NA_real_), tolerance = 1e-07)
	expect_equal(result7$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result7$allocationRatioPlanned, 2)
	expect_equal(result7$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result7$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -38.955154, -25.969325, 3.8960985), tolerance = 1e-07)
	expect_equal(result7$repeatedConfidenceIntervalUpperBounds, c(170.18532, 157.84404, 136.59001, 140.93958), tolerance = 1e-07)
	expect_equal(result7$repeatedPValues, c(0.10782416, 0.16254779, 0.12132816, 0.017942439), tolerance = 1e-07)
	expect_equal(result7$finalStage, 4)
	expect_equal(result7$finalPValues, c(NA_real_, NA_real_, NA_real_, 0.022610692), tolerance = 1e-07)
	expect_equal(result7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, 1.5235285), tolerance = 1e-07)
	expect_equal(result7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, 127.93924), tolerance = 1e-07)
	expect_equal(result7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, 66.58768), tolerance = 1e-07)
	expect_equal(result7$normalApproximation, FALSE)
	expect_equal(result7$equalVariances, TRUE)
	expect_equal(result7$directionUpper, TRUE)
	expect_equal(result7$overallTestStatistics, c(1.8745926, 1.4830004, 1.5863394, 2.3864368), tolerance = 1e-07)
	expect_equal(result7$overallPValues, c(0.033826026, 0.071381585, 0.057753539, 0.0091998951), tolerance = 1e-07)

})

test_that("'getAnalysisResults' for an inverse normal design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestifDisabled()

	dataExample5 <- getDataset(
		n1 = c(23, 13, 22, 13),
		n2 = c(22, 11, 22, 11),				
		means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design9 <- getDesignInverseNormal(kMax = 4, alpha = 0.025,  typeOfDesign = "WT", deltaWT = 0.4)

	##
	## Comparison of the results of TrialDesignInverseNormal object 'design9' with expected results
	##
	expect_equal(design9$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(design9$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(design9$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)

	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterUnequalVariances}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result2 <- getAnalysisResults(design = design9, dataInput = dataExample5, equalVariances = FALSE,
		stage = 2, nPlanned = c(15, 15), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	##
	expect_equal(result2$stages, c(1, 2, 3, 4))
	expect_equal(result2$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result2$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result2$futilityBounds, c(-6, -6, -6))
	expect_equal(result2$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result2$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result2$effectSizes, c(70, 59.444444), tolerance = 1e-07)
	expect_equal(result2$testStatistics, c(1.8780002, 0.42565792, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$pValues, c(0.033590771, 0.33726198, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result2$thetaH0, 0)
	expect_equal(result2$thetaH1, 130)
	expect_equal(result2$assumedStDev, 100)
	expect_equal(result2$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$nPlanned, c(NA_real_, NA_real_, 15, 15))
	expect_equal(result2$allocationRatioPlanned, 2)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, 0.7399771, 0.96741599), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.10725005, 0.13184907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$normalApproximation, FALSE)
	expect_equal(result2$equalVariances, FALSE)
	expect_equal(result2$directionUpper, TRUE)
	expect_equal(result2$combinationTestStatistics, c(1.8304576, 1.5912766, NA_real_, NA_real_), tolerance = 1e-07)

	# @refFS[Formula]{fs:testStatisticDifferenceMeansUnequalVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterUnequalVariances}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerTwoMeansEffect}
	result5 <- getAnalysisResults(design = design9, dataInput = dataExample5, equalVariances = FALSE,
		stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'result5' with expected results
	##
	expect_equal(result5$stages, c(1, 2, 3, 4))
	expect_equal(result5$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result5$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result5$futilityBounds, c(-6, -6, -6))
	expect_equal(result5$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result5$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result5$effectSizes, c(70, 59.444444, 55.310345), tolerance = 1e-07)
	expect_equal(result5$testStatistics, c(1.8780002, 0.42565792, 0.7710996, NA_real_), tolerance = 1e-07)
	expect_equal(result5$pValues, c(0.033590771, 0.33726198, 0.22248687, NA_real_), tolerance = 1e-07)
	expect_equal(result5$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result5$thetaH0, 0)
	expect_equal(result5$thetaH1, 130)
	expect_equal(result5$assumedStDev, 100)
	expect_equal(result5$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, 0.073275512, NA_real_), tolerance = 1e-07)
	expect_equal(result5$nPlanned, c(NA_real_, NA_real_, NA_real_, 15))
	expect_equal(result5$allocationRatioPlanned, 2)
	expect_equal(result5$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.82164236), tolerance = 1e-07)
	expect_equal(result5$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, -19.230333, NA_real_), tolerance = 1e-07)
	expect_equal(result5$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, 134.96564, NA_real_), tolerance = 1e-07)
	expect_equal(result5$repeatedPValues, c(0.10725005, 0.13184907, 0.088247169, NA_real_), tolerance = 1e-07)
	expect_equal(result5$finalStage, NA_integer_)
	expect_equal(result5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$normalApproximation, FALSE)
	expect_equal(result5$equalVariances, FALSE)
	expect_equal(result5$directionUpper, TRUE)
	expect_equal(result5$combinationTestStatistics, c(1.8304576, 1.5912766, 1.7402643, NA_real_), tolerance = 1e-07)

	# @refFS[Formula]{fs:testStatisticDifferenceMeansUnequalVariances}
	# @refFS[Formula]{fs:pValuesTwoMeansAlternativeGreaterUnequalVariances}
	# @refFS[Formula]{fs:testStatisticsGroupSequential}
	# @refFS[Formula]{fs:definitionRCIInverseNormal}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:orderingPValueUpper}
	# @refFS[Formula]{fs:finalCITwoMeans}
	result8 <- getAnalysisResults(design = design9, dataInput = dataExample5, equalVariances = FALSE,
		stage = 4, nPlanned = numeric(0), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'result8' with expected results
	##
	expect_equal(result8$stages, c(1, 2, 3, 4))
	expect_equal(result8$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result8$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(result8$futilityBounds, c(-6, -6, -6))
	expect_equal(result8$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.025), tolerance = 1e-07)
	expect_equal(result8$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	expect_equal(result8$effectSizes, c(70, 59.444444, 55.310345, 72.41784), tolerance = 1e-07)
	expect_equal(result8$testStatistics, c(1.8780002, 0.42565792, 0.7710996, 2.8165036), tolerance = 1e-07)
	expect_equal(result8$pValues, c(0.033590771, 0.33726198, 0.22248687, 0.0051181248), tolerance = 1e-07)
	expect_equal(result8$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result8$thetaH0, 0)
	expect_equal(result8$thetaH1, 130)
	expect_equal(result8$assumedStDev, 100)
	expect_equal(result8$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, 0.073275512, NA_real_), tolerance = 1e-07)
	expect_equal(result8$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result8$allocationRatioPlanned, 2)
	expect_equal(result8$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result8$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, -19.230333, 16.862491), tolerance = 1e-07)
	expect_equal(result8$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, 134.96564, 146.10543), tolerance = 1e-07)
	expect_equal(result8$repeatedPValues, c(0.10725005, 0.13184907, 0.088247169, 0.0050030118), tolerance = 1e-07)
	expect_equal(result8$finalStage, 4)
	expect_equal(result8$finalPValues, c(NA_real_, NA_real_, NA_real_, 0.019192988), tolerance = 1e-07)
	expect_equal(result8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, 4.0866333), tolerance = 1e-07)
	expect_equal(result8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, 135.35067), tolerance = 1e-07)
	expect_equal(result8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, 71.819795), tolerance = 1e-07)
	expect_equal(result8$normalApproximation, FALSE)
	expect_equal(result8$equalVariances, FALSE)
	expect_equal(result8$directionUpper, TRUE)
	expect_equal(result8$combinationTestStatistics, c(1.8304576, 1.5912766, 1.7402643, 2.7909855), tolerance = 1e-07)

})

test_that("'getAnalysisResults' for a Fisher design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestifDisabled()

	dataExample6 <- getDataset(
		n1 = c(23, 13, 22, 13),
		n2 = c(22, 11, 22, 11),				
		means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design10 <- getDesignFisher(kMax = 4, alpha = 0.025)

	##
	## Comparison of the results of TrialDesignFisher object 'design10' with expected results
	##
	expect_equal(design10$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(design10$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(design10$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(design10$scale, c(1, 1, 1))
	expect_equal(design10$nonStochasticCurtailment, FALSE)

	result3 <- getAnalysisResults(design = design10, dataInput = dataExample6, equalVariances = TRUE,
		stage = 2, nPlanned = c(15, 15), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	##
	expect_equal(result3$stages, c(1, 2, 3, 4))
	expect_equal(result3$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result3$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(result3$futilityBounds, c(1, 1, 1))
	expect_equal(result3$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(result3$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(result3$effectSizes, c(70, 59.444444), tolerance = 1e-07)
	expect_equal(result3$testStatistics, c(1.8745926, 0.42245245, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$pValues, c(0.033826026, 0.33839752, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$thetaH0, 0)
	expect_equal(result3$thetaH1, 130)
	expect_equal(result3$assumedStDev, 100)
	expect_equal(result3$conditionalRejectionProbabilities, c(0.077408717, 0.036086707, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$nPlanned, c(NA_real_, NA_real_, 15, 15))
	expect_equal(result3$allocationRatioPlanned, 2)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-19.610191, -28.583726, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(159.61019, 157.36315, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.07529439, 0.13212373, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$normalApproximation, FALSE)
	expect_equal(result3$equalVariances, TRUE)
	expect_equal(result3$directionUpper, TRUE)
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.629, 0.911), tolerance = 1e-07)
	expect_equal(result3$combinationTestStatistics, c(0.033826026, 0.011446643, NA_real_, NA_real_), tolerance = 1e-07)

	result6 <- getAnalysisResults(design = design10, dataInput = dataExample6, equalVariances = TRUE,
		stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result6' with expected results
	##
	expect_equal(result6$stages, c(1, 2, 3, 4))
	expect_equal(result6$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result6$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(result6$futilityBounds, c(1, 1, 1))
	expect_equal(result6$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(result6$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(result6$effectSizes, c(70, 59.444444, 55.310345), tolerance = 1e-07)
	expect_equal(result6$testStatistics, c(1.8745926, 0.42245245, 0.7710996, NA_real_), tolerance = 1e-07)
	expect_equal(result6$pValues, c(0.033826026, 0.33839752, 0.22248223, NA_real_), tolerance = 1e-07)
	expect_equal(result6$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result6$thetaH0, 0)
	expect_equal(result6$thetaH1, 130)
	expect_equal(result6$assumedStDev, 100)
	expect_equal(result6$conditionalRejectionProbabilities, c(0.077408717, 0.036086707, 0.017989301, NA_real_), tolerance = 1e-07)
	expect_equal(result6$nPlanned, c(NA_real_, NA_real_, NA_real_, 15))
	expect_equal(result6$allocationRatioPlanned, 2)
	expect_equal(result6$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.60883935), tolerance = 1e-07)
	expect_equal(result6$repeatedConfidenceIntervalLowerBounds, c(-19.610191, -28.583726, -24.875191, NA_real_), tolerance = 1e-07)
	expect_equal(result6$repeatedConfidenceIntervalUpperBounds, c(159.61019, 157.36315, 146.25589, NA_real_), tolerance = 1e-07)
	expect_equal(result6$repeatedPValues, c(0.07529439, 0.13212373, 0.13321282, NA_real_), tolerance = 1e-07)
	expect_equal(result6$finalStage, NA_integer_)
	expect_equal(result6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$normalApproximation, FALSE)
	expect_equal(result6$equalVariances, TRUE)
	expect_equal(result6$directionUpper, TRUE)
	expect_equal(result6$combinationTestStatistics, c(0.033826026, 0.011446643, 0.0025466747, NA_real_), tolerance = 1e-07)

	result9 <- getAnalysisResults(design = design10, dataInput = dataExample6, equalVariances = TRUE,
		stage = 4, nPlanned = numeric(0), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result9' with expected results
	##
	expect_equal(result9$stages, c(1, 2, 3, 4))
	expect_equal(result9$informationRates, c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)
	expect_equal(result9$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(result9$futilityBounds, c(1, 1, 1))
	expect_equal(result9$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(result9$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(result9$effectSizes, c(70, 59.444444, 55.310345, 72.41784), tolerance = 1e-07)
	expect_equal(result9$testStatistics, c(1.8745926, 0.42245245, 0.7710996, 2.8165036), tolerance = 1e-07)
	expect_equal(result9$pValues, c(0.033826026, 0.33839752, 0.22248223, 0.0050256902), tolerance = 1e-07)
	expect_equal(result9$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result9$thetaH0, 0)
	expect_equal(result9$thetaH1, 130)
	expect_equal(result9$assumedStDev, 100)
	expect_equal(result9$conditionalRejectionProbabilities, c(0.077408717, 0.036086707, 0.017989301, NA_real_), tolerance = 1e-07)
	expect_equal(result9$nPlanned, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$allocationRatioPlanned, 2)
	expect_equal(result9$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$repeatedConfidenceIntervalLowerBounds, c(-19.610191, -28.583726, -24.875191, 10.125544), tolerance = 1e-07)
	expect_equal(result9$repeatedConfidenceIntervalUpperBounds, c(159.61019, 157.36315, 146.25589, 154.53063), tolerance = 1e-07)
	expect_equal(result9$repeatedPValues, c(0.07529439, 0.13212373, 0.13321282, 0.010110881), tolerance = 1e-07)
	expect_equal(result9$finalStage, NA_integer_)
	expect_equal(result9$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$normalApproximation, FALSE)
	expect_equal(result9$equalVariances, TRUE)
	expect_equal(result9$directionUpper, TRUE)
	expect_equal(result9$combinationTestStatistics, c(0.033826026, 0.011446643, 0.0025466747, 1.2798798e-05), tolerance = 1e-07)

})

test_that("Check that the conditional power is as expected for different designs and datasets", {

	.skipTestifDisabled()

	informationRates <- c(0.2, 0.5, 0.8, 1)

	dataExample7 <- getDataset(
			n1 = c(22, 13, 22, 13),
			n2 = c(22, 11, 22, 11),		
			means1 = c(1, 1.1, 1, 1),
			means2 = c(1.4, 1.5, 1, 2.5), 
			stds1 = c(1, 2, 2, 1.3),
			stds2 = c(1, 2, 2, 1.3))

	design11 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
		informationRates = informationRates, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.45)

	##
	## Comparison of the results of TrialDesignGroupSequential object 'design11' with expected results
	##
	expect_equal(design11$alphaSpent, c(0.008066711, 0.01611168, 0.021671928, 0.025), tolerance = 1e-07)
	expect_equal(design11$criticalValues, c(2.4058832, 2.2981456, 2.2447684, 2.2198623), tolerance = 1e-07)
	expect_equal(design11$stageLevels, c(0.008066711, 0.010776752, 0.012391502, 0.013214058), tolerance = 1e-07)

	result1 <- getAnalysisResults(design = design11, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = FALSE, stage = 2, thetaH0 = 0.2, thetaH1 = -0.2, nPlanned = c(96, 64), 
		allocationRatioPlanned = 3, normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	##
	expect_equal(result1$stages, c(1, 2, 3, 4))
	expect_equal(result1$informationRates, c(0.2, 0.5, 0.8, 1), tolerance = 1e-07)
	expect_equal(result1$criticalValues, c(2.4058832, 2.2981456, 2.2447684, 2.2198623), tolerance = 1e-07)
	expect_equal(result1$futilityBounds, c(0.5244, 0.5244, 0.5244), tolerance = 1e-07)
	expect_equal(result1$alphaSpent, c(0.008066711, 0.01611168, 0.021671928, 0.025), tolerance = 1e-07)
	expect_equal(result1$stageLevels, c(0.008066711, 0.010776752, 0.012391502, 0.013214058), tolerance = 1e-07)
	expect_equal(result1$effectSizes, c(-0.4, -0.39619048), tolerance = 1e-07)
	expect_equal(result1$testStatistics, c(-1.9899749, -0.73229093, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$pValues, c(0.026564837, 0.23586057, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result1$thetaH0, 0.2, tolerance = 1e-07)
	expect_equal(result1$thetaH1, -0.2, tolerance = 1e-07)
	expect_equal(result1$assumedStDev, 1.4042956, tolerance = 1e-07)
	expect_equal(result1$conditionalRejectionProbabilities, c(0.13790633, 0.11434101, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$nPlanned, c(NA_real_, NA_real_, 96, 64))
	expect_equal(result1$allocationRatioPlanned, 3)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, 0.4081395, 0.60690858), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-1.1558731, -1.198323, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(0.35587299, 0.40594209, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.06267268, 0.077641512, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$normalApproximation, FALSE)
	expect_equal(result1$equalVariances, TRUE)
	expect_equal(result1$directionUpper, FALSE)
	expect_equal(result1$overallTestStatistics, c(-1.9899749, -1.7496977, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$overallPValues, c(0.026564837, 0.042409297, NA_real_, NA_real_), tolerance = 1e-07)

	design12 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
		informationRates = informationRates, typeOfDesign = "WT", deltaWT = 0.45)

	##
	## Comparison of the results of TrialDesignInverseNormal object 'design12' with expected results
	##
	expect_equal(design12$alphaSpent, c(0.0064937119, 0.013848609, 0.020340933, 0.025), tolerance = 1e-07)
	expect_equal(design12$criticalValues, c(2.484114, 2.3728731, 2.3177603, 2.2920443), tolerance = 1e-07)
	expect_equal(design12$stageLevels, c(0.0064937119, 0.0088251631, 0.010231176, 0.010951542), tolerance = 1e-07)

	stageResults <- getStageResults(design = design12, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = T, stage = 2, thetaH0 = -1) 

	##
	## Comparison of the results of StageResultsMeans object 'stageResults' with expected results
	##
	expect_equal(stageResults$overallTestStatistics, c(1.9899749, 1.7720581, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$overallPValues, c(0.026564837, 0.040500218, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$overallMeans1, c(1, 1.0371429, 1.022807, 1.0185714), tolerance = 1e-07)
	expect_equal(stageResults$overallMeans2, c(1.4, 1.4333333, 1.26, 1.4666667), tolerance = 1e-07)
	expect_equal(stageResults$overallStDevs1, c(1, 1.4254175, 1.6534615, 1.5851935), tolerance = 1e-07)
	expect_equal(stageResults$overallStDevs2, c(1, 1.3814998, 1.6530107, 1.6573689), tolerance = 1e-07)
	expect_equal(stageResults$overallSampleSizes1, c(22, 35))
	expect_equal(stageResults$overallSampleSizes2, c(22, 33))
	expect_equal(stageResults$testStatistics, c(1.9899749, 0.73229093, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$pValues, c(0.026564837, 0.23586057, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$effectSizes, c(-0.4, -0.39619048), tolerance = 1e-07)
	expect_equal(stageResults$combInverseNormal, c(1.9338654, 1.7805468, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$weightsInverseNormal, c(0.4472136, 0.54772256, 0.54772256, 0.4472136), tolerance = 1e-07)

	conditionalPower <- getConditionalPower(design = design12, stageResults = stageResults,
			stage = 2, thetaH1 = 0.840, nPlanned = c(96,64), assumedStDev = 2)

	##
	## Comparison of the results of list object 'conditionalPower' with expected results
	##
	expect_equal(conditionalPower$nPlanned, c(NA_real_, NA_real_, 96, 64))
	expect_equal(conditionalPower$conditionalPower, c(NA_real_, NA_real_, 0.99873967, 0.99999483), tolerance = 1e-07)

	conditionalPowerPlot <- .getConditionalPowerPlot(stageResults = stageResults,
			stage = 2, thetaRange = seq(-0.8,0.5,0.1), nPlanned = c(96,64), assumedStDev = 2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of list object 'conditionalPowerPlot' with expected results
	##
	expect_equal(conditionalPowerPlot$xValues, c(-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$condPowerValues, c(0.22956329, 0.31502432, 0.41251256, 0.51641352, 0.6197496, 0.71556407, 0.79832397, 0.86487377, 0.91466948, 0.94932539, 0.97175172, 0.98524183, 0.99278324, 0.99670055), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$likelihoodValues, c(0.49547937, 0.67200309, 0.83620171, 0.95465162, 0.9999375, 0.96093693, 0.84724887, 0.68536385, 0.50865752, 0.34635689, 0.21637958, 0.12402316, 0.065220394, 0.031467201), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$main, "Conditional Power Plot with Likelihood")
	expect_equal(conditionalPowerPlot$xlab, "Effect size")
	expect_equal(conditionalPowerPlot$ylab, "Conditional power / Likelihood")
	expect_equal(conditionalPowerPlot$sub, "Stage = 2, # of remaining subjects = 160, std = 2, allocation ratio = 3")

	result2 <- getAnalysisResults(design = design12, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = FALSE,	stage = 2, thetaH0 = 0.2, thetaH1 = -0.2, nPlanned = c(96, 64), 
		allocationRatioPlanned = 3, normalApproximation = FALSE)

	##
	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	##
	expect_equal(result2$stages, c(1, 2, 3, 4))
	expect_equal(result2$informationRates, c(0.2, 0.5, 0.8, 1), tolerance = 1e-07)
	expect_equal(result2$criticalValues, c(2.484114, 2.3728731, 2.3177603, 2.2920443), tolerance = 1e-07)
	expect_equal(result2$futilityBounds, c(-6, -6, -6))
	expect_equal(result2$alphaSpent, c(0.0064937119, 0.013848609, 0.020340933, 0.025), tolerance = 1e-07)
	expect_equal(result2$stageLevels, c(0.0064937119, 0.0088251631, 0.010231176, 0.010951542), tolerance = 1e-07)
	expect_equal(result2$effectSizes, c(-0.4, -0.39619048), tolerance = 1e-07)
	expect_equal(result2$testStatistics, c(-1.9899749, -0.73229093, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$pValues, c(0.026564837, 0.23586057, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result2$thetaH0, 0.2, tolerance = 1e-07)
	expect_equal(result2$thetaH1, -0.2, tolerance = 1e-07)
	expect_equal(result2$assumedStDev, 1.4042956, tolerance = 1e-07)
	expect_equal(result2$conditionalRejectionProbabilities, c(0.11857307, 0.10556981, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$nPlanned, c(NA_real_, NA_real_, 96, 64))
	expect_equal(result2$allocationRatioPlanned, 3)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, 0.39060766, 0.5889102), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-1.182291, -1.2104795, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(0.3822909, 0.41047947, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.081445577, 0.092870573, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$normalApproximation, FALSE)
	expect_equal(result2$equalVariances, TRUE)
	expect_equal(result2$directionUpper, FALSE)
	expect_equal(result2$combinationTestStatistics, c(1.9338654, 1.7805468, NA_real_, NA_real_), tolerance = 1e-07)

	design13 <- getDesignFisher(kMax = 4, alpha = 0.025, informationRates = informationRates)

	##
	## Comparison of the results of TrialDesignFisher object 'design13' with expected results
	##
	expect_equal(design13$alphaSpent, c(0.0099747046, 0.017168497, 0.022142404, 0.025), tolerance = 1e-07)
	expect_equal(design13$criticalValues, c(0.0099747046, 0.00059134153, 6.046221e-05, 1.3203687e-05), tolerance = 1e-07)
	expect_equal(design13$stageLevels, c(0.0099747046, 0.0099747046, 0.0099747046, 0.0099747046), tolerance = 1e-07)
	expect_equal(design13$scale, c(1.2247449, 1.2247449, 1), tolerance = 1e-07)
	expect_equal(design13$nonStochasticCurtailment, FALSE)

	result3 <- getAnalysisResults(design = design13, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = FALSE,	stage = 2, nPlanned = c(96,64), thetaH1 = -0.4, allocationRatio = 2, 
		normalApproximation = FALSE, iterations = 10000, seed = 442018)

	##
	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	##
	expect_equal(result3$stages, c(1, 2, 3, 4))
	expect_equal(result3$informationRates, c(0.2, 0.5, 0.8, 1), tolerance = 1e-07)
	expect_equal(result3$criticalValues, c(0.0099747046, 0.00059134153, 6.046221e-05, 1.3203687e-05), tolerance = 1e-07)
	expect_equal(result3$futilityBounds, c(1, 1, 1))
	expect_equal(result3$alphaSpent, c(0.0099747046, 0.017168497, 0.022142404, 0.025), tolerance = 1e-07)
	expect_equal(result3$stageLevels, c(0.0099747046, 0.0099747046, 0.0099747046, 0.0099747046), tolerance = 1e-07)
	expect_equal(result3$effectSizes, c(-0.4, -0.39619048), tolerance = 1e-07)
	expect_equal(result3$testStatistics, c(-1.3266499, -0.48819395, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$pValues, c(0.095896458, 0.31512146, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$thetaH0, 0)
	expect_equal(result3$thetaH1, -0.4, tolerance = 1e-07)
	expect_equal(result3$assumedStDev, 1.4042956, tolerance = 1e-07)
	expect_equal(result3$conditionalRejectionProbabilities, c(0.031447357, 0.012731128, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$nPlanned, c(NA_real_, NA_real_, 96, 64))
	expect_equal(result3$allocationRatioPlanned, 2)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-1.1295139, -1.2072533, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(0.32951385, 0.40725333, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.19930232, 0.29225486, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$normalApproximation, FALSE)
	expect_equal(result3$equalVariances, TRUE)
	expect_equal(result3$directionUpper, FALSE)
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.1353, 0.2436), tolerance = 1e-07)
	expect_equal(result3$combinationTestStatistics, c(0.095896458, 0.023311276, NA_real_, NA_real_), tolerance = 1e-07)

})

context("Testing 'getStageResultsMeans'")


test_that("'getStageResultsMeans' for an inverse normal design and one or two treatments", {
	.skipTestifDisabled()

	designInverseNormal <- getDesignInverseNormal(kMax = 2, alpha = 0.025, sided = 1, 
		informationRates = c(0.5, 1), typeOfDesign = "WT", 
		deltaWT = 0.25, futilityBounds = qnorm(0.7))

	dataExample8 <- getDataset(
		n = c(10, 10),
		means = c(2, 3),
		stDevs = c(1, 1.5))

	stageResults1 <- getStageResultsMeans(design = designInverseNormal, dataInput = dataExample8, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	##
	expect_equal(stageResults1$overallTestStatistics, c(6.3245553, 8.3272484), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(6.846828e-05, 4.5964001e-08), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(2, 2.5), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(1, 1.3426212), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(10, 20))
	expect_equal(stageResults1$testStatistics, c(6.3245553, 6.3245553), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(6.846828e-05, 6.846828e-05), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(2, 2.5), tolerance = 1e-07)
	expect_equal(stageResults1$combInverseNormal, c(3.813637, 5.3932972), tolerance = 1e-07)
	expect_equal(stageResults1$weightsInverseNormal, c(0.70710678, 0.70710678), tolerance = 1e-07)

	dataExample9 <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)

	stageResults2 <- getStageResultsMeans(design = designInverseNormal, dataInput = dataExample9, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	##
	expect_equal(stageResults2$overallTestStatistics, c(-1.3266499, -1.1850988), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.90410354, 0.87988596), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes1, c(22, 33))
	expect_equal(stageResults2$overallSampleSizes2, c(22, 35))
	expect_equal(stageResults2$testStatistics, c(-1.3266499, -0.48819395), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.90410354, 0.68487854), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(-0.4, -0.40380952), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(-1.3052935, -1.2633725), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.70710678, 0.70710678), tolerance = 1e-07)

})

test_that("'getStageResultsMeans' for a Fisher design and one or two treatments", {

	.skipTestifDisabled()

	designFisher <- getDesignFisher(kMax = 2, alpha = 0.025, 
		alpha0Vec = 1, informationRates = c(0.5, 1), 
		method = "equalAlpha")

	dataExample10 <- getDataset(
		n = c(10, 10),
		means = c(2, 3),
		stDevs = c(1, 1.5))

	stageResults3 <- getStageResultsMeans(design = designFisher, dataInput = dataExample10, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	##
	expect_equal(stageResults3$overallTestStatistics, c(6.3245553, 8.3272484), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(6.846828e-05, 4.5964001e-08), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(2, 2.5), tolerance = 1e-07)
	expect_equal(stageResults3$overallStDevs, c(1, 1.3426212), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(10, 20))
	expect_equal(stageResults3$testStatistics, c(6.3245553, 6.3245553), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(6.846828e-05, 6.846828e-05), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(2, 2.5), tolerance = 1e-07)
	expect_equal(stageResults3$combFisher, c(6.846828e-05, 4.6879053e-09), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1))

	dataExample11 <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)

	stageResults4 <- getStageResultsMeans(design = designFisher, dataInput = dataExample11, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	##
	## Comparison of the results of StageResultsMeans object 'stageResults4' with expected results
	##
	expect_equal(stageResults4$overallTestStatistics, c(-1.3266499, -1.1850988), tolerance = 1e-07)
	expect_equal(stageResults4$overallPValues, c(0.90410354, 0.87988596), tolerance = 1e-07)
	expect_equal(stageResults4$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(stageResults4$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(stageResults4$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(stageResults4$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(stageResults4$overallSampleSizes1, c(22, 33))
	expect_equal(stageResults4$overallSampleSizes2, c(22, 35))
	expect_equal(stageResults4$testStatistics, c(-1.3266499, -0.48819395), tolerance = 1e-07)
	expect_equal(stageResults4$pValues, c(0.90410354, 0.68487854), tolerance = 1e-07)
	expect_equal(stageResults4$effectSizes, c(-0.4, -0.40380952), tolerance = 1e-07)
	expect_equal(stageResults4$combFisher, c(0.90410354, 0.61920111), tolerance = 1e-07)
	expect_equal(stageResults4$weightsFisher, c(1, 1))

})

