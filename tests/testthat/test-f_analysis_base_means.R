#:#  
#:#  *Unit tests*
#:#  
#:#  This file is part of the R package rpact:
#:#  Confirmatory Adaptive Clinical Trial Design and Analysis
#:#  
#:#  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
#:#  Licensed under "GNU Lesser General Public License" version 3
#:#  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
#:#  
#:#  RPACT company website: https://www.rpact.com
#:#  RPACT package website: https://www.rpact.org
#:#  
#:#  Contact us for information about our services: info@rpact.com
#:#  
#:#  File name: test-f_analysis_base_means.R
#:#  Creation date: 05 September 2020, 14:21:59
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the analysis means functionality for one treatment")


test_that("'getAnalysisResults' for group sequential design and a dataset of one mean per stage (bindingFutility = FALSE)", {
	.skipTestIfDisabled()

	dataExample <- getDataset(
		n = c(120, 130),
		means = c(0.45, 0.41) * 100,
		stDevs = c(1.3, 1.4) * 100
	)

	design <- getDesignGroupSequential(kMax = 3, alpha = 0.025, futilityBounds = rep(0.5244, 2), 
		bindingFutility = FALSE, typeOfDesign = "WT", deltaWT = 0.4)

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
	result <- getAnalysisResults(design = design, dataInput = dataExample, 
		nPlanned = 130, thetaH1 = 22, assumedStDev = 100, thetaH0 = 25)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result' with expected results
	expect_equal(result$testActions, c("continue", "continue", NA_character_))
	expect_equal(result$conditionalRejectionProbabilities, c(0.10127313, 0.20204948, NA_real_), tolerance = 1e-07)
	expect_equal(result$conditionalPower, c(NA_real_, NA_real_, 0.11972239), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalLowerBounds, c(15.620913, 23.359338, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalUpperBounds, c(74.379087, 62.480662, NA_real_), tolerance = 1e-07)
	expect_equal(result$repeatedPValues, c(0.11501103, 0.039167372, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalStage, NA_integer_)
	expect_equal(result$finalPValues, c(NA_real_, NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_))
	expect_equal(result$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_))
	expect_equal(result$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	}

})

test_that("'getAnalysisResults' for group sequential design and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestIfDisabled()

	dataExample0 <- getDataset(
		n = c(120, 130, 130),
		means = c(0.45, 0.41, 0.45) * 100,
		stDevs = c(1.3, 1.4, 1.2) * 100
	)

	design1 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

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
	result1 <- getAnalysisResults(design = design1, dataInput = dataExample0, 
		nPlanned = 130, thetaH1 = 22, assumedStDev = 100, thetaH0 = 25)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.11438278, 0.24787613, 0.68016764, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.55017955), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(14.924587, 22.902668, 28.667333, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(75.075413, 62.937332, 58.595825, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.10271087, 0.041641931, 0.0060474693, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, 3)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, 0.014723218, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 26.836053, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 56.851998, NA_real_), tolerance = 1e-07)
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 42.083094, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	}

})

test_that("'getStageResults' for group sequential design and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestIfDisabled()

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

	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	expect_equal(stageResults1$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(130, 134.76601, 128.66279, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(20, 50, NA_real_, NA_real_))
	expect_equal(stageResults1$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults1), NA)))
	    expect_output(print(stageResults1)$show())
	    invisible(capture.output(expect_error(summary(stageResults1), NA)))
	    expect_output(summary(stageResults1)$show())
	}

	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	plotData1 <- testGetStageResultsPlotData(stageResults1, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData1$condPowerValues, c(0.20492816, 0.31007642, 0.43512091, 0.5683138, 0.6950205, 0.80243295, 0.88343665, 0.93770927, 0.96998259, 0.98700232, 0.99495733, 0.99825113, 0.99945881, 0.9998508, 0.9999634), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Effect size")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

})

test_that("'getAnalysisResults' for inverse normal and Fisher designs and a dataset of one mean per stage (bindingFutility = TRUE)", {

	.skipTestIfDisabled()

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

	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	expect_equal(stageResults2$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs, c(130, 134.76601, 128.66279, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes, c(20, 50, NA_real_, NA_real_))
	expect_equal(stageResults2$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(1.1666257, 1.9256836, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults2), NA)))
	    expect_output(print(stageResults2)$show())
	    invisible(capture.output(expect_error(summary(stageResults2), NA)))
	    expect_output(summary(stageResults2)$show())
	}

	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalPowerOneMeanEffect}
	plotData2 <- testGetStageResultsPlotData(stageResults2, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData2$condPowerValues, c(0.18776792, 0.28883478, 0.41147918, 0.5447391, 0.67401995, 0.78575942, 0.87165951, 0.93031941, 0.96586805, 0.98497137, 0.99406923, 0.99790729, 0.999341, 0.99981509, 0.99995383), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.12861339, 0.21139553, 0.32435073, 0.46456173, 0.62112851, 0.77522713, 0.90320416, 0.98231862, 0.99730568, 0.94517816, 0.83619688, 0.69057821, 0.53238607, 0.38313335, 0.25738469), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "Effect size")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

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

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.054544013, 0.18776792, 0.47147471, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.98296857), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-35.118855, 1.5735511, 13.58964, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(125.11886, 94.865725, 80.385626, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.18164628, 0.056608473, 0.014183052, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, 3)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, 0.016754234, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 13.011822, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 82.848073, NA_real_), tolerance = 1e-07)
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 45.714272, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	}

	design3 <- getDesignFisher(kMax = 4, alpha = 0.025, alpha0Vec = rep(0.4, 3), bindingFutility = TRUE)

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	stageResults3 <- getStageResults(design3, dataExample1, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	expect_equal(stageResults3$overallTestStatistics, c(1.2040366, 2.025312, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(0.12168078, 0.02415027, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(45, 48.6, 47.25, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallStDevs, c(130, 134.76601, 128.66279, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(20, 50, NA_real_, NA_real_))
	expect_equal(stageResults3$testStatistics, c(1.2040366, 1.6040446, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(0.12168078, 0.059770605, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(45, 48.6, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$combFisher, c(0.12168078, 0.007272934, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults3), NA)))
	    expect_output(print(stageResults3)$show())
	    invisible(capture.output(expect_error(summary(stageResults3), NA)))
	    expect_output(summary(stageResults3)$show())
	}

	# @refFS[Formula]{fs:testStatisticOneMean}
	# @refFS[Formula]{fs:pValuesOneMeanAlternativeGreater}
	# @refFS[Formula]{fs:testStatisticFisherCombinationTest}
	# @refFS[Formula]{fs:definitionRCIFisherCombination}
	# @refFS[Formula]{fs:calculationRepeatedpValue}
	# @refFS[Formula]{fs:finalPValueFisherCombinationTest}
	# @refFS[Formula]{fs:conditionalRejectionUnderNullGroupSequential}
	result3 <- getAnalysisResults(design = design3, dataInput = dataExample1, thetaH0 = 10, 
		nPlanned = 30, thetaH1 = 50, assumedStDev = 100, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.029249394, 0.067046868, 0.15552139, NA_real_), tolerance = 1e-07)
	expect_equal(result3$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.88057256), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-24.226675, 0.014834887, 8.7947814, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(114.22668, 96.713521, 85.125684, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.165096, 0.068572907, 0.029926287, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	}

})

test_that("'getAnalysisResults' for different designs and a dataset of one mean per stage (bindingFutility = FALSE)", {

	.skipTestIfDisabled()

	design4 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	dataExample2 <- getDataset(
			n = c(20, 20, 20),
			means = c(0.45, 0.51, 0.45) * 100,
			stDevs = c(1.3, 1.4, 1.2) * 100
	)

	stageResults1 <- getStageResults(design4, dataExample2, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	expect_equal(stageResults1$overallTestStatistics, c(1.2040366, 1.8018141, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(0.12168078, 0.039654359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(45, 48, 47, NA_real_))
	expect_equal(stageResults1$overallStDevs, c(130, 133.38396, 128.06116, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(20, 40, NA_real_, NA_real_))
	expect_equal(stageResults1$testStatistics, c(1.2040366, 1.309697, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(0.12168078, 0.10295724, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(45, 48, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults1), NA)))
	    expect_output(print(stageResults1)$show())
	    invisible(capture.output(expect_error(summary(stageResults1), NA)))
	    expect_output(summary(stageResults1)$show())
	}

	plotData1 <- testGetStageResultsPlotData(stageResults1, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData1$condPowerValues, c(0.11518708, 0.19320212, 0.2981846, 0.42448846, 0.55999334, 0.68937861, 0.79916986, 0.8818727, 0.93712809, 0.96985063, 0.98701854, 0.99499503, 0.99827592, 0.99947032, 0.99985507), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.19725323, 0.29399425, 0.4142314, 0.5517428, 0.69473602, 0.8269751, 0.93058175, 0.98993369, 0.99551351, 0.94640644, 0.85054578, 0.72261535, 0.58037159, 0.44065083, 0.31628057), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Effect size")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

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

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$thetaH1, 47)
	expect_equal(result1$assumedStDev, 128.06116, tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.046837862, 0.11518708, 0.2468754, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-37.7517, -4.7433931, 7.9671114, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(127.7517, 100.74339, 86.032888, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.28074785, 0.098382799, 0.033210734, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	}

	design5 <- getDesignInverseNormal(kMax = 4, alpha = 0.025,  typeOfDesign = "WT", deltaWT = 0.4)

	## Comparison of the results of TrialDesignInverseNormal object 'design5' with expected results
	expect_equal(design5$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.02499999), tolerance = 1e-07)
	expect_equal(design5$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(design5$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design5), NA)))
	    expect_output(print(design5)$show())
	    invisible(capture.output(expect_error(summary(design5), NA)))
	    expect_output(summary(design5)$show())
	}

	stageResults2 <- getStageResults(design5, dataExample2, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	expect_equal(stageResults2$overallTestStatistics, c(1.2040366, 1.8018141, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.12168078, 0.039654359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans, c(45, 48, 47, NA_real_))
	expect_equal(stageResults2$overallStDevs, c(130, 133.38396, 128.06116, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes, c(20, 40, NA_real_, NA_real_))
	expect_equal(stageResults2$testStatistics, c(1.2040366, 1.309697, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.12168078, 0.10295724, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(45, 48, NA_real_, NA_real_))
	expect_equal(stageResults2$combInverseNormal, c(1.1666257, 1.7193339, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults2), NA)))
	    expect_output(print(stageResults2)$show())
	    invisible(capture.output(expect_error(summary(stageResults2), NA)))
	    expect_output(summary(stageResults2)$show())
	}

	plotData2 <- testGetStageResultsPlotData(stageResults2, stage = 2, nPlanned = c(30, 20), 
		thetaRange = seq(10, 80, 5), assumedStDev = 100)

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80))
	expect_equal(plotData2$condPowerValues, c(0.10694527, 0.18165277, 0.28365551, 0.40813694, 0.54357522, 0.6747028, 0.78751068, 0.8736511, 0.93198732, 0.96700263, 0.98562146, 0.9943885, 0.99804297, 0.99939119, 0.99983131), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.19725323, 0.29399425, 0.4142314, 0.5517428, 0.69473602, 0.8269751, 0.93058175, 0.98993369, 0.99551351, 0.94640644, 0.85054578, 0.72261535, 0.58037159, 0.44065083, 0.31628057), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "Effect size")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 50, sd = 100")

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

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$thetaH1, 47)
	expect_equal(result2$assumedStDev, 128.06116, tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.046837862, 0.10694527, 0.21929053, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-37.7517, -5.8599359, 6.9798507, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(127.7517, 101.68482, 86.758637, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.28074785, 0.10502799, 0.037620516, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	}

	design6 <- getDesignFisher(kMax = 4, alpha = 0.025)

	## Comparison of the results of TrialDesignFisher object 'design6' with expected results
	expect_equal(design6$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(design6$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(design6$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(design6$scale, c(1, 1, 1))
	expect_equal(design6$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design6), NA)))
	    expect_output(print(design6)$show())
	    invisible(capture.output(expect_error(summary(design6), NA)))
	    expect_output(summary(design6)$show())
	}

	stageResults3 <- getStageResults(design6, dataExample2, thetaH0 = 10, stage = 2)

	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
	expect_equal(stageResults3$overallTestStatistics, c(1.2040366, 1.8018141, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallPValues, c(0.12168078, 0.039654359, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallMeans, c(45, 48, 47, NA_real_))
	expect_equal(stageResults3$overallStDevs, c(130, 133.38396, 128.06116, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$overallSampleSizes, c(20, 40, NA_real_, NA_real_))
	expect_equal(stageResults3$testStatistics, c(1.2040366, 1.309697, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$pValues, c(0.12168078, 0.10295724, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$effectSizes, c(45, 48, NA_real_, NA_real_))
	expect_equal(stageResults3$combFisher, c(0.12168078, 0.012527917, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults3$weightsFisher, c(1, 1, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults3), NA)))
	    expect_output(print(stageResults3)$show())
	    invisible(capture.output(expect_error(summary(stageResults3), NA)))
	    expect_output(summary(stageResults3)$show())
	}

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

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.026695414, 0.033302173, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-28.274837, -9.0994871, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(118.27484, 104.78379, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.23830752, 0.14118934, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.54125, 0.8125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	}

})

context("Testing the analysis means functionality for two treatments")


test_that("'getAnalysisResults' for a Fisher design and a dataset of two means per stage", {
	.skipTestIfDisabled()

	# note: if third stage value of means1 (4.5) increases, lower bound of RCI does not increase
	design7 <- getDesignFisher(kMax = 4, alpha = 0.025)

	## Comparison of the results of TrialDesignFisher object 'design7' with expected results
	expect_equal(design7$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(design7$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(design7$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(design7$scale, c(1, 1, 1))
	expect_equal(design7$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design7), NA)))
	    expect_output(print(design7)$show())
	    invisible(capture.output(expect_error(summary(design7), NA)))
	    expect_output(summary(design7)$show())
	}

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
	result <- getAnalysisResults(design = design7, dataInput = dataExample3, equalVariances = TRUE, 
		directionUpper = TRUE, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'result' with expected results
	expect_equal(result$thetaH1, 188.47418, tolerance = 1e-07)
	expect_equal(result$assumedStDev, 192.76382, tolerance = 1e-07)
	expect_equal(result$testActions, c("reject and stop", "reject and stop", "reject and stop", "reject"))
	expect_equal(result$conditionalRejectionProbabilities, c(1, 1, 1, NA_real_))
	expect_equal(result$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result$repeatedConfidenceIntervalLowerBounds, c(80.389809, 58.773337, 126.21876, 121.44462), tolerance = 1e-07)
	expect_equal(result$repeatedConfidenceIntervalUpperBounds, c(259.61019, 232.56315, 252.86796, 238.01813), tolerance = 1e-07)
	expect_equal(result$repeatedPValues, c(6.2988707e-05, 0.00026325991, 1.9536724e-06, 1.9536724e-06), tolerance = 1e-07)
	expect_equal(result$finalStage, 1)
	expect_equal(result$finalPValues, c(2.1583718e-05, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalConfidenceIntervalLowerBounds, c(96.812108, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result$finalConfidenceIntervalUpperBounds, c(243.18789, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result$medianUnbiasedEstimates, c(170, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result), NA)))
	    expect_output(print(result)$show())
	    invisible(capture.output(expect_error(summary(result), NA)))
	    expect_output(summary(result)$show())
	}

})

test_that("'getAnalysisResults' for a group sequential design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestIfDisabled()

	dataExample4 <- getDataset(
		n1 = c(23, 23, 22, 23),
		n2 = c(22, 22, 22, 21),				
		means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design8 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.4)

	## Comparison of the results of TrialDesignGroupSequential object 'design8' with expected results
	expect_equal(design8$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.02499999), tolerance = 1e-07)
	expect_equal(design8$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(design8$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design8), NA)))
	    expect_output(print(design8)$show())
	    invisible(capture.output(expect_error(summary(design8), NA)))
	    expect_output(summary(design8)$show())
	}

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

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.12319684, 0.052938347, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, 0.65019157, 0.95040435), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -39.416167, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(170.18532, 149.41617, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.10782416, 0.1777417, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	}

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

	## Comparison of the results of AnalysisResultsGroupSequential object 'result4' with expected results
	expect_equal(result4$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result4$conditionalRejectionProbabilities, c(0.12319684, 0.052938347, 0.042196066, NA_real_), tolerance = 1e-07)
	expect_equal(result4$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.74141468), tolerance = 1e-07)
	expect_equal(result4$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -39.416167, -24.461261, NA_real_), tolerance = 1e-07)
	expect_equal(result4$repeatedConfidenceIntervalUpperBounds, c(170.18532, 149.41617, 130.73577, NA_real_), tolerance = 1e-07)
	expect_equal(result4$repeatedPValues, c(0.10782416, 0.1777417, 0.11951427, NA_real_), tolerance = 1e-07)
	expect_equal(result4$finalStage, NA_integer_)
	expect_equal(result4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result4), NA)))
	    expect_output(print(result4)$show())
	    invisible(capture.output(expect_error(summary(result4), NA)))
	    expect_output(summary(result4)$show())
	}

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

	## Comparison of the results of AnalysisResultsGroupSequential object 'result7' with expected results
	expect_equal(result7$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result7$conditionalRejectionProbabilities, c(0.12319684, 0.052938347, 0.042196066, NA_real_), tolerance = 1e-07)
	expect_equal(result7$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result7$repeatedConfidenceIntervalLowerBounds, c(-30.185323, -39.416167, -24.461261, 16.408896), tolerance = 1e-07)
	expect_equal(result7$repeatedConfidenceIntervalUpperBounds, c(170.18532, 149.41617, 130.73577, 138.52605), tolerance = 1e-07)
	expect_equal(result7$repeatedPValues, c(0.10782416, 0.1777417, 0.11951427, 0.0045471564), tolerance = 1e-07)
	expect_equal(result7$finalStage, 4)
	expect_equal(result7$finalPValues, c(NA_real_, NA_real_, NA_real_, 0.019111276), tolerance = 1e-07)
	expect_equal(result7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, 3.8518993), tolerance = 1e-07)
	expect_equal(result7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, 122.8312), tolerance = 1e-07)
	expect_equal(result7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, 65.8091), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result7), NA)))
	    expect_output(print(result7)$show())
	    invisible(capture.output(expect_error(summary(result7), NA)))
	    expect_output(summary(result7)$show())
	}

})

test_that("'getAnalysisResults' for an inverse normal design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestIfDisabled()

	dataExample5 <- getDataset(
		n1 = c(23, 13, 22, 13),
		n2 = c(22, 11, 22, 11),				
		means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design9 <- getDesignInverseNormal(kMax = 4, alpha = 0.025,  typeOfDesign = "WT", deltaWT = 0.4)

	## Comparison of the results of TrialDesignInverseNormal object 'design9' with expected results
	expect_equal(design9$alphaSpent, c(0.0051577307, 0.011892822, 0.018620498, 0.02499999), tolerance = 1e-07)
	expect_equal(design9$criticalValues, c(2.5650713, 2.3932961, 2.2981973, 2.2330242), tolerance = 1e-07)
	expect_equal(design9$stageLevels, c(0.0051577307, 0.0083488792, 0.010775281, 0.012773673), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design9), NA)))
	    expect_output(print(design9)$show())
	    invisible(capture.output(expect_error(summary(design9), NA)))
	    expect_output(summary(design9)$show())
	}

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

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, 0.7399771, 0.96741599), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.10725005, 0.13184907, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	}

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

	## Comparison of the results of AnalysisResultsInverseNormal object 'result5' with expected results
	expect_equal(result5$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result5$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, 0.073275512, NA_real_), tolerance = 1e-07)
	expect_equal(result5$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.82164236), tolerance = 1e-07)
	expect_equal(result5$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, -19.230333, NA_real_), tolerance = 1e-07)
	expect_equal(result5$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, 134.96564, NA_real_), tolerance = 1e-07)
	expect_equal(result5$repeatedPValues, c(0.10725005, 0.13184907, 0.088247169, NA_real_), tolerance = 1e-07)
	expect_equal(result5$finalStage, NA_integer_)
	expect_equal(result5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result5), NA)))
	    expect_output(print(result5)$show())
	    invisible(capture.output(expect_error(summary(result5), NA)))
	    expect_output(summary(result5)$show())
	}

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

	## Comparison of the results of AnalysisResultsInverseNormal object 'result8' with expected results
	expect_equal(result8$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result8$conditionalRejectionProbabilities, c(0.12372016, 0.08089089, 0.073275512, NA_real_), tolerance = 1e-07)
	expect_equal(result8$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result8$repeatedConfidenceIntervalLowerBounds, c(-30.008991, -32.585516, -19.230333, 16.862491), tolerance = 1e-07)
	expect_equal(result8$repeatedConfidenceIntervalUpperBounds, c(170.00899, 154.76457, 134.96564, 146.10543), tolerance = 1e-07)
	expect_equal(result8$repeatedPValues, c(0.10725005, 0.13184907, 0.088247169, 0.0050030118), tolerance = 1e-07)
	expect_equal(result8$finalStage, 4)
	expect_equal(result8$finalPValues, c(NA_real_, NA_real_, NA_real_, 0.019192988), tolerance = 1e-07)
	expect_equal(result8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, 4.0866333), tolerance = 1e-07)
	expect_equal(result8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, 135.35067), tolerance = 1e-07)
	expect_equal(result8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, 71.819795), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result8), NA)))
	    expect_output(print(result8)$show())
	    invisible(capture.output(expect_error(summary(result8), NA)))
	    expect_output(summary(result8)$show())
	}

})

test_that("'getAnalysisResults' for a Fisher design and a dataset of two means per stage, stages: default, 2, 3, and 4", {

	.skipTestIfDisabled()

	dataExample6 <- getDataset(
		n1 = c(23, 13, 22, 13),
		n2 = c(22, 11, 22, 11),				
		means1 = c(1.7, 1.5, 1.8, 2.5) * 100,
		means2 = c(1, 1.1, 1.3, 1) * 100,				
		stds1 = c(1.3, 2.4, 2.2, 1.3) * 100,
		stds2 = c(1.2, 2.2, 2.1, 1.3) * 100
	)

	design10 <- getDesignFisher(kMax = 4, alpha = 0.025)

	## Comparison of the results of TrialDesignFisher object 'design10' with expected results
	expect_equal(design10$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(design10$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(design10$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(design10$scale, c(1, 1, 1))
	expect_equal(design10$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design10), NA)))
	    expect_output(print(design10)$show())
	    invisible(capture.output(expect_error(summary(design10), NA)))
	    expect_output(summary(design10)$show())
	}

	result3 <- getAnalysisResults(design = design10, dataInput = dataExample6, equalVariances = TRUE,
		stage = 2, nPlanned = c(15, 15), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.077408717, 0.036086707, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-19.610191, -28.583726, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(159.61019, 157.36315, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.07529439, 0.13212373, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.599, 0.917), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	}

	result6 <- getAnalysisResults(design = design10, dataInput = dataExample6, equalVariances = TRUE,
		stage = 3, nPlanned = 15, thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'result6' with expected results
	expect_equal(result6$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(result6$conditionalRejectionProbabilities, c(0.077408717, 0.036086707, 0.017989301, NA_real_), tolerance = 1e-07)
	expect_equal(result6$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.60883935), tolerance = 1e-07)
	expect_equal(result6$repeatedConfidenceIntervalLowerBounds, c(-19.610191, -28.583726, -24.875191, NA_real_), tolerance = 1e-07)
	expect_equal(result6$repeatedConfidenceIntervalUpperBounds, c(159.61019, 157.36315, 146.25589, NA_real_), tolerance = 1e-07)
	expect_equal(result6$repeatedPValues, c(0.07529439, 0.13212373, 0.13321282, NA_real_), tolerance = 1e-07)
	expect_equal(result6$finalStage, NA_integer_)
	expect_equal(result6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result6), NA)))
	    expect_output(print(result6)$show())
	    invisible(capture.output(expect_error(summary(result6), NA)))
	    expect_output(summary(result6)$show())
	}

	result9 <- getAnalysisResults(design = design10, dataInput = dataExample6, equalVariances = TRUE,
		stage = 4, nPlanned = numeric(0), thetaH0 = 0, thetaH1 = 130, 
		assumedStDev = 100, allocationRatioPlanned = 2, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'result9' with expected results
	expect_equal(result9$testActions, c("continue", "continue", "continue", "reject"))
	expect_equal(result9$conditionalRejectionProbabilities, c(0.077408717, 0.036086707, 0.017989301, NA_real_), tolerance = 1e-07)
	expect_equal(result9$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$repeatedConfidenceIntervalLowerBounds, c(-19.610191, -28.583726, -24.875191, 10.125544), tolerance = 1e-07)
	expect_equal(result9$repeatedConfidenceIntervalUpperBounds, c(159.61019, 157.36315, 146.25589, 154.53063), tolerance = 1e-07)
	expect_equal(result9$repeatedPValues, c(0.07529439, 0.13212373, 0.13321282, 0.010110881), tolerance = 1e-07)
	expect_equal(result9$finalStage, NA_integer_)
	expect_equal(result9$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result9$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result9), NA)))
	    expect_output(print(result9)$show())
	    invisible(capture.output(expect_error(summary(result9), NA)))
	    expect_output(summary(result9)$show())
	}

})

test_that("Check that the conditional power is as expected for different designs and datasets", {

	.skipTestIfDisabled()

	informationRates <- c(0.2, 0.5, 0.8, 1)

	dataExample7 <- getDataset(
			n1 = c(22, 33, 31, 13),
			n2 = c(22, 31, 30, 11),		
			means1 = c(1, 1.1, 1, 1),
			means2 = c(1.4, 1.5, 1, 2.5), 
			stds1 = c(1, 2, 2, 1.3),
			stds2 = c(1, 2, 2, 1.3))

	design11 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
		informationRates = informationRates, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.45)

	## Comparison of the results of TrialDesignGroupSequential object 'design11' with expected results
	expect_equal(design11$alphaSpent, c(0.008066711, 0.01611168, 0.021671928, 0.02499999), tolerance = 1e-07)
	expect_equal(design11$criticalValues, c(2.4058832, 2.2981456, 2.2447684, 2.2198623), tolerance = 1e-07)
	expect_equal(design11$stageLevels, c(0.008066711, 0.010776752, 0.012391502, 0.013214058), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design11), NA)))
	    expect_output(print(design11)$show())
	    invisible(capture.output(expect_error(summary(design11), NA)))
	    expect_output(summary(design11)$show())
	}

	result1 <- getAnalysisResults(design = design11, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = FALSE, stage = 2, thetaH0 = 0.2, thetaH1 = -0.2, nPlanned = c(96, 64), 
		allocationRatioPlanned = 3, normalApproximation = FALSE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'result1' with expected results
	expect_equal(result1$assumedStDev, 1.6547835, tolerance = 1e-07)
	expect_equal(result1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result1$conditionalRejectionProbabilities, c(0.13790633, 0.14848468, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$conditionalPower, c(NA_real_, NA_real_, 0.40521176, 0.57857102), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalLowerBounds, c(-1.1558731, -1.1414911, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedConfidenceIntervalUpperBounds, c(0.35587299, 0.34450997, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$repeatedPValues, c(0.06267268, 0.06133487, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result1$finalStage, NA_integer_)
	expect_equal(result1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result1), NA)))
	    expect_output(print(result1)$show())
	    invisible(capture.output(expect_error(summary(result1), NA)))
	    expect_output(summary(result1)$show())
	}

	design12 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
		informationRates = informationRates, typeOfDesign = "WT", deltaWT = 0.45)

	## Comparison of the results of TrialDesignInverseNormal object 'design12' with expected results
	expect_equal(design12$alphaSpent, c(0.0064937119, 0.013848609, 0.020340933, 0.02499999), tolerance = 1e-07)
	expect_equal(design12$criticalValues, c(2.484114, 2.3728731, 2.3177603, 2.2920443), tolerance = 1e-07)
	expect_equal(design12$stageLevels, c(0.0064937119, 0.0088251631, 0.010231176, 0.010951542), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design12), NA)))
	    expect_output(print(design12)$show())
	    invisible(capture.output(expect_error(summary(design12), NA)))
	    expect_output(summary(design12)$show())
	}

	stageResults <- getStageResults(design = design12, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = T, stage = 2, thetaH0 = -1) 

	## Comparison of the results of StageResultsMeans object 'stageResults' with expected results
	expect_equal(stageResults$overallTestStatistics, c(1.9899749, 1.8884638, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$overallPValues, c(0.026564837, 0.030848764, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$overallMeans1, c(1, 1.06, 1.0383721, 1.0333333), tolerance = 1e-07)
	expect_equal(stageResults$overallMeans2, c(1.4, 1.4584906, 1.2927711, 1.4340426), tolerance = 1e-07)
	expect_equal(stageResults$overallStDevs1, c(1, 1.6618374, 1.7796344, 1.7187442), tolerance = 1e-07)
	expect_equal(stageResults$overallStDevs2, c(1, 1.6474262, 1.7846078, 1.7725841), tolerance = 1e-07)
	expect_equal(stageResults$overallSampleSizes1, c(22, 55, NA_real_, NA_real_))
	expect_equal(stageResults$overallSampleSizes2, c(22, 53, NA_real_, NA_real_))
	expect_equal(stageResults$testStatistics, c(1.9899749, 1.1994139, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$pValues, c(0.026564837, 0.11746538, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$effectSizes, c(-0.4, -0.39849057, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$combInverseNormal, c(1.9338654, 2.1431134, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults$weightsInverseNormal, c(0.4472136, 0.54772256, 0.54772256, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults), NA)))
	    expect_output(print(stageResults)$show())
	    invisible(capture.output(expect_error(summary(stageResults), NA)))
	    expect_output(summary(stageResults)$show())
	}

	conditionalPower <- getConditionalPower(stageResults,
		thetaH1 = 0.840, nPlanned = c(96,64), assumedStDev = 2)

	## Comparison of the results of ConditionalPowerResultsMeans object 'conditionalPower' with expected results
	expect_equal(conditionalPower$nPlanned, c(NA_real_, NA_real_, 96, 64))
	expect_equal(conditionalPower$conditionalPower, c(NA_real_, NA_real_, 0.99975751, 0.99999919), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(conditionalPower), NA)))
	    expect_output(print(conditionalPower)$show())
	    invisible(capture.output(expect_error(summary(conditionalPower), NA)))
	    expect_output(summary(conditionalPower)$show())
	}

	conditionalPowerPlot <- .getConditionalPowerPlot(stageResults = stageResults,
		thetaRange = seq(-0.8,0.5,0.1), nPlanned = c(96,64), assumedStDev = 2, allocationRatioPlanned = 3)

	## Comparison of the results of list object 'conditionalPowerPlot' with expected results
	expect_equal(conditionalPowerPlot$xValues, c(-0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$condPowerValues, c(0.37570702, 0.47532662, 0.57738365, 0.67516684, 0.76267391, 0.83573986, 0.89261201, 0.9338489, 0.96168571, 0.97917178, 0.98938899, 0.99494035, 0.99774434, 0.99906067), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$likelihoodValues, c(0.45180702, 0.63888737, 0.81863148, 0.95048525, 0.99998877, 0.95331773, 0.82351787, 0.64461615, 0.45721677, 0.29385692, 0.17113644, 0.090311253, 0.043185112, 0.018711949), tolerance = 1e-07)
	expect_equal(conditionalPowerPlot$main, "Conditional Power with Likelihood")
	expect_equal(conditionalPowerPlot$xlab, "Effect size")
	expect_equal(conditionalPowerPlot$ylab, "Conditional power / Likelihood")
	expect_equal(conditionalPowerPlot$sub, "Stage = 2, # of remaining subjects = 160, sd = 2, allocation ratio = 3")

	result2 <- getAnalysisResults(design = design12, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = FALSE,	stage = 2, thetaH0 = 0.2, thetaH1 = -0.2, nPlanned = c(96, 64), 
		allocationRatioPlanned = 3, normalApproximation = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'result2' with expected results
	expect_equal(result2$assumedStDev, 1.6547835, tolerance = 1e-07)
	expect_equal(result2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result2$conditionalRejectionProbabilities, c(0.11857307, 0.20646025, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$conditionalPower, c(NA_real_, NA_real_, 0.50295479, 0.65954708), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalLowerBounds, c(-1.182291, -1.0666303, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedConfidenceIntervalUpperBounds, c(0.3822909, 0.2666303, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$repeatedPValues, c(0.081445577, 0.043264349, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result2$finalStage, NA_integer_)
	expect_equal(result2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result2), NA)))
	    expect_output(print(result2)$show())
	    invisible(capture.output(expect_error(summary(result2), NA)))
	    expect_output(summary(result2)$show())
	}

	design13 <- getDesignFisher(kMax = 4, alpha = 0.025, informationRates = informationRates)

	## Comparison of the results of TrialDesignFisher object 'design13' with expected results
	expect_equal(design13$alphaSpent, c(0.0099747046, 0.017168497, 0.022142404, 0.025), tolerance = 1e-07)
	expect_equal(design13$criticalValues, c(0.0099747046, 0.00059134153, 6.046221e-05, 1.3203687e-05), tolerance = 1e-07)
	expect_equal(design13$stageLevels, c(0.0099747046, 0.0099747046, 0.0099747046, 0.0099747046), tolerance = 1e-07)
	expect_equal(design13$scale, c(1.2247449, 1.2247449, 1), tolerance = 1e-07)
	expect_equal(design13$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(design13), NA)))
	    expect_output(print(design13)$show())
	    invisible(capture.output(expect_error(summary(design13), NA)))
	    expect_output(summary(design13)$show())
	}

	result3 <- getAnalysisResults(design = design13, dataInput = dataExample7, equalVariances = TRUE, 
		directionUpper = FALSE,	stage = 2, nPlanned = c(96,64), thetaH1 = -0.4, allocationRatioPlanned = 2, 
		normalApproximation = FALSE, iterations = 10000, seed = 442018)

	## Comparison of the results of AnalysisResultsFisher object 'result3' with expected results
	expect_equal(result3$assumedStDev, 1.6547835, tolerance = 1e-07)
	expect_equal(result3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(result3$conditionalRejectionProbabilities, c(0.031447357, 0.018451139, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalLowerBounds, c(-1.1295139, -1.1012297, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedConfidenceIntervalUpperBounds, c(0.32951385, 0.30122972, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$repeatedPValues, c(0.19930232, 0.21960219, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(result3$finalStage, NA_integer_)
	expect_equal(result3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(result3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.1239, 0.2143), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(result3), NA)))
	    expect_output(print(result3)$show())
	    invisible(capture.output(expect_error(summary(result3), NA)))
	    expect_output(summary(result3)$show())
	}

})

context("Testing 'getStageResultsMeans'")


test_that("'getStageResultsMeans' for an inverse normal design and one or two treatments", {
	.skipTestIfDisabled()

	designInverseNormal <- getDesignInverseNormal(kMax = 4, alpha = 0.025, sided = 1, 
		typeOfDesign = "WT", 
		deltaWT = 0.25, futilityBounds = rep(qnorm(0.7),3))

	dataExample8 <- getDataset(
		n = c(10, 10),
		means = c(2, 3),
		stDevs = c(1, 1.5))

	stageResults1 <- getStageResults(design = designInverseNormal, dataInput = dataExample8, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	## Comparison of the results of StageResultsMeans object 'stageResults1' with expected results
	expect_equal(stageResults1$overallTestStatistics, c(6.3245553, 8.3272484, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallPValues, c(6.846828e-05, 4.5964001e-08, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallMeans, c(2, 2.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallStDevs, c(1, 1.3426212, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$overallSampleSizes, c(10, 20, NA_real_, NA_real_))
	expect_equal(stageResults1$testStatistics, c(6.3245553, 6.3245553, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$pValues, c(6.846828e-05, 6.846828e-05, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$effectSizes, c(2, 2.5, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$combInverseNormal, c(3.813637, 5.3932972, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults1$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults1), NA)))
	    expect_output(print(stageResults1)$show())
	    invisible(capture.output(expect_error(summary(stageResults1), NA)))
	    expect_output(summary(stageResults1)$show())
	}

	dataExample9 <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)

	stageResults2 <- getStageResults(design = designInverseNormal, dataInput = dataExample9, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	## Comparison of the results of StageResultsMeans object 'stageResults2' with expected results
	expect_equal(stageResults2$overallTestStatistics, c(-1.3266499, -1.1850988, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallPValues, c(0.90410354, 0.87988596, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(stageResults2$overallSampleSizes1, c(22, 33, NA_real_, NA_real_))
	expect_equal(stageResults2$overallSampleSizes2, c(22, 35, NA_real_, NA_real_))
	expect_equal(stageResults2$testStatistics, c(-1.3266499, -0.48819395, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$pValues, c(0.90410354, 0.68487854, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$effectSizes, c(-0.4, -0.40380952, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$combInverseNormal, c(-1.3052935, -1.2633725, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(stageResults2$weightsInverseNormal, c(0.5, 0.5, 0.5, 0.5), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults2), NA)))
	    expect_output(print(stageResults2)$show())
	    invisible(capture.output(expect_error(summary(stageResults2), NA)))
	    expect_output(summary(stageResults2)$show())
	}

})

test_that("'getStageResultsMeans' for a Fisher design and one or two treatments", {

	.skipTestIfDisabled()

	designFisher <- getDesignFisher(kMax = 2, alpha = 0.025, 
		alpha0Vec = 1, informationRates = c(0.5, 1), 
		method = "equalAlpha")

	dataExample10 <- getDataset(
		n = c(10, 10),
		means = c(2, 3),
		stDevs = c(1, 1.5))

	stageResults3 <- getStageResults(design = designFisher, dataInput = dataExample10, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	## Comparison of the results of StageResultsMeans object 'stageResults3' with expected results
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
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults3), NA)))
	    expect_output(print(stageResults3)$show())
	    invisible(capture.output(expect_error(summary(stageResults3), NA)))
	    expect_output(summary(stageResults3)$show())
	}

	dataExample11 <- getDataset(
		n1 = c(22, 11),
		n2 = c(22, 13),
		means1 = c(1, 1.1),
		means2 = c(1.4, 1.5),
		stDevs1 = c(1, 2),
		stDevs2 = c(1, 2)
	)

	stageResults4 <- getStageResults(design = designFisher, dataInput = dataExample11, stage = 2,
		thetaH0 = C_THETA_H0_MEANS_DEFAULT, 
		directionUpper = C_DIRECTION_UPPER_DEFAULT, 
		normalApproximation = C_NORMAL_APPROXIMATION_MEANS_DEFAULT, 
		equalVariances = C_EQUAL_VARIANCES_DEFAULT)

	## Comparison of the results of StageResultsMeans object 'stageResults4' with expected results
	expect_equal(stageResults4$overallTestStatistics, c(-1.3266499, -1.1850988), tolerance = 1e-07)
	expect_equal(stageResults4$overallPValues, c(0.90410354, 0.87988596), tolerance = 1e-07)
	expect_equal(stageResults4$overallMeans1, c(1, 1.0333333), tolerance = 1e-07)
	expect_equal(stageResults4$overallMeans2, c(1.4, 1.4371429), tolerance = 1e-07)
	expect_equal(stageResults4$overallStDevs1, c(1, 1.3814998), tolerance = 1e-07)
	expect_equal(stageResults4$overallStDevs2, c(1, 1.4254175), tolerance = 1e-07)
	expect_equal(stageResults4$overallSampleSizes1, c(22, 33))
	expect_equal(stageResults4$overallSampleSizes2, c(22, 35))
	expect_equal(stageResults4$testStatistics, c(-1.3266499, -0.48819395), tolerance = 1e-07)
	expect_equal(stageResults4$pValues, c(0.90410354, 0.68487854), tolerance = 1e-07)
	expect_equal(stageResults4$effectSizes, c(-0.4, -0.40380952), tolerance = 1e-07)
	expect_equal(stageResults4$combFisher, c(0.90410354, 0.61920111), tolerance = 1e-07)
	expect_equal(stageResults4$weightsFisher, c(1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(stageResults4), NA)))
	    expect_output(print(stageResults4)$show())
	    invisible(capture.output(expect_error(summary(stageResults4), NA)))
	    expect_output(summary(stageResults4)$show())
	}

})

test_that("'getAnalysisResults' with a dataset of means and without defining a design", {

	.skipTestIfDisabled()

	data <- getDataset(
		n1 = c(22),
		n2 = c(21),
		means1 = c(1.63),
		means2 = c(1.4),
		stds1 = c(1.2),
		stds2 = c(1.3))
	analysisResults1 <- getAnalysisResults(data, alpha = 0.02, beta = 0.9,stage = 1)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
	expect_equal(analysisResults1$thetaH1, 0.23, tolerance = 1e-07)
	expect_equal(analysisResults1$assumedStDev, 1.2497805, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, -0.57876509, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, 1.0387651, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults1), NA)))
	    expect_output(print(analysisResults1)$show())
	    invisible(capture.output(expect_error(summary(analysisResults1), NA)))
	    expect_output(summary(analysisResults1)$show())
	}

	analysisResults2 <- getAnalysisResults(data, alpha = 0.02, beta = 0.9, sided = 2)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults2' with expected results
	expect_equal(analysisResults2$thetaH1, 0.23, tolerance = 1e-07)
	expect_equal(analysisResults2$assumedStDev, 1.2497805, tolerance = 1e-07)
	expect_equal(analysisResults2$repeatedConfidenceIntervalLowerBounds, -0.69301003, tolerance = 1e-07)
	expect_equal(analysisResults2$repeatedConfidenceIntervalUpperBounds, 1.1530101, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults2), NA)))
	    expect_output(print(analysisResults2)$show())
	    invisible(capture.output(expect_error(summary(analysisResults2), NA)))
	    expect_output(summary(analysisResults2)$show())
	}

	analysisResults3 <- getAnalysisResults(data, alpha = 0.02, beta = 0.9, sided = 2, twoSidedPower = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults3' with expected results
	expect_equal(analysisResults3$thetaH1, 0.23, tolerance = 1e-07)
	expect_equal(analysisResults3$assumedStDev, 1.2497805, tolerance = 1e-07)
	expect_equal(analysisResults3$repeatedConfidenceIntervalLowerBounds, -0.69301003, tolerance = 1e-07)
	expect_equal(analysisResults3$repeatedConfidenceIntervalUpperBounds, 1.1530101, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults3), NA)))
	    expect_output(print(analysisResults3)$show())
	    invisible(capture.output(expect_error(summary(analysisResults3), NA)))
	    expect_output(summary(analysisResults3)$show())
	}
})

