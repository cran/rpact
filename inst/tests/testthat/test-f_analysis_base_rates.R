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
#:#  File name: test-f_analysis_base_rates.R
#:#  Creation date: 05 September 2020, 14:23:19
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the analysis rates functionality for one treatment")


test_that("'getAnalysisResults' for a group sequential design and one treatment", {
	.skipTestIfDisabled()

	design1 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1), 
		futilityBounds = c(-0.5, 0, 0.5),	typeOfDesign = "asKD", gammaA = 2.8)

	dataExample1 <- getDataset(
		n = c(10, 10, 20, 11), 
		events = c(4, 5, 5, 6)
	)

	x1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.75, normalApproximation = FALSE, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.45, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.13502024, 0.39663603, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.035340808, 0.15564776, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.88809228, 0.77284187, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.056127482, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$pi1, 0.35, tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "reject and stop", "reject and stop", NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.21465031, 0.55995383, 1, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.088987938, 0.19243532, 0.20635802, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.8198195, 0.7374817, 0.52720839, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.47958473, 0.014066714, 1.9536724e-06, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, 2)
	expect_equal(x2$finalPValues, c(NA_real_, 0.0011783609, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.18821106, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.62661996, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, 0.40681825, NA_real_, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	x3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.13502024, 0.39663603, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.85193241, 0.94869662), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.035340808, 0.15564776, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.88809228, 0.77284187, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.056127482, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.98024945, 0.94869662, 0.88988709, 0.79611571, 0.66506207, 0.50313625, 0.32784789), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(1, 0.9039239, 0.66761715, 0.40289032, 0.19865977, 0.080038099, 0.026347981), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18")

	x4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x4' with expected results
	expect_equal(x4$testActions, c("continue", "reject and stop", NA_character_, NA_character_))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.21465031, 0.55995383, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.9494174, 0.9843063), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.088987938, 0.19243532, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.8198195, 0.7374817, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.47958473, 0.014066714, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, 2)
	expect_equal(x4$finalPValues, c(NA_real_, 0.0011783609, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, 0.18821106, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.62661996, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, 0.40681825, NA_real_, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	}

	plotData2 <- testGetAnalysisResultsPlotData(x4, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.99501417, 0.9843063, 0.96005739, 0.91353722, 0.83535366, 0.71802165, 0.55995335), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(1, 0.9039239, 0.66761715, 0.40289032, 0.19865977, 0.080038099, 0.026347981), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18")

	x5 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x5' with expected results
	expect_equal(x5$pi1, 0.35, tolerance = 1e-07)
	expect_equal(x5$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x5$conditionalRejectionProbabilities, c(0.033369686, 0.13517192, 0.020135527, NA_real_), tolerance = 1e-07)
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.035340808, 0.15564776, 0.18966473, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.88809228, 0.77284187, 0.5392556, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedPValues, c(0.49999905, 0.49999905, 0.20027888, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalStage, NA_integer_)
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	}

	x6 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x6' with expected results
	expect_equal(x6$pi1, 0.35, tolerance = 1e-07)
	expect_equal(x6$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x6$conditionalRejectionProbabilities, c(0.049321561, 0.20984263, 0.048813265, NA_real_), tolerance = 1e-07)
	expect_equal(x6$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.088987938, 0.19243532, 0.20635802, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.8198195, 0.7374817, 0.52720839, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedPValues, c(0.49999905, 0.27035282, 0.14086509, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalStage, NA_integer_)
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	}

	x7 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = TRUE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x7' with expected results
	expect_equal(x7$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x7$conditionalRejectionProbabilities, c(0.033369686, 0.13517192, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$conditionalPower, c(NA_real_, NA_real_, 0.58576815, 0.82581584), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds, c(0.035340808, 0.15564776, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds, c(0.88809228, 0.77284187, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$finalStage, NA_integer_)
	expect_equal(x7$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	}

	plotData3 <- testGetAnalysisResultsPlotData(x7, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData3' with expected results
	expect_equal(plotData3$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData3$condPowerValues, c(0.099723847, 0.21903134, 0.37478113, 0.54310492, 0.6994843, 0.82581584, 0.91388883), tolerance = 1e-07)
	expect_equal(plotData3$likelihoodValues, c(0.19865977, 0.40289032, 0.66761715, 0.9039239, 1, 0.9039239, 0.66761715), tolerance = 1e-07)
	expect_equal(plotData3$main, "Conditional Power with Likelihood")
	expect_equal(plotData3$xlab, "pi1")
	expect_equal(plotData3$ylab, "Conditional power / Likelihood")
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 18")

	x8 <- getAnalysisResults(design = design1, dataInput = dataExample1,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = TRUE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x8' with expected results
	expect_equal(x8$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x8$conditionalRejectionProbabilities, c(0.049321561, 0.20984263, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$conditionalPower, c(NA_real_, NA_real_, 0.76152324, 0.91259792), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds, c(0.088987938, 0.19243532, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds, c(0.8198195, 0.7374817, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedPValues, c(0.49999905, 0.27035282, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$finalStage, NA_integer_)
	expect_equal(x8$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	}

	plotData4 <- testGetAnalysisResultsPlotData(x8, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData4' with expected results
	expect_equal(plotData4$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData4$condPowerValues, c(0.20983878, 0.3743042, 0.54811429, 0.70471917, 0.82789376, 0.91259792, 0.96272982), tolerance = 1e-07)
	expect_equal(plotData4$likelihoodValues, c(0.19865977, 0.40289032, 0.66761715, 0.9039239, 1, 0.9039239, 0.66761715), tolerance = 1e-07)
	expect_equal(plotData4$main, "Conditional Power with Likelihood")
	expect_equal(plotData4$xlab, "pi1")
	expect_equal(plotData4$ylab, "Conditional power / Likelihood")
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 18")

})

test_that("'getAnalysisResults' for an inverse sequential design and one treatment", {

	.skipTestIfDisabled()

	design2 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1),
		futilityBounds = c(-0.5, 0, 0.5), typeOfDesign = "asKD", gammaA = 2.8)

	dataExample2 <- getDataset(
		n = c(8, 10, 9, 11), # cumsum, overall n = (8, 18, 27, 38)
		events = c(4, 5, 5, 6) # cumsum, overall events = (4, 9, 14, 20)
	)

	x1 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	expect_equal(x1$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, 0.28098687, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, 0.26858957, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, 0.76870152, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.43799317, 0.045574143, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	.skipTestIfDisabled()

	x2 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, 0.78413538, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, 0.31861038, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, 0.72001941, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.1020964, 0.0075111702, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, 3)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, 0.0050707339, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.30413229, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.68870859, NA_real_), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.49547717, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	x3 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.69921202, 0.88465983), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.43799317, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.94793138, 0.88465983, 0.78396384, 0.64581102, 0.48045808, 0.30888816, 0.15917802), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18")

	x4 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsInverseNormal object 'x4' with expected results
	expect_equal(x4$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 0.85385983, 0.95015898), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.49999905, 0.1020964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	}

	plotData2 <- testGetAnalysisResultsPlotData(x4, piTreatmentRange = seq(0.45, 0.75, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.98088099, 0.95015898, 0.89232288, 0.79901831, 0.66708346, 0.50248974, 0.32350374), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.91393119, 1, 0.91393119, 0.69767633, 0.44485807, 0.23692776, 0.10539922), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18")

	x5 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x5' with expected results
	expect_equal(x5$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x5$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, 0.6508521, NA_real_), tolerance = 1e-07)
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, 0.26858957, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, 0.76870152, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedPValues, c(0.49999905, 0.43799317, 0.013282796, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalStage, 3)
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, 0.007752129, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.29554194, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.67875285, NA_real_), tolerance = 1e-07)
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.48769645, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	}

	x6 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x6' with expected results
	expect_equal(x6$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x6$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x6$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, 0.96959663, NA_real_), tolerance = 1e-07)
	expect_equal(x6$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, 0.31861038, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, 0.72001941, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedPValues, c(0.49999905, 0.1020964, 0.0013103922, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalStage, 3)
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, 0.002378519, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.3437363, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.73847376, NA_real_), tolerance = 1e-07)
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.54446903, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	}

	x7 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = FALSE, directionUpper = TRUE) 

	## Comparison of the results of AnalysisResultsInverseNormal object 'x7' with expected results
	expect_equal(x7$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x7$conditionalRejectionProbabilities, c(0.055828724, 0.15918316, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$conditionalPower, c(NA_real_, NA_real_, 0.69921202, 0.88465983), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalLowerBounds, c(0.04626695, 0.16132367, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedConfidenceIntervalUpperBounds, c(0.95373305, 0.83867633, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$repeatedPValues, c(0.49999905, 0.43799317, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$finalStage, NA_integer_)
	expect_equal(x7$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	}

	plotData3 <- testGetAnalysisResultsPlotData(x7, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData3' with expected results
	expect_equal(plotData3$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData3$condPowerValues, c(0.15917802, 0.30888816, 0.48045808, 0.64581102, 0.78396384, 0.88465983, 0.94793138), tolerance = 1e-07)
	expect_equal(plotData3$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData3$main, "Conditional Power with Likelihood")
	expect_equal(plotData3$xlab, "pi1")
	expect_equal(plotData3$ylab, "Conditional power / Likelihood")
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 18")

	x8 <- getAnalysisResults(design = design2, dataInput = dataExample2,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, 
		normalApproximation = TRUE, directionUpper = TRUE) 

	## Comparison of the results of AnalysisResultsInverseNormal object 'x8' with expected results
	expect_equal(x8$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x8$conditionalRejectionProbabilities, c(0.088079629, 0.32350577, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$conditionalPower, c(NA_real_, NA_real_, 0.85385983, 0.95015898), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalLowerBounds, c(0.11314483, 0.21610036, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedConfidenceIntervalUpperBounds, c(0.88685517, 0.78389964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$repeatedPValues, c(0.49999905, 0.1020964, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$finalStage, NA_integer_)
	expect_equal(x8$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	}

	plotData4 <- testGetAnalysisResultsPlotData(x8, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData4' with expected results
	expect_equal(plotData4$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData4$condPowerValues, c(0.32350374, 0.50248974, 0.66708346, 0.79901831, 0.89232288, 0.95015898, 0.98088099), tolerance = 1e-07)
	expect_equal(plotData4$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData4$main, "Conditional Power with Likelihood")
	expect_equal(plotData4$xlab, "pi1")
	expect_equal(plotData4$ylab, "Conditional power / Likelihood")
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 18")

})

test_that("'getAnalysisResults' for a Fisher design and one treatment", {

	.skipTestIfDisabled()

	design3 <- getDesignFisher(kMax = 4, alpha = 0.025, informationRates = c(0.2, 0.4, 0.8, 1))

	dataExample3 <- getDataset(
		n = c(8, 10, 9, 11), # cumsum, overall n = (8, 18, 27, 38)
		events = c(4, 5, 5, 6) # cumsum, overall events = (4, 9, 14, 20)
	)

	x1 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = FALSE, 
		directionUpper = FALSE, iterations = 1000, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	expect_equal(x1$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, 0.018233808, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, 0.23521677, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, 0.79971589, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.23393398, 0.11483365, 0.11050779, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.75, normalApproximation = TRUE, 
		directionUpper = FALSE, iterations = 1000, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x2' with expected results
	expect_equal(x2$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.051264101, 0.1206033, 1, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.18175814, 0.2424364, 0.28642867, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.81824186, 0.7575636, 0.7496417, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.11554509, 0.032131177, 0.024656293, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	x3 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 2, 	thetaH0 = 0.75, nPlanned = c(12, 6), pi1 = 0.5, normalApproximation = FALSE, 
		directionUpper = FALSE, iterations = 1000, seed = 123) 

	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.23393398, 0.11483365, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.522, 0.672), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.997, 0.99, 0.967, 0.9, 0.822, 0.659, 0.534), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 18")

	x4 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = FALSE, 
		directionUpper = TRUE, iterations = 1000, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x4' with expected results
	expect_equal(x4$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x4$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, 0.10237226, NA_real_), tolerance = 1e-07)
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, 0.23521677, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, 0.79971589, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.23393398, 0.11483365, 0.040061917, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	}

	x5 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 3, 	thetaH0 = 0.25, normalApproximation = TRUE, 
		directionUpper = TRUE, iterations = 1000, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x5' with expected results
	expect_equal(x5$pi1, 0.51851852, tolerance = 1e-07)
	expect_equal(x5$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x5$conditionalRejectionProbabilities, c(0.051264101, 0.1206033, 1, NA_real_), tolerance = 1e-07)
	expect_equal(x5$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$repeatedConfidenceIntervalLowerBounds, c(0.18175814, 0.2424364, 0.28642867, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedConfidenceIntervalUpperBounds, c(0.81824186, 0.7575636, 0.7496417, NA_real_), tolerance = 1e-07)
	expect_equal(x5$repeatedPValues, c(0.11554509, 0.032131177, 0.0055275316, NA_real_), tolerance = 1e-07)
	expect_equal(x5$finalStage, NA_integer_)
	expect_equal(x5$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	}

	x6 <- getAnalysisResults(design = design3, dataInput = dataExample3,
		stage = 2, 	thetaH0 = 0.25, nPlanned = c(12,6), pi1 = 0.5, normalApproximation = FALSE, 
		directionUpper = TRUE, iterations = 1000, seed = 123) 

	## Comparison of the results of AnalysisResultsFisher object 'x6' with expected results
	expect_equal(x6$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x6$conditionalRejectionProbabilities, c(0.027980027, 0.040164764, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalLowerBounds, c(0.12025548, 0.19023888, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedConfidenceIntervalUpperBounds, c(0.87974452, 0.80976112, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$repeatedPValues, c(0.23393398, 0.11483365, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$finalStage, NA_integer_)
	expect_equal(x6$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.522, 0.672), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	}

	plotData2 <- testGetAnalysisResultsPlotData(x6, piTreatmentRange = seq(0.25, 0.55, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.039, 0.114, 0.208, 0.363, 0.54, 0.659, 0.817), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.10539922, 0.23692776, 0.44485807, 0.69767633, 0.91393119, 1, 0.91393119), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 18")

})

test_that("'getAnalysisResults' produces the correct exact tests and final CIs", {

	.skipTestIfDisabled()

	dataExample4 <- getDataset(
		n1 = c(29, 70),
		n2 = c(31, 71),
		events1 = c(8, 54),
		events2 = c(6, 45)
	)

	design4 <- getDesignGroupSequential(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x1 <- getAnalysisResults(design4, dataExample4, thetaH0 = 0, stage = 2, directionUpper = TRUE, 
		normalApproximation = FALSE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.62626263, tolerance = 1e-07)
	expect_equal(x1$pi2, 0.5, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "accept"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.019823852, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.26992433, -0.01139735), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.42527237, 0.25916403), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.049955314), tolerance = 1e-07)
	expect_equal(x1$finalStage, 2)
	expect_equal(x1$finalPValues, c(NA_real_, 0.048430128), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.020837679), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.25135839), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, 0.11531433), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	design5 <- getDesignInverseNormal(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x2 <- getAnalysisResults(design5, dataExample4, thetaH0 = 0, stage = 2, directionUpper = TRUE, 
		normalApproximation = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$pi1, 0.62626263, tolerance = 1e-07)
	expect_equal(x2$pi2, 0.5, tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "accept"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.019823852, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.26992433, -0.0037603341), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.42527237, 0.24245614), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.058639455), tolerance = 1e-07)
	expect_equal(x2$finalStage, 2)
	expect_equal(x2$finalPValues, c(NA_real_, 0.056348492), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.025981238), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.24620708), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, 0.11015867), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	design6 <- getDesignFisher(kMax = 2, alpha = 0.025, method = "fullAlpha", 
		informationRates = c(0.3, 1))

	x3 <- getAnalysisResults(design6, dataExample4, thetaH0 = 0, stage = 2, directionUpper = TRUE, 
		normalApproximation = FALSE, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	expect_equal(x3$pi1, 0.62626263, tolerance = 1e-07)
	expect_equal(x3$pi2, 0.5, tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "accept"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.018987489, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.2682578, -0.011427525), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.42375822, 0.25486184), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.06829825), tolerance = 1e-07)
	expect_equal(x3$finalStage, 2)
	expect_equal(x3$finalPValues, c(NA_real_, 0.06829789), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

})

context("Testing the analysis rates functionality for two treatments")


test_that("'getAnalysisResults' for a group sequential design and two treatments", {
	.skipTestIfDisabled()

	design7 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
			typeOfDesign = "WT", deltaWT = 0.25, informationRates = c(0.2, 0.4, 0.8, 1), 
			futilityBounds = c(0, 0.5, 0.8), bindingFutility = T)

	dataExample5 <- getDataset(
		n1 = c(17, 18, 22),
		n2 = c(18, 17, 19),
		events1 = c(11, 12, 17),
		events2 = c(5, 10, 7)
	)

	x1 <- getAnalysisResults(design7, dataExample5, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.19002543, 0.18837824, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.97639752, 0.99770454), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.076268818, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.4944943, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.083297284, 0.074570768, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x1, piTreatmentRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)  			

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.47726473, 0.64780315, 0.79588169, 0.90153211, 0.96202912, 0.98889368, 0.99770454), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618, 0.51184997, 0.20491809), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2")

	# reversed "directionUpper"

	x2 <- getAnalysisResults(design7, dataExample5, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$testActions, c("accept and stop", "accept and stop", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0, 0, NA_real_, NA_real_))
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 6.6613381e-16, 6.6613381e-16), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.076268818, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.4944943, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.21185459, 0.21185459, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, 1)
	expect_equal(x2$finalPValues, c(0.98580558, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(0.03932898, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(0.62730993, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(0.36928105, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	plotData2 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)  				

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 1, 1), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(8.9244677e-08, 2.5604189e-06, 4.9816924e-05, 0.00065732471, 0.0058819346, 0.035694195, 0.14689674), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5")

})

test_that("'getAnalysisResults' for an inverse design and two treatments", {

	.skipTestIfDisabled()

	design8 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
			typeOfDesign = "WT", deltaWT = 0.25, informationRates = c(0.2, 0.4, 0.8, 1), 
			futilityBounds = c(0, 0.5, 0.8), bindingFutility = T)

	dataExample6 <- getDataset(
			n1 = c(17, 18, 22),
			n2 = c(18, 17, 19),
			events1 = c(11, 12, 17),
			events2 = c(5, 10, 7)
	)

	x1 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0.0, stage = 2,  nPlanned = c(30,30), 
		pi2 = 0.2, pi1 = 0.4, directionUpper = T)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.19002543, 0.18093983, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, 0.51829859, 0.74637814), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.078581299, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.083297284, 0.077942413, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x1, piTreatmentRange = seq(0.4, 0.7, 0.05), nPlanned = c(30,30))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.74637814, 0.85191228, 0.92421447, 0.96693166, 0.98816058, 0.99670572, 0.99934119), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.0058819346, 0.035694195, 0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1")

	x2 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0.0, stage = 2,  nPlanned = c(30,30), 
		pi2 = 0.2, pi1 = 0.4, directionUpper = T)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.19002543, 0.18093983, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.51829859, 0.74637814), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.078581299, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.083297284, 0.077942413, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	plotData2 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.4, 0.7, 0.05), nPlanned = c(30, 30))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.74637814, 0.85191228, 0.92421447, 0.96693166, 0.98816058, 0.99670572, 0.99934119), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.0058819346, 0.035694195, 0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1")

	plotData3 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.4, 0.7, 0.05))

	## Comparison of the results of list object 'plotData3' with expected results
	expect_equal(plotData3$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData3$condPowerValues, c(0.74637814, 0.85191228, 0.92421447, 0.96693166, 0.98816058, 0.99670572, 0.99934119), tolerance = 1e-07)
	expect_equal(plotData3$likelihoodValues, c(0.0058819346, 0.035694195, 0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618), tolerance = 1e-07)
	expect_equal(plotData3$main, "Conditional Power with Likelihood")
	expect_equal(plotData3$xlab, "pi1")
	expect_equal(plotData3$ylab, "Conditional power / Likelihood")
	expect_equal(plotData3$sub, "Stage = 2, # of remaining subjects = 60, pi2 = 0.2, allocation ratio = 1")

	x3 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.19002543, 0.18093983, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.97637134, 0.99770045), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.078581299, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.083297284, 0.077942413, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$finalStage, NA_integer_)
	expect_equal(x3$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

	plotData4 <- testGetAnalysisResultsPlotData(x3, piTreatmentRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)  			

	## Comparison of the results of list object 'plotData4' with expected results
	expect_equal(plotData4$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07)
	expect_equal(plotData4$condPowerValues, c(0.4771434, 0.64764919, 0.79574037, 0.90143545, 0.96198044, 0.98887633, 0.99770045), tolerance = 1e-07)
	expect_equal(plotData4$likelihoodValues, c(0.14689674, 0.40998118, 0.77598415, 0.99604498, 0.86704618, 0.51184997, 0.20491809), tolerance = 1e-07)
	expect_equal(plotData4$main, "Conditional Power with Likelihood")
	expect_equal(plotData4$xlab, "pi1")
	expect_equal(plotData4$ylab, "Conditional power / Likelihood")
	expect_equal(plotData4$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2")

	# reversed "directionUpper"

	x4 <- getAnalysisResults(design8, dataExample6, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x4' with expected results
	expect_equal(x4$testActions, c("accept and stop", "accept and stop", NA_character_, NA_character_))
	expect_equal(x4$conditionalRejectionProbabilities, c(0, 0, NA_real_, NA_real_))
	expect_equal(x4$conditionalPower, c(NA_real_, NA_real_, 6.6613381e-16, 6.6613381e-16), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(-0.14000098, -0.078581299, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.72492405, 0.48870113, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.21185459, 0.21185459, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, 1)
	expect_equal(x4$finalPValues, c(0.98580558, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(0.03932898, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(0.62730993, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$medianUnbiasedEstimates, c(0.36928105, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	}

	plotData5 <- testGetAnalysisResultsPlotData(x4, piTreatmentRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5)  		

	## Comparison of the results of list object 'plotData5' with expected results
	expect_equal(plotData5$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07)
	expect_equal(plotData5$condPowerValues, c(6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 6.6613381e-16, 1, 1), tolerance = 1e-07)
	expect_equal(plotData5$likelihoodValues, c(8.9244677e-08, 2.5604189e-06, 4.9816924e-05, 0.00065732471, 0.0058819346, 0.035694195, 0.14689674), tolerance = 1e-07)
	expect_equal(plotData5$main, "Conditional Power with Likelihood")
	expect_equal(plotData5$xlab, "pi1")
	expect_equal(plotData5$ylab, "Conditional power / Likelihood")
	expect_equal(plotData5$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5")

})

test_that("'getAnalysisResults' for a Fisher design and two treatments", {

	.skipTestIfDisabled()

	design9 <- getDesignFisher(kMax = 4, alpha = 0.025, method = "equalAlpha", 
		informationRates = c(0.2, 0.4, 0.8, 1))

	dataExample7 <- getDataset(
			n1 = c(17, 23, 22),
			n2 = c(18, 20, 19),
			events1 = c(11, 12, 17),
			events2 = c(5, 10, 7)
	)

	x1 <- getAnalysisResults(design9, dataExample7, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.4, pi1 = 0.8, directionUpper = TRUE, allocationRatioPlanned = 2, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.13898608, 0.050808351, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.023853561, -0.068378457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.66428984, 0.40418869, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.035427069, 0.088523734, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, NA_integer_)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.925, 0.972), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x1, piTreatmentRange = seq(0.5, 0.8, 0.05), allocationRatioPlanned = 2)  			

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.199, 0.364, 0.506, 0.686, 0.839, 0.927, 0.979), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.63105765, 0.95013529, 0.95013529, 0.63105765, 0.27837883, 0.081561833, 0.015871623), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.4, allocation ratio = 2")

	# reversed "directionUpper"

	x2 <- getAnalysisResults(design9, dataExample7, thetaH0 = 0, stage = 2, nPlanned = c(60,30), 
		pi2 = 0.8, pi1 = 0.4, directionUpper = FALSE, allocationRatioPlanned = 0.5, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.0056634595, 0.0023089469, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.023853561, -0.068378457, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.66428984, 0.40418869, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.49999905, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerSimulated, c(NA_real_, NA_real_, 0.591, 0.788), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	plotData2 <- testGetAnalysisResultsPlotData(x2,piTreatmentRange = seq(0.2, 0.5, 0.05), allocationRatioPlanned = 0.5) 				

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.998, 0.992, 0.967, 0.892, 0.807, 0.623, 0.493), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(1.003982e-05, 0.00017609247, 0.0020513476, 0.015871623, 0.081561833, 0.27837883, 0.63105765), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "pi1")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, # of remaining subjects = 90, pi2 = 0.8, allocation ratio = 0.5")

})

test_that("'getAnalysisResults' produces the correct exact tests and final CIs", {

	.skipTestIfDisabled()

	dataExample8 <- getDataset(
		n2 = c(31, 72),
		n1 = c(30, 69),
		events2 = c(8, 54),
		events1 = c(6, 45)
	)

	design10 <- getDesignGroupSequential(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x1 <- getAnalysisResults(design10, dataExample8, thetaH0 = 0, stage = 2, directionUpper = FALSE, 
		normalApproximation = FALSE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.51515152, tolerance = 1e-07)
	expect_equal(x1$pi2, 0.60194175, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "accept"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.013966781, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.39509356, -0.22101239), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.29306132, 0.050448367), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.49999905, 0.15271161), tolerance = 1e-07)
	expect_equal(x1$finalStage, 2)
	expect_equal(x1$finalPValues, c(NA_real_, 0.13570939), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.21309581), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.059922132), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, -0.076600215), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	design11 <- getDesignInverseNormal(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	x2 <- getAnalysisResults(design11, dataExample8, thetaH0 = 0, stage = 2, directionUpper = FALSE, 
		normalApproximation = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$pi1, 0.51515152, tolerance = 1e-07)
	expect_equal(x2$pi2, 0.60194175, tolerance = 1e-07)
	expect_equal(x2$testActions, c("continue", "accept"))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.013966781, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.39509356, -0.20744996), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.29306132, 0.038390121), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.49999905, 0.171251), tolerance = 1e-07)
	expect_equal(x2$finalStage, 2)
	expect_equal(x2$finalPValues, c(NA_real_, 0.15026298), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.20860056), tolerance = 1e-07)
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.064410651), tolerance = 1e-07)
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, -0.072106127), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	design12 <- getDesignFisher(kMax = 2, alpha = 0.025, method = "fullAlpha", 
		informationRates = c(0.3, 1))

	x3 <- getAnalysisResults(design12, dataExample8, thetaH0 = 0, stage = 2, directionUpper = FALSE, 
		normalApproximation = FALSE, seed = 123)

	## Comparison of the results of AnalysisResultsFisher object 'x3' with expected results
	expect_equal(x3$pi1, 0.51515152, tolerance = 1e-07)
	expect_equal(x3$pi2, 0.60194175, tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "accept"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.016431334, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.39357809, -0.2198965), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.29140184, 0.047490149), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.18563047), tolerance = 1e-07)
	expect_equal(x3$finalStage, 2)
	expect_equal(x3$finalPValues, c(NA_real_, 0.18562957), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

})

test_that("'getAnalysisResults' produes the correct non-inferiority results for a group sequential design", {

	.skipTestIfDisabled()

	design13 <- getDesignGroupSequential(kMax = 2, alpha = 0.025, 
		typeOfDesign = "WT", deltaWT = 0.1, informationRates = c(0.3, 1))

	dataExample9 <- getDataset(
			n1 = c(29, 70),
			n2 = c(31, 71),
			events1 = c(8, 54),
			events2 = c(6, 45)
	)

	x1 <- getAnalysisResults(design13, dataExample9, thetaH0 = -0.1, stage = 2, directionUpper = TRUE, 
		normalApproximation = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$pi1, 0.62626263, tolerance = 1e-07)
	expect_equal(x1$pi2, 0.5, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "reject"))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.1027905, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(-0.26992433, -0.01139735), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(0.42527237, 0.25916403), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.17488831, 0.00058560119), tolerance = 1e-07)
	expect_equal(x1$finalStage, 2)
	expect_equal(x1$finalPValues, c(NA_real_, 0.0012732763), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.016122345), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.26034096), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, 0.12355576), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design13, dataExample9, thetaH0 = -0.1, stage = 1, nPlanned = 40, 
		pi1 = 0.45, pi2 = 0.4, directionUpper = TRUE, normalApproximation = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.1027905, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, 0.38169554), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(-0.26992433, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(0.42527237, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.17488831, NA_real_), tolerance = 1e-07)
	expect_equal(x2$finalStage, NA_integer_)
	expect_equal(x2$finalPValues, c(NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x2$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x2$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	}

	plotData1 <- testGetAnalysisResultsPlotData(x2, piTreatmentRange = seq(0.25, 0.7, 0.05)) 

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.053165998, 0.1027905, 0.17500031, 0.26934912, 0.38169554, 0.50456648, 0.62825352, 0.74249459, 0.83846571, 0.91065807), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.95261056, 0.95859015, 0.67101367, 0.32674624, 0.11068039, 0.026080239, 0.0042749722, 0.00048745649, 3.866511e-05, 2.1334549e-06), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "pi1")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 1, # of remaining subjects = 40, pi2 = 0.4, allocation ratio = 1")

	# non-inferiority, reversed "directionUpper"

	x3 <- getAnalysisResults(design13, dataExample9, thetaH0 = 0.1, stage = 2, directionUpper = FALSE, 
		normalApproximation = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	expect_equal(x3$pi1, 0.62626263, tolerance = 1e-07)
	expect_equal(x3$pi2, 0.5, tolerance = 1e-07)
	expect_equal(x3$testActions, c("continue", "accept"))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.012395218, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_))
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(-0.26992433, -0.01139735), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(0.42527237, 0.25916403), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.49999905, 0.49999905), tolerance = 1e-07)
	expect_equal(x3$finalStage, 2)
	expect_equal(x3$finalPValues, c(NA_real_, 0.64703032), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalLowerBounds, c(NA_real_, -0.0098227452), tolerance = 1e-07)
	expect_equal(x3$finalConfidenceIntervalUpperBounds, c(NA_real_, 0.26218829), tolerance = 1e-07)
	expect_equal(x3$medianUnbiasedEstimates, c(NA_real_, 0.12618258), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

	x4 <- getAnalysisResults(design13, dataExample9, thetaH0 = 0.1, stage = 1, nPlanned = 40, 
		pi1 = 0.4, pi2 = 0.45, directionUpper = FALSE, normalApproximation = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x4' with expected results
	expect_equal(x4$testActions, c("continue", NA_character_))
	expect_equal(x4$conditionalRejectionProbabilities, c(0.012395218, NA_real_), tolerance = 1e-07)
	expect_equal(x4$conditionalPower, c(NA_real_, 0.10084143), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalLowerBounds, c(-0.26992433, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedConfidenceIntervalUpperBounds, c(0.42527237, NA_real_), tolerance = 1e-07)
	expect_equal(x4$repeatedPValues, c(0.49999905, NA_real_), tolerance = 1e-07)
	expect_equal(x4$finalStage, NA_integer_)
	expect_equal(x4$finalPValues, c(NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_))
	expect_equal(x4$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_))
	expect_equal(x4$medianUnbiasedEstimates, c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	}

})

test_that("'getAnalysisResults' with a dataset of rates and without defining a design", {

	.skipTestIfDisabled()

	data <- getDataset(
		n1 = c(10),
		n2 = c(15),
		events1 = c(8),
		events2 = c(6)
	)
	analysisResults1 <- getAnalysisResults(data, alpha = 0.02, beta = 0.9)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
	expect_equal(analysisResults1$pi1, 0.8, tolerance = 1e-07)
	expect_equal(analysisResults1$pi2, 0.4, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, -0.016534096, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, 0.68698827, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults1), NA)))
	    expect_output(print(analysisResults1)$show())
	    invisible(capture.output(expect_error(summary(analysisResults1), NA)))
	    expect_output(summary(analysisResults1)$show())
	}

	analysisResults2 <- getAnalysisResults(data, alpha = 0.02, beta = 0.9, sided = 2)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults2' with expected results
	expect_equal(analysisResults2$pi1, 0.8, tolerance = 1e-07)
	expect_equal(analysisResults2$pi2, 0.4, tolerance = 1e-07)
	expect_equal(analysisResults2$repeatedConfidenceIntervalLowerBounds, -0.071971904, tolerance = 1e-07)
	expect_equal(analysisResults2$repeatedConfidenceIntervalUpperBounds, 0.71345341, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults2), NA)))
	    expect_output(print(analysisResults2)$show())
	    invisible(capture.output(expect_error(summary(analysisResults2), NA)))
	    expect_output(summary(analysisResults2)$show())
	}

	analysisResults3 <- getAnalysisResults(data, alpha = 0.02, beta = 0.9, sided = 2, twoSidedPower = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults3' with expected results
	expect_equal(analysisResults3$pi1, 0.8, tolerance = 1e-07)
	expect_equal(analysisResults3$pi2, 0.4, tolerance = 1e-07)
	expect_equal(analysisResults3$repeatedConfidenceIntervalLowerBounds, -0.071971904, tolerance = 1e-07)
	expect_equal(analysisResults3$repeatedConfidenceIntervalUpperBounds, 0.71345341, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults3), NA)))
	    expect_output(print(analysisResults3)$show())
	    invisible(capture.output(expect_error(summary(analysisResults3), NA)))
	    expect_output(summary(analysisResults3)$show())
	}

})

