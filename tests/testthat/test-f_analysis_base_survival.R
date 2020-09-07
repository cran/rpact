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
#:#  File name: test-f_analysis_base_survival.R
#:#  Creation date: 05 September 2020, 14:25:11
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the analysis survival functionality for the group sequential design")


test_that("'getAnalysisResults' for a group sequential design and survival data", {
	.skipTestIfDisabled()

	design1 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample1 <- getDataset(
		overallEvents = c(8, 15, 22),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design1, dataExample1, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$thetaH1, 3.4437609, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.044563047, 0.46900287, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.58958009, 1.2616605, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(17.013382, 7.0540547, 9.3999049, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.19438137, 0.0054226276, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.013371274, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 1.1500043, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 7.3698391, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 3.0385051, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design1, dataExample1, stage = 2, nPlanned = c(20,40), 
		allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.07432319, 0.044563047, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.24122422, 0.76137238), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.58958009, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(17.013382, 7.0540547, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.16918725, 0.19438137, NA_real_, NA_real_), tolerance = 1e-07)
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

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(1, 2.5, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.042054884, 0.058920703, 0.079860688, 0.10500347, 0.13429067, 0.16748187, 0.20417526, 0.24383962, 0.28585263, 0.32954089, 0.37421781, 0.41921675, 0.46391757, 0.50776612, 0.55028679, 0.59108872, 0.62986668, 0.6663978, 0.70053535, 0.73220037, 0.76137238, 0.78807962, 0.81238956, 0.83439998, 0.85423087, 0.87201737, 0.88790373, 0.90203829, 0.91456948, 0.92564264, 0.93539766), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

	x3 <- getAnalysisResults(design1, dataExample1, thetaH0 = 0.95, stage = 2, 
		nPlanned = c(20, 40), allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.082607165, 0.055558825, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.32497202, 0.83762717), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.58958009, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(17.013382, 7.0540547, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.15076802, 0.16617365, NA_real_, NA_real_), tolerance = 1e-07)
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

	plotData2 <- testGetAnalysisResultsPlotData(x3, thetaRange = seq(1, 2.5, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.073284723, 0.098867221, 0.12923781, 0.16416019, 0.20317699, 0.24565174, 0.29082221, 0.33785714, 0.3859101, 0.43416582, 0.48187662, 0.52838782, 0.57315275, 0.61573859, 0.65582473, 0.69319563, 0.72772995, 0.75938755, 0.7881956, 0.81423493, 0.83762717, 0.85852313, 0.87709283, 0.89351704, 0.9079804, 0.92066609, 0.93175171, 0.94140636, 0.94978864, 0.95704547, 0.96331147), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "Hazard ratio")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

})

test_that("'getAnalysisResults' for a group sequential design and survival data ('directionUpper' reversed)", {

	.skipTestIfDisabled()

	design2 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample2 <- getDataset(
		overallEvents = c(8, 15, 23),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = -c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design2, dataExample2, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsGroupSequential object 'x1' with expected results
	expect_equal(x1$thetaH1, 0.29838114, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.044563047, 0.46900287, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14176244, 0.11175487, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.6961224, 0.79666577, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.19438137, 0.0054226276, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.013371274, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.14177818, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.87223737, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.33724786, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design2, dataExample2, thetaH0 = 1.1, stage = 2, 
		nPlanned = c(20, 40), allocationRatioPlanned = 0.5, thetaH1 = 0.5, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsGroupSequential object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.090339948, 0.066890003, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.40494574, 0.88883511), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14176244, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.6961224, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.13608528, 0.14422583, NA_real_, NA_real_), tolerance = 1e-07)
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

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(0.4, 1, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.97858552, 0.94519604, 0.88883511, 0.81002306, 0.71447863, 0.61071863, 0.50731205, 0.41100476, 0.32600179, 0.25411912, 0.19536859, 0.14863199, 0.11223419), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.92517582, 0.98626675, 0.99928862, 0.9755955, 0.92648393, 0.86161675, 0.78854281, 0.71277663, 0.63811141, 0.56698955, 0.50084781, 0.44040564, 0.38589113), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 0.5")

})

context("Testing the analysis survival functionality for the inverse normal design")


test_that("'getAnalysisResults' for an inverse normal design and survival data", {
	.skipTestIfDisabled()

	design3 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample3 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design3, dataExample3, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	expect_equal(x1$thetaH1, 2.9359555, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.042056716, 0.36917623, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.5816096, 1.1345596, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(17.013382, 6.9683119, 6.6631754, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.20216143, 0.010091808, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.014307783, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 1.121428, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 5.6413216, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 2.6253218, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design3, stage = 2, nPlanned = c(20,40), 
		allocationRatioPlanned = 2, thetaH1 = 2, dataExample3, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.07432319, 0.042056716, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.24122422, 0.76137238), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.5816096, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(17.013382, 6.9683119, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.16918725, 0.20216143, NA_real_, NA_real_), tolerance = 1e-07)
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

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(1, 2.5, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.042054884, 0.058920703, 0.079860688, 0.10500347, 0.13429067, 0.16748187, 0.20417526, 0.24383962, 0.28585263, 0.32954089, 0.37421781, 0.41921675, 0.46391757, 0.50776612, 0.55028679, 0.59108872, 0.62986668, 0.6663978, 0.70053535, 0.73220037, 0.76137238, 0.78807962, 0.81238956, 0.83439998, 0.85423087, 0.87201737, 0.88790373, 0.90203829, 0.91456948, 0.92564264, 0.93539766), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

	x3 <- getAnalysisResults(design3, dataExample3, thetaH0 = 0.95, stage = 2, 
		nPlanned = c(20,40), allocationRatioPlanned = 2, thetaH1 = 2, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x3' with expected results
	expect_equal(x3$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x3$conditionalRejectionProbabilities, c(0.082607165, 0.052483916, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPower, c(NA_real_, NA_real_, 0.32497202, 0.83762717), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalLowerBounds, c(0.50439514, 0.5816096, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedConfidenceIntervalUpperBounds, c(17.013382, 6.9683119, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$repeatedPValues, c(0.15076802, 0.17323655, NA_real_, NA_real_), tolerance = 1e-07)
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

	plotData2 <- testGetAnalysisResultsPlotData(x3, thetaRange = seq(1, 2.5, 0.05))

	## Comparison of the results of list object 'plotData2' with expected results
	expect_equal(plotData2$xValues, c(1, 1.05, 1.1, 1.15, 1.2, 1.25, 1.3, 1.35, 1.4, 1.45, 1.5, 1.55, 1.6, 1.65, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2, 2.05, 2.1, 2.15, 2.2, 2.25, 2.3, 2.35, 2.4, 2.45, 2.5), tolerance = 1e-07)
	expect_equal(plotData2$condPowerValues, c(0.073284723, 0.098867221, 0.12923781, 0.16416019, 0.20317699, 0.24565174, 0.29082221, 0.33785714, 0.3859101, 0.43416582, 0.48187662, 0.52838782, 0.57315275, 0.61573859, 0.65582473, 0.69319563, 0.72772995, 0.75938755, 0.7881956, 0.81423493, 0.83762717, 0.85852313, 0.87709283, 0.89351704, 0.9079804, 0.92066609, 0.93175171, 0.94140636, 0.94978864, 0.95704547, 0.96331147), tolerance = 1e-07)
	expect_equal(plotData2$likelihoodValues, c(0.38589113, 0.43767503, 0.48942229, 0.54046191, 0.59019718, 0.63811141, 0.6837696, 0.72681695, 0.76697506, 0.8040366, 0.83785893, 0.86835727, 0.89549763, 0.91928996, 0.93978142, 0.95705021, 0.97119988, 0.98235405, 0.99065189, 0.99624395, 0.99928862, 0.99994909, 0.9983907, 0.99477877, 0.98927675, 0.98204476, 0.97323838, 0.96300773, 0.95149676, 0.93884271, 0.92517582), tolerance = 1e-07)
	expect_equal(plotData2$main, "Conditional Power with Likelihood")
	expect_equal(plotData2$xlab, "Hazard ratio")
	expect_equal(plotData2$ylab, "Conditional power / Likelihood")
	expect_equal(plotData2$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 2")

})

test_that("'getAnalysisResults' for an inverse normal design and survival data ('directionUpper' reversed)", {

	.skipTestIfDisabled()

	design4 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), bindingFutility = F,
		typeOfDesign = "WT", deltaWT = 0.45, futilityBounds = c(0, 0, 0))

	dataExample4 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = -c(1.52, 1.38, 2.9)
	)

	x1 <- getAnalysisResults(design4, dataExample4, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'x1' with expected results
	expect_equal(x1$thetaH1, 0.34060461, tolerance = 1e-07)
	expect_equal(x1$testActions, c("continue", "continue", "reject and stop", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.07432319, 0.042056716, 0.36917623, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14350678, 0.15007853, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.7193664, 0.88139925, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.16918725, 0.20216143, 0.010091808, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalStage, 3)
	expect_equal(x1$finalPValues, c(NA_real_, NA_real_, 0.014307783, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalLowerBounds, c(NA_real_, NA_real_, 0.17726343, NA_real_), tolerance = 1e-07)
	expect_equal(x1$finalConfidenceIntervalUpperBounds, c(NA_real_, NA_real_, 0.89172021, NA_real_), tolerance = 1e-07)
	expect_equal(x1$medianUnbiasedEstimates, c(NA_real_, NA_real_, 0.38090568, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getAnalysisResults(design4, dataExample4, thetaH0 = 1.1, stage = 2, 
		nPlanned = c(20, 40), allocationRatioPlanned = 0.5, thetaH1 = 0.5, directionUpper = FALSE) 

	## Comparison of the results of AnalysisResultsInverseNormal object 'x2' with expected results
	expect_equal(x2$testActions, c("continue", "continue", NA_character_, NA_character_))
	expect_equal(x2$conditionalRejectionProbabilities, c(0.090339948, 0.063249751, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPower, c(NA_real_, NA_real_, 0.40494574, 0.88883511), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalLowerBounds, c(0.058777136, 0.14350678, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedConfidenceIntervalUpperBounds, c(1.9825732, 1.7193664, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$repeatedPValues, c(0.13608528, 0.15066694, NA_real_, NA_real_), tolerance = 1e-07)
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

	plotData1 <- testGetAnalysisResultsPlotData(x2, thetaRange = seq(0.4, 1, 0.05))

	## Comparison of the results of list object 'plotData1' with expected results
	expect_equal(plotData1$xValues, c(0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1), tolerance = 1e-07)
	expect_equal(plotData1$condPowerValues, c(0.97858552, 0.94519604, 0.88883511, 0.81002306, 0.71447863, 0.61071863, 0.50731205, 0.41100476, 0.32600179, 0.25411912, 0.19536859, 0.14863199, 0.11223419), tolerance = 1e-07)
	expect_equal(plotData1$likelihoodValues, c(0.92517582, 0.98626675, 0.99928862, 0.9755955, 0.92648393, 0.86161675, 0.78854281, 0.71277663, 0.63811141, 0.56698955, 0.50084781, 0.44040564, 0.38589113), tolerance = 1e-07)
	expect_equal(plotData1$main, "Conditional Power with Likelihood")
	expect_equal(plotData1$xlab, "Hazard ratio")
	expect_equal(plotData1$ylab, "Conditional power / Likelihood")
	expect_equal(plotData1$sub, "Stage = 2, maximum number of remaining events = 60, allocation ratio = 0.5")

})

context("Testing the analysis survival functionality for the Fisher design")


test_that("'getAnalysisResults' for a Fisher design and 'bindingFutility = TRUE'", {
	.skipTestIfDisabled()

	design5 <- getDesignFisher(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), alpha0Vec = c(0.6,0.5,0.4), bindingFutility = TRUE)

	dataExample5 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = c(1.52, 1.38, 2)
	)

	x1 <- getAnalysisResults(design5, dataExample5, thetaH1 = 2, allocationRatioPlanned = 2, 
		nPlanned = 50, directionUpper = TRUE, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.046367462, 0.024190775, 0.042101664, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.72028527), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.59937028, 0.5945604, 0.81409304, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(14.31747, 6.9389819, 5.3768854, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.10915739, 0.16855974, 0.081195715, NA_real_), tolerance = 1e-07)
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

})

test_that("'getAnalysisResults' for a Fisher design and 'bindingFutility = TRUE' ('directionUpper' reversed)", {

	.skipTestIfDisabled()

	design6 <- getDesignFisher(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.4, 0.6, 1), alpha0Vec = c(0.6,0.5,0.4), bindingFutility = TRUE)

	dataExample6 <- getDataset(
		overallEvents = c(8, 15, 29),
		overallAllocationRatios = c(1, 1, 1),
		overallLogRanks = -c(1.52, 1.38, 2)
	)

	x1 <- getAnalysisResults(design6, dataExample6, thetaH1 = 0.5, allocationRatioPlanned = 0.5, 
		nPlanned = 50, directionUpper = FALSE, seed = 123456789)

	## Comparison of the results of AnalysisResultsFisher object 'x1' with expected results
	expect_equal(x1$testActions, c("continue", "continue", "continue", NA_character_))
	expect_equal(x1$conditionalRejectionProbabilities, c(0.046367462, 0.024190775, 0.042101664, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPower, c(NA_real_, NA_real_, NA_real_, 0.72028527), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalLowerBounds, c(0.069844861, 0.14411336, 0.18598127, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedConfidenceIntervalUpperBounds, c(1.6684179, 1.6819149, 1.2283608, NA_real_), tolerance = 1e-07)
	expect_equal(x1$repeatedPValues, c(0.10915739, 0.16855974, 0.081195715, NA_real_), tolerance = 1e-07)
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

})

test_that("'getAnalysisResults' with a dataset of survival data and without defining a design", {

	.skipTestIfDisabled()

	data <- getDataset(
		overallEvents = c(38),
		overallAllocationRatios = c(1),
		overallLogRanks = -c(1.72)
	)
	analysisResults1 <- getAnalysisResults(data, alpha = 0.05, beta = 0.1, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults1' with expected results
	expect_equal(analysisResults1$thetaH1, 0.57232877, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalLowerBounds, 0.33564434, tolerance = 1e-07)
	expect_equal(analysisResults1$repeatedConfidenceIntervalUpperBounds, 0.97591411, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults1), NA)))
	    expect_output(print(analysisResults1)$show())
	    invisible(capture.output(expect_error(summary(analysisResults1), NA)))
	    expect_output(summary(analysisResults1)$show())
	}

	analysisResults2 <- getAnalysisResults(data, alpha = 0.05, beta = 0.1, sided = 2)

	## Comparison of the results of AnalysisResultsInverseNormal object 'analysisResults2' with expected results
	expect_equal(analysisResults2$thetaH1, 0.57232877, tolerance = 1e-07)
	expect_equal(analysisResults2$repeatedConfidenceIntervalLowerBounds, 0.3030255, tolerance = 1e-07)
	expect_equal(analysisResults2$repeatedConfidenceIntervalUpperBounds, 1.0809654, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(analysisResults2), NA)))
	    expect_output(print(analysisResults2)$show())
	    invisible(capture.output(expect_error(summary(analysisResults2), NA)))
	    expect_output(summary(analysisResults2)$show())
	}

})

