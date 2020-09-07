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
#:#  File name: test-f_analysis_multiarm_rates.R
#:#  Creation date: 05 September 2020, 14:41:01
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the analysis rates functionality for three or more treatments")

test_that("'getAnalysisResultsMultiArm' with dataset of rates", {

design1 <- getDesignInverseNormal(kMax = 4, alpha = 0.02, futilityBounds = c(-0.5,0,0.5), 
	bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.15,0.4,0.7,1))

design2 <- getDesignFisher(kMax = 4, alpha = 0.02, alpha0Vec = c(0.7,0.5,0.3), method = "equalAlpha", 
	bindingFutility = TRUE, informationRates = c(0.15,0.4,0.7,1))

design3 <- getDesignConditionalDunnett(alpha = 0.02, informationAtInterim = 0.4, secondStageConditioning = TRUE)

# directionUpper = TRUE
dataExample1 <- getDataset(
	n1 = c(23, 25),
	n2 = c(25, NA),	
	n3 = c(24, 27),	
	n4 = c(22, 29), 
	events1 = c(15, 12), 
	events2 = c(19, NA), 
	events3 = c(18, 22), 
	events4 = c(12, 13))

# directionUpper = FALSE
dataExample2 <- getDataset(
	n1 = c(23, 25), 
	n2 = c(25, NA),	
	n3 = c(24, 27),	
	n4 = c(22, 29), 
	events1 = c(15, 12), 
	events2 = c(19, NA),
	events3 = c(18, 22), 
	events4 = c(21, 25)) 


	.skipTestIfDisabled()

	results1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results1$piTreatments[2, ], NA_real_)
	expect_equal(results1$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results1$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985229, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.038810537, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[3, ], c(0.033880715, 0.37039628, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results1$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81428082, 0.93347167), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33617071, -0.23608296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.2215671, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23714232, -0.0031827562, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51316305, 0.35555518, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(0.58840788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[3, ], c(0.58287276, 0.54898875, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[3, ], c(0.5, 0.013737697, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	}

	results2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results2$piTreatments[2, ], NA_real_)
	expect_equal(results2$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results2$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.024462749, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[3, ], c(0.020995116, 0.21196145, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284718), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results2$conditionalPower[3, ], c(NA_real_, NA_real_, 0.64209753, 0.85310671), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33617071, -0.23608296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.2215671, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23714232, -0.0031827562, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51316305, 0.35555518, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(0.58840788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[3, ], c(0.58287276, 0.54898875, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[3, ], c(0.5, 0.046180679, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	}

	results3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
	expect_equal(results3$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results3$piTreatments[2, ], NA_real_)
	expect_equal(results3$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results3$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985229, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.02977072, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[3, ], c(0.02977072, 0.34464822, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results3$conditionalPower[3, ], c(NA_real_, NA_real_, 0.79293293, 0.92442377), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33614711, -0.23559832, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.22154213, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23711724, -0.00054192973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51314406, 0.35456939, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(0.5883893, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[3, ], c(0.58285522, 0.54835256, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[3, ], c(0.5, 0.016581549, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results3), NA)))
	    expect_output(print(results3)$show())
	    invisible(capture.output(expect_error(summary(results3), NA)))
	    expect_output(summary(results3)$show())
	}

	results4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
	expect_equal(results4$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results4$piTreatments[2, ], NA_real_)
	expect_equal(results4$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results4$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.018245207, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[3, ], c(0.018245207, 0.1923144, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284718), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results4$conditionalPower[3, ], c(NA_real_, NA_real_, 0.6117861, 0.83710728), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33614711, -0.23559832, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.22154213, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23711724, -0.00054192973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51314406, 0.35456939, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(0.5883893, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[3, ], c(0.58285522, 0.54835256, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[3, ], c(0.5, 0.054586348, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results4), NA)))
	    expect_output(print(results4)$show())
	    invisible(capture.output(expect_error(summary(results4), NA)))
	    expect_output(summary(results4)$show())
	}

	results5 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results5' with expected results
	expect_equal(results5$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results5$piTreatments[2, ], NA_real_)
	expect_equal(results5$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results5$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985229, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.028282751, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[3, ], c(0.028282751, 0.3345541, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results5$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78399481, 0.92056041), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33617071, -0.23608296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.2215671, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23714232, -0.0031827562, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51316305, 0.35555518, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(0.58840788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[3, ], c(0.58287276, 0.54898875, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[3, ], c(0.5, 0.01785947, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results5), NA)))
	    expect_output(print(results5)$show())
	    invisible(capture.output(expect_error(summary(results5), NA)))
	    expect_output(summary(results5)$show())
	}

	results6 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results6' with expected results
	expect_equal(results6$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results6$piTreatments[2, ], NA_real_)
	expect_equal(results6$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results6$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.016153759, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[3, ], c(0.016153759, 0.17608688, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284718), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results6$conditionalPower[3, ], c(NA_real_, NA_real_, 0.58459185, 0.82222793), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33617071, -0.23608296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.2215671, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23714232, -0.0031827562, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51316305, 0.35555518, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(0.58840788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[3, ], c(0.58287276, 0.54898875, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[3, ], c(0.5, 0.063002507, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results6), NA)))
	    expect_output(print(results6)$show())
	    invisible(capture.output(expect_error(summary(results6), NA)))
	    expect_output(summary(results6)$show())
	}

	results7 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results7' with expected results
	expect_equal(results7$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results7$piTreatments[2, ], NA_real_)
	expect_equal(results7$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results7$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985229, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.035094837, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[3, ], c(0.035094837, 0.38122177, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results7$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82267404, 0.93695965), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.33434815, -0.23081928, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.21963967, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.23520576, 0.0084264225, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(0.51164006, 0.3485873, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(0.58704231, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[3, ], c(0.5814919, 0.54471327, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[3, ], c(0.5, 0.012697241, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results7), NA)))
	    expect_output(print(results7)$show())
	    invisible(capture.output(expect_error(summary(results7), NA)))
	    expect_output(summary(results7)$show())
	}

	results8 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
	expect_equal(results8$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results8$piTreatments[2, ], NA_real_)
	expect_equal(results8$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results8$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.022055696, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[3, ], c(0.019625609, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.28055857, -0.23664115, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16334758, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17854537, 0.014565093, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(0.4661328, 0.35109531, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54610763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[3, ], c(0.54006864, 0.54440727, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[2, ], c(0.20120204, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[3, ], c(0.23342758, 0.0097847252, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-05)
	expect_equal(results8$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results8$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results8), NA)))
	    expect_output(print(results8)$show())
	    invisible(capture.output(expect_error(summary(results8), NA)))
	    expect_output(summary(results8)$show())
	}

	results9 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results9' with expected results
	expect_equal(results9$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results9$piTreatments[2, ], NA_real_)
	expect_equal(results9$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results9$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(0.015301846, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[3, ], c(0.013807315, 0.11536321, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.28055857, -0.23664115, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16334758, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17854537, 0.014565093, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(0.4661328, 0.35109531, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54610763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[3, ], c(0.54006864, 0.54440727, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[2, ], c(0.31730879, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[3, ], c(0.35852937, 0.027386657, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0))
	expect_equal(results9$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results9$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.56, 0.69), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results9), NA)))
	    expect_output(print(results9)$show())
	    invisible(capture.output(expect_error(summary(results9), NA)))
	    expect_output(summary(results9)$show())
	}

	results10 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results10' with expected results
	expect_equal(results10$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results10$piTreatments[2, ], NA_real_)
	expect_equal(results10$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results10$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.017689143, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[3, ], c(0.017689143, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.28043417, -0.2361668, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16321884, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17841556, 0.01681143, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(0.4660269, 0.35022196, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54601155, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[3, ], c(0.53997145, 0.54380433, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[2, ], c(0.26580762, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[3, ], c(0.26580762, 0.010948206, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-05)
	expect_equal(results10$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results10$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	}

	results11 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results11' with expected results
	expect_equal(results11$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results11$piTreatments[2, ], NA_real_)
	expect_equal(results11$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results11$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.012656338, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[3, ], c(0.012656338, 0.1064468, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.28043417, -0.2361668, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16321884, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17841556, 0.01681143, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(0.4660269, 0.35022196, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54601155, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[3, ], c(0.53997145, 0.54380433, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[2, ], c(0.39658567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[3, ], c(0.39658567, 0.02994536, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0))
	expect_equal(results11$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results11$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.529, 0.675), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results11), NA)))
	    expect_output(print(results11)$show())
	    invisible(capture.output(expect_error(summary(results11), NA)))
	    expect_output(summary(results11)$show())
	}

	results12 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results12' with expected results
	expect_equal(results12$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results12$piTreatments[2, ], NA_real_)
	expect_equal(results12$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results12$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.017007318, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[3, ], c(0.017007318, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.28055857, -0.23664115, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16334758, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17854537, 0.014565093, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(0.4661328, 0.35109531, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54610763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[3, ], c(0.54006864, 0.54440727, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[2, ], c(0.27902457, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[3, ], c(0.27902457, 0.011439347, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-05)
	expect_equal(results12$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results12$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results12), NA)))
	    expect_output(print(results12)$show())
	    invisible(capture.output(expect_error(summary(results12), NA)))
	    expect_output(summary(results12)$show())
	}

	results13 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results13' with expected results
	expect_equal(results13$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results13$piTreatments[2, ], NA_real_)
	expect_equal(results13$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results13$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.011800065, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[3, ], c(0.011800065, 0.099420037, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.28055857, -0.23664115, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.16334758, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17854537, 0.014565093, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(0.4661328, 0.35109531, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54610763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[3, ], c(0.54006864, 0.54440727, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[2, ], c(0.4293014, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[3, ], c(0.4293014, 0.032289487, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0))
	expect_equal(results13$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results13$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.506, 0.665), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results13), NA)))
	    expect_output(print(results13)$show())
	    invisible(capture.output(expect_error(summary(results13), NA)))
	    expect_output(summary(results13)$show())
	}

	results14 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results14' with expected results
	expect_equal(results14$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results14$piTreatments[2, ], NA_real_)
	expect_equal(results14$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results14$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.020213007, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[3, ], c(0.020213007, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.27679385, -0.23136474, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.15945093, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.17461613, 0.023744131, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(0.46290904, 0.34491674, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(0.54319528, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[3, ], c(0.53712182, 0.54014893, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[2, ], c(0.22488458, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[3, ], c(0.22488458, 0.0091963094, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-05)
	expect_equal(results14$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results14$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results14), NA)))
	    expect_output(print(results14)$show())
	    invisible(capture.output(expect_error(summary(results14), NA)))
	    expect_output(summary(results14)$show())
	}

	results15 <- getAnalysisResults(design = design3, dataInput = dataExample1,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results15' with expected results
	expect_equal(results15$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results15$piTreatments[2, ], NA_real_)
	expect_equal(results15$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results15$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.019942093), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.049973806), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.043516196), tolerance = 1e-05)
	expect_equal(results15$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results15$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results15$conditionalPower[3, ], c(NA_real_, NA_real_))
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.15273762), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, 0.084324837), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.29385853), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 0.49662495), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[1, ], c(NA_real_, 0.26025152), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[2, ], c(NA_real_, NA_real_))
	expect_equal(results15$repeatedPValues[3, ], c(NA_real_, 0.0018034409), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results15), NA)))
	    expect_output(print(results15)$show())
	    invisible(capture.output(expect_error(summary(results15), NA)))
	    expect_output(summary(results15)$show())
	}

	results16 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results16' with expected results
	expect_equal(results16$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results16$piTreatments[2, ], NA_real_)
	expect_equal(results16$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results16$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.11406435, 0.70828238, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[3, ], c(0.086909033, 0.061693402, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalPower[1, ], c(NA_real_, NA_real_, 0.98900857, 0.99826811), tolerance = 1e-05)
	expect_equal(results16$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results16$conditionalPower[3, ], c(NA_real_, NA_real_, 0.12043379, 0.28181072), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63361963, -0.5823635, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52674718, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54212421, -0.35656989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(0.097967144, -0.079768101, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18414788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17705489, 0.1249415, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[1, ], c(0.16441222, 0.00092987693, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[3, ], c(0.30001108, 0.22922856, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results16), NA)))
	    expect_output(print(results16)$show())
	    invisible(capture.output(expect_error(summary(results16), NA)))
	    expect_output(summary(results16)$show())
	}

	results17 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results17' with expected results
	expect_equal(results17$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results17$piTreatments[2, ], NA_real_)
	expect_equal(results17$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results17$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.076949018, 0.52315965, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.055973245, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[3, ], c(0.055973245, 0.020679317, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95949476, 0.99262252), tolerance = 1e-05)
	expect_equal(results17$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results17$conditionalPower[3, ], c(NA_real_, NA_real_, 0.037209808, 0.14447518), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63361963, -0.5823635, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52674718, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54212421, -0.35656989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(0.097967144, -0.079768101, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18414788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17705489, 0.1249415, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[1, ], c(0.38274502, 0.0044680016, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[3, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results17), NA)))
	    expect_output(print(results17)$show())
	    invisible(capture.output(expect_error(summary(results17), NA)))
	    expect_output(summary(results17)$show())
	}

	results18 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results18' with expected results
	expect_equal(results18$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results18$piTreatments[2, ], NA_real_)
	expect_equal(results18$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results18$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[1, ], c(0.11433533, 0.70886969, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[2, ], c(0.065561427, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[3, ], c(0.065561427, 0.04545846, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_, 0.98906324, 0.99827754), tolerance = 1e-05)
	expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results18$conditionalPower[3, ], c(NA_real_, NA_real_, 0.088056152, 0.23515644), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63360262, -0.58192083, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52672767, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54210537, -0.35424882, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(0.097937994, -0.080540243, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18411998, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17702685, 0.12415127, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[1, ], c(0.16348906, 0.00092510856, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[3, ], c(0.5, 0.30237619, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results18), NA)))
	    expect_output(print(results18)$show())
	    invisible(capture.output(expect_error(summary(results18), NA)))
	    expect_output(summary(results18)$show())
	}

	results19 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results19' with expected results
	expect_equal(results19$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results19$piTreatments[2, ], NA_real_)
	expect_equal(results19$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results19$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results19$conditionalRejectionProbabilities[1, ], c(0.077470165, 0.5249466, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$conditionalRejectionProbabilities[2, ], c(0.039196557, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$conditionalRejectionProbabilities[3, ], c(0.039196557, 0.013566941, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95993096, 0.99271232), tolerance = 1e-05)
	expect_equal(results19$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results19$conditionalPower[3, ], c(NA_real_, NA_real_, 0.022770873, 0.11010622), tolerance = 1e-05)
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63360262, -0.58192083, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52672767, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54210537, -0.35424882, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[1, ], c(0.097937994, -0.080540243, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18411998, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17702685, 0.12415127, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedPValues[1, ], c(0.37775541, 0.0044069665, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results19$repeatedPValues[3, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results19), NA)))
	    expect_output(print(results19)$show())
	    invisible(capture.output(expect_error(summary(results19), NA)))
	    expect_output(summary(results19)$show())
	}

	results20 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results20' with expected results
	expect_equal(results20$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results20$piTreatments[2, ], NA_real_)
	expect_equal(results20$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results20$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results20$conditionalRejectionProbabilities[1, ], c(0.11406435, 0.70828238, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$conditionalRejectionProbabilities[2, ], c(0.065068496, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$conditionalRejectionProbabilities[3, ], c(0.065068496, 0.045086324, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$conditionalPower[1, ], c(NA_real_, NA_real_, 0.98900857, 0.99826811), tolerance = 1e-05)
	expect_equal(results20$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results20$conditionalPower[3, ], c(NA_real_, NA_real_, 0.087302678, 0.23399862), tolerance = 1e-05)
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63361963, -0.5823635, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52674718, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54212421, -0.35656989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[1, ], c(0.097967144, -0.079768101, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18414788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17705489, 0.1249415, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedPValues[1, ], c(0.16441222, 0.00092987693, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results20$repeatedPValues[3, ], c(0.5, 0.30450192, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results20), NA)))
	    expect_output(print(results20)$show())
	    invisible(capture.output(expect_error(summary(results20), NA)))
	    expect_output(summary(results20)$show())
	}

	results21 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results21' with expected results
	expect_equal(results21$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results21$piTreatments[2, ], NA_real_)
	expect_equal(results21$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results21$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results21$conditionalRejectionProbabilities[1, ], c(0.076949018, 0.52315965, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$conditionalRejectionProbabilities[2, ], c(0.038318782, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$conditionalRejectionProbabilities[3, ], c(0.038318782, 0.01320671, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95949476, 0.99262252), tolerance = 1e-05)
	expect_equal(results21$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results21$conditionalPower[3, ], c(NA_real_, NA_real_, 0.022053077, 0.10818661), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63361963, -0.5823635, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52674718, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54212421, -0.35656989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[1, ], c(0.097967144, -0.079768101, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18414788, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17705489, 0.1249415, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedPValues[1, ], c(0.38274502, 0.0044680016, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedPValues[3, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results21), NA)))
	    expect_output(print(results21)$show())
	    invisible(capture.output(expect_error(summary(results21), NA)))
	    expect_output(summary(results21)$show())
	}

	results22 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results22' with expected results
	expect_equal(results22$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results22$piTreatments[2, ], NA_real_)
	expect_equal(results22$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results22$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results22$conditionalRejectionProbabilities[1, ], c(0.11938655, 0.7207883, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$conditionalRejectionProbabilities[2, ], c(0.069087084, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$conditionalRejectionProbabilities[3, ], c(0.069087084, 0.048124183, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99013266, 0.99846089), tolerance = 1e-05)
	expect_equal(results22$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results22$conditionalPower[3, ], c(NA_real_, NA_real_, 0.09343954, 0.24332023), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.63233547, -0.57845053, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.52528387, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.54066479, -0.34682167, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[1, ], c(0.095718512, -0.086484277, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[2, ], c(0.18199564, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[3, ], c(0.17489121, 0.11961094, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedPValues[1, ], c(0.14736628, 0.00082115827, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedPValues[2, ], c(0.46923167, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedPValues[3, ], c(0.46923167, 0.28786416, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results22), NA)))
	    expect_output(print(results22)$show())
	    invisible(capture.output(expect_error(summary(results22), NA)))
	    expect_output(summary(results22)$show())
	}

	results23 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results23' with expected results
	expect_equal(results23$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results23$piTreatments[2, ], NA_real_)
	expect_equal(results23$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results23$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results23$conditionalRejectionProbabilities[1, ], c(0.078339465, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$conditionalRejectionProbabilities[3, ], c(0.053203298, 0.026318573, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59388835, -0.58107184, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48211848, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49748465, -0.34071736, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[1, ], c(0.031402797, -0.081544042, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[2, ], c(0.12009821, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1127058, 0.11918993, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedPValues[1, ], c(0.035870527, 0.0012045346, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedPValues[3, ], c(0.061679763, 0.061679763, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results23$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results23$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.084, 0.145), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results23), NA)))
	    expect_output(print(results23)$show())
	    invisible(capture.output(expect_error(summary(results23), NA)))
	    expect_output(summary(results23)$show())
	}

	results24 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results24' with expected results
	expect_equal(results24$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results24$piTreatments[2, ], NA_real_)
	expect_equal(results24$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results24$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results24$conditionalRejectionProbabilities[1, ], c(0.045496227, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$conditionalRejectionProbabilities[2, ], c(0.031516943, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$conditionalRejectionProbabilities[3, ], c(0.031516943, 0.011741452, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59388835, -0.58107184, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48211848, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49748465, -0.34071736, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[1, ], c(0.031402797, -0.081544042, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[2, ], c(0.12009821, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1127058, 0.11918993, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedPValues[1, ], c(0.076563728, 0.0048857101, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedPValues[2, ], c(0.12598399, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedPValues[3, ], c(0.12598399, 0.12598399, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results24$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results24$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.043, 0.085), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results24), NA)))
	    expect_output(print(results24)$show())
	    invisible(capture.output(expect_error(summary(results24), NA)))
	    expect_output(summary(results24)$show())
	}

	results25 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results25' with expected results
	expect_equal(results25$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results25$piTreatments[2, ], NA_real_)
	expect_equal(results25$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results25$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results25$conditionalRejectionProbabilities[1, ], c(0.078624036, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$conditionalRejectionProbabilities[2, ], c(0.037550772, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$conditionalRejectionProbabilities[3, ], c(0.037550772, 0.018728465, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59379831, -0.58064923, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48201888, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49738474, -0.33870554, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[1, ], c(0.031259231, -0.082256979, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[2, ], c(0.11995927, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1125663, 0.1184188, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedPValues[1, ], c(0.035687422, 0.0011997662, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedPValues[2, ], c(0.099510994, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedPValues[3, ], c(0.099510994, 0.099510994, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results25$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results25$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.063, 0.113), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results25), NA)))
	    expect_output(print(results25)$show())
	    invisible(capture.output(expect_error(summary(results25), NA)))
	    expect_output(summary(results25)$show())
	}

	results26 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results26' with expected results
	expect_equal(results26$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results26$piTreatments[2, ], NA_real_)
	expect_equal(results26$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results26$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results26$conditionalRejectionProbabilities[1, ], c(0.045881369, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$conditionalRejectionProbabilities[2, ], c(0.022251097, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$conditionalRejectionProbabilities[3, ], c(0.022251097, 0.0083198991, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59379831, -0.58064923, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48201888, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49738474, -0.33870554, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[1, ], c(0.031259231, -0.082256979, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[2, ], c(0.11995927, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1125663, 0.1184188, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedPValues[1, ], c(0.075680628, 0.0048332581, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedPValues[2, ], c(0.19893611, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedPValues[3, ], c(0.19893611, 0.19893611, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results26$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results26$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.033, 0.061), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results26), NA)))
	    expect_output(print(results26)$show())
	    invisible(capture.output(expect_error(summary(results26), NA)))
	    expect_output(summary(results26)$show())
	}

	results27 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results27' with expected results
	expect_equal(results27$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results27$piTreatments[2, ], NA_real_)
	expect_equal(results27$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results27$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results27$conditionalRejectionProbabilities[1, ], c(0.078339465, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$conditionalRejectionProbabilities[2, ], c(0.037226445, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$conditionalRejectionProbabilities[3, ], c(0.037226445, 0.01856996, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59388835, -0.58107184, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48211848, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49748465, -0.34071736, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[1, ], c(0.031402797, -0.081544042, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[2, ], c(0.12009821, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1127058, 0.11918993, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedPValues[1, ], c(0.035870527, 0.0012045346, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedPValues[2, ], c(0.10068878, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedPValues[3, ], c(0.10068878, 0.10068878, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results27$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results27$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.063, 0.112), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results27), NA)))
	    expect_output(print(results27)$show())
	    invisible(capture.output(expect_error(summary(results27), NA)))
	    expect_output(summary(results27)$show())
	}

	results28 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results28' with expected results
	expect_equal(results28$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results28$piTreatments[2, ], NA_real_)
	expect_equal(results28$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results28$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results28$conditionalRejectionProbabilities[1, ], c(0.045496227, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$conditionalRejectionProbabilities[2, ], c(0.021807866, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$conditionalRejectionProbabilities[3, ], c(0.021807866, 0.0081554619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59388835, -0.58107184, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.48211848, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49748465, -0.34071736, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[1, ], c(0.031402797, -0.081544042, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[2, ], c(0.12009821, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[3, ], c(0.1127058, 0.11918993, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedPValues[1, ], c(0.076563728, 0.0048857101, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedPValues[2, ], c(0.20413935, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedPValues[3, ], c(0.20413935, 0.20413935, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results28$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results28$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.033, 0.061), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results28), NA)))
	    expect_output(print(results28)$show())
	    invisible(capture.output(expect_error(summary(results28), NA)))
	    expect_output(summary(results28)$show())
	}

	results29 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results29' with expected results
	expect_equal(results29$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results29$piTreatments[2, ], NA_real_)
	expect_equal(results29$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results29$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results29$conditionalRejectionProbabilities[1, ], c(0.084062149, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$conditionalRejectionProbabilities[2, ], c(0.039916869, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$conditionalRejectionProbabilities[3, ], c(0.039916869, 0.019883243, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.59115949, -0.57714325, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.47910224, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[3, ], c(-0.49445866, -0.33300782, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[1, ], c(0.027065714, -0.087788801, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[2, ], c(0.11589928, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[3, ], c(0.10849067, 0.1137779, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedPValues[1, ], c(0.032465916, 0.0010853255, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedPValues[2, ], c(0.091578347, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedPValues[3, ], c(0.091578347, 0.091578347, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results29$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results29$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.066, 0.118), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results29), NA)))
	    expect_output(print(results29)$show())
	    invisible(capture.output(expect_error(summary(results29), NA)))
	    expect_output(summary(results29)$show())
	}

	results30 <- getAnalysisResults(design = design3, dataInput = dataExample2,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results30' with expected results
	expect_equal(results30$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results30$piTreatments[2, ], NA_real_)
	expect_equal(results30$piTreatments[3, ], 0.78431373, tolerance = 1e-05)
	expect_equal(results30$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results30$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.19523583), tolerance = 1e-05)
	expect_equal(results30$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.13026808), tolerance = 1e-05)
	expect_equal(results30$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.13280489), tolerance = 1e-05)
	expect_equal(results30$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results30$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results30$conditionalPower[3, ], c(NA_real_, NA_real_))
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.51585674), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, -0.28438473), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, -0.14259434), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 0.062195778), tolerance = 1e-05)
	expect_equal(results30$repeatedPValues[1, ], c(NA_real_, 0.00011067233), tolerance = 1e-05)
	expect_equal(results30$repeatedPValues[2, ], c(NA_real_, NA_real_))
	expect_equal(results30$repeatedPValues[3, ], c(NA_real_, 0.058156897), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results30), NA)))
	    expect_output(print(results30)$show())
	    invisible(capture.output(expect_error(summary(results30), NA)))
	    expect_output(summary(results30)$show())
	}

})
