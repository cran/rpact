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
#:#  File name: test-f_analysis_multiarm_survival.R
#:#  Creation date: 05 September 2020, 14:45:16
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the analysis survival functionality for three or more treatments")

test_that("'getAnalysisResultsMultiArm' with survival data", {

design1 <- getDesignInverseNormal(kMax = 4, alpha = 0.02, futilityBounds = c(-0.5,0,0.5), 
	bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.15,0.4,0.7,1))

design2 <- getDesignFisher(kMax = 4, alpha = 0.02, alpha0Vec = c(0.7,0.5,0.3), method = "equalAlpha", 
	bindingFutility = TRUE, informationRates = c(0.15,0.4,0.7,1))

design3 <- getDesignConditionalDunnett(alpha = 0.02, informationAtInterim = 0.4, secondStageConditioning = TRUE)

# directionUpper = TRUE
dataExample1 <- getDataset(
	events1   = c(25, 32), 
	events2   = c(18, NA),
	events3   = c(22, 36), 
	logRanks1 = c(2.2,1.8),	
	logRanks2 = c(1.99, NA), 
	logRanks3 = c(2.32, 2.11)
)

# directionUpper = FALSE
dataExample2 <- getDataset(
	events1   =  c(25, 32),
	events2   =  c(18, NA),
	events3   =  c(22, 36),
	logRanks1 = -c(2.2,1.8),
	logRanks2 = -c(1.99, NA),
	logRanks3 = -c(2.32, 2.11)
) 


	.skipTestIfDisabled()

	results1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Simes", nPlanned = c(20, 20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results1$thetaH1[2, ], NA_real_)
	expect_equal(results1$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.099060128, 0.34597824, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.099060128, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[3, ], c(0.10409478, 0.36155201, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.85542287, 0.96084748), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results1$conditionalPower[3, ], c(NA_real_, NA_real_, 0.89591404, 0.97755236), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(0.66999589, 0.92133957, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(0.56497761, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[3, ], c(0.68677354, 1.0019952, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(8.6753402, 4.7834346, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(11.555489, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[3, ], c(10.530254, 5.1517124, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[1, ], c(0.2269674, 0.016421332, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[2, ], c(0.2269674, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[3, ], c(0.2031914, 0.014653223, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	}

	results2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Sidak", nPlanned = c(20, 20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results2$thetaH1[2, ], NA_real_)
	expect_equal(results2$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.087726876, 0.32477957, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.087726876, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[3, ], c(0.087726876, 0.32477957, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.84057577, 0.95616208), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results2$conditionalPower[3, ], c(NA_real_, NA_real_, 0.87494835, 0.97227598), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(0.67004761, 0.92264852, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(0.56502933, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[3, ], c(0.68683172, 1.0033911, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(8.6746555, 4.7646013, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(11.554414, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[3, ], c(10.529368, 5.1190343, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[1, ], c(0.29425758, 0.019196518, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[2, ], c(0.29425758, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[3, ], c(0.29425758, 0.019196518, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	}

	results3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Bonferroni", nPlanned = c(20, 20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
	expect_equal(results3$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results3$thetaH1[2, ], NA_real_)
	expect_equal(results3$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.087296086, 0.32253389, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.087296086, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[3, ], c(0.087296086, 0.32253389, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.83892045, 0.95563305), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results3$conditionalPower[3, ], c(NA_real_, NA_real_, 0.87354342, 0.97191491), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(0.66999589, 0.92133957, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(0.56497761, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[3, ], c(0.68677354, 1.0019952, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(8.6753402, 4.7834346, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(11.555489, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[3, ], c(10.530254, 5.1517124, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[1, ], c(0.29727118, 0.019517906, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[2, ], c(0.29727118, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[3, ], c(0.29727118, 0.019517906, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results3), NA)))
	    expect_output(print(results3)$show())
	    invisible(capture.output(expect_error(summary(results3), NA)))
	    expect_output(summary(results3)$show())
	}

	results4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Dunnett", nPlanned = c(20, 20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
	expect_equal(results4$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results4$thetaH1[2, ], NA_real_)
	expect_equal(results4$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.092674349, 0.33571403, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.092674349, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[3, ], c(0.092674349, 0.34705822, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.84840572, 0.95864641), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results4$conditionalPower[3, ], c(NA_real_, NA_real_, 0.88808793, 0.97560748), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(0.67359439, 0.93573219, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(0.56855511, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[3, ], c(0.69070755, 1.0177116, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(8.6289872, 4.672622, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(11.48276, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[3, ], c(10.470287, 5.0057373, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[1, ], c(0.26222086, 0.017707836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[2, ], c(0.26222086, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[3, ], c(0.26222086, 0.016291632, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results4), NA)))
	    expect_output(print(results4)$show())
	    invisible(capture.output(expect_error(summary(results4), NA)))
	    expect_output(summary(results4)$show())
	}

	results5 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Simes", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results5' with expected results
	expect_equal(results5$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results5$thetaH1[2, ], NA_real_)
	expect_equal(results5$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.063658141, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.063658141, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[3, ], c(0.068355597, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(0.80236535, 0.91979364, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(0.69872866, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[3, ], c(0.83230463, 0.99943912, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(7.2441282, 4.6804766, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(9.3435174, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[3, ], c(8.6890109, 4.945118, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[1, ], c(0.048030804, 0.017175686, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[2, ], c(0.048030804, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[3, ], c(0.043466527, 0.015490547, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results5$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results5$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results5), NA)))
	    expect_output(print(results5)$show())
	    invisible(capture.output(expect_error(summary(results5), NA)))
	    expect_output(summary(results5)$show())
	}

	results6 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results6' with expected results
	expect_equal(results6$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results6$thetaH1[2, ], NA_real_)
	expect_equal(results6$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.05386957, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.05386957, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[3, ], c(0.05386957, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(0.80267942, 0.92107469, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(0.699051, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[3, ], c(0.83265239, 1.0008064, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(7.2412937, 4.6645989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(9.339209, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[3, ], c(8.6853868, 4.9198349, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[1, ], c(0.060623094, 0.019828803, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[2, ], c(0.060623094, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[3, ], c(0.060623094, 0.019828803, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results6$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results6$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results6), NA)))
	    expect_output(print(results6)$show())
	    invisible(capture.output(expect_error(summary(results6), NA)))
	    expect_output(summary(results6)$show())
	}

	results7 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results7' with expected results
	expect_equal(results7$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results7$thetaH1[2, ], NA_real_)
	expect_equal(results7$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.053517968, 0.15167457, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.053517968, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[3, ], c(0.053517968, 0.15167457, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(0.80236535, 0.91979364, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(0.69872866, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[3, ], c(0.83230463, 0.99943912, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(7.2441282, 4.6804766, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(9.3435174, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[3, ], c(8.6890109, 4.945118, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[1, ], c(0.061177178, 0.020122534, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[2, ], c(0.061177178, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[3, ], c(0.061177178, 0.020122534, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.7099, 0.8179), tolerance = 1e-05)
	expect_equal(results7$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results7$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.7603, 0.8587), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results7), NA)))
	    expect_output(print(results7)$show())
	    invisible(capture.output(expect_error(summary(results7), NA)))
	    expect_output(summary(results7)$show())
	}

	results8 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
	expect_equal(results8$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results8$thetaH1[2, ], NA_real_)
	expect_equal(results8$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.058013163, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.058013163, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[3, ], c(0.058013163, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(0.81096563, 0.93415368, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(0.7075635, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[3, ], c(0.84181826, 1.0150289, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(7.167304, 4.5866586, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(9.226852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[3, ], c(8.5908161, 4.8344972, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[1, ], c(0.054684576, 0.018406877, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[2, ], c(0.054684576, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[3, ], c(0.054684576, 0.017144215, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results8$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results8$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results8), NA)))
	    expect_output(print(results8)$show())
	    invisible(capture.output(expect_error(summary(results8), NA)))
	    expect_output(summary(results8)$show())
	}

	results9 <- getAnalysisResults(design = design3, dataInput = dataExample1,
						intersectionTest = "Dunnett", directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results9' with expected results
	expect_equal(results9$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results9$thetaH1[2, ], NA_real_)
	expect_equal(results9$thetaH1[3, ], 2.2519241, tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.17566781), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.15231615), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.19263962), tolerance = 1e-05)
	expect_equal(results9$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results9$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$conditionalPower[3, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, 1.1216364), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, 1.2173719), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 3.9109201), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 4.1968966), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[1, ], c(NA_real_, 0.0032883088), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedPValues[3, ], c(NA_real_, 0.0022402228), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results9), NA)))
	    expect_output(print(results9)$show())
	    invisible(capture.output(expect_error(summary(results9), NA)))
	    expect_output(summary(results9)$show())
	}

	results10 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Simes", nPlanned = c(20, 20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results10' with expected results
	expect_equal(results10$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results10$thetaH1[2, ], NA_real_)
	expect_equal(results10$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.099060128, 0.34597824, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.099060128, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[3, ], c(0.10409478, 0.36155201, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_, 0.85542287, 0.96084748), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results10$conditionalPower[3, ], c(NA_real_, NA_real_, 0.89591404, 0.97755236), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(0.11526984, 0.20905477, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(0.086539238, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[3, ], c(0.094964273, 0.19410999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(1.4925478, 1.0853759, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(1.7699843, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[3, ], c(1.4560829, 0.99800903, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[1, ], c(0.2269674, 0.016421332, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[2, ], c(0.2269674, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[3, ], c(0.2031914, 0.014653223, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	}

	results11 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Sidak", nPlanned = c(20, 20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results11' with expected results
	expect_equal(results11$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results11$thetaH1[2, ], NA_real_)
	expect_equal(results11$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.087726876, 0.32477957, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.087726876, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[3, ], c(0.087726876, 0.32477957, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalPower[1, ], c(NA_real_, NA_real_, 0.84057577, 0.95616208), tolerance = 1e-05)
	expect_equal(results11$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results11$conditionalPower[3, ], c(NA_real_, NA_real_, 0.87494835, 0.97227598), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(0.11527835, 0.20988118, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(0.086546789, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[3, ], c(0.094972389, 0.19534936, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(1.49243, 1.0838364, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(1.7698196, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[3, ], c(1.4559604, 0.99662032, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[1, ], c(0.29425758, 0.019196518, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[2, ], c(0.29425758, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[3, ], c(0.29425758, 0.019196518, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results11), NA)))
	    expect_output(print(results11)$show())
	    invisible(capture.output(expect_error(summary(results11), NA)))
	    expect_output(summary(results11)$show())
	}

	results12 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Bonferroni", nPlanned = c(20, 20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results12' with expected results
	expect_equal(results12$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results12$thetaH1[2, ], NA_real_)
	expect_equal(results12$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.087296086, 0.32253389, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.087296086, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[3, ], c(0.087296086, 0.32253389, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalPower[1, ], c(NA_real_, NA_real_, 0.83892045, 0.95563305), tolerance = 1e-05)
	expect_equal(results12$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results12$conditionalPower[3, ], c(NA_real_, NA_real_, 0.87354342, 0.97191491), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(0.11526984, 0.20905477, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(0.086539238, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[3, ], c(0.094964273, 0.19410999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(1.4925478, 1.0853759, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(1.7699843, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[3, ], c(1.4560829, 0.99800903, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[1, ], c(0.29727118, 0.019517906, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[2, ], c(0.29727118, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[3, ], c(0.29727118, 0.019517906, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results12), NA)))
	    expect_output(print(results12)$show())
	    invisible(capture.output(expect_error(summary(results12), NA)))
	    expect_output(summary(results12)$show())
	}

	results13 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Dunnett", nPlanned = c(20, 20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results13' with expected results
	expect_equal(results13$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results13$thetaH1[2, ], NA_real_)
	expect_equal(results13$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.092674349, 0.33571403, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.092674349, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[3, ], c(0.092674349, 0.34705822, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalPower[1, ], c(NA_real_, NA_real_, 0.84840572, 0.95864641), tolerance = 1e-05)
	expect_equal(results13$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results13$conditionalPower[3, ], c(NA_real_, NA_real_, 0.88808793, 0.97560748), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(0.11588846, 0.21401261, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(0.087087079, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[3, ], c(0.095508367, 0.19977077, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(1.4845729, 1.0686818, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(1.758844, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[3, ], c(1.4477907, 0.98259664, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[1, ], c(0.26222086, 0.017707836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[2, ], c(0.26222086, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[3, ], c(0.26222086, 0.016291632, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results13), NA)))
	    expect_output(print(results13)$show())
	    invisible(capture.output(expect_error(summary(results13), NA)))
	    expect_output(summary(results13)$show())
	}

	results14 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Simes", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results14' with expected results
	expect_equal(results14$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results14$thetaH1[2, ], NA_real_)
	expect_equal(results14$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.063658141, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.063658141, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[3, ], c(0.068355597, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(0.13804292, 0.21365359, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(0.10702587, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[3, ], c(0.11508802, 0.20221946, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(1.2463151, 1.0872004, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(1.4311707, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[3, ], c(1.2014829, 1.0005612, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[1, ], c(0.048030804, 0.017175686, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[2, ], c(0.048030804, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[3, ], c(0.043466527, 0.015490547, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results14$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results14$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results14), NA)))
	    expect_output(print(results14)$show())
	    invisible(capture.output(expect_error(summary(results14), NA)))
	    expect_output(summary(results14)$show())
	}

	results15 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Sidak", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results15' with expected results
	expect_equal(results15$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results15$thetaH1[2, ], NA_real_)
	expect_equal(results15$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[1, ], c(0.05386957, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[2, ], c(0.05386957, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[3, ], c(0.05386957, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(0.13809696, 0.21438058, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(0.10707565, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[3, ], c(0.11513589, 0.20325903, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(1.2458274, 1.0856883, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(1.4305108, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[3, ], c(1.2009817, 0.99919429, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[1, ], c(0.060623094, 0.019828803, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[2, ], c(0.060623094, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[3, ], c(0.060623094, 0.019828803, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results15$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results15$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results15), NA)))
	    expect_output(print(results15)$show())
	    invisible(capture.output(expect_error(summary(results15), NA)))
	    expect_output(summary(results15)$show())
	}

	results16 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Bonferroni", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results16' with expected results
	expect_equal(results16$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results16$thetaH1[2, ], NA_real_)
	expect_equal(results16$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.053517968, 0.15167457, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.053517968, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[3, ], c(0.053517968, 0.15167457, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(0.13804292, 0.21365359, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(0.10702587, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[3, ], c(0.11508802, 0.20221946, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(1.2463151, 1.0872004, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(1.4311707, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[3, ], c(1.2014829, 1.0005612, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[1, ], c(0.061177178, 0.020122534, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[2, ], c(0.061177178, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[3, ], c(0.061177178, 0.020122534, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.7099, 0.8179), tolerance = 1e-05)
	expect_equal(results16$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results16$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.7603, 0.8587), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results16), NA)))
	    expect_output(print(results16)$show())
	    invisible(capture.output(expect_error(summary(results16), NA)))
	    expect_output(summary(results16)$show())
	}

	results17 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 1234, iterations = 10000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results17' with expected results
	expect_equal(results17$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results17$thetaH1[2, ], NA_real_)
	expect_equal(results17$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.058013163, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.058013163, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[3, ], c(0.058013163, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(0.13952252, 0.21802365, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(0.10837911, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[3, ], c(0.11640339, 0.20684674, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(1.2330978, 1.0704876, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(1.4133007, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[3, ], c(1.1879048, 0.98519346, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[1, ], c(0.054684576, 0.018406877, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[2, ], c(0.054684576, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[3, ], c(0.054684576, 0.017144215, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results17$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results17$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results17), NA)))
	    expect_output(print(results17)$show())
	    invisible(capture.output(expect_error(summary(results17), NA)))
	    expect_output(summary(results17)$show())
	}

	results18 <- getAnalysisResults(design = design3, dataInput = dataExample2,
						intersectionTest = "Dunnett", directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results18' with expected results
	expect_equal(results18$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results18$thetaH1[2, ], NA_real_)
	expect_equal(results18$thetaH1[3, ], 0.44406471, tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.17566781), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.15231615), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.19263962), tolerance = 1e-05)
	expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$conditionalPower[3, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, 0.25569439), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, 0.23827076), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.89155483), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 0.8214407), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[1, ], c(NA_real_, 0.0032883088), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedPValues[3, ], c(NA_real_, 0.0022402228), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results18), NA)))
	    expect_output(print(results18)$show())
	    invisible(capture.output(expect_error(summary(results18), NA)))
	    expect_output(summary(results18)$show())
	}

})
