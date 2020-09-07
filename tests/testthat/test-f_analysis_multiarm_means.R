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
#:#  File name: test-f_analysis_multiarm_means.R
#:#  Creation date: 05 September 2020, 14:25:43
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the analysis means functionality for three or more treatments")

test_that("'getAnalysisResultsMultiArm' with dataset of means", {

design1 <- getDesignInverseNormal(kMax = 4, alpha = 0.02, futilityBounds = c(-0.5,0,0.5), 
	bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.15,0.4,0.7,1))

design2 <- getDesignFisher(kMax = 4, alpha = 0.02, alpha0Vec = c(0.7,0.5,0.3), method = "equalAlpha", 
	bindingFutility = TRUE, informationRates = c(0.15,0.4,0.7,1))

design3 <- getDesignConditionalDunnett(alpha = 0.02, informationAtInterim = 0.4, secondStageConditioning = TRUE)

# directionUpper = TRUE
dataExample1 <- getDataset(
	n1 = c(13, 25), 
	n2 = c(15, NA), 
	n3 = c(14, 27), 
	n4 = c(12, 29), 
	means1 = c(24.2, 22.2), 
	means2 = c(18.8, NA),
	means3 = c(26.7, 27.7), 
	means4 = c(9.2, 12.2), 
	stDevs1 = c(24.4, 22.1), 
	stDevs2 = c(21.2, NA), 
	stDevs3 = c(25.6, 23.2), 
	stDevs4 = c(21.5, 22.7))

# directionUpper = FALSE
dataExample2 <- getDataset(
	n1 = c(13, 25), 
	n2 = c(15, NA), 
	n3 = c(14, 27),	
	n4 = c(12, 29), 
	means1 = -c(24.2, 22.2), 
	means2 = -c(18.8, NA), 
	means3 = -c(26.7, 27.7), 
	means4 = -c(9.2, 12.2), 
	stDevs1 = c(24.4, 22.1), 
	stDevs2 = c(21.2, NA), 
	stDevs3 = c(25.6, 23.2), 
	stDevs4 = c(21.5, 22.7)) 


	.skipTestIfDisabled()

	results1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results1$thetaH1[2, ], NA_real_)
	expect_equal(results1$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results1$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results1$assumedStDevs[2, ], NA_real_)
	expect_equal(results1$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[3, ], c(0.052717287, 0.35672949, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196347), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results1$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83948961, 0.95090316), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.395028, -4.0669225, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.071338, 0.24154039, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(44.395028, 27.895908, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[3, ], c(47.071339, 32.285152, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[3, ], c(0.5, 0.015177743, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	}

	results2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results2$thetaH1[2, ], NA_real_)
	expect_equal(results2$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results2$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results2$assumedStDevs[2, ], NA_real_)
	expect_equal(results2$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[3, ], c(0.048708233, 0.3133215, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767621, 0.6794713), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results2$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80590445, 0.93881804), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.433727, -4.7641753, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.426744, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[3, ], c(-15.938808, -0.40329842, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(48.433726, 28.584393, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[3, ], c(50.938808, 32.927366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[3, ], c(0.5, 0.020901685, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	}

	results3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
	expect_equal(results3$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results3$thetaH1[2, ], NA_real_)
	expect_equal(results3$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results3$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results3$assumedStDevs[2, ], NA_real_)
	expect_equal(results3$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198144, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[3, ], c(0.051237296, 0.36121246, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results3$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84263383, 0.95200602), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.802158, -4.2854677, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.232722, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.786808, 0.41764291, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(44.802158, 28.113846, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[3, ], c(46.786808, 32.10754, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[3, ], c(0.5, 0.014689462, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results3), NA)))
	    expect_output(print(results3)$show())
	    invisible(capture.output(expect_error(summary(results3), NA)))
	    expect_output(summary(results3)$show())
	}

	results4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
	expect_equal(results4$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results4$thetaH1[2, ], NA_real_)
	expect_equal(results4$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results4$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results4$assumedStDevs[2, ], NA_real_)
	expect_equal(results4$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.04074021, 0.14372404, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[3, ], c(0.049414261, 0.33374326, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results4$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82244694, 0.94484021), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.56757, -4.6627973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.940705, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.521691, 0.049006954, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(46.567569, 28.528695, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[3, ], c(48.521691, 32.491815, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[3, ], c(0.5, 0.017966281, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results4), NA)))
	    expect_output(print(results4)$show())
	    invisible(capture.output(expect_error(summary(results4), NA)))
	    expect_output(summary(results4)$show())
	}

	results5 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results5' with expected results
	expect_equal(results5$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results5$thetaH1[2, ], NA_real_)
	expect_equal(results5$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results5$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results5$assumedStDevs[2, ], NA_real_)
	expect_equal(results5$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.043219832, 0.15803857, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[3, ], c(0.052145589, 0.35513472, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results5$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83835744, 0.95050484), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.549821, -4.1214, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.483404, 0.16013937, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(44.549821, 27.945069, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[3, ], c(47.483404, 32.356999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[3, ], c(0.5, 0.015356079, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results5), NA)))
	    expect_output(print(results5)$show())
	    invisible(capture.output(expect_error(summary(results5), NA)))
	    expect_output(summary(results5)$show())
	}

	results6 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results6' with expected results
	expect_equal(results6$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results6$thetaH1[2, ], NA_real_)
	expect_equal(results6$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results6$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results6$assumedStDevs[2, ], NA_real_)
	expect_equal(results6$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.037322006, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[3, ], c(0.048226967, 0.31219358, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684079), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results6$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80494934, 0.93846621), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.601467, -4.8144339, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.153638, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[3, ], c(-16.403927, -0.48327176, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(48.601467, 28.627869, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[3, ], c(51.403927, 32.999307, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[3, ], c(0.5, 0.021078114, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results6), NA)))
	    expect_output(print(results6)$show())
	    invisible(capture.output(expect_error(summary(results6), NA)))
	    expect_output(summary(results6)$show())
	}

	results7 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results7' with expected results
	expect_equal(results7$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results7$thetaH1[2, ], NA_real_)
	expect_equal(results7$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results7$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results7$assumedStDevs[2, ], NA_real_)
	expect_equal(results7$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.044513617, 0.16250147, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[3, ], c(0.049538053, 0.34419132, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46254707, 0.70494474), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results7$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83038985, 0.94768376), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.393216, -4.0328452, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.889915, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.069516, 0.29402598, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(44.393216, 27.725836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(36.089915, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[3, ], c(47.069516, 32.182569, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[1, ], c(0.5, 0.071351909, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[3, ], c(0.5, 0.016637815, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results7), NA)))
	    expect_output(print(results7)$show())
	    invisible(capture.output(expect_error(summary(results7), NA)))
	    expect_output(summary(results7)$show())
	}

	results8 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results8' with expected results
	expect_equal(results8$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results8$thetaH1[2, ], NA_real_)
	expect_equal(results8$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results8$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results8$assumedStDevs[2, ], NA_real_)
	expect_equal(results8$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.040941914, 0.14648989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[3, ], c(0.043912864, 0.29382832, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalPower[1, ], c(NA_real_, NA_real_, 0.4325103, 0.68306799), tolerance = 1e-05)
	expect_equal(results8$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results8$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78874215, 0.93242714), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.431163, -4.7231897, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.424453, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[3, ], c(-15.936268, -0.34247221, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(48.431163, 28.407231, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(39.624453, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[3, ], c(50.936268, 32.815818, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[1, ], c(0.5, 0.083136439, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[3, ], c(0.5, 0.024192808, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results8), NA)))
	    expect_output(print(results8)$show())
	    invisible(capture.output(expect_error(summary(results8), NA)))
	    expect_output(summary(results8)$show())
	}

	results9 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results9' with expected results
	expect_equal(results9$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results9$thetaH1[2, ], NA_real_)
	expect_equal(results9$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results9$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results9$assumedStDevs[2, ], NA_real_)
	expect_equal(results9$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(0.043192759, 0.15430882, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[3, ], c(0.050842102, 0.35990794, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44741758, 0.69402491), tolerance = 1e-05)
	expect_equal(results9$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results9$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84172467, 0.95168763), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.800321, -4.2506388, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.230944, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.785003, 0.46968004, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(44.800321, 27.943326, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(38.430944, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[3, ], c(46.785003, 32.005071, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[1, ], c(0.5, 0.077086341, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[3, ], c(0.5, 0.014829652, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results9), NA)))
	    expect_output(print(results9)$show())
	    invisible(capture.output(expect_error(summary(results9), NA)))
	    expect_output(summary(results9)$show())
	}

	results10 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results10' with expected results
	expect_equal(results10$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results10$thetaH1[2, ], NA_real_)
	expect_equal(results10$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results10$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results10$assumedStDevs[2, ], NA_real_)
	expect_equal(results10$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.041569453, 0.14613212, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[3, ], c(0.047839714, 0.32760313, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_, 0.43181681, 0.68255335), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results10$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81761872, 0.94309649), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.565416, -4.6248784, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.938622, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.519575, 0.10461547, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(46.565416, 28.357046, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(40.138622, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[3, ], c(48.519575, 32.386196, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[1, ], c(0.5, 0.083428262, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[3, ], c(0.5, 0.018799791, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	}

	results11 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results11' with expected results
	expect_equal(results11$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results11$thetaH1[2, ], NA_real_)
	expect_equal(results11$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results11$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results11$assumedStDevs[2, ], NA_real_)
	expect_equal(results11$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.044003076, 0.16034604, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[3, ], c(0.047740982, 0.33733332, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45861366, 0.70212467), tolerance = 1e-05)
	expect_equal(results11$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results11$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82521432, 0.94583446), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.548, -4.0869289, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.846937, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.481556, 0.21501795, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(44.548, 27.773536, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(36.046937, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[3, ], c(47.481556, 32.250037, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[1, ], c(0.5, 0.072804352, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[3, ], c(0.5, 0.017498028, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results11), NA)))
	    expect_output(print(results11)$show())
	    invisible(capture.output(expect_error(summary(results11), NA)))
	    expect_output(summary(results11)$show())
	}

	results12 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results12' with expected results
	expect_equal(results12$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results12$thetaH1[2, ], NA_real_)
	expect_equal(results12$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results12$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results12$assumedStDevs[2, ], NA_real_)
	expect_equal(results12$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.040514523, 0.14472681, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.037322006, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[3, ], c(0.042460333, 0.28832504, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42908294, 0.68052019), tolerance = 1e-05)
	expect_equal(results12$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results12$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78363361, 0.93049656), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.598892, -4.7729883, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.151395, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[3, ], c(-16.401351, -0.41981683, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(48.598892, 28.449073, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(39.351395, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[3, ], c(51.401351, 32.883177, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[1, ], c(0.5, 0.084586974, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[3, ], c(0.5, 0.025221821, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results12), NA)))
	    expect_output(print(results12)$show())
	    invisible(capture.output(expect_error(summary(results12), NA)))
	    expect_output(summary(results12)$show())
	}

	results13 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results13' with expected results
	expect_equal(results13$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results13$thetaH1[2, ], NA_real_)
	expect_equal(results13$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results13$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results13$assumedStDevs[2, ], NA_real_)
	expect_equal(results13$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[3, ], c(0.048616927, 0.34001465, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196347), tolerance = 1e-05)
	expect_equal(results13$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results13$conditionalPower[3, ], c(NA_real_, NA_real_, 0.827255, 0.94656521), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.395028, -4.0669225, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.071338, 0.24154039, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(44.395028, 27.895908, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[3, ], c(47.071339, 32.285152, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[3, ], c(0.5, 0.017155659, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results13), NA)))
	    expect_output(print(results13)$show())
	    invisible(capture.output(expect_error(summary(results13), NA)))
	    expect_output(summary(results13)$show())
	}

	results14 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results14' with expected results
	expect_equal(results14$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results14$thetaH1[2, ], NA_real_)
	expect_equal(results14$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results14$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results14$assumedStDevs[2, ], NA_real_)
	expect_equal(results14$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[3, ], c(0.042866371, 0.28890175, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767621, 0.6794713), tolerance = 1e-05)
	expect_equal(results14$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results14$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78417464, 0.93070164), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.433727, -4.7641753, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.426744, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[3, ], c(-15.938808, -0.40329842, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(48.433726, 28.584393, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[3, ], c(50.938808, 32.927366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[3, ], c(0.5, 0.025112148, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results14), NA)))
	    expect_output(print(results14)$show())
	    invisible(capture.output(expect_error(summary(results14), NA)))
	    expect_output(summary(results14)$show())
	}

	results15 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results15' with expected results
	expect_equal(results15$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results15$thetaH1[2, ], NA_real_)
	expect_equal(results15$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results15$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results15$assumedStDevs[2, ], NA_real_)
	expect_equal(results15$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198144, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[3, ], c(0.049947129, 0.35588619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-05)
	expect_equal(results15$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results15$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83889182, 0.95069292), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.802158, -4.2854677, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.232722, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.786808, 0.41764291, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(44.802158, 28.113846, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[3, ], c(46.786808, 32.10754, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[3, ], c(0.5, 0.015272156, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results15), NA)))
	    expect_output(print(results15)$show())
	    invisible(capture.output(expect_error(summary(results15), NA)))
	    expect_output(summary(results15)$show())
	}

	results16 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results16' with expected results
	expect_equal(results16$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results16$thetaH1[2, ], NA_real_)
	expect_equal(results16$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results16$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results16$assumedStDevs[2, ], NA_real_)
	expect_equal(results16$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.04074021, 0.14372404, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[3, ], c(0.046882975, 0.32321323, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-05)
	expect_equal(results16$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results16$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81409137, 0.94181531), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.56757, -4.6627973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.940705, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.521691, 0.049006954, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(46.567569, 28.528695, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[3, ], c(48.521691, 32.491815, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[3, ], c(0.5, 0.019420631, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results16), NA)))
	    expect_output(print(results16)$show())
	    invisible(capture.output(expect_error(summary(results16), NA)))
	    expect_output(summary(results16)$show())
	}

	results17 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results17' with expected results
	expect_equal(results17$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results17$thetaH1[2, ], NA_real_)
	expect_equal(results17$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results17$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results17$assumedStDevs[2, ], NA_real_)
	expect_equal(results17$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.043219832, 0.15803857, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[3, ], c(0.046782117, 0.33290332, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-05)
	expect_equal(results17$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results17$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8217936, 0.94460493), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.549821, -4.1214, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[3, ], c(-12.483404, 0.16013937, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(44.549821, 27.945069, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[3, ], c(47.483404, 32.356999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[3, ], c(0.5, 0.018077861, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results17), NA)))
	    expect_output(print(results17)$show())
	    invisible(capture.output(expect_error(summary(results17), NA)))
	    expect_output(summary(results17)$show())
	}

	results18 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results18' with expected results
	expect_equal(results18$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results18$thetaH1[2, ], NA_real_)
	expect_equal(results18$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results18$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results18$assumedStDevs[2, ], NA_real_)
	expect_equal(results18$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[2, ], c(0.037322006, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[3, ], c(0.041377736, 0.28315003, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684079), tolerance = 1e-05)
	expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results18$conditionalPower[3, ], c(NA_real_, NA_real_, 0.77871789, 0.92862656), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(-18.601467, -4.8144339, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.153638, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[3, ], c(-16.403927, -0.48327176, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(48.601467, 28.627869, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[3, ], c(51.403927, 32.999307, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[3, ], c(0.5, 0.026234621, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results18), NA)))
	    expect_output(print(results18)$show())
	    invisible(capture.output(expect_error(summary(results18), NA)))
	    expect_output(summary(results18)$show())
	}

	results19 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results19' with expected results
	expect_equal(results19$thetaH1[1, ], 11.562259, tolerance = 3e-04)
	expect_equal(results19$thetaH1[2, ], NA_real_)
	expect_equal(results19$thetaH1[3, ], 16.036585, tolerance = 3e-04)
	expect_equal(results19$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results19$assumedStDevs[2, ], NA_real_)
	expect_equal(results19$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results19$conditionalRejectionProbabilities[1, ], c(0.046821831, 0.16471605, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$conditionalRejectionProbabilities[3, ], c(0.056787494, 0.38875376, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46655429, 0.70780431), tolerance = 3e-04)
	expect_equal(results19$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results19$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8607725, 0.95827239), tolerance = 3e-04)
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[1, ], c(-14.645299, -3.9276912, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[2, ], c(-19.080965, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedConfidenceIntervalLowerBounds[3, ], c(-11.632661, 0.82949344, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[1, ], c(44.645299, 27.415432, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[2, ], c(38.280965, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedConfidenceIntervalUpperBounds[3, ], c(46.632661, 31.563143, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedPValues[1, ], c(0.5, 0.069897558, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results19$repeatedPValues[3, ], c(0.5, 0.012021087, NA_real_, NA_real_), tolerance = 3e-04)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results19), NA)))
	    expect_output(print(results19)$show())
	    invisible(capture.output(expect_error(summary(results19), NA)))
	    expect_output(summary(results19)$show())
	}

	results20 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results20' with expected results
	expect_equal(results20$thetaH1[1, ], 11.562259, tolerance = 3e-04)
	expect_equal(results20$thetaH1[2, ], NA_real_)
	expect_equal(results20$thetaH1[3, ], 16.036585, tolerance = 3e-04)
	expect_equal(results20$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results20$assumedStDevs[2, ], NA_real_)
	expect_equal(results20$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results20$conditionalRejectionProbabilities[1, ], c(0.045317936, 0.15683262, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$conditionalRejectionProbabilities[3, ], c(0.054085341, 0.35883746, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45213049, 0.6974477), tolerance = 3e-04)
	expect_equal(results20$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results20$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84097508, 0.95142481), tolerance = 3e-04)
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[1, ], c(-16.332082, -4.255678, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[2, ], c(-20.712879, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedConfidenceIntervalLowerBounds[3, ], c(-13.290276, 0.50948619, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[1, ], c(46.332082, 27.786339, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[2, ], c(39.912879, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedConfidenceIntervalUpperBounds[3, ], c(48.290276, 31.900739, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedPValues[1, ], c(0.5, 0.075257197, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results20$repeatedPValues[3, ], c(0.5, 0.014946, NA_real_, NA_real_), tolerance = 3e-04)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results20), NA)))
	    expect_output(print(results20)$show())
	    invisible(capture.output(expect_error(summary(results20), NA)))
	    expect_output(summary(results20)$show())
	}

	results21 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results21' with expected results
	expect_equal(results21$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results21$thetaH1[2, ], NA_real_)
	expect_equal(results21$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results21$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results21$assumedStDevs[2, ], NA_real_)
	expect_equal(results21$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results21$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$conditionalRejectionProbabilities[3, ], c(0.029595078, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.256183, -4.0576303, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.9076686, 0.4326836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[1, ], c(40.256183, 26.806923, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[2, ], c(32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedConfidenceIntervalUpperBounds[3, ], c(42.907669, 31.664999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$repeatedPValues[3, ], c(0.13700176, 0.014275569, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results21$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2797, 0.439), tolerance = 1e-05)
	expect_equal(results21$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results21$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results21), NA)))
	    expect_output(print(results21)$show())
	    invisible(capture.output(expect_error(summary(results21), NA)))
	    expect_output(summary(results21)$show())
	}

	results22 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results22' with expected results
	expect_equal(results22$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results22$thetaH1[2, ], NA_real_)
	expect_equal(results22$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results22$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results22$assumedStDevs[2, ], NA_real_)
	expect_equal(results22$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results22$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$conditionalRejectionProbabilities[3, ], c(0.027312859, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.830851, -4.722817, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.376075, -0.16648466, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[1, ], c(42.830851, 27.447651, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[2, ], c(34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedConfidenceIntervalUpperBounds[3, ], c(45.376075, 32.257244, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$repeatedPValues[3, ], c(0.15234731, 0.019097336, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results22$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2591, 0.4187), tolerance = 1e-05)
	expect_equal(results22$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results22$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results22), NA)))
	    expect_output(print(results22)$show())
	    invisible(capture.output(expect_error(summary(results22), NA)))
	    expect_output(summary(results22)$show())
	}

	results23 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results23' with expected results
	expect_equal(results23$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results23$thetaH1[2, ], NA_real_)
	expect_equal(results23$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results23$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results23$assumedStDevs[2, ], NA_real_)
	expect_equal(results23$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results23$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$conditionalRejectionProbabilities[3, ], c(0.028741907, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.605988, -4.2731734, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.6631999, 0.60791563, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[1, ], c(40.605988, 27.021858, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[2, ], c(34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedConfidenceIntervalUpperBounds[3, ], c(42.6632, 31.486561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$repeatedPValues[3, ], c(0.14242148, 0.013628025, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results23$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2699, 0.4282), tolerance = 1e-05)
	expect_equal(results23$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results23$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results23), NA)))
	    expect_output(print(results23)$show())
	    invisible(capture.output(expect_error(summary(results23), NA)))
	    expect_output(summary(results23)$show())
	}

	results24 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results24' with expected results
	expect_equal(results24$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results24$thetaH1[2, ], NA_real_)
	expect_equal(results24$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results24$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results24$assumedStDevs[2, ], NA_real_)
	expect_equal(results24$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results24$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$conditionalRejectionProbabilities[3, ], c(0.027708171, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.74771, -4.6401767, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.7851784, 0.25415362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[1, ], c(41.74771, 27.425347, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[2, ], c(35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedConfidenceIntervalUpperBounds[3, ], c(43.785178, 31.856528, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$repeatedPValues[3, ], c(0.1494882, 0.016474737, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results24$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2576, 0.4176), tolerance = 1e-05)
	expect_equal(results24$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results24$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results24), NA)))
	    expect_output(print(results24)$show())
	    invisible(capture.output(expect_error(summary(results24), NA)))
	    expect_output(summary(results24)$show())
	}

	results25 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results25' with expected results
	expect_equal(results25$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results25$thetaH1[2, ], NA_real_)
	expect_equal(results25$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results25$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results25$assumedStDevs[2, ], NA_real_)
	expect_equal(results25$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results25$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$conditionalRejectionProbabilities[3, ], c(0.029264016, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.389181, -4.1091853, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2617152, 0.37246523, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[1, ], c(40.389181, 26.847363, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[2, ], c(32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedConfidenceIntervalUpperBounds[3, ], c(43.261715, 31.705217, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$repeatedPValues[3, ], c(0.13906265, 0.014376658, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results25$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2768, 0.436), tolerance = 1e-05)
	expect_equal(results25$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results25$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results25), NA)))
	    expect_output(print(results25)$show())
	    invisible(capture.output(expect_error(summary(results25), NA)))
	    expect_output(summary(results25)$show())
	}

	results26 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results26' with expected results
	expect_equal(results26$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results26$thetaH1[2, ], NA_real_)
	expect_equal(results26$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results26$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results26$assumedStDevs[2, ], NA_real_)
	expect_equal(results26$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results26$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$conditionalRejectionProbabilities[3, ], c(0.027044989, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.972232, -4.7692163, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.763995, -0.22335705, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[1, ], c(42.972232, 27.481288, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[2, ], c(34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedConfidenceIntervalUpperBounds[3, ], c(45.763994, 32.295837, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$repeatedPValues[3, ], c(0.15433667, 0.019180306, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results26$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2568, 0.4157), tolerance = 1e-05)
	expect_equal(results26$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results26$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results26), NA)))
	    expect_output(print(results26)$show())
	    invisible(capture.output(expect_error(summary(results26), NA)))
	    expect_output(summary(results26)$show())
	}

	results27 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results27' with expected results
	expect_equal(results27$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results27$thetaH1[2, ], NA_real_)
	expect_equal(results27$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results27$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results27$assumedStDevs[2, ], NA_real_)
	expect_equal(results27$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results27$conditionalRejectionProbabilities[1, ], c(0.025021019, 0.054834069, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$conditionalRejectionProbabilities[3, ], c(0.027777772, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.247199, -4.0258193, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.153418, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.8986307, 0.47811558, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[1, ], c(40.247199, 26.680539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[2, ], c(32.353418, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedConfidenceIntervalUpperBounds[3, ], c(42.898631, 31.584065, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedPValues[1, ], c(0.17089623, 0.061105652, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$repeatedPValues[3, ], c(0.14899419, 0.015246407, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results27$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2833, 0.4422), tolerance = 1e-05)
	expect_equal(results27$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results27$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results27), NA)))
	    expect_output(print(results27)$show())
	    invisible(capture.output(expect_error(summary(results27), NA)))
	    expect_output(summary(results27)$show())
	}

	results28 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results28' with expected results
	expect_equal(results28$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results28$thetaH1[2, ], NA_real_)
	expect_equal(results28$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results28$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results28$assumedStDevs[2, ], NA_real_)
	expect_equal(results28$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results28$conditionalRejectionProbabilities[1, ], c(0.023144095, 0.048545015, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$conditionalRejectionProbabilities[3, ], c(0.0247006, 0.1449328, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.8192, -4.6852584, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.40635, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.364486, -0.1144866, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[1, ], c(42.8192, 27.314543, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[2, ], c(34.60635, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedConfidenceIntervalUpperBounds[3, ], c(45.364486, 32.169333, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedPValues[1, ], c(0.18910184, 0.069324401, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$repeatedPValues[3, ], c(0.17379158, 0.021189694, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results28$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2618, 0.4212), tolerance = 1e-05)
	expect_equal(results28$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results28$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6607, 0.7765), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results28), NA)))
	    expect_output(print(results28)$show())
	    invisible(capture.output(expect_error(summary(results28), NA)))
	    expect_output(summary(results28)$show())
	}

	results29 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results29' with expected results
	expect_equal(results29$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results29$thetaH1[2, ], NA_real_)
	expect_equal(results29$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results29$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results29$assumedStDevs[2, ], NA_real_)
	expect_equal(results29$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results29$conditionalRejectionProbabilities[1, ], c(0.024319059, 0.051462476, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$conditionalRejectionProbabilities[3, ], c(0.028516214, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.59688, -4.2407133, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.164237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.6542489, 0.6529301, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[1, ], c(40.59688, 26.894985, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[2, ], c(34.364237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedConfidenceIntervalUpperBounds[3, ], c(42.654249, 31.405859, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedPValues[1, ], c(0.17734783, 0.06527034, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$repeatedPValues[3, ], c(0.14391589, 0.013711948, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results29$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2722, 0.4311), tolerance = 1e-05)
	expect_equal(results29$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results29$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results29), NA)))
	    expect_output(print(results29)$show())
	    invisible(capture.output(expect_error(summary(results29), NA)))
	    expect_output(summary(results29)$show())
	}

	results30 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results30' with expected results
	expect_equal(results30$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results30$thetaH1[2, ], NA_real_)
	expect_equal(results30$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results30$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results30$assumedStDevs[2, ], NA_real_)
	expect_equal(results30$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results30$conditionalRejectionProbabilities[1, ], c(0.023469013, 0.048270226, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$conditionalRejectionProbabilities[3, ], c(0.026830382, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.737451, -4.6050352, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.267707, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.7750975, 0.30217392, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[1, ], c(41.737451, 27.295819, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[2, ], c(35.467707, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedConfidenceIntervalUpperBounds[3, ], c(43.775098, 31.772829, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedPValues[1, ], c(0.18572393, 0.069730666, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$repeatedPValues[3, ], c(0.15596268, 0.017006886, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results30$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.261, 0.4206), tolerance = 1e-05)
	expect_equal(results30$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results30$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results30), NA)))
	    expect_output(print(results30)$show())
	    invisible(capture.output(expect_error(summary(results30), NA)))
	    expect_output(summary(results30)$show())
	}

	results31 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results31' with expected results
	expect_equal(results31$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results31$thetaH1[2, ], NA_real_)
	expect_equal(results31$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results31$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results31$assumedStDevs[2, ], NA_real_)
	expect_equal(results31$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results31$conditionalRejectionProbabilities[1, ], c(0.024748593, 0.053966892, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$conditionalRejectionProbabilities[3, ], c(0.0267758, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.38015, -4.0770639, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.116502, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2525514, 0.41959343, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedConfidenceIntervalUpperBounds[1, ], c(40.38015, 26.720108, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedConfidenceIntervalUpperBounds[2, ], c(32.316502, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedConfidenceIntervalUpperBounds[3, ], c(43.252551, 31.62149, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedPValues[1, ], c(0.17335289, 0.062127989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$repeatedPValues[3, ], c(0.15638134, 0.015781417, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results31$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2799, 0.439), tolerance = 1e-05)
	expect_equal(results31$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results31$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results31), NA)))
	    expect_output(print(results31)$show())
	    invisible(capture.output(expect_error(summary(results31), NA)))
	    expect_output(summary(results31)$show())
	}

	results32 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results32' with expected results
	expect_equal(results32$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results32$thetaH1[2, ], NA_real_)
	expect_equal(results32$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results32$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results32$assumedStDevs[2, ], NA_real_)
	expect_equal(results32$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results32$conditionalRejectionProbabilities[1, ], c(0.022923976, 0.04788638, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$conditionalRejectionProbabilities[3, ], c(0.023933809, 0.14146912, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.960526, -4.7313117, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.225975, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.752245, -0.16953037, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedConfidenceIntervalUpperBounds[1, ], c(42.960526, 27.347242, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedConfidenceIntervalUpperBounds[2, ], c(34.425975, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedConfidenceIntervalUpperBounds[3, ], c(45.752245, 32.205007, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedPValues[1, ], c(0.19144883, 0.07030573, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$repeatedPValues[3, ], c(0.18106429, 0.021778109, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results32$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2594, 0.4195), tolerance = 1e-05)
	expect_equal(results32$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results32$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6537, 0.7739), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results32), NA)))
	    expect_output(print(results32)$show())
	    invisible(capture.output(expect_error(summary(results32), NA)))
	    expect_output(summary(results32)$show())
	}

	results33 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results33' with expected results
	expect_equal(results33$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results33$thetaH1[2, ], NA_real_)
	expect_equal(results33$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results33$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results33$assumedStDevs[2, ], NA_real_)
	expect_equal(results33$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results33$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$conditionalRejectionProbabilities[3, ], c(0.027261939, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.256183, -4.0576303, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.9076686, 0.4326836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedConfidenceIntervalUpperBounds[1, ], c(40.256183, 26.806923, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedConfidenceIntervalUpperBounds[2, ], c(32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedConfidenceIntervalUpperBounds[3, ], c(42.907669, 31.664999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$repeatedPValues[3, ], c(0.1527221, 0.015597359, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results33$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2797, 0.439), tolerance = 1e-05)
	expect_equal(results33$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results33$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results33), NA)))
	    expect_output(print(results33)$show())
	    invisible(capture.output(expect_error(summary(results33), NA)))
	    expect_output(summary(results33)$show())
	}

	results34 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results34' with expected results
	expect_equal(results34$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results34$thetaH1[2, ], NA_real_)
	expect_equal(results34$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results34$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results34$assumedStDevs[2, ], NA_real_)
	expect_equal(results34$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results34$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$conditionalRejectionProbabilities[3, ], c(0.024147032, 0.14148061, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.830851, -4.722817, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.376075, -0.16648466, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedConfidenceIntervalUpperBounds[1, ], c(42.830851, 27.447651, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedConfidenceIntervalUpperBounds[2, ], c(34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedConfidenceIntervalUpperBounds[3, ], c(45.376075, 32.257244, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$repeatedPValues[3, ], c(0.17899101, 0.021776202, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results34$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2591, 0.4187), tolerance = 1e-05)
	expect_equal(results34$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results34$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6537, 0.774), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results34), NA)))
	    expect_output(print(results34)$show())
	    invisible(capture.output(expect_error(summary(results34), NA)))
	    expect_output(summary(results34)$show())
	}

	results35 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results35' with expected results
	expect_equal(results35$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results35$thetaH1[2, ], NA_real_)
	expect_equal(results35$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results35$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results35$assumedStDevs[2, ], NA_real_)
	expect_equal(results35$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results35$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$conditionalRejectionProbabilities[3, ], c(0.028008383, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.605988, -4.2731734, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.6631999, 0.60791563, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedConfidenceIntervalUpperBounds[1, ], c(40.605988, 27.021858, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedConfidenceIntervalUpperBounds[2, ], c(34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedConfidenceIntervalUpperBounds[3, ], c(42.6632, 31.486561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$repeatedPValues[3, ], c(0.14737581, 0.014014262, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results35$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2699, 0.4282), tolerance = 1e-05)
	expect_equal(results35$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results35$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results35), NA)))
	    expect_output(print(results35)$show())
	    invisible(capture.output(expect_error(summary(results35), NA)))
	    expect_output(summary(results35)$show())
	}

	results36 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results36' with expected results
	expect_equal(results36$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results36$thetaH1[2, ], NA_real_)
	expect_equal(results36$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results36$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results36$assumedStDevs[2, ], NA_real_)
	expect_equal(results36$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results36$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$conditionalRejectionProbabilities[3, ], c(0.026303733, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.74771, -4.6401767, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedConfidenceIntervalLowerBounds[2, ], c(-16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.7851784, 0.25415362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedConfidenceIntervalUpperBounds[1, ], c(41.74771, 27.425347, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedConfidenceIntervalUpperBounds[2, ], c(35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedConfidenceIntervalUpperBounds[3, ], c(43.785178, 31.856528, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$repeatedPValues[3, ], c(0.16007682, 0.01742078, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results36$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2576, 0.4176), tolerance = 1e-05)
	expect_equal(results36$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results36$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results36), NA)))
	    expect_output(print(results36)$show())
	    invisible(capture.output(expect_error(summary(results36), NA)))
	    expect_output(summary(results36)$show())
	}

	results37 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results37' with expected results
	expect_equal(results37$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results37$thetaH1[2, ], NA_real_)
	expect_equal(results37$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results37$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results37$assumedStDevs[2, ], NA_real_)
	expect_equal(results37$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results37$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$conditionalRejectionProbabilities[3, ], c(0.026248507, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.389181, -4.1091853, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedConfidenceIntervalLowerBounds[2, ], c(-13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.2617152, 0.37246523, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedConfidenceIntervalUpperBounds[1, ], c(40.389181, 26.847363, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedConfidenceIntervalUpperBounds[2, ], c(32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedConfidenceIntervalUpperBounds[3, ], c(43.261715, 31.705217, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$repeatedPValues[3, ], c(0.16051933, 0.01616384, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results37$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2768, 0.436), tolerance = 1e-05)
	expect_equal(results37$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results37$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results37), NA)))
	    expect_output(print(results37)$show())
	    invisible(capture.output(expect_error(summary(results37), NA)))
	    expect_output(summary(results37)$show())
	}

	results38 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results38' with expected results
	expect_equal(results38$thetaH1[1, ], 11.562259, tolerance = 1e-05)
	expect_equal(results38$thetaH1[2, ], NA_real_)
	expect_equal(results38$thetaH1[3, ], 16.036585, tolerance = 1e-05)
	expect_equal(results38$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results38$assumedStDevs[2, ], NA_real_)
	expect_equal(results38$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results38$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$conditionalRejectionProbabilities[3, ], c(0.023369532, 0.13794488, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedConfidenceIntervalLowerBounds[1, ], c(-12.972232, -4.7692163, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedConfidenceIntervalLowerBounds[3, ], c(-10.763995, -0.22335705, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedConfidenceIntervalUpperBounds[1, ], c(42.972232, 27.481288, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedConfidenceIntervalUpperBounds[2, ], c(34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedConfidenceIntervalUpperBounds[3, ], c(45.763994, 32.295837, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$repeatedPValues[3, ], c(0.18674722, 0.022408487, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results38$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2568, 0.4157), tolerance = 1e-05)
	expect_equal(results38$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results38$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6466, 0.7714), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results38), NA)))
	    expect_output(print(results38)$show())
	    invisible(capture.output(expect_error(summary(results38), NA)))
	    expect_output(summary(results38)$show())
	}

	results39 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results39' with expected results
	expect_equal(results39$thetaH1[1, ], 11.562259, tolerance = 3e-04)
	expect_equal(results39$thetaH1[2, ], NA_real_)
	expect_equal(results39$thetaH1[3, ], 16.036585, tolerance = 3e-04)
	expect_equal(results39$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results39$assumedStDevs[2, ], NA_real_)
	expect_equal(results39$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results39$conditionalRejectionProbabilities[1, ], c(0.026270246, 0.055429547, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$conditionalRejectionProbabilities[3, ], c(0.032007375, 1, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedConfidenceIntervalLowerBounds[1, ], c(-10.308255, -3.9367007, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedConfidenceIntervalLowerBounds[2, ], c(-14.885001, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedConfidenceIntervalLowerBounds[3, ], c(-7.3706149, 0.96851182, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedConfidenceIntervalUpperBounds[1, ], c(40.308255, 26.527826, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedConfidenceIntervalUpperBounds[2, ], c(34.085001, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedConfidenceIntervalUpperBounds[3, ], c(42.370615, 31.063058, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedPValues[1, ], c(0.1603448, 0.060420915, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$repeatedPValues[3, ], c(0.12340907, 0.011635803, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results39$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2846, 0.4438), tolerance = 3e-04)
	expect_equal(results39$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results39$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results39), NA)))
	    expect_output(print(results39)$show())
	    invisible(capture.output(expect_error(summary(results39), NA)))
	    expect_output(summary(results39)$show())
	}

	results40 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results40' with expected results
	expect_equal(results40$thetaH1[1, ], 11.562259, tolerance = 3e-04)
	expect_equal(results40$thetaH1[2, ], NA_real_)
	expect_equal(results40$thetaH1[3, ], 16.036585, tolerance = 3e-04)
	expect_equal(results40$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results40$assumedStDevs[2, ], NA_real_)
	expect_equal(results40$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results40$conditionalRejectionProbabilities[1, ], c(0.025453046, 0.052196173, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$conditionalRejectionProbabilities[3, ], c(0.030395001, 1, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedConfidenceIntervalLowerBounds[1, ], c(-11.358332, -4.2589864, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedConfidenceIntervalLowerBounds[2, ], c(-15.90092, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedConfidenceIntervalLowerBounds[3, ], c(-8.402534, 0.65713259, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedConfidenceIntervalUpperBounds[1, ], c(41.358332, 26.891213, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedConfidenceIntervalUpperBounds[2, ], c(35.10092, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedConfidenceIntervalUpperBounds[3, ], c(43.402534, 31.392765, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedPValues[1, ], c(0.16711874, 0.064319528, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$repeatedPValues[3, ], c(0.13222673, 0.014209765, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results40$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2736, 0.4337), tolerance = 3e-04)
	expect_equal(results40$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results40$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results40), NA)))
	    expect_output(print(results40)$show())
	    invisible(capture.output(expect_error(summary(results40), NA)))
	    expect_output(summary(results40)$show())
	}

	results41 <- getAnalysisResults(design = design3, dataInput = dataExample1,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results41' with expected results
	expect_equal(results41$thetaH1[1, ], 11.562259, tolerance = 3e-04)
	expect_equal(results41$thetaH1[2, ], NA_real_)
	expect_equal(results41$thetaH1[3, ], 16.036585, tolerance = 3e-04)
	expect_equal(results41$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results41$assumedStDevs[2, ], NA_real_)
	expect_equal(results41$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results41$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.061352393), tolerance = 3e-04)
	expect_equal(results41$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.037447419), tolerance = 3e-04)
	expect_equal(results41$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.08651207), tolerance = 3e-04)
	expect_equal(results41$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results41$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results41$conditionalPower[3, ], c(NA_real_, NA_real_))
	expect_equal(results41$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.72440621), tolerance = 3e-04)
	expect_equal(results41$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results41$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, 3.9389233), tolerance = 3e-04)
	expect_equal(results41$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 22.538721), tolerance = 3e-04)
	expect_equal(results41$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results41$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, 26.753524), tolerance = 3e-04)
	expect_equal(results41$repeatedPValues[1, ], c(NA_real_, 0.017445576), tolerance = 3e-04)
	expect_equal(results41$repeatedPValues[2, ], c(NA_real_, NA_real_))
	expect_equal(results41$repeatedPValues[3, ], c(NA_real_, 0.0019493527), tolerance = 3e-04)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results41), NA)))
	    expect_output(print(results41)$show())
	    invisible(capture.output(expect_error(summary(results41), NA)))
	    expect_output(summary(results41)$show())
	}

	results42 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results42' with expected results
	expect_equal(results42$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results42$thetaH1[2, ], NA_real_)
	expect_equal(results42$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results42$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results42$assumedStDevs[2, ], NA_real_)
	expect_equal(results42$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results42$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$conditionalRejectionProbabilities[3, ], c(0.052717287, 0.35672949, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196347), tolerance = 1e-05)
	expect_equal(results42$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results42$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83948961, 0.95090316), tolerance = 1e-05)
	expect_equal(results42$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.395028, -27.895908, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.071339, -32.285152, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedConfidenceIntervalUpperBounds[1, ], c(14.395028, 4.0669225, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedConfidenceIntervalUpperBounds[2, ], c(16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedConfidenceIntervalUpperBounds[3, ], c(12.071338, -0.24154039, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results42$repeatedPValues[3, ], c(0.5, 0.015177743, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results42), NA)))
	    expect_output(print(results42)$show())
	    invisible(capture.output(expect_error(summary(results42), NA)))
	    expect_output(summary(results42)$show())
	}

	results43 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results43' with expected results
	expect_equal(results43$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results43$thetaH1[2, ], NA_real_)
	expect_equal(results43$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results43$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results43$assumedStDevs[2, ], NA_real_)
	expect_equal(results43$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results43$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$conditionalRejectionProbabilities[3, ], c(0.048708233, 0.3133215, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767621, 0.6794713), tolerance = 1e-05)
	expect_equal(results43$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results43$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80590445, 0.93881804), tolerance = 1e-05)
	expect_equal(results43$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.433726, -28.584393, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedConfidenceIntervalLowerBounds[3, ], c(-50.938808, -32.927366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedConfidenceIntervalUpperBounds[1, ], c(18.433727, 4.7641753, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedConfidenceIntervalUpperBounds[2, ], c(20.426744, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedConfidenceIntervalUpperBounds[3, ], c(15.938808, 0.40329842, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results43$repeatedPValues[3, ], c(0.5, 0.020901685, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results43), NA)))
	    expect_output(print(results43)$show())
	    invisible(capture.output(expect_error(summary(results43), NA)))
	    expect_output(summary(results43)$show())
	}

	results44 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results44' with expected results
	expect_equal(results44$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results44$thetaH1[2, ], NA_real_)
	expect_equal(results44$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results44$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results44$assumedStDevs[2, ], NA_real_)
	expect_equal(results44$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results44$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198144, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$conditionalRejectionProbabilities[3, ], c(0.051237296, 0.36121246, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-05)
	expect_equal(results44$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results44$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84263383, 0.95200602), tolerance = 1e-05)
	expect_equal(results44$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.802158, -28.113846, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.786808, -32.10754, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedConfidenceIntervalUpperBounds[1, ], c(14.802158, 4.2854677, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedConfidenceIntervalUpperBounds[2, ], c(19.232722, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedConfidenceIntervalUpperBounds[3, ], c(11.786808, -0.41764291, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results44$repeatedPValues[3, ], c(0.5, 0.014689462, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results44), NA)))
	    expect_output(print(results44)$show())
	    invisible(capture.output(expect_error(summary(results44), NA)))
	    expect_output(summary(results44)$show())
	}

	results45 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results45' with expected results
	expect_equal(results45$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results45$thetaH1[2, ], NA_real_)
	expect_equal(results45$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results45$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results45$assumedStDevs[2, ], NA_real_)
	expect_equal(results45$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results45$conditionalRejectionProbabilities[1, ], c(0.04074021, 0.14372404, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$conditionalRejectionProbabilities[3, ], c(0.049414261, 0.33374326, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-05)
	expect_equal(results45$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results45$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82244694, 0.94484021), tolerance = 1e-05)
	expect_equal(results45$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.567569, -28.528695, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedConfidenceIntervalLowerBounds[2, ], c(-40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.521691, -32.491815, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedConfidenceIntervalUpperBounds[1, ], c(16.56757, 4.6627973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedConfidenceIntervalUpperBounds[2, ], c(20.940705, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedConfidenceIntervalUpperBounds[3, ], c(13.521691, -0.049006954, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results45$repeatedPValues[3, ], c(0.5, 0.017966281, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results45), NA)))
	    expect_output(print(results45)$show())
	    invisible(capture.output(expect_error(summary(results45), NA)))
	    expect_output(summary(results45)$show())
	}

	results46 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results46' with expected results
	expect_equal(results46$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results46$thetaH1[2, ], NA_real_)
	expect_equal(results46$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results46$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results46$assumedStDevs[2, ], NA_real_)
	expect_equal(results46$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results46$conditionalRejectionProbabilities[1, ], c(0.043219832, 0.15803857, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$conditionalRejectionProbabilities[3, ], c(0.052145589, 0.35513472, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-05)
	expect_equal(results46$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results46$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83835744, 0.95050484), tolerance = 1e-05)
	expect_equal(results46$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.549821, -27.945069, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.483404, -32.356999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedConfidenceIntervalUpperBounds[1, ], c(14.549821, 4.1214, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedConfidenceIntervalUpperBounds[2, ], c(16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedConfidenceIntervalUpperBounds[3, ], c(12.483404, -0.16013937, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results46$repeatedPValues[3, ], c(0.5, 0.015356079, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results46), NA)))
	    expect_output(print(results46)$show())
	    invisible(capture.output(expect_error(summary(results46), NA)))
	    expect_output(summary(results46)$show())
	}

	results47 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results47' with expected results
	expect_equal(results47$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results47$thetaH1[2, ], NA_real_)
	expect_equal(results47$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results47$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results47$assumedStDevs[2, ], NA_real_)
	expect_equal(results47$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results47$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$conditionalRejectionProbabilities[2, ], c(0.037322006, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$conditionalRejectionProbabilities[3, ], c(0.048226967, 0.31219358, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684079), tolerance = 1e-05)
	expect_equal(results47$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results47$conditionalPower[3, ], c(NA_real_, NA_real_, 0.80494934, 0.93846621), tolerance = 1e-05)
	expect_equal(results47$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.601467, -28.627869, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedConfidenceIntervalLowerBounds[3, ], c(-51.403927, -32.999307, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedConfidenceIntervalUpperBounds[1, ], c(18.601467, 4.8144339, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedConfidenceIntervalUpperBounds[2, ], c(20.153638, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedConfidenceIntervalUpperBounds[3, ], c(16.403927, 0.48327176, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results47$repeatedPValues[3, ], c(0.5, 0.021078114, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results47), NA)))
	    expect_output(print(results47)$show())
	    invisible(capture.output(expect_error(summary(results47), NA)))
	    expect_output(summary(results47)$show())
	}

	results48 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results48' with expected results
	expect_equal(results48$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results48$thetaH1[2, ], NA_real_)
	expect_equal(results48$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results48$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results48$assumedStDevs[2, ], NA_real_)
	expect_equal(results48$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results48$conditionalRejectionProbabilities[1, ], c(0.044513617, 0.16250147, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$conditionalRejectionProbabilities[3, ], c(0.049538053, 0.34419132, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46254707, 0.70494474), tolerance = 1e-05)
	expect_equal(results48$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results48$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83038985, 0.94768376), tolerance = 1e-05)
	expect_equal(results48$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.393216, -27.725836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.089915, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.069516, -32.182569, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedConfidenceIntervalUpperBounds[1, ], c(14.393216, 4.0328452, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedConfidenceIntervalUpperBounds[2, ], c(16.889915, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedConfidenceIntervalUpperBounds[3, ], c(12.069516, -0.29402598, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedPValues[1, ], c(0.5, 0.071351909, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results48$repeatedPValues[3, ], c(0.5, 0.016637815, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results48), NA)))
	    expect_output(print(results48)$show())
	    invisible(capture.output(expect_error(summary(results48), NA)))
	    expect_output(summary(results48)$show())
	}

	results49 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results49' with expected results
	expect_equal(results49$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results49$thetaH1[2, ], NA_real_)
	expect_equal(results49$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results49$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results49$assumedStDevs[2, ], NA_real_)
	expect_equal(results49$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results49$conditionalRejectionProbabilities[1, ], c(0.040941914, 0.14648989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$conditionalRejectionProbabilities[3, ], c(0.043912864, 0.29382832, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$conditionalPower[1, ], c(NA_real_, NA_real_, 0.4325103, 0.68306799), tolerance = 1e-05)
	expect_equal(results49$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results49$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78874215, 0.93242714), tolerance = 1e-05)
	expect_equal(results49$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.431163, -28.407231, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.624453, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedConfidenceIntervalLowerBounds[3, ], c(-50.936268, -32.815818, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedConfidenceIntervalUpperBounds[1, ], c(18.431163, 4.7231897, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedConfidenceIntervalUpperBounds[2, ], c(20.424453, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedConfidenceIntervalUpperBounds[3, ], c(15.936268, 0.34247221, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedPValues[1, ], c(0.5, 0.083136439, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results49$repeatedPValues[3, ], c(0.5, 0.024192808, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results49), NA)))
	    expect_output(print(results49)$show())
	    invisible(capture.output(expect_error(summary(results49), NA)))
	    expect_output(summary(results49)$show())
	}

	results50 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results50' with expected results
	expect_equal(results50$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results50$thetaH1[2, ], NA_real_)
	expect_equal(results50$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results50$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results50$assumedStDevs[2, ], NA_real_)
	expect_equal(results50$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results50$conditionalRejectionProbabilities[1, ], c(0.043192759, 0.15430882, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$conditionalRejectionProbabilities[3, ], c(0.050842102, 0.35990794, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44741758, 0.69402491), tolerance = 1e-05)
	expect_equal(results50$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results50$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84172467, 0.95168763), tolerance = 1e-05)
	expect_equal(results50$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.800321, -27.943326, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.430944, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.785003, -32.005071, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedConfidenceIntervalUpperBounds[1, ], c(14.800321, 4.2506388, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedConfidenceIntervalUpperBounds[2, ], c(19.230944, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedConfidenceIntervalUpperBounds[3, ], c(11.785003, -0.46968004, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedPValues[1, ], c(0.5, 0.077086341, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results50$repeatedPValues[3, ], c(0.5, 0.014829652, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results50), NA)))
	    expect_output(print(results50)$show())
	    invisible(capture.output(expect_error(summary(results50), NA)))
	    expect_output(summary(results50)$show())
	}

	results51 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results51' with expected results
	expect_equal(results51$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results51$thetaH1[2, ], NA_real_)
	expect_equal(results51$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results51$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results51$assumedStDevs[2, ], NA_real_)
	expect_equal(results51$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results51$conditionalRejectionProbabilities[1, ], c(0.041569453, 0.14613212, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$conditionalRejectionProbabilities[3, ], c(0.047839714, 0.32760313, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$conditionalPower[1, ], c(NA_real_, NA_real_, 0.43181681, 0.68255335), tolerance = 1e-05)
	expect_equal(results51$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results51$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81761872, 0.94309649), tolerance = 1e-05)
	expect_equal(results51$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.565416, -28.357046, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedConfidenceIntervalLowerBounds[2, ], c(-40.138622, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.519575, -32.386196, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedConfidenceIntervalUpperBounds[1, ], c(16.565416, 4.6248784, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedConfidenceIntervalUpperBounds[2, ], c(20.938622, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedConfidenceIntervalUpperBounds[3, ], c(13.519575, -0.10461547, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedPValues[1, ], c(0.5, 0.083428262, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results51$repeatedPValues[3, ], c(0.5, 0.018799791, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results51), NA)))
	    expect_output(print(results51)$show())
	    invisible(capture.output(expect_error(summary(results51), NA)))
	    expect_output(summary(results51)$show())
	}

	results52 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results52' with expected results
	expect_equal(results52$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results52$thetaH1[2, ], NA_real_)
	expect_equal(results52$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results52$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results52$assumedStDevs[2, ], NA_real_)
	expect_equal(results52$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results52$conditionalRejectionProbabilities[1, ], c(0.044003076, 0.16034604, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$conditionalRejectionProbabilities[3, ], c(0.047740982, 0.33733332, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45861366, 0.70212467), tolerance = 1e-05)
	expect_equal(results52$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results52$conditionalPower[3, ], c(NA_real_, NA_real_, 0.82521432, 0.94583446), tolerance = 1e-05)
	expect_equal(results52$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.548, -27.773536, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.046937, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.481556, -32.250037, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedConfidenceIntervalUpperBounds[1, ], c(14.548, 4.0869289, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedConfidenceIntervalUpperBounds[2, ], c(16.846937, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedConfidenceIntervalUpperBounds[3, ], c(12.481556, -0.21501795, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedPValues[1, ], c(0.5, 0.072804352, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results52$repeatedPValues[3, ], c(0.5, 0.017498028, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results52), NA)))
	    expect_output(print(results52)$show())
	    invisible(capture.output(expect_error(summary(results52), NA)))
	    expect_output(summary(results52)$show())
	}

	results53 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results53' with expected results
	expect_equal(results53$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results53$thetaH1[2, ], NA_real_)
	expect_equal(results53$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results53$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results53$assumedStDevs[2, ], NA_real_)
	expect_equal(results53$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results53$conditionalRejectionProbabilities[1, ], c(0.040514523, 0.14472681, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$conditionalRejectionProbabilities[2, ], c(0.037322006, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$conditionalRejectionProbabilities[3, ], c(0.042460333, 0.28832504, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42908294, 0.68052019), tolerance = 1e-05)
	expect_equal(results53$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results53$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78363361, 0.93049656), tolerance = 1e-05)
	expect_equal(results53$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.598892, -28.449073, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.351395, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedConfidenceIntervalLowerBounds[3, ], c(-51.401351, -32.883177, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedConfidenceIntervalUpperBounds[1, ], c(18.598892, 4.7729883, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedConfidenceIntervalUpperBounds[2, ], c(20.151395, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedConfidenceIntervalUpperBounds[3, ], c(16.401351, 0.41981683, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedPValues[1, ], c(0.5, 0.084586974, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results53$repeatedPValues[3, ], c(0.5, 0.025221821, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results53), NA)))
	    expect_output(print(results53)$show())
	    invisible(capture.output(expect_error(summary(results53), NA)))
	    expect_output(summary(results53)$show())
	}

	results54 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results54' with expected results
	expect_equal(results54$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results54$thetaH1[2, ], NA_real_)
	expect_equal(results54$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results54$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results54$assumedStDevs[2, ], NA_real_)
	expect_equal(results54$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results54$conditionalRejectionProbabilities[1, ], c(0.043739576, 0.16022367, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$conditionalRejectionProbabilities[2, ], c(0.03844608, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$conditionalRejectionProbabilities[3, ], c(0.048616927, 0.34001465, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45838936, 0.70196347), tolerance = 1e-05)
	expect_equal(results54$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results54$conditionalPower[3, ], c(NA_real_, NA_real_, 0.827255, 0.94656521), tolerance = 1e-05)
	expect_equal(results54$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.395028, -27.895908, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.091548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.071339, -32.285152, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedConfidenceIntervalUpperBounds[1, ], c(14.395028, 4.0669225, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedConfidenceIntervalUpperBounds[2, ], c(16.891548, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedConfidenceIntervalUpperBounds[3, ], c(12.071338, -0.24154039, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedPValues[1, ], c(0.5, 0.072888275, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results54$repeatedPValues[3, ], c(0.5, 0.017155659, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results54), NA)))
	    expect_output(print(results54)$show())
	    invisible(capture.output(expect_error(summary(results54), NA)))
	    expect_output(summary(results54)$show())
	}

	results55 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results55' with expected results
	expect_equal(results55$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results55$thetaH1[2, ], NA_real_)
	expect_equal(results55$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results55$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results55$assumedStDevs[2, ], NA_real_)
	expect_equal(results55$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results55$conditionalRejectionProbabilities[1, ], c(0.040100206, 0.14400686, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$conditionalRejectionProbabilities[2, ], c(0.037171319, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$conditionalRejectionProbabilities[3, ], c(0.042866371, 0.28890175, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42767621, 0.6794713), tolerance = 1e-05)
	expect_equal(results55$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results55$conditionalPower[3, ], c(NA_real_, NA_real_, 0.78417464, 0.93070164), tolerance = 1e-05)
	expect_equal(results55$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.433726, -28.584393, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.626743, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedConfidenceIntervalLowerBounds[3, ], c(-50.938808, -32.927366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedConfidenceIntervalUpperBounds[1, ], c(18.433727, 4.7641753, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedConfidenceIntervalUpperBounds[2, ], c(20.426744, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedConfidenceIntervalUpperBounds[3, ], c(15.938808, 0.40329842, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedPValues[1, ], c(0.5, 0.085188742, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results55$repeatedPValues[3, ], c(0.5, 0.025112148, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results55), NA)))
	    expect_output(print(results55)$show())
	    invisible(capture.output(expect_error(summary(results55), NA)))
	    expect_output(summary(results55)$show())
	}

	results56 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results56' with expected results
	expect_equal(results56$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results56$thetaH1[2, ], NA_real_)
	expect_equal(results56$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results56$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results56$assumedStDevs[2, ], NA_real_)
	expect_equal(results56$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results56$conditionalRejectionProbabilities[1, ], c(0.042394596, 0.15198144, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$conditionalRejectionProbabilities[3, ], c(0.049947129, 0.35588619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$conditionalPower[1, ], c(NA_real_, NA_real_, 0.44302928, 0.69082025), tolerance = 1e-05)
	expect_equal(results56$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results56$conditionalPower[3, ], c(NA_real_, NA_real_, 0.83889182, 0.95069292), tolerance = 1e-05)
	expect_equal(results56$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.802158, -28.113846, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.432721, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.786808, -32.10754, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedConfidenceIntervalUpperBounds[1, ], c(14.802158, 4.2854677, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedConfidenceIntervalUpperBounds[2, ], c(19.232722, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedConfidenceIntervalUpperBounds[3, ], c(11.786808, -0.41764291, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedPValues[1, ], c(0.5, 0.078823932, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results56$repeatedPValues[3, ], c(0.5, 0.015272156, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results56), NA)))
	    expect_output(print(results56)$show())
	    invisible(capture.output(expect_error(summary(results56), NA)))
	    expect_output(summary(results56)$show())
	}

	results57 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results57' with expected results
	expect_equal(results57$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results57$thetaH1[2, ], NA_real_)
	expect_equal(results57$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results57$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results57$assumedStDevs[2, ], NA_real_)
	expect_equal(results57$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results57$conditionalRejectionProbabilities[1, ], c(0.04074021, 0.14372404, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$conditionalRejectionProbabilities[3, ], c(0.046882975, 0.32321323, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42712247, 0.6790579), tolerance = 1e-05)
	expect_equal(results57$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results57$conditionalPower[3, ], c(NA_real_, NA_real_, 0.81409137, 0.94181531), tolerance = 1e-05)
	expect_equal(results57$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.567569, -28.528695, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedConfidenceIntervalLowerBounds[2, ], c(-40.140706, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.521691, -32.491815, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedConfidenceIntervalUpperBounds[1, ], c(16.56757, 4.6627973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedConfidenceIntervalUpperBounds[2, ], c(20.940705, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedConfidenceIntervalUpperBounds[3, ], c(13.521691, -0.049006954, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedPValues[1, ], c(0.5, 0.08542716, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results57$repeatedPValues[3, ], c(0.5, 0.019420631, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results57), NA)))
	    expect_output(print(results57)$show())
	    invisible(capture.output(expect_error(summary(results57), NA)))
	    expect_output(summary(results57)$show())
	}

	results58 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results58' with expected results
	expect_equal(results58$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results58$thetaH1[2, ], NA_real_)
	expect_equal(results58$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results58$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results58$assumedStDevs[2, ], NA_real_)
	expect_equal(results58$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results58$conditionalRejectionProbabilities[1, ], c(0.043219832, 0.15803857, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$conditionalRejectionProbabilities[2, ], c(0.038533075, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$conditionalRejectionProbabilities[3, ], c(0.046782117, 0.33290332, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45436587, 0.6990644), tolerance = 1e-05)
	expect_equal(results58$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results58$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8217936, 0.94460493), tolerance = 1e-05)
	expect_equal(results58$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.549821, -27.945069, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedConfidenceIntervalLowerBounds[2, ], c(-36.048567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedConfidenceIntervalLowerBounds[3, ], c(-47.483404, -32.356999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedConfidenceIntervalUpperBounds[1, ], c(14.549821, 4.1214, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedConfidenceIntervalUpperBounds[2, ], c(16.848567, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedConfidenceIntervalUpperBounds[3, ], c(12.483404, -0.16013937, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedPValues[1, ], c(0.5, 0.07440366, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results58$repeatedPValues[3, ], c(0.5, 0.018077861, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results58), NA)))
	    expect_output(print(results58)$show())
	    invisible(capture.output(expect_error(summary(results58), NA)))
	    expect_output(summary(results58)$show())
	}

	results59 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results59' with expected results
	expect_equal(results59$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results59$thetaH1[2, ], NA_real_)
	expect_equal(results59$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results59$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results59$assumedStDevs[2, ], NA_real_)
	expect_equal(results59$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results59$conditionalRejectionProbabilities[1, ], c(0.039664178, 0.14221619, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$conditionalRejectionProbabilities[2, ], c(0.037322006, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$conditionalRejectionProbabilities[3, ], c(0.041377736, 0.28315003, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$conditionalPower[1, ], c(NA_real_, NA_real_, 0.42415922, 0.67684079), tolerance = 1e-05)
	expect_equal(results59$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results59$conditionalPower[3, ], c(NA_real_, NA_real_, 0.77871789, 0.92862656), tolerance = 1e-05)
	expect_equal(results59$repeatedConfidenceIntervalLowerBounds[1, ], c(-48.601467, -28.627869, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.353637, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedConfidenceIntervalLowerBounds[3, ], c(-51.403927, -32.999307, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedConfidenceIntervalUpperBounds[1, ], c(18.601467, 4.8144339, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedConfidenceIntervalUpperBounds[2, ], c(20.153638, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedConfidenceIntervalUpperBounds[3, ], c(16.403927, 0.48327176, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedPValues[1, ], c(0.5, 0.086711756, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results59$repeatedPValues[3, ], c(0.5, 0.026234621, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results59), NA)))
	    expect_output(print(results59)$show())
	    invisible(capture.output(expect_error(summary(results59), NA)))
	    expect_output(summary(results59)$show())
	}

	results60 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results60' with expected results
	expect_equal(results60$thetaH1[1, ], -11.562259, tolerance = 3e-04)
	expect_equal(results60$thetaH1[2, ], NA_real_)
	expect_equal(results60$thetaH1[3, ], -16.036585, tolerance = 3e-04)
	expect_equal(results60$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results60$assumedStDevs[2, ], NA_real_)
	expect_equal(results60$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results60$conditionalRejectionProbabilities[1, ], c(0.046821831, 0.16471605, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$conditionalRejectionProbabilities[2, ], c(0.034321105, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$conditionalRejectionProbabilities[3, ], c(0.056787494, 0.38875376, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$conditionalPower[1, ], c(NA_real_, NA_real_, 0.46655429, 0.70780431), tolerance = 3e-04)
	expect_equal(results60$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results60$conditionalPower[3, ], c(NA_real_, NA_real_, 0.8607725, 0.95827239), tolerance = 3e-04)
	expect_equal(results60$repeatedConfidenceIntervalLowerBounds[1, ], c(-44.645299, -27.415432, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedConfidenceIntervalLowerBounds[2, ], c(-38.280965, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedConfidenceIntervalLowerBounds[3, ], c(-46.632661, -31.563143, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedConfidenceIntervalUpperBounds[1, ], c(14.645299, 3.9276912, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedConfidenceIntervalUpperBounds[2, ], c(19.080965, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedConfidenceIntervalUpperBounds[3, ], c(11.632661, -0.82949344, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedPValues[1, ], c(0.5, 0.069897558, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results60$repeatedPValues[3, ], c(0.5, 0.012021087, NA_real_, NA_real_), tolerance = 3e-04)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results60), NA)))
	    expect_output(print(results60)$show())
	    invisible(capture.output(expect_error(summary(results60), NA)))
	    expect_output(summary(results60)$show())
	}

	results61 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results61' with expected results
	expect_equal(results61$thetaH1[1, ], -11.562259, tolerance = 3e-04)
	expect_equal(results61$thetaH1[2, ], NA_real_)
	expect_equal(results61$thetaH1[3, ], -16.036585, tolerance = 3e-04)
	expect_equal(results61$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results61$assumedStDevs[2, ], NA_real_)
	expect_equal(results61$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results61$conditionalRejectionProbabilities[1, ], c(0.045317936, 0.15683262, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$conditionalRejectionProbabilities[2, ], c(0.033856263, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$conditionalRejectionProbabilities[3, ], c(0.054085341, 0.35883746, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$conditionalPower[1, ], c(NA_real_, NA_real_, 0.45213049, 0.6974477), tolerance = 3e-04)
	expect_equal(results61$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results61$conditionalPower[3, ], c(NA_real_, NA_real_, 0.84097508, 0.95142481), tolerance = 3e-04)
	expect_equal(results61$repeatedConfidenceIntervalLowerBounds[1, ], c(-46.332082, -27.786339, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedConfidenceIntervalLowerBounds[2, ], c(-39.912879, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedConfidenceIntervalLowerBounds[3, ], c(-48.290276, -31.900739, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedConfidenceIntervalUpperBounds[1, ], c(16.332082, 4.255678, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedConfidenceIntervalUpperBounds[2, ], c(20.712879, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedConfidenceIntervalUpperBounds[3, ], c(13.290276, -0.50948619, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedPValues[1, ], c(0.5, 0.075257197, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results61$repeatedPValues[3, ], c(0.5, 0.014946, NA_real_, NA_real_), tolerance = 3e-04)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results61), NA)))
	    expect_output(print(results61)$show())
	    invisible(capture.output(expect_error(summary(results61), NA)))
	    expect_output(summary(results61)$show())
	}

	results62 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results62' with expected results
	expect_equal(results62$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results62$thetaH1[2, ], NA_real_)
	expect_equal(results62$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results62$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results62$assumedStDevs[2, ], NA_real_)
	expect_equal(results62$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results62$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$conditionalRejectionProbabilities[3, ], c(0.029595078, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.256183, -26.806923, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.907669, -31.664999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedConfidenceIntervalUpperBounds[1, ], c(10.256183, 4.0576303, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedConfidenceIntervalUpperBounds[2, ], c(13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedConfidenceIntervalUpperBounds[3, ], c(7.9076686, -0.4326836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$repeatedPValues[3, ], c(0.13700176, 0.014275569, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results62$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2797, 0.439), tolerance = 1e-05)
	expect_equal(results62$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results62$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results62), NA)))
	    expect_output(print(results62)$show())
	    invisible(capture.output(expect_error(summary(results62), NA)))
	    expect_output(summary(results62)$show())
	}

	results63 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results63' with expected results
	expect_equal(results63$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results63$thetaH1[2, ], NA_real_)
	expect_equal(results63$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results63$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results63$assumedStDevs[2, ], NA_real_)
	expect_equal(results63$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results63$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$conditionalRejectionProbabilities[3, ], c(0.027312859, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.830851, -27.447651, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.376075, -32.257244, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedConfidenceIntervalUpperBounds[1, ], c(12.830851, 4.722817, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedConfidenceIntervalUpperBounds[2, ], c(15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedConfidenceIntervalUpperBounds[3, ], c(10.376075, 0.16648466, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$repeatedPValues[3, ], c(0.15234731, 0.019097336, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results63$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2591, 0.4187), tolerance = 1e-05)
	expect_equal(results63$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results63$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results63), NA)))
	    expect_output(print(results63)$show())
	    invisible(capture.output(expect_error(summary(results63), NA)))
	    expect_output(summary(results63)$show())
	}

	results64 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results64' with expected results
	expect_equal(results64$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results64$thetaH1[2, ], NA_real_)
	expect_equal(results64$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results64$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results64$assumedStDevs[2, ], NA_real_)
	expect_equal(results64$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results64$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$conditionalRejectionProbabilities[3, ], c(0.028741907, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.605988, -27.021858, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.6632, -31.486561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedConfidenceIntervalUpperBounds[1, ], c(10.605988, 4.2731734, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedConfidenceIntervalUpperBounds[2, ], c(15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedConfidenceIntervalUpperBounds[3, ], c(7.6631999, -0.60791563, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$repeatedPValues[3, ], c(0.14242148, 0.013628025, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results64$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2699, 0.4282), tolerance = 1e-05)
	expect_equal(results64$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results64$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results64), NA)))
	    expect_output(print(results64)$show())
	    invisible(capture.output(expect_error(summary(results64), NA)))
	    expect_output(summary(results64)$show())
	}

	results65 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results65' with expected results
	expect_equal(results65$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results65$thetaH1[2, ], NA_real_)
	expect_equal(results65$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results65$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results65$assumedStDevs[2, ], NA_real_)
	expect_equal(results65$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results65$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$conditionalRejectionProbabilities[3, ], c(0.027708171, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.74771, -27.425347, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.785178, -31.856528, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedConfidenceIntervalUpperBounds[1, ], c(11.74771, 4.6401767, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedConfidenceIntervalUpperBounds[2, ], c(16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedConfidenceIntervalUpperBounds[3, ], c(8.7851784, -0.25415362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$repeatedPValues[3, ], c(0.1494882, 0.016474737, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results65$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2576, 0.4176), tolerance = 1e-05)
	expect_equal(results65$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results65$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results65), NA)))
	    expect_output(print(results65)$show())
	    invisible(capture.output(expect_error(summary(results65), NA)))
	    expect_output(summary(results65)$show())
	}

	results66 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results66' with expected results
	expect_equal(results66$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results66$thetaH1[2, ], NA_real_)
	expect_equal(results66$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results66$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results66$assumedStDevs[2, ], NA_real_)
	expect_equal(results66$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results66$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$conditionalRejectionProbabilities[3, ], c(0.029264016, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.389181, -26.847363, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.261715, -31.705217, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedConfidenceIntervalUpperBounds[1, ], c(10.389181, 4.1091853, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedConfidenceIntervalUpperBounds[2, ], c(13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedConfidenceIntervalUpperBounds[3, ], c(8.2617152, -0.37246523, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$repeatedPValues[3, ], c(0.13906265, 0.014376658, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results66$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2768, 0.436), tolerance = 1e-05)
	expect_equal(results66$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results66$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results66), NA)))
	    expect_output(print(results66)$show())
	    invisible(capture.output(expect_error(summary(results66), NA)))
	    expect_output(summary(results66)$show())
	}

	results67 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results67' with expected results
	expect_equal(results67$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results67$thetaH1[2, ], NA_real_)
	expect_equal(results67$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results67$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results67$assumedStDevs[2, ], NA_real_)
	expect_equal(results67$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results67$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$conditionalRejectionProbabilities[3, ], c(0.027044989, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.972232, -27.481288, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.763994, -32.295837, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedConfidenceIntervalUpperBounds[1, ], c(12.972232, 4.7692163, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedConfidenceIntervalUpperBounds[2, ], c(15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedConfidenceIntervalUpperBounds[3, ], c(10.763995, 0.22335705, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$repeatedPValues[3, ], c(0.15433667, 0.019180306, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results67$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2568, 0.4157), tolerance = 1e-05)
	expect_equal(results67$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results67$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results67), NA)))
	    expect_output(print(results67)$show())
	    invisible(capture.output(expect_error(summary(results67), NA)))
	    expect_output(summary(results67)$show())
	}

	results68 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results68' with expected results
	expect_equal(results68$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results68$thetaH1[2, ], NA_real_)
	expect_equal(results68$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results68$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results68$assumedStDevs[2, ], NA_real_)
	expect_equal(results68$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results68$conditionalRejectionProbabilities[1, ], c(0.025021019, 0.054834069, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$conditionalRejectionProbabilities[3, ], c(0.027777772, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.247199, -26.680539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.353418, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.898631, -31.584065, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedConfidenceIntervalUpperBounds[1, ], c(10.247199, 4.0258193, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedConfidenceIntervalUpperBounds[2, ], c(13.153418, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedConfidenceIntervalUpperBounds[3, ], c(7.8986307, -0.47811558, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedPValues[1, ], c(0.17089623, 0.061105652, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$repeatedPValues[3, ], c(0.14899419, 0.015246407, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results68$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2833, 0.4422), tolerance = 1e-05)
	expect_equal(results68$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results68$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results68), NA)))
	    expect_output(print(results68)$show())
	    invisible(capture.output(expect_error(summary(results68), NA)))
	    expect_output(summary(results68)$show())
	}

	results69 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results69' with expected results
	expect_equal(results69$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results69$thetaH1[2, ], NA_real_)
	expect_equal(results69$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results69$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results69$assumedStDevs[2, ], NA_real_)
	expect_equal(results69$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results69$conditionalRejectionProbabilities[1, ], c(0.023144095, 0.048545015, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$conditionalRejectionProbabilities[3, ], c(0.0247006, 0.1449328, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.8192, -27.314543, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.60635, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.364486, -32.169333, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedConfidenceIntervalUpperBounds[1, ], c(12.8192, 4.6852584, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedConfidenceIntervalUpperBounds[2, ], c(15.40635, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedConfidenceIntervalUpperBounds[3, ], c(10.364486, 0.1144866, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedPValues[1, ], c(0.18910184, 0.069324401, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$repeatedPValues[3, ], c(0.17379158, 0.021189694, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results69$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2618, 0.4212), tolerance = 1e-05)
	expect_equal(results69$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results69$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6607, 0.7765), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results69), NA)))
	    expect_output(print(results69)$show())
	    invisible(capture.output(expect_error(summary(results69), NA)))
	    expect_output(summary(results69)$show())
	}

	results70 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results70' with expected results
	expect_equal(results70$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results70$thetaH1[2, ], NA_real_)
	expect_equal(results70$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results70$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results70$assumedStDevs[2, ], NA_real_)
	expect_equal(results70$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results70$conditionalRejectionProbabilities[1, ], c(0.024319059, 0.051462476, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$conditionalRejectionProbabilities[3, ], c(0.028516214, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.59688, -26.894985, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.364237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.654249, -31.405859, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedConfidenceIntervalUpperBounds[1, ], c(10.59688, 4.2407133, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedConfidenceIntervalUpperBounds[2, ], c(15.164237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedConfidenceIntervalUpperBounds[3, ], c(7.6542489, -0.6529301, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedPValues[1, ], c(0.17734783, 0.06527034, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$repeatedPValues[3, ], c(0.14391589, 0.013711948, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results70$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2722, 0.4311), tolerance = 1e-05)
	expect_equal(results70$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results70$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results70), NA)))
	    expect_output(print(results70)$show())
	    invisible(capture.output(expect_error(summary(results70), NA)))
	    expect_output(summary(results70)$show())
	}

	results71 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results71' with expected results
	expect_equal(results71$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results71$thetaH1[2, ], NA_real_)
	expect_equal(results71$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results71$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results71$assumedStDevs[2, ], NA_real_)
	expect_equal(results71$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results71$conditionalRejectionProbabilities[1, ], c(0.023469013, 0.048270226, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$conditionalRejectionProbabilities[3, ], c(0.026830382, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.737451, -27.295819, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.467707, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.775098, -31.772829, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedConfidenceIntervalUpperBounds[1, ], c(11.737451, 4.6050352, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedConfidenceIntervalUpperBounds[2, ], c(16.267707, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedConfidenceIntervalUpperBounds[3, ], c(8.7750975, -0.30217392, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedPValues[1, ], c(0.18572393, 0.069730666, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$repeatedPValues[3, ], c(0.15596268, 0.017006886, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results71$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.261, 0.4206), tolerance = 1e-05)
	expect_equal(results71$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results71$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results71), NA)))
	    expect_output(print(results71)$show())
	    invisible(capture.output(expect_error(summary(results71), NA)))
	    expect_output(summary(results71)$show())
	}

	results72 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results72' with expected results
	expect_equal(results72$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results72$thetaH1[2, ], NA_real_)
	expect_equal(results72$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results72$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results72$assumedStDevs[2, ], NA_real_)
	expect_equal(results72$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results72$conditionalRejectionProbabilities[1, ], c(0.024748593, 0.053966892, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$conditionalRejectionProbabilities[3, ], c(0.0267758, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.38015, -26.720108, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.316502, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.252551, -31.62149, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedConfidenceIntervalUpperBounds[1, ], c(10.38015, 4.0770639, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedConfidenceIntervalUpperBounds[2, ], c(13.116502, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedConfidenceIntervalUpperBounds[3, ], c(8.2525514, -0.41959343, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedPValues[1, ], c(0.17335289, 0.062127989, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$repeatedPValues[3, ], c(0.15638134, 0.015781417, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results72$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2799, 0.439), tolerance = 1e-05)
	expect_equal(results72$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results72$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results72), NA)))
	    expect_output(print(results72)$show())
	    invisible(capture.output(expect_error(summary(results72), NA)))
	    expect_output(summary(results72)$show())
	}

	results73 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Sidak", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results73' with expected results
	expect_equal(results73$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results73$thetaH1[2, ], NA_real_)
	expect_equal(results73$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results73$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results73$assumedStDevs[2, ], NA_real_)
	expect_equal(results73$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results73$conditionalRejectionProbabilities[1, ], c(0.022923976, 0.04788638, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$conditionalRejectionProbabilities[3, ], c(0.023933809, 0.14146912, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.960526, -27.347242, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.425975, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.752245, -32.205007, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedConfidenceIntervalUpperBounds[1, ], c(12.960526, 4.7313117, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedConfidenceIntervalUpperBounds[2, ], c(15.225975, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedConfidenceIntervalUpperBounds[3, ], c(10.752245, 0.16953037, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedPValues[1, ], c(0.19144883, 0.07030573, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$repeatedPValues[3, ], c(0.18106429, 0.021778109, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results73$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2594, 0.4195), tolerance = 1e-05)
	expect_equal(results73$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results73$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6537, 0.7739), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results73), NA)))
	    expect_output(print(results73)$show())
	    invisible(capture.output(expect_error(summary(results73), NA)))
	    expect_output(summary(results73)$show())
	}

	results74 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results74' with expected results
	expect_equal(results74$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results74$thetaH1[2, ], NA_real_)
	expect_equal(results74$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results74$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results74$assumedStDevs[2, ], NA_real_)
	expect_equal(results74$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results74$conditionalRejectionProbabilities[1, ], c(0.024608533, 0.053964296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$conditionalRejectionProbabilities[2, ], c(0.021871903, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$conditionalRejectionProbabilities[3, ], c(0.027261939, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.256183, -26.806923, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.361515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.907669, -31.664999, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedConfidenceIntervalUpperBounds[1, ], c(10.256183, 4.0576303, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedConfidenceIntervalUpperBounds[2, ], c(13.161515, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedConfidenceIntervalUpperBounds[3, ], c(7.9076686, -0.4326836, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedPValues[1, ], c(0.17463845, 0.062131804, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedPValues[2, ], c(0.20337355, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$repeatedPValues[3, ], c(0.1527221, 0.015597359, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results74$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2797, 0.439), tolerance = 1e-05)
	expect_equal(results74$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results74$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results74), NA)))
	    expect_output(print(results74)$show())
	    invisible(capture.output(expect_error(summary(results74), NA)))
	    expect_output(summary(results74)$show())
	}

	results75 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "notPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results75' with expected results
	expect_equal(results75$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results75$thetaH1[2, ], NA_real_)
	expect_equal(results75$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results75$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results75$assumedStDevs[2, ], NA_real_)
	expect_equal(results75$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results75$conditionalRejectionProbabilities[1, ], c(0.022711489, 0.047669561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$conditionalRejectionProbabilities[2, ], c(0.021234311, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$conditionalRejectionProbabilities[3, ], c(0.024147032, 0.14148061, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.830851, -27.447651, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.616779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.376075, -32.257244, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedConfidenceIntervalUpperBounds[1, ], c(12.830851, 4.722817, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedConfidenceIntervalUpperBounds[2, ], c(15.416779, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedConfidenceIntervalUpperBounds[3, ], c(10.376075, 0.16648466, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedPValues[1, ], c(0.19376148, 0.070634747, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedPValues[2, ], c(0.2112175, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$repeatedPValues[3, ], c(0.17899101, 0.021776202, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results75$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2591, 0.4187), tolerance = 1e-05)
	expect_equal(results75$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results75$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6537, 0.774), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results75), NA)))
	    expect_output(print(results75)$show())
	    invisible(capture.output(expect_error(summary(results75), NA)))
	    expect_output(summary(results75)$show())
	}

	results76 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results76' with expected results
	expect_equal(results76$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results76$thetaH1[2, ], NA_real_)
	expect_equal(results76$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results76$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results76$assumedStDevs[2, ], NA_real_)
	expect_equal(results76$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results76$conditionalRejectionProbabilities[1, ], c(0.02389937, 0.050606752, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$conditionalRejectionProbabilities[3, ], c(0.028008383, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.605988, -27.021858, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.373049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.6632, -31.486561, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedConfidenceIntervalUpperBounds[1, ], c(10.605988, 4.2731734, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedConfidenceIntervalUpperBounds[2, ], c(15.173049, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedConfidenceIntervalUpperBounds[3, ], c(7.6631999, -0.60791563, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedPValues[1, ], c(0.18140284, 0.066412839, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$repeatedPValues[3, ], c(0.14737581, 0.014014262, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results76$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2699, 0.4282), tolerance = 1e-05)
	expect_equal(results76$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results76$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results76), NA)))
	    expect_output(print(results76)$show())
	    invisible(capture.output(expect_error(summary(results76), NA)))
	    expect_output(summary(results76)$show())
	}

	results77 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results77' with expected results
	expect_equal(results77$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results77$thetaH1[2, ], NA_real_)
	expect_equal(results77$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results77$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results77$assumedStDevs[2, ], NA_real_)
	expect_equal(results77$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results77$conditionalRejectionProbabilities[1, ], c(0.023040094, 0.047419024, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$conditionalRejectionProbabilities[3, ], c(0.026303733, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.74771, -27.425347, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.477631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.785178, -31.856528, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedConfidenceIntervalUpperBounds[1, ], c(11.74771, 4.6401767, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedConfidenceIntervalUpperBounds[2, ], c(16.277631, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedConfidenceIntervalUpperBounds[3, ], c(8.7851784, -0.25415362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedPValues[1, ], c(0.19020524, 0.071018123, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$repeatedPValues[3, ], c(0.16007682, 0.01742078, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results77$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2576, 0.4176), tolerance = 1e-05)
	expect_equal(results77$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results77$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results77), NA)))
	    expect_output(print(results77)$show())
	    invisible(capture.output(expect_error(summary(results77), NA)))
	    expect_output(summary(results77)$show())
	}

	results78 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results78' with expected results
	expect_equal(results78$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results78$thetaH1[2, ], NA_real_)
	expect_equal(results78$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results78$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results78$assumedStDevs[2, ], NA_real_)
	expect_equal(results78$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results78$conditionalRejectionProbabilities[1, ], c(0.024333354, 0.053095357, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$conditionalRejectionProbabilities[2, ], c(0.021915713, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$conditionalRejectionProbabilities[3, ], c(0.026248507, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.389181, -26.847363, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedConfidenceIntervalLowerBounds[2, ], c(-32.324586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.261715, -31.705217, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedConfidenceIntervalUpperBounds[1, ], c(10.389181, 4.1091853, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedConfidenceIntervalUpperBounds[2, ], c(13.124586, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedConfidenceIntervalUpperBounds[3, ], c(8.2617152, -0.37246523, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedPValues[1, ], c(0.17721241, 0.063189426, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedPValues[2, ], c(0.20285189, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$repeatedPValues[3, ], c(0.16051933, 0.01616384, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results78$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2768, 0.436), tolerance = 1e-05)
	expect_equal(results78$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results78$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results78), NA)))
	    expect_output(print(results78)$show())
	    invisible(capture.output(expect_error(summary(results78), NA)))
	    expect_output(summary(results78)$show())
	}

	results79 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Bonferroni", varianceOption = "pairwisePooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results79' with expected results
	expect_equal(results79$thetaH1[1, ], -11.562259, tolerance = 1e-05)
	expect_equal(results79$thetaH1[2, ], NA_real_)
	expect_equal(results79$thetaH1[3, ], -16.036585, tolerance = 1e-05)
	expect_equal(results79$assumedStDevs[1, ], 22.357668, tolerance = 1e-05)
	expect_equal(results79$assumedStDevs[2, ], NA_real_)
	expect_equal(results79$assumedStDevs[3, ], 22.943518, tolerance = 1e-05)
	expect_equal(results79$conditionalRejectionProbabilities[1, ], c(0.02248882, 0.047009108, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$conditionalRejectionProbabilities[2, ], c(0.021309255, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$conditionalRejectionProbabilities[3, ], c(0.023369532, 0.13794488, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedConfidenceIntervalLowerBounds[1, ], c(-42.972232, -27.481288, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.436237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedConfidenceIntervalLowerBounds[3, ], c(-45.763994, -32.295837, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedConfidenceIntervalUpperBounds[1, ], c(12.972232, 4.7692163, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedConfidenceIntervalUpperBounds[2, ], c(15.236237, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedConfidenceIntervalUpperBounds[3, ], c(10.763995, 0.22335705, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedPValues[1, ], c(0.19623626, 0.071653269, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedPValues[2, ], c(0.21026955, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$repeatedPValues[3, ], c(0.18674722, 0.022408487, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results79$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2568, 0.4157), tolerance = 1e-05)
	expect_equal(results79$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results79$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 0.6466, 0.7714), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results79), NA)))
	    expect_output(print(results79)$show())
	    invisible(capture.output(expect_error(summary(results79), NA)))
	    expect_output(summary(results79)$show())
	}

	results80 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results80' with expected results
	expect_equal(results80$thetaH1[1, ], -11.562259, tolerance = 3e-04)
	expect_equal(results80$thetaH1[2, ], NA_real_)
	expect_equal(results80$thetaH1[3, ], -16.036585, tolerance = 3e-04)
	expect_equal(results80$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results80$assumedStDevs[2, ], NA_real_)
	expect_equal(results80$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results80$conditionalRejectionProbabilities[1, ], c(0.026270246, 0.055429547, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$conditionalRejectionProbabilities[2, ], c(0.019837849, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$conditionalRejectionProbabilities[3, ], c(0.032007375, 1, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedConfidenceIntervalLowerBounds[1, ], c(-40.308255, -26.527826, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedConfidenceIntervalLowerBounds[2, ], c(-34.085001, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedConfidenceIntervalLowerBounds[3, ], c(-42.370615, -31.063058, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedConfidenceIntervalUpperBounds[1, ], c(10.308255, 3.9367007, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedConfidenceIntervalUpperBounds[2, ], c(14.885001, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedConfidenceIntervalUpperBounds[3, ], c(7.3706149, -0.96851182, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedPValues[1, ], c(0.1603448, 0.060420915, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedPValues[2, ], c(0.23027951, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$repeatedPValues[3, ], c(0.12340907, 0.011635803, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results80$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2846, 0.4438), tolerance = 3e-04)
	expect_equal(results80$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results80$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results80), NA)))
	    expect_output(print(results80)$show())
	    invisible(capture.output(expect_error(summary(results80), NA)))
	    expect_output(summary(results80)$show())
	}

	results81 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", nPlanned = c(20, 20), seed = 1234, iterations = 10000, normalApproximation = FALSE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results81' with expected results
	expect_equal(results81$thetaH1[1, ], -11.562259, tolerance = 3e-04)
	expect_equal(results81$thetaH1[2, ], NA_real_)
	expect_equal(results81$thetaH1[3, ], -16.036585, tolerance = 3e-04)
	expect_equal(results81$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results81$assumedStDevs[2, ], NA_real_)
	expect_equal(results81$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results81$conditionalRejectionProbabilities[1, ], c(0.025453046, 0.052196173, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$conditionalRejectionProbabilities[2, ], c(0.019613852, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$conditionalRejectionProbabilities[3, ], c(0.030395001, 1, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedConfidenceIntervalLowerBounds[1, ], c(-41.358332, -26.891213, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedConfidenceIntervalLowerBounds[2, ], c(-35.10092, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedConfidenceIntervalLowerBounds[3, ], c(-43.402534, -31.392765, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedConfidenceIntervalUpperBounds[1, ], c(11.358332, 4.2589864, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedConfidenceIntervalUpperBounds[2, ], c(15.90092, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedConfidenceIntervalUpperBounds[3, ], c(8.402534, -0.65713259, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedPValues[1, ], c(0.16711874, 0.064319528, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedPValues[2, ], c(0.23360401, NA_real_, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$repeatedPValues[3, ], c(0.13222673, 0.014209765, NA_real_, NA_real_), tolerance = 3e-04)
	expect_equal(results81$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.2736, 0.4337), tolerance = 3e-04)
	expect_equal(results81$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results81$conditionalPowerSimulated[3, ], c(NA_real_, NA_real_, 1, 1))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results81), NA)))
	    expect_output(print(results81)$show())
	    invisible(capture.output(expect_error(summary(results81), NA)))
	    expect_output(summary(results81)$show())
	}

	results82 <- getAnalysisResults(design = design3, dataInput = dataExample2,
								intersectionTest = "Dunnett", varianceOption = "overallPooled", normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results82' with expected results
	expect_equal(results82$thetaH1[1, ], -11.562259, tolerance = 3e-04)
	expect_equal(results82$thetaH1[2, ], NA_real_)
	expect_equal(results82$thetaH1[3, ], -16.036585, tolerance = 3e-04)
	expect_equal(results82$assumedStDevs[1, ], 22.357668, tolerance = 3e-04)
	expect_equal(results82$assumedStDevs[2, ], NA_real_)
	expect_equal(results82$assumedStDevs[3, ], 22.943518, tolerance = 3e-04)
	expect_equal(results82$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.061352393), tolerance = 3e-04)
	expect_equal(results82$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.037447419), tolerance = 3e-04)
	expect_equal(results82$conditionalRejectionProbabilities[3, ], c(NA_real_, 0.08651207), tolerance = 3e-04)
	expect_equal(results82$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results82$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results82$conditionalPower[3, ], c(NA_real_, NA_real_))
	expect_equal(results82$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -22.538721), tolerance = 3e-04)
	expect_equal(results82$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results82$repeatedConfidenceIntervalLowerBounds[3, ], c(NA_real_, -26.753524), tolerance = 3e-04)
	expect_equal(results82$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.72440621), tolerance = 3e-04)
	expect_equal(results82$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results82$repeatedConfidenceIntervalUpperBounds[3, ], c(NA_real_, -3.9389233), tolerance = 3e-04)
	expect_equal(results82$repeatedPValues[1, ], c(NA_real_, 0.017445576), tolerance = 3e-04)
	expect_equal(results82$repeatedPValues[2, ], c(NA_real_, NA_real_))
	expect_equal(results82$repeatedPValues[3, ], c(NA_real_, 0.0019493527), tolerance = 3e-04)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results82), NA)))
	    expect_output(print(results82)$show())
	    invisible(capture.output(expect_error(summary(results82), NA)))
	    expect_output(summary(results82)$show())
	}

})
