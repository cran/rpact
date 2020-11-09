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
#:#  Creation date: 09 November 2020, 11:47:23
#:#  File version: $Revision$
#:#  Last changed: $Date$
#:#  Last changed by: $Author$
#:#  

context("Testing the Analysis Survival Functionality for Three or More Treatments")

test_that("'getAnalysisResultsMultiArm' with survival data and different options", {

	design1 <- getDesignInverseNormal(kMax = 3, alpha = 0.025, futilityBounds = c(-0.5,0), 
			bindingFutility = FALSE, typeOfDesign = "asKD", gammaA = 1.2, informationRates = c(0.4,0.7,1))
		
		design2 <- getDesignFisher(kMax = 3, alpha = 0.025, alpha0Vec = c(0.7,0.5), method = "equalAlpha", 
			bindingFutility = TRUE, informationRates = c(0.4,0.7,1))
		
		design3 <- getDesignConditionalDunnett(alpha = 0.025, informationAtInterim = 0.4, secondStageConditioning = TRUE)
		
		# directionUpper = TRUE
		dataExample1 <- getDataset(
			events1   = c(25, 32), 
			events2   = c(18, NA),
			logRanks1 = c(2.2,1.8),	
			logRanks2 = c(1.99, NA) 
		)
		
		# directionUpper = FALSE
		dataExample2 <- getDataset(
			events1   =  c(25, 32),
			events2   =  c(18, NA),
			logRanks1 = -c(2.2,1.8),
			logRanks2 = -c(1.99, NA)
		)


 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Dunnett", nPlanned = c(20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results1$thetaH1[2, ], NA_real_)
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.16551988, 0.53357188, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.16551988, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95961075), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(0.84462483, 1.0978923, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(0.74230032, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(6.8816796, 4.1951386, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(8.7950723, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[1, ], c(0.077362906, 0.0096216473, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[2, ], c(0.077362906, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	}

	.skipTestIfDisabled()

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Simes", nPlanned = c(20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results2$thetaH1[2, ], NA_real_)
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.17669226, 0.55323068, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.17669226, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.96373388), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(0.83909699, 1.0883374, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(0.73657799, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(6.9270216, 4.2761954, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(8.8634059, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[1, ], c(0.069951918, 0.0087766935, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[2, ], c(0.069951918, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results3 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Sidak", nPlanned = c(20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results3' with expected results
	expect_equal(results3$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results3$thetaH1[2, ], NA_real_)
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.15801679, 0.51979239, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.15801679, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9565118), tolerance = 1e-05)
	expect_equal(results3$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(0.83933393, 1.0895056, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(0.73682316, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(6.9250602, 4.2563039, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(8.8604482, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[1, ], c(0.082919001, 0.010252978, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[2, ], c(0.082919001, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results3), NA)))
	    expect_output(print(results3)$show())
	    invisible(capture.output(expect_error(summary(results3), NA)))
	    expect_output(summary(results3)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results4 <- getAnalysisResults(design = design1, dataInput = dataExample1,
						intersectionTest = "Bonferroni", nPlanned = c(20), directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results4' with expected results
	expect_equal(results4$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results4$thetaH1[2, ], NA_real_)
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.15727093, 0.51839597, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.15727093, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95618769), tolerance = 1e-05)
	expect_equal(results4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(0.83909699, 1.0883374, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(0.73657799, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(6.9270216, 4.2761954, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(8.8634059, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[1, ], c(0.083499788, 0.010318782, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[2, ], c(0.083499788, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results4), NA)))
	    expect_output(print(results4)$show())
	    invisible(capture.output(expect_error(summary(results4), NA)))
	    expect_output(summary(results4)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results5 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Dunnett", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results5' with expected results
	expect_equal(results5$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results5$thetaH1[2, ], NA_real_)
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(0.10966368, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(0.10966368, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_, 0.93227664), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(0.91202463, 1.0654055, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(0.81259534, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(6.3731146, 4.2132456, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(8.0342369, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[1, ], c(0.04389568, 0.013378163, NA_real_), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[2, ], c(0.04389568, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results5), NA)))
	    expect_output(print(results5)$show())
	    invisible(capture.output(expect_error(summary(results5), NA)))
	    expect_output(summary(results5)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results6 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Simes", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results6' with expected results
	expect_equal(results6$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results6$thetaH1[2, ], NA_real_)
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.1211541, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.1211541, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.94819096), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(0.90417824, 1.0568242, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(0.80436275, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(6.4284199, 4.2747728, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(8.1164667, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[1, ], c(0.039924588, 0.01222708, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[2, ], c(0.039924588, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results6), NA)))
	    expect_output(print(results6)$show())
	    invisible(capture.output(expect_error(summary(results6), NA)))
	    expect_output(summary(results6)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results7 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Sidak", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results7' with expected results
	expect_equal(results7$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results7$thetaH1[2, ], NA_real_)
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.1023739, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.1023739, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.92036569), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(0.90464342, 1.0577667, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(0.80485046, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(6.4251144, 4.2597035, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(8.1115484, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[1, ], c(0.046853018, 0.014230746, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[2, ], c(0.046853018, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results7), NA)))
	    expect_output(print(results7)$show())
	    invisible(capture.output(expect_error(summary(results7), NA)))
	    expect_output(summary(results7)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results8 <- getAnalysisResults(design = design2, dataInput = dataExample1,
						intersectionTest = "Bonferroni", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
	expect_equal(results8$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results8$thetaH1[2, ], NA_real_)
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.10166729, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.10166729, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalPower[1, ], c(NA_real_, NA_real_, 0.91912747), tolerance = 1e-05)
	expect_equal(results8$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(0.90417824, 1.0568242, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(0.80436275, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(6.4284199, 4.2747728, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(8.1164667, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[1, ], c(0.047161054, 0.014319438, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[2, ], c(0.047161054, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results8), NA)))
	    expect_output(print(results8)$show())
	    invisible(capture.output(expect_error(summary(results8), NA)))
	    expect_output(summary(results8)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results9 <- getAnalysisResults(design = design3, dataInput = dataExample1,
						intersectionTest = "Dunnett", directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results9' with expected results
	expect_equal(results9$thetaH1[1, ], 2.1027372, tolerance = 1e-05)
	expect_equal(results9$thetaH1[2, ], NA_real_)
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.20921255), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.18260705), tolerance = 1e-05)
	expect_equal(results9$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results9$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, 1.2250509), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 3.6401262), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results9$repeatedPValues[1, ], c(NA_real_, 0.0032883088), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[2, ], c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results9), NA)))
	    expect_output(print(results9)$show())
	    invisible(capture.output(expect_error(summary(results9), NA)))
	    expect_output(summary(results9)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results10 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Dunnett", nPlanned = c(20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results10' with expected results
	expect_equal(results10$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results10$thetaH1[2, ], NA_real_)
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(0.16551988, 0.53357188, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(0.16551988, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95961075), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14531336, 0.23837116, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(0.11370003, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1839576, 0.91083607, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3471639, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[1, ], c(0.077362906, 0.0096216473, NA_real_), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[2, ], c(0.077362906, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results11 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Simes", nPlanned = c(20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results11' with expected results
	expect_equal(results11$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results11$thetaH1[2, ], NA_real_)
	expect_equal(results11$conditionalRejectionProbabilities[1, ], c(0.17669226, 0.55323068, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalRejectionProbabilities[2, ], c(0.17669226, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$conditionalPower[1, ], c(NA_real_, NA_real_, 0.96373388), tolerance = 1e-05)
	expect_equal(results11$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14436262, 0.23385271, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalLowerBounds[2, ], c(0.1128237, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1917587, 0.91883308, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3576308, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[1, ], c(0.069951918, 0.0087766935, NA_real_), tolerance = 1e-05)
	expect_equal(results11$repeatedPValues[2, ], c(0.069951918, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results11), NA)))
	    expect_output(print(results11)$show())
	    invisible(capture.output(expect_error(summary(results11), NA)))
	    expect_output(summary(results11)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results12 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Sidak", nPlanned = c(20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results12' with expected results
	expect_equal(results12$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results12$thetaH1[2, ], NA_real_)
	expect_equal(results12$conditionalRejectionProbabilities[1, ], c(0.15801679, 0.51979239, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalRejectionProbabilities[2, ], c(0.15801679, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$conditionalPower[1, ], c(NA_real_, NA_real_, 0.9565118), tolerance = 1e-05)
	expect_equal(results12$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14440308, 0.23494562, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalLowerBounds[2, ], c(0.11286087, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1914212, 0.91784736, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3571778, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[1, ], c(0.082919001, 0.010252978, NA_real_), tolerance = 1e-05)
	expect_equal(results12$repeatedPValues[2, ], c(0.082919001, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results12), NA)))
	    expect_output(print(results12)$show())
	    invisible(capture.output(expect_error(summary(results12), NA)))
	    expect_output(summary(results12)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results13 <- getAnalysisResults(design = design1, dataInput = dataExample2,
						intersectionTest = "Bonferroni", nPlanned = c(20), directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results13' with expected results
	expect_equal(results13$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results13$thetaH1[2, ], NA_real_)
	expect_equal(results13$conditionalRejectionProbabilities[1, ], c(0.15727093, 0.51839597, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalRejectionProbabilities[2, ], c(0.15727093, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$conditionalPower[1, ], c(NA_real_, NA_real_, 0.95618769), tolerance = 1e-05)
	expect_equal(results13$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[1, ], c(0.14436262, 0.23385271, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalLowerBounds[2, ], c(0.1128237, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1917587, 0.91883308, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedConfidenceIntervalUpperBounds[2, ], c(1.3576308, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[1, ], c(0.083499788, 0.010318782, NA_real_), tolerance = 1e-05)
	expect_equal(results13$repeatedPValues[2, ], c(0.083499788, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results13), NA)))
	    expect_output(print(results13)$show())
	    invisible(capture.output(expect_error(summary(results13), NA)))
	    expect_output(summary(results13)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results14 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Dunnett", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results14' with expected results
	expect_equal(results14$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results14$thetaH1[2, ], NA_real_)
	expect_equal(results14$conditionalRejectionProbabilities[1, ], c(0.10966368, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalRejectionProbabilities[2, ], c(0.10966368, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$conditionalPower[1, ], c(NA_real_, NA_real_, 0.93227664), tolerance = 1e-05)
	expect_equal(results14$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15690919, 0.23734662, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalLowerBounds[2, ], c(0.12446713, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[1, ], c(1.0964616, 0.93860979, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2306248, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[1, ], c(0.04389568, 0.013378163, NA_real_), tolerance = 1e-05)
	expect_equal(results14$repeatedPValues[2, ], c(0.04389568, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results14), NA)))
	    expect_output(print(results14)$show())
	    invisible(capture.output(expect_error(summary(results14), NA)))
	    expect_output(summary(results14)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results15 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Simes", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results15' with expected results
	expect_equal(results15$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results15$thetaH1[2, ], NA_real_)
	expect_equal(results15$conditionalRejectionProbabilities[1, ], c(0.1211541, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalRejectionProbabilities[2, ], c(0.1211541, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$conditionalPower[1, ], c(NA_real_, NA_real_, 0.94819096), tolerance = 1e-05)
	expect_equal(results15$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15555937, 0.23393056, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalLowerBounds[2, ], c(0.12320632, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1059766, 0.94623115, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2432202, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[1, ], c(0.039924588, 0.01222708, NA_real_), tolerance = 1e-05)
	expect_equal(results15$repeatedPValues[2, ], c(0.039924588, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results15), NA)))
	    expect_output(print(results15)$show())
	    invisible(capture.output(expect_error(summary(results15), NA)))
	    expect_output(summary(results15)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results16 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Sidak", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results16' with expected results
	expect_equal(results16$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results16$thetaH1[2, ], NA_real_)
	expect_equal(results16$conditionalRejectionProbabilities[1, ], c(0.1023739, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalRejectionProbabilities[2, ], c(0.1023739, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$conditionalPower[1, ], c(NA_real_, NA_real_, 0.92036569), tolerance = 1e-05)
	expect_equal(results16$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15563938, 0.23475813, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalLowerBounds[2, ], c(0.1232811, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1054079, 0.94538806, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2424668, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[1, ], c(0.046853018, 0.014230746, NA_real_), tolerance = 1e-05)
	expect_equal(results16$repeatedPValues[2, ], c(0.046853018, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results16), NA)))
	    expect_output(print(results16)$show())
	    invisible(capture.output(expect_error(summary(results16), NA)))
	    expect_output(summary(results16)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results17 <- getAnalysisResults(design = design2, dataInput = dataExample2,
						intersectionTest = "Bonferroni", nPlanned = c(20), seed = 1234, iterations = 1000, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results17' with expected results
	expect_equal(results17$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results17$thetaH1[2, ], NA_real_)
	expect_equal(results17$conditionalRejectionProbabilities[1, ], c(0.10166729, 1, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalRejectionProbabilities[2, ], c(0.10166729, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$conditionalPower[1, ], c(NA_real_, NA_real_, 0.91912747), tolerance = 1e-05)
	expect_equal(results17$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[1, ], c(0.15555937, 0.23393056, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalLowerBounds[2, ], c(0.12320632, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[1, ], c(1.1059766, 0.94623115, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2432202, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[1, ], c(0.047161054, 0.014319438, NA_real_), tolerance = 1e-05)
	expect_equal(results17$repeatedPValues[2, ], c(0.047161054, NA_real_, NA_real_), tolerance = 1e-05)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results17), NA)))
	    expect_output(print(results17)$show())
	    invisible(capture.output(expect_error(summary(results17), NA)))
	    expect_output(summary(results17)$show())
	}

 	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityDunnett}
	# @refFS[Formula]{fs:pValueConditionalSecondStageDunnett}
	# @refFS[Formula]{fs:pValueUnconditionalSecondStageDunnett}
	# @refFS[Formula]{fs:conditionalPowerMultiArm}
	# @refFS[Formula]{fs:conditionalRejectionProbability}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityShiftedBoundaries}
	# @refFS[Formula]{fs:conditionalRejectionProbabilityMultiArm}
	# @refFS[Formula]{fs:calculationRepeatedpValueMultiArm}
	# @refFS[Formula]{fs:adjustedPValueForRCIDunnett}
	# @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimes}
	# @refFS[Formula]{fs:adjustedPValueForRCISidak}
	# @refFS[Formula]{fs:computeRCIsMultiArm}
	# @refFS[Formula]{fs:testStatisticMultiArmSurvival}
	results18 <- getAnalysisResults(design = design3, dataInput = dataExample2,
						intersectionTest = "Dunnett", directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results18' with expected results
	expect_equal(results18$thetaH1[1, ], 0.47557061, tolerance = 1e-05)
	expect_equal(results18$thetaH1[2, ], NA_real_)
	expect_equal(results18$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.20921255), tolerance = 1e-05)
	expect_equal(results18$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.18260705), tolerance = 1e-05)
	expect_equal(results18$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results18$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, 0.27471638), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.81629276), tolerance = 1e-05)
	expect_equal(results18$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results18$repeatedPValues[1, ], c(NA_real_, 0.0032883088), tolerance = 1e-05)
	expect_equal(results18$repeatedPValues[2, ], c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results18), NA)))
	    expect_output(print(results18)$show())
	    invisible(capture.output(expect_error(summary(results18), NA)))
	    expect_output(summary(results18)$show())
	}

})
