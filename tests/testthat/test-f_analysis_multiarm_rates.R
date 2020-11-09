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
#:#  Creation date: 09 November 2020, 11:46:38
#:#  File version: $Revision$
#:#  Last changed: $Date$
#:#  Last changed by: $Author$
#:#  

context("Testing the Analysis Rates Functionality for Three or More Treatments")

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
			n3 = c(22, 29), 
			events1 = c(15, 12), 
			events2 = c(19, NA), 
			events3 = c(12, 13))
		
		# directionUpper = FALSE
		dataExample2 <- getDataset(
			n1 = c(23, 25), 
			n2 = c(25, NA),	
			n3 = c(22, 29), 
			events1 = c(15, 12), 
			events2 = c(19, NA),
			events3 = c(21, 25))


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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results1 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results1' with expected results
	expect_equal(results1$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results1$piTreatments[2, ], NA_real_)
	expect_equal(results1$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[1, ], c(0.015420568, 0.003193865, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalRejectionProbabilities[2, ], c(0.024462749, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.0010766875, 0.011284718), tolerance = 1e-05)
	expect_equal(results1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32184855, -0.20584893, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20645626, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[1, ], c(0.50115821, 0.32866159, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57764472, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results1$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results2 <- getAnalysisResults(design = design1, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results2' with expected results
	expect_equal(results2$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results2$piTreatments[2, ], NA_real_)
	expect_equal(results2$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[1, ], c(0.022712676, 0.0087985229, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalRejectionProbabilities[2, ], c(0.043097832, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.004624756, 0.026737358), tolerance = 1e-05)
	expect_equal(results2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.32069398, -0.20381973, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.20524167, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[1, ], c(0.5001877, 0.32441792, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedConfidenceIntervalUpperBounds[2, ], c(0.57677218, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[1, ], c(0.5, 0.5, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results2$repeatedPValues[2, ], c(0.5, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results3 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = FALSE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results3' with expected results
	expect_equal(results3$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results3$piTreatments[2, ], NA_real_)
	expect_equal(results3$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[1, ], c(0.011503611, 0, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalRejectionProbabilities[2, ], c(0.015301846, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26319109, -0.20678373, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14541584, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[1, ], c(0.45121457, 0.32319296, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53261778, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[1, ], c(0.4416362, 0.4416362, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$repeatedPValues[2, ], c(0.31730879, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results3$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0, 0))
	expect_equal(results3$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results4 <- getAnalysisResults(design = design2, dataInput = dataExample1,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results4' with expected results
	expect_equal(results4$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results4$piTreatments[2, ], NA_real_)
	expect_equal(results4$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[1, ], c(0.014541388, 0.0059378141, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalRejectionProbabilities[2, ], c(0.024268969, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.26076213, -0.20472006, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.14291708, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[1, ], c(0.44911894, 0.31972469, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedConfidenceIntervalUpperBounds[2, ], c(0.53072029, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[1, ], c(0.3372539, 0.3372539, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$repeatedPValues[2, ], c(0.17782371, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results4$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 0.011, 0.018), tolerance = 1e-05)
	expect_equal(results4$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results5 <- getAnalysisResults(design = design3, dataInput = dataExample1,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = TRUE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results5' with expected results
	expect_equal(results5$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results5$piTreatments[2, ], NA_real_)
	expect_equal(results5$piControl[1, ], 0.49019608, tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.019942093), tolerance = 1e-05)
	expect_equal(results5$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.049973806), tolerance = 1e-05)
	expect_equal(results5$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results5$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.10423565), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, 0.28064632), tolerance = 1e-05)
	expect_equal(results5$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results5$repeatedPValues[1, ], c(NA_real_, 0.26025152), tolerance = 1e-05)
	expect_equal(results5$repeatedPValues[2, ], c(NA_real_, NA_real_))
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results6 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results6' with expected results
	expect_equal(results6$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results6$piTreatments[2, ], NA_real_)
	expect_equal(results6$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[1, ], c(0.13434137, 0.80112393, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99558173, 0.99935678), tolerance = 1e-05)
	expect_equal(results6$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.62349543, -0.55900232, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.51524969, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[1, ], c(0.080410537, -0.10884751, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedConfidenceIntervalUpperBounds[2, ], c(0.16732347, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[1, ], c(0.10960848, 0.00033097065, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results6$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results7 <- getAnalysisResults(design = design1, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmInverseNormal object 'results7' with expected results
	expect_equal(results7$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results7$piTreatments[2, ], NA_real_)
	expect_equal(results7$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[1, ], c(0.13739667, 0.80531488, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalRejectionProbabilities[2, ], c(0.086909033, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[1, ], c(NA_real_, NA_real_, 0.99579217, 0.99938978), tolerance = 1e-05)
	expect_equal(results7$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.6226769, -0.55784932, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.5143226, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[1, ], c(0.079006881, -0.11253618, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedConfidenceIntervalUpperBounds[2, ], c(0.1659763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[1, ], c(0.10337051, 0.00031285088, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results7$repeatedPValues[2, ], c(0.30001108, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results8 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Simes", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results8' with expected results
	expect_equal(results8$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results8$piTreatments[2, ], NA_real_)
	expect_equal(results8$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[1, ], c(0.10173644, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.58125932, -0.55861966, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.46821261, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[1, ], c(0.011590857, -0.11157179, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedConfidenceIntervalUpperBounds[2, ], c(0.10089066, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[1, ], c(0.024755475, 0.00046257745, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results8$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results8$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results9 <- getAnalysisResults(design = design2, dataInput = dataExample2,
								intersectionTest = "Dunnett", nPlanned = c(20, 20), seed = 123, iterations = 1000, normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsMultiArmFisher object 'results9' with expected results
	expect_equal(results9$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results9$piTreatments[2, ], NA_real_)
	expect_equal(results9$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[1, ], c(0.10565624, 1, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalRejectionProbabilities[2, ], c(0.053203298, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[1, ], c(-0.57948552, -0.55733034, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalLowerBounds[2, ], c(-0.4662704, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[1, ], c(0.0088609184, -0.11474637, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedConfidenceIntervalUpperBounds[2, ], c(0.098238963, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[1, ], c(0.023456573, 0.000443504, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$repeatedPValues[2, ], c(0.061679763, NA_real_, NA_real_, NA_real_), tolerance = 1e-05)
	expect_equal(results9$conditionalPowerSimulated[1, ], c(NA_real_, NA_real_, 1, 1))
	expect_equal(results9$conditionalPowerSimulated[2, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
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
	# @refFS[Formula]{fs:testStatisticMultiArmRates}
	results10 <- getAnalysisResults(design = design3, dataInput = dataExample2,
								intersectionTest = "Dunnett", normalApproximation = TRUE, directionUpper = FALSE)

	## Comparison of the results of AnalysisResultsConditionalDunnett object 'results10' with expected results
	expect_equal(results10$piTreatments[1, ], 0.5625, tolerance = 1e-05)
	expect_equal(results10$piTreatments[2, ], NA_real_)
	expect_equal(results10$piControl[1, ], 0.90196078, tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[1, ], c(NA_real_, 0.21935683), tolerance = 1e-05)
	expect_equal(results10$conditionalRejectionProbabilities[2, ], c(NA_real_, 0.13026808), tolerance = 1e-05)
	expect_equal(results10$conditionalPower[1, ], c(NA_real_, NA_real_))
	expect_equal(results10$conditionalPower[2, ], c(NA_real_, NA_real_))
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[1, ], c(NA_real_, -0.46994305), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalLowerBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[1, ], c(NA_real_, -0.15490055), tolerance = 1e-05)
	expect_equal(results10$repeatedConfidenceIntervalUpperBounds[2, ], c(NA_real_, NA_real_))
	expect_equal(results10$repeatedPValues[1, ], c(NA_real_, 7.2525431e-05), tolerance = 1e-05)
	expect_equal(results10$repeatedPValues[2, ], c(NA_real_, NA_real_))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results10), NA)))
	    expect_output(print(results10)$show())
	    invisible(capture.output(expect_error(summary(results10), NA)))
	    expect_output(summary(results10)$show())
	}

})
