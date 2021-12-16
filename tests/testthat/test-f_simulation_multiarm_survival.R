## |  
## |  *Unit tests*
## |  
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |  
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |  
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |  
## |  Contact us for information about our services: info@rpact.com
## |  
## |  File name: test-f_simulation_multiarm_survival.R
## |  Creation date: 08 December 2021, 09:10:05
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

context("Testing Simulation Multi-Arm Survival Function")


test_that("'getSimulationMultiArmSurvival': several configurations", {
	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetBonferroni}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	# @refFS[Formula]{fs:adjustedPValueSubsetHierarchical}
	# @refFS[Formula]{fs:adjustedPValueSubsetSidak}
	# @refFS[Formula]{fs:adjustedPValueSubsetSimes}
	x1 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x1' with expected results
	expect_equal(x1$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x1$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x1$iterations[3, ], c(10, 10, 9, 9))
	expect_equal(x1$rejectAtLeastOne, c(0, 0.1, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x1$futilityStop, c(0, 0, 0, 0))
	expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x1$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x1$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x1$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x1$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x1$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x1$successPerStage[3, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x1$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x1$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x1$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x1$expectedNumberOfEvents, c(182.68801, 153.5825, 114.70922, 140.61265), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$eventsPerStage)), c(4, 35.860669, 73.075204, 3.7272727, 24.561041, 57.244387, 3.5, 20.751077, 42.303464, 3.3076923, 22.100961, 49.222492, 4, 35.860669, 73.075204, 3.8181818, 25.16009, 58.640591, 3.6666667, 21.739223, 44.317915, 3.5384615, 23.642889, 52.65662, 4, 35.860669, 73.075204, 3.9090909, 25.75914, 60.036796, 3.8333333, 22.72737, 46.332366, 3.7692308, 25.184816, 56.090747, 4, 35.860669, 73.075204, 4, 26.35819, 61.433001, 4, 23.715516, 48.346816, 4, 26.726744, 59.524874), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$singleNumberOfEventsPerStage)), c(2, 15.930334, 18.607268, 1.9090909, 10.670954, 16.74025, 1.8333333, 9.0362783, 11.289346, 1.7692308, 10.052214, 14.506865, 2, 15.930334, 18.607268, 2, 11.179095, 17.537405, 2, 9.8577582, 12.31565, 2, 11.363372, 16.399065, 2, 15.930334, 18.607268, 2.0909091, 11.687236, 18.33456, 2.1666667, 10.679238, 13.341954, 2.2307692, 12.67453, 18.291265, 2, 15.930334, 18.607268, 2.1818182, 12.195376, 19.131715, 2.3333333, 11.500718, 14.368258, 2.4615385, 13.985688, 20.183465, 2, 15.930334, 18.607268, 1.8181818, 10.162814, 15.943096, 1.6666667, 8.2147985, 10.263042, 1.5384615, 8.7410553, 12.614666), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(5.8245202e-05, 0.033918251, 0.017570415, 0.062651459), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.081443645, 0.17714318, 0.49831, 0.30622362), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$iterations, x1$iterations, tolerance = 1e-05)
	    expect_equal(x1CodeBased$rejectAtLeastOne, x1$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x1CodeBased$rejectedArmsPerStage, x1$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$futilityStop, x1$futilityStop, tolerance = 1e-05)
	    expect_equal(x1CodeBased$futilityPerStage, x1$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$earlyStop, x1$earlyStop, tolerance = 1e-05)
	    expect_equal(x1CodeBased$successPerStage, x1$successPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$selectedArms, x1$selectedArms, tolerance = 1e-05)
	    expect_equal(x1CodeBased$numberOfActiveArms, x1$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x1CodeBased$expectedNumberOfEvents, x1$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x1CodeBased$eventsPerStage, x1$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$singleNumberOfEventsPerStage, x1$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x1CodeBased$conditionalPowerAchieved, x1$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x2 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms =  4, 
		plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x2' with expected results
	expect_equal(x2$iterations[1, ], c(10, 10))
	expect_equal(x2$iterations[2, ], c(10, 10))
	expect_equal(x2$iterations[3, ], c(3, 9))
	expect_equal(x2$rejectAtLeastOne, c(0, 0))
	expect_equal(unlist(as.list(x2$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x2$futilityStop, c(0.7, 0.1), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0, 0))
	expect_equal(x2$futilityPerStage[2, ], c(0.7, 0.1), tolerance = 1e-07)
	expect_equal(x2$earlyStop[1, ], c(0, 0))
	expect_equal(x2$earlyStop[2, ], c(0.7, 0.1), tolerance = 1e-07)
	expect_equal(x2$successPerStage[1, ], c(0, 0))
	expect_equal(x2$successPerStage[2, ], c(0, 0))
	expect_equal(x2$successPerStage[3, ], c(0, 0))
	expect_equal(unlist(as.list(x2$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.4, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.1, 1, 0.1, 0, 1, 0.2, 0.1, 1, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x2$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x2$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x2$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(x2$expectedNumberOfEvents, c(140, 189.47868), tolerance = 1e-07)
	expect_equal(unlist(as.list(x2$eventsPerStage)), c(5.5, 60.5, 115.5, 5, 49.739341, 99.739341, 6.5, 71.5, 136.5, 5.8333333, 58.029231, 116.36256, 6, 66, 126, 5.4166667, 53.884286, 108.05095, 7, 77, 147, 6.25, 62.174176, 124.67418), tolerance = 1e-07)
	expect_equal(unlist(as.list(x2$singleNumberOfEventsPerStage)), c(0.5, 5, 5, 0.83333333, 7.4565568, 8.3333333, 1.5, 15, 15, 1.6666667, 14.913114, 16.666667, 1, 10, 10, 1.25, 11.184835, 12.5, 2, 20, 20, 2.0833333, 18.641392, 20.833333, 5, 50, 50, 4.1666667, 37.282784, 41.666667), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0, 1.5253195e-09), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(0, 1.1842379e-15), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$iterations, x2$iterations, tolerance = 1e-05)
	    expect_equal(x2CodeBased$rejectAtLeastOne, x2$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x2CodeBased$rejectedArmsPerStage, x2$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityStop, x2$futilityStop, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityPerStage, x2$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$earlyStop, x2$earlyStop, tolerance = 1e-05)
	    expect_equal(x2CodeBased$successPerStage, x2$successPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$selectedArms, x2$selectedArms, tolerance = 1e-05)
	    expect_equal(x2CodeBased$numberOfActiveArms, x2$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x2CodeBased$expectedNumberOfEvents, x2$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x2CodeBased$eventsPerStage, x2$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$singleNumberOfEventsPerStage, x2$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x2CodeBased$conditionalPowerAchieved, x2$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	.skipTestIfDisabled()

	x3 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x3' with expected results
	expect_equal(x3$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x3$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x3$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(x3$rejectAtLeastOne, c(0, 0.1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x3$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x3$futilityStop, c(0, 0, 0, 0))
	expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x3$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x3$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x3$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x3$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x3$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x3$successPerStage[3, ], c(0, 0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x3$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x3$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x3$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x3$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x3$expectedNumberOfEvents, c(182.68801, 158.69386, 129.88152, 143.2193), tolerance = 1e-07)
	expect_equal(unlist(as.list(x3$eventsPerStage)), c(4, 35.860669, 73.075204, 3.8499139, 25.950748, 61.095771, 3.7209785, 22.452769, 48.328635, 3.6090171, 24.278187, 54.733634, 4, 35.860669, 73.075204, 3.8816273, 26.164515, 61.599044, 3.7799362, 22.808526, 49.094387, 3.6916324, 24.833947, 55.986561, 4, 35.860669, 73.075204, 3.9002999, 26.29038, 61.895366, 3.8146499, 23.017992, 49.545254, 3.7402755, 25.161174, 56.724273, 4, 35.860669, 73.075204, 3.9133408, 26.378283, 62.102317, 3.8388939, 23.164282, 49.860138, 3.7742477, 25.389708, 57.239488), tolerance = 1e-07)
	expect_equal(unlist(as.list(x3$singleNumberOfEventsPerStage)), c(2, 15.930334, 18.607268, 2.0015199, 11.489935, 18.271439, 2.0028257, 10.082432, 13.927748, 2.0039595, 11.476859, 16.910832, 2, 15.930334, 18.607268, 2.0332334, 11.671989, 18.560944, 2.0617834, 10.379231, 14.337743, 2.0865748, 11.950004, 17.607999, 2, 15.930334, 18.607268, 2.0519059, 11.779181, 18.731401, 2.0964971, 10.553983, 14.579144, 2.135218, 12.228588, 18.018484, 2, 15.930334, 18.607268, 2.0649468, 11.854043, 18.850449, 2.120741, 10.67603, 14.747737, 2.1691901, 12.423149, 18.305166, 2, 15.930334, 18.607268, 1.848394, 10.610899, 16.873585, 1.7181528, 8.6493588, 11.948119, 1.6050576, 9.1923108, 13.544615), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(5.8245202e-05, 0.027881828, 0.017394693, 0.05621525), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.081443645, 0.17047212, 0.40326875, 0.20898924), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$iterations, x3$iterations, tolerance = 1e-05)
	    expect_equal(x3CodeBased$rejectAtLeastOne, x3$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x3CodeBased$rejectedArmsPerStage, x3$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityStop, x3$futilityStop, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityPerStage, x3$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$earlyStop, x3$earlyStop, tolerance = 1e-05)
	    expect_equal(x3CodeBased$successPerStage, x3$successPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$selectedArms, x3$selectedArms, tolerance = 1e-05)
	    expect_equal(x3CodeBased$numberOfActiveArms, x3$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x3CodeBased$expectedNumberOfEvents, x3$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x3CodeBased$eventsPerStage, x3$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$singleNumberOfEventsPerStage, x3$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x3CodeBased$conditionalPowerAchieved, x3$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x4 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x4' with expected results
	expect_equal(x4$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x4$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x4$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(x4$rejectAtLeastOne, c(0, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x4$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x4$futilityStop, c(0, 0, 0, 0))
	expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x4$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x4$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x4$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x4$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x4$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x4$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x4$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(x4$expectedNumberOfEvents, c(209.51335, 210, 205.68884, 195.53918), tolerance = 1e-07)
	expect_equal(unlist(as.list(x4$eventsPerStage)), c(4, 43.80534, 83.80534, 3.7272727, 41, 78.272727, 3.5, 36.991095, 71.991095, 3.3076923, 31.601422, 64.678345, 4, 43.80534, 83.80534, 3.8181818, 42, 80.181818, 3.6666667, 38.752575, 75.419242, 3.5384615, 33.806172, 69.190787, 4, 43.80534, 83.80534, 3.9090909, 43, 82.090909, 3.8333333, 40.514056, 78.847389, 3.7692308, 36.010922, 73.70323, 4, 43.80534, 83.80534, 4, 44, 84, 4, 42.275537, 82.275537, 4, 38.215673, 78.215673), tolerance = 1e-07)
	expect_equal(unlist(as.list(x4$singleNumberOfEventsPerStage)), c(2, 19.90267, 20, 1.9090909, 19.090909, 19.090909, 1.8333333, 17.542954, 18.333333, 1.7692308, 15.133855, 17.692308, 2, 19.90267, 20, 2, 20, 20, 2, 19.137768, 20, 2, 17.107836, 20, 2, 19.90267, 20, 2.0909091, 20.909091, 20.909091, 2.1666667, 20.732582, 21.666667, 2.2307692, 19.081818, 22.307692, 2, 19.90267, 20, 2.1818182, 21.818182, 21.818182, 2.3333333, 22.327396, 23.333333, 2.4615385, 21.055799, 24.615385, 2, 19.90267, 20, 1.8181818, 18.181818, 18.181818, 1.6666667, 15.94814, 16.666667, 1.5384615, 13.159874, 15.384615), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.09225544, 0.10755451, 0.080008195, 0.16137979), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.011907723, 0.030096405, 0.063317228, 0.080810126), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$iterations, x4$iterations, tolerance = 1e-05)
	    expect_equal(x4CodeBased$rejectAtLeastOne, x4$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x4CodeBased$rejectedArmsPerStage, x4$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$futilityStop, x4$futilityStop, tolerance = 1e-05)
	    expect_equal(x4CodeBased$futilityPerStage, x4$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$earlyStop, x4$earlyStop, tolerance = 1e-05)
	    expect_equal(x4CodeBased$successPerStage, x4$successPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$selectedArms, x4$selectedArms, tolerance = 1e-05)
	    expect_equal(x4CodeBased$numberOfActiveArms, x4$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x4CodeBased$expectedNumberOfEvents, x4$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x4CodeBased$eventsPerStage, x4$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$singleNumberOfEventsPerStage, x4$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x4CodeBased$conditionalPowerAchieved, x4$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x5 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rBest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x5' with expected results
	expect_equal(x5$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x5$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x5$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(x5$rejectAtLeastOne, c(0.1, 0, 0.2, 0.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.4), tolerance = 1e-07)
	expect_equal(x5$futilityStop, c(0, 0, 0, 0))
	expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x5$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x5$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x5$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x5$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x5$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x5$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(x5$expectedNumberOfEvents, c(181.7241, 185.49972, 161.03264, 167.26743), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$eventsPerStage)), c(4, 37.648344, 72.689642, 3.7272727, 34.12598, 69.140806, 3.5, 24.454168, 56.361423, 3.3076923, 29.564881, 55.326921, 4, 37.648344, 72.689642, 3.8181818, 34.958321, 70.827167, 3.6666667, 25.618652, 59.0453, 3.5384615, 31.627547, 59.186938, 4, 37.648344, 72.689642, 3.9090909, 35.790662, 72.513528, 3.8333333, 26.783137, 61.729177, 3.7692308, 33.690213, 63.046956, 4, 37.648344, 72.689642, 4, 36.623003, 74.199889, 4, 27.947621, 64.413055, 4, 35.75288, 66.906974), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$singleNumberOfEventsPerStage)), c(2, 16.824172, 17.520649, 1.9090909, 15.57007, 17.934423, 1.8333333, 10.975993, 16.713324, 1.7692308, 14.044543, 13.779695, 2, 16.824172, 17.520649, 2, 16.311501, 18.788443, 2, 11.97381, 18.232717, 2, 15.87644, 15.577047, 2, 16.824172, 17.520649, 2.0909091, 17.052933, 19.642463, 2.1666667, 12.971628, 19.75211, 2.2307692, 17.708337, 17.374399, 2, 16.824172, 17.520649, 2.1818182, 17.794365, 20.496483, 2.3333333, 13.969446, 21.271503, 2.4615385, 19.540234, 19.17175, 2, 16.824172, 17.520649, 1.8181818, 14.828638, 17.080403, 1.6666667, 9.9781754, 15.193931, 1.5384615, 12.212646, 11.982344), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.0011884888, 0.025687618, 0.050936222, 0.056920177), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.16000064, 0.17717891, 0.25226702, 0.41435883), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$iterations, x5$iterations, tolerance = 1e-05)
	    expect_equal(x5CodeBased$rejectAtLeastOne, x5$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x5CodeBased$rejectedArmsPerStage, x5$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$futilityStop, x5$futilityStop, tolerance = 1e-05)
	    expect_equal(x5CodeBased$futilityPerStage, x5$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$earlyStop, x5$earlyStop, tolerance = 1e-05)
	    expect_equal(x5CodeBased$successPerStage, x5$successPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$selectedArms, x5$selectedArms, tolerance = 1e-05)
	    expect_equal(x5CodeBased$numberOfActiveArms, x5$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x5CodeBased$expectedNumberOfEvents, x5$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x5CodeBased$eventsPerStage, x5$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$singleNumberOfEventsPerStage, x5$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x5CodeBased$conditionalPowerAchieved, x5$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x6 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x6' with expected results
	expect_equal(x6$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x6$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x6$iterations[3, ], c(10, 9, 9, 10))
	expect_equal(x6$rejectAtLeastOne, c(0, 0.3, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.3, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x6$futilityStop, c(0, 0, 0, 0))
	expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x6$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x6$earlyStop[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x6$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x6$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x6$successPerStage[3, ], c(0, 0.2, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$selectedArms)), c(1, 0.5, 0.5, 1, 0.3, 0, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.5, 0.5, 1, 0.2, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x6$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x6$numberOfActiveArms[2, ], c(1.1, 1.3, 1.1, 1.2), tolerance = 1e-07)
	expect_equal(x6$numberOfActiveArms[3, ], c(1, 1.1111111, 1, 1), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfEvents, c(182.78185, 142.9628, 156.19514, 150.78355), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$eventsPerStage)), c(4, 36.65721, 73.112738, 3.7272727, 24.638721, 56.469181, 3.5, 26.818726, 57.762697, 3.3076923, 22.27063, 49.87456, 4, 36.65721, 73.112738, 3.8181818, 25.239665, 57.846478, 3.6666667, 28.095809, 60.513301, 3.5384615, 23.824395, 53.354181, 4, 36.65721, 73.112738, 3.9090909, 25.84061, 59.223775, 3.8333333, 29.372891, 63.263906, 3.7692308, 25.37816, 56.833801, 4, 36.65721, 73.112738, 4, 26.441554, 60.601072, 4, 30.649973, 66.01451, 4, 26.931924, 60.313422), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$singleNumberOfEventsPerStage)), c(2, 16.328605, 18.227764, 1.9090909, 10.710742, 16.303406, 1.8333333, 12.214571, 16.208746, 1.7692308, 10.142967, 14.764893, 2, 16.328605, 18.227764, 2, 11.220777, 17.079759, 2, 13.324987, 17.682269, 2, 11.465962, 16.690749, 2, 16.328605, 18.227764, 2.0909091, 11.730812, 17.856112, 2.1666667, 14.435402, 19.155791, 2.2307692, 12.788958, 18.616604, 2, 16.328605, 18.227764, 2.1818182, 12.240848, 18.632464, 2.3333333, 15.545818, 20.629313, 2.4615385, 14.111953, 20.54246, 2, 16.328605, 18.227764, 1.8181818, 10.200706, 15.527054, 1.6666667, 11.104155, 14.735224, 1.5384615, 8.8199709, 12.839037), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.018816179, 0.071905821, 0.002298516, 0.067085771), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.080015186, 0.29125387, 0.18887123, 0.4033636), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$iterations, x6$iterations, tolerance = 1e-05)
	    expect_equal(x6CodeBased$rejectAtLeastOne, x6$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x6CodeBased$rejectedArmsPerStage, x6$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityStop, x6$futilityStop, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityPerStage, x6$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$earlyStop, x6$earlyStop, tolerance = 1e-05)
	    expect_equal(x6CodeBased$successPerStage, x6$successPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$selectedArms, x6$selectedArms, tolerance = 1e-05)
	    expect_equal(x6CodeBased$numberOfActiveArms, x6$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x6CodeBased$expectedNumberOfEvents, x6$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x6CodeBased$eventsPerStage, x6$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$singleNumberOfEventsPerStage, x6$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x6CodeBased$conditionalPowerAchieved, x6$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x7 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x7' with expected results
	expect_equal(x7$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x7$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x7$iterations[3, ], c(10, 10, 9, 9))
	expect_equal(x7$rejectAtLeastOne, c(0, 0.1, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x7$futilityStop, c(0, 0, 0, 0))
	expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x7$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x7$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x7$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x7$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x7$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x7$successPerStage[3, ], c(0, 0.1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x7$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x7$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x7$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x7$expectedNumberOfEvents, c(169.30334, 121.79095, 98.577582, 123.23372), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$eventsPerStage)), c(4, 35.860669, 67.721337, 3.7272727, 24.561041, 45.394809, 3.5, 20.751077, 36.030051, 3.3076923, 22.100961, 42.835362, 4, 35.860669, 67.721337, 3.8181818, 25.16009, 46.501999, 3.6666667, 21.739223, 37.745768, 3.5384615, 23.642889, 45.823876, 4, 35.860669, 67.721337, 3.9090909, 25.75914, 47.60919, 3.8333333, 22.72737, 39.461484, 3.7692308, 25.184816, 48.81239, 4, 35.860669, 67.721337, 4, 26.35819, 48.71638, 4, 23.715516, 41.177201, 4, 26.726744, 51.800903), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$singleNumberOfEventsPerStage)), c(2, 15.930334, 15.930334, 1.9090909, 10.670954, 10.670954, 1.8333333, 9.0362783, 8.0032722, 1.7692308, 10.052214, 11.090494, 2, 15.930334, 15.930334, 2, 11.179095, 11.179095, 2, 9.8577582, 8.7308424, 2, 11.363372, 12.53708, 2, 15.930334, 15.930334, 2.0909091, 11.687236, 11.687236, 2.1666667, 10.679238, 9.4584126, 2.2307692, 12.67453, 13.983666, 2, 15.930334, 15.930334, 2.1818182, 12.195376, 12.195376, 2.3333333, 11.500718, 10.185983, 2.4615385, 13.985688, 15.430252, 2, 15.930334, 15.930334, 1.8181818, 10.162814, 10.162814, 1.6666667, 8.2147985, 7.275702, 1.5384615, 8.7410553, 9.6439076), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(5.8245202e-05, 0.033918251, 0.017570415, 0.062651459), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[3, ], c(0.075858531, 0.086024261, 0.37522404, 0.19729909), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$iterations, x7$iterations, tolerance = 1e-05)
	    expect_equal(x7CodeBased$rejectAtLeastOne, x7$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x7CodeBased$rejectedArmsPerStage, x7$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$futilityStop, x7$futilityStop, tolerance = 1e-05)
	    expect_equal(x7CodeBased$futilityPerStage, x7$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$earlyStop, x7$earlyStop, tolerance = 1e-05)
	    expect_equal(x7CodeBased$successPerStage, x7$successPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$selectedArms, x7$selectedArms, tolerance = 1e-05)
	    expect_equal(x7CodeBased$numberOfActiveArms, x7$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x7CodeBased$expectedNumberOfEvents, x7$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x7CodeBased$eventsPerStage, x7$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$singleNumberOfEventsPerStage, x7$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x7CodeBased$conditionalPowerAchieved, x7$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x7), "character")
	    df <- as.data.frame(x7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x8 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x8' with expected results
	expect_equal(x8$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x8$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x8$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(x8$rejectAtLeastOne, c(0, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x8$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x8$futilityStop, c(0, 0, 0, 0))
	expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x8$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x8$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x8$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x8$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x8$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x8$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(x8$expectedNumberOfEvents, c(209.0267, 210, 201.37768, 181.07836), tolerance = 1e-07)
	expect_equal(unlist(as.list(x8$eventsPerStage)), c(4, 43.80534, 83.61068, 3.7272727, 41, 78.272727, 3.5, 36.991095, 70.482189, 3.3076923, 31.601422, 59.895151, 4, 43.80534, 83.61068, 3.8181818, 42, 80.181818, 3.6666667, 38.752575, 73.838484, 3.5384615, 33.806172, 64.073883, 4, 43.80534, 83.61068, 3.9090909, 43, 82.090909, 3.8333333, 40.514056, 77.194778, 3.7692308, 36.010922, 68.252614, 4, 43.80534, 83.61068, 4, 44, 84, 4, 42.275537, 80.551073, 4, 38.215673, 72.431346), tolerance = 1e-07)
	expect_equal(unlist(as.list(x8$singleNumberOfEventsPerStage)), c(2, 19.90267, 19.90267, 1.9090909, 19.090909, 19.090909, 1.8333333, 17.542954, 17.542954, 1.7692308, 15.133855, 15.133855, 2, 19.90267, 19.90267, 2, 20, 20, 2, 19.137768, 19.137768, 2, 17.107836, 17.107836, 2, 19.90267, 19.90267, 2.0909091, 20.909091, 20.909091, 2.1666667, 20.732582, 20.732582, 2.2307692, 19.081818, 19.081818, 2, 19.90267, 19.90267, 2.1818182, 21.818182, 21.818182, 2.3333333, 22.327396, 22.327396, 2.4615385, 21.055799, 21.055799, 2, 19.90267, 19.90267, 1.8181818, 18.181818, 18.181818, 1.6666667, 15.94814, 15.94814, 1.5384615, 13.159874, 13.159874), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.09225544, 0.10755451, 0.080008195, 0.16137979), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(0.011968708, 0.030096405, 0.063317862, 0.066369104), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$iterations, x8$iterations, tolerance = 1e-05)
	    expect_equal(x8CodeBased$rejectAtLeastOne, x8$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x8CodeBased$rejectedArmsPerStage, x8$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$futilityStop, x8$futilityStop, tolerance = 1e-05)
	    expect_equal(x8CodeBased$futilityPerStage, x8$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$earlyStop, x8$earlyStop, tolerance = 1e-05)
	    expect_equal(x8CodeBased$successPerStage, x8$successPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$selectedArms, x8$selectedArms, tolerance = 1e-05)
	    expect_equal(x8CodeBased$numberOfActiveArms, x8$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x8CodeBased$expectedNumberOfEvents, x8$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x8CodeBased$eventsPerStage, x8$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$singleNumberOfEventsPerStage, x8$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x8CodeBased$conditionalPowerAchieved, x8$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x8), "character")
	    df <- as.data.frame(x8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x9 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rBest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x9' with expected results
	expect_equal(x9$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x9$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x9$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(x9$rejectAtLeastOne, c(0.1, 0, 0.2, 0.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.4), tolerance = 1e-07)
	expect_equal(x9$futilityStop, c(0, 0, 0, 0))
	expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x9$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x9$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x9$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x9$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x9$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x9$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(x9$expectedNumberOfEvents, c(178.24172, 173.11501, 129.7381, 168.7644), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$eventsPerStage)), c(4, 37.648344, 71.296689, 3.7272727, 34.12598, 64.524687, 3.5, 24.454168, 45.408337, 3.3076923, 29.564881, 55.82207, 4, 37.648344, 71.296689, 3.8181818, 34.958321, 66.09846, 3.6666667, 25.618652, 47.570638, 3.5384615, 31.627547, 59.716633, 4, 37.648344, 71.296689, 3.9090909, 35.790662, 67.672233, 3.8333333, 26.783137, 49.73294, 3.7692308, 33.690213, 63.611196, 4, 37.648344, 71.296689, 4, 36.623003, 69.246006, 4, 27.947621, 51.895242, 4, 35.75288, 67.505759), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$singleNumberOfEventsPerStage)), c(2, 16.824172, 16.824172, 1.9090909, 15.57007, 15.57007, 1.8333333, 10.975993, 10.975993, 1.7692308, 14.044543, 14.044543, 2, 16.824172, 16.824172, 2, 16.311501, 16.311501, 2, 11.97381, 11.97381, 2, 15.87644, 15.87644, 2, 16.824172, 16.824172, 2.0909091, 17.052933, 17.052933, 2.1666667, 12.971628, 12.971628, 2.2307692, 17.708337, 17.708337, 2, 16.824172, 16.824172, 2.1818182, 17.794365, 17.794365, 2.3333333, 13.969446, 13.969446, 2.4615385, 19.540234, 19.540234, 2, 16.824172, 16.824172, 1.8181818, 14.828638, 14.828638, 1.6666667, 9.9781754, 9.9781754, 1.5384615, 12.212646, 12.212646), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.0011884888, 0.025687618, 0.050936222, 0.056920177), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[3, ], c(0.13630501, 0.14441052, 0.13257023, 0.41932885), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x9), NA)))
	    expect_output(print(x9)$show())
	    invisible(capture.output(expect_error(summary(x9), NA)))
	    expect_output(summary(x9)$show())
	    x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
	    expect_equal(x9CodeBased$iterations, x9$iterations, tolerance = 1e-05)
	    expect_equal(x9CodeBased$rejectAtLeastOne, x9$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x9CodeBased$rejectedArmsPerStage, x9$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$futilityStop, x9$futilityStop, tolerance = 1e-05)
	    expect_equal(x9CodeBased$futilityPerStage, x9$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$earlyStop, x9$earlyStop, tolerance = 1e-05)
	    expect_equal(x9CodeBased$successPerStage, x9$successPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$selectedArms, x9$selectedArms, tolerance = 1e-05)
	    expect_equal(x9CodeBased$numberOfActiveArms, x9$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x9CodeBased$expectedNumberOfEvents, x9$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x9CodeBased$eventsPerStage, x9$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$singleNumberOfEventsPerStage, x9$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x9CodeBased$conditionalPowerAchieved, x9$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x9), "character")
	    df <- as.data.frame(x9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x10 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Hierarchical",  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x10' with expected results
	expect_equal(x10$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x10$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x10$iterations[3, ], c(6, 3, 2, 1))
	expect_equal(x10$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x10$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x10$futilityStop, c(0.4, 0.7, 0.8, 0.9), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$futilityPerStage[2, ], c(0.4, 0.7, 0.8, 0.9), tolerance = 1e-07)
	expect_equal(x10$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x10$earlyStop[2, ], c(0.4, 0.7, 0.8, 0.9), tolerance = 1e-07)
	expect_equal(x10$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x10$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x10$selectedArms)), c(1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.5, 0.4, 1, 0.1, 0, 1, 0.4, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.4, 0, 1, 0.3, 0, 1, 0.3, 0, 1, 0.2, 0.1, 1, 0.4, 0.1, 1, 0.4, 0, 1, 0.5, 0), tolerance = 1e-07)
	expect_equal(x10$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x10$numberOfActiveArms[2, ], c(1.5, 1.2, 1.3, 1), tolerance = 1e-07)
	expect_equal(x10$numberOfActiveArms[3, ], c(1.8333333, 1.3333333, 1, 1), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfEvents, c(148.64919, 116.07216, 81.180483, 62.574824), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$eventsPerStage)), c(4, 36.228838, 74.946902, 3.7272727, 32.081441, 69.354168, 3.5, 24.722719, 43.17497, 3.3076923, 20.210056, 25.087756, 4, 36.228838, 74.946902, 3.8181818, 32.863915, 71.045733, 3.6666667, 25.899991, 45.230921, 3.5384615, 21.62006, 26.838065, 4, 36.228838, 74.946902, 3.9090909, 33.646389, 72.737298, 3.8333333, 27.077263, 47.286871, 3.7692308, 23.030064, 28.588373, 4, 36.228838, 74.946902, 4, 34.428863, 74.428863, 4, 28.254536, 49.342822, 4, 24.440068, 30.338682), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$singleNumberOfEventsPerStage)), c(2, 16.114419, 19.359032, 1.9090909, 14.522866, 19.090909, 1.8333333, 11.116662, 9.6654647, 1.7692308, 9.0407994, 2.6090022, 2, 16.114419, 19.359032, 2, 15.214432, 20, 2, 12.127268, 10.544143, 2, 10.220034, 2.9493068, 2, 16.114419, 19.359032, 2.0909091, 15.905997, 20.909091, 2.1666667, 13.137874, 11.422822, 2.2307692, 11.399269, 3.2896115, 2, 16.114419, 19.359032, 2.1818182, 16.597562, 21.818182, 2.3333333, 14.148479, 12.301501, 2.4615385, 12.578503, 3.6299161, 2, 16.114419, 19.359032, 1.8181818, 13.831301, 18.181818, 1.6666667, 10.106057, 8.7867861, 1.5384615, 7.8615647, 2.2686976), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.0031444794, 0.00037604601, 0.038145414, 0.045847923), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[3, ], c(7.9302274e-08, 1.361166e-06, 0.16667791, 0.040805908), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x10), NA)))
	    expect_output(print(x10)$show())
	    invisible(capture.output(expect_error(summary(x10), NA)))
	    expect_output(summary(x10)$show())
	    x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
	    expect_equal(x10CodeBased$iterations, x10$iterations, tolerance = 1e-05)
	    expect_equal(x10CodeBased$rejectAtLeastOne, x10$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x10CodeBased$rejectedArmsPerStage, x10$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityStop, x10$futilityStop, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityPerStage, x10$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$earlyStop, x10$earlyStop, tolerance = 1e-05)
	    expect_equal(x10CodeBased$successPerStage, x10$successPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$selectedArms, x10$selectedArms, tolerance = 1e-05)
	    expect_equal(x10CodeBased$numberOfActiveArms, x10$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x10CodeBased$expectedNumberOfEvents, x10$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x10CodeBased$eventsPerStage, x10$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$singleNumberOfEventsPerStage, x10$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x10CodeBased$conditionalPowerAchieved, x10$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x10), "character")
	    df <- as.data.frame(x10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x11 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Hierarchical",  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x11' with expected results
	expect_equal(x11$iterations[1, ], c(10, 10, 10))
	expect_equal(x11$iterations[2, ], c(0, 0, 0))
	expect_equal(x11$iterations[3, ], c(0, 0, 0))
	expect_equal(x11$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(unlist(as.list(x11$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x11$futilityStop, c(1, 1, 1))
	expect_equal(x11$futilityPerStage[1, ], c(1, 1, 1))
	expect_equal(x11$futilityPerStage[2, ], c(0, 0, 0))
	expect_equal(x11$earlyStop[1, ], c(1, 1, 1))
	expect_equal(x11$earlyStop[2, ], c(0, 0, 0))
	expect_equal(x11$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x11$successPerStage[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x11$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x11$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x11$numberOfActiveArms[2, ], c(NaN, NaN, NaN))
	expect_equal(x11$numberOfActiveArms[3, ], c(NaN, NaN, NaN))
	expect_equal(x11$expectedNumberOfEvents, c(NaN, NaN, NaN))
	expect_equal(unlist(as.list(x11$eventsPerStage)), c(1.5454545, 1.5454545, 1.5454545, 2, 2, 2, 2.3846154, 2.3846154, 2.3846154, 2.3636364, 2.3636364, 2.3636364, 2.6666667, 2.6666667, 2.6666667, 2.9230769, 2.9230769, 2.9230769, 3.1818182, 3.1818182, 3.1818182, 3.3333333, 3.3333333, 3.3333333, 3.4615385, 3.4615385, 3.4615385, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x11$singleNumberOfEventsPerStage)), c(1.1818182, NaN, NaN, 1.3333333, NaN, NaN, 1.4615385, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2.8181818, NaN, NaN, 2.6666667, NaN, NaN, 2.5384615, NaN, NaN, 3.6363636, NaN, NaN, 3.3333333, NaN, NaN, 3.0769231, NaN, NaN, 0.36363636, NaN, NaN, 0.66666667, NaN, NaN, 0.92307692, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$iterations, x11$iterations, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectAtLeastOne, x11$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x11CodeBased$rejectedArmsPerStage, x11$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityStop, x11$futilityStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityPerStage, x11$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$earlyStop, x11$earlyStop, tolerance = 1e-05)
	    expect_equal(x11CodeBased$successPerStage, x11$successPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$selectedArms, x11$selectedArms, tolerance = 1e-05)
	    expect_equal(x11CodeBased$numberOfActiveArms, x11$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x11CodeBased$expectedNumberOfEvents, x11$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x11CodeBased$eventsPerStage, x11$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x11CodeBased$singleNumberOfEventsPerStage, x11$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x11), "character")
	    df <- as.data.frame(x11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x12 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Hierarchical", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x12' with expected results
	expect_equal(x12$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x12$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x12$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x12$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x12$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x12$futilityStop, c(1, 1, 1, 1))
	expect_equal(x12$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x12$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x12$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x12$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x12$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x12$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x12$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x12$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x12$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x12$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x12$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x12$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x12$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x12$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$iterations, x12$iterations, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectAtLeastOne, x12$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x12CodeBased$rejectedArmsPerStage, x12$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityStop, x12$futilityStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityPerStage, x12$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$earlyStop, x12$earlyStop, tolerance = 1e-05)
	    expect_equal(x12CodeBased$successPerStage, x12$successPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$selectedArms, x12$selectedArms, tolerance = 1e-05)
	    expect_equal(x12CodeBased$numberOfActiveArms, x12$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x12CodeBased$expectedNumberOfEvents, x12$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x12CodeBased$eventsPerStage, x12$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x12CodeBased$singleNumberOfEventsPerStage, x12$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x12), "character")
	    df <- as.data.frame(x12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x13 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined",
		activeArms = 4, directionUpper = FALSE, threshold = 0, 
		plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Sidak", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x13' with expected results
	expect_equal(x13$iterations[1, ], c(10, 10))
	expect_equal(x13$iterations[2, ], c(0, 0))
	expect_equal(x13$iterations[3, ], c(0, 0))
	expect_equal(x13$rejectAtLeastOne, c(0, 0))
	expect_equal(unlist(as.list(x13$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x13$futilityStop, c(1, 1))
	expect_equal(x13$futilityPerStage[1, ], c(1, 1))
	expect_equal(x13$futilityPerStage[2, ], c(0, 0))
	expect_equal(x13$earlyStop[1, ], c(1, 1))
	expect_equal(x13$earlyStop[2, ], c(0, 0))
	expect_equal(x13$successPerStage[1, ], c(0, 0))
	expect_equal(x13$successPerStage[2, ], c(0, 0))
	expect_equal(x13$successPerStage[3, ], c(0, 0))
	expect_equal(unlist(as.list(x13$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x13$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x13$numberOfActiveArms[2, ], c(NaN, NaN))
	expect_equal(x13$numberOfActiveArms[3, ], c(NaN, NaN))
	expect_equal(x13$expectedNumberOfEvents, c(NaN, NaN))
	expect_equal(unlist(as.list(x13$eventsPerStage)), c(5.5, 5.5, 5.5, 5, 5, 5, 6.5, 6.5, 6.5, 5.8333333, 5.8333333, 5.8333333, 6, 6, 6, 5.4166667, 5.4166667, 5.4166667, 7, 7, 7, 6.25, 6.25, 6.25), tolerance = 1e-07)
	expect_equal(unlist(as.list(x13$singleNumberOfEventsPerStage)), c(0.5, NaN, NaN, 0.83333333, NaN, NaN, 1.5, NaN, NaN, 1.6666667, NaN, NaN, 1, NaN, NaN, 1.25, NaN, NaN, 2, NaN, NaN, 2.0833333, NaN, NaN, 5, NaN, NaN, 4.1666667, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x13), NA)))
	    expect_output(print(x13)$show())
	    invisible(capture.output(expect_error(summary(x13), NA)))
	    expect_output(summary(x13)$show())
	    x13CodeBased <- eval(parse(text = getObjectRCode(x13, stringWrapParagraphWidth = NULL)))
	    expect_equal(x13CodeBased$iterations, x13$iterations, tolerance = 1e-05)
	    expect_equal(x13CodeBased$rejectAtLeastOne, x13$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x13CodeBased$rejectedArmsPerStage, x13$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$futilityStop, x13$futilityStop, tolerance = 1e-05)
	    expect_equal(x13CodeBased$futilityPerStage, x13$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$earlyStop, x13$earlyStop, tolerance = 1e-05)
	    expect_equal(x13CodeBased$successPerStage, x13$successPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$selectedArms, x13$selectedArms, tolerance = 1e-05)
	    expect_equal(x13CodeBased$numberOfActiveArms, x13$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x13CodeBased$expectedNumberOfEvents, x13$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x13CodeBased$eventsPerStage, x13$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x13CodeBased$singleNumberOfEventsPerStage, x13$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x13), "character")
	    df <- as.data.frame(x13)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x13)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x14 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Sidak", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x14' with expected results
	expect_equal(x14$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x14$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x14$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x14$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x14$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x14$futilityStop, c(1, 1, 1, 1))
	expect_equal(x14$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x14$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x14$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x14$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x14$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x14$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x14$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x14$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x14$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x14$eventsPerStage)), c(4, 4, 4, 4.1452587, 4.1452587, 4.1452587, 4.2627857, 4.2627857, 4.2627857, 4.3598306, 4.3598306, 4.3598306, 4, 4, 4, 4.1145653, 4.1145653, 4.1145653, 4.2072587, 4.2072587, 4.2072587, 4.2837979, 4.2837979, 4.2837979, 4, 4, 4, 4.0964933, 4.0964933, 4.0964933, 4.1745649, 4.1745649, 4.1745649, 4.2390305, 4.2390305, 4.2390305, 4, 4, 4, 4.0838719, 4.0838719, 4.0838719, 4.1517317, 4.1517317, 4.1517317, 4.2077651, 4.2077651, 4.2077651), tolerance = 1e-07)
	expect_equal(unlist(as.list(x14$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 1.9985289, NaN, NaN, 1.9973387, NaN, NaN, 1.996356, NaN, NaN, 2, NaN, NaN, 1.9678356, NaN, NaN, 1.9418117, NaN, NaN, 1.9203232, NaN, NaN, 2, NaN, NaN, 1.9497636, NaN, NaN, 1.9091179, NaN, NaN, 1.8755558, NaN, NaN, 2, NaN, NaN, 1.9371422, NaN, NaN, 1.8862847, NaN, NaN, 1.8442904, NaN, NaN, 2, NaN, NaN, 2.1467297, NaN, NaN, 2.265447, NaN, NaN, 2.3634747, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x14), NA)))
	    expect_output(print(x14)$show())
	    invisible(capture.output(expect_error(summary(x14), NA)))
	    expect_output(summary(x14)$show())
	    x14CodeBased <- eval(parse(text = getObjectRCode(x14, stringWrapParagraphWidth = NULL)))
	    expect_equal(x14CodeBased$iterations, x14$iterations, tolerance = 1e-05)
	    expect_equal(x14CodeBased$rejectAtLeastOne, x14$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x14CodeBased$rejectedArmsPerStage, x14$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$futilityStop, x14$futilityStop, tolerance = 1e-05)
	    expect_equal(x14CodeBased$futilityPerStage, x14$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$earlyStop, x14$earlyStop, tolerance = 1e-05)
	    expect_equal(x14CodeBased$successPerStage, x14$successPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$selectedArms, x14$selectedArms, tolerance = 1e-05)
	    expect_equal(x14CodeBased$numberOfActiveArms, x14$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x14CodeBased$expectedNumberOfEvents, x14$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x14CodeBased$eventsPerStage, x14$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x14CodeBased$singleNumberOfEventsPerStage, x14$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x14), "character")
	    df <- as.data.frame(x14)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x14)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x15 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Sidak", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x15' with expected results
	expect_equal(x15$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x15$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x15$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x15$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x15$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x15$futilityStop, c(1, 1, 1, 1))
	expect_equal(x15$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x15$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x15$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x15$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x15$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x15$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x15$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x15$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x15$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x15$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x15$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x15), NA)))
	    expect_output(print(x15)$show())
	    invisible(capture.output(expect_error(summary(x15), NA)))
	    expect_output(summary(x15)$show())
	    x15CodeBased <- eval(parse(text = getObjectRCode(x15, stringWrapParagraphWidth = NULL)))
	    expect_equal(x15CodeBased$iterations, x15$iterations, tolerance = 1e-05)
	    expect_equal(x15CodeBased$rejectAtLeastOne, x15$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x15CodeBased$rejectedArmsPerStage, x15$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$futilityStop, x15$futilityStop, tolerance = 1e-05)
	    expect_equal(x15CodeBased$futilityPerStage, x15$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$earlyStop, x15$earlyStop, tolerance = 1e-05)
	    expect_equal(x15CodeBased$successPerStage, x15$successPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$selectedArms, x15$selectedArms, tolerance = 1e-05)
	    expect_equal(x15CodeBased$numberOfActiveArms, x15$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x15CodeBased$expectedNumberOfEvents, x15$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x15CodeBased$eventsPerStage, x15$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x15CodeBased$singleNumberOfEventsPerStage, x15$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x15), "character")
	    df <- as.data.frame(x15)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x15)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x16 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "rBest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Simes",
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x16' with expected results
	expect_equal(x16$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x16$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x16$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x16$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x16$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x16$futilityStop, c(1, 1, 1, 1))
	expect_equal(x16$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x16$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x16$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x16$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x16$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x16$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x16$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x16$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x16$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x16$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x16$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x16), NA)))
	    expect_output(print(x16)$show())
	    invisible(capture.output(expect_error(summary(x16), NA)))
	    expect_output(summary(x16)$show())
	    x16CodeBased <- eval(parse(text = getObjectRCode(x16, stringWrapParagraphWidth = NULL)))
	    expect_equal(x16CodeBased$iterations, x16$iterations, tolerance = 1e-05)
	    expect_equal(x16CodeBased$rejectAtLeastOne, x16$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x16CodeBased$rejectedArmsPerStage, x16$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$futilityStop, x16$futilityStop, tolerance = 1e-05)
	    expect_equal(x16CodeBased$futilityPerStage, x16$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$earlyStop, x16$earlyStop, tolerance = 1e-05)
	    expect_equal(x16CodeBased$successPerStage, x16$successPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$selectedArms, x16$selectedArms, tolerance = 1e-05)
	    expect_equal(x16CodeBased$numberOfActiveArms, x16$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x16CodeBased$expectedNumberOfEvents, x16$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x16CodeBased$eventsPerStage, x16$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x16CodeBased$singleNumberOfEventsPerStage, x16$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x16), "character")
	    df <- as.data.frame(x16)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x16)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x17 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Simes",
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x17' with expected results
	expect_equal(x17$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x17$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x17$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x17$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x17$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x17$futilityStop, c(1, 1, 1, 1))
	expect_equal(x17$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x17$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x17$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x17$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x17$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x17$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x17$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x17$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x17$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x17$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x17$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x17), NA)))
	    expect_output(print(x17)$show())
	    invisible(capture.output(expect_error(summary(x17), NA)))
	    expect_output(summary(x17)$show())
	    x17CodeBased <- eval(parse(text = getObjectRCode(x17, stringWrapParagraphWidth = NULL)))
	    expect_equal(x17CodeBased$iterations, x17$iterations, tolerance = 1e-05)
	    expect_equal(x17CodeBased$rejectAtLeastOne, x17$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x17CodeBased$rejectedArmsPerStage, x17$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$futilityStop, x17$futilityStop, tolerance = 1e-05)
	    expect_equal(x17CodeBased$futilityPerStage, x17$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$earlyStop, x17$earlyStop, tolerance = 1e-05)
	    expect_equal(x17CodeBased$successPerStage, x17$successPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$selectedArms, x17$selectedArms, tolerance = 1e-05)
	    expect_equal(x17CodeBased$numberOfActiveArms, x17$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x17CodeBased$expectedNumberOfEvents, x17$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x17CodeBased$eventsPerStage, x17$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x17CodeBased$singleNumberOfEventsPerStage, x17$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x17), "character")
	    df <- as.data.frame(x17)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x17)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x18 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Simes",
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x18' with expected results
	expect_equal(x18$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x18$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x18$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x18$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x18$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x18$futilityStop, c(1, 1, 1, 1))
	expect_equal(x18$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x18$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x18$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x18$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x18$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x18$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x18$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x18$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x18$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x18$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x18$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x18), NA)))
	    expect_output(print(x18)$show())
	    invisible(capture.output(expect_error(summary(x18), NA)))
	    expect_output(summary(x18)$show())
	    x18CodeBased <- eval(parse(text = getObjectRCode(x18, stringWrapParagraphWidth = NULL)))
	    expect_equal(x18CodeBased$iterations, x18$iterations, tolerance = 1e-05)
	    expect_equal(x18CodeBased$rejectAtLeastOne, x18$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x18CodeBased$rejectedArmsPerStage, x18$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$futilityStop, x18$futilityStop, tolerance = 1e-05)
	    expect_equal(x18CodeBased$futilityPerStage, x18$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$earlyStop, x18$earlyStop, tolerance = 1e-05)
	    expect_equal(x18CodeBased$successPerStage, x18$successPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$selectedArms, x18$selectedArms, tolerance = 1e-05)
	    expect_equal(x18CodeBased$numberOfActiveArms, x18$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x18CodeBased$expectedNumberOfEvents, x18$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x18CodeBased$eventsPerStage, x18$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x18CodeBased$singleNumberOfEventsPerStage, x18$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x18), "character")
	    df <- as.data.frame(x18)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x18)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x19 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "all",
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Bonferroni", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x19' with expected results
	expect_equal(x19$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x19$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x19$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x19$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x19$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x19$futilityStop, c(1, 1, 1, 1))
	expect_equal(x19$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x19$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x19$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x19$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x19$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x19$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x19$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x19$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x19$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x19$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x19$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x19$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x19$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x19$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x19), NA)))
	    expect_output(print(x19)$show())
	    invisible(capture.output(expect_error(summary(x19), NA)))
	    expect_output(summary(x19)$show())
	    x19CodeBased <- eval(parse(text = getObjectRCode(x19, stringWrapParagraphWidth = NULL)))
	    expect_equal(x19CodeBased$iterations, x19$iterations, tolerance = 1e-05)
	    expect_equal(x19CodeBased$rejectAtLeastOne, x19$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x19CodeBased$rejectedArmsPerStage, x19$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$futilityStop, x19$futilityStop, tolerance = 1e-05)
	    expect_equal(x19CodeBased$futilityPerStage, x19$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$earlyStop, x19$earlyStop, tolerance = 1e-05)
	    expect_equal(x19CodeBased$successPerStage, x19$successPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$selectedArms, x19$selectedArms, tolerance = 1e-05)
	    expect_equal(x19CodeBased$numberOfActiveArms, x19$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x19CodeBased$expectedNumberOfEvents, x19$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x19CodeBased$eventsPerStage, x19$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x19CodeBased$singleNumberOfEventsPerStage, x19$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x19), "character")
	    df <- as.data.frame(x19)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x19)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x20 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "rBest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), intersectionTest = "Bonferroni",
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x20' with expected results
	expect_equal(x20$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x20$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x20$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x20$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x20$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x20$futilityStop, c(1, 1, 1, 1))
	expect_equal(x20$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x20$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x20$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x20$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x20$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x20$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x20$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x20$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x20$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x20$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x20$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x20$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x20$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x20$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x20), NA)))
	    expect_output(print(x20)$show())
	    invisible(capture.output(expect_error(summary(x20), NA)))
	    expect_output(summary(x20)$show())
	    x20CodeBased <- eval(parse(text = getObjectRCode(x20, stringWrapParagraphWidth = NULL)))
	    expect_equal(x20CodeBased$iterations, x20$iterations, tolerance = 1e-05)
	    expect_equal(x20CodeBased$rejectAtLeastOne, x20$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x20CodeBased$rejectedArmsPerStage, x20$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$futilityStop, x20$futilityStop, tolerance = 1e-05)
	    expect_equal(x20CodeBased$futilityPerStage, x20$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$earlyStop, x20$earlyStop, tolerance = 1e-05)
	    expect_equal(x20CodeBased$successPerStage, x20$successPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$selectedArms, x20$selectedArms, tolerance = 1e-05)
	    expect_equal(x20CodeBased$numberOfActiveArms, x20$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x20CodeBased$expectedNumberOfEvents, x20$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x20CodeBased$eventsPerStage, x20$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x20CodeBased$singleNumberOfEventsPerStage, x20$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x20), "character")
	    df <- as.data.frame(x20)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x20)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x21 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), intersectionTest = "Bonferroni",
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x21' with expected results
	expect_equal(x21$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x21$iterations[2, ], c(0, 0, 0, 0))
	expect_equal(x21$iterations[3, ], c(0, 0, 0, 0))
	expect_equal(x21$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x21$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x21$futilityStop, c(1, 1, 1, 1))
	expect_equal(x21$futilityPerStage[1, ], c(1, 1, 1, 1))
	expect_equal(x21$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x21$earlyStop[1, ], c(1, 1, 1, 1))
	expect_equal(x21$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x21$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x21$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x21$numberOfActiveArms[2, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x21$numberOfActiveArms[3, ], c(NaN, NaN, NaN, NaN))
	expect_equal(x21$expectedNumberOfEvents, c(NaN, NaN, NaN, NaN))
	expect_equal(unlist(as.list(x21$eventsPerStage)), c(4, 4, 4, 4.2727273, 4.2727273, 4.2727273, 4.5, 4.5, 4.5, 4.6923077, 4.6923077, 4.6923077, 4, 4, 4, 4.1818182, 4.1818182, 4.1818182, 4.3333333, 4.3333333, 4.3333333, 4.4615385, 4.4615385, 4.4615385, 4, 4, 4, 4.0909091, 4.0909091, 4.0909091, 4.1666667, 4.1666667, 4.1666667, 4.2307692, 4.2307692, 4.2307692, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x21$singleNumberOfEventsPerStage)), c(2, NaN, NaN, 2.0909091, NaN, NaN, 2.1666667, NaN, NaN, 2.2307692, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 2, NaN, NaN, 1.9090909, NaN, NaN, 1.8333333, NaN, NaN, 1.7692308, NaN, NaN, 2, NaN, NaN, 1.8181818, NaN, NaN, 1.6666667, NaN, NaN, 1.5384615, NaN, NaN, 2, NaN, NaN, 2.1818182, NaN, NaN, 2.3333333, NaN, NaN, 2.4615385, NaN, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x21), NA)))
	    expect_output(print(x21)$show())
	    invisible(capture.output(expect_error(summary(x21), NA)))
	    expect_output(summary(x21)$show())
	    x21CodeBased <- eval(parse(text = getObjectRCode(x21, stringWrapParagraphWidth = NULL)))
	    expect_equal(x21CodeBased$iterations, x21$iterations, tolerance = 1e-05)
	    expect_equal(x21CodeBased$rejectAtLeastOne, x21$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x21CodeBased$rejectedArmsPerStage, x21$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$futilityStop, x21$futilityStop, tolerance = 1e-05)
	    expect_equal(x21CodeBased$futilityPerStage, x21$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$earlyStop, x21$earlyStop, tolerance = 1e-05)
	    expect_equal(x21CodeBased$successPerStage, x21$successPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$selectedArms, x21$selectedArms, tolerance = 1e-05)
	    expect_equal(x21CodeBased$numberOfActiveArms, x21$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x21CodeBased$expectedNumberOfEvents, x21$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x21CodeBased$eventsPerStage, x21$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x21CodeBased$singleNumberOfEventsPerStage, x21$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_type(names(x21), "character")
	    df <- as.data.frame(x21)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x21)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	x22 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0.1, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.1, 0.3, 0.1), intersectionTest = "Bonferroni",
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x22' with expected results
	expect_equal(x22$iterations[1, ], c(10, 10, 10))
	expect_equal(x22$iterations[2, ], c(1, 4, 3))
	expect_equal(x22$iterations[3, ], c(0, 0, 0))
	expect_equal(x22$rejectAtLeastOne, c(0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x22$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.2, 0), tolerance = 1e-07)
	expect_equal(x22$futilityStop, c(0.9, 0.7, 0.8), tolerance = 1e-07)
	expect_equal(x22$futilityPerStage[1, ], c(0.9, 0.6, 0.7), tolerance = 1e-07)
	expect_equal(x22$futilityPerStage[2, ], c(0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x22$earlyStop[1, ], c(0.9, 0.6, 0.7), tolerance = 1e-07)
	expect_equal(x22$earlyStop[2, ], c(0.1, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x22$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x22$successPerStage[2, ], c(0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x22$successPerStage[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x22$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x22$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x22$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x22$numberOfActiveArms[3, ], c(NaN, NaN, NaN))
	expect_equal(x22$expectedNumberOfEvents, c(NaN, NaN, NaN))
	expect_equal(unlist(as.list(x22$eventsPerStage)), c(6.4545455, 9.0363636, 9.0363636, 6, 8.5078894, 8.5078894, 5.6153846, 8.0390249, 8.0390249, 5.6363636, 7.8909091, 7.8909091, 5.3333333, 7.5625684, 7.5625684, 5.0769231, 7.2681595, 7.2681595, 4.8181818, 6.7454545, 6.7454545, 4.6666667, 6.6172473, 6.6172473, 4.5384615, 6.4972941, 6.4972941, 4, 5.6, 5.6, 4, 5.6719263, 5.6719263, 4, 5.7264287, 5.7264287), tolerance = 1e-07)
	expect_equal(unlist(as.list(x22$singleNumberOfEventsPerStage)), c(2.8181818, 1.1272727, NaN, 2.6666667, 1.1146175, NaN, 2.5384615, 1.0956182, NaN, 2, 0.8, NaN, 2, 0.83596315, NaN, 2, 0.86321435, NaN, 1.1818182, 0.47272727, NaN, 1.3333333, 0.55730877, NaN, 1.4615385, 0.63081049, NaN, 0.36363636, 0.14545455, NaN, 0.66666667, 0.27865438, NaN, 0.92307692, 0.39840662, NaN, 3.6363636, 1.4545455, NaN, 3.3333333, 1.3932719, NaN, 3.0769231, 1.3280221, NaN), tolerance = 1e-07)
	expect_equal(x22$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x22$conditionalPowerAchieved[2, ], c(0.99998124, 0.93006261, 0.86196268), tolerance = 1e-07)
	expect_equal(x22$conditionalPowerAchieved[3, ], c(NaN, NaN, NaN))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x22), NA)))
	    expect_output(print(x22)$show())
	    invisible(capture.output(expect_error(summary(x22), NA)))
	    expect_output(summary(x22)$show())
	    x22CodeBased <- eval(parse(text = getObjectRCode(x22, stringWrapParagraphWidth = NULL)))
	    expect_equal(x22CodeBased$iterations, x22$iterations, tolerance = 1e-05)
	    expect_equal(x22CodeBased$rejectAtLeastOne, x22$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(x22CodeBased$rejectedArmsPerStage, x22$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$futilityStop, x22$futilityStop, tolerance = 1e-05)
	    expect_equal(x22CodeBased$futilityPerStage, x22$futilityPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$earlyStop, x22$earlyStop, tolerance = 1e-05)
	    expect_equal(x22CodeBased$successPerStage, x22$successPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$selectedArms, x22$selectedArms, tolerance = 1e-05)
	    expect_equal(x22CodeBased$numberOfActiveArms, x22$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(x22CodeBased$expectedNumberOfEvents, x22$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(x22CodeBased$eventsPerStage, x22$eventsPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$singleNumberOfEventsPerStage, x22$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(x22CodeBased$conditionalPowerAchieved, x22$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x22), "character")
	    df <- as.data.frame(x22)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x22)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmSurvival': using calcSubjectsFunction", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	calcSubjectsFunctionSimulationMultiArmSurvival <- function(..., stage, minNumberOfEventsPerStage) {
		return(ifelse(stage == 3, 33, minNumberOfEventsPerStage[stage]))
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),directionUpper = FALSE,
		minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10, calcEventsFunction = calcSubjectsFunctionSimulationMultiArmSurvival)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(9, 10, 8, 9))
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.4, 0.7, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.3, 0, 0.1, 0.3, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0.1, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.4, 0.5, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.4, 0.3, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x$expectedNumberOfEvents, c(43.7, 47, 40.4, 43.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(5.6153846, 7.8615385, 26.392308, 5.2857143, 7.4, 24.842857, 5, 7, 23.5, 4.75, 6.65, 22.325, 5.0769231, 7.1076923, 23.861538, 4.8571429, 6.8, 22.828571, 4.6666667, 6.5333333, 21.933333, 4.5, 6.3, 21.15, 4.5384615, 6.3538462, 21.330769, 4.4285714, 6.2, 20.814286, 4.3333333, 6.0666667, 20.366667, 4.25, 5.95, 19.975, 4, 5.6, 18.8, 4, 5.6, 18.8, 4, 5.6, 18.8, 4, 5.6, 18.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$singleNumberOfEventsPerStage)), c(2.5384615, 1.0153846, 8.3769231, 2.4285714, 0.97142857, 8.0142857, 2.3333333, 0.93333333, 7.7, 2.25, 0.9, 7.425, 2, 0.8, 6.6, 2, 0.8, 6.6, 2, 0.8, 6.6, 2, 0.8, 6.6, 1.4615385, 0.58461538, 4.8230769, 1.5714286, 0.62857143, 5.1857143, 1.6666667, 0.66666667, 5.5, 1.75, 0.7, 5.775, 0.92307692, 0.36923077, 3.0461538, 1.1428571, 0.45714286, 3.7714286, 1.3333333, 0.53333333, 4.4, 1.5, 0.6, 4.95, 3.0769231, 1.2307692, 10.153846, 2.8571429, 1.1428571, 9.4285714, 2.6666667, 1.0666667, 8.8, 2.5, 1, 8.25), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.13227215, 0.33500952, 0.32478794, 0.19174696), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.28682503, 0.6076832, 0.60939504, 0.37477275), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$expectedNumberOfEvents, x$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(xCodeBased$eventsPerStage, x$eventsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$singleNumberOfEventsPerStage, x$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmSurvival': using selectArmsFunction", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	selectArmsFunctionSimulationMultiArmSurvival <- function(effectSizes) {
		return(c(TRUE, FALSE, FALSE, FALSE))
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),directionUpper = FALSE,
		maxNumberOfIterations = 10, selectArmsFunction = selectArmsFunctionSimulationMultiArmSurvival, typeOfSelection = "userDefined")

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(x$expectedNumberOfEvents, c(50, 50, 50, 48))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(5.6153846, 16.846154, 28.076923, 5.2857143, 15.857143, 26.428571, 5, 15, 25, 4.75, 14.25, 23.75, 5.0769231, 15.230769, 25.384615, 4.8571429, 14.571429, 24.285714, 4.6666667, 14, 23.333333, 4.5, 13.5, 22.5, 4.5384615, 13.615385, 22.692308, 4.4285714, 13.285714, 22.142857, 4.3333333, 13, 21.666667, 4.25, 12.75, 21.25, 4, 12, 20, 4, 12, 20, 4, 12, 20, 4, 12, 20), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$singleNumberOfEventsPerStage)), c(2.5384615, 5.0769231, 5.0769231, 2.4285714, 4.8571429, 4.8571429, 2.3333333, 4.6666667, 4.6666667, 2.25, 4.5, 4.5, 2, 4, 4, 2, 4, 4, 2, 4, 4, 2, 4, 4, 1.4615385, 2.9230769, 2.9230769, 1.5714286, 3.1428571, 3.1428571, 1.6666667, 3.3333333, 3.3333333, 1.75, 3.5, 3.5, 0.92307692, 1.8461538, 1.8461538, 1.1428571, 2.2857143, 2.2857143, 1.3333333, 2.6666667, 2.6666667, 1.5, 3, 3, 3.0769231, 6.1538462, 6.1538462, 2.8571429, 5.7142857, 5.7142857, 2.6666667, 5.3333333, 5.3333333, 2.5, 5, 5), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.33564601, 0.59192905, 0.61161484, 0.44432847), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10158651, 0.080642472, 0.3234231, 0.034914809), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$expectedNumberOfEvents, x$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(xCodeBased$eventsPerStage, x$eventsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$singleNumberOfEventsPerStage, x$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmSurvival': typeOfShape = sigmoidEmax", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	# @refFS[Formula]{fs:adjustedPValueSubsetDunnett}
	designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
	x <- getSimulationMultiArmSurvival(designIN, activeArms = 3, typeOfShape = "sigmoidEmax", omegaMaxVector = seq(1, 1.9, 0.3), gED50 = 2, plannedEvents = cumsum(rep(50, 3)), 
		intersectionTest = "Sidak", typeOfSelection = "rBest", rValue = 2, threshold = -Inf, successCriterion = "all", maxNumberOfIterations = 100, seed = 3456)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(40, 57, 66, 79))
	expect_equal(x$iterations[3, ], c(27, 48, 55, 70))
	expect_equal(x$rejectAtLeastOne, c(0.02, 0.07, 0.19, 0.21), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0.01, 0.01, 0.01, 0.02, 0.01, 0.02, 0, 0.02, 0.02, 0.01, 0, 0, 0, 0.01, 0.02, 0.03, 0.03, 0.01, 0.03, 0.06, 0.01, 0.01, 0, 0, 0.01, 0.01, 0.02, 0.04, 0.03, 0.07, 0.03, 0.09, 0.06), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.73, 0.51, 0.41, 0.24), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.6, 0.43, 0.34, 0.21), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.13, 0.08, 0.07, 0.03), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.6, 0.43, 0.34, 0.21), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.13, 0.09, 0.11, 0.09), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.01, 0.04, 0.06), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.02, 0.03, 0.05), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.2, 1, 0.31, 0.28, 1, 0.42, 0.37, 1, 0.35, 0.32, 1, 0.26, 0.19, 1, 0.45, 0.36, 1, 0.38, 0.31, 1, 0.59, 0.52, 1, 0.24, 0.15, 1, 0.38, 0.32, 1, 0.52, 0.42, 1, 0.64, 0.56), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(x$expectedNumberOfEvents, c(83.5, 102.5, 110.5, 124.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(25, 50, 75, 23.702032, 47.404063, 71.106095, 22.633745, 45.26749, 67.901235, 21.73913, 43.478261, 65.217391, 25, 50, 75, 24.266366, 48.532731, 72.799097, 23.662551, 47.325103, 70.987654, 23.1569, 46.3138, 69.470699, 25, 50, 75, 24.604966, 49.209932, 73.814898, 24.279835, 48.559671, 72.839506, 24.007561, 48.015123, 72.022684), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$singleNumberOfEventsPerStage)), c(12.5, 12.5, 12.5, 12.41535, 12.41535, 12.41535, 12.345679, 12.345679, 12.345679, 12.287335, 12.287335, 12.287335, 12.5, 12.5, 12.5, 12.979684, 12.979684, 12.979684, 13.374486, 13.374486, 13.374486, 13.705104, 13.705104, 13.705104, 12.5, 12.5, 12.5, 13.318284, 13.318284, 13.318284, 13.99177, 13.99177, 13.99177, 14.555766, 14.555766, 14.555766, 12.5, 12.5, 12.5, 11.286682, 11.286682, 11.286682, 10.288066, 10.288066, 10.288066, 9.4517958, 9.4517958, 9.4517958), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.066083689, 0.14406787, 0.27240426, 0.24161087), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.13321164, 0.19096794, 0.29528894, 0.30979546), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	    xCodeBased <- eval(parse(text = getObjectRCode(x, stringWrapParagraphWidth = NULL)))
	    expect_equal(xCodeBased$iterations, x$iterations, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectAtLeastOne, x$rejectAtLeastOne, tolerance = 1e-05)
	    expect_equal(xCodeBased$rejectedArmsPerStage, x$rejectedArmsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityStop, x$futilityStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$futilityPerStage, x$futilityPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$earlyStop, x$earlyStop, tolerance = 1e-05)
	    expect_equal(xCodeBased$successPerStage, x$successPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$selectedArms, x$selectedArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$numberOfActiveArms, x$numberOfActiveArms, tolerance = 1e-05)
	    expect_equal(xCodeBased$expectedNumberOfEvents, x$expectedNumberOfEvents, tolerance = 1e-05)
	    expect_equal(xCodeBased$eventsPerStage, x$eventsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$singleNumberOfEventsPerStage, x$singleNumberOfEventsPerStage, tolerance = 1e-05)
	    expect_equal(xCodeBased$conditionalPowerAchieved, x$conditionalPowerAchieved, tolerance = 1e-05)
	    expect_type(names(x), "character")
	    df <- as.data.frame(x)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, inverse normal design", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	allocationRatioPlanned <- 1
	design <- getDesignInverseNormal(typeOfDesign = "WT", deltaWT = 0.05, futilityBounds = c(-0.5,0.5), informationRates = c(0.2, 0.8, 1))

	x <- getSimulationMultiArmSurvival(design, activeArms = 1, omegaMaxVector = 1/seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60), 
		conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10), #thetaH1 = 2,
		maxNumberOfIterations = 100, directionUpper = FALSE, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	y <- getSimulationSurvival(design, pi2 = 0.2, hazardRatio = 1/seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60), maxNumberOfSubjects = 500, 
		conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10), #thetaH1 = 2,
		maxNumberOfIterations = 100, directionUpper = FALSE, allocation1 = 1, allocation2 = 1, seed = 1234)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.02, 0.01, 0.06), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0))
	expect_equal(comp2[2, ], c(-0.02, 0.02, 0.03), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0, -0.01, 0.03), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(-0.06, -0.02, -0.03), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0.08, 0.06, 0), tolerance = 1e-07)

	comp4 <- round(y$eventsPerStage - x$eventsPerStage[, , 1], 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0))
	expect_equal(comp4[2, ], c(1.2, -0.4, 1), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(1.7, -0.8, 1), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(6.9, -4.7, 3.6), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.43, -0.73, -0.52), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.13, -0.32, -0.04), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, Fisher design", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	allocationRatioPlanned <- 1
	design <- getDesignFisher(alpha0Vec = c(0.6, 0.4), informationRates = c(0.5, 0.6, 1))

	x <- getSimulationMultiArmSurvival(design, activeArms = 1, omegaMaxVector = 1/seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60), 
		conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10), #thetaH1 = 2,
		maxNumberOfIterations = 100, directionUpper = FALSE, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	y <- getSimulationSurvival(design, pi2 = 0.2, hazardRatio = 1/seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60), maxNumberOfSubjects = 500, 
		conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10), #thetaH1 = 2,
		maxNumberOfIterations = 100, directionUpper = FALSE, allocation1 = 1, allocation2 = 1, seed = 1234)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.02, -0.01, 0.02), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(-0.02, 0.01, -0.01), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(0, -0.03, 0.01), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0, 0.01, 0.02), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(-0.03, 0.01, -0.01), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0.05, 0.05, -0.01), tolerance = 1e-07)

	comp4 <- round(y$eventsPerStage - x$eventsPerStage[, , 1], 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0))
	expect_equal(comp4[2, ], c(-0.6, 0.8, -0.3), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(-0.6, 0.8, -0.3), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(4.7, -5.3, 3.6), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.27, -0.42, -0.29), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.22, -0.54, -0.18), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, inverse normal design with user alpha spending", {

	.skipTestIfDisabled()

	# @refFS[Sec.]{fs:sec:reproducibilityOfSimulationResults}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDesigns}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmDoseResponseRelationShips}
	# @refFS[Sec.]{fs:sec:simulatingMultiArmSelections}
	# @refFS[Tab.]{fs:tab:output:getSimulationMultiArmSurvival}
	# @refFS[Formula]{fs:SimulationMultiArmDoseResponse}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCholeskyTransformation}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalCorrMatrix}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalEvents}
	# @refFS[Formula]{fs:SimulationMultiArmSurvivalLogRanks}
	# @refFS[Formula]{fs:SimulationMultiArmSelections}
	# @refFS[Formula]{fs:multiarmRejectionRule}
	allocationRatioPlanned <- 1
	design <- getDesignInverseNormal(typeOfDesign = "asUser", userAlphaSpending = c(0,0,0.025), informationRates = c(0.2, 0.8, 1))

	x <- getSimulationMultiArmSurvival(design, activeArms = 1, omegaMaxVector = 1/seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60), 
		conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10), #thetaH1 = 2,
		maxNumberOfIterations = 100, directionUpper = FALSE, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	y <- getSimulationSurvival(design, pi2 = 0.2, hazardRatio = 1/seq(1, 1.8, 0.4), plannedEvents = c(20, 40, 60), maxNumberOfSubjects = 500, 
		conditionalPower = 0.99, maxNumberOfEventsPerStage = c(NA, 100, 100), minNumberOfEventsPerStage = c(NA, 10, 10), #thetaH1 = 2,
		maxNumberOfIterations = 100, directionUpper = FALSE, allocation1 = 1, allocation2 = 1, seed = 1234)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.01, 0.02, 0.01), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0))
	expect_equal(comp2[2, ], c(0, 0, 0))
	expect_equal(comp2[3, ], c(-0.01, 0.02, 0.01), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0, 0, 0))
	expect_equal(comp3[2, ], c(0, 0, 0))

	comp4 <- round(y$eventsPerStage - x$eventsPerStage[, , 1], 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0))
	expect_equal(comp4[2, ], c(0, 0, 0))
	expect_equal(comp4[3, ], c(-0.2, -3.5, 0.6), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-0.2, -3.5, 0.6), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(0, 0, 0))
	expect_equal(comp6[2, ], c(0, 0, 0))

})

