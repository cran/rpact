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
#:#  File name: test-f_simulation_multiarm_survival.R
#:#  Creation date: 09 November 2020, 11:49:37
#:#  File version: $Revision$
#:#  Last changed: $Date$
#:#  Last changed by: $Author$
#:#  

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
	expect_equal(x1$iterations[3, ], c(10, 10, 9, 8))
	expect_equal(unlist(as.list(x1$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x1$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x1$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x1$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x1$rejectAtLeastOne, c(0, 0.1, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x1$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x1$futilityStop, c(0, 0, 0, 0))
	expect_equal(x1$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x1$earlyStop[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x1$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x1$successPerStage[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x1$successPerStage[3, ], c(0, 0.1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x1$eventsPerStage)), c(4, 29.436339, 59.436339, 3.7272727, 33.727273, 63.727273, 3.5, 13.5, 24.611111, 3.3076923, 13.722725, 26.222725, 4, 15.022546, 35.022546, 3.8181818, 14.218182, 25.451332, 3.6666667, 13.666667, 24.777778, 3.5384615, 13.538462, 26.038462, 4, 46.262375, 88.319477, 3.9090909, 15.780383, 55.780383, 3.8333333, 8.4704109, 31.262843, 3.7692308, 18.880792, 48.301305, 4, 4, 4, 4, 4.9663813, 7.4092152, 4, 25.86476, 39.692802, 4, 24, 27.867652), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.0040144458, 0.033481368, 0.030634083, 0.057515963), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.099057058, 0.19386846, 0.57054557, 0.50912101), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	}

	x2 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms =  4, 
		plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x2' with expected results
	expect_equal(x2$iterations[1, ], c(10, 10))
	expect_equal(x2$iterations[2, ], c(9, 10))
	expect_equal(x2$iterations[3, ], c(0, 5))
	expect_equal(unlist(as.list(x2$selectedArms)), c(1, 0.2, 0, 1, 0.3, 0, 1, 0.6, 0, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.2, 0, 1, 0, 0, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x2$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x2$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x2$numberOfActiveArms[3, ], c(NaN, 1))
	expect_equal(unlist(as.list(x2$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x2$rejectAtLeastOne, c(0, 0))
	expect_equal(x2$futilityPerStage[1, ], c(0.1, 0), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[2, ], c(0.9, 0.5), tolerance = 1e-07)
	expect_equal(x2$futilityStop, c(1, 0.5), tolerance = 1e-07)
	expect_equal(x2$earlyStop[1, ], c(0.1, 0), tolerance = 1e-07)
	expect_equal(x2$earlyStop[2, ], c(0.9, 0.5), tolerance = 1e-07)
	expect_equal(x2$successPerStage[1, ], c(0, 0))
	expect_equal(x2$successPerStage[2, ], c(0, 0))
	expect_equal(x2$successPerStage[3, ], c(0, 0))
	expect_equal(unlist(as.list(x2$eventsPerStage)), c(5.5, 27.722222, 27.722222, 5, 35, 35, 6.5, 73.166667, 73.166667, 5.8333333, 45.833333, 125.83333, 6, 17.111111, 17.111111, 5.4166667, 25.416667, 25.416667, 7, 7, 7, 6.25, 16.25, 36.25), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(1.5056793e-05, 0.00040792221), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(NaN, 1.3881896e-10), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
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
	expect_equal(unlist(as.list(x3$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x3$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x3$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x3$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x3$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x3$rejectAtLeastOne, c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x3$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x3$futilityStop, c(0, 0, 0, 0))
	expect_equal(x3$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x3$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x3$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x3$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x3$successPerStage[3, ], c(0, 0.1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x3$eventsPerStage)), c(4, 29.436339, 59.436339, 3.8499139, 33.849914, 63.849914, 3.7209785, 13.720978, 23.720978, 3.6090171, 14.009505, 25.120616, 4, 15.022546, 35.022546, 3.8816273, 14.281627, 25.499919, 3.7799362, 13.779936, 23.779936, 3.6916324, 13.691632, 24.802744, 4, 46.262375, 88.319477, 3.9002999, 15.909295, 55.909295, 3.8146499, 8.7298074, 31.539255, 3.7402755, 18.970722, 49.690982, 4, 4, 4, 3.9133408, 4.9597138, 8.591765, 3.8388939, 25.9242, 45.195751, 3.7742477, 23.774248, 42.286161), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.0040144458, 0.031004558, 0.028045661, 0.057619827), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.099057058, 0.18515685, 0.55423766, 0.46896112), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	}

	x4 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x4' with expected results
	expect_equal(x4$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x4$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x4$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x4$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x4$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x4$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x4$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x4$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x4$rejectAtLeastOne, c(0, 0.1, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x4$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x4$futilityStop, c(0, 0, 0, 0))
	expect_equal(x4$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x4$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x4$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x4$eventsPerStage)), c(4, 43.772282, 83.772282, 3.7272727, 41, 78.272727, 3.5, 37.402494, 72.402494, 3.3076923, 32.105389, 64.884851, 4, 43.772282, 83.772282, 3.8181818, 42, 80.181818, 3.6666667, 39.183565, 75.850232, 3.5384615, 34.3453, 69.411701, 4, 43.772282, 83.772282, 3.9090909, 43, 82.090909, 3.8333333, 40.964637, 79.29797, 3.7692308, 36.585211, 73.938551, 4, 43.772282, 83.772282, 4, 44, 84, 4, 42.745708, 82.745708, 4, 38.825122, 78.465402), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.08877696, 0.11097922, 0.081804044, 0.16575383), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.029928092, 0.12374234, 0.16125668, 0.27984685), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	}

	x5 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x5' with expected results
	expect_equal(x5$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x5$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x5$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x5$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x5$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x5$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x5$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x5$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x5$rejectAtLeastOne, c(0.1, 0, 0.3, 0.8), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x5$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x5$futilityStop, c(0, 0, 0, 0))
	expect_equal(x5$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x5$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x5$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x5$eventsPerStage)), c(4, 45.130824, 85.750498, 3.7272727, 23.879745, 49.409316, 3.5, 15.676471, 27.852941, 3.3076923, 16.528466, 31.521954, 4, 32.204972, 62.209092, 3.8181818, 23.212121, 42.606061, 3.6666667, 22.796175, 54.74711, 3.5384615, 21.989171, 33.319513, 4, 31.797491, 59.083831, 3.9090909, 26.253102, 52.804992, 3.8333333, 22.698366, 58.177012, 3.7692308, 26.329972, 48.184111, 4, 18.871639, 35.542426, 4, 41.089889, 88.256059, 4, 23.778819, 52.409436, 4, 48.628156, 87.133763), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.0066385262, 0.028647141, 0.067819885, 0.053024271), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.16018737, 0.20764174, 0.32461588, 0.49935362), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	}

	x6 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x6' with expected results
	expect_equal(x6$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x6$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x6$iterations[3, ], c(10, 9, 9, 10))
	expect_equal(unlist(as.list(x6$selectedArms)), c(1, 0.5, 0.5, 1, 0.5, 0.1, 1, 0.1, 0, 1, 0.2, 0.2, 1, 0.3, 0.2, 1, 0.7, 0.6, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x6$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x6$numberOfActiveArms[2, ], c(1.1, 1.6, 1.2, 1), tolerance = 1e-07)
	expect_equal(x6$numberOfActiveArms[3, ], c(1, 1.2222222, 1, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0.3), tolerance = 1e-07)
	expect_equal(x6$rejectAtLeastOne, c(0, 0.1, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x6$futilityStop, c(0, 0, 0, 0))
	expect_equal(x6$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x6$earlyStop[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x6$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x6$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x6$successPerStage[3, ], c(0, 0, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x6$eventsPerStage)), c(4, 41.1574, 82.296219, 3.7272727, 26.422987, 33.654028, 3.5, 7.875, 7.875, 3.3076923, 23.307692, 43.307692, 4, 30.646919, 50.646919, 3.8181818, 33.070783, 91.993342, 3.6666667, 18.375379, 35.078236, 3.5384615, 15.618247, 35.993699, 4, 14, 24, 3.9090909, 8.9090909, 20.020202, 3.8333333, 22.992827, 45.21505, 3.7692308, 11.245396, 19.091449, 4, 15.31861, 35.31861, 4, 16.793083, 38.025862, 4, 31.777379, 70.085624, 4, 21.44892, 34.112595), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.018476348, 0.073916838, 0.069569381, 0.045997197), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.080984732, 0.15483493, 0.45714346, 0.50228478), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	}

	x7 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x7' with expected results
	expect_equal(x7$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x7$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x7$iterations[3, ], c(10, 10, 9, 8))
	expect_equal(unlist(as.list(x7$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x7$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x7$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x7$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x7$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x7$rejectAtLeastOne, c(0, 0.1, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x7$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x7$futilityStop, c(0, 0, 0, 0))
	expect_equal(x7$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x7$earlyStop[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x7$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x7$successPerStage[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x7$successPerStage[3, ], c(0, 0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x7$eventsPerStage)), c(4, 29.436339, 54.872679, 3.7272727, 33.727273, 63.727273, 3.5, 13.5, 24.611111, 3.3076923, 13.722725, 26.222725, 4, 15.022546, 26.045092, 3.8181818, 14.218182, 24.618182, 3.6666667, 13.666667, 24.777778, 3.5384615, 13.538462, 26.038462, 4, 46.262375, 88.52475, 3.9090909, 15.780383, 27.651675, 3.8333333, 8.4704109, 13.622719, 3.7692308, 18.880792, 37.770244, 4, 4, 4, 4, 4.9663813, 5.9327627, 4, 25.86476, 39.047826, 4, 24, 36.5), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.0040144458, 0.033481368, 0.030634083, 0.057515963), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[3, ], c(0.086614621, 0.11647878, 0.4057679, 0.42818398), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	}

	x8 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x8' with expected results
	expect_equal(x8$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x8$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x8$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x8$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x8$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x8$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x8$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x8$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x8$rejectAtLeastOne, c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x8$futilityStop, c(0, 0, 0, 0))
	expect_equal(x8$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x8$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x8$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x8$eventsPerStage)), c(4, 43.772282, 83.544564, 3.7272727, 41, 78.272727, 3.5, 37.402494, 71.304989, 3.3076923, 32.105389, 60.903087, 4, 43.772282, 83.544564, 3.8181818, 42, 80.181818, 3.6666667, 39.183565, 74.700464, 3.5384615, 34.3453, 65.152139, 4, 43.772282, 83.544564, 3.9090909, 43, 82.090909, 3.8333333, 40.964637, 78.09594, 3.7692308, 36.585211, 69.401192, 4, 43.772282, 83.544564, 4, 44, 84, 4, 42.745708, 81.491415, 4, 38.825122, 73.650244), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.08877696, 0.11097922, 0.081804044, 0.16575383), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(0.029928092, 0.12374234, 0.16125668, 0.26457965), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	}

	x9 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x9' with expected results
	expect_equal(x9$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x9$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x9$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x9$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x9$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x9$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x9$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x9$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x9$rejectAtLeastOne, c(0.1, 0, 0.3, 0.8), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x9$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x9$futilityStop, c(0, 0, 0, 0))
	expect_equal(x9$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x9$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x9$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x9$eventsPerStage)), c(4, 45.130824, 86.261649, 3.7272727, 23.879745, 44.032217, 3.5, 15.676471, 27.852941, 3.3076923, 16.528466, 29.74924, 4, 32.204972, 60.409945, 3.8181818, 23.212121, 42.606061, 3.6666667, 22.796175, 41.925684, 3.5384615, 21.989171, 40.43988, 4, 31.797491, 59.594982, 3.9090909, 26.253102, 48.597114, 3.8333333, 22.698366, 41.563398, 3.7692308, 26.329972, 48.890713, 4, 18.871639, 33.743278, 4, 41.089889, 78.179778, 4, 23.778819, 43.557637, 4, 48.628156, 93.256312), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.0066385262, 0.028647141, 0.067819885, 0.053024271), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[3, ], c(0.14251665, 0.16918248, 0.20749232, 0.50040031), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x9), NA)))
	    expect_output(print(x9)$show())
	    invisible(capture.output(expect_error(summary(x9), NA)))
	    expect_output(summary(x9)$show())
	}

	x10 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Hierarchical",  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x10' with expected results
	expect_equal(x10$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x10$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x10$iterations[3, ], c(6, 3, 2, 2))
	expect_equal(unlist(as.list(x10$selectedArms)), c(1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.2, 0, 1, 0.4, 0.1, 1, 0.2, 0, 1, 0.2, 0.1, 1, 0.5, 0, 1, 0.5, 0, 1, 0.5, 0), tolerance = 1e-07)
	expect_equal(x10$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x10$numberOfActiveArms[2, ], c(1.5, 1, 1.2, 1), tolerance = 1e-07)
	expect_equal(x10$numberOfActiveArms[3, ], c(1.8333333, 1, 1.5, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x10$rejectAtLeastOne, c(0, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$futilityPerStage[2, ], c(0.4, 0.7, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x10$futilityStop, c(0.4, 0.7, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x10$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x10$earlyStop[2, ], c(0.4, 0.7, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x10$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x10$successPerStage[3, ], c(0, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x10$eventsPerStage)), c(4, 45.41043, 114.42781, 3.7272727, 24.796736, 95.028281, 3.5, 15.516091, 75.596548, 3.3076923, 5.2679727, 15.069374, 4, 32.472576, 76.917021, 3.8181818, 3.8181818, 3.8181818, 3.6666667, 9.7777778, 9.7777778, 3.5384615, 4.2549839, 4.2549839, 4, 24, 24, 3.9090909, 23.909091, 23.909091, 3.8333333, 26.990135, 38.030635, 3.7692308, 14.169231, 14.169231, 4, 11.066667, 22.177778, 4, 54, 54, 4, 32.503406, 32.503406, 4, 31.512459, 31.512459), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.0051908501, 0.0011240512, 0.020523961, 0.067061552), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[3, ], c(1.3780048e-06, 0.21942433, 1.5376589e-13, 0.20999872), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x10), NA)))
	    expect_output(print(x10)$show())
	    invisible(capture.output(expect_error(summary(x10), NA)))
	    expect_output(summary(x10)$show())
	}

	x11 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Hierarchical",  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x11' with expected results
	expect_equal(x11$iterations[1, ], c(10, 10, 10))
	expect_equal(x11$iterations[2, ], c(10, 10, 10))
	expect_equal(x11$iterations[3, ], c(1, 0, 0))
	expect_equal(unlist(as.list(x11$selectedArms)), c(1, 0.1, 0.1, 1, 0, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0.4, 0, 1, 0.5, 0, 1, 0.4, 0, 1, 0.4, 0, 1, 0.4, 0, 1, 0.5, 0), tolerance = 1e-07)
	expect_equal(x11$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x11$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x11$numberOfActiveArms[3, ], c(1, NaN, NaN))
	expect_equal(unlist(as.list(x11$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x11$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(x11$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$futilityPerStage[2, ], c(0.9, 1, 1), tolerance = 1e-07)
	expect_equal(x11$futilityStop, c(0.9, 1, 1), tolerance = 1e-07)
	expect_equal(x11$earlyStop[1, ], c(0, 0, 0))
	expect_equal(x11$earlyStop[2, ], c(0.9, 1, 1), tolerance = 1e-07)
	expect_equal(x11$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x11$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x11$successPerStage[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x11$eventsPerStage)), c(1.5454545, 11.545455, 111.54545, 2, 2, 2, 2.3846154, 2.3846154, 2.3846154, 2.3636364, 12.363636, 12.363636, 2.6666667, 12.666667, 12.666667, 2.9230769, 12.923077, 12.923077, 3.1818182, 43.181818, 43.181818, 3.3333333, 53.333333, 53.333333, 3.4615385, 43.461538, 43.461538, 4, 44, 44, 4, 44, 44, 4, 54, 54), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x11$conditionalPowerAchieved[2, ], c(0.00028026151, 4.3545167e-12, 1.6867306e-05), tolerance = 1e-07)
	expect_equal(x11$conditionalPowerAchieved[3, ], c(0, NaN, NaN))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	}

	x12 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Hierarchical", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x12' with expected results
	expect_equal(x12$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x12$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x12$iterations[3, ], c(3, 3, 2, 5))
	expect_equal(unlist(as.list(x12$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.2, 0, 1, 0.3, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0.5, 0, 1, 0.3, 0, 1, 0.4, 0, 1, 0.4, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.3, 0, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x12$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x12$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x12$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x12$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x12$rejectAtLeastOne, c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x12$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x12$futilityPerStage[2, ], c(0.7, 0.7, 0.8, 0.5), tolerance = 1e-07)
	expect_equal(x12$futilityStop, c(0.7, 0.7, 0.8, 0.5), tolerance = 1e-07)
	expect_equal(x12$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x12$earlyStop[2, ], c(0.7, 0.7, 0.8, 0.5), tolerance = 1e-07)
	expect_equal(x12$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x12$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x12$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x12$eventsPerStage)), c(4, 34, 134, 4.2727273, 34.272727, 134.27273, 4.5, 24.5, 124.5, 4.6923077, 54.692308, 154.69231, 4, 24, 24, 4.1818182, 34.181818, 34.181818, 4.3333333, 14.333333, 14.333333, 4.4615385, 14.461538, 14.461538, 4, 45.127323, 45.127323, 4.0909091, 34.090909, 34.090909, 4.1666667, 44.166667, 44.166667, 4.2307692, 36.545958, 36.545958, 4, 4, 4, 4, 14, 14, 4, 34, 34, 4, 4, 4), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x12$conditionalPowerAchieved[2, ], c(0.22253242, 0.00045258608, 0.093314341, 0.10629785), tolerance = 1e-07)
	expect_equal(x12$conditionalPowerAchieved[3, ], c(0.18123583, 0.067877411, 0.13527998, 0.11673603), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	}

	x13 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined",
		activeArms = 4, directionUpper = FALSE, threshold = 0, 
		plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Sidak", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x13' with expected results
	expect_equal(x13$iterations[1, ], c(10, 10))
	expect_equal(x13$iterations[2, ], c(10, 10))
	expect_equal(x13$iterations[3, ], c(6, 3))
	expect_equal(unlist(as.list(x13$selectedArms)), c(1, 0.2, 0, 1, 0.3, 0, 1, 0.6, 0.4, 1, 0.4, 0.2, 1, 0.1, 0.1, 1, 0.2, 0, 1, 0.1, 0.1, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x13$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x13$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x13$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x13$rejectedArmsPerStage)), c(0.1, 0.2, 0, 0, 0.3, 0, 0, 0.2, 0.3, 0, 0.2, 0, 0, 0, 0.1, 0, 0.2, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x13$rejectAtLeastOne, c(0.9, 0.7), tolerance = 1e-07)
	expect_equal(x13$futilityPerStage[1, ], c(0, 0))
	expect_equal(x13$futilityPerStage[2, ], c(0, 0))
	expect_equal(x13$futilityStop, c(0, 0))
	expect_equal(x13$earlyStop[1, ], c(0, 0))
	expect_equal(x13$earlyStop[2, ], c(0.4, 0.7), tolerance = 1e-07)
	expect_equal(x13$successPerStage[1, ], c(0, 0))
	expect_equal(x13$successPerStage[2, ], c(0.4, 0.7), tolerance = 1e-07)
	expect_equal(x13$successPerStage[3, ], c(0.4, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x13$eventsPerStage)), c(5.5, 7.1946911, 7.1946911, 5, 11.719471, 11.719471, 6.5, 10.496172, 13.162839, 5.8333333, 30.553924, 33.254583, 6, 6.8139406, 8.6802967, 5.4166667, 21.877281, 21.877281, 7, 7.4, 8.0666667, 6.25, 14.209807, 15.543141), tolerance = 1e-07)
	expect_equal(x13$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x13$conditionalPowerAchieved[2, ], c(0.91184531, 0.7069194), tolerance = 1e-07)
	expect_equal(x13$conditionalPowerAchieved[3, ], c(0.95541048, 0.90206616), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x13), NA)))
	    expect_output(print(x13)$show())
	    invisible(capture.output(expect_error(summary(x13), NA)))
	    expect_output(summary(x13)$show())
	}

	x14 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Sidak", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x14' with expected results
	expect_equal(x14$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x14$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x14$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x14$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x14$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x14$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x14$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x14$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x14$rejectAtLeastOne, c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x14$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x14$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x14$futilityStop, c(0, 0, 0, 0))
	expect_equal(x14$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x14$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x14$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x14$eventsPerStage)), c(4, 34, 64, 4.1452587, 34.145259, 64.145259, 4.2627857, 22.607261, 42.607261, 4.3598306, 44.359831, 84.359831, 4, 24, 44, 4.1145653, 24.114565, 44.114565, 4.2072587, 14.207259, 24.207259, 4.2837979, 14.283798, 24.283798, 4, 45.127323, 95.127323, 4.0964933, 44.096493, 84.096493, 4.1745649, 44.174565, 84.174565, 4.2390305, 36.683736, 76.683736, 4, 4, 4, 4.0838719, 14.083872, 24.083872, 4.1517317, 34.151732, 64.151732, 4.2077651, 14.207765, 24.207765), tolerance = 1e-07)
	expect_equal(x14$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x14$conditionalPowerAchieved[2, ], c(0.22253242, 0.0004342892, 0.098725954, 0.10762321), tolerance = 1e-07)
	expect_equal(x14$conditionalPowerAchieved[3, ], c(0.06946024, 0.025936856, 0.092298874, 0.23872986), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x14), NA)))
	    expect_output(print(x14)$show())
	    invisible(capture.output(expect_error(summary(x14), NA)))
	    expect_output(summary(x14)$show())
	}

	x15 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  intersectionTest = "Sidak", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x15' with expected results
	expect_equal(x15$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x15$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x15$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x15$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x15$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x15$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x15$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x15$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x15$rejectAtLeastOne, c(0, 0.1, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x15$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x15$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x15$futilityStop, c(0, 0, 0, 0))
	expect_equal(x15$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x15$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x15$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x15$eventsPerStage)), c(4, 40.450929, 80.450929, 4.2727273, 44.448267, 87.17554, 4.5, 49.351208, 90.81906, 4.6923077, 51.615385, 95.324015, 4, 40.450929, 80.450929, 4.1818182, 43.502559, 85.320741, 4.3333333, 47.523385, 87.455391, 4.4615385, 49.076923, 90.635949, 4, 40.450929, 80.450929, 4.0909091, 42.556851, 83.465942, 4.1666667, 45.695563, 84.091723, 4.2307692, 46.538462, 85.947882, 4, 40.450929, 80.450929, 4, 41.611144, 81.611144, 4, 43.86774, 80.728054, 4, 44, 81.259816), tolerance = 1e-07)
	expect_equal(x15$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x15$conditionalPowerAchieved[2, ], c(0.15181324, 0.086639331, 0.089369825, 0.10485947), tolerance = 1e-07)
	expect_equal(x15$conditionalPowerAchieved[3, ], c(0.033667272, 0.07601143, 0.1243661, 0.18764712), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x15), NA)))
	    expect_output(print(x15)$show())
	    invisible(capture.output(expect_error(summary(x15), NA)))
	    expect_output(summary(x15)$show())
	}

	x16 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Simes",
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x16' with expected results
	expect_equal(x16$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x16$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x16$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x16$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.4, 0.4, 1, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x16$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x16$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x16$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x16$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x16$rejectAtLeastOne, c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x16$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x16$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x16$futilityStop, c(0, 0, 0, 0))
	expect_equal(x16$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x16$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x16$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x16$eventsPerStage)), c(4, 50.171272, 92.107801, 4.2727273, 32.025937, 59.779147, 4.5, 25.825637, 47.151275, 4.6923077, 34.027435, 63.362563, 4, 36.837938, 65.441134, 4.1818182, 24.787879, 45.393939, 4.3333333, 46.33033, 74.988985, 4.4615385, 40.320988, 69.694147, 4, 37.333333, 70.666667, 4.0909091, 38.400864, 72.710819, 4.1666667, 52.058355, 87.124715, 4.2307692, 38.762562, 61.758107, 4, 24, 44, 4, 57.544585, 111.08917, 4, 30.867439, 57.734878, 4, 43.791098, 78.490492), tolerance = 1e-07)
	expect_equal(x16$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x16$conditionalPowerAchieved[2, ], c(0.18026845, 0.021058109, 0.096260528, 0.10423032), tolerance = 1e-07)
	expect_equal(x16$conditionalPowerAchieved[3, ], c(0.12054024, 0.038128067, 0.22136384, 0.23880719), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x16), NA)))
	    expect_output(print(x16)$show())
	    invisible(capture.output(expect_error(summary(x16), NA)))
	    expect_output(summary(x16)$show())
	}

	x17 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Simes",
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x17' with expected results
	expect_equal(x17$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x17$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x17$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x17$selectedArms)), c(1, 0.5, 0.5, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x17$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x17$numberOfActiveArms[2, ], c(1.1, 1.3, 1.2, 1.2), tolerance = 1e-07)
	expect_equal(x17$numberOfActiveArms[3, ], c(1, 1.1, 1.1, 1.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x17$rejectedArmsPerStage)), c(0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x17$rejectAtLeastOne, c(0.1, 0, 0.2, 0), tolerance = 1e-07)
	expect_equal(x17$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x17$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x17$futilityStop, c(0, 0, 0, 0))
	expect_equal(x17$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x17$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x17$successPerStage[3, ], c(0.1, 0, 0.2, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x17$eventsPerStage)), c(4, 50.666667, 100.66667, 4.2727273, 39.494949, 69.494949, 4.5, 41.797297, 79.094595, 4.6923077, 54.692308, 98.142699, 4, 30.666667, 50.666667, 4.1818182, 36.158601, 73.024273, 4.3333333, 31.555556, 61.555556, 4.4615385, 24.730525, 45.016944, 4, 14, 24, 4.0909091, 25.807327, 42.523745, 4.1666667, 14.166667, 24.166667, 4.2307692, 24.485842, 44.757447, 4, 24, 44, 4, 24, 44, 4, 37.153153, 63.63964, 4, 4.24116, 4.24116), tolerance = 1e-07)
	expect_equal(x17$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x17$conditionalPowerAchieved[2, ], c(0.13047811, 0.023969368, 0.034183159, 0.11152873), tolerance = 1e-07)
	expect_equal(x17$conditionalPowerAchieved[3, ], c(0.14651341, 0.027305188, 0.051243316, 0.24293265), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x17), NA)))
	    expect_output(print(x17)$show())
	    invisible(capture.output(expect_error(summary(x17), NA)))
	    expect_output(summary(x17)$show())
	}

	x18 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Simes",
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x18' with expected results
	expect_equal(x18$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x18$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x18$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x18$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x18$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x18$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x18$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x18$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x18$rejectAtLeastOne, c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x18$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x18$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x18$futilityStop, c(0, 0, 0, 0))
	expect_equal(x18$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x18$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x18$successPerStage[3, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x18$eventsPerStage)), c(4, 34, 64, 4.2727273, 34.272727, 64.272727, 4.5, 24.5, 44.5, 4.6923077, 54.692308, 104.69231, 4, 24, 44, 4.1818182, 34.181818, 64.181818, 4.3333333, 14.333333, 24.333333, 4.4615385, 14.461538, 24.461538, 4, 45.127323, 86.254645, 4.0909091, 34.090909, 64.090909, 4.1666667, 44.166667, 84.166667, 4.2307692, 36.545958, 68.861146, 4, 4, 4, 4, 14, 24, 4, 34, 64, 4, 4, 4), tolerance = 1e-07)
	expect_equal(x18$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x18$conditionalPowerAchieved[2, ], c(0.22253242, 0.00045258608, 0.093314341, 0.10629785), tolerance = 1e-07)
	expect_equal(x18$conditionalPowerAchieved[3, ], c(0.06946024, 0.023986909, 0.11935011, 0.21831369), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x18), NA)))
	    expect_output(print(x18)$show())
	    invisible(capture.output(expect_error(summary(x18), NA)))
	    expect_output(summary(x18)$show())
	}

	x19 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "all",
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), intersectionTest = "Bonferroni", 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x19' with expected results
	expect_equal(x19$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x19$iterations[2, ], c(6, 7, 9, 8))
	expect_equal(x19$iterations[3, ], c(4, 5, 7, 8))
	expect_equal(unlist(as.list(x19$selectedArms)), c(1, 0.6, 0.4, 1, 0.7, 0.5, 1, 0.9, 0.7, 1, 0.8, 0.8, 1, 0.6, 0.4, 1, 0.7, 0.5, 1, 0.9, 0.7, 1, 0.8, 0.8, 1, 0.6, 0.4, 1, 0.7, 0.5, 1, 0.9, 0.7, 1, 0.8, 0.8, 1, 0.6, 0.4, 1, 0.7, 0.5, 1, 0.9, 0.7, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x19$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x19$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x19$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x19$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x19$rejectAtLeastOne, c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x19$futilityPerStage[1, ], c(0.4, 0.3, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x19$futilityPerStage[2, ], c(0.2, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x19$futilityStop, c(0.6, 0.5, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x19$earlyStop[1, ], c(0.4, 0.3, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x19$earlyStop[2, ], c(0.2, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x19$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x19$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x19$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x19$eventsPerStage)), c(4, 38.084882, 69.212204, 4.2727273, 43.354667, 86.08194, 4.5, 49.334675, 94.122115, 4.6923077, 51.615385, 98.538462, 4, 38.084882, 69.212204, 4.1818182, 42.432228, 84.250409, 4.3333333, 47.507465, 90.636111, 4.4615385, 49.076923, 93.692308, 4, 38.084882, 69.212204, 4.0909091, 41.509788, 82.418879, 4.1666667, 45.680255, 87.150106, 4.2307692, 46.538462, 88.846154, 4, 38.084882, 69.212204, 4, 40.587348, 80.587348, 4, 43.853045, 83.664102, 4, 44, 84), tolerance = 1e-07)
	expect_equal(x19$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x19$conditionalPowerAchieved[2, ], c(0.25302206, 0.12377047, 0.099299805, 0.13107434), tolerance = 1e-07)
	expect_equal(x19$conditionalPowerAchieved[3, ], c(0.035478017, 0.14956549, 0.19989626, 0.2482574), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x19), NA)))
	    expect_output(print(x19)$show())
	    invisible(capture.output(expect_error(summary(x19), NA)))
	    expect_output(summary(x19)$show())
	}

	x20 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), intersectionTest = "Bonferroni",
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x20' with expected results
	expect_equal(x20$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x20$iterations[2, ], c(9, 9, 8, 9))
	expect_equal(x20$iterations[3, ], c(6, 2, 4, 4))
	expect_equal(unlist(as.list(x20$selectedArms)), c(1, 0.7, 0.6, 1, 0.4, 0.1, 1, 0.3, 0.3, 1, 0.4, 0.2, 1, 0.4, 0.2, 1, 0.3, 0.1, 1, 0.4, 0.2, 1, 0.5, 0.1, 1, 0.4, 0.2, 1, 0.4, 0.1, 1, 0.6, 0.2, 1, 0.4, 0.2, 1, 0.3, 0.2, 1, 0.7, 0.1, 1, 0.3, 0.1, 1, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x20$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x20$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x20$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x20$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x20$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(x20$futilityPerStage[1, ], c(0.1, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x20$futilityPerStage[2, ], c(0.3, 0.7, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x20$futilityStop, c(0.4, 0.8, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x20$earlyStop[1, ], c(0.1, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x20$earlyStop[2, ], c(0.3, 0.7, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x20$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x20$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x20$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x20$eventsPerStage)), c(4, 55.301413, 121.14242, 4.2727273, 35.109627, 69.66845, 4.5, 31.157047, 84.47114, 4.6923077, 37.286894, 74.268816, 4, 33.079191, 54.475755, 4.1818182, 27.077441, 61.925926, 4.3333333, 39.018018, 73.252252, 4.4615385, 44.305371, 62.895114, 4, 33.62963, 55.851852, 4.0909091, 34.520773, 67.609009, 4.1666667, 55.585332, 88.924592, 4.2307692, 34.45128, 68.496811, 4, 26.222222, 48.444444, 4, 55.972616, 89.305949, 4, 29.250965, 45.467181, 4, 40.508627, 89.936989), tolerance = 1e-07)
	expect_equal(x20$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x20$conditionalPowerAchieved[2, ], c(0.20029828, 0.023397899, 0.12032566, 0.11581147), tolerance = 1e-07)
	expect_equal(x20$conditionalPowerAchieved[3, ], c(0.23075304, 0.0041701775, 0.31504523, 0.2540259), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x20), NA)))
	    expect_output(print(x20)$show())
	    invisible(capture.output(expect_error(summary(x20), NA)))
	    expect_output(summary(x20)$show())
	}

	x21 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), intersectionTest = "Bonferroni",
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x21' with expected results
	expect_equal(x21$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x21$iterations[2, ], c(9, 8, 6, 8))
	expect_equal(x21$iterations[3, ], c(6, 3, 3, 3))
	expect_equal(unlist(as.list(x21$selectedArms)), c(1, 0.5, 0.5, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.5, 0.4, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.1, 1, 0.2, 0.1, 1, 0.4, 0.1, 1, 0.1, 0, 1, 0.2, 0, 1, 0.2, 0, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x21$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x21$numberOfActiveArms[2, ], c(1.4444444, 1.125, 1, 1.375), tolerance = 1e-07)
	expect_equal(x21$numberOfActiveArms[3, ], c(1.6666667, 1, 1, 2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x21$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x21$rejectAtLeastOne, c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x21$futilityPerStage[1, ], c(0.1, 0.2, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x21$futilityPerStage[2, ], c(0.3, 0.5, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x21$futilityStop, c(0.4, 0.7, 0.7, 0.7), tolerance = 1e-07)
	expect_equal(x21$earlyStop[1, ], c(0.1, 0.2, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x21$earlyStop[2, ], c(0.3, 0.5, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x21$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x21$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x21$eventsPerStage)), c(4, 32.418519, 75.046298, 4.2727273, 41.772727, 108.43939, 4.5, 21.166667, 54.5, 4.6923077, 39.105888, 97.542102, 4, 32.418519, 58.379632, 4.1818182, 16.681818, 16.681818, 4.3333333, 21, 54.333333, 4.4615385, 17.297771, 18.194391, 4, 26.222222, 42.888889, 4.0909091, 34.523544, 59.266981, 4.1666667, 25.707065, 59.040399, 4.2307692, 42.049611, 42.899854, 4, 15.111111, 15.111111, 4, 24.961538, 24.961538, 4, 37.333333, 37.333333, 4, 12.326141, 34.529185), tolerance = 1e-07)
	expect_equal(x21$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x21$conditionalPowerAchieved[2, ], c(0.25987482, 0.19826206, 0.15640007, 0.12642857), tolerance = 1e-07)
	expect_equal(x21$conditionalPowerAchieved[3, ], c(0.048082009, 0.075639723, 0.28252555, 0.2847753), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x21), NA)))
	    expect_output(print(x21)$show())
	    invisible(capture.output(expect_error(summary(x21), NA)))
	    expect_output(summary(x21)$show())
	}

	x22 <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0.1, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.1, 0.3, 0.1), intersectionTest = "Bonferroni",
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x22' with expected results
	expect_equal(x22$iterations[1, ], c(10, 10, 10))
	expect_equal(x22$iterations[2, ], c(10, 9, 9))
	expect_equal(x22$iterations[3, ], c(10, 4, 5))
	expect_equal(unlist(as.list(x22$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.3, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.4, 0.2, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x22$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x22$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x22$numberOfActiveArms[3, ], c(1, 1, 1))
	expect_equal(unlist(as.list(x22$rejectedArmsPerStage)), c(0, 0, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x22$rejectAtLeastOne, c(0.5, 0, 0.3), tolerance = 1e-07)
	expect_equal(x22$futilityPerStage[1, ], c(0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x22$futilityPerStage[2, ], c(0, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(x22$futilityStop, c(0, 0.6, 0.5), tolerance = 1e-07)
	expect_equal(x22$earlyStop[1, ], c(0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x22$earlyStop[2, ], c(0, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(x22$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x22$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x22$successPerStage[3, ], c(0.5, 0, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x22$eventsPerStage)), c(6.4545455, 32.47393, 52.864258, 6, 50.444444, 64.82931, 5.6153846, 35.692605, 70.601287, 5.6363636, 15.636364, 16.036364, 5.3333333, 38.666667, 45.950971, 5.0769231, 16.188034, 16.988034, 4.8181818, 15.218182, 16.018182, 4.6666667, 15.777778, 15.777778, 4.5384615, 48.982906, 69.782906, 4, 4, 4, 4, 15.111111, 15.111111, 4, 15.111111, 15.111111), tolerance = 1e-07)
	expect_equal(x22$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x22$conditionalPowerAchieved[2, ], c(0.58322884, 0.02361667, 0.20366982), tolerance = 1e-07)
	expect_equal(x22$conditionalPowerAchieved[3, ], c(0.8691289, 0.8, 0.6808749), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x22), NA)))
	    expect_output(print(x22)$show())
	    invisible(capture.output(expect_error(summary(x22), NA)))
	    expect_output(summary(x22)$show())
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
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.2, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.3, 0.2, 0.1, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(5.6153846, 8.0153846, 27.815385, 5.2857143, 6.8857143, 20.085714, 5, 6.2, 16.1, 4.75, 6.75, 23.25, 5.0769231, 5.4769231, 8.7769231, 4.8571429, 5.6571429, 12.257143, 4.6666667, 5.0666667, 8.3666667, 4.5, 4.9, 8.2, 4.5384615, 5.7384615, 15.638462, 4.4285714, 5.6285714, 15.528571, 4.3333333, 6.3333333, 22.833333, 4.25, 5.85, 19.05, 4, 4, 4, 4, 4.4, 7.7, 4, 4.4, 7.7, 4, 4, 4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.14134075, 0.0028918831, 0.010226393, 0.055484194), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.53336741, 0.20111077, 0.092533335, 0.072083235), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
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
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.2, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.2, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(5.6153846, 25.615385, 45.615385, 5.2857143, 25.285714, 45.285714, 5, 25, 45, 4.75, 24.75, 44.75, 5.0769231, 5.0769231, 5.0769231, 4.8571429, 4.8571429, 4.8571429, 4.6666667, 4.6666667, 4.6666667, 4.5, 4.5, 4.5, 4.5384615, 4.5384615, 4.5384615, 4.4285714, 4.4285714, 4.4285714, 4.3333333, 4.3333333, 4.3333333, 4.25, 4.25, 4.25, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.32045131, 0.003073154, 0.032400891, 0.10136911), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.41019028, 0.18490085, 0.061049831, 0.059505076), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
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
		intersectionTest = "Sidak", typeOfSelection = "rbest", rValue = 2, threshold = -Inf, successCriterion = "all", maxNumberOfIterations = 100, seed = 3456)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(55, 73, 79, 87))
	expect_equal(x$iterations[3, ], c(43, 67, 75, 82))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.35, 0.29, 1, 0.35, 0.34, 1, 0.42, 0.4, 1, 0.32, 0.3, 1, 0.36, 0.3, 1, 0.53, 0.47, 1, 0.51, 0.48, 1, 0.65, 0.62, 1, 0.39, 0.27, 1, 0.58, 0.53, 1, 0.65, 0.62, 1, 0.77, 0.72), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0.01, 0, 0.01, 0.02, 0.01, 0.03, 0, 0.02, 0.01, 0.02, 0, 0, 0.01, 0.02, 0.02, 0.01, 0.05, 0.02, 0.05, 0.05, 0.03, 0.03, 0, 0, 0.01, 0.02, 0.01, 0.04, 0.02, 0.05, 0.05, 0.09, 0.08), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.05, 0.11, 0.22, 0.32), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.45, 0.27, 0.21, 0.13), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.12, 0.06, 0.03, 0.01), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.57, 0.33, 0.24, 0.14), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.45, 0.27, 0.21, 0.13), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.12, 0.06, 0.04, 0.05), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.01, 0.04), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.02, 0.02), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(25, 46.212121, 68.692741, 23.702032, 39.111063, 55.417803, 22.633745, 39.154866, 55.72608, 21.73913, 32.837071, 43.881278, 25, 46.818182, 70.073996, 24.266366, 47.867469, 70.679294, 23.662551, 44.201391, 64.559653, 23.1569, 46.320844, 69.773989, 25, 48.636364, 69.566596, 24.604966, 50.7485, 76.790505, 24.279835, 51.12776, 78.105882, 24.007561, 52.492972, 80.744145), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.061965837, 0.11411982, 0.22216197, 0.25194938), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.099350044, 0.22362844, 0.31614669, 0.44420837), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
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
	expect_equal(comp1, c(0, -0.06, 0.02), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0))
	expect_equal(comp2[2, ], c(0, 0.02, 0.02), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0, -0.08, 0), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.01, -0.03, 0), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0.02, 0.02, 0), tolerance = 1e-07)

	comp4 <- round(y$eventsPerStage - x$eventsPerStage[, , 1], 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0))
	expect_equal(comp4[2, ], c(1, -0.6, 6.7), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(1, 0.4, 8.7), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-3.2, 1.7, 5), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.5, -0.72, -0.55), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.09, -0.28, -0.03), tolerance = 1e-07)

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
	expect_equal(comp1, c(-0.03, -0.08, -0.01), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(-0.02, 0.03, -0.11), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(0, 0.01, 0.07), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(-0.01, -0.12, 0.03), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.05, 0.01, 0.02), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(-0.01, 0.01, -0.01), tolerance = 1e-07)

	comp4 <- round(y$eventsPerStage - x$eventsPerStage[, , 1], 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0))
	expect_equal(comp4[2, ], c(-0.6, 1.7, -0.3), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(-0.6, 1.7, -0.3), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-5.3, -8.6, 11.5), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.35, -0.44, -0.22), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.16, -0.54, -0.24), tolerance = 1e-07)

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
	expect_equal(comp1, c(-0.01, -0.04, 0), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0))
	expect_equal(comp2[2, ], c(0, 0, 0))
	expect_equal(comp2[3, ], c(-0.01, -0.04, 0), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0, 0, 0))
	expect_equal(comp3[2, ], c(0, 0, 0))

	comp4 <- round(y$eventsPerStage - x$eventsPerStage[, , 1], 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0))
	expect_equal(comp4[2, ], c(0, 0, 0))
	expect_equal(comp4[3, ], c(-0.9, -4.3, 1.7), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfEvents - x$expectedNumberOfEvents, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-0.9, -4.3, 1.7), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(0, 0, 0))
	expect_equal(comp6[2, ], c(0, 0, 0))

})

