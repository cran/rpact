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
#:#  Creation date: 05 September 2020, 14:49:19
#:#  File version: $Revision: 3588 $
#:#  Last changed: $Date: 2020-09-04 09:47:38 +0200 (Fri, 04 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing simulation multi-arm survival function")


test_that("'getSimulationMultiArmSurvival': several configurations", {
	.skipTestIfDisabled()

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 8, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0, 0, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(219.04134, 185.26099, 136.84329, 136.13817), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 20, 30, 10, 40, 70, 10, 10, 10, 10, 28.095884, 50.318106, 10, 41.226834, 81.226834, 10, 20.813802, 33.022835, 10, 20, 32.5, 10, 20, 31.111111, 10, 54.240592, 97.814507, 10, 19.178082, 49.178082, 10, 17.233522, 39.311599, 10, 18.955617, 32.252627, 10, 10, 10, 10, 21.238536, 33.060076, 10, 43.014904, 66.680413, 10, 23.584027, 27.512172), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0040144461, 0.013155969, 0.038834319, 0.024303571), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.14953987, 0.22058624, 0.61704823, 0.53757373), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms =  4, 
		plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10))
	expect_equal(x$iterations[2, ], c(10, 10))
	expect_equal(x$iterations[3, ], c(1, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.2, 0, 1, 0.5, 0.3, 1, 0.3, 0, 1, 0.1, 0, 1, 0.5, 0.1, 1, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0.9, 0.5), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.9, 0.5), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.9, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0))
	expect_equal(x$expectedNumberOfEvents, c(150, 190))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 10, 10, 10, 10, 10, 10, 30, 30, 10, 60, 120, 10, 40, 40, 10, 20, 20, 10, 60, 160, 10, 50, 90))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(4.3545167e-12, 0.0001035465), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0, 1.5571766e-11), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(219.04134, 187.39531, 154.59865, 159.09457), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 20, 30, 10, 40, 70, 10, 20, 30, 10, 26.299147, 48.52137, 10, 41.226834, 81.226834, 10, 26.222891, 48.431924, 10, 20, 30, 10, 20, 31.111111, 10, 54.240592, 97.814507, 10, 14.477125, 34.477125, 10, 18.472491, 41.618839, 10, 21.409885, 40.292149, 10, 10, 10, 10, 21.465612, 34.486259, 10, 34.071665, 52.979811, 10, 30, 45.990554), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0040144461, 0.012285229, 0.031193185, 0.024468099), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.14953987, 0.20001033, 0.55735211, 0.505358), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.2, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.2, 0.3, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.4, 0, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfEvents, c(840, 840, 840, 754.03947), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 188.50987, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 188.50987, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 188.50987, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 188.50987), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0077846876, 0.053103546, 0.0075779219, 0.13483355), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.029928092, 0.12000739, 0.33951852, 0.5406732), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.3, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(408.09348, 397.65658, 369.92322, 329.43346), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 74.464805, 138.35949, 10, 34.054895, 64.054895, 10, 20, 30, 10, 38.47081, 56.451283, 10, 55.687249, 105.68725, 10, 40, 70, 10, 54.677643, 114.67764, 10, 43.329452, 57.886492, 10, 54.464805, 98.359488, 10, 62.283994, 114.7734, 10, 63.220756, 134.96161, 10, 53.308406, 84.272031, 10, 35.687249, 65.687249, 10, 76.338889, 148.82829, 10, 48.543113, 90.283967, 10, 88.449764, 130.82365), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.006595444, 0.03494985, 0.049942509, 0.0324581), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.11132692, 0.15941421, 0.40883057, 0.61449939), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.6, 1, 1.2, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.3, 1, 1, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(282.85706, 224.11926, 206.44462, 154.26943), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 34.464805, 44.464805, 10, 31.700792, 61.700792, 10, 27.008523, 47.008523, 10, 30, 50, 10, 60, 100, 10, 22.418466, 42.418466, 10, 32.678076, 52.678076, 10, 24.225697, 45.572343, 10, 47.834174, 82.293664, 10, 20, 30, 10, 23.097162, 37.87889, 10, 20, 24.024955, 10, 31.226834, 56.098591, 10, 50, 90, 10, 38.479126, 68.879126, 10, 23.154345, 34.672129), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.004763666, 0.003640867, 0.0082005814, 0.053819747), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.22046701, 0.16045236, 0.32678673, 0.52126421), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 8, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0, 0, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(210.93485, 162.46084, 129.36867, 140.17765), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 20, 30, 10, 40, 70, 10, 10, 10, 10, 28.095884, 46.987528, 10, 41.226834, 72.453667, 10, 20.813802, 31.627605, 10, 20, 32.5, 10, 20, 31.111111, 10, 54.240592, 98.481183, 10, 19.178082, 28.356164, 10, 17.233522, 24.865197, 10, 18.955617, 28.906303, 10, 10, 10, 10, 21.238536, 32.477072, 10, 43.014904, 71.783534, 10, 23.584027, 38.677391), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0040144461, 0.013155969, 0.038834319, 0.024303571), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.095284355, 0.15158168, 0.52996124, 0.53841276), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.2, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.2, 0.3, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.4, 0, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfEvents, c(840, 840, 840, 840))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0077846876, 0.053103546, 0.0075779219, 0.13483355), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.029928092, 0.12000739, 0.33951852, 0.55492711), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.2, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(400.60822, 385.35555, 332.88302, 407.11686), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 74.464805, 138.92961, 10, 34.054895, 58.10979, 10, 20, 30, 10, 38.47081, 66.94162, 10, 55.687249, 101.3745, 10, 40, 70, 10, 54.677643, 99.355287, 10, 43.329452, 76.658904, 10, 54.464805, 98.929609, 10, 62.283994, 114.56799, 10, 63.220756, 116.44151, 10, 53.308406, 96.616812, 10, 35.687249, 61.374499, 10, 76.338889, 142.67778, 10, 48.543113, 87.086226, 10, 88.449764, 166.89953), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.006595444, 0.03494985, 0.049942509, 0.0324581), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10512619, 0.15327386, 0.37009896, 0.67691803), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 9, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.6, 0.6, 1, 0, 0, 1, 0, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.4, 1, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5, 1, 1.1, 1.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.5, 1, 1.1111111, 1.1111111), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0, 0, 0, 0.4), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.2, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.2, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(291.21286, 225.45788, 158.48946, 159.44243), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 30, 50, 10, 32.72894, 55.457881, 10, 28.133299, 48.281409, 10, 21.093404, 32.204515, 10, 63.369369, 116.73874, 10, 10, 10, 10, 10, 10, 10, 20, 31.111111, 10, 40, 70, 10, 40, 70, 10, 23.096528, 37.648226, 10, 22.369971, 36.114383, 10, 32.23706, 54.474119, 10, 50, 90, 10, 43.014904, 68.58702, 10, 36.804544, 66.587371), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0084959046, 0.0013101159, 0.035600052, 0.03020885), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.14139131, 0.28227908, 0.47205287, 0.64361749), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10))
	expect_equal(x$iterations[3, ], c(0, 0, 1))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.4, 0, 1, 0.5, 0, 1, 0.4, 0, 1, 0.5, 0, 1, 0.4, 0, 1, 0.5, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, NaN, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0))
	expect_equal(x$expectedNumberOfEvents, c(NaN, NaN, 150))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 20, 20, 10, 20, 20, 10, 20, 120, 10, 50, 50, 10, 60, 60, 10, 50, 50, 10, 60, 60, 10, 50, 50, 10, 60, 60))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(4.3545167e-12, 4.3545167e-12, 4.3545167e-12), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, NaN, 0))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(240, 240, 240, 231.73187), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 20, 30, 10, 50, 90, 10, 20, 30, 10, 50, 90, 10, 50, 90, 10, 40, 70, 10, 20, 30, 10, 20, 30, 10, 60, 110, 10, 30, 50, 10, 50, 90, 10, 50, 81.731873, 10, 10, 10, 10, 20, 30, 10, 50, 90, 10, 20, 30), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.040648481, 0.00059182703, 0.012440614, 0.036890936), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.063939122, 0.026726976, 0.12663266, 0.28411905), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined",
		activeArms = 4, directionUpper = FALSE, threshold = 0, 
		plannedEvents = c(10, 30, 50), adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10))
	expect_equal(x$iterations[2, ], c(10, 10))
	expect_equal(x$iterations[3, ], c(5, 1))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.2, 0, 1, 0.5, 0, 1, 0.3, 0.2, 1, 0.1, 0, 1, 0.5, 0.3, 1, 0.4, 0.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0.2, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.5, 0, 0, 0.1, 0.1, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(1, 1))
	expect_equal(x$futilityPerStage[1, ], c(0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0))
	expect_equal(x$futilityStop, c(0, 0))
	expect_equal(x$earlyStop, c(0.5, 0.9), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0.5, 0.9), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(58.931329, 125.65295), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 10, 10, 10, 10, 10, 10, 20.4, 20.4, 10, 53.667846, 53.667846, 10, 13.808648, 15.408648, 10, 20, 20, 10, 12.722681, 15.122681, 10, 41.585106, 45.585106), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.80490553, 0.42880816), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.96093208, 0.99690158), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(240, 240, 240, 240))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 20, 30, 10, 40, 70, 10, 20, 30, 10, 40, 70, 10, 50, 90, 10, 40, 70, 10, 20, 30, 10, 20, 30, 10, 60, 110, 10, 30, 50, 10, 50, 90, 10, 50, 90, 10, 10, 10, 10, 30, 50, 10, 50, 90, 10, 30, 50))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.040648481, 0.00053668018, 0.0084687327, 0.032911913), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.063939122, 0.037232144, 0.090757658, 0.28203354), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "all", 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.4, 0, 0.3, 0.7), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.6, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(840, 840, 801.6, 779.69855), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 110, 210, 10, 110, 210, 10, 110, 200.4, 10, 110, 194.92464, 10, 110, 210, 10, 110, 210, 10, 110, 200.4, 10, 110, 194.92464, 10, 110, 210, 10, 110, 210, 10, 110, 200.4, 10, 110, 194.92464, 10, 110, 210, 10, 110, 210, 10, 110, 200.4, 10, 110, 194.92464), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.03718888, 0.0091707639, 0.0080589326, 0.017848328), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0020322806, 0.075808954, 0.11582242, 0.30323624), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.2, 0.2, 1, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(440, 440, 404.82896, 405.01683), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 80, 150, 10, 60, 110, 10, 60, 110, 10, 50, 90, 10, 60, 110, 10, 50, 90, 10, 80, 132.41448, 10, 60, 101.53864, 10, 60, 110, 10, 60, 110, 10, 70, 112.41448, 10, 60, 92.508414, 10, 40, 70, 10, 70, 130, 10, 30, 50, 10, 70, 120.96977), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.043172495, 0.0055002571, 0.004353596, 0.0039892404), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10191611, 0.017694103, 0.2183846, 0.28636575), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.1, 1, 0.4, 0.3, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.4, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.1, 0, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.6, 0.4, 1, 0.4, 0.3, 1, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.6, 1.4, 1.3, 1.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.3, 1.1, 1, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfEvents, c(322.19465, 290, 268.40141, 242.13187), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 50, 10, 50, 80, 10, 40, 60, 10, 40, 70, 10, 56.097325, 96.097325, 10, 20, 30, 10, 60, 110, 10, 40, 70, 10, 60, 110, 10, 40, 70, 10, 20, 20, 10, 30, 41.731873, 10, 36.097325, 66.097325, 10, 70, 110, 10, 50, 78.401409, 10, 50, 60.4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.10750304, 0.0051389106, 0.077807582, 0.039293221), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.02253308, 0.069088153, 0.24500784, 0.41820469), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(240, 240, 240, 240))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 20, 30, 10, 50, 90, 10, 20, 30, 10, 50, 90, 10, 50, 90, 10, 40, 70, 10, 20, 30, 10, 20, 30, 10, 60, 110, 10, 30, 50, 10, 50, 90, 10, 50, 90, 10, 10, 10, 10, 20, 30, 10, 50, 90, 10, 20, 30))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.040648481, 0.00059182703, 0.012440614, 0.036890936), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.063939122, 0.026726976, 0.12663266, 0.30032961), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "all",
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.4, 0, 0.3, 0.7), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.6, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(840, 840, 840, 840))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 110, 210))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.03718888, 0.0091707639, 0.0080589326, 0.017848328), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0020322806, 0.075808954, 0.12961552, 0.32677376), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.2, 0.2, 1, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(440, 440, 440, 440))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 80, 150, 10, 60, 110, 10, 60, 110, 10, 50, 90, 10, 60, 110, 10, 50, 90, 10, 80, 150, 10, 60, 110, 10, 60, 110, 10, 60, 110, 10, 70, 130, 10, 60, 110, 10, 40, 70, 10, 70, 130, 10, 30, 50, 10, 70, 130))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.043172495, 0.0055002571, 0.004353596, 0.0039892404), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10191611, 0.017694103, 0.23796628, 0.3230194), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedEvents = c(10, 30, 50), omegaMaxVector = 1/seq(1, 1.6, 0.2), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.6, 0.6, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5, 1.1, 1.2, 1.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.5, 1.1, 1.2, 1.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(324.3893, 260, 280, 280), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 30, 50, 10, 20, 30, 10, 60, 110, 10, 50, 90, 10, 66.097325, 122.19465, 10, 30, 50, 10, 40, 70, 10, 40, 70, 10, 40, 70, 10, 50, 90, 10, 20, 30, 10, 30, 50, 10, 46.097325, 82.194649, 10, 50, 90, 10, 40, 70, 10, 40, 70), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.10750304, 0.002280546, 0.041649467, 0.032378431), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.020738286, 0.017807895, 0.18840502, 0.29860077), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms = 4, directionUpper = FALSE,threshold = 0.1, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100), 
		maxNumberOfIterations = 1)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(1, 1, 1))
	expect_equal(x$iterations[2, ], c(1, 1, 1))
	expect_equal(x$iterations[3, ], c(1, 1, 0))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, NaN))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(1, 0, 1))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 1))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 1))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0))
	expect_equal(x$expectedNumberOfEvents, c(48, 89.570149, NaN), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 10, 10, 10, 49.456184, 59.570149, 10, 10, 10, 10, 14, 18, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 110, 110, 10, 10, 10, 10, 10, 10, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.91037084, 0.8, 0.066139734), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.9249643, 0.8, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmSurvival': using calcSubjectsFunction", {

	.skipTestIfDisabled()

	myFunction <- function(..., stage, minNumberOfEventsPerStage) {
		return(ifelse(stage == 3, 33, minNumberOfEventsPerStage[stage]))
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),directionUpper = FALSE,
		minNumberOfEventsPerStage = c(10, 4, 4), maxNumberOfEventsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10, calcEventsFunction = myFunction)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(54, 54, 54, 54))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 13, 14.2, 10, 14, 15.6, 10, 13, 14.2, 10, 14, 15.6, 10, 12, 12.8, 10, 13, 14.2, 10, 12, 12.8, 10, 11, 11.4, 10, 15, 17, 10, 12, 12.8, 10, 14, 15.6, 10, 14, 15.6, 10, 10, 10, 10, 11, 11.4, 10, 11, 11.4, 10, 11, 11.4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.19942446, 0.009253815, 0.015465934, 0.038553587), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.49696611, 0.16061515, 0.045987685, 0.062151038), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmSurvival': using selectArmsFunction", {

	.skipTestIfDisabled()

	mySelectionFunction <- function(effectSizes) {
		return(c(TRUE, FALSE, FALSE, FALSE))
	}

	x <- getSimulationMultiArmSurvival(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedEvents = c(10, 30, 50), omegaMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),directionUpper = FALSE,
		maxNumberOfIterations = 10, selectArmsFunction = mySelectionFunction, typeOfSelection = "userDefined")

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(80, 80, 80, 80))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 30, 50, 10, 30, 50, 10, 30, 50, 10, 30, 50, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.27424058, 0.0095374209, 0.02301013, 0.038606887), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.5520944, 0.25458843, 0.083408128, 0.086653854), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmSurvival': typeOfShape = sigmoidEmax", {

	.skipTestIfDisabled()

	designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
	x <- getSimulationMultiArmSurvival(designIN, activeArms = 3, typeOfShape = "sigmoidEmax", omegaMaxVector = seq(1, 1.9, 0.3), gED50 = 2, plannedEvents = cumsum(rep(50, 3)), 
		intersectionTest = "Sidak", typeOfSelection = "rbest", rValue = 2, threshold = -Inf, successCriterion = "all", maxNumberOfIterations = 100, seed = 3456)

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(55, 79, 90, 97))
	expect_equal(x$iterations[3, ], c(43, 75, 86, 86))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.39, 0.31, 1, 0.47, 0.44, 1, 0.5, 0.46, 1, 0.49, 0.45, 1, 0.34, 0.29, 1, 0.59, 0.56, 1, 0.58, 0.56, 1, 0.7, 0.61, 1, 0.37, 0.26, 1, 0.52, 0.5, 1, 0.72, 0.7, 1, 0.75, 0.66), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0.01, 0.01, 0.01, 0.03, 0.03, 0.05, 0.02, 0.06, 0.05, 0.02, 0, 0, 0.01, 0.02, 0.03, 0.02, 0.1, 0.04, 0.09, 0.13, 0.15, 0.03, 0, 0, 0.01, 0.02, 0.03, 0.05, 0.04, 0.1, 0.11, 0.19, 0.17), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.05, 0.12, 0.38, 0.68), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.45, 0.21, 0.1, 0.03), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.12, 0.04, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.57, 0.25, 0.1, 0.03), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.57, 0.25, 0.14, 0.14), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.04, 0.11), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.03, 0.04, 0.18), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfEvents, c(248, 304, 326, 333))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(50, 85.454545, 121.50106, 50, 79.746835, 109.08017, 50, 77.777778, 104.52196, 50, 75.257732, 101.42052, 50, 80.909091, 114.63002, 50, 87.341772, 124.67511, 50, 82.222222, 114.78036, 50, 86.082474, 121.54759, 50, 83.636364, 113.86892, 50, 82.911392, 116.24473, 50, 90, 130.69767, 50, 88.659794, 127.03189), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.04194117, 0.057281643, 0.12281909, 0.2476561), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.065054916, 0.19928511, 0.29372881, 0.49917239), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, inverse normal design", {

	.skipTestIfDisabled()

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

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(-0.03, -0.01, -0.02), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, Fisher design", {

	.skipTestIfDisabled()

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

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(-0.02, -0.06, 0.03), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmSurvival': comparison of base and multi-arm, inverse normal design with user alpha spending", {

	.skipTestIfDisabled()

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

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(0, 0, 0))

})

test_that("'getSimulationMultiArmSurvival': omegaMaxVector with length 1", {

	.skipTestIfDisabled()

	sim <- getSimulationMultiArmSurvival(getDesignInverseNormal(futilityBounds = c(-0.5, 0.5)), plannedEvents = c(20,40,60),
		allocationRatioPlanned = 2,
		activeArms = 1,
		thetaH1 = c(1.5),
		conditionalPower = 0.8,
		directionUpper = FALSE,
		typeOfShape = "sigmoidEmax",
		gED50 = 2,
		typeOfSelection = "epsilon",
		epsilonValue = 0.4,
		omegaMaxVector = 2/3,
		minNumberOfEventsPerStage = c(NA, 1, 1),
		maxNumberOfEventsPerStage = c(NA, 400, 400),
		maxNumberOfIterations = 10, 
		seed = 1234
	) 

	## Comparison of the results of SimulationResultsMultiArmSurvival object 'sim' with expected results
	expect_equal(sim$iterations[1, ], 10)
	expect_equal(sim$iterations[2, ], 10)
	expect_equal(sim$iterations[3, ], 9)
	expect_equal(unlist(as.list(sim$selectedArms)), c(1, 1, 0.9), tolerance = 1e-07)
	expect_equal(sim$numberOfActiveArms[1, ], 1)
	expect_equal(sim$numberOfActiveArms[2, ], 1)
	expect_equal(sim$numberOfActiveArms[3, ], 1)
	expect_equal(unlist(as.list(sim$rejectedArmsPerStage)), c(0, 0, 0.4), tolerance = 1e-07)
	expect_equal(sim$rejectAtLeastOne, 0.4, tolerance = 1e-07)
	expect_equal(sim$futilityPerStage[1, ], 0)
	expect_equal(sim$futilityPerStage[2, ], 0.1, tolerance = 1e-07)
	expect_equal(sim$futilityStop, 0.1, tolerance = 1e-07)
	expect_equal(sim$earlyStop, 0.1, tolerance = 1e-07)
	expect_equal(sim$successPerStage[1, ], 0)
	expect_equal(sim$successPerStage[2, ], 0)
	expect_equal(sim$successPerStage[3, ], 0.4, tolerance = 1e-07)
	expect_equal(sim$expectedNumberOfEvents, 399.24507, tolerance = 1e-07)
	expect_equal(unlist(as.list(sim$eventsPerStage)), c(20, 300.81497, 410.18175), tolerance = 1e-07)
	expect_equal(sim$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(sim$conditionalPowerAchieved[2, ], 0.00047193263, tolerance = 1e-07)
	expect_equal(sim$conditionalPowerAchieved[3, ], 0.017841283, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(sim), NA)))
	    expect_output(print(sim)$show())
	    invisible(capture.output(expect_error(summary(sim), NA)))
	    expect_output(summary(sim)$show())
	}

})

