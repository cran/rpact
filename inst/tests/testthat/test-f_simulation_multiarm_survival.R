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
#:#  Creation date: 25 September 2020, 12:07:37
#:#  File version: $Revision$
#:#  Last changed: $Date$
#:#  Last changed by: $Author$
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
	expect_equal(x$iterations[3, ], c(10, 10, 8, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.5, 0.5, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 70, 10, 30, 50, 10, 20, 32.5, 10, 20, 32.5, 10, 22.556364, 42.556364, 10, 20.810691, 33.005281, 10, 20, 32.5, 10, 10.989045, 10.989045, 10, 55.655937, 100.79869, 10, 34.25202, 74.25202, 10, 18.854235, 43.485971, 10, 28.929811, 56.135113, 10, 10, 10, 10, 21.956943, 35.19828, 10, 32.880115, 45.639772, 10, 40, 54.655239), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0040144467, 0.039064805, 0.040169929, 0.082418629), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0888861, 0.20541704, 0.6152189, 0.54917808), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(8, 10))
	expect_equal(x$iterations[3, ], c(0, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0, 1, 0.3, 0, 1, 0.5, 0, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.8, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(1, 0.4), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.8, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 35, 35, 10, 40, 40, 10, 72.5, 72.5, 10, 50, 116.66667, 10, 22.5, 22.5, 10, 20, 20, 10, 10, 10, 10, 30, 63.333333), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(4.3545167e-12, 0.0001035465), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, 1.2976472e-11), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 9, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.5, 0.5, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 70, 10, 40, 70, 10, 20, 31.111111, 10, 20, 31.111111, 10, 22.556364, 42.556364, 10, 20.79629, 32.924676, 10, 20, 31.111111, 10, 20.864019, 31.975131, 10, 55.655937, 100.79869, 10, 35.090378, 75.090378, 10, 19.788923, 45.55696, 10, 29.408026, 59.399187, 10, 10, 10, 10, 12.281844, 18.068091, 10, 33.614508, 54.096928, 10, 30, 46.171909), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0040144467, 0.036264199, 0.0353593, 0.077835517), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0888861, 0.19216746, 0.50763556, 0.51772249), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.2, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.5, 0, 0.3, 0.3, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0.8, 0, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 176.81212, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 176.81212, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 176.81212, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 176.81212), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.05227804, 0.057630661, 0.055162105, 0.17330249), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.029928092, 0.16168623, 0.38604984, 0.5443008), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0.3, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 74.240592, 137.90699, 10, 43.751312, 83.751312, 10, 30, 50, 10, 33.883442, 50.790012, 10, 55.768646, 105.76865, 10, 40, 70, 10, 49.103049, 101.54814, 10, 42.33947, 56.957772, 10, 54.240592, 97.906994, 10, 53.165888, 93.872787, 10, 44.430244, 96.214131, 10, 48.768985, 80.692557, 10, 35.768646, 65.768646, 10, 76.9172, 147.6241, 10, 45.327194, 84.66599, 10, 80.312958, 122.45428), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0066385265, 0.027571068, 0.072079839, 0.057878923), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.13764048, 0.20424689, 0.39474586, 0.59693221), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 9, 9, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.5, 1, 0.3, 0, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.1, 1, 0.2, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0, 0, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.9, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.1, 1.3, 1, 1.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1.1111111, 1, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0.3, 0, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0.1, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.3, 0.4, 0.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 51.226834, 94.073882, 10, 30.904013, 30.904013, 10, 25.469341, 47.691563, 10, 20, 20, 10, 40, 60, 10, 34.730261, 79.174705, 10, 30, 36.235336, 10, 20.989045, 20.989045, 10, 20, 30, 10, 39.120786, 72.454119, 10, 20, 31.111111, 10, 10, 10, 10, 23.296525, 43.296525, 10, 32.432651, 57.003879, 10, 35.457656, 66.864618, 10, 64.660607, 116.77152), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.018476348, 0.073735096, 0.011285197, 0.12333321), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.081016099, 0.22606309, 0.50605102, 0.52201842), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 8, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.5, 0.5, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.6, 0.5), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 70, 10, 30, 50, 10, 20, 32.5, 10, 20, 32.5, 10, 22.556364, 35.112729, 10, 20.810691, 31.621381, 10, 20, 32.5, 10, 10.989045, 10.989045, 10, 55.655937, 101.31187, 10, 34.25202, 58.504041, 10, 18.854235, 27.419279, 10, 28.929811, 52.592074, 10, 10, 10, 10, 21.956943, 33.913886, 10, 32.880115, 48.980259, 10, 40, 65), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0040144467, 0.039064805, 0.040169929, 0.082418629), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.086645449, 0.15142811, 0.5234086, 0.51377086), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.2, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.5, 0, 0.3, 0.3, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0.8, 0, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 201.3602, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 201.3602, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 201.3602, 10, 110, 210, 10, 110, 210, 10, 110, 210, 10, 105.6801, 201.3602), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.05227804, 0.057630661, 0.055162105, 0.17330249), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.029928092, 0.16168623, 0.38604984, 0.57195335), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.5, 0.5, 1, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0.3, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 74.240592, 138.48118, 10, 43.751312, 77.502623, 10, 30, 50, 10, 33.883442, 57.766885, 10, 55.768646, 101.53729, 10, 40, 70, 10, 49.103049, 88.206098, 10, 42.33947, 74.678939, 10, 54.240592, 98.481183, 10, 53.165888, 96.331776, 10, 44.430244, 78.860487, 10, 48.768985, 87.537971, 10, 35.768646, 61.537293, 10, 76.9172, 143.8344, 10, 45.327194, 80.654389, 10, 80.312958, 150.62592), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0066385265, 0.027571068, 0.072079839, 0.057878923), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.12361019, 0.20494749, 0.34265117, 0.63604925), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 7, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0, 0, 1, 0.3, 0.3, 1, 0, 0, 1, 0.2, 0.2, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.4, 0.1, 1, 0.8, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5, 1.2, 1.2, 1.3), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.5, 1.2, 1.2857143, 1.25), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.3, 0.1, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.6, 0.5), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 70, 130, 10, 20, 30, 10, 20, 34.285714, 10, 17.644178, 27.199402, 10, 54.514774, 99.029548, 10, 10, 10, 10, 34.690683, 69.963088, 10, 10, 10, 10, 30, 50, 10, 54.302045, 98.604089, 10, 37.797078, 77.507189, 10, 21.475452, 23.319766, 10, 20.95774, 31.91548, 10, 51.956943, 93.913886, 10, 41.392332, 55.678047, 10, 62.489959, 109.89655), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0051908509, 0.043855483, 0.032039053, 0.036531317), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10536734, 0.20067749, 0.14175278, 0.55628673), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(0, 0, 2))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.3, 0, 1, 0.1, 0, 1, 0.4, 0, 1, 0.7, 0, 1, 0.8, 0, 1, 0.6, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, NaN, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 20, 20, 10, 10, 10, 10, 40, 40, 10, 20, 20, 10, 50, 50, 10, 80, 80, 10, 90, 90, 10, 70, 170))
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 70, 10, 40, 70, 10, 40, 70, 10, 60, 110, 10, 30, 50, 10, 40, 70, 10, 20, 30, 10, 20, 30, 10, 52.818306, 102.81831, 10, 40, 70, 10, 50, 90, 10, 43.629209, 75.211367, 10, 10, 10, 10, 20, 30, 10, 30, 50, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.12291588, 0.00059182703, 0.060659169, 0.11466106), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.066702179, 0.028733731, 0.12621983, 0.25531242), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(9, 10))
	expect_equal(x$iterations[3, ], c(2, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0, 1, 0.3, 0, 1, 0.5, 0.1, 1, 0.4, 0.2, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0, 0, 1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0.2, 0.3, 0, 0, 0.3, 0, 0.2, 0.4, 0.1, 0, 0.2, 0.2, 0.5, 0, 0.1, 0, 0.1, 0, 0.6, 0, 0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(1, 1))
	expect_equal(x$futilityPerStage[1, ], c(0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0))
	expect_equal(x$futilityStop, c(0, 0))
	expect_equal(x$earlyStop[1, ], c(0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 11.56846, 11.56846, 10, 14.156183, 14.156183, 10, 12.222222, 14.222222, 10, 27.059593, 29.860452, 10, 10.444444, 13.028147, 10, 12.126177, 12.126177, 10, 10, 10, 10, 13.478659, 15.88541), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.93905983, 0.72117856), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.89839646, 0.86232544), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 70, 10, 40, 70, 10, 30, 50, 10, 60, 110, 10, 30, 50, 10, 30, 50, 10, 20, 30, 10, 20, 30, 10, 52.818306, 102.81831, 10, 50, 90, 10, 50, 90, 10, 43.790266, 83.790266, 10, 10, 10, 10, 20, 30, 10, 40, 70, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.12291588, 0.00055794426, 0.073552509, 0.11408393), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.066702179, 0.030131613, 0.099573122, 0.2658895), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0.1, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.5, 0, 0.3, 0.6), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 102.81831, 202.81831, 10, 107.77543, 207.77543, 10, 110, 200.4, 10, 110, 192.14274, 10, 102.81831, 202.81831, 10, 107.77543, 207.77543, 10, 110, 200.4, 10, 110, 192.14274, 10, 102.81831, 202.81831, 10, 107.77543, 207.77543, 10, 110, 200.4, 10, 110, 192.14274, 10, 102.81831, 202.81831, 10, 107.77543, 207.77543, 10, 110, 200.4, 10, 110, 192.14274), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.10942309, 0.085139854, 0.075590006, 0.090816177), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0055662903, 0.095339942, 0.15863843, 0.3274014), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 9, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.5, 1, 0.5, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.6, 1, 0.4, 0.3, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.4, 0.4, 1, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 80, 144.84211, 10, 50, 90, 10, 40, 73.333333, 10, 70, 136.66667, 10, 60, 104.84211, 10, 40, 70, 10, 70, 114.94435, 10, 60, 104.44444, 10, 60, 110, 10, 60, 110, 10, 80, 136.05546, 10, 50, 73.088018, 10, 40, 70, 10, 90, 170, 10, 50, 94.444444, 10, 60, 105.31024), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.072086441, 0.015731878, 0.055497453, 0.089862666), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10991101, 0.044600706, 0.17375545, 0.19820744), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.3, 0.2, 1, 0.6, 0.5, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.3, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.2, 0, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.1, 1.3, 1.2, 1.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1.2, 1, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.1, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 60, 110, 10, 40, 70, 10, 40, 70, 10, 70, 121.83977, 10, 40, 60, 10, 70, 120, 10, 40, 70, 10, 40, 70, 10, 20, 30, 10, 30, 50, 10, 50, 90, 10, 40, 50, 10, 30, 50, 10, 30, 50, 10, 30, 30, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.044557404, 0.012054809, 0.060497638, 0.027282411), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.14040171, 0.019864973, 0.19132166, 0.15196617), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 40, 70, 10, 40, 70, 10, 40, 70, 10, 60, 110, 10, 30, 50, 10, 40, 70, 10, 20, 30, 10, 20, 30, 10, 52.818306, 95.636613, 10, 40, 70, 10, 50, 90, 10, 43.629209, 77.258419, 10, 10, 10, 10, 20, 30, 10, 30, 50, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.12291588, 0.00059182703, 0.060659169, 0.11466106), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.066702179, 0.028733731, 0.12621983, 0.25999579), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0.1, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.5, 0, 0.3, 0.6), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 102.81831, 195.63661, 10, 107.77543, 205.55087, 10, 110, 210, 10, 110, 210, 10, 102.81831, 195.63661, 10, 107.77543, 205.55087, 10, 110, 210, 10, 110, 210, 10, 102.81831, 195.63661, 10, 107.77543, 205.55087, 10, 110, 210, 10, 110, 210, 10, 102.81831, 195.63661, 10, 107.77543, 205.55087, 10, 110, 210, 10, 110, 210), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.10942309, 0.085139854, 0.075590006, 0.090816177), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0055662895, 0.095339941, 0.17288171, 0.35973055), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 9, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.5, 1, 0.5, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.6, 1, 0.4, 0.3, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.4, 0.4, 1, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0, 0.1, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 80, 150, 10, 50, 90, 10, 40, 73.333333, 10, 70, 136.66667, 10, 60, 110, 10, 40, 70, 10, 70, 125.55556, 10, 60, 104.44444, 10, 60, 110, 10, 60, 110, 10, 80, 146.66667, 10, 50, 83.333333, 10, 40, 70, 10, 90, 170, 10, 50, 94.444444, 10, 60, 115.55556), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.072086441, 0.015731878, 0.055497453, 0.089862666), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.12331315, 0.044600706, 0.19576401, 0.21976221), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5, 1.4, 1.1, 1.3), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.5, 1.4, 1.1, 1.2222222), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 58.412503, 106.82501, 10, 70, 130, 10, 60, 110, 10, 60, 115.55556, 10, 48.412503, 86.825005, 10, 50, 90, 10, 20, 30, 10, 50, 83.333333, 10, 30, 50, 10, 30, 50, 10, 40, 70, 10, 40, 62.222222, 10, 30, 50, 10, 30, 50, 10, 30, 50, 10, 20, 31.111111), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.17666095, 0.040694426, 0.021934165, 0.055804824), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0196821, 0.045238162, 0.2319165, 0.092789084), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(1, 1, 1))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 1))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 1))
	expect_equal(x$successPerStage[3, ], c(0, 1, 0))
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 10, 10, 10, 49.456184, 59.570149, 10, 10, 10, 10, 14, 18, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 110, 110, 10, 10, 10, 10, 10, 10, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.96372159, 0.8, 0.738851), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.96101103, 0.8, NaN), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 12.4, 32.2, 10, 11.6, 24.8, 10, 11.6, 24.8, 10, 12, 28.5, 10, 10.4, 13.7, 10, 11.2, 21.1, 10, 10.4, 13.7, 10, 10.4, 13.7, 10, 11.2, 21.1, 10, 10.8, 17.4, 10, 11.6, 24.8, 10, 11.6, 24.8, 10, 10, 10, 10, 10.4, 13.7, 10, 10.4, 13.7, 10, 10, 10), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.20038002, 0.0090743029, 0.01423466, 0.057002661), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.62371801, 0.22152811, 0.094215754, 0.098131985), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.2, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0.2, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(10, 30, 50, 10, 30, 50, 10, 30, 50, 10, 30, 50, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.398113, 0.009618082, 0.032584226, 0.094513828), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(43, 75, 85, 86))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.35, 0.29, 1, 0.39, 0.38, 1, 0.39, 0.35, 1, 0.29, 0.28, 1, 0.36, 0.3, 1, 0.58, 0.54, 1, 0.64, 0.61, 1, 0.77, 0.67, 1, 0.39, 0.27, 1, 0.61, 0.58, 1, 0.77, 0.74, 1, 0.88, 0.77), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0.01, 0, 0.01, 0.03, 0.03, 0.02, 0.02, 0.03, 0.04, 0.02, 0, 0, 0.01, 0.03, 0.02, 0.02, 0.09, 0.07, 0.09, 0.13, 0.16, 0.03, 0, 0, 0.01, 0.02, 0.03, 0.05, 0.05, 0.12, 0.11, 0.22, 0.22), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.05, 0.12, 0.39, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.45, 0.21, 0.1, 0.03), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.12, 0.04, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.57, 0.25, 0.1, 0.03), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.45, 0.21, 0.1, 0.03), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.12, 0.04, 0.05, 0.11), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.05, 0.11), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.02, 0.04, 0.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$eventsPerStage)), c(50, 81.818182, 115.53911, 50, 74.683544, 100.01688, 50, 71.666667, 92.254902, 50, 64.948454, 81.227523, 50, 82.727273, 117.61099, 50, 86.708861, 122.70886, 50, 85.555556, 121.43791, 50, 89.690722, 128.64421, 50, 85.454545, 116.84989, 50, 88.607595, 127.27426, 50, 92.777778, 136.30719, 50, 95.360825, 140.12827), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.051250463, 0.10314226, 0.22726517, 0.32216634), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.079750661, 0.22459463, 0.34993844, 0.58348764), tolerance = 1e-07)
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

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.5, -0.72, -0.55), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.09, -0.28, -0.03), tolerance = 1e-07)

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

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.35, -0.44, -0.22), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.16, -0.54, -0.24), tolerance = 1e-07)

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

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(0, 0, 0))
	expect_equal(comp6[2, ], c(0, 0, 0))

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
	expect_equal(sim$earlyStop[1, ], 0)
	expect_equal(sim$earlyStop[2, ], 0.1, tolerance = 1e-07)
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

