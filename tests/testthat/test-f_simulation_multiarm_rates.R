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
#:#  File name: test-f_simulation_multiarm_rates.R
#:#  Creation date: 05 September 2020, 14:49:08
#:#  File version: $Revision: 3588 $
#:#  Last changed: $Date: 2020-09-04 09:47:38 +0200 (Fri, 04 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing simulation multi-arm rates function")


test_that("'getSimulationMultiArmRates': several configurations", {
	.skipTestIfDisabled()

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 8, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.2, 0.2, 1, 0, 0, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.5, 0.2, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1, 0, 0.3, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(399, 319.4, 332.4, 226.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 32.4, 40, 10, 17.5, 20, 10, 34.2, 50, 10, 33.2, 46.714286, 10, 0, 0, 10, 11.5, 10.7, 10, 0, 0, 10, 0, 0, 10, 20, 20, 10, 20.5, 30.5, 10, 24.5, 37.5, 10, 0.7, 3, 10, 22.1, 40, 10, 12.4, 11.6, 10, 8, 5.625, 10, 18.2, 2, 10, 74.5, 100, 10, 61.9, 72.8, 10, 66.7, 93.125, 10, 52.1, 51.714286), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.031716179, 0.043252134, 0.053263571, 0.10343172), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0074748887, 0.36849501, 0.17642204, 0.46322147), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10))
	expect_equal(x$iterations[2, ], c(10, 10))
	expect_equal(x$iterations[3, ], c(9, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 1, 0.9, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.4), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0))
	expect_equal(x$futilityStop, c(0, 0))
	expect_equal(x$earlyStop, c(0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(389.8, 295), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 37.5, 44.444444, 10, 30, 30, 10, 10, 11.111111, 10, 14.3, 30, 10, 38.8, 37.333333, 10, 13.2, 5, 10, 86.3, 92.888889, 10, 57.5, 65), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0028955336, 0.0436119), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.092760072, 0.33907589), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0, 0, 1, 0.2, 0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(440, 433.4, 389.4, 389.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 40, 40, 10, 20, 20, 10, 40, 40, 10, 20, 20, 10, 20, 20, 10, 35, 40, 10, 38, 50, 10, 20, 20, 10, 5, 10, 10, 26.7, 30, 10, 0.9, 0.8, 10, 40, 40, 10, 30, 30, 10, 10, 10, 10, 0, 0, 10, 5.4, 4.3, 10, 95, 100, 10, 91.7, 100, 10, 78.9, 90.8, 10, 85.4, 84.3), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(9.8528687e-05, 0.00039477469, 0.038818287, 0.036423399), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0033345053, 8.0911579e-07, 0.081010958, 0.16071706), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all",
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.2, 0.1, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0.1, 0, 0.5, 0.2, 0, 0, 0, 0, 0.2, 0.3, 0, 0.3, 0.4, 0, 0.8, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.5, 0.8, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(1026, 935, 860.5, 561.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 95.2, 100, 10, 99.7, 77.3, 10, 100, 62.1, 10, 85.5, 16.8, 10, 95.2, 100, 10, 99.7, 77.3, 10, 100, 62.1, 10, 85.5, 16.8, 10, 95.2, 100, 10, 99.7, 77.3, 10, 100, 62.1, 10, 85.5, 16.8, 10, 95.2, 100, 10, 99.7, 77.3, 10, 100, 62.1, 10, 85.5, 16.8, 10, 95.2, 100, 10, 99.7, 77.3, 10, 100, 62.1, 10, 85.5, 16.8), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.16138991, 0.10341029, 0.081144653, 0.2211948), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.00015776369, 0.34374774, 0.49128099, 0.87930663), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 9, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.7, 0.7, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.7, 0.6, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.5, 1, 0.8, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0, 0, 0.3, 0, 0, 0, 0, 0, 0.2, 0, 0.3, 0.3, 0, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(620, 586.7, 368.3, 332.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 60, 70, 10, 40.7, 40.4, 10, 25.4, 27.111111, 10, 37.3, 43.1, 10, 30, 30, 10, 58.3, 59.5, 10, 20, 11.555556, 10, 1.5, 5.8, 10, 50, 50, 10, 49, 50.4, 10, 38.8, 25, 10, 13.8, 16.6, 10, 40, 50, 10, 30, 29.5, 10, 37.2, 37.222222, 10, 36.2, 33.9, 10, 90, 100, 10, 89, 89.9, 10, 60.7, 50.444444, 10, 44.4, 49.7), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.081773814, 0.099852155, 0.15501141, 0.24915867), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.055537045, 0.16850455, 0.52889918, 0.75314226), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 9, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.7, 0.3, 1, 0.2, 0.1, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.4, 0.2, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.8, 0.7, 1, 0.5, 0.3, 1, 0.8, 0.7, 1, 0.6, 0.5, 1, 0.7, 0.5, 1, 0.7, 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 1.6, 2.6, 1.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.9, 1.5, 1.8888889, 1.5714286), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0.4, 0, 0.2, 0.3, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0.4, 0, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.5, 0.7, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.4, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(603.2, 438.2, 576.8, 264.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 30, 30, 10, 21.5, 30, 10, 60.8, 33.333333, 10, 11, 3.2857143, 10, 47.5, 50, 10, 11.2, 11.2, 10, 35.1, 22.222222, 10, 10.7, 24.285714, 10, 40, 40, 10, 36.2, 40.2, 10, 68.3, 68.555556, 10, 25.8, 26.428571, 10, 64.1, 70, 10, 44.4, 50, 10, 57, 45.888889, 10, 40.4, 16.714286, 10, 81.6, 100, 10, 62.1, 81.4, 10, 79.6, 81.111111, 10, 45.4, 45.285714), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.029980042, 0.037719077, 0.049004144, 0.1186065), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.097432155, 0.30709034, 0.15699982, 0.7513439), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 9, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.4, 0.3, 1, 0.3, 0.1, 1, 0.3, 0.3, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.2, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.3, 0.6, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(345.2, 273.6, 204.6, 245.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 21.5, 21.5, 10, 0, 0, 10, 7.5, 8.3333333, 10, 20, 28.571429, 10, 10, 10, 10, 17.5, 19.444444, 10, 7.5, 8.3333333, 10, 10, 14.285714, 10, 19.9, 19.9, 10, 19.9, 22, 10, 17.5, 16.111111, 10, 17.9, 0.57142857, 10, 22.4, 22.4, 10, 21.1, 17.777778, 10, 7.7, 8.4444444, 10, 13.5, 8.5714286, 10, 73.8, 73.8, 10, 58.5, 59.222222, 10, 40.2, 41.222222, 10, 61.4, 52), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.053341787, 0.089680006, 0.037197812, 0.059733516), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.16686282, 0.46825673, 0.55807983, 0.58805464), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0.2, 0, 0.1, 0.2, 0.4, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.4, 0, 0.4, 0.5, 0, 0, 0, 0, 0.2, 0.2, 0, 0.6, 0.3, 0.1, 0.7, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(1019, 1050, 1006.5, 856), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 96.9, 96.9, 10, 100, 100, 10, 95.7, 95.6, 10, 85.6, 84, 10, 96.9, 96.9, 10, 100, 100, 10, 95.7, 95.6, 10, 85.6, 84, 10, 96.9, 96.9, 10, 100, 100, 10, 95.7, 95.6, 10, 85.6, 84, 10, 96.9, 96.9, 10, 100, 100, 10, 95.7, 95.6, 10, 85.6, 84, 10, 96.9, 96.9, 10, 100, 100, 10, 95.7, 95.6, 10, 85.6, 84), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.081287056, 0.002617558, 0.15842238, 0.34263223), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.00021089265, 0.20791323, 0.652891, 0.81286977), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 8, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.6, 0.6, 1, 0.6, 0.6, 1, 0.8, 0.7, 1, 0.5, 0.5, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.3, 1, 0.4, 0.3, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.7, 0.5, 1, 0.6, 0.6, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0.1, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0.1, 0, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0.2, 0.3, 0.1, 0.4, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.9, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(583.7, 473.3, 450.5, 337.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 49, 48.9, 10, 37.3, 41.333333, 10, 30, 37.5, 10, 36.6, 40.444444, 10, 60, 60, 10, 62, 57.777778, 10, 31.6, 39.375, 10, 16.4, 7.1111111, 10, 32.1, 32, 10, 28.3, 31.444444, 10, 36.3, 32.25, 10, 26.2, 18, 10, 36.9, 36.9, 10, 23.6, 15, 10, 46.1, 44.625, 10, 26.8, 29.555556, 10, 89, 88.9, 10, 75.6, 72.777778, 10, 72, 76.875, 10, 53, 47.555556), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.11427171, 0.1122468, 0.13835461, 0.2469738), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.045659701, 0.25398514, 0.48807678, 0.77975551), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 9, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.1, 1, 0.2, 0.2, 1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.5, 0.2, 1, 0.6, 0.6, 1, 0.7, 0.6, 1, 0.8, 0.7, 1, 0.9, 0.4, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5, 2.1, 1.8, 2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.5, 2.2222222, 1.8888889, 2.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.4, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0.3, 0.3, 0.1, 0.8, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.1, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(453.2, 518.8, 453.5, 353.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 52.2, 52.2, 10, 28.7, 31.666667, 10, 30, 33.333333, 10, 17.3, 43, 10, 10, 10, 10, 40, 44.444444, 10, 32.2, 35.666667, 10, 15, 11.25, 10, 20, 20, 10, 47.8, 52.888889, 10, 30, 33.333333, 10, 45.9, 39.5, 10, 39.7, 39.7, 10, 51.6, 54.222222, 10, 46.2, 48.222222, 10, 71.9, 54.25, 10, 79.7, 79.7, 10, 69.4, 73.777778, 10, 66.2, 70.444444, 10, 72.3, 54.25), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.033468007, 0.0039727362, 0.027455064, 0.17749888), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0056107453, 0.051234176, 0.29779158, 0.78661976), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100), directionUpper = FALSE, 
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10))
	expect_equal(x$iterations[2, ], c(7, 6, 10))
	expect_equal(x$iterations[3, ], c(5, 6, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.1, 1, 0.1, 0.1, 1, 0.6, 0.4, 1, 0.7, 0.5, 1, 0.6, 0.6, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.4, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.3, 0.4, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.3, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.5, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(192.4, 242.6, 307), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 3.5714286, 20, 10, 20.833333, 33.333333, 10, 20, 28.571429, 10, 14.285714, 20, 10, 7.6666667, 16.666667, 10, 11.4, 2.7142857, 10, 18.571429, 26.4, 10, 20.333333, 28.333333, 10, 0, 0, 10, 8.5714286, 13, 10, 16.666667, 16.666667, 10, 35.2, 57.142857, 10, 45, 79.4, 10, 65.5, 95, 10, 66.6, 88.428571), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.078068984, 0.036584879, 0.060648481), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.55765408, 0.38462461, 0.20906851), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(7, 8, 10, 10))
	expect_equal(x$iterations[3, ], c(4, 7, 6, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.2, 0.2, 1, 0.3, 0.1, 1, 0.2, 0.2, 1, 0.6, 0.5, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.1, 0, 1, 0.1, 0, 1, 0.3, 0, 1, 0.7, 0.4, 1, 0.8, 0.7, 1, 1, 0.6, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0.3, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.3, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.3, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.6, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.6, 0.3, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(245, 313.6, 320.8, 271.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 14.285714, 25, 10, 44.125, 57.142857, 10, 10, 0, 10, 7, 23.142857, 10, 36.142857, 25, 10, 19.875, 28.571429, 10, 39.5, 83.333333, 10, 15.9, 28.571429, 10, 14.285714, 25, 10, 6.625, 14.285714, 10, 15.9, 16.666667, 10, 14, 33.857143, 10, 17.428571, 25, 10, 6.625, 0, 10, 10, 0, 10, 13.9, 0, 10, 82.142857, 100, 10, 77.25, 100, 10, 75.4, 100, 10, 50.8, 85.571429), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.068731243, 0.055183835, 0.077528429, 0.10008751), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.011874627, 0.18163822, 0.099814914, 0.62330839), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10))
	expect_equal(x$iterations[2, ], c(7, 8))
	expect_equal(x$iterations[3, ], c(6, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.5, 0.4, 1, 0.5, 0.5, 1, 0.2, 0.1, 1, 0.7, 0.6, 1, 0.8, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.4, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(216, 253))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 6, 16.666667, 10, 1.5, 1.6666667, 10, 13, 0, 10, 46.375, 66.666667, 10, 27.142857, 67.833333, 10, 15.25, 16.666667, 10, 46.142857, 84.5, 10, 63.125, 85), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0083419999, 0.0082739238), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.40958094, 0.20463093), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(4, 6, 6, 9))
	expect_equal(x$iterations[3, ], c(0, 1, 2, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0, 1, 0.2, 0, 1, 0, 0, 1, 0.4, 0.2, 1, 0.1, 0, 1, 0.1, 0, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.5, 0.1, 1, 0.2, 0.1, 1, 0.1, 0, 1, 0.2, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.4, 0, 1, 0.6, 0.1, 1, 0.6, 0.2, 1, 0.9, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0.6, 0.4, 0.4, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.4, 0.5, 0.4, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(1, 0.9, 0.8, 0.5), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(1, 0.9, 0.8, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(NaN, 167.2, 148.6, 289.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 25, 0, 10, 31.833333, 0, 10, 0, 0, 10, 28.222222, 40, 10, 25, 0, 10, 16.666667, 0, 10, 0, 0, 10, 11.111111, 0, 10, 25, 0, 10, 7, 100, 10, 41.833333, 50, 10, 21.222222, 20, 10, 22.75, 0, 10, 25.5, 0, 10, 7, 50, 10, 17, 40, 10, 97.75, 0, 10, 81, 100, 10, 48.833333, 100, 10, 77.555556, 100), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0021879996, 0.0033809276, 0.0085402997, 0.029986864), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, 0.036271504, 0.12898128, 0.071798515), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(7, 8, 8, 10))
	expect_equal(x$iterations[3, ], c(5, 8, 5, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.2, 1, 0.5, 0.5, 1, 0.1, 0.1, 1, 0.5, 0.3, 1, 0, 0, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.5, 0.2, 1, 0.5, 0.3, 1, 0.7, 0.5, 1, 0.8, 0.8, 1, 0.8, 0.5, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5714286, 1.625, 1.625, 2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.4, 1.625, 1.8, 1.875), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.1, 0, 0.1, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0.4, 0, 0.1, 0.4, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.6, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.2, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.5, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.5, 0.2, 0.5, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(320.9, 436.4, 332.8, 363.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 57.571429, 40, 10, 53.5, 62.5, 10, 12.5, 20, 10, 29.6, 25.5, 10, 0, 0, 10, 29.5, 37.5, 10, 19.875, 20.8, 10, 35.4, 15.375, 10, 33.428571, 40, 10, 25, 25, 10, 57.375, 62.6, 10, 33.8, 50.5, 10, 38.428571, 60, 10, 29.5, 37.5, 10, 62.5, 3.4, 10, 31.2, 2.875, 10, 86.142857, 100, 10, 83, 100, 10, 94.875, 63.4, 10, 65, 53.375), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.11579013, 0.17739666, 0.020445074, 0.18285633), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.018216475, 0.26321472, 0.53666353, 0.68252815), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(6, 8, 10, 10))
	expect_equal(x$iterations[3, ], c(5, 7, 9, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0.2, 1, 0.1, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.5, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.9, 0.6, 1, 0.2, 0.1, 1, 0.3, 0.2, 1, 0.4, 0.3, 1, 0.5, 0.4, 1, 0.8, 0.4, 1, 0.6, 0.5, 1, 0.8, 0.7, 1, 1, 0.9, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.5, 1.625, 2.1, 1.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.6, 1.7142857, 1.5555556, 1.1666667), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.1, 0.1, 0, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.4, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.5, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.5, 0.3, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(305, 389.8, 460.2, 232.9), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 42.166667, 60, 10, 30.25, 57.142857, 10, 35.5, 22.222222, 10, 3.6, 0, 10, 0, 0, 10, 29.5, 42.857143, 10, 20, 22.222222, 10, 19.4, 23, 10, 50, 60, 10, 25, 28.571429, 10, 65.6, 56, 10, 10.9, 0.66666667, 10, 37, 40, 10, 36.125, 42.857143, 10, 32, 32.222222, 10, 39.8, 34.333333, 10, 79.166667, 100, 10, 66.375, 100, 10, 68.3, 77.111111, 10, 43.4, 51.666667), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.11631926, 0.17025864, 0.13415318, 0.13213075), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.020832378, 0.33707827, 0.44482603, 0.6067925), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(9, 9, 10, 10))
	expect_equal(x$iterations[3, ], c(9, 8, 9, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0, 0, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0, 0, 1, 0.5, 0.5, 1, 0.4, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.2, 0.1, 1, 0.3, 0.3, 1, 0.7, 0.6, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.9, 0.9, 1, 0.9, 0.8, 1, 1, 0.9, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.2, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.1, 0.2, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(324.2, 207, 238.6, 181.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 22.777778, 22.777778, 10, 0, 0, 10, 7.3, 8, 10, 10.6, 17.666667, 10, 11.111111, 11.111111, 10, 0, 0, 10, 33.2, 36.666667, 10, 16.1, 8.6666667, 10, 17.666667, 17.555556, 10, 22.222222, 25, 10, 2.7, 0, 10, 8.1, 4.6666667, 10, 24.666667, 24.666667, 10, 24.777778, 20.25, 10, 5.5, 6, 10, 6.5, 9.6666667, 10, 76.222222, 76.111111, 10, 47, 45.25, 10, 48.7, 50.666667, 10, 41.3, 40.666667), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.062127402, 0.056830717, 0.084457951, 0.096570758), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.028305637, 0.14375379, 0.1366597, 0.24607567), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(5, 9, 10, 10))
	expect_equal(x$iterations[3, ], c(5, 9, 7, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.8, 0.8, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 1, 0.8, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.7, 0.6, 1, 0.8, 0.7, 1, 0.2, 0.2, 1, 0.7, 0.7, 1, 0.8, 0.5, 1, 0.9, 0.8, 1, 0.5, 0.5, 1, 0.9, 0.9, 1, 1, 0.7, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2.7777778, 2.4, 3.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2.7777778, 2.8571429, 3.875), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0.4, 0.3, 0, 0, 0, 0, 0.2, 0.1, 0, 0.3, 0.1, 0, 0.6, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.5, 0.2, 0.3, 0.6, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.5, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.5, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.5, 0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(300.2, 726.8, 660, 576.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 44.4, 44.4, 10, 55.111111, 55.111111, 10, 50, 71.428571, 10, 48.7, 60.75, 10, 33.8, 33.8, 10, 55.111111, 55.111111, 10, 40, 57.142857, 10, 68.7, 60.75, 10, 53.8, 53.8, 10, 88.888889, 88.888889, 10, 70, 85.714286, 10, 48.7, 48.25, 10, 33.8, 33.8, 10, 77.333333, 77.333333, 10, 80, 71.428571, 10, 58.7, 60.75, 10, 84.4, 84.4, 10, 99.555556, 99.555556, 10, 100, 100, 10, 68.7, 60.75), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.16025418, 0.065647426, 0.096417459, 0.48390207), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.008658595, 0.1549295, 0.61461637, 0.8800311), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(9, 7, 9, 10))
	expect_equal(x$iterations[3, ], c(9, 7, 9, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.6, 0.6, 1, 0.1, 0.1, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0, 0, 1, 0.4, 0.4, 1, 0.4, 0.2, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.2, 1, 0.9, 0.9, 1, 0.7, 0.7, 1, 0.9, 0.9, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.6666667, 1.7142857, 1.8888889, 2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.6666667, 1.7142857, 1.8888889, 2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0.2, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.5, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.1, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.1, 0.3, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(429.4, 293.4, 449.1, 353.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 40, 40, 10, 9.2857143, 9.1428571, 10, 11.111111, 11.111111, 10, 33.6, 47.285714, 10, 11.111111, 11.111111, 10, 56.428571, 56.142857, 10, 57.888889, 57.777778, 10, 30.4, 28.857143, 10, 36.444444, 36.333333, 10, 0, 0, 10, 35, 35, 10, 26.8, 9.5714286, 10, 43.444444, 43.444444, 10, 43, 42.714286, 10, 40.222222, 40.111111, 10, 40.8, 15.142857, 10, 79.888889, 79.777778, 10, 65.714286, 65.285714, 10, 77.666667, 77.555556, 10, 65.8, 50.428571), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.092037721, 0.2057481, 0.11130003, 0.35885036), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.018306204, 0.21231029, 0.35198278, 0.26893737), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(8, 8, 9, 9))
	expect_equal(x$iterations[3, ], c(8, 8, 8, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.6, 0.5, 1, 0.7, 0.4, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 0.9, 0.8, 1, 0.9, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.875, 2, 1.5555556, 2.1111111), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.875, 2, 1.625, 3.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0.4, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.1, 0, 0.6, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.5, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.2, 0.2, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.2, 0.2, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.2, 0.2, 0.2, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(460, 401, 345.4, 349.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 56.625, 56.625, 10, 23.375, 23.125, 10, 22.222222, 25, 10, 26.666667, 58.75, 10, 37.5, 37.5, 10, 37.5, 37.5, 10, 11.111111, 12.5, 10, 26.222222, 58.75, 10, 37.5, 37.5, 10, 47.25, 47, 10, 47.444444, 53.25, 10, 32, 70, 10, 40.25, 40.25, 10, 44.75, 44.625, 10, 30.666667, 31, 10, 50.555556, 70, 10, 84.375, 84.375, 10, 67, 66.625, 10, 55.888889, 59.25, 10, 51.666667, 70), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.040798651, 0.0028024649, 0.021937703, 0.33978918), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.010331891, 0.16216413, 0.47354809, 0.89216443), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0.1, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100), directionUpper = FALSE, 
		maxNumberOfIterations = 1)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(1, 1, 1))
	expect_equal(x$iterations[2, ], c(0, 1, 1))
	expect_equal(x$iterations[3, ], c(0, 1, 0))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(NaN, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, 1, NaN))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(1, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 1))
	expect_equal(x$futilityStop, c(1, 0, 1))
	expect_equal(x$earlyStop, c(1, 0, 1))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(NaN, 300, NaN))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 25, 100, 10, 100, NaN, 10, 0, 0, 10, 0, 0, 10, 0, NaN, 10, 0, 0, 10, 0, 0, 10, 0, NaN, 10, 0, 0, 10, 0, 0, 10, 0, NaN, 10, 0, 0, 10, 25, 100, 10, 100, NaN))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(NaN, 0.015180491, 0.68837756), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, 0.45064062, NaN), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': using calcSubjectsFunction", {

	.skipTestIfDisabled()

	myFunction <- function(..., stage, minNumberOfSubjectsPerStage) {
		return(ifelse(stage == 3, 33, minNumberOfSubjectsPerStage[stage]))
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10, calcSubjectsFunction = myFunction)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 8, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.7, 0.7, 1, 0.1, 0, 1, 0.4, 0.2, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.2, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(78, 78, 76.4, 76.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 4, 1.6, 10, 1, 0.4, 10, 3, 1.5, 10, 1, 0.5, 10, 2, 0.8, 10, 1, 0.4, 10, 4, 2, 10, 1, 0.5, 10, 1, 0.4, 10, 1, 0.4, 10, 2, 0.5, 10, 4, 2, 10, 3, 1.2, 10, 7, 2.8, 10, 1, 0, 10, 4, 1, 10, 10, 4, 10, 10, 4, 10, 10, 4, 10, 10, 4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.069872709, 0.030257924, 0.015883809, 0.17842713), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.035901906, 0.056082253, 0.029555835, 0.06694977), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': using selectArmsFunction", {

	.skipTestIfDisabled()

	mySelectionFunction <- function(effectSizes) {
		return(c(TRUE, FALSE, FALSE, FALSE))
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		maxNumberOfIterations = 10, selectArmsFunction = mySelectionFunction, typeOfSelection = "userDefined")

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 9, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(130, 130, 126, 130))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.087189004, 0.10104379, 0.071123643, 0.38553328), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.08846992, 0.087299251, 0.20564078, 0.33234639), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': typeOfShape = sigmoidEmax", {

	.skipTestIfDisabled()

	designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
	x <- getSimulationMultiArmRates(designIN, activeArms = 3, typeOfShape = "sigmoidEmax", piMaxVector = seq(0.1, 0.9, 0.2), gED50 = 2, plannedSubjects = cumsum(rep(20, 3)), piControl = 0.1, 
		intersectionTest = "Sidak", typeOfSelection = "rbest", rValue = 2, threshold = -Inf, successCriterion = "all", maxNumberOfIterations = 100, seed = 3456)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(14, 64, 86, 94, 81))
	expect_equal(x$iterations[3, ], c(4, 59, 77, 45, 11))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.04, 1, 0.36, 0.32, 1, 0.31, 0.29, 1, 0.24, 0.18, 1, 0.18, 0.06, 1, 0.08, 0.02, 1, 0.38, 0.34, 1, 0.62, 0.54, 1, 0.77, 0.32, 1, 0.69, 0.07, 1, 0.1, 0.02, 1, 0.54, 0.52, 1, 0.79, 0.71, 1, 0.87, 0.4, 1, 0.75, 0.09, 1, 0.14, 0.04, 1, 0.64, 0.59, 1, 0.86, 0.77, 1, 0.94, 0.45, 1, 0.81, 0.11), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.01, 0.01, 0.03, 0.04, 0.04, 0.03, 0.23, 0.08, 0.02, 0, 0, 0, 0, 0.01, 0.01, 0.04, 0.06, 0.09, 0.16, 0.35, 0.18, 0.47, 0.37, 0.03, 0, 0, 0, 0.01, 0.01, 0, 0.07, 0.22, 0.13, 0.34, 0.43, 0.09, 0.67, 0.26, 0.01), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.04, 0.45, 0.92, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.86, 0.36, 0.13, 0.04, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.1, 0.05, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.96, 0.41, 0.15, 0.04, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.96, 0.41, 0.23, 0.55, 0.89), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0.01, 0.02, 0.19), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.07, 0.49, 0.7), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.12, 0.21, 0.06), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(90.8, 153.8, 177.8, 163.4, 135.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(20, 14.285714, 20, 20, 11.25, 10.847458, 20, 7.2093023, 7.5324675, 20, 5.106383, 8, 20, 4.4444444, 10.909091, 20, 11.428571, 10, 20, 11.875, 11.525424, 20, 14.418605, 14.025974, 20, 16.382979, 14.222222, 20, 17.037037, 12.727273, 20, 14.285714, 10, 20, 16.875, 17.627119, 20, 18.372093, 18.441558, 20, 18.510638, 17.777778, 20, 18.518519, 16.363636, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0094710248, 0.083376862, 0.21492635, 0.46850194, 0.71902266), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.01685475, 0.11787252, 0.39760566, 0.7144188, 0.94846731), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': comparison of base and multi-arm", {

	.skipTestIfDisabled()

	allocationRatioPlanned <- 2
	design <- getDesignInverseNormal(typeOfDesign = "WT", deltaWT = 0.15, futilityBounds = c(-0.5, 0.5), informationRates = c(0.2, 0.8, 1))

	x <- getSimulationMultiArmRates(design, activeArms = 1, plannedSubjects = c(20, 40, 60), 
		directionUpper = FALSE, piControl = 0.6, piMaxVector = seq(0.3, 0.6, 0.1),  
		conditionalPower = 0.6, minNumberOfSubjectsPerStage = c(NA, 20, 20), maxNumberOfSubjectsPerStage = c(NA, 80, 80), 
		piControlH1 = 0.4, 
		piH1 = 0.3,  
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	y <- getSimulationRates(design, plannedSubjects = round((1 + 1/allocationRatioPlanned) *c(20, 40, 60)), normalApproximation = TRUE,  pi2 = 0.6, pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE, 
		conditionalPower = 0.6, 
		pi2H1 = 0.4, 
		pi1H1 = 0.3, 
		minNumberOfSubjectsPerStage = round((1 + 1/allocationRatioPlanned)*c(NA, 20, 20)), maxNumberOfSubjectsPerStage = round((1 + 1/allocationRatioPlanned)*c(NA, 80, 80)),   
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.02, -0.03, 0.06, 0.03), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0.01, 0), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(0.06, -0.02, 0.05, 0.02), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(-0.08, -0.01, 0, 0.01), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.03, 0.07, -0.06, 0.15), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0, 0.02, 0.02, -0.1), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0))
	expect_equal(comp4[2, ], c(-0.1, -0.4, 0.2, 0), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(-55.1, 2, 0.5, -4.8), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-11.8, -15.4, 4.2, -27.6), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(-0.09, -0.07, -0.02, -0.07), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmRates': comparison of base and multi-arm, Fisher design", {

	.skipTestIfDisabled()

	allocationRatioPlanned <- 1 
	design <- getDesignFisher(alpha0Vec = c(0.3, 0.4), informationRates = c(0.5, 0.7, 1))

	x <- getSimulationMultiArmRates(design, activeArms = 1, plannedSubjects = c(20, 40, 60), 
		directionUpper = FALSE, piControl = 0.6, piMaxVector = seq(0.3, 0.6, 0.1),  
		conditionalPower = 0.6, minNumberOfSubjectsPerStage = c(NA, 20, 20), maxNumberOfSubjectsPerStage = c(NA, 80, 80), 
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = -1008239793)

	y <- getSimulationRates(design, plannedSubjects = round((1 + 1/allocationRatioPlanned) *c(20, 40, 60)), 
		normalApproximation = TRUE,  pi2 = 0.6, pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE, 
		conditionalPower = 0.6, minNumberOfSubjectsPerStage = round((1 + 1/allocationRatioPlanned)*c(NA, 20, 20)), 
		maxNumberOfSubjectsPerStage = round((1 + 1/allocationRatioPlanned)*c(NA, 80, 80)),   
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = -2039707705)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(0.04, 0.03, 0, 0.02), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(-0.02, 0.02, -0.05, 0.03), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(0.01, 0.03, 0, -0.01), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0.05, -0.02, 0.05, 0), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(-0.03, -0.01, 0.06, 0.11), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0, -0.01, -0.08, -0.06), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0))
	expect_equal(comp4[2, ], c(10, -3, -0.7, 3.4), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(-5, 4.9, 2.4, 1.5), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(12.4, -5.1, 8.8, -28.5), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(0.04, -0.03, 0.07, -0.07), tolerance = 1e-07)

})

