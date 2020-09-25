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
#:#  Creation date: 23 September 2020, 11:13:13
#:#  File version: $Revision$
#:#  Last changed: $Date$
#:#  Last changed by: $Author$
#:#  

context("Testing simulation multi-arm rates function")


test_that("'getSimulationMultiArmRates': several configurations", {
	.skipTestIfDisabled()
	.skipTestIfNotX64()

	x <- getSimulationMultiArmRates(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piControl = 0.3, piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 9, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.3, 1, 0.5, 0.3, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(334.8, 445, 331.8, 179.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 8, 10.4, 10, 10, 10, 10, 11.3, 15.333333, 10, 0, 0, 10, 10, 10, 10, 17.5, 20, 10, 0, 0, 10, 13, 19.142857, 10, 22.4, 22.5, 10, 40, 40, 10, 37.5, 36.555556, 10, 4.4, 8.5714286, 10, 20.4, 38.7, 10, 30, 30, 10, 28.2, 19.111111, 10, 17.1, 15.714286, 10, 60.8, 81.6, 10, 97.5, 100, 10, 77, 71, 10, 34.5, 43.428571), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.032197948, 0.00019444487, 0.052129075, 0.12394528), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.33607045, 0.04525892, 0.4023749, 0.68738904), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0, 0, 1, 0.5, 0.5, 1, 0.7, 0.5, 1, 1, 1, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.2, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0))
	expect_equal(x$futilityStop, c(0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(397.2, 312.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 30, 30, 10, 22.4, 37.5, 10, 13, 20, 10, 0, 0, 10, 38.8, 41.8, 10, 52.8, 32.75, 10, 81.8, 91.8, 10, 75.2, 70.25), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0097327907, 0.021741893), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.14656813, 0.35197865), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(434.8, 402, 440, 425), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 15, 20, 10, 10, 10, 10, 30, 30, 10, 12.7, 20, 10, 40, 40, 10, 20, 20, 10, 30, 30, 10, 29.1, 34.2, 10, 30, 30, 10, 33.4, 40, 10, 32.4, 40, 10, 40, 40, 10, 10, 10, 10, 26.7, 27.4, 10, 92.4, 100, 10, 81.8, 94.2, 10, 95, 100, 10, 90.1, 97.4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0098526063, 0.0022619481, 0.010226943, 0.0071111057), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.00025317548, 0.089328639, 4.5501958e-05, 0.12015791), tolerance = 1e-07)
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
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0, 0.3, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0, 0.6, 0.2, 0, 0, 0, 0, 0, 0.2, 0, 0.4, 0.3, 0, 0.8, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.7, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(1026, 1002, 924.5, 714.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9, 10, 95.2, 100, 10, 100, 90.4, 10, 91.8, 83.1, 10, 100, 32.9), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.16336896, 3.7379108e-06, 0.18421481, 0.069788183), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.00052547754, 0.089531131, 0.32040425, 0.67566016), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 8, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.8, 0.8, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.2, 1, 0.3, 0.1, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.6, 0.5, 1, 0.6, 0.3, 1, 0.5, 0.5, 1, 0.8, 0.8, 1, 0.8, 0.7, 1, 0.9, 0.6, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.1, 0, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.3, 0.1, 0, 0, 0, 0, 0, 0.2, 0, 0.3, 0.4, 0, 0.8, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(642.8, 566.9, 399.8, 265.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 77.6, 80, 10, 23.9, 30, 10, 20, 1, 10, 12.3, 1.3333333, 10, 10, 10, 10, 30.2, 28.6, 10, 28.6, 25, 10, 20, 3.1666667, 10, 60, 60, 10, 49.7, 41.1, 10, 37.4, 28.25, 10, 40.8, 9.8333333, 10, 47.6, 50, 10, 63.8, 77.3, 10, 61.2, 53.25, 10, 53.1, 14.333333, 10, 97.6, 100, 10, 83.8, 88.5, 10, 73.6, 53.75, 10, 63.1, 14.333333), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.080486965, 0.12759682, 0.10458054, 0.065420449), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.022470074, 0.31122739, 0.58569198, 0.85520318), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 8, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.4, 0.1, 1, 0.3, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.2, 0.1, 1, 0.2, 0, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.9, 0.4, 1, 0.3, 0.3, 1, 0.7, 0.7, 1, 0.9, 0.7, 1, 0.9, 0.6, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.3, 1.6, 1.7, 2.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.3, 1.2, 1.375, 1.5714286), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.3, 0.4, 0, 0, 0, 0, 0, 0.4, 0, 0.2, 0.5, 0, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.8, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.4, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(436.4, 438.6, 346.7, 372.5), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 16.7, 20, 10, 27.9, 10, 10, 9.1, 12.5, 10, 1.2, 14.285714, 10, 37.5, 40, 10, 1.2, 0, 10, 12.3, 11.625, 10, 7.9, 0, 10, 32.4, 32.5, 10, 31.2, 40, 10, 21.5, 13.375, 10, 63.2, 50.142857, 10, 15.4, 28.7, 10, 56.2, 59, 10, 60.4, 63, 10, 58, 51.714286, 10, 72, 91.2, 10, 74.1, 89, 10, 61.9, 63.875, 10, 64.7, 66), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.031688257, 0.035836944, 0.12967885, 0.10427074), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.2491354, 0.21222327, 0.47711159, 0.3978836), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 9, 8, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.2, 1, 0, 0, 1, 0.3, 0.3, 1, 0, 0, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.6, 0.5, 1, 0.4, 0.2, 1, 0.7, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.1, 0.2, 0, 0, 0, 0, 0.1, 0.2, 0, 0.2, 0, 0, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.4, 0.5, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.3, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(355.2, 334, 233, 193.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 20, 10, 0, 0, 10, 20, 25, 10, 0, 0, 10, 30, 30, 10, 0, 0, 10, 3, 3.75, 10, 0, 0, 10, 12.4, 12.4, 10, 20.7, 22.777778, 10, 15, 18.75, 10, 16.2, 26.4, 10, 13.9, 13.9, 10, 54.2, 51.777778, 10, 13.1, 3, 10, 30.3, 24, 10, 76.3, 76.3, 10, 74.9, 74.555556, 10, 51.1, 50.5, 10, 46.5, 50.4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.035427106, 0.012436575, 0.08338715, 0.046283385), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.076058567, 0.27636533, 0.46741694, 0.70493817), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 9, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0.2, 0, 0.1, 0.3, 0, 0.2, 0.4, 0, 0, 0, 0, 0, 0.1, 0, 0.3, 0.3, 0.1, 0.4, 0.5, 0, 0, 0, 0, 0, 0.1, 0, 0.4, 0.5, 0.1, 0.8, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(952, 1050, 909.5, 860), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75, 10, 90.2, 90.2, 10, 100, 100, 10, 91, 89.888889, 10, 91, 88.75), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.16068828, 0.022112719, 0.21849189, 0.19646842), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.0018216452, 0.044801331, 0.47086458, 0.69046124), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 10, 10, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.7, 0.7, 1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.3, 0.2, 1, 0.5, 0.5, 1, 0.7, 0.7, 1, 0.6, 0.6, 1, 0.6, 0.3, 1, 0.2, 0.2, 1, 0.7, 0.7, 1, 0.6, 0.6, 1, 0.9, 0.4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.2, 0, 0.4, 0.2, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3, 0.1, 0.6, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.7, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(603.2, 605.9, 453.2, 361.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 52.2, 52.2, 10, 33.6, 33.5, 10, 21.2, 21.2, 10, 9.2, 17.6, 10, 70, 70, 10, 20, 20, 10, 35.6, 35.3, 10, 19.7, 21.4, 10, 45.3, 45.3, 10, 62.7, 62.6, 10, 36.2, 35.8, 10, 52.8, 45.4, 10, 16.9, 16.9, 10, 69.1, 69.1, 10, 41.8, 41.7, 10, 61.7, 44.4, 10, 92.2, 92.2, 10, 92.7, 92.6, 10, 67.4, 67, 10, 71.7, 64.4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.083443128, 0.076003514, 0.14647721, 0.085145955), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.043093175, 0.13127607, 0.3479275, 0.64693149), tolerance = 1e-07)
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
	expect_equal(x$iterations[3, ], c(10, 9, 7, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.5, 0.4, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.3, 1, 0.4, 0.4, 1, 0.7, 0.6, 1, 0.5, 0.3, 1, 0.5, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.2, 1.8, 1.5, 1.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.2, 1.7777778, 1.7142857, 1.8333333), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.1, 0, 0.3, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.2, 0.6, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(313.2, 474, 363.7, 263.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 15.9, 15.8, 10, 35.9, 39.777778, 10, 12.7, 18, 10, 2.8, 4.6666667, 10, 22.2, 22.2, 10, 30, 22.222222, 10, 22.7, 28.571429, 10, 27.4, 43.166667, 10, 18.1, 18, 10, 32.8, 36.444444, 10, 38.6, 54.857143, 10, 26.7, 26.5, 10, 15.8, 15.8, 10, 54.9, 49.777778, 10, 37.3, 24.571429, 10, 24.9, 23.666667, 10, 59.8, 59.6, 10, 73.6, 70.444444, 10, 68.6, 65.142857, 10, 43, 50.166667), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.067103341, 0.011749166, 0.024807536, 0.13720867), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10265269, 0.46661697, 0.4198773, 0.2422132), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(9, 10, 9))
	expect_equal(x$iterations[3, ], c(4, 8, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.1, 1, 0.2, 0.2, 1, 0.4, 0.2, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0, 0, 1, 0.2, 0.2, 1, 0.4, 0, 1, 0.4, 0.4, 1, 0.3, 0.2, 1, 0.9, 0.4, 1, 1, 0.8, 1, 0.9, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0.2, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.6, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.1, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.5, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.5, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(220.6, 344.4, 279), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0.88888889, 3.25, 10, 11.4, 25, 10, 17.333333, 15.666667, 10, 5.5555556, 50, 10, 21.6, 25, 10, 11.111111, 16.666667, 10, 12.888889, 21, 10, 0, 0, 10, 15.777778, 33.333333, 10, 42.444444, 0, 10, 34.2, 50, 10, 17, 33.333333, 10, 61.777778, 74.25, 10, 67.2, 100, 10, 61.222222, 99), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.092467863, 0.076543666, 0.070533409), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.68219789, 0.21426803, 0.34246832), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(8, 9, 8, 9))
	expect_equal(x$iterations[3, ], c(3, 5, 4, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.1, 0.1, 1, 0.3, 0, 1, 0.3, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.4, 0.1, 1, 0.3, 0.2, 1, 0.3, 0.1, 1, 0.4, 0.2, 1, 0.4, 0.2, 1, 0.8, 0.3, 1, 0.9, 0.5, 1, 0.8, 0.4, 1, 0.9, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.2, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.1, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.2, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.5, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.7, 0.4, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.2, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.5, 0.4, 0.4, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.3, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(228, 255, 194, 185), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 7.375, 32, 10, 7.2222222, 22, 10, 25, 25, 10, 5.8888889, 25, 10, 26.625, 0, 10, 33.333333, 20, 10, 1.75, 25, 10, 0, 0, 10, 12.5, 0, 10, 11.111111, 20, 10, 1.75, 0, 10, 13.333333, 22.75, 10, 27.75, 66.666667, 10, 16.666667, 20, 10, 18.25, 36.5, 10, 18.444444, 36.25, 10, 74.25, 98.666667, 10, 68.333333, 82, 10, 46.75, 86.5, 10, 37.666667, 84), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.080071046, 0.019269683, 0.13759011, 0.029094411), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.43970154, 0.32440854, 0.65996071, 0.60200615), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(9, 8))
	expect_equal(x$iterations[3, ], c(8, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0, 0, 1, 0, 0, 1, 0.6, 0.6, 1, 0.5, 0.2, 1, 0.9, 0.8, 1, 0.8, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(303, 263.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 28.777778, 25, 10, 32.375, 50, 10, 0, 0, 10, 0, 0, 10, 34.111111, 62.375, 10, 52.25, 47.25, 10, 62.888889, 87.375, 10, 84.625, 97.25), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.030517799, 0.00088428211), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.55299105, 0.62021691), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(5, 6, 9, 9))
	expect_equal(x$iterations[3, ], c(0, 1, 5, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0, 1, 0.1, 0, 1, 0.2, 0, 1, 0.1, 0.1, 1, 0.2, 0, 1, 0, 0, 1, 0.2, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.4, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.3, 1, 0.2, 0, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0, 1, 0.6, 0.1, 1, 0.9, 0.5, 1, 0.9, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0.5, 0.4, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.5, 0.5, 0.4, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(1, 0.9, 0.5, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.5, 0.4, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.5, 0.5, 0.4, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(NaN, 171.2, 271.2, 368.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 0, 10, 8.8333333, 0, 10, 17, 0, 10, 5.8888889, 11.111111, 10, 40, 0, 10, 0, 0, 10, 8.8888889, 20, 10, 3, 11.111111, 10, 0, 0, 10, 58.833333, 100, 10, 8.1111111, 20, 10, 28.111111, 33.333333, 10, 38.2, 0, 10, 16.666667, 0, 10, 33.333333, 60, 10, 39.888889, 44.444444, 10, 98.2, 0, 10, 84.333333, 100, 10, 67.333333, 100, 10, 76.888889, 100), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0010701396, 1.0749986e-05, 0.015009054, 0.019936014), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, 0.062530095, 0.19373785, 0.13543053), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(9, 9, 10, 10))
	expect_equal(x$iterations[3, ], c(7, 8, 10, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.6, 0.4, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.4, 0.2, 1, 0.2, 0.1, 1, 0.6, 0.6, 1, 0.8, 0.6, 1, 0.4, 0.3, 1, 0.8, 0.7, 1, 0.8, 0.8, 1, 0.9, 0.8, 1, 0.9, 0.7, 1, 0.9, 0.8, 1, 1, 1, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.8888889, 1.8888889, 1.9, 2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.7142857, 1.875, 1.9, 2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.2, 0, 0.2, 0.4, 0, 0, 0, 0, 0.2, 0.1, 0, 0.1, 0.3, 0, 0.7, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.7, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.2, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.3, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.2, 0.1, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(465.5, 424.5, 413.1, 244.1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20.555556, 42.857143, 10, 6.5555556, 25, 10, 30, 1.2, 10, 2.2, 0.5, 10, 66.666667, 57.142857, 10, 42.111111, 62.5, 10, 4.9, 13.9, 10, 14.1, 5, 10, 35.111111, 28.571429, 10, 11.777778, 1, 10, 44.7, 33.8, 10, 36.6, 21.75, 10, 41, 42.857143, 10, 63.333333, 76, 10, 49.6, 57.3, 10, 32.9, 27.25, 10, 87.222222, 100, 10, 67.444444, 88.5, 10, 69.6, 58.1, 10, 42.9, 27.25), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.096913955, 0.09039929, 0.11243241, 0.1746525), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.093425176, 0.36276928, 0.67843506, 0.87119979), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(9, 9, 8, 10))
	expect_equal(x$iterations[3, ], c(7, 8, 6, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.1, 1, 0.2, 0.1, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.6, 0.4, 1, 0.3, 0.1, 1, 0.3, 0.1, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.2, 1, 0.7, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.7, 0.5, 1, 0.6, 0.2, 1, 0.9, 0.7, 1, 0.9, 0.8, 1, 0.8, 0.6, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.3333333, 1.4444444, 1.875, 1.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.1428571, 1.25, 1.3333333, 1.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.5, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.2, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.3, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.2, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(339.9, 359.2, 222.7, 176), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 31.333333, 14.285714, 10, 17.666667, 8.125, 10, 12.5, 0, 10, 1.2, 7.4, 10, 21.222222, 14.285714, 10, 35.888889, 50, 10, 25.625, 16.666667, 10, 13.6, 7.4, 10, 24.666667, 21.142857, 10, 33.222222, 50, 10, 31.5, 17.333333, 10, 26.2, 9.8, 10, 22.444444, 57.142857, 10, 5.1111111, 12.5, 10, 33, 19.833333, 10, 21.2, 12.2, 10, 67.333333, 92.571429, 10, 59.444444, 95.625, 10, 45.5, 36.5, 10, 34.4, 22), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.039329058, 0.14668797, 0.16576057, 0.14296603), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.28763166, 0.40839298, 0.6012117, 0.84313531), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(7, 8, 8, 10))
	expect_equal(x$iterations[3, ], c(7, 8, 7, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0, 0, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.4, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.3, 0.2, 1, 0.7, 0.7, 1, 0.8, 0.8, 1, 0.8, 0.7, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0.1, 0.2, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0.1, 0.4, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.3, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.1, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.1, 0.1, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(241.6, 306.8, 235.2, 156), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 27.285714, 27.285714, 10, 25, 25, 10, 0, 0, 10, 2.6, 4.4, 10, 16.142857, 16.142857, 10, 16, 16, 10, 3.5, 4, 10, 1.4, 2.8, 10, 14.285714, 14.285714, 10, 12.5, 12.5, 10, 40.875, 46.571429, 10, 15.8, 5.2, 10, 10.714286, 10.714286, 10, 26.75, 26.75, 10, 19.875, 8.2857143, 10, 18.6, 16.8, 10, 68.428571, 68.428571, 10, 80.25, 80.25, 10, 64.25, 58.857143, 10, 38.4, 29.2), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.064400041, 0.012818439, 0.075196936, 0.13824332), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.066989319, 0.23112098, 0.45267281, 0.52012057), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(8, 8, 10, 10))
	expect_equal(x$iterations[3, ], c(8, 8, 10, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.8, 0.8, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.6, 0.6, 1, 0.7, 0.7, 1, 0.9, 0.8, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.9, 0.9, 1, 0.8, 0.7, 1, 0.6, 0.6, 1, 0.5, 0.5, 1, 0.9, 0.9, 1, 0.8, 0.7, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 1, 1, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2.375, 2.375, 3.3, 3.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(2.375, 2.375, 3.3, 3.1111111), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.4, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.2, 0, 0.6, 0.2, 0, 0, 0, 0, 0, 0, 0, 0.8, 0.1, 0, 0.7, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.2, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.2, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.2, 0.2, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(523.8, 590, 858.4, 765.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 28.125, 28.125, 10, 37.5, 37.5, 10, 73.6, 73.5, 10, 55.2, 61.333333, 10, 58.625, 58.625, 10, 75, 75, 10, 70, 70, 10, 85.2, 83.555556, 10, 53.125, 53.125, 10, 62.5, 62.5, 10, 83.6, 83.5, 10, 71.1, 67.777778, 10, 65.625, 65.625, 10, 62.5, 62.5, 10, 83.6, 83.5, 10, 75.2, 72.444444, 10, 90.625, 90.625, 10, 100, 100, 10, 93.6, 93.5, 10, 91.1, 90), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.10081958, 0.049714416, 0.18629752, 0.24626925), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.088506618, 0.13049081, 0.6465822, 0.85577973), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(7, 7, 9, 10))
	expect_equal(x$iterations[3, ], c(7, 7, 7, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.5, 0.5, 1, 0.4, 0.3, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.8, 0.4, 1, 0.5, 0.5, 1, 0.4, 0.4, 1, 0.6, 0.4, 1, 0.7, 0.3, 1, 0.7, 0.7, 1, 0.7, 0.7, 1, 0.9, 0.7, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.7142857, 2, 1.7777778, 2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.7142857, 2, 1.8571429, 2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.1, 0, 0.4, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0.1, 0, 0.6, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.3, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.3, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.3, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(383.2, 335, 399.8, 407.3), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 24.142857, 24.142857, 10, 51.714286, 51.571429, 10, 39, 35.714286, 10, 8.8, 14.666667, 10, 36.142857, 36.142857, 10, 16.285714, 16.142857, 10, 4.1111111, 5.2857143, 10, 28.2, 46.666667, 10, 28.571429, 28.571429, 10, 30.142857, 30.142857, 10, 42.555556, 54.571429, 10, 60.9, 34.333333, 10, 60.285714, 60.285714, 10, 37.857143, 37.571429, 10, 55.222222, 42.142857, 10, 61.5, 35.666667, 10, 88.857143, 88.857143, 10, 68, 67.714286, 10, 81.555556, 76, 10, 79.7, 65.666667), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.14688077, 0.19244817, 0.083030211, 0.1268121), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.017888759, 0.37719292, 0.42839666, 0.48702638), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(9, 9, 9, 10))
	expect_equal(x$iterations[3, ], c(9, 9, 7, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.4, 0.4, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.2, 0.2, 1, 0.7, 0.4, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.7, 0.5, 1, 0.9, 0.6, 1, 0.9, 0.9, 1, 0.9, 0.9, 1, 0.9, 0.7, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.4444444, 1.4444444, 1.7777778, 2.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.4444444, 1.4444444, 2, 2.2857143), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0.1, 0, 0.5, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.6, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.1, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.1, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(347.6, 413, 407.8, 317.7), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 17.666667, 17.555556, 10, 39.222222, 39.222222, 10, 35.777778, 45.857143, 10, 7.9, 11.142857, 10, 13.555556, 13.555556, 10, 22.222222, 22.222222, 10, 24.666667, 31.571429, 10, 17.7, 25.142857, 10, 42.333333, 42.222222, 10, 47.555556, 47.555556, 10, 22.222222, 28.571429, 10, 44.9, 26.857143, 10, 27.111111, 27.111111, 10, 14.111111, 14, 10, 51.888889, 54.142857, 10, 50.7, 35.142857, 10, 64.888889, 64.666667, 10, 78.666667, 78.555556, 10, 74.111111, 82.714286, 10, 51.9, 36.857143), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.071382822, 0.0014758747, 0.067299064, 0.14413714), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.1013929, 0.24986305, 0.49462073, 0.55222023), tolerance = 1e-07)
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
	expect_equal(x$iterations[2, ], c(0, 1, 0))
	expect_equal(x$iterations[3, ], c(0, 0, 0))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(NaN, 1, NaN))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, NaN, NaN))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(1, 0, 1))
	expect_equal(x$futilityPerStage[2, ], c(0, 1, 0))
	expect_equal(x$futilityStop, c(1, 1, 1))
	expect_equal(x$earlyStop[1, ], c(1, 0, 1))
	expect_equal(x$earlyStop[2, ], c(0, 1, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(NaN, NaN, NaN))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, NaN, NaN, 10, 0, 0, 10, 0, 0, 10, NaN, NaN, 10, 0, 0, 10, 91, 0, 10, NaN, NaN, 10, 0, 0, 10, 0, 0, 10, NaN, NaN, 10, 0, 0, 10, 91, 0, 10, NaN, NaN))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(NaN, 3.7427402e-05, NaN), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, NaN, NaN))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': using calcSubjectsFunction", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

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
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0.1, 1, 0, 0, 1, 0.1, 0.1, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.6, 0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(124, 124, 124, 117.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0.4, 3.3, 10, 1.6, 13.2, 10, 0, 0, 10, 0.4, 3.6666667, 10, 0, 0, 10, 0.4, 3.3, 10, 1.6, 13.2, 10, 0.8, 7.3333333, 10, 1.2, 9.9, 10, 0.8, 6.6, 10, 0.8, 6.6, 10, 0.4, 3.6666667, 10, 2.4, 19.8, 10, 1.2, 9.9, 10, 1.6, 13.2, 10, 2.4, 18.333333, 10, 4, 33, 10, 4, 33, 10, 4, 33, 10, 4, 33), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.012189382, 0.016190277, 0.020380353, 0.11925746), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.32488024, 0.34652134, 0.40081174, 0.68872913), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': using selectArmsFunction", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	mySelectionFunction <- function(effectSizes) {
		return(c(TRUE, FALSE, FALSE, FALSE))
	}

	x <- getSimulationMultiArmRates(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), piMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		maxNumberOfIterations = 10, selectArmsFunction = mySelectionFunction, typeOfSelection = "userDefined")

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0.1, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop[1, ], c(0, 0, 0, 0))
	expect_equal(x$earlyStop[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(130, 130, 130, 126))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.044616119, 0.11264062, 0.1248477, 0.43958255), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.087582974, 0.1172724, 0.15105487, 0.4331775), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': typeOfShape = sigmoidEmax", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

	designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
	x <- getSimulationMultiArmRates(designIN, activeArms = 3, typeOfShape = "sigmoidEmax", piMaxVector = seq(0.1, 0.9, 0.2), gED50 = 2, plannedSubjects = cumsum(rep(20, 3)), piControl = 0.1, 
		intersectionTest = "Sidak", typeOfSelection = "rbest", rValue = 2, threshold = -Inf, successCriterion = "all", maxNumberOfIterations = 100, seed = 3456)

	## Comparison of the results of SimulationResultsMultiArmRates object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(20, 60, 88, 84, 81))
	expect_equal(x$iterations[3, ], c(4, 45, 70, 38, 20))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.11, 0.01, 1, 0.24, 0.17, 1, 0.26, 0.2, 1, 0.24, 0.14, 1, 0.14, 0.08, 1, 0.13, 0.03, 1, 0.44, 0.34, 1, 0.7, 0.55, 1, 0.69, 0.31, 1, 0.69, 0.13, 1, 0.16, 0.04, 1, 0.52, 0.39, 1, 0.8, 0.65, 1, 0.75, 0.31, 1, 0.79, 0.19, 1, 0.2, 0.04, 1, 0.6, 0.45, 1, 0.88, 0.7, 1, 0.84, 0.38, 1, 0.81, 0.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.03, 0.02, 0.01, 0.11, 0.11, 0.05, 0.19, 0.06, 0.03, 0, 0, 0, 0, 0.01, 0.03, 0.07, 0.1, 0.13, 0.3, 0.22, 0.14, 0.45, 0.3, 0.12, 0, 0, 0, 0.01, 0.03, 0.01, 0.11, 0.23, 0.18, 0.41, 0.32, 0.09, 0.62, 0.31, 0.04), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.07, 0.55, 0.89, 0.99), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.8, 0.4, 0.11, 0.05, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.16, 0.14, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.96, 0.54, 0.13, 0.05, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop[1, ], c(0.8, 0.4, 0.12, 0.16, 0.19), tolerance = 1e-07)
	expect_equal(x$earlyStop[2, ], c(0.16, 0.15, 0.18, 0.46, 0.61), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0.01, 0.11, 0.19), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0, 0.01, 0.16, 0.46, 0.61), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.01, 0.15, 0.18, 0.14), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(94.4, 143, 174.8, 153.2, 140.6), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(20, 11, 5, 20, 8, 7.5555556, 20, 5.9090909, 5.7142857, 20, 5.7142857, 7.3684211, 20, 3.4567901, 8, 20, 13, 15, 20, 14.666667, 15.111111, 20, 15.909091, 15.714286, 20, 16.428571, 16.315789, 20, 17.037037, 13, 20, 16, 20, 20, 17.333333, 17.333333, 20, 18.181818, 18.571429, 20, 17.857143, 16.315789, 20, 19.506173, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.011866207, 0.085418744, 0.23090361, 0.47460917, 0.65183497), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.02497337, 0.151524, 0.4525101, 0.68922536, 0.80573911), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmRates': comparison of base and multi-arm", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

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
	expect_equal(comp1, c(-0.03, -0.02, 0.09, 0.03), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0, 0, 0))
	expect_equal(comp2[2, ], c(0.09, -0.01, 0.06, 0.02), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(-0.12, -0.01, 0.03, 0.01), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.04, 0.04, -0.12, -0.03), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0.01, 0.02, -0.05, 0.03), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0))
	expect_equal(comp4[2, ], c(1.1, 0.3, 0, 0), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(-44.7, 9.7, 1.3, -3.2), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-14.6, -6.6, 26.9, 0.4), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.96, -0.39, -0.75, -0.06), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(0.1, -0.16, -0.38, -0.43), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmRates': comparison of base and multi-arm, Fisher design", {

	.skipTestIfDisabled()
	.skipTestIfNotX64()

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
	expect_equal(comp1, c(0.05, 0.1, 0.07, 0.02), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0.05, 0.01, 0.02, 0.03), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(-0.03, 0.04, -0.01, -0.01), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0.03, 0.05, 0.06, 0), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(-0.05, -0.09, 0, 0), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(0, 0, -0.05, 0.01), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0))
	expect_equal(comp4[2, ], c(7.4, 3.6, -6.3, 6.6), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(0.5, 12.9, -5, 26), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(6.1, 19.9, -2, -3.9), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of matrixarray object 'comp6' with expected results
	expect_equal(comp6[1, ], c(-0.38, -0.17, -0.41, 0.14), tolerance = 1e-07)
	expect_equal(comp6[2, ], c(-0.29, -0.61, -0.52, -0.78), tolerance = 1e-07)

})

