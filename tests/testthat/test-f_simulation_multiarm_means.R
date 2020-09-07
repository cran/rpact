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
#:#  File name: test-f_simulation_multiarm_means.R
#:#  Creation date: 05 September 2020, 14:48:54
#:#  File version: $Revision: 3588 $
#:#  Last changed: $Date: 2020-09-04 09:47:38 +0200 (Fri, 04 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing simulation multi-arm means function")


test_that("'getSimulationMultiArmMeans': several configurations", {
	.skipTestIfDisabled()

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(9, 8, 8, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.3, 1, 0.6, 0.3, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0, 0.2, 0.1, 0, 0, 0.3, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0, 0.2, 0.3, 0, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.6, 0.8, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.4, 0.6, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(268.55306, 310.74423, 296.80608, 214.56859), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.1840544, 11.111111, 10, 10, 12.5, 10, 0.74314427, 1.9878756, 10, 0, 0, 10, 7.3350068, 25.517647, 10, 26.989766, 43.604406, 10, 0, 0, 10, 21.344686, 26.724319, 10, 2.6348908, 7.2351621, 10, 21.298615, 12.5, 10, 40, 44.643278, 10, 10, 0, 10, 33.493936, 27.945681, 10, 4.3287276, 16.089351, 10, 25.258173, 25.120998, 10, 23.39578, 28.363338, 10, 44.647888, 71.809601, 10, 62.617108, 84.693757, 10, 66.001318, 71.752151, 10, 54.740466, 55.087656), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.046651357, 0.022479034, 0.083769211, 0.082365248), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.49123587, 0.2668344, 0.64496483, 0.65218675), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10))
	expect_equal(x$iterations[2, ], c(10, 10))
	expect_equal(x$iterations[3, ], c(8, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.2, 0, 1, 0.5, 0.3, 1, 0.2, 0.2, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0, 0, 0, 0.2, 0, 0, 0.2, 0.2, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0))
	expect_equal(x$futilityStop, c(0, 0))
	expect_equal(x$earlyStop, c(0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(238.96461, 281.13648), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.1060693, 12.5, 10, 20, 25, 10, 4.7297328, 25.346201, 10, 18.776011, 38.686485, 10, 2.8470245, 10.408309, 10, 11.298615, 0, 10, 26.795872, 25.5, 10, 3.2314462, 14.141225, 10, 35.478699, 73.75451, 10, 53.306071, 77.82771), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.064857702, 0.041878984), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.72573181, 0.45099208), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms =  4, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 9, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0, 0, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.4, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.6, 0.6, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.4), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0, 0.3, 0.6, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.2, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(295.76875, 343.71408, 335.10548, 281.56474), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.0357205, 10, 10, 30, 33.333333, 10, 0.59871171, 1.0418812, 10, 0, 0, 10, 7.3350068, 22.965882, 10, 16.989766, 27.64836, 10, 0, 0, 10, 21.344686, 16.702699, 10, 13.17796, 20, 10, 15.323901, 2.6274327, 10, 40, 44.444444, 10, 10, 0, 10, 25.447372, 22.922435, 10, 7.2951578, 22.222222, 10, 38.282522, 25.259795, 10, 36.742398, 42.916408, 10, 46.996059, 75.888318, 10, 69.608825, 85.831349, 10, 78.881233, 70.74612, 10, 68.087084, 59.619107), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.042062266, 0.013174936, 0.075843331, 0.053971766), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.41527426, 0.27301585, 0.35639557, 0.62491311), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0, 0.3, 0, 0.2, 0.3, 0, 0.6, 0.2, 0, 0, 0.4, 0, 0.1, 0.7, 0, 0.4, 0.6, 0, 0.7, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.4, 0.8, 1, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(1050, 891.96665, 849.19143, 705.05343), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8, 10, 100, 100, 10, 87.259377, 81.133954, 10, 94.901963, 64.936322, 10, 98.210686, 32.8), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0086377938, 0.22005253, 0.081022458, 0.15135806), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.17779298, 0.23451185, 0.45925582, 0.77364695), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 8, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.5, 1, 0.7, 0.6, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.9, 0.7, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.6, 0.4, 1, 0.4, 0.2, 1, 0.7, 0.7, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0.9, 0.7, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.3, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.3, 0, 0.3, 0.2, 0, 0.2, 0.2, 0, 0.1, 0.3, 0, 0.1, 0.4, 0, 0.1, 0.3, 0, 0.6, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.5, 0.9, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(591.09538, 503.05596, 452.93301, 405.41488), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 42.50248, 47.078471, 10, 45.384313, 50.975979, 10, 10, 12.5, 10, 29.554131, 37.5, 10, 15.855942, 30, 10, 22.437029, 19.843895, 10, 72.307665, 59.768075, 10, 30.61074, 15.281075, 10, 47.430714, 50, 10, 35.976108, 53.08315, 10, 50.052941, 40.398451, 10, 31.50186, 5.7250423, 10, 60.784176, 67.078471, 10, 46.971175, 44.173288, 10, 20.632484, 31.869624, 10, 71.666731, 33.506118, 10, 83.286657, 97.078471, 10, 75.384313, 84.038156, 10, 76.496545, 72.268075, 10, 81.666731, 46.006118), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.061919533, 0.10420825, 0.16753344, 0.13874821), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.29816652, 0.52092951, 0.66819594, 0.56533632), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 7, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0, 0, 1, 0.1, 0.1, 1, 0.5, 0.4, 1, 0.1, 0, 1, 0.3, 0.2, 1, 0.3, 0.2, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0, 0, 1, 0.1, 0, 1, 0.4, 0.4, 1, 0.6, 0.5, 1, 0.8, 0.5, 1, 0.5, 0.3, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.7, 1, 1, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.3, 1.2, 1.1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.2, 1, 1, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.2, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0, 0.1, 0.4, 0, 0.3, 0.5, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.4, 0.6, 0.8, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.4, 0.5, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(436.56282, 365.15193, 284.70045, 253.12175), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 4.7999536, 10, 10, 16.971175, 11.111111, 10, 0, 0, 10, 10, 16.666667, 10, 35.332961, 40, 10, 10, 0, 10, 21.400604, 22.595075, 10, 21.344686, 22.270265, 10, 23.218148, 30, 10, 22.202225, 23.298934, 10, 0, 0, 10, 10, 0, 10, 29.860691, 40, 10, 41.405234, 49.459866, 10, 62.809861, 31.890295, 10, 22.672359, 23.636115, 10, 73.351063, 100, 10, 73.607458, 83.869911, 10, 74.210465, 54.485369, 10, 64.017046, 62.573047), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.024687171, 0.015314975, 0.045856815, 0.050229622), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.1883251, 0.40048173, 0.51841906, 0.54348956), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignInverseNormal(informationRates = c(0.2, 0.6, 1)), activeArms =  4, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(9, 8, 8, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.4, 0.4, 1, 0.1, 0, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.3, 1, 0.6, 0.3, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.2, 0.1, 0, 0, 0.3, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0.2, 0.2, 0, 0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.2, 0.4, 0.7, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.2, 0.2, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.1, 0.2, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(222.21727, 277.8712, 297.53775, 227.3405), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.1840544, 1.315616, 10, 10, 12.5, 10, 0.74314427, 0.92893034, 10, 0, 0, 10, 7.3350068, 8.1500075, 10, 26.989766, 33.737207, 10, 0, 0, 10, 21.344686, 40, 10, 2.6348908, 2.9276564, 10, 21.298615, 12.5, 10, 40, 50, 10, 10, 0, 10, 33.493936, 33.674217, 10, 4.3287276, 5.4109095, 10, 25.258173, 21.280514, 10, 23.39578, 27.859565, 10, 44.647888, 46.067497, 10, 62.617108, 64.148116, 10, 66.001318, 72.209444, 10, 54.740466, 67.859565), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.046651357, 0.022479034, 0.083769211, 0.082365248), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.39772697, 0.18083546, 0.60828997, 0.66318671), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 10, 10, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[3, ], c(4, 4, 4, 4))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0.2, 0, 0, 0.2, 0.3, 0, 0, 0, 0.1, 0, 0.2, 0, 0.4, 0.2, 0, 0.7, 0.2, 0, 0.2, 0.1, 0.1, 0.2, 0.3, 0, 0.7, 0.3, 0, 0.8, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.6, 1, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(1050, 914.65115, 996.33236, 1027.6565), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652, 10, 100, 100, 10, 86.465115, 86.465115, 10, 94.633236, 94.633236, 10, 97.765652, 97.765652), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.015572779, 0.22941785, 0.084615364, 0.1668833), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.10350918, 0.24229761, 0.63483372, 0.79913622), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 8, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.5, 0.5, 1, 0.7, 0.6, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.9, 0.7, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.5, 0.5, 1, 0.6, 0.4, 1, 0.4, 0.1, 1, 0.7, 0.7, 1, 0.5, 0.4, 1, 0.4, 0.4, 1, 0.9, 0.6, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.2, 0.3, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0.2, 0, 0.5, 0, 0, 0.3, 0, 0, 0.2, 0.1, 0, 0.1, 0.1, 0, 0.1, 0, 0.1, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.4, 0.6, 0.7, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(541.86022, 465.03543, 438.85623, 427.93855), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 42.315846, 42.315846, 10, 43.044196, 41.478749, 10, 10, 12.5, 10, 28.887554, 41.267934, 10, 15.358913, 15.358913, 10, 21.683959, 24.093288, 10, 70.857557, 63.571946, 10, 27.933797, 39.905424, 10, 46.61779, 46.61779, 10, 34.631951, 38.479946, 10, 49.194842, 36.493552, 10, 31.168408, 1.6691539, 10, 59.660857, 59.660857, 10, 44.698358, 43.316707, 10, 19.566345, 24.457932, 10, 67.989758, 54.271083, 10, 81.976703, 81.976703, 10, 72.029232, 73.684344, 10, 74.809372, 68.511715, 10, 77.989758, 68.556797), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.085169097, 0.1203719, 0.19239671, 0.15260753), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.20442999, 0.2985599, 0.51072411, 0.55234699), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms =  4, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(9, 8, 5, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.5, 0.5, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.2, 0, 1, 0.3, 0.2, 1, 0.5, 0.4, 1, 0, 0, 1, 0.2, 0.1, 1, 0.4, 0.4, 1, 0.5, 0.5, 1, 0.8, 0.3, 1, 0.4, 0.1, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.5, 1, 1, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.3, 1.1, 1.1, 1.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.3333333, 1.125, 1.2, 1.25), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0, 0, 0.2, 0, 0.5, 0.2, 0, 0.3, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.4, 0.4, 0.7, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0.1, 0.2, 0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.2, 0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.3, 0.2, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(345.54374, 302.86662, 257.52897, 218.07751), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 4.0457324, 4.4952582, 10, 0, 0, 10, 0, 0, 10, 19.775564, 49.43891, 10, 32.822644, 36.469604, 10, 3.2126584, 0, 10, 19.95956, 39.919121, 10, 3.856971, 0, 10, 22.866081, 14.295646, 10, 30.227444, 37.094169, 10, 0, 0, 10, 11.772035, 4.4300867, 10, 28.302956, 31.447728, 10, 36.658936, 45.82367, 10, 61.237846, 22.483886, 10, 27.086584, 14.869399, 10, 69.734457, 66.371619, 10, 60.099038, 70.417839, 10, 72.562982, 45.134158, 10, 56.543394, 53.868997), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.036728598, 0.116905, 0.051363618, 0.090419013), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.094248246, 0.38959944, 0.26721433, 0.10956309), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 9, 8))
	expect_equal(x$iterations[3, ], c(9, 7, 6))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.1, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.4, 0.3, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 1, 0.9, 1, 0.9, 0.7, 1, 0.8, 0.6), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(329.27319, 340.03958, 255.41029), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.2856401, 11.111111, 10, 22.222222, 14.285714, 10, 7.7416781, 0, 10, 8.8829625, 33.333333, 10, 27.982564, 28.571429, 10, 16.133764, 16.666667, 10, 13.667498, 22.222222, 10, 26.613998, 36.975546, 10, 37.998017, 55.643583, 10, 25.800496, 33.333333, 10, 11.111111, 14.285714, 10, 1.5241424, 14.334858, 10, 49.636596, 100, 10, 87.929896, 94.118403, 10, 63.397602, 86.645107), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.035168824, 0.072496943, 0.033790406), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.23779127, 0.21714632, 0.49736351), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 9, 8, 10))
	expect_equal(x$iterations[3, ], c(7, 8, 1, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.4, 0.3, 1, 0.5, 0.2, 1, 0.2, 0.1, 1, 0.6, 0.1, 1, 0.4, 0, 1, 1, 0.7, 1, 0.9, 0.8, 1, 0.8, 0.1, 1, 1, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0.1, 0.2, 0, 0, 0.1, 0.1, 0.2, 0.3, 0.1, 0.1, 0.3, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.2, 0.8, 0.7), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.3, 0.2, 0.9, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.3, 0.1, 0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(260.7309, 321.58944, 127.72687, 265.63922), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.1167748, 14.285714, 10, 5.9718999, 12.5, 10, 0.5, 0, 10, 10, 25, 10, 6.7277808, 37.50755, 10, 37.581628, 50, 10, 0, 0, 10, 10, 0, 10, 2.4005123, 14.285714, 10, 8.5110901, 18.608798, 10, 11.503905, 0, 10, 32.126443, 75, 10, 28.865098, 28.571429, 10, 15.610585, 12.5, 10, 24.075387, 100, 10, 15.693169, 0, 10, 39.110166, 94.650408, 10, 67.675203, 93.608798, 10, 36.079292, 100, 10, 67.819612, 100), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.064552587, 0.050542809, 0.13271614, 0.098246228), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.36314442, 0.3083032, 0.6049542, 0.15885798), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "userDefined", activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, adaptations = rep(TRUE, 2), 
		effectMatrix = matrix(c(0.1,0.2,0.3,0.4,0.2,0.3,0.4,0.5), ncol = 4),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10))
	expect_equal(x$iterations[2, ], c(10, 9))
	expect_equal(x$iterations[3, ], c(7, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.5, 0.4, 1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.5, 0.2, 1, 0.2, 0.1, 1, 1, 0.7, 1, 0.9, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0.2, 0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0))
	expect_equal(x$futilityStop, c(0, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0))
	expect_equal(x$successPerStage[2, ], c(0.3, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(238.16649, 275.50348), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.0395374, 14.285714, 10, 4.3933102, 11.199547, 10, 4.4634729, 31.899994, 10, 38.793234, 57.142857, 10, 2.5722467, 14.285714, 10, 5.3695979, 6.9814836, 10, 23.677991, 28.571429, 10, 11.241946, 8.8667681, 10, 31.753247, 89.042851, 10, 59.798088, 84.190656), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.095374468, 0.085831831), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.56669649, 0.49770257), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "sigmoidEmax", gED50 = 2, slope = 0.5, activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 9, 8, 10))
	expect_equal(x$iterations[3, ], c(9, 9, 5, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.2, 0.2, 1, 0.3, 0.3, 1, 0.2, 0.1, 1, 0.2, 0.2, 1, 0.4, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.4, 0.2, 1, 1, 0.9, 1, 0.9, 0.9, 1, 0.8, 0.5, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0.1, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.1, 0.1, 0.5, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(302.82831, 359.55539, 193.16056, 326.21609), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0.96871141, 11.111111, 10, 4.8692533, 11.111111, 10, 0.5, 0, 10, 30, 42.857143, 10, 6.7277808, 29.172539, 10, 37.581628, 44.444444, 10, 12.5, 20, 10, 10, 0, 10, 12.834638, 22.222222, 10, 21.991558, 33.249006, 10, 17.610119, 20, 10, 12.962323, 28.571429, 10, 24.585127, 27.825125, 10, 7.6171061, 11.111111, 10, 20.182233, 21.892795, 10, 22.561443, 17.977538, 10, 45.116257, 90.330997, 10, 72.059546, 99.915673, 10, 50.792352, 61.892795, 10, 75.523767, 89.406109), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.054394525, 0.033810654, 0.16623293, 0.07472066), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.39787587, 0.27550431, 0.61914721, 0.24074486), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 9, 9, 10))
	expect_equal(x$iterations[3, ], c(10, 8, 8, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.6, 0.5, 1, 0.2, 0.1, 1, 0.7, 0.5, 1, 0.7, 0.6, 1, 0.8, 0.7, 1, 0.7, 0.7, 1, 0.7, 0.7, 1, 0.7, 0.7, 1, 0.8, 0.7, 1, 0.7, 0.7, 1, 0.9, 0.9, 1, 0.6, 0.6, 1, 0.6, 0.5, 1, 0.7, 0.6, 1, 0.8, 0.8, 1, 1, 1, 1, 0.9, 0.8, 1, 0.9, 0.8, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2.6, 3.1111111, 2.5555556, 3.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(2.5, 3, 2.625, 2.9), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0.1, 0, 0, 0, 0, 0.2, 0, 0.1, 0.2, 0, 0, 0, 0, 0.2, 0.1, 0, 0.3, 0.3, 0, 0.5, 0.4, 0, 0.1, 0, 0, 0.3, 0.1, 0, 0.5, 0.2, 0.1, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0.6, 0.9, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0, 0.2, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(690.38911, 619.77858, 554.02061, 670.88154), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 54.180167, 50.4, 10, 57.917242, 50.5, 10, 16.188147, 12.5, 10, 64.800747, 25.135561, 10, 65.454083, 50.4, 10, 71.01474, 75.5, 10, 71.743702, 62.866861, 10, 64.800747, 45.135561, 10, 69.120607, 60.4, 10, 71.01474, 75.5, 10, 71.743702, 62.866861, 10, 84.800747, 55.535561, 10, 55.454083, 50.4, 10, 48.792518, 50.5, 10, 71.743702, 50.366861, 10, 74.800747, 45.535561, 10, 94.180167, 90.4, 10, 82.125851, 88, 10, 93.965925, 75.366861, 10, 94.800747, 65.535561), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.086326519, 0.23897424, 0.15375141, 0.19252038), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.19907656, 0.37086672, 0.52811383, 0.57866018), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(8, 8, 9, 10))
	expect_equal(x$iterations[3, ], c(8, 8, 7, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.1, 1, 0.6, 0.6, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.2, 0.2, 1, 0.3, 0.1, 1, 0.5, 0.5, 1, 0.3, 0.3, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0.5, 0.3, 1, 0.8, 0.8, 1, 0.7, 0.7, 1, 0.5, 0.4, 1, 0.7, 0.4, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 0.9, 0.7, 1, 1, 0.7), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 1.7777778, 1.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.875, 2, 1.7142857, 1.8571429), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.2, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0.1, 0.1, 0, 0.2, 0.1, 0, 0.1, 0.1, 0.1, 0.2, 0.3, 0, 0.1, 0.4, 0.1, 0.6, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.2, 0.6, 0.7, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.2, 0.2, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.2, 0.2, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.2, 0.2, 0.3, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.1, 0.1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(485.19749, 377.01763, 429.89127, 345.60572), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 25, 12.5, 10, 52.984739, 51, 10, 38.255848, 57.142857, 10, 5.1691192, 14.285714, 10, 28.803833, 37.5, 10, 25, 25, 10, 24.228929, 12.568187, 10, 28.635967, 34.757362, 10, 31.69512, 37.5, 10, 5.6938105, 1.5787961, 10, 40.9155, 42.857143, 10, 42.851335, 17.605103, 10, 85.498953, 100, 10, 58.678549, 52.578796, 10, 35.341046, 55.42533, 10, 50.953751, 18.295116, 10, 85.498953, 100, 10, 71.178549, 65.078796, 10, 76.256545, 98.282473, 10, 73.805086, 49.614505), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.017664185, 0.17480419, 0.093445917, 0.088580327), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.16524243, 0.38443342, 0.40637997, 0.6510419), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(9, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(7, 7, 5, 5))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.4, 0.3, 1, 0.4, 0.2, 1, 0, 0, 1, 0.2, 0.1, 1, 0.2, 0.1, 1, 0.3, 0.1, 1, 0.6, 0.5, 1, 0.3, 0.1, 1, 0.4, 0.4, 1, 0.3, 0.3, 1, 0.6, 0.1, 1, 0.6, 0.4, 1, 0.9, 0.7, 1, 1, 0.7, 1, 1, 0.5, 1, 1, 0.5), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.2222222, 1.2, 1.3, 1.2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.2857143, 1.1428571, 1.4, 1.2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0.1, 0, 0.2, 0, 0, 0.1, 0.4, 0, 0.2, 0, 0, 0, 0.1, 0, 0, 0, 0.2, 0.3, 0.1, 0, 0.2, 0.4), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.2, 0.9, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.1, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.2, 0.1, 0, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.3, 0.3, 0.5, 0.5), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.2, 0.5, 0.4), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(328.39002, 299.51382, 285.23022, 240.4545), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 4.4952582, 14.285714, 10, 19.967039, 28.571429, 10, 10, 20, 10, 10, 0, 10, 21.883735, 42.857143, 10, 26.51119, 28.571429, 10, 0, 0, 10, 13.162215, 6.3433684, 10, 14.295646, 14.285714, 10, 12.191217, 7.901455, 10, 34.361222, 100, 10, 22.260169, 5.4863466, 10, 27.97297, 57.142857, 10, 13.444855, 26.007756, 10, 23.167319, 20, 10, 26.747723, 50.618475, 10, 62.896861, 100, 10, 55.457645, 83.150613, 10, 47.701679, 100, 10, 59.007892, 56.104821), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.025620238, 0.099222073, 0.15711506, 0.067612991), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.2137719, 0.23826695, 0.15636561, 0.6965125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 9, 8, 10))
	expect_equal(x$iterations[3, ], c(7, 8, 1, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.1, 0, 1, 0.1, 0.1, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.4, 0.3, 1, 0.5, 0.2, 1, 0.2, 0.1, 1, 0.6, 0.1, 1, 0.4, 0, 1, 1, 0.7, 1, 0.9, 0.8, 1, 0.8, 0.1, 1, 1, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0, 0.1, 0, 0.2, 0.3, 0, 0.1, 0.3, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.1, 0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.3, 0.2, 0.9, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.3, 0.1, 0.7, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(179.95701, 273.63073, 113.26043, 249.89211), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1.1167748, 1.5953926, 10, 5.9718999, 6.7183874, 10, 0.5, 0, 10, 10, 25, 10, 6.7277808, 9.6111155, 10, 37.581628, 42.279332, 10, 0, 0, 10, 10, 0, 10, 2.4005123, 3.4293032, 10, 8.5110901, 9.5749763, 10, 11.503905, 0, 10, 32.126443, 55.316107, 10, 28.865098, 22.318956, 10, 15.610585, 5.061908, 10, 24.075387, 27.667829, 10, 15.693169, 0, 10, 39.110166, 36.954767, 10, 67.675203, 63.634604, 10, 36.079292, 27.667829, 10, 67.819612, 80.316107), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.064552587, 0.050542809, 0.13271614, 0.098246228), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.1164829, 0.22353174, 0.16556673, 0.12567304), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "all", 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 7, 9, 10))
	expect_equal(x$iterations[3, ], c(9, 6, 8, 9))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.6, 0.6, 1, 0.6, 0.6, 1, 0.5, 0.4, 1, 0.7, 0.7, 1, 1, 0.9, 1, 0.6, 0.6, 1, 0.7, 0.6, 1, 0.7, 0.7, 1, 0.7, 0.7, 1, 0.7, 0.6, 1, 0.5, 0.4, 1, 0.9, 0.8, 1, 0.8, 0.8, 1, 1, 0.9, 1, 0.7, 0.6, 1, 0.9, 0.8, 1, 1, 0.9), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(2.5, 2.8571429, 3, 3.1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(2.6666667, 2.8333333, 3.25, 3.3333333), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0.2, 0.1, 0.1, 0.4, 0, 0, 0.1, 0, 0.1, 0.2, 0, 0.5, 0, 0.1, 0.2, 0.3, 0, 0.3, 0, 0, 0.3, 0.1, 0, 0.7, 0.1, 0.1, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.4, 0.5, 0.9, 0.9), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0.3, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.1, 0.4, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.1, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.1, 0.1), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(700.66781, 479.0483, 750, 754), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 56.333476, 62.592752, 10, 42.857143, 50, 10, 44.444444, 50, 10, 50.4, 56, 10, 56.333476, 62.592752, 10, 52.89273, 58.728605, 10, 77.777778, 87.5, 10, 90.4, 89.333333, 10, 60, 66.666667, 10, 81.464159, 92.061938, 10, 77.777778, 87.5, 10, 60.4, 67.111111, 10, 66.333476, 62.592752, 10, 52.89273, 58.728605, 10, 100, 100, 10, 70.4, 78.222222, 10, 96.333476, 95.926085, 10, 81.464159, 92.061938, 10, 100, 100, 10, 90.4, 89.333333), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.014835699, 0.082104288, 0.088043543, 0.18689602), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.27215045, 0.40442511, 0.77949108, 0.5761862), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "rbest", rValue = 2,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(9, 9, 8, 10))
	expect_equal(x$iterations[3, ], c(9, 9, 6, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.2, 0.2, 1, 0.6, 0.6, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.3, 0.1, 1, 0.5, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.8, 0.8, 1, 0.8, 0.8, 1, 0.5, 0.4, 1, 0.8, 0.6, 1, 0.9, 0.9, 1, 0.9, 0.9, 1, 0.8, 0.6, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.8888889, 2, 1.75, 2), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.8888889, 2, 1.6666667, 2), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0.2, 0, 0.1, 0.3, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.2, 0, 0.1, 0.1, 0.1, 0.2, 0.2, 0, 0.1, 0.1, 0, 0.5, 0.1), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.5, 0.4, 1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.1, 0.1, 0.2, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.1, 0.1, 0.4, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.2, 0, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(444.18332, 451.65704, 316.99616, 403.16589), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 22.222222, 22.222222, 10, 47.097546, 47.097546, 10, 30.537829, 40.717105, 10, 25.370782, 31.713478, 10, 33.228314, 33.228314, 10, 11.111111, 11.111111, 10, 27.257545, 3.0100597, 10, 38.448036, 24.799564, 10, 17.763874, 17.763874, 10, 27.283387, 27.283387, 10, 33.529937, 28.039916, 10, 22.273651, 27.842063, 10, 69.075708, 69.075708, 10, 63.269822, 63.269822, 10, 39.758676, 36.344902, 10, 50.237878, 39.536866, 10, 76.700615, 76.700615, 10, 74.380933, 74.380933, 10, 73.288614, 64.384818, 10, 68.165174, 61.945985), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.0535706, 0.15544115, 0.10470149, 0.094637028), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.30445768, 0.40002122, 0.30386238, 0.85955358), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0, typeOfSelection = "epsilon", epsilonValue = 0.1,
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = c(TRUE, FALSE),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(9, 9, 10, 10))
	expect_equal(x$iterations[3, ], c(8, 8, 5, 4))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.4, 0.3, 1, 0, 0, 1, 0.3, 0.3, 1, 0.3, 0.3, 1, 0, 0, 1, 0.3, 0.1, 1, 0.3, 0.2, 1, 0.2, 0.2, 1, 0.2, 0.2, 1, 0.1, 0, 1, 0.3, 0.3, 1, 0.5, 0.4, 1, 0.5, 0.1, 1, 0.6, 0.3, 1, 0.9, 0.8, 1, 0.9, 0.8, 1, 1, 0.5, 1, 1, 0.4), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1.1111111, 1.2222222, 1.1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[3, ], c(1.125, 1.25, 1.2, 1), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0.1, 0.1, 0, 0, 0.1, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0.1, 0.1, 0, 0.2, 0, 0.5, 0, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.3, 0.4, 0.7, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0.1, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.2, 0.2, 0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.1, 0.5, 0.6), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.2, 0.2, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(293.98441, 248.56782, 313.91469, 200.56497), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 4.4952582, 5.0571655, 10, 11.111111, 12.5, 10, 30.407004, 59.494569, 10, 0, 0, 10, 19.514172, 21.953443, 10, 21.967417, 24.713344, 10, 0, 0, 10, 21.272121, 25, 10, 25.406757, 16.082601, 10, 11.52108, 12.961215, 10, 16.622221, 33.244443, 10, 10, 0, 10, 25.603407, 28.803833, 10, 20.034041, 22.038296, 10, 38.558614, 20, 10, 21.010924, 32.498597, 10, 71.638409, 68.09321, 10, 46.126253, 51.392035, 10, 75.587839, 92.739012, 10, 52.283045, 57.498597), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.023159424, 0.14301241, 0.046563399, 0.11230633), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.12194648, 0.18993668, 0.37583316, 0.33709374), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), activeArms = 4, threshold = 0.1, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.1, 0.3, 0.1), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 1)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(1, 1, 1))
	expect_equal(x$iterations[2, ], c(1, 1, 1))
	expect_equal(x$iterations[3, ], c(0, 1, 1))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(NaN, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
	expect_equal(x$rejectAtLeastOne, c(0, 0, 0))
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(1, 0, 0))
	expect_equal(x$futilityStop, c(1, 0, 0))
	expect_equal(x$earlyStop, c(1, 0, 0))
	expect_equal(x$successPerStage[1, ], c(0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0, 0))
	expect_equal(x$successPerStage[3, ], c(0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(NaN, 450, 148.90979), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 32.875253, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 100, 100, 10, 10.358511, 39.096382, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 32.875253, 0, 10, 100, 100, 10, 10.358511, 39.096382), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.011749146, 0.0034013018, 0.045375018), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(NaN, 0.15769372, 0.8), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmMeans': using calcSubjectsFunction", {

	.skipTestIfDisabled()

	myFunction <- function(..., stage, minNumberOfSubjectsPerStage) {
		return(ifelse(stage == 3, 33, minNumberOfSubjectsPerStage[stage]))
	}

	x <- getSimulationMultiArmMeans(seed = 1234, getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		minNumberOfSubjectsPerStage = c(10, 4, 4), maxNumberOfSubjectsPerStage = c(10, 100, 100),  
		maxNumberOfIterations = 10, calcSubjectsFunction = myFunction)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(9, 9, 8, 8))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.1, 0.1, 1, 0.1, 0.1, 1, 0.1, 0, 1, 0, 0, 1, 0.3, 0.3, 1, 0.4, 0.4, 1, 0, 0, 1, 0.3, 0.2, 1, 0.1, 0.1, 1, 0.3, 0.2, 1, 0.4, 0.4, 1, 0.1, 0.1, 1, 0.5, 0.4, 1, 0.2, 0.2, 1, 0.5, 0.4, 1, 0.6, 0.5, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 0.8, 1, 1, 0.8), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0.1, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0.1, 0.1, 0.2, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(77.2, 77.2, 76.4, 76.4), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 1, 0.44444444, 10, 1, 0.44444444, 10, 1, 0, 10, 0, 0, 10, 3, 1.3333333, 10, 4, 1.7777778, 10, 0, 0, 10, 3, 1, 10, 1, 0.44444444, 10, 3, 0.88888889, 10, 4, 2, 10, 1, 0.5, 10, 5, 1.7777778, 10, 2, 0.88888889, 10, 5, 2, 10, 6, 2.5, 10, 10, 4, 10, 10, 4, 10, 10, 4, 10, 10, 4), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.070329785, 0.020530181, 0.12710343, 0.087091186), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.071302049, 0.04578464, 0.10481847, 0.1422122), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmMeans': using selectArmsFunction", {

	.skipTestIfDisabled()

	mySelectionFunction <- function(effectSizes) {
		return(c(TRUE, FALSE, FALSE, FALSE))
	}

	x <- getSimulationMultiArmMeans(seed = 1234, 
		getDesignFisher(informationRates = c(0.2, 0.6, 1)), typeOfShape = "linear", activeArms =  4, 
		plannedSubjects = c(10, 30, 50), stDev = 1.2, muMaxVector = seq(0.3, 0.6, 0.1), adaptations = rep(TRUE, 2),
		maxNumberOfIterations = 10, selectArmsFunction = mySelectionFunction, typeOfSelection = "userDefined")

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[2, ], c(10, 10, 10, 10))
	expect_equal(x$iterations[3, ], c(10, 9, 9, 10))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0.9, 1, 1, 0.9, 1, 1, 1), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(4, 4, 4, 4))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0, 0, 0.1, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.1, 0, 0), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.1, 0.1, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(x$futilityStop, c(0, 0, 0, 0))
	expect_equal(x$earlyStop, c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x$successPerStage[2, ], c(0, 0.1, 0.1, 0), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0, 0))
	expect_equal(x$expectedNumberOfSubjects, c(130, 126, 126, 130))
	expect_equal(unlist(as.list(x$sampleSizes)), c(10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 0, 0, 10, 20, 20, 10, 20, 20, 10, 20, 20, 10, 20, 20))
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.091251689, 0.027836233, 0.13855746, 0.12908437), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.071420101, 0.027813347, 0.076509581, 0.21688562), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmMeans': using intersectionTest = 'Sidak' and typeOfSelection = 'rbest'", {

	.skipTestIfDisabled()

	designIN <- getDesignInverseNormal(typeOfDesign = "P", kMax = 3, futilityBounds = c(0, 0))
	x <- getSimulationMultiArmMeans(designIN, activeArms = 3, typeOfShape = "sigmoidEmax", 
		muMaxVector = seq(0, 1, 0.2), gED50 = 2, plannedSubjects = cumsum(rep(20, 3)), 
		intersectionTest = "Sidak", typeOfSelection = "rbest", rValue = 2, threshold = -Inf, 
		successCriterion = "all", maxNumberOfIterations = 100, seed = 3456)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(42, 52, 69, 77, 88, 87))
	expect_equal(x$iterations[3, ], c(30, 33, 61, 73, 80, 61))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.25, 0.17, 1, 0.25, 0.16, 1, 0.31, 0.26, 1, 0.32, 0.3, 1, 0.42, 0.41, 1, 0.32, 0.26, 1, 0.32, 0.22, 1, 0.43, 0.26, 1, 0.48, 0.45, 1, 0.56, 0.54, 1, 0.63, 0.56, 1, 0.7, 0.47, 1, 0.27, 0.21, 1, 0.36, 0.24, 1, 0.59, 0.51, 1, 0.66, 0.62, 1, 0.71, 0.63, 1, 0.72, 0.49, 1, 0.42, 0.3, 1, 0.52, 0.33, 1, 0.69, 0.61, 1, 0.77, 0.73, 1, 0.88, 0.8, 1, 0.87, 0.61), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(3, 3, 3, 3, 3, 3))
	expect_equal(x$numberOfActiveArms[2, ], c(2, 2, 2, 2, 2, 2))
	expect_equal(x$numberOfActiveArms[3, ], c(2, 2, 2, 2, 2, 2))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.01, 0, 0, 0, 0.01, 0.01, 0.02, 0.03, 0.01, 0.02, 0.01, 0.01, 0.06, 0.1, 0.04, 0.04, 0.01, 0, 0, 0.01, 0.02, 0, 0.03, 0, 0.03, 0.04, 0.06, 0.08, 0.08, 0.11, 0.1, 0.14, 0.27, 0.12, 0, 0, 0, 0, 0.01, 0, 0.02, 0.01, 0.08, 0.08, 0.05, 0.11, 0.09, 0.16, 0.13, 0.18, 0.25, 0.24), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.02, 0.03, 0.18, 0.33, 0.49, 0.8), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.58, 0.48, 0.31, 0.22, 0.11, 0.07), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.12, 0.18, 0.08, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.7, 0.66, 0.39, 0.23, 0.11, 0.07), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.7, 0.67, 0.39, 0.27, 0.2, 0.39), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0, 0.01, 0.01, 0.06), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0, 0.01, 0, 0.03, 0.08, 0.26), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0, 0, 0.03, 0.1, 0.16, 0.2), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(123.2, 131, 158, 170, 180.8, 168.8), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(20, 11.904762, 11.333333, 20, 9.6153846, 9.6969697, 20, 8.9855072, 8.5245902, 20, 8.3116883, 8.2191781, 20, 9.5454545, 10.25, 20, 7.3563218, 8.5245902, 20, 15.238095, 14.666667, 20, 16.538462, 15.757576, 20, 13.913043, 14.754098, 20, 14.545455, 14.794521, 20, 14.318182, 14, 20, 16.091954, 15.409836, 20, 12.857143, 14, 20, 13.846154, 14.545455, 20, 17.101449, 16.721311, 20, 17.142857, 16.986301, 20, 16.136364, 15.75, 20, 16.551724, 16.065574, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.058967382, 0.048523877, 0.17154294, 0.22180985, 0.2182802, 0.37414282), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.077820194, 0.14430526, 0.21266388, 0.28752608, 0.40185892, 0.5016109), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

})

test_that("'getSimulationMultiArmMeans': plot drift - comparison of raw values", {

	.skipTestIfDisabled()

	designPureConditionalDunnett <- getDesignInverseNormal(typeOfDesign = "asUser", userAlphaSpending = c(0, 0.025))
	designCombinationDunnett <- getDesignConditionalDunnett(informationAtInterim = 0.5, secondStageConditioning = TRUE)

	resultsPureConditionalDunnett <- getSimulationMultiArmMeans(designPureConditionalDunnett, activeArms = 3, muMaxVector = seq(0, 1, 0.2), 
		typeOfShape = "linear", plannedSubjects = cumsum(rep(20, 2)), intersectionTest = "Dunnett", 
		adaptations = TRUE, typeOfSelection = "best",  effectMeasure = "effectDifference", 
		threshold = -Inf, maxNumberOfIterations = 100, 
		allocationRatioPlanned = 1, seed = 123)

	resultsCombinationDunnett <- getSimulationMultiArmMeans(designCombinationDunnett, activeArms = 3, muMaxVector = seq(0, 1, 0.2), 
		typeOfShape = "linear", plannedSubjects = cumsum(rep(20, 2)), intersectionTest = "Dunnett", 
		adaptations = TRUE, typeOfSelection = "best",  effectMeasure = "effectDifference", 
		threshold = -Inf, maxNumberOfIterations = 100, 
		allocationRatioPlanned = 1, seed = 123)

	drift <- resultsPureConditionalDunnett$effectMatrix[nrow(resultsPureConditionalDunnett$effectMatrix), ]

	## Comparison of the results of numeric object 'drift' with expected results
	expect_equal(drift, c(0, 0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)
	expect_equal(resultsPureConditionalDunnett$rejectAtLeastOne, resultsCombinationDunnett$rejectAtLeastOne, tolerance = 0.06)

})

test_that("'getSimulationMultiArmMeans': comparison of base and multi-arm", {

	.skipTestIfDisabled()

	allocationRatioPlanned <- 2

	design <- getDesignInverseNormal(typeOfDesign = "WT", deltaWT = 0.15, futilityBounds = c(-0.5, 0), informationRates = c(0.4, 0.8, 1))
	x <- getSimulationMultiArmMeans(design = design, activeArms = 1, 
		plannedSubjects = c(20, 40, 60), stDev = 1.5, muMaxVector = seq(0, 1, 0.2), 
		conditionalPower = 0.80, minNumberOfSubjectsPerStage = c(NA, 20, 20), 
		maxNumberOfSubjectsPerStage = c(NA, 80, 80), #thetaH1 = 0.5, 
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(81, 88, 89, 88, 93, 79))
	expect_equal(x$iterations[3, ], c(53, 70, 64, 51, 37, 12))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.81, 0.53, 1, 0.88, 0.7, 1, 0.89, 0.64, 1, 0.88, 0.51, 1, 0.93, 0.37, 1, 0.79, 0.12, 1, 0.81, 0.53, 1, 0.88, 0.7, 1, 0.89, 0.64, 1, 0.88, 0.51, 1, 0.93, 0.37, 1, 0.79, 0.12), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0, 0, 0.01, 0, 0.05, 0.06, 0.01, 0.22, 0.16, 0.02, 0.37, 0.34, 0.06, 0.56, 0.31, 0.2, 0.67, 0.11), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.01, 0.11, 0.39, 0.73, 0.93, 0.98), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.19, 0.12, 0.1, 0.1, 0.01, 0.01), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.28, 0.13, 0.03, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.47, 0.25, 0.13, 0.1, 0.01, 0.01), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.47, 0.3, 0.36, 0.49, 0.63, 0.88), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0, 0, 0.01, 0.02, 0.06, 0.2), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0, 0.05, 0.22, 0.37, 0.56, 0.67), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.01, 0.06, 0.16, 0.34, 0.31, 0.11), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(182.97526, 204.64426, 195.25807, 156.41809, 139.22312, 94.296637), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(20, 74.777896, 78.138507, 20, 71.766138, 76.107578, 20, 69.720212, 75.189157, 20, 60.637889, 60.622327, 20, 55.732819, 56.713222, 20, 47.895918, 41.888746, 10, 37.388948, 39.069254, 10, 35.883069, 38.053789, 10, 34.860106, 37.594578, 10, 30.318944, 30.311164, 10, 27.86641, 28.356611, 10, 23.947959, 20.944373), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.22017652, 0.27054625, 0.3536952, 0.48224278, 0.56831776, 0.65933958), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.12006552, 0.18276066, 0.26908136, 0.50518351, 0.66786884, 0.67359844), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	allocationRatioPlanned <- 2
	factor <- 1 + 1/allocationRatioPlanned
	y <- getSimulationMeans(design, plannedSubjects = round(factor*c(20, 40, 60)), normalApproximation = TRUE,  stDev = 1.5, 
		conditionalPower = 0.80, minNumberOfSubjectsPerStage = round(factor*c(NA, 20, 20)), 
		maxNumberOfSubjectsPerStage = round(factor*c(NA, 80, 80)), alternative = seq(0, 1, 0.2), #thetaH1 = 0.5, 
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 5678)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(0.03, 0.07, -0.04, 0.01, 0.02, -0.02), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(0, 0.02, 0.01, -0.01, 0.04, -0.09), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(0.03, 0, -0.07, 0.06, 0.02, 0.03), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(0, 0.05, 0.02, -0.04, -0.04, 0.04), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.17, 0, 0.04, -0.04, 0, 0.02), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(-0.05, 0.01, 0, 0, 0, 0), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(comp4[2, ], c(-2.8, -1.3, -0.3, -0.1, -1.4, 10.8), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(1.7, -3.3, -3.4, 13.2, -9.7, -6.7), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-37.8, -8.9, -5.5, 10.1, -12.7, 15.8), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(-0.15, -0.03, 0.02, -0.01, -0.06, 0.04), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmMeans': configuration 1", {

	.skipTestIfDisabled()

	results1 <- getSimulationMultiArmMeans(getDesignInverseNormal(), plannedSubjects = c(20, 40, 60), 
		muMaxVector = 0.1, conditionalPower = 0.9,
		minNumberOfSubjectsPerStage = c(NA,10,10), maxNumberOfSubjectsPerStage = c(NA, 100, 100),
		maxNumberOfIterations = 20, seed = 99123)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'results1' with expected results
	expect_equal(results1$iterations[1, ], 20)
	expect_equal(results1$iterations[2, ], 20)
	expect_equal(results1$iterations[3, ], 20)
	expect_equal(unlist(as.list(results1$selectedArms)), c(1, 0.25, 0.25, 1, 0.45, 0.45, 1, 0.3, 0.3, 1, 1, 1), tolerance = 1e-07)
	expect_equal(results1$numberOfActiveArms[1, ], 3)
	expect_equal(results1$numberOfActiveArms[2, ], 1)
	expect_equal(results1$numberOfActiveArms[3, ], 1)
	expect_equal(unlist(as.list(results1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.05, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(results1$rejectAtLeastOne, 0.15, tolerance = 1e-07)
	expect_equal(results1$futilityPerStage[1, ], 0)
	expect_equal(results1$futilityPerStage[2, ], 0)
	expect_equal(results1$futilityStop, 0)
	expect_equal(results1$earlyStop, 0)
	expect_equal(results1$successPerStage[1, ], 0)
	expect_equal(results1$successPerStage[2, ], 0)
	expect_equal(results1$successPerStage[3, ], 0.15, tolerance = 1e-07)
	expect_equal(results1$expectedNumberOfSubjects, 453.10342, tolerance = 1e-07)
	expect_equal(unlist(as.list(results1$sampleSizes)), c(20, 25, 25, 20, 38.391383, 45, 20, 24.489976, 28.670353, 20, 87.881359, 98.670353), tolerance = 1e-07)
	expect_equal(results1$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(results1$conditionalPowerAchieved[2, ], 0.063515359, tolerance = 1e-07)
	expect_equal(results1$conditionalPowerAchieved[3, ], 0.14768102, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	}

})

test_that("'getSimulationMultiArmMeans': comparison of base and multi-arm, Fisher design", {

	.skipTestIfDisabled()

	allocationRatioPlanned <- 2

	design <- getDesignFisher(alpha0Vec = c(0.3, 0.4), informationRates = c(0.3, 0.6, 1))
	x <- getSimulationMultiArmMeans(design = design, activeArms = 1, 
		plannedSubjects = c(20, 40, 60), stDev = 1.5, muMaxVector = seq(0, 1, 0.2), 
		conditionalPower = 0.80, minNumberOfSubjectsPerStage = c(NA, 20, 20), 
		maxNumberOfSubjectsPerStage = c(NA, 80, 80), #thetaH1 = 0.5, 
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 1234)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'x' with expected results
	expect_equal(x$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x$iterations[2, ], c(28, 41, 50, 54, 56, 51))
	expect_equal(x$iterations[3, ], c(7, 24, 27, 21, 24, 7))
	expect_equal(unlist(as.list(x$selectedArms)), c(1, 0.28, 0.07, 1, 0.41, 0.24, 1, 0.5, 0.27, 1, 0.54, 0.21, 1, 0.56, 0.24, 1, 0.51, 0.07, 1, 0.28, 0.07, 1, 0.41, 0.24, 1, 0.5, 0.27, 1, 0.54, 0.21, 1, 0.56, 0.24, 1, 0.51, 0.07), tolerance = 1e-07)
	expect_equal(x$numberOfActiveArms[1, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[2, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(x$numberOfActiveArms[3, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(unlist(as.list(x$rejectedArmsPerStage)), c(0.02, 0, 0.01, 0.05, 0.01, 0.02, 0.05, 0.16, 0.07, 0.17, 0.3, 0.14, 0.24, 0.31, 0.2, 0.39, 0.44, 0.06), tolerance = 1e-07)
	expect_equal(x$rejectAtLeastOne, c(0.03, 0.08, 0.28, 0.61, 0.75, 0.89), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[1, ], c(0.7, 0.54, 0.45, 0.29, 0.2, 0.1), tolerance = 1e-07)
	expect_equal(x$futilityPerStage[2, ], c(0.21, 0.16, 0.07, 0.03, 0.01, 0), tolerance = 1e-07)
	expect_equal(x$futilityStop, c(0.91, 0.7, 0.52, 0.32, 0.21, 0.1), tolerance = 1e-07)
	expect_equal(x$earlyStop, c(0.93, 0.76, 0.73, 0.79, 0.76, 0.93), tolerance = 1e-07)
	expect_equal(x$successPerStage[1, ], c(0.02, 0.05, 0.05, 0.17, 0.24, 0.39), tolerance = 1e-07)
	expect_equal(x$successPerStage[2, ], c(0, 0.01, 0.16, 0.3, 0.31, 0.44), tolerance = 1e-07)
	expect_equal(x$successPerStage[3, ], c(0.01, 0.02, 0.07, 0.14, 0.2, 0.06), tolerance = 1e-07)
	expect_equal(x$expectedNumberOfSubjects, c(68.211396, 101.92536, 114.30453, 107.14861, 109.24288, 79.622055), tolerance = 1e-07)
	expect_equal(unlist(as.list(x$sampleSizes)), c(20, 70.979514, 80, 20, 71.410143, 77.800325, 20, 69.572428, 79.321509, 20, 66.884783, 72.926791, 20, 62.423876, 74.4634, 20, 55.785406, 66.154471, 10, 35.489757, 40, 10, 35.705072, 38.900163, 10, 34.786214, 39.660755, 10, 33.442392, 36.463396, 10, 31.211938, 37.2317, 10, 27.892703, 33.077236), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$conditionalPowerAchieved[2, ], c(0.53965216, 0.44870166, 0.54176291, 0.51257459, 0.62161545, 0.65580386), tolerance = 1e-07)
	expect_equal(x$conditionalPowerAchieved[3, ], c(0.33271205, 0.28302479, 0.35942136, 0.59988705, 0.63386368, 0.5469144), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x), NA)))
	    expect_output(print(x)$show())
	    invisible(capture.output(expect_error(summary(x), NA)))
	    expect_output(summary(x)$show())
	}

	allocationRatioPlanned <- 2
	factor <- 1 + 1 / allocationRatioPlanned
	y <- getSimulationMeans(design, plannedSubjects = round(factor*c(20, 40, 60)), normalApproximation = TRUE,  stDev = 1.5, 
		conditionalPower = 0.80, minNumberOfSubjectsPerStage = round(factor*c(NA, 20, 20)), 
		maxNumberOfSubjectsPerStage = round(factor*c(NA, 80, 80)), alternative = seq(0, 1, 0.2), #thetaH1 = 0.5, 
		maxNumberOfIterations = 100, allocationRatioPlanned = allocationRatioPlanned, seed = 5678)

	comp1 <- y$overallReject - x$rejectAtLeastOne

	## Comparison of the results of numeric object 'comp1' with expected results
	expect_equal(comp1, c(-0.01, 0.02, 0.05, -0.03, -0.04, -0.04), tolerance = 1e-07)

	comp2 <- y$rejectPerStage - x$rejectedArmsPerStage[, , 1]

	## Comparison of the results of matrixarray object 'comp2' with expected results
	expect_equal(comp2[1, ], c(-0.01, -0.02, 0.05, -0.01, -0.05, -0.06), tolerance = 1e-07)
	expect_equal(comp2[2, ], c(0.01, 0.03, -0.07, -0.08, 0.04, 0.05), tolerance = 1e-07)
	expect_equal(comp2[3, ], c(-0.01, 0.01, 0.07, 0.06, -0.03, -0.03), tolerance = 1e-07)

	comp3 <- y$futilityPerStage - x$futilityPerStage

	## Comparison of the results of matrixarray object 'comp3' with expected results
	expect_equal(comp3[1, ], c(0.08, 0.03, 0.01, 0.04, 0.02, 0.04), tolerance = 1e-07)
	expect_equal(comp3[2, ], c(-0.1, 0.03, 0, 0, 0.03, 0.01), tolerance = 1e-07)

	comp4 <- round(y$sampleSizes - (x$sampleSizes[, , 1] + x$sampleSizes[, , 2]), 1)

	## Comparison of the results of matrixarray object 'comp4' with expected results
	expect_equal(comp4[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(comp4[2, ], c(-3.6, -5.8, 8.4, 5.5, -3.5, 4.7), tolerance = 1e-07)
	expect_equal(comp4[3, ], c(0, -1.8, -3.2, 7.1, -0.8, -19), tolerance = 1e-07)

	comp5 <- round(y$expectedNumberOfSubjects - x$expectedNumberOfSubjects, 1)

	## Comparison of the results of numeric object 'comp5' with expected results
	expect_equal(comp5, c(-5.8, -11.9, -2.3, 7.1, -3.9, -0.3), tolerance = 1e-07)

	comp6 <- x$earlyStop - y$earlyStop

	## Comparison of the results of numeric object 'comp6' with expected results
	expect_equal(comp6, c(0.02, -0.07, 0.01, 0.05, -0.04, -0.04), tolerance = 1e-07)

})

test_that("'getSimulationMultiArmMeans': configuration 1", {

	.skipTestIfDisabled()

	results1 <- getSimulationMultiArmMeans(getDesignInverseNormal(), plannedSubjects = c(20, 40, 60), 
		muMaxVector = 0.1, conditionalPower = 0.9,
		minNumberOfSubjectsPerStage = c(NA,10,10), maxNumberOfSubjectsPerStage = c(NA, 100, 100),
		maxNumberOfIterations = 20, seed = 99123)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'results1' with expected results
	expect_equal(results1$iterations[1, ], 20)
	expect_equal(results1$iterations[2, ], 20)
	expect_equal(results1$iterations[3, ], 20)
	expect_equal(unlist(as.list(results1$selectedArms)), c(1, 0.25, 0.25, 1, 0.45, 0.45, 1, 0.3, 0.3, 1, 1, 1), tolerance = 1e-07)
	expect_equal(results1$numberOfActiveArms[1, ], 3)
	expect_equal(results1$numberOfActiveArms[2, ], 1)
	expect_equal(results1$numberOfActiveArms[3, ], 1)
	expect_equal(unlist(as.list(results1$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0.05, 0, 0, 0.1), tolerance = 1e-07)
	expect_equal(results1$rejectAtLeastOne, 0.15, tolerance = 1e-07)
	expect_equal(results1$futilityPerStage[1, ], 0)
	expect_equal(results1$futilityPerStage[2, ], 0)
	expect_equal(results1$futilityStop, 0)
	expect_equal(results1$earlyStop, 0)
	expect_equal(results1$successPerStage[1, ], 0)
	expect_equal(results1$successPerStage[2, ], 0)
	expect_equal(results1$successPerStage[3, ], 0.15, tolerance = 1e-07)
	expect_equal(results1$expectedNumberOfSubjects, 453.10342, tolerance = 1e-07)
	expect_equal(unlist(as.list(results1$sampleSizes)), c(20, 25, 25, 20, 38.391383, 45, 20, 24.489976, 28.670353, 20, 87.881359, 98.670353), tolerance = 1e-07)
	expect_equal(results1$conditionalPowerAchieved[1, ], NA_real_)
	expect_equal(results1$conditionalPowerAchieved[2, ], 0.063515359, tolerance = 1e-07)
	expect_equal(results1$conditionalPowerAchieved[3, ], 0.14768102, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results1), NA)))
	    expect_output(print(results1)$show())
	    invisible(capture.output(expect_error(summary(results1), NA)))
	    expect_output(summary(results1)$show())
	}

})

test_that("'getSimulationMultiArmMeans': configuration 2", {

	.skipTestIfDisabled()

	results2 <- getSimulationMultiArmMeans(getDesignInverseNormal(), plannedSubjects = c(20, 40, 60), 
		conditionalPower = 0.9,
		minNumberOfSubjectsPerStage = c(NA,10,10), maxNumberOfSubjectsPerStage = c(NA, 100, 100),
		maxNumberOfIterations = 20, seed = 99123)

	## Comparison of the results of SimulationResultsMultiArmMeans object 'results2' with expected results
	expect_equal(results2$iterations[1, ], c(20, 20, 20, 20, 20, 20))
	expect_equal(results2$iterations[2, ], c(20, 20, 20, 20, 20, 20))
	expect_equal(results2$iterations[3, ], c(20, 19, 12, 9, 0, 1))
	expect_equal(unlist(as.list(results2$selectedArms)), c(1, 0.3, 0.3, 1, 0.15, 0.15, 1, 0.15, 0.15, 1, 0.2, 0.2, 1, 0, 0, 1, 0, 0, 1, 0.45, 0.45, 1, 0.3, 0.25, 1, 0.25, 0.15, 1, 0.2, 0.15, 1, 0.05, 0, 1, 0.15, 0.05, 1, 0.25, 0.25, 1, 0.55, 0.55, 1, 0.6, 0.3, 1, 0.6, 0.1, 1, 0.95, 0, 1, 0.85, 0, 1, 1, 1, 1, 1, 0.95, 1, 1, 0.6, 1, 1, 0.45, 1, 1, 0, 1, 1, 0.05), tolerance = 1e-07)
	expect_equal(results2$numberOfActiveArms[1, ], c(3, 3, 3, 3, 3, 3))
	expect_equal(results2$numberOfActiveArms[2, ], c(1, 1, 1, 1, 1, 1))
	expect_equal(results2$numberOfActiveArms[3, ], c(1, 1, 1, 1, NaN, 1))
	expect_equal(unlist(as.list(results2$rejectedArmsPerStage)), c(0, 0, 0, 0, 0, 0, 0, 0, 0.1, 0, 0, 0.15, 0, 0, 0, 0, 0, 0, 0, 0, 0.05, 0, 0.05, 0.1, 0, 0.1, 0.1, 0, 0.05, 0.15, 0.05, 0.05, 0, 0.1, 0.05, 0.05, 0, 0, 0, 0, 0, 0.25, 0, 0.3, 0.3, 0.1, 0.4, 0.05, 0.1, 0.85, 0, 0.3, 0.55, 0), tolerance = 1e-07)
	expect_equal(results2$rejectAtLeastOne, c(0.05, 0.4, 0.9, 0.9, 1, 1), tolerance = 1e-07)
	expect_equal(results2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(results2$futilityPerStage[2, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(results2$futilityStop, c(0, 0, 0, 0, 0, 0))
	expect_equal(results2$earlyStop, c(0, 0.05, 0.4, 0.55, 1, 0.95), tolerance = 1e-07)
	expect_equal(results2$successPerStage[1, ], c(0, 0, 0, 0, 0, 0))
	expect_equal(results2$successPerStage[2, ], c(0, 0.05, 0.4, 0.55, 1, 0.95), tolerance = 1e-07)
	expect_equal(results2$successPerStage[3, ], c(0.05, 0.35, 0.5, 0.35, 0, 0.05), tolerance = 1e-07)
	expect_equal(results2$expectedNumberOfSubjects, c(465.49165, 456.30536, 338.90883, 248.56861, NaN, 128.4143), tolerance = 1e-07)
	expect_equal(unlist(as.list(results2$sampleSizes)), c(20, 30, 30, 20, 15, 15.789474, 20, 11.477569, 25, 20, 11.044373, 40.614546, 20, 0, 0, 20, 0, 0, 20, 39.864908, 45, 20, 23.853346, 26.315789, 20, 15.403506, 9.8429942, 20, 15.793171, 33.333333, 20, 3.6047183, 0, 20, 2.6461036, 10, 20, 22.880918, 25, 20, 54.506958, 57.676187, 20, 52.326716, 48.90138, 20, 23.170217, 2.2222222, 20, 28.313083, 0, 20, 21.061048, 0, 20, 92.745826, 100, 20, 93.360304, 99.78145, 20, 79.207791, 83.744374, 20, 50.007761, 76.170101, 20, 31.917801, 0, 20, 23.707152, 10), tolerance = 1e-07)
	expect_equal(results2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(results2$conditionalPowerAchieved[2, ], c(0.043769294, 0.040156593, 0.11302275, 0.3466809, 0.42534342, 0.59551224), tolerance = 1e-07)
	expect_equal(results2$conditionalPowerAchieved[3, ], c(0.032388387, 0.19348326, 0.62832278, 0.53018841, NaN, 0.92904739), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(results2), NA)))
	    expect_output(print(results2)$show())
	    invisible(capture.output(expect_error(summary(results2), NA)))
	    expect_output(summary(results2)$show())
	}

})

