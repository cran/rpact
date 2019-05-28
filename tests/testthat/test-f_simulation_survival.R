######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 27 May 2019, 14:30:39                                                        #
# Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD                             #
# Licensed under "GNU Lesser General Public License" version 3                       #
# License text can be found here: https://www.r-project.org/Licenses/LGPL-3          #
#                                                                                    #
# RPACT company website: https://www.rpact.com                                       #
# RPACT package website: https://www.rpact.org                                       #
#                                                                                    #
# Contact us for information about our services: info@rpact.com                      #
#                                                                                    #
######################################################################################

context("Testing simulation survival function")


test_that("'getSimulationSurvival': configuration 1", {
	.skipTestifDisabled()

	simulationResults <- getSimulationSurvival(maxNumberOfSubjects = 200, plannedEvents = 50, 
		accrualTime = c(0, 3, 6, 12), accrualIntensity = c(0.1, 0.2, 0.2) , 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResults$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResults$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResults$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResults$analysisTime[1, ], c(22.223223, 18.818775, 16.321595, 14.790808), tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, c(22.223223, 18.818775, 16.321595, 14.790808), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, c(50, 50, 50, 50))
	expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(simulationResults$overallReject, c(0.01, 0.41, 0.81, 1), tolerance = 1e-07)
	expect_equal(simulationResults$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResults$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': configuration 2", {

	.skipTestifDisabled()

	design <- getDesignFisher(kMax = 3, alpha0Vec = c(0.5, 0.5))

	simulationResults <- getSimulationSurvival(design = design, pi2 = 0.6, pi1 = seq(0.3, 0.45, 0.05),  
			directionUpper = FALSE, maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20, 
			allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
			accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
			dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
			maxNumberOfAdditionalEventsPerStage = c(100, 100, 200), maxNumberOfIterations = 100,
			seed = 1234567890)


	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(23.809524, 47.619048, 47.619048), tolerance = 1e-07)
	expect_equal(simulationResults$median1, c(23.320299, 19.308487, 16.282985, 13.9131), tolerance = 1e-07)
	expect_equal(simulationResults$median2, 9.0776496, tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.029722912, 0.035898576, 0.042568802, 0.04981975), tolerance = 1e-07)
	expect_equal(simulationResults$lambda2, 0.076357561, tolerance = 1e-07)
	expect_equal(simulationResults$hazardRatio, c(0.38925958, 0.47013781, 0.55749295, 0.6524534), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResults$iterations[2, ], c(60, 73, 78, 65))
	expect_equal(simulationResults$iterations[3, ], c(5, 9, 28, 46))
	expect_equal(simulationResults$analysisTime[1, ], c(5.4183926, 5.2945044, 5.1495619, 5.0392001), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], c(10.130549, 10.39649, 10.458778, 9.7641943), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], c(13.506679, 14.455396, 18.382917, 18.866629), tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, c(8.500396, 9.4448778, 11.628285, 12.227203), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], c(20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], c(84.483333, 93.054795, 98.884615, 92.015385), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], c(125.8, 159.44444, 229.53571, 250.93478), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, c(60.755833, 79.305068, 118.11231, 139.91292), tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$numberOfSubjects[1, ], c(186.51, 180.63, 173.73, 168.48), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], c(406.05, 420.67123, 424.60256, 393.44615), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], c(428.4, 466.33333, 480.96429, 488.78261), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, c(319.3515, 359.96969, 385.19188, 358.56277), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], c(0.4, 0.21, 0.2, 0.13), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], c(0.55, 0.63, 0.5, 0.15), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], c(0.05, 0.09, 0.26, 0.41), tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, c(1, 0.93, 0.96, 0.69), tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0.06, 0.02, 0.22), tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0.01, 0, 0.04), tolerance = 1e-07)
	expect_equal(simulationResults$futilityStop, c(0, 0.07, 0.02, 0.26), tolerance = 1e-07)
	expect_equal(simulationResults$earlyStop, c(0.95, 0.91, 0.72, 0.54), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.61612368, 0.57564124, 0.49458667, 0.52832804), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.78816558, 0.77803263, 0.64572713, 0.66129837), tolerance = 1e-07)
})

test_that("'getSimulationSurvival': configuration 3", {

	.skipTestifDisabled()

	design <- getDesignFisher(kMax = 3, alpha0Vec = c(0.5, 0.5))

	simulationResults <- getSimulationSurvival(design = design, pi2 = 0.2, pi1 = seq(0.3, 0.45, 0.05),  
			directionUpper = TRUE, maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20, 
			allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
			accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
			dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
			maxNumberOfAdditionalEventsPerStage = c(100, 100, 200), maxNumberOfIterations = 100,
			seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(23.809524, 47.619048, 47.619048), tolerance = 1e-07)
	expect_equal(simulationResults$median1, c(23.320299, 19.308487, 16.282985, 13.9131), tolerance = 1e-07)
	expect_equal(simulationResults$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.029722912, 0.035898576, 0.042568802, 0.04981975), tolerance = 1e-07)
	expect_equal(simulationResults$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResults$hazardRatio, c(1.5984103, 1.9305192, 2.2892242, 2.6791588), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResults$iterations[2, ], c(69, 72, 59, 50))
	expect_equal(simulationResults$iterations[3, ], c(37, 12, 7, 2))
	expect_equal(simulationResults$analysisTime[1, ], c(7.2763799, 7.0838561, 6.7193502, 6.3616317), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], c(16.764021, 14.756285, 13.821816, 12.988284), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], c(38.977945, 24.200748, 26.934721, 11.875967), tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, c(22.098154, 13.978342, 11.899449, 9.7796143), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], c(20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], c(106.50725, 94.541667, 94.677966, 94.48), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], c(259.13514, 175, 204, 84.5), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, c(136.16232, 83.325, 71.712542, 57.0404), tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$numberOfSubjects[1, ], c(275.04, 265.86, 248.46, 231.45), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], c(496.07246, 481.84722, 476, 463.84), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], c(500, 500, 500, 494), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, c(429.00559, 423.54913, 384.3886, 348.2482), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], c(0.18, 0.22, 0.39, 0.49), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], c(0.31, 0.59, 0.52, 0.48), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], c(0.35, 0.11, 0.07, 0.02), tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, c(0.84, 0.92, 0.98, 0.99), tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], c(0.13, 0.06, 0.02, 0.01), tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[2, ], c(0.01, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(simulationResults$futilityStop, c(0.14, 0.07, 0.02, 0.01), tolerance = 1e-07)
	expect_equal(simulationResults$earlyStop, c(0.63, 0.88, 0.93, 0.98), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.46273079, 0.58305775, 0.61313502, 0.59484117), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.66165116, 0.75066235, 0.71981679, 0.8), tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 4", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	piecewiseSurvivalTime <- list(
		"<6"      = 0.025, 
		"6 - <9"   = 0.04, 
		"9 - <15"  = 0.015, 
		"15 - <21" = 0.01, 
		">=21"      = 0.007)

	simulationResults <- getSimulationSurvival(design = design, 
		directionUpper = TRUE, maxNumberOfSubjects = 500, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 1.7, 
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
		dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
		maxNumberOfAdditionalEventsPerStage = c(100, 100, 200), maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(23.809524, 47.619048, 47.619048), tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.0425, 0.068, 0.0255, 0.017, 0.0119), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], 100)
	expect_equal(simulationResults$iterations[2, ], 95)
	expect_equal(simulationResults$iterations[3, ], 30)
	expect_equal(simulationResults$analysisTime[1, ], 6.3619038, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], 12.345684, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], 36.687962, tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, 19.26207, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], 20, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], 91.694737, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], 207.83333, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, 122.95158, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResults$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResults$eventsNotAchieved[3, ], 0)
	expect_equal(simulationResults$numberOfSubjects[1, ], 231.41, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], 448.23158, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], 491.66667, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, 450.42103, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], 0.05, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], 0.65, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], 0.29, tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, 0.99, tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], 0)
	expect_equal(simulationResults$futilityPerStage[2, ], 0)
	expect_equal(simulationResults$futilityStop, 0)
	expect_equal(simulationResults$earlyStop, 0.7, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.49425129, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.73157546, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 5", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	simulationResults <- getSimulationSurvival(design = design, pi2 = 0.6, pi1 = seq(0.3, 0.45, 0.05), 
		directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
		dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
		maxNumberOfAdditionalEventsPerStage = c(40,40,40), maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$median1, c(23.320299, 19.308487, 16.282985, 13.9131), tolerance = 1e-07)
	expect_equal(simulationResults$median2, 9.0776496, tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.029722912, 0.035898576, 0.042568802, 0.04981975), tolerance = 1e-07)
	expect_equal(simulationResults$lambda2, 0.076357561, tolerance = 1e-07)
	expect_equal(simulationResults$hazardRatio, c(0.38925958, 0.47013781, 0.55749295, 0.6524534), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResults$iterations[2, ], c(87, 89, 92, 100))
	expect_equal(simulationResults$iterations[3, ], c(18, 34, 58, 77))
	expect_equal(simulationResults$analysisTime[1, ], c(8.1674426, 7.9228743, 7.6045868, 7.4881493), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], c(12.354338, 12.56529, 12.380125, 12.254955), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], c(16.473595, 17.9949, 17.847597, 17.390492), tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, c(12.562909, 13.818364, 15.044701, 16.144285), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], c(20, 20, 20, 20), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], c(48.54023, 51.561798, 55.130435, 55.79), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], c(75.277778, 87.176471, 94.103448, 94.545455), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, c(49.642759, 60.198989, 74.924348, 85.6317), tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$eventsNotAchieved[2, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$eventsNotAchieved[3, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$numberOfSubjects[1, ], c(126.03, 121.42, 115.37, 113.16), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], c(187.50575, 190.98876, 193.16304, 192.33), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], c(199.11111, 200, 199.39655, 199.28571), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, c(181.60287, 186.40002, 190.55503, 197.6859), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], c(0.13, 0.11, 0.08, 0), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], c(0.69, 0.55, 0.34, 0.23), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], c(0.17, 0.31, 0.26, 0.25), tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, c(0.99, 0.97, 0.68, 0.48), tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0, 0, 0))
	expect_equal(simulationResults$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResults$earlyStop, c(0.82, 0.66, 0.42, 0.23), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.56161185, 0.47418383, 0.31608317, 0.29578133), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.71394365, 0.57778506, 0.37448609, 0.32265113), tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 6", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	piecewiseSurvivalTime <- list(
		"0 - <6"      = 0.025, 
		"6 - <9"   = 0.04, 
		"9 - <15"  = 0.015, 
		"15 - <21" = 0.01, 
		">=21"      = 0.007)

	simulationResults <- getSimulationSurvival(design = design, 
		directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = c(0.8, 0.9),
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
		dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
		maxNumberOfAdditionalEventsPerStage = c(100, 400, 200), maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.02, 0.032, 0.012, 0.008, 0.0056), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], 100)
	expect_equal(simulationResults$iterations[2, ], 29)
	expect_equal(simulationResults$iterations[3, ], 8)
	expect_equal(simulationResults$analysisTime[1, ], 11.099103, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], 137.1048, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], 83.267347, tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, 10.437225, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], 20, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], 102.10345, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], 96.375, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, 96.661422, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], 0, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[2, ], 0.71, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[3, ], 0.16, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[1, ], 179.93, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, 200)
	expect_equal(simulationResults$rejectPerStage[1, ], 0, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], 0.05, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], 0.03, tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, 0.08, tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], 0)
	expect_equal(simulationResults$futilityPerStage[2, ], 0)
	expect_equal(simulationResults$futilityStop, 0)
	expect_equal(simulationResults$earlyStop, 0.05, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.80243482, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.8, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 7", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	piecewiseSurvivalTime <- list("<6" = 0.025)

	simulationResults <- getSimulationSurvival(design = design, 
		directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		piecewiseSurvivalTime = piecewiseSurvivalTime, hazardRatio = 0.8,
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
		dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
		maxNumberOfAdditionalEventsPerStage = c(100, 400, 200), maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$pi1, 0.21337214, tolerance = 1e-07)
	expect_equal(simulationResults$pi2, 0.25918178, tolerance = 1e-07)
	expect_equal(simulationResults$median1, 34.657359, tolerance = 1e-07)
	expect_equal(simulationResults$median2, 27.725887, tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, 0.02, tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], 100)
	expect_equal(simulationResults$iterations[2, ], 26)
	expect_equal(simulationResults$iterations[3, ], 12)
	expect_equal(simulationResults$analysisTime[1, ], 11.419107, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], 43.00709, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], 62.010907, tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, 8.0301114, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], 20, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], 87.076923, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], 122.5, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, 121.79154, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], 0, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[2, ], 0.74, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[3, ], 0.12, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[1, ], 183.49, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, 200)
	expect_equal(simulationResults$rejectPerStage[1, ], 0, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], 0.02, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], 0.04, tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, 0.06, tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], 0)
	expect_equal(simulationResults$futilityPerStage[2, ], 0)
	expect_equal(simulationResults$futilityStop, 0)
	expect_equal(simulationResults$earlyStop, 0.02, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.80311744, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.8, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 8", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	piecewiseSurvivalTime <- list("<6" = 0.025)

	simulationResults <- getSimulationSurvival(design = design, 
		directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		piecewiseSurvivalTime = c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = 0.8,
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0.04, dropoutRate2 = 0.08,
		dropoutTime = 12, maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$pi1, c(0.091535984, 0.25023841), tolerance = 1e-07)
	expect_equal(simulationResults$pi2, c(0.11307956, 0.30232367), tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.008, 0.024), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], 100)
	expect_equal(simulationResults$iterations[2, ], 99)
	expect_equal(simulationResults$iterations[3, ], 95)
	expect_equal(simulationResults$analysisTime[1, ], 14.155697, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], 19.508242, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], 25.008056, tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, 24.627971, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], 20)
	expect_equal(simulationResults$eventsPerStage[2, ], 40)
	expect_equal(simulationResults$eventsPerStage[3, ], 60)
	expect_equal(simulationResults$expectedNumberOfEvents, 58.8, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResults$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResults$eventsNotAchieved[3, ], 0)
	expect_equal(simulationResults$numberOfSubjects[1, ], 199.73, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, 199.9973, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], 0.01, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], 0.04, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], 0.06, tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, 0.11, tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], 0)
	expect_equal(simulationResults$futilityPerStage[2, ], 0)
	expect_equal(simulationResults$futilityStop, 0)
	expect_equal(simulationResults$earlyStop, 0.05, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.13387917, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.12806393, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 9; ", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	piecewiseSurvivalTime <- list("<6" = 0.025)

	simulationResults <- getSimulationSurvival(design = design, 
		directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		piecewiseSurvivalTime = c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = c(0.75),
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
		dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
		maxNumberOfAdditionalEventsPerStage = c(100, 400, 200), maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$pi1, c(0.086068815, 0.23662051), tolerance = 1e-07)
	expect_equal(simulationResults$pi2, c(0.11307956, 0.30232367), tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.0075, 0.0225), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], 100)
	expect_equal(simulationResults$iterations[2, ], 27)
	expect_equal(simulationResults$iterations[3, ], 2)
	expect_equal(simulationResults$analysisTime[1, ], 14.263292, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], 43.719076, tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], 38.174522, tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, 6.7834542, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], 20, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], 90.037037, tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], 84.5, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, 84.519444, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], 0, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[2, ], 0.72, tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[3, ], 0.13, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[1, ], 199.79, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], 200, tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, 199.9979, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], 0.01, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], 0.12, tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], 0.02, tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, 0.15, tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], 0)
	expect_equal(simulationResults$futilityPerStage[2, ], 0)
	expect_equal(simulationResults$futilityStop, 0)
	expect_equal(simulationResults$earlyStop, 0.13, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], 0.80261071, tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], 0.8, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': configuration 10; ", {

	.skipTestifDisabled()

	design <- getDesignGroupSequential(kMax = 3, typeOfDesign = "WT", deltaWT = 0.25)

	piecewiseSurvivalTime <- list("<6" = 0.025)

	simulationResults <- getSimulationSurvival(design = design, 
		directionUpper = FALSE, maxNumberOfSubjects = 200, plannedEvents = (1:design$kMax) * 20, 
		allocation1 = 1, allocation2 = 1, accrualTime = c(0, 3, 6, 12), 
		lambda2 = 0.03, hazardRatio = c(0.75, 0.8, 0.9),
		accrualIntensity = c(0.1, 0.2, 0.2) , dropoutRate1 = 0, dropoutRate2 = 0,
		dropoutTime = 12, conditionalPower = 0.8, minNumberOfAdditionalEventsPerStage = c(20, 10, 10), 
		maxNumberOfAdditionalEventsPerStage = c(100, 400, 200), maxNumberOfIterations = 100,
		seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResults' with expected results
	##
	expect_equal(simulationResults$accrualIntensity, c(9.5238095, 19.047619, 19.047619), tolerance = 1e-07)
	expect_equal(simulationResults$pi1, c(0.23662051, 0.25023841, 0.27674976), tolerance = 1e-07)
	expect_equal(simulationResults$pi2, 0.30232367, tolerance = 1e-07)
	expect_equal(simulationResults$median1, c(30.806541, 28.881133, 25.672118), tolerance = 1e-07)
	expect_equal(simulationResults$median2, 23.104906, tolerance = 1e-07)
	expect_equal(simulationResults$lambda1, c(0.0225, 0.024, 0.027), tolerance = 1e-07)
	expect_equal(simulationResults$iterations[1, ], c(100, 100, 100))
	expect_equal(simulationResults$iterations[2, ], c(31, 24, 16))
	expect_equal(simulationResults$iterations[3, ], c(5, 5, 7))
	expect_equal(simulationResults$analysisTime[1, ], c(10.701574, 10.513732, 10.265089), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[2, ], c(41.567704, 37.665074, 37.552932), tolerance = 1e-07)
	expect_equal(simulationResults$analysisTime[3, ], c(33.732435, 53.513221, 36.546609), tolerance = 1e-07)
	expect_equal(simulationResults$studyDuration, c(6.1071436, 6.3055402, 2.5582627), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[1, ], c(20, 20, 20), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[2, ], c(95.290323, 89.541667, 93.9375), tolerance = 1e-07)
	expect_equal(simulationResults$eventsPerStage[3, ], c(93.8, 124, 109.71429), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfEvents, c(93.949032, 120.19708, 109.71429), tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[1, ], c(0, 0, 0), tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[2, ], c(0.69, 0.74, 0.84), tolerance = 1e-07)
	expect_equal(simulationResults$eventsNotAchieved[3, ], c(0.16, 0.14, 0.09), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[1, ], c(173.61, 170.39, 165.9), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[2, ], c(199.6129, 200, 200), tolerance = 1e-07)
	expect_equal(simulationResults$numberOfSubjects[3, ], c(200, 200, 200), tolerance = 1e-07)
	expect_equal(simulationResults$expectedNumberOfSubjects, c(199.96129, 199.4078, 200), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[1, ], c(0, 0.02, 0), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[2, ], c(0.1, 0.05, 0), tolerance = 1e-07)
	expect_equal(simulationResults$rejectPerStage[3, ], c(0.05, 0.01, 0.03), tolerance = 1e-07)
	expect_equal(simulationResults$overallReject, c(0.15, 0.08, 0.03), tolerance = 1e-07)
	expect_equal(simulationResults$futilityPerStage[1, ], c(0, 0, 0))
	expect_equal(simulationResults$futilityPerStage[2, ], c(0, 0, 0))
	expect_equal(simulationResults$futilityStop, c(0, 0, 0))
	expect_equal(simulationResults$earlyStop, c(0.1, 0.07, 0), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[2, ], c(0.80326129, 0.80161244, 0.8), tolerance = 1e-07)
	expect_equal(simulationResults$conditionalPowerAchieved[3, ], c(0.8, 0.8, 0.8), tolerance = 1e-07)

})

test_that("'getSimulationSurvival': test accrual time and intensity definition", {

	.skipTestifDisabled()

	maxNumberOfSubjects <- getSimulationSurvival(plannedEvents = 100, 
		accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33), 
		maxNumberOfIterations = 100)$maxNumberOfSubjects
	expect_equal(maxNumberOfSubjects, 330)

	accrualIntensity <- getSimulationSurvival(plannedEvents = 100, 
		accrualTime = c(0, 6, 12), accrualIntensity = c(0.2, 0.3), 
		maxNumberOfSubjects = 330, maxNumberOfIterations = 100)$accrualIntensity 
	expect_equal(accrualIntensity, c(22, 33))

})

test_that("'getSimulationSurvival': test exptected warnings and errors", {

	dIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))

	expect_warning(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44), maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100),
		"'minNumberOfAdditionalEventsPerStage' (58, 44, 44) will be ignored because no 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_warning(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44), maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100),
		"'maxNumberOfAdditionalEventsPerStage' (232, 176, 176) will be ignored because no 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_warning(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44), 
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100),
		"'minNumberOfAdditionalEventsPerStage' (58, 44, 44) will be ignored because no 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_warning(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, plannedEvents = c(58, 102, 146), 
			maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100),
		"'maxNumberOfAdditionalEventsPerStage' (232, 176, 176) will be ignored because no 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Missing argument: 'minNumberOfAdditionalEventsPerStage' must be defined because 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Missing argument: 'maxNumberOfAdditionalEventsPerStage' must be defined because 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
			maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Missing argument: 'minNumberOfAdditionalEventsPerStage' must be defined because 'conditionalPower' is defined", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = -0.1, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44), maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Argument out of bounds: 'conditionalPower' (-0.1) is out of bounds (0; 1)", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = 1.1, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44), maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Argument out of bounds: 'conditionalPower' (1.1) is out of bounds (0; 1)", 
		fixed = TRUE)

	#expect_error(getSimulationSurvival(plannedEvents = 100,
	#		conditionalPower = 0.8,
	#		minNumberOfAdditionalEventsPerStage = c(50, 100),
	#		maxNumberOfAdditionalEventsPerStage = 150,
	#		accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33), 
	#		maxNumberOfIterations = 100), 
	#	"Illegal argument: 'minNumberOfAdditionalEventsPerStage' (50, 100) must have length 1", 
	#	fixed = TRUE)

	#expect_error(getSimulationSurvival(plannedEvents = 100,
	#		conditionalPower = 0.8,
	#		minNumberOfAdditionalEventsPerStage = 50,
	#		maxNumberOfAdditionalEventsPerStage = c(150, 200),
	#		accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33), 
	#		maxNumberOfIterations = 100), 
	#	"Illegal argument: 'maxNumberOfAdditionalEventsPerStage' (150, 200) must have length 1", 
	#	fixed = TRUE)

	expect_error(getSimulationSurvival(plannedEvents = -100, 
			accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33), 
			maxNumberOfIterations = 100), 
		"Argument out of bounds: 'plannedEvents' (-100) must be >= 1", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, plannedEvents = c(100,100, 150), 
				accrualTime = c(0, 6, 12), accrualIntensity = c(22, 33), 
				maxNumberOfIterations = 100), 
			"Illegal argument: 'plannedEvents' (100, 100, 150) must be strictly increasing: x_1 < .. < x_3", 
			fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, -44), 
			maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Argument out of bounds: each value of 'minNumberOfAdditionalEventsPerStage' (58, 44, -44) must be >= 1", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
			pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
			minNumberOfAdditionalEventsPerStage = c(58, 44, 44), 
			maxNumberOfAdditionalEventsPerStage = 4 * c(-58, 44, 44),
			maxNumberOfSubjects = 800, maxNumberOfIterations = 100), 
		"Illegal argument: 'maxNumberOfAdditionalEventsPerStage' (-232, 176, 176) must be not smaller than minNumberOfAdditionalEventsPerStage' (58, 44, 44)", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(plannedEvents = 100, maxNumberOfIterations = 100), 
		"Illegal argument: 'maxNumberOfSubjects' must be defined", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(plannedEvents = 100, accrualTime = c(0, 12), 
			accrualIntensity = 20, thetaH1 = 0, maxNumberOfIterations = 100), 
		"Argument out of bounds: 'thetaH1' (0) must be > 0", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(plannedEvents = 100, accrualTime = c(0, 12), 
			accrualIntensity = 20, conditionalPower = 0, maxNumberOfIterations = 100), 
		"Argument out of bounds: 'conditionalPower' (0) is out of bounds (0; 1)", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(plannedEvents = 100, accrualTime = c(0, 12), 
			accrualIntensity = 20, conditionalPower = 1, maxNumberOfIterations = 100), 
		"Argument out of bounds: 'conditionalPower' (1) is out of bounds (0; 1)", 
		fixed = TRUE)

	expect_error(getSimulationSurvival(plannedEvents = 100, accrualTime = c(0, 12), 
			accrualIntensity = 20, conditionalPower = c(0.5, 0.8), maxNumberOfIterations = 100), 
		"Illegal argument: 'conditionalPower' c(0.5, 0.8) must be a single numerical value", 
		fixed = TRUE)

})

context("Testing the simulation of survival data for different parameter variants")


test_that("'getSimulationSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default ", {
	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(plannedEvents = 40, maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(17.941133, 15.499503, 13.535749, 12.34), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(17.941133, 15.499503, 13.535749, 12.34), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 199.71, 196.74), tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, c(0.01, 0.3, 0.68, 0.95), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Increase number of simulation iterations ", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(plannedEvents = 40, maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(17.941133, 15.499503, 13.535749, 12.34), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(17.941133, 15.499503, 13.535749, 12.34), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 199.71, 196.74), tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, c(0.01, 0.3, 0.68, 0.95), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited ", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(plannedEvents = 40, accrualTime = 0, 
		accrualIntensity = 30, maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(15.255121, 12.685136, 10.656532, 9.4294312), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(15.255121, 12.685136, 10.656532, 9.4294312), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(simulationResult$overallReject, c(0.02, 0.28, 0.77, 0.96), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit ", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(plannedEvents = 40, accrualTime = c(0, 6), 
		accrualIntensity = c(20, 30), maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(simulationResult$overallReject, c(0.01, 0.28, 0.72, 0.96), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(plannedEvents = 40, accrualTime = c(0, 6, 10), 
		accrualIntensity = c(20, 30), maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$maxNumberOfSubjects, 240)
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(240, 240, 239.63, 237.56), tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, c(0.04, 0.33, 0.75, 0.95), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Specify accrual time as a list", {

	.skipTestifDisabled()

	at <- list("0 - <6" = 20, "6 - Inf" = 30)
	simulationResult <- getSimulationSurvival(plannedEvents = 40, accrualTime = at, 
		maxNumberOfSubjects = 200, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(16.683961, 14.141068, 12.109744, 10.96314), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(200, 200, 200, 200))
	expect_equal(simulationResult$overallReject, c(0.01, 0.28, 0.72, 0.96), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {

	.skipTestifDisabled()

	at <- list("0 - <6" = 20, "6 - <=10" = 30)
	simulationResult <- getSimulationSurvival(plannedEvents = 40, accrualTime = at, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$maxNumberOfSubjects, 240)
	expect_equal(simulationResult$median1, c(37.275405, 23.320299, 16.282985, 12), tolerance = 1e-07)
	expect_equal(simulationResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.018595296, 0.029722912, 0.042568802, 0.057762265), tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, c(1, 1.5984103, 2.2892242, 3.1062837), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(simulationResult$analysisTime[1, ], c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, c(15.210978, 13.172199, 11.59631, 10.698373), tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, c(40, 40, 40, 40))
	expect_equal(simulationResult$eventsNotAchieved[1, ], c(0, 0, 0, 0))
	expect_equal(simulationResult$expectedNumberOfSubjects, c(240, 240, 239.63, 237.56), tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, c(0.04, 0.33, 0.75, 0.95), tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, c(0, 0, 0, 0))
	expect_equal(simulationResult$earlyStop, c(0, 0, 0, 0))

})

test_that("'getSimulationSurvival': Specify effect size for a two-stage group design with O'Brien & Fleming boundaries Effect size is based on event rates at specified event time, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
		pi1 = 0.2, pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), 
		maxNumberOfSubjects = 200, directionUpper = FALSE, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$median1, 74.550809, tolerance = 1e-07)
	expect_equal(simulationResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, 0.009297648, tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, 0.62562161, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 97)
	expect_equal(simulationResult$analysisTime[1, ], 14.769473, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 24.499634, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 24.198958, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 39.4, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 199.47, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.9841, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.03, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.24, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.27, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.03, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.29516222, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': As above, but with a three-stage O'Brien and Flemming design with specified information rates, note that planned events consists of integer values", {

	.skipTestifDisabled()

	d3 <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
	simulationResult <- getSimulationSurvival(design = d3, pi1 = 0.2, pi2 = 0.3, eventTime = 24, 
		plannedEvents = round(d3$informationRates * 40), 
		maxNumberOfSubjects = 200, directionUpper = FALSE, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$median1, 74.550809, tolerance = 1e-07)
	expect_equal(simulationResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, 0.009297648, tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(simulationResult$hazardRatio, 0.62562161, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 1000)
	expect_equal(simulationResult$iterations[2, ], 985)
	expect_equal(simulationResult$iterations[3, ], 861)
	expect_equal(simulationResult$analysisTime[1, ], 13.073331, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 18.748105, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[3, ], 24.810251, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 23.877826, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 16)
	expect_equal(simulationResult$eventsPerStage[2, ], 28)
	expect_equal(simulationResult$eventsPerStage[3, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 38.152, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[3, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 195.313, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[3, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.92969, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.015, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.124, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[3, ], 0.183, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.322, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityPerStage[2, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.139, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.19637573, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[3, ], 0.23542216, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, 
		pi2 = 0.3, eventTime = 24, plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
		directionUpper = FALSE, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$pi1, 0.16333997, tolerance = 1e-07)
	expect_equal(simulationResult$median1, 93.281194, tolerance = 1e-07)
	expect_equal(simulationResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, 0.007430728, tolerance = 1e-07)
	expect_equal(simulationResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 92)
	expect_equal(simulationResult$analysisTime[1, ], 15.596955, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 26.310745, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 25.440402, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 38.4, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 199.69, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.9752, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.08, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.44, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.52, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.08, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.43087375, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Effect size is based on hazard rate for the reference group and hazard ratio, directionUpper = FALSE needs to be specified because it should be shown that hazard ratio < 1", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, 
		lambda2 = 0.02, plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
		directionUpper = FALSE, maxNumberOfIterations = 100, seed = 1234567890) 

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(simulationResult$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(simulationResult$median1, 69.314718, tolerance = 1e-07)
	expect_equal(simulationResult$median2, 34.657359, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, 0.01, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 94)
	expect_equal(simulationResult$analysisTime[1, ], 13.132525, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 21.186744, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 20.690944, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 38.8, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 195.5, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.73, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.06, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.43, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.49, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.06, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.48014443, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Specification of piecewise exponential survival time and hazard ratios, note that in getSimulationSurvival only one hazard ratio is used in the case that the survival time is piecewise exponential", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
		piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
		hazardRatio = 1.5, plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$pi1, c(0.16472979, 0.30232367, 0.51324774), tolerance = 1e-07)
	expect_equal(simulationResult$pi2, c(0.11307956, 0.21337214, 0.38121661), tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 96)
	expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07)

	pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10"    = 0.04)
	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
		piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2), 
		plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 96)
	expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Specification of piecewise exponential survival time for both treatment arms  ", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
		piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
		lambda1 = c(0.015, 0.03, 0.06), plannedEvents = c(20, 40), 
		maxNumberOfSubjects = 200, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$pi1, c(0.16472979, 0.30232367, 0.51324774), tolerance = 1e-07)
	expect_equal(simulationResult$pi2, c(0.11307956, 0.21337214, 0.38121661), tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 96)
	expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Specification of piecewise exponential survival time as a list, note that in getSimulationSurvival only on hazard ratio (not a vector) can be used", {

	.skipTestifDisabled()

	pws <- list("0 - <5"  = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
		piecewiseSurvivalTime = pws, hazardRatio = 1.5, 
		plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 96)
	expect_equal(simulationResult$analysisTime[1, ], 12.106711, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 16.150578, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 16.020702, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 39.2, tolerance = 1e-07)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 193.51, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 199.7404, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[1, ], 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.28, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.32, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0.04, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.33404702, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Specification of piecewise exponential survival time and delayed effect (response after 5 time units)  ", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(design = getDesignGroupSequential(kMax = 2), 
		piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), 
		lambda1 = c(0.01, 0.02, 0.06), plannedEvents = c(20, 40), maxNumberOfSubjects = 200, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$iterations[2, ], 100)
	expect_equal(simulationResult$analysisTime[1, ], 12.973056, tolerance = 1e-07)
	expect_equal(simulationResult$analysisTime[2, ], 17.030809, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 17.030809, tolerance = 1e-07)
	expect_equal(simulationResult$eventsPerStage[1, ], 20)
	expect_equal(simulationResult$eventsPerStage[2, ], 40)
	expect_equal(simulationResult$expectedNumberOfEvents, 40)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$eventsNotAchieved[2, ], 0)
	expect_equal(simulationResult$numberOfSubjects[1, ], 197.81, tolerance = 1e-07)
	expect_equal(simulationResult$numberOfSubjects[2, ], 200, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfSubjects, 200)
	expect_equal(simulationResult$rejectPerStage[1, ], 0, tolerance = 1e-07)
	expect_equal(simulationResult$rejectPerStage[2, ], 0.06, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.06, tolerance = 1e-07)
	expect_equal(simulationResult$futilityPerStage[1, ], 0)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0)
	expect_equal(simulationResult$conditionalPowerAchieved[1, ], NA_real_, tolerance = 1e-07)
	expect_equal(simulationResult$conditionalPowerAchieved[2, ], 0.1789388, tolerance = 1e-07)

})

test_that("'getSimulationSurvival': Specify effect size based on median survival times (median1 = 5, median2 = 3)", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(lambda1 = log(2) / 5, 
		lambda2 = log(2) / 3, plannedEvents = 40, 
		maxNumberOfSubjects = 200, directionUpper = FALSE, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$pi1, 0.81053543, tolerance = 1e-07)
	expect_equal(simulationResult$pi2, 0.9375, tolerance = 1e-07)
	expect_equal(simulationResult$median1, 5, tolerance = 1e-07)
	expect_equal(simulationResult$median2, 3)
	expect_equal(simulationResult$hazardRatio, 0.6, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$analysisTime[1, ], 6.1552733, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 6.1552733, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, 40)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$expectedNumberOfSubjects, 102.09, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.29, tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0)

})

test_that("'getSimulationSurvival': Specify effect size based on median survival times of Weibull distribtion with kappa = 2 (median1 = 5, median2 = 3)", {

	.skipTestifDisabled()

	simulationResult <- getSimulationSurvival(lambda1 = getLambdaByMedian(median = 5, kappa = 2), 
		lambda2 = getLambdaByMedian(median = 3, kappa = 2), kappa = 2, 
		plannedEvents = 40, maxNumberOfSubjects = 200, directionUpper = FALSE, 
		maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'simulationResult' with expected results
	##
	expect_equal(simulationResult$accrualIntensity, 16.666667, tolerance = 1e-07)
	expect_equal(simulationResult$pi1, 0.98154699, tolerance = 1e-07)
	expect_equal(simulationResult$pi2, 0.99998474, tolerance = 1e-07)
	expect_equal(simulationResult$median1, 5, tolerance = 1e-07)
	expect_equal(simulationResult$median2, 3)
	expect_equal(simulationResult$hazardRatio, 0.36, tolerance = 1e-07)
	expect_equal(simulationResult$iterations[1, ], 100)
	expect_equal(simulationResult$analysisTime[1, ], 6.3123397, tolerance = 1e-07)
	expect_equal(simulationResult$studyDuration, 6.3123397, tolerance = 1e-07)
	expect_equal(simulationResult$expectedNumberOfEvents, 40)
	expect_equal(simulationResult$eventsNotAchieved[1, ], 0)
	expect_equal(simulationResult$expectedNumberOfSubjects, 104.7, tolerance = 1e-07)
	expect_equal(simulationResult$overallReject, 0.9, tolerance = 1e-07)
	expect_equal(simulationResult$futilityStop, 0)
	expect_equal(simulationResult$earlyStop, 0)

})

test_that("'getSimulationSurvival': Perform recalculation of number of events based on conditional power", {

	.skipTestifDisabled()

	# Perform recalculation of number of events based on conditional power for a 
	# three-stage design with inverse normal combination test, where the conditional power 
	# is calculated under the specified effect size thetaH1 = 1.3 and up to a four-fold 
	# increase in originally planned sample size (number of events) is allowed
	# Note that the first value in \code{minNumberOfAdditionalEventsPerStage} and 
	# \code{maxNumberOfAdditionalEventsPerStage} is arbitrary, i.e., it has no effect.

	dIN <- getDesignInverseNormal(informationRates = c(0.4, 0.7, 1))

	resultsWithSSR1 <- getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
		pi2 = 0.3, conditionalPower = 0.8, thetaH1 = 1.3, plannedEvents = c(58, 102, 146), 
		minNumberOfAdditionalEventsPerStage = c(58, 44, 44), 
		maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
		maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'resultsWithSSR1' with expected results
	##
	expect_equal(resultsWithSSR1$accrualIntensity, 66.666667, tolerance = 1e-07)
	expect_equal(resultsWithSSR1$pi1, c(0.3, 0.32452723, 0.34819506, 0.37103359, 0.39307188, 0.41433798, 0.43485894), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$median1, c(23.320299, 21.200271, 19.433582, 17.938691, 16.657356, 15.546866, 14.575187), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$median2, 23.320299, tolerance = 1e-07)
	expect_equal(resultsWithSSR1$lambda1, c(0.029722912, 0.032695203, 0.035667494, 0.038639786, 0.041612077, 0.044584368, 0.047556659), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$lambda2, 0.029722912, tolerance = 1e-07)
	expect_equal(resultsWithSSR1$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100))
	expect_equal(resultsWithSSR1$iterations[2, ], c(100, 100, 100, 98, 96, 99, 92))
	expect_equal(resultsWithSSR1$iterations[3, ], c(96, 96, 88, 67, 50, 35, 11))
	expect_equal(resultsWithSSR1$analysisTime[1, ], c(7.9761501, 7.8239889, 7.5191849, 7.4832292, 7.3291066, 7.1091953, 6.9737455), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$analysisTime[2, ], c(17.76189, 17.229038, 16.567328, 16.175906, 15.668575, 15.328143, 14.604753), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$analysisTime[3, ], c(30.192276, 28.615009, 26.463502, 25.657109, 23.821118, 23.34898, 22.534023), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$studyDuration, c(29.683899, 28.160756, 25.20615, 22.190278, 19.319577, 18.030286, 14.789904), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$eventsPerStage[1, ], c(58, 58, 58, 58, 58, 58, 58), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$eventsPerStage[2, ], c(233.65, 231.27, 229.84, 229.43878, 228.57292, 227.67677, 219.44565), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$eventsPerStage[3, ], c(409.28125, 401.01042, 385.875, 382.38806, 371.3, 374.14286, 367.72727), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$expectedNumberOfEvents, c(402.256, 394.2208, 367.1508, 328.48602, 293.11354, 277.24313, 222.84098), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$eventsNotAchieved[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR1$eventsNotAchieved[2, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR1$eventsNotAchieved[3, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR1$numberOfSubjects[1, ], c(531.25, 521.07, 500.8, 498.42, 488.13, 473.47, 464.37), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$numberOfSubjects[2, ], c(800, 800, 799.45, 798.66327, 796.55208, 797.06061, 793.47826), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$numberOfSubjects[3, ], c(800, 800, 800, 800, 800, 800, 800), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$expectedNumberOfSubjects, c(800, 800, 799.934, 793.55401, 785.93916, 794.85349, 767.86699), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$rejectPerStage[1, ], c(0, 0, 0, 0.02, 0.04, 0.01, 0.08), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$rejectPerStage[2, ], c(0.04, 0.04, 0.12, 0.31, 0.46, 0.64, 0.81), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$rejectPerStage[3, ], c(0, 0.12, 0.26, 0.42, 0.41, 0.3, 0.11), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$overallReject, c(0.04, 0.16, 0.38, 0.75, 0.91, 0.95, 1), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR1$futilityPerStage[2, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR1$futilityStop, c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR1$earlyStop, c(0.04, 0.04, 0.12, 0.33, 0.5, 0.65, 0.89), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$conditionalPowerAchieved[2, ], c(0.12165751, 0.15502837, 0.23497758, 0.29890789, 0.33886493, 0.41286728, 0.49916888), tolerance = 1e-07)
	expect_equal(resultsWithSSR1$conditionalPowerAchieved[3, ], c(0.14749827, 0.23857933, 0.44868993, 0.59763371, 0.65378645, 0.66059558, 0.69812096), tolerance = 1e-07)

	# If thetaH1 is unspecified, the observed hazard ratio estimate 
	# (calculated from the log-rank statistic) is used for performing the 
	# recalculation of the number of events
	resultsWithSSR2 <- getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
		pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 146), 
		minNumberOfAdditionalEventsPerStage = c(58, 44, 44), 
		maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
		maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of SimulationResultsSurvival object 'resultsWithSSR2' with expected results
	##
	expect_equal(resultsWithSSR2$accrualIntensity, 66.666667, tolerance = 1e-07)
	expect_equal(resultsWithSSR2$pi1, c(0.3, 0.32452723, 0.34819506, 0.37103359, 0.39307188, 0.41433798, 0.43485894), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$median1, c(23.320299, 21.200271, 19.433582, 17.938691, 16.657356, 15.546866, 14.575187), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$median2, 23.320299, tolerance = 1e-07)
	expect_equal(resultsWithSSR2$lambda1, c(0.029722912, 0.032695203, 0.035667494, 0.038639786, 0.041612077, 0.044584368, 0.047556659), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$lambda2, 0.029722912, tolerance = 1e-07)
	expect_equal(resultsWithSSR2$iterations[1, ], c(100, 100, 100, 100, 100, 100, 100))
	expect_equal(resultsWithSSR2$iterations[2, ], c(100, 100, 100, 98, 96, 99, 92))
	expect_equal(resultsWithSSR2$iterations[3, ], c(99, 95, 92, 71, 60, 45, 21))
	expect_equal(resultsWithSSR2$analysisTime[1, ], c(7.9761501, 7.8239889, 7.5191849, 7.4832292, 7.3291066, 7.1091953, 6.9737455), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$analysisTime[2, ], c(17.532866, 16.792737, 15.753436, 15.242772, 14.414526, 13.395253, 12.536642), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$analysisTime[3, ], c(29.782185, 28.27297, 25.249508, 24.235039, 21.407797, 20.846814, 17.625231), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$studyDuration, c(29.663096, 27.530562, 24.305604, 21.136576, 18.176787, 16.398878, 13.170673), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$eventsPerStage[1, ], c(58, 58, 58, 58, 58, 58, 58), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$eventsPerStage[2, ], c(229.71, 222.76, 213.91, 210.63265, 201.21875, 185.82828, 171.84783), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$eventsPerStage[3, ], c(403.55556, 395.78947, 365.25, 358.80282, 327.35, 327.17778, 272.14286), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$expectedNumberOfEvents, c(401.8171, 387.138, 353.1428, 312.78082, 271.16875, 248.15727, 183.80196), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$eventsNotAchieved[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR2$eventsNotAchieved[2, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR2$eventsNotAchieved[3, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR2$numberOfSubjects[1, ], c(531.25, 521.07, 500.8, 498.42, 488.13, 473.47, 464.37), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$numberOfSubjects[2, ], c(798.3, 792.67, 784.71, 785.72449, 774.40625, 754.47475, 731), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$numberOfSubjects[3, ], c(800, 800, 800, 800, 799.08333, 797.51111, 794.95238), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$expectedNumberOfSubjects, c(799.983, 799.6335, 798.7768, 790.11401, 777.76145, 771.03106, 723.0996), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$rejectPerStage[1, ], c(0, 0, 0, 0.02, 0.04, 0.01, 0.08), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$rejectPerStage[2, ], c(0.01, 0.05, 0.08, 0.27, 0.36, 0.54, 0.71), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$rejectPerStage[3, ], c(0.03, 0.11, 0.29, 0.39, 0.48, 0.37, 0.19), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$overallReject, c(0.04, 0.16, 0.37, 0.68, 0.88, 0.92, 0.98), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$futilityPerStage[1, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR2$futilityPerStage[2, ], c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR2$futilityStop, c(0, 0, 0, 0, 0, 0, 0))
	expect_equal(resultsWithSSR2$earlyStop, c(0.01, 0.05, 0.08, 0.29, 0.4, 0.55, 0.79), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$conditionalPowerAchieved[2, ], c(0.13442705, 0.17515425, 0.27216274, 0.37121019, 0.42163288, 0.51345413, 0.62679958), tolerance = 1e-07)
	expect_equal(resultsWithSSR2$conditionalPowerAchieved[3, ], c(0.088787205, 0.13342075, 0.37806621, 0.51790868, 0.64116584, 0.64220287, 0.73456911), tolerance = 1e-07)

	# Compare it with design without event size recalculation
	resultsWithoutSSR <- getSimulationSurvival(design = dIN, hazardRatio = seq(1, 1.6, 0.1), 
		pi2 = 0.3, plannedEvents = c(58,102,145), maxNumberOfSubjects = 800, 
		maxNumberOfIterations = 100, seed = 1234567890)


	##
	## Comparison of the results of numeric object 'resultsWithoutSSR$overallReject' with expected results
	##
	expect_equal(resultsWithoutSSR$overallReject, c(0.06, 0.09, 0.26, 0.36, 0.5, 0.62, 0.8), tolerance = 1e-07)

	##
	## Comparison of the results of numeric object 'resultsWithSSR1$overallReject' with expected results
	##
	expect_equal(resultsWithSSR1$overallReject, c(0.04, 0.16, 0.38, 0.75, 0.91, 0.95, 1), tolerance = 1e-07)

	##
	## Comparison of the results of numeric object 'resultsWithSSR2$overallReject' with expected results
	##
	expect_equal(resultsWithSSR2$overallReject, c(0.04, 0.16, 0.37, 0.68, 0.88, 0.92, 0.98), tolerance = 1e-07)
})

test_that("'getSimulationSurvival': Confirm that event size racalcuation increases the Type I error rate, i.e., you have to use the combination test ", {

	.skipTestifDisabled()

	dGS <- getDesignGroupSequential(informationRates = c(0.4, 0.7, 1))
	resultsWithSSRGS <- getSimulationSurvival(design = dGS, hazardRatio = seq(1), 
		pi2 = 0.3, conditionalPower = 0.8, plannedEvents = c(58, 102, 145), 
		minNumberOfAdditionalEventsPerStage = c(58, 44, 44), 
		maxNumberOfAdditionalEventsPerStage = 4 * c(58, 44, 44),
		maxNumberOfSubjects = 800, maxNumberOfIterations = 100, seed = 1234567890)

	##
	## Comparison of the results of numeric object 'resultsWithSSRGS$overallReject' with expected results
	##
	expect_equal(resultsWithSSRGS$overallReject, 0.05, tolerance = 1e-07)
})

