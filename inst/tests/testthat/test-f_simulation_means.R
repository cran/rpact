######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 23 July 2019, 11:46:57                                                       #
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

context("Testing simulation means function")


test_that("'getSimulationMeans': several configurations", {
	.skipTestifDisabled()

	maxNumberOfIterations <- 100

	seed <- 99123
	options(width = 180)
	maxNumberOfSubjects <- 90
	informationRates <- c(0.2, 0.5, 1)
	plannedSubjects <- round(informationRates * maxNumberOfSubjects)

	x1 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = TRUE, thetaH0 = 0.4, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x1' with expected results
	##
	expect_equal(x1$effect, c(-0.4, -0.2, 0, 0.2, 0.4, 0.6), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x1$iterations[2, ], c(42, 49, 68, 87, 94, 97))
	expect_equal(x1$iterations[3, ], c(4, 9, 23, 43, 65, 68))
	expect_equal(x1$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x1$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x1$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x1$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0, 0, 0, 0.01, 0.09, 0.23), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[3, ], c(0, 0, 0.01, 0.13, 0.33, 0.53), tolerance = 1e-07)
	expect_equal(x1$overallReject, c(0, 0, 0.01, 0.14, 0.42, 0.76), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0.58, 0.51, 0.32, 0.13, 0.06, 0.03), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[2, ], c(0.38, 0.4, 0.45, 0.43, 0.2, 0.06), tolerance = 1e-07)
	expect_equal(x1$futilityStop, c(0.96, 0.91, 0.77, 0.56, 0.26, 0.09), tolerance = 1e-07)
	expect_equal(x1$earlyStop, c(0.96, 0.91, 0.77, 0.57, 0.35, 0.32), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(31.14, 35.28, 46.71, 60.84, 72.63, 74.79), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.017557086, 0.030814475, 0.058601262, 0.09027436, 0.17816715, 0.24070046), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.10771631, 0.32388388, 0.32415334, 0.38125404, 0.51933559, 0.59400955), tolerance = 1e-07)

	x2 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = FALSE, thetaH0 = 0.2, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x2' with expected results
	##
	expect_equal(x2$effect, c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x2$iterations[2, ], c(63, 73, 84, 83, 89, 96))
	expect_equal(x2$iterations[3, ], c(15, 24, 42, 53, 69, 76))
	expect_equal(x2$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x2$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x2$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x2$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[2, ], c(0, 0, 0.02, 0.03, 0.06, 0.1), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[3, ], c(0, 0.02, 0.05, 0.15, 0.27, 0.43), tolerance = 1e-07)
	expect_equal(x2$overallReject, c(0, 0.02, 0.07, 0.18, 0.33, 0.53), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0.37, 0.27, 0.16, 0.17, 0.11, 0.04), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[2, ], c(0.48, 0.49, 0.4, 0.27, 0.14, 0.1), tolerance = 1e-07)
	expect_equal(x2$futilityStop, c(0.85, 0.76, 0.56, 0.44, 0.25, 0.14), tolerance = 1e-07)
	expect_equal(x2$earlyStop, c(0.85, 0.76, 0.58, 0.47, 0.31, 0.24), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(41.76, 48.51, 59.58, 64.26, 73.08, 78.12), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.056595809, 0.082243527, 0.1171868, 0.14183443, 0.20192022, 0.18371302), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(0.36165449, 0.31543938, 0.36771185, 0.4758946, 0.54527876, 0.61204049), tolerance = 1e-07)

	x3 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 1, thetaH0 = 0.2,  
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x3' with expected results
	##
	expect_equal(x3$effect, c(-0.2, 0, 0.2, 0.4, 0.6, 0.8), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x3$iterations[2, ], c(50, 71, 87, 96, 97, 99))
	expect_equal(x3$iterations[3, ], c(9, 21, 63, 67, 49, 29))
	expect_equal(x3$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x3$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x3$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x3$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0.01), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0, 0, 0.03, 0.21, 0.47, 0.7), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[3, ], c(0, 0.02, 0.18, 0.38, 0.47, 0.29), tolerance = 1e-07)
	expect_equal(x3$overallReject, c(0, 0.02, 0.21, 0.59, 0.94, 1), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0.5, 0.29, 0.13, 0.04, 0.03, 0), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[2, ], c(0.41, 0.5, 0.21, 0.08, 0.01, 0), tolerance = 1e-07)
	expect_equal(x3$futilityStop, c(0.91, 0.79, 0.34, 0.12, 0.04, 0), tolerance = 1e-07)
	expect_equal(x3$earlyStop, c(0.91, 0.79, 0.37, 0.33, 0.51, 0.71), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(35.55, 46.62, 69.84, 74.07, 66.24, 57.78), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.047252355, 0.074094582, 0.18424333, 0.30402818, 0.54078356, 0.67131653), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.27249296, 0.30454177, 0.45212728, 0.62638376, 0.84307565, 0.91215549), tolerance = 1e-07)

	x4 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = TRUE, thetaH0 = 1.1,  
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x4' with expected results
	##
	expect_equal(x4$effect, c(-1.1, -0.9, -0.7, -0.5, -0.3, -0.1), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x4$iterations[2, ], c(95, 97, 88, 83, 82, 80))
	expect_equal(x4$iterations[3, ], c(74, 76, 68, 55, 50, 41))
	expect_equal(x4$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x4$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x4$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x4$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[2, ], c(0.16, 0.12, 0.06, 0.06, 0.01, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[3, ], c(0.56, 0.52, 0.38, 0.2, 0.11, 0.04), tolerance = 1e-07)
	expect_equal(x4$overallReject, c(0.72, 0.64, 0.44, 0.26, 0.12, 0.04), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0.05, 0.03, 0.12, 0.17, 0.18, 0.2), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[2, ], c(0.05, 0.09, 0.14, 0.22, 0.31, 0.39), tolerance = 1e-07)
	expect_equal(x4$futilityStop, c(0.1, 0.12, 0.26, 0.39, 0.49, 0.59), tolerance = 1e-07)
	expect_equal(x4$earlyStop, c(0.26, 0.24, 0.32, 0.45, 0.5, 0.59), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(76.95, 78.39, 72.36, 65.16, 62.64, 58.05), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.32767401, 0.23178095, 0.2071599, 0.2070829, 0.10752485, 0.096294166), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.75737536, 0.64651318, 0.56642877, 0.51397128, 0.44717442, 0.36357098), tolerance = 1e-07)

	x5 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = FALSE, thetaH0 = 1.1, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x5' with expected results
	##
	expect_equal(x5$effect, c(-1.1, -0.9, -0.7, -0.5, -0.3, -0.1), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x5$iterations[2, ], c(98, 96, 88, 84, 82, 79))
	expect_equal(x5$iterations[3, ], c(77, 74, 69, 58, 54, 43))
	expect_equal(x5$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x5$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x5$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x5$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[2, ], c(0.19, 0.14, 0.08, 0.06, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[3, ], c(0.59, 0.57, 0.43, 0.21, 0.13, 0.04), tolerance = 1e-07)
	expect_equal(x5$overallReject, c(0.78, 0.71, 0.51, 0.27, 0.13, 0.04), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0.02, 0.04, 0.12, 0.16, 0.18, 0.21), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[2, ], c(0.02, 0.08, 0.11, 0.2, 0.28, 0.36), tolerance = 1e-07)
	expect_equal(x5$futilityStop, c(0.04, 0.12, 0.23, 0.36, 0.46, 0.57), tolerance = 1e-07)
	expect_equal(x5$earlyStop, c(0.23, 0.26, 0.31, 0.42, 0.46, 0.57), tolerance = 1e-07)
	expect_equal(x5$expectedNumberOfSubjects, c(79.11, 77.22, 72.81, 66.78, 64.44, 58.68), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.33588936, 0.25194744, 0.19824827, 0.19178721, 0.11444971, 0.092566355), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.74226501, 0.69902839, 0.55641803, 0.50033698, 0.45636572, 0.33236099), tolerance = 1e-07)

	x6 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 1, thetaH0 = 0.8,  
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x6' with expected results
	##
	expect_equal(x6$effect, c(-0.8, -0.6, -0.4, -0.2, 0, 0.2), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x6$iterations[2, ], c(100, 99, 96, 81, 70, 49))
	expect_equal(x6$iterations[3, ], c(22, 43, 75, 57, 27, 7))
	expect_equal(x6$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18))
	expect_equal(x6$sampleSizes[2, ], c(27, 27, 27, 27, 27, 27))
	expect_equal(x6$sampleSizes[3, ], c(45, 45, 45, 45, 45, 45))
	expect_equal(x6$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[2, ], c(0.78, 0.56, 0.13, 0.05, 0, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[3, ], c(0.22, 0.4, 0.53, 0.21, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$overallReject, c(1, 0.96, 0.66, 0.26, 0.02, 0), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0, 0.01, 0.04, 0.19, 0.3, 0.51), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0.08, 0.19, 0.43, 0.42), tolerance = 1e-07)
	expect_equal(x6$futilityStop, c(0, 0.01, 0.12, 0.38, 0.73, 0.93), tolerance = 1e-07)
	expect_equal(x6$earlyStop, c(0.78, 0.57, 0.25, 0.43, 0.73, 0.93), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(54.9, 64.08, 77.67, 65.52, 49.05, 34.38), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.67267344, 0.52857476, 0.27194206, 0.18361852, 0.064769395, 0.04670856), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.81011604, 0.77276452, 0.65795757, 0.50391481, 0.35327029, 0.24591214), tolerance = 1e-07)

	x7 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 1, thetaH0 = -0.2, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, stDev = 3.5, alternative = seq(-1.2,-0.2,0.2), 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10,10,10), maxNumberOfSubjectsPerStage = c(100,100,100), directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x7' with expected results
	##
	expect_equal(x7$effect, c(-1, -0.8, -0.6, -0.4, -0.2, 0), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x7$iterations[2, ], c(93, 97, 88, 78, 78, 74))
	expect_equal(x7$iterations[3, ], c(52, 77, 69, 57, 51, 35))
	expect_equal(x7$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[2, ], c(74.918717, 83.151367, 90.734126, 88.517379, 94.605927, 95.502536), tolerance = 1e-07)
	expect_equal(x7$sampleSizes[3, ], c(34.779445, 56.130993, 68.133125, 83.503922, 92.63947, 93.575595), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0, 0, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[2, ], c(0.4, 0.19, 0.12, 0.07, 0, 0), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[3, ], c(0.41, 0.63, 0.47, 0.25, 0.12, 0.03), tolerance = 1e-07)
	expect_equal(x7$overallReject, c(0.81, 0.82, 0.59, 0.32, 0.12, 0.03), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0.07, 0.03, 0.12, 0.22, 0.22, 0.26), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[2, ], c(0.01, 0.01, 0.07, 0.14, 0.27, 0.39), tolerance = 1e-07)
	expect_equal(x7$futilityStop, c(0.08, 0.04, 0.19, 0.36, 0.49, 0.65), tolerance = 1e-07)
	expect_equal(x7$earlyStop, c(0.48, 0.23, 0.31, 0.43, 0.49, 0.65), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(105.75972, 141.87769, 144.85789, 134.64079, 139.03875, 121.42333), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.48960058, 0.35501907, 0.33230293, 0.3239724, 0.20164899, 0.17099815), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[3, ], c(0.75975737, 0.70067902, 0.61722401, 0.51061814, 0.40378864, 0.28388391), tolerance = 1e-07)

	x8 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5)), groups = 2, meanRatio = FALSE, thetaH0 = -0.1, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 3.5, 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10,40,40), maxNumberOfSubjectsPerStage = c(100,400,400), 
		seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x8' with expected results
	##
	expect_equal(x8$effect, c(0.1, 0.3, 0.5, 0.7, 0.9, 1.1), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x8$iterations[2, ], c(75, 75, 82, 77, 85, 88))
	expect_equal(x8$iterations[3, ], c(32, 45, 59, 64, 62, 66))
	expect_equal(x8$sampleSizes[1, ], c(18, 18, 18, 18, 18, 18), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[2, ], c(312.537, 315.47118, 298.71109, 298.99103, 273.70172, 271.45585), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[3, ], c(337.66315, 320.33174, 340.67902, 295.14071, 245.92316, 230.91095), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0, 0, 0, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.02, 0, 0.05, 0.08, 0.21, 0.21), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[3, ], c(0.02, 0.06, 0.22, 0.27, 0.42, 0.5), tolerance = 1e-07)
	expect_equal(x8$overallReject, c(0.04, 0.06, 0.27, 0.36, 0.63, 0.71), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0.25, 0.25, 0.18, 0.22, 0.15, 0.12), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[2, ], c(0.41, 0.3, 0.18, 0.05, 0.02, 0.01), tolerance = 1e-07)
	expect_equal(x8$futilityStop, c(0.66, 0.55, 0.36, 0.27, 0.17, 0.13), tolerance = 1e-07)
	expect_equal(x8$earlyStop, c(0.68, 0.55, 0.41, 0.36, 0.38, 0.34), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(360.45496, 398.75267, 463.94372, 437.11315, 403.11882, 409.28238), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.31465275, 0.38687556, 0.41716705, 0.36457183, 0.46957137, 0.48650775), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(0.36402168, 0.44332107, 0.47182355, 0.52975853, 0.68482255, 0.64923586), tolerance = 1e-07)

	x9 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5)), groups = 2, meanRatio = TRUE, thetaH0 = 1.6, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, alternative = seq(0.8,1.6,0.2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(50,50,50), maxNumberOfSubjectsPerStage = c(400,400,400), directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsMeans object 'x9' with expected results
	##
	expect_equal(x9$effect, c(-0.8, -0.6, -0.4, -0.2, 0), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x9$iterations[2, ], c(84, 86, 78, 67, 70))
	expect_equal(x9$iterations[3, ], c(53, 62, 62, 45, 23))
	expect_equal(x9$sampleSizes[1, ], c(18, 18, 18, 18, 18), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[2, ], c(257.73836, 278.8361, 303.27301, 306.23977, 339.30408), tolerance = 1e-07)
	expect_equal(x9$sampleSizes[3, ], c(153.57289, 230.09947, 313.53643, 325.28234, 342.27563), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0, 0, 0, 0.01, 0), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[2, ], c(0.3, 0.21, 0.06, 0.03, 0.02), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[3, ], c(0.44, 0.45, 0.39, 0.09, 0.01), tolerance = 1e-07)
	expect_equal(x9$overallReject, c(0.74, 0.66, 0.45, 0.13, 0.03), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0.16, 0.14, 0.22, 0.32, 0.3), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[2, ], c(0.01, 0.03, 0.1, 0.19, 0.45), tolerance = 1e-07)
	expect_equal(x9$futilityStop, c(0.17, 0.17, 0.32, 0.51, 0.75), tolerance = 1e-07)
	expect_equal(x9$earlyStop, c(0.47, 0.38, 0.38, 0.55, 0.77), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(315.89385, 400.46072, 448.94553, 369.5577, 334.23625), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.63427493, 0.5497222, 0.49353349, 0.49721138, 0.34872602), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[3, ], c(0.84712236, 0.77882668, 0.6178067, 0.51284066, 0.41825576), tolerance = 1e-07)

	myStageSubjects <- function(..., stage, thetaH0, allocationRatioPlanned,
			minNumberOfSubjectsPerStage,	maxNumberOfSubjectsPerStage,
			sampleSizesPerStage, thetaStandardized,	conditionalPower, conditionalCriticalValue) {
		mult <- 1
		if (stage == 2){
			stageSubjects <- (1 + 1/allocationRatioPlanned + thetaH0^2 * (1 + allocationRatioPlanned))*
					(max(0, conditionalCriticalValue + stats::qnorm(conditionalPower)))^2 * mult / 
					(max(1e-12, thetaStandardized))^2
			stageSubjects <- min(max(minNumberOfSubjectsPerStage[stage], stageSubjects), 
					maxNumberOfSubjectsPerStage[stage])
		} else {
			stageSubjects <- sampleSizesPerStage[stage - 1]
		}
		return(stageSubjects)
	}
	x10 <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5)), groups = 2, meanRatio = TRUE, thetaH0 = 1.6, 
		plannedSubjects = c(80,160,240), maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, alternative = seq(0.8,1.6,0.2),
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10,40,40), maxNumberOfSubjectsPerStage = c(200,400,400), 
		allocationRatioPlanned = 3, directionUpper = FALSE, seed = seed, calcSubjectsFunction = myStageSubjects)

	##
	## Comparison of the results of SimulationResultsMeans object 'x10' with expected results
	##
	expect_equal(x10$effect, c(-0.8, -0.6, -0.4, -0.2, 0), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x10$iterations[2, ], c(76, 73, 61, 39, 31))
	expect_equal(x10$iterations[3, ], c(26, 36, 49, 28, 25))
	expect_equal(x10$sampleSizes[1, ], c(80, 80, 80, 80, 80), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[2, ], c(226.65982, 237.82641, 309.41238, 322.48967, 279.64545), tolerance = 1e-07)
	expect_equal(x10$sampleSizes[3, ], c(192.90106, 242.76454, 304.34981, 316.96502, 285.17103), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0.01, 0, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.5, 0.37, 0.11, 0.04, 0.02), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[3, ], c(0.23, 0.26, 0.27, 0.07, 0.01), tolerance = 1e-07)
	expect_equal(x10$overallReject, c(0.74, 0.63, 0.38, 0.11, 0.03), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0.23, 0.27, 0.39, 0.61, 0.69), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[2, ], c(0, 0, 0.01, 0.07, 0.04), tolerance = 1e-07)
	expect_equal(x10$futilityStop, c(0.23, 0.27, 0.4, 0.68, 0.73), tolerance = 1e-07)
	expect_equal(x10$earlyStop, c(0.74, 0.64, 0.51, 0.72, 0.75), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(302.41574, 341.00851, 417.87296, 294.52118, 237.98285), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.83285854, 0.79260753, 0.67563397, 0.60585275, 0.66737426), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[3, ], c(0.84091805, 0.78999444, 0.60898256, 0.53740242, 0.36130254), tolerance = 1e-07)

	#x <- getSimulationRates(getDesignGroupSequential(),plannedSubjects = c(33,67,100))
	#y <- getPowerRates(getDesignGroupSequential(),maxNumberOfSubjects = 100)
	#
	#plot(x, type = 5)
	#plot(y, type = 5)

	#options(width = 180)
	#maxNumberOfSubjects <- 200
	#informationRates <- c(0.2,0.5,1)
	#plannedSubjects <- round(informationRates*maxNumberOfSubjects)
	#maxNumberOfIterations <- 10000
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = TRUE, thetaH0 = 0.4, 
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5)
	#y <- getPowerMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates),  groups = 2, meanRatio = TRUE, thetaH0 = 0.4,
	#		maxNumberOfSubjects = maxNumberOfSubjects, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE)
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$futilityStop - y$futilityStop,4)
	#x$overallReject
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = FALSE, thetaH0 = 0.2, 
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5)
	#y <- getPowerMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates),  groups = 2, meanRatio = FALSE, thetaH0 = 0.2,
	#		maxNumberOfSubjects = maxNumberOfSubjects, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE)
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$futilityStop - y$futilityStop,4)
	#x$overallReject
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 1, thetaH0 = 0.2,  
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5)
	#y <- getPowerMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates),  groups = 1, thetaH0 = 0.2, 
	#		maxNumberOfSubjects = maxNumberOfSubjects, stDev = 1.5, normalApproximation = TRUE)
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$futilityStop - y$futilityStop,4)
	#x$overallReject
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = TRUE, thetaH0 = 1.1,  
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE)
	#y <- getPowerMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates),  groups = 2, meanRatio = TRUE, thetaH0 = 1.1,
	#		maxNumberOfSubjects = maxNumberOfSubjects, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE, directionUpper = FALSE)
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$futilityStop - y$futilityStop,4)
	#x$overallReject
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 2, meanRatio = FALSE, thetaH0 = 1.1, 
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 1.5, directionUpper = FALSE)
	#y <- getPowerMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates),  groups = 2, meanRatio = FALSE, thetaH0 = 1.1,
	#		maxNumberOfSubjects = maxNumberOfSubjects, allocationRatioPlanned = 3, stDev = 1.5, normalApproximation = TRUE, directionUpper = FALSE)
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$futilityStop - y$futilityStop,4)
	#x$overallReject
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates), groups = 1, thetaH0 = 0.8,  
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, directionUpper = FALSE)
	#y <- getPowerMeans(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), informationRates = informationRates),  groups = 1, thetaH0 = 0.8, 
	#		maxNumberOfSubjects = maxNumberOfSubjects, stDev = 1.5, normalApproximation = TRUE, directionUpper = FALSE)
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$futilityStop - y$futilityStop,4)
	#x$overallReject
	#
	#
	#options(width = 180)
	#maxNumberOfSubjects <- 150
	#informationRates <- (1:3)/3
	#plannedSubjects <- round(informationRates*maxNumberOfSubjects)
	#maxNumberOfIterations <- 20000
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5), informationRates = informationRates), groups = 1, thetaH0 = 0, 
	#		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, stDev = 3.5, alternative = seq(-1,0,0.2), 
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10,10,10), maxNumberOfSubjectsPerStage = c(100,100,100), directionUpper = FALSE)
	#x$overallReject
	#x$futilityStop
	#x$expectedNumberOfSubjects
	#x$conditionalPowerAchieved
	#x$sampleSizes
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(0.5, 0.5)), groups = 2, meanRatio = FALSE, thetaH0 = -0.1, 
	#		plannedSubjects = c(80,160,240), maxNumberOfIterations = maxNumberOfIterations, allocationRatioPlanned = 3, stDev = 3.5, 
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10, 40, 40), maxNumberOfSubjectsPerStage = c(100,400, 400))
	#x$overallReject
	#x$futilityStop
	#x$expectedNumberOfSubjects
	#x$conditionalPowerAchieved
	#x$sampleSizes
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5)), groups = 2, meanRatio = TRUE, thetaH0 = 1.6, 
	#		plannedSubjects = c(80,160,240), maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, alternative = seq(0.8,1.6,0.2),
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10,40,40), maxNumberOfSubjectsPerStage = c(200,400,400), 
	#		allocationRatioPlanned = 3, directionUpper = FALSE)
	#
	#x$overallReject
	#x$futilityStop
	#x$expectedNumberOfSubjects
	#x$conditionalPowerAchieved
	#x$sampleSizes
	#
	#myStageSubjects <- function(..., stage, thetaH0, allocationRatioPlanned,
	#		minNumberOfSubjectsPerStage,	maxNumberOfSubjectsPerStage,
	#		sampleSizesPerStage, thetaStandardized,	conditionalPower, conditionalCriticalValue) {
	#	if (stage == 2){
	#		stageSubjects <- (1 + 1/allocationRatioPlanned + thetaH0^2 * (1 + allocationRatioPlanned))*
	#				(max(0, conditionalCriticalValue + stats::qnorm(conditionalPower)))^2 * mult / 
	#				(max(1e-12, thetaStandardized))^2
	#		stageSubjects <- min(max(minNumberOfSubjectsPerStage[stage], stageSubjects), 
	#				maxNumberOfSubjectsPerStage[stage])
	#	} else {
	#		stageSubjects <- sampleSizesPerStage[stage - 1]
	#	}
	#	return(stageSubjects)
	#}
	#
	#x <- getSimulationMeans(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5)), groups = 2, meanRatio = TRUE, thetaH0 = 1.6, 
	#		plannedSubjects = c(80,160,240), maxNumberOfIterations = maxNumberOfIterations, stDev = 1.5, alternative = seq(0.8,1.6,0.2),
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(10,40,40), maxNumberOfSubjectsPerStage = c(200,400,400), 
	#		allocationRatioPlanned = 3, directionUpper = FALSE, calcSubjectsFunction = myStageSubjects)
	#
	#

})

