######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 06 November 2019, 17:12:58                                                   #
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

context("Testing simulation rates function")


test_that("'getSimulationRates': check several configurations", {
	.skipTestifDisabled()

	# @refFS[Sec.]{fs:subsec:seed}
	seed <- 99123
	maxNumberOfIterations <- 100
	options(width = 180)
	maxNumberOfSubjects <- 90
	informationRates <- (1:3) / 3
	plannedSubjects <- round(informationRates * maxNumberOfSubjects)

	x1 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), 
		informationRates = informationRates), groups = 2, riskRatio = TRUE, thetaH0 = 0.8, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, 
		allocationRatioPlanned = 3, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x1' with expected results
	##
	expect_equal(x1$effect, c(0.2, 0.7, 1.2, 1.7), tolerance = 1e-07)
	expect_equal(x1$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x1$iterations[2, ], c(78, 93, 99, 96))
	expect_equal(x1$iterations[3, ], c(41, 68, 56, 40))
	expect_equal(x1$sampleSizes[1, ], c(30, 30, 30, 30))
	expect_equal(x1$sampleSizes[2, ], c(30, 30, 30, 30))
	expect_equal(x1$sampleSizes[3, ], c(30, 30, 30, 30))
	expect_equal(x1$rejectPerStage[1, ], c(0, 0, 0, 0.04), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[2, ], c(0.02, 0.04, 0.34, 0.54), tolerance = 1e-07)
	expect_equal(x1$rejectPerStage[3, ], c(0.03, 0.19, 0.4, 0.3), tolerance = 1e-07)
	expect_equal(x1$overallReject, c(0.05, 0.23, 0.74, 0.88), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[1, ], c(0.22, 0.07, 0.01, 0), tolerance = 1e-07)
	expect_equal(x1$futilityPerStage[2, ], c(0.35, 0.21, 0.09, 0.02), tolerance = 1e-07)
	expect_equal(x1$futilityStop, c(0.57, 0.28, 0.1, 0.02), tolerance = 1e-07)
	expect_equal(x1$earlyStop, c(0.59, 0.32, 0.44, 0.6), tolerance = 1e-07)
	expect_equal(x1$expectedNumberOfSubjects, c(65.7, 78.3, 76.5, 70.8), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x1$conditionalPowerAchieved[2, ], c(0.090943215, 0.15808459, 0.48521663, 0.52642331), tolerance = 1e-07)
	expect_equal(x1$conditionalPowerAchieved[3, ], c(0.22475932, 0.38294099, 0.60961381, 0.67377136), tolerance = 1e-07)

	x2 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), 
		informationRates = informationRates), groups = 2, riskRatio = FALSE, thetaH0 = -0.1, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, 
		allocationRatioPlanned = 3, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x2' with expected results
	##
	expect_equal(x2$effect, c(0.1, 0.2, 0.3, 0.4), tolerance = 1e-07)
	expect_equal(x2$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x2$iterations[2, ], c(84, 95, 100, 97))
	expect_equal(x2$iterations[3, ], c(55, 73, 64, 42))
	expect_equal(x2$sampleSizes[1, ], c(30, 30, 30, 30))
	expect_equal(x2$sampleSizes[2, ], c(30, 30, 30, 30))
	expect_equal(x2$sampleSizes[3, ], c(30, 30, 30, 30))
	expect_equal(x2$rejectPerStage[1, ], c(0, 0, 0, 0.03), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[2, ], c(0.02, 0.09, 0.33, 0.53), tolerance = 1e-07)
	expect_equal(x2$rejectPerStage[3, ], c(0.06, 0.3, 0.48, 0.32), tolerance = 1e-07)
	expect_equal(x2$overallReject, c(0.08, 0.39, 0.81, 0.88), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[1, ], c(0.16, 0.05, 0, 0), tolerance = 1e-07)
	expect_equal(x2$futilityPerStage[2, ], c(0.27, 0.13, 0.03, 0.02), tolerance = 1e-07)
	expect_equal(x2$futilityStop, c(0.43, 0.18, 0.03, 0.02), tolerance = 1e-07)
	expect_equal(x2$earlyStop, c(0.45, 0.27, 0.36, 0.58), tolerance = 1e-07)
	expect_equal(x2$expectedNumberOfSubjects, c(71.7, 80.4, 79.2, 71.7), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x2$conditionalPowerAchieved[2, ], c(0.10237911, 0.25306891, 0.43740091, 0.54067879), tolerance = 1e-07)
	expect_equal(x2$conditionalPowerAchieved[3, ], c(0.30171473, 0.4623858, 0.59071853, 0.68245332), tolerance = 1e-07)

	x3 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(-0.5,0.5), 
		informationRates = informationRates), groups = 1, thetaH0 = 0.2, pi1 = seq(0.2, 0.4, 0.05),  
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x3' with expected results
	##
	expect_equal(x3$effect, c(0, 0.05, 0.1, 0.15, 0.2), tolerance = 1e-07)
	expect_equal(x3$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x3$iterations[2, ], c(78, 91, 96, 90, 72))
	expect_equal(x3$iterations[3, ], c(32, 65, 62, 37, 6))
	expect_equal(x3$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x3$sampleSizes[2, ], c(30, 30, 30, 30, 30))
	expect_equal(x3$sampleSizes[3, ], c(30, 30, 30, 30, 30))
	expect_equal(x3$rejectPerStage[1, ], c(0, 0.02, 0.04, 0.1, 0.28), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[2, ], c(0.01, 0.06, 0.28, 0.53, 0.66), tolerance = 1e-07)
	expect_equal(x3$rejectPerStage[3, ], c(0.02, 0.22, 0.28, 0.3, 0.05), tolerance = 1e-07)
	expect_equal(x3$overallReject, c(0.03, 0.3, 0.6, 0.93, 0.99), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[1, ], c(0.22, 0.07, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x3$futilityPerStage[2, ], c(0.45, 0.2, 0.06, 0, 0), tolerance = 1e-07)
	expect_equal(x3$futilityStop, c(0.67, 0.27, 0.06, 0, 0), tolerance = 1e-07)
	expect_equal(x3$earlyStop, c(0.68, 0.35, 0.38, 0.63, 0.94), tolerance = 1e-07)
	expect_equal(x3$expectedNumberOfSubjects, c(63, 76.8, 77.4, 68.1, 53.4), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x3$conditionalPowerAchieved[2, ], c(0.12773913, 0.18983473, 0.36146118, 0.53982038, 0.7268178), tolerance = 1e-07)
	expect_equal(x3$conditionalPowerAchieved[3, ], c(0.32676971, 0.35596086, 0.46114911, 0.56126649, 0.75350644), tolerance = 1e-07)

	x4 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5), 
		informationRates = informationRates), groups = 2, riskRatio = TRUE, thetaH0 = 1.5, 
		pi1 = seq(0.05,0.25,0.05), plannedSubjects = plannedSubjects, 
		maxNumberOfIterations = maxNumberOfIterations, directionUpper = FALSE, 
		allocationRatioPlanned = 3, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x4' with expected results
	##
	expect_equal(x4$effect, c(-1.25, -1, -0.75, -0.5, -0.25), tolerance = 1e-07)
	expect_equal(x4$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x4$iterations[2, ], c(74, 64, 47, 36, 39))
	expect_equal(x4$iterations[3, ], c(28, 28, 30, 20, 25))
	expect_equal(x4$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x4$sampleSizes[2, ], c(30, 30, 30, 30, 30))
	expect_equal(x4$sampleSizes[3, ], c(30, 30, 30, 30, 30))
	expect_equal(x4$rejectPerStage[1, ], c(0.06, 0.05, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[2, ], c(0.43, 0.29, 0.09, 0.04, 0.04), tolerance = 1e-07)
	expect_equal(x4$rejectPerStage[3, ], c(0.17, 0.17, 0.08, 0.04, 0.06), tolerance = 1e-07)
	expect_equal(x4$overallReject, c(0.66, 0.51, 0.19, 0.08, 0.1), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[1, ], c(0.2, 0.31, 0.51, 0.64, 0.61), tolerance = 1e-07)
	expect_equal(x4$futilityPerStage[2, ], c(0.03, 0.07, 0.08, 0.12, 0.1), tolerance = 1e-07)
	expect_equal(x4$futilityStop, c(0.23, 0.38, 0.59, 0.76, 0.71), tolerance = 1e-07)
	expect_equal(x4$earlyStop, c(0.72, 0.72, 0.7, 0.8, 0.75), tolerance = 1e-07)
	expect_equal(x4$expectedNumberOfSubjects, c(60.6, 57.6, 53.1, 46.8, 49.2), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x4$conditionalPowerAchieved[2, ], c(0.65569733, 0.50411153, 0.40992455, 0.37112776, 0.28877148), tolerance = 1e-07)
	expect_equal(x4$conditionalPowerAchieved[3, ], c(0.52876953, 0.55375049, 0.46252843, 0.37280654, 0.34687207), tolerance = 1e-07)

	x5 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5), 
		informationRates = informationRates), groups = 2, riskRatio = FALSE, thetaH0 = 0.1, 
		plannedSubjects = plannedSubjects, maxNumberOfIterations = maxNumberOfIterations, 
		allocationRatioPlanned = 3, directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x5' with expected results
	##
	expect_equal(x5$effect, c(-0.1, 2.7755576e-17, 0.1, 0.2), tolerance = 1e-07)
	expect_equal(x5$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x5$iterations[2, ], c(50, 41, 12, 2))
	expect_equal(x5$iterations[3, ], c(34, 29, 3, 0))
	expect_equal(x5$sampleSizes[1, ], c(30, 30, 30, 30))
	expect_equal(x5$sampleSizes[2, ], c(30, 30, 30, 30))
	expect_equal(x5$sampleSizes[3, ], c(30, 30, 30, NaN))
	expect_equal(x5$rejectPerStage[1, ], c(0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[2, ], c(0.09, 0.02, 0, 0), tolerance = 1e-07)
	expect_equal(x5$rejectPerStage[3, ], c(0.12, 0.01, 0, 0), tolerance = 1e-07)
	expect_equal(x5$overallReject, c(0.22, 0.03, 0, 0), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[1, ], c(0.49, 0.59, 0.88, 0.98), tolerance = 1e-07)
	expect_equal(x5$futilityPerStage[2, ], c(0.07, 0.1, 0.09, 0.02), tolerance = 1e-07)
	expect_equal(x5$futilityStop, c(0.56, 0.69, 0.97, 1), tolerance = 1e-07)
	expect_equal(x5$earlyStop, c(0.66, 0.71, 0.97, 1), tolerance = 1e-07)
	expect_equal(x5$expectedNumberOfSubjects, c(55.2, 51, 34.5, 30.6), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x5$conditionalPowerAchieved[2, ], c(0.36523014, 0.20927326, 0.16995311, 0.25129054), tolerance = 1e-07)
	expect_equal(x5$conditionalPowerAchieved[3, ], c(0.43064609, 0.32068397, 0.041565592, NaN), tolerance = 1e-07)

	x6 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(0.5,0.5), 
		informationRates = informationRates), groups = 1, thetaH0 = 0.4, pi1 = seq(0.2, 0.4, 0.05),
		plannedSubjects = plannedSubjects, directionUpper = FALSE, 
		maxNumberOfIterations = maxNumberOfIterations, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x6' with expected results
	##
	expect_equal(x6$effect, c(-0.2, -0.15, -0.1, -0.05, 0), tolerance = 1e-07)
	expect_equal(x6$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x6$iterations[2, ], c(91, 89, 66, 56, 39))
	expect_equal(x6$iterations[3, ], c(19, 49, 51, 48, 24))
	expect_equal(x6$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x6$sampleSizes[2, ], c(30, 30, 30, 30, 30))
	expect_equal(x6$sampleSizes[3, ], c(30, 30, 30, 30, 30))
	expect_equal(x6$rejectPerStage[1, ], c(0.03, 0.01, 0, 0, 0), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[2, ], c(0.72, 0.4, 0.14, 0.01, 0.01), tolerance = 1e-07)
	expect_equal(x6$rejectPerStage[3, ], c(0.17, 0.37, 0.26, 0.14, 0.02), tolerance = 1e-07)
	expect_equal(x6$overallReject, c(0.92, 0.78, 0.4, 0.15, 0.03), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[1, ], c(0.06, 0.1, 0.34, 0.44, 0.61), tolerance = 1e-07)
	expect_equal(x6$futilityPerStage[2, ], c(0, 0, 0.01, 0.07, 0.14), tolerance = 1e-07)
	expect_equal(x6$futilityStop, c(0.06, 0.1, 0.35, 0.51, 0.75), tolerance = 1e-07)
	expect_equal(x6$earlyStop, c(0.81, 0.51, 0.49, 0.52, 0.76), tolerance = 1e-07)
	expect_equal(x6$expectedNumberOfSubjects, c(63, 71.4, 65.1, 61.2, 48.9), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x6$conditionalPowerAchieved[2, ], c(0.72335875, 0.55247274, 0.3843863, 0.29482523, 0.18598438), tolerance = 1e-07)
	expect_equal(x6$conditionalPowerAchieved[3, ], c(0.71459365, 0.68392316, 0.54740245, 0.39208559, 0.15519282), tolerance = 1e-07)

	x7 <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(0.5), typeOfDesign = "P"), 
		thetaH0 = 0.3, groups = 1, plannedSubjects = c(30,60), 
		pi1 = seq(0.3,0.5,0.05),maxNumberOfIterations = maxNumberOfIterations, 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(30, 30), 
		maxNumberOfSubjectsPerStage = 5 * c(30, 30), 
		directionUpper = TRUE, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x7' with expected results
	##
	expect_equal(x7$effect, c(0, 0.05, 0.1, 0.15, 0.2), tolerance = 1e-07)
	expect_equal(x7$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x7$iterations[2, ], c(25, 41, 53, 50, 35))
	expect_equal(x7$sampleSizes[1, ], c(30, 30, 30, 30, 30))
	expect_equal(x7$sampleSizes[2, ], c(114.24, 115.68293, 100.39623, 101.92, 82.371429), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[1, ], c(0.02, 0.06, 0.15, 0.36, 0.59), tolerance = 1e-07)
	expect_equal(x7$rejectPerStage[2, ], c(0.03, 0.12, 0.32, 0.41, 0.32), tolerance = 1e-07)
	expect_equal(x7$overallReject, c(0.05, 0.18, 0.47, 0.77, 0.91), tolerance = 1e-07)
	expect_equal(x7$futilityPerStage[1, ], c(0.73, 0.53, 0.32, 0.14, 0.06), tolerance = 1e-07)
	expect_equal(x7$futilityStop, c(0.73, 0.53, 0.32, 0.14, 0.06), tolerance = 1e-07)
	expect_equal(x7$earlyStop, c(0.75, 0.59, 0.47, 0.5, 0.65), tolerance = 1e-07)
	expect_equal(x7$expectedNumberOfSubjects, c(58.56, 77.43, 83.21, 80.96, 58.83), tolerance = 1e-07)
	expect_equal(x7$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x7$conditionalPowerAchieved[2, ], c(0.60107965, 0.60407724, 0.68409402, 0.68536207, 0.68807468), tolerance = 1e-07)

	x8 <- getSimulationRates(design = getDesignGroupSequential(
		futilityBounds = c(0.5,0.5), typeOfDesign = "P"), 
		thetaH0 = 0.3, groups = 2, allocationRatioPlanned = 3, plannedSubjects = (1:3) * 100, 
		pi1 = seq(0.2, 0.4, 0.05), pi2 = 0.2, maxNumberOfIterations = maxNumberOfIterations, 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100,100,100), 
		maxNumberOfSubjectsPerStage = 5*c(100,100,100), 
		directionUpper = FALSE, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x8' with expected results
	##
	expect_equal(x8$effect, c(-0.3, -0.25, -0.2, -0.15, -0.1), tolerance = 1e-07)
	expect_equal(x8$iterations[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x8$iterations[2, ], c(7, 23, 41, 52, 59))
	expect_equal(x8$iterations[3, ], c(0, 1, 1, 11, 20))
	expect_equal(x8$sampleSizes[1, ], c(100, 100, 100, 100, 100))
	expect_equal(x8$sampleSizes[2, ], c(225.57143, 148.73913, 239.7561, 361.73077, 405.05085), tolerance = 1e-07)
	expect_equal(x8$sampleSizes[3, ], c(NaN, 112, 316, 398, 405.85), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[1, ], c(0.93, 0.75, 0.54, 0.29, 0.1), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[2, ], c(0.07, 0.22, 0.4, 0.41, 0.37), tolerance = 1e-07)
	expect_equal(x8$rejectPerStage[3, ], c(0, 0.01, 0.01, 0.11, 0.14), tolerance = 1e-07)
	expect_equal(x8$overallReject, c(1, 0.98, 0.95, 0.81, 0.61), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[1, ], c(0, 0.02, 0.05, 0.19, 0.31), tolerance = 1e-07)
	expect_equal(x8$futilityPerStage[2, ], c(0, 0, 0, 0, 0.02), tolerance = 1e-07)
	expect_equal(x8$futilityStop, c(0, 0.02, 0.05, 0.19, 0.33), tolerance = 1e-07)
	expect_equal(x8$earlyStop, c(1, 0.99, 0.99, 0.89, 0.8), tolerance = 1e-07)
	expect_equal(x8$expectedNumberOfSubjects, c(115.79, 135.33, 201.46, 331.88, 420.15), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x8$conditionalPowerAchieved[2, ], c(0.79294349, 0.80728899, 0.77763316, 0.64160567, 0.53147513), tolerance = 1e-07)
	expect_equal(x8$conditionalPowerAchieved[3, ], c(NaN, 0.80069037, 0.80071364, 0.56677072, 0.57523679), tolerance = 1e-07)

	x9 <- getSimulationRates(design = getDesignGroupSequential(
		futilityBounds = c(0), typeOfDesign = "P"), 
		thetaH0 = 0.8, groups = 2, riskRatio = TRUE, allocationRatioPlanned = 3, 
		maxNumberOfIterations = maxNumberOfIterations,
		plannedSubjects = c(100,200), pi1 = seq(0.15, 0.4, 0.05), pi2 = 0.2, 
		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100,100), 
		maxNumberOfSubjectsPerStage = 5*c(100, 100), directionUpper = TRUE, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x9' with expected results
	##
	expect_equal(x9$effect, c(-0.05, 0.2, 0.45, 0.7, 0.95, 1.2), tolerance = 1e-07)
	expect_equal(x9$iterations[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x9$iterations[2, ], c(48, 66, 75, 74, 57, 35))
	expect_equal(x9$sampleSizes[1, ], c(100, 100, 100, 100, 100, 100))
	expect_equal(x9$sampleSizes[2, ], c(466.29167, 407.39394, 382.84, 357.2973, 256.61404, 268.45714), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[1, ], c(0.01, 0.02, 0.11, 0.24, 0.41, 0.65), tolerance = 1e-07)
	expect_equal(x9$rejectPerStage[2, ], c(0, 0.05, 0.34, 0.62, 0.51, 0.35), tolerance = 1e-07)
	expect_equal(x9$overallReject, c(0.01, 0.07, 0.45, 0.86, 0.92, 1), tolerance = 1e-07)
	expect_equal(x9$futilityPerStage[1, ], c(0.51, 0.32, 0.14, 0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x9$futilityStop, c(0.51, 0.32, 0.14, 0.02, 0.02, 0), tolerance = 1e-07)
	expect_equal(x9$earlyStop, c(0.52, 0.34, 0.25, 0.26, 0.43, 0.65), tolerance = 1e-07)
	expect_equal(x9$expectedNumberOfSubjects, c(323.82, 368.88, 387.13, 364.4, 246.27, 193.96), tolerance = 1e-07)
	expect_equal(x9$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x9$conditionalPowerAchieved[2, ], c(0.32248415, 0.49314797, 0.522945, 0.55888112, 0.72047998, 0.75410423), tolerance = 1e-07)

	mySampleSizeCalculationFunction <- function(...,stage,
			plannedSubjects,
			minNumberOfSubjectsPerStage,
			maxNumberOfSubjectsPerStage,
			conditionalPower,
			conditionalCriticalValue,
			overallRate) {
		if (overallRate[1] - overallRate[2] < 0.1){
	 		return(plannedSubjects[stage] - plannedSubjects[stage - 1]) 
	 	} else {
	 		rateUnderH0 <- (overallRate[1] + overallRate[2])/2 
	 		stageSubjects <- 2 * (max(0, conditionalCriticalValue * 
				sqrt(2 * rateUnderH0 * (1 - rateUnderH0)) + 
				stats::qnorm(conditionalPower) * sqrt(overallRate[1] * (1 - overallRate[1]) +
				overallRate[2] * (1 - overallRate[2]))))^2 /
				(max(1e-12,	(overallRate[1] - overallRate[2])))^2
	 		stageSubjects <- ceiling(min(max(minNumberOfSubjectsPerStage[stage], 
	 			stageSubjects), maxNumberOfSubjectsPerStage[stage]))
	 		return(stageSubjects)
	 	}	
	}
	x10 <- getSimulationRates(design = getDesignInverseNormal(kMax = 2), 
		pi1 = seq(0.3,0.6,0.1), pi2 = 0.3, plannedSubjects = c(40, 80), 
		minNumberOfSubjectsPerStage = c(40, 20), 
		maxNumberOfSubjectsPerStage = c(40, 160),
		conditionalPower = 0.8,	calcSubjectsFunction = mySampleSizeCalculationFunction, 
		maxNumberOfIterations = maxNumberOfIterations, seed = seed)

	##
	## Comparison of the results of SimulationResultsRates object 'x10' with expected results
	##
	expect_equal(x10$effect, c(0, 0.1, 0.2, 0.3), tolerance = 1e-07)
	expect_equal(x10$iterations[1, ], c(100, 100, 100, 100))
	expect_equal(x10$iterations[2, ], c(100, 99, 95, 75))
	expect_equal(x10$sampleSizes[1, ], c(40, 40, 40, 40))
	expect_equal(x10$sampleSizes[2, ], c(64.34, 74.444444, 65.126316, 58.253333), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[1, ], c(0, 0.01, 0.05, 0.25), tolerance = 1e-07)
	expect_equal(x10$rejectPerStage[2, ], c(0.02, 0.19, 0.47, 0.64), tolerance = 1e-07)
	expect_equal(x10$overallReject, c(0.02, 0.2, 0.52, 0.89), tolerance = 1e-07)
	expect_equal(x10$futilityPerStage[1, ], c(0, 0, 0, 0))
	expect_equal(x10$futilityStop, c(0, 0, 0, 0))
	expect_equal(x10$earlyStop, c(0, 0.01, 0.05, 0.25), tolerance = 1e-07)
	expect_equal(x10$expectedNumberOfSubjects, c(104.34, 113.7, 101.87, 83.69), tolerance = 1e-07)
	expect_equal(x10$conditionalPowerAchieved[1, ], c(NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x10$conditionalPowerAchieved[2, ], c(0.20349537, 0.39194633, 0.57556995, 0.71162895), tolerance = 1e-07)

	#options(width = 180)
	#maxNumberOfSubjects <- 300
	#informationRates <- (1:2)/2
	#plannedSubjects <- round(informationRates*maxNumberOfSubjects)
	#maxNumberOfIterations <- 10000
	#
	#x <- getSimulationRates(design = getDesignInverseNormal(futilityBounds = c(-1), informationRates = informationRates,typeOfDesign = "P"), 
	#		thetaH0 = 0.4, groups = 1, plannedSubjects = plannedSubjects, pi1 = seq(0.3,0.4,0.02),maxNumberOfIterations = maxNumberOfIterations, 
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100,100), maxNumberOfSubjectsPerStage = 5*c(100,100), directionUpper = FALSE)
	#x$overallReject
	#x$expectedNumberOfSubjects
	#x$conditionalPowerAchieved
	#x$sampleSizes
	#x$rejectPerStage
	#x$futilityStop
	#y <- getPowerRates(design = getDesignGroupSequential(futilityBounds = c(-1,1), informationRates = informationRates,typeOfDesign = "P"), 
	#		thetaH0 = 0.4, groups = 1, pi1 = seq(0.3,0.4,0.02), directionUpper = FALSE, maxNumberOfSubjects = maxNumberOfSubjects)

	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#x$expectedNumberOfSubjects
	#y$expectedNumberOfSubjects
	#x$overallReject
	#round(x$overallReject - y$overallReject,4)
	#round(x$rejectPerStage - y$rejectPerStage,4)
	#round(x$futilityPerStage - y$futilityPerStage,4)
	#
	#x <- getSimulationRates(design = getDesignGroupSequential(futilityBounds = c(-1,1), typeOfDesign = "P"), 
	#		thetaH0 = 0.3, groups = 2, allocationRatioPlanned = 2, plannedSubjects = (1:3)*100, pi1 = seq(0.2, 0.4, 0.05), pi2 = 0.1, 
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = c(100,100,100), maxNumberOfSubjectsPerStage = 1*c(100,100,100), directionUpper = FALSE)
	#
	#y <- getPowerRates(design = getDesignGroupSequential(futilityBounds = c(-1,1), typeOfDesign = "P"), 
	#		thetaH0 = 0.3, groups = 2, allocationRatioPlanned = 2, pi1 = seq(0.2, 0.4, 0.05), pi2 = 0.1, directionUpper = FALSE, maxNumberOfSubjects = 300)
	#
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#x$expectedNumberOfSubjects
	#y$expectedNumberOfSubjects
	#x$overallReject
	#round(x$overallReject - y$overallReject,4)
	#round(x$rejectPerStage - y$rejectPerStage,4)
	#round(x$futilityPerStage - y$futilityPerStage,4)
	#
	#x <- getSimulationRates(design = getDesignGroupSequential(futilityBounds = c(-1,1), informationRates = informationRates,typeOfDesign = "P"), 
	#		thetaH0 = 0.8, groups = 2, riskRatio = TRUE, allocationRatioPlanned = 2, plannedSubjects = plannedSubjects, pi1 = seq(0.15,0.4,0.05), pi2 = 0.2, 
	#		conditionalPower = 0.8, minNumberOfSubjectsPerStage = plannedSubjects, maxNumberOfSubjectsPerStage = c(100,200,300), directionUpper = TRUE)
	#
	#y <- getPowerRates(design = getDesignGroupSequential(futilityBounds = c(-1,1), informationRates = informationRates,typeOfDesign = "P"), 
	#		thetaH0 = 0.8, groups = 2, riskRatio = TRUE, allocationRatioPlanned = 2, pi1 = seq(0.15,0.4,0.05), pi2 = 0.2, maxNumberOfSubjects = maxNumberOfSubjects, 
	#		directionUpper = TRUE)
	#
	#round((x$expectedNumberOfSubjects - y$expectedNumberOfSubjects)/maxNumberOfSubjects,4)
	#x$expectedNumberOfSubjects
	#y$expectedNumberOfSubjects
	#x$overallReject
	#round(x$overallReject - y$overallReject,4)
	#round(x$rejectPerStage - y$rejectPerStage,4)
	#round(x$futilityPerStage - y$futilityPerStage,4)
	#
	##############################################################################################################################
	#

	#x <- getSimulationSurvival(design = getDesignInverseNormal(typeOfDesign = "P", futilityBounds = c(0,0)), 
	#		pi1 = seq(0.2, 0.4, 0.05), maxNumberOfIterations = 10000, accrualTime = 24, plannedEvents = c(67,134,201), maxNumberOfSubjects = 396, allocation1 = 1, allocation2 = 1)
	#toc()
	#y <- getPowerSurvival(design = getDesignInverseNormal(typeOfDesign = "P", futilityBounds = c(0,0)), 
	#		pi1 = seq(0.2, 0.4, 0.05), maxNumberOfEvents = 201, accrualTime = 24, maxNumberOfSubjects = 396, allocationRatioPlanned = 1)
	#
	#round(x$expectedNumberOfEvents - y$expectedNumberOfEvents,1)
	#round(x$expectedNumberOfSubjects - y$expectedNumberOfSubjects,1)
	#round(x$numberOfSubjects - y$numberOfSubjects,1)
	#round(x$rejectPerStage - y$rejectPerStage,4)
	#round(x$overallReject - y$overallReject,4)
	#round(x$earlyStop - y$earlyStop,4)
	#round(x$futilityPerStage - y$futilityPerStage,4)
	#round(x$futilityStop - y$futilityStop,4)
	#round(x$analysisTime - y$analysisTime,4)
	#round(x$studyDuration - y$studyDuration,4)
	#x$conditionalPowerAchieved

})

