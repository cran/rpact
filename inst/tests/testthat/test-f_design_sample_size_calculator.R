######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 12 December 2019, 12:31:28                                                   #
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

context("Testing internal functions of the sample size calculator")


test_that("'.getLambdaStepFunctionByTime': return correct lambda for specified time and piecewise exponential bounds", {
	lambda1 <- .getLambdaStepFunctionByTime(time = 1, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda1' with expected results
	##
	expect_equal(lambda1, 0.025, tolerance = 1e-07)

	lambda2 <- .getLambdaStepFunctionByTime(time = 6, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda2' with expected results
	##
	expect_equal(lambda2, 0.025, tolerance = 1e-07)

	lambda3 <- .getLambdaStepFunctionByTime(time = 7, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda3' with expected results
	##
	expect_equal(lambda3, 0.04, tolerance = 1e-07)

	lambda4 <- .getLambdaStepFunctionByTime(time = 9, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda4' with expected results
	##
	expect_equal(lambda4, 0.04, tolerance = 1e-07)

	lambda5 <- .getLambdaStepFunctionByTime(time = 14, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda5' with expected results
	##
	expect_equal(lambda5, 0.015, tolerance = 1e-07)

	lambda6 <- .getLambdaStepFunctionByTime(time = 15, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda6' with expected results
	##
	expect_equal(lambda6, 0.015, tolerance = 1e-07)

	lambda7 <- .getLambdaStepFunctionByTime(time = 16, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda7' with expected results
	##
	expect_equal(lambda7, 0.01, tolerance = 1e-07)

	lambda8 <- .getLambdaStepFunctionByTime(time = 21, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda8' with expected results
	##
	expect_equal(lambda8, 0.01, tolerance = 1e-07)

	lambda9 <- .getLambdaStepFunctionByTime(time = 50, c(6, 9, 15, 21), c(0.025, 0.04, 0.015, 0.01, 0.007))

	##
	## Comparison of the results of numeric object 'lambda9' with expected results
	##
	expect_equal(lambda9, 0.007, tolerance = 1e-07)

})

context("Testing the sample size calculation of means for different designs and arguments")


test_that("'getSampleSizeMeans': sample size calculation of means for one sided group sequential design", {
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	designGS1pretest <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	##
	## Comparison of the results of TrialDesignGroupSequential object 'designGS1pretest' with expected results
	##
	expect_equal(designGS1pretest$alphaSpent, c(0.0020595603, 0.0098772988, 0.024999999), tolerance = 1e-07)
	expect_equal(designGS1pretest$criticalValues, c(2.8688923, 2.3885055, 2.0793148), tolerance = 1e-07)
	expect_equal(designGS1pretest$stageLevels, c(0.0020595603, 0.0084585282, 0.018794214), tolerance = 1e-07)

	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeOneMeanVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageOneMean}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 1, thetaH0 = 0.5, stDev = 2, normalApproximation = FALSE, alternative = 0.8)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 494.6455, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.929099, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.32275, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 494.6455, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 491.89699, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 462.87248, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 360.24062, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.090771, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.80583608, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.68748891, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeOneMeanVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageOneMean}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 1, thetaH0 = 0.5, stDev = 2, normalApproximation = TRUE, alternative = 0.8)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 492.61495, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 98.522991, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 246.30748, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 492.61495, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 489.87773, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 460.97237, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 358.76182, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.0780634, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.80438093, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.68736844, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 107.00299, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.501497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 53.501497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 21.400599, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 53.501497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 107.00299, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 106.40843, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 100.12977, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 77.928183, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8110917, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3500437, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81436669, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 104.93573, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 52.467865, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 52.467865, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 20.987146, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 52.467865, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 104.93573, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 104.35265, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 98.195298, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 76.422636, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.5049412, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.318984, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81192991, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 141.97133, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 106.4785, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 35.492832, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 28.394266, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 70.985664, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 141.97133, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 21.295699, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 53.239248, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 106.4785, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 7.0985664, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 17.746416, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 35.492832, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 141.18246, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 132.85195, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 103.39494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.7228801, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3419598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81376184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 139.91431, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 104.93573, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 34.978577, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 27.982861, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 69.957153, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 139.91431, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 20.987146, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 52.467865, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 104.93573, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 6.9957153, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 17.489288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 34.978577, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 139.13687, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 130.92706, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 101.89685, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.5049412, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.318984, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.81192991, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, normalApproximation = FALSE, alternative = 2.1, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 71.36231, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 35.681155, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 35.681155, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 14.272462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 35.681155, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 71.36231, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 70.965784, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 66.77843, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 51.971772, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.222748, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.1829515, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5038177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, normalApproximation = TRUE, alternative = 2.1, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 69.273978, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 34.636989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 34.636989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.854796, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 34.636989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 69.273978, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 68.889056, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 64.824239, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 50.450881, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5830046, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.123365, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.4992983, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, normalApproximation = FALSE, alternative = 2.1, allocationRatioPlanned = 0.4)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 86.937573, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.839307, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 62.098267, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 17.387515, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 43.468787, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 86.937573, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 4.9678613, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 12.419653, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 24.839307, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 12.419653, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 31.049133, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 62.098267, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 86.454503, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 81.353233, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 63.314931, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.0734522, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.1712593, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5029983, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, thetaH0 = 0.5, stDev = 2, normalApproximation = TRUE, alternative = 2.1, allocationRatioPlanned = 0.4)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 84.860623, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.245892, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 60.614731, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 16.972125, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 42.430311, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 84.860623, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 4.8491785, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 12.122946, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 24.245892, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 12.122946, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 30.307365, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 60.614731, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 84.389093, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 79.409693, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 61.802329, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5830046, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.123365, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.4992983, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, stDev = 3, normalApproximation = FALSE, alternative = 1.9, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 363.14949, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 181.57474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 181.57474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.629897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 181.57474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 363.14949, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 361.13164, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 339.82298, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 264.47466, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8861856, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9212807, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5251098, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 361.11139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 180.5557, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 180.5557, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.222278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 180.5557, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 361.11139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 359.10487, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 337.9158, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 262.99035, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8268779, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9146031, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5245615, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, stDev = 3, normalApproximation = FALSE, alternative = 1.9, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 458.2463, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 343.68473, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 114.56158, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 91.64926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 229.12315, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 458.2463, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.736945, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 171.84236, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 343.68473, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.912315, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 57.280788, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 114.56158, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 455.70005, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 428.81135, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 333.7318, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8732837, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9198713, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5249957, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 456.21071, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 342.15803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 114.05268, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 91.242142, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 228.10535, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 456.21071, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.431606, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 171.07902, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 342.15803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.810535, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 57.026339, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 114.05268, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 453.67577, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 426.90651, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 332.24932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8268779, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9146031, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5245615, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansRatioVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizeRatioMeansOptimumAllocationRatio}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS1, groups = 2, meanRatio = TRUE, thetaH0 = 0.9, stDev = 3, normalApproximation = TRUE, alternative = 1.9, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.1111111, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 360.11385, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 189.5336, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 170.58024, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 72.022769, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 180.05692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 360.11385, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 37.906721, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 94.766802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 189.5336, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 34.116049, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 85.290122, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 170.58024, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 358.11287, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 336.98233, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 262.26386, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 2.8268779, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9146031, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5245615, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

})

test_that("'getSampleSizeMeans': sample size calculation of means for two sided group sequential design", {

	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	designGS2pretest <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), alpha = 0.4, 
		sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	##
	## Comparison of the results of TrialDesignGroupSequential object 'designGS2pretest' with expected results
	##
	expect_equal(designGS2pretest$alphaSpent, c(0.12265406, 0.26238998, 0.4), tolerance = 1e-07)
	expect_equal(designGS2pretest$criticalValues, c(1.5437287, 1.2852363, 1.1188632), tolerance = 1e-07)
	expect_equal(designGS2pretest$stageLevels, c(0.06132703, 0.099354859, 0.13159925), tolerance = 1e-07)

	designGS2 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), alpha = 0.4, 
		sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeOneMeanVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageOneMean}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 1, thetaH0 = 0.5, stDev = 2, normalApproximation = FALSE, alternative = 0.8)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 234.92433, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 46.984866, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.46217, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 234.92433, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 195.45911, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 176.81177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 134.60888, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.041134725, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.26146972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.3536511, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.95886527, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.73853028, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.6463489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeOneMeanVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageOneMean}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 1, thetaH0 = 0.5, stDev = 2, normalApproximation = TRUE, alternative = 0.8)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 234.50706, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 46.901412, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.25353, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 234.50706, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 195.11194, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 176.49772, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 134.36979, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.049174965, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.26261678, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.35387349, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.95082503, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.73738322, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.64612651, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 50.39219, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 25.196095, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 25.196095, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 10.078438, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 25.196095, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 50.39219, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 41.926745, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 37.926818, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 28.874132, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -2.1720469, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0543228, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63787834, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.1720469, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0543228, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63787834, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 49.954167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 24.977083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 24.977083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 9.9908334, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 24.977083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 49.954167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 41.562306, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 37.597148, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 28.62315, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -1.9535752, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0286606, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63321489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.9535752, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0286606, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63321489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceUnknownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = FALSE, alternative = 1.3, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 67.037534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 50.27815, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 16.759383, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.407507, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 33.518767, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 67.037534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 10.05563, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 25.139075, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 50.27815, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 3.3518767, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 8.3796917, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 16.759383, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 55.775818, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 50.454651, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 38.411718, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -2.1030977, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0473776, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63668307, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.1030977, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0473776, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63668307, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoMeansDiffVarianceKnownOnesided}
	# @refFS[Formula]{fs:sampleSizePerStageTwoMeans}
	sampleSizeResult <- getSampleSizeMeans(designGS2, groups = 2, thetaH0 = 0, stDev = 2, normalApproximation = TRUE, alternative = 1.3, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanMeans object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 66.605556, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 49.954167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 16.651389, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 13.321111, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 33.302778, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 66.605556, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 9.9908334, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 24.977083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 49.954167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 3.3302778, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 8.3256945, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 16.651389, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 55.416408, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 50.12953, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 38.164199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -1.9535752, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -1.0286606, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.63321489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.9535752, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.0286606, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.63321489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

})

context("Testing the sample size calculation of rates for different designs and arguments")


test_that("'getSampleSizeRates': sample size calculation of rates for one sided group sequential design", {
	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeOneRateExactOnesidedLargerpi1}
	# @refFS[Formula]{fs:sampleSizePerStageOneRate}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = FALSE)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 29.536017, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.9072033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 14.768008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 29.536017, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 29.371899, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 27.638803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 21.510502, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.090192, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.81076728, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.6912997, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeOneRateExactOnesidedSmallerpi1}
	# @refFS[Formula]{fs:sampleSizePerStageOneRate}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 1, thetaH0 = 0.5, pi1 = 0.2, normalApproximation = FALSE)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 29.536017, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.9072033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 14.768008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 29.536017, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 29.371899, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 27.638803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 21.510502, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], -0.090191958, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.18923272, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.3087003, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeOneRateApproximation}
	# @refFS[Formula]{fs:sampleSizePerStageOneRate}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = TRUE)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 26.111979, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 5.2223957, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 13.055989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 26.111979, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 25.966887, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 24.434704, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 19.016842, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.127696, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.83051514, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.70345593, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeOneRateExactOnesidedSmallerpi1}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 261.60183, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 130.80091, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 130.80091, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 52.320365, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 130.80091, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 261.60183, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 260.14823, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 244.79812, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 190.51949, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.39662162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.20482715, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.12354802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 349.41307, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 262.0598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 87.353268, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 69.882614, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 174.70654, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 349.41307, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 52.411961, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 131.0299, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 262.0598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 17.470654, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 43.676634, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 87.353268, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 347.47155, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 326.9689, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 254.47069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.38949339, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.20784714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.12553463, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	# @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0.2, pi1 = 0.5, pi2 = 0.1, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 201.70565, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 100.85283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 100.85283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 40.341131, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 100.85283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 201.70565, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 200.58487, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 188.74931, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 146.89828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.6326463, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.40827798, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.32212934, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	# @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0.2, pi1 = 0.5, pi2 = 0.1, allocationRatioPlanned = 0.4)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 267.48868, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 76.425337, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 191.06334, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 53.497736, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 133.74434, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 267.48868, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 15.285067, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 38.212668, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 76.425337, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 38.212668, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 95.531671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 191.06334, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 266.00237, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 250.30683, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 194.80676, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.59822838, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.40051537, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.32119139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:EstimatesDiffFarringtonManning}
	# @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
	# @refFS[Formula]{fs:sampleSizeRatesDiffOptimumAllocationRatio}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, thetaH0 = 0.2, pi1 = 0.5, pi2 = 0.1, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.1669392, tolerance = 1e-07)
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 200.45189, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 107.94727, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 92.504622, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 40.090378, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 100.22594, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 200.45189, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 21.589453, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 53.973634, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 107.94727, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 18.500924, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 46.252311, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 92.504622, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 199.33807, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 187.57608, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 145.98518, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.63834776, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.41018483, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 0.32243267, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
	# @refFS[Formula]{fs:sampleSizeTwoRatesRatio}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, riskRatio = TRUE, thetaH0 = 0.9, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 171.20812, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 85.604059, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 85.604059, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 34.241624, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 85.604059, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 171.20812, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 170.2568, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 160.21075, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 124.68752, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.1899424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.0225352, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5569402, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
	# @refFS[Formula]{fs:sampleSizeTwoRatesRatio}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, riskRatio = TRUE, thetaH0 = 0.9, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 221.72371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 166.29278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 55.430927, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 44.344741, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 110.86185, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 221.72371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 33.258556, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 83.14639, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 166.29278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 11.086185, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 27.715463, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 55.430927, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 220.4917, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 207.48153, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 161.47703, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.1917697, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.0740853, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5843199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:EstimatesRatioFarringtonManning}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	# @refFS[Formula]{fs:sampleSizeTwoRatesRatioOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeRates(designGS1, groups = 2, riskRatio = TRUE, thetaH0 = 0.9, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.0304199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 171.17189, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 86.868201, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 84.303693, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 34.234379, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 85.585947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 171.17189, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 17.37364, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 43.434101, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 86.868201, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 16.860739, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 42.151846, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 84.303693, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 170.22077, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 160.17685, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 124.66114, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.1919838, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.0241846, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5576701, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

})

test_that("'getSampleSizeRates': sample size calculation of rates for two sided group sequential design", {

	designGS2 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), alpha = 0.4, 
		sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeOneRateApproximation}
	# @refFS[Formula]{fs:sampleSizePerStageOneRate}
	sampleSizeResult <- getSampleSizeRates(designGS2, groups = 1, thetaH0 = 0.5, pi1 = 0.8, normalApproximation = TRUE)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 11.331566, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 2.2663131, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 5.6657828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 11.331566, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 9.4279622, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 8.5285086, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 6.4928537, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -0.01272092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.23002532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.33381109, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.0127209, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.76997468, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.66618891, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS2, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 123.43553, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 61.717765, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 61.717765, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 24.687106, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 61.717765, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 123.43553, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 102.69945, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 92.901636, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 70.727105, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -0.23899172, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -0.13791313, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.087906186, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.30941892, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.15876644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.095938144, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeTwoRatesDiff}
	# @refFS[Formula]{fs:sampleSizePerStageTwoRates}
	sampleSizeResult <- getSampleSizeRates(designGS2, groups = 2, thetaH0 = 0, pi1 = 0.5, pi2 = 0.3, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanRates object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 162.30744, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 121.73058, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 40.576859, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 32.461488, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 81.153719, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 162.30744, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 24.346116, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 60.865289, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 121.73058, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 8.1153719, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 20.28843, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 40.576859, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH0, 135.04122, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH01, 122.15791, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 93.000251, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], -0.21587527, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], -0.13203224, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], -0.086052993, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 0.31213587, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 0.16272503, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 0.09811449, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

})

context("Testing the sample size calculation of survival data for different designs and arguments")


test_that("'getSampleSizeSurvival': sample size calculation of survival data for one sided group sequential design and typeOfComputation = 'Schoenfeld'", {
	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 218.43651, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 109.21825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 109.21825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 18.203042, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.423875, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 129.1955, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 209.26106, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 218.43651, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 207.20268, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.03082, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 187.52311, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 62.507704, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 20.835901, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.391854, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 146.60794, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 238.15931, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.03082, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 109.95596, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 178.61948, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 187.52311, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 36.651986, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 59.539826, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 62.507704, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 236.50497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 359.16189, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 179.58095, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 179.58095, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 29.930158, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.423875, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 212.42831, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 344.07526, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 359.16189, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 340.69079, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 411.1105, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 308.33287, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 102.77762, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 34.259208, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.391854, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 241.05854, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 391.59089, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 411.1105, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 180.79391, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 293.69317, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 308.33287, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 60.264635, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 97.897723, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 102.77762, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 388.87078, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 216.4138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 108.2069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 108.2069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 27.051725, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.872604, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 168.44491, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 216.4138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 216.4138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 212.39441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 247.45413, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 185.5906, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 61.863534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 30.931767, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.832344, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 190.83096, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 247.45413, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 247.45413, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 143.12322, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 185.5906, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 185.5906, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 47.70774, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 61.863534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 61.863534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 242.70959, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 355.83608, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 177.91804, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 177.91804, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 44.47951, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.872604, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 276.96374, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 355.83608, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 355.83608, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 349.22724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 406.87381, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 305.15536, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 101.71845, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 50.859227, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.832344, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 313.77176, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 406.87381, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 406.87381, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 235.32882, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 305.15536, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 305.15536, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 78.442941, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 101.71845, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 101.71845, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 399.07264, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 224.258, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 112.129, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 112.129, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 64.634742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 28.03225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.818027, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.926948, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.317371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 64.634742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 64.275598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 60.483, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 47.072216, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 172.34323, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 224.258, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 224.258, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 219.90797, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 257.43359, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 193.07519, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 64.358398, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 86.179656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 32.179199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.771337, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 17.235931, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 43.089828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 86.179656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 85.700797, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 80.644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 62.762955, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 195.71655, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 257.43359, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 257.43359, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 146.78741, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 193.07519, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 193.07519, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 48.929138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 64.358398, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 64.358398, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 252.26222, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.9325954, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3170793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6774418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 368.73381, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 184.36691, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 184.36691, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 106.27498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 46.091727, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.818027, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.254997, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 53.137492, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 106.27498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 105.68446, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 99.448526, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 77.397988, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 283.37351, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 368.73381, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 368.73381, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 361.58134, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 423.28243, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 317.46182, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 105.82061, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 141.69998, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 52.910303, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.771337, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 28.339996, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 70.849989, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 141.69998, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 140.91262, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 132.59803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 103.19732, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 321.80485, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 423.28243, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 423.28243, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 241.35364, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 317.46182, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 317.46182, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 80.451212, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 105.82061, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 105.82061, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 414.77946, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	# @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Schoenfeld", thetaH0 = 1.2, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 359.78876, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 206.97289, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 152.81587, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 108.73874, tolerance = 1e-07)
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 44.973595, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.802401, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 21.747749, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 54.369372, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 108.73874, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 108.13454, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 101.75403, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 79.192297, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 275.50245, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 359.78876, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 359.78876, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 158.48615, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 206.97289, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 206.97289, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 117.01629, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 152.81587, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 152.81587, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 352.72627, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.1656631, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.3109184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7962847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for one sided group sequential design and typeOfComputation = 'Freedman'", {

	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 243.82563, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 121.91282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 121.91282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 20.318803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.423875, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 144.21204, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 233.58371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 243.82563, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 231.28609, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 399.20253, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 299.4019, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 99.800633, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 137.59559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 33.266878, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.391854, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 27.519117, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 68.797794, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 137.59559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 136.83103, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 128.75728, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 100.20817, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 234.07619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 380.24832, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 399.20253, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 175.55715, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 285.18624, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 299.4019, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 58.519049, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 95.06208, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 99.800633, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 377.60699, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5359417, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9445403, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5058707, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 241.56782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.78391, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.78391, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 30.195978, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.872604, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 188.02345, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 241.56782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 241.56782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 237.08125, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 395.08857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 296.31643, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 98.772142, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 137.59559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 49.386071, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.832344, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 27.519117, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 68.797794, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 137.59559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 136.83103, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 128.75728, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 100.20817, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 304.68325, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 395.08857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 395.08857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 228.51244, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 296.31643, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 296.31643, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 76.170813, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 98.772142, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 98.772142, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 387.51336, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 3.5359417, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.9445403, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.5058707, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.32376, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.16188, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.16188, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 31.29047, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.818027, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 192.37488, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 250.32376, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.32376, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 245.46813, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	# @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 236.31869, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 89.479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 146.8394, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 62.770758, tolerance = 1e-07)
	expect_equal(sampleSizeResult$allocationRatioPlanned, 0.60936839, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 29.539836, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1891498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.272294, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.846839, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 12.554152, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 31.385379, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 62.770758, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 62.421971, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 58.738747, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 45.714713, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 182.82647, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 236.31869, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 236.31869, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 69.22509, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 89.479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 89.479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 113.60138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 146.8394, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 146.8394, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 231.83649, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 5.3084847, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.4084373, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.7178517, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for one sided group sequential design and typeOfComputation = 'HsiehFreedman'", {

	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 243.82563, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 121.91282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 121.91282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 20.318803, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.423875, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 144.21204, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 233.58371, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 243.82563, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 231.28609, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 279.09218, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 209.31914, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 69.773046, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 96.196416, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 23.257682, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 14.391854, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 19.239283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 48.098208, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 96.196416, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 95.661899, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 90.017344, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 70.057965, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 163.64835, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 265.84083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 279.09218, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 122.73626, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 199.38062, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 209.31914, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 40.912088, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 66.460208, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 69.773046, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 263.99422, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 241.56782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 120.78391, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 120.78391, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 30.195978, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.872604, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 188.02345, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 241.56782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 241.56782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 237.08125, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 276.21601, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 207.16201, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 69.054003, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 96.196416, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 34.527001, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.832344, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 19.239283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 48.098208, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 96.196416, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 95.661899, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 90.017344, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 70.057965, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 213.01146, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 276.21601, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 276.21601, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 159.75859, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 207.16201, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 207.16201, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 53.252865, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 69.054003, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 69.054003, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 270.92, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 250.32376, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 125.16188, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.16188, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 31.29047, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.818027, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.429462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.073656, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 72.147312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 71.746424, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 67.513008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 52.543474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 192.37488, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 250.32376, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 250.32376, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 245.46813, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	# @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeSurvival(designGS1, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 244.2512, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 140.50849, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 103.74271, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 73.819895, tolerance = 1e-07)
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 30.5314, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 13.802401, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 14.763979, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 36.909947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 73.819895, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 73.409713, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 69.078154, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 53.761583, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 187.03142, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 244.2512, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 244.2512, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 107.59211, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 140.50849, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 140.50849, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 79.439306, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 103.74271, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 103.74271, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 239.45666, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.08379162, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.40937259, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.40683579, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.49316421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 4.52897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 2.2152278, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], 1.6316611, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0020595603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0084585282, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.018794214, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'Schoenfeld'", {

	designGS2 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), alpha = 0.4, 
		sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 103.98569, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 51.992843, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 51.992843, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 8.6654738, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 12.356848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 61.502916, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 99.617756, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 103.98569, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 89.550349, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Schoenfeld", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 119.02601, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 89.269507, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 29.756502, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 41.025425, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 9.9188341, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 12.314673, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.205085, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 20.512713, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 41.025425, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 34.133514, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 30.877083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 23.507086, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 69.79203, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 113.37463, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 119.02601, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 52.344023, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 85.030974, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 89.269507, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 17.448008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 28.343658, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 29.756502, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 102.08444, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 103.02279, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 51.511393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 51.511393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 12.877848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.672467, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 80.187417, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 103.02279, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 103.02279, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 96.108996, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 117.79939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 88.349544, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 29.449848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 41.025425, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 14.724924, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.623913, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 8.205085, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 20.512713, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 41.025425, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 34.133514, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 30.877083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 23.507086, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 90.844192, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 117.79939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 117.79939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 68.133144, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 88.349544, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 88.349544, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 22.711048, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 29.449848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 29.449848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 109.63825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 106.75698, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 53.378489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 53.378489, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 30.769069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 13.344622, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.606421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.1538138, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.384534, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 30.769069, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 25.600136, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 23.157812, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 17.630314, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 82.043195, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 106.75698, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 106.75698, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 99.274467, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalSchoenfeld}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	# @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 104.16718, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 59.923444, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 44.243734, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 31.482385, tolerance = 1e-07)
	expect_equal(sampleSizeResult$allocationRatioPlanned, 1.3543939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 13.020897, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1258711, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.176695, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.587598, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.2964769, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 15.741192, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 31.482385, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 26.193621, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 23.694677, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 18.039036, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 79.764338, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 104.16718, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 104.16718, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 45.885412, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 59.923444, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 59.923444, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 33.878926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 44.243734, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 44.243734, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 96.778811, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.28805692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.51926225, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.66803619, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.4715361, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9258092, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4969249, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'Freedman'", {

	designGS2 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), alpha = 0.4, 
		sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 116.07206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 58.03603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 58.03603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 9.6726717, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 12.356848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.8690786, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 17.172696, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 28.575669, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 25.849471, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 19.679506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 68.65147, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 111.19644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 116.07206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 99.958888, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Freedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 190.03851, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 142.52888, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 47.509628, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 65.50174, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 15.836543, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 12.314673, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 13.100348, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.75087, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 65.50174, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 54.498024, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 49.298762, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 37.531726, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 111.43089, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 181.01545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 190.03851, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 83.573164, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 135.76159, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 142.52888, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 27.857721, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 45.253862, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 47.509628, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 162.98937, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.37344541, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.59532615, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.72668369, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.6777676, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.6797515, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.3761146, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 114.99724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 57.49862, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 57.49862, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 14.374655, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.672467, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.8690786, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 17.172696, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 28.575669, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 25.849471, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 19.679506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 89.507692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 114.99724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 114.99724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 107.27985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 188.08008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 141.06006, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 47.02002, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 65.50174, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 23.51001, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.623913, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 13.100348, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 32.75087, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 65.50174, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 54.498024, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 49.298762, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 37.531726, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 145.04305, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 188.08008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 188.08008, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 108.78229, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 141.06006, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 141.06006, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 36.260762, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 47.02002, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 47.02002, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 175.0499, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.37344541, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.59532615, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.72668369, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 2.6777676, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.6797515, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.3761146, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 119.16546, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 59.582732, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 59.582732, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 14.895683, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1480342, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.21033, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.606421, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.8690786, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 17.172696, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 28.575669, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 25.849471, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 19.679506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 91.579168, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 119.16546, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 119.16546, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 110.81325, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedman}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	# @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "Freedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 0)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 112.49841, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 42.596199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 69.902213, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 29.881728, tolerance = 1e-07)
	expect_equal(sampleSizeResult$allocationRatioPlanned, 0.60936839, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 14.062302, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1891498, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.272294, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.641184, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 5.9763456, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 14.940864, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 29.881728, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 24.86186, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 22.48997, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 17.121878, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 87.033692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 112.49841, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 112.49841, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 32.954283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 42.596199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 42.596199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 54.079409, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 69.902213, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 69.902213, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 104.78854, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.2720218, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.50383572, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.65574857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.676176, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.9847739, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.5249747, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': sample size calculation of survival data for two sided group sequential design and typeOfComputation = 'HsiehFreedman'", {

	designGS2 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), alpha = 0.4, 
		sided = 2, beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 116.07206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 58.03603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 58.03603, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 9.6726717, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0974672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.495939, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 12.356848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.8690786, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 17.172696, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 28.575669, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 25.849471, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 19.679506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 68.65147, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 111.19644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 116.07206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 99.958888, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityNotAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", pi1 = 0.4, pi2 = 0.2, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 16.282985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 132.86054, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 99.645403, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 33.215134, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 45.793857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 11.071711, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 7.0363139, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 11.430238, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 12.314673, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 9.1587714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 22.896929, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 45.793857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 38.100892, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 34.465962, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 26.239341, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 77.904037, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 126.55229, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 132.86054, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 58.428028, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 94.914221, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 99.645403, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 19.476009, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 31.638074, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 33.215134, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 113.94983, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 114.99724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 57.49862, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 57.49862, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 14.374655, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.327532, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.672467, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 6.8690786, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 17.172696, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 34.345393, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 28.575669, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 25.849471, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 19.679506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 89.507692, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 114.99724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 114.99724, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 107.27985, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 131.49135, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 98.618512, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 32.872837, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 45.793857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 16.436419, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.1694167, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.240925, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.623913, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 9.1587714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 22.896929, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 45.793857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 38.100892, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 34.465962, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 26.239341, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 101.40312, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 131.49135, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 131.49135, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 76.052337, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 98.618512, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 98.618512, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 25.350779, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 32.872837, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 32.872837, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 122.38163, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = c(0.3,0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(503.85206, 136.7942), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(377.88904, 102.59565), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(125.96301, 34.19855), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(132.64612, 45.793857), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, c(62.981507, 17.099275), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(6.2267383, 6.0820828), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(10.326085, 10.109775), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], c(18, 18))
	expect_equal(sampleSizeResult$studyDurationH1, c(11.671936, 11.550242), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18))
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(26.529224, 9.1587714), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(66.32306, 22.896929), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], c(132.64612, 45.793857), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(110.36274, 38.100892), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(99.833829, 34.465962), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(76.004665, 26.239341), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(392.16937, 103.99921), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(503.85206, 136.7942), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(503.85206, 136.7942), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(294.12702, 77.999405), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(377.88904, 102.59565), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(377.88904, 102.59565), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(98.042341, 25.999802), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(125.96301, 34.19855), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(125.96301, 34.19855), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(470.03826, 126.86497), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.50049257, 0.30788852), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.6945715, 0.53778926), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.79903418, 0.68260947), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(1.9980317, 3.2479288), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4397366, 1.8594644), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2515109, 1.4649665), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.3, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 27.207015, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.025476782, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 1.5984103, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 503.85206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 377.88904, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 125.96301, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 132.64612, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 62.981507, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.2267383, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.326085, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.671936, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 26.529224, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 66.32306, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 132.64612, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 110.36274, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 99.833829, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 76.004665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 392.16937, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 503.85206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 503.85206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 294.12702, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 377.88904, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 377.88904, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 98.042341, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 125.96301, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 125.96301, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 470.03826, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.50049257, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.6945715, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.79903418, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 1.9980317, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.4397366, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.2515109, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, eventTime = 14, accrualTime = 8, followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$median1, 18.996816, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.036487545, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 2.2892242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 136.7942, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 102.59565, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 34.19855, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 45.793857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 17.099275, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 6.0820828, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 10.109775, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 11.550242, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 9.1587714, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 22.896929, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], 45.793857, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 38.100892, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 34.465962, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 26.239341, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 103.99921, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 136.7942, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], 136.7942, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], 77.999405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], 102.59565, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], 102.59565, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], 25.999802, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], 34.19855, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], 34.19855, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 126.86497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], 0.30788852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], 0.53778926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], 0.68260947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], 3.2479288, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], 1.8594644, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], 1.4649665, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	sampleSizeResult <- getSampleSizeSurvival(designGS2, maxNumberOfSubjects = 0, 
		typeOfComputation = "HsiehFreedman", thetaH0 = 1, 
		pi1 = c(0.3,0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8, 
		followUpTime = 10, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime = 16, 
		accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(503.85206, 136.7942), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(377.88904, 102.59565), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(125.96301, 34.19855), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(132.64612, 45.793857), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, c(62.981507, 17.099275), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(6.2267383, 6.0820828), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(10.326085, 10.109775), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], c(18, 18))
	expect_equal(sampleSizeResult$studyDurationH1, c(11.671936, 11.550242), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18))
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(26.529224, 9.1587714), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(66.32306, 22.896929), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], c(132.64612, 45.793857), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(110.36274, 38.100892), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(99.833829, 34.465962), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(76.004665, 26.239341), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(392.16937, 103.99921), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(503.85206, 136.7942), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(503.85206, 136.7942), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(294.12702, 77.999405), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(377.88904, 102.59565), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(377.88904, 102.59565), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(98.042341, 25.999802), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(125.96301, 34.19855), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(125.96301, 34.19855), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(470.03826, 126.86497), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.50049257, 0.30788852), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.6945715, 0.53778926), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.79903418, 0.68260947), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(1.9980317, 3.2479288), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4397366, 1.8594644), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2515109, 1.4649665), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

	# @refFS[Formula]{fs:sampleSizeSurvivalFreedmanHsieh}
	# @refFS[Formula]{fs:sampleSizeSurvivalPatientNumber}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventProbabilityAcccountForOberservationTimes}
	# @refFS[Formula]{fs:sampleSizeSurvivalEventsPerStage}
	# @refFS[Formula]{fs:sampleSizeSurvivalExpectedTimePoints}
	# @refFS[Formula]{fs:sampleSizeSurvivalOptimumAllocationRatio}
	sampleSizeResult <- getSampleSizeSurvival(maxNumberOfSubjects = 194, designGS2, typeOfComputation = "HsiehFreedman", thetaH0 = 1, pi1 = c(0.3,0.4), pi2 = 0.2, eventTime = 14, accrualTime = 8, dropoutRate1 = 0.1, dropoutRate2 = 0.05, dropoutTime =16, accountForObservationTimes = TRUE, allocationRatioPlanned = 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(27.207015, 18.996816), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 43.487972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.025476782, 0.036487545), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.015938825, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(1.5984103, 2.2892242), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(145.5, 145.5), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(48.5, 48.5), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(132.64612, 45.793857), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 24.25, tolerance = 1e-07)
	expect_equal(sampleSizeResult$followUpTime, c(65.207473, 5.0567417), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.2, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(10.593965, 5.0743995), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(23.577694, 8.185751), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], c(73.207473, 13.056742), tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDurationH1, c(36.377625, 8.8858243), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(73.207473, 13.056742), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(26.529224, 9.1587714), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(66.32306, 22.896929), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], c(132.64612, 45.793857), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(110.36274, 38.100892), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(99.833829, 34.465962), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(76.004665, 26.239341), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(194, 123.05419), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(194, 194))
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(194, 194))
	expect_equal(sampleSizeResult$numberOfSubjects1[1, ], c(145.5, 92.290642), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[2, ], c(145.5, 145.5), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects1[3, ], c(145.5, 145.5), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[1, ], c(48.5, 30.763547), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[2, ], c(48.5, 48.5), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects2[3, ], c(48.5, 48.5), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(194, 172.51997), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.30276671, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.3601177, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.23711559, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.66288441, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[1, ], c(0.50049257, 0.30788852), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[2, ], c(0.6945715, 0.53778926), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleLower[3, ], c(0.79903418, 0.68260947), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[1, ], c(1.9980317, 3.2479288), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[2, ], c(1.4397366, 1.8594644), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScaleUpper[3, ], c(1.2515109, 1.4649665), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.12265406, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.19870972, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.26319851, tolerance = 1e-07)

})

context("Testing the sample size calculation of survival data for other parameter variants")


test_that("'getSampleSizeSurvival': Fixed sample size with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default, only alpha = 0.01 is specified  ", {
	sampleSizeResult <- getSampleSizeSurvival(alpha = 0.01)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(58.52451, 31.248898, 20.120262), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$nFixed, c(197.78666, 90.804254, 51.314209), tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed1, c(98.893329, 45.402127, 25.657105), tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed2, c(98.893329, 45.402127, 25.657105), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[1, ], 18)
	expect_equal(sampleSizeResult$studyDuration, 18)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.8370942, 2.2986321, 2.821477), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.01, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Four stage O'Brien and Fleming group sequential design with minimum required definitions, pi1 = c(0.4, 0.5, 0.6) and pi2 = 0.2 at event time 12, accrual time 12 and follow-up time 6 as default", {

	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 4))

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(158.37172, 72.708775, 41.088309), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(79.185858, 36.354387, 20.544155), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(79.185858, 36.354387, 20.544155), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(46.861741, 25.021615, 16.110694), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, c(13.197643, 6.0590646, 3.4240258), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.25, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[3, ], 0.75, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[4, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(7.9739331, 7.8101434, 7.6105076), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(11.495939, 11.330412, 11.125901), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[3, ], c(14.574435, 14.425585, 14.235444), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[4, ], c(18, 18, 18))
	expect_equal(sampleSizeResult$studyDurationH1, c(15.491732, 15.406299, 15.298535), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18))
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(11.715435, 6.2554038, 4.0276736), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(23.43087, 12.510808, 8.0553472), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[3, ], c(35.146306, 18.766211, 12.083021), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[4, ], c(46.861741, 25.021615, 16.110694), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(46.71422, 24.942847, 16.059978), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(44.876904, 23.961821, 15.428323), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(38.052731, 20.318084, 13.082227), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(105.23712, 47.322163, 26.058574), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(151.7193, 68.651695, 38.095372), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[3, ], c(158.37172, 72.708775, 41.088309), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[4, ], c(158.37172, 72.708775, 41.088309), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(156.87296, 71.824595, 40.451776), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.0042542622, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.19131467, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[3, ], 0.35652274, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[4, ], 0.24790832, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.55209168, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(10.651203, 25.469293, 56.523607), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(3.2636181, 5.0467111, 7.5182183), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[3, ], c(2.2002206, 2.9422007, 3.8377495), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[4, ], c(1.8065487, 2.2464886, 2.741937), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 2.5763449e-05, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.0020996694, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[3, ], 0.0097077663, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[4, ], 0.021469878, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': For fixed sample design, determine necessary accrual time if 200 subjects and 30 subjects per time unit can be recruited ", {

	sampleSizeResult <- getSampleSizeSurvival(accrualTime = c(0), accrualIntensity = c(30), 
		maxNumberOfSubjects = 200)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualTime, 6.6666667, tolerance = 1e-07)
	expect_equal(sampleSizeResult$followUpTime, c(5.4039758, 0.22825781, -1.7164516), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$nFixed, c(200, 200, 200))
	expect_equal(sampleSizeResult$nFixed1, c(100, 100, 100))
	expect_equal(sampleSizeResult$nFixed2, c(100, 100, 100))
	expect_equal(sampleSizeResult$analysisTime[1, ], c(12.070642, 6.8949245, 4.950215), tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDuration, c(12.070642, 6.8949245, 4.950215), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Determine necessary accrual time if 200 subjects and if the first 6 time units 20 subjects per time unit can be recruited, then 30 subjects per time unit", {

	sampleSizeResult <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(20, 30), 
		maxNumberOfSubjects = 200)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualTime, c(6, 8.6666667), tolerance = 1e-07)
	expect_equal(sampleSizeResult$totalAccrualTime, 8.6666667, tolerance = 1e-07)
	expect_equal(sampleSizeResult$followUpTime, c(4.8516734, -0.31523272, -2.5326655), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$nFixed, c(200, 200, 200))
	expect_equal(sampleSizeResult$nFixed1, c(100, 100, 100))
	expect_equal(sampleSizeResult$nFixed2, c(100, 100, 100))
	expect_equal(sampleSizeResult$analysisTime[1, ], c(13.51834, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDuration, c(13.51834, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Determine maximum number of Subjects if the first 6 time units 20 subjects per time unit can be recruited, and after 10 time units 30 subjects per time unit", {

	sampleSizeResult <- getSampleSizeSurvival(accrualTime = c(0, 6, 10), accrualIntensity = c(20, 30))

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(240, 240, 240))
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07)
	expect_equal(sampleSizeResult$totalAccrualTime, 10)
	expect_equal(sampleSizeResult$followUpTime, c(2.6783764, -1.6485661, -3.8659989), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$nFixed, c(240, 240, 240))
	expect_equal(sampleSizeResult$nFixed1, c(120, 120, 120))
	expect_equal(sampleSizeResult$nFixed2, c(120, 120, 120))
	expect_equal(sampleSizeResult$analysisTime[1, ], c(12.678376, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDuration, c(12.678376, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specify accrual time as a list", {

	at <- list("0 - <6"  = 20, "6 - Inf" = 30)
	sampleSizeResult <- getSampleSizeSurvival(accrualTime = at, maxNumberOfSubjects = 200)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualTime, c(6, 8.6666667), tolerance = 1e-07)
	expect_equal(sampleSizeResult$totalAccrualTime, 8.6666667, tolerance = 1e-07)
	expect_equal(sampleSizeResult$followUpTime, c(4.8516734, -0.31523272, -2.5326655), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$nFixed, c(200, 200, 200))
	expect_equal(sampleSizeResult$nFixed1, c(100, 100, 100))
	expect_equal(sampleSizeResult$nFixed2, c(100, 100, 100))
	expect_equal(sampleSizeResult$analysisTime[1, ], c(13.51834, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDuration, c(13.51834, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specify accrual time as a list, if maximum number of subjects need to be calculated", {

	at <- list("0 - <6"   = 20, "6 - <=10" = 30)
	sampleSizeResult <- getSampleSizeSurvival(accrualTime = at)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$median1, c(16.282985, 12, 9.0776496), tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 37.275405, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, c(0.042568802, 0.057762265, 0.076357561), tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, c(2.2892242, 3.1062837, 4.1062837), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(240, 240, 240))
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(45.770282, 24.438835, 15.735459), tolerance = 1e-07)
	expect_equal(sampleSizeResult$totalAccrualTime, 10)
	expect_equal(sampleSizeResult$followUpTime, c(2.6783764, -1.6485661, -3.8659989), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$nFixed, c(240, 240, 240))
	expect_equal(sampleSizeResult$nFixed1, c(120, 120, 120))
	expect_equal(sampleSizeResult$nFixed2, c(120, 120, 120))
	expect_equal(sampleSizeResult$analysisTime[1, ], c(12.678376, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDuration, c(12.678376, 8.3514339, 6.1340011), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.7849857, 2.2098739, 2.686355), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specify effect size for a two-stage group design with O'Brien & Fleming boundaries Effect size is based on event rates at specified event time needs to be specified because it should be shown that hazard ratio < 1", {

	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), pi1 = 0.2, pi2 = 0.3, eventTime = 24)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$median1, 74.550809, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.009297648, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 0.62562161, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 1076.0672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 538.03358, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 538.03358, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 143.8377, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 89.672263, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 11.811468, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 16.702852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 71.918848, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 143.8377, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 143.65194, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 141.26582, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 128.76314, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 1059.161, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 1076.0672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 1072.5235, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.51710185, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.71909794, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Effect size is based on event rate at specified event time for the reference group and hazard ratio ", {

	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, pi2 = 0.3, eventTime = 24)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$pi1, 0.16333997, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median1, 93.281194, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 46.640597, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.007430728, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda2, 0.014861456, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 532.72433, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 266.36217, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 266.36217, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 65.854457, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 44.393694, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 11.816947, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 16.704001, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 32.927229, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 65.854457, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 65.76941, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 64.676952, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 58.952743, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 524.59793, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 532.72433, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 531.021, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.37730742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.61425355, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Effect size is based on hazard rate for the reference group and hazard ratio", {

	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), hazardRatio = 0.5, lambda2 = 0.02) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult$lambda1, 0.01, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 406.47112, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 203.23556, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 203.23556, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 65.854457, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 33.872594, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 11.754955, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 16.691007, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 32.927229, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 65.854457, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 65.76941, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 64.676952, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 58.952743, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 398.17083, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 406.47112, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 404.73134, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.37730742, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 0.61425355, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time and hazard ratios  ", {

	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), hazardRatio = c(1.5, 1.8, 2))

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18))
	expect_equal(sampleSizeResult$studyDurationH1, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18))
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time as a list and hazard ratios ", {

	pws <- list("0 - <5"  = 0.01, "5 - <10" = 0.02, ">=10"    = 0.04)
	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2))

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18))
	expect_equal(sampleSizeResult$studyDurationH1, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18))
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time for both treatment arms", {

	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), piecewiseSurvivalTime = c(0, 5, 10), lambda2 = c(0.01, 0.02, 0.04), lambda1 = c(0.015, 0.03, 0.06))

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, TRUE)
	expect_equal(sampleSizeResult$lambda1, c(0.015, 0.03, 0.06), tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 1.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 762.70199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, 381.35099, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, 381.35099, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 192.45497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, 63.558499, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], 13.350554, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], 18)
	expect_equal(sampleSizeResult$studyDurationH1, 17.025453, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, 18)
	expect_equal(sampleSizeResult$eventsPerStage[1, ], 96.227483, tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], 192.45497, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, 192.20642, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, 189.01379, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, 172.2852, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], 762.70199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], 762.70199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, 762.70199, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 1.76855, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], 1.3298684, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specification of piecewise exponential survival time as a list", {

	pws <- list("0 - <5" = 0.01, "5 - <10" = 0.02, ">=10" = 0.04)
	sampleSizeResult <- getSampleSizeSurvival(design = getDesignGroupSequential(kMax = 2), piecewiseSurvivalTime = pws, hazardRatio = c(1.5, 1.8, 2))

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, c(TRUE, TRUE, TRUE))
	expect_equal(sampleSizeResult$maxNumberOfSubjects, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects1, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects2, c(381.35099, 166.59922, 113.99774), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, c(192.45497, 91.579156, 65.854457), tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualIntensity, c(63.558499, 27.766537, 18.999624), tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$informationRates[1, ], 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$informationRates[2, ], 1)
	expect_equal(sampleSizeResult$analysisTime[1, ], c(13.350554, 13.286013, 13.241069), tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[2, ], c(18, 18, 18))
	expect_equal(sampleSizeResult$studyDurationH1, c(17.025453, 17.011925, 17.002504), tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxStudyDuration, c(18, 18, 18))
	expect_equal(sampleSizeResult$eventsPerStage[1, ], c(96.227483, 45.789578, 32.927229), tolerance = 1e-07)
	expect_equal(sampleSizeResult$eventsPerStage[2, ], c(192.45497, 91.579156, 65.854457), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH0, c(192.20642, 91.460887, 65.76941), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH01, c(189.01379, 89.941683, 64.676952), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedEventsH1, c(172.2852, 81.981429, 58.952743), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[1, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$numberOfSubjects[2, ], c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$expectedNumberOfSubjectsH1, c(762.70199, 333.19844, 227.99548), tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[1, ], 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$rejectPerStage[2, ], 0.59039494, tolerance = 1e-07)
	expect_equal(sampleSizeResult$earlyStop, 0.20960506, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], c(1.76855, 2.2853938, 2.6503587), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[2, ], c(1.3298684, 1.5117519, 1.6279922), tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.0025828932, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[2, ], 0.023996469, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specify effect size based on median survival times (median1 = 5, median2 = 3)", {

	sampleSizeResult <- getSampleSizeSurvival(lambda1 = log(2) / 5, lambda2 = log(2) / 3)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$pi1, 0.81053543, tolerance = 1e-07)
	expect_equal(sampleSizeResult$pi2, 0.9375, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median1, 5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 3)
	expect_equal(sampleSizeResult$hazardRatio, 0.6, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 120.3157, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$nFixed, 141.26641, tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed1, 70.633206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed2, 70.633206, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[1, ], 18)
	expect_equal(sampleSizeResult$studyDuration, 18)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Specify effect size based on median survival times of Weibull distribtion with kappa = 2 (median1 = 5, median2 = 3)", {

	sampleSizeResult <- getSampleSizeSurvival(
		lambda1 = getLambdaByMedian(median = 5, kappa = 2), 
		lambda2 = getLambdaByMedian(median = 3, kappa = 2), kappa = 2)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$pi1, 0.98154699, tolerance = 1e-07)
	expect_equal(sampleSizeResult$pi2, 0.99998474, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median1, 5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 3)
	expect_equal(sampleSizeResult$hazardRatio, 0.36, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 30.078926, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, FALSE)
	expect_equal(sampleSizeResult$nFixed, 31.248566, tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed1, 15.624283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed2, 15.624283, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[1, ], 18)
	expect_equal(sampleSizeResult$studyDuration, 18)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.48932026, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Calculation of maximum number of subjects for given follow-up time", {

	sampleSizeResult <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 5)


	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult' with expected results
	##
	expect_equal(sampleSizeResult$directionUpper, FALSE)
	expect_equal(sampleSizeResult$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfSubjects, 477.30924, tolerance = 1e-07)
	expect_equal(sampleSizeResult$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult$accrualTime, c(6, 12.515269), tolerance = 1e-07)
	expect_equal(sampleSizeResult$totalAccrualTime, 12.515269, tolerance = 1e-07)
	expect_equal(sampleSizeResult$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult$nFixed, 477.30924, tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed1, 238.65462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$nFixed2, 238.65462, tolerance = 1e-07)
	expect_equal(sampleSizeResult$analysisTime[1, ], 17.515269, tolerance = 1e-07)
	expect_equal(sampleSizeResult$studyDuration, 17.515269, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)
	sampleSizeResult2 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = -1)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
	##
	expect_equal(sampleSizeResult2$directionUpper, FALSE)
	expect_equal(sampleSizeResult2$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$maxNumberOfSubjects, 741.77932, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$accrualTime, c(6, 17.50527), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$totalAccrualTime, 17.50527, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult2$nFixed, 741.77932, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$nFixed1, 370.88966, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$nFixed2, 370.88966, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$analysisTime[1, ], 16.50527, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$studyDuration, 16.50527, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	.skipTestifDisabled()

	sampleSizeResult3 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 200)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult3' with expected results
	##
	expect_equal(sampleSizeResult3$directionUpper, FALSE)
	expect_equal(sampleSizeResult3$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$maxNumberOfSubjects, 70.679258, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$accrualTime, 3.2126936, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult3$nFixed, 70.679258, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$nFixed1, 35.339629, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$nFixed2, 35.339629, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$analysisTime[1, ], 203.2127, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$studyDuration, 203.2127, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult4 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = -200)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult4' with expected results
	##
	expect_equal(sampleSizeResult4$directionUpper, FALSE)
	expect_equal(sampleSizeResult4$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$maxNumberOfSubjects, 11288.779, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$accrualTime, c(6, 216.50527), tolerance = 1e-07)
	expect_equal(sampleSizeResult4$totalAccrualTime, 216.50527, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult4$nFixed, 11288.779, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$nFixed1, 5644.3897, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$nFixed2, 5644.3897, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$analysisTime[1, ], 16.50527, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$studyDuration, 16.50527, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult5 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 44.43107095)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult5' with expected results
	##
	expect_equal(sampleSizeResult5$directionUpper, FALSE)
	expect_equal(sampleSizeResult5$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$maxNumberOfSubjects, 131.99999, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$accrualTime, 5.9999996, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult5$nFixed, 131.99999, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$nFixed1, 65.999995, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$nFixed2, 65.999995, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$analysisTime[1, ], 50.43107, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$studyDuration, 50.43107, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult5$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult6 <- getSampleSizeSurvival(accrualTime = c(0, 60), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), maxNumberOfSubjects = 500000)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult6' with expected results
	##
	expect_equal(sampleSizeResult6$directionUpper, FALSE)
	expect_equal(sampleSizeResult6$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$accrualTime, c(60, 9469.0566), tolerance = 1e-07)
	expect_equal(sampleSizeResult6$totalAccrualTime, 9469.0566, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$followUpTime, -9448.0008, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult6$nFixed, 5e+05)
	expect_equal(sampleSizeResult6$nFixed1, 250000)
	expect_equal(sampleSizeResult6$nFixed2, 250000)
	expect_equal(sampleSizeResult6$analysisTime[1, ], 21.055818, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$studyDuration, 21.055818, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult6$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult7 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 44)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult7' with expected results
	##
	expect_equal(sampleSizeResult7$directionUpper, FALSE)
	expect_equal(sampleSizeResult7$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$maxNumberOfSubjects, 132.8172, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$accrualTime, c(6, 6.0154188), tolerance = 1e-07)
	expect_equal(sampleSizeResult7$totalAccrualTime, 6.0154188, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult7$nFixed, 132.8172, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$nFixed1, 66.408599, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$nFixed2, 66.408599, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$analysisTime[1, ], 50.015396, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$studyDuration, 50.015396, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult7$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult8 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, lambda1 = c(0.01), followUpTime = 45)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult8' with expected results
	##
	expect_equal(sampleSizeResult8$directionUpper, FALSE)
	expect_equal(sampleSizeResult8$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$hazardRatio, 0.5, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$maxNumberOfSubjects, 130.99398, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$maxNumberOfEvents, 65.345659, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$accrualTime, 5.9542719, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult8$nFixed, 130.99398, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$nFixed1, 65.496991, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$nFixed2, 65.496991, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$analysisTime[1, ], 50.954287, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$studyDuration, 50.954287, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$criticalValuesEffectScale[1, ], 0.61574672, tolerance = 1e-07)
	expect_equal(sampleSizeResult8$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

test_that("'getSampleSizeSurvival': Calculation of median1 and median2", {

	sampleSizeResult <- getSampleSizeSurvival(lambda1 = log(2) / 3, lambda2 = log(2) / 5)
	expect_equal(sampleSizeResult$median1, 3)
	expect_equal(sampleSizeResult$median2, 5)

	kappa <- 2
	sampleSizeResult2 <- getSampleSizeSurvival(lambda1 = log(2)^(1 / kappa) / 3, 
		lambda2 = log(2)^(1 / kappa) / 5, kappa = kappa)
	expect_equal(sampleSizeResult2$median1, 3)
	expect_equal(sampleSizeResult2$median2, 5)

	sampleSizeResult1 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		median2 = 4, median1 = c(5), followUpTime = 5)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult1' with expected results
	##
	expect_equal(sampleSizeResult1$directionUpper, FALSE)
	expect_equal(sampleSizeResult1$pi1, 0.81053543, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$pi2, 0.875, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$lambda1, 0.13862944, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$lambda2, 0.1732868, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$maxNumberOfSubjects, 770.8069, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$maxNumberOfEvents, 630.52017, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$accrualTime, c(6, 18.05296), tolerance = 1e-07)
	expect_equal(sampleSizeResult1$totalAccrualTime, 18.05296, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult1$nFixed, 770.8069, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$nFixed1, 385.40345, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$nFixed2, 385.40345, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$analysisTime[1, ], 23.052959, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$studyDuration, 23.052959, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$criticalValuesEffectScale[1, ], 0.85546574, tolerance = 1e-07)
	expect_equal(sampleSizeResult1$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult2 <- getSampleSizeSurvival(median2 = 25, lambda1 = c(0.021, 0.023), maxNumberOfSubjects = 2280)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult2' with expected results
	##
	expect_equal(sampleSizeResult2$directionUpper, c(FALSE, FALSE))
	expect_equal(sampleSizeResult2$pi1, c(0.22275526, 0.24118707), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$pi2, 0.28302238, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$median1, c(33.007009, 30.136834), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$lambda2, 0.027725887, tolerance = 1e-07)
	expect_equal(sampleSizeResult2$hazardRatio, c(0.7574149, 0.82954965), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$maxNumberOfEvents, c(406.69171, 899.03732), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$accrualIntensity, 190)
	expect_equal(sampleSizeResult2$followUpTime, c(2.2277357, 13.964693), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult2$nFixed, c(2280, 2280))
	expect_equal(sampleSizeResult2$nFixed1, c(1140, 1140))
	expect_equal(sampleSizeResult2$nFixed2, c(1140, 1140))
	expect_equal(sampleSizeResult2$analysisTime[1, ], c(14.227736, 25.964693), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$studyDuration, c(14.227736, 25.964693), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$criticalValuesEffectScale[1, ], c(0.82334724, 0.87745097), tolerance = 1e-07)
	expect_equal(sampleSizeResult2$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult3 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		median2 = 50, lambda1 = 0.01, followUpTime = 5)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult3' with expected results
	##
	expect_equal(sampleSizeResult3$directionUpper, FALSE)
	expect_equal(sampleSizeResult3$pi1, 0.11307956, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$pi2, 0.15325469, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$median1, 69.314718, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$lambda2, 0.013862944, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$hazardRatio, 0.72134752, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$maxNumberOfSubjects, 1477.2065, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$maxNumberOfEvents, 294.26878, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$accrualTime, c(6, 31.381254), tolerance = 1e-07)
	expect_equal(sampleSizeResult3$totalAccrualTime, 31.381254, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult3$nFixed, 1477.2065, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$nFixed1, 738.60324, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$nFixed2, 738.60324, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$analysisTime[1, ], 36.381254, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$studyDuration, 36.381254, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$criticalValuesEffectScale[1, ], 0.79571801, tolerance = 1e-07)
	expect_equal(sampleSizeResult3$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeResult4 <- getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
		lambda2 = 0.02, median1 = 32, followUpTime = 5)

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeResult4' with expected results
	##
	expect_equal(sampleSizeResult4$directionUpper, TRUE)
	expect_equal(sampleSizeResult4$pi1, 0.22889459, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$pi2, 0.21337214, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$median2, 34.657359, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$lambda1, 0.021660849, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$hazardRatio, 1.0830425, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$maxNumberOfSubjects, 7086.5152, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$maxNumberOfEvents, 4933.3616, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$accrualTime, c(6, 137.21727), tolerance = 1e-07)
	expect_equal(sampleSizeResult4$totalAccrualTime, 137.21727, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeResult4$nFixed, 7086.5152, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$nFixed1, 3543.2576, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$nFixed2, 3543.2576, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$analysisTime[1, ], 142.21727, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$studyDuration, 142.21727, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$criticalValuesEffectScale[1, ], 1.057396, tolerance = 1e-07)
	expect_equal(sampleSizeResult4$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

})

context("Testing the follow-up time calculation")


test_that("'getSampleSizeSurvival': analysis time at last stage equals accrual time + follow-up time", {
	x1 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "P"), 
		accrualTime = 12, maxNumberOfSubjects = 766,
		pi2 = 0.05, pi1 = 0.1)
	expect_equal(x1$analysisTime[3], x1$accrualTime + x1$followUpTime)

	x2 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "P"), 
		accrualTime = 12, maxNumberOfSubjects = 766, 
		lambda2 = 0.005, lambda1 = 0.01)

	expect_equal(x2$analysisTime[3], x2$accrualTime + x2$followUpTime)

	x3 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "WT"), 
		accrualTime = c(0, 12, 15), accrualIntensity = c(20, 30), 
		lambda2 = 0.005, lambda1 = 0.01)

	expect_equal(x3$analysisTime[length(x3$analysisTime)], x3$accrualTime[length(x3$accrualTime)] + x3$followUpTime)

	x4 <- getSampleSizeSurvival(getDesignGroupSequential(typeOfDesign = "WT"), 
		accrualTime = c(0, 12, 15), accrualIntensity = c(40, 60), 
		piecewiseSurvivalTime = c(0, 5), lambda2 = c(0.005, 0.01), hazardRatio = 0.8)

	expect_equal(x4$analysisTime[length(x4$analysisTime)], x4$accrualTime[length(x4$accrualTime)] + x4$followUpTime)

})

test_that("'getSampleSizeSurvival': follow-up time is equal for different argument-target constellations", {

	designGS1 <- getDesignGroupSequential(informationRates = c(0.2,0.5,1), sided = 1, 
		beta = 0.1, typeOfDesign = "WT", deltaWT = 0.3)

	x5 <- getSampleSizeSurvival(designGS1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, 
		eventTime = 14, accrualTime = 8, followUpTime = 10, 
		accountForObservationTimes = TRUE, allocationRatioPlanned = 1)
	x6 <- getSampleSizeSurvival(designGS1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, 
		eventTime = 14, accrualTime = 8, maxNumberOfSubjects = x5$maxNumberOfSubjects,
		accountForObservationTimes = TRUE, allocationRatioPlanned = 1)
	expect_equal(x5$followUpTime, x6$followUpTime)

	x7 <- getSampleSizeSurvival(designGS1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, 
		eventTime = 6, accrualTime = 8, followUpTime = 10, 
		accountForObservationTimes = TRUE, allocationRatioPlanned = 1)
	x8 <- getSampleSizeSurvival(designGS1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, 
		eventTime = 6, accrualTime = 8, maxNumberOfSubjects = x7$maxNumberOfSubjects, 
		accountForObservationTimes = TRUE, allocationRatioPlanned = 1)
	expect_equal(x7$followUpTime, x8$followUpTime)

	x9 <- getSampleSizeSurvival(designGS1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, 
		eventTime = 16, accrualTime = 8, followUpTime = 10, 
		accountForObservationTimes = TRUE, allocationRatioPlanned = 1)
	x10 <- getSampleSizeSurvival(designGS1, 
		typeOfComputation = "Schoenfeld", thetaH0 = 1, pi1 = 0.4, pi2 = 0.2, 
		eventTime = 16, accrualTime = 8, maxNumberOfSubjects = x9$maxNumberOfSubjects, 
		accountForObservationTimes = TRUE, allocationRatioPlanned = 1)
	expect_equal(x9$followUpTime, x10$followUpTime)

})

context("Testing expected warnings and errors")


test_that("'getSampleSizeSurvival': illegal arguments", {
	expect_error(getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
			lambda2 = 0.02, lambda1 = c(0.01, 0.015), followUpTime = 5),
		paste0("Illegal argument: the calulation of 'maxNumberOfSubjects' for given 'followUpTime' ",
			"is only available for a single 'lambda1'; lambda1 = c(0.01, 0.015)"), fixed = TRUE)

	expect_error(getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
			lambda2 = 0.02, median1 = c(5, 6), followUpTime = 5),
		paste0("Illegal argument: the calulation of 'maxNumberOfSubjects' for given 'followUpTime' ",
			"is only available for a single 'lambda1'; lambda1 = c(0.139, 0.116)"), fixed = TRUE)

	expect_error(getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
			median2 = 4, median1 = c(5, 6), followUpTime = 5),
		paste0("Illegal argument: the calulation of 'maxNumberOfSubjects' for given 'followUpTime' ",
			"is only available for a single 'median1'; median1 = c(5, 6)"), fixed = TRUE)

	expect_error(getSampleSizeSurvival(accrualTime = c(0, 6), accrualIntensity = c(22, 53), 
			pi2 =  0.213, pi1 = c(0.113, 0.165), followUpTime = 5),
		paste0("Illegal argument: the calulation of 'maxNumberOfSubjects' for given 'followUpTime' ",
			"is only available for a single 'pi1'; pi1 = c(0.113, 0.165)"), fixed = TRUE)

	expect_error(getSampleSizeSurvival(accrualTime = c(0), pi1 = c(0.4, 0.5), 
			accrualIntensity = c(22), followUpTime = 6),
		paste0("Illegal argument: the calulation of 'maxNumberOfSubjects' for given 'followUpTime' ",
			"is only available for a single 'pi1'; pi1 = c(0.4, 0.5)"), 
		fixed = TRUE)

	expect_warning(getSampleSizeSurvival(accrualTime = c(0, 6, 30), pi1 = 0.4,
		accrualIntensity = c(0.22, 0.53), maxNumberOfSubjects = 1000),
		"Accrual duration longer than maximal study duration (time to maximal number of events); followUpTime = -17.501", 
		fixed = TRUE)

	expect_error(getSampleSizeSurvival(accrualTime = c(0, 6), pi1 = 0.4,
		accrualIntensity = c(0.22, 0.53), maxNumberOfSubjects = 1000),
		paste0("Illegal argument: the calulation of 'followUpTime' for given 'maxNumberOfSubjects' and ",
			"relative accrual intensities (< 1) can only be done if end of accrual is defined"), fixed = TRUE)

	expect_error(getSampleSizeSurvival(lambda2 = -1, hazardRatio = 2),
		"Argument out of bounds: 'lambda2' (-1) must be >= 0", fixed = TRUE)

	expect_error(getSampleSizeSurvival(lambda2 = 0, hazardRatio = 2),
		"Illegal argument: 'lambda2' (0) not allowed: at least one lambda value must be > 0", fixed = TRUE)

	expect_error(getSampleSizeSurvival(lambda2 = 0.9, hazardRatio = 0.8, kappa = 0),
		"Argument out of bounds: 'kappa' (0) must be > 0", fixed = TRUE)

})

context("Testing the calculation of event probabilities and number of subjects")


test_that("'getEventProbabilities': check expected events over time for overall survival (case 1)", {
	design <- getDesignGroupSequential(
		sided = 1, alpha = 0.025, beta = 0.2, 
		informationRates = c(0.33, 0.7, 1), 
		futilityBounds = c(0, 0), 
		bindingFutility = FALSE)

	piecewiseSurvivalTime <- list(
		"0 - <6"   = 0.025, 
		"6 - <9"   = 0.04, 
		"9 - <15"  = 0.015, 
		"15 - <21" = 0.01, 
		">=21"     = 0.007)

	accrualTime <- list(
		"0  - <12" = 15,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39,
		">=16"     = 45)

	powerResults <- getPowerSurvival(
		design = design, typeOfComputation = "Schoenfeld", 
		thetaH0 = 1, directionUpper = FALSE,
		dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
		allocationRatioPlanned = 1,
		accrualTime = accrualTime,
		piecewiseSurvivalTime = piecewiseSurvivalTime, 
		hazardRatio = seq(0.6, 1, 0.05),
		maxNumberOfEvents = 404,
		maxNumberOfSubjects = 1405)

	piecewiseSurvivalTimeOS <- list(
		"0  - <14" = 0.015,
		"14 - <24" = 0.01,
		"24 - <44" = 0.005,
		">=44"     = 0.0025
	)

	timeOS <- c(powerResults$analysisTime[2:3, 4], 17 + 3.5 * 12)
	eventsOS <- getEventProbabilities(
		timeOS, accrualTime = accrualTime,
		piecewiseSurvivalTime = piecewiseSurvivalTimeOS, kappa = 1,
		allocationRatioPlanned = 1, hazardRatio = 0.8,
		dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
		maxNumberOfSubjects = 1405)$overallEventProbabilities
	eventsOS <- eventsOS * 1405

	expect_equal(round(timeOS, 2), c(37.60, 46.72, 59.00))
	expect_equal(round(eventsOS, 1), c(194.1, 288.7, 365.1))

})

test_that("'getEventProbabilities': check expected events over time for overall survival (case 2)", {

	accrualTime <- list(
		"0  - <12" = 15,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39,
		">=16"     = 45)

	piecewiseSurvivalTimeOS <- list(
		"0  - <14" = 0.015,
		"14 - <24" = 0.01,
		"24 - <44" = 0.005,
		">=44"     = 0.0025
	)

	timeOS <- c(37.59823, 46.71658, 59)
	eventsOS <- getEventProbabilities(
		timeOS, accrualTime = accrualTime,
		piecewiseSurvivalTime = piecewiseSurvivalTimeOS, kappa = 1,
		allocationRatioPlanned = 1, hazardRatio = 0.8,
		dropoutRate1 = 0.05, dropoutRate2 = 0.05, dropoutTime = 12,
		maxNumberOfSubjects = 1405)

	##
	## Comparison of the results of EventProbabilities object 'eventsOS' with expected results
	##
	expect_equal(eventsOS$time, c(37.59823, 46.71658, 59), tolerance = 1e-07)
	expect_equal(eventsOS$accrualTime, c(12, 13, 14, 15, 16, 40.555556), tolerance = 1e-07)
	expect_equal(eventsOS$lambda1, c(0.012, 0.008, 0.004, 0.002), tolerance = 1e-07)
	expect_equal(eventsOS$overallEventProbabilities, c(0.13811859, 0.20546928, 0.2598385), tolerance = 1e-07)
	expect_equal(eventsOS$eventProbabilities1, c(0.12437783, 0.18544801, 0.23527681), tolerance = 1e-07)
	expect_equal(eventsOS$eventProbabilities2, c(0.15185935, 0.22549055, 0.28440019), tolerance = 1e-07)

})

test_that("'getNumberOfSubjects': check the number of recruited subjects at given time vector", {

	accrualTime1 <- list(
		"0  - <12" = 12,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39,
		">=16"     = 45)

	numberOfSubjects1 <- getNumberOfSubjects(time = 1:3, 
		accrualTime = getAccrualTime(accrualTime1, maxNumberOfSubjects = 1405))

	##
	## Comparison of the results of NumberOfSubjects object 'numberOfSubjects1' with expected results
	##
	expect_equal(numberOfSubjects1$time, c(1, 2, 3))
	expect_equal(numberOfSubjects1$accrualTime, c(12, 13, 14, 15, 16, 41.355556), tolerance = 1e-07)
	expect_equal(numberOfSubjects1$numberOfSubjects, c(12, 24, 36), tolerance = 1e-07)

	accrualTime2 <- list(
		"0  - <12" = 12,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39)

	numberOfSubjects2 <- getNumberOfSubjects(time = 1:3, accrualTime = getAccrualTime(accrualTime2))

	##
	## Comparison of the results of NumberOfSubjects object 'numberOfSubjects2' with expected results
	##
	expect_equal(numberOfSubjects2$time, c(1, 2, 3))
	expect_equal(numberOfSubjects2$maxNumberOfSubjects, 264)
	expect_equal(numberOfSubjects2$numberOfSubjects, c(12, 24, 36))

})

test_that("'getSampleSizeSurvival': check the calulation of 'maxNumberOfSubjects' for given 'followUpTime'", {

	sampleSizeSurvival1 <- getSampleSizeSurvival(lambda2 = c(0.02, 0.03), 
		piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.6, 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival1' with expected results
	##
	expect_equal(sampleSizeSurvival1$directionUpper, FALSE)
	expect_equal(sampleSizeSurvival1$lambda1, c(0.012, 0.018), tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$maxNumberOfSubjects, 484.65038, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$maxNumberOfEvents, 120.3157, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$accrualTime, 16.155013, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeSurvival1$nFixed, 484.65038, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$nFixed1, 242.32519, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$nFixed2, 242.32519, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$analysisTime[1, ], 24.155014, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$studyDuration, 24.155014, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival1$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeSurvival2 <- getSampleSizeSurvival(piecewiseSurvivalTime = list(
			"<12"  = 0.02,
			">=12" = 0.03), hazardRatio = 0.6, 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival2' with expected results
	##
	expect_equal(sampleSizeSurvival2$directionUpper, FALSE)
	expect_equal(sampleSizeSurvival2$lambda1, c(0.012, 0.018), tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$maxNumberOfSubjects, 484.65038, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$maxNumberOfEvents, 120.3157, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$accrualTime, 16.155013, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeSurvival2$nFixed, 484.65038, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$nFixed1, 242.32519, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$nFixed2, 242.32519, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$analysisTime[1, ], 24.155014, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$studyDuration, 24.155014, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival2$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeSurvival3 <- getSampleSizeSurvival(lambda2 = c(0.02, 0.03), 
		piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.6, 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival3' with expected results
	##
	expect_equal(sampleSizeSurvival3$directionUpper, FALSE)
	expect_equal(sampleSizeSurvival3$lambda1, c(0.012, 0.018), tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$maxNumberOfSubjects, 484.65038, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$maxNumberOfEvents, 120.3157, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$accrualTime, 16.155013, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeSurvival3$nFixed, 484.65038, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$nFixed1, 242.32519, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$nFixed2, 242.32519, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$analysisTime[1, ], 24.155014, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$studyDuration, 24.155014, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$criticalValuesEffectScale[1, ], 0.6995143, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival3$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeSurvival4 <- getSampleSizeSurvival(lambda2 = c(0.02, 0.03), 
		piecewiseSurvivalTime = c(0, 12), hazardRatio = 0.8, 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival4' with expected results
	##
	expect_equal(sampleSizeSurvival4$directionUpper, FALSE)
	expect_equal(sampleSizeSurvival4$lambda1, c(0.016, 0.024), tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$maxNumberOfSubjects, 1325.4661, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$maxNumberOfEvents, 630.52017, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$accrualTime, 44.182203, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeSurvival4$nFixed, 1325.4661, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$nFixed1, 662.73305, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$nFixed2, 662.73305, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$analysisTime[1, ], 52.182201, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$studyDuration, 52.182201, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$criticalValuesEffectScale[1, ], 0.85546574, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival4$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeSurvival5 <- getSampleSizeSurvival(lambda1 = 0.03, lambda2 = 0.2, hazardRatio = 0.6, 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival5' with expected results
	##
	expect_equal(sampleSizeSurvival5$directionUpper, FALSE)
	expect_equal(sampleSizeSurvival5$pi1, 0.30232367, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$pi2, 0.90928205, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$median1, 23.104906, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$median2, 3.4657359, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$hazardRatio, 0.15, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$maxNumberOfSubjects, 16.953283, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$maxNumberOfEvents, 8.723245, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$accrualTime, 0.56510944, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeSurvival5$nFixed, 16.953283, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$nFixed1, 8.4766417, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$nFixed2, 8.4766417, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$analysisTime[1, ], 8.5650223, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$studyDuration, 8.5650223, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$criticalValuesEffectScale[1, ], 0.26521666, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival5$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	sampleSizeSurvival6 <- getSampleSizeSurvival(lambda1 = 0.03, lambda2 = 0.2, hazardRatio = c(0.6, 0.7), 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0) 

	##
	## Comparison of the results of TrialDesignPlanSurvival object 'sampleSizeSurvival6' with expected results
	##
	expect_equal(sampleSizeSurvival6$directionUpper, FALSE)
	expect_equal(sampleSizeSurvival6$pi1, 0.30232367, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$pi2, 0.90928205, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$median1, 23.104906, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$median2, 3.4657359, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$hazardRatio, 0.15, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$maxNumberOfSubjects, 16.953283, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$maxNumberOfEvents, 8.723245, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$accrualTime, 0.56510944, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$calculateFollowUpTime, TRUE)
	expect_equal(sampleSizeSurvival6$nFixed, 16.953283, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$nFixed1, 8.4766417, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$nFixed2, 8.4766417, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$analysisTime[1, ], 8.5650223, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$studyDuration, 8.5650223, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$criticalValuesEffectScale[1, ], 0.26521666, tolerance = 1e-07)
	expect_equal(sampleSizeSurvival6$criticalValuesPValueScale[1, ], 0.025, tolerance = 1e-07)

	expect_error(getSampleSizeSurvival(lambda2 = 0.2, hazardRatio = c(0.6, 0.7), 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0))

	expect_error(getSampleSizeSurvival(lambda1 = c(0.02, 0.03), lambda2 = 0.2, hazardRatio = 0.6, 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0))

	expect_error(getSampleSizeSurvival(lambda2 = c(0.02, 0.03), 
		piecewiseSurvivalTime = c(0, 12), hazardRatio = c(0.6, 0.8), 
		followUpTime = 8, accrualIntensity = 30, accrualTime = 0)) 

})

