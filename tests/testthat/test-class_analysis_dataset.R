######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 06 November 2019, 17:08:54                                                   #
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

context("Testing the class 'Dataset'")


test_that("Usage of 'getDataset'", {
	datasetOfMeans1 <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)
	x <- getMultipleStageResultsForDataset(datasetOfMeans1)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

	datasetOfMeans2 <- getDataset(data.frame(
		stages = 1:4,
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	))
	x <- getMultipleStageResultsForDataset(datasetOfMeans2)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

	datasetOfMeans3 <- getDataset(
		overallSampleSizes1 = c(22, 33, 55, 66),
		overallSampleSizes2 = c(22, 35, 57, 70),
		overallMeans1 = c(1, 1.033333, 1.02, 1.016667),
		overallMeans2 = c(1.4, 1.437143, 2.040351, 2.125714),
		overallStDevs1 = c(1, 1.381500, 1.639151, 1.578664),
		overallStDevs2 = c(1, 1.425418, 1.822857, 1.738706)
	)
	x <- getMultipleStageResultsForDataset(datasetOfMeans3)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.033333, 1.02, 1.016667), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.033333, 1.02, 1.016667), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.033333, 1.02, 1.016667), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of means using stage wise data (one group)", {

	datasetOfMeans4 <- getDataset(
		n = c(22, 11, 22, 11),
		means = c(1, 1.1, 1, 1),
		stDevs = c(1, 2, 2, 1.3)
	)
	x <- getMultipleStageResultsForDataset(datasetOfMeans4)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of means using overall data (one group)", {

	datasetOfMeans5 <- getDataset(
		overallSampleSizes = c(22, 33, 55, 66),
		overallMeans = c(1.000, 1.033, 1.020, 1.017 ),
		overallStDevs = c(1.00, 1.38, 1.64, 1.58)
	)
	x <- getMultipleStageResultsForDataset(datasetOfMeans5)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of rates using stage wise data (one group)", {

	datasetOfRates1 <- getDataset(
		n = c(8, 10, 9, 11), 
		events = c(4, 5, 5, 6)
	)
	x <- getMultipleStageResultsForDataset(datasetOfRates1, thetaH0 = 0.99)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$overallEvents, c(4, 9, 14, 20))
	expect_equal(x$stageResults1$overallSampleSizes, c(8, 18, 27, 38))
	expect_equal(x$stageResults1$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$overallEvents, c(4, 9, 14, 20))
	expect_equal(x$stageResults2$overallSampleSizes, c(8, 18, 27, 38))
	expect_equal(x$stageResults2$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(-Inf, -Inf, -Inf, -Inf, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$overallEvents, c(4, 9, 14, 20))
	expect_equal(x$stageResults3$overallSampleSizes, c(8, 18, 27, 38))
	expect_equal(x$stageResults3$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of rates using stage wise data (two groups)", {

	datasetOfRates2 <- getDataset(
		n2 = c(8, 10, 9, 11),
		n1 = c(11, 13, 12, 13),
		events2 = c(3, 5, 5, 6),
		events1 = c(10, 10, 12, 12)
	)
	x <- getMultipleStageResultsForDataset(datasetOfRates2, thetaH0 = 0.99)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$overallEvents1, c(10, 20, 32, 44))
	expect_equal(x$stageResults1$overallEvents2, c(3, 8, 13, 19))
	expect_equal(x$stageResults1$overallSampleSizes1, c(11, 24, 36, 49))
	expect_equal(x$stageResults1$overallSampleSizes2, c(8, 18, 27, 38))
	expect_equal(x$stageResults1$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$overallEvents1, c(10, 20, 32, 44))
	expect_equal(x$stageResults2$overallEvents2, c(3, 8, 13, 19))
	expect_equal(x$stageResults2$overallSampleSizes1, c(11, 24, 36, 49))
	expect_equal(x$stageResults2$overallSampleSizes2, c(8, 18, 27, 38))
	expect_equal(x$stageResults2$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(-Inf, -Inf, -Inf, -Inf, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$overallEvents1, c(10, 20, 32, 44))
	expect_equal(x$stageResults3$overallEvents2, c(3, 8, 13, 19))
	expect_equal(x$stageResults3$overallSampleSizes1, c(11, 24, 36, 49))
	expect_equal(x$stageResults3$overallSampleSizes2, c(8, 18, 27, 38))
	expect_equal(x$stageResults3$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of rates using stage wise data (three groups)", {

	datasetOfRates3 <- getDataset(
		n1 = c(11, 13, 12, 13),
		n2 = c(8, 10, 9, 11),
		n3 = c(7, 10, 8, 9),
		events1 = c(10, 10, 12, 12),
		events2 = c(3, 5, 5, 6),
		events3 = c(2, 4, 3, 5)
	)

	##
	## Comparison of the results of DatasetRates object 'datasetOfRates3' with expected results
	##
	expect_equal(datasetOfRates3$stages, c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
	expect_equal(datasetOfRates3$groups, c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3))
	expect_equal(datasetOfRates3$sampleSizes, c(11, 8, 7, 13, 10, 10, 12, 9, 8, 13, 11, 9))
	expect_equal(datasetOfRates3$events, c(10, 3, 2, 10, 5, 4, 12, 5, 3, 12, 6, 5))
	expect_equal(datasetOfRates3$overallSampleSizes, c(11, 8, 7, 24, 18, 17, 36, 27, 25, 49, 38, 34))
	expect_equal(datasetOfRates3$overallEvents, c(10, 3, 2, 20, 8, 6, 32, 13, 9, 44, 19, 14))

})

test_that("Creation of a dataset of rates using overall data (two groups)", {

	datasetOfRates4 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19)
	)
	x <- getMultipleStageResultsForDataset(datasetOfRates4, thetaH0 = 0.99)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$overallEvents1, c(10, 20, 32, 44))
	expect_equal(x$stageResults1$overallEvents2, c(3, 8, 13, 19))
	expect_equal(x$stageResults1$overallSampleSizes1, c(11, 24, 36, 49))
	expect_equal(x$stageResults1$overallSampleSizes2, c(8, 18, 27, 38))
	expect_equal(x$stageResults1$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$overallEvents1, c(10, 20, 32, 44))
	expect_equal(x$stageResults2$overallEvents2, c(3, 8, 13, 19))
	expect_equal(x$stageResults2$overallSampleSizes1, c(11, 24, 36, 49))
	expect_equal(x$stageResults2$overallSampleSizes2, c(8, 18, 27, 38))
	expect_equal(x$stageResults2$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(-Inf, -Inf, -Inf, -Inf, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$overallEvents1, c(10, 20, 32, 44))
	expect_equal(x$stageResults3$overallEvents2, c(3, 8, 13, 19))
	expect_equal(x$stageResults3$overallSampleSizes1, c(11, 24, 36, 49))
	expect_equal(x$stageResults3$overallSampleSizes2, c(8, 18, 27, 38))
	expect_equal(x$stageResults3$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of rates using overall data (three groups)", {

	datasetOfRates5 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallSampleSizes3 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19),
		overallEvents3 = c(3, 7, 12, 20)
	)

	##
	## Comparison of the results of DatasetRates object 'datasetOfRates5' with expected results
	##
	expect_equal(datasetOfRates5$stages, c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
	expect_equal(datasetOfRates5$groups, c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3))
	expect_equal(datasetOfRates5$sampleSizes, c(11, 8, 8, 13, 10, 10, 12, 9, 9, 13, 11, 11))
	expect_equal(datasetOfRates5$events, c(10, 3, 3, 10, 5, 4, 12, 5, 5, 12, 6, 8))
	expect_equal(datasetOfRates5$overallSampleSizes, c(11, 8, 8, 24, 18, 18, 36, 27, 27, 49, 38, 38))
	expect_equal(datasetOfRates5$overallEvents, c(10, 3, 3, 20, 8, 7, 32, 13, 12, 44, 19, 20))

})

test_that("Creation of a dataset of survival data using stage wise data", {

	datasetSurvival1 <- getDataset(
		events = c(8, 7, 4, 12),
		allocationRatios = c(1, 1, 1, 3.58333333333333),
		logRanks = c(1.520, 1.273, 0.503, 0.887)
	)
	x <- getMultipleStageResultsForDataset(datasetSurvival1)

	##
	## Comparison of the results of StageResultsSurvival object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults1$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults1$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$logRanks, c(1.52, 1.273, 0.503, 0.887, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsSurvival object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults2$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults2$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$logRanks, c(1.52, 1.273, 0.503, 0.887, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsSurvival object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults3$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults3$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$logRanks, c(1.52, 1.273, 0.503, 0.887, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

test_that("Creation of a dataset of survival data using overall data", {

	datasetSurvival2 <- getDataset(
		overallEvents = c(8, 15, 19, 31),
		overallAllocationRatios = c(1, 1, 1, 2),
		overallLogRanks = c(1.52, 1.98, 1.99, 2.11)
	)
	x <- getMultipleStageResultsForDataset(datasetSurvival2)

	##
	## Comparison of the results of StageResultsSurvival object 'x$stageResults1' with expected results
	##
	expect_equal(x$stageResults1$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults1$overallAllocationRatios, c(1, 1, 1, 2, NA_real_))
	expect_equal(x$stageResults1$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults1$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$logRanks, c(1.52, 1.2734749, 0.50285094, 0.8873221, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsSurvival object 'x$stageResults2' with expected results
	##
	expect_equal(x$stageResults2$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults2$overallAllocationRatios, c(1, 1, 1, 2, NA_real_))
	expect_equal(x$stageResults2$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults2$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$logRanks, c(1.52, 1.2734749, 0.50285094, 0.8873221, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)

	##
	## Comparison of the results of StageResultsSurvival object 'x$stageResults3' with expected results
	##
	expect_equal(x$stageResults3$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults3$overallAllocationRatios, c(1, 1, 1, 2, NA_real_))
	expect_equal(x$stageResults3$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults3$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$logRanks, c(1.52, 1.2734749, 0.50285094, 0.8873221, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)

})

context("Testing that 'getDataset' throws exceptions as expected")


test_that("Wrong parameter usage of 'getDataset'", {
	expect_error(getDataset(), 
		"Missing argument: data.frame or data vectors expected", fixed = TRUE)

	expect_error(getDataset(1), 
		"Illegal argument: all parameters must be named", fixed = TRUE)

	expect_error(getDataset(n = 1), 
		"Illegal argument: failed to identify dataset type", fixed = TRUE)

	expect_error(getDataset(1, x = 2), 
		"Illegal argument: all parameters must be named", fixed = TRUE)

	expect_error(getDataset(
			overallSampleSizes1 = c(11, 24, 36, 49),
			overallSampleSizes2 = c(8, 18, 27, 38),
			overallSampleSizes3 = c(8, 18, 27, 38),
			overallEvents1 = c(10, 20, 32, 44),
			overallEvents2 = c(3, 8, 13, 19),
			overallEvents3 = c(3, 8, 13, 19),
			overallEvents1 = c(3, 8, 13, 19)
		), "Illegal argument: the parameter names must be unique", fixed = TRUE)

})

