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
#:#  File name: test-class_analysis_dataset.R
#:#  Creation date: 09 November 2020, 11:42:15
#:#  File version: $Revision$
#:#  Last changed: $Date$
#:#  Last changed by: $Author$
#:#  

context("Testing the Class 'Dataset'")


test_that("Usage of 'getDataset'", {
	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeans1 <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans1' with expected results
	expect_equal(datasetOfMeans1$stages, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfMeans1$groups, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfMeans1$sampleSizes, c(22, 22, 11, 13, 22, 22, 11, 13))
	expect_equal(datasetOfMeans1$means, c(1, 1.4, 1.1, 1.5, 1, 3, 1, 2.5), tolerance = 1e-07)
	expect_equal(datasetOfMeans1$stDevs, c(1, 1, 2, 2, 2, 2, 1.3, 1.3), tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallSampleSizes, c(22, 22, 33, 35, 55, 57, 66, 70))
	expect_equal(datasetOfMeans1$overallMeans, c(1, 1.4, 1.0333333, 1.4371429, 1.02, 2.0403509, 1.0166667, 2.1257143), tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallStDevs, c(1, 1, 1.3814998, 1.4254175, 1.6391506, 1.8228568, 1.5786638, 1.7387056), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans1), NA)))
	    expect_output(print(datasetOfMeans1)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans1), NA)))
	    expect_output(summary(datasetOfMeans1)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfMeans1$.data' with expected results
	expect_equal(datasetOfMeans1$.data$stage, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfMeans1$.data$group, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfMeans1$.data$sampleSize, c(22, 22, 11, 13, 22, 22, 11, 13))
	expect_equal(datasetOfMeans1$.data$mean, c(1, 1.4, 1.1, 1.5, 1, 3, 1, 2.5), tolerance = 1e-07)
	expect_equal(datasetOfMeans1$.data$stDev, c(1, 1, 2, 2, 2, 2, 1.3, 1.3), tolerance = 1e-07)
	expect_equal(datasetOfMeans1$.data$overallSampleSize, c(22, 22, 33, 35, 55, 57, 66, 70))
	expect_equal(datasetOfMeans1$.data$overallMean, c(1, 1.4, 1.0333333, 1.4371429, 1.02, 2.0403509, 1.0166667, 2.1257143), tolerance = 1e-07)
	expect_equal(datasetOfMeans1$.data$overallStDev, c(1, 1, 1.3814998, 1.4254175, 1.6391506, 1.8228568, 1.5786638, 1.7387056), tolerance = 1e-07)

	expect_equal(datasetOfMeans1$stages, datasetOfMeans1$.data$stage, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$groups, datasetOfMeans1$.data$group, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$sampleSizes, datasetOfMeans1$.data$sampleSize, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$means, datasetOfMeans1$.data$mean, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$stDevs, datasetOfMeans1$.data$stDev, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallSampleSizes, datasetOfMeans1$.data$overallSampleSize, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallMeans, datasetOfMeans1$.data$overallMean, tolerance = 1e-07)
	expect_equal(datasetOfMeans1$overallStDevs, datasetOfMeans1$.data$overallStDev, tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetOfMeans1)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

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

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.4371429, 2.0403509, 2.1257143, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.4254175, 1.8228568, 1.7387056, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40380952, -1.0203509, -1.1090476, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

	datasetOfMeans3 <- getDataset(
		overallSampleSizes1 = c(22, 33, 55, 66),
		overallSampleSizes2 = c(22, 35, 57, 70),
		overallMeans1 = c(1, 1.033333, 1.02, 1.016667),
		overallMeans2 = c(1.4, 1.437143, 2.040351, 2.125714),
		overallStDevs1 = c(1, 1.381500, 1.639151, 1.578664),
		overallStDevs2 = c(1, 1.425418, 1.822857, 1.738706)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans3' with expected results
	expect_equal(datasetOfMeans3$stages, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfMeans3$groups, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfMeans3$sampleSizes, c(22, 22, 11, 13, 22, 22, 11, 13))
	expect_equal(datasetOfMeans3$means, c(1, 1.4, 1.099999, 1.5000004, 1.0000005, 3.0000001, 1.000002, 2.4999979), tolerance = 1e-07)
	expect_equal(datasetOfMeans3$stDevs, c(1, 1, 2.0000005, 2.0000009, 2.0000005, 1.9999999, 1.2999989, 1.3000023), tolerance = 1e-07)
	expect_equal(datasetOfMeans3$overallSampleSizes, c(22, 22, 33, 35, 55, 57, 66, 70))
	expect_equal(datasetOfMeans3$overallMeans, c(1, 1.4, 1.033333, 1.437143, 1.02, 2.040351, 1.016667, 2.125714), tolerance = 1e-07)
	expect_equal(datasetOfMeans3$overallStDevs, c(1, 1, 1.3815, 1.425418, 1.639151, 1.822857, 1.578664, 1.738706), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans3), NA)))
	    expect_output(print(datasetOfMeans3)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans3), NA)))
	    expect_output(summary(datasetOfMeans3)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfMeans3$.data' with expected results
	expect_equal(datasetOfMeans3$.data$stage, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfMeans3$.data$group, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfMeans3$.data$sampleSize, c(22, 22, 11, 13, 22, 22, 11, 13))
	expect_equal(datasetOfMeans3$.data$mean, c(1, 1.4, 1.099999, 1.5000004, 1.0000005, 3.0000001, 1.000002, 2.4999979), tolerance = 1e-07)
	expect_equal(datasetOfMeans3$.data$stDev, c(1, 1, 2.0000005, 2.0000009, 2.0000005, 1.9999999, 1.2999989, 1.3000023), tolerance = 1e-07)
	expect_equal(datasetOfMeans3$.data$overallSampleSize, c(22, 22, 33, 35, 55, 57, 66, 70))
	expect_equal(datasetOfMeans3$.data$overallMean, c(1, 1.4, 1.033333, 1.437143, 1.02, 2.040351, 1.016667, 2.125714), tolerance = 1e-07)
	expect_equal(datasetOfMeans3$.data$overallStDev, c(1, 1, 1.3815, 1.425418, 1.639151, 1.822857, 1.578664, 1.738706), tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetOfMeans3)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans1, c(1, 1.033333, 1.02, 1.016667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans1, c(1, 1.033333, 1.02, 1.016667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans1, c(1, 1.033333, 1.02, 1.016667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallMeans2, c(1.4, 1.437143, 2.040351, 2.125714, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs1, c(1, 1.3815, 1.639151, 1.578664, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs2, c(1, 1.425418, 1.822857, 1.738706, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes1, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes2, c(22, 35, 57, 70, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(-0.4, -0.40381, -1.020351, -1.109047, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

})

test_that("Creation of a dataset of means using stage wise data (one group)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeans4 <- getDataset(
		n = c(22, 11, 22, 11),
		means = c(1, 1.1, 1, 1),
		stDevs = c(1, 2, 2, 1.3)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans4' with expected results
	expect_equal(datasetOfMeans4$stages, c(1, 2, 3, 4))
	expect_equal(datasetOfMeans4$groups, c(1, 1, 1, 1))
	expect_equal(datasetOfMeans4$sampleSizes, c(22, 11, 22, 11))
	expect_equal(datasetOfMeans4$means, c(1, 1.1, 1, 1), tolerance = 1e-07)
	expect_equal(datasetOfMeans4$stDevs, c(1, 2, 2, 1.3), tolerance = 1e-07)
	expect_equal(datasetOfMeans4$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(datasetOfMeans4$overallMeans, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(datasetOfMeans4$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans4), NA)))
	    expect_output(print(datasetOfMeans4)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans4), NA)))
	    expect_output(summary(datasetOfMeans4)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfMeans4$.data' with expected results
	expect_equal(datasetOfMeans4$.data$stage, c(1, 2, 3, 4))
	expect_equal(datasetOfMeans4$.data$group, c(1, 1, 1, 1))
	expect_equal(datasetOfMeans4$.data$sampleSize, c(22, 11, 22, 11))
	expect_equal(datasetOfMeans4$.data$mean, c(1, 1.1, 1, 1), tolerance = 1e-07)
	expect_equal(datasetOfMeans4$.data$stDev, c(1, 2, 2, 1.3), tolerance = 1e-07)
	expect_equal(datasetOfMeans4$.data$overallSampleSize, c(22, 33, 55, 66))
	expect_equal(datasetOfMeans4$.data$overallMean, c(1, 1.0333333, 1.02, 1.0166667), tolerance = 1e-07)
	expect_equal(datasetOfMeans4$.data$overallStDev, c(1, 1.3814998, 1.6391506, 1.5786638), tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetOfMeans4)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs, c(1, 1.3814998, 1.6391506, 1.5786638, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(1, 1.0333333, 1.02, 1.0166667, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

})

test_that("Creation of a dataset of means using overall data (one group)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeans5 <- getDataset(
		overallSampleSizes = c(22, 33, 55, 66),
		overallMeans = c(1.000, 1.033, 1.020, 1.017 ),
		overallStDevs = c(1.00, 1.38, 1.64, 1.58)
	)

	## Comparison of the results of DatasetMeans object 'datasetOfMeans5' with expected results
	expect_equal(datasetOfMeans5$stages, c(1, 2, 3, 4))
	expect_equal(datasetOfMeans5$groups, c(1, 1, 1, 1))
	expect_equal(datasetOfMeans5$sampleSizes, c(22, 11, 22, 11))
	expect_equal(datasetOfMeans5$means, c(1, 1.099, 1.0005, 1.002), tolerance = 1e-07)
	expect_equal(datasetOfMeans5$stDevs, c(1, 1.9967205, 2.003374, 1.3047847), tolerance = 1e-07)
	expect_equal(datasetOfMeans5$overallSampleSizes, c(22, 33, 55, 66))
	expect_equal(datasetOfMeans5$overallMeans, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07)
	expect_equal(datasetOfMeans5$overallStDevs, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfMeans5), NA)))
	    expect_output(print(datasetOfMeans5)$show())
	    invisible(capture.output(expect_error(summary(datasetOfMeans5), NA)))
	    expect_output(summary(datasetOfMeans5)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfMeans5$.data' with expected results
	expect_equal(datasetOfMeans5$.data$stage, c(1, 2, 3, 4))
	expect_equal(datasetOfMeans5$.data$group, c(1, 1, 1, 1))
	expect_equal(datasetOfMeans5$.data$sampleSize, c(22, 11, 22, 11))
	expect_equal(datasetOfMeans5$.data$mean, c(1, 1.099, 1.0005, 1.002), tolerance = 1e-07)
	expect_equal(datasetOfMeans5$.data$stDev, c(1, 1.9967205, 2.003374, 1.3047847), tolerance = 1e-07)
	expect_equal(datasetOfMeans5$.data$overallSampleSize, c(22, 33, 55, 66))
	expect_equal(datasetOfMeans5$.data$overallMean, c(1, 1.033, 1.02, 1.017), tolerance = 1e-07)
	expect_equal(datasetOfMeans5$.data$overallStDev, c(1, 1.38, 1.64, 1.58), tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetOfMeans5)

	## Comparison of the results of StageResultsMeans object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallMeans, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallStDevs, c(1, 1.38, 1.64, 1.58, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallSampleSizes, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallMeans, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallStDevs, c(1, 1.38, 1.64, 1.58, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallSampleSizes, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsMeans object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallMeans, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallStDevs, c(1, 1.38, 1.64, 1.58, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallSampleSizes, c(22, 33, 55, 66, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(1, 1.033, 1.02, 1.017, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

})

test_that("Trim command works as expected for means", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	datasetOfMeansExpected <- getDataset(
		n1 = c(13, 25), 
		n2 = c(15, NA), 
		n3 = c(14, 27), 
		n4 = c(12, 29), 
		means1 = c(24.2, 22.2), 
		means2 = c(18.8, NA),
		means3 = c(26.7, 27.7), 
		means4 = c(9.2, 12.2), 
		stDevs1 = c(24.4, 22.1), 
		stDevs2 = c(21.2, NA), 
		stDevs3 = c(25.6, 23.2), 
		stDevs4 = c(21.5, 22.7)
	)
	datasetOfMeans <- getDataset(
		n1 = c(13, 25), 
		n2 = c(15, NA), 
		n3 = c(14, 27), 
		n4 = c(12, 29), 
		means1 = c(24.2, 22.2), 
		means2 = c(18.8, NA),
		means3 = c(26.7, 27.7), 
		means4 = c(9.2, 12.2), 
		stDevs1 = c(24.4, 22.1), 
		stDevs2 = c(21.2, NA), 
		stDevs3 = c(25.6, 23.2), 
		stDevs4 = c(21.5, 22.7)
	)
	datasetOfMeans$.fillWithNAs(4)
	datasetOfMeans$.trim(2)

	expect_equal(datasetOfMeans$stages, datasetOfMeansExpected$stages)
	expect_equal(datasetOfMeans$groups, datasetOfMeansExpected$groups)
	expect_equal(datasetOfMeans$overallMeans, datasetOfMeansExpected$overallMeans)
	expect_equal(datasetOfMeans$means, datasetOfMeansExpected$means)
	expect_equal(datasetOfMeans$overallStDevs, datasetOfMeansExpected$overallStDevs)
	expect_equal(datasetOfMeans$stDevs, datasetOfMeansExpected$stDevs)

	expect_equal(datasetOfMeans$.data$stage, datasetOfMeansExpected$.data$stage)
	expect_equal(datasetOfMeans$.data$group, datasetOfMeansExpected$.data$group)
	expect_equal(datasetOfMeans$.data$overallMeans, datasetOfMeansExpected$.data$overallMeans)
	expect_equal(datasetOfMeans$.data$means, datasetOfMeansExpected$.data$means)
	expect_equal(datasetOfMeans$.data$overallStDevs, datasetOfMeansExpected$.data$overallStDevs)
	expect_equal(datasetOfMeans$.data$stDevs, datasetOfMeansExpected$.data$stDevs)

})

test_that("Creation of a dataset of rates using stage wise data (one group)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates1 <- getDataset(
		n = c(8, 10, 9, 11), 
		events = c(4, 5, 5, 6)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates1' with expected results
	expect_equal(datasetOfRates1$stages, c(1, 2, 3, 4))
	expect_equal(datasetOfRates1$groups, c(1, 1, 1, 1))
	expect_equal(datasetOfRates1$sampleSizes, c(8, 10, 9, 11))
	expect_equal(datasetOfRates1$events, c(4, 5, 5, 6))
	expect_equal(datasetOfRates1$overallSampleSizes, c(8, 18, 27, 38))
	expect_equal(datasetOfRates1$overallEvents, c(4, 9, 14, 20))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates1), NA)))
	    expect_output(print(datasetOfRates1)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates1), NA)))
	    expect_output(summary(datasetOfRates1)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfRates1$.data' with expected results
	expect_equal(datasetOfRates1$.data$stage, c(1, 2, 3, 4))
	expect_equal(datasetOfRates1$.data$group, c(1, 1, 1, 1))
	expect_equal(datasetOfRates1$.data$sampleSize, c(8, 10, 9, 11))
	expect_equal(datasetOfRates1$.data$event, c(4, 5, 5, 6))
	expect_equal(datasetOfRates1$.data$overallSampleSize, c(8, 18, 27, 38))
	expect_equal(datasetOfRates1$.data$overallEvent, c(4, 9, 14, 20))

	x <- getMultipleStageResultsForDataset(datasetOfRates1, thetaH0 = 0.99)

	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$overallEvents, c(4, 9, 14, 20, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$overallEvents, c(4, 9, 14, 20, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(-Inf, -Inf, -Inf, -Inf, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.929113, -20.89367, -24.622317, -28.727412, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$overallEvents, c(4, 9, 14, 20, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(-13.929113, -15.573222, -13.098993, -14.818182, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(0.5, 0.5, 0.51851852, 0.52631579, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

})

test_that("Creation of a dataset of rates using stage wise data (two groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates2 <- getDataset(
		n2 = c(8, 10, 9, 11),
		n1 = c(11, 13, 12, 13),
		events2 = c(3, 5, 5, 6),
		events1 = c(10, 10, 12, 12)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates2' with expected results
	expect_equal(datasetOfRates2$stages, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfRates2$groups, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfRates2$sampleSizes, c(11, 8, 13, 10, 12, 9, 13, 11))
	expect_equal(datasetOfRates2$events, c(10, 3, 10, 5, 12, 5, 12, 6))
	expect_equal(datasetOfRates2$overallSampleSizes, c(11, 8, 24, 18, 36, 27, 49, 38))
	expect_equal(datasetOfRates2$overallEvents, c(10, 3, 20, 8, 32, 13, 44, 19))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates2), NA)))
	    expect_output(print(datasetOfRates2)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates2), NA)))
	    expect_output(summary(datasetOfRates2)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfRates2$.data' with expected results
	expect_equal(datasetOfRates2$.data$stage, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfRates2$.data$group, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfRates2$.data$sampleSize, c(11, 8, 13, 10, 12, 9, 13, 11))
	expect_equal(datasetOfRates2$.data$event, c(10, 3, 10, 5, 12, 5, 12, 6))
	expect_equal(datasetOfRates2$.data$overallSampleSize, c(11, 8, 24, 18, 36, 27, 49, 38))
	expect_equal(datasetOfRates2$.data$overallEvent, c(10, 3, 20, 8, 32, 13, 44, 19))

	x <- getMultipleStageResultsForDataset(datasetOfRates2, thetaH0 = 0.99)

	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$overallEvents1, c(10, 20, 32, 44, NA_real_))
	expect_equal(x$stageResults1$overallEvents2, c(3, 8, 13, 19, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes1, c(11, 24, 36, 49, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes2, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$overallEvents1, c(10, 20, 32, 44, NA_real_))
	expect_equal(x$stageResults2$overallEvents2, c(3, 8, 13, 19, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes1, c(11, 24, 36, 49, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes2, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(-Inf, -Inf, -Inf, -Inf, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$overallEvents1, c(10, 20, 32, 44, NA_real_))
	expect_equal(x$stageResults3$overallEvents2, c(3, 8, 13, 19, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes1, c(11, 24, 36, 49, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes2, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

})

test_that("Creation of a dataset of rates using stage wise data (four groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates3 <- getDataset(
		n1 = c(11, 13, 12, 13),
		n2 = c(8, 10, 9, 11),
		n3 = c(7, 10, 8, 9),
		n4 = c(9, 11, 5, 2),
		events1 = c(10, 10, 12, 12),
		events2 = c(3, 5, 5, 6),
		events3 = c(2, 4, 3, 5),
		events4 = c(3, 4, 3, 0)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates3' with expected results
	expect_equal(datasetOfRates3$stages, c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4))
	expect_equal(datasetOfRates3$groups, c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))
	expect_equal(datasetOfRates3$sampleSizes, c(11, 8, 7, 9, 13, 10, 10, 11, 12, 9, 8, 5, 13, 11, 9, 2))
	expect_equal(datasetOfRates3$events, c(10, 3, 2, 3, 10, 5, 4, 4, 12, 5, 3, 3, 12, 6, 5, 0))
	expect_equal(datasetOfRates3$overallSampleSizes, c(11, 8, 7, 9, 24, 18, 17, 20, 36, 27, 25, 25, 49, 38, 34, 27))
	expect_equal(datasetOfRates3$overallEvents, c(10, 3, 2, 3, 20, 8, 6, 7, 32, 13, 9, 10, 44, 19, 14, 10))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates3), NA)))
	    expect_output(print(datasetOfRates3)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates3), NA)))
	    expect_output(summary(datasetOfRates3)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfRates3$.data' with expected results
	expect_equal(datasetOfRates3$.data$stage, c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4))
	expect_equal(datasetOfRates3$.data$group, c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))
	expect_equal(datasetOfRates3$.data$sampleSize, c(11, 8, 7, 9, 13, 10, 10, 11, 12, 9, 8, 5, 13, 11, 9, 2))
	expect_equal(datasetOfRates3$.data$event, c(10, 3, 2, 3, 10, 5, 4, 4, 12, 5, 3, 3, 12, 6, 5, 0))
	expect_equal(datasetOfRates3$.data$overallSampleSize, c(11, 8, 7, 9, 24, 18, 17, 20, 36, 27, 25, 25, 49, 38, 34, 27))
	expect_equal(datasetOfRates3$.data$overallEvent, c(10, 3, 2, 3, 20, 8, 6, 7, 32, 13, 9, 10, 44, 19, 14, 10))

})

test_that("Creation of a dataset of rates using overall data (two groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates4 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates4' with expected results
	expect_equal(datasetOfRates4$stages, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfRates4$groups, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfRates4$sampleSizes, c(11, 8, 13, 10, 12, 9, 13, 11))
	expect_equal(datasetOfRates4$events, c(10, 3, 10, 5, 12, 5, 12, 6))
	expect_equal(datasetOfRates4$overallSampleSizes, c(11, 8, 24, 18, 36, 27, 49, 38))
	expect_equal(datasetOfRates4$overallEvents, c(10, 3, 20, 8, 32, 13, 44, 19))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates4), NA)))
	    expect_output(print(datasetOfRates4)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates4), NA)))
	    expect_output(summary(datasetOfRates4)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfRates4$.data' with expected results
	expect_equal(datasetOfRates4$.data$stage, c(1, 1, 2, 2, 3, 3, 4, 4))
	expect_equal(datasetOfRates4$.data$group, c(1, 2, 1, 2, 1, 2, 1, 2))
	expect_equal(datasetOfRates4$.data$sampleSize, c(11, 8, 13, 10, 12, 9, 13, 11))
	expect_equal(datasetOfRates4$.data$event, c(10, 3, 10, 5, 12, 5, 12, 6))
	expect_equal(datasetOfRates4$.data$overallSampleSize, c(11, 8, 24, 18, 36, 27, 49, 38))
	expect_equal(datasetOfRates4$.data$overallEvent, c(10, 3, 20, 8, 32, 13, 44, 19))

	x <- getMultipleStageResultsForDataset(datasetOfRates4, thetaH0 = 0.99)

	## Comparison of the results of StageResultsRates object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$overallEvents1, c(10, 20, 32, 44, NA_real_))
	expect_equal(x$stageResults1$overallEvents2, c(3, 8, 13, 19, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes1, c(11, 24, 36, 49, NA_real_))
	expect_equal(x$stageResults1$overallSampleSizes2, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults1$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$overallEvents1, c(10, 20, 32, 44, NA_real_))
	expect_equal(x$stageResults2$overallEvents2, c(3, 8, 13, 19, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes1, c(11, 24, 36, 49, NA_real_))
	expect_equal(x$stageResults2$overallSampleSizes2, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults2$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(-Inf, -Inf, -Inf, -Inf, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsRates object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallTestStatistics, c(-13.397899, -26.707477, -31.300879, -37.503444, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$overallPValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$overallEvents1, c(10, 20, 32, 44, NA_real_))
	expect_equal(x$stageResults3$overallEvents2, c(3, 8, 13, 19, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes1, c(11, 24, 36, 49, NA_real_))
	expect_equal(x$stageResults3$overallSampleSizes2, c(8, 18, 27, 38, NA_real_))
	expect_equal(x$stageResults3$testStatistics, c(-13.397899, -23.909016, -16.449119, -20.614826, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$pValues, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(0.53409091, 0.38888889, 0.40740741, 0.39795918, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(1, 1, 1, 1, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

})

test_that("Creation of a dataset of rates using overall data (three groups)", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRates5 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallSampleSizes3 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19),
		overallEvents3 = c(3, 7, 12, 20)
	)

	## Comparison of the results of DatasetRates object 'datasetOfRates5' with expected results
	expect_equal(datasetOfRates5$stages, c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
	expect_equal(datasetOfRates5$groups, c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3))
	expect_equal(datasetOfRates5$sampleSizes, c(11, 8, 8, 13, 10, 10, 12, 9, 9, 13, 11, 11))
	expect_equal(datasetOfRates5$events, c(10, 3, 3, 10, 5, 4, 12, 5, 5, 12, 6, 8))
	expect_equal(datasetOfRates5$overallSampleSizes, c(11, 8, 8, 24, 18, 18, 36, 27, 27, 49, 38, 38))
	expect_equal(datasetOfRates5$overallEvents, c(10, 3, 3, 20, 8, 7, 32, 13, 12, 44, 19, 20))
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetOfRates5), NA)))
	    expect_output(print(datasetOfRates5)$show())
	    invisible(capture.output(expect_error(summary(datasetOfRates5), NA)))
	    expect_output(summary(datasetOfRates5)$show())
	}

	## Comparison of the results of data.frame object 'datasetOfRates5$.data' with expected results
	expect_equal(datasetOfRates5$.data$stage, c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))
	expect_equal(datasetOfRates5$.data$group, c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3))
	expect_equal(datasetOfRates5$.data$sampleSize, c(11, 8, 8, 13, 10, 10, 12, 9, 9, 13, 11, 11))
	expect_equal(datasetOfRates5$.data$event, c(10, 3, 3, 10, 5, 4, 12, 5, 5, 12, 6, 8))
	expect_equal(datasetOfRates5$.data$overallSampleSize, c(11, 8, 8, 24, 18, 18, 36, 27, 27, 49, 38, 38))
	expect_equal(datasetOfRates5$.data$overallEvent, c(10, 3, 3, 20, 8, 7, 32, 13, 12, 44, 19, 20))

})

test_that("Trim command works as expected for rates", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	datasetOfRatesExpected <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallSampleSizes3 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19),
		overallEvents3 = c(3, 7, 12, 20)
	)
	datasetOfRates <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallSampleSizes3 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19),
		overallEvents3 = c(3, 7, 12, 20)
	)
	datasetOfRates$.fillWithNAs(6)
	datasetOfRates$.trim(4)

	expect_equal(datasetOfRates$stages, datasetOfRatesExpected$stages)
	expect_equal(datasetOfRates$groups, datasetOfRatesExpected$groups)
	expect_equal(datasetOfRates$overallEvents, datasetOfRatesExpected$overallEvents)
	expect_equal(datasetOfRates$events, datasetOfRatesExpected$events)

	expect_equal(datasetOfRates$.data$stage, datasetOfRatesExpected$.data$stage)
	expect_equal(datasetOfRates$.data$group, datasetOfRatesExpected$.data$group)
	expect_equal(datasetOfRates$.data$overallEvent, datasetOfRatesExpected$.data$overallEvent)
	expect_equal(datasetOfRates$.data$event, datasetOfRatesExpected$.data$event)

})

test_that("Creation of a dataset of survival data using stage wise data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	datasetSurvival1 <- getDataset(
		events = c(8, 7, 4, 12),
		allocationRatios = c(1, 1, 1, 3.58333333333333),
		logRanks = c(1.520, 1.273, 0.503, 0.887)
	)

	## Comparison of the results of DatasetSurvival object 'datasetSurvival1' with expected results
	expect_equal(datasetSurvival1$stages, c(1, 2, 3, 4))
	expect_equal(datasetSurvival1$groups, c(1, 1, 1, 1))
	expect_equal(datasetSurvival1$overallEvents, c(8, 15, 19, 31))
	expect_equal(datasetSurvival1$overallAllocationRatios, c(1, 1, 1, 2), tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallLogRanks, c(1.52, 1.9796756, 1.9897802, 2.1096275), tolerance = 1e-07)
	expect_equal(datasetSurvival1$events, c(8, 7, 4, 12))
	expect_equal(datasetSurvival1$allocationRatios, c(1, 1, 1, 3.5833333), tolerance = 1e-07)
	expect_equal(datasetSurvival1$logRanks, c(1.52, 1.273, 0.503, 0.887), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetSurvival1), NA)))
	    expect_output(print(datasetSurvival1)$show())
	    invisible(capture.output(expect_error(summary(datasetSurvival1), NA)))
	    expect_output(summary(datasetSurvival1)$show())
	}

	## Comparison of the results of data.frame object 'datasetSurvival1$.data' with expected results
	expect_equal(datasetSurvival1$.data$stage, c(1, 2, 3, 4))
	expect_equal(datasetSurvival1$.data$group, c(1, 1, 1, 1))
	expect_equal(datasetSurvival1$.data$overallEvent, c(8, 15, 19, 31))
	expect_equal(datasetSurvival1$.data$overallAllocationRatio, c(1, 1, 1, 2), tolerance = 1e-07)
	expect_equal(datasetSurvival1$.data$overallLogRank, c(1.52, 1.9796756, 1.9897802, 2.1096275), tolerance = 1e-07)
	expect_equal(datasetSurvival1$.data$event, c(8, 7, 4, 12))
	expect_equal(datasetSurvival1$.data$allocationRatio, c(1, 1, 1, 3.5833333), tolerance = 1e-07)
	expect_equal(datasetSurvival1$.data$logRank, c(1.52, 1.273, 0.503, 0.887), tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetSurvival1)

	## Comparison of the results of StageResultsSurvival object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults1$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults1$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$logRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults2$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults2$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$logRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults3$overallAllocationRatios, c(1, 1, 1, 2, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults3$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$logRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(2.9294137, 2.7795807, 2.4917213, 2.2339445, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

	expect_equal(datasetSurvival1$stages, datasetSurvival1$.data$stage, tolerance = 1e-07)
	expect_equal(datasetSurvival1$groups, datasetSurvival1$.data$group, tolerance = 1e-07)
	expect_equal(datasetSurvival1$events, datasetSurvival1$.data$event, tolerance = 1e-07)
	expect_equal(datasetSurvival1$allocationRatios, datasetSurvival1$.data$allocationRatio, tolerance = 1e-07)
	expect_equal(datasetSurvival1$logRanks, datasetSurvival1$.data$logRank, tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallEvents, datasetSurvival1$.data$overallEvent, tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallAllocationRatios, datasetSurvival1$.data$overallAllocationRatio, tolerance = 1e-07)
	expect_equal(datasetSurvival1$overallLogRanks, datasetSurvival1$.data$overallLogRank, tolerance = 1e-07)

})

test_that("Creation of a dataset of survival data using overall data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	datasetSurvival2 <- getDataset(
		overallEvents = c(8, 15, 19, 31),
		overallAllocationRatios = c(1, 1, 1, 2),
		overallLogRanks = c(1.52, 1.98, 1.99, 2.11)
	)

	## Comparison of the results of DatasetSurvival object 'datasetSurvival2' with expected results
	expect_equal(datasetSurvival2$stages, c(1, 2, 3, 4))
	expect_equal(datasetSurvival2$groups, c(1, 1, 1, 1))
	expect_equal(datasetSurvival2$overallEvents, c(8, 15, 19, 31))
	expect_equal(datasetSurvival2$overallAllocationRatios, c(1, 1, 1, 2))
	expect_equal(datasetSurvival2$overallLogRanks, c(1.52, 1.98, 1.99, 2.11), tolerance = 1e-07)
	expect_equal(datasetSurvival2$events, c(8, 7, 4, 12))
	expect_equal(datasetSurvival2$allocationRatios, c(1, 1, 1, 3.5833333), tolerance = 1e-07)
	expect_equal(datasetSurvival2$logRanks, c(1.52, 1.2734749, 0.50285094, 0.8873221), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetSurvival2), NA)))
	    expect_output(print(datasetSurvival2)$show())
	    invisible(capture.output(expect_error(summary(datasetSurvival2), NA)))
	    expect_output(summary(datasetSurvival2)$show())
	}

	## Comparison of the results of data.frame object 'datasetSurvival2$.data' with expected results
	expect_equal(datasetSurvival2$.data$stage, c(1, 2, 3, 4))
	expect_equal(datasetSurvival2$.data$group, c(1, 1, 1, 1))
	expect_equal(datasetSurvival2$.data$overallEvent, c(8, 15, 19, 31))
	expect_equal(datasetSurvival2$.data$overallAllocationRatio, c(1, 1, 1, 2))
	expect_equal(datasetSurvival2$.data$overallLogRank, c(1.52, 1.98, 1.99, 2.11), tolerance = 1e-07)
	expect_equal(datasetSurvival2$.data$event, c(8, 7, 4, 12))
	expect_equal(datasetSurvival2$.data$allocationRatio, c(1, 1, 1, 3.5833333), tolerance = 1e-07)
	expect_equal(datasetSurvival2$.data$logRank, c(1.52, 1.2734749, 0.50285094, 0.8873221), tolerance = 1e-07)

	x <- getMultipleStageResultsForDataset(datasetSurvival2)

	## Comparison of the results of StageResultsSurvival object 'x$stageResults1' with expected results
	expect_equal(x$stageResults1$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults1$overallAllocationRatios, c(1, 1, 1, 2, NA_real_))
	expect_equal(x$stageResults1$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults1$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults1$logRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults1$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults1), NA)))
	    expect_output(print(x$stageResults1)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults1), NA)))
	    expect_output(summary(x$stageResults1)$show())
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults2' with expected results
	expect_equal(x$stageResults2$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults2$overallAllocationRatios, c(1, 1, 1, 2, NA_real_))
	expect_equal(x$stageResults2$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults2$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$logRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults2$combInverseNormal, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults2$weightsInverseNormal, c(0.4472136, 0.4472136, 0.4472136, 0.4472136, 0.4472136), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults2), NA)))
	    expect_output(print(x$stageResults2)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults2), NA)))
	    expect_output(summary(x$stageResults2)$show())
	}

	## Comparison of the results of StageResultsSurvival object 'x$stageResults3' with expected results
	expect_equal(x$stageResults3$overallLogRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallEvents, c(8, 15, 19, 31, NA_real_))
	expect_equal(x$stageResults3$overallAllocationRatios, c(1, 1, 1, 2, NA_real_))
	expect_equal(x$stageResults3$events, c(8, 7, 4, 12, NA_real_))
	expect_equal(x$stageResults3$allocationRatios, c(1, 1, 1, 3.5833333, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$logRanks, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$pValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$overallPValues, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$effectSizes, c(2.9294137, 2.7800464, 2.4919726, 2.2342616, NA_real_), tolerance = 1e-07)
	expect_equal(x$stageResults3$combFisher, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
	expect_equal(x$stageResults3$weightsFisher, c(1, 1, 1, 1, 1), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x$stageResults3), NA)))
	    expect_output(print(x$stageResults3)$show())
	    invisible(capture.output(expect_error(summary(x$stageResults3), NA)))
	    expect_output(summary(x$stageResults3)$show())
	}

	datasetSurvival3 <- getDataset(
		events1  = c(25, 32), 
		events2  = c(18, NA), 
		events3  = c(22, 36),
		logRanks1 = -c(2.2,1.8),
		logRanks2 = -c(1.99, NA),
		logRanks3 = -c(2.32, 2.11)
	)

	## Comparison of the results of DatasetSurvival object 'datasetSurvival3' with expected results
	expect_equal(datasetSurvival3$stages, c(1, 1, 1, 2, 2, 2))
	expect_equal(datasetSurvival3$groups, c(1, 2, 3, 1, 2, 3))
	expect_equal(datasetSurvival3$overallEvents, c(25, 18, 22, 57, NA_real_, 58))
	expect_equal(datasetSurvival3$overallAllocationRatios, c(1, 1, 1, 1, NA_real_, 1))
	expect_equal(datasetSurvival3$overallLogRanks, c(-2.2, -1.99, -2.32, -2.8056692, NA_real_, -3.0911851), tolerance = 1e-07)
	expect_equal(datasetSurvival3$events, c(25, 18, 22, 32, NA_real_, 36))
	expect_equal(datasetSurvival3$allocationRatios, c(1, 1, 1, 1, NA_real_, 1))
	expect_equal(datasetSurvival3$logRanks, c(-2.2, -1.99, -2.32, -1.8, NA_real_, -2.11), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(datasetSurvival3), NA)))
	    expect_output(print(datasetSurvival3)$show())
	    invisible(capture.output(expect_error(summary(datasetSurvival3), NA)))
	    expect_output(summary(datasetSurvival3)$show())
	}

	## Comparison of the results of data.frame object 'datasetSurvival3$.data' with expected results
	expect_equal(datasetSurvival3$.data$stage, c(1, 1, 1, 2, 2, 2))
	expect_equal(datasetSurvival3$.data$group, c(1, 2, 3, 1, 2, 3))
	expect_equal(datasetSurvival3$.data$overallEvent, c(25, 18, 22, 57, NA_real_, 58))
	expect_equal(datasetSurvival3$.data$overallAllocationRatio, c(1, 1, 1, 1, NA_real_, 1))
	expect_equal(datasetSurvival3$.data$overallLogRank, c(-2.2, -1.99, -2.32, -2.8056692, NA_real_, -3.0911851), tolerance = 1e-07)
	expect_equal(datasetSurvival3$.data$event, c(25, 18, 22, 32, NA_real_, 36))
	expect_equal(datasetSurvival3$.data$allocationRatio, c(1, 1, 1, 1, NA_real_, 1))
	expect_equal(datasetSurvival3$.data$logRank, c(-2.2, -1.99, -2.32, -1.8, NA_real_, -2.11), tolerance = 1e-07)

})

test_that("Trim command works as expected for suvival data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	dataExampleSurvivalExpected <- getDataset(
		events1   = c(25, 32), 
		events2   = c(18, NA),
		events3   = c(22, 36), 
		logRanks1 = c(2.2,1.8),	
		logRanks2 = c(1.99, NA), 
		logRanks3 = c(2.32, 2.11)
	)
	dataExampleSurvival <- getDataset(
		events1   = c(25, 32), 
		events2   = c(18, NA),
		events3   = c(22, 36), 
		logRanks1 = c(2.2,1.8),	
		logRanks2 = c(1.99, NA), 
		logRanks3 = c(2.32, 2.11)
	)
	dataExampleSurvival$.fillWithNAs(4)
	dataExampleSurvival$.trim(2)

	expect_equal(dataExampleSurvival$stages, dataExampleSurvivalExpected$stages)
	expect_equal(dataExampleSurvival$groups, dataExampleSurvivalExpected$groups)
	expect_equal(dataExampleSurvival$overallEvents, dataExampleSurvivalExpected$overallEvents)
	expect_equal(dataExampleSurvival$overallAllocationRatios, dataExampleSurvivalExpected$overallAllocationRatios)
	expect_equal(dataExampleSurvival$overallLogRanks, dataExampleSurvivalExpected$overallLogRanks, tolerance = 1e-07)
	expect_equal(dataExampleSurvival$events, dataExampleSurvivalExpected$events)
	expect_equal(dataExampleSurvival$allocationRatios, dataExampleSurvivalExpected$allocationRatios)
	expect_equal(dataExampleSurvival$logRanks, dataExampleSurvivalExpected$logRanks, tolerance = 1e-07)

	expect_equal(dataExampleSurvival$.data$stage, dataExampleSurvivalExpected$.data$stage)
	expect_equal(dataExampleSurvival$.data$group, dataExampleSurvivalExpected$.data$group)
	expect_equal(dataExampleSurvival$.data$overallEvent, dataExampleSurvivalExpected$.data$overallEvent)
	expect_equal(dataExampleSurvival$.data$overallAllocationRatio, dataExampleSurvivalExpected$.data$overallAllocationRatio)
	expect_equal(dataExampleSurvival$.data$overallLogRank, dataExampleSurvivalExpected$.data$overallLogRank, tolerance = 1e-07)
	expect_equal(dataExampleSurvival$.data$event, dataExampleSurvivalExpected$.data$event)
	expect_equal(dataExampleSurvival$.data$allocationRatio, dataExampleSurvivalExpected$.data$allocationRatio)
	expect_equal(dataExampleSurvival$.data$logRank, dataExampleSurvivalExpected$.data$logRank, tolerance = 1e-07)

})

test_that("Dataset functions 'getNumberOfStages' and 'getNumberOfGroups' work as expected for means", {

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetMeans}
	data1 <- getDataset(
		overallN1 = c(22, 33, NA),
		overallN2 = c(20, 34, 56),
		overallN3 = c(22, 31, 52),
		overallMeans1 = c(1.64, 1.54, NA),
		overallMeans2 = c(1.7, 1.5, 1.77),
		overallMeans3 = c(2.5, 2.06, 2.99),
		overallStDevs1 = c(1.5, 1.9, NA),
		overallStDevs2 = c(1.3, 1.3, 1.1),
		overallStDevs3 = c(1, 1.3, 1.8))

	expect_equal(data1$getNumberOfStages(), 3)
	expect_equal(data1$getNumberOfStages(FALSE), 3)
	expect_equal(data1$getNumberOfGroups(), 3)
	expect_equal(data1$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	.skipTestIfDisabled()

	data2 <- getDataset(
		overallN1 = c(22, 33, 55),
		overallN2 = c(20, 34, 56),
		overallN3 = c(22, 31, 52),
		overallMeans1 = c(1.64, 1.54, 2.10),
		overallMeans2 = c(1.7, 1.5, 1.77),
		overallMeans3 = c(2.5, 2.06, 2.99),
		overallStDevs1 = c(1.5, 1.9, 1.7),
		overallStDevs2 = c(1.3, 1.3, 1.1),
		overallStDevs3 = c(1, 1.3, 1.8))

	expect_equal(data2$getNumberOfStages(), 3)
	expect_equal(data2$getNumberOfStages(FALSE), 3)
	expect_equal(data2$getNumberOfGroups(), 3)
	expect_equal(data2$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	data3 <- getDataset(
		overallN1 = c(22, 33, 55),
		overallN2 = c(20, 34, 56),
		overallN3 = c(22, 31, 52),
		overallMeans1 = c(1.64, 1.54, 2.10),
		overallMeans2 = c(1.7, 1.5, 1.77),
		overallMeans3 = c(2.5, 2.06, 2.99),
		overallStDevs1 = c(1.5, 1.9, 1.7),
		overallStDevs2 = c(1.3, 1.3, 1.1),
		overallStDevs3 = c(1, 1.3, 1.8))

	expect_equal(data3$getNumberOfStages(), 3)
	expect_equal(data3$getNumberOfStages(FALSE), 3)
	expect_equal(data3$getNumberOfGroups(), 3)
	expect_equal(data3$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

})

test_that("Dataset functions 'getNumberOfStages' and 'getNumberOfGroups' work as expected for rates", {

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetRates}
	data1 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, NA),
		overallSampleSizes2 = c(8, 18, 27, NA),
		overallSampleSizes3 = c(8, 18, 27, NA),
		overallEvents1 = c(10, 20, 32, NA),
		overallEvents2 = c(3, 8, 13, NA),
		overallEvents3 = c(3, 7, 12, NA))

	expect_equal(data1$getNumberOfStages(), 3)
	expect_equal(data1$getNumberOfStages(FALSE), 4)
	expect_equal(data1$getNumberOfGroups(), 3)
	expect_equal(data1$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	.skipTestIfDisabled()

	data2 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, 27, 38),
		overallSampleSizes3 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, 13, 19),
		overallEvents3 = c(3, 7, 12, 20))

	expect_equal(data2$getNumberOfStages(), 4)
	expect_equal(data2$getNumberOfStages(FALSE), 4)
	expect_equal(data2$getNumberOfGroups(), 3)
	expect_equal(data2$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 3)

	data3 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36, 49),
		overallSampleSizes2 = c(8, 18, NA, NA),
		overallSampleSizes3 = c(8, 18, NA, NA),
		overallSampleSizes4 = c(8, 18, 27, 38),
		overallEvents1 = c(10, 20, 32, 44),
		overallEvents2 = c(3, 8, NA, NA),
		overallEvents3 = c(3, 8, NA, NA),
		overallEvents4 = c(3, 7, 12, 20))

	expect_equal(data3$getNumberOfStages(), 4)
	expect_equal(data3$getNumberOfStages(FALSE), 4)
	expect_equal(data3$getNumberOfGroups(), 4)
	expect_equal(data3$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 4)

	data4 <- getDataset(
		overallSampleSizes1 = c(11, 24, 36),
		overallSampleSizes2 = c(8, 18, 27),
		overallEvents1 = c(10, 20, 32),
		overallEvents2 = c(3, 7, 12))

	expect_equal(data4$getNumberOfStages(), 3)
	expect_equal(data4$getNumberOfStages(FALSE), 3)
	expect_equal(data4$getNumberOfGroups(), 2)
	expect_equal(data4$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

	data5 <- getDataset(
		overallSampleSizes1 = c(11, 24, NA),
		overallSampleSizes2 = c(8, 18, NA),
		overallEvents1 = c(10, 20, NA),
		overallEvents2 = c(3, 7, NA))

	expect_equal(data5$getNumberOfStages(), 2)
	expect_equal(data5$getNumberOfStages(FALSE), 3)
	expect_equal(data5$getNumberOfGroups(), 2)
	expect_equal(data5$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

	data6 <- getDataset(
		overallSampleSizes = c(11, 24, NA),
		overallEvents = c(3, 7, NA))

	expect_equal(data6$getNumberOfStages(), 2)
	expect_equal(data6$getNumberOfStages(FALSE), 3)
	expect_equal(data6$getNumberOfGroups(), 1)
	expect_equal(data6$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 1)

})

test_that("Dataset functions 'getNumberOfStages' and 'getNumberOfGroups' work as expected for survival data", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:dataInputVariants}
	# @refFS[Tab.]fs:tab:output:getDatasetSurvival}
	data3 <- getDataset(
		overallEvents1   = c(13, 33), 
		overallLogRanks1 = c(1.23, 1.55), 
		overallEvents2   = c(16, 33), 
		overallLogRanks2 = c(1.55, 2.2))
	expect_equal(data3$getNumberOfStages(), 2)
	expect_equal(data3$getNumberOfStages(FALSE), 2)
	expect_equal(data3$getNumberOfGroups(), 3)
	expect_equal(data3$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

	data4 <- getDataset(
		events1   = c(13, NA), 
		logRanks1 = c(1.23, NA), 
		events2   = c(16, NA), 
		logRanks2 = c(1.55, NA))
	expect_equal(data4$getNumberOfStages(), 1)
	expect_equal(data4$getNumberOfStages(FALSE), 2)
	expect_equal(data4$getNumberOfGroups(), 3)
	expect_equal(data4$getNumberOfGroups(survivalCorrectionEnabled = FALSE), 2)

})

test_that("Function '.naOmitBackward' works as expected", {

	expect_equal(.naOmitBackward(c(1, NA_real_, 3, NA_real_)), c(1, NA_real_, 3))
	expect_equal(.naOmitBackward(c(1, NA_real_, 3, NA_real_, 5)), c(1, NA_real_, 3, NA_real_, 5))
	expect_equal(.naOmitBackward(c(1, NA_real_, NA_real_)), c(1))
	expect_equal(.naOmitBackward(c(1, NA_real_, NA_real_, 4)), c(1, NA_real_, NA_real_, 4))
	expect_equal(.naOmitBackward(c(1)), c(1))
	expect_equal(.naOmitBackward(c(NA_real_)), c(NA_real_))
	expect_equal(.naOmitBackward(c(1, 2, NA_real_)), c(1, 2))

})

context("Testing that 'getDataset' Throws Exceptions as Expected")


test_that("Wrong parameter usage of 'getDataset'", {
	# @refFS[Tab.]{fs:tab:dataInputVariants}
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

