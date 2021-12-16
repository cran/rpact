## |  
## |  *Unit tests*
## |  
## |  This file is part of the R package rpact:
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## |  
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## |  
## |  RPACT company website: https://www.rpact.com
## |  RPACT package website: https://www.rpact.org
## |  
## |  Contact us for information about our services: info@rpact.com
## |  
## |  File name: test-f_design_group_sequential.R
## |  Creation date: 08 December 2021, 09:08:50
## |  File version: $Revision$
## |  Last changed: $Date$
## |  Last changed by: $Author$
## |  

context("Testing the Group Sequential and Inverse Normal Design Functionality")


test_that("'getDesignInverseNormal' with default parameters: parameters and results are as expected", {
	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:criticalValuesOBrienFleming}
	x0 <- getDesignInverseNormal()

	## Comparison of the results of TrialDesignInverseNormal object 'x0' with expected results
	expect_equal(x0$alphaSpent, c(0.00025917372, 0.0071600594, 0.02499999), tolerance = 1e-07)
	expect_equal(x0$criticalValues, c(3.4710914, 2.4544323, 2.0040356), tolerance = 1e-07)
	expect_equal(x0$stageLevels, c(0.00025917372, 0.0070553616, 0.022533125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x0), NA)))
	    expect_output(print(x0)$show())
	    invisible(capture.output(expect_error(summary(x0), NA)))
	    expect_output(summary(x0)$show())
	    x0CodeBased <- eval(parse(text = getObjectRCode(x0, stringWrapParagraphWidth = NULL)))
	    expect_equal(x0CodeBased$alphaSpent, x0$alphaSpent, tolerance = 1e-05)
	    expect_equal(x0CodeBased$criticalValues, x0$criticalValues, tolerance = 1e-05)
	    expect_equal(x0CodeBased$stageLevels, x0$stageLevels, tolerance = 1e-05)
	    expect_type(names(x0), "character")
	    df <- as.data.frame(x0)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x0)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignInverseNormal' with type of design = 'asHSD', 'bsHSD', 'asKD', and 'bsKD'", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingHwangShiDeCani}
	x1 <- getDesignInverseNormal(kMax = 3, informationRates = c(0.2, 0.4, 1), 
		alpha = 0.03, sided = 1, beta = 0.14, typeOfDesign = "asHSD", gammaA = 0) 

	## Comparison of the results of TrialDesignInverseNormal object 'x1' with expected results
	expect_equal(x1$alphaSpent, c(0.006, 0.012, 0.02999999), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(2.5121443, 2.4228747, 2.0280392), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.006, 0.0076991189, 0.021278125), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x1), NA)))
	    expect_output(print(x1)$show())
	    invisible(capture.output(expect_error(summary(x1), NA)))
	    expect_output(summary(x1)$show())
	    x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
	    expect_equal(x1CodeBased$alphaSpent, x1$alphaSpent, tolerance = 1e-05)
	    expect_equal(x1CodeBased$criticalValues, x1$criticalValues, tolerance = 1e-05)
	    expect_equal(x1CodeBased$stageLevels, x1$stageLevels, tolerance = 1e-05)
	    expect_type(names(x1), "character")
	    df <- as.data.frame(x1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignCharacteristics}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y1 <- getDesignCharacteristics(x1)

	## Comparison of the results of TrialDesignCharacteristics object 'y1' with expected results
	expect_equal(y1$nFixed, 8.7681899, tolerance = 1e-07)
	expect_equal(y1$shift, 9.4594102, tolerance = 1e-07)
	expect_equal(y1$inflationFactor, 1.0788327, tolerance = 1e-07)
	expect_equal(y1$information, c(1.891882, 3.7837641, 9.4594102), tolerance = 1e-07)
	expect_equal(y1$power, c(0.12783451, 0.34055165, 0.86), tolerance = 1e-07)
	expect_equal(y1$rejectionProbabilities, c(0.12783451, 0.21271713, 0.51944835), tolerance = 1e-07)
	expect_equal(y1$futilityProbabilities, c(9.8658765e-10, 9.7584074e-10), tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber1, 0.83081135, tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber01, 1.0142116, tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber0, 1.0697705, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y1), NA)))
	    expect_output(print(y1)$show())
	    invisible(capture.output(expect_error(summary(y1), NA)))
	    expect_output(summary(y1)$show())
	    y1CodeBased <- eval(parse(text = getObjectRCode(y1, stringWrapParagraphWidth = NULL)))
	    expect_equal(y1CodeBased$nFixed, y1$nFixed, tolerance = 1e-05)
	    expect_equal(y1CodeBased$shift, y1$shift, tolerance = 1e-05)
	    expect_equal(y1CodeBased$inflationFactor, y1$inflationFactor, tolerance = 1e-05)
	    expect_equal(y1CodeBased$information, y1$information, tolerance = 1e-05)
	    expect_equal(y1CodeBased$power, y1$power, tolerance = 1e-05)
	    expect_equal(y1CodeBased$rejectionProbabilities, y1$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(y1CodeBased$futilityProbabilities, y1$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(y1CodeBased$averageSampleNumber1, y1$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(y1CodeBased$averageSampleNumber01, y1$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(y1CodeBased$averageSampleNumber0, y1$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y1), "character")
	    df <- as.data.frame(y1)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y1)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingHwangShiDeCani}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingHwangShiDeCani}
	x2 <- getDesignInverseNormal(kMax = 3, informationRates = c(0.2, 0.4, 1), 
		alpha = 0.07, sided = 1, beta = 0.14, typeOfDesign = "asHSD", gammaA = -1, 
		typeBetaSpending = "bsHSD", gammaB = -2)

	## Comparison of the results of TrialDesignInverseNormal object 'x2' with expected results
	expect_equal(x2$power, c(0.12038954, 0.32895265, 0.86), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(-1.1063623, -0.35992438), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.0090195874, 0.020036136, 0.06999999), tolerance = 1e-07)
	expect_equal(x2$betaSpent, c(0.010777094, 0.026854629, 0.14), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.364813, 2.1928805, 1.5660474), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.0090195874, 0.014157994, 0.058668761), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x2), NA)))
	    expect_output(print(x2)$show())
	    invisible(capture.output(expect_error(summary(x2), NA)))
	    expect_output(summary(x2)$show())
	    x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
	    expect_equal(x2CodeBased$power, x2$power, tolerance = 1e-05)
	    expect_equal(x2CodeBased$futilityBounds, x2$futilityBounds, tolerance = 1e-05)
	    expect_equal(x2CodeBased$alphaSpent, x2$alphaSpent, tolerance = 1e-05)
	    expect_equal(x2CodeBased$betaSpent, x2$betaSpent, tolerance = 1e-05)
	    expect_equal(x2CodeBased$criticalValues, x2$criticalValues, tolerance = 1e-05)
	    expect_equal(x2CodeBased$stageLevels, x2$stageLevels, tolerance = 1e-05)
	    expect_type(names(x2), "character")
	    df <- as.data.frame(x2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignCharacteristics}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y2 <- getDesignCharacteristics(x2)

	## Comparison of the results of TrialDesignCharacteristics object 'y2' with expected results
	expect_equal(y2$nFixed, 6.5337002, tolerance = 1e-07)
	expect_equal(y2$shift, 7.1015943, tolerance = 1e-07)
	expect_equal(y2$inflationFactor, 1.0869177, tolerance = 1e-07)
	expect_equal(y2$information, c(1.4203189, 2.8406377, 7.1015943), tolerance = 1e-07)
	expect_equal(y2$power, c(0.12038953, 0.32895265, 0.86), tolerance = 1e-07)
	expect_equal(y2$rejectionProbabilities, c(0.12038953, 0.20856311, 0.53104735), tolerance = 1e-07)
	expect_equal(y2$futilityProbabilities, c(0.010777094, 0.016077535), tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber1, 0.82636428, tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber01, 0.91614201, tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber0, 0.79471657, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y2), NA)))
	    expect_output(print(y2)$show())
	    invisible(capture.output(expect_error(summary(y2), NA)))
	    expect_output(summary(y2)$show())
	    y2CodeBased <- eval(parse(text = getObjectRCode(y2, stringWrapParagraphWidth = NULL)))
	    expect_equal(y2CodeBased$nFixed, y2$nFixed, tolerance = 1e-05)
	    expect_equal(y2CodeBased$shift, y2$shift, tolerance = 1e-05)
	    expect_equal(y2CodeBased$inflationFactor, y2$inflationFactor, tolerance = 1e-05)
	    expect_equal(y2CodeBased$information, y2$information, tolerance = 1e-05)
	    expect_equal(y2CodeBased$power, y2$power, tolerance = 1e-05)
	    expect_equal(y2CodeBased$rejectionProbabilities, y2$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(y2CodeBased$futilityProbabilities, y2$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(y2CodeBased$averageSampleNumber1, y2$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(y2CodeBased$averageSampleNumber01, y2$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(y2CodeBased$averageSampleNumber0, y2$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y2), "character")
	    df <- as.data.frame(y2)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y2)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingKimDeMets}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x3 <- getDesignInverseNormal(kMax = 3, informationRates = c(0.3, 0.7, 1), 
		alpha = 0.03, sided = 1, beta = 0.34, typeOfDesign = "asKD", gammaA = 2.2, 
		typeBetaSpending = "bsKD", gammaB = 3.2)

	## Comparison of the results of TrialDesignInverseNormal object 'x3' with expected results
	expect_equal(x3$power, c(0.058336437, 0.39824601, 0.66), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(-1.1558435, 0.72836893), tolerance = 1e-07)
	expect_equal(x3$alphaSpent, c(0.0021222083, 0.013687904, 0.02999999), tolerance = 1e-07)
	expect_equal(x3$betaSpent, c(0.0072155083, 0.1085907, 0.34), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(2.8594012, 2.2435708, 1.9735737), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.0021222083, 0.012430015, 0.02421512), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x3), NA)))
	    expect_output(print(x3)$show())
	    invisible(capture.output(expect_error(summary(x3), NA)))
	    expect_output(summary(x3)$show())
	    x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
	    expect_equal(x3CodeBased$power, x3$power, tolerance = 1e-05)
	    expect_equal(x3CodeBased$futilityBounds, x3$futilityBounds, tolerance = 1e-05)
	    expect_equal(x3CodeBased$alphaSpent, x3$alphaSpent, tolerance = 1e-05)
	    expect_equal(x3CodeBased$betaSpent, x3$betaSpent, tolerance = 1e-05)
	    expect_equal(x3CodeBased$criticalValues, x3$criticalValues, tolerance = 1e-05)
	    expect_equal(x3CodeBased$stageLevels, x3$stageLevels, tolerance = 1e-05)
	    expect_type(names(x3), "character")
	    df <- as.data.frame(x3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignCharacteristics}
	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y3 <- getDesignCharacteristics(x3)

	## Comparison of the results of TrialDesignCharacteristics object 'y3' with expected results
	expect_equal(y3$nFixed, 5.2590265, tolerance = 1e-07)
	expect_equal(y3$shift, 5.5513711, tolerance = 1e-07)
	expect_equal(y3$inflationFactor, 1.0555891, tolerance = 1e-07)
	expect_equal(y3$information, c(1.6654113, 3.8859597, 5.5513711), tolerance = 1e-07)
	expect_equal(y3$power, c(0.058336437, 0.39824601, 0.66), tolerance = 1e-07)
	expect_equal(y3$rejectionProbabilities, c(0.058336437, 0.33990957, 0.26175399), tolerance = 1e-07)
	expect_equal(y3$futilityProbabilities, c(0.0072155083, 0.10137519), tolerance = 1e-07)
	expect_equal(y3$averageSampleNumber1, 0.86740735, tolerance = 1e-07)
	expect_equal(y3$averageSampleNumber01, 0.87361708, tolerance = 1e-07)
	expect_equal(y3$averageSampleNumber0, 0.75480974, tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(y3), NA)))
	    expect_output(print(y3)$show())
	    invisible(capture.output(expect_error(summary(y3), NA)))
	    expect_output(summary(y3)$show())
	    y3CodeBased <- eval(parse(text = getObjectRCode(y3, stringWrapParagraphWidth = NULL)))
	    expect_equal(y3CodeBased$nFixed, y3$nFixed, tolerance = 1e-05)
	    expect_equal(y3CodeBased$shift, y3$shift, tolerance = 1e-05)
	    expect_equal(y3CodeBased$inflationFactor, y3$inflationFactor, tolerance = 1e-05)
	    expect_equal(y3CodeBased$information, y3$information, tolerance = 1e-05)
	    expect_equal(y3CodeBased$power, y3$power, tolerance = 1e-05)
	    expect_equal(y3CodeBased$rejectionProbabilities, y3$rejectionProbabilities, tolerance = 1e-05)
	    expect_equal(y3CodeBased$futilityProbabilities, y3$futilityProbabilities, tolerance = 1e-05)
	    expect_equal(y3CodeBased$averageSampleNumber1, y3$averageSampleNumber1, tolerance = 1e-05)
	    expect_equal(y3CodeBased$averageSampleNumber01, y3$averageSampleNumber01, tolerance = 1e-05)
	    expect_equal(y3CodeBased$averageSampleNumber0, y3$averageSampleNumber0, tolerance = 1e-05)
	    expect_type(names(y3), "character")
	    df <- as.data.frame(y3)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(y3)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignInverseNormal' with binding futility bounds", {

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:criticalValuesWithFutility}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x4 <- getDesignInverseNormal(kMax = 4, alpha = 0.035, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

	## Comparison of the results of TrialDesignInverseNormal object 'x4' with expected results
	expect_equal(x4$alphaSpent, c(0.0099446089, 0.020756912, 0.029001537, 0.03499999), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(2.3284312, 2.1725031, 2.0861776, 2.0270171), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.0099446089, 0.014908866, 0.018481267, 0.021330332), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x4), NA)))
	    expect_output(print(x4)$show())
	    invisible(capture.output(expect_error(summary(x4), NA)))
	    expect_output(summary(x4)$show())
	    x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
	    expect_equal(x4CodeBased$alphaSpent, x4$alphaSpent, tolerance = 1e-05)
	    expect_equal(x4CodeBased$criticalValues, x4$criticalValues, tolerance = 1e-05)
	    expect_equal(x4CodeBased$stageLevels, x4$stageLevels, tolerance = 1e-05)
	    expect_type(names(x4), "character")
	    df <- as.data.frame(x4)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x4)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asUser'", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignInverseNormal}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	x5 <- getDesignGroupSequential(typeOfDesign = "asUser", 
		userAlphaSpending = c(0.01, 0.02, 0.03, 0.05))

	## Comparison of the results of TrialDesignGroupSequential object 'x5' with expected results
	expect_equal(x5$alphaSpent, c(0.01, 0.02, 0.03, 0.04999999), tolerance = 1e-07)
	expect_equal(x5$criticalValues, c(2.3263479, 2.2192994, 2.1201347, 1.8189562), tolerance = 1e-07)
	expect_equal(x5$stageLevels, c(0.01, 0.01323318, 0.016997342, 0.034459058), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x5), NA)))
	    expect_output(print(x5)$show())
	    invisible(capture.output(expect_error(summary(x5), NA)))
	    expect_output(summary(x5)$show())
	    x5CodeBased <- eval(parse(text = getObjectRCode(x5, stringWrapParagraphWidth = NULL)))
	    expect_equal(x5CodeBased$alphaSpent, x5$alphaSpent, tolerance = 1e-05)
	    expect_equal(x5CodeBased$criticalValues, x5$criticalValues, tolerance = 1e-05)
	    expect_equal(x5CodeBased$stageLevels, x5$stageLevels, tolerance = 1e-05)
	    expect_type(names(x5), "character")
	    df <- as.data.frame(x5)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x5)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsUser'", {

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	x6 <- getDesignGroupSequential(kMax = 3, alpha = 0.03, 
		typeOfDesign = "asOF", typeBetaSpending = "bsUser",
		bindingFutility = FALSE,
		userBetaSpending = c(0.01, 0.05, 0.3))

	## Comparison of the results of TrialDesignGroupSequential object 'x6' with expected results
	expect_equal(x6$power, c(0.014685829, 0.33275272, 0.7), tolerance = 1e-07)
	expect_equal(x6$futilityBounds, c(-0.92327973, 0.29975473), tolerance = 1e-07)
	expect_equal(x6$alphaSpent, c(0.00017079385, 0.0078650906, 0.03), tolerance = 1e-07)
	expect_equal(x6$betaSpent, c(0.01, 0.05, 0.3), tolerance = 1e-07)
	expect_equal(x6$criticalValues, c(3.5815302, 2.417863, 1.9175839), tolerance = 1e-07)
	expect_equal(x6$stageLevels, c(0.00017079385, 0.0078059773, 0.027581894), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x6), NA)))
	    expect_output(print(x6)$show())
	    invisible(capture.output(expect_error(summary(x6), NA)))
	    expect_output(summary(x6)$show())
	    x6CodeBased <- eval(parse(text = getObjectRCode(x6, stringWrapParagraphWidth = NULL)))
	    expect_equal(x6CodeBased$power, x6$power, tolerance = 1e-05)
	    expect_equal(x6CodeBased$futilityBounds, x6$futilityBounds, tolerance = 1e-05)
	    expect_equal(x6CodeBased$alphaSpent, x6$alphaSpent, tolerance = 1e-05)
	    expect_equal(x6CodeBased$betaSpent, x6$betaSpent, tolerance = 1e-05)
	    expect_equal(x6CodeBased$criticalValues, x6$criticalValues, tolerance = 1e-05)
	    expect_equal(x6CodeBased$stageLevels, x6$stageLevels, tolerance = 1e-05)
	    expect_type(names(x6), "character")
	    df <- as.data.frame(x6)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x6)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsKD' and binding futility bounds", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingKimDeMets}
	x7 <- getDesignGroupSequential(kMax = 3, alpha = 0.03, 
		typeOfDesign = "asOF", typeBetaSpending = "bsKD",
		informationRates = c(0.4, 0.75, 1),
		gammaB = 2.5, bindingFutility = TRUE)

	## Comparison of the results of TrialDesignGroupSequential object 'x7' with expected results
	expect_equal(x7$power, c(0.068966747, 0.55923121, 0.8), tolerance = 1e-07)
	expect_equal(x7$futilityBounds, c(-0.29391761, 1.0736333), tolerance = 1e-07)
	expect_equal(x7$alphaSpent, c(0.00060088601, 0.012217314, 0.03), tolerance = 1e-07)
	expect_equal(x7$betaSpent, c(0.020238577, 0.097427858, 0.2), tolerance = 1e-07)
	expect_equal(x7$criticalValues, c(3.2384592, 2.2562378, 1.905812), tolerance = 1e-07)
	expect_equal(x7$stageLevels, c(0.00060088601, 0.012027871, 0.0283373), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x7), NA)))
	    expect_output(print(x7)$show())
	    invisible(capture.output(expect_error(summary(x7), NA)))
	    expect_output(summary(x7)$show())
	    x7CodeBased <- eval(parse(text = getObjectRCode(x7, stringWrapParagraphWidth = NULL)))
	    expect_equal(x7CodeBased$power, x7$power, tolerance = 1e-05)
	    expect_equal(x7CodeBased$futilityBounds, x7$futilityBounds, tolerance = 1e-05)
	    expect_equal(x7CodeBased$alphaSpent, x7$alphaSpent, tolerance = 1e-05)
	    expect_equal(x7CodeBased$betaSpent, x7$betaSpent, tolerance = 1e-05)
	    expect_equal(x7CodeBased$criticalValues, x7$criticalValues, tolerance = 1e-05)
	    expect_equal(x7CodeBased$stageLevels, x7$stageLevels, tolerance = 1e-05)
	    expect_type(names(x7), "character")
	    df <- as.data.frame(x7)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x7)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with binding futility bounds ", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesWithFutility}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

	## Comparison of the results of TrialDesignGroupSequential object 'x8' with expected results
	expect_equal(x8$alphaSpent, c(0.0062828133, 0.013876673, 0.02015684, 0.02499999), tolerance = 1e-07)
	expect_equal(x8$criticalValues, c(2.4958485, 2.328709, 2.2361766, 2.1727623), tolerance = 1e-07)
	expect_equal(x8$stageLevels, c(0.0062828133, 0.0099372444, 0.012670104, 0.014899106), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x8), NA)))
	    expect_output(print(x8)$show())
	    invisible(capture.output(expect_error(summary(x8), NA)))
	    expect_output(summary(x8)$show())
	    x8CodeBased <- eval(parse(text = getObjectRCode(x8, stringWrapParagraphWidth = NULL)))
	    expect_equal(x8CodeBased$alphaSpent, x8$alphaSpent, tolerance = 1e-05)
	    expect_equal(x8CodeBased$criticalValues, x8$criticalValues, tolerance = 1e-05)
	    expect_equal(x8CodeBased$stageLevels, x8$stageLevels, tolerance = 1e-05)
	    expect_type(names(x8), "character")
	    df <- as.data.frame(x8)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x8)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with Haybittle Peto boundaries ", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesHaybittlePeto}
	x9 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "HP")

	## Comparison of the results of TrialDesignGroupSequential object 'x9' with expected results
	expect_equal(x9$alphaSpent, c(0.001349898, 0.0024617416, 0.0033695882, 0.025), tolerance = 1e-07)
	expect_equal(x9$criticalValues, c(3, 3, 3, 1.9827514), tolerance = 1e-07)
	expect_equal(x9$stageLevels, c(0.001349898, 0.001349898, 0.001349898, 0.023697604), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x9), NA)))
	    expect_output(print(x9)$show())
	    invisible(capture.output(expect_error(summary(x9), NA)))
	    expect_output(summary(x9)$show())
	    x9CodeBased <- eval(parse(text = getObjectRCode(x9, stringWrapParagraphWidth = NULL)))
	    expect_equal(x9CodeBased$alphaSpent, x9$alphaSpent, tolerance = 1e-05)
	    expect_equal(x9CodeBased$criticalValues, x9$criticalValues, tolerance = 1e-05)
	    expect_equal(x9CodeBased$stageLevels, x9$stageLevels, tolerance = 1e-05)
	    expect_type(names(x9), "character")
	    df <- as.data.frame(x9)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x9)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignGroupSequential' with Pampallona Tsiatis boundaries ", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x10 <- getDesignGroupSequential(kMax = 3, alpha = 0.035, beta = 0.1,  
		informationRates = c(0.3, 0.8, 1), typeOfDesign = "PT", sided = 1, 
		bindingFutility = TRUE, deltaPT1 = 0.2, deltaPT0 = 0.3)

	## Comparison of the results of TrialDesignGroupSequential object 'x10' with expected results
	expect_equal(x10$power, c(0.19834666, 0.83001122, 0.9), tolerance = 1e-07)
	expect_equal(x10$futilityBounds, c(-0.042079551, 1.4407359), tolerance = 1e-07)
	expect_equal(x10$alphaSpent, c(0.0038332428, 0.024917169, 0.035), tolerance = 1e-07)
	expect_equal(x10$betaSpent, c(0.031375367, 0.080734149, 0.099999999), tolerance = 1e-07)
	expect_equal(x10$criticalValues, c(2.6664156, 1.9867225, 1.8580792), tolerance = 1e-07)
	expect_equal(x10$stageLevels, c(0.0038332428, 0.023476576, 0.031578886), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x10), NA)))
	    expect_output(print(x10)$show())
	    invisible(capture.output(expect_error(summary(x10), NA)))
	    expect_output(summary(x10)$show())
	    x10CodeBased <- eval(parse(text = getObjectRCode(x10, stringWrapParagraphWidth = NULL)))
	    expect_equal(x10CodeBased$power, x10$power, tolerance = 1e-05)
	    expect_equal(x10CodeBased$futilityBounds, x10$futilityBounds, tolerance = 1e-05)
	    expect_equal(x10CodeBased$alphaSpent, x10$alphaSpent, tolerance = 1e-05)
	    expect_equal(x10CodeBased$betaSpent, x10$betaSpent, tolerance = 1e-05)
	    expect_equal(x10CodeBased$criticalValues, x10$criticalValues, tolerance = 1e-05)
	    expect_equal(x10CodeBased$stageLevels, x10$stageLevels, tolerance = 1e-05)
	    expect_type(names(x10), "character")
	    df <- as.data.frame(x10)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x10)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x11 <- getDesignGroupSequential(kMax = 3, alpha = 0.035, beta = 0.05,  
		informationRates = c(0.3, 0.8, 1), typeOfDesign = "PT", sided = 2, 
		bindingFutility = TRUE, deltaPT1 = 0.2, deltaPT0 = 0.3)

	## Comparison of the results of TrialDesignGroupSequential object 'x11' with expected results
	expect_equal(x11$power, c(0.16615376, 0.88013007, 0.94999991), tolerance = 1e-07)
	expect_equal(x11$futilityBounds, c(NA_real_, 1.671433), tolerance = 1e-07)
	expect_equal(x11$alphaSpent, c(0.0019236202, 0.022017713, 0.035), tolerance = 1e-07)
	expect_equal(x11$betaSpent, c(0, 0.035025978, 0.05), tolerance = 1e-07)
	expect_equal(x11$criticalValues, c(3.1017782, 2.3111074, 2.1614596), tolerance = 1e-07)
	expect_equal(x11$stageLevels, c(0.00096181012, 0.010413463, 0.015329928), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x11), NA)))
	    expect_output(print(x11)$show())
	    invisible(capture.output(expect_error(summary(x11), NA)))
	    expect_output(summary(x11)$show())
	    x11CodeBased <- eval(parse(text = getObjectRCode(x11, stringWrapParagraphWidth = NULL)))
	    expect_equal(x11CodeBased$power, x11$power, tolerance = 1e-05)
	    expect_equal(x11CodeBased$futilityBounds, x11$futilityBounds, tolerance = 1e-05)
	    expect_equal(x11CodeBased$alphaSpent, x11$alphaSpent, tolerance = 1e-05)
	    expect_equal(x11CodeBased$betaSpent, x11$betaSpent, tolerance = 1e-05)
	    expect_equal(x11CodeBased$criticalValues, x11$criticalValues, tolerance = 1e-05)
	    expect_equal(x11CodeBased$stageLevels, x11$stageLevels, tolerance = 1e-05)
	    expect_type(names(x11), "character")
	    df <- as.data.frame(x11)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x11)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

	# @refFS[Tab.]{fs:tab:output:getDesignGroupSequential}
	# @refFS[Formula]{fs:criticalValuesPampallonaTiatis}
	x12 <- getDesignGroupSequential(kMax = 3, alpha = 0.035, beta = 0.05,  
		informationRates = c(0.3, 0.8, 1), typeOfDesign = "PT", sided = 2, 
		bindingFutility = FALSE, deltaPT1 = 0.2, deltaPT0 = 0.3)

	## Comparison of the results of TrialDesignGroupSequential object 'x12' with expected results
	expect_equal(x12$power, c(0.15712278, 0.87874666, 0.94999995), tolerance = 1e-07)
	expect_equal(x12$futilityBounds, c(NA_real_, 1.7090472), tolerance = 1e-07)
	expect_equal(x12$alphaSpent, c(0.0015647742, 0.019435851, 0.035), tolerance = 1e-07)
	expect_equal(x12$betaSpent, c(0, 0.034947415, 0.05), tolerance = 1e-07)
	expect_equal(x12$criticalValues, c(3.1623945, 2.356272, 2.2036998), tolerance = 1e-07)
	expect_equal(x12$stageLevels, c(0.00078238708, 0.009229697, 0.013772733), tolerance = 1e-07)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(x12), NA)))
	    expect_output(print(x12)$show())
	    invisible(capture.output(expect_error(summary(x12), NA)))
	    expect_output(summary(x12)$show())
	    x12CodeBased <- eval(parse(text = getObjectRCode(x12, stringWrapParagraphWidth = NULL)))
	    expect_equal(x12CodeBased$power, x12$power, tolerance = 1e-05)
	    expect_equal(x12CodeBased$futilityBounds, x12$futilityBounds, tolerance = 1e-05)
	    expect_equal(x12CodeBased$alphaSpent, x12$alphaSpent, tolerance = 1e-05)
	    expect_equal(x12CodeBased$betaSpent, x12$betaSpent, tolerance = 1e-05)
	    expect_equal(x12CodeBased$criticalValues, x12$criticalValues, tolerance = 1e-05)
	    expect_equal(x12CodeBased$stageLevels, x12$stageLevels, tolerance = 1e-05)
	    expect_type(names(x12), "character")
	    df <- as.data.frame(x12)
	    expect_s3_class(df, "data.frame")
	    expect_true(nrow(df) > 0 && ncol(df) > 0)
	    mtx <- as.matrix(x12)
	    expect_true(is.matrix(mtx))
	    expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
	}

})

test_that("'getDesignInverseNormal': illegal arguments throw exceptions as expected", {

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.025), kMax = 4), 
		paste0("Conflicting arguments: length of 'userAlphaSpending' (5) ", 
		"must be equal to 'kMax' (4)"), fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.021)),
		paste0("'userAlphaSpending' = c(0.01, 0.02, 0.023, 0.023, 0.021) must be a vector that ", 
		"satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_5 <= alpha = 0.021"), fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = "asUser", 
		userAlphaSpending = c(0.01, 0.02, 0.023), alpha = 0.02),
		paste0("'userAlphaSpending' = c(0.01, 0.02, 0.023) must be a vector that ", 
		"satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_3 <= alpha = 0.02"), fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_WT, deltaWT = NA_real_), 
		"Missing argument: parameter 'deltaWT' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_WT_OPTIMUM, 
		optimizationCriterion = "x"), 
		"Illegal argument: optimization criterion must be one of the following: 'ASNH1', 'ASNIFH1', 'ASNsum'", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_KD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_HSD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER), 
		"Missing argument: parameter 'userAlphaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = "x"), 
		"Illegal argument: type of beta spending must be one of the following: 'none', 'bsP', 'bsOF', 'bsKD', 'bsHSD', 'bsUser'", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER), 
		"Missing argument: parameter 'userBetaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
		userBetaSpending = c(0.1, 0.2)),
		paste0("Conflicting arguments: length of 'userBetaSpending' (2) must ",
		"be equal to length of 'informationRates' (3)"), fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
		userBetaSpending = c(0.2, 0.1, 0.05)),
		paste0("'userBetaSpending' = c(0.2, 0.1, 0.05) must be a vector that satisfies the ", 
		"following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.05"), fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
		userBetaSpending = c(0.1, 0.2, 0.3), beta = 0.2),
		paste0("'userBetaSpending' = c(0.1, 0.2, 0.3) must be a vector that satisfies the ", 
		"following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.2"), fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = Inf), 
		paste0("Argument out of bounds: 'kMax' (Inf) is out of bounds [1; ",  
		C_KMAX_UPPER_BOUND, "]"), fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = -Inf), 
		paste0("Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; ",  
		C_KMAX_UPPER_BOUND, "]"), fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = -Inf), "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -10), "Argument out of bounds: 'kMax' (-10) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -9), "Argument out of bounds: 'kMax' (-9) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -8), "Argument out of bounds: 'kMax' (-8) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -7), "Argument out of bounds: 'kMax' (-7) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -6), "Argument out of bounds: 'kMax' (-6) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -4), "Argument out of bounds: 'kMax' (-4) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -3), "Argument out of bounds: 'kMax' (-3) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -2), "Argument out of bounds: 'kMax' (-2) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -1), "Argument out of bounds: 'kMax' (-1) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 21), "Argument out of bounds: 'kMax' (21) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 22), "Argument out of bounds: 'kMax' (22) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 23), "Argument out of bounds: 'kMax' (23) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 24), "Argument out of bounds: 'kMax' (24) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 25), "Argument out of bounds: 'kMax' (25) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 26), "Argument out of bounds: 'kMax' (26) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 27), "Argument out of bounds: 'kMax' (27) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 28), "Argument out of bounds: 'kMax' (28) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 29), "Argument out of bounds: 'kMax' (29) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 30), "Argument out of bounds: 'kMax' (30) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = Inf), "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; 20]", fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = 2, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (2) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 3, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (3) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 4, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (4) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 6, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (6) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 7, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (7) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 8, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (8) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 9, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (9) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 10, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (10) - 1", fixed = TRUE)

	expect_warning(expect_error(getDesignInverseNormal(kMax = 11, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (11) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 12, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (12) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 13, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (13) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 14, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (14) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 15, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (15) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 16, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (16) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 17, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (17) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 18, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (18) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 19, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (19) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 20, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (20) - 1", fixed = TRUE))

	expect_error(getDesignInverseNormal(futilityBounds = c(-7, 5)), 
		"Illegal argument: 'futilityBounds' (-7, 5) too extreme for this situation", fixed = TRUE)

	expect_error(getDesignInverseNormal(futilityBounds = c(1, 7)), 
		"Argument out of bounds: 'futilityBounds' (1, 7) is out of bounds [-Inf; 6]", fixed = TRUE)

})

test_that("'getDesignGroupSequential': illegal arguments throw exceptions as expected", {

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.025), kMax = 4), 
		paste0("Conflicting arguments: length of 'userAlphaSpending' (5) ", 
		"must be equal to 'kMax' (4)"), fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.021)),
		paste0("'userAlphaSpending' = c(0.01, 0.02, 0.023, 0.023, 0.021) must be a vector that ", 
		"satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_5 <= alpha = 0.021"), fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = "asUser", 
		userAlphaSpending = c(0.01, 0.02, 0.023), alpha = 0.02),
		paste0("'userAlphaSpending' = c(0.01, 0.02, 0.023) must be a vector that ", 
		"satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_3 <= alpha = 0.02"), fixed = TRUE)

	expect_equal(getDesignGroupSequential(typeOfDesign = "asUser", 
		userAlphaSpending = c(0.01, 0.02, 0.023))$alpha, 0.023)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_WT, deltaWT = NA_real_), 
		"Missing argument: parameter 'deltaWT' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_WT_OPTIMUM, 
		optimizationCriterion = "x"), 
		"Illegal argument: optimization criterion must be one of the following: 'ASNH1', 'ASNIFH1', 'ASNsum'", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_KD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_HSD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER), 
		"Missing argument: parameter 'userAlphaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = "x"), 
		paste0("Illegal argument: type of beta spending must be one of the following: ",
		"'none', 'bsP', 'bsOF', 'bsKD', 'bsHSD', 'bsUser'"), fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER), 
		"Missing argument: parameter 'userBetaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
		userBetaSpending = c(0.1, 0.2)),
		paste0("Conflicting arguments: length of 'userBetaSpending' (2) must ",
		"be equal to length of 'informationRates' (3)"), fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
		userBetaSpending = c(0.2, 0.1, 0.05)),
		paste0("'userBetaSpending' = c(0.2, 0.1, 0.05) must be a vector that satisfies the ", 
		"following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.05"), fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
		userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER,
		userBetaSpending = c(0.1, 0.2, 0.3), beta = 0.2),
		paste0("'userBetaSpending' = c(0.1, 0.2, 0.3) must be a vector that satisfies the ", 
		"following condition: 0 <= beta_1 <= .. <= beta_3 <= beta = 0.2"), fixed = TRUE)

	expect_error(getDesignGroupSequential(kMax = Inf), 
		paste0("Argument out of bounds: 'kMax' (Inf) is out of bounds [1; ",  
		C_KMAX_UPPER_BOUND, "]"), fixed = TRUE)

	expect_error(getDesignGroupSequential(kMax = -Inf), 
		paste0("Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; ",  
		C_KMAX_UPPER_BOUND, "]"), fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 20]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 21), "Argument out of bounds: 'kMax' (21) is out of bounds [1; 20]", fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = 2, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (2) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 3, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (3) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 4, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (4) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 6, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (6) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 7, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (7) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 8, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (8) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 9, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (9) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 10, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (10) - 1", fixed = TRUE)

	expect_warning(expect_error(getDesignInverseNormal(kMax = 11, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (11) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 12, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (12) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 13, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (13) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 14, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (14) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 15, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (15) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 16, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (16) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 17, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (17) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 18, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (18) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 19, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (19) - 1", fixed = TRUE))
	expect_warning(expect_error(getDesignInverseNormal(kMax = 20, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (20) - 1", fixed = TRUE))

	expect_error(getDesignGroupSequential(futilityBounds = c(-7, 5)), 
		"Illegal argument: 'futilityBounds' (-7, 5) too extreme for this situation", fixed = TRUE)

	expect_error(getDesignGroupSequential(futilityBounds = c(1, 7)),  
		"Argument out of bounds: 'futilityBounds' (1, 7) is out of bounds [-Inf; 6]", fixed = TRUE)

})

