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
## |  File name: test-f_analysis_enrichment_survival.R
## |  Creation date: 12 September 2024, 12:51:31
## |  File version: $Revision: 8200 $
## |  Last changed: $Date: 2024-09-12 15:05:38 +0200 (Do, 12 Sep 2024) $
## |  Last changed by: $Author: pahlke $
## |

test_plan_section("Testing Analysis Enrichment Survival Function")


test_that("'getAnalysisResults': enrichment survival, one sub-population, non-stratified analysis, select S1 at second, gMax = 2", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentSurvival}
    # @refFS[Formula]{fs:computeRCIsEnrichment}
    # @refFS[Formula]{fs:conditionalPowerEnrichment}
    # @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
    # @refFS[Formula]{fs:stratifiedTestEnrichmentSurvival}
    # @refFS[Formula]{fs:testStatisticEnrichmentSurvival}
    S1 <- getDataset(
        events = c(37, 35, 22),
        logRanks = c(1.66, 1.38, 1.22),
        allocationRatios = c(1, 1, 1)
    )

    F <- getDataset(
        events = c(66, 55, NA),
        logRanks = c(1.98, 1.57, NA),
        allocationRatios = c(1, 1, NA)
    )

    dataInput1 <- getDataset(S1 = S1, F = F)

    ## Comparison of the results of DatasetSurvival object 'dataInput1' with expected results
    expect_equal(dataInput1$events, c(37, 66, 35, 55, 22, NA_real_), label = paste0(dataInput1$events))
    expect_equal(dataInput1$allocationRatios, c(1, 1, 1, 1, 1, NA_real_), tolerance = 1e-07, label = paste0(dataInput1$allocationRatios))
    expect_equal(dataInput1$logRanks, c(1.66, 1.98, 1.38, 1.57, 1.22, NA_real_), tolerance = 1e-07, label = paste0(dataInput1$logRanks))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(dataInput1), NA)))
        expect_output(print(dataInput1)$show())
        invisible(capture.output(expect_error(summary(dataInput1), NA)))
        expect_output(summary(dataInput1)$show())
        dataInput1CodeBased <- eval(parse(text = getObjectRCode(dataInput1, stringWrapParagraphWidth = NULL)))
        expect_equal(dataInput1CodeBased$events, dataInput1$events, tolerance = 1e-07)
        expect_equal(dataInput1CodeBased$allocationRatios, dataInput1$allocationRatios, tolerance = 1e-07)
        expect_equal(dataInput1CodeBased$logRanks, dataInput1$logRanks, tolerance = 1e-07)
        expect_type(names(dataInput1), "character")
        df <- as.data.frame(dataInput1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(dataInput1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    design1 <- getDesignInverseNormal(
        kMax = 3, typeOfDesign = "asP", typeBetaSpending = "bsKD", gammaB = 1.3, alpha = 0.025,
        informationRates = c(0.4, 0.7, 1), bindingFutility = FALSE, beta = 0.1
    )

    x1 <- getAnalysisResults(
        design = design1,
        dataInput = dataInput1,
        directionUpper = TRUE,
        stage = 3,
        allocationRatioPlanned = 1,
        intersectionTest = "SpiessensDebois"
    )

    ## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
    expect_equal(x1$thetaH1[1, ], 1.6657832, tolerance = 1e-07, label = paste0(x1$thetaH1[1, ]))
    expect_equal(x1$thetaH1[2, ], NA_real_, label = paste0(x1$thetaH1[2, ]))
    expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.082268614, 0.17873234, NA_real_), tolerance = 1e-07, label = paste0(x1$conditionalRejectionProbabilities[1, ]))
    expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.10062355, 0.20651301, NA_real_), tolerance = 1e-07, label = paste0(x1$conditionalRejectionProbabilities[2, ]))
    expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x1$conditionalPower[1, ]))
    expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x1$conditionalPower[2, ]))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(0.77807561, 0.90042934, 0.98057987), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ]))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(0.89663851, 0.98596182, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ]))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(3.8287578, 3.0779077, 2.841847), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ]))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(2.9564444, 2.541245, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ]))
    expect_equal(x1$repeatedPValues[1, ], c(0.09262834, 0.035310721, 0.016798032), tolerance = 1e-07, label = paste0(x1$repeatedPValues[1, ]))
    expect_equal(x1$repeatedPValues[2, ], c(0.074049848, 0.03027247, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedPValues[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-06)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-06)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-06)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    .skipTestIfDisabled()

    x2 <- getAnalysisResults(
        design = design1,
        dataInput = dataInput1,
        directionUpper = TRUE,
        stage = 3,
        allocationRatioPlanned = 1,
        intersectionTest = "Sidak"
    )

    ## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x2' with expected results
    expect_equal(x2$thetaH1[1, ], 1.6657832, tolerance = 1e-07, label = paste0(x2$thetaH1[1, ]))
    expect_equal(x2$thetaH1[2, ], NA_real_, label = paste0(x2$thetaH1[2, ]))
    expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.082268614, 0.14135111, NA_real_), tolerance = 1e-07, label = paste0(x2$conditionalRejectionProbabilities[1, ]))
    expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.08442718, 0.14135111, NA_real_), tolerance = 1e-07, label = paste0(x2$conditionalRejectionProbabilities[2, ]))
    expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x2$conditionalPower[1, ]))
    expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x2$conditionalPower[2, ]))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(0.76355966, 0.87078132, 0.95099133), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ]))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(0.88408373, 0.96064864, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ]))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(3.9015478, 3.1815164, 2.9283489), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ]))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(2.9984281, 2.606883, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ]))
    expect_equal(x2$repeatedPValues[1, ], c(0.09262834, 0.044241863, 0.02067471), tolerance = 1e-07, label = paste0(x2$repeatedPValues[1, ]))
    expect_equal(x2$repeatedPValues[2, ], c(0.090100155, 0.044241863, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedPValues[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$thetaH1, x2$thetaH1, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    design2 <- getDesignFisher(kMax = 3, method = "equalAlpha", alpha = 0.025, informationRates = c(0.4, 0.7, 1))

    x3 <- getAnalysisResults(
        design = design2,
        dataInput = dataInput1,
        stratifiedAnalysis = TRUE,
        directionUpper = TRUE,
        stage = 2,
        nPlanned = 30,
        allocationRatioPlanned = 1,
        intersectionTest = "SpiessensDebois"
    )

    ## Comparison of the results of AnalysisResultsEnrichmentFisher object 'x3' with expected results
    expect_equal(x3$thetaH1[1, ], 1.6607445, tolerance = 1e-07, label = paste0(x3$thetaH1[1, ]))
    expect_equal(x3$thetaH1[2, ], 1.5814324, tolerance = 1e-07, label = paste0(x3$thetaH1[2, ]))
    expect_equal(x3$conditionalRejectionProbabilities[1, ], c(0.058300881, 0.080849353, NA_real_), tolerance = 1e-07, label = paste0(x3$conditionalRejectionProbabilities[1, ]))
    expect_equal(x3$conditionalRejectionProbabilities[2, ], c(0.073230444, 0.10089716, NA_real_), tolerance = 1e-07, label = paste0(x3$conditionalRejectionProbabilities[2, ]))
    expect_equal(x3$conditionalPower[1, ], c(NA_real_, NA_real_, 0.49594042), tolerance = 1e-07, label = paste0(x3$conditionalPower[1, ]))
    expect_equal(x3$conditionalPower[2, ], c(NA_real_, NA_real_, 0.49151717), tolerance = 1e-07, label = paste0(x3$conditionalPower[2, ]))
    expect_equal(x3$repeatedConfidenceIntervalLowerBounds[1, ], c(0.77887293, 0.87495539, NA_real_), tolerance = 1e-07, label = paste0(x3$repeatedConfidenceIntervalLowerBounds[1, ]))
    expect_equal(x3$repeatedConfidenceIntervalLowerBounds[2, ], c(0.89732572, 0.9655589, NA_real_), tolerance = 1e-07, label = paste0(x3$repeatedConfidenceIntervalLowerBounds[2, ]))
    expect_equal(x3$repeatedConfidenceIntervalUpperBounds[1, ], c(3.8248388, 3.1694642, NA_real_), tolerance = 1e-07, label = paste0(x3$repeatedConfidenceIntervalUpperBounds[1, ]))
    expect_equal(x3$repeatedConfidenceIntervalUpperBounds[2, ], c(2.9541783, 2.6004037, NA_real_), tolerance = 1e-07, label = paste0(x3$repeatedConfidenceIntervalUpperBounds[2, ]))
    expect_equal(x3$repeatedPValues[1, ], c(0.086600177, 0.047636937, NA_real_), tolerance = 1e-07, label = paste0(x3$repeatedPValues[1, ]))
    expect_equal(x3$repeatedPValues[2, ], c(0.070085432, 0.040357555, NA_real_), tolerance = 1e-07, label = paste0(x3$repeatedPValues[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x3), NA)))
        expect_output(print(x3)$show())
        invisible(capture.output(expect_error(summary(x3), NA)))
        expect_output(summary(x3)$show())
        x3CodeBased <- eval(parse(text = getObjectRCode(x3, stringWrapParagraphWidth = NULL)))
        expect_equal(x3CodeBased$thetaH1, x3$thetaH1, tolerance = 1e-06)
        expect_equal(x3CodeBased$conditionalRejectionProbabilities, x3$conditionalRejectionProbabilities, tolerance = 1e-06)
        expect_equal(x3CodeBased$conditionalPower, x3$conditionalPower, tolerance = 1e-06)
        expect_equal(x3CodeBased$repeatedConfidenceIntervalLowerBounds, x3$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-06)
        expect_equal(x3CodeBased$repeatedConfidenceIntervalUpperBounds, x3$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-06)
        expect_equal(x3CodeBased$repeatedPValues, x3$repeatedPValues, tolerance = 1e-06)
        expect_type(names(x3), "character")
        df <- as.data.frame(x3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getAnalysisResults': enrichment survival, one sub-population, stratified data input, select S1 at first, gMax = 2", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentSurvival}
    # @refFS[Formula]{fs:computeRCIsEnrichment}
    # @refFS[Formula]{fs:conditionalPowerEnrichment}
    # @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
    # @refFS[Formula]{fs:stratifiedTestEnrichmentSurvival}
    # @refFS[Formula]{fs:testStatisticEnrichmentSurvival}
    S1 <- getDataset(
        overallExpectedEvents = c(13.4, 35.4, 43.7),
        overallEvents = c(16, 38, 47),
        overallVarianceEvents = c(2.8, 4.7, 3.4),
        overallAllocationRatios = c(1, 1, 1)
    )

    R <- getDataset(
        overallExpectedEvents = c(23.3, NA, NA),
        overallEvents = c(27, NA, NA),
        overallVarianceEvents = c(3.9, NA, NA),
        overallAllocationRatios = c(1, NA, NA)
    )

    dataInput2 <- getDataset(S1 = S1, R = R)

    ## Comparison of the results of DatasetEnrichmentSurvival object 'dataInput2' with expected results
    expect_equal(dataInput2$events, c(16, 27, 22, NA_real_, 9, NA_real_), label = paste0(dataInput2$events))
    expect_equal(dataInput2$allocationRatios, c(1, 1, 1, NA_real_, 1, NA_real_), tolerance = 1e-07, label = paste0(dataInput2$allocationRatios))
    expect_equal(dataInput2$expectedEvents, c(13.4, 23.3, 22, NA_real_, 8.3, NA_real_), tolerance = 1e-07, label = paste0(dataInput2$expectedEvents))
    expect_equal(dataInput2$varianceEvents, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(dataInput2$varianceEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(dataInput2), NA)))
        expect_output(print(dataInput2)$show())
        invisible(capture.output(expect_error(summary(dataInput2), NA)))
        expect_output(summary(dataInput2)$show())
        dataInput2CodeBased <- eval(parse(text = getObjectRCode(dataInput2, stringWrapParagraphWidth = NULL)))
        expect_equal(dataInput2CodeBased$events, dataInput2$events, tolerance = 1e-07)
        expect_equal(dataInput2CodeBased$allocationRatios, dataInput2$allocationRatios, tolerance = 1e-07)
        expect_equal(dataInput2CodeBased$expectedEvents, dataInput2$expectedEvents, tolerance = 1e-07)
        expect_equal(dataInput2CodeBased$varianceEvents, dataInput2$varianceEvents, tolerance = 1e-07)
        expect_type(names(dataInput2), "character")
        df <- as.data.frame(dataInput2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(dataInput2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    design1 <- getDesignInverseNormal(
        kMax = 3, typeOfDesign = "asP",
        typeBetaSpending = "bsKD", gammaB = 1.3, alpha = 0.025,
        informationRates = c(0.4, 0.7, 1), bindingFutility = FALSE, beta = 0.1
    )

    x4 <- getAnalysisResults(
        design = design1,
        dataInput = dataInput2,
        stratifiedAnalysis = TRUE,
        directionUpper = TRUE,
        stage = 2,
        nPlanned = 30,
        thetaH1 = 2.5,
        allocationRatioPlanned = 1,
        intersectionTest = "SpiessensDebois"
    )


    ## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x4' with expected results
    expect_equal(x4$conditionalRejectionProbabilities[1, ], c(0.066531397, 0.014937437, NA_real_), tolerance = 1e-07, label = paste0(x4$conditionalRejectionProbabilities[1, ]))
    expect_equal(x4$conditionalRejectionProbabilities[2, ], c(0.21112053, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x4$conditionalRejectionProbabilities[2, ]))
    expect_equal(x4$conditionalPower[1, ], c(NA_real_, NA_real_, 0.63217527), tolerance = 1e-07, label = paste0(x4$conditionalPower[1, ]))
    expect_equal(x4$conditionalPower[2, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x4$conditionalPower[2, ]))
    expect_equal(x4$repeatedConfidenceIntervalLowerBounds[1, ], c(0.63930031, 0.68758378, NA_real_), tolerance = 1e-07, label = paste0(x4$repeatedConfidenceIntervalLowerBounds[1, ]))
    expect_equal(x4$repeatedConfidenceIntervalLowerBounds[2, ], c(0.99553933, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x4$repeatedConfidenceIntervalLowerBounds[2, ]))
    expect_equal(x4$repeatedConfidenceIntervalUpperBounds[1, ], c(7.3977709, 3.5674239, NA_real_), tolerance = 1e-07, label = paste0(x4$repeatedConfidenceIntervalUpperBounds[1, ]))
    expect_equal(x4$repeatedConfidenceIntervalUpperBounds[2, ], c(4.4332679, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x4$repeatedConfidenceIntervalUpperBounds[2, ]))
    expect_equal(x4$repeatedPValues[1, ], c(0.11491566, 0.11491566, NA_real_), tolerance = 1e-07, label = paste0(x4$repeatedPValues[1, ]))
    expect_equal(x4$repeatedPValues[2, ], c(0.026005739, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x4$repeatedPValues[2, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x4), NA)))
        expect_output(print(x4)$show())
        invisible(capture.output(expect_error(summary(x4), NA)))
        expect_output(summary(x4)$show())
        x4CodeBased <- eval(parse(text = getObjectRCode(x4, stringWrapParagraphWidth = NULL)))
        expect_equal(x4CodeBased$conditionalRejectionProbabilities, x4$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x4CodeBased$conditionalPower, x4$conditionalPower, tolerance = 1e-07)
        expect_equal(x4CodeBased$repeatedConfidenceIntervalLowerBounds, x4$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x4CodeBased$repeatedConfidenceIntervalUpperBounds, x4$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x4CodeBased$repeatedPValues, x4$repeatedPValues, tolerance = 1e-07)
        expect_type(names(x4), "character")
        df <- as.data.frame(x4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})

test_that("'getAnalysisResults': enrichment survival, two sub-populations, non-stratified analysis, select S1 and S2 at first IA, select S1 at second, gMax = 3", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
    # @refFS[Formula]{fs:computeRCIsEnrichment}
    # @refFS[Formula]{fs:conditionalPowerEnrichment}
    # @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
    # @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
    # @refFS[Formula]{fs:testStatisticEnrichmentRates}
    design1 <- getDesignInverseNormal(
        kMax = 3, typeOfDesign = "asP", typeBetaSpending = "bsKD", gammaB = 1.3, alpha = 0.02,
        informationRates = c(0.4, 0.7, 1), bindingFutility = FALSE, beta = 0.1
    )

    design1b <- getDesignInverseNormal(
        kMax = 3, typeOfDesign = "asP", typeBetaSpending = "bsKD", gammaB = 1.3, alpha = 0.02,
        informationRates = c(0.4, 0.7, 1), bindingFutility = FALSE, beta = 0.1,
        directionUpper = FALSE
    )

    F <- getDataset(
        events = c(66, NA, NA),
        logRanks = -c(2.18, NA, NA)
    )

    S1 <- getDataset(
        events = c(37, 13, 26),
        logRanks = -c(1.66, 1.239, 0.785)
    )

    S2 <- getDataset(
        events = c(31, 18, NA),
        logRanks = -c(1.98, 1.064, NA)
    )

    dataInput3 <- getDataset(S1 = S1, S2 = S2, F = F)

    ## Comparison of the results of DatasetSurvival object 'dataInput3' with expected results
    expect_equal(dataInput3$events, c(37, 31, 66, 13, 18, NA_real_, 26, NA_real_, NA_real_), label = paste0(dataInput3$events))
    expect_equal(dataInput3$allocationRatios, c(1, 1, 1, 1, 1, NA_real_, 1, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(dataInput3$allocationRatios))
    expect_equal(dataInput3$logRanks, c(-1.66, -1.98, -2.18, -1.239, -1.064, NA_real_, -0.785, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(dataInput3$logRanks))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(dataInput3), NA)))
        expect_output(print(dataInput3)$show())
        invisible(capture.output(expect_error(summary(dataInput3), NA)))
        expect_output(summary(dataInput3)$show())
        dataInput3CodeBased <- eval(parse(text = getObjectRCode(dataInput3, stringWrapParagraphWidth = NULL)))
        expect_equal(dataInput3CodeBased$events, dataInput3$events, tolerance = 1e-07)
        expect_equal(dataInput3CodeBased$allocationRatios, dataInput3$allocationRatios, tolerance = 1e-07)
        expect_equal(dataInput3CodeBased$logRanks, dataInput3$logRanks, tolerance = 1e-07)
        expect_type(names(dataInput3), "character")
        df <- as.data.frame(dataInput3)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(dataInput3)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x1 <- getAnalysisResults(
        design = design1,
        dataInput = dataInput3,
        directionUpper = FALSE,
        stage = 2,
        nPlanned = 30,
        allocationRatioPlanned = 1,
        intersectionTest = "Sidak"
    )

    ## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x1' with expected results
    expect_equal(x1$thetaH1[1, ], 0.55845203, tolerance = 1e-07, label = paste0(x1$thetaH1[1, ]))
    expect_equal(x1$thetaH1[2, ], 0.53035001, tolerance = 1e-07, label = paste0(x1$thetaH1[2, ]))
    expect_equal(x1$thetaH1[3, ], NA_real_, label = paste0(x1$thetaH1[3, ]))
    expect_equal(x1$conditionalRejectionProbabilities[1, ], c(0.063444981, 0.051842822, NA_real_), tolerance = 1e-07, label = paste0(x1$conditionalRejectionProbabilities[1, ]))
    expect_equal(x1$conditionalRejectionProbabilities[2, ], c(0.065210901, 0.051842822, NA_real_), tolerance = 1e-07, label = paste0(x1$conditionalRejectionProbabilities[2, ]))
    expect_equal(x1$conditionalRejectionProbabilities[3, ], c(0.070888966, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x1$conditionalRejectionProbabilities[3, ]))
    expect_equal(x1$conditionalPower[1, ], c(NA_real_, NA_real_, 0.48733039), tolerance = 1e-07, label = paste0(x1$conditionalPower[1, ]))
    expect_equal(x1$conditionalPower[2, ], c(NA_real_, NA_real_, 0.54365075), tolerance = 1e-07, label = paste0(x1$conditionalPower[2, ]))
    expect_equal(x1$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x1$conditionalPower[3, ]))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds[1, ], c(0.23870487, 0.2370187, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalLowerBounds[1, ]))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds[2, ], c(0.1863782, 0.22932092, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalLowerBounds[2, ]))
    expect_equal(x1$repeatedConfidenceIntervalLowerBounds[3, ], c(0.30101352, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalLowerBounds[3, ]))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds[1, ], c(1.406238, 1.2861572, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalUpperBounds[1, ]))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds[2, ], c(1.2936975, 1.2386982, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalUpperBounds[2, ]))
    expect_equal(x1$repeatedConfidenceIntervalUpperBounds[3, ], c(1.1356925, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedConfidenceIntervalUpperBounds[3, ]))
    expect_equal(x1$repeatedPValues[1, ], c(0.09262834, 0.074349301, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedPValues[1, ]))
    expect_equal(x1$repeatedPValues[2, ], c(0.090100155, 0.074349301, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedPValues[2, ]))
    expect_equal(x1$repeatedPValues[3, ], c(0.082670093, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x1$repeatedPValues[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x1), NA)))
        expect_output(print(x1)$show())
        invisible(capture.output(expect_error(summary(x1), NA)))
        expect_output(summary(x1)$show())
        x1CodeBased <- eval(parse(text = getObjectRCode(x1, stringWrapParagraphWidth = NULL)))
        expect_equal(x1CodeBased$thetaH1, x1$thetaH1, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalRejectionProbabilities, x1$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x1CodeBased$conditionalPower, x1$conditionalPower, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalLowerBounds, x1$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedConfidenceIntervalUpperBounds, x1$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x1CodeBased$repeatedPValues, x1$repeatedPValues, tolerance = 1e-07)
        expect_type(names(x1), "character")
        df <- as.data.frame(x1)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x1)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    x1b <- getAnalysisResults(
        design = design1b,
        dataInput = dataInput3,
        stage = 2,
        nPlanned = 30,
        allocationRatioPlanned = 1,
        intersectionTest = "Sidak"
    )

    ## Pairwise comparison of the results of x1 with the results of x1b
    expect_equal(x1b$thetaH1[1, ], x1$thetaH1[1, ], tolerance = 1e-07)
    expect_equal(x1b$thetaH1[2, ], x1$thetaH1[2, ], tolerance = 1e-07)
    expect_equal(x1b$thetaH1[3, ], x1$thetaH1[3, ])
    expect_equal(x1b$conditionalRejectionProbabilities[1, ], x1$conditionalRejectionProbabilities[1, ], tolerance = 1e-07)
    expect_equal(x1b$conditionalRejectionProbabilities[2, ], x1$conditionalRejectionProbabilities[2, ], tolerance = 1e-07)
    expect_equal(x1b$conditionalRejectionProbabilities[3, ], x1$conditionalRejectionProbabilities[3, ], tolerance = 1e-07)
    expect_equal(x1b$conditionalPower[1, ], x1$conditionalPower[1, ], tolerance = 1e-07)
    expect_equal(x1b$conditionalPower[2, ], x1$conditionalPower[2, ], tolerance = 1e-07)
    expect_equal(x1b$conditionalPower[3, ], x1$conditionalPower[3, ])
    expect_equal(x1b$repeatedConfidenceIntervalLowerBounds[1, ], x1$repeatedConfidenceIntervalLowerBounds[1, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedConfidenceIntervalLowerBounds[2, ], x1$repeatedConfidenceIntervalLowerBounds[2, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedConfidenceIntervalLowerBounds[3, ], x1$repeatedConfidenceIntervalLowerBounds[3, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedConfidenceIntervalUpperBounds[1, ], x1$repeatedConfidenceIntervalUpperBounds[1, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedConfidenceIntervalUpperBounds[2, ], x1$repeatedConfidenceIntervalUpperBounds[2, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedConfidenceIntervalUpperBounds[3, ], x1$repeatedConfidenceIntervalUpperBounds[3, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedPValues[1, ], x1$repeatedPValues[1, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedPValues[2, ], x1$repeatedPValues[2, ], tolerance = 1e-07)
    expect_equal(x1b$repeatedPValues[3, ], x1$repeatedPValues[3, ], tolerance = 1e-07)
})

test_that("'getAnalysisResults': enrichment survival, two sub-populations, stratified analysis, select S1 and S2 at first IA, select S1 at second, gMax = 3", {
    .skipTestIfDisabled()

    # @refFS[Formula]{fs:adjustedPValueBonferroniEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCIBonferroniSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueForRCISpiessensEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSidakEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSimesEnrichment}
    # @refFS[Formula]{fs:adjustedPValueSpiessensDeboisEnrichmentRates}
    # @refFS[Formula]{fs:computeRCIsEnrichment}
    # @refFS[Formula]{fs:conditionalPowerEnrichment}
    # @refFS[Formula]{fs:conditionalRejectionProbabilityEnrichment}
    # @refFS[Formula]{fs:stratifiedTestEnrichmentRates}
    # @refFS[Formula]{fs:testStatisticEnrichmentRates}
    S1 <- getDataset(
        overallExpectedEvents = c(13.4, 35.4, 43.7),
        overallEvents = c(16, 37, 47),
        overallVarianceEvents = c(2.8, 4.7, 3.4),
        overallAllocationRatios = c(1, 1, 1)
    )

    S2 <- getDataset(
        overallExpectedEvents = c(11.5, 31.1, NA),
        overallEvents = c(15, 33, NA),
        overallVarianceEvents = c(2.2, 4.4, NA),
        overallAllocationRatios = c(1, 1, NA)
    )

    S12 <- getDataset(
        overallExpectedEvents = c(10.1, 29.6, 39.1),
        overallEvents = c(11, 31, 42),
        overallVarianceEvents = c(2.8, 4.7, 3.4),
        overallAllocationRatios = c(1, 1, 1)
    )

    R <- getDataset(
        overallExpectedEvents = c(23.3, NA, NA),
        overallEvents = c(25, NA, NA),
        overallVarianceEvents = c(3.9, NA, NA),
        overallAllocationRatios = c(1, NA, NA)
    )

    dataInput4 <- getDataset(S1 = S1, S2 = S2, S12 = S12, R = R)

    ## Comparison of the results of DatasetEnrichmentSurvival object 'dataInput4' with expected results
    expect_equal(dataInput4$events, c(16, 15, 11, 25, 21, 18, 20, NA_real_, 10, NA_real_, 11, NA_real_), label = paste0(dataInput4$events))
    expect_equal(dataInput4$allocationRatios, c(1, 1, 1, 1, 1, 1, 1, NA_real_, 1, NA_real_, 1, NA_real_), tolerance = 1e-07, label = paste0(dataInput4$allocationRatios))
    expect_equal(dataInput4$expectedEvents, c(13.4, 11.5, 10.1, 23.3, 22, 19.6, 19.5, NA_real_, 8.3, NA_real_, 9.5, NA_real_), tolerance = 1e-07, label = paste0(dataInput4$expectedEvents))
    expect_equal(dataInput4$varianceEvents, c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), label = paste0(dataInput4$varianceEvents))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(dataInput4), NA)))
        expect_output(print(dataInput4)$show())
        invisible(capture.output(expect_error(summary(dataInput4), NA)))
        expect_output(summary(dataInput4)$show())
        dataInput4CodeBased <- eval(parse(text = getObjectRCode(dataInput4, stringWrapParagraphWidth = NULL)))
        expect_equal(dataInput4CodeBased$events, dataInput4$events, tolerance = 1e-07)
        expect_equal(dataInput4CodeBased$allocationRatios, dataInput4$allocationRatios, tolerance = 1e-07)
        expect_equal(dataInput4CodeBased$expectedEvents, dataInput4$expectedEvents, tolerance = 1e-07)
        expect_equal(dataInput4CodeBased$varianceEvents, dataInput4$varianceEvents, tolerance = 1e-07)
        expect_type(names(dataInput4), "character")
        df <- as.data.frame(dataInput4)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(dataInput4)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }

    design1 <- getDesignInverseNormal(
        kMax = 3, typeOfDesign = "asP", typeBetaSpending = "bsKD", gammaB = 1.3, alpha = 0.02,
        informationRates = c(0.4, 0.7, 1), bindingFutility = FALSE, beta = 0.1
    )

    x2 <- getAnalysisResults(
        design = design1,
        dataInput = dataInput4,
        stratifiedAnalysis = TRUE,
        directionUpper = TRUE,
        stage = 2,
        nPlanned = 30,
        thetaH1 = 2,
        allocationRatioPlanned = 1,
        intersectionTest = "Sidak"
    )

    ## Comparison of the results of AnalysisResultsEnrichmentInverseNormal object 'x2' with expected results
    expect_equal(x2$conditionalRejectionProbabilities[1, ], c(0.043010929, 0.0010677592, NA_real_), tolerance = 1e-07, label = paste0(x2$conditionalRejectionProbabilities[1, ]))
    expect_equal(x2$conditionalRejectionProbabilities[2, ], c(0.063395248, 0.0010677592, NA_real_), tolerance = 1e-07, label = paste0(x2$conditionalRejectionProbabilities[2, ]))
    expect_equal(x2$conditionalRejectionProbabilities[3, ], c(0.15397803, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x2$conditionalRejectionProbabilities[3, ]))
    expect_equal(x2$conditionalPower[1, ], c(NA_real_, NA_real_, 0.12050895), tolerance = 1e-07, label = paste0(x2$conditionalPower[1, ]))
    expect_equal(x2$conditionalPower[2, ], c(NA_real_, NA_real_, 0.12050895), tolerance = 1e-07, label = paste0(x2$conditionalPower[2, ]))
    expect_equal(x2$conditionalPower[3, ], c(NA_real_, NA_real_, NA_real_), label = paste0(x2$conditionalPower[3, ]))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds[1, ], c(0.62578554, 0.64439022, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalLowerBounds[1, ]))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds[2, ], c(0.75127376, 0.66639106, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalLowerBounds[2, ]))
    expect_equal(x2$repeatedConfidenceIntervalLowerBounds[3, ], c(0.96321381, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalLowerBounds[3, ]))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds[1, ], c(4.9893102, 2.8192192, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalUpperBounds[1, ]))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds[2, ], c(6.2314391, 3.0969281, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalUpperBounds[2, ]))
    expect_equal(x2$repeatedConfidenceIntervalUpperBounds[3, ], c(3.5981376, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedConfidenceIntervalUpperBounds[3, ]))
    expect_equal(x2$repeatedPValues[1, ], c(0.13298203, 0.13298203, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedPValues[1, ]))
    expect_equal(x2$repeatedPValues[2, ], c(0.092701773, 0.092701773, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedPValues[2, ]))
    expect_equal(x2$repeatedPValues[3, ], c(0.031299575, NA_real_, NA_real_), tolerance = 1e-07, label = paste0(x2$repeatedPValues[3, ]))
    if (isTRUE(.isCompleteUnitTestSetEnabled())) {
        invisible(capture.output(expect_error(print(x2), NA)))
        expect_output(print(x2)$show())
        invisible(capture.output(expect_error(summary(x2), NA)))
        expect_output(summary(x2)$show())
        x2CodeBased <- eval(parse(text = getObjectRCode(x2, stringWrapParagraphWidth = NULL)))
        expect_equal(x2CodeBased$conditionalRejectionProbabilities, x2$conditionalRejectionProbabilities, tolerance = 1e-07)
        expect_equal(x2CodeBased$conditionalPower, x2$conditionalPower, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalLowerBounds, x2$repeatedConfidenceIntervalLowerBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedConfidenceIntervalUpperBounds, x2$repeatedConfidenceIntervalUpperBounds, tolerance = 1e-07)
        expect_equal(x2CodeBased$repeatedPValues, x2$repeatedPValues, tolerance = 1e-07)
        expect_type(names(x2), "character")
        df <- as.data.frame(x2)
        expect_s3_class(df, "data.frame")
        expect_true(nrow(df) > 0 && ncol(df) > 0)
        mtx <- as.matrix(x2)
        expect_true(is.matrix(mtx))
        expect_true(nrow(mtx) > 0 && ncol(mtx) > 0)
    }
})
