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
#:#  File name: test-f_design_fisher_combination_test.R
#:#  Creation date: 09 November 2020, 11:48:28
#:#  File version: $Revision: 3854 $
#:#  Last changed: $Date: 2020-11-09 14:53:50 +0100 (Mon, 09 Nov 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing the Fisher Design Functionality")


test_that("'getDesignFisher' with default parameters: parameters and results are as expected", {
	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher0 <- getDesignFisher()

	## Comparison of the results of TrialDesignFisher object 'designFisher0' with expected results
	expect_equal(designFisher0$alphaSpent, c(0.012308547, 0.01962413, 0.025), tolerance = 1e-07)
	expect_equal(designFisher0$criticalValues, c(0.012308547, 0.0016635923, 0.00029106687), tolerance = 1e-07)
	expect_equal(designFisher0$stageLevels, c(0.012308547, 0.012308547, 0.012308547), tolerance = 1e-07)
	expect_equal(designFisher0$scale, c(1, 1))
	expect_equal(designFisher0$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher0), NA)))
	    expect_output(print(designFisher0)$show())
	    invisible(capture.output(expect_error(summary(designFisher0), NA)))
	    expect_output(summary(designFisher0)$show())
	}

})

test_that("'getDesignFisher' with kMax = 4: parameters and results are as expected for different arguments", {

	.skipTestIfDisabled()

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher1 <- getDesignFisher(kMax = 4)

	## Comparison of the results of TrialDesignFisher object 'designFisher1' with expected results
	expect_equal(designFisher1$alphaSpent, c(0.010404785, 0.016661203, 0.021286477, 0.025), tolerance = 1e-07)
	expect_equal(designFisher1$criticalValues, c(0.010404785, 0.0013703718, 0.00023506069, 4.5812899e-05), tolerance = 1e-07)
	expect_equal(designFisher1$stageLevels, c(0.010404785, 0.010404785, 0.010404785, 0.010404785), tolerance = 1e-07)
	expect_equal(designFisher1$scale, c(1, 1, 1))
	expect_equal(designFisher1$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher1), NA)))
	    expect_output(print(designFisher1)$show())
	    invisible(capture.output(expect_error(summary(designFisher1), NA)))
	    expect_output(summary(designFisher1)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationEqualAlpha}
	designFisher2 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7,0.6,0.5), informationRates = c(0.1,0.3,0.7,1))

	## Comparison of the results of TrialDesignFisher object 'designFisher2' with expected results
	expect_equal(designFisher2$alphaSpent, c(0.010565317, 0.017774885, 0.022713904, 0.025), tolerance = 1e-07)
	expect_equal(designFisher2$criticalValues, c(0.010565317, 0.00031144789, 2.8609076e-06, 1.4533579e-07), tolerance = 1e-07)
	expect_equal(designFisher2$stageLevels, c(0.010565317, 0.010565317, 0.010565317, 0.010565317), tolerance = 1e-07)
	expect_equal(designFisher2$scale, c(1.4142136, 2, 1.7320508), tolerance = 1e-07)
	expect_equal(designFisher2$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher2), NA)))
	    expect_output(print(designFisher2)$show())
	    invisible(capture.output(expect_error(summary(designFisher2), NA)))
	    expect_output(summary(designFisher2)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationFullAlpha}
	designFisher3 <- getDesignFisher(kMax = 4, method = "fullAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher3' with expected results
	expect_equal(designFisher3$alphaSpent, c(0.00015574772, 0.0015212305, 0.0075070105, 0.025), tolerance = 1e-07)
	expect_equal(designFisher3$criticalValues, c(0.00015574772, 0.00015574772, 0.00015574772, 0.00015574772), tolerance = 1e-07)
	expect_equal(designFisher3$stageLevels, c(0.00015574772, 0.0015212305, 0.0075070105, 0.025), tolerance = 1e-07)
	expect_equal(designFisher3$scale, c(1, 1, 1))
	expect_equal(designFisher3$nonStochasticCurtailment, TRUE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher3), NA)))
	    expect_output(print(designFisher3)$show())
	    invisible(capture.output(expect_error(summary(designFisher3), NA)))
	    expect_output(summary(designFisher3)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationFullAlpha}
	designFisher4 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7,0.6,0.5), informationRates = c(0.1,0.3,0.7,1), method = "fullAlpha")

	## Comparison of the results of TrialDesignFisher object 'designFisher4' with expected results
	expect_equal(designFisher4$alphaSpent, c(0.0075234886, 0.012807964, 0.016496254, 0.025), tolerance = 1e-07)
	expect_equal(designFisher4$criticalValues, c(0.0075234886, 0.00019010097, 1.4149989e-06, 1.0550077e-06), tolerance = 1e-07)
	expect_equal(designFisher4$stageLevels, c(0.0075234886, 0.0075234886, 0.0075234886, 0.025), tolerance = 1e-07)
	expect_equal(designFisher4$scale, c(1.4142136, 2, 1.7320508), tolerance = 1e-07)
	expect_equal(designFisher4$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher4), NA)))
	    expect_output(print(designFisher4)$show())
	    invisible(capture.output(expect_error(summary(designFisher4), NA)))
	    expect_output(summary(designFisher4)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationNoTreatmentStageInteraction}
	designFisher5 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7,0.6,0.5), method = "noInteraction")

	## Comparison of the results of TrialDesignFisher object 'designFisher5' with expected results
	expect_equal(designFisher5$alphaSpent, c(0.0098603693, 0.012073314, 0.018133935, 0.025), tolerance = 1e-07)
	expect_equal(designFisher5$criticalValues, c(0.0098603693, 0.00051915905, 0.00031149543, 0.00015574772), tolerance = 1e-07)
	expect_equal(designFisher5$stageLevels, c(0.0098603693, 0.0044457148, 0.012979977, 0.025), tolerance = 1e-07)
	expect_equal(designFisher5$scale, c(1, 1, 1))
	expect_equal(designFisher5$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher5), NA)))
	    expect_output(print(designFisher5)$show())
	    invisible(capture.output(expect_error(summary(designFisher5), NA)))
	    expect_output(summary(designFisher5)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationNoTreatmentStageInteraction}
	designFisher6 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7,0.6,0.5), informationRates = c(0.1,0.3,0.7,1), method = "noInteraction")

	## Comparison of the results of TrialDesignFisher object 'designFisher6' with expected results
	expect_equal(designFisher6$alphaSpent, c(0.01128689, 0.011490625, 0.016266616, 0.025), tolerance = 1e-07)
	expect_equal(designFisher6$criticalValues, c(0.01128689, 2.0322622e-06, 1.5741835e-06, 1.0550077e-06), tolerance = 1e-07)
	expect_equal(designFisher6$stageLevels, c(0.01128689, 0.0003175156, 0.0079214091, 0.025), tolerance = 1e-07)
	expect_equal(designFisher6$scale, c(1.4142136, 2, 1.7320508), tolerance = 1e-07)
	expect_equal(designFisher6$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher6), NA)))
	    expect_output(print(designFisher6)$show())
	    invisible(capture.output(expect_error(summary(designFisher6), NA)))
	    expect_output(summary(designFisher6)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationUserDefinedAlphaSpending}
	designFisher7 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7,0.6,0.5), method = "userDefinedAlpha", userAlphaSpending = c(0.01,0.015,0.02,0.025))

	## Comparison of the results of TrialDesignFisher object 'designFisher7' with expected results
	expect_equal(designFisher7$alphaSpent, c(0.01, 0.015, 0.02, 0.025), tolerance = 1e-07)
	expect_equal(designFisher7$criticalValues, c(0.01, 0.0011768873, 0.00031357454, 0.00011586425), tolerance = 1e-07)
	expect_equal(designFisher7$stageLevels, c(0.01, 0.0091148534, 0.013047692, 0.020300118), tolerance = 1e-07)
	expect_equal(designFisher7$scale, c(1, 1, 1))
	expect_equal(designFisher7$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher7), NA)))
	    expect_output(print(designFisher7)$show())
	    invisible(capture.output(expect_error(summary(designFisher7), NA)))
	    expect_output(summary(designFisher7)$show())
	}

	# @refFS[Tab.]{fs:tab:output:getDesignFisher}
	# @refFS[Formula]{fs:FisherCombinationUserDefinedAlphaSpending}
	designFisher8 <- getDesignFisher(kMax = 4, alpha0Vec = c(0.7,0.6,0.5), informationRates = c(0.1,0.3,0.7,1), method = "userDefinedAlpha", userAlphaSpending = c(0.01,0.015,0.02,0.025))

	## Comparison of the results of TrialDesignFisher object 'designFisher8' with expected results
	expect_equal(designFisher8$alphaSpent, c(0.01, 0.015, 0.02, 0.025), tolerance = 1e-07)
	expect_equal(designFisher8$criticalValues, c(0.01, 0.00018389153, 2.6484943e-06, 5.2344628e-07), tolerance = 1e-07)
	expect_equal(designFisher8$stageLevels, c(0.01, 0.0073532156, 0.0101804, 0.018500415), tolerance = 1e-07)
	expect_equal(designFisher8$scale, c(1.4142136, 2, 1.7320508), tolerance = 1e-07)
	expect_equal(designFisher8$nonStochasticCurtailment, FALSE)
	if (isTRUE(.isCompleteUnitTestSetEnabled())) {
	    invisible(capture.output(expect_error(print(designFisher8), NA)))
	    expect_output(print(designFisher8)$show())
	    invisible(capture.output(expect_error(summary(designFisher8), NA)))
	    expect_output(summary(designFisher8)$show())
	}

})

test_that("'getDesignFisher': illegal arguments throw exceptions as expected", {

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA, 
			userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.025), kMax = 4), 
		paste0("Conflicting arguments: length of 'userAlphaSpending' (5) ", 
			"must be equal to 'kMax' (4)"), fixed = TRUE)

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA, 
			userAlphaSpending = c(0.01, 0.02, 0.025), informationRates = c(0.5, 1)),
		paste0("Conflicting arguments: length of 'userAlphaSpending' (3) ", 
			"must be equal to length of 'informationRates' (2)"), fixed = TRUE)

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA, 
			userAlphaSpending = c(0.01, 0.02, 0.025), informationRates = c(0.4, 1)),
		paste0("Conflicting arguments: length of 'userAlphaSpending' (3) ", 
			"must be equal to length of 'informationRates' (2)"), fixed = TRUE)

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA, 
			userAlphaSpending = c(0.01, 0.02, 0.023, 0.023, 0.021)),
		paste0("'userAlphaSpending' = c(0.01, 0.02, 0.023, 0.023, 0.021) must be a vector that ", 
			"satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_5 <= alpha = 0.021"), fixed = TRUE)

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA, 
			userAlphaSpending = c(0.01, 0.02, 0.023), alpha = 0.02),
		paste0("'userAlphaSpending' = c(0.01, 0.02, 0.023) must be a vector that ", 
			"satisfies the following condition: 0 <= alpha_1 <= .. <= alpha_3 <= alpha = 0.02"), fixed = TRUE)

	expect_equal(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA, 
			userAlphaSpending = c(0.01, 0.02, 0.023))$alpha, 0.023)

	expect_error(getDesignFisher(method = C_FISHER_METHOD_USER_DEFINED_ALPHA), 
		"Missing argument: parameter 'userAlphaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignFisher(kMax = Inf), 
		paste0("Argument out of bounds: 'kMax' (Inf) is out of bounds [1; ", 
			C_KMAX_UPPER_BOUND_FISHER, "]"), fixed = TRUE)

	expect_error(getDesignFisher(kMax = -Inf), 
		paste0("Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; ", 
			C_KMAX_UPPER_BOUND_FISHER, "]"), fixed = TRUE)

	expect_error(getDesignFisher(kMax = -Inf), "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -10), "Argument out of bounds: 'kMax' (-10) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -9), "Argument out of bounds: 'kMax' (-9) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -8), "Argument out of bounds: 'kMax' (-8) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -7), "Argument out of bounds: 'kMax' (-7) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -6), "Argument out of bounds: 'kMax' (-6) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -4), "Argument out of bounds: 'kMax' (-4) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -3), "Argument out of bounds: 'kMax' (-3) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -2), "Argument out of bounds: 'kMax' (-2) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = -1), "Argument out of bounds: 'kMax' (-1) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 7), "Argument out of bounds: 'kMax' (7) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 8), "Argument out of bounds: 'kMax' (8) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 9), "Argument out of bounds: 'kMax' (9) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 10), "Argument out of bounds: 'kMax' (10) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 11), "Argument out of bounds: 'kMax' (11) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 12), "Argument out of bounds: 'kMax' (12) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 13), "Argument out of bounds: 'kMax' (13) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 14), "Argument out of bounds: 'kMax' (14) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 15), "Argument out of bounds: 'kMax' (15) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 16), "Argument out of bounds: 'kMax' (16) is out of bounds [1; 6]", fixed = TRUE)
	expect_error(getDesignFisher(kMax = Inf), "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; 6]", fixed = TRUE)

	expect_error(getDesignFisher(kMax = 2, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (2)", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 3, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (3)", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 5, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (5)", fixed = TRUE)
	expect_error(getDesignFisher(kMax = 6, informationRates = c(0.01, 0.02, 0.04, 0.05)), "Conflicting arguments: length of 'informationRates' (4) must be equal to 'kMax' (6)", fixed = TRUE)

	expect_error(getDesignFisher(alpha0Vec = c(0, 1)), 
		"Argument out of bounds: 'alpha0Vec' (0, 1) is out of bounds (0; 1]", fixed = TRUE)

	expect_error(getDesignFisher(alpha0Vec = c(0.1, 1.01)), 
		"Argument out of bounds: 'alpha0Vec' (0.1, 1.01) is out of bounds (0; 1]", fixed = TRUE)

})

