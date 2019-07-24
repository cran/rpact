######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 23 July 2019, 11:46:26                                                       #
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

context("Testing the group sequential and inverse normal design functionality")


test_that("'getDesignInverseNormal' with default parameters: parameters and results are as expected", {
	# @refFS[Formula]{fs:criticalValuesOBrienFleming}
	x1 <- getDesignInverseNormal()

	##
	## Comparison of the results of TrialDesignInverseNormal object 'x1' with expected results
	##
	expect_equal(x1$alphaSpent, c(0.00025917372, 0.0071600594, 0.025), tolerance = 1e-07)
	expect_equal(x1$criticalValues, c(3.4710914, 2.4544323, 2.0040356), tolerance = 1e-07)
	expect_equal(x1$stageLevels, c(0.00025917372, 0.0070553616, 0.022533125), tolerance = 1e-07)

})

test_that("'getDesignInverseNormal' and 'getDesignCharacteristics' with kMax = 4: parameters and results are as expected for different arguments", {

	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingHwangShiDeCani}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingHwangShiDeCani}
	x2 <- getDesignInverseNormal(kMax = 4, alpha = 0.07, sided = 1, beta = 0.14, typeOfDesign = "asHSD",gammaA = -1, 
		typeBetaSpending = C_TYPE_OF_DESIGN_BS_HSD, gammaB = -2)

	##
	## Comparison of the results of TrialDesignInverseNormal object 'x2' with expected results
	##
	expect_equal(x2$power, c(0.18540359, 0.47374657, 0.7208955, 0.86), tolerance = 1e-07)
	expect_equal(x2$futilityBounds, c(-0.81517021, 0.063469084, 0.84025384), tolerance = 1e-07)
	expect_equal(x2$alphaSpent, c(0.011570732, 0.026427847, 0.045504759, 0.07), tolerance = 1e-07)
	expect_equal(x2$betaSpent, c(0.014215085, 0.037651799, 0.076292407, 0.14), tolerance = 1e-07)
	expect_equal(x2$criticalValues, c(2.2710911, 2.0692301, 1.8645608, 1.6606881), tolerance = 1e-07)
	expect_equal(x2$stageLevels, c(0.011570732, 0.01926225, 0.031121494, 0.048388055), tolerance = 1e-07)
	y1 <- getDesignCharacteristics(x2)

	##
	## Comparison of the results of TrialDesignCharacteristics object 'y1' with expected results
	##
	expect_equal(y1$nFixed, 6.5337002, tolerance = 1e-07)
	expect_equal(y1$shift, 7.5749205, tolerance = 1e-07)
	expect_equal(y1$inflationFactor, 1.1593615, tolerance = 1e-07)
	expect_equal(y1$information, c(1.8937301, 3.7874603, 5.6811904, 7.5749205), tolerance = 1e-07)
	expect_equal(y1$power, c(0.18540359, 0.47374657, 0.7208955, 0.86), tolerance = 1e-07)
	expect_equal(y1$rejectionProbabilities, c(0.18540359, 0.28834298, 0.24714893, 0.1391045), tolerance = 1e-07)
	expect_equal(y1$futilityProbabilities, c(0.014215085, 0.023436714, 0.038640608), tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber1, 0.72222281, tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber01, 0.82592961, tolerance = 1e-07)
	expect_equal(y1$averageSampleNumber0, 0.68240644, tolerance = 1e-07)

	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingHwangShiDeCani}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingHwangShiDeCani}
	x3 <- getDesignInverseNormal(kMax = 4, informationRates = c(0.2, 0.4, 0.8, 1), 
			alpha = 0.07, sided = 1, beta = 0.14, typeOfDesign = "asHSD",gammaA = -1, 
			typeBetaSpending = C_TYPE_OF_DESIGN_BS_HSD, gammaB = -2)

	##
	## Comparison of the results of TrialDesignInverseNormal object 'x3' with expected results
	##
	expect_equal(x3$power, c(0.12840586, 0.34869365, 0.76424148, 0.86), tolerance = 1e-07)
	expect_equal(x3$futilityBounds, c(-1.0672796, -0.30464832, 1.028624), tolerance = 1e-07)
	expect_equal(x3$alphaSpent, c(0.0090195874, 0.020036136, 0.049926539, 0.07), tolerance = 1e-07)
	expect_equal(x3$betaSpent, c(0.010777094, 0.026854629, 0.086620705, 0.14), tolerance = 1e-07)
	expect_equal(x3$criticalValues, c(2.364813, 2.1928805, 1.7718975, 1.6682985), tolerance = 1e-07)
	expect_equal(x3$stageLevels, c(0.0090195874, 0.014157994, 0.038205784, 0.047628242), tolerance = 1e-07)

	# @refFS[Formula]{fs:inflationFactor}
	# @refFS[Formula]{fs:expectedReduction}
	y2 <- getDesignCharacteristics(x3)

	##
	## Comparison of the results of TrialDesignCharacteristics object 'y2' with expected results
	##
	expect_equal(y2$nFixed, 6.5337002, tolerance = 1e-07)
	expect_equal(y2$shift, 7.5750078, tolerance = 1e-07)
	expect_equal(y2$inflationFactor, 1.1593749, tolerance = 1e-07)
	expect_equal(y2$information, c(1.5150016, 3.0300031, 6.0600063, 7.5750078), tolerance = 1e-07)
	expect_equal(y2$power, c(0.12840586, 0.34869365, 0.76424148, 0.86), tolerance = 1e-07)
	expect_equal(y2$rejectionProbabilities, c(0.12840586, 0.22028779, 0.41554783, 0.095758523), tolerance = 1e-07)
	expect_equal(y2$futilityProbabilities, c(0.010777094, 0.016077535, 0.059766076), tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber1, 0.75564768, tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber01, 0.85242855, tolerance = 1e-07)
	expect_equal(y2$averageSampleNumber0, 0.720263, tolerance = 1e-07)

})

test_that("'getDesignInverseNormal' with binding futility bounds", {

	# @refFS[Formula]{fs:criticalValuesWithFutility}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x4 <- getDesignInverseNormal(kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

	##
	## Comparison of the results of TrialDesignInverseNormal object 'x4' with expected results
	##
	expect_equal(x4$alphaSpent, c(0.0062828133, 0.013876673, 0.02015684, 0.025), tolerance = 1e-07)
	expect_equal(x4$criticalValues, c(2.4958485, 2.328709, 2.2361766, 2.1727623), tolerance = 1e-07)
	expect_equal(x4$stageLevels, c(0.0062828133, 0.0099372444, 0.012670104, 0.014899106), tolerance = 1e-07)

})

test_that("'getDesignGroupSequential' with type of design = 'asUser'", {

	# @refFS[Formula]{fs:alphaSpendingConcept}
	x5 <- getDesignGroupSequential(typeOfDesign = "asUser", 
		userAlphaSpending = c(0.01, 0.02, 0.03, 0.05))

	##
	## Comparison of the results of TrialDesignGroupSequential object 'x5' with expected results
	##
	expect_equal(x5$alphaSpent, c(0.01, 0.02, 0.03, 0.05), tolerance = 1e-07)
	expect_equal(x5$criticalValues, c(2.3263479, 2.2192994, 2.1201347, 1.8189562), tolerance = 1e-07)
	expect_equal(x5$stageLevels, c(0.01, 0.01323318, 0.016997342, 0.034459058), tolerance = 1e-07)

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsUser'", {

	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	x6 <- getDesignGroupSequential(kMax = 3, alpha = 0.03, 
		typeOfDesign = "asOF", typeBetaSpending = "bsUser",
		userBetaSpending = c(0.01, 0.05, 0.3))

	##
	## Comparison of the results of TrialDesignGroupSequential object 'x6' with expected results
	##
	expect_equal(x6$power, c(0.014685829, 0.33275272, 0.7), tolerance = 1e-07)
	expect_equal(x6$futilityBounds, c(-0.92327973, 0.29975473), tolerance = 1e-07)
	expect_equal(x6$alphaSpent, c(0.00017079385, 0.0078650906, 0.03), tolerance = 1e-07)
	expect_equal(x6$betaSpent, c(0.01, 0.05, 0.3), tolerance = 1e-07)
	expect_equal(x6$criticalValues, c(3.5815302, 2.417863, 1.9175839), tolerance = 1e-07)
	expect_equal(x6$stageLevels, c(0.00017079385, 0.0078059773, 0.027581894), tolerance = 1e-07)

})

test_that("'getDesignGroupSequential' with type of design = 'asOF' and 'bsP'", {

	# @refFS[Formula]{fs:alphaSpendingConcept}
	# @refFS[Formula]{fs:alphaSpendingOBrienFleming}
	# @refFS[Formula]{fs:betaSpendingApproach}
	# @refFS[Formula]{fs:betaSpendingPocock}
	x7 <- getDesignGroupSequential(kMax = 3, alpha = 0.03, 
			typeOfDesign = "asOF", typeBetaSpending = "bsP",
			userBetaSpending = c(0.01, 0.05, 0.3))

	##
	## Comparison of the results of TrialDesignGroupSequential object 'x7' with expected results
	##
	expect_equal(x7$power, c(0.03410434, 0.52267986, 0.8), tolerance = 1e-07)
	expect_equal(x7$futilityBounds, c(0.42062972, 1.2539286), tolerance = 1e-07)
	expect_equal(x7$alphaSpent, c(0.00017079385, 0.0078650906, 0.03), tolerance = 1e-07)
	expect_equal(x7$betaSpent, c(0.090566485, 0.1526765, 0.2), tolerance = 1e-07)
	expect_equal(x7$criticalValues, c(3.5815302, 2.417863, 1.9175839), tolerance = 1e-07)
	expect_equal(x7$stageLevels, c(0.00017079385, 0.0078059773, 0.027581894), tolerance = 1e-07)

})

test_that("'getDesignGroupSequential'  with binding futility bounds ", {

	# @refFS[Formula]{fs:criticalValuesWithFutility}
	# @refFS[Formula]{fs:criticalValuesWangTiatis}
	x8 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, futilityBounds = rep(0.5244, 3), 
		bindingFutility = TRUE, typeOfDesign = "WT", deltaWT = 0.4)

	##
	## Comparison of the results of TrialDesignGroupSequential object 'x8' with expected results
	##
	expect_equal(x8$alphaSpent, c(0.0062828133, 0.013876673, 0.02015684, 0.025), tolerance = 1e-07)
	expect_equal(x8$criticalValues, c(2.4958485, 2.328709, 2.2361766, 2.1727623), tolerance = 1e-07)
	expect_equal(x8$stageLevels, c(0.0062828133, 0.0099372444, 0.012670104, 0.014899106), tolerance = 1e-07)

})

test_that("'getDesignGroupSequential'  with Haybittle Peto boundaries ", {

	# @refFS[Formula]{fs:criticalValuesHaybittlePeto}
	x9 <- getDesignGroupSequential(kMax = 4, alpha = 0.025, typeOfDesign = "HP")

	##
	## Comparison of the results of TrialDesignGroupSequential object 'x9' with expected results
	##
	expect_equal(x9$alphaSpent, c(0.001349898, 0.0024617416, 0.0033695882, 0.025), tolerance = 1e-07)
	expect_equal(x9$criticalValues, c(3, 3, 3, 1.9827514), tolerance = 1e-07)
	expect_equal(x9$stageLevels, c(0.001349898, 0.001349898, 0.001349898, 0.023697604), tolerance = 1e-07)

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

	expect_equal(getDesignInverseNormal(typeOfDesign = "asUser", 
			userAlphaSpending = c(0.01, 0.02, 0.023))$alpha, 0.023)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_WT, deltaWT = NA_real_), 
		"Missing argument: parameter 'deltaWT' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_WT_OPTIMUM, 
			optimizationCriterion = NA_character_), 
		"Missing argument: parameter 'optimizationCriterion' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_KD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_HSD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER), 
		"Missing argument: parameter 'userAlphaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignInverseNormal(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
			userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = NA_character_), 
		"Missing argument: parameter 'typeBetaSpending' must be specified in design", fixed = TRUE)

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

	expect_error(getDesignInverseNormal(kMax = -Inf), "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -10), "Argument out of bounds: 'kMax' (-10) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -9), "Argument out of bounds: 'kMax' (-9) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -8), "Argument out of bounds: 'kMax' (-8) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -7), "Argument out of bounds: 'kMax' (-7) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -6), "Argument out of bounds: 'kMax' (-6) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -4), "Argument out of bounds: 'kMax' (-4) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -3), "Argument out of bounds: 'kMax' (-3) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -2), "Argument out of bounds: 'kMax' (-2) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -1), "Argument out of bounds: 'kMax' (-1) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 11), "Argument out of bounds: 'kMax' (11) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 12), "Argument out of bounds: 'kMax' (12) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 13), "Argument out of bounds: 'kMax' (13) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 14), "Argument out of bounds: 'kMax' (14) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 15), "Argument out of bounds: 'kMax' (15) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 16), "Argument out of bounds: 'kMax' (16) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 17), "Argument out of bounds: 'kMax' (17) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 18), "Argument out of bounds: 'kMax' (18) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 19), "Argument out of bounds: 'kMax' (19) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 20), "Argument out of bounds: 'kMax' (20) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = Inf), "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; 10]", fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = 2, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (2) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 3, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (3) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 4, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (4) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 6, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (6) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 7, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (7) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 8, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (8) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 9, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (9) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 10, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (10) - 1", fixed = TRUE)

	expect_error(getDesignInverseNormal(futilityBounds = c(-7, 5)), 
		"Argument out of bounds: 'futilityBounds' (-7, 5) is out of bounds [-6; 6]", fixed = TRUE)

	expect_error(getDesignInverseNormal(futilityBounds = c(1, 7)), 
		"Argument out of bounds: 'futilityBounds' (1, 7) is out of bounds [-6; 6]", fixed = TRUE)

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
			optimizationCriterion = NA_character_), 
		"Missing argument: parameter 'optimizationCriterion' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_KD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_HSD, gammaA = NA_real_), 
		"Missing argument: parameter 'gammaA' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER), 
		"Missing argument: parameter 'userAlphaSpending' must be specified in design", fixed = TRUE)

	expect_error(getDesignGroupSequential(typeOfDesign = C_TYPE_OF_DESIGN_AS_USER, 
			userAlphaSpending = c(0.01, 0.02, 0.025), typeBetaSpending = NA_character_), 
		"Missing argument: parameter 'typeBetaSpending' must be specified in design", fixed = TRUE)

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

	expect_error(getDesignInverseNormal(kMax = -Inf), "Argument out of bounds: 'kMax' (-Inf) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -10), "Argument out of bounds: 'kMax' (-10) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -9), "Argument out of bounds: 'kMax' (-9) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -8), "Argument out of bounds: 'kMax' (-8) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -7), "Argument out of bounds: 'kMax' (-7) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -6), "Argument out of bounds: 'kMax' (-6) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -5), "Argument out of bounds: 'kMax' (-5) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -4), "Argument out of bounds: 'kMax' (-4) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -3), "Argument out of bounds: 'kMax' (-3) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -2), "Argument out of bounds: 'kMax' (-2) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = -1), "Argument out of bounds: 'kMax' (-1) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 0), "Argument out of bounds: 'kMax' (0) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 11), "Argument out of bounds: 'kMax' (11) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 12), "Argument out of bounds: 'kMax' (12) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 13), "Argument out of bounds: 'kMax' (13) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 14), "Argument out of bounds: 'kMax' (14) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 15), "Argument out of bounds: 'kMax' (15) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 16), "Argument out of bounds: 'kMax' (16) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 17), "Argument out of bounds: 'kMax' (17) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 18), "Argument out of bounds: 'kMax' (18) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 19), "Argument out of bounds: 'kMax' (19) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 20), "Argument out of bounds: 'kMax' (20) is out of bounds [1; 10]", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = Inf), "Argument out of bounds: 'kMax' (Inf) is out of bounds [1; 10]", fixed = TRUE)

	expect_error(getDesignInverseNormal(kMax = 2, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (2) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 3, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (3) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 4, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (4) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 6, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (6) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 7, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (7) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 8, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (8) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 9, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (9) - 1", fixed = TRUE)
	expect_error(getDesignInverseNormal(kMax = 10, futilityBounds = c(0, 0, 1, 2)), "Conflicting arguments: length of 'futilityBounds' (4) must be equal to 'kMax' (10) - 1", fixed = TRUE)

	expect_error(getDesignGroupSequential(futilityBounds = c(-7, 5)), 
		"Argument out of bounds: 'futilityBounds' (-7, 5) is out of bounds [-6; 6]", fixed = TRUE)

	expect_error(getDesignGroupSequential(futilityBounds = c(1, 7)), 
		"Argument out of bounds: 'futilityBounds' (1, 7) is out of bounds [-6; 6]", fixed = TRUE)

})

