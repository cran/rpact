######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 11 September 2019, 13:41:50                                                  #
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

context("Testing assertion functions")


test_that("Testing '.assertDesignParameterExists'", {
	expect_error(.assertDesignParameterExists(), 
		"Missing argument: 'design' must be defined", fixed = TRUE)

	expect_error(.assertDesignParameterExists(design = getAssertionTestDesign()), 
		"Missing argument: 'parameterName' must be defined", fixed = TRUE)

	expect_error(.assertDesignParameterExists(design = getAssertionTestDesign(), parameterName = "kMax"), 
		"Missing argument: 'defaultValue' must be defined", fixed = TRUE)

	expect_error(.assertDesignParameterExists(design = getAssertionTestDesign(), 
			parameterName = "kMax", defaultValue = C_KMAX_DEFAULT),
		"Missing argument: parameter 'kMax' must be specified in design", fixed = TRUE)

	expect_error(.assertDesignParameterExists(design = getAssertionTestDesign(kMax = NA_integer_), 
			parameterName = "kMax", defaultValue = C_KMAX_DEFAULT),
		"Missing argument: parameter 'kMax' must be specified in design", fixed = TRUE)

})

test_that("Testing '.assertIsValidThetaRange'	", {

	expect_error(.assertIsValidThetaRange(thetaRange = c()), 
		"Illegal argument: 'thetaRange' must be a vector with two entries defining minimum and maximum or a sequence of values with length > 2", fixed = TRUE)

	#expect_error(.assertIsValidThetaRange(thetaRange = c(1, -2)), 
	#	"Illegal argument: 'thetaRange' with length 2 must contain minimum < maximum (1 >= -2)", fixed = TRUE)

	expect_equal(.assertIsValidThetaRange(thetaRange = c(1, 2, 3)), c(1, 2, 3))

	expect_equal(.assertIsValidThetaRange(thetaRange = c(-1, 2)), seq(-1, 2, 3 / C_THETA_RANGE_SEQUENCE_LENGTH_DEFAULT))

})

test_that("Testing '.assertIsSingleNumber'", {

	expect_error(.assertIsSingleNumber(NA, "x"), 
		"Illegal argument: 'x' (NA) must be a valid single numerical value", fixed = TRUE)

	expect_error(.assertIsSingleNumber(NULL, "x"), 
		"Missing argument: 'x' must be a valid single numerical value", fixed = TRUE)

	expect_error(.assertIsSingleNumber(c(1, 2), "x"), 
		"Illegal argument: 'x' c(1, 2) must be a single numerical value", fixed = TRUE)

	expect_error(.assertIsSingleNumber(numeric(0), "x"), 
		"Missing argument: 'x' must be a valid single numerical value", fixed = TRUE)

})

test_that("Testing '.assertAssociatedArgumentsAreDefined'", {

	expect_error(.assertAssociatedArgumentsAreDefined(a = NA, b = 1), 
		"Missing argument: 'a' must be defined because 'b' is defined", fixed = TRUE)

	expect_error(.assertAssociatedArgumentsAreDefined(a = NA, b = 1, c = NA), 
		"Missing argument: 'a', 'c' must be defined because 'b' is defined", fixed = TRUE)

	expect_error(.assertAssociatedArgumentsAreDefined(a = NA, b = 1, c = 2), 
		"Missing argument: 'a' must be defined because 'b', 'c' are defined", fixed = TRUE)

})

test_that("Testing '.associatedArgumentsAreDefined'", {

	expect_equal(.associatedArgumentsAreDefined(nPlanned = NA_real_, thetaH1 = NA_real_), FALSE)

	expect_equal(.associatedArgumentsAreDefined(nPlanned = NA_real_, thetaH1 = 1), FALSE)

	expect_equal(.associatedArgumentsAreDefined(nPlanned = 1, thetaH1 = 1), TRUE)

})

test_that("Testing '.isValidNPlanned", {

	expect_equal(.isValidNPlanned(nPlanned = c(1, 2), kMax = 4, stage = 2), TRUE)

	expect_silent(.isValidNPlanned(nPlanned = NA_real_, kMax = 4, stage = 2))

	expect_warning(.isValidNPlanned(nPlanned = c(1), kMax = 4, stage = 2),
		"'nPlanned' (1) will be ignored: length must be equal to 'kMax' (4) - 'stage' (2)", 
		fixed = TRUE)

	expect_warning(.isValidNPlanned(nPlanned = c(1, 2, 3), kMax = 4, stage = 2),
		"'nPlanned' (1, 2, 3) will be ignored: length must be equal to 'kMax' (4) - 'stage' (2)", 
		fixed = TRUE)

})

