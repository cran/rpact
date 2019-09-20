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

context("Testing core utility functions")


test_that("'getValidatedInformationRates': 'informationRates' must be generated correctly based on specified 'kMax'", {
	design1 <- getTestDesign(kMax = 1L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design1), 1, tolerance = 1e-08)

	design2 <- getTestDesign(kMax = 2L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design2), c(0.5, 1), tolerance = 1e-08)

	design3 <- getTestDesign(kMax = 3L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design3), c(0.33333333, 0.66666667, 1), tolerance = 1e-08)

	design4 <- getTestDesign(kMax = 4L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design4), c(0.25, 0.5, 0.75, 1), tolerance = 1e-08)

	design5 <- getTestDesign(kMax = 5L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design5), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-08)

	design6 <- getTestDesign(kMax = 6L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design6), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-08)

	design7 <- getTestDesign(kMax = 1L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design7), 1, tolerance = 1e-08)

	design8 <- getTestDesign(kMax = 2L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design8), c(0.5, 1), tolerance = 1e-08)

	design9 <- getTestDesign(kMax = 3L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design9), c(0.33333333, 0.66666667, 1), tolerance = 1e-08)

	design10 <- getTestDesign(kMax = 4L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design10), c(0.25, 0.5, 0.75, 1), tolerance = 1e-08)

	design11 <- getTestDesign(kMax = 5L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design11), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-08)

	design12 <- getTestDesign(kMax = 6L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design12), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-08)

	design13 <- getTestDesign(kMax = 1L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design13), 1, tolerance = 1e-08)

	design14 <- getTestDesign(kMax = 2L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design14), c(0.5, 1), tolerance = 1e-08)

	design15 <- getTestDesign(kMax = 3L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design15), c(0.33333333, 0.66666667, 1), tolerance = 1e-08)

	design16 <- getTestDesign(kMax = 4L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design16), c(0.25, 0.5, 0.75, 1), tolerance = 1e-08)

	design17 <- getTestDesign(kMax = 5L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design17), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-08)

	design18 <- getTestDesign(kMax = 6L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design18), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-08)




})

test_that("'getValidatedInformationRates': 'informationRates' must be set correctly based on specified 'informationRates'", {

	design19 <- getTestDesign(informationRates = 1, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design19), 1, tolerance = 1e-07)

	design20 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design20), c(0.4, 1), tolerance = 1e-07)

	design21 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design21), c(0.26666667, 0.53333333, 1), tolerance = 1e-07)

	design22 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design22), c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)

	design23 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design23), c(0.16, 0.32, 0.48, 0.64, 1), tolerance = 1e-07)

	design24 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design24), c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), tolerance = 1e-07)

	design25 <- getTestDesign(informationRates = 1, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design25), 1, tolerance = 1e-07)

	design26 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design26), c(0.4, 1), tolerance = 1e-07)

	design27 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design27), c(0.26666667, 0.53333333, 1), tolerance = 1e-07)

	design28 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design28), c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)

	design29 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design29), c(0.16, 0.32, 0.48, 0.64, 1), tolerance = 1e-07)

	design30 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design30), c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), tolerance = 1e-07)

	design31 <- getTestDesign(informationRates = 1, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design31), 1, tolerance = 1e-07)

	design32 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design32), c(0.4, 1), tolerance = 1e-07)

	design33 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design33), c(0.26666667, 0.53333333, 1), tolerance = 1e-07)

	design34 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design34), c(0.2, 0.4, 0.6, 1), tolerance = 1e-07)

	design35 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design35), c(0.16, 0.32, 0.48, 0.64, 1), tolerance = 1e-07)

	design36 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design36), c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), tolerance = 1e-07)




	design37 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design37), c(0.5, 1), tolerance = 1e-07)

	design38 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design38), c(0.33333333, 0.66666667, 1), tolerance = 1e-07)

	design39 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design39), c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)

	design40 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design40), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)

	design41 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedInformationRates(design41), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-07)

	design42 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design42), c(0.5, 1), tolerance = 1e-07)

	design43 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design43), c(0.33333333, 0.66666667, 1), tolerance = 1e-07)

	design44 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design44), c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)

	design45 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design45), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)

	design46 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedInformationRates(design46), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-07)

	design47 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design47), c(0.5, 1), tolerance = 1e-07)

	design48 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design48), c(0.33333333, 0.66666667, 1), tolerance = 1e-07)

	design49 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design49), c(0.25, 0.5, 0.75, 1), tolerance = 1e-07)

	design50 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design50), c(0.2, 0.4, 0.6, 0.8, 1), tolerance = 1e-07)

	design51 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedInformationRates(design51), c(0.16666667, 0.33333333, 0.5, 0.66666667, 0.83333333, 1), tolerance = 1e-07)




})

test_that("'getValidatedInformationRates': 'kMax' must be set correctly based on specified 'informationRates'", {

	design52 <- getTestDesign(informationRates = 1, designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design52)
	expect_equal(design52$kMax, 1, tolerance = 1e-07)

	design53 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design53)
	expect_equal(design53$kMax, 2, tolerance = 1e-07)

	design54 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design54)
	expect_equal(design54$kMax, 3, tolerance = 1e-07)

	design55 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design55)
	expect_equal(design55$kMax, 4, tolerance = 1e-07)

	design56 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design56)
	expect_equal(design56$kMax, 5, tolerance = 1e-07)

	design57 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design57)
	expect_equal(design57$kMax, 6, tolerance = 1e-07)

	design58 <- getTestDesign(informationRates = 1, designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design58)
	expect_equal(design58$kMax, 1, tolerance = 1e-07)

	design59 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design59)
	expect_equal(design59$kMax, 2, tolerance = 1e-07)

	design60 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design60)
	expect_equal(design60$kMax, 3, tolerance = 1e-07)

	design61 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design61)
	expect_equal(design61$kMax, 4, tolerance = 1e-07)

	design62 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design62)
	expect_equal(design62$kMax, 5, tolerance = 1e-07)

	design63 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design63)
	expect_equal(design63$kMax, 6, tolerance = 1e-07)

	design64 <- getTestDesign(informationRates = 1, designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design64)
	expect_equal(design64$kMax, 1, tolerance = 1e-07)

	design65 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design65)
	expect_equal(design65$kMax, 2, tolerance = 1e-07)

	design66 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design66)
	expect_equal(design66$kMax, 3, tolerance = 1e-07)

	design67 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design67)
	expect_equal(design67$kMax, 4, tolerance = 1e-07)

	design68 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design68)
	expect_equal(design68$kMax, 5, tolerance = 1e-07)

	design69 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design69)
	expect_equal(design69$kMax, 6, tolerance = 1e-07)




	design70 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design70)
	expect_equal(design70$kMax, 2, tolerance = 1e-07)

	design71 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design71)
	expect_equal(design71$kMax, 3, tolerance = 1e-07)

	design72 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design72)
	expect_equal(design72$kMax, 4, tolerance = 1e-07)

	design73 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design73)
	expect_equal(design73$kMax, 5, tolerance = 1e-07)

	design74 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedInformationRates(design74)
	expect_equal(design74$kMax, 6, tolerance = 1e-07)

	design75 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design75)
	expect_equal(design75$kMax, 2, tolerance = 1e-07)

	design76 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design76)
	expect_equal(design76$kMax, 3, tolerance = 1e-07)

	design77 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design77)
	expect_equal(design77$kMax, 4, tolerance = 1e-07)

	design78 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design78)
	expect_equal(design78$kMax, 5, tolerance = 1e-07)

	design79 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedInformationRates(design79)
	expect_equal(design79$kMax, 6, tolerance = 1e-07)

	design80 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design80)
	expect_equal(design80$kMax, 2, tolerance = 1e-07)

	design81 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design81)
	expect_equal(design81$kMax, 3, tolerance = 1e-07)

	design82 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design82)
	expect_equal(design82$kMax, 4, tolerance = 1e-07)

	design83 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design83)
	expect_equal(design83$kMax, 5, tolerance = 1e-07)

	design84 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignFisher")
	.getValidatedInformationRates(design84)
	expect_equal(design84$kMax, 6, tolerance = 1e-07)




})

test_that("'getValidatedInformationRates': 'futilityBounds' must be generated correctly based on specified 'kMax'", {

	design85 <- getTestDesign(kMax = 1L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design85), numeric(0), tolerance = 1e-08)

	design86 <- getTestDesign(kMax = 2L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design86), -6, tolerance = 1e-08)

	design87 <- getTestDesign(kMax = 3L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design87), c(-6, -6), tolerance = 1e-08)

	design88 <- getTestDesign(kMax = 4L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design88), c(-6, -6, -6), tolerance = 1e-08)

	design89 <- getTestDesign(kMax = 5L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design89), c(-6, -6, -6, -6), tolerance = 1e-08)

	design90 <- getTestDesign(kMax = 6L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design90), c(-6, -6, -6, -6, -6), tolerance = 1e-08)

	design91 <- getTestDesign(kMax = 7L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design91), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design92 <- getTestDesign(kMax = 8L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design92), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design93 <- getTestDesign(kMax = 9L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design93), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design94 <- getTestDesign(kMax = 10L, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design94), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design95 <- getTestDesign(kMax = 1L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design95), numeric(0), tolerance = 1e-08)

	design96 <- getTestDesign(kMax = 2L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design96), -6, tolerance = 1e-08)

	design97 <- getTestDesign(kMax = 3L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design97), c(-6, -6), tolerance = 1e-08)

	design98 <- getTestDesign(kMax = 4L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design98), c(-6, -6, -6), tolerance = 1e-08)

	design99 <- getTestDesign(kMax = 5L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design99), c(-6, -6, -6, -6), tolerance = 1e-08)

	design100 <- getTestDesign(kMax = 6L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design100), c(-6, -6, -6, -6, -6), tolerance = 1e-08)

	design101 <- getTestDesign(kMax = 7L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design101), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design102 <- getTestDesign(kMax = 8L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design102), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design103 <- getTestDesign(kMax = 9L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design103), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)

	design104 <- getTestDesign(kMax = 10L, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design104), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-08)




	design105 <- getTestDesign(kMax = 1L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design105), numeric(0), tolerance = 1e-08)

	design106 <- getTestDesign(kMax = 2L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design106), 1, tolerance = 1e-08)

	design107 <- getTestDesign(kMax = 3L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design107), c(1, 1), tolerance = 1e-08)

	design108 <- getTestDesign(kMax = 4L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design108), c(1, 1, 1), tolerance = 1e-08)

	design109 <- getTestDesign(kMax = 5L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design109), c(1, 1, 1, 1), tolerance = 1e-08)

	design110 <- getTestDesign(kMax = 6L, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design110), c(1, 1, 1, 1, 1), tolerance = 1e-08)



})

test_that("'getValidatedInformationRates': 'futilityBounds' must be set correctly based on specified 'futilityBounds'", {

	design111 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design111), 2, tolerance = 1e-07)

	design112 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design112), c(1, 2), tolerance = 1e-07)

	design113 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design113), c(0, 1, 2), tolerance = 1e-07)

	design114 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design114), c(0, 0, 1, 2), tolerance = 1e-07)

	design115 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design115), c(0, 0, 0, 1, 2), tolerance = 1e-07)

	design116 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design116), c(0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design117 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design117), c(0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design118 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design118), c(0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design119 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design119), c(0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design120 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design120), 2, tolerance = 1e-07)

	design121 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design121), c(1, 2), tolerance = 1e-07)

	design122 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design122), c(0, 1, 2), tolerance = 1e-07)

	design123 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design123), c(0, 0, 1, 2), tolerance = 1e-07)

	design124 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design124), c(0, 0, 0, 1, 2), tolerance = 1e-07)

	design125 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design125), c(0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design126 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design126), c(0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design127 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design127), c(0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)

	design128 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design128), c(0, 0, 0, 0, 0, 0, 0, 1, 2), tolerance = 1e-07)




	design129 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design129), 2, tolerance = 1e-07)

	design130 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design130), c(1, 2), tolerance = 1e-07)

	design131 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design131), c(0, 1, 2), tolerance = 1e-07)

	design132 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design132), c(0, 0, 1, 2), tolerance = 1e-07)

	design133 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design133), c(0, 0, 0, 1, 2), tolerance = 1e-07)



	design134 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design134), -6, tolerance = 1e-07)

	design135 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design135), c(-6, -6), tolerance = 1e-07)

	design136 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design136), c(-6, -6, -6), tolerance = 1e-07)

	design137 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design137), c(-6, -6, -6, -6), tolerance = 1e-07)

	design138 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design138), c(-6, -6, -6, -6, -6), tolerance = 1e-07)

	design139 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design139), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design140 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design140), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design141 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design141), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design142 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignGroupSequential")
	expect_equal(.getValidatedFutilityBounds(design142), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design143 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design143), -6, tolerance = 1e-07)

	design144 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design144), c(-6, -6), tolerance = 1e-07)

	design145 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design145), c(-6, -6, -6), tolerance = 1e-07)

	design146 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design146), c(-6, -6, -6, -6), tolerance = 1e-07)

	design147 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design147), c(-6, -6, -6, -6, -6), tolerance = 1e-07)

	design148 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design148), c(-6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design149 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design149), c(-6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design150 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design150), c(-6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)

	design151 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignInverseNormal")
	expect_equal(.getValidatedFutilityBounds(design151), c(-6, -6, -6, -6, -6, -6, -6, -6, -6), tolerance = 1e-07)




	design152 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design152), 1, tolerance = 1e-07)

	design153 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design153), c(1, 1), tolerance = 1e-07)

	design154 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design154), c(1, 1, 1), tolerance = 1e-07)

	design155 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design155), c(1, 1, 1, 1), tolerance = 1e-07)

	design156 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignFisher")
	expect_equal(.getValidatedAlpha0Vec(design156), c(1, 1, 1, 1, 1), tolerance = 1e-07)



})

test_that("'getValidatedInformationRates': 'kMax' must be set correctly based on specified 'futilityBounds'", {

	design157 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design157)
	expect_equal(design157$kMax, 2, tolerance = 1e-07)

	design158 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design158)
	expect_equal(design158$kMax, 3, tolerance = 1e-07)

	design159 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design159)
	expect_equal(design159$kMax, 4, tolerance = 1e-07)

	design160 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design160)
	expect_equal(design160$kMax, 5, tolerance = 1e-07)

	design161 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design161)
	expect_equal(design161$kMax, 6, tolerance = 1e-07)

	design162 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design162)
	expect_equal(design162$kMax, 7, tolerance = 1e-07)

	design163 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design163)
	expect_equal(design163$kMax, 8, tolerance = 1e-07)

	design164 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design164)
	expect_equal(design164$kMax, 9, tolerance = 1e-07)

	design165 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design165)
	expect_equal(design165$kMax, 10, tolerance = 1e-07)

	design166 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design166)
	expect_equal(design166$kMax, 2, tolerance = 1e-07)

	design167 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design167)
	expect_equal(design167$kMax, 3, tolerance = 1e-07)

	design168 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design168)
	expect_equal(design168$kMax, 4, tolerance = 1e-07)

	design169 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design169)
	expect_equal(design169$kMax, 5, tolerance = 1e-07)

	design170 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design170)
	expect_equal(design170$kMax, 6, tolerance = 1e-07)

	design171 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design171)
	expect_equal(design171$kMax, 7, tolerance = 1e-07)

	design172 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design172)
	expect_equal(design172$kMax, 8, tolerance = 1e-07)

	design173 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design173)
	expect_equal(design173$kMax, 9, tolerance = 1e-07)

	design174 <- getTestDesign(futilityBounds = c(0, 0, 0, 0, 0, 0, 0, 1, 2), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design174)
	expect_equal(design174$kMax, 10, tolerance = 1e-07)




	design175 <- getTestDesign(futilityBounds = 2, designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design175)
	expect_equal(design175$kMax, 2, tolerance = 1e-07)

	design176 <- getTestDesign(futilityBounds = c(1, 2), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design176)
	expect_equal(design176$kMax, 3, tolerance = 1e-07)

	design177 <- getTestDesign(futilityBounds = c(0, 1, 2), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design177)
	expect_equal(design177$kMax, 4, tolerance = 1e-07)

	design178 <- getTestDesign(futilityBounds = c(0, 0, 1, 2), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design178)
	expect_equal(design178$kMax, 5, tolerance = 1e-07)

	design179 <- getTestDesign(futilityBounds = c(0, 0, 0, 1, 2), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design179)
	expect_equal(design179$kMax, 6, tolerance = 1e-07)



	design180 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design180)
	expect_equal(design180$kMax, 2, tolerance = 1e-07)

	design181 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design181)
	expect_equal(design181$kMax, 3, tolerance = 1e-07)

	design182 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design182)
	expect_equal(design182$kMax, 4, tolerance = 1e-07)

	design183 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design183)
	expect_equal(design183$kMax, 5, tolerance = 1e-07)

	design184 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design184)
	expect_equal(design184$kMax, 6, tolerance = 1e-07)

	design185 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design185)
	expect_equal(design185$kMax, 7, tolerance = 1e-07)

	design186 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design186)
	expect_equal(design186$kMax, 8, tolerance = 1e-07)

	design187 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design187)
	expect_equal(design187$kMax, 9, tolerance = 1e-07)

	design188 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignGroupSequential")
	.getValidatedFutilityBounds(design188)
	expect_equal(design188$kMax, 10, tolerance = 1e-07)

	design189 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design189)
	expect_equal(design189$kMax, 2, tolerance = 1e-07)

	design190 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design190)
	expect_equal(design190$kMax, 3, tolerance = 1e-07)

	design191 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design191)
	expect_equal(design191$kMax, 4, tolerance = 1e-07)

	design192 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design192)
	expect_equal(design192$kMax, 5, tolerance = 1e-07)

	design193 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design193)
	expect_equal(design193$kMax, 6, tolerance = 1e-07)

	design194 <- getTestDesign(informationRates = c(0.11428571, 0.22857143, 0.34285714, 0.45714286, 0.57142857, 0.68571429, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design194)
	expect_equal(design194$kMax, 7, tolerance = 1e-07)

	design195 <- getTestDesign(informationRates = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design195)
	expect_equal(design195$kMax, 8, tolerance = 1e-07)

	design196 <- getTestDesign(informationRates = c(0.088888889, 0.17777778, 0.26666667, 0.35555556, 0.44444444, 0.53333333, 0.62222222, 0.71111111, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design196)
	expect_equal(design196$kMax, 9, tolerance = 1e-07)

	design197 <- getTestDesign(informationRates = c(0.08, 0.16, 0.24, 0.32, 0.4, 0.48, 0.56, 0.64, 0.72, 1), designClass = "TrialDesignInverseNormal")
	.getValidatedFutilityBounds(design197)
	expect_equal(design197$kMax, 10, tolerance = 1e-07)




	design198 <- getTestDesign(informationRates = c(0.4, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design198)
	expect_equal(design198$kMax, 2, tolerance = 1e-07)

	design199 <- getTestDesign(informationRates = c(0.26666667, 0.53333333, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design199)
	expect_equal(design199$kMax, 3, tolerance = 1e-07)

	design200 <- getTestDesign(informationRates = c(0.2, 0.4, 0.6, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design200)
	expect_equal(design200$kMax, 4, tolerance = 1e-07)

	design201 <- getTestDesign(informationRates = c(0.16, 0.32, 0.48, 0.64, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design201)
	expect_equal(design201$kMax, 5, tolerance = 1e-07)

	design202 <- getTestDesign(informationRates = c(0.13333333, 0.26666667, 0.4, 0.53333333, 0.66666667, 1), designClass = "TrialDesignFisher")
	.getValidatedAlpha0Vec(design202)
	expect_equal(design202$kMax, 6, tolerance = 1e-07)



})

context("Testing utilities")


test_that("Testing '.toCapitalized'", {
	expect_equal(.toCapitalized("zip code"), "Zip Code")	
	expect_equal(.toCapitalized("state of the art"), "State of the Art")	
	expect_equal(.toCapitalized("final and count"), "Final and Count")	

})

test_that("Testing '.equalsRegexpIgnoreCase'	", {

	expect_equal(.equalsRegexpIgnoreCase("stage2", "^stages?$"), FALSE)
	expect_equal(.equalsRegexpIgnoreCase("stage", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("stages", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("Stage", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("STAGES", "^stages?$"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("stages2", "^stages?$"), FALSE)
	expect_equal(.equalsRegexpIgnoreCase(" stages", "^stages?$"), FALSE)

	expect_equal(.equalsRegexpIgnoreCase("stages2", "stages?"), TRUE)
	expect_equal(.equalsRegexpIgnoreCase("1stage2", "stages?"), TRUE)		

})

test_that("Testing 'isUndefinedArgument' and 'isValidArgument'", {

	expect_equal(.isUndefinedArgument(NULL), TRUE)
	expect_equal(.isUndefinedArgument(numeric(0)), TRUE)
	expect_equal(.isUndefinedArgument(NA), TRUE)
	expect_equal(.isUndefinedArgument(NA_integer_), TRUE)
	expect_equal(.isUndefinedArgument(NA_real_), TRUE)
	expect_equal(.isUndefinedArgument(NA_complex_), TRUE)
	expect_equal(.isUndefinedArgument(NA_character_), TRUE)
	expect_equal(.isUndefinedArgument(c(NA, NA)), FALSE)
	expect_equal(.isUndefinedArgument(c(1, NA, NA)), FALSE)
	expect_equal(.isUndefinedArgument(c(NA, NA, 1)), FALSE)
	expect_equal(.isUndefinedArgument(1), FALSE)

	expect_equal(.isDefinedArgument(NULL), FALSE)
	expect_equal(.isDefinedArgument(numeric(0)), FALSE)
	expect_equal(.isDefinedArgument(NA), FALSE)
	expect_equal(.isDefinedArgument(NA_integer_), FALSE)
	expect_equal(.isDefinedArgument(NA_real_), FALSE)
	expect_equal(.isDefinedArgument(NA_complex_), FALSE)
	expect_equal(.isDefinedArgument(NA_character_), FALSE)
	expect_equal(.isDefinedArgument(c(NA, NA)), TRUE)
	expect_equal(.isDefinedArgument(c(1, NA, NA)), TRUE)
	expect_equal(.isDefinedArgument(c(NA, NA, 1)), TRUE)
	expect_equal(.isDefinedArgument(1), TRUE)

	skip_if_translated()

	expect_error(.isDefinedArgument(notExistingTestVariable, argumentExistsValidationEnabled = FALSE),
		"object 'notExistingTestVariable' not found", fixed = TRUE)
	expect_error(.isDefinedArgument(notExistingTestVariable),
		"Missing argument: the object 'notExistingTestVariable' has not been defined anywhere. Please define it first, e.g., run 'notExistingTestVariable <- 1'", fixed = TRUE)

})

test_that("Result of 'setSeed(seed)' is working for different arguments, incl. NULL and NA", {

	# @refFS[Sec.]{fs:subsec:reproducibilityOfSimulationResults}
	expect_false(is.null(.setSeed()))
	expect_false(is.na(.setSeed()))
	expect_true(is.numeric(.setSeed()))

	expect_false(is.null(.setSeed(NULL)))
	expect_false(is.na(.setSeed(NULL)))
	expect_true(is.numeric(.setSeed(NULL)))

	expect_false(is.null(.setSeed(NA)))
	expect_false(is.na(.setSeed(NA)))
	expect_true(is.numeric(.setSeed(NA)))

	expect_true(.setSeed() != .setSeed())

	expect_equal(.setSeed(123), 123)
	expect_equal(.setSeed(0), 0)
	expect_equal(.setSeed(5e-5), 5e-5)

})

test_that("Testing '.getInputForZeroOutputInsideTolerance''", {

	input <- 99
	tolerance <- 1e-05
	epsilon <- 1e-08

	expect_equal(.getInputForZeroOutputInsideTolerance(input, tolerance, tolerance), input)
	expect_equal(.getInputForZeroOutputInsideTolerance(input, tolerance + epsilon, tolerance), NA_real_)
	expect_equal(.getInputForZeroOutputInsideTolerance(input, tolerance - epsilon, tolerance), input)

})

test_that("Testing '.arrayToString'", {

	expect_equal(.arrayToString(NA, vectorLookAndFeelEnabled = TRUE), "NA")
	expect_equal(.arrayToString(NULL, vectorLookAndFeelEnabled = TRUE), "NULL")
	expect_equal(.arrayToString(c(1, 2, 3), vectorLookAndFeelEnabled = TRUE), "c(1, 2, 3)")
	expect_equal(.arrayToString(c(NA, 2, 3), vectorLookAndFeelEnabled = TRUE), "c(NA, 2, 3)")
	expect_equal(.arrayToString(c(1, 2, NA), vectorLookAndFeelEnabled = TRUE), "c(1, 2, NA)")
	expect_equal(.arrayToString(c(NA, NA, NA), vectorLookAndFeelEnabled = TRUE), "c(NA, NA, NA)")
	expect_equal(.arrayToString(c(1, NULL, 3), vectorLookAndFeelEnabled = TRUE), "c(1, 3)")

})

test_that("Testing '.getInputProducingZeroOutput'", {

	tolerance <- 1e-05
	epsilon <- 1e-08

	expect_equal(.getInputProducingZeroOutput(1, 0, 2, 99, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, 99, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, 0, NA, 0, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(NA, 0, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, 0, NA, NA, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(NA, NA, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, 0, 2, NA, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, NA, 2, 0, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, 99, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, 99, 2, tolerance, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, tolerance + epsilon, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, tolerance + epsilon, 2, tolerance, tolerance), 2)

	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, tolerance - epsilon, tolerance), 2)
	expect_equal(.getInputProducingZeroOutput(1, tolerance - epsilon, 2, tolerance, tolerance), 1)

	expect_equal(.getInputProducingZeroOutput(1, tolerance - epsilon, 2, tolerance, tolerance), 1)
	expect_equal(.getInputProducingZeroOutput(1, tolerance, 2, tolerance - epsilon, tolerance), 2)

})

test_that("Testing '.getOneDimensionalRoot'", {

	tolerance <- 1e-08	

	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 2}, lower = -1, upper = 1, tolerance = tolerance), NA_real_)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 2}, lower = -1, upper = 1, tolerance = tolerance), NA_real_)

	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 1 - tolerance}, lower = -1, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 1 + tolerance}, lower = -1, upper = 1, tolerance = tolerance), -1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 1}, lower = -1, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 1}, lower = -1, upper = 1, tolerance = tolerance), -1)

	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 1}, lower = 0, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 1}, lower = tolerance, upper = 1, tolerance = tolerance), 1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 1}, lower = -1, upper = 0, tolerance = tolerance), -1)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 1}, lower = -1, upper = 1- tolerance, tolerance = tolerance), -1)

	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 3}, lower = 1, upper = 5, tolerance = tolerance), 3)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 3}, lower = -5, upper = -1, tolerance = tolerance), -3)

	expect_equal(.getOneDimensionalRoot(f = function(x) {3 * x - 700}, lower = 100, upper = 1000, tolerance = tolerance), 233.33333333)
	expect_equal(.getOneDimensionalRoot(f = function(x) {3 * x + 700}, lower = -1000, upper = -100, tolerance = tolerance), -233.33333333)

	expect_equal(.getOneDimensionalRoot(f = function(x) {x - 4}, lower = -10, upper = 10), 4, tolerance = tolerance)
	expect_equal(.getOneDimensionalRoot(f = function(x) {x + 4}, lower = -10, upper = 10), -4, tolerance = tolerance)

	dataExample1 <- getDataset(
		overallEvents = c(33, 55, 129),
		overallAllocationRatios = c(1, 1, 4),
		overallLogRanks = c(1.02, 1.38, 2.2)
	)
	design1 <- getDesignGroupSequential(kMax = 3, alpha = 0.025, typeOfDesign = "WT", deltaWT = 0.25)
	result1 <- getRepeatedConfidenceIntervals(design1, dataExample1, stage = 3) 
# [ERROR] in line 'getUnitTestObject(result1, "result1")': could not find function "expect_equal"

	design2 <- getDesignGroupSequential(kMax = 3, alpha = 0.025, informationRates = c(0.4, 0.7, 1),
		typeOfDesign = "WT", deltaWT = 0.35)
	dataExample2 <- getDataset(
		overallN2 = c(30,80,100),
		overallN1 = c(30,80,100),
		overallEvents2 = c(10,25,36),
		overallEvents1 = c(14,35,53))
	result2 <- getRepeatedConfidenceIntervals(design = design2, dataInput = dataExample2, 
		stage = 3, normalApproximation = T, directionUpper = TRUE)

	##
	## Comparison of the results of matrix object 'result2' with expected results
	##
	expect_equal(result2[1, ], c(-0.17491836, -0.048575353, 0.018957992), tolerance = 1e-07)
	expect_equal(result2[2, ], c(0.41834422, 0.29168781, 0.31353692), tolerance = 1e-07)

	design3 <- getDesignInverseNormal(kMax = 2, alpha = 0.025, informationRates = c(0.5, 1),  
		typeOfDesign = "WT", deltaWT = 0.25)
	dataExample3 <- getDataset(
		events1 = c(7,57),
		events2 = c(7,57),
		n1 = c(30,300),
		n2 = c(30,300)
	) 
	result3 <- getRepeatedConfidenceIntervals(design3, dataExample3)

	##
	## Comparison of the results of matrix object 'result3' with expected results
	##
	expect_equal(result3[1, ], c(-0.26729325, -0.071745801), tolerance = 1e-07)
	expect_equal(result3[2, ], c(0.26729325, 0.071745801), tolerance = 1e-07)

	design4 <- getDesignInverseNormal(kMax = 2, alpha = 0.025, informationRates = c(0.5, 1),  
		typeOfDesign = "WT", deltaWT = 0.25)
	dataExample4 <- getDataset(
		events1 = c(4,55),
		events2 = c(4,46),
		n1 = c(30,300),
		n2 = c(30,300))
	result4 <- getRepeatedConfidenceIntervals(design4, dataExample4)

	##
	## Comparison of the results of matrix object 'result4' with expected results
	##
	expect_equal(result4[1, ], c(-0.23589449, -0.043528513), tolerance = 1e-07)
	expect_equal(result4[2, ], c(0.23589449, 0.088471324), tolerance = 1e-07)

})

