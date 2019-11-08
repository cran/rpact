######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 07 November 2019, 10:22:23                                                   #
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

context("Testing class 'PiecewiseSurvivalTime'")


test_that("Testing 'getPiecewiseSurvivalTime': isPiecewiseSurvivalEnabled()", {
	expect_false(getPiecewiseSurvivalTime()$isPiecewiseSurvivalEnabled())
	expect_false(getPiecewiseSurvivalTime(piecewiseSurvivalTime = NA)$isPiecewiseSurvivalEnabled())

})

test_that("Testing 'getPiecewiseSurvivalTime': simple vector based definition", {

	pwSurvivalTime1 <- getPiecewiseSurvivalTime(lambda2 = 0.5, hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime1' with expected results
	##
	expect_equal(pwSurvivalTime1$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime1$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$lambda2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$pi1, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$pi2, 0.99752125, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$median2, 1.3862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime1$eventTime, 12)
	expect_equal(pwSurvivalTime1$kappa, 1)
	expect_equal(pwSurvivalTime1$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime1$delayedResponseEnabled, FALSE)

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(lambda2 = 0.5, lambda1 = 0.4)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	##
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime2$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi2, 0.99752125, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median2, 1.3862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$eventTime, 12)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(pi2 = 0.5, hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	##
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime2$lambda1, 0.046209812, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.057762265, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, 0.42565082, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median1, 15)
	expect_equal(pwSurvivalTime2$median2, 12)
	expect_equal(pwSurvivalTime2$eventTime, 12)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(pi2 = 0.5, pi1 = 0.4)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime2' with expected results
	##
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime2$lambda1, 0.042568802, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$lambda2, 0.057762265, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$hazardRatio, 0.73696559, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$pi2, 0.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median1, 16.282985, tolerance = 1e-07)
	expect_equal(pwSurvivalTime2$median2, 12)
	expect_equal(pwSurvivalTime2$eventTime, 12)
	expect_equal(pwSurvivalTime2$kappa, 1)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime2$delayedResponseEnabled, FALSE)

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime3' with expected results
	##
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime3$lambda1, c(0.24, 0.32), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$hazardRatio, c(0.6, 0.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$pi1, c(0.94386524, 0.9785064), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$pi2, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$median1, c(2.8881133, 2.1660849), tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime3$eventTime, 12)
	expect_equal(pwSurvivalTime3$kappa, 1)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime3$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime3$delayedResponseEnabled, FALSE)

	pwSurvivalTime4 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4, pi1 = 0.3)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime4' with expected results
	##
	expect_equal(pwSurvivalTime4$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime4$lambda1, c(0.24, 0.32), tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$hazardRatio, c(0.6, 0.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$pi1, c(0.94386524, 0.9785064), tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$pi2, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$median1, c(2.8881133, 2.1660849), tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime4$eventTime, 12)
	expect_equal(pwSurvivalTime4$kappa, 1)
	expect_equal(pwSurvivalTime4$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime4$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime4$delayedResponseEnabled, FALSE)

	pwSurvivalTime5 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4, pi1 = 0.3)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime5' with expected results
	##
	expect_equal(pwSurvivalTime5$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime5$lambda1, c(0.24, 0.32), tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$hazardRatio, c(0.6, 0.8), tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$pi1, c(0.94386524, 0.9785064), tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$pi2, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$median1, c(2.8881133, 2.1660849), tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime5$eventTime, 12)
	expect_equal(pwSurvivalTime5$kappa, 1)
	expect_equal(pwSurvivalTime5$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime5$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime5$delayedResponseEnabled, FALSE)

	pwSurvivalTime6 <- getPiecewiseSurvivalTime(lambda2 = 0.4, lambda1 = 0.3, pi2 = 0.4, pi1 = 0.3)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime6' with expected results
	##
	expect_equal(pwSurvivalTime6$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime6$lambda1, 0.3, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$hazardRatio, 0.75, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$pi1, 0.97267628, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$pi2, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$median1, 2.3104906, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime6$eventTime, 12)
	expect_equal(pwSurvivalTime6$kappa, 1)
	expect_equal(pwSurvivalTime6$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime6$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime6$delayedResponseEnabled, FALSE)

	pwSurvivalTime7 <- getPiecewiseSurvivalTime(lambda2 = 0.4, lambda1 = 0.3, pi2 = 0.4, pi1 = 0.3)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime7' with expected results
	##
	expect_equal(pwSurvivalTime7$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime7$lambda1, 0.3, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$hazardRatio, 0.75, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$pi1, 0.97267628, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$pi2, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$median1, 2.3104906, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime7$eventTime, 12)
	expect_equal(pwSurvivalTime7$kappa, 1)
	expect_equal(pwSurvivalTime7$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime7$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime7$delayedResponseEnabled, FALSE)

	pwSurvivalTime8 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi2 = 0.4, pi1 = 0.3)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime8' with expected results
	##
	expect_equal(pwSurvivalTime8$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime8$lambda1, 0.029722912, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$lambda2, 0.042568802, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$hazardRatio, 0.69823229, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$pi1, 0.3, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$pi2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$median1, 23.320299, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$median2, 16.282985, tolerance = 1e-07)
	expect_equal(pwSurvivalTime8$eventTime, 12)
	expect_equal(pwSurvivalTime8$kappa, 1)
	expect_equal(pwSurvivalTime8$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime8$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime8$delayedResponseEnabled, FALSE)

	pwSurvivalTime9 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi1 = 0.3)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime9' with expected results
	##
	expect_equal(pwSurvivalTime9$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime9$lambda1, 0.029722912, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$hazardRatio, 1.5984103, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$pi1, 0.3, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$pi2, 0.2, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$median1, 23.320299, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$median2, 37.275405, tolerance = 1e-07)
	expect_equal(pwSurvivalTime9$eventTime, 12)
	expect_equal(pwSurvivalTime9$kappa, 1)
	expect_equal(pwSurvivalTime9$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime9$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime9$delayedResponseEnabled, FALSE)

	pwSurvivalTime10 <- getPiecewiseSurvivalTime(median2 = 1.386294, hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime10' with expected results
	##
	expect_equal(pwSurvivalTime10$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime10$lambda1, 0.4000001, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$lambda2, 0.50000013, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$pi1, 0.99177026, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$pi2, 0.99752125, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$median1, 1.7328675, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$median2, 1.386294, tolerance = 1e-07)
	expect_equal(pwSurvivalTime10$eventTime, 12)
	expect_equal(pwSurvivalTime10$kappa, 1)
	expect_equal(pwSurvivalTime10$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime10$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime10$delayedResponseEnabled, FALSE)

	pwSurvivalTime11 <- getPiecewiseSurvivalTime(median2 = 1.386294, lambda1 = 0.4)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime11' with expected results
	##
	expect_equal(pwSurvivalTime11$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime11$lambda1, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$lambda2, 0.50000013, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$hazardRatio, 0.79999979, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$pi1, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$pi2, 0.99752125, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$median1, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$median2, 1.386294, tolerance = 1e-07)
	expect_equal(pwSurvivalTime11$eventTime, 12)
	expect_equal(pwSurvivalTime11$kappa, 1)
	expect_equal(pwSurvivalTime11$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime11$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime11$delayedResponseEnabled, FALSE)

	pwSurvivalTime12 <- getPiecewiseSurvivalTime(median2 = 5, median1 = 6)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime12' with expected results
	##
	expect_equal(pwSurvivalTime12$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime12$lambda1, 0.11552453, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$lambda2, 0.13862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$hazardRatio, 0.83333333, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$pi1, 0.75, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$pi2, 0.81053543, tolerance = 1e-07)
	expect_equal(pwSurvivalTime12$median1, 6)
	expect_equal(pwSurvivalTime12$median2, 5)
	expect_equal(pwSurvivalTime12$eventTime, 12)
	expect_equal(pwSurvivalTime12$kappa, 1)
	expect_equal(pwSurvivalTime12$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime12$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime12$delayedResponseEnabled, FALSE)

	pwSurvivalTime13 <- getPiecewiseSurvivalTime(median2 = 1.386294, lambda1 = c(0.3, 0.4))

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime13' with expected results
	##
	expect_equal(pwSurvivalTime13$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime13$lambda1, c(0.3, 0.4), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$lambda2, 0.50000013, tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$hazardRatio, c(0.59999984, 0.79999979), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$pi1, c(0.97267628, 0.99177025), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$pi2, 0.99752125, tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$median1, c(2.3104906, 1.732868), tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$median2, 1.386294, tolerance = 1e-07)
	expect_equal(pwSurvivalTime13$eventTime, 12)
	expect_equal(pwSurvivalTime13$kappa, 1)
	expect_equal(pwSurvivalTime13$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime13$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime13$delayedResponseEnabled, FALSE)

	pwSurvivalTime14 <- getPiecewiseSurvivalTime(median2 = 5, median1 = c(6:8))

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime14' with expected results
	##
	expect_equal(pwSurvivalTime14$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime14$lambda1, c(0.11552453, 0.099021026, 0.086643398), tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$lambda2, 0.13862944, tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$hazardRatio, c(0.83333333, 0.71428571, 0.625), tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$pi1, c(0.75, 0.69524659, 0.64644661), tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$pi2, 0.81053543, tolerance = 1e-07)
	expect_equal(pwSurvivalTime14$median1, c(6, 7, 8))
	expect_equal(pwSurvivalTime14$median2, 5)
	expect_equal(pwSurvivalTime14$eventTime, 12)
	expect_equal(pwSurvivalTime14$kappa, 1)
	expect_equal(pwSurvivalTime14$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime14$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime14$delayedResponseEnabled, FALSE)

	pwSurvivalTime15 <- getPiecewiseSurvivalTime(median2 = 2, hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime15' with expected results
	##
	expect_equal(pwSurvivalTime15$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime15$lambda1, 0.27725887, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$lambda2, 0.34657359, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$pi1, 0.96410318, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$pi2, 0.984375, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$median1, 2.5, tolerance = 1e-07)
	expect_equal(pwSurvivalTime15$median2, 2)
	expect_equal(pwSurvivalTime15$eventTime, 12)
	expect_equal(pwSurvivalTime15$kappa, 1)
	expect_equal(pwSurvivalTime15$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime15$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime15$delayedResponseEnabled, FALSE)

	pwSurvivalTime16 <- getPiecewiseSurvivalTime(median1 = c(2, 3), hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime16' with expected results
	##
	expect_equal(pwSurvivalTime16$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime16$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$pi1, c(0.984375, 0.9375), tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$pi2, 0.2, tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$median1, c(2, 3))
	expect_equal(pwSurvivalTime16$median2, 37.275405, tolerance = 1e-07)
	expect_equal(pwSurvivalTime16$eventTime, 12)
	expect_equal(pwSurvivalTime16$kappa, 1)
	expect_equal(pwSurvivalTime16$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime16$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime16$delayedResponseEnabled, FALSE)

	pwSurvivalTime17 <- getPiecewiseSurvivalTime(median1 = c(2, 3), median2 = 4)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime17' with expected results
	##
	expect_equal(pwSurvivalTime17$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime17$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$lambda2, 0.1732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$hazardRatio, c(2, 1.3333333), tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$pi1, c(0.984375, 0.9375), tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$pi2, 0.875, tolerance = 1e-07)
	expect_equal(pwSurvivalTime17$median1, c(2, 3))
	expect_equal(pwSurvivalTime17$median2, 4)
	expect_equal(pwSurvivalTime17$eventTime, 12)
	expect_equal(pwSurvivalTime17$kappa, 1)
	expect_equal(pwSurvivalTime17$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime17$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime17$delayedResponseEnabled, FALSE)

	pwSurvivalTime18 <- getPiecewiseSurvivalTime(median1 = c(2, 3), lambda2 = 0.4)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime18' with expected results
	##
	expect_equal(pwSurvivalTime18$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime18$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$lambda2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$hazardRatio, c(0.86643398, 0.57762265), tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$pi1, c(0.984375, 0.9375), tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$pi2, 0.99177025, tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$median1, c(2, 3))
	expect_equal(pwSurvivalTime18$median2, 1.732868, tolerance = 1e-07)
	expect_equal(pwSurvivalTime18$eventTime, 12)
	expect_equal(pwSurvivalTime18$kappa, 1)
	expect_equal(pwSurvivalTime18$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime18$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime18$delayedResponseEnabled, FALSE)

	pwSurvivalTime19 <- getPiecewiseSurvivalTime(median1 = c(2, 3), pi2 = 0.4) 

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime19' with expected results
	##
	expect_equal(pwSurvivalTime19$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime19$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$lambda2, 0.042568802, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$hazardRatio, c(8.1414927, 5.4276618), tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$pi1, c(0.984375, 0.9375), tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$pi2, 0.4, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$median1, c(2, 3))
	expect_equal(pwSurvivalTime19$median2, 16.282985, tolerance = 1e-07)
	expect_equal(pwSurvivalTime19$eventTime, 12)
	expect_equal(pwSurvivalTime19$kappa, 1)
	expect_equal(pwSurvivalTime19$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime19$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime19$delayedResponseEnabled, FALSE)

	pwSurvivalTime20 <- getPiecewiseSurvivalTime(median1 = c(2, 3), hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime20' with expected results
	##
	expect_equal(pwSurvivalTime20$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime20$lambda1, c(0.34657359, 0.23104906), tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$pi1, c(0.984375, 0.9375), tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$pi2, 0.2, tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$median1, c(2, 3))
	expect_equal(pwSurvivalTime20$median2, 37.275405, tolerance = 1e-07)
	expect_equal(pwSurvivalTime20$eventTime, 12)
	expect_equal(pwSurvivalTime20$kappa, 1)
	expect_equal(pwSurvivalTime20$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime20$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime20$delayedResponseEnabled, FALSE)

	pwSurvivalTime21 <- getPiecewiseSurvivalTime(median1 = 3, hazardRatio = 0.8)

	##
	## Comparison of the results of PiecewiseSurvivalTime object 'pwSurvivalTime21' with expected results
	##
	expect_equal(pwSurvivalTime21$piecewiseSurvivalTime, NA_real_)
	expect_equal(pwSurvivalTime21$lambda1, 0.23104906, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$lambda2, 0.018595296, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$hazardRatio, 0.8, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$pi1, 0.9375, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$pi2, 0.2, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$median1, 3)
	expect_equal(pwSurvivalTime21$median2, 37.275405, tolerance = 1e-07)
	expect_equal(pwSurvivalTime21$eventTime, 12)
	expect_equal(pwSurvivalTime21$kappa, 1)
	expect_equal(pwSurvivalTime21$piecewiseSurvivalEnabled, FALSE)
	expect_equal(pwSurvivalTime21$delayedResponseAllowed, FALSE)
	expect_equal(pwSurvivalTime21$delayedResponseEnabled, FALSE)

	getPiecewiseSurvivalTime(median1 = c(2, 3), lambda2 = 0.8)

	expect_error(getPiecewiseSurvivalTime(median2 = 1.386294, lambda2 = 0.4, hazardRatio = 0.8)) 
	expect_error(getPiecewiseSurvivalTime(median2 = c(1.5, 1.7), lambda1 = c(0.3, 0.4)))

})

test_that("Testing 'getPiecewiseSurvivalTime': vector based definition", {

	pwSurvivalTime1 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
		lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8)
	expect_equal(pwSurvivalTime1$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime1$lambda1,  c(0.025, 0.04, 0.015) * 0.8)
	expect_false(pwSurvivalTime1$isDelayedResponseEnabled())

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 5, 10), 
		lambda2 = c(0.1, 0.2, 0.8), hazardRatio = 0.8)
	expect_true(pwSurvivalTime2$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime2$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime2$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(pwSurvivalTime2$lambda2, c(0.1, 0.2, 0.8))

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = 0.8)
	expect_true(pwSurvivalTime3$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime3$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime3$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, c(0, 6))
	expect_equal(pwSurvivalTime3$lambda2, c(0.01, 0.03))

	pwSurvivalTime4 <- getPiecewiseSurvivalTime(0, lambda2 = 0.01, hazardRatio = 0.8)
	expect_true(pwSurvivalTime4$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime4$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime4$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime4$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime4$lambda2, 0.01)
	expect_equal(pwSurvivalTime4$lambda1, 0.01 * 0.8)

	pwSurvivalTime5 <- getPiecewiseSurvivalTime(NA_real_, lambda2 = 0.01, hazardRatio = 0.8)
	expect_true(pwSurvivalTime5$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime5$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime5$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime5$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime5$lambda2, 0.01)
	expect_equal(pwSurvivalTime5$lambda1, 0.01 * 0.8)

	pwSurvivalTime6 <- getPiecewiseSurvivalTime(0, lambda2 = 0.01, lambda1 = 0.008)
	expect_true(pwSurvivalTime6$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime6$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime6$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime6$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime6$lambda2, 0.01)
	expect_equal(pwSurvivalTime6$lambda1, 0.008)

	pwSurvivalTime7 <- getPiecewiseSurvivalTime(NA_real_, lambda2 = 0.01, lambda1 = 0.008)
	expect_true(pwSurvivalTime7$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime7$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime7$hazardRatio,  0.8)
	expect_equal(pwSurvivalTime7$piecewiseSurvivalTime, 0)
	expect_equal(pwSurvivalTime7$lambda2, 0.01)
	expect_equal(pwSurvivalTime7$lambda1, 0.008)

	# case 2.2
	pwSurvivalTime9 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
		lambda2 = c(0.025, 0.04, 0.015), 
		lambda1 = c(0.025, 0.04, 0.015) * 0.8)
	expect_true(pwSurvivalTime9$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime9$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime9$hazardRatio,  0.8)

	# case 2.2: error expected
	expect_error(getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
			lambda2 = c(0.025, 0.04, 0.015), 
			lambda1 = c(0.03, 0.04, 0.025)), 
		paste0("Illegal argument: 'hazardRatio' can only be calculated if ", 
			"'unique(lambda1 / lambda2)' result in a single value; ", 
			"current result = c(1.2, 1, 1.667) (delayed response is not allowed)"), fixed = TRUE)

	# case 3
	expect_false(getPiecewiseSurvivalTime(delayedResponseAllowed = TRUE)$isPiecewiseSurvivalEnabled())
	expect_false(getPiecewiseSurvivalTime(piecewiseSurvivalTime = NA, 
			delayedResponseAllowed = TRUE)$isPiecewiseSurvivalEnabled())

	# case 3.1
	pwSurvivalTimeSim1 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
		lambda2 = c(0.025, 0.04, 0.015), hazardRatio = 0.8, 
		delayedResponseAllowed = TRUE)
	expect_equal(pwSurvivalTimeSim1$hazardRatio,  0.8)
	expect_equal(pwSurvivalTimeSim1$lambda1,  c(0.025, 0.04, 0.015) * 0.8)
	expect_false(pwSurvivalTimeSim1$isDelayedResponseEnabled())

	# case 3.2
	pwSurvivalTimeSim2 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 6, 9), 
		lambda2 = c(0.025, 0.04, 0.015), 
		lambda1 = c(0.03, 0.04, 0.025), delayedResponseAllowed = TRUE)
	expect_true(pwSurvivalTimeSim2$isPiecewiseSurvivalEnabled())
	expect_true(pwSurvivalTimeSim2$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTimeSim2$hazardRatio,  c(1.2, 1, 5/3))

	pwsTime1 <- getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4)
	expect_equal(pwsTime1$.isLambdaBased(minNumberOfLambdas = 1), TRUE)

})

test_that("Testing 'getPiecewiseSurvivalTime': check warnings", {

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4),
		"'pi2' (0.4) will be ignored", fixed = TRUE)

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4, pi1 = 0.3),
		"'pi1' (0.3) will be ignored", fixed = TRUE)
	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), lambda2 = 0.4, pi2 = 0.4, pi1 = 0.3),
		"'pi2' (0.4) will be ignored", fixed = TRUE)

	expect_warning(getPiecewiseSurvivalTime(lambda2 = 0.4, lambda1 = 0.3, pi2 = 0.4, pi1 = 0.3),
		"'pi1' (0.3) will be ignored", fixed = TRUE)
	expect_warning(getPiecewiseSurvivalTime(lambda2 = 0.4, lambda1 = 0.3, pi2 = 0.4, pi1 = 0.3),
		"'pi2' (0.4) will be ignored", fixed = TRUE)

	expect_equal(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi2 = 0.4)$.isPiBased(), TRUE)

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi2 = 0.4, pi1 = 0.3),
		"'hazardRatio' (0.6, 0.8) will be ignored because it will be calculated", fixed = TRUE)

	expect_warning(getPiecewiseSurvivalTime(hazardRatio = c(0.6, 0.8), pi1 = 0.3),
		"'hazardRatio' (0.6, 0.8) will be ignored because it will be calculated", fixed = TRUE)

})

test_that("Testing 'getPiecewiseSurvivalTime': list-wise definition", {

	pwSurvivalTime8 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = list(
			"<6"       = 0.025, 
			"6 - <9"   = 0.04, 
			"9 - <15"  = 0.015, 
			"15 - <21" = 0.01, 
			">=21"     = 0.007), hazardRatio = 0.6)
	expect_true(pwSurvivalTime8$isPiecewiseSurvivalEnabled())
	expect_false(pwSurvivalTime8$isDelayedResponseEnabled())
	expect_equal(pwSurvivalTime8$hazardRatio,  0.6)
	expect_equal(pwSurvivalTime8$piecewiseSurvivalTime, c(0, 6,9, 15, 21))
	expect_equal(pwSurvivalTime8$lambda2, c(0.025, 0.040, 0.015, 0.010, 0.007))
	expect_equal(pwSurvivalTime8$lambda1, c(0.0150, 0.0240, 0.0090, 0.0060, 0.0042))

	result1 <- getPiecewiseSurvivalTime(list(
		"<5" = 0.1, 
		"5 - <10" = 0.2, 
		">=10" = 0.8), hazardRatio = 0.8)
	expect_equal(result1$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(result1$lambda2, c(0.1, 0.2, 0.8))

	result2 <- getPiecewiseSurvivalTime(list(
			"0 - <5" = 0.1, 
			"5 - <10" = 0.2, 
			"10 - Inf" = 0.8), hazardRatio = 0.8)
	expect_equal(result2$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(result2$lambda2, c(0.1, 0.2, 0.8))

	pwSurvivalTime2 <- getPiecewiseSurvivalTime(piecewiseSurvivalTime = c(0, 5, 10), 
		lambda2 = c(0.1, 0.2, 0.8), hazardRatio = 0.8)
	expect_equal(pwSurvivalTime2$piecewiseSurvivalTime, c(0, 5, 10))
	expect_equal(pwSurvivalTime2$lambda2, c(0.1, 0.2, 0.8))

	pwSurvivalTime3 <- getPiecewiseSurvivalTime(c(0, 6), lambda2 = c(0.01, 0.03), hazardRatio = 0.8)
	expect_equal(pwSurvivalTime3$piecewiseSurvivalTime, c(0, 6))
	expect_equal(pwSurvivalTime3$lambda2, c(0.01, 0.03))

})

context("Testing class 'AccrualTime'")


test_that("Testing 'getAccrualTime': isAccrualTimeEnabled()", {
	expect_true(getAccrualTime()$isAccrualTimeEnabled())
	expect_true(getAccrualTime(maxNumberOfSubjects = 100)$isAccrualTimeEnabled())

})

test_that("Testing 'getAccrualTime': vector based definition", {

	accrualTime1 <- getAccrualTime(accrualTime = c(0, 6, 9, 15), 
		accrualIntensity = c(15, 21, 27), maxNumberOfSubjects = 315)
	expect_equal(accrualTime1$accrualTime,  c(0, 6, 9, 15))
	expect_equal(accrualTime1$accrualIntensity,  c(15, 21, 27))
	expect_equal(accrualTime1$remainingTime, 6)

	accrualTime2 <- getAccrualTime(accrualTime = c(0, 6, 9), 
		accrualIntensity = c(15, 21, 27), maxNumberOfSubjects = 1000)
	expect_equal(accrualTime2$accrualTime,  c(0, 6, 9, 40.37037))
	expect_equal(accrualTime2$accrualIntensity,  c(15, 21, 27))
	expect_equal(accrualTime2$remainingTime, 31.37037)

	accrualTime3 <- getAccrualTime(accrualTime = c(0, 12, 13, 14, 15, 16), 
		accrualIntensity = c(15, 21, 27, 33, 39, 45), maxNumberOfSubjects = 1405)
	expect_equal(accrualTime3$accrualTime,      c( 0, 12, 13, 14, 15, 16, 40.55555556))
	expect_equal(accrualTime3$accrualIntensity, c(15, 21, 27, 33, 39, 45))
	expect_equal(accrualTime3$remainingTime, 24.55555556)

	accrualTime4 <- getAccrualTime(accrualTime = c(0, 24), 
		accrualIntensity = c(30, 45), maxNumberOfSubjects = 720)

	##
	## Comparison of the results of AccrualTime object 'accrualTime4' with expected results
	##
	expect_equal(accrualTime4$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime4$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime4$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime4$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime4$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime4$accrualTime, c(0, 24))
	expect_equal(accrualTime4$accrualIntensity, 30)
	expect_equal(accrualTime4$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime4$maxNumberOfSubjects, 720)
	expect_equal(accrualTime4$remainingTime, 24)
	expect_equal(accrualTime4$piecewiseAccrualEnabled, FALSE)

	accrualTime5 <- getAccrualTime(accrualTime = c(0, 24, 30), 
		accrualIntensity = c(30, 45))

	##
	## Comparison of the results of AccrualTime object 'accrualTime5' with expected results
	##
	expect_equal(accrualTime5$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime5$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime5$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime5$accrualTime, c(0, 24, 30))
	expect_equal(accrualTime5$accrualIntensity, c(30, 45))
	expect_equal(accrualTime5$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime5$maxNumberOfSubjects, 990)
	expect_equal(accrualTime5$remainingTime, 6)
	expect_equal(accrualTime5$piecewiseAccrualEnabled, TRUE)

	accrualTime6 <- getAccrualTime(accrualTime = c(0, 24, 30), 
		accrualIntensity = c(30, 45, 55), maxNumberOfSubjects = 720)

	##
	## Comparison of the results of AccrualTime object 'accrualTime6' with expected results
	##
	expect_equal(accrualTime6$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime6$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime6$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime6$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime6$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime6$accrualTime, c(0, 24))
	expect_equal(accrualTime6$accrualIntensity, 30)
	expect_equal(accrualTime6$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime6$maxNumberOfSubjects, 720)
	expect_equal(accrualTime6$remainingTime, 24)
	expect_equal(accrualTime6$piecewiseAccrualEnabled, FALSE)

	accrualTime7 <- getAccrualTime(accrualTime = c(0, 24, 30, 40), accrualIntensity = c(30, 45, 55, 66), maxNumberOfSubjects = 720)

	##
	## Comparison of the results of AccrualTime object 'accrualTime7' with expected results
	##
	expect_equal(accrualTime7$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime7$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime7$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime7$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime7$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime7$accrualTime, c(0, 24))
	expect_equal(accrualTime7$accrualIntensity, 30)
	expect_equal(accrualTime7$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime7$maxNumberOfSubjects, 720)
	expect_equal(accrualTime7$remainingTime, 24)
	expect_equal(accrualTime7$piecewiseAccrualEnabled, FALSE)

	accrualTime8 <- getAccrualTime(accrualTime = 0, accrualIntensity = 15, maxNumberOfSubjects = 1000)

	##
	## Comparison of the results of AccrualTime object 'accrualTime8' with expected results
	##
	expect_equal(accrualTime8$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime8$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime8$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime8$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime8$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime8$accrualTime, c(0, 66.666667), tolerance = 1e-07)
	expect_equal(accrualTime8$accrualIntensity, 15)
	expect_equal(accrualTime8$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime8$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime8$remainingTime, 66.666667, tolerance = 1e-07)
	expect_equal(accrualTime8$piecewiseAccrualEnabled, FALSE)

	accrualTime9 <- getAccrualTime(accrualTime = c(0, 5), accrualIntensity = 15)

	##
	## Comparison of the results of AccrualTime object 'accrualTime9' with expected results
	##
	expect_equal(accrualTime9$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime9$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime9$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime9$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime9$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime9$accrualTime, c(0, 5))
	expect_equal(accrualTime9$accrualIntensity, 15)
	expect_equal(accrualTime9$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime9$maxNumberOfSubjects, 75)
	expect_equal(accrualTime9$remainingTime, 5)
	expect_equal(accrualTime9$piecewiseAccrualEnabled, FALSE)

	accrualTime10 <- getAccrualTime(accrualTime = 0, accrualIntensity = 15, maxNumberOfSubjects = 10)

	##
	## Comparison of the results of AccrualTime object 'accrualTime10' with expected results
	##
	expect_equal(accrualTime10$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime10$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime10$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime10$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime10$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime10$accrualTime, c(0, 0.66666667), tolerance = 1e-07)
	expect_equal(accrualTime10$accrualIntensity, 15)
	expect_equal(accrualTime10$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime10$maxNumberOfSubjects, 10)
	expect_equal(accrualTime10$remainingTime, 0.66666667, tolerance = 1e-07)
	expect_equal(accrualTime10$piecewiseAccrualEnabled, FALSE)

	accrualTime11 <- getAccrualTime(accrualTime = c(0, 5), accrualIntensity = 15, maxNumberOfSubjects = 10)

	##
	## Comparison of the results of AccrualTime object 'accrualTime11' with expected results
	##
	expect_equal(accrualTime11$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime11$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime11$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime11$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime11$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime11$accrualTime, c(0, 0.66666667), tolerance = 1e-07)
	expect_equal(accrualTime11$accrualIntensity, 15)
	expect_equal(accrualTime11$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime11$maxNumberOfSubjects, 10)
	expect_equal(accrualTime11$remainingTime, 0.66666667, tolerance = 1e-07)
	expect_equal(accrualTime11$piecewiseAccrualEnabled, FALSE)

	accrualTime12 <- getAccrualTime(accrualTime = c(0, 6, 15, 25), accrualIntensity = c(22, 0, 33))

	##
	## Comparison of the results of AccrualTime object 'accrualTime12' with expected results
	##
	expect_equal(accrualTime12$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime12$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime12$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime12$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime12$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime12$accrualTime, c(0, 6, 15, 25))
	expect_equal(accrualTime12$accrualIntensity, c(22, 0, 33))
	expect_equal(accrualTime12$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime12$maxNumberOfSubjects, 462)
	expect_equal(accrualTime12$remainingTime, 10)
	expect_equal(accrualTime12$piecewiseAccrualEnabled, TRUE)

	accrualTime13 <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000)

	##
	## Comparison of the results of AccrualTime object 'accrualTime13' with expected results
	##
	expect_equal(accrualTime13$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime13$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime13$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime13$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime13$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime13$accrualTime, c(0, 6, 32.30303), tolerance = 1e-07)
	expect_equal(accrualTime13$accrualIntensity, c(22, 33))
	expect_equal(accrualTime13$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime13$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime13$remainingTime, 26.30303, tolerance = 1e-07)
	expect_equal(accrualTime13$piecewiseAccrualEnabled, TRUE)

})

test_that("Testing 'getAccrualTime': test absolute and relative definition", {

	accrualTime1 <- getAccrualTime(accrualTime = c(0, 6, 30), 
		accrualIntensity = c(22, 33), maxNumberOfSubjects = 924) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime1' with expected results
	##
	expect_equal(accrualTime1$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime1$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime1$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime1$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime1$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime1$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime1$accrualIntensity, c(22, 33))
	expect_equal(accrualTime1$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime1$maxNumberOfSubjects, 924)
	expect_equal(accrualTime1$remainingTime, 24)
	expect_equal(accrualTime1$piecewiseAccrualEnabled, TRUE)

	accrualTime2 <- getAccrualTime(list(
			"0 - <6"   = 22,
			"6 - <=30" = 33), 
		maxNumberOfSubjects = 924) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime2' with expected results
	##
	expect_equal(accrualTime2$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime2$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime2$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime2$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime2$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime2$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime2$accrualIntensity, c(22, 33))
	expect_equal(accrualTime2$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime2$maxNumberOfSubjects, 924)
	expect_equal(accrualTime2$remainingTime, 24)
	expect_equal(accrualTime2$piecewiseAccrualEnabled, TRUE)

	accrualTime3 <- getAccrualTime(accrualTime = c(0, 6, 30), 
		accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime3' with expected results
	##
	expect_equal(accrualTime3$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime3$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime3$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime3$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime3$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime3$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime3$accrualIntensity, c(23.809524, 35.714286), tolerance = 1e-07)
	expect_equal(accrualTime3$accrualIntensityRelative, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime3$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime3$remainingTime, 24)
	expect_equal(accrualTime3$piecewiseAccrualEnabled, TRUE)

	accrualTime4 <- getAccrualTime(list(
			"0 - <6"   = 0.22,
			"6 - <=30" = 0.33), 
		maxNumberOfSubjects = 1000) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime4' with expected results
	##
	expect_equal(accrualTime4$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime4$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime4$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime4$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime4$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime4$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime4$accrualIntensity, c(23.809524, 35.714286), tolerance = 1e-07)
	expect_equal(accrualTime4$accrualIntensityRelative, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime4$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime4$remainingTime, 24)
	expect_equal(accrualTime4$piecewiseAccrualEnabled, TRUE)

	accrualTime5 <- getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime5' with expected results
	##
	expect_equal(accrualTime5$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime5$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime5$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime5$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime5$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime5$accrualIntensity, c(22, 33))
	expect_equal(accrualTime5$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime5$maxNumberOfSubjects, 924)
	expect_equal(accrualTime5$remainingTime, 24)
	expect_equal(accrualTime5$piecewiseAccrualEnabled, TRUE)

	accrualTime6 <- getAccrualTime(list(
			"0 - <6"   = 22,
			"6 - <=30" = 33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime6' with expected results
	##
	expect_equal(accrualTime6$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime6$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime6$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime6$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime6$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime6$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime6$accrualIntensity, c(22, 33))
	expect_equal(accrualTime6$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime6$maxNumberOfSubjects, 924)
	expect_equal(accrualTime6$remainingTime, 24)
	expect_equal(accrualTime6$piecewiseAccrualEnabled, TRUE)

	accrualTime7 <- getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(0.22, 0.33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime7' with expected results
	##
	expect_equal(accrualTime7$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime7$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime7$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime7$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime7$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime7$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime7$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime7$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime7$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime7$remainingTime, NA_real_)
	expect_equal(accrualTime7$piecewiseAccrualEnabled, TRUE)

	accrualTime8 <- getAccrualTime(list(
			"0 - <6"   = 0.22,
			"6 - <=30" = 0.33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime8' with expected results
	##
	expect_equal(accrualTime8$endOfAccrualIsUserDefined, TRUE)
	expect_equal(accrualTime8$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime8$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime8$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime8$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime8$accrualTime, c(0, 6, 30))
	expect_equal(accrualTime8$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime8$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime8$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime8$remainingTime, NA_real_)
	expect_equal(accrualTime8$piecewiseAccrualEnabled, TRUE)

	accrualTime9 <- getAccrualTime(accrualTime = c(0, 6), 
		accrualIntensity = c(22, 33), maxNumberOfSubjects = 1000) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime9' with expected results
	##
	expect_equal(accrualTime9$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime9$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime9$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime9$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime9$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime9$accrualTime, c(0, 6, 32.30303), tolerance = 1e-07)
	expect_equal(accrualTime9$accrualIntensity, c(22, 33))
	expect_equal(accrualTime9$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime9$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime9$remainingTime, 26.30303, tolerance = 1e-07)
	expect_equal(accrualTime9$piecewiseAccrualEnabled, TRUE)

	accrualTime10 <- getAccrualTime(list(
			"0 - <6" = 22,
			"6"      = 33), 
		maxNumberOfSubjects = 1000) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime10' with expected results
	##
	expect_equal(accrualTime10$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime10$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime10$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime10$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime10$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime10$accrualTime, c(0, 6, 32.30303), tolerance = 1e-07)
	expect_equal(accrualTime10$accrualIntensity, c(22, 33))
	expect_equal(accrualTime10$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime10$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime10$remainingTime, 26.30303, tolerance = 1e-07)
	expect_equal(accrualTime10$piecewiseAccrualEnabled, TRUE)

	accrualTime11 <- getAccrualTime(accrualTime = c(0, 6), 
		accrualIntensity = c(0.22, 0.33), maxNumberOfSubjects = 1000) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime11' with expected results
	##
	expect_equal(accrualTime11$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime11$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime11$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime11$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime11$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime11$accrualTime, c(0, 6))
	expect_equal(accrualTime11$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime11$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime11$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime11$remainingTime, NA_real_)
	expect_equal(accrualTime11$piecewiseAccrualEnabled, TRUE)

	accrualTime12 <- getAccrualTime(list(
			"0 - <6" = 0.22,
			"6"      = 0.33), 
		maxNumberOfSubjects = 1000) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime12' with expected results
	##
	expect_equal(accrualTime12$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime12$followUpTimeMustBeUserDefined, FALSE)
	expect_equal(accrualTime12$maxNumberOfSubjectsIsUserDefined, TRUE)
	expect_equal(accrualTime12$maxNumberOfSubjectsCanBeCalculatedDirectly, TRUE)
	expect_equal(accrualTime12$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime12$accrualTime, c(0, 6))
	expect_equal(accrualTime12$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime12$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime12$maxNumberOfSubjects, 1000)
	expect_equal(accrualTime12$remainingTime, NA_real_)
	expect_equal(accrualTime12$piecewiseAccrualEnabled, TRUE)

	accrualTime13 <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(22, 33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime13' with expected results
	##
	expect_equal(accrualTime13$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime13$followUpTimeMustBeUserDefined, TRUE)
	expect_equal(accrualTime13$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime13$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime13$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime13$accrualTime, c(0, 6))
	expect_equal(accrualTime13$accrualIntensity, c(22, 33))
	expect_equal(accrualTime13$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime13$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime13$remainingTime, NA_real_)
	expect_equal(accrualTime13$piecewiseAccrualEnabled, FALSE)

	accrualTime14 <- getAccrualTime(list(
			"0 - <6" = 22,
			"6"      = 33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime14' with expected results
	##
	expect_equal(accrualTime14$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime14$followUpTimeMustBeUserDefined, TRUE)
	expect_equal(accrualTime14$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime14$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime14$absoluteAccrualIntensityEnabled, TRUE)
	expect_equal(accrualTime14$accrualTime, c(0, 6))
	expect_equal(accrualTime14$accrualIntensity, c(22, 33))
	expect_equal(accrualTime14$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime14$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime14$remainingTime, NA_real_)
	expect_equal(accrualTime14$piecewiseAccrualEnabled, FALSE)

	accrualTime15 <- getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0.22, 0.33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime15' with expected results
	##
	expect_equal(accrualTime15$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime15$followUpTimeMustBeUserDefined, TRUE)
	expect_equal(accrualTime15$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime15$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime15$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime15$accrualTime, c(0, 6))
	expect_equal(accrualTime15$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime15$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime15$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime15$remainingTime, NA_real_)
	expect_equal(accrualTime15$piecewiseAccrualEnabled, FALSE)

	accrualTime16 <- getAccrualTime(list(
			"0 - <6" = 0.22,
			"6"      = 0.33)) 

	##
	## Comparison of the results of AccrualTime object 'accrualTime16' with expected results
	##
	expect_equal(accrualTime16$endOfAccrualIsUserDefined, FALSE)
	expect_equal(accrualTime16$followUpTimeMustBeUserDefined, TRUE)
	expect_equal(accrualTime16$maxNumberOfSubjectsIsUserDefined, FALSE)
	expect_equal(accrualTime16$maxNumberOfSubjectsCanBeCalculatedDirectly, FALSE)
	expect_equal(accrualTime16$absoluteAccrualIntensityEnabled, FALSE)
	expect_equal(accrualTime16$accrualTime, c(0, 6))
	expect_equal(accrualTime16$accrualIntensity, c(0.22, 0.33), tolerance = 1e-07)
	expect_equal(accrualTime16$accrualIntensityRelative, NA_real_)
	expect_equal(accrualTime16$maxNumberOfSubjects, NA_real_)
	expect_equal(accrualTime16$remainingTime, NA_real_)
	expect_equal(accrualTime16$piecewiseAccrualEnabled, FALSE)

})

test_that("Testing 'getAccrualTime': check expected warnings and errors", {

	expect_warning(getAccrualTime(accrualTime = c(0, 24), accrualIntensity = c(30, 45), maxNumberOfSubjects = 720), 
		"Last accrual intensity value (45) ignored", fixed = TRUE)

	expect_warning(getAccrualTime(accrualTime = c(0, 24, 30), accrualIntensity = c(30, 45, 55), maxNumberOfSubjects = 720),
		"Last 2 accrual intensity values (45, 55) ignored", fixed = TRUE)

	expect_warning(getAccrualTime(accrualTime = c(0, 24, 30, 40), accrualIntensity = c(30, 45, 55, 66), maxNumberOfSubjects = 720),
		"Last 2 accrual time values (30, 40) ignored", fixed = TRUE)

	expect_warning(getAccrualTime(accrualTime = c(0, 24, 30, 40), accrualIntensity = c(30, 45, 55, 66), maxNumberOfSubjects = 720),
		"Last 3 accrual intensity values (45, 55, 66) ignored", fixed = TRUE)

	expect_warning(getAccrualTime(accrualTime = c(0, 6, 15, 25), accrualIntensity = c(0, 22, 33)),
		"It makes no sense to start 'accrualIntensity' (0, 22, 33) with 0", fixed = TRUE)

	expect_error(getAccrualTime(accrualTime = c(0, 6), accrualIntensity = c(0)),
		"Illegal argument: at least one 'accrualIntensity' value must be > 0", fixed = TRUE)

	expect_error(getAccrualTime(accrualTime = c(0, 6, 30), accrualIntensity = c(22, 33), 
			maxNumberOfSubjects = 1000),
		paste0("Conflicting arguments: 'maxNumberOfSubjects' (1000) disagrees with the defined ", 
			"accrual time and intensity: 6 * 22 + 24 * 33 = 924"), fixed = TRUE)

})

test_that("Testing 'getAccrualTime': list-wise definition", {

	accrualTime <- list(
		"0  - <12"  = 15,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39,
		">=16"     = 45)
	accrualTime4 <- getAccrualTime(accrualTime = accrualTime, maxNumberOfSubjects = 1405)
	expect_equal(accrualTime4$accrualTime,      c( 0, 12, 13, 14, 15, 16, 40.55555556))
	expect_equal(accrualTime4$accrualIntensity, c(15, 21, 27, 33, 39, 45))
	expect_equal(accrualTime4$remainingTime, 24.55555556)

	accrualTime <- list(
		"0  - <12"  = 15,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39,
		"16 - ?"   = 45)
	accrualTime5 <- getAccrualTime(accrualTime = accrualTime, maxNumberOfSubjects = 1405)
	expect_equal(accrualTime5$accrualTime,      c( 0, 12, 13, 14, 15, 16, 40.55555556))
	expect_equal(accrualTime5$accrualIntensity, c(15, 21, 27, 33, 39, 45))
	expect_equal(accrualTime5$remainingTime, 24.55555556)	

	accrualTime <- list(
		"0 - <11"  = 20,
		"11 - <16" = 40,
		">=16"     = 60)
	accrualTime6 <- getAccrualTime(accrualTime = accrualTime, maxNumberOfSubjects = 800)
	expect_equal(accrualTime6$accrualTime,      c(0, 11, 16, 22.3333333))
	expect_equal(accrualTime6$accrualIntensity, c(20, 40, 60))
	expect_equal(accrualTime6$remainingTime, 6.33333333)	

	accrualTime <- list(
		"0 - <11"  = 20,
		"11 - <16" = 40,
		"16 - ?"   = 60)
	accrualTime7 <- getAccrualTime(accrualTime = accrualTime, maxNumberOfSubjects = 800)
	expect_equal(accrualTime7$accrualTime,      c(0, 11, 16, 22.3333333))
	expect_equal(accrualTime7$accrualIntensity, c(20, 40, 60))
	expect_equal(accrualTime7$remainingTime, 6.33333333)	

})

