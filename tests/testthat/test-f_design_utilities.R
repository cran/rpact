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
#:#  File name: test-f_design_utilities.R
#:#  Creation date: 05 September 2020, 14:48:35
#:#  File version: $Revision: 3596 $
#:#  Last changed: $Date: 2020-09-07 08:04:48 +0200 (Mo, 07 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:#  

context("Testing design utility functions")


test_that("'getPiByLambda' and 'getLambdaByPi' produce corresponding results", {
	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 1, kappa = 1), eventTime = 1, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 1, kappa = 2), eventTime = 1, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 1, kappa = 3), eventTime = 1, kappa = 3), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 1, kappa = 4), eventTime = 1, kappa = 4), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 1, kappa = 5), eventTime = 1, kappa = 5), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 2, kappa = 1), eventTime = 2, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 2, kappa = 2), eventTime = 2, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 2, kappa = 3), eventTime = 2, kappa = 3), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 2, kappa = 4), eventTime = 2, kappa = 4), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 2, kappa = 5), eventTime = 2, kappa = 5), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 3, kappa = 1), eventTime = 3, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 3, kappa = 2), eventTime = 3, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 3, kappa = 3), eventTime = 3, kappa = 3), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 3, kappa = 4), eventTime = 3, kappa = 4), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 3, kappa = 5), eventTime = 3, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 3, kappa = 5), eventTime = 3, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 3, kappa = 5), eventTime = 3, kappa = 5), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 3, kappa = 5), eventTime = 3, kappa = 5), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 3, kappa = 5), eventTime = 3, kappa = 5), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 3, kappa = 5), eventTime = 3, kappa = 5), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 4, kappa = 1), eventTime = 4, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 4, kappa = 2), eventTime = 4, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 4, kappa = 3), eventTime = 4, kappa = 3), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 4, kappa = 4), eventTime = 4, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 4, kappa = 4), eventTime = 4, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 4, kappa = 4), eventTime = 4, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 4, kappa = 4), eventTime = 4, kappa = 4), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 4, kappa = 4), eventTime = 4, kappa = 4), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 4, kappa = 4), eventTime = 4, kappa = 4), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 4, kappa = 5), eventTime = 4, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 4, kappa = 5), eventTime = 4, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 4, kappa = 5), eventTime = 4, kappa = 5), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 4, kappa = 5), eventTime = 4, kappa = 5), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 4, kappa = 5), eventTime = 4, kappa = 5), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 5, kappa = 1), eventTime = 5, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 5, kappa = 2), eventTime = 5, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 5, kappa = 3), eventTime = 5, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 5, kappa = 3), eventTime = 5, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 5, kappa = 3), eventTime = 5, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 5, kappa = 3), eventTime = 5, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 5, kappa = 3), eventTime = 5, kappa = 3), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 5, kappa = 3), eventTime = 5, kappa = 3), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 5, kappa = 4), eventTime = 5, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 5, kappa = 4), eventTime = 5, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 5, kappa = 4), eventTime = 5, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 5, kappa = 4), eventTime = 5, kappa = 4), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 5, kappa = 5), eventTime = 5, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 5, kappa = 5), eventTime = 5, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 5, kappa = 5), eventTime = 5, kappa = 5), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 5, kappa = 5), eventTime = 5, kappa = 5), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 6, kappa = 1), eventTime = 6, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 6, kappa = 2), eventTime = 6, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 6, kappa = 3), eventTime = 6, kappa = 3), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 6, kappa = 4), eventTime = 6, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 6, kappa = 4), eventTime = 6, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 6, kappa = 4), eventTime = 6, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 6, kappa = 4), eventTime = 6, kappa = 4), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 6, kappa = 5), eventTime = 6, kappa = 5), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 7, kappa = 1), eventTime = 7, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 7, kappa = 2), eventTime = 7, kappa = 2), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 7, kappa = 3), eventTime = 7, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 7, kappa = 3), eventTime = 7, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 7, kappa = 3), eventTime = 7, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 7, kappa = 3), eventTime = 7, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 7, kappa = 4), eventTime = 7, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 7, kappa = 4), eventTime = 7, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 7, kappa = 4), eventTime = 7, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 7, kappa = 5), eventTime = 7, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 7, kappa = 5), eventTime = 7, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 8, kappa = 1), eventTime = 8, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 8, kappa = 2), eventTime = 8, kappa = 2), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 8, kappa = 3), eventTime = 8, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 8, kappa = 3), eventTime = 8, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 8, kappa = 3), eventTime = 8, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 8, kappa = 3), eventTime = 8, kappa = 3), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 8, kappa = 4), eventTime = 8, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 8, kappa = 4), eventTime = 8, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 8, kappa = 4), eventTime = 8, kappa = 4), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 8, kappa = 5), eventTime = 8, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 8, kappa = 5), eventTime = 8, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 9, kappa = 1), eventTime = 9, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 9, kappa = 2), eventTime = 9, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 9, kappa = 2), eventTime = 9, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 9, kappa = 2), eventTime = 9, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 9, kappa = 2), eventTime = 9, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 9, kappa = 2), eventTime = 9, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 9, kappa = 2), eventTime = 9, kappa = 2), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 9, kappa = 3), eventTime = 9, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 9, kappa = 3), eventTime = 9, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 9, kappa = 3), eventTime = 9, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 9, kappa = 4), eventTime = 9, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 9, kappa = 4), eventTime = 9, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 9, kappa = 5), eventTime = 9, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 9, kappa = 5), eventTime = 9, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 10, kappa = 1), eventTime = 10, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 10, kappa = 2), eventTime = 10, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 10, kappa = 2), eventTime = 10, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 10, kappa = 2), eventTime = 10, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 10, kappa = 2), eventTime = 10, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 10, kappa = 2), eventTime = 10, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 10, kappa = 3), eventTime = 10, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 10, kappa = 3), eventTime = 10, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 10, kappa = 3), eventTime = 10, kappa = 3), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 10, kappa = 4), eventTime = 10, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 10, kappa = 4), eventTime = 10, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 10, kappa = 5), eventTime = 10, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 10, kappa = 5), eventTime = 10, kappa = 5), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 11, kappa = 1), eventTime = 11, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 11, kappa = 2), eventTime = 11, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 11, kappa = 2), eventTime = 11, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 11, kappa = 2), eventTime = 11, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 11, kappa = 2), eventTime = 11, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 11, kappa = 2), eventTime = 11, kappa = 2), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 11, kappa = 3), eventTime = 11, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 11, kappa = 4), eventTime = 11, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 11, kappa = 4), eventTime = 11, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 11, kappa = 5), eventTime = 11, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 12, kappa = 1), eventTime = 12, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 12, kappa = 2), eventTime = 12, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 12, kappa = 2), eventTime = 12, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 12, kappa = 2), eventTime = 12, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 12, kappa = 2), eventTime = 12, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 12, kappa = 3), eventTime = 12, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 12, kappa = 3), eventTime = 12, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 12, kappa = 4), eventTime = 12, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 12, kappa = 4), eventTime = 12, kappa = 4), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 12, kappa = 5), eventTime = 12, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 13, kappa = 1), eventTime = 13, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 13, kappa = 2), eventTime = 13, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 13, kappa = 2), eventTime = 13, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 13, kappa = 2), eventTime = 13, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 13, kappa = 2), eventTime = 13, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 13, kappa = 3), eventTime = 13, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 13, kappa = 3), eventTime = 13, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 13, kappa = 4), eventTime = 13, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 13, kappa = 5), eventTime = 13, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 14, kappa = 1), eventTime = 14, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 14, kappa = 2), eventTime = 14, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 14, kappa = 2), eventTime = 14, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 14, kappa = 2), eventTime = 14, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 14, kappa = 2), eventTime = 14, kappa = 2), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 14, kappa = 3), eventTime = 14, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 14, kappa = 3), eventTime = 14, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 14, kappa = 4), eventTime = 14, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 14, kappa = 5), eventTime = 14, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 15, kappa = 1), eventTime = 15, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 15, kappa = 2), eventTime = 15, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 15, kappa = 2), eventTime = 15, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 15, kappa = 2), eventTime = 15, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 15, kappa = 3), eventTime = 15, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 15, kappa = 3), eventTime = 15, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 15, kappa = 4), eventTime = 15, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 15, kappa = 5), eventTime = 15, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 16, kappa = 1), eventTime = 16, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 16, kappa = 2), eventTime = 16, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 16, kappa = 2), eventTime = 16, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 16, kappa = 2), eventTime = 16, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 16, kappa = 3), eventTime = 16, kappa = 3), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 16, kappa = 4), eventTime = 16, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 16, kappa = 5), eventTime = 16, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 17, kappa = 1), eventTime = 17, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 17, kappa = 2), eventTime = 17, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 17, kappa = 2), eventTime = 17, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 17, kappa = 2), eventTime = 17, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 17, kappa = 3), eventTime = 17, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 17, kappa = 4), eventTime = 17, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 17, kappa = 5), eventTime = 17, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 18, kappa = 1), eventTime = 18, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 18, kappa = 2), eventTime = 18, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 18, kappa = 2), eventTime = 18, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 18, kappa = 2), eventTime = 18, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 18, kappa = 3), eventTime = 18, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 18, kappa = 4), eventTime = 18, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 18, kappa = 5), eventTime = 18, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 19, kappa = 1), eventTime = 19, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 19, kappa = 2), eventTime = 19, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 19, kappa = 2), eventTime = 19, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 19, kappa = 2), eventTime = 19, kappa = 2), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 19, kappa = 3), eventTime = 19, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 19, kappa = 4), eventTime = 19, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 19, kappa = 5), eventTime = 19, kappa = 5), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.3, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.3, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.4, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.4, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.5, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.5, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.6, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.6, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.7, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.7, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.8, eventTime = 20, kappa = 1), eventTime = 20, kappa = 1), 0.8, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 20, kappa = 2), eventTime = 20, kappa = 2), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.2, eventTime = 20, kappa = 2), eventTime = 20, kappa = 2), 0.2, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 20, kappa = 3), eventTime = 20, kappa = 3), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 20, kappa = 4), eventTime = 20, kappa = 4), 0.1, tolerance = 1e-04)

	expect_equal(getLambdaByPi(getPiByLambda(0.1, eventTime = 20, kappa = 5), eventTime = 20, kappa = 5), 0.1, tolerance = 1e-04)




})

test_that("'getPiecewiseExponentialDistribution' and 'getPiecewiseExponentialQuantile' produce corresponding results", {

	piecewiseLambda <- c(0.03, 0.05, 0.08)
	piecewiseSurvivalTime <- c(0, 16, 22)
	time <- seq(2, 50, 4)
	quantile <- getPiecewiseExponentialDistribution(time, 
		piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda) 
	y <- getPiecewiseExponentialQuantile(quantile, 
		piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'ppwexp' and 'qpwexp' produce corresponding results", {

	piecewiseLambda <- c(0.03, 0.05, 0.08)
	piecewiseSurvivalTime <- c(0, 16, 22)
	time <- seq(2, 50, 4)
	quantile <- ppwexp(time, 
		s = piecewiseSurvivalTime, lambda = piecewiseLambda) 
	y <- qpwexp(quantile, 
		s = piecewiseSurvivalTime, lambda = piecewiseLambda)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'getPiecewiseExponentialDistribution' and 'getPiecewiseExponentialQuantile' produce corresponding results ('piecewiseSurvivalTime' defined as list)", {

	piecewiseSurvivalTime <- list(
		"<16"      = 0.03, 
		"16 - <22" = 0.05, 
		">=22"      = 0.08)
	time <- seq(2, 50, 4)
	quantile <- getPiecewiseExponentialDistribution(time, 
		piecewiseSurvivalTime = piecewiseSurvivalTime) 
	y <- getPiecewiseExponentialQuantile(quantile, 
		piecewiseSurvivalTime = piecewiseSurvivalTime)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'ppwexp' and 'qpwexp' produce corresponding results ('piecewiseSurvivalTime' defined as list)", {

	piecewiseSurvivalTime <- list(
		"<16"      = 0.03, 
		"16 - <22" = 0.05, 
		">=22"      = 0.08)
	time <- seq(2, 50, 4)
	quantile <- ppwexp(time, s = piecewiseSurvivalTime) 
	y <- qpwexp(quantile, s = piecewiseSurvivalTime)

	expect_equal(y, time, tolerance = 1e-06)

})

test_that("'getPiecewiseExponentialRandomNumbers': test that mean random numbers are as expected", {

	piecewiseSurvivalTime <- c(0, 16, 22)
	piecewiseLambda <- c(0.003, 0.003, 0.003)
	y <- 1 / mean(getPiecewiseExponentialRandomNumbers(5000, 
			piecewiseSurvivalTime = piecewiseSurvivalTime, piecewiseLambda = piecewiseLambda, kappa = 1))

	expect_equal(y, piecewiseLambda[1], tolerance = 5e-04)

})

test_that("'rpwexp': test that mean random numbers are as expected", {

	piecewiseSurvivalTime <- c(0, 16, 22)
	piecewiseLambda <- c(0.003, 0.003, 0.003)
	y <- 1 / mean(rpwexp(5000, s = piecewiseSurvivalTime, lambda = piecewiseLambda, kappa = 1))

	expect_equal(y, piecewiseLambda[1], tolerance = 5e-04)

})

test_that("'getPiecewiseExponentialRandomNumbers': test that mean random numbers are as expected ('piecewiseSurvivalTime' defined as list)", {

	piecewiseSurvivalTime <- list(
		"<16"      = 0.003, 
		"16 - <22" = 0.003, 
		">=22"      = 0.003)
	y <- 1 / mean(getPiecewiseExponentialRandomNumbers(5000, 
			piecewiseSurvivalTime = piecewiseSurvivalTime, kappa = 1))

	expect_equal(y, 0.003, tolerance = 5e-04)

})

test_that("'rpwexp': test that mean random numbers are as expected ('piecewiseSurvivalTime' defined as list)", {

	piecewiseSurvivalTime <- list(
		"<16"      = 0.003, 
		"16 - <22" = 0.003, 
		">=22"      = 0.003)
	y <- 1 / mean(rpwexp(5000, s = piecewiseSurvivalTime, kappa = 1))

	expect_equal(y, 0.003, tolerance = 5e-04)

})

test_that("'getPiecewiseExponentialDistribution': test that function call with singel lambda is working", {

	expect_equal(getPiecewiseExponentialDistribution(4, piecewiseLambda = 0.003), 0.01192829, tolerance = 5e-05)

})

test_that("'.convertStageWiseToOverallValues': test that function is working as expected", {

	x1 <- .convertStageWiseToOverallValues(c(1:5))

	## Comparison of the results of matrixarray object 'x1' with expected results
	expect_equal(x1[1, ], 1)
	expect_equal(x1[2, ], 3)
	expect_equal(x1[3, ], 6)
	expect_equal(x1[4, ], 10)
	expect_equal(x1[5, ], 15)

	x2 <- .convertStageWiseToOverallValues(matrix(c(1:5), ncol = 1))

	## Comparison of the results of matrixarray object 'x2' with expected results
	expect_equal(x2[1, ], 1)
	expect_equal(x2[2, ], 3)
	expect_equal(x2[3, ], 6)
	expect_equal(x2[4, ], 10)
	expect_equal(x2[5, ], 15)

	x3 <- .convertStageWiseToOverallValues(matrix(c(1:5), nrow = 1))

	## Comparison of the results of matrixarray object 'x3' with expected results
	expect_equal(x3[1, ], c(1, 2, 3, 4, 5))

	x4 <- .convertStageWiseToOverallValues(matrix(c(1:5, 1:5), ncol = 2))

	## Comparison of the results of matrixarray object 'x4' with expected results
	expect_equal(x4[1, ], c(1, 1))
	expect_equal(x4[2, ], c(3, 3))
	expect_equal(x4[3, ], c(6, 6))
	expect_equal(x4[4, ], c(10, 10))
	expect_equal(x4[5, ], c(15, 15))

	x5 <- .convertStageWiseToOverallValues(matrix(sort(rep(1:5, 2)), nrow = 2))

	## Comparison of the results of matrixarray object 'x5' with expected results
	expect_equal(x5[1, ], c(1, 2, 3, 4, 5))
	expect_equal(x5[2, ], c(2, 4, 6, 8, 10))

})

