######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 26-02-2019                                                                   #
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


context("Testing design utility functions")

test_that("'getPiecewiseExponentialDistribution' and 'getPiecewiseExponentialQuantile' produce corresponding results", {
	for (eventTime in 1:20) {
		for (kappa in 1:5) {
			for (lambda in seq(0.1, 0.8, 0.1)) {
				pi <- getPiByLambda(lambda, eventTime = eventTime, kappa = kappa)
				if (pi < 1 - 1e-15 || pi > 1 + 1e-15) {
					result <- getLambdaByPi(pi, eventTime = eventTime, kappa = kappa)
					expect_equal(result, lambda, tolerance = 1e-04)
				}

			}
		}
	}
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

