######################################################################################
#                                                                                    #
# -- Unit test helper functions --                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 25-09-2018                                                                   #
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

getTestInformationRatesDefault <- function(kMax) {
	return((1:kMax) / kMax)
}

getTestFutilityBoundsDefault <- function(kMax) {
	return(rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - 1))
}

getTestAlpha0VecDefault <- function(kMax) {
	return(rep(C_ALPHA_0_VEC_DEFAULT, kMax - 1))
}

getTestInformationRates <- function(kMax) {
	if (kMax == 1L) {
		return(1)
	}
	
	a <- 0.8 / kMax

	b <- c()
	for (i in 1:(kMax - 1)) {
		b <- c(b, a * i)
	}
	
	return(c(b, 1))
}

getTestFutilityBounds <- function(kMax) {
	if (kMax < 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'kMax' must be >= 2")
	}
	
	k <- kMax - 1
	futilityBounds <- c(2)
	k <- k - 1
	if (k > 0) {
		futilityBounds <- c(1, futilityBounds)
		k <- k - 1
	}
	if (k > 0) {
		futilityBounds <- c(rep(0, k), futilityBounds)
	}
	
	return(futilityBounds)
}

getTestDesign <- function(kMax = NA_real_, informationRates = NA_real_, futilityBounds = NA_real_,
		designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) {

	if (designClass == C_CLASS_NAME_TRIAL_DESIGN_FISHER) {
		return(TrialDesignFisher(
			kMax = as.integer(kMax), 
			alpha = C_ALPHA_DEFAULT, 
			method = C_FISHER_METHOD_DEFAULT, 
			alpha0Vec = futilityBounds,  
			informationRates = informationRates, 
			tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT
		))
	}	
		
	return(.createDesign(
		designClass             = designClass,
		kMax                    = as.integer(kMax), 
		alpha                   = C_ALPHA_DEFAULT, 
		beta                    = C_BETA_DEFAULT, 
		sided                   = 1, 
		informationRates        = informationRates, 
		futilityBounds          = futilityBounds, 		
		typeOfDesign            = C_DEFAULT_TYPE_OF_DESIGN, 
		deltaWT                 = 0, 
		optimizationCriterion   = C_OPTIMIZATION_CRITERION_DEFAULT, 
		gammaA                  = 1, 
		typeBetaSpending        = C_TYPE_OF_DESIGN_BS_NONE, 
		userAlphaSpending       = NA_real_, 
		userBetaSpending        = NA_real_, 
		gammaB                  = 1, 
		tolerance               = 1e-06))
}
