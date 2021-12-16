## |
## |  *Unit tests helper functions*
## | 
## |  This file is part of the R package rpact: 
## |  Confirmatory Adaptive Clinical Trial Design and Analysis
## | 
## |  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
## |  Licensed under "GNU Lesser General Public License" version 3
## |  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
## | 
## |  RPACT company website: https://www.rpact.com
## |  rpact package website: https://www.rpact.org
## | 
## |  Contact us for information about our services: info@rpact.com
## | 
## |  File version: $Revision: 5577 $
## |  Last changed: $Date: 2021-11-19 09:14:42 +0100 (Fr, 19 Nov 2021) $
## |

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

getTestFutilityBounds <- function(kMax, fisherDesignEnabled = FALSE) {
	if (kMax < 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'kMax' must be >= 2")
	}
	
	if (kMax == 2 && fisherDesignEnabled) {
		return(0.5)
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
	
	if (fisherDesignEnabled) {
		futilityBounds[futilityBounds > 0] <- futilityBounds[futilityBounds > 0] / max(futilityBounds)
		futilityBounds[futilityBounds == 0] <- 0.01
	}
	
	return(futilityBounds)
}

getTestDesign <- function(kMax = NA_real_, informationRates = NA_real_, futilityBounds = NA_real_,
		designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) {
	
	design <- NULL 
	
	currentWarningOption <- getOption("warn")
	options(warn = -1)
	if (designClass == C_CLASS_NAME_TRIAL_DESIGN_FISHER) {
		design <- getDesignFisher(
			kMax = as.integer(kMax), 
			alpha0Vec = futilityBounds,  
			informationRates = informationRates
		)
	}
	else if (designClass == C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) {
		design <- getDesignGroupSequential(
			kMax                = as.integer(kMax), 
			informationRates    = informationRates, 
			futilityBounds      = futilityBounds, 		
			tolerance           = 1e-06
		)
	}
	else {
		design <- getDesignInverseNormal(
			kMax                    = as.integer(kMax), 
			informationRates        = informationRates, 
			futilityBounds          = futilityBounds, 		
			tolerance               = 1e-06
		)
	}
	options(warn = currentWarningOption)

	return(design)
}
