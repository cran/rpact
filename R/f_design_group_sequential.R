#:#
#:#  *Group sequential design*
#:# 
#:#  This file is part of the R package rpact: 
#:#  Confirmatory Adaptive Clinical Trial Design and Analysis
#:# 
#:#  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
#:#  Licensed under "GNU Lesser General Public License" version 3
#:#  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
#:# 
#:#  RPACT company website: https://www.rpact.com
#:#  rpact package website: https://www.rpact.org
#:# 
#:#  Contact us for information about our services: info@rpact.com
#:# 
#:#  File version: $Revision: 3688 $
#:#  Last changed: $Date: 2020-09-24 14:37:04 +0200 (Thu, 24 Sep 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

#' @include f_core_constants.R
NULL

.getGroupSequentialProbabilities <- function(decisionMatrix, informationRates) {

	.assertAreValidInformationRates(informationRates)
	
	if (length(decisionMatrix) != 2 * length(informationRates)) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
			"length of 'decisionMatrix' (%s) must be equal to  2 x length of 'informationRates' (%s)"), 
			length(decisionMatrix), length(informationRates)))
	}
	
	decisionMatrix[decisionMatrix <= C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT
	decisionMatrix[decisionMatrix >= C_UPPER_BOUNDS_DEFAULT] <- C_UPPER_BOUNDS_DEFAULT
	M <- C_CONST_NEWTON_COTES * 6 + 1  # number of grid points with constant of Newton Cotes algorithm (n*6 + 1)
	dn <- rep(NA_real_, M)             # density values	
	w <- rep(NA_real_, M)              # weights		
	x <- rep(NA_real_, M)              # grid points
	dn2 <- rep(NA_real_, M)            # density values in recursion
	x2 <- rep(NA_real_, M)	           # grid points in recursion
	kMax <- length(informationRates)   # maximum number of stages
	probs <- matrix(NA_real_, 3, kMax) # probability matrix output
	
	probs[, 1] <- c(stats::pnorm(decisionMatrix[1, 1]), stats::pnorm(decisionMatrix[2, 1]), 1)
	if (kMax <= 1) {
		return(probs)
	}
	
	epsilonVec <- informationRates
	epsilonVec[2:kMax] <- informationRates[2:kMax] - informationRates[1:(kMax - 1)]
	
	informationRatesSqrt <- sqrt(informationRates)
	epsilonVecSqrt <- sqrt(epsilonVec)
	
	for (k in 2:kMax) {
		dx <- (decisionMatrix[2, k - 1] - decisionMatrix[1, k - 1]) / (M - 1)
		w <- c(rep(c(492, 1296, 162, 1632, 162, 1296) * dx / 840, M %/% 6), 246 * dx / 840)
		w[1] <- 246 * dx / 840
		x <- rep(decisionMatrix[1, k - 1], M) + (0:(M - 1)) * dx
		
		dn <- w * .getDnormValues(x, k, informationRates, epsilonVec, x2, dn2)
		
		# as alternative, use crossprod (x, y)
		seq1 <- stats::pnorm((decisionMatrix[1, k] * informationRatesSqrt[k] - x * 
			informationRatesSqrt[k - 1]) / epsilonVecSqrt[k]) %*% dn  
		seq2 <- stats::pnorm((decisionMatrix[2, k] * informationRatesSqrt[k] - x * 
			informationRatesSqrt[k - 1]) / epsilonVecSqrt[k]) %*% dn
		x2 <- x
		dn2 <- dn
		probs[, k] <- c(seq1, seq2, probs[2, k - 1] - probs[1, k - 1])
	}
	
	.validateGroupSequentialProbabilityResultsMulti(dn = dn, dn2 = dn2, x = x, x2 = x2, w = w)
	
	return(probs)
}

.getDnormValues <- function(x, k, informationRates, epsilonVec, x2, dn2) {
	if (is.loaded("R_getDensityValues", PACKAGE = "rpact", type = "Call")) {
		return(.Call("R_getDensityValues", x, as.integer(k), informationRates, epsilonVec, x2, dn2))
	}

	# slow R-based call
	return(sapply(x, .getDnormValuesSlow, k = k, informationRates = informationRates, epsilonVec = epsilonVec, x2 = x2, dn2 = dn2))
}

.getDnormValuesSlow <- function(x, k, informationRates, epsilonVec, x2, dn2) {
	if (k == 2) {
		return(stats::dnorm(x))
	} 
	
	sum(sqrt(informationRates[k - 1] / epsilonVec[k - 1]) * 
		stats::dnorm((x * sqrt(informationRates[k - 1]) - x2 * sqrt(informationRates[k - 2])) / 
		sqrt(epsilonVec[k - 1])) * dn2)
}

.validateGroupSequentialProbabilityResultsMulti <- function(...) {
	args <- list(...)
	for (variableName in names(args)) {
		if (!.validateGroupSequentialProbabilityResults(results = args[[variableName]], variableName)) {
			return(invisible())
		}
	}
}

.validateGroupSequentialProbabilityResults <- function(results, variableName) {
	numberOfNAs <- sum(is.na(results))
	if (numberOfNAs == 0) {
		return(TRUE)
	}
	
	warning(sprintf(paste0(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
		"in .getGroupSequentialProbabilities(): ", 
		"variable '%s' contains %s NA's (%.1f%%)"), 
	variableName, numberOfNAs, 100 * numberOfNAs / length(results)), call. = FALSE)
	return(FALSE)
}

.getSpendingValue <- function(alpha, x, sided, typeOfDesign, gamma = 1) {
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_P || typeOfDesign == C_TYPE_OF_DESIGN_BS_P) {
		return(alpha * log(1 + (exp(1) - 1) * x))
	}
	
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_OF || typeOfDesign == C_TYPE_OF_DESIGN_BS_OF) {
		return(2 * sided * (1 - stats::pnorm(stats::qnorm(1 - alpha / (2 * sided)) / sqrt(x))))
	}
	
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_KD || typeOfDesign == C_TYPE_OF_DESIGN_BS_KD) {
		return(alpha * x^gamma)
	}
	
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_HSD || typeOfDesign == C_TYPE_OF_DESIGN_BS_HSD) {
		if (gamma == 0)	{
			return(alpha * x)
		} 
			
		return(alpha * (1 - exp(-gamma * x)) / (1 - exp(-gamma)))
	}
	
	return(NA)
}

.getOptimumDesign <- function(deltaWT, design) {
	
	scale <- .getOneDimensionalRoot(function(scale) {
		criticalValues <- scale * design$informationRates^(deltaWT - 0.5)
		if (design$sided == 2) {
			decisionMatrix <- (matrix(c(-criticalValues, criticalValues), nrow = 2, byrow = TRUE))
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$alpha)
		} else {
			if (design$bindingFutility) { 
				decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
					criticalValues), nrow = 2, byrow = TRUE)
			} else {
				decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
					criticalValues), nrow = 2, byrow = TRUE)
			}
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ]) - design$alpha)
		}									 
	}, lower = 0, upper = 5, tolerance = design$tolerance, callingFunctionInformation = ".getOptimumDesign")

	design$criticalValues <- scale * design$informationRates^(deltaWT - 0.5)
	designCharacteristics <- .getDesignCharacteristics(design = design)
	
	y <- NA_integer_
	if (design$optimizationCriterion == C_OPTIMIZATION_CRITERION_ASNH1) {
		y <- designCharacteristics$averageSampleNumber1
	}
	if (design$optimizationCriterion == C_OPTIMIZATION_CRITERION_ASNIFH1) {
		y <- designCharacteristics$inflationFactor + designCharacteristics$averageSampleNumber1
	}
	if (design$optimizationCriterion == C_OPTIMIZATION_CRITERION_ASN_SUM) {
		y <- designCharacteristics$averageSampleNumber0 + 
			designCharacteristics$averageSampleNumber01 + designCharacteristics$averageSampleNumber1
	}
	return(y)
}

.validateTypeOfDesign <- function(design) {
	
	.assertDesignParameterExists(design, "typeOfDesign", C_DEFAULT_TYPE_OF_DESIGN)
	
	design$.setParameterType("userAlphaSpending", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("userBetaSpending", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("deltaWT", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("optimizationCriterion", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("gammaA", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("gammaB", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("typeBetaSpending", C_PARAM_NOT_APPLICABLE)
	design$.setParameterType("constantBoundsHP", C_PARAM_NOT_APPLICABLE) 
	
	if (!(design$typeOfDesign %in% .getDesignTypes())) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"type of design (", design$typeOfDesign, ") must be one of the following: ", .printDesignTypes())
	}
	
	if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT) {
		.assertDesignParameterExists(design, "deltaWT", NA_real_) 
		
		if (design$deltaWT < -0.5 || design$deltaWT > 1) {
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, "'deltaWT' (", design$deltaWT, ") out of bounds [-0.5; 1]")
		}
	}
	else if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) {
		.assertDesignParameterExists(design, "optimizationCriterion", C_OPTIMIZATION_CRITERION_DEFAULT)
		
		if (!.isOptimizationCriterion(design$optimizationCriterion)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"optimization criterion must be one of the following: ", .printOptimizationCriterion())
		}		
	}
	else if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
		.assertDesignParameterExists(design, "constantBoundsHP", C_CONST_BOUND_HP_DEFAULT)
		.assertIsSingleNumber(design$constantBoundsHP, "constantBoundsHP")
		.assertIsInClosedInterval(design$constantBoundsHP, "constantBoundsHP", lower = 2, upper = NULL)
	}
	else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_KD) {
		.assertDesignParameterExists(design, "gammaA", NA_real_)
		
		if (design$gammaA < 0.4 || design$gammaA > 8) {
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
				"parameter 'gammaA' (", design$gammaA, ") for Kim & DeMets alpha ", 
				"spending function out of bounds [0.4; 8]")
		}	
	}
	else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_HSD) {
		.assertDesignParameterExists(design, "gammaA", NA_real_)
		
		if (design$gammaA < -10 || design$gammaA > 5) {
			stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, 
				"Parameter 'gammaA' (", design$gammaA, ") for Hwang, Shih & DeCani ", 
				"alpha spending function out of bounds [-10; 5]")
		}	
	}
	else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
		.validateUserAlphaSpending(design)		
	}
	
	if (.isUndefinedArgument(design$alpha)) {
		design$alpha <- C_ALPHA_DEFAULT
		design$.setParameterType("alpha", C_PARAM_DEFAULT_VALUE)
	}
	
	if (.isAlphaSpendingDesignType(design$typeOfDesign)) {
		.assertDesignParameterExists(design, "typeBetaSpending", C_TYPE_OF_DESIGN_BS_NONE)
		
		if (!.isBetaSpendingDesignType(design$typeBetaSpending, noneIncluded = TRUE)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
				"type of beta spending must be one of the following: ", .printBetaSpendingDesignTypes())
		}
		
		if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_KD) {
			.assertDesignParameterExists(design, "gammaB", NA_real_)
			
			if (design$gammaB < 0.4 || design$gammaB > 8) {
				stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
					"Parameter 'gammaB' (", design$gammaB, ") for Kim & DeMets beta ", 
					"spending function out of bounds [0.4; 8]")
			}
		}
		
		if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_HSD) {
			.assertDesignParameterExists(design, "gammaB", NA_real_)
			
			if (design$gammaB < -10 || design$gammaB > 5) {
				stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
					"Parameter 'gammaB' (", design$gammaB, ") for Hwang, Shih & DeCani ", 
					"beta spending out of bounds [-10; 5]")
			}	
		}
		
		if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
			.validateUserBetaSpending(design)
		}
		
	} else {
		
		if (.designParameterExists(design, "typeBetaSpending") && design$typeBetaSpending != C_TYPE_OF_DESIGN_BS_NONE) {
			warning("'typeBetaSpending' (", design$typeBetaSpending, ") will be ignored ", 
				"because 'typeOfDesign' (", design$typeOfDesign, ") is not an alpha spending design", 
				call. = FALSE)
			design$typeBetaSpending <- C_TYPE_OF_DESIGN_BS_NONE
			design$.setParameterType("typeBetaSpending", C_PARAM_DEFAULT_VALUE)
		}
		
		if (.designParameterExists(design, "userBetaSpending")) {
			userBetaSpending <- NA_real_
			warning("'userBetaSpending' (", .arrayToString(design$userBetaSpending), ") will be ignored ", 
				"because 'typeOfDesign' (", design$typeOfDesign, ") is not an alpha spending design", 
				call. = FALSE)
		}
	}
	
	if (.isUndefinedArgument(design$beta)) {
		design$beta <- C_BETA_DEFAULT
		design$.setParameterType("beta", C_PARAM_DEFAULT_VALUE)
	}
	
	invisible(design)
}

.validateBaseParameters <- function(design) {
	
	if (.isDefinedArgument(design$kMax)) {	
		
		.assertDesignParameterExists(design, "kMax", C_KMAX_DEFAULT)
		
		.assertIsValidKMax(design$kMax)
		
		if (.isDefinedArgument(design$informationRates)) {
			.assertAreValidInformationRates(design$informationRates, design$kMax)
		}
		
		if (.isDefinedArgument(design$futilityBounds)) {
			.assertAreValidFutilityBounds(design$futilityBounds, design$kMax)
		}
	}
	
	.assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
	.assertIsValidSidedParameter(design$sided)
	
	.setKmaxBasedOnAlphaSpendingDefintion(design)
	
	design$informationRates <- .getValidatedInformationRates(design)	
	design$futilityBounds <- .getValidatedFutilityBounds(design)

	.assertDesignParameterExists(design, "tolerance", C_DESIGN_TOLERANCE_DEFAULT)
	if (design$tolerance < 1e-10 || design$tolerance > 1e-03) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS,
			"'tolerance' (", tolerance, ") out of bounds [1e-10; 1e-03]")
	}
	
	invisible(design)
}

.createDesign <- function(
		designClass,
		kMax = NA_integer_, 
		alpha = NA_real_, 
		beta = NA_real_, 
		sided = C_SIDED_DEFAULT, 
		informationRates = NA_real_, 
		futilityBounds = NA_real_, 		
		typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN, 
		deltaWT = NA_real_, 
		optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT, 
		gammaA = NA_real_, 
		typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE, 
		userAlphaSpending = NA_real_, 
		userBetaSpending = NA_real_, 
		gammaB = NA_real_, 
		bindingFutility = C_BINDING_FUTILITY_DEFAULT,
		constantBoundsHP = C_CONST_BOUND_HP_DEFAULT,
		twoSidedPower = NA,
		tolerance = C_DESIGN_TOLERANCE_DEFAULT) {
	
	if (designClass == C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) {
		design <- TrialDesignInverseNormal(kMax = kMax)
	}
	else if (designClass == C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL) {
		design <- TrialDesignGroupSequential(kMax = kMax)
	}
	else {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'designClass' ('", designClass, "') must be '", C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL, "' or ", 
				"'", C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL, "'")
	}
		
	if (!is.integer(sided) && sided %in% c(1, 2)) {
		sided <- as.integer(sided)
	}
	
	design$alpha <- alpha 
	design$beta <- beta 
	design$sided <- sided 
	design$typeOfDesign <- typeOfDesign 
	design$deltaWT <- deltaWT 
	design$gammaA <- gammaA 
	design$gammaB <- gammaB 
	design$optimizationCriterion <- optimizationCriterion 
	design$typeBetaSpending <- typeBetaSpending 
	design$futilityBounds <- futilityBounds
	design$informationRates <- informationRates 
	design$userAlphaSpending <- userAlphaSpending 
	design$userBetaSpending <- userBetaSpending 
	design$bindingFutility <- bindingFutility
	if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {
		.assertIsSingleNumber(constantBoundsHP, "constantBoundsHP")
		.assertIsInClosedInterval(constantBoundsHP, "constantBoundsHP", lower = 2, upper = NULL)
		design$constantBoundsHP <- constantBoundsHP
	} else if (constantBoundsHP != C_CONST_BOUND_HP_DEFAULT) {
		warning("'constantBoundsHP' (", constantBoundsHP, 
			") will be ignored because it is only applicable for 'typeOfDesign' = \"", C_TYPE_OF_DESIGN_HP, "\"")
	}
	if (is.na(twoSidedPower)) {
		design$twoSidedPower <- C_TWO_SIDED_POWER_DEFAULT
		design$.setParameterType("twoSidedPower", C_PARAM_DEFAULT_VALUE)
	} else {
		design$twoSidedPower <- twoSidedPower
		design$.setParameterType("twoSidedPower", C_PARAM_USER_DEFINED)
	}
	
	design$tolerance <- tolerance
	
	return(design)
}

.getDesignGroupSequentialKMax1 <- function(design) {
	design$criticalValues <- stats::qnorm(1 - design$alpha / design$sided)
	design$alphaSpent[1] <- design$alpha
	invisible(design)
}

# 
# Wang and Tsiatis design
# 
.getDesignGroupSequentialWangAndTsiatis <- function(design) {

	if (design$typeOfDesign == C_TYPE_OF_DESIGN_P) {
		design$deltaWT <- 0.5
	}
	else if (design$typeOfDesign == C_TYPE_OF_DESIGN_OF) {
		design$deltaWT <- 0
	}
	
	scale <- .getOneDimensionalRoot(function(scale) {
		design$criticalValues <- scale * design$informationRates^(design$deltaWT - 0.5)
		if (design$sided == 2) {
			decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$alpha)
		} else {
			if (design$bindingFutility) {
				decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
					design$criticalValues), nrow = 2, byrow = TRUE)
			} else {
				decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
					design$criticalValues), nrow = 2, byrow = TRUE)
			}
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ]) - design$alpha)
		}									 
	}, lower = 0, upper = 8, tolerance = design$tolerance, 
		callingFunctionInformation = ".getDesignGroupSequentialWangAndTsiatis")
	design$criticalValues <- scale * design$informationRates^(design$deltaWT - 0.5)
	
	.calculateAlphaSpent(design)

	invisible(design)
}

.calculateAlphaSpent <- function(design) {
	if (design$sided == 2) {
		decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
	} else {
		if (design$bindingFutility) {
			decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
				design$criticalValues), nrow = 2, byrow = TRUE)
		} else {
			decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
				design$criticalValues), nrow = 2, byrow = TRUE)
		}
	}
	
	tryCatch({	
		probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
		if (design$sided == 1) {
			design$alphaSpent <- cumsum(probs[3, ] - probs[2, ])
		} else {
			design$alphaSpent <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
		} 
		if (!is.na(design$alphaSpent[design$kMax])) {
			design$alphaSpent[design$kMax] <- floor(design$alphaSpent[design$kMax] * 1e8) / 1e8
		}
		design$.setParameterType("alphaSpent", C_PARAM_GENERATED)
	}, error = function(e) {
		warning("Failed to calculate 'alphaSpent': ", e, call. = FALSE)
	})	
}

# 
# Haybittle & Peto design
# 
.getDesignGroupSequentialHaybittleAndPeto <- function(design) {
	
	scale <- .getOneDimensionalRoot(function(scale) {
		design$criticalValues <- c(rep(design$constantBoundsHP, design$kMax - 1), scale)
		if (design$sided == 2) {
			decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$alpha)
		} else {
			if (design$bindingFutility) {
				decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
					design$criticalValues), nrow = 2, byrow = TRUE)
			} else {
				decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
					design$criticalValues), nrow = 2, byrow = TRUE)
			}
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ]) - design$alpha)
		}									 
	}, lower = 0, upper = 8, tolerance = design$tolerance, 
		callingFunctionInformation = ".getDesignGroupSequentialHaybittleAndPeto")
	
	design$criticalValues <- c(rep(design$constantBoundsHP, design$kMax - 1), scale)
	
	.calculateAlphaSpent(design)
	
	if (!is.na(design$criticalValues[design$kMax]) &&
			!is.na(design$alphaSpent[design$kMax]) &&
			(design$criticalValues[design$kMax] > 6 || abs(design$alphaSpent[design$kMax] - design$alpha) > 0.001)) {
		stop(sprintf(paste0(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
			"critical values according to the Haybittle & Peto design cannot be calculated ", 
			"(criticalValues[%s] = %s, alpha = %s)"),
			design$kMax, design$criticalValues[design$kMax], design$alpha))
	}
	
	invisible(design)
}

# 
# Optimum design within Wang and Tsiatis class
# 
.getDesignGroupSequentialWangAndTsiatisOptimum <- function(design) {
	
	.assertDesignParameterExists(design, "optimizationCriterion", C_OPTIMIZATION_CRITERION_DEFAULT)
	.assertIsOptimizationCriterion(design$optimizationCriterion)
	
	optimumDesign <- stats::optimize(.getOptimumDesign, design = design,
			interval = c(0, 1), tol = 0.001)
	
	design$deltaWT <- round(optimumDesign$minimum, 2)
	design$.setParameterType("deltaWT", C_PARAM_GENERATED)
	
	# Recalculation of design characteristics with rounded design$deltaWT						
	scale <- .getOneDimensionalRoot(function(scale) {
		design$criticalValues <- scale * design$informationRates^(design$deltaWT - 0.5)
		if (design$sided == 2) {
			decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$alpha)
		} else{
			if (design$bindingFutility) {
				decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
					design$criticalValues), nrow = 2, byrow = TRUE)
			} else {
				decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
					design$criticalValues), nrow = 2, byrow = TRUE)
			}
			probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
			return(sum(probs[3, ] - probs[2, ]) - design$alpha)
		}									 
	}, lower = 0, upper = 5, tolerance = design$tolerance,
		callingFunctionInformation = ".getDesignGroupSequentialWangAndTsiatisOptimum")
	
	design$criticalValues <- scale * design$informationRates^(design$deltaWT - 0.5)
	designCharacteristics <- .getDesignCharacteristics(design = design)	
	design$power <- designCharacteristics$power
	design$.setParameterType("power", C_PARAM_GENERATED)

	.calculateAlphaSpent(design)
	
	invisible(design)
}

# 
# alpha spending approaches
# 
.getDesignGroupSequentialAlphaSpending <- function(design) {
	
	design$criticalValues <- rep(NA_real_, design$kMax)
	spendingValue <- .getSpendingValue(design$alpha, design$informationRates[1], design$sided, 
		design$typeOfDesign, design$gammaA)
	if (spendingValue < 0) {
		.logWarn("Cannot calculate alpha spent: 'spendingValue' (%s) is < 0", spendingValue)
		design$alphaSpent <- NA_real_
		design$.setParameterType("alphaSpent", C_PARAM_GENERATED)
		return(.getDesignGroupSequentialBetaSpendingApproaches(design))
	}
	
	design$criticalValues[1] <- stats::qnorm(1 - spendingValue / design$sided)
		
	for (k in 2:design$kMax) {
		design$criticalValues[k]  <- .getOneDimensionalRoot(function(scale) {
			design$criticalValues[k] <- scale
			if (design$sided == 2) {
				decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
				probs <- .getGroupSequentialProbabilities(decisionMatrix[, 1:k], design$informationRates[1:k])
				return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - .getSpendingValue(design$alpha, design$informationRates[k], 
									design$sided, design$typeOfDesign, design$gammaA))
			} else{
				if (design$bindingFutility) {
					decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
						design$criticalValues), nrow = 2, byrow = TRUE)
				} else {
					decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
						design$criticalValues), nrow = 2, byrow = TRUE)
				}
				probs <- .getGroupSequentialProbabilities(decisionMatrix[, 1:k], design$informationRates[1:k])
				return(sum(probs[3, ] - probs[2, ]) - .getSpendingValue(design$alpha, design$informationRates[k], 
					design$sided, design$typeOfDesign, design$gammaA))
			}									 
		}, lower = 0, upper = 8, tolerance = design$tolerance,
			callingFunctionInformation = ".getDesignGroupSequentialAlphaSpending")
	}
	
	.calculateAlphaSpent(design)
	
	.getDesignGroupSequentialBetaSpendingApproaches(design)
}

# 
# User defined alpha spending approach
# 
.getDesignGroupSequentialUserDefinedAlphaSpending <- function(design) {
	
	design$criticalValues <- rep(NA_real_, design$kMax)
	design$criticalValues[1] <- stats::qnorm(1 - design$userAlphaSpending[1] / design$sided)
	for (k in (2:design$kMax)) {
		design$criticalValues[k]  <- .getOneDimensionalRoot(function(scale) {
			design$criticalValues[k] <- scale
			if (design$sided == 2) {
				decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), 
					nrow = 2, byrow = TRUE)
				probs <- .getGroupSequentialProbabilities(decisionMatrix[, 1:k], design$informationRates[1:k])
				return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - design$userAlphaSpending[k])
			} else{
				if (design$bindingFutility) {
					decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
						design$criticalValues), nrow = 2, byrow = TRUE)
				} else {
					decisionMatrix <- matrix(c(rep(C_FUTILITY_BOUNDS_DEFAULT, design$kMax), 
						design$criticalValues), nrow = 2, byrow = TRUE)
				}
				probs <- .getGroupSequentialProbabilities(decisionMatrix[, 1:k], design$informationRates[1:k])
				return(sum(probs[3, ] - probs[2, ]) - design$userAlphaSpending[k])
			}									 
		}, lower = 0, upper = 8, tolerance = design$tolerance,
			callingFunctionInformation = ".getDesignGroupSequentialUserDefinedAlphaSpending")
	}
	
	design$criticalValues[!is.na(design$criticalValues) & design$criticalValues >= 8] <- Inf
	
	.calculateAlphaSpent(design)
	
	invisible(.getDesignGroupSequentialBetaSpendingApproaches(design))
}

# 
# Only for alpha spending approaches 
# 
.getDesignGroupSequentialBetaSpendingApproaches <- function(design) {
	
	# beta spending approaches (additional to alpha spending)!
	if (.isBetaSpendingDesignType(design$typeBetaSpending, 
			userDefinedBetaSpendingIncluded = FALSE, noneIncluded = FALSE)) { 		
		.getDesignGroupSequentialBetaSpending(design)
	}
	
	# User defined beta spending
	if (design$typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) { 
		.getDesignGroupSequentialUserDefinedBetaSpending(design)
	}
	
	invisible(design)
}

# 
# Beta spending approaches (additional to alpha spending)
# Find shift with beta spending such that last critical values coincide
# 
.getDesignGroupSequentialBetaSpending <- function(design) {
	
	# Note: calculated without .getOneDimensionalRoot because results may not achieved in inner search
	# Direct bisection produced reliable results (although sometimes slowly)   
	
	if (design$sided == 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'sided' = 2 not allowed; for beta-spending approach, only one-sided testing is possible")
	} 
	if (design$bindingFutility) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"Binding futility is not available for the beta spending function approach")
	} 
	
	iteration <- design$kMax*1000
	cLower1 <- -4
	cUpper1 <- 10
	prec1 <- 1
	while (prec1 > design$tolerance) {
		shift <- (cLower1 + cUpper1) / 2
		futilityBounds <- rep(NA_real_, design$kMax)
		futilityBounds[1] <- stats::qnorm(.getSpendingValue(design$beta, design$informationRates[1], design$sided, 
			design$typeBetaSpending, design$gammaB)) + sqrt(design$informationRates[1]) * shift 
		for (k in 2:design$kMax) {
			cLower2 <- -6
			cUpper2 <- 5
			prec2 <- 1
			while (prec2 > design$tolerance) {
				scale <- (cLower2 + cUpper2) / 2
				futilityBounds[k] <- scale 
				decisionMatrix <- matrix(c(futilityBounds - sqrt(design$informationRates) * shift, 
					design$criticalValues - sqrt(design$informationRates) * shift), nrow = 2, byrow = TRUE)
				probs <- .getGroupSequentialProbabilities(decisionMatrix[, 1:k], design$informationRates[1:k])
				ifelse(sum(probs[1, ]) < .getSpendingValue(design$beta, design$informationRates[k], design$sided, 
					design$typeBetaSpending, design$gammaB), cLower2 <- scale, cUpper2 <- scale)
				ifelse (iteration > 0, prec2 <- cUpper2 - cLower2, prec2 <- 0)
				iteration <- iteration - 1
			}
		}
		ifelse(futilityBounds[design$kMax] < design$criticalValues[design$kMax], cLower1 <- shift, cUpper1 <- shift)	
		ifelse (iteration > 0, prec1 <- cUpper1 - cLower1, prec1 <- 0)
	}
	if ((abs(futilityBounds[design$kMax] - design$criticalValues[design$kMax]) > 1e-05) || (iteration < 0)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "calculation of 'betaSpent' and 'power' ",
				"not possible due to numerical overflow")
	} 
	
	decisionMatrix <- matrix(c(futilityBounds - sqrt(design$informationRates)*shift, 
								design$criticalValues - sqrt(design$informationRates)*shift), nrow = 2, byrow = TRUE)
	probs <- .getGroupSequentialProbabilities(decisionMatrix, design$informationRates)
	design$betaSpent <- cumsum(probs[1, ])
	design$power <- cumsum(probs[3, ] - probs[2, ])
	
	design$.setParameterType("betaSpent", C_PARAM_GENERATED)
	design$.setParameterType("power", C_PARAM_GENERATED)	

	design$futilityBounds <- futilityBounds[1:(design$kMax - 1)]
	design$.setParameterType("futilityBounds", C_PARAM_GENERATED)
	
	invisible(design)
}

# 
# User defined beta spending.
# # Find shift with beta spending such that last critical values coincide
# 
.getDesignGroupSequentialUserDefinedBetaSpending <- function(design) {
	
	# Note: calculated without .getOneDimensionalRoot because results may not achieved in inner search
	# Direct bisection produced reliable results (although sometimes slowly)   
	
	if (design$typeBetaSpending != C_TYPE_OF_DESIGN_BS_USER) { 
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"'typeBetaSpending' ('", design$typeBetaSpending, "') must be '", C_TYPE_OF_DESIGN_BS_USER, "'")
	}
	if (design$sided == 2) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'sided' = 2 not allowed; for beta-spending approach, only one-sided testing is possible")
	} 
	if (design$bindingFutility) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"Binding futility is not available for the beta spending function approach")
	} 
		
	iteration <- design$kMax*1000
	cLower1 <- -4
	cUpper1 <- 10
	prec1 <- 1
	while (prec1 > design$tolerance) {
		shift <- (cLower1 + cUpper1)/2
		futilityBounds <- rep(NA_real_, design$kMax)
		futilityBounds[1] <- stats::qnorm(design$userBetaSpending[1]) + sqrt(design$informationRates[1]) * shift 
		for (k in (2:design$kMax)) {
			cLower2 <- -6
			cUpper2 <- 5
			prec2 <- 1
			while (prec2 > design$tolerance) {
				scale <- (cLower2 + cUpper2)/2
				futilityBounds[k] <- scale 
				nDecisionMatrix <- (matrix(c(futilityBounds - sqrt(design$informationRates) * shift, 
					design$criticalValues - sqrt(design$informationRates) * shift), nrow = 2, byrow = TRUE))
				probs <- .getGroupSequentialProbabilities(nDecisionMatrix[, 1:k], design$informationRates[1:k])
				ifelse(sum(probs[1, ]) < design$userBetaSpending[k], cLower2 <- scale, cUpper2 <- scale)
				ifelse (iteration > 0, prec2 <- cUpper2 - cLower2, prec2 <- 0)
				iteration <- iteration - 1
			}
		}
		ifelse(futilityBounds[design$kMax] < design$criticalValues[design$kMax], cLower1 <- shift, cUpper1 <- shift)	
		ifelse (iteration > 0, prec1 <- cUpper1 - cLower1, prec1 <- 0)	
	}
	
	if ((abs(futilityBounds[design$kMax] - design$criticalValues[design$kMax]) > 1e-05) || (iteration < 0)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "calculation of 'betaSpent' and 'power' ",
			"not possible due to numerical overflow")
	} 
	
	nDecisionMatrix <- matrix(c(futilityBounds - sqrt(design$informationRates) * shift, 
		design$criticalValues - sqrt(design$informationRates)*shift), nrow = 2, byrow = TRUE)
	probs <- .getGroupSequentialProbabilities(nDecisionMatrix, design$informationRates)
	design$betaSpent <- cumsum(probs[1, ])
	design$power <- cumsum(probs[3, ] - probs[2, ])
	
	design$.setParameterType("betaSpent", C_PARAM_GENERATED)
	design$.setParameterType("power", C_PARAM_GENERATED)

	design$futilityBounds <- futilityBounds[1:(design$kMax - 1)]
	design$.setParameterType("futilityBounds", C_PARAM_GENERATED)
	
	invisible(design)
}

#' 
#' @title
#' Get Design Inverse Normal
#' 
#' @description  
#' Provides adjusted boundaries and defines a group sequential design for its use in 
#' the inverse normal combination test.   
#' 
#' @inheritParams getDesignGroupSequential
#' 
#' @template details_group_sequential_design
#'
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#' 
#' @family design functions
#'
#' @template examples_get_design_inverse_normal
#'  
#' @export
#' 
getDesignInverseNormal <- function(
		...,
		kMax = NA_integer_, 
		alpha = NA_real_, 
		beta = NA_real_, 
		sided = 1, # C_SIDED_DEFAULT
		informationRates = NA_real_, 
		futilityBounds = NA_real_, 		
		typeOfDesign = c("OF", "P", "WT", "HP", "WToptimum", "asP", "asOF", "asKD", "asHSD", "asUser"), # C_DEFAULT_TYPE_OF_DESIGN,
		deltaWT = NA_real_, 
		optimizationCriterion = c("ASNH1", "ASNIFH1", "ASNsum"), # C_OPTIMIZATION_CRITERION_DEFAULT
		gammaA = NA_real_, 
		typeBetaSpending = c("none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"), # C_TYPE_OF_DESIGN_BS_NONE
		userAlphaSpending = NA_real_, 
		userBetaSpending = NA_real_, 
		gammaB = NA_real_, 
		bindingFutility = NA,
		constantBoundsHP = 3, # C_CONST_BOUND_HP_DEFAULT,
		twoSidedPower = NA,
		tolerance = 1e-08 # C_DESIGN_TOLERANCE_DEFAULT	
		) {
			
	.warnInCaseOfUnknownArguments(functionName = "getDesignInverseNormal", ...)
	
	return(.getDesignGroupSequential(
		designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
		kMax = kMax, 
		alpha = alpha, 
		beta = beta, 
		sided = sided, 
		informationRates = informationRates, 
		futilityBounds = futilityBounds, 		
		typeOfDesign = typeOfDesign, 
		deltaWT = deltaWT, 
		optimizationCriterion = optimizationCriterion, 
		gammaA = gammaA, 
		typeBetaSpending = typeBetaSpending, 
		userAlphaSpending = userAlphaSpending, 
		userBetaSpending = userBetaSpending, 
		gammaB = gammaB, 
		bindingFutility = bindingFutility,
		constantBoundsHP = constantBoundsHP,
		twoSidedPower = twoSidedPower,
		tolerance = tolerance, 
		userFunctionCallEnabled = TRUE
	))
}

.getDesignInverseNormal <- function(
		...,
		kMax = NA_integer_, 
		alpha = NA_real_, 
		beta = NA_real_, 
		sided = C_SIDED_DEFAULT, 
		informationRates = NA_real_, 
		futilityBounds = NA_real_, 		
		typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN, 
		deltaWT = NA_real_, 
		optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT, 
		gammaA = NA_real_, 
		typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE, 
		userAlphaSpending = NA_real_, 
		userBetaSpending = NA_real_, 
		gammaB = NA_real_, 
		bindingFutility = NA,
		constantBoundsHP = C_CONST_BOUND_HP_DEFAULT,
		twoSidedPower = NA,
		tolerance = C_DESIGN_TOLERANCE_DEFAULT) {
	
	.warnInCaseOfUnknownArguments(functionName = "getDesignInverseNormal", ...)
	
	return(.getDesignGroupSequential(
		designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
		kMax = kMax, 
		alpha = alpha, 
		beta = beta, 
		sided = sided, 
		informationRates = informationRates, 
		futilityBounds = futilityBounds, 		
		typeOfDesign = typeOfDesign, 
		deltaWT = deltaWT, 
		optimizationCriterion = optimizationCriterion, 
		gammaA = gammaA, 
		typeBetaSpending = typeBetaSpending, 
		userAlphaSpending = userAlphaSpending, 
		userBetaSpending = userBetaSpending, 
		gammaB = gammaB, 
		bindingFutility = bindingFutility,
		constantBoundsHP = constantBoundsHP,
		twoSidedPower = twoSidedPower,
		tolerance = tolerance, 
		userFunctionCallEnabled = FALSE
	))
}

.getDesignGroupSequentialDefaultValues <- function() {
	return(list(
		kMax = NA_integer_, 
		alpha = NA_real_, 
		beta = NA_real_, 
		sided = C_SIDED_DEFAULT,
		informationRates = NA_real_, 
		futilityBounds = NA_real_, 		
		typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN, 
		deltaWT = NA_real_, 
		optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT, 
		gammaA = NA_real_, 
		typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE, 
		userAlphaSpending = NA_real_, 
		userBetaSpending = NA_real_, 
		gammaB = NA_real_, 
		twoSidedPower = C_TWO_SIDED_POWER_DEFAULT,
		tolerance = C_DESIGN_TOLERANCE_DEFAULT
	))
}

.getDesignInverseNormalDefaultValues <- function() {
	return(.getDesignGroupSequentialDefaultValues())
}  

# 
# Param: userFunctionCallEnabled if \code{TRUE}, additional parameter validation methods will be called.
#
.getDesignGroupSequential <- function(
		...,
		designClass = C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
		kMax = NA_integer_, 
		alpha = NA_real_, 
		beta = NA_real_, 
		sided = C_SIDED_DEFAULT, 
		informationRates = NA_real_, 
		futilityBounds = NA_real_, 		
		typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN, 
		deltaWT = NA_real_, 
		optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT, 
		gammaA = NA_real_, 
		typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE, 
		userAlphaSpending = NA_real_, 
		userBetaSpending = NA_real_, 
		gammaB = NA_real_, 
		bindingFutility = C_BINDING_FUTILITY_DEFAULT,
		constantBoundsHP = C_CONST_BOUND_HP_DEFAULT,
		twoSidedPower = NA,
		tolerance = C_DESIGN_TOLERANCE_DEFAULT, 
		userFunctionCallEnabled = FALSE) {
	
	typeOfDesign <- .matchArgument(typeOfDesign, C_DEFAULT_TYPE_OF_DESIGN)
	optimizationCriterion <- .matchArgument(optimizationCriterion, C_OPTIMIZATION_CRITERION_DEFAULT)
	typeBetaSpending <- .matchArgument(typeBetaSpending, C_TYPE_OF_DESIGN_BS_NONE)		

	if (.isDefinedArgument(kMax, argumentExistsValidationEnabled = userFunctionCallEnabled)) {
		.assertIsValidKMax(kMax)
		if (!is.integer(kMax)) {
			kMax <- as.integer(kMax)
		}
	}
	
	if (is.na(bindingFutility)) {
		bindingFutility <- C_BINDING_FUTILITY_DEFAULT
	} 
	else if (userFunctionCallEnabled && ((!is.na(kMax) && kMax == 1) || (!any(is.na(futilityBounds)) &&
			all(futilityBounds == C_FUTILITY_BOUNDS_DEFAULT)))) {
		warning("'bindingFutility' (", bindingFutility, ") will be ignored", call. = FALSE)
	}
		
	design <- .createDesign(
		designClass             = designClass,
		kMax                    = kMax,
		alpha                   = alpha,  
		beta                    = beta,  
		sided                   = sided,  
		informationRates        = informationRates,
		futilityBounds  		= futilityBounds,	
		typeOfDesign            = typeOfDesign,
		deltaWT                 = deltaWT,
		optimizationCriterion   = optimizationCriterion,
		gammaA                  = gammaA,
		typeBetaSpending        = typeBetaSpending,
		userAlphaSpending       = userAlphaSpending,  
		userBetaSpending        = userBetaSpending,
		gammaB                  = gammaB,
		bindingFutility         = bindingFutility,
		constantBoundsHP        = constantBoundsHP,
		twoSidedPower           = twoSidedPower,
		tolerance               = tolerance)
	
	if (userFunctionCallEnabled) {
		.validateBaseParameters(design)
		.validateTypeOfDesign(design)
		
		.assertIsValidTolerance(tolerance)	
		.assertDesignParameterExists(design, "alpha", C_ALPHA_DEFAULT)
		.assertDesignParameterExists(design, "beta", C_BETA_DEFAULT)
		.assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
		.assertDesignParameterExists(design, "typeOfDesign", C_DEFAULT_TYPE_OF_DESIGN)
		.assertDesignParameterExists(design, "bindingFutility", C_BINDING_FUTILITY_DEFAULT)
		.assertDesignParameterExists(design, "tolerance", C_DESIGN_TOLERANCE_DEFAULT)
		
		if (typeOfDesign != C_TYPE_OF_DESIGN_WT && !is.na(deltaWT)) {
			warning("'deltaWT' (", deltaWT, ") will be ignored. ", call. = FALSE)
		}
		if ((typeOfDesign != C_TYPE_OF_DESIGN_AS_KD) && 
				(typeOfDesign != C_TYPE_OF_DESIGN_AS_HSD) && !is.na(gammaA)) {
			warning("'gammaA' (", gammaA, ") will be ignored. ", call. = FALSE)
		}
		if ((typeBetaSpending != C_TYPE_OF_DESIGN_BS_KD) && 
				(typeBetaSpending != C_TYPE_OF_DESIGN_BS_HSD) && !is.na(gammaB)) {
			warning("'gammaB' (", gammaB, ") will be ignored. ", call. = FALSE)
		}
	}
	
	if (design$sided == 2 && design$bindingFutility) {
		warning("'bindingFutility' will be ignored because the test is defined as two-sided", call. = FALSE)
		design$bindingFutility <- FALSE
	}
	
	if (design$sided == 1 && design$twoSidedPower) {
		warning("'twoSidedPower' will be ignored because the test is defined as one-sided", call. = FALSE)
		design$twoSidedPower <- FALSE
	}
	
	if (userFunctionCallEnabled) {
		.validateAlphaAndBeta(design)
	}
		
	design$alphaSpent <- rep(NA_real_, design$kMax)
	design$betaSpent <- rep(NA_real_, design$kMax)
	design$power <- rep(NA_real_, design$kMax)
	
	if (userFunctionCallEnabled) {
		design$.setParameterType("betaSpent", C_PARAM_NOT_APPLICABLE)
		design$.setParameterType("power", C_PARAM_NOT_APPLICABLE)
		design$.setParameterType("alphaSpent", C_PARAM_NOT_APPLICABLE)
		design$.setParameterType("criticalValues", C_PARAM_GENERATED)
	}
	
	if (design$kMax == 1) {
		
		.getDesignGroupSequentialKMax1(design)
		
	} else {
	
		# Wang and Tsiatis design
		if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT || 
				design$typeOfDesign == C_TYPE_OF_DESIGN_P || 
				design$typeOfDesign == C_TYPE_OF_DESIGN_OF) {  
			.getDesignGroupSequentialWangAndTsiatis(design)
		}
		
		# Haybittle & Peto design
		else if (design$typeOfDesign == C_TYPE_OF_DESIGN_HP) {  
			.getDesignGroupSequentialHaybittleAndPeto(design)
		}
		
		# Optimum design within Wang and Tsiatis class
		else if (design$typeOfDesign == C_TYPE_OF_DESIGN_WT_OPTIMUM) { 
			.getDesignGroupSequentialWangAndTsiatisOptimum(design)
		}
		
		# alpha spending approaches
		else if (.isAlphaSpendingDesignType(design$typeOfDesign, userDefinedAlphaSpendingIncluded = FALSE)) { 
			.getDesignGroupSequentialAlphaSpending(design)
		}
		
		# user defined alpha spending approach
		else if (design$typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) { 
			.getDesignGroupSequentialUserDefinedAlphaSpending(design)
		}
	}	
	
	design$stageLevels <- 1 - stats::pnorm(design$criticalValues)	
	design$.setParameterType("stageLevels", C_PARAM_GENERATED)
	
	if (design$kMax == 1) {
		design$.setParameterType("futilityBounds", C_PARAM_NOT_APPLICABLE)
	}
	
	if (length(design$futilityBounds) == 0 ||
			all(design$futilityBounds == C_FUTILITY_BOUNDS_DEFAULT)) {
		design$.setParameterType("bindingFutility", C_PARAM_NOT_APPLICABLE)
		design$.setParameterType("futilityBounds", C_PARAM_NOT_APPLICABLE)
	}
	
	design$.initStages()
	
	return(design)
}

#'
#' @title
#' Get Design Group Sequential
#' 
#' @description
#' Provides adjusted boundaries and defines a group sequential design.   
#' 
#' @inheritParams param_kMax
#' @inheritParams param_alpha 
#' @inheritParams param_beta 
#' @inheritParams param_sided
#' @inheritParams param_typeOfDesign 
#' @inheritParams param_informationRates
#' @param futilityBounds The futility bounds, defined on the test statistic z scale 
#'        (numeric vector of length \code{kMax - 1}).
#' @inheritParams param_bindingFutility
#' @param deltaWT Delta for Wang & Tsiatis Delta class.
#' @param constantBoundsHP The constant bounds up to stage \code{kMax - 1} for the 
#'        Haybittle & Peto design (default is \code{3}).
#' @param optimizationCriterion Optimization criterion for optimum design within 
#'        Wang & Tsiatis class (\code{"ASNH1"}, \code{"ASNIFH1"}, 
#'        \code{"ASNsum"}), default is \code{"ASNH1"}, see details.
#' @param typeBetaSpending Type of beta spending. Type of of beta spending is one of the following: 
#'        O'Brien & Fleming type beta spending, Pocock type beta spending, 
#'        Kim & DeMets beta spending, Hwang, Shi & DeCani beta spending, user defined 
#'        beta spending (\code{"bsOF"}, \code{"bsP"}, \code{"bsKD"}, 
#'        \code{"bsHSD"}, \code{"bsUser"}, default is \code{"none"}).
#' @param gammaA Parameter for alpha spending function.
#' @param gammaB Parameter for beta spending function.
#' @inheritParams param_userAlphaSpending
#' @param userBetaSpending The user defined beta spending. Vector of length \code{kMax} containing the cumulative 
#' 		  beta-spending up to each interim stage.
#' @param twoSidedPower For two-sided testing, if \code{twoSidedPower = TRUE} is specified 
#'        the sample size calculation is performed by considering both tails of the distribution. 
#'        Default is \code{FALSE}, i.e., it is assumed that one tail probability is equal to 0 or the power
#'        should be directed to one part.
#' @param tolerance The numerical tolerance, default is \code{1e-08}.
#' @inheritParams param_three_dots
#' 
#' @template details_group_sequential_design
#'
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#' 
#' @family design functions
#' 
#' @template examples_get_design_group_sequential
#' 
#' @export
#' 
getDesignGroupSequential <- function(
		...,
		kMax = NA_integer_, 
		alpha = NA_real_, 
		beta = NA_real_, 
		sided = 1, # C_SIDED_DEFAULT
		informationRates = NA_real_, 
		futilityBounds = NA_real_, 		
		typeOfDesign = c("OF", "P", "WT", "HP", "WToptimum", "asP", "asOF", "asKD", "asHSD", "asUser"), # C_DEFAULT_TYPE_OF_DESIGN,
		deltaWT = NA_real_, 
		optimizationCriterion = c("ASNH1", "ASNIFH1", "ASNsum"), # C_OPTIMIZATION_CRITERION_DEFAULT
		gammaA = NA_real_, 
		typeBetaSpending = c("none", "bsP", "bsOF", "bsKD", "bsHSD", "bsUser"), # C_TYPE_OF_DESIGN_BS_NONE
		userAlphaSpending = NA_real_, 
		userBetaSpending = NA_real_, 
		gammaB = NA_real_, 
		bindingFutility = NA,
		constantBoundsHP = 3, # C_CONST_BOUND_HP_DEFAULT,
		twoSidedPower = NA,
		tolerance = 1e-08 # C_DESIGN_TOLERANCE_DEFAULT	
		) {
		
	.warnInCaseOfUnknownArguments(functionName = "getDesignGroupSequential", ...)
	
	return(.getDesignGroupSequential(
		designClass = C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,
		kMax = kMax, 
		alpha = alpha, 
		beta = beta, 
		sided = sided, 
		informationRates = informationRates, 
		futilityBounds = futilityBounds, 
		typeOfDesign = typeOfDesign, 
		deltaWT = deltaWT, 
		optimizationCriterion = optimizationCriterion, 
		gammaA = gammaA, 
		typeBetaSpending = typeBetaSpending, 
		userAlphaSpending = userAlphaSpending, 
		userBetaSpending = userBetaSpending, 
		gammaB = gammaB, 
		bindingFutility = bindingFutility,
		constantBoundsHP = constantBoundsHP,
		twoSidedPower = twoSidedPower,
		tolerance = tolerance,
		userFunctionCallEnabled = TRUE))
}

.getFixedSampleSize <- function(alpha, beta, sided, twoSidedPower = C_TWO_SIDED_POWER_DEFAULT) {	
	
	.assertIsValidAlphaAndBeta(alpha = alpha, beta = beta)
	.assertIsValidSidedParameter(sided)
	
	if (sided == 1) {
		return((stats::qnorm(1 - alpha) + stats::qnorm(1 - beta))^2)
	} 
	if (twoSidedPower) {
		n <- .getOneDimensionalRoot(function(n) {
				stats::pnorm(-stats::qnorm(1 - alpha / 2) - sqrt(n)) - 
					stats::pnorm(stats::qnorm(1 - alpha / 2) - sqrt(n)) + beta
			}, 
			lower = 0, 
			upper = 2 * (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2, 
			tolerance = 1e-08, callingFunctionInformation = ".getFixedSampleSize")
	} else {
		n <- (stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta))^2
	}	
	return(n)
	
}

#' @title
#' Get Design Characteristics
#' 
#' @description  
#' Calculates the characteristics of a design and returns it.
#' 
#' @inheritParams param_design
#' 
#' @details 
#' Calculates the inflation factor (IF), 
#' the expected reduction in sample size under H1, under H0, and under a value in between H0 and H1. 
#' Furthermore, absolute information values are calculated 
#' under the prototype case testing H0: mu = 0 against H1: mu = 1.
#' 
#' @return Returns a \code{\link{TrialDesignCharacteristics}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary}} to display a summary of the object,
#'   \item \code{\link[=plot.ParameterSet]{plot}} to plot the object,
#'   \item \code{\link[=as.data.frame.TrialDesignCharacteristics]{as.data.frame}} to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#' 
#' @family design functions
#' 
#' @template examples_get_design_characteristics
#'
#' @export
#' 
getDesignCharacteristics <- function(design) {
	return(.getDesignCharacteristics(design = design, userFunctionCallEnabled = TRUE) )
}

.getDesignCharacteristics <- function(..., design, userFunctionCallEnabled = FALSE) {

	.assertIsTrialDesignInverseNormalOrGroupSequential(design)
	.assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
	.assertIsValidSidedParameter(design$sided)	
	
	if (userFunctionCallEnabled) {
		.validateAlphaAndBeta(design = design)
	}
	
	design$informationRates <- .getValidatedInformationRates(design, writeToDesign = FALSE)	
	design$futilityBounds <- .getValidatedFutilityBounds(design, writeToDesign = FALSE)
	
	designCharacteristics <- TrialDesignCharacteristics(design = design)
		
	designCharacteristics$rejectionProbabilities <- rep(NA_real_, design$kMax)
	designCharacteristics$.setParameterType("rejectionProbabilities", C_PARAM_NOT_APPLICABLE)
	
	designCharacteristics$futilityProbabilities <- rep(NA_real_, design$kMax - 1)
	designCharacteristics$.setParameterType("futilityProbabilities", C_PARAM_NOT_APPLICABLE)
		
	nFixed <- .getFixedSampleSize(alpha = design$alpha, beta = design$beta, 
		sided = design$sided, twoSidedPower = design$twoSidedPower)
	designCharacteristics$nFixed <- nFixed
	designCharacteristics$.setParameterType("nFixed", C_PARAM_GENERATED) 
	
	if (design$kMax == 1) {
		designCharacteristics$shift <- nFixed
		designCharacteristics$.setParameterType("shift", C_PARAM_GENERATED) 
		
		designCharacteristics$inflationFactor <- designCharacteristics$shift / nFixed
		designCharacteristics$.setParameterType("inflationFactor", C_PARAM_GENERATED) 
		
		designCharacteristics$power <- 1 - design$beta
		designCharacteristics$.setParameterType("power", design$.getParameterType("power")) 
		
		designCharacteristics$.setParameterType("information", C_PARAM_NOT_APPLICABLE)
		
		designCharacteristics$.setParameterType("averageSampleNumber1", C_PARAM_NOT_APPLICABLE)
		designCharacteristics$.setParameterType("averageSampleNumber01", C_PARAM_NOT_APPLICABLE)
		designCharacteristics$.setParameterType("averageSampleNumber0", C_PARAM_NOT_APPLICABLE)
		designCharacteristics$.setParameterType(".probs", C_PARAM_NOT_APPLICABLE)
		
		return(designCharacteristics)
	} 
	
	informationRates <- design$informationRates
	
	shift <- .getOneDimensionalRoot(function(shift) {
		if (design$sided == 2) {
			decisionMatrix <- matrix(c(-design$criticalValues - sqrt(shift*informationRates), 
							design$criticalValues - sqrt(shift*informationRates)), nrow = 2, byrow = TRUE)
			probs <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
			if (design$twoSidedPower) {
				return(sum(probs[3, ] - probs[2, ] + probs[1, ]) - 1 + design$beta)
			} else {
				return(sum(probs[3, ] - probs[2, ]) - 1 + design$beta)
			}	
		} else {
			shiftedFutilityBounds <- design$futilityBounds - sqrt(shift*informationRates[1:(design$kMax - 1)])
			shiftedFutilityBounds[design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- 
					C_FUTILITY_BOUNDS_DEFAULT
			decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
							design$criticalValues - sqrt(shift*informationRates)), nrow = 2, byrow = TRUE)
			probs <- .getGroupSequentialProbabilities(decisionMatrix , informationRates)
			return(sum(probs[3, ] - probs[2, ]) - 1 + design$beta)
		}
	}, lower = 0, upper = 4 * (stats::qnorm(1 - design$alpha / design$sided) + stats::qnorm(1 - design$beta))^2, 
		tolerance = design$tolerance, callingFunctionInformation = ".getDesignCharacteristics")
	
	if (design$sided == 2) {
		decisionMatrix <- matrix(c(-design$criticalValues - sqrt(shift*informationRates), 
						design$criticalValues - sqrt(shift*informationRates)), nrow = 2, byrow = TRUE)
	} else {
		shiftedFutilityBounds <- design$futilityBounds - sqrt(shift*informationRates[1:(design$kMax - 1)])
		shiftedFutilityBounds[design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- 
				C_FUTILITY_BOUNDS_DEFAULT
		decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, 
						design$criticalValues - sqrt(shift*informationRates)), nrow = 2, byrow = TRUE)
	}
	probs <- .getGroupSequentialProbabilities(decisionMatrix , informationRates)

	designCharacteristics$shift <- shift
	designCharacteristics$.setParameterType("shift", C_PARAM_GENERATED) 
	designCharacteristics$.probs <- probs
	designCharacteristics$.setParameterType(".probs", C_PARAM_GENERATED)

	if (design$twoSidedPower) {
		designCharacteristics$power <- cumsum(probs[3, ] - probs[2, ] + probs[1, ])
	} else {
		designCharacteristics$power <- cumsum(probs[3, ] - probs[2, ])
	}
	designCharacteristics$.setParameterType("power", C_PARAM_GENERATED) 

	if (design$twoSidedPower) {
		designCharacteristics$rejectionProbabilities <- probs[3, ] - probs[2, ] + probs[1, ]
	} else {
		designCharacteristics$rejectionProbabilities <- probs[3, ] - probs[2, ]
	}	
	designCharacteristics$.setParameterType("rejectionProbabilities", C_PARAM_GENERATED)
	
	if (design$kMax > 1) {
		if (design$sided == 2) {
			designCharacteristics$futilityProbabilities <- rep(0, design$kMax - 1)
		} else {
			designCharacteristics$futilityProbabilities <- probs[1, 1:(design$kMax - 1)]
		}
		designCharacteristics$.setParameterType("futilityProbabilities", C_PARAM_GENERATED)
	} 	
		
	designCharacteristics$information <- informationRates * shift
	designCharacteristics$.setParameterType("information", C_PARAM_GENERATED)
	
	designCharacteristics$averageSampleNumber1 <- .getAverageSampleNumber(design$kMax, design$informationRates, probs, shift, nFixed)
	designCharacteristics$.setParameterType("averageSampleNumber1", C_PARAM_GENERATED)
	
	if (design$sided == 2) {
		decisionMatrix <- matrix(c(-design$criticalValues, design$criticalValues), nrow = 2, byrow = TRUE)
	} else {
		decisionMatrix <- matrix(c(design$futilityBounds, C_FUTILITY_BOUNDS_DEFAULT, design$criticalValues), nrow = 2, byrow = TRUE)
	}
	
	probs0 <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)		
	designCharacteristics$averageSampleNumber0 <- .getAverageSampleNumber(design$kMax, design$informationRates, probs0, shift, nFixed)
	designCharacteristics$.setParameterType("averageSampleNumber0", C_PARAM_GENERATED)
	
	if (design$sided == 2) {
		decisionMatrix <- matrix(c(-design$criticalValues - sqrt(shift * informationRates) / 2, 
				design$criticalValues - sqrt(shift * informationRates) / 2), nrow = 2, byrow = TRUE)
	} else {
		shiftedFutilityBounds <- design$futilityBounds - sqrt(shift * informationRates[1:(design$kMax - 1)]) / 2
		shiftedFutilityBounds[design$futilityBounds <= C_FUTILITY_BOUNDS_DEFAULT] <- C_FUTILITY_BOUNDS_DEFAULT
		decisionMatrix <- matrix(c(shiftedFutilityBounds, C_FUTILITY_BOUNDS_DEFAULT, design$criticalValues - sqrt(shift * informationRates) / 2), nrow = 2, byrow = TRUE)
	}
	probs01 <- .getGroupSequentialProbabilities(decisionMatrix, informationRates)
	designCharacteristics$averageSampleNumber01 <- .getAverageSampleNumber(design$kMax, design$informationRates, probs01, shift, nFixed)
	designCharacteristics$.setParameterType("averageSampleNumber01", C_PARAM_GENERATED)
	
	designCharacteristics$inflationFactor <- shift / nFixed
	designCharacteristics$.setParameterType("inflationFactor", C_PARAM_GENERATED) 
	
	if ((designCharacteristics$inflationFactor > 4) || (designCharacteristics$inflationFactor < 1 - 1e-08)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "Inflation factor cannot be calculated")
	}
	
	return(designCharacteristics)
}

.getAverageSampleNumber <- function(kMax, informationRates, probs, shift, nFixed) {
	return((shift - sum((probs[3, 1:(kMax - 1)] - 
		probs[2, 1:(kMax - 1)] + probs[1, 1:(kMax - 1)]) * 
		(informationRates[kMax] - informationRates[1:(kMax - 1)]) * shift)) / nFixed)
}

#' 
#' @title 
#' Get Power And Average Sample Number
#' 
#' @description 
#' Returns the power and average sample number of the specified design.
#' 
#' @inheritParams param_design
#' @inheritParams param_theta
#' @inheritParams param_nMax
#' 
#' @details 
#' This function returns the power and average sample number (ASN) of the specified 
#' design for the prototype case which is testing H0: mu = mu0 in a one-sample design.
#' \code{theta} represents the standardized effect \code{(mu - mu0) / sigma} and power and ASN 
#' is calculated for maximum sample size \code{nMax}.
#' For other designs than the one-sample test of a mean the standardized effect needs to be adjusted accordingly.  
#' 
#' @return Returns a \code{\link{PowerAndAverageSampleNumberResult}} object.
#' The following generics (R generic functions) are available for this result object:
#' \itemize{
#'   \item \code{\link[=names.FieldSet]{names}} to obtain the field names,
#'   \item \code{\link[=print.FieldSet]{print}} to print the object,
#'   \item \code{\link[=summary.ParameterSet]{summary}} to display a summary of the object,
#'   \item \code{\link[=plot.ParameterSet]{plot}} to plot the object,
#'   \item \code{\link[=as.data.frame.PowerAndAverageSampleNumberResult]{as.data.frame}} 
#'         to coerce the object to a \code{\link[base]{data.frame}},
#'   \item \code{\link[=as.matrix.FieldSet]{as.matrix}} to coerce the object to a \code{\link[base]{matrix}}.
#' }
#' @template how_to_get_help_for_generics
#' 
#' @family design functions
#' 
#' @template examples_get_power_and_average_sample_number
#' 
#' @export
#' 
getPowerAndAverageSampleNumber <- function(design, theta = seq(-1, 1, 0.02), nMax = 100) {
	.assertIsTrialDesign(design)
	.assertIsSingleNumber(nMax, "nMax")
	.assertIsInClosedInterval(nMax, "nMax", lower = 1, upper = NULL)
	return(PowerAndAverageSampleNumberResult(design = design, theta = theta, nMax = nMax))	
}
