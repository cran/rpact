######################################################################################
#                                                                                    #
# -- Trial design classes --                                                         #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 28-09-2018                                                                   #
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


#' @include f_core_constants.R
NULL

#' 
#' @name TrialDesign
#' 
#' @title
#' Basic Trial Design
#' 
#' @description 
#' Basic class for trial designs.
#' 
#' @details
#' \code{TrialDesign} is the basic class for
#' \itemize{ 
#'   \item \code{\link{TrialDesignFisher}}, 
#'   \item \code{\link{TrialDesignGroupSequential}}, and 
#'   \item \code{\link{TrialDesignInverseNormal}}.
#' }
#' 
#' @name TrialDesign_initialize
#' Initializes the object.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesign <- setRefClass("TrialDesign",
	contains = "ParameterSet",
	fields = list(
		kMax = "integer", 
		alpha = "numeric",
		stages = "integer",
		informationRates = "numeric",
		userAlphaSpending = "numeric",
		criticalValues = "numeric",
		stageLevels = "numeric", 
		alphaSpent = "numeric",
		bindingFutility = "logical",
		tolerance = "numeric"
	),
	methods = list(
		initialize = function(...,
			kMax = NA_integer_, 
			alpha = NA_real_, 
			informationRates = NA_real_, 
			userAlphaSpending = NA_real_, 
			criticalValues = NA_real_, 
			stageLevels = NA_real_, 
			alphaSpent = NA_real_, 
			bindingFutility = NA,
			tolerance = C_ANALYSIS_TOLERANCE_DEFAULT) {
			callSuper(..., 
				kMax = kMax, 
				alpha = alpha, 
				informationRates = informationRates, 
				userAlphaSpending = userAlphaSpending,
				criticalValues = criticalValues, 
				stageLevels = stageLevels, 
				alphaSpent = alphaSpent, 
				bindingFutility = bindingFutility,
				tolerance = tolerance)
			
			.parameterNames <<- .getSubListByNames(.getParameterNames(design = .self), c(
				"stages", 
				"kMax", 
				"alpha", 
				"informationRates", 
				"userAlphaSpending", 
				"criticalValues", 
				"stageLevels", 
				"alphaSpent", 
				"bindingFutility",
				"tolerance"
			))
			
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		show = function(showType = 1) {
			'Method for automatically printing trial design objects'
			.initStages()
			if (showType == 2) {
				cat("Technical summary of the trial design object of class",
					methods::classLabel(class(.self)), ":\n\n", sep = "")
				.showAllParameters()
				.showParameterTypeDescription()
			} else {
				cat("Design parameters and output of ", .toString(), ":\n\n", sep = "")
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getDerivedParameters(), "Derived from user defined parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getDefaultParameters(), "Default parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Output",
					orderByParameterName = FALSE)
				.showUnknownParameters()
			}
		},
		.toString = function(startWithUpperCase = FALSE) {
			if (.isTrialDesignGroupSequential(.self)) {
				return(paste(ifelse(startWithUpperCase, "Group", "group"), "sequential design"))
			}
			
			if (.isTrialDesignInverseNormal(.self)) {
				return(paste(ifelse(startWithUpperCase, "Inverse", "inverse"), "normal design"))
			}
			
			if (.isTrialDesignFisher(.self)) {
				return("Fisher design")
			}
			
			return("unknown trial design")
		},
		.initStages = function() {
			if (!is.na(kMax) && kMax > 0) {
				stages <<- c(1L:kMax)
				if (kMax == C_KMAX_DEFAULT) {
					.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
				} else {
					.setParameterType("stages", C_PARAM_USER_DEFINED)
				}
			}
		},
		.setParameterType = function(parameterName, parameterType) {
			parameterType <- callSuper(parameterName = parameterName, 
				parameterType = parameterType)
			
			if (parameterName == "futilityBounds" && !bindingFutility) {
				.parameterNames$futilityBounds <<- C_PARAMETER_NAMES[["futilityBoundsNonBinding"]]
			}
			
			invisible(parameterType)
		}
	)
)

#' 
#' @name TrialDesignCharacteristics
#' 
#' @title
#' Trial Design Characteristics
#' 
#' @description
#' Class for trial design characteristics.
#' 
#' @details
#' \code{TrialDesignCharacteristics} contains all fields required to collect the characteristics of a design.
#' This object should not be created directly; use \code{getDesignCharacteristics} 
#' with suitable arguments to create it.
#' 
#' @seealso \code{\link{getDesignCharacteristics}} for getting the design characteristics.
#' 
#' @include class_core_parameter_set.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignCharacteristics <- setRefClass("TrialDesignCharacteristics",	
	contains = "ParameterSet",
	fields = list(
		.design = "TrialDesign", 
		.probs = "matrix", 
		nFixed = "numeric", 
		shift = "numeric", 
		inflationFactor = "numeric", 
		stages = "integer",
		information = "numeric", 
		power = "numeric", 
		rejectionProbabilities = "numeric", # efficacy probabilities
		futilityProbabilities = "numeric",
		averageSampleNumber1 = "numeric", 
		averageSampleNumber01 = "numeric", 
		averageSampleNumber0 = "numeric"
	),
	methods = list(
		initialize = function(design, ...) {
			callSuper(.design = design, ...)
			.parameterNames <<- .getParameterNames(design)
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
		},
		show = function(showType = 1) {
			'Method for automatically printing trial design characteristics objects'	
			.initStages()		
			if (showType == 2) {
				cat("Technical summary of the design characteristics object of class",
					methods::classLabel(class(.self)), ":\n", sep = "")
				.showAllParameters()
				.showParameterTypeDescription()
			} else {
				cat(.toString(startWithUpperCase = TRUE), ":\n\n", sep = "")
				.showParametersOfOneGroup(.getUserDefinedParameters(), "User defined parameters",
					orderByParameterName = FALSE)
				.showParametersOfOneGroup(.getGeneratedParameters(), "Calculated design characteristics",
					orderByParameterName = FALSE)
				.showUnknownParameters()
			}
		},
		.initStages = function() {
			if (!is.na(.design$kMax) && .design$kMax > 0) {
				stages <<- c(1L:.design$kMax)
				if (.design$kMax == C_KMAX_DEFAULT) {
					.setParameterType("stages", C_PARAM_DEFAULT_VALUE)
				} else {
					.setParameterType("stages", C_PARAM_USER_DEFINED)
				}
			}
		},
		.toString = function(startWithUpperCase = FALSE) {
			return(paste(.design$.toString(startWithUpperCase = startWithUpperCase), "characteristics"))
		}
	)
)

#'
#' @name TrialDesignCharacteristics_as.data.frame
#' 
#' @title
#' Coerce TrialDesignCharacteristics to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesignCharacteristics} as data frame.
#' 
#' @param niceColumnNamesEnabled logical. If \code{TRUE}, nice looking names will be used; 
#'        syntactic names otherwise (see \code{\link[base]{make.names}}).
#' @param includeAllParameters logical. If \code{TRUE}, all parameters will be included; 
#'        a meaningful parameter selection otherwise.
#'
#' @details 
#' Each element of the \code{TrialDesignCharacteristics} is converted to a column in the data frame. 
#' 
#' @export
#' 
#' @keywords internal
#'
as.data.frame.TrialDesignCharacteristics <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = TRUE, includeAllParameters = FALSE, ...) {
	
	x$.initStages()
	parameterNamesToBeExcluded = c("nFixed", "shift")
	return(x$.getAsDataFrame(parameterNames = parameterNamesToBeExcluded, 
			niceColumnNamesEnabled = niceColumnNamesEnabled, includeAllParameters = includeAllParameters,
			handleParameterNamesAsToBeExcluded = TRUE,
			tableColumnNames = .getTableColumnNames(design = x$.design)))
}

#' 
#' @name TrialDesignFisher
#' 
#' @title
#' Fisher Design
#' 
#' @description
#' Trial design for Fisher's combination test.
#' 
#' @details
#' This object should not be created directly; use \code{\link{getDesignFisher}} 
#' with suitable arguments to create a Fisher design.
#' 
#' @seealso \code{\link{getDesignFisher}} for creating a Fisher design.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignFisher <- setRefClass(C_CLASS_NAME_TRIAL_DESIGN_FISHER,	
	contains = "TrialDesign",
	fields = list(
		method = "character", 
		alpha0Vec = "numeric",
		scale = "numeric",
		nonStochasticCurtailment = "logical",
		sided = "integer",	
		simAlpha = "numeric",
		iterations = "numeric",
		seed = "numeric"
	),
	methods = list(
		initialize = function(..., 
			method = NA_character_, 
			alpha0Vec = NA_real_, 
			scale = NA_real_, 
			nonStochasticCurtailment = FALSE,
			sided = as.integer(1), 
			simAlpha = NA_real_, 
			iterations = 0, 
			seed = NA_real_,
			tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT) {
			callSuper(..., 
				method = method, 
				alpha0Vec = alpha0Vec,
				scale = scale, 
				nonStochasticCurtailment = nonStochasticCurtailment, 
				sided = sided,
				simAlpha = simAlpha, 
				iterations = iterations,
				seed = seed,
				tolerance = tolerance
			)
	
			.parameterNames <<- c(.parameterNames, .getSubListByNames(
					.getParameterNames(design = .self), c(
				"method", 
				"alpha0Vec", 
				"scale", 
				"nonStochasticCurtailment",
				"sided",
				"simAlpha"
			)))
			
			.parameterFormatFunctions$criticalValues <<- "formatFisherCriticalValues"
			
			.initParameterTypes()
		},
		
		# Defines the order of the parameter output
		.getParametersToShow = function() {
			return(c(
				"method",
				"kMax",
				"stages",
				"informationRates",
				"alpha",
				"alpha0Vec",
				"bindingFutility",
				"sided",
				"tolerance",
				"iterations",
				"seed",
				"alphaSpent",
				"userAlphaSpending",
				"criticalValues",
				"stageLevels",
				"scale",
				"simAlpha",
				"nonStochasticCurtailment"
			))
		}
	)
)

#'					
#' @name TrialDesignInverseNormal
#' 
#' @title
#' Inverse Normal Design
#' 
#' @description
#' Trial design for inverse normal method.
#' 
#' @details
#' This object should not be created directly; use \code{\link{getDesignInverseNormal}} 
#' with suitable arguments to create a inverse normal design.
#' 
#' @seealso \code{\link{getDesignInverseNormal}} for creating a inverse normal design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignInverseNormal <- setRefClass(C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,	
	contains = "TrialDesign",
	fields = list( 
		typeOfDesign = "character",
		beta = "numeric", 
		deltaWT = "numeric",
		futilityBounds = "numeric",
		gammaA = "numeric",
		gammaB = "numeric",
		optimizationCriterion = "character",
		sided = "integer",	
		betaSpent = "numeric", 
		typeBetaSpending = "character",
		userBetaSpending = "numeric",
		power = "numeric",
		twoSidedPower = "logical",
		constantBoundsHP = "numeric"
	),
	methods = list(
		initialize = function(..., 
			beta = C_BETA_DEFAULT, 
			betaSpent = NA_real_,
			sided = as.integer(1), 
			futilityBounds = NA_real_,
			typeOfDesign = C_DEFAULT_TYPE_OF_DESIGN,
			deltaWT = 0,
			optimizationCriterion = C_OPTIMIZATION_CRITERION_DEFAULT,
			gammaA = 1,
			gammaB = 1,
			typeBetaSpending = C_TYPE_OF_DESIGN_BS_NONE,
			userBetaSpending = NA_real_, 
			power = NA_real_,
			twoSidedPower = C_TWO_SIDED_POWER_DEFAULT,
			constantBoundsHP = NA_real_
		) {
			callSuper(..., 
				beta = beta, 
				betaSpent = betaSpent,
				sided = sided, 
				futilityBounds = futilityBounds, 
				typeOfDesign = typeOfDesign, 
				deltaWT = deltaWT,
				optimizationCriterion = optimizationCriterion,
				gammaA = gammaA,
				gammaB = gammaB,
				typeBetaSpending = typeBetaSpending,
				userBetaSpending = userBetaSpending,
				power = power,
				twoSidedPower = twoSidedPower,
				constantBoundsHP = constantBoundsHP
			)
			
			.parameterNames <<- c(.parameterNames, .getSubListByNames(
					.getParameterNames(design = .self), c(
				"beta",
				"betaSpent",
				"sided",
				"futilityBounds",
				"typeOfDesign",
				"deltaWT",
				"optimizationCriterion",
				"gammaA",
				"gammaB",
				"typeBetaSpending",
				"userBetaSpending",
				"power",
				"twoSidedPower",
				"constantBoundsHP"
			)))
			
			.parameterFormatFunctions$criticalValues <<- "formatGroupSequentialCriticalValues"
			
			.initParameterTypes()
			
		},
		
		# Defines the order of the parameter output
		.getParametersToShow = function() {
			return(c(
				"typeOfDesign",
				"kMax",
				"stages",
				"informationRates",
				"alpha",
				"beta",
				"power",
				"twoSidedPower",
				"deltaWT",
				"futilityBounds",
				"bindingFutility",
				"constantBoundsHP",
				"gammaA",
				"gammaB",
				"optimizationCriterion",
				"sided",
				"tolerance",
				"alphaSpent",
				"userAlphaSpending",
				"betaSpent",
				"typeBetaSpending",
				"userBetaSpending",
				"criticalValues",
				"stageLevels"
			))
		}
	)
)

#' 
#' @name TrialDesignGroupSequential
#' 
#' @title
#' Group Sequential Design
#' 
#' @description
#' Trial design for group sequential design.
#' 
#' @details
#' This object should not be created directly; use \code{\link{getDesignGroupSequential}} 
#' with suitable arguments to create a group sequential design.
#' 
#' @seealso \code{\link{getDesignGroupSequential}} for creating a group sequential design.
#'
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignGroupSequential <- setRefClass(
	C_CLASS_NAME_TRIAL_DESIGN_GROUP_SEQUENTIAL,	
	contains = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL,
	methods = list(
		initialize = function(...) {
			callSuper(...)
			.parameterFormatFunctions$criticalValues <<- "formatGroupSequentialCriticalValues"
		},
		show = function(showType = 1) {
			'Method for automatically printing trial design objects'
			callSuper(showType)
		}
	)
)

#' 
#' @name TrialDesign_plot
#' 
#' @title
#' Trial Design Plotting
#' 
#' @description
#' Plots a trial design.
#' 
#' @details
#' Generic function to plot a trial design.
#' 
#' @param x The trial design, obtained from \cr
#'        \code{\link{getDesignGroupSequential}}, \cr
#'        \code{\link{getDesignInverseNormal}} or \cr
#'        \code{\link{getDesignFisher}}.
#' @param y Not available for this kind of plot (is only defined to be compatible to the generic plot function).
#' @param main The main title.
#' @param xlab The x-axis label.
#' @param ylab The y-axis label.
#' @param palette The palette, default is \code{"Set1"}.
#' @param theta A vector of theta values.
#' @param nMax The maximum sample size.
#' @param plotPointsEnabled If \code{TRUE}, additional points will be plotted.
#' @param legendPosition The position of the legend. 
#' By default (\code{NA_integer_}) the algorithm tries to find a suitable position. 
#' Choose one of the following values to specify the position manually:
#' \itemize{
#'   \item \code{-1}: no legend will be shown
#'   \item \code{NA}: the algorithm tries to find a suitable position
#'   \item \code{0}: legend position outside plot
#'   \item \code{1}: legend position left top
#'   \item \code{2}: legend position left center
#'   \item \code{3}: legend position left bottom
#'   \item \code{4}: legend position right top
#'   \item \code{5}: legend position right center
#'   \item \code{6}: legend position right bottom
#' }
#' @param type The plot type (default = \code{1}). The following plot types are available:
#' \itemize{
#'   \item \code{1}: creates a 'Boundary Plot'
#'   \item \code{2}: creates an 'Average Sample Size and Power / Early Stop' plot
#'   \item \code{3}: creates a 'Stage Levels Plot'
#'   \item \code{4}: creates a 'Power' plot
#'   \item \code{5}: creates a 'Stopping Probability' plot
#'   \item \code{6}: creates an 'Error Spending Plot'
#' }
#' @param ... Optional \code{ggplot2} arguments.
#' 
#' @details
#' Generic function to plot a trial design.
#' 
#' @seealso \code{\link{TrialDesignSet_plot}} to compare different designs or design parameters visual.
#'
#' @export
#' 
#' @examples 
#' 
#' design <- getDesignInverseNormal(kMax = 3, alpha = 0.025, 
#'     typeOfDesign = "asKD", gammaA = 2, 
#'     informationRates = c(0.2, 0.7, 1), 
#'     typeBetaSpending = "bsOF")
#' 
#' if (require(ggplot2)) {
#'     plot(design) # default: type = 1
#'     plot(design, type = 2, nMax = 20, theta = seq(0, 1, 0.05))
#'     plot(design, type = 3)
#'     plot(design, type = 4, nMax = 20, theta = seq(0, 1, 0.05))
#'     plot(design, type = 5, nMax = 20, theta = seq(0, 1, 0.05))
#'     plot(design, type = 6)
#' }
#' 
plot.TrialDesign = function(x, y, main = NA_character_,
		xlab = NA_character_, ylab = NA_character_, type = 1, palette = "Set1",
		theta = seq(-2, 2, 0.01), nMax = NA_integer_, plotPointsEnabled = NA, 
		legendPosition = NA_integer_, ...) {
		
	.assertGgplotIsInstalled()
	
	if (.isTrialDesignFisher(x)) {
		cat("Plot object of class ", methods::classLabel(class(x)), "\n")
		stop("Plot function for Fisher design is not implemented yet.\n")
	}
		
	designSet <- TrialDesignSet(design = x, singleDesign = TRUE)
	
	plot.TrialDesignSet(x = designSet, y = y, main = main, xlab = xlab, ylab = ylab, type = type,
		palette = palette, theta = theta, nMax = nMax, 
		plotPointsEnabled = plotPointsEnabled, legendPosition = legendPosition, ...)
}

#'
#' @name TrialDesign_summary
#' 
#' @title
#' Object Summary
#'
#' @description
#' Displays a summary of the trial design.
#' Applicable for group sequential design, inverse normal design and Fisher design.
#' 
#' @details
#' Summarizes the parameters and results of a design.
#' 
#' @export
#' 
#' @keywords internal
#' 
summary.TrialDesign <- function(object, ...) {
	cat("This output summarizes the", object$.toString(), "specification and output.\n\n")
	object$show()
	cat("\n")
	object$show(showType = 2)
	cat("\n")
	print.StageResults(object, ...) 
}



#'
#' @name TrialDesign_as.data.frame
#' 
#' @title
#' Coerce TrialDesign to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesign} as data frame.
#' 
#' @param niceColumnNamesEnabled logical. If \code{TRUE}, nice looking names will be used; 
#'        syntactic names otherwise (see \code{\link[base]{make.names}}).
#' @param includeAllParameters logical. If \code{TRUE}, all parameters will be included; 
#'        a meaningful parameter selection otherwise.
#'
#' @details 
#' Each element of the \code{TrialDesign} is converted to a column in the data frame. 
#' 
#' @export
#' 
#' @keywords internal
#' 
as.data.frame.TrialDesign <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = TRUE, includeAllParameters = FALSE, ...) {
		
	.assertIsTrialDesign(x)
	
	x$.initStages()
	if (includeAllParameters) {
		parameterNames <- NULL
	} else {
		parameterNames <- x$.getParametersToShow()
	}
	return(x$.getAsDataFrame(parameterNames = parameterNames, 
			niceColumnNamesEnabled = niceColumnNamesEnabled,
			includeAllParameters = includeAllParameters,
			tableColumnNames = .getTableColumnNames(design = x)))
}

