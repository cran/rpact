######################################################################################
#                                                                                    #
# -- Trial design set classes --                                                     #
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



#' @title
#' Get Design Set
#' 
#' @description  
#' Creates a trial design set object and returns it.    
#' 
#' @param ... 'designs' OR 'design' and one or more design parameters, e.g., deltaWT = c(0.1, 0.3, 0.4).
#' \itemize{
#'   \item \code{design} The master design (optional, you need to specify an 
#'         additional parameter that shall be varied).
#'   \item \code{designs} The designs to compare (optional).
#' }
#' 
#' @details 
#' Specify a master design and one or more design parameters or a list of designs.
#' 
#' @return Returns a \code{\link{TrialDesignSet}} object.
#'  
#' @examples
#' 
#' # Example 1
#'  design <- getDesignGroupSequential(alpha = 0.05, kMax = 6, 
#'     sided = 2, typeOfDesign = "WT", deltaWT = 0.1)
#'  designSet <- getDesignSet()
#'  designSet$add(design = design, deltaWT = c(0.3, 0.4))
#'  if (require(ggplot2)) plot(designSet, type = 1)
#' 
#' # Example 2 (shorter script)
#' design <- getDesignGroupSequential(alpha = 0.05, kMax = 6, 
#'     sided = 2, typeOfDesign = "WT", deltaWT = 0.1)
#' designSet <- getDesignSet(design = design, deltaWT = c(0.3, 0.4))
#' if (require(ggplot2)) plot(designSet)
#'
#' @export
#' 
getDesignSet <- function(...) {
	return(TrialDesignSet(...))
}

#' 
#' @name TrialDesignSet
#' 
#' @title
#' Class for trial design sets.
#' 
#' @description 
#' \code{TrialDesignSet} is a class for creating a collection of different trial designs.
#' 
#' @field designs The designs (optional).
#' @field design The master design (optional).
#' 
#' @details
#' This object can not be created directly; better use \code{\link{getDesignSet}} 
#' with suitable arguments to create a set of designs.
#' 
#' @seealso \code{\link{getDesignSet}}
#' 
#' @name TrialDesignSet_initialize
#' Initializes the object.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
TrialDesignSet <- setRefClass("TrialDesignSet",
	contains = "FieldSet",
	fields = list(
		.plotSettings = "PlotSettings",
		designs = "list",
		variedParameters = "character"
	),
	methods = list(
		# 
		# @param ... 'designs' OR 'design' and one or more design parameters, e.g., deltaWT = c(0.1, 0.3, 0.4)
		# 
		initialize = function(...) {
			.plotSettings <<- PlotSettings()
			designs <<- list()
			variedParameters <<- character(0)
			if (length(list(...)) > 0) {
				add(...)
			}
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},
		
		show = function(showType = 1) {
			'Method for automatically printing trial design sets'
			cat("Trial design set with", length(designs), "designs\n\n")
			for (design in designs) {
				design$show(showType = showType)
			}
		},
		
		isEmpty = function() {
			return(length(designs) == 0)
		},
		
		getSize = function() {
			return(length(designs))
		},
		
		getDesignMaster = function() {
			if (length(designs) == 0) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "no design master defined")
			}
			
			return(designs[[1]])
		},
		
		.validateDesignsArgument = function(designsToAdd, args) {
			if (!is.list(designsToAdd)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'designsToAdd' must be a list")
			}
			
			if (length(designsToAdd) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'designsToAdd' must be not empty")
			}
			
			for (d in designsToAdd) {
				if (!.isTrialDesign(d)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'designsToAdd' must be a list of trial designs (found '", class(d), "')")
				}
			}
			
			varPar <- args[["variedParameters"]]
			if (!is.null(varPar) && length(varPar) > 0) {
				variedParameters <<- c(variedParameters, varPar)
			}
			
			args <- args[names(args) != "designs" && names(args) != "variedParameters"]
			if (length(args) > 0) {
				warning("Argument", ifelse(length(args) > 1, "s", ""), " ", 
					.arrayToString(args, encapsulate = TRUE), " will be ignored ", 
					"because for 'designs' only argument 'variedParameters' will be respected", call. = FALSE)
			}
			
			designs <<- c(designs, designsToAdd)
		},
		
		addVariedParameters = function(varPar) {
			if (is.null(varPar) || !is.character(varPar)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'varPar' must be a valid character vector")
			}
			
			variedParameters <<- c(variedParameters, varPar)
		},
		
		.validateOptionalArguments = function(...) {
			args <- list(...)
			designsToAdd <- .getOptionalArgument(optionalArgumentName = "designs", ...)
			if (!is.null(designsToAdd)) {
				.validateDesignsArgument(designsToAdd = designsToAdd, args = args)
				return(NULL)
			}
			
			design <- .getOptionalArgument(optionalArgumentName = "design", ...)
			optionalArgumentsDefined = (length(args) > 0)
			if (is.null(design) && !optionalArgumentsDefined) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"please specify a 'design' to add and/or a design parameter, ", 
					"e.g., deltaWT = c(0.1, 0.3, 0.4)")
			}
			
			if (is.null(design) && optionalArgumentsDefined && length(designs) == 0) {
				stop(C_EXCEPTION_TYPE_INCOMPLETE_ARGUMENTS, 
						"at least one design (master) must be defined in this ", 
						"design set to respect any design parameters")
			}
			
			if (!is.null(design)) {
				designs <<- c(designs, design)
			} 
			else if (length(designs) > 0) {
				design <- designs[[1]] # use design master
			}
			
			if (!.isTrialDesign(design)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'design' (", class(design), ") must be an instance of class 'TrialDesign'")
			}
			
			.getArgumentNames(validatedDesign = design, ...)

			invisible(design)
		},
		
		.getArgumentNames = function(validatedDesign, ...) {
			args <- list(...)
			if (length(args) == 0) {
				return(character(0))
			}
			
			argumentNames <- names(args)
			if (length(argumentNames) == 0) {
				warning("No argument names available for ", paste(args, collapse = ", "), call. = FALSE)
				return(character(0))
			}

			argumentNames <- argumentNames[nchar(argumentNames) != 0]
			argumentNames <- argumentNames[!(argumentNames %in% c("design", "designs", "singleDesign"))]
			
			visibleFieldNames <- validatedDesign$.getVisibleFieldNames()
			for (arg in argumentNames) {
				if (!(arg %in% visibleFieldNames)) {
					stop(sprintf(paste0(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
							"'%s' does not contain a field with name '%s'"), class(validatedDesign), arg))
				}
			}
			
			invisible(argumentNames)
		},
		
		add = function(...) {
			"Adds 'designs' OR a 'design' and/or a design parameter, e.g., deltaWT = c(0.1, 0.3, 0.4)"
			design <- .validateOptionalArguments(...)

			args <- list(...)
			singleDesign <- args[["singleDesign"]]
			if (!is.null(singleDesign) && is.logical(singleDesign) && singleDesign) {
				return()
			}
			
			if (!is.null(design)) {	
				d <- .createDesignVariants(validatedDesign = design, ...)
				designs <<- c(designs, d)
			}
		},
		
		assertHaveEqualSidedValues = function() {
			if (length(designs) == 0) {
				return()
			}
			
			sided = getDesignMaster()$sided
			for (design in designs) {
				if (sided != design$sided) {
					stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
						"designs have different directions of alternative (design master is ",
						ifelse(sided == 1, "one", "two"), " sided)")
				}
			}
		},
		
		.createDesignVariants = function(validatedDesign, ...) {
			.assertIsTrialDesign(validatedDesign)
			argumentNames <- .getArgumentNames(validatedDesign = validatedDesign, ...)
	
			if (length(argumentNames) == 0) {
				warning("Creation of design variants stopped: no valid design parameters found", call. = FALSE)
				return(list())
			}
			
			if (length(argumentNames) > 2) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"too many arguments (", .arrayToString(argumentNames, encapsulate = TRUE), 
					"): up to 2 design parameters are allowed")
			}

			designVariants <- .createDesignVariantsRecursive(designMaster = validatedDesign, 
				args = list(...), argumentIndex = 1, argumentNames = argumentNames)

			return(designVariants)
		},
		
		.designSettingExists = function(parameterName, parameterValue, numberOfArguments = 1,
				parameterNameBefore = NULL, parameterValueBefore = NULL) {
				
			if (length(designs) == 0) {
				return(FALSE)
			}
			
			for (design in designs) {
				if (!is.null(parameterNameBefore) && !is.null(parameterValueBefore)) {
					if (design[[parameterNameBefore]] == parameterValueBefore &&
							design[[parameterName]] == parameterValue) {
						return(TRUE)
					}
				} else if (numberOfArguments == 1) {
					if (design[[parameterName]] == parameterValue) {
						return(TRUE)
					}
				}
			}
			return(FALSE)
		},
		
		.createDesignVariantsRecursive = function(designMaster, args, argumentIndex, argumentNames,
				parameterNameBefore = NULL, parameterValueBefore = NULL) {
			if (argumentIndex > length(argumentNames)) {
				return(list())
			}
			
			designVariants <- list()
			argumentName <- argumentNames[argumentIndex]
			variedParameters <<- unique(c(variedParameters, argumentName))
			argumentValues <- args[[argumentName]]
			
			for (argumentValue in argumentValues) {
				if (.designSettingExists(argumentName, argumentValue,
						numberOfArguments = length(argumentNames),
						parameterNameBefore, parameterValueBefore)) {
					if (!is.null(parameterNameBefore) && !is.null(parameterValueBefore)) {
						warning(sprintf("Argument ignored: there exists already a design with %s = %s (%s = %s)", 
								argumentName, argumentValue, parameterNameBefore, parameterValueBefore), call. = FALSE)
					} else {
						warning(sprintf("Argument ignored: there exists already a design with %s = %s", 
								argumentName, argumentValue), call. = FALSE)
					}
				} else {	
					designMaster2 <- .createDesignVariant(designMaster = designMaster, 
						argumentName = argumentName, argumentValue = argumentValue)
					if (argumentIndex == length(argumentNames)) {
						if (is.null(parameterNameBefore) || is.null(parameterValueBefore)) {
							.logDebug("Create design variant %s = %s", argumentName, argumentValue)
						} else {
							.logDebug("Create design variant %s = %s (%s = %s)", argumentName, argumentValue,
								parameterNameBefore, parameterValueBefore)
						}
						designVariants <- c(designVariants, designMaster2)
					}
					designCopies2 <- .createDesignVariantsRecursive(designMaster = designMaster2, 
						args = args, argumentIndex = argumentIndex + 1, argumentNames = argumentNames,
						parameterNameBefore = argumentName, parameterValueBefore = argumentValue)
					if (length(designCopies2) > 0) {
						designVariants <- c(designVariants, designCopies2)
					}
				}
			}

			return(designVariants)
		},
		
		.createDesignVariant = function(designMaster, argumentName, argumentValue) {
			if (.isTrialDesignGroupSequential(designMaster)) {
				defaultValues <- .getDesignGroupSequentialDefaultValues()
			}
			else if (.isTrialDesignInverseNormal(designMaster)) {
				defaultValues <- .getDesignInverseNormalDefaultValues()
			}
			else if (.isTrialDesignFisher(designMaster)) {
				defaultValues <- .getDesignFisherDefaultValues()
			}
			
			for (userDefinedParamName in designMaster$.getUserDefinedParameters()) {
				defaultValues[[userDefinedParamName]] <- designMaster[[userDefinedParamName]]
			}
			defaultValues[[argumentName]] <- argumentValue
			
			if (.isTrialDesignGroupSequential(designMaster)) {
				return(getDesignGroupSequential(  
					kMax = defaultValues$kMax, 
					alpha = defaultValues$alpha, 
					beta = defaultValues$beta, 
					sided = defaultValues$sided, 
					informationRates = defaultValues$informationRates, 
					futilityBounds = defaultValues$futilityBounds, 		
					typeOfDesign = defaultValues$typeOfDesign, 
					deltaWT = defaultValues$deltaWT, 
					optimizationCriterion = defaultValues$optimizationCriterion, 
					gammaA = defaultValues$gammaA, 
					typeBetaSpending = defaultValues$typeBetaSpending, 
					userAlphaSpending = defaultValues$userAlphaSpending, 
					userBetaSpending = defaultValues$userBetaSpending, 
					gammaB = defaultValues$gammaB, 
					tolerance = defaultValues$tolerance))
			} 
			
			else if (.isTrialDesignInverseNormal(designMaster)) {
				return(getDesignInverseNormal(
					kMax = defaultValues$kMax, 
					alpha = defaultValues$alpha, 
					beta = defaultValues$beta, 
					sided = defaultValues$sided, 
					informationRates = defaultValues$informationRates, 
					futilityBounds = defaultValues$futilityBounds, 		
					typeOfDesign = defaultValues$typeOfDesign, 
					deltaWT = defaultValues$deltaWT, 
					optimizationCriterion = defaultValues$optimizationCriterion, 
					gammaA = defaultValues$gammaA, 
					typeBetaSpending = defaultValues$typeBetaSpending, 
					userAlphaSpending = defaultValues$userAlphaSpending, 
					userBetaSpending = defaultValues$userBetaSpending, 
					gammaB = defaultValues$gammaB, 
					tolerance = defaultValues$tolerance))
			}
			
			else if (.isTrialDesignFisher(designMaster)) {
				return(getDesignFisher(
					kMax = defaultValues$kMax, 
					alpha = defaultValues$alpha, 
					method = defaultValues$method, 
					userAlphaSpending = defaultValues$userAlphaSpending, 
					informationRates = defaultValues$informationRates, 
					alpha0Vec = defaultValues$alpha0Vec, 
					sided = defaultValues$sided,
					tolerance = defaultValues$tolerance,
					iterations = defaultValues$iterations,
					seed = defaultValues$seed))
			}		
		}
	)
)

#' 
#' @title
#' Access Trial Design by Index
#'
#' @description
#' Function to the \code{TrialDesign} at position \code{i} in a \code{TrialDesignSet} object.
#' 
#' @details
#' Can be used to iterate with "[index]"-syntax over all designs in a design set.
#'
#' @export
#' 
#' @keywords internal
#' 
setMethod("[", "TrialDesignSet",
	function(x, i, j = NA_character_) {
		if (length(x$designs) == 0) {
			return(NULL)
		}
		
		design <- x$designs[[i]]
		if (!is.na(j) && is.character(j)) {
			return(design[[j]])
		}
		
		return(design)
	}
)

#'
#' @name TrialDesignSet_names
#' 
#' @title
#' The Names of a Trial Design Set object
#'
#' @description
#' Function to get the names of a \code{TrialDesignSet} object.
#' 
#' @details
#' Returns the names of a design set that can be accessed by the user.
#'
#' @export
#' 
#' @keywords internal
#' 
names.TrialDesignSet <- function(x) {
	return(x$.getVisibleFieldNames())
}

#' 
#' @name TrialDesignSet_length
#' 
#' @title
#' Length of Trial Design Set
#'
#' @description
#' Returns the number of designs in a \code{TrialDesignSet}.
#' 
#' @details
#' Is helpful for iteration over all designs in a design set with "[index]"-syntax.
#'
#' @export
#' 
#' @keywords internal
#' 
length.TrialDesignSet <- function(x) {
	return(length(x$designs))
}

#'
#' @name TrialDesignSet_print
#' 
#' @title
#' Print Trial Design Set Values
#'
#' @description
#' \code{print} prints its \code{TrialDesignSet} argument and returns it invisibly (via \code{invisible(x)}). 
#' 
#' @details
#' Prints the design set.
#'
#' @export
#' 
#' @keywords internal
#' 
print.TrialDesignSet <- function(x, ...) {
	x$show()
	invisible(x)
}

#'
#' @name TrialDesignSet_as.data.frame
#' 
#' @title
#' Coerce Trial Design Set to a Data Frame
#'
#' @description
#' Returns the \code{TrialDesignSet} as data frame.
#' 
#' @details
#' Coerces the design set to a data frame.
#' 
#' @export
#' 
#' @keywords internal
#' 
as.data.frame.TrialDesignSet <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = TRUE, includeAllParameters = FALSE, 
		addPowerAndAverageSampleNumber = FALSE, theta = seq(-1, 1, 0.02), nMax = NA_integer_, ...) {
		
	if (class(x) != "TrialDesignSet") {
		stop("'x' must be an instance of class 'TrialDesignSet' (is '", class(x), "')")
	}
	
	if (x$isEmpty()) {
		stop("The design set is empty")
	}
	
	fCall = match.call(expand.dots = FALSE)
	theta <- .assertIsValidThetaRange(thetaRange = theta, thetaAutoSeqEnabled = (as.character(fCall$theta)[1] != "seq"))
	
	if (addPowerAndAverageSampleNumber) {
		.assertAssociatedArgumentsAreDefined(
			addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber, 
			theta = theta, nMax = nMax)
	}
	
	fisherDesignEnabled <- .isTrialDesignFisher(x$getDesignMaster())
	dataFrame <- NULL
	for (design in x$designs) {
		if (fisherDesignEnabled != .isTrialDesignFisher(design)) {
			stop("All trial designs must be from the same type ", 
				"('", class(x$designs[[1]]), "' != '", class(design), ")'")
		}
		
		df <- as.data.frame(design, niceColumnNamesEnabled = niceColumnNamesEnabled,
			includeAllParameters = includeAllParameters)
		
		if (.isTrialDesignWithValidFutilityBounds(design)) {
			futilityBoundsName <- "futilityBounds"
			if (niceColumnNamesEnabled) {
				futilityBoundsName <- .getTableColumnNames(design = design)[["futilityBounds"]]
			}
			
			kMax <- design$kMax
			df[[futilityBoundsName]][kMax] <- design$criticalValues[kMax]
		}
		
		if (addPowerAndAverageSampleNumber) {
			results <- PowerAndAverageSampleNumberResult(design, theta = theta, nMax = nMax)
			df2 <- as.data.frame(results, niceColumnNamesEnabled = niceColumnNamesEnabled,
				includeAllParameters = includeAllParameters)
			df <- merge(df, df2, all.y = TRUE)
		}
		if (is.null(dataFrame)) {
			if (niceColumnNamesEnabled) {
				dataFrame <- cbind("Design number" = rep(1, nrow(df)), df)
			} else {
				dataFrame <- cbind(designNumber = rep(1, nrow(df)), df)
			}
		} else {
			if (niceColumnNamesEnabled) {
				df <- cbind("Design number" = rep(max(dataFrame$"Design number") + 1, nrow(df)), df)
			} else {
				df <- cbind(designNumber = rep(max(dataFrame$designNumber) + 1, nrow(df)), df)
			}
			dataFrame <- rbind(dataFrame, df)
		}
	}

	return(dataFrame)
}

#'
#' @name TrialDesignSet_plot
#' 
#' @title
#' Trial Design Set Plotting
#' 
#' @description
#' Plots a trial design set.
#' 
#' @param x The trial design set, obtained from \code{\link{getDesignSet}}.
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
#' Generic function to plot a trial design set.
#' Is, e.g., useful to compare different designs or design parameters visual.
#' 
#' @return 
#' Returns a \code{ggplot2} object.
#'  
#' @export
#' 
#' @examples 
#' 
#' design <- getDesignInverseNormal(kMax = 3, alpha = 0.025, 
#'     typeOfDesign = "asKD", gammaA = 2, 
#'     informationRates = c(0.2, 0.7, 1), typeBetaSpending = "bsOF")
#'  
#' # Create a set of designs based on the master design defined above 
#' # and varied parameter 'gammaA'
#' designSet <- getDesignSet(design = design, gammaA = 4)
#'   
#' if (require(ggplot2)) plot(designSet, type = 1, legendPosition = 6)
#'
plot.TrialDesignSet <- function(x, y, type = 1L, main = NA_character_, 
		xlab = NA_character_, ylab = NA_character_, palette = "Set1",
		theta = seq(-1, 1, 0.02), nMax = NA_integer_, plotPointsEnabled = NA, 
		legendPosition = NA_integer_, ...) {
		
	.assertGgplotIsInstalled()
	
	.assertIsValidLegendPosition(legendPosition)
	theta <- .assertIsValidThetaRange(thetaRange = theta)
	
	if (type == 1) {
		main <- ifelse(is.na(main), "Boundary Plot", main)
		xParameterName <- "informationRates"
		if (x$getDesignMaster()$sided == 1 && .isTrialDesignWithValidFutilityBounds(x$getDesignMaster())) {
			yParameterNames <- c("futilityBounds", "criticalValues")
		} else {
			yParameterNames <- c("criticalValues")
		}
	}
		
	else if (type == 2) {
		if (is.na(main)) {
			main <- bquote(atop(bold('Average Sample Size and Power / Early Stop'), 
							atop(bold('(N'['max']*'='*.(nMax)*")"))))
		}
		xParameterName <- "theta"
		yParameterNames <- c("averageSampleNumber", "overallEarlyStop", "calculatedPower")
	} 
		
	else if (type == 3) {
		main <- ifelse(is.na(main), "Stage Levels Plot", main)
		xParameterName <- "informationRates"
		yParameterNames <- "stageLevels"
	} 
	
	else if (type == 4) {
		#eval(expression(Power~( ~ N[max]~ nMax)))
		#p <- p + ggtitle(bquote(list(alpha==.(alpha), lambda==.(lambda), memory==.(mem))))
		#p <- p +  ggtitle(bquote(atop(bold(.(title)), atop(.(subtitle)))))
		#p <- p +  ggtitle(bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))
		if (is.na(main)) {
			main <- bquote(atop(bold('Power'), atop('(N'['max']*'='*.(nMax)*")")))
		}
		xParameterName <- "theta"
		yParameterNames <- "calculatedPower"
	} 
	
	else if (type == 5) {
		if (is.na(main)) {
			main <- bquote(atop(bold('Stopping Probability'), atop('(N'['max']*'='*.(nMax)*")")))
		}
		xParameterName <- "theta"
		yParameterNames <- "overallEarlyStop"
	} 
	
	else if (type == 6) {
		main <- ifelse(is.na(main), "Error Spending Plot", main)
		xParameterName <- "informationRates"
		yParameterNames <- c("alphaSpent")
		if (x$getDesignMaster()$typeBetaSpending != C_TYPE_OF_DESIGN_BS_NONE) {
			yParameterNames <- c(yParameterNames, "betaSpent")
			palette <- "Paired"
		}
		plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), FALSE, plotPointsEnabled)
	}
	
	else {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'type' (", type, ") is not allowed; must be 1, 2, ..., 6")
	}
	
	companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
	if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
		companyAnnotationEnabled <- FALSE
	}
	return(.plotTrialDesignSet(designSet = x, xParameterName = xParameterName,
		yParameterNames = yParameterNames, mainTitle = main, xlab = xlab, ylab = ylab,
		palette = palette, theta = theta, nMax = nMax, plotPointsEnabled = plotPointsEnabled,
		legendPosition = legendPosition,
		companyAnnotationEnabled = companyAnnotationEnabled))
}

.getDesignSetAsDataFrame <- function(designSet, 
		addPowerAndAverageSampleNumber = FALSE, theta = seq(-1, 1, 0.02), nMax = NA_integer_) {
		
	if (designSet$getSize() > 1 && (is.null(designSet$variedParameters) || 
			length(designSet$variedParameters) == 0)) {
		stop("'variedParameters' must be not empty; ",
			"use 'DesignSet$addVariedParameters(character)' to add one or more varied parameters")
	}
	
	data <- as.data.frame(designSet, niceColumnNamesEnabled = FALSE, 
		includeAllParameters = TRUE, addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber,
		theta = theta, nMax = nMax)	
	if (designSet$getSize() <= 1) {
		return(list(data = data, variedParameters = designSet$variedParameters))
	}
	
	variedParameters <- designSet$variedParameters
	if (nrow(data) > 1) {
		for (variedParameter in variedParameters) {
			column <- data[[variedParameter]]
			if (length(column) <= 1) {
				stop("Varied parameter '", variedParameter, "' hast length ", length(column))
			}
			
			valueBefore <- column[1]
			for (i in 2:length(column)) {
				if (is.na(column[i])) {
					column[i] <- valueBefore
				} else {
					valueBefore <- column[i]
				}
			}
			data[[variedParameter]] <- column
		}
	}
	variedParameterNames <- c()
	for (variedParameter in variedParameters) {
		variedParameterNames <- c(variedParameterNames, 
			.getTableColumnNames(design = designSet$getDesignMaster())[[variedParameter]])
	}
	names(variedParameters) <- variedParameterNames
	return(list(data = data, variedParameters = variedParameters))
}

.getCategories <- function(data, yParameterName, tableColumnNames) {
	if (sum(is.na(data$categories)) > 0) {
		return(rep(.getAxisLabel(yParameterName, tableColumnNames), nrow(data)))
	}
	
	return(paste(data$categories, .getAxisLabel(yParameterName, tableColumnNames), sep = ", "))
}

.getAxisLabel <- function(parameterName, tableColumnNames) {
	return(tableColumnNames[[parameterName]])
}

.plotTrialDesignSet <- function(designSet, xParameterName, yParameterNames,
		mainTitle = NA_character_, xlab = NA_character_, ylab = NA_character_, 
		palette = "Set1", theta = seq(-1, 1, 0.02), nMax = NA_integer_, 
		plotPointsEnabled = NA, legendPosition = NA_integer_, companyAnnotationEnabled = FALSE) {
		
	designSet$assertHaveEqualSidedValues()
	
	addPowerAndAverageSampleNumber <- xParameterName == "theta" && 
		yParameterNames[1] %in% c("averageSampleNumber", "calculatedPower", "overallEarlyStop",
			"overallRejectPerStage", "overallFutilityPerStage") 
	df <- .getDesignSetAsDataFrame(designSet, 
		addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber, theta = theta, nMax = nMax)
	data <- df$data	
	variedParameters <- df$variedParameters
	
	legendTitle <- paste(names(variedParameters), collapse = "\n")
	categoryParameterName <- variedParameters[1]
	
	yParameterName1 <- yParameterNames[1]
	yParameterName2 <- NULL
	yParameterName3 <- NULL
	if (length(yParameterNames) >= 2) {
		yParameterName2 <- yParameterNames[2]
	}
	if (length(yParameterNames) >= 3) {
		yParameterName3 <- yParameterNames[3]
	}
	
	tableColumnNames <- .getTableColumnNames(design = designSet$getDesignMaster())
	
	xAxisLabel <- .getAxisLabel(xParameterName, tableColumnNames)
	yAxisLabel1 <- .getAxisLabel(yParameterName1, tableColumnNames)
	yAxisLabel2 <- NULL
	if (!is.null(yParameterName2) && !is.null(yParameterName3)) {
		pn2 <- .getAxisLabel(yParameterName2, tableColumnNames)
		if (yParameterName2 == "overallEarlyStop") {
			pn2 <- "Stopping Probability"
		}
		yAxisLabel2 <- paste(pn2, .getAxisLabel(yParameterName3, tableColumnNames), sep = " and ")
	}
	else if (!is.null(yParameterName2)) {
		yAxisLabel1 <- paste(yAxisLabel1, .getAxisLabel(yParameterName2, tableColumnNames), sep = " and ")
	}
	if (yParameterName1 %in% c("alphaSpent", "betaSpent")) {
		yAxisLabel1 <- "Cumulative Error"
		if (is.null(yParameterName2)) {
			yAxisLabel1 <- paste0(yAxisLabel1, " (", .getAxisLabel(yParameterName1, tableColumnNames), ")")
		}
	}
	
	data$xValues <- data[[xParameterName]]
	data$yValues <- data[[yParameterName1]]
	if (is.null(yParameterName2)) {
		data$yValues2 <- rep(NA_real_, nrow(data))
	} else {
		data$yValues2 <- data[[yParameterName2]]
	}
	if (is.null(yParameterName3)) {
		data$yValues3 <- rep(NA_real_, nrow(data))
	} else {
		data$yValues3 <- data[[yParameterName3]]
	}
	
	if (!is.na(categoryParameterName)) {
		data$categories <- data[[categoryParameterName]]
		if (length(variedParameters) > 1) {
			data$categories <- paste0(variedParameters[1], " = ", data$categories, ", ", 
				variedParameters[2], " = ", data[[variedParameters[2]]])
		}
	} else {
		data$categories <- rep(NA_character_, nrow(data))
	}
	
	if (!is.na(nMax) && is.null(yParameterName3) && xParameterName == "informationRates") {
		xAxisLabel <- "Sample Size"
		data$xValues <- data$xValues * nMax
	}
		
	# add zero point to data
	if (yParameterName1 %in% c("alphaSpent", "betaSpent")) {
		data <- data[, c("xValues", "yValues", "yValues2", "categories")]
		uc <- unique(data$categories)
		data <- rbind(data.frame(
			xValues = rep(-0.00001, length(uc)),
			yValues = rep(0, length(uc)),
			yValues2 = rep(0, length(uc)),
			categories = uc
		), data)
	}

	scalingFactor1 <- 1
	scalingFactor2 <- 1
	if (!is.null(yParameterName2)) {
		if (!is.null(yParameterName3)) {
			m1 <- max(data$yValues)
			m2 <- max(data$yValues2, data$yValues3)
			if (m1 > m2) {
				scalingFactor1 <- 1
				scalingFactor2 <- m1
			} else if (m1 < m2) {
				scalingFactor1 <- m2
				scalingFactor2 <- 1
			}
		}
		df1 <- data.frame(
			xValues = data$xValues,
			yValues = data$yValues * scalingFactor1,
			categories = .getCategories(data, yParameterName1, tableColumnNames)
		)
		df2 <- data.frame(
			xValues = data$xValues,
			yValues = data$yValues2 * scalingFactor2,
			categories = .getCategories(data, yParameterName2, tableColumnNames)
		)
		if (!is.null(yParameterName3)) {
			df3 <- data.frame(
				xValues = data$xValues,
				yValues = data$yValues3 * scalingFactor2,
				categories = .getCategories(data, yParameterName3, tableColumnNames)
			)
			data <- rbind(df1, df2, df3)
		} else {
			data <- rbind(df1, df2)
		}
		
		# sort categories for pairwise printing of the legend
		unqiueValues <- unique(as.character(data$categories))
		data$categories <- factor(data$categories, levels = unqiueValues[order(unqiueValues)])
		
		if (yParameterName1 == "alphaSpent" && yParameterName2 == "betaSpent") {
			legendTitle <- paste(legendTitle, "Type of error", sep = "\n")
		}
	}
	
	numberOfCategories <- 1
	if (sum(is.na(data$categories)) < length(data$categories)) {
		numberOfCategories <- length(unique(as.character(data$categories)))
	}
	
	nRow <- nrow(data)
	data <- data[!(data$xValues == 0 & data$xValues == data$yValues), ]
	removedRows1 <- nRow - nrow(data)
	
	nRow <- nrow(data)
	data <- data[!is.na(data$yValues), ]
	removedRows2 <- nRow - nrow(data)
	
	if (C_LOG_LEVEL == C_LOG_LEVEL_WARN && (removedRows1 > 0 || removedRows2 > 0)) {
		warning(sprintf("Removed %s rows containing (0, 0)-points and %s rows containing missing values", 
				removedRows1, removedRows2), call. = FALSE)
	}
	
	categoryEnabled <- (sum(is.na(data$categories)) < length(data$categories))
	
	if (categoryEnabled) {
		data <- data[, c("xValues", "yValues", "categories")]
	} else {
		data <- data[, c("xValues", "yValues")]
	}
	
	if (categoryEnabled) {	
		p <- ggplot2::ggplot(data, ggplot2::aes(x = data$xValues, y = data$yValues, 
				colour = factor(data$categories)))
	} else {
		p <- ggplot2::ggplot(data, ggplot2::aes(x = data$xValues, y = data$yValues))
	}
			
	p <- designSet$getPlotSettings()$setTheme(p)
	p <- designSet$getPlotSettings()$hideGridLines(p)
	
	# set main title
	p <- designSet$getPlotSettings()$setMainTitle(p, mainTitle)
	
	# set legend
	if (!is.na(legendPosition) && legendPosition == -1) {
		p <- p + ggplot2::theme(legend.position = "none")
	} else if (categoryEnabled) {
		if (is.na(legendPosition)) {
			legendPosition <- .getLegendPosition(designSet, data, yParameterName1, 
				yParameterName2, addPowerAndAverageSampleNumber)
		}
		p <- designSet$getPlotSettings()$setLegendPosition(p, legendPosition = legendPosition)
		p <- designSet$getPlotSettings()$setLegendBorder(p)
		p <- designSet$getPlotSettings()$setLegendTitle(p, legendTitle)
		p <- designSet$getPlotSettings()$setLegendLabelSize(p)
	}
	
	xAxisLabel <- .toCapitalized(xAxisLabel)
	yAxisLabel1 <- .toCapitalized(yAxisLabel1)
	yAxisLabel2 <- .toCapitalized(yAxisLabel2)
	
	p <- designSet$getPlotSettings()$setAxesLabels(p, 
		xAxisLabel = xAxisLabel, yAxisLabel1 = yAxisLabel1, yAxisLabel2 = yAxisLabel2, 
		xlab = xlab, ylab = ylab, scalingFactor1 = scalingFactor1, scalingFactor2 = scalingFactor2)
	
	# plot lines and points
	plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), !addPowerAndAverageSampleNumber, plotPointsEnabled) 
	pointBorder <- 4
	if (numberOfCategories > 8) {
		pointBorder <- 1
	}
	else if (numberOfCategories > 6) {
		pointBorder <- 2
	}
	else if (numberOfCategories > 4) {
		pointBorder <- 3
	}
	p <- designSet$getPlotSettings()$plotValues(p, plotPointsEnabled = plotPointsEnabled, pointBorder = pointBorder)
	
	if (yParameterName1 == "criticalValues" && designSet$getDesignMaster()$sided == 2) {
		p <- designSet$getPlotSettings()$mirrorYValues(p, yValues = data$yValues, 
			plotPointsEnabled = !addPowerAndAverageSampleNumber, pointBorder = pointBorder)
	}
	
	if (yParameterName1 == "criticalValues" || 
			(yParameterName1 == "futilityBounds" && yParameterName2 == "criticalValues")) {
		p <- .addQnormAlphaLine(p, designSet, data)
	}
	
	p <- designSet$getPlotSettings()$setAxesAppearance(p)
	p <- designSet$getPlotSettings()$setColorPalette(p, palette)
	p <- designSet$getPlotSettings()$enlargeAxisTicks(p)
	
	p <- designSet$getPlotSettings()$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)
	
	# start plot generation
	return(p)
}

.getLegendPosition <- function(designSet, data, yParameterName1, 
		yParameterName2, addPowerAndAverageSampleNumber) {
		
	if (length(unique(data$categories)) > 6) {
		designSet$getPlotSettings()$adjustPointSize(-0.5)
		designSet$getPlotSettings()$adjustLegendFontSize(-2)
		return(C_POSITION_OUTSIDE_PLOT)
	}
	
	if (.isTrialDesignWithValidFutilityBounds(designSet$getDesignMaster()) &&
		yParameterName1 == "futilityBounds" && yParameterName2 == "criticalValues") {
		return(C_POSITION_RIGHT_BOTTOM)
	}

	if (yParameterName1 == "criticalValues") {
		return(C_POSITION_RIGHT_TOP)
	} 
	
	if (yParameterName1 %in% c("stageLevels", "alphaSpent", "betaSpent")) {
		return(C_POSITION_LEFT_TOP)
	}
	
	if (addPowerAndAverageSampleNumber) {
		return(C_POSITION_LEFT_CENTER)
	}
	
	return(C_POSITION_OUTSIDE_PLOT)
} 

.addQnormAlphaLine <- function(p, designSet, data, annotationEnabled = TRUE) {
	alpha <- designSet$getDesignMaster()$alpha
	if (designSet$getDesignMaster()$sided == 2) {
		alpha <- alpha / 2
	}
	yValue <- stats::qnorm(1 - alpha)
	yValueLabel <- paste0("qnorm(1 - ", alpha, " ) == ", round(yValue, 4))
	if (designSet$getDesignMaster()$sided == 1) {
		p <- p + ggplot2::geom_hline(yintercept = yValue, linetype = "dashed") 
	} else {
		p <- p + ggplot2::geom_hline(yintercept = yValue, linetype = "dashed")
		p <- p + ggplot2::geom_hline(yintercept = -yValue, linetype = "dashed")
	}
	if (annotationEnabled) {
		p <- p + ggplot2::annotate("label", x = -Inf, hjust = -0.1, y = yValue, 
			label = yValueLabel, size = 2.5, parse = TRUE, colour = "white", fill = "white")
		p <- p + ggplot2::annotate("text", x = -Inf, hjust = -0.15, y = yValue, 
			label = yValueLabel, size = 2.5, parse = TRUE)
	}
	
	# expand y-axis range
	if (designSet$getDesignMaster()$sided == 1) {
		
		yMax <- max(stats::na.omit(data$yValues))
		if (!is.null(data$yValues2) && length(data$yValues2) > 0) {
			yMax <- max(yMax, stats::na.omit(data$yValues2))
		}
		eps <- (yMax - yValue) * 0.15
		
		p <- designSet$getPlotSettings()$expandAxesRange(p, y = yValue - eps)
	}
	
	return(p)
}



