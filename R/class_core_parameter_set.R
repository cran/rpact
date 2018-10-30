######################################################################################
#                                                                                    #
# -- Parameter set classes --                                                        #
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

#' @include f_core_constants.R
NULL

#' 
#' @name FieldSet
#' 
#' @title
#' Field Set
#' 
#' @description 
#' Basic class for field sets.
#' 
#' @name FieldSet_initialize
#' Initializes the object.
#' 
#' @details
#' The field set implements basic functions for a set of fields.
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
FieldSet <- setRefClass("FieldSet",
	methods = list(
		.getFieldNames = function() {
			return(names(.self$getRefClass()$fields()))
		},
		.getVisibleFieldNames = function() {
			fieldNames <- names(.self$getRefClass()$fields())
			fieldNames <- fieldNames[!startsWith(fieldNames, ".")]
			return(fieldNames)
		},
		.getFields = function(values) {
			flds = names(.self$getRefClass()$fields())
			if (!missing(values)) {
				flds = flds[flds %in% values]
			}
			result = setNames(vector("list", length(flds)), flds)
			for (fld in flds) {
				result[[fld]] = .self[[fld]]
			}
			return(result)
		}
	)
)

#' 
#' @name ParameterSet
#' 
#' @title
#' Parameter Set
#' 
#' @description 
#' Basic class for parameter sets.
#' 
#' @name ParameterSet_initialize
#' Initializes the object.
#' 
#' @details
#' The parameter set implements basic functions for a set of parameters.
#' 
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
ParameterSet <- setRefClass("ParameterSet",
	contains = "FieldSet",
	fields = list(
		.parameterTypes = "list",
		.parameterNames = "list",
		.parameterFormatFunctions = "list",
		.showParameterTypeEnabled = "logical"
	),
	methods = list(
		initialize = function(...,
			.showParameterTypeEnabled = TRUE) {
			callSuper(..., 
				.showParameterTypeEnabled = .showParameterTypeEnabled)
			.parameterTypes <<- list()
			.parameterNames <<- list()
			.parameterFormatFunctions <<- list()
		},
		.initParameterTypes = function() {
			for (parameterName in names(.parameterNames)) {
				.parameterTypes[[parameterName]] <<- C_PARAM_TYPE_UNKNOWN
			}
		},
		.getParameterType = function(parameterName) {
			if (is.null(parameterName) || is.na(parameterName) || length(parameterName) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'parameterName' must be a valid character with length > 0")
			}
			
			parameterType <- .parameterTypes[[parameterName]]
			if (is.null(parameterType)) {
				return(C_PARAM_TYPE_UNKNOWN)
			}
			
			return(parameterType[1])
		},
		.getParametersToShow = function() {
			return(.getVisibleFieldNames())
		},
		.setParameterType = function(parameterName, parameterType) {
			if (is.null(parameterName) || is.na(parameterName) || length(parameterName) == 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'parameterName' must be a valid character with length > 0")
			}
			
			parameterType <- parameterType[1]
			
			if (!all(parameterType %in% c(C_PARAM_USER_DEFINED, C_PARAM_DEFAULT_VALUE, 
						C_PARAM_GENERATED, C_PARAM_DERIVED, C_PARAM_NOT_APPLICABLE))) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'parameterType' ('", parameterType, "') is invalid")
			}
			
			.parameterTypes[[parameterName]] <<- parameterType
			
			invisible(parameterType)
		},
		isUserDefinedParameter = function(parameterName) {
			return(.getParameterType(parameterName) == C_PARAM_USER_DEFINED)
		},
		isDefaultParameter = function(parameterName) {
			return(.getParameterType(parameterName) == C_PARAM_DEFAULT_VALUE)
		},
		isGeneratedParameter = function(parameterName) {
			return(.getParameterType(parameterName) == C_PARAM_GENERATED)
		},
		isDerivedParameter = function(parameterName) {
			return(.getParameterType(parameterName) == C_PARAM_DERIVED)
		},
		isUndefinedParameter = function(parameterName) {
			return(.getParameterType(parameterName) == C_PARAM_TYPE_UNKNOWN)
		},
		.getUserDefinedParameters = function() {
			return(.getParametersOfOneGroup(C_PARAM_USER_DEFINED))
		},
		.getDefaultParameters = function() {
			return(.getParametersOfOneGroup(C_PARAM_DEFAULT_VALUE))
		},
		.getGeneratedParameters = function() {
			return(.getParametersOfOneGroup(C_PARAM_GENERATED))
		},
		.getDerivedParameters = function() {
			return(.getParametersOfOneGroup(C_PARAM_DERIVED))
		},
		.getUndefinedParameters = function() {
			return(.getParametersOfOneGroup(C_PARAM_TYPE_UNKNOWN))
		},
		.getParametersOfOneGroup = function(parameterType) {
			parameterNames <- names(.parameterTypes[.parameterTypes == parameterType])
			parametersToShow <- .getParametersToShow()
			if (is.null(parametersToShow) || length(parametersToShow) == 0) {
				return(parameterNames)
			}
			
			return(parametersToShow[parametersToShow %in% parameterNames])
		},
		.showParameterType = function(parameterName) {
			if (!.showParameterTypeEnabled) {
				return("  ")
			}
			
			return(paste0("[", .getParameterType(parameterName), "]"))
		},
		.isMatrix = function(param) {
			if (missing(param) || is.null(param) || is.list(param)) {
				return(FALSE)
			}
			
			return(is.matrix(param))
		},
		.isVector = function(param) {
			if (missing(param) || is.null(param) || is.list(param)) {
				return(FALSE)
			}
			
			return(length(param) > 1)
		},
		.showAllParameters = function() {
			parametersToShow <- .getVisibleFieldNamesOrdered()
			for (parameter in parametersToShow) {
				.showParameter(parameter, showParameterType = TRUE)
			}
		},
		.getVisibleFieldNamesOrdered = function() {
			visibleFieldNames <- .getVisibleFieldNames()
			
			parametersToShowSorted <- .getParametersToShow()
			if (is.null(parametersToShowSorted) || length(parametersToShowSorted) == 0) {
				return(visibleFieldNames)
			}
			
			visibleFieldNames <- visibleFieldNames[!(visibleFieldNames %in% parametersToShowSorted)]
			visibleFieldNames <- c(parametersToShowSorted, visibleFieldNames)
			return(visibleFieldNames)
		},
		.getOrderedParameters = function(parametersToOrder) {
			
		},
		.showParametersOfOneGroup = function(parameters, title, orderByParameterName = TRUE) {
			output <- ""
			if (is.na(parameters) || length(parameters) == 0) {
				if (!missing(title) && !is.null(title)) {
					output <- paste0(title, ": not available\n\n")
					cat(output)
				}
				invisible(output)
			} else {
				if (orderByParameterName) {
					parameters <- sort(parameters)
				}
				
				if (!missing(title) && !is.null(title)) {
					output <- paste0(title, ":\n")
					cat(output)
				}
				for (parameterName in parameters) {
					output <- paste0(output, .showParameter(parameterName))
				}			
				cat("\n")
				output <- paste0(output, "\n")
				invisible(output)				
			}
		},
		.showParameter = function(parameterName, showParameterType = FALSE) {
			param <- .getParameterValueFormatted(parameterName = parameterName)
			output <- ""
			if (!is.null(param)) {
				if (param$isMatrix) {
					for (i in 1:length(param$paramValueFormatted)) {
						output <- paste0(output, .showParameterFormatted(paramName = param$paramName, 
							paramValue = param$paramValue[i, ], 
							paramValueFormatted = param$paramValueFormatted[[i]], 
							showParameterType = showParameterType,
							matrixRow = i))
					}
				} else {
					output <- .showParameterFormatted(paramName = param$paramName, paramValue = param$paramValue, 
						paramValueFormatted = param$paramValueFormatted, showParameterType = showParameterType)
				}
			}
			invisible(output)
		},
		.getParameterValueFormatted = function(parameterName) {
			tryCatch({					
				d <- regexpr("\\.design\\$", parameterName)
				if (d[1] == 1) {
					parameterName <- substr(parameterName, attr(d, "match.length") + 1, nchar(parameterName))	
					paramValue <- get(".design")[[parameterName]]
				} else {
					paramValue <- get(parameterName)
				}

				if (isS4(paramValue)) {
					return(NULL)
				}
				
				paramValueFormatted = paramValue
				
				formatFunctionName <- .parameterFormatFunctions[[parameterName]]
				if (!is.null(formatFunctionName)) {
					paramValueFormatted <- eval(call(formatFunctionName, paramValueFormatted))
				}
				
				isMatrix <- FALSE
				if (.isMatrix(paramValue)) {
					matrixFormatted <- paramValueFormatted
					paramValueFormatted <- .arrayToString(matrixFormatted[1, ])
					if (nrow(matrixFormatted) > 1 && ncol(matrixFormatted) > 0) {
						isMatrix <- TRUE
						paramValueFormatted <- list(paramValueFormatted)
						for (i in 2:nrow(matrixFormatted)) {
							paramValueFormatted <- c(paramValueFormatted, .arrayToString(matrixFormatted[i, ]))
						}
					}
				}
				
				else if (.isVector(paramValue)) {
					paramValueFormatted <- .arrayToString(paramValueFormatted)
				}
				
				else if (parameterName == "sided") {
					paramValueFormatted <- ifelse(paramValue == 1, "one-sided", "two-sided")
				}
				
				return(list(
					paramName = parameterName,
					paramValue = paramValue,
					paramValueFormatted = paramValueFormatted,
					isMatrix = isMatrix
				))
			}, error = function(e) {
				.logError(paste0("Error in '.getParameterValueFormatted'. ",
					"Failed to show parameter '%s' (class '%s'): %s"), parameterName, class(.self), e)
			})
		
			return(NULL)
		},
		.showUnknownParameters = function() {
			params <- .getUndefinedParameters()
			if (length(params) > 0) {
				.showParametersOfOneGroup(params, "ISSUES (parameters with undefined type)")
			}
		},
		.showParameterFormatted = function(paramName, paramValue, paramValueFormatted = NA_character_,
				showParameterType = FALSE, matrixRow = NA_real_) {
			paramCaption <- .parameterNames[[paramName]]
			if (is.null(paramCaption)) {
				paramCaption <- paste0("%", paramName, "%")
			}
			if (!is.na(matrixRow)) {
				paramCaption <- paste0(paramCaption, " [", matrixRow, "]")
			}
			if (is.null(paramValueFormatted) || length(paramValueFormatted) == 0 || is.na(paramValueFormatted)) {
				paramValueFormatted <- paramValue
			}
			if (is.list(paramValueFormatted)) {
				paramValueFormatted <- .listToString(paramValueFormatted)
			}
			prefix <- ifelse(showParameterType, .showParameterType(paramName), "")
			variableNameFormatted <- formatVariableName(name = paramCaption, n = .getNChar(), prefix = prefix)
			output <- paste(variableNameFormatted, paramValueFormatted, "\n")
			cat(output)
			invisible(output)
		},
		.getNChar = function() {
			if (length(.parameterNames) == 0) {
				return(40)
			}
			
			return(min(40, max(nchar(.parameterNames))) + 4)
		},
		.showParameterTypeDescription = function() {
			cat("\n")
			cat("Legend:\n")
			cat("  ", C_PARAM_USER_DEFINED, ": user defined\n")
			cat("  ", C_PARAM_DERIVED, ": derived value\n")
			cat("  ", C_PARAM_DEFAULT_VALUE, ": default value\n")
			cat("  ", C_PARAM_GENERATED, ": generated/calculated value\n")
			cat("  ", C_PARAM_NOT_APPLICABLE, ": not applicable for specified values\n")
		},
		.printAsDataFrame = function(parameterNames, niceColumnNamesEnabled = TRUE,
				includeAllParameters = FALSE, handleParameterNamesAsToBeExcluded = FALSE,
				lineBreakEnabled = FALSE) {
			
			if (.isTrialDesign(.self)) {
				tableColumnNames <- .getTableColumnNames(design = .self)	
			} else {
				tableColumnNames <- C_TABLE_COLUMN_NAMES
			}
			
			if (.isTrialDesignPlan(.self)) {
				parameterNames <- NULL
			}
					
			dataFrame <- .getAsDataFrame(parameterNames, niceColumnNamesEnabled = niceColumnNamesEnabled,
				includeAllParameters = includeAllParameters, 
				handleParameterNamesAsToBeExcluded = handleParameterNamesAsToBeExcluded,
				returnParametersAsCharacter = TRUE, tableColumnNames = tableColumnNames)
			result <- as.matrix(dataFrame)
			if (.isTrialDesignPlan(.self)) {
				dimnames(result)[[1]] <- paste("  ", c(1:nrow(dataFrame)))
			} else {
				dimnames(result)[[1]] <- paste("  Stage", c(1:nrow(dataFrame)))
			}
			
			
			print(result, quote=FALSE, right=FALSE)
		},
		.getNumberOfRows = function(parameterNames) {
			numberOfRows <- 1
			for (parameterName in parameterNames) {
				parameterValues <- .self[[parameterName]]
				if (is.vector(parameterValues) && length(parameterValues) > numberOfRows) {
					numberOfRows <- length(parameterValues)
				}
			}
			return(numberOfRows)
		},
		.getAsDataFrame = function(parameterNames, niceColumnNamesEnabled = TRUE, 
				includeAllParameters = FALSE, handleParameterNamesAsToBeExcluded = FALSE,
				returnParametersAsCharacter = FALSE, tableColumnNames = C_TABLE_COLUMN_NAMES) {
			
			parameterNamesToBeExcluded <- c()
			if (handleParameterNamesAsToBeExcluded) {
				parameterNamesToBeExcluded <- parameterNames 
				parameterNames <- .getVisibleFieldNamesOrdered()
				if (!is.null(parameterNamesToBeExcluded) && length(parameterNamesToBeExcluded) > 0) {
					parameterNames <- parameterNames[!(parameterNames %in% parameterNamesToBeExcluded)]
				}
			}
			else if (is.null(parameterNames)) {
				parameterNames <- .getVisibleFieldNamesOrdered()
			}
				
			for (parameterName in parameterNames) {
				if (is.matrix(.self[[parameterName]])) {
					parameterNames <- parameterNames[parameterNames != parameterName]
				}
			}
				
			if (length(parameterNames) == 0) {
				return(data.frame())
			}
			
			parameterName1 <- parameterNames[1]
			
			parameterCaption <- ifelse(niceColumnNamesEnabled 
					&& !is.null(tableColumnNames[[parameterName1]]), 
				tableColumnNames[[parameterName1]], parameterName1)
			parameterValues <- .self[[parameterName1]]
			if (parameterName1 == "kMax" && length(parameterValues) == 1) {
				parameterValues <- c(parameterValues[1], rep(NA_integer_, parameterValues[1] - 1))
			}
			
			numberOfRows <- .getNumberOfRows(parameterNames)
			while (length(parameterValues) < numberOfRows) {
				parameterValues <- c(parameterValues, NA)
			}
			
			dataFrame <- data.frame(parameterValues)
			names(dataFrame) <- parameterCaption
			if (length(parameterNames) < 2) {
				return(dataFrame)
			}
	
			for (i in 2:length(parameterNames)) {			
				tryCatch({	
					parameterName <- parameterNames[i]
					parameterCaption <- ifelse(niceColumnNamesEnabled 
							&& !is.null(tableColumnNames[[parameterName]]), 
						tableColumnNames[[parameterName]], parameterName)
					parameterValues <- .self[[parameterName]]
					if (parameterName == "futilityBounds") {
						parameterValues[parameterValues == C_FUTILITY_BOUNDS_DEFAULT] <- -Inf
					}
					while (length(parameterValues) < nrow(dataFrame)) {
						parameterValues <- c(parameterValues, NA)
					}
					if (includeAllParameters || sum(is.na(parameterValues)) < length(parameterValues)) {
						dataFrame[[parameterCaption]] <- parameterValues
					}
					if (returnParametersAsCharacter) {
						tryCatch({	
							formatFunctionName <- .parameterFormatFunctions[[parameterName]]
							if (!is.null(formatFunctionName)) {
								parameterValuesFormatted <- eval(call(formatFunctionName, parameterValues))
							} else {
								parameterValuesFormatted <- as.character(parameterValues)
							}
							
							if (parameterName == "sided") {
								parameterValuesFormatted <- ifelse(parameterValues == 1, 
									"one-sided", "two-sided")
							}
							
							if (!is.null(dataFrame[[parameterCaption]])) {
								parameterValuesFormatted[is.na(dataFrame[[parameterCaption]])] <- ""
							}
							parameterValuesFormatted[is.na(parameterValuesFormatted)] <- ""
							parameterValuesFormatted[parameterValuesFormatted == "NA"] <- ""
							dataFrame[[parameterCaption]] <- parameterValuesFormatted

						}, error = function(e) {
							.logError(paste0("Error in '.getAsDataFrame'. Failed to show parameter '%s' ", 
								"(class '%s'): %s"), parameterName, class(.self), e)
						})
					}
				}, error = function(e) {
					.logError("Failed to add parameter '%s' to data.frame: %s", parameterName, e)
				})
			}
			
			return(dataFrame)
		},
		
		# 
		# Returns a sub-list.
		# 
		# @param x A list from which you would like to get a sub-list.
		# @param listEntryNames A vector of names which specify the entries of the sub-list to return.
		# 
		.getSubListByNames = function(x, listEntryNames) {
			"Returns a sub-list."
			if (!is.list(x)) {
				stop("'x' must be a list")
			}
			
			if (!is.character(listEntryNames)) {
				stop("'listEntryNames' must be a character vector")
			}
			
			return(x[which(names(x) %in% listEntryNames)])
		}
	)
)

#'
#' @name FieldSet_names
#' 
#' @title
#' The Names of a Field Set object
#'
#' @description
#' Function to get the names of a \code{FieldSet} object.
#' 
#' @details
#' Returns the names of a field set that can be accessed by the user.
#'
#' @export
#' 
#' @keywords internal
#' 
names.FieldSet <- function(x) {
	return(x$.getVisibleFieldNames())
}

#'
#' @name FieldSet_print
#' 
#' @title
#' Print Field Set Values
#'
#' @description
#' \code{print} prints its \code{FieldSet} argument and returns it invisibly (via \code{invisible(x)}). 
#' 
#' @details
#' Prints the field set.
#'
#' @export
#' 
#' @keywords internal
#' 
print.FieldSet <- function(x, ...) {
	x$show()
	invisible(x)
}

#'
#' @name ParameterSet_as.data.frame
#' 
#' @title
#' Coerce Parameter Set to a Data Frame
#'
#' @description
#' Returns the \code{ParameterSet} as data frame.
#' 
#' @details
#' Coerces the parameter set to a data frame.
#' 
#' @export
#' 
#' @keywords internal
#'  
as.data.frame.ParameterSet <- function(x, row.names = NULL, 
		optional = FALSE, niceColumnNamesEnabled = TRUE, includeAllParameters = FALSE, ...) {	
	return(x$.getAsDataFrame(parameterNames = NULL, 
			niceColumnNamesEnabled = niceColumnNamesEnabled, includeAllParameters = includeAllParameters))
}

#'
#' @name ParameterSet_summary
#' 
#' @title
#' Parameter Set Summary
#'
#' @description
#' Displays a summary of \code{ParameterSet} object.
#' 
#' @details
#' Summarizes the parameters and results of a parameter set.
#' 
#' @export
#' 
#' @keywords internal
#' 
summary.ParameterSet <- function(object, ...) {
	cat("This output summarizes the", object$.toString(), "specification.\n\n")
	object$show()
	cat("\n")
	object$show(showType = 2)
	cat("\n")
	print.ParameterSet(object, ...) 
}

#'
#' @name ParameterSet_print
#' 
#' @title
#' Print Parameter Set Values
#'
#' @description
#' \code{print} prints its \code{ParameterSet} argument and returns it invisibly (via \code{invisible(x)}). 
#' 
#' @details
#' Prints the parameters and results of a parameter set.
#'
#' @export
#' 
#' @keywords internal
#' 
print.ParameterSet <- function(x, ...) {
	cat(x$.toString(startWithUpperCase = TRUE), "table:\n")
	parametersToShow <- x$.getParametersToShow()
	parametersToShow <- parametersToShow[parametersToShow != "stages" & parametersToShow != "stage"]
	for (parameter in parametersToShow) {
		if (length(x[[parameter]]) == 1) {
			parametersToShow <- parametersToShow[parametersToShow != parameter]
		}
	}
	x$.printAsDataFrame(parameterNames = parametersToShow)
	invisible(x)
}
