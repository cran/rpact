#:#
#:#  *Summary classes and functions*
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
#:#  File version: $Revision: 4062 $
#:#  Last changed: $Date: 2020-12-01 12:21:16 +0100 (Tue, 01 Dec 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

SummaryItem <- setRefClass("SummaryItem",
	fields = list(
		title = "character",
		values = "character",
		legendEntry = "list"
	),
	methods = list(
		initialize = function(title = NA_character_, values = NA_character_, ...) {
			callSuper(title = title, values = values, ...)
		},
		
		show = function() {
			cat(title, "=", values, "\n")
		},
		
		toList = function() {
			result <- list()
			result[[title]] <- values
		}
	)
)

#' @name SummaryFactory
#'  
#' @title
#' Summary Factory
#' 
#' @description
#' Basic class for summaries
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
SummaryFactory <- setRefClass("SummaryFactory",
	contains = "ParameterSet",
	fields = list(
		object = "ParameterSet",
		title = "character",
		header = "character",
		summaryItems = "list",
		intervalFormat = "character",
		justify = "character"
	),
	
	methods = list(
		initialize = function(..., intervalFormat = "[%s; %s]") {
			callSuper(..., intervalFormat = intervalFormat)
			summaryItems <<- list()
			justify <<- getOption("rpact.summary.justify", "right")
		},
		
		show = function(showType = 1, digits = NA_integer_) {
			.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
			if (is.null(title) || length(title) == 0) {
				title <<- .createSummaryObjectTitle(object)	
			}
			if (!is.null(title) && length(title) == 1 && trimws(title) != "") {
				.cat(title, "\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
			}
		
			if (is.null(header) || length(header) == 0) {
				header <<- .createSummaryObjectHeader(object, .self)
			}
			if (!is.null(header) && length(header) == 1 && trimws(header) != "") {
				.cat(header, "\n\n", heading = 2,
					consoleOutputEnabled = consoleOutputEnabled)
			}
			
			legendEntries <- c()
			legendEntriesUnique <- c()
			summaryItemNames <- c()
			for (summaryItem in summaryItems) {
				if (!is.null(summaryItem$title) && length(summaryItem$title) == 1 && !is.na(summaryItem$title)) {
					summaryItemNames <- c(summaryItemNames, summaryItem$title)
				}
				if (length(summaryItem$legendEntry) > 0) {
					a <- sort(names(summaryItem$legendEntry))
					for (aa in a) {
						if (!(aa %in% legendEntriesUnique)) {
							legendEntriesUnique <- c(legendEntriesUnique, aa)
							b <- summaryItem$legendEntry[[aa]]
							legendEntries <- c(legendEntries, paste0("  ", aa, ": ", b))
						}
					}
				}
			}
			summaryItemNames <- paste0(format(summaryItemNames), " ")
			
			tableColumns <- 0
			maxValueWidth <- 1
			if (length(summaryItems) > 0) {
				for (i in 1:length(summaryItems)) {
					validValues <- na.omit(summaryItems[[i]]$values)
					if (length(validValues) > 0) {
						w <- max(nchar(validValues))
						maxValueWidth <- max(maxValueWidth, w)
						tableColumns <- max(tableColumns, 1 + length(validValues))
					}
				}
				spaceString <- paste0(rep(" ", maxValueWidth + 1), collapse = "")
				for (i in 1:length(summaryItems)) {
					itemTitle <- summaryItems[[i]]$title
					if (!is.null(itemTitle) && length(itemTitle) == 1 && !is.na(itemTitle)) {
						summaryItemName <- summaryItemNames[i]
						values <- summaryItems[[i]]$values
						values <- trimws(values)
						indices <- !grepl("(\\])$", values)
						values[indices] <- paste0(values[indices], " ")
						values <- format(c(spaceString, values), justify = justify)[2:(length(values) + 1)]
						.cat(summaryItemName, values, "\n", tableColumns = tableColumns, 
							consoleOutputEnabled = consoleOutputEnabled)
						if (!consoleOutputEnabled && trimws(summaryItemName) == "Stage") {
							.cat(rep("----- ", tableColumns), "\n", tableColumns = tableColumns, 
								consoleOutputEnabled = consoleOutputEnabled)
						}
					}
				}
			}
			
			if (length(legendEntries) > 0) {
				.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
				.cat("Legend:\n", consoleOutputEnabled = consoleOutputEnabled)
				for (legendEntry in legendEntries) {
					.cat(legendEntry, "\n", consoleOutputEnabled = consoleOutputEnabled)
				}
				.cat("\n", consoleOutputEnabled = consoleOutputEnabled)
			}
		},
		
		addItem = function(title, values, legendEntry = list()) {
			if (!is.character(values)) {
				values <- as.character(values)
			}
			tryCatch({
				addSummaryItem(SummaryItem(title = title, values = values, legendEntry = legendEntry))
			}, error = function(e) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to add summary item '", title, 
					"' = ", .arrayToString(values), " (class: ", class(values), "): ", e$message)
			})
		},
		
		addSummaryItem = function(summaryItem) {
			if (!inherits(summaryItem, "SummaryItem")) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'summaryItem' must be an instance of class 'SummaryItem' (was '", class(summaryItem), "')")
			}
			summaryItems <<- c(summaryItems, summaryItem)
		},
		
		.getFormattedParameterValue = function(valuesToShow, valuesToShow2) {

			naText <- getOption("rpact.summary.na", "")
			if (length(valuesToShow) == length(valuesToShow2) && !all(is.na(valuesToShow2))) {
				for (variantIndex in 1:length(valuesToShow)) {
					value1 <- as.character(valuesToShow[variantIndex])
					value2 <- as.character(valuesToShow2[variantIndex])
					if (grepl("^ *NA *$", value1)) {
						value1 <- naText
					}
					if (grepl("^ *NA *$", value2)) {
						value2 <- naText
					}
					if (trimws(value1) == "" && trimws(value2) == "") {
						valuesToShow[variantIndex] <- naText
					} else {
						valuesToShow[variantIndex] <- sprintf(intervalFormat, value1, value2)
					}
				}
			} else {
				valuesToShow[is.na(valuesToShow) | trimws(valuesToShow) == "NA"] <- naText
			}
			
			return(valuesToShow)
		},
		
		addParameter = function(parameterSet, ..., 
				parameterName = NULL, values = NULL, parameterCaption, 
				roundDigits = NA_integer_, ceilingEnabeld = FALSE, cumsumEnabled = FALSE, 
				twoSided = FALSE, transpose = FALSE, smoothedZeroFormat = FALSE,
				parameterCaptionSingle = parameterCaption, legendEntry = list(),
				enforceFirstCase = FALSE) {
				
			if (!is.null(parameterName) && length(parameterName) == 1 &&
					inherits(parameterSet, "ParameterSet") &&
					parameterSet$.getParameterType(parameterName) == C_PARAM_NOT_APPLICABLE) {
					
				if (.getLogicalEnvironmentVariable("RPACT_DEVELOPMENT_MODE")) {
					warning("Failed to add parameter ", .arrayToString(parameterName), " (", 
						.arrayToString(values), ") stored in ", 
						class(parameterSet), " because the parameter has type C_PARAM_NOT_APPLICABLE")
				}
					
				return(invisible())
			}
				
			parameterName1 <- parameterName[1]
			if (!is.null(parameterName1) && is.character(parameterName1) && is.null(values)) {
				values <- parameterSet[[parameterName1]]
				if (is.null(values)) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, class(parameterSet), 
						" does not contain a field '", parameterName1, "'")
				}
			}
			
			parameterName2 <- NA_character_
			values2 <- NA_real_
			if (!is.null(parameterName) && length(parameterName) > 1) {
				parameterName2 <- parameterName[2]
				values2 <- parameterSet[[parameterName2]]
				parameterName <- parameterName[1]
				if (is.null(values2)) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, class(parameterSet), 
						" does not contain a field '", parameterName2, "'")
				}
			}
			
			if (is.null(values) && is.null(parameterName1)) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'parameterName' or 'values' must be defined")
			}
			
			if (transpose) {
				if (!is.matrix(values)) {
					values <- as.matrix(values)
				} else {
					values <- t(values)
				}
			}
			
			if (is.list(parameterSet) && is.matrix(values)) {
				parameterSet <- parameterSet[["parameterSet"]]
				if (is.null(parameterSet)) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'parameterSet' must be added to list")
				}
			}
			
			parameterNames <- ""
			numberOfVariants <- 1
			numberOfStages <- ifelse(is.matrix(values), ncol(values), length(values))
			if (inherits(parameterSet, "ParameterSet")) {
				parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
				numberOfVariants <- parameterSet$.getMultidimensionalNumberOfVariants(parameterNames) 
				numberOfStages <- parameterSet$.getMultidimensionalNumberOfStages(parameterNames)
			}
			
			stages <- parameterSet[["stages"]]
			if (is.null(stages) && !is.null(parameterSet[[".stageResults"]])) {
				stages <- parameterSet[[".stageResults"]][["stages"]]
			}
			if (is.null(stages) && inherits(parameterSet, "ClosedCombinationTestResults")) {
				stages <- parameterSet[[".design"]][["stages"]]
			}
			if (!is.null(stages) && length(stages) > 0) {
				numberOfStages <- max(na.omit(stages))
				if (is.matrix(values) && nrow(values) > 0) {
					numberOfVariants <- nrow(values)
				}
				if (is.matrix(values) && ncol(values) > 0) {
					numberOfStages <- ncol(values)
				}
			}
			
			if (twoSided) {
				values <- 2 * values
			}

			if (enforceFirstCase || inherits(parameterSet, "Dataset") || (
						(!transpose || numberOfVariants == 1) && 
						(!is.matrix(values) || (!transpose && ncol(values) == 1) || (transpose && nrow(values) == 1)) && ( 
							.isTrialDesign(parameterSet) || 
							(numberOfStages > 1 && numberOfStages == length(values)) || 
							length(values) != numberOfVariants ||
							length(values) == 1 ||
							parameterName %in% c("futilityBoundsEffectScale", "futilityPerStage") 
						)
					)) {
				
				valuesToShow <- .getSummaryValuesFormatted(parameterSet, parameterName1, values, roundDigits = roundDigits, 
					ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled, smoothedZeroFormat = smoothedZeroFormat)
				valuesToShow <- .getInnerValues(valuesToShow, transpose = transpose)
				valuesToShow2 <- NA_real_
				if (!all(is.na(values2))) {
					valuesToShow2 <- .getSummaryValuesFormatted(parameterSet, parameterName1, values2, roundDigits = roundDigits, 
						ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled, smoothedZeroFormat = smoothedZeroFormat)
					valuesToShow2 <- .getInnerValues(valuesToShow2, transpose = transpose)
				}
				
				valuesToShow <- .getFormattedParameterValue(valuesToShow, valuesToShow2) 
				addItem(parameterCaptionSingle, valuesToShow, legendEntry)
			} else {
				if (!inherits(parameterSet, "ParameterSet")) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"for varied values 'parameterSet' must be an instance of ", 
						"class 'ParameterSet' (was '", class(parameterSet), "')")
				}
				
				transposed <- !transpose && grepl("MultiArm", class(parameterSet)) && 
					(!is.matrix(values) || ncol(values) > 1)
				
				userDefinedEffectMatrix <- FALSE
				if (grepl("MultiArm", class(parameterSet)) || 
						inherits(parameterSet, "AnalysisResultsConditionalDunnett") || 
						inherits(parameterSet, "ClosedCombinationTestResults") ||
						inherits(parameterSet, "ConditionalPowerResults")) {
					
					if (grepl("SimulationResultsMultiArm", class(parameterSet)) && 
							parameterName %in% c("rejectAtLeastOne", "earlyStop", 
								"futilityPerStage", "successPerStage", 
								"expectedNumberOfSubjects", "expectedNumberOfEvents",
								"singleNumberOfEventsPerStage", "numberOfActiveArms", "conditionalPowerAchieved")) {
						transposed <- TRUE
						userDefinedEffectMatrix <- parameterSet$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED
						if (userDefinedEffectMatrix) {
							legendEntry[["[j]"]] <- "effect matrix row j (situation to consider)"
						}
						if (grepl("Survival", class(parameterSet))) {
							legendEntry[["(i)"]] <- "results of treatment arm i vs. control arm"
						}
						variedParam <- .getSimulationMultiArmVariedParameter(parameterSet)
						variedParameterValues <- parameterSet[[variedParam]]
						variedParameterCaption <- C_PARAMETER_NAMES[[variedParam]]
						numberOfVariants <- length(variedParameterValues)
					} else if (!inherits(parameterSet, "ClosedCombinationTestResults") || 
							parameterName %in% c("rejected", "separatePValues")) {
							
						if (inherits(parameterSet, "AnalysisResultsConditionalDunnett") && 
								(!is.matrix(values) || ncol(values) > 1)) {
							transposed <- TRUE
						}
			
						if (inherits(parameterSet, "ClosedCombinationTestResults") && 
								parameterSet$.getParameterType("adjustedStageWisePValues") != "g" &&
								parameterName == "separatePValues") {
							transposed <- TRUE
						}
						
						if (inherits(parameterSet, "ClosedCombinationTestResults") && 
								parameterName %in% c("rejected")) {
							transposed <- TRUE
						}
						
						if (inherits(parameterSet, "ConditionalPowerResults") && 
								parameterName %in% c("conditionalPower", "values")) {
							transposed <- TRUE
						}
						
						variedParameterCaption <- "arm" 
						variedParameterValues <- 1:numberOfVariants
						legendEntry[["(i)"]] <- "results of treatment arm i vs. control arm"
					} else  {
						transposed <- TRUE
						variedParameterCaption <- "arms"
						variedParameterValues <- parameterSet$.getHypothesisTreatmentArmVariants()
						numberOfVariants <- length(variedParameterValues)
						legendEntry[["(i, j, ...)"]] <- "comparison of treatment arms 'i, j, ...' vs. control arm"
					}
				} else {
					if (inherits(parameterSet, "Dataset")) {
						variedParameter <- "groups"
					} else {
						variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
					}
					if (length(variedParameter) == 0 || variedParameter == "") {
						warning("Failed to get varied parameter from ", class(parameterSet), 
							" (", length(parameterNames), " parameter names; numberOfVariants: ", numberOfVariants, ")")
						return(invisible())
					}
					
					variedParameterCaption <- parameterSet$.getDataFrameColumnCaption(variedParameter, 
						tableColumnNames = C_TABLE_COLUMN_NAMES, niceColumnNamesEnabled = TRUE)
					variedParameterCaption <- tolower(variedParameterCaption)
					
					if (variedParameterCaption == "alternative") {
						legendEntry[["alt."]] <- "alternative"
						variedParameterCaption <- "alt."
					}
					else if (variedParameterCaption == "hazard ratio") {
						legendEntry[["HR"]] <- "hazard ratio"
						variedParameterCaption <- "HR"
					}
					else if (grepl("\\(1\\)$", variedParameterCaption)) {
						groups <- parameterSet[["groups"]]
						if (!is.null(groups) && length(groups) == 1 && groups == 1) {
							variedParameterCaption = sub(" \\(1\\)$", "", variedParameterCaption)
						} else if (grepl("Survival", class(parameterSet))) {
							#legendEntry[["(1)"]] <- "treatment arm"
						}
					}
					
					variedParameterValues <- round(parameterSet[[variedParameter]], 3)
				}
				
				for (variantIndex in 1:numberOfVariants) {
					colValues <- .getColumnValues(values, variantIndex, transposed)
					colValues <- .getSummaryValuesFormatted(parameterSet, parameterName1, colValues, roundDigits = roundDigits, 
						ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled, smoothedZeroFormat = smoothedZeroFormat)
					colValues2 <- NA_real_
					if (!all(is.na(values2))) {
						colValues2 <- .getColumnValues(values2, variantIndex, transposed)
						colValues2 <- .getSummaryValuesFormatted(parameterSet, parameterName2, colValues2, 
							roundDigits = roundDigits, ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled,
							smoothedZeroFormat = smoothedZeroFormat)
					}
					colValues <- .getFormattedParameterValue(valuesToShow = colValues, valuesToShow2 = colValues2) 
					
					if (numberOfVariants == 1) {
						addItem(parameterCaption, colValues, legendEntry)
					} else if (
							(grepl("MultiArm", class(parameterSet)) && !grepl("Simulation", class(parameterSet))) || 
							inherits(parameterSet, "AnalysisResultsConditionalDunnett") || 
							inherits(parameterSet, "ClosedCombinationTestResults") ||
							inherits(parameterSet, "ConditionalPowerResults")) {
						spacePrefix <- ifelse(parameterCaption %in% c("pi", "lambda", "median"), "", " ")
						addItem(paste0(parameterCaption, spacePrefix, 
							"(", variedParameterValues[variantIndex], ")"), colValues, legendEntry)
					} else if (userDefinedEffectMatrix) {
						addItem(paste0(parameterCaption, " [", variantIndex, "]"), colValues, legendEntry)
					} else {
						addItem(paste0(parameterCaption, ", ", 
							variedParameterCaption, " = ", variedParameterValues[variantIndex]), colValues, legendEntry)
					}
				}
			}
		},
		
		.getInnerValues = function(values, transpose = FALSE) {
			if (!is.matrix(values)) {
				return(values)
			}
			
			if (nrow(values) == 1 && ncol(values) == 1) {
				return(values[1, 1])
			} 
			
			if (transpose) {
				return(values[1, ])
			}
			
			return(values[, 1])
		},
		
		.getColumnValues = function(values, variantIndex, transposed = FALSE) {		
			if (transposed) {
				if (!is.matrix(values)) {
					return(values)
				}
				
				if (nrow(values) == 0) {
					return("")
				}
				
				if (nrow(values) == 1 && ncol(values) == 1) {
					colValues <- values[1, 1]
				} else if (nrow(values) == 1) {
					colValues <- values[1, variantIndex] 
				} else if (ncol(values) == 1) {
					colValues <- values[variantIndex, 1] 
				} else {
					colValues <- values[variantIndex, ]
				}
				return(colValues)
			}
			
			if (length(values) <= 1 && !is.matrix(values)) {
				colValues <- values
			} else if (is.matrix(values)) {
				if (nrow(values) == 1 && ncol(values) == 1) {
					colValues <- values[1, 1]
				} else if (ncol(values) == 1) {
					colValues <- values[variantIndex, 1]
				} else if (nrow(values) == 1) {
					colValues <- values[1, variantIndex]
				} else {
					if (ncol(values) == 0) {
						return("")
					}
					
					colValues <- values[, variantIndex]
				}
			} else {
				colValues <- values[variantIndex]
			}
			return(colValues)
		}
	)
)

.formatSummaryValues <- function(values, digits, smoothedZeroFormat = FALSE) {
	if (is.na(digits) || digits == 0) {
		digits <- 3
	}
	
	if (digits < 1) {
		formattedValue <- as.character(values)
		formattedValue[is.na(formattedValue) | trimws(formattedValue) == "NA"] <- getOption("rpact.summary.na", "")
		return(formattedValue)
	}
	
	if (sum(is.na(values)) == length(values)) {
		formattedValue <- rep(getOption("rpact.summary.na", ""), length(values))
		return(formattedValue)
	}
	
	threshold <- 10^-digits
	text <- "<0."
	if (digits > 1) {
		for (i in 1:(digits - 1)) {
			text <- paste0(text, "0")
		}
	}
	text <- paste0(text, "1")
	
	if (smoothedZeroFormat) {
		values[abs(values) < 1e-15] <- 0
	}
	indices <- (!is.na(values) & values > 1e-24 & abs(values) < threshold)
	values[!is.na(values) & !indices] <- round(values[!is.na(values) & !indices], digits)
	if (sum(indices) > 0) {
		values[indices] <- threshold
		formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits, scientific = FALSE)
		formattedValue[indices] <- text
	} else {
		formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits, scientific = FALSE)
		formattedValue <- format(formattedValue, scientific = FALSE)
	}
	
	if (as.logical(getOption("rpact.summary.trim.zeroes", TRUE))) {
		zeroes <- grepl("^0\\.0*$", formattedValue)
		if (sum(zeroes) > 0) {
			formattedValue[zeroes] <- "0"
		}
	}
	
	formattedValue[is.na(formattedValue) | trimws(formattedValue) == "NA"] <- getOption("rpact.summary.na", "")
	
	return(formattedValue)
}

.getSummaryValuesFormatted <- function(fieldSet, parameterName, values, 
		roundDigits = NA_integer_, ceilingEnabeld = FALSE, cumsumEnabled = FALSE, smoothedZeroFormat = FALSE) {
	if (!is.numeric(values)) {
		return(values)
	}
	
	if (cumsumEnabled) {
		values <- cumsum(values)
	}
	
	if (ceilingEnabeld) {
		values <- ceiling(values)
	}
	else {
		tryCatch({
			if (!is.null(parameterName) && length(parameterName) == 1 && 
					!is.na(parameterName) && parameterName %in% c("criticalValues", "overallAdjustedTestStatistics")) {
				design <- fieldSet
				if (!.isTrialDesign(design)) {
					design <- fieldSet[[".design"]]
				}
				if (!is.null(design) && .isTrialDesignFisher(design)) {
					roundDigits <- 0
				}
			}
			
			formatFunctionName <- NULL
			if (!is.null(parameterName) && length(parameterName) == 1 && 
					!is.na(parameterName) && !is.na(roundDigits) && roundDigits == 0 && inherits(fieldSet, "FieldSet")) {
				formatFunctionName <- fieldSet$.parameterFormatFunctions[[parameterName]]
			}
			if (is.null(formatFunctionName) && !is.na(roundDigits) && roundDigits == 0) {
				formatFunctionName <- C_PARAMETER_FORMAT_FUNCTIONS[[parameterName]]
			}
			
			if (!is.null(formatFunctionName)) {
				values <- eval(call(formatFunctionName, values))
			} else {
				values <- .formatSummaryValues(values, digits = roundDigits, smoothedZeroFormat = smoothedZeroFormat)
			}
		}, error = function(e) {
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to show parameter '", parameterName, "': ", e$message)
		})
	}
	
	return(format(values))
}

.createSummaryObjectTitle <- function(object) {
	design <- NULL
	designPlan <- NULL
	if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
		design <- object$.design
		designPlan <- object
	}
	else if (inherits(object, "AnalysisResults")) {
		return(.createSummaryAnalysisResultsTitle(object$.design, object)) 
	}
	else if (.isTrialDesign(object)) {
		design <- object
	}
	if (!is.null(design)) {
		return(.createSummaryDesignTitle(design, designPlan)) 
	}
	return("")
}

.createSummaryAnalysisResultsTitle <- function(design, analysisResults) {
	kMax <- design$kMax
	
	title <- ""
	if (kMax == 1) {
		title <- paste0(title, "Fixed sample analysis results")
	} else {
		title <- paste0(title, "Sequential analysis results with a maximum of ", kMax, " looks")
	}
	if (!is.null(analysisResults)) {
		if (inherits(analysisResults, "AnalysisResultsMultiArm")) {
			title <- "Multi-arm analysis results for a "
		} else {
			title <- "Analysis results for a "
		}
		
		if (grepl("Means", class(analysisResults$.dataInput))) {
			title <- paste0(title, "continuous endpoint")
		}
		else if (grepl("Rates", class(analysisResults$.dataInput))) {
			title <- paste0(title, "binary endpoint")
		}
		else if (grepl("Survival", class(analysisResults$.dataInput))) {
			title <- paste0(title, "survival endpoint")
		}
	} 
	else if (kMax > 1) {
		title <- .concatenateSummaryText(title, paste0("(", design$.toString(startWithUpperCase = FALSE), ")"), sep = " ")
	}
	
	return(title)
}

.createSummaryDesignTitle <- function(design, designPlan) {
	kMax <- design$kMax
	
	title <- ""
	if (kMax == 1) {
		title <- paste0(title, "Fixed sample analysis")
	} else {
		title <- paste0(title, "Sequential analysis with a maximum of ", kMax, " looks")
	}
	if (!is.null(designPlan)) {
		if (inherits(designPlan, "SimulationResults")) {
			title <- "Simulation of a "
		} else if (designPlan$.isSampleSizeObject()) {
			title <- "Sample size calculation for a "
		} else if (designPlan$.isPowerObject()) {
			title <- "Power calculation for a "
		}
		
		if (grepl("Means", class(designPlan))) {
			title <- paste0(title, "continuous endpoint")
		}
		else if (grepl("Rates", class(designPlan))) {
			title <- paste0(title, "binary endpoint")
		}
		else if (grepl("Survival", class(designPlan))) {
			title <- paste0(title, "survival endpoint")
		}
		
		if (grepl("MultiArm", class(designPlan)) && 
				!is.null(designPlan[["activeArms"]]) && designPlan$activeArms > 1) {
			title <- .concatenateSummaryText(title, "(multi-arm design)", sep = " ")
		}
	} 
	else if (kMax > 1) {
		title <- .concatenateSummaryText(title, 
			paste0("(", design$.toString(startWithUpperCase = FALSE), ")"), sep = " ")
	}
	
	return(title)
}

.getSummaryObjectSettings <- function(object) {
	multiArmEnabled <- grepl("MultiArm", class(object))
	if (inherits(object, "AnalysisResults")) {
		groups <- object$.dataInput$getNumberOfGroups()
		meansEnabled <- grepl("Means", class(object$.dataInput))
		ratesEnabled <- grepl("Rates", class(object$.dataInput))
		survivalEnabled <- grepl("Survival", class(object$.dataInput))
	} else {
		meansEnabled <- grepl("Means", class(object))
		ratesEnabled <- grepl("Rates", class(object))
		survivalEnabled <- grepl("Survival", class(object))
		if (inherits(object, "SimulationResults") && multiArmEnabled) {
			groups <- object$activeArms
		} else {
			groups <- ifelse(multiArmEnabled || survivalEnabled, 2, object[["groups"]])
		}
	}
	
	return(list(
		meansEnabled = meansEnabled,
		ratesEnabled = ratesEnabled,
		survivalEnabled = survivalEnabled,
		groups = groups,
		multiArmEnabled = multiArmEnabled
	))
}

.createSummaryHypothesisText <- function(object, summaryFactory) {
	
	if (!inherits(object, "AnalysisResults") && !inherits(object, "TrialDesignPlan") && 
			!inherits(object, "SimulationResults")) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'object' must be an instance of class 'AnalysisResults', 'TrialDesignPlan' ",
			"or 'SimulationResults' (is '", class(object), "')")
	}
	
	design <- object[[".design"]]
	if (is.null(design)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.design' must be defined in specified ", class(object))
	}
	
	settings <- .getSummaryObjectSettings(object)
	sided <- ifelse(settings$multiArmEnabled, 1, design$sided)
	directionUpper <- object[["directionUpper"]]
	comparsionH0 <- " = "
	comparsionH1 <- NA_character_
	if (inherits(object, "AnalysisResults") && !is.null(directionUpper)) {
		comparsionH1 <- ifelse(sided == 2, " != ", ifelse(directionUpper, " > ", " < "))
	}
	
	if (!is.null(object[["thetaH0"]])) {
		thetaH0 <- round(object$thetaH0, 3)
	} else {
		thetaH0 <- ifelse(settings$survivalEnabled, 1, 0)
	}
	
	treatmentArmIndex <- ifelse(settings$groups > 1, "(i)", "(treatment)")
	controlArmIndex <- ifelse(settings$groups > 1, "(i)", "(control)")
	
	if (settings$multiArmEnabled) {
		if (settings$survivalEnabled) {
			treatmentArmIndex <- "(i)"
			controlArmIndex   <- ""
		} else if (settings$groups == 1) {
			treatmentArmIndex <- "(treatment)"
			controlArmIndex   <- "(control)"
		} else {
			treatmentArmIndex <- "(i)"
			controlArmIndex   <- "(control)"
		}
	} else {
		if (settings$groups == 1 || settings$survivalEnabled) {
			treatmentArmIndex <- ""
			controlArmIndex   <- ""
		} else {
			treatmentArmIndex <- "(1)"
			controlArmIndex   <- "(2)"
		}
	}
	
	value <- "?"
	if (settings$meansEnabled) {
		value <- "mu"
	} else if (settings$ratesEnabled) {
		value <- "pi"
	} else if (settings$survivalEnabled) {
		value <- "hazard ratio"
	}
	
	hypothesis <- ""
	if (!settings$survivalEnabled && (settings$multiArmEnabled || settings$groups == 2)) {
		hypothesis <- paste0(hypothesis, "H0: ", value, treatmentArmIndex, " - ", value, controlArmIndex, comparsionH0, thetaH0)
		if (!is.na(comparsionH1)) {
			hypothesis <- paste0(hypothesis, " against ")
			hypothesis <- paste0(hypothesis, "H1: ", value, treatmentArmIndex, " - ", value, controlArmIndex, comparsionH1, thetaH0)
		}
	} else {
		hypothesis <- paste0(hypothesis, "H0: ", value, treatmentArmIndex, comparsionH0, thetaH0)
		if (!is.na(comparsionH1)) {
			hypothesis <- paste0(hypothesis, " against ")
			hypothesis <- paste0(hypothesis, "H1: ", value, treatmentArmIndex, comparsionH1, thetaH0)
		}
	}
	hypothesis <- .concatenateSummaryText(hypothesis, 
		.createSummaryHypothesisPowerDirectionText(object, sided, directionUpper))
	return(hypothesis)
}

.createSummaryHypothesisPowerDirectionText <- function(object, sided, directionUpper) {
	if (sided == 2 || is.null(directionUpper)) {
		return("")
	}
	
	directionUpper <- unique(directionUpper)
	if (length(directionUpper) != 1) {
		return("")
	}
	
	if (inherits(object, "AnalysisResults")) { 
		return("")
	}
	
	if (.isTrialDesignPlan(object) && object$.objectType != "power") {
		return("")
	}
	
	if (directionUpper) {
		return("power directed towards larger values")
	} else {
		return("power directed towards smaller values")
	}
}

.addSummaryLineBreak <- function(text, newLineLength) {
	maxLineLength <- as.integer(getOption("rpact.summary.width", 83))
	lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
	lastLine <- lines[length(lines)]
	if (nchar(lastLine) + newLineLength > maxLineLength) {
		text <- paste0(text, "\n")
	}
	return(text)
}

.concatenateSummaryText <- function(a, b, sep = ", ") {
	if (is.na(b) || nchar(trimws(b)) == 0) {
		return(a)
	}
	
	if (a == "") {
		return(b)
	}
	
	a <- paste0(a, sep)
	a <- .addSummaryLineBreak(a, nchar(b))
	return(paste0(a, b))
}

.createSummaryObjectHeader <- function(object, summaryFactory) {
	if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
		return(.createSummaryDesignHeader(object$.design, object, summaryFactory)) 
	}
	
	if (inherits(object, "AnalysisResults")) {
		return(.createSummaryAnalysisResultsHeader(object$.design, object, summaryFactory)) 
	}
	
	if (.isTrialDesign(object)) {
		return(.createSummaryDesignHeader(object, NULL, summaryFactory)) 
	}
	
	return("")
}

.addOptimumAllocationRatioToHeader <- function(parameterSet, header) {
	
	if (!.isTrialDesignPlanSurvival(parameterSet) && !grepl("Simulation", class(parameterSet))) {
		numberOfGroups <- 1
		if (inherits(parameterSet, "TrialDesignPlan")) {
			numberOfGroups <- parameterSet$groups
		}
		else if (inherits(parameterSet, "AnalysisResults")) {
			numberOfGroups <- parameterSet$.dataInput$getNumberOfGroups()
		}
		if (numberOfGroups == 1) {
			return(header)
		}
	}
	
	prefix <- ""
	if (!is.null(parameterSet[["optimumAllocationRatio"]]) && 
		length(parameterSet$optimumAllocationRatio) == 1 && 
		parameterSet$optimumAllocationRatio) {
		if (length(unique(parameterSet$allocationRatioPlanned)) > 1) {
			return(.concatenateSummaryText(header, "optimum planned allocation ratio"))
		}
		prefix <- "optimum "
	}
	
	allocationRatioPlanned <- round(unique(parameterSet$allocationRatioPlanned), 3)
	if (identical(allocationRatioPlanned, 1)) {
		return(header)
	}
	
	if (!is.na(allocationRatioPlanned)){
		return(.concatenateSummaryText(header, paste0(prefix, "planned allocation ratio = ", allocationRatioPlanned)))
	} else {
		return(header)
	} 
}

.createSummaryAnalysisResultsHeader <- function(design, analysisResults, summaryFactory) {
	stageResults <- analysisResults$.stageResults
	dataInput <- analysisResults$.dataInput
	
	multiArmEnabled <- .isMultiArmAnalysisResults(analysisResults)
	
	header <- ""
	if (design$kMax == 1) {
		header <- paste0(header, "Fixed sample analysis.")
	} else {
		header <- paste0(header, "Sequential analysis with ", design$kMax, " looks")
		header <- .concatenateSummaryText(header, paste0("(", design$.toString(startWithUpperCase = FALSE), ")."), sep = " ")
	}
	header <- paste0(header, "\n")
	
	header <- paste0(header, "The results were calculated using a ")
	if (stageResults$isDatasetMeans()) {
		if (dataInput$getNumberOfGroups() == 1) {
			header <- paste0(header, "one-sample t-test")
		} else if (dataInput$getNumberOfGroups() == 2) {
			header <- paste0(header, "two-sample t-test")
		} else {
			header <- paste0(header, "multi-arm t-test")
		}
	}
	else if (stageResults$isDatasetRates()) {
		if (dataInput$getNumberOfGroups() == 1) {
			header <- paste0(header, "one-sample test for rates")
		} else if (dataInput$getNumberOfGroups() == 2) {
			header <- paste0(header, "two-sample test for rates")
		} else {
			header <- paste0(header, "multi-arm test for rates")
		}
	}
	else if (stageResults$isDatasetSurvival()) {
		if (dataInput$getNumberOfGroups() == 2) {
			header <- paste0(header, "two-sample logrank test")
		} else {
			header <- paste0(header, "multi-arm logrank test")
		}
	}
	
	if (design$sided == 1) {
		header <- paste0(header, " (one-sided)")
	} else {
		header <- paste0(header, " (two-sided)")
	}
	
	if (!.isTrialDesignConditionalDunnett(design) && multiArmEnabled) {
		if (stageResults$intersectionTest == "Dunnett") {
			header <- .concatenateSummaryText(header, "Dunnett intersection test")
		} else if (stageResults$intersectionTest == "Bonferroni") {
			header <- .concatenateSummaryText(header, "Bonferroni intersection test")
		} else if (stageResults$intersectionTest == "Simes") {
			header <- .concatenateSummaryText(header, "Simes intersection test")
		} else if (stageResults$intersectionTest == "Sidak") {
			header <- .concatenateSummaryText(header, "Sidak intersection test")
		} else if (stageResults$intersectionTest == "Hierarchical") {
			header <- .concatenateSummaryText(header, "Hierarchical intersection test")			
		}	
	}
	
	if (!.isTrialDesignConditionalDunnett(design) && 
			(stageResults$isDatasetMeans() || stageResults$isDatasetRates()) && 
			stageResults$normalApproximation) {
		header <- .concatenateSummaryText(header, "normal approximation")	
	}
	
	if (stageResults$isDatasetMeans() && multiArmEnabled) {
		if (stageResults$varianceOption == "overallPooled") {
			header <- .concatenateSummaryText(header, "overall pooled variances option")	
		} else if (stageResults$varianceOption == "pairwisePooled") {
			header <- .concatenateSummaryText(header, "pairwise pooled variances option")
		} else if (stageResults$varianceOption == "notPooled") {
			header <- .concatenateSummaryText(header, "not pooled variances option")
		}
	}
	
	if (stageResults$isDatasetMeans() && (dataInput$getNumberOfGroups() == 2)) {
		if (stageResults$equalVariances) {
			header <- .concatenateSummaryText(header, "equal variances option")	
		} else {
			header <- .concatenateSummaryText(header, "unequal variances option")
		}
	}

	if (.isTrialDesignConditionalDunnett(design)) {
		if (design$secondStageConditioning) {
			header <- .concatenateSummaryText(header, "conditional second stage p-values")
		} else {
			header <- .concatenateSummaryText(header, "unconditional second stage p-values")
		}
	}	
	
	header <- paste0(header, ".\n", .createSummaryHypothesisText(analysisResults, summaryFactory))
	header <- .addOptimumAllocationRatioToHeader(analysisResults, header)
	
	if (stageResults$isDatasetMeans()) {
		header <- .getSummaryAnalysisResultsHeaderEntry(header, analysisResults, 
			paramName1 = "thetaH1", 
			paramName2 = ifelse(multiArmEnabled, "assumedStDevs", "assumedStDev"),
			paramCaption1 = "assumed effect", 
			paramCaption2 = "assumed standard deviation",
			shortcut1 = "thetaH1",
			shortcut2 = "sd", multiArmEnabled = multiArmEnabled
		)
	}
	else if (stageResults$isDatasetRates()) {
		header <- .getSummaryAnalysisResultsHeaderEntry(header, analysisResults, 
			paramName1 = ifelse(multiArmEnabled, "piTreatments", "pi1"), 
			paramName2 = ifelse(multiArmEnabled, "piControl", "pi2"),
			paramCaption1 = "assumed treatment rate", 
			paramCaption2 = "assumed control rate",
			shortcut1 = "pi",
			shortcut2 = "pi", multiArmEnabled = multiArmEnabled
		)
	}
	else if (stageResults$isDatasetSurvival()) {
		header <- .getSummaryAnalysisResultsHeaderEntry(header, analysisResults, 
			paramName1 = "thetaH1", 
			paramCaption1 = "assumed effect",
			shortcut1 = "thetaH1", multiArmEnabled = multiArmEnabled)
	}
	
	header <- paste0(header, ".")
	return(header)
}

.getSummaryAnalysisResultsHeaderEntryValue <- function(shortcut, value) {
	value[!is.na(value)] <- round(value[!is.na(value)], 2)
	if ((is.matrix(value) && nrow(value) > 1) || length(value) > 1) {
		value <- paste0(paste(paste0(shortcut, "(", 1:length(value), ") = ", value)), collapse = ", ")
	}
	return(value)
}

.getSummaryAnalysisResultsHeaderEntry <- function(header, analysisResults, ...,  
		paramName1, paramName2 = NA_character_,
		paramCaption1, paramCaption2 = NA_character_,
		shortcut1, shortcut2 = NA_character_, multiArmEnabled) {
	paramValue1 <- analysisResults[[paramName1]]
	case1 <- analysisResults$.getParameterType(paramName1) != C_PARAM_GENERATED &&
		!all(is.na(paramValue1))
	case2 <- FALSE
	if (!is.na(paramName2)) {
		paramValue2 <- analysisResults[[paramName2]]	
		case2 <- analysisResults$.getParameterType(paramName2) != C_PARAM_GENERATED &&
			!all(is.na(paramValue2))
	}
	
	if (case1) {
		if (!any(is.na(paramValue1)) && length(unique(paramValue1)) == 1) {
			paramValue1 <- paramValue1[1]
		}
		if (length(paramValue1) == 1) {
			header <- .concatenateSummaryText(header, 
				paste0(paramCaption1, " = ", ifelse(is.na(paramValue1), paramValue1, round(paramValue1, 2))))
		} else {
			header <- .concatenateSummaryText(header, 
				paste0(paramCaption1, ": ", .getSummaryAnalysisResultsHeaderEntryValue(shortcut1, paramValue1)))
		}
	}
	
	if (case2) {
		if (length(paramValue2) == 1) {
			header <- .concatenateSummaryText(header, 
				paste0(paramCaption2, " = ", ifelse(is.na(paramValue2), paramValue2, round(paramValue2, 2))))
		} else {
			header <- .concatenateSummaryText(header, 
				paste0(paramCaption2, ": ", .getSummaryAnalysisResultsHeaderEntryValue(shortcut2, paramValue2)))
		}
	}
	return(header)
}

.createSummaryDesignHeader <- function(design, designPlan, summaryFactory) {
	if (is.null(designPlan)) {
		if (.isTrialDesignFisher(design)) {
			designType <- "Fisher's combination test"
		}
		else if (.isTrialDesignConditionalDunnett(design)) {
			designType <- "Conditional Dunnett test"
		}
		else {
			designType <- C_TYPE_OF_DESIGN_LIST[[design$typeOfDesign]]
		}
		header <- paste0(designType, " design")
		header <- .concatenateSummaryText(header, paste0(ifelse(design$sided == 1, "one-sided", "two-sided"), " local"))
		header <- .concatenateSummaryText(header, paste0("significance level ", round(100 * design$alpha, 2), "%"), sep = " ")
		if (.isTrialDesignInverseNormalOrGroupSequential(design)) {
			header <- .concatenateSummaryText(header, paste0("power ", round(100 * (1 - design$beta), 1), "%"))
		}
		header <- .concatenateSummaryText(header, "undefined endpoint")
		header <- paste0(header, ".")
		return(header)
	}
	
	header <- ""
	if (design$kMax == 1) {
		header <- paste0(header, "Fixed sample analysis.")
	} else {
		header <- paste0(header, "Sequential analysis with a maximum of ", design$kMax, " looks")
		header <- .concatenateSummaryText(header, paste0("(", design$.toString(startWithUpperCase = FALSE), ")."), sep = " ")
	}
	header <- paste0(header, "\n")
	
	header <- paste0(header, "The ", ifelse(inherits(designPlan, "SimulationResults") || 
				designPlan$.isPowerObject(), "results were ", "sample size was "))
	header <- paste0(header, ifelse(inherits(designPlan, "SimulationResults"), "simulated", "calculated"))
	header <- paste0(header, " for a ")
	settings <- .getSummaryObjectSettings(designPlan)
	if (settings$meansEnabled) {
		if (settings$multiArmEnabled && settings$groups > 1) {
			header <- .concatenateSummaryText(header, "multi-arm comparisons for means", sep = "")
		} else if (settings$groups == 1 && !settings$multiArmEnabled) {
			header <- .concatenateSummaryText(header, "one-sample t-test", sep = "")
		} else if (settings$groups == 2 || settings$multiArmEnabled) {
			header <- .concatenateSummaryText(header, "two-sample t-test", sep = "")
		}
	}
	else if (settings$ratesEnabled) {
		if (settings$multiArmEnabled && settings$groups > 1) {
			header <- .concatenateSummaryText(header, "multi-arm comparisons for rates", sep = "") 
		} else if (settings$groups == 1 && !settings$multiArmEnabled) {
			header <- .concatenateSummaryText(header, "one-sample test for rates", sep = "")
		} else if (settings$groups == 2 || settings$multiArmEnabled) {
			header <- .concatenateSummaryText(header, "two-sample test for rates", sep = "")
		}
	}
	else if (settings$survivalEnabled) {
		if (settings$multiArmEnabled && settings$groups > 1) {
			header <- .concatenateSummaryText(header, "multi-arm logrank test", sep = "")
		} else if (settings$groups == 2 || settings$multiArmEnabled) {
			header <- .concatenateSummaryText(header, "two-sample logrank test", sep = "")
		}
	}
	
	part <- ""
	if (settings$multiArmEnabled && settings$groups > 1) {
		part <- paste0(settings$groups, " treatments vs. control")
	}
	if (!inherits(designPlan, "SimulationResults") && !settings$multiArmEnabled) {
		part <- .concatenateSummaryText(part, ifelse(design$sided == 1, "one-sided", "two-sided"))
	}
	if (inherits(designPlan, "SimulationResults") && !settings$multiArmEnabled && 
			!settings$survivalEnabled) {
		part <- .concatenateSummaryText(part, ifelse(designPlan$normalApproximation, 
			"normal approximation", "exact test of Fisher"))	
	}
	if (part != "") {
		header <- .concatenateSummaryText(header, paste0("(", part, ")"), sep = " ")
	}
	if (settings$meansEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) || 
			inherits(designPlan, "SimulationResults"))) {
		header <- .concatenateSummaryText(header, .createSummaryHypothesisText(designPlan, summaryFactory))
		if (!is.null(designPlan[["alternative"]]) && length(designPlan$alternative) == 1) {
			alternativeText <- paste0("H1: effect = ", round(designPlan$alternative, 3))
		} else if (!is.null(designPlan[["muMaxVector"]]) && length(designPlan$muMaxVector) == 1) {
			alternativeText <- paste0("H1: mu_max = ", round(designPlan$muMaxVector, 3))
		} else {
			alternativeText <- "H1: effect as specified"
		}	
		header <- .concatenateSummaryText(header, alternativeText)
		header <- .concatenateSummaryText(header, paste0("standard deviation = ", round(designPlan$stDev, 3)))
		header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
	}
	else if (settings$ratesEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) || 
			inherits(designPlan, "SimulationResults"))) {
		if (settings$groups == 1) {
			if (!is.null(designPlan[["pi1"]]) && length(designPlan$pi1) == 1) {
				treatmentRateText <- paste0("H1: treatment rate pi = ", round(designPlan$pi1, 3))
			} else {
				treatmentRateText <- "H1: treatment rate pi as specified"
			}
			header <- paste0(header, ",\n", .createSummaryHypothesisText(designPlan, summaryFactory))
			header <- .concatenateSummaryText(header, treatmentRateText)
		} else {
			if (!is.null(designPlan[["pi1"]]) && length(designPlan$pi1) == 1) {
				treatmentRateText1 <- paste0("H1; treatment rate pi(1) = ", round(designPlan$pi1, 3))
			} else if (!is.null(designPlan[["piMaxVector"]]) && length(designPlan$piMaxVector) == 1) {
				treatmentRateText1 <- paste0("H1: treatment rate pi_max = ", round(designPlan$piMaxVector, 3))
			} else {
				treatmentRateText1 <- paste0("H1: treatment rate pi", 
				ifelse(settings$multiArmEnabled, "_max", "(1)"), " as specified")
			}
			
			if (settings$multiArmEnabled && !is.null(designPlan[["piControl"]])) {
				treatmentRateText2 <- paste0("control rate pi(control) = ", round(designPlan$piControl, 3))
			} else if (!is.null(designPlan[["pi2"]])) {
				treatmentRateText2 <- paste0("control rate pi(2) = ", round(designPlan$pi2, 3))
			} else {
				treatmentRateText2 <- NA_character_
			}
			header <- paste0(header, ",\n", .createSummaryHypothesisText(designPlan, summaryFactory))
			header <- .concatenateSummaryText(header, treatmentRateText1)
			header <- .concatenateSummaryText(header, treatmentRateText2)
			header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
		}
	}
	else if (settings$survivalEnabled && (.isTrialDesignInverseNormalOrGroupSequential(design) || 
			inherits(designPlan, "SimulationResults"))) {
		
		parameterNames <- designPlan$.getVisibleFieldNamesOrdered()
		numberOfVariants <- designPlan$.getMultidimensionalNumberOfVariants(parameterNames)
		
		userDefinedParam <- "pi1"
		for (param in c("pi1", "lambda1", "median1", "hazardRatio")) {
			if (designPlan$.getParameterType(param) == C_PARAM_USER_DEFINED &&
				length(designPlan[[param]]) == numberOfVariants) {
				userDefinedParam <- param
			}
		}
		paramValue <- designPlan[[userDefinedParam]]
		if (is.null(paramValue) || length(paramValue) == 0 || all(is.na(paramValue))) {
			userDefinedParam <- "hazardRatio"
		}
		paramName <- "treatment pi(1)"
		if (userDefinedParam == "lambda1") {
			paramName <- "treatment lambda(1)"
		}
		else if (userDefinedParam == "median1") {
			paramName <- "treatment median(1)"
		}
		else if (userDefinedParam == "hazardRatio") {
			paramName <- ifelse(grepl("SimulationResultsMultiArm", class(designPlan)), "omega_max", "hazard ratio")
		}
		
		if (length(designPlan[[userDefinedParam]]) == 1) {
			treatmentRateText <- paste0("H1: ", paramName, " = ", round(designPlan[[userDefinedParam]], 3))
		} else if (!is.null(designPlan[["omegaMaxVector"]]) && length(designPlan$omegaMaxVector) == 1) {
			treatmentRateText <- paste0("H1: omega_max = ", round(designPlan$omegaMaxVector, 3))
		} else {
			treatmentRateText <- paste0("H1: ", paramName, " as specified")
		}
		if (userDefinedParam %in% c("hazardRatio", "pi1") && 
			(designPlan$.getParameterType("pi2") == C_PARAM_USER_DEFINED ||
				designPlan$.getParameterType("pi2") == C_PARAM_DEFAULT_VALUE) &&
			length(designPlan$pi2) == 1) {
			treatmentRateText <- paste0(treatmentRateText, ", control pi(2) = ", round(designPlan$pi2, 3))
		}
		else if (userDefinedParam %in% c("hazardRatio", "lambda1") && 
			(designPlan$.getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
				designPlan$.getParameterType("lambda2") == C_PARAM_DEFAULT_VALUE) &&
			length(designPlan$lambda2) == 1) {
			treatmentRateText <- paste0(treatmentRateText, ", control lambda(2) = ", round(designPlan$lambda2, 3))
		}
		else if (userDefinedParam %in% c("hazardRatio", "median1") && 
			(designPlan$.getParameterType("median2") == C_PARAM_USER_DEFINED ||
				designPlan$.getParameterType("median2") == C_PARAM_GENERATED) &&
			length(designPlan$median2) == 1) {
			treatmentRateText <- paste0(treatmentRateText, ", control median(2) = ", round(designPlan$median2, 3))
		}
		else if (!is.null(designPlan[[".piecewiseSurvivalTime"]]) && 
				designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
				treatmentRateText <- paste0(treatmentRateText, ", piecewise survival distribution")
		}
		header <- paste0(header, ", \n", .createSummaryHypothesisText(designPlan, summaryFactory))
		header <- .concatenateSummaryText(header, treatmentRateText)
		header <- .addAdditionalArgumentsToHeader(header, designPlan, settings)
	}
	#header <- .concatenateSummaryText(header, paste0("significance level ", round(100 * design$alpha, 2), "%"))
	if (!inherits(designPlan, "SimulationResults") && designPlan$.isSampleSizeObject()) {
		header <- .concatenateSummaryText(header, paste0("power ", round(100 * (1 - design$beta), 1), "%"))
	}
	
	if (inherits(designPlan, "SimulationResultsSurvival")) {
		header <- .concatenateSummaryText(header, paste0("maximum number of subjects = ", designPlan$maxNumberOfSubjects))
	}
	
	if (inherits(designPlan, "SimulationResults")) {
		header <- .concatenateSummaryText(header, paste0("simulation runs = ", designPlan$maxNumberOfIterations))
		header <- .concatenateSummaryText(header, paste0("seed = ", designPlan$seed))
	}
	header <- paste0(header, ".")
	return(header)
}

.addAdditionalArgumentsToHeader <- function(header, designPlan, settings) {
	if (settings$survivalEnabled) {
		if (!is.null(designPlan[["plannedEvents"]])) {
			header <- .concatenateSummaryText(header, paste0("planned cumulative events = ", 
				.arrayToString(designPlan$plannedEvents, 
				vectorLookAndFeelEnabled = (length(designPlan$plannedEvents) > 1))))
		}
	} else {
		if (!is.null(designPlan[["plannedSubjects"]])) {
			header <- .concatenateSummaryText(header, paste0("planned cumulative sample size = ", 
				.arrayToString(designPlan$plannedSubjects, 
				vectorLookAndFeelEnabled = (length(designPlan$plannedSubjects) > 1))))
		}
	}
	
	header <- .addOptimumAllocationRatioToHeader(designPlan, header)
	
	if (settings$multiArmEnabled && designPlan$activeArms > 1) {
		header <- .addShapeSelectionToHeader(header, designPlan)
	}
	
	functionName <- ifelse(settings$survivalEnabled, "calcEventsFunction", "calcSubjectsFunction")
	userDefinedFunction <- !is.null(designPlan[[functionName]]) && 
		designPlan$.getParameterType(functionName) == C_PARAM_USER_DEFINED	

	if (userDefinedFunction || (!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
		
		if (userDefinedFunction) {
			header <- .concatenateSummaryText(header, paste0("sample size reassessment: user defined '", functionName, "'"))
			if ((!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
				header <- .concatenateSummaryText(header, 
						paste0("conditional power = ", designPlan$conditionalPower))
			}
		} else {	
			if ((!is.null(designPlan[["conditionalPower"]]) && !is.na(designPlan$conditionalPower))) {
				header <- .concatenateSummaryText(header, 
					paste0("sample size reassessment: conditional power = ", designPlan$conditionalPower))
			}
		}	
		
		paramName1 <- ifelse(settings$survivalEnabled, "minNumberOfEventsPerStage", "minNumberOfSubjectsPerStage")
		paramName2 <- ifelse(settings$survivalEnabled, "maxNumberOfEventsPerStage", "maxNumberOfSubjectsPerStage")
		paramCaption <- ifelse(settings$survivalEnabled, "events", "subjects")
		if (!is.null(designPlan[[paramName1]])) {
			header <- .concatenateSummaryText(header, paste0("minimum ", paramCaption, " per stage = ", 
				.arrayToString(designPlan[[paramName1]], 
					vectorLookAndFeelEnabled = (length(designPlan[[paramName1]]) > 1))))
		}
		if (!is.null(designPlan[[paramName2]])) {
			header <- .concatenateSummaryText(header, paste0("maximum ", paramCaption, " per stage = ", 
				.arrayToString(designPlan[[paramName2]], 
					vectorLookAndFeelEnabled = (length(designPlan[[paramName2]]) > 1))))
		}
		
		if (settings$meansEnabled) {
			if (!is.na(designPlan$thetaH1)) {
				header <- .concatenateSummaryText(header, paste0("theta H1 = ", round(designPlan$thetaH1, 3)))
			} 
			if (!is.na(designPlan$stDevH1)) {
				header <- .concatenateSummaryText(header, paste0("standard deviation H1 = ", round(designPlan$stDevH1, 3)))
			}  
		} else if (settings$ratesEnabled) {
			if (settings$multiArmEnabled) { 
				if (!is.na(designPlan$piH1)) {
					header <- .concatenateSummaryText(header, paste0("pi(treatment)H1 = ", round(designPlan$piH1, 3))) 
				} 
				if (!is.na(designPlan$piControlH1)) {
					header <- .concatenateSummaryText(header, paste0("pi(control)H1 = ", round(designPlan$piControlH1, 3)))
				}  
			} else {
				if (!is.na(designPlan$pi1H1)) {
					header <- .concatenateSummaryText(header, paste0("pi(treatment)H1 = ", round(designPlan$pi1H1, 3))) 
				} 
				if (!is.na(designPlan$pi2H1)) {
					header <- .concatenateSummaryText(header, paste0("pi(control)H1 = ", round(designPlan$pi2H1, 3)))
				}  
			}
		}
		
		if (settings$survivalEnabled && !is.null(designPlan[["thetaH1"]]) && !is.na(designPlan$thetaH1)) {
			header <- .concatenateSummaryText(header, paste0("thetaH1 = ", round(designPlan$thetaH1, 3)))
		}
	}	

	return(header)
}

.addShapeSelectionToHeader <- function(header, designPlan) {
	header <- .concatenateSummaryText(header, paste0("intersection test = ", designPlan$intersectionTest))
	header <- .concatenateSummaryText(header, paste0("effect shape = ", .formatCamelCase(designPlan$typeOfShape)))
	if (designPlan$typeOfShape == "sigmoidEmax") {
		header <- .concatenateSummaryText(header, paste0("slope = ", designPlan$slope))
		header <- .concatenateSummaryText(header, paste0("ED50 = ", designPlan$gED50))
	}	
	
	typeOfSelectionText <- paste0("selection = ", .formatCamelCase(designPlan$typeOfSelection))
	if (designPlan$typeOfSelection == "rBest") {	
		typeOfSelectionText <- paste0(typeOfSelectionText, ", r = ", designPlan$rValue)
	} else if (designPlan$typeOfSelection == "epsilon") {	
		typeOfSelectionText <- paste0(typeOfSelectionText, " rule, eps = ", designPlan$epsilonValue)
	}
	if (!is.null(designPlan$threshold) && length(designPlan$threshold) == 1 && designPlan$threshold > -Inf) {
		typeOfSelectionText <- paste0(typeOfSelectionText, ", threshold = ", designPlan$threshold)
	}
	header <- .concatenateSummaryText(header, typeOfSelectionText)
	header <- .concatenateSummaryText(header, paste0("effect measure based on ", .formatCamelCase(designPlan$effectMeasure)))
	header <- .concatenateSummaryText(header, paste0("success criterion: ", .formatCamelCase(designPlan$successCriterion)))
	
	return(header)
}	

.createSummary <- function(object, digits = NA_integer_) {
	if (inherits(object, "TrialDesignCharacteristics")) {
		return(.createDesignPlanSummary(object$.design, digits = digits))
	}
	
	if (.isTrialDesign(object) || .isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
		return(.createDesignPlanSummary(object, digits = digits))
	} 
	
	if (inherits(object, "AnalysisResults")) {
		return(.createAnalysisResultsSummary(object, digits = digits))
	}
	
	stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "function 'summary' not implemented yet for class ", class(object))
}

#
# Main function for creating a summary of an analysis result
#
.createAnalysisResultsSummary <- function(object, digits = NA_integer_) {
	if (!inherits(object, "AnalysisResults")) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'object' must be a valid analysis result object (is class ", class(object), ")")
	}
	
	digitSettings <- .getSummaryDigits(digits)
	digits <- digitSettings$digits
	digitsSampleSize <- digitSettings$digitsSampleSize
	digitsGeneral <- digitSettings$digitsGeneral
	digitsProbabilities <- digitSettings$digitsProbabilities
	
	outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)
	
	intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
	.assertIsValidSummaryIntervalFormat(intervalFormat)
	
	multiArmEnabled <- .isMultiArmAnalysisResults(object)
	
	analysisResults <- object
	design <- analysisResults$.design
	stageResults <- analysisResults$.stageResults
	dataInput <- analysisResults$.dataInput
	closedTestResults <- NULL
	conditionalPowerResults <- NULL
	if (multiArmEnabled) {
		closedTestResults <- analysisResults$.closedTestResults
		conditionalPowerResults <- analysisResults$.conditionalPowerResults
	}
	
	summaryFactory <- SummaryFactory(object = object, intervalFormat = intervalFormat)
	
	.addDesignInformationToSummary(design, object, summaryFactory)
	
	if (!.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(design, parameterName = "criticalValues", 
			parameterCaption = ifelse(.isTrialDesignFisher(design),
				"Efficacy boundary (p product scale)", "Efficacy boundary (z-value scale)"), 
			roundDigits = digitsProbabilities - ifelse(.isTrialDesignFisher(design) || digitsProbabilities <= 1, 0, 1), 
			smoothedZeroFormat = !.isTrialDesignFisher(design))
	}
	
	if (.isTrialDesignFisher(design)) {
		if (any(design$alpha0Vec < 1)) {
			summaryFactory$addParameter(design, parameterName = "alpha0Vec", 
				parameterCaption = "Futility boundary (separate p-value scale)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
	} else if (!.isTrialDesignConditionalDunnett(design)) {
		if (any(design$futilityBounds > - 6)) {
			summaryFactory$addParameter(design, parameterName = "futilityBounds", 
				parameterCaption = "Futility boundary (z-value scale)", 
				roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities), 
				smoothedZeroFormat = TRUE)
		}
	}
	
	if (design$kMax > 1 && !.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(design, parameterName = "alphaSpent", 
			parameterCaption = "Cumulative alpha spent", 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
	}
	
	if (!.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(design, parameterName = "stageLevels", 
			parameterCaption = "Stage level", roundDigits = digitsProbabilities, 
			smoothedZeroFormat = TRUE)
	}
	
	if (stageResults$isDatasetRates()) {
		if (multiArmEnabled) {
			summaryFactory$addParameter(stageResults, parameterName = "piControl", 
				parameterCaption = "Control rate", roundDigits = digitsGeneral)
			gMax <- nrow(stageResults$separatePValues)
			effectSizes <- matrix(numeric(0), ncol = ncol(stageResults$piControl))
			for (g in 1:gMax) {
				effectSizes <- rbind(effectSizes, stageResults$piTreatments[g, ] - stageResults$piControl)
			}
			summaryFactory$addParameter(list(effectSizes = effectSizes, parameterSet = stageResults), 
				parameterName = "effectSizes", 
				parameterCaption = "Overall effect size", roundDigits = digitsGeneral)
		} else {
			summaryFactory$addParameter(stageResults, parameterName = "effectSizes", 
				parameterCaption = "Overall effect size", roundDigits = digitsGeneral)
		}
	} else {
		summaryFactory$addParameter(stageResults, parameterName = "effectSizes", 
			parameterCaption = "Overall effect size", roundDigits = digitsGeneral)
		if (stageResults$isDatasetMeans()) {
			summaryFactory$addParameter(stageResults, parameterName = "overallStDevs", 
				parameterCaption = "Overall standard deviation", roundDigits = digitsGeneral)
		}
	}
	
	if (.isTrialDesignGroupSequential(design)) {
		summaryFactory$addParameter(stageResults, 
			parameterName = ifelse(!multiArmEnabled && stageResults$isDatasetSurvival(), "overallLogRanks", "overallTestStatistics"), 
			parameterCaption = "Overall test statistics", 
			roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities), 
			smoothedZeroFormat = TRUE)
		summaryFactory$addParameter(stageResults, parameterName = ifelse(multiArmEnabled, "separatePValues", "overallPValues"), 
			parameterCaption = "Overall p-value", roundDigits = digitsProbabilities)
	} else {
		summaryFactory$addParameter(stageResults, 
				parameterName = ifelse(!multiArmEnabled && stageResults$isDatasetSurvival(), "logRanks", "testStatistics"), 
				parameterCaption = "Test statistics", 
				roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities), 
				smoothedZeroFormat = TRUE)
		summaryFactory$addParameter(stageResults, parameterName = ifelse(multiArmEnabled, "separatePValues", "pValues"), 
				parameterCaption = "p-value", roundDigits = digitsProbabilities)
	}  

	if (!is.null(closedTestResults)) {
		if (outputSize == "large") {
			if (.isTrialDesignConditionalDunnett(design)) {
				summaryFactory$addParameter(closedTestResults, parameterName = "conditionalErrorRate", 
					parameterCaption = "Conditional error rate", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
				summaryFactory$addParameter(closedTestResults, parameterName = "secondStagePValues", 
					parameterCaption = "Second stage p-value", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
			} else {
				summaryFactory$addParameter(closedTestResults, parameterName = "adjustedStageWisePValues", 
					parameterCaption = "Adjusted stage-wise p-value", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
				summaryFactory$addParameter(closedTestResults, parameterName = "overallAdjustedTestStatistics", 
					parameterCaption = "Overall adjusted test statistics", 
					roundDigits = digitsProbabilities - ifelse(.isTrialDesignFisher(design) || digitsProbabilities <= 1, 0, 1), 
					smoothedZeroFormat = !.isTrialDesignFisher(design))
			}
		} else if (outputSize == "medium") {
			legendEntry <- list("(i, j, ...)" = "comparison of treatment arms 'i, j, ...' vs. control arm")
			gMax <- nrow(stageResults$separatePValues)
			if (.isTrialDesignConditionalDunnett(design)) {
				summaryFactory$addParameter(closedTestResults, 
					parameterName = "adjustedStageWisePValues",
					values = closedTestResults$conditionalErrorRate[1, ], 
					parameterCaption = paste0("Conditional error rate (", 
						paste0(1:gMax, collapse = ", "), ")"), roundDigits = digitsProbabilities, 
					smoothedZeroFormat = TRUE, 
					legendEntry = legendEntry)				
				summaryFactory$addParameter(closedTestResults, 
					parameterName = "overallAdjustedTestStatistics",
					values = closedTestResults$secondStagePValues[1, ], 
					parameterCaption = paste0("Second stage p-value (", 
						paste0(1:gMax, collapse = ", "), ")"), 
					roundDigits = digitsProbabilities + ifelse(.isTrialDesignFisher(design), 1, 0), 
					smoothedZeroFormat = !.isTrialDesignFisher(design), 
					legendEntry = legendEntry)		
			} else {
				summaryFactory$addParameter(closedTestResults, 
					parameterName = "adjustedStageWisePValues",
					values = closedTestResults$adjustedStageWisePValues[1, ], 
					parameterCaption = paste0("Adjusted stage-wise p-value (", 
						paste0(1:gMax, collapse = ", "), ")"), roundDigits = digitsProbabilities, 
					smoothedZeroFormat = TRUE, legendEntry = legendEntry)				
				summaryFactory$addParameter(closedTestResults, 
					parameterName = "overallAdjustedTestStatistics",
					values = closedTestResults$overallAdjustedTestStatistics[1, ], 
					parameterCaption = paste0("Overall adjusted test statistics (", 
						paste0(1:gMax, collapse = ", "), ")"), 
					roundDigits = digitsProbabilities - ifelse(.isTrialDesignFisher(design) || digitsProbabilities <= 1, 0, 1), 
					smoothedZeroFormat = !.isTrialDesignFisher(design), 
					legendEntry = legendEntry)		
			}
		}
	}
	
	if (multiArmEnabled) {
		summaryFactory$addParameter(closedTestResults, parameterName = "rejected", 
			parameterCaption = "Test action: reject", roundDigits = digitsGeneral)
	} else {
		if (.isTrialDesignFisher(design)) {
			summaryFactory$addParameter(stageResults, parameterName = "combFisher", 
				parameterCaption = "Fisher combination", roundDigits = 0)
		} else {
			summaryFactory$addParameter(stageResults, parameterName = "combInverseNormal", 
				parameterCaption = "Inverse normal combination", 
				roundDigits = ifelse(digitsProbabilities > 1, digitsProbabilities - 1, digitsProbabilities), 
				smoothedZeroFormat = TRUE)
		}
		summaryFactory$addParameter(analysisResults, parameterName = "testActions", 
			parameterCaption = "Test action", roundDigits = digitsGeneral)
	}

	if (!.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(analysisResults, parameterName = "conditionalRejectionProbabilities", 
			parameterCaption = "Conditional rejection probability", 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
	}	
	
	if (design$kMax > 1) {
		if (!is.null(conditionalPowerResults)) {
			summaryFactory$addParameter(conditionalPowerResults, parameterName = "nPlanned", 
				parameterCaption = "Planned sample size", roundDigits = -1) 
		} else {
			summaryFactory$addParameter(analysisResults, parameterName = "nPlanned", 
				parameterCaption = "Planned sample size", roundDigits = -1) 
		}
	}
		
	if (stageResults$isDatasetRates()) {
		piTreatmentName <- ifelse(multiArmEnabled, "piTreatments", "pi1")
		piControlName <- ifelse(multiArmEnabled, "piControl", "pi2")
		if (analysisResults$.getParameterType(piTreatmentName) == C_PARAM_GENERATED) {
			piTreatment <- analysisResults[[piTreatmentName]]
			gMax <- ifelse(multiArmEnabled, nrow(stageResults$separatePValues), length(piTreatment))
			for (i in 1:gMax) {
				summaryFactory$addParameter(analysisResults, 
					parameterName = "piTreatmentName",
					values = .getFullStagesVectorAtStage(piTreatment[i], 
						design$kMax, stageResults$stage),  
					parameterCaption = ifelse(gMax > 1, 
						paste0("Assumed rate of treatment ", i), "Assumed treatment rate"), 
					roundDigits = digitsGeneral)
			}
		}
		if (analysisResults$.getParameterType(piControlName) == C_PARAM_GENERATED) {
			summaryFactory$addParameter(analysisResults, 
				parameterName = "piControlName",
				values = .getFullStagesVectorAtStage(analysisResults[[piControlName]], 
					design$kMax, stageResults$stage), 
				parameterCaption = "Assumed control rate", roundDigits = digitsGeneral)
		}	
	} else {	
		if (analysisResults$.getParameterType("thetaH1") == C_PARAM_GENERATED) {
			summaryFactory$addParameter(analysisResults, 
				parameterName = "thetaH1",
				values = .getFullStagesVectorAtStage(analysisResults$thetaH1, 
					design$kMax, stageResults$stage), 
				parameterCaption = "Assumed effect", roundDigits = digitsGeneral)
		}
		if (stageResults$isDatasetMeans()) {
			parameterName <- ifelse(multiArmEnabled, "assumedStDevs", "assumedStDev")
			if (analysisResults$.getParameterType(parameterName) == C_PARAM_GENERATED) {
				summaryFactory$addParameter(analysisResults, 
					parameterName = parameterName,
					values = .getFullStagesVectorAtStage(analysisResults[[parameterName]], 
						design$kMax, stageResults$stage), 
					parameterCaption = "Assumed standard deviation", roundDigits = digitsGeneral)
			}
		}
	} 	
	
	if (design$kMax > 1) {
		if (!is.null(conditionalPowerResults)) {
			summaryFactory$addParameter(conditionalPowerResults, parameterName = "conditionalPower", 
				parameterCaption = "Conditional power", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		} else if (!multiArmEnabled) {
			parameterName = "conditionalPower"
			if (!is.null(analysisResults[["conditionalPowerSimulated"]]) && 
					length(analysisResults[["conditionalPowerSimulated"]]) > 0) {
				parameterName <- "conditionalPowerSimulated"
			}
			summaryFactory$addParameter(analysisResults, parameterName = parameterName, 
				parameterCaption = "Conditional power", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
	}
		
	summaryFactory$addParameter(analysisResults, 
		parameterName = c("repeatedConfidenceIntervalLowerBounds", "repeatedConfidenceIntervalUpperBounds"), 
		parameterCaption = "Repeated confidence interval", roundDigits = digitsGeneral)
	
	summaryFactory$addParameter(analysisResults, parameterName = "repeatedPValues", 
		parameterCaption = "Repeated p-value", roundDigits = digitsProbabilities)
	
	if (!multiArmEnabled) {
		summaryFactory$addParameter(analysisResults, parameterName = "finalPValues", 
			parameterCaption = "Final p-value", roundDigits = digitsProbabilities)
		summaryFactory$addParameter(analysisResults, 
			parameterName = c("finalConfidenceIntervalLowerBounds", "finalConfidenceIntervalUpperBounds"), 
			parameterCaption = "Final confidence interval", roundDigits = digitsGeneral)
		summaryFactory$addParameter(analysisResults, parameterName = "medianUnbiasedEstimates", 
			parameterCaption = "Median unbiased estimate", roundDigits = digitsGeneral)
	}
	
	return(summaryFactory)
}

.getFullStagesVectorAtStage <- function(value, kMax, stage) {
	if (is.matrix(value) && nrow(value) > 1 && ncol(value) == 1) {
		x <- matrix(rep(NA_real_, nrow(value) * kMax), nrow = nrow(value), ncol = kMax)
		for (i in 1:nrow(value)) {
			x[i, stage] <- value[i, 1]
		}
	} else {
		x <- rep(NA_real_, kMax)
		x[stage] <- value
		
	}
	return(x)
}

.getSummaryDigits <- function(digits = NA_integer_) {
	if (is.na(digits)) {
		digits <- as.integer(getOption("rpact.summary.digits", 3))
	}
	.assertIsSingleInteger(digits, "digits", validateType = FALSE, naAllowed = TRUE)
	.assertIsInClosedInterval(digits, "digits", lower = -1, upper = 12, naAllowed = TRUE)
	
	digitsSampleSize <- 1
	if (digits > 0) {
		digitsGeneral <- digits
		digitsProbabilities <- NA_integer_
		tryCatch({
			digitsProbabilities <- as.integer(getOption("rpact.summary.digits.probs", digits + 1))
		}, warning = function(e) {
		})
		if (is.na(digitsProbabilities)) {
			digitsProbabilities <- digits + 1
		}
		.assertIsSingleInteger(digitsProbabilities, "digitsProbabilities", validateType = FALSE, naAllowed = FALSE)
		.assertIsInClosedInterval(digitsProbabilities, "digitsProbabilities", lower = -1, upper = 12, naAllowed = FALSE)
	} else {
		digitsSampleSize <- digits
		digitsGeneral <- digits
		digitsProbabilities <- digits
	}
	return(list(
		digits = digits,
		digitsSampleSize = digitsSampleSize,
		digitsGeneral = digitsGeneral,
		digitsProbabilities = digitsProbabilities
	))
}

.getSummaryValuesInPercent <- function(values, percentFormatEnabled = TRUE, digits = 1) {
	if (!percentFormatEnabled) {
		return(as.character(round(values, digits + 2)))
	}
	return(paste0(round(100 * values, digits), "%"))
}

.addDesignInformationToSummary <- function(design, object, summaryFactory) {
	if (design$kMax > 1) {
		summaryFactory$addItem("Stage", c(1:design$kMax))
		
		if (.isTrialDesignConditionalDunnett(design)) {
			summaryFactory$addItem("Fixed information at interim", 
				.getSummaryValuesInPercent(design$informationAtInterim, FALSE))
		} else {
			informationRatesCaption <- ifelse(inherits(object, "SimulationResults") || 
				inherits(object, "AnalysisResults"), "Fixed weight", "Information")
			
			if (inherits(object, "SimulationResults") || inherits(object, "AnalysisResults")) {
				if (.isTrialDesignFisher(design)) {
					weights <- .getWeightsFisher(design)	
				} else {
					weights <- .getWeightsInverseNormal(design)
				}
				summaryFactory$addItem(informationRatesCaption, .getSummaryValuesInPercent(weights, FALSE))
			} else {
				summaryFactory$addItem(paste0(informationRatesCaption, 
					ifelse(inherits(object, "SimulationResults"), "", " rate")), 
					.getSummaryValuesInPercent(design$informationRates))
			}
		}
	} else {
		summaryFactory$addItem("Stage", "Fixed")
	}
}

#
# Main function for creating a summary of a design or design plan 
#
.createDesignPlanSummary <- function(object, digits = NA_integer_) {
	designPlan <- NULL
	if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
		design <- object$.design
		designPlan <- object
	}
	else if (.isTrialDesign(object)) {
		design <- object
	}
	else {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'object' must be a valid design, design plan, ", 
			"or simulation result object (is class ", class(object), ")")
	}
	
	digitSettings <- .getSummaryDigits(digits)
	digits <- digitSettings$digits
	digitsSampleSize <- digitSettings$digitsSampleSize
	digitsGeneral <- digitSettings$digitsGeneral
	digitsProbabilities <- digitSettings$digitsProbabilities
	
	outputSize <- getOption("rpact.summary.output.size", C_SUMMARY_OUTPUT_SIZE_DEFAULT)
	
	intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
	.assertIsValidSummaryIntervalFormat(intervalFormat)
	
	designCharacteristics <- NULL
	probsH0 <- NULL
	probsH1 <- NULL
	if (.isTrialDesignInverseNormalOrGroupSequential(design) && design$kMax > 1)  {
		designCharacteristics <- getDesignCharacteristics(design)
		probsH0 <- getPowerAndAverageSampleNumber(design, theta = 0, nMax = designCharacteristics$shift) 
		probsH1 <- getPowerAndAverageSampleNumber(design, theta = 1, nMax = designCharacteristics$shift) 
	}
	if (!is.null(designPlan) && design$kMax > 1) {
		if (!is.null(designPlan[["rejectPerStage"]])) {
			probsH1 <- list(
				earlyStop = designPlan$rejectPerStage[1:(design$kMax - 1), ] + designPlan$futilityPerStage,
				rejectPerStage = designPlan$rejectPerStage,
				futilityPerStage = designPlan$futilityPerStage
			)
			numberOfVariants <- 1
			if (inherits(designPlan, "ParameterSet")) {
				parameterNames <- designPlan$.getVisibleFieldNamesOrdered()
				numberOfVariants <- designPlan$.getMultidimensionalNumberOfVariants(parameterNames)
			}
			if (numberOfVariants > 1 && is.matrix(probsH1$earlyStop) && ncol(probsH1$earlyStop) == 1) {
				probsH1$earlyStop <- matrix(rep(probsH1$earlyStop, numberOfVariants), ncol = numberOfVariants)
				probsH1$rejectPerStage <- matrix(rep(probsH1$rejectPerStage, numberOfVariants), ncol = numberOfVariants)
				probsH1$futilityPerStage <- matrix(rep(probsH1$futilityPerStage, numberOfVariants), ncol = numberOfVariants)
			}
		} 
	}
	
	summaryFactory <- SummaryFactory(object = object, intervalFormat = intervalFormat)
	
	.addDesignInformationToSummary(design, object, summaryFactory)
	
	if (!.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(design, parameterName = "criticalValues", 
			parameterCaption = ifelse(.isTrialDesignFisher(design),
				"Efficacy boundary (p product scale)", "Efficacy boundary (z-value scale)"), 
			roundDigits = digitsGeneral)
	}
	
	if (.isTrialDesignFisher(design)) {
		if (any(design$alpha0Vec < 1)) {
			summaryFactory$addParameter(design, parameterName = "alpha0Vec", 
				parameterCaption = "Futility boundary (separate p-value scale)", 
				roundDigits = digitsGeneral)
		}
	} else if (!.isTrialDesignConditionalDunnett(design)) {
		if (any(design$futilityBounds > C_FUTILITY_BOUNDS_DEFAULT)) {
			summaryFactory$addParameter(design, parameterName = "futilityBounds", 
				parameterCaption = "Futility boundary (z-value scale)", 
				roundDigits = digitsGeneral)
		}
	}
	
	if (!is.null(designPlan) && grepl("SimulationResultsMultiArm", class(designPlan))) {
		
		# simulation multi-arm #1:rejectAtLeastOne per mu_max
		summaryFactory$addParameter(designPlan, parameterName = "rejectAtLeastOne", 
			parameterCaption = "Reject at least one", roundDigits = digitsProbabilities, 
			smoothedZeroFormat = TRUE, transpose = TRUE,
			legendEntry = list("(i)" = "treatment arm i"))
	
		# simulation multi-arm #2: rejectedArmsPerStage
		if (outputSize == "large") {
			.addSimulationMultiArmArrayParameter(designPlan, 
				parameterName = "rejectedArmsPerStage", 
				parameterCaption = ifelse(design$kMax == 1, "Rejected arms", "Rejected arms per stage"), 
				designPlan, summaryFactory, roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
		
		# simulation multi-arm #4: successPerStage 
		summaryFactory$addParameter(designPlan, parameterName = "successPerStage", 
			parameterCaption = "Success per stage", 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE, transpose = TRUE)
		
		# simulation multi-arm #3: futilityPerStage
		if (any(designPlan$futilityPerStage != 0)) {
			summaryFactory$addParameter(designPlan, parameterName = "futilityPerStage", 
				parameterCaption = "Exit probability for futility", # (under H1) 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE, transpose = TRUE)
		}
		
		if (grepl("Survival", class(designPlan))) {
			summaryFactory$addParameter(designPlan, parameterName = "expectedNumberOfEvents", 
				parameterCaption = "Expected number of events", 
				roundDigits = digitsSampleSize, transpose = TRUE)
		} else {
			summaryFactory$addParameter(designPlan, parameterName = "expectedNumberOfSubjects", 
				parameterCaption = "Expected number of subjects", 
				roundDigits = digitsSampleSize, transpose = TRUE)
		}
	
		# simulation multi-arm #5: earlyStop per mu_max
		if (outputSize %in% c("medium", "large")) {
			summaryFactory$addParameter(designPlan, parameterName = "earlyStop", 
				parameterCaption = "Overall exit probability", # (under H1) 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE, transpose = TRUE)
		}
		
		# simulation multi-arm #6: sampleSizes
		if (outputSize %in% c("medium", "large")) {
			if (grepl("Survival", class(designPlan))) {
				parameterName <- "eventsPerStage"
				parameterCaption <- "Cumulative number of events"
			} else {
				parameterName <- "sampleSizes"
				parameterCaption <- "Stagewise number of subjects"
			}
			.addSimulationArrayToSummary(designPlan, parameterName, parameterCaption, summaryFactory, digitsSampleSize) 
		}
		
		if (outputSize == "large") {
			if (inherits(designPlan, "SimulationResultsMultiArmSurvival")) {
				.addSimulationArrayToSummary(
					designPlan = designPlan, 
					parameterName = "singleNumberOfEventsPerStage", 
					parameterCaption = "Single number of events", 
					summaryFactory = summaryFactory,
					digitsSampleSize = digitsSampleSize) 
			}
		}
		
		# simulation multi-arm #7: selectedArms
		if (outputSize %in% c("medium", "large")) {
			.addSimulationMultiArmArrayParameter(
				parameterSet = designPlan, 
				parameterName = "selectedArms", 
				parameterCaption = "Selected arms", 
				designPlan = designPlan, summaryFactory = summaryFactory, 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
		
		# simulation multi-arm #8: numberOfActiveArms
		if (outputSize %in% c("medium", "large")) {
			summaryFactory$addParameter(designPlan, parameterName = "numberOfActiveArms", 
				parameterCaption = "Number of active arms",
				roundDigits = digitsGeneral, transpose = TRUE)
		}
		
		if (outputSize == "large") {
			summaryFactory$addParameter(designPlan, parameterName = "conditionalPowerAchieved", 
				parameterCaption = "Conditional power (achieved)",
				roundDigits = digitsProbabilities, transpose = TRUE)
		}
	}
	
	if (!is.null(designPlan) && !grepl("SimulationResultsMultiArm", class(designPlan))) {
		parameterName <- "rejectPerStage"
		if (design$kMax == 1) {
			parameterName <- "overallReject"
		}
		if (any(!is.na(designPlan[[parameterName]]))) {
			summaryFactory$addParameter(designPlan, parameterName = parameterName, 
				parameterCaption = ifelse(design$kMax == 1, "Power", "Overall power"), 
				roundDigits = digitsProbabilities, cumsumEnabled = TRUE, smoothedZeroFormat = TRUE)
		}
	}
	
	if (!is.null(designPlan) && !grepl("MultiArm", class(designPlan))) {
		if (inherits(designPlan, "SimulationResults")) {
			summaryFactory$addParameter(designPlan, parameterName = "expectedNumberOfSubjects", 
				parameterCaption = "Expected number of subjects", 
				roundDigits = digitsSampleSize, transpose = TRUE)
			
			parameterName1 <- ifelse(grepl("Survival", class(designPlan)), "numberOfSubjects", "sampleSizes")
			parameterName2 <- "eventsPerStage"
		} else {
			if (design$kMax == 1 && (designPlan$.isSampleSizeObject() || 
					.isTrialDesignPlanMeans(designPlan) || .isTrialDesignPlanRates(designPlan))) {
				parameterName1 <- "nFixed"
				parameterName2 <- "eventsFixed"
			} else if (design$kMax == 1 && designPlan$.isPowerObject()) {
				parameterName1 <- "expectedNumberOfSubjects"
				parameterName2 <- "expectedNumberOfEvents"
			} else {
				parameterName1 <- "numberOfSubjects"
				parameterName2 <- "eventsPerStage"
			}
		}
		
		if (outputSize %in% c("medium", "large")) {
			subjectsCaption <- ifelse(design$kMax > 1 && inherits(designPlan, "SimulationResults") && 
					!grepl("Survival", class(designPlan)), "Stagewise number of subjects", "Number of subjects")
			summaryFactory$addParameter(designPlan, parameterName = parameterName1, 
				parameterCaption = subjectsCaption, roundDigits = digitsSampleSize)
		}
		
		if (!is.null(designPlan[["futilityPerStage"]]) && !any(is.na(designPlan[["futilityPerStage"]])) && 
				any(designPlan$futilityPerStage != 0) && any(designPlan$futilityPerStage > 1e-08)) {
			summaryFactory$addParameter(designPlan, parameterName = "futilityPerStage", 
				parameterCaption = "Exit probability for futility", # (under H1) 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE) 
		}
		
		if (grepl("Survival", class(designPlan))) {
			
			if (design$kMax > 1 && !(inherits(designPlan, "TrialDesignPlanSurvival") && designPlan$.isSampleSizeObject())) {			
				summaryFactory$addParameter(designPlan, parameterName = "expectedNumberOfEvents", 
					parameterCaption = "Expected number of events", 
					roundDigits = digitsSampleSize, transpose = TRUE)
			}
			
			if (outputSize %in% c("medium", "large")) {
				summaryFactory$addParameter(designPlan, parameterName = parameterName2, 
					parameterCaption = ifelse(design$kMax == 1, 
						"Number of events", "Cumulative number of events"), ceilingEnabeld = TRUE, cumsumEnabled = FALSE)
			}
			
			if (outputSize == "large") {
				summaryFactory$addParameter(designPlan, parameterName = "analysisTime", 
					parameterCaption = "Analysis time", roundDigits = digitsSampleSize)
			}
			
			summaryFactory$addParameter(designPlan, parameterName = "studyDuration", 
				parameterCaption = "Expected study duration", 
				roundDigits = digitsSampleSize, smoothedZeroFormat = TRUE, transpose = TRUE) 
		}
	}
	
	if (!is.null(designPlan) && !is.null(designPlan[["allocationRatioPlanned"]]) && 
		length(unique(designPlan$allocationRatioPlanned)) > 1) {
		summaryFactory$addParameter(designPlan, parameterName = "allocationRatioPlanned", 
			parameterCaption = "Optimum allocation ratio", roundDigits = digitsGeneral)
	}
	
	if (design$kMax > 1 && !inherits(designPlan, "SimulationResults") && 
			!.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(design, parameterName = "alphaSpent", 
			parameterCaption = "Cumulative alpha spent", 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
	}
	
	if (!is.null(designPlan)) {
		if (!grepl("SimulationResultsMultiArm", class(designPlan))) {
			if (outputSize == "large" && inherits(designPlan, "SimulationResults")) {
				summaryFactory$addParameter(designPlan, parameterName = "conditionalPowerAchieved", 
					parameterCaption = "Conditional power (achieved)",
					roundDigits = digitsProbabilities)
			}
		}
	} else if (!is.null(designCharacteristics)) {
		summaryFactory$addParameter(designCharacteristics, parameterName = "power", 
			parameterCaption = ifelse(design$kMax == 1, "Power", "Overall power"), 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
	}
	
	if (.isTrialDesignConditionalDunnett(design)) {
		summaryFactory$addParameter(design, parameterName = "alpha", 
			parameterCaption = "Significance level", roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
	}
	else if (!inherits(designPlan, "SimulationResults")) {
		summaryFactory$addParameter(design, parameterName = "stageLevels", 
			twoSided = design$sided == 2,
			parameterCaption = paste0(ifelse(design$sided == 2, "Two", "One"), "-sided local significance level"), 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
	}
	
	if (!is.null(designPlan) && .isTrialDesignPlan(designPlan)) {
		if (!is.null(designPlan[["normalApproximation"]]) && 
				length(designPlan$normalApproximation) == 1 && !designPlan$normalApproximation) {
			legendEntry <- list("(t)" = "treatment effect scale")
		} else {
			legendEntry <- list("(t)" = "approximate treatment effect scale")
		}
		
		if (ncol(designPlan$criticalValuesEffectScale) > 0) {
			summaryFactory$addParameter(designPlan, parameterName = "criticalValuesEffectScale", 
				parameterCaption = "Efficacy boundary (t)", 
				roundDigits = digitsGeneral, legendEntry = legendEntry)
		} else if (ncol(designPlan$criticalValuesEffectScaleUpper) > 0) {
			if (as.logical(getOption("rpact.summary.enforceIntervalView", FALSE))) {
				summaryFactory$addParameter(designPlan, 
					parameterName = c("criticalValuesEffectScaleLower", "criticalValuesEffectScaleUpper"), 
					parameterCaption = "Efficacy boundary (t)", 
					roundDigits = digitsGeneral, legendEntry = legendEntry)
			} else {
				summaryFactory$addParameter(designPlan, 
					parameterName = "criticalValuesEffectScaleLower", 
					parameterCaption = "Lower efficacy boundary (t)", 
					roundDigits = digitsGeneral, legendEntry = legendEntry)
				summaryFactory$addParameter(designPlan, 
					parameterName = "criticalValuesEffectScaleUpper", 
					parameterCaption = "Upper efficacy boundary (t)", 
					roundDigits = digitsGeneral, legendEntry = legendEntry)
			}
		}
	}
	
	if (!is.null(designPlan) && .isTrialDesignPlan(designPlan)) {
		if (ncol(designPlan$futilityBoundsEffectScale) > 0 && 
			!all(is.na(designPlan$futilityBoundsEffectScale))) {
			summaryFactory$addParameter(designPlan, 
				parameterName = "futilityBoundsEffectScale", 
				parameterCaption = "Futility boundary (t)", 
				roundDigits = digitsGeneral, legendEntry = legendEntry)
		}
	}
	
	if (!is.null(designPlan) && !inherits(designPlan, "SimulationResults") && 
			!is.null(probsH1) && !is.null(probsH0) && design$kMax > 1) {
		
		probsH0$earlyStop <- matrix(probsH0$earlyStop[1:(design$kMax - 1), 1], ncol = 1)
		probsH0$rejectPerStage <- matrix(probsH0$rejectPerStage[1:(design$kMax - 1), 1], ncol = 1)
		
		if (is.matrix(probsH1$rejectPerStage)) { 
			if (!is.null(designPlan) && design$kMax > 1 && .isTrialDesignPlan(designPlan) && designPlan$.isSampleSizeObject()) {
				probsH1$rejectPerStage <- probsH1$rejectPerStage[1:(design$kMax - 1), 1]
			} else {
				probsH1$rejectPerStage <- matrix(probsH1$rejectPerStage[1:(design$kMax - 1), ], 
					ncol = ncol(probsH1$rejectPerStage))
			}
		} else {
			probsH1$rejectPerStage <- probsH1$rejectPerStage[1:(design$kMax - 1)]
		}
		
		if (any(design$futilityBounds > -6)) {
			if (is.matrix(probsH1$earlyStop)) {
				probsH1$earlyStop <- matrix(probsH1$earlyStop[1:(design$kMax - 1), ],
					ncol = ncol(probsH1$earlyStop))
			} else {
				probsH1$earlyStop <- probsH1$earlyStop[1:(design$kMax - 1)]
			}
			summaryFactory$addParameter(probsH0, parameterName = "earlyStop", 
				parameterCaption = "Overall exit probability (under H0)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
			x <- designPlan
			if (is.null(x)) {
				x <- design
			}
			summaryFactory$addParameter(x,
				parameterName = "earlyStop",
				values = probsH1$earlyStop, 
				parameterCaption = "Overall exit probability (under H1)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
		summaryFactory$addParameter(probsH0, parameterName = "rejectPerStage", 
			parameterCaption = "Exit probability for efficacy (under H0)", 
			roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		if (!is.null(designPlan) && (inherits(designPlan, "SimulationResults") || 
				.isTrialDesignPlan(designPlan) && designPlan$.isPowerObject())) { 
			summaryFactory$addParameter(designPlan, parameterName = "rejectPerStage", 
				values = probsH1$rejectPerStage, 
				parameterCaption = "Exit probability for efficacy (under H1)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		} else {
			summaryFactory$addParameter(probsH1, parameterName = "rejectPerStage", 
				parameterCaption = "Exit probability for efficacy (under H1)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
		
		if (any(design$futilityBounds > -6)) {
			summaryFactory$addParameter(probsH0, parameterName = "futilityPerStage", 
				parameterCaption = "Exit probability for futility (under H0)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
			x <- designPlan
			if (is.null(x)) {
				x <- design
			}
			futilityPerStage <- probsH1$futilityPerStage
			if (.isTrialDesignPlan(x) && x$.isSampleSizeObject() && ncol(futilityPerStage) > 1) {
				futilityPerStage <- futilityPerStage[, 1]
			}
			summaryFactory$addParameter(x, parameterName = "futilityPerStage", 
				values = futilityPerStage, 
				parameterCaption = "Exit probability for futility (under H1)", 
				roundDigits = digitsProbabilities, smoothedZeroFormat = TRUE)
		}
	} 
	
	return(summaryFactory)
}

.addSimulationArrayToSummary <- function(designPlan, 
		parameterName, parameterCaption, summaryFactory, digitsSampleSize) {
		
	userDefinedEffectMatrix <- designPlan$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED 
	variedParam <- .getSimulationMultiArmVariedParameter(designPlan)
	variedParamValues <- designPlan[[variedParam]]
	variedParamName <- C_PARAMETER_NAMES[[variedParam]]
	arrayData <- designPlan[[parameterName]]
	numberOfGroups <- dim(arrayData)[3]
	listItemPrefix <- getOption("rpact.summary.list.item.prefix", C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT)
	numberOfVariedParams <- dim(arrayData)[2]
	for (variedParamNumber in 1:numberOfVariedParams) {
		if (numberOfVariedParams == 1) {
			summaryFactory$addItem(parameterCaption, "")
		} else {
			if (userDefinedEffectMatrix) {
				summaryFactory$addItem(paste0(parameterCaption, " [", variedParamNumber, "]"), "",
					legendEntry = list("[j]" = "effect matrix row j (situation to consider)"))
			} else {
				summaryFactory$addItem(paste0(parameterCaption, ", ", 
					variedParamName, " = ", round(variedParamValues[variedParamNumber], 2), ""), "")
			}
		}
		for (groupNumber in 1:numberOfGroups) {
			dataPerGroupAndStage <- arrayData[, variedParamNumber, groupNumber]
			
			if ((inherits(designPlan, "SimulationResultsMultiArmSurvival") && 
					parameterName == "singleNumberOfEventsPerStage") || !grepl("Survival", class(designPlan))) {
				groupCaption <- ifelse(groupNumber == numberOfGroups, 
					paste0(listItemPrefix, "control arm"), 
					paste0(listItemPrefix, "treatment arm ", groupNumber))
			} else {
				groupCaption <- paste0(listItemPrefix, "treatment arm ", groupNumber, " vs. control")
			}
			
			summaryFactory$addParameter(designPlan, parameterName = parameterName, 
				values = dataPerGroupAndStage, parameterCaption = groupCaption, 
				roundDigits = digitsSampleSize, enforceFirstCase = TRUE)
		}
	}
}

.addSimulationMultiArmArrayParameter <- function(parameterSet, parameterName, parameterCaption,
		designPlan, summaryFactory, roundDigits, smoothedZeroFormat = FALSE) {
		
	if (is.array(parameterSet[[parameterName]]) && length(dim(parameterSet[[parameterName]])) == 3) {
		listItemPrefix <- getOption("rpact.summary.list.item.prefix", C_SUMMARY_LIST_ITEM_PREFIX_DEFAULT)
		userDefinedEffectMatrix <- parameterSet$.getParameterType("effectMatrix") == C_PARAM_USER_DEFINED 
		variedParam <- .getSimulationMultiArmVariedParameter(designPlan)
		variedParamValues <- designPlan[[variedParam]]
		variedParamName <- C_PARAMETER_NAMES[[variedParam]]
		arrayData <- parameterSet[[parameterName]]
		totalNumberOfGroups <- dim(designPlan[[ifelse(grepl("Survival", class(designPlan)), "eventsPerStage", "sampleSizes")]])[3]
		numberOfGroups <- dim(arrayData)[3]
		if (parameterName == "selectedArms" && !grepl("Survival", class(designPlan))) { # remove control group
			numberOfGroups <- numberOfGroups - 1
		}
		numberOfVariedParams <- dim(arrayData)[2]
		
		for (variedParamNumber in 1:numberOfVariedParams) {
			if (numberOfVariedParams == 1) {
				summaryFactory$addItem(parameterCaption, "")
			} else {
				if (userDefinedEffectMatrix) {
					summaryFactory$addItem(paste0(parameterCaption, " [", variedParamNumber, "]"), "",
						legendEntry = list("[j]" = "effect matrix row j (situation to consider)"))
				} else {
					summaryFactory$addItem(paste0(parameterCaption, ", ", 
							variedParamName, " = ", round(variedParamValues[variedParamNumber], 2), ""), "")
				}
			}

			for (groupNumber in 1:numberOfGroups) {
				dataPerGroupAndStage <- arrayData[, variedParamNumber, groupNumber]
				
				if (!grepl("Survival", class(designPlan)) || 
						((inherits(designPlan, "SimulationResultsMultiArmSurvival") && 
						parameterName == "singleNumberOfEventsPerStage") || !grepl("Survival", class(designPlan)))) {
					groupCaption <- ifelse(groupNumber == totalNumberOfGroups, 
						paste0(listItemPrefix, "control arm"), 
						paste0(listItemPrefix, "treatment arm ", groupNumber))
				} else {
					groupCaption <- paste0(listItemPrefix, "treatment arm ", groupNumber, " vs. control")
				}
				
				summaryFactory$addParameter(designPlan, parameterName = parameterName, 
					values = dataPerGroupAndStage, parameterCaption = groupCaption, 
					roundDigits = roundDigits, smoothedZeroFormat = smoothedZeroFormat,
					enforceFirstCase = TRUE)
			}
		}

	} else {
		data <- parameterSet[[parameterName]]
		numberOfGroups <- ncol(data)
		for (groupNumber in 1:numberOfGroups) {
			dataPerGroupAndStage <- data[, groupNumber]
			summaryFactory$addParameter(designPlan, parameterName = parameterName, 
				values = dataPerGroupAndStage, 
				parameterCaption = ifelse(groupNumber == numberOfGroups, 
					paste0(parameterCaption, ", control"), 
					paste0(parameterCaption, ", treatment ", groupNumber)
				), 
				roundDigits = roundDigits, smoothedZeroFormat = smoothedZeroFormat)
		}
	}
}

.getSimulationMultiArmVariedParameter <- function(designPlan) {
	if (!grepl("SimulationResultsMultiArm", class(designPlan))) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'designPlan' (", 
			class(designPlan), ") must be of class 'SimulationResultsMultiArm'")
	}
	if (grepl("Means", class(designPlan))) {
		return("muMaxVector")
	}
	else if (grepl("Rates", class(designPlan))) {
		return("piMaxVector")
	}
	else if (grepl("Survival", class(designPlan))) {
		return("omegaMaxVector")
	}
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'designPlan' (", 
		class(designPlan), ") must be of class 'SimulationResultsMultiArm'")
}

