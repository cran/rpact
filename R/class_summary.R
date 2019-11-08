
######################################################################################
#                                                                                    #
# -- RPACT design utilities --                                                       #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 29-10-2019                                                                   #
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


SummaryItem <- setRefClass("SummaryItem",
	fields = list(
		title = "character",
		values = "character",
		legendEntry = "list"
	),
	methods = list(
		initialize = function(...) {
			callSuper(...)
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

SummaryFactory <- setRefClass("SummaryFactory",
	contains = "ParameterSet",
	fields = list(
			object = "ParameterSet",
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
			.cat(.createSummaryObjectTitle(object), "\n\n", heading = 1,
					consoleOutputEnabled = consoleOutputEnabled)
			
			header <- .createSummaryObjectHeader(object)
			if (header != "") {
				.cat(header, "\n\n", heading = 2,
						consoleOutputEnabled = consoleOutputEnabled)
			}
			
			legendEntries <- c()
			legendEntriesUnique <- c()
			summaryItemNames <- c()
			for (summaryItem in summaryItems) {
				summaryItemNames <- c(summaryItemNames, summaryItem$title)
				if (length(summaryItem$legendEntry) > 0) {
					a <- names(summaryItem$legendEntry)
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
			
			maxValueWidth <- 1
			for (i in 1:length(summaryItems)) {
				validValues <- na.omit(summaryItems[[i]]$values)
				if (length(validValues) > 0) {
					w <- max(nchar(validValues))
					maxValueWidth <- max(maxValueWidth, w)
				}
			}
			spaceString <- paste0(rep(" ", maxValueWidth + 1), collapse = "")
			for (i in 1:length(summaryItems)) {
				summaryItemName <- summaryItemNames[i]
				values <- summaryItems[[i]]$values
				values <- trimws(values)
				indices <- !grepl("(\\])$", values)
				values[indices] <- paste0(values[indices], " ")
				values <- format(c(spaceString, values), justify = justify)[2:(length(values) + 1)]
				cat(summaryItemName, values, "\n")
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
		
		addParameter = function(parameterSet, ..., 
				parameterName = NULL, values = NULL, parameterCaption, 
				roundDigits = NA_integer_, ceilingEnabeld = FALSE, cumsumEnabled = FALSE, 
				twoSided = FALSE,
				parameterCaptionSingle = parameterCaption, legendEntry = list()) {
			
			parameterName1 <- parameterName[1]
			if (is.character(parameterName1)) {
				if (!is.null(values)) {
					warning("'values' (", .arrayToString(values), 
							") will be ignored because 'parameterName' (", parameterName1, ") is defined")
				}
				values <- parameterSet[[parameterName1]]
			}
			
			parameterName2 <- NA_character_
			values2 <- NA_real_
			if (length(parameterName) > 1) {
				parameterName2 <- parameterName[2]
				values2 <- parameterSet[[parameterName2]]
			}
			
			if (is.null(values) && is.null(parameterName1)) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'parameterName' or 'values' must be defined")
			}
			
			parameterNames <- ""
			numberOfVariants <- 1
			numberOfStages <- length(values)
			if (inherits(parameterSet, "ParameterSet")) {
				parameterNames <- parameterSet$.getVisibleFieldNamesOrdered()
				numberOfVariants <- parameterSet$.getMultidimensionalNumberOfVariants(parameterNames)
				numberOfStages <- parameterSet$.getMultidimensionalNumberOfStages(parameterNames)
			}
			
			if (twoSided) {
				values <- 2 * values
			}
			
			if ((!is.matrix(values) || ncol(values) == 1) && 
					(.isTrialDesign(parameterSet) || 
						(numberOfStages > 1 && numberOfStages == length(values)) || 
						length(values) != numberOfVariants ||
						length(values) == 1) ||
						(parameterName == "futilityBoundsEffectScale" && ncol(values) == 1)) {
				valuesToShow <- .getSummaryValuesFormatted(parameterSet, parameterName1, values, roundDigits = roundDigits, 
						ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled)
				valuesToShow <- .getInnerValues(valuesToShow)
				if (!all(is.na(values2))) {
					valuesToShow2 <- .getSummaryValuesFormatted(parameterSet, parameterName1, values2, roundDigits = roundDigits, 
							ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled)
					valuesToShow2 <- .getInnerValues(valuesToShow2)
					if (length(valuesToShow) == length(valuesToShow2) && !all(is.na(valuesToShow2))) {
						for (variantIndex in 1:length(valuesToShow)) {
							valuesToShow[variantIndex] <- sprintf(
									intervalFormat, valuesToShow[variantIndex], valuesToShow2[variantIndex])
						}
					}
				}
				addItem(parameterCaptionSingle, valuesToShow, legendEntry)
			} else {
				if (!inherits(parameterSet, "ParameterSet")) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"for varied values 'parameterSet' must be an instance of ", 
							"class 'ParameterSet' (was '", class(parameterSet), "')")
				}
				
				variedParameter <- parameterSet$.getVariedParameter(parameterNames, numberOfVariants)
				if (length(variedParameter) == 0 || variedParameter == "") {
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
				else if (grepl(" \\(1\\)$", variedParameterCaption)) {
					groups <- parameterSet[["groups"]]
					if (!is.null(groups) && length(groups) == 1 && groups == 1) {
						variedParameterCaption = sub(" \\(1\\)$", "", variedParameterCaption)
					} else {
						legendEntry[["(1)"]] <- "values of treatment arm 1"
					}
				}
				
				variedParameterValues <- round(parameterSet[[variedParameter]], 3)
				for (variantIndex in 1:numberOfVariants) {
					colValues <- .getColumnValues(values, variantIndex)
					colValues <- .getSummaryValuesFormatted(parameterSet, parameterName1, colValues, roundDigits = roundDigits, 
						ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled)
					colValues2 <- NA_real_
					if (!all(is.na(values2))) {
						colValues2 <- .getColumnValues(values2, variantIndex)
						colValues2 <- .getSummaryValuesFormatted(parameterSet, parameterName2, colValues2, 
							roundDigits = roundDigits, ceilingEnabeld = ceilingEnabeld, cumsumEnabled = cumsumEnabled)
					}
					if (length(colValues) == length(colValues2) && !all(is.na(colValues2))) {
						for (valueIndex in 1:length(colValues)) {
							colValues[valueIndex] <- sprintf(intervalFormat, 
								colValues[valueIndex], colValues2[valueIndex])
						}
					}
					addItem(paste0(parameterCaption, ", ", 
						variedParameterCaption," = ", variedParameterValues[variantIndex]), colValues, legendEntry)
				}
			}
		},
		
		.getInnerValues = function(values) {
			if (!is.matrix(values)) {
				return(values)
			}
			
			if (nrow(values) == 1 && ncol(values) == 1) {
				return(values[1, 1])
			} 
			
			return(values[, 1])
		},
		
		.getColumnValues = function(values, variantIndex) {
			if (length(values) <= 1 && !is.matrix(values)) {
				colValues <- values
			} else if (is.matrix(values)) {
				if (nrow(values) == 1 && ncol(values) == 1) {
					colValues <- values[1, 1]
				} else if (ncol(values) == 1) {
					colValues <- values[variantIndex, 1]
				} else {
					colValues <- values[, variantIndex]
				}
			} else {
				colValues <- values[variantIndex]
			}
			return(colValues)
		}
	)
)

.formatSummaryValues <- function(values, digits) {
	if (sum(is.na(values)) == length(values)) {
		return(values)
	}
	
	threshold <- 10^-digits
	text <- "<0."
	for (i in 1:(digits - 1)) {
		text <- paste0(text, "0")
	}
	text <- paste0(text, "1")
	
	indices <- (!is.na(values) & values > 1e-24 & abs(values) < threshold)
	values[!is.na(values) & !indices] <- round(values[!is.na(values) & !indices], digits)
	if (sum(indices) > 0) {
		values[indices] <- threshold
		formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits)
		formattedValue[indices] <- text
	} else {
		formattedValue <- .getFormattedValue(values, digits = digits, nsmall = digits)
		formattedValue <- format(formattedValue)
	}
	
	if (as.logical(getOption("rpact.summary.trim.zeroes", TRUE))) {
		zeroes <- grepl("^0\\.0*$", formattedValue)
		if (sum(zeroes) > 0) {
			formattedValue[zeroes] <- "0"
		}
	}
	
	return(formattedValue)
}

.getFormattedSimpleBoundarySummaryValues <- function(values, digits = NA_integer_) {
	if (is.na(digits) || digits < 1) {
		digits <- 3
	}
	return(.formatSummaryValues(values = values, digits = digits))
}

.getSummaryValuesFormatted <- function(fieldSet, parameterName, values, 
		roundDigits = NA_integer_, ceilingEnabeld = FALSE, cumsumEnabled = FALSE) {
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
			formatFunctionName <- NULL
			if (!is.null(parameterName) && length(parameterName) == 1 && 
					!is.na(parameterName) && !is.na(roundDigits) && roundDigits < 1 && inherits(fieldSet, "FieldSet")) {
				formatFunctionName <- fieldSet$.parameterFormatFunctions[[parameterName]]
			}
			if (!is.null(formatFunctionName)) {
				values <- eval(call(formatFunctionName, values))
			} else {
				values <- .getFormattedSimpleBoundarySummaryValues(values, digits = roundDigits)
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
	else if (.isTrialDesign(object)) {
		design <- object
	}
	if (!is.null(design)) {
		return(.createSummaryDesignTitle(design, designPlan)) 
	}
	return("")
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
	} 
	else if (kMax > 1) {
		title <- paste0(title, " (", design$.toString(startWithUpperCase = FALSE), ")")
	}
	
	return(title)
}

.createSummaryObjectHeader <- function(object) {
	design <- NULL
	designPlan <- NULL
	if (.isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
		design <- object$.design
		designPlan <- object
	}
	else if (.isTrialDesign(object)) {
		design <- object
	}
	if (!is.null(design)) {
		return(.createSummaryDesignHeader(design, designPlan)) 
	}
	return("")
}

.addOptimumAllocationRatioToHeader <- function(designPlan, header) {
	if (!.isTrialDesignPlanSurvival(designPlan) && 
			!grepl("Simulation", class(designPlan)) && designPlan$groups == 1) {
		return(header)
	}
	
	prefix <- ""
	if (!is.null(designPlan[["optimumAllocationRatio"]]) && 
			length(designPlan$optimumAllocationRatio) == 1 && 
			designPlan$optimumAllocationRatio) {
		if (length(unique(designPlan$allocationRatioPlanned)) > 1) {
			return(paste0(header, ", optimum allocation ratio"))
		}
		prefix <- "optimum "
	}
	
	return(paste0(header, ", ", prefix, "allocation ratio = ", 
		round(unique(designPlan$allocationRatioPlanned), 3)))
}

.createSummaryDesignHeader <- function(design, designPlan) {
	if (is.null(designPlan)) {
		return("")
	}
	
	header <- ""
	if (design$kMax == 1) {
		header <- paste0(header, "Fixed sample analysis.\n")
	} else {
		header <- paste0(header, "Sequential analysis with a maximum of ", design$kMax, " looks")
		header <- paste0(header, " (", design$.toString(startWithUpperCase = FALSE), ").\n")
	}
	
	header <- paste0(header, "The ", ifelse(inherits(designPlan, "SimulationResults") || 
		designPlan$.isPowerObject(), "results were ", "sample size was "))
	header <- paste0(header, ifelse(inherits(designPlan, "SimulationResults"), "simulated", "calculated"))
	header <- paste0(header, " for a ")
	if (grepl("Means", class(designPlan))) {
		if (designPlan$groups == 1) {
			header <- paste0(header, "one-sample t-test")
		}
		else if (designPlan$groups == 2) {
			header <- paste0(header, "two-sample t-test")
		}
	}
	else if (grepl("Rates", class(designPlan))) {
		if (designPlan$groups == 1) {
			header <- paste0(header, "one-sample test for rates")
		}
		else if (designPlan$groups == 2) {
			header <- paste0(header, "two-sample test for rates")
		}
	}
	else if (grepl("Survival", class(designPlan))) {
		header <- paste0(header, "two-sample logrank test")
	}
	
	if (nchar(header) > 0) {
		# header <- paste0(header, " with ")
	}
	if (design$sided == 1) {
		header <- paste0(header, " (one-sided)")
	} else {
		header <- paste0(header, " (two-sided)")
	}
	if (grepl("Means", class(designPlan)) && (.isTrialDesignInverseNormalOrGroupSequential(design) || 
			inherits(designPlan, "SimulationResults"))) {
		alternativeText <- "alternative as specified"
		if (length(designPlan$alternative) == 1) {
			alternativeText <- paste0("alternative = ", round(designPlan$alternative, 3))
		}
		header <- paste0(header, ",", "\n", alternativeText, ", standard deviation = ", round(designPlan$stDev, 3))
		header <- .addOptimumAllocationRatioToHeader(designPlan, header)
	}
	else if (grepl("Rates", class(designPlan)) && (.isTrialDesignInverseNormalOrGroupSequential(design) || 
			inherits(designPlan, "SimulationResults"))) {
		if (designPlan$groups == 1) {
			if (length(designPlan$pi1) == 1) {
				treatmentRateText <- paste0("treatment rate pi = ", round(designPlan$pi1, 3))
			} else {
				treatmentRateText <- "treatment rate (pi) as specified"
			}
			header <- paste0(header, ",", "\n", treatmentRateText, ", H0: pi = ", designPlan$thetaH0)
		} else {
			if (length(designPlan$pi1) == 1) {
				treatmentRateText <- paste0("treatment rate pi (1) = ", round(designPlan$pi1, 3))
			} else {
				treatmentRateText <- "treatment rate pi (1) as specified"
			}
			header <- paste0(header, ",", "\n", treatmentRateText, ", control rate pi (2) = ", round(designPlan$pi2, 3))
			header <- .addOptimumAllocationRatioToHeader(designPlan, header)
		}
	}
	else if (grepl("Survival", class(designPlan)) && (.isTrialDesignInverseNormalOrGroupSequential(design) || 
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
		paramName <- "treatment pi (1)"
		if (userDefinedParam == "lambda1") {
			paramName <- "treatment lambda (1)"
		}
		else if (userDefinedParam == "median1") {
			paramName <- "treatment median (1)"
		}
		else if (userDefinedParam == "hazardRatio") {
			paramName <- "hazard ratio"
		}
		
		if (length(designPlan[[userDefinedParam]]) == 1) {
			s <- ifelse(grepl("\\)$", paramName), " = ", " ")
			header <- paste0(header, ",", "\n", paramName, s, round(designPlan[[userDefinedParam]], 3))
		} else {
			header <- paste0(header, ",", "\n", paramName, " as specified")
		}
		if (userDefinedParam %in% c("hazardRatio", "pi1") && 
				(designPlan$.getParameterType("pi2") == C_PARAM_USER_DEFINED ||
				designPlan$.getParameterType("pi2") == C_PARAM_DEFAULT_VALUE) &&
				length(designPlan$pi2) == 1) {
			header <- paste0(header, ", control pi (2) = ", round(designPlan$pi2, 3))
		}
		else if (userDefinedParam %in% c("hazardRatio", "lambda1") && 
				(designPlan$.getParameterType("lambda2") == C_PARAM_USER_DEFINED ||
				designPlan$.getParameterType("lambda2") == C_PARAM_DEFAULT_VALUE) &&
				length(designPlan$lambda2) == 1) {
			header <- paste0(header, ", control lambda (2) = ", round(designPlan$lambda2, 3))
		}
		else if (userDefinedParam %in% c("hazardRatio", "median1") && 
				(designPlan$.getParameterType("median2") == C_PARAM_USER_DEFINED ||
				designPlan$.getParameterType("median2") == C_PARAM_GENERATED) &&
				length(designPlan$median2) == 1) {
			header <- paste0(header, ", control median (2) = ", round(designPlan$median2, 3))
		}
		else if (designPlan$.piecewiseSurvivalTime$piecewiseSurvivalEnabled) {
			header <- paste0(header, ", piecewise survival distribution")
		}
		header <- .addOptimumAllocationRatioToHeader(designPlan, header)	
	}
	if (!inherits(designPlan, "SimulationResults") && designPlan$.isSampleSizeObject()) {
		header <- paste0(header, ", and power ", paste0(round(100 * (1 - design$beta), 1), "%"))
	}
	header <- paste0(header, ".")
	return(header)
}

.createSummary <- function(object, digits = NA_integer_) {
	if (.isTrialDesign(object) || .isTrialDesignPlan(object) || inherits(object, "SimulationResults")) {
		.createDesignPlanSummary(object, digits = digits)
	} else {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "function 'summary' not implemented yet for class ", class(object))
	}
}

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
	
	if (is.na(digits)) {
		digits <- as.numeric(getOption("rpact.summary.digits", 3))
	}
	
	.assertIsSingleInteger(digits, "digits", validateType = FALSE, naAllowed = TRUE)
	.assertIsInClosedInterval(digits, "digits", lower = -1, upper = 12, naAllowed = TRUE)
	
	intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
	
	digitsSampleSize <- 1
	if (!is.na(digits)) {
		if (digits > 0) {
			digitsGeneral <- digits
			digitsProbabilities <- digits
			if (!as.logical(getOption("rpact.summary.digits.fixed", FALSE))) {
				digitsProbabilities <- digitsProbabilities + 1
			}
		} else {
			digitsGeneral <- -1
			digitsProbabilities <- -1
		}
	} else {
		digitsGeneral <- NA_integer_
		digitsProbabilities <- NA_integer_
	}
	
	designCharacteristics <- NULL
	probsH0 <- NULL
	probsH1 <- NULL
	if (.isTrialDesignInverseNormalOrGroupSequential(design) && design$kMax > 1)  {
		designCharacteristics <- getDesignCharacteristics(design)
		probsH0 <- getPowerAndAverageSampleNumber(design, theta = 0, nMax = designCharacteristics$shift) 
		probsH1 <- getPowerAndAverageSampleNumber(design, theta = 1, nMax = designCharacteristics$shift) 
	}
	if (!is.null(designPlan) && design$kMax > 1) {
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
	
	summaryFactory <- SummaryFactory(object = object, intervalFormat = intervalFormat)
	
	if (design$kMax > 1) {
		summaryFactory$addItem("Stage", c(1:design$kMax))
		summaryFactory$addItem("Information rate", paste0(round(100 * design$informationRates, 1), "%"))
	}
	
	summaryFactory$addParameter(design, parameterName = "criticalValues", 
		parameterCaption = ifelse(.isTrialDesignFisher(design),
			"Efficacy boundary (p product scale)", "Efficacy boundary (z-value scale)"), 
		roundDigits = digitsGeneral)
	
	if (.isTrialDesignFisher(design)) {
		if (any(design$alpha0Vec < 1)) summaryFactory$addParameter(design, parameterName = "alpha0Vec", 
				parameterCaption = "Futility boundary (separate p-value scale)", 
				roundDigits = digitsGeneral)
	} else {
		if (any(design$futilityBounds > - 6)) summaryFactory$addParameter(design, parameterName = "futilityBounds", 
				parameterCaption = "Futility boundary (z-value scale)", 
				roundDigits = digitsGeneral)
	}
	
	if (!is.null(designPlan)) {
		if (inherits(designPlan, "SimulationResults")) {
			parameterName1 <- ifelse(grepl("Survival", class(designPlan)), "numberOfSubjects", "sampleSizes")
			parameterName2 <- "eventsPerStage"
		} else {
			if (design$kMax == 1 && designPlan$.isSampleSizeObject()) {
				parameterName1 <- "nFixed"
				parameterName2 <- "eventsFixed"
			} else {
				parameterName1 <- "numberOfSubjects"
				parameterName2 <- "eventsPerStage"
			}
		}
		
		subjectsCaption <- ifelse(design$kMax > 1 && inherits(designPlan, "SimulationResults") && 
			!grepl("Survival", class(designPlan)), "Stagewise number of subjects", "Number of subjects")
		summaryFactory$addParameter(designPlan, parameterName = parameterName1, 
			parameterCaption = subjectsCaption,
			ceilingEnabeld = TRUE, cumsumEnabled = FALSE)

		if (grepl("Survival", class(designPlan))) {
			summaryFactory$addParameter(designPlan, parameterName = parameterName2, 
				parameterCaption = ifelse(design$kMax == 1, 
					"Number of events", "Cumulative number of events"), roundDigits = digitsSampleSize)
			summaryFactory$addParameter(designPlan, parameterName = "analysisTime", 
				parameterCaption = "Analysis time", roundDigits = digitsSampleSize)
		}
	}
	
	
	if (!is.null(designPlan) && !is.null(designPlan[["allocationRatioPlanned"]]) && 
		length(unique(designPlan$allocationRatioPlanned)) > 1) {
		summaryFactory$addParameter(designPlan, parameterName = "allocationRatioPlanned", 
			parameterCaption = "Optimum allocation ratio", roundDigits = digitsGeneral)
	}
	
	if (design$kMax > 1 && !inherits(designPlan, "SimulationResults")) {
		summaryFactory$addParameter(design, parameterName = "alphaSpent", 
			parameterCaption = "Cumulative alpha spent", 
			roundDigits = digitsProbabilities)
	}
	
	if (!is.null(designPlan)) {
		parameterName <- "rejectPerStage"
		if (design$kMax == 1) {
			parameterName <- "overallReject"
		}
		if (any(!is.na(designPlan[[parameterName]]))) {
			powerText <- ifelse(design$kMax == 1, "Power", "Cumulative power")
			if (inherits(designPlan, "SimulationResults")) {
				powerText <- ifelse(design$kMax == 1, "Simulated power", "Simulated cumulative power")
			}
			summaryFactory$addParameter(designPlan, parameterName = parameterName, 
				parameterCaption = powerText, 
				roundDigits = digitsProbabilities, cumsumEnabled = TRUE)
		}
	} else if (!is.null(designCharacteristics)) {
		summaryFactory$addParameter(designCharacteristics, parameterName = "power", 
				parameterCaption = ifelse(design$kMax == 1, "Power", "Cumulative power"), 
				roundDigits = digitsProbabilities)
	}
	
	if (!inherits(designPlan, "SimulationResults")) {
		summaryFactory$addParameter(design, parameterName = "stageLevels", 
			twoSided = design$sided == 2,
			parameterCaption = paste0(ifelse(design$sided == 2, "Two", "One"), "-sided local significance level"), 
			roundDigits = digitsProbabilities)
	}
	
	if (!is.null(designPlan) && .isTrialDesignPlan(designPlan)) {
		if (ncol(designPlan$criticalValuesEffectScale) > 0) {
			summaryFactory$addParameter(designPlan, parameterName = "criticalValuesEffectScale", 
					parameterCaption = "Efficacy boundary (t)", 
					roundDigits = digitsGeneral, legendEntry = list("(t)" = "approximate treatment effect scale"))
		} else if (ncol(designPlan$criticalValuesEffectScaleUpper) > 0) {
			if (as.logical(getOption("rpact.summary.enforceIntervalView", FALSE))) {
				summaryFactory$addParameter(designPlan, 
						parameterName = c("criticalValuesEffectScaleLower", "criticalValuesEffectScaleUpper"), 
						parameterCaption = "Efficacy boundary (t)", 
						roundDigits = digitsGeneral, legendEntry = list("(t)" = "approximate treatment effect scale"))
			} else {
				summaryFactory$addParameter(designPlan, 
						parameterName = "criticalValuesEffectScaleLower", 
						parameterCaption = "Lower efficacy boundary (t)", 
						roundDigits = digitsGeneral, legendEntry = list("(t)" = "approximate treatment effect scale"))
				summaryFactory$addParameter(designPlan, 
						parameterName = "criticalValuesEffectScaleUpper", 
						parameterCaption = "Upper efficacy boundary (t)", 
						roundDigits = digitsGeneral, legendEntry = list("(t)" = "approximate treatment effect scale"))
			}
		}
		
		if (ncol(designPlan$futilityBoundsEffectScale) > 0 && 
				!all(is.na(designPlan$futilityBoundsEffectScale))) {
			summaryFactory$addParameter(designPlan, 
					parameterName = "futilityBoundsEffectScale", 
					parameterCaption = "Futility boundary (t)", 
					roundDigits = digitsGeneral, legendEntry = list("(t)" = "approximate treatment effect scale"))
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
					parameterCaption = "Overall exit probability (under H0)", roundDigits = digitsProbabilities)
			x <- designPlan
			if (is.null(x)) {
				x <- design
			}
			summaryFactory$addParameter(x, values = probsH1$earlyStop, 
					parameterCaption = "Overall exit probability (under H1)", roundDigits = digitsProbabilities)
		}
		summaryFactory$addParameter(probsH0, parameterName = "rejectPerStage", 
				parameterCaption = "Exit probability for efficacy (under H0)", roundDigits = digitsProbabilities)
		if (!is.null(designPlan) && (inherits(designPlan, "SimulationResults") || 
					.isTrialDesignPlan(designPlan) && designPlan$.isPowerObject())) { 
			summaryFactory$addParameter(designPlan, values = probsH1$rejectPerStage, 
					parameterCaption = "Exit probability for efficacy (under H1)", 
					roundDigits = digitsProbabilities)
		} else {
			summaryFactory$addParameter(probsH1, parameterName = "rejectPerStage", 
					parameterCaption = "Exit probability for efficacy (under H1)", roundDigits = digitsProbabilities)
		}
		
		if (any(design$futilityBounds > -6)) {
			summaryFactory$addParameter(probsH0, parameterName = "futilityPerStage", 
					parameterCaption = "Exit probability for futility (under H0)", roundDigits = digitsProbabilities)
			x <- designPlan
			if (is.null(x)) {
				x <- design
			}
			summaryFactory$addParameter(x, values = probsH1$futilityPerStage, 
					parameterCaption = "Exit probability for futility (under H1)", 
					roundDigits = digitsProbabilities)
		}
	}
	summaryFactory$show()
}


