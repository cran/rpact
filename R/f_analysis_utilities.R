#:#
#:#  *Analysis of multi-arm designs with adaptive test*
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
#:#  File version: $Revision: 4359 $
#:#  Last changed: $Date: 2021-02-08 16:26:14 +0100 (Mon, 08 Feb 2021) $
#:#  Last changed by: $Author: wassmer $
#:# 

.arraysAreEqual <- function(a1, a2) {
	if (length(a1) != length(a2)) {
		return(FALSE)
	}
	
	l <- length(a1)
	if (l > 0) {
		a1 <- sort(a1)
		a2 <- sort(a2)
		if (sum(a1 == a2) < l) {
			return(FALSE)
		}
	}
	
	return(TRUE)
}

.getNumberOfGroupsFromArgumentNames <- function(argNames) {
	numbers <- gsub("\\D", "", argNames)
	numbers <- numbers[numbers != ""]
	return(ifelse(length(numbers) == 0, 1, max(as.numeric(numbers))))
}

.getGroupNumberFromArgumentName <- function(argName) {
	n <- gsub("\\D", "", argName)
	return(ifelse(n == "", 1, as.numeric(n)))
}

.isControlGroupArgument <- function(argName, numberOfGroups) {
	if (numberOfGroups <= 2) {
		return(FALSE)
	}
	
	return(ifelse(numberOfGroups == 1, FALSE, .getGroupNumberFromArgumentName(argName) == numberOfGroups))
}

.naOmitBackward <- function(x) {
	indices <- which(is.na(x))
	if (length(indices) == 0) {
		return(x)
	}
	
	if (length(x) == 1 || !is.na(x[length(x)])) {
		return(x)
	}
	
	if (length(indices) == 1) {
		return(x[1:(length(x) - 1)])
	}
	
	indexBefore <- NA_real_
	for (i in length(indices):1) {
		index <- indices[i]
		if (!is.na(indexBefore) && index != indexBefore - 1) {
			return(x[1:(indexBefore - 1)])
		}
		indexBefore <- index
	}
	if (!is.na(indexBefore)) {
		return(x[1:(indexBefore - 1)])
	}
	return(x)
}

.getNumberOfStagesFromArguments <- function(args, argNames) {
	numberOfStages <- 1
	for (argName in argNames) {	
		argValues <- args[[argName]]
		n <- length(.naOmitBackward(argValues))
		if (n > numberOfStages) {
			numberOfStages <- n
		}
	}
	return(numberOfStages)
}

.assertIsValidTreatmentArmArgumentDefined <- function(args, argNames, numberOfGroups, numberOfStages) {
	tratmentArgNames <- argNames[!grepl(paste0(".*\\D{1}", numberOfGroups, "$"), argNames)]
	for (argName in tratmentArgNames) {
		argValues <- args[[argName]]
		if (!is.null(argValues) && length(.naOmitBackward(argValues)) == numberOfStages) {
			return(invisible())
		}
	}
	stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
		"at least for one treatment arm the values for ", numberOfStages, " stages must be defined ",
		"because the control arm defines ", numberOfStages, " stages")
}

.createDataFrame <- function(...) {
	args <- list(...)
	argNames <- .getArgumentNames(...)
	if (length(args) == 0 || length(argNames) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
	}
	
	multiArmEnabled <- any(grep("3", argNames))
	numberOfGroups <- .getNumberOfGroupsFromArgumentNames(argNames)
	numberOfStages <- .getNumberOfStagesFromArguments(args, argNames)
	survivalDataEnabled <- .isDataObjectSurvival(...)
	if (multiArmEnabled) {
		.assertIsValidTreatmentArmArgumentDefined(args, argNames, numberOfGroups, numberOfStages)
	}
	
	numberOfValues <- length(args[[1]])
	naIndicesBefore <- NULL
	if (!survivalDataEnabled && multiArmEnabled) {
		naIndicesBefore <- list()
	}
	for (argName in argNames) {	
		argValues <- args[[argName]]
		if (is.null(argValues) || length(argValues) == 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'", argName, "' is not a valid numeric vector")
		}
		
		if (is.na(argValues[1])) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'", argName, "' is NA at first stage; a valid numeric value must be specified at stage 1")
		}
		
		if (length(argValues) != numberOfValues) {
			stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
				"all data vectors must have the same length: '", 
				argName, "' (", length(argValues), ") differs from '", 
				argNames[1], "' (", numberOfValues, ")")
		}
		
		if (.equalsRegexpIgnoreCase(argName, "^stages?$")) {
			if (length(stats::na.omit(argValues)) != length(argValues)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"NA's not allowed for '", argName, "'; stages must be defined completely")
			}
			
			definedStages <- sort(intersect(unique(argValues), 1:numberOfValues))
			if (length(definedStages) < numberOfValues) {
				if (length(definedStages) == 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "no valid stages are defined; ",
						"stages must be defined completely (", .arrayToString(1:numberOfValues), ")")
				}
				msg <- ifelse(length(definedStages) == 1,  
					paste0("only stage ", definedStages, " is defined"), 
					paste0("only stages ", .arrayToString(definedStages), " are defined"))
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, msg, "; stages must be defined completely")
			}
		}
		
		if (!survivalDataEnabled && .isControlGroupArgument(argName, numberOfGroups) &&
			length(na.omit(argValues)) < numberOfStages) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"control group '", argName, "' (", .arrayToString(argValues, digits = 2), ") must be defined for all stages")
		}
		
		naIndices <- which(is.na(argValues))
		if (length(naIndices) > 0) {
			stageIndex <- naIndices[length(naIndices)]
			if (stageIndex != numberOfValues) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'", argName, "' contains a NA at stage ", stageIndex, 
					" followed by a value for a higher stage; NA's must be the last values")
			}
		}
		if (length(naIndices) > 1) { 
			indexBefore <- naIndices[length(naIndices)]
			for (i in (length(naIndices) - 1):1) {
				index <- naIndices[i]
				if (indexBefore - index > 1) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'", argName, "' contains alternating values and NA's; ",
						"NA's must be the last values")
				}
				indexBefore <- index
			}
		}
		
		if (!survivalDataEnabled) {
			if (!multiArmEnabled) {
				if (!is.null(naIndicesBefore) && !.equalsRegexpIgnoreCase(argName, "^stages?$")) {
					if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
						stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
							"inconsistent NA definition; ",
							"if NA's exist, then they are mandatory for each group at the same stage")
					}
				}
				naIndicesBefore <- naIndices
			} else {
				groupNumber <- .getGroupNumberFromArgumentName(argName)
				if (!is.null(naIndicesBefore[[as.character(groupNumber)]]) && 
					!.equalsRegexpIgnoreCase(argName, "^stages?$") &&
					!.isControlGroupArgument(argName, numberOfGroups)) {
					if (!.arraysAreEqual(naIndicesBefore[[as.character(groupNumber)]], naIndices)) {
						stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
							"values of treatment ", groupNumber, " not correctly specified; ",
							"if NA's exist, then they are mandatory for each parameter at the same stage")
					}
				}
				if (!.isControlGroupArgument(argName, numberOfGroups)) {
					naIndicesBefore[[as.character(groupNumber)]] <- naIndices
				}
			}
		}
		
		if (sum(is.infinite(argValues)) > 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all data values must be finite; ",
				"'", argName, "' contains infinite values")
		}
		
		if (!is.numeric(argValues)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all data vectors must be numeric ('", 
				argName, "' is ", class(argValues), ")")
		}
		
		if (length(argValues) > C_KMAX_UPPER_BOUND) {
			stop(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
				"'", argName, "' is out of bounds [1, ", C_KMAX_UPPER_BOUND, "]")
		}
	}
	
	for (groupNumber in 1:numberOfGroups) {
		groupVars <- argNames[grepl(paste0("\\D", groupNumber, "$"), argNames)]
		naIndicesBefore <- NULL
		for (argName in groupVars) {	
			argValues <- args[[argName]]
			naIndices <- which(is.na(argValues))
			if (!is.null(naIndicesBefore) && !.equalsRegexpIgnoreCase(argName, "^stages?$")) {
				if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
					stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
						"inconsistent NA definition for group ", groupNumber, "; ",
						"if NA's exist, then they are mandatory for each group at the same stage")
				}
			}
			naIndicesBefore <- naIndices
		}
	}
	
	dataFrame <- as.data.frame(args)
	if (length(intersect(tolower(names(dataFrame)), c("stage", "stages"))) == 0) {
		dataFrame$stages <- 1:nrow(dataFrame)
	}
	return(dataFrame)
}

.getDataFrameFromArgs <- function(...) {
	args <- list(...)
	if (length(args) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
			"cannot initialize dataset because no data are defined")
	}
	
	dataFrame <- NULL
	dataFrameCounter <- 0
	for (arg in args) {	
		if (is.data.frame(arg)) {
			dataFrameCounter <- dataFrameCounter + 1
			if (is.null(dataFrame)) {
				dataFrame <- arg
			}
		}
	}
	
	if (dataFrameCounter > 1) {
		warning("Found ", dataFrameCounter, ", data.frame arguments; ", 
			"only the first data.frame will be used for the initialization of the dataset", call. = FALSE)
	}
	
	return(dataFrame)
}

.getArgumentNames <- function(...) {
	dataFrame <- .getDataFrameFromArgs(...)
	if (!is.null(dataFrame)) {
		return(names(dataFrame))
	} 
	
	args <- list(...)
	if (length(args) == 0) {
		return(character(0))
	}
	
	return(names(args))
}

.assertIsValidDatasetArgument <- function(...) {
	argNames <- .getArgumentNames(...)
	if (length(argNames) == 0) {
		return(TRUE)
	}
	
	argNamesLower <- tolower(argNames)
	dataObjectkeyWords <- tolower(C_KEY_WORDS)
	
	multiArmKeywords <- tolower(c(
		C_KEY_WORDS_EVENTS, 
		C_KEY_WORDS_OVERALL_EVENTS, 
		C_KEY_WORDS_SAMPLE_SIZES, 
		C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 
		C_KEY_WORDS_MEANS, 
		C_KEY_WORDS_OVERALL_MEANS, 
		C_KEY_WORDS_ST_DEVS, 
		C_KEY_WORDS_OVERALL_ST_DEVS,
		C_KEY_WORDS_ALLOCATION_RATIOS,
		C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
		C_KEY_WORDS_LOG_RANKS,
		C_KEY_WORDS_OVERALL_LOG_RANKS))
	unknownArgs <- setdiff(argNamesLower, dataObjectkeyWords)
	unknownArgsChecked <- unknownArgs
	unknownArgs <- c()
	for (unknownArg in unknownArgsChecked) {
		unknown <- TRUE
		for (multiArmKeyword in multiArmKeywords) {
			if (grepl(paste0(multiArmKeyword, "\\d{1,4}"), unknownArg)) {
				unknown <- FALSE
			}
		}
		if (unknown) {
			unknownArgs <- c(unknownArgs, unknownArg)
		}
	}
	
	if (length(unknownArgs) > 0) {
		for (i in 1:length(unknownArgs)) {
			unknownArgs[i] <- argNames[argNamesLower == unknownArgs[i]][1]
		}
		if (length(unknownArgs) == 1) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"the argument '", unknownArgs, "' is not a valid dataset argument")
		} else {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"the arguments ", .arrayToString(unknownArgs, encapsulate = TRUE), 
				" are no valid dataset arguments")
		}
	}
	
	invisible(TRUE)
}

.isDataObject <- function(..., dataObjectkeyWords) {
	.assertIsValidDatasetArgument(...)
	
	argNames <- .getArgumentNames(...)
	if (length(argNames) == 0) {
		return(FALSE)
	}
	
	argNames <- tolower(argNames)
	matching <- intersect(argNames, tolower(dataObjectkeyWords))
	
	return(length(matching) > 0)
}

.isDataObjectMeans <- function(...) {
	return(.isDataObject(..., dataObjectkeyWords = c(
		C_KEY_WORDS_MEANS, C_KEY_WORDS_ST_DEVS, 
		C_KEY_WORDS_MEANS_1, C_KEY_WORDS_ST_DEVS_1,
		C_KEY_WORDS_MEANS_2, C_KEY_WORDS_ST_DEVS_2,
		C_KEY_WORDS_OVERALL_MEANS, C_KEY_WORDS_OVERALL_ST_DEVS, 
		C_KEY_WORDS_OVERALL_MEANS_1, C_KEY_WORDS_OVERALL_ST_DEVS_1,
		C_KEY_WORDS_OVERALL_MEANS_2, C_KEY_WORDS_OVERALL_ST_DEVS_2)))
}

.isDataObjectRates <- function(...) {	
	dataObjectkeyWords1 <- c(C_KEY_WORDS_EVENTS, C_KEY_WORDS_OVERALL_EVENTS)
	dataObjectkeyWords2 <- c(C_KEY_WORDS_OVERALL_LOG_RANKS, 
		C_KEY_WORDS_LOG_RANKS, 
		C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, 
		C_KEY_WORDS_ALLOCATION_RATIOS)
	
	dataObjectkeyWords1 <- c(dataObjectkeyWords1, paste0(dataObjectkeyWords1, c(1, 2)))
	dataObjectkeyWords2 <- c(dataObjectkeyWords2, paste0(dataObjectkeyWords2, c(1, 2)))
	
	return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords1) &&
			!.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords2))
}

.isDataObjectSurvival <- function(...) {
	dataObjectkeyWords <- c(C_KEY_WORDS_OVERALL_LOG_RANKS, 
		C_KEY_WORDS_LOG_RANKS, 
		C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, 
		C_KEY_WORDS_ALLOCATION_RATIOS)
	dataObjectkeyWords <- c(dataObjectkeyWords, paste0(dataObjectkeyWords, c(1, 2)))
	return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords))
}



