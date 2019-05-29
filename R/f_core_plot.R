######################################################################################
#                                                                                    #
# -- Plot functions --                                                               #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 25-02-2019                                                                   #
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

.addNumberToPlotCaption <- function(caption, type, numberInCaptionEnabled = FALSE) {
	if (!numberInCaptionEnabled) {
		return(caption)
	}
	
	return(paste0(caption, " [", type, "]"))
}

.getPlotCaption <- function(obj, type, numberInCaptionEnabled = FALSE) {
	if (is.null(obj) || length(type) == 0) {
		return(NA_character_)
	}
	
	if (inherits(obj, "TrialDesignPlan") || inherits(obj, "SimulationResults")) {
		if (type == 1) {
			if (.isTrialDesignPlanSurvival(obj)) {
				return(.addNumberToPlotCaption("Boundaries Z Scale", type, numberInCaptionEnabled))
			} else {
				return(.addNumberToPlotCaption("Boundaries", type, numberInCaptionEnabled))
			}
		}
		else if (type == 2) { 
			return(.addNumberToPlotCaption("Boundaries Effect Scale", type, numberInCaptionEnabled))
		} 
		else if (type == 3) { 
			return(.addNumberToPlotCaption("Boundaries p Values Scale", type, numberInCaptionEnabled))
		} 
		else if (type == 4) {
			return(.addNumberToPlotCaption("Type One Error Spending", type, numberInCaptionEnabled))
		} 
		
		else if (type == 5) { 
			if (obj$.isSampleSizeObject()) { 
				return(.addNumberToPlotCaption("Sample Size", type, numberInCaptionEnabled))
			} else {
				return(.addNumberToPlotCaption("Overall Power and Early Stopping", 
					type, numberInCaptionEnabled))
			}
		} 
		else if (type == 6) { 
			return(.addNumberToPlotCaption(ifelse(.isTrialDesignPlanSurvival(obj) || 
				inherits(obj, "SimulationResultsSurvival"), 
				"Number of Events", "Sample Size"), type, numberInCaptionEnabled))
		}
		else if (type == 7) {
			return(.addNumberToPlotCaption("Overall Power", type, numberInCaptionEnabled))
		}
		else if (type == 8) {
			return(.addNumberToPlotCaption("Overall Early Stopping", type, numberInCaptionEnabled))
		}
		else if (type == 9) {
			if (.isTrialDesignPlanSurvival(obj) || 
					inherits(obj, "SimulationResultsSurvival")) {
				return(.addNumberToPlotCaption("Expected Number of Events", type, numberInCaptionEnabled))
			} else {
				return(.addNumberToPlotCaption("Expected Sample Size", type, numberInCaptionEnabled))
			}
		}
		else if (type == 10) { 
			return(.addNumberToPlotCaption("Study Duration", type, numberInCaptionEnabled))
		}
		else if (type == 11) {
			return(.addNumberToPlotCaption("Expected Number of Subjects", type, numberInCaptionEnabled))
		}
		else if (type == 12) {
			return(.addNumberToPlotCaption("Analysis Times", type, numberInCaptionEnabled))
		}
		else if (type == 13) { 
			return(.addNumberToPlotCaption("Cumulative Distribution Function", type, numberInCaptionEnabled))
		}
		else if (type == 14) { 
			return(.addNumberToPlotCaption("Survival Function", type, numberInCaptionEnabled))
		}
	}
	else if (inherits(obj, "TrialDesign") || inherits(obj, "TrialDesignSet")) {
		if (type == 1) {
			return(.addNumberToPlotCaption("Boundaries", type, numberInCaptionEnabled))
		}
		else if (type == 3) {
			return(.addNumberToPlotCaption("Stage Levels", type, numberInCaptionEnabled))
		} 
		else if (type == 4) {
			return(.addNumberToPlotCaption("Type One Error Spending", type, numberInCaptionEnabled))
		}
		else if (type == 5) {
			return(.addNumberToPlotCaption('Power and Early Stopping', type, numberInCaptionEnabled))
		}
		else if (type == 6) { 
			return(.addNumberToPlotCaption('Average Sample Size and Power / Early Stop', 
				type, numberInCaptionEnabled))
		} 
		else if (type == 7) { 
			return(.addNumberToPlotCaption('Power', type, numberInCaptionEnabled))
		} 
		else if (type == 8) {
			return(.addNumberToPlotCaption('Early Stopping', type, numberInCaptionEnabled))
		} 
		else if (type == 9) {
			return(.addNumberToPlotCaption('Average Sample Size', type, numberInCaptionEnabled))
		}
	}
	
	return(NA_character_)
}

#' 
#' @title
#' Get Available Plot Types
#'
#' @description
#' Function to identify the available plot types of an object. 
#' 
#' @param obj The object for which the plot types shall be identified, e.g. produced by
#'        \code{\link{getDesignGroupSequential}} or \code{\link{getSampleSizeMeans}}.
#' @param output The output type. Can be one of \code{c("numeric", "caption", "numcap", "capnum")}.
#' @param numberInCaptionEnabled If \code{TRUE}, the number will be added to the 
#'        caption, default is \code{FALSE}.
#' 
#' @details
#' 
#' \code{output}:
#' \enumerate{
#'   \item \code{numeric}: numeric output
#'   \item \code{caption}: caption as character output
#'   \item \code{numcap}:  list with number and caption
#'   \item \code{capnum}:  list with caption and number
#' }
#' 
#' @keywords internal
#' 
#' @export 
#' 
getAvailablePlotTypes <- function(obj, output = c("numeric", "caption", "numcap", "capnum"),
		numberInCaptionEnabled = FALSE) {
	output <- match.arg(output)
	if (is.null(obj)) {
		if (output == "numeric") {
			return(NA_real_)
		}
		if (output == "caption") {
			return(NA_character_)
		}
		return(list())
	}
	
	types <- c()
	if (inherits(obj, "TrialDesignPlan")) {
		if (obj$.design$kMax > 1) {
			types <- c(types, 1:4)
		}
		types <- c(types, 5)
		if (obj$.isSampleSizeObject()) {
			if (.isTrialDesignPlanSurvival(obj)) {
				types <- c(types, 13, 14)
			}
		} else {
			types <- c(types, 6:9)
			if (.isTrialDesignPlanSurvival(obj)) {
				types <- c(types, 10:14)
			}
		}
	}
	else if (inherits(obj, "SimulationResults")) {
		types <- c(types, 5:9)
		if (inherits(obj, "SimulationResultsSurvival")) {
			types <- c(types, 10:14)
		}
	}
	else if (inherits(obj, "TrialDesign") || inherits(obj, "TrialDesignSet")) {
		types <- c(types, 1, 3:9)
	}
	
	if (output == "numeric") {
		return(types)
	}
	
	if (output == "caption") {
		captions <- c()
		for (type in types) {
			captions <- c(captions, .getPlotCaption(obj, type = type, 
				numberInCaptionEnabled = numberInCaptionEnabled))
		}
		return(captions)
	}
	
	if (output == "numcap") {
		numcap <- list()
		for (type in types) {
			numcap[[as.character(type)]] <- .getPlotCaption(obj, type = type, 
				numberInCaptionEnabled = numberInCaptionEnabled)
		}
		return(numcap)
	}
	
	capnum <- list()
	for (type in types) {
		capnum[[.getPlotCaption(obj, type = type, 
				numberInCaptionEnabled = numberInCaptionEnabled)]] <- type
	}
	return(capnum)
}

.getVariedParameterHint <- function(variedParameter, variedParameterName) {
	return(paste0("Note: interim values between ", round(variedParameter[1], 4), " and ",
		round(variedParameter[2], 4), " were calculated to get smoother lines; use, e.g., '", 
		variedParameterName, " = ",
		.getVariedParameterVectorSeqCommand(variedParameter), "' to get all interim values"))
}

.showPlotSourceInformation <- function(objectName, ..., xParameterName, yParameterNames, 
		hint = NA_character_, nMax = NA_integer_, showSource = FALSE) {
	if (!isTRUE(showSource)) {
		return(invisible())
	}
	
	cat("Source data of the plot:\n")
	if (length(objectName) == 0 || is.na(objectName)) {
		objectName <- "x"
	}
	if (!is.na(nMax) && length(yParameterNames) < 3 && 
			xParameterName == "informationRates") {
		cat("  x-axis: ", objectName, "$", xParameterName, " * ", nMax, "\n", sep = "")
	} else {
		cat("  x-axis: ", objectName, "$", xParameterName, "\n", sep = "")
	}

	if (length(yParameterNames) == 1) {
		cat("  y-axis: ", objectName, "$", yParameterNames, "\n", sep = "")
	} else {
		cat("  y-axes:\n")
		for (i in 1:length(yParameterNames)) {
			cat("    y", i, ": ", objectName, "$", yParameterNames[i], "\n", sep = "")
		}
	}
	
	if (!is.na(hint) && is.character(hint) && nchar(hint) > 0) {
		cat(hint, "\n", sep = "")
	}
	
	# Open work: add simple plot command example
}

.getParameterSetAsDataFrame <- function(parameterSet, designMaster,
		addPowerAndAverageSampleNumber = FALSE, 
		theta = seq(-1, 1, 0.02), nMax = NA_integer_) {
	
	if (.isTrialDesignSet(parameterSet) && parameterSet$getSize() > 1 && 
		(is.null(parameterSet$variedParameters) || length(parameterSet$variedParameters) == 0)) {
		stop("'variedParameters' must be not empty; ",
			"use 'DesignSet$addVariedParameters(character)' to add one or more varied parameters")
	}
	
	data <- as.data.frame(parameterSet, niceColumnNamesEnabled = FALSE, 
		includeAllParameters = TRUE, addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber,
		theta = theta, nMax = nMax)	
	
	if (!.isTrialDesignSet(parameterSet)) {
		return(list(data = data, variedParameters = character(0)))
	}
	
	if (parameterSet$getSize() <= 1) {
		return(list(data = data, variedParameters = parameterSet$variedParameters))
	}
	
	variedParameters <- parameterSet$variedParameters
	if (nrow(data) > 1) {
		for (variedParameter in variedParameters) {
			column <- data[[variedParameter]]
			if (length(column) <= 1) {
				stop("Varied parameter '", variedParameter, "' has length ", length(column))
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
			.getTableColumnNames(design = designMaster)[[variedParameter]])
	}
	names(variedParameters) <- variedParameterNames
	return(list(data = data, variedParameters = variedParameters))
}

.getCategories <- function(data, yParameterName, tableColumnNames) {
	if (is.null(data$categories) || sum(is.na(data$categories)) > 0) {
		return(rep(.getAxisLabel(yParameterName, tableColumnNames), nrow(data)))
	}
	
	return(paste(data$categories, .getAxisLabel(yParameterName, tableColumnNames), sep = ", "))
}

.getAxisLabel <- function(parameterName, tableColumnNames) {
	axisLabel <- tableColumnNames[[parameterName]]
	if (is.null(axisLabel)) {
		return(paste0("%", parameterName, "%"))
	}
	return(axisLabel)
}

.plotParameterSet <- function(parameterSet, designMaster, xParameterName, yParameterNames,
		mainTitle = NA_character_, xlab = NA_character_, ylab = NA_character_, 
		palette = "Set1", theta = seq(-1, 1, 0.02), nMax = NA_integer_, 
		plotPointsEnabled = NA, legendPosition = NA_integer_, 
		variedParameters = logical(0), qnormAlphaLineEnabled = TRUE, yAxisScalingEnabled = TRUE, 
		ratioEnabled = NA, ...) {
	
	if (.isParameterSet(parameterSet) || .isTrialDesignSet(parameterSet)) {
		parameterNames <- c(xParameterName, yParameterNames)
		parameterNames <- parameterNames[!(parameterNames %in% c("theta", "averageSampleNumber",
						"overallEarlyStop", "calculatedPower"))]
		fieldNames <- c(names(parameterSet$getRefClass()$fields()), 
			names(designMaster$getRefClass()$fields()))
		for (parameterName in parameterNames) {
			if (!is.na(parameterName) && !(parameterName %in% fieldNames)) {
				print(fieldNames)
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'", class(parameterSet), "' and '", class(designMaster), "' ", 
					"do not contain a field with name '", parameterName, "'")
			}
		}
		plotSettings <- parameterSet$getPlotSettings()
	} else {
		plotSettings <- PlotSettings()
	}
	
	if (.isTrialDesignSet(parameterSet)) {
		parameterSet$assertHaveEqualSidedValues()
	}
	
	addPowerAndAverageSampleNumber <- xParameterName == "theta" && 
		yParameterNames[1] %in% c("averageSampleNumber", "calculatedPower", "overallEarlyStop",
			"overallReject", "overallFutility") 
	
	if (!addPowerAndAverageSampleNumber) {
		addPowerAndAverageSampleNumber <- xParameterName == "effect" &&
			yParameterNames[1] %in% c("overallReject", "futilityStop", 
				"earlyStop", "expectedNumberOfSubjects")
	}
	
	if (.isParameterSet(parameterSet) || .isTrialDesignSet(parameterSet)) {
		df <- .getParameterSetAsDataFrame(parameterSet, designMaster,
			addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber, 
			theta = theta, nMax = nMax)
		data <- df$data	
		variedParameters <- df$variedParameters
	} else if (is.data.frame(parameterSet)) {
		data <- parameterSet
	} else {
		stop("'parameterSet' (", class(parameterSet), ") must be a data.frame, a 'TrialDesignSet' ", 
			"or an object that inherits from 'ParameterSet'")
	}
	
	if (length(variedParameters) > 0) {
		legendTitle <- paste(names(variedParameters), collapse = "\n")
		categoryParameterName <- variedParameters[1]
	} else {
		legendTitle <- NA_character_
		categoryParameterName <- NA_character_
	}
	
	yParameterName1 <- yParameterNames[1]
	yParameterName2 <- NULL
	yParameterName3 <- NULL
	if (length(yParameterNames) >= 2) {
		yParameterName2 <- yParameterNames[2]
	}
	if (length(yParameterNames) >= 3) {
		yParameterName3 <- yParameterNames[3]
	}
	
	mirrorModeEnabled <- FALSE
	if (!is.null(yParameterName2) && !is.na(yParameterName2)) {
		mirrorModeEnabled <- paste0(yParameterName1, "Mirrored") == yParameterName2
	}
	
	tableColumnNames <- .getTableColumnNames(design = designMaster)
	
	xAxisLabel <- .getAxisLabel(xParameterName, tableColumnNames)
	yAxisLabel1 <- .getAxisLabel(yParameterName1, tableColumnNames)
	yAxisLabel2 <- NULL
	if (!is.null(yParameterName2) && !is.null(yParameterName3)) {
		if (!is.na(yParameterName2)) {
			pn2 <- .getAxisLabel(yParameterName2, tableColumnNames)
			if (yParameterName2 == "overallEarlyStop") {
				pn2 <- "Stopping Probability"
			}
			yAxisLabel2 <- paste(pn2, .getAxisLabel(yParameterName3, tableColumnNames), sep = " and ")
		} else {
			yAxisLabel2 <- .getAxisLabel(yParameterName3, tableColumnNames)
		}
	}
	else if (!is.null(yParameterName2) && !mirrorModeEnabled) {
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
	if (is.null(yParameterName2) || is.na(yParameterName2)) {
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
		if (yAxisScalingEnabled && !is.null(yParameterName3)) {
			if (is.na(yParameterName2)) {
				scalingFactors <- .getScalingFactors(data$yValues, data$yValues3)
			} else {
				scalingFactors <- .getScalingFactors(data$yValues, c(data$yValues2, data$yValues3))
			}
			scalingFactor1 <- scalingFactors$scalingFactor1
			scalingFactor2 <- scalingFactors$scalingFactor2
		}
		df1 <- data.frame(
			xValues = data$xValues,
			yValues = data$yValues * scalingFactor1,
			categories = .getCategories(data, yParameterName1, tableColumnNames)
		)
		if (!is.na(yParameterName2)) {
			df2 <- data.frame(
				xValues = data$xValues,
				yValues = data$yValues2 * scalingFactor2,
				categories = .getCategories(data, yParameterName2, tableColumnNames)
			)
		}
		if (!is.null(yParameterName3)) {
			df3 <- data.frame(
				xValues = data$xValues,
				yValues = data$yValues3 * scalingFactor2,
				categories = .getCategories(data, yParameterName3, tableColumnNames)
			)
			if (is.na(yParameterName2)) {
				data <- rbind(df1, df3)
			} else {
				data <- rbind(df1, df2, df3)
			}
		} else {
			data <- rbind(df1, df2)
		}
		
		# sort categories for pairwise printing of the legend
		unqiueValues <- unique(as.character(data$categories))
		decreasing <- addPowerAndAverageSampleNumber && xParameterName == "effect"
		data$categories <- factor(data$categories, levels = unqiueValues[order(unqiueValues, decreasing = decreasing)])
		
		if (yParameterName1 == "alphaSpent" && yParameterName2 == "betaSpent") {
			legendTitle <- paste(legendTitle, "Type of error", sep = "\n")
		}
	}
	
	if (is.na(legendPosition)) {
		legendPosition <- .getLegendPosition(plotSettings, designMaster, data, yParameterName1, 
			yParameterName2, addPowerAndAverageSampleNumber)
	}
	
	if (is.na(ratioEnabled)) {
		ratioEnabled <- .isTrialDesignPlanSurvival(parameterSet) || 
			(.isTrialDesignPlanMeans(parameterSet) && parameterSet$meanRatio) || 
			(.isTrialDesignPlanRates(parameterSet) && parameterSet$riskRatio)
	}
	
	p <- .plotDataFrame(data, mainTitle = mainTitle, xlab = xlab, ylab = ylab, 
		xAxisLabel = xAxisLabel, yAxisLabel1 = yAxisLabel1, yAxisLabel2 = yAxisLabel2, 
		palette = palette, plotPointsEnabled = plotPointsEnabled, legendTitle = legendTitle,
		legendPosition = legendPosition, scalingFactor1 = scalingFactor1, scalingFactor2 = scalingFactor2,
		addPowerAndAverageSampleNumber = addPowerAndAverageSampleNumber, mirrorModeEnabled = mirrorModeEnabled,
		ratioEnabled = ratioEnabled, plotSettings = plotSettings, sided = designMaster$sided, ...)
	
	if (!is.data.frame(parameterSet) && yParameterName1 == "criticalValues" && designMaster$sided == 2) {
		p <- plotSettings$mirrorYValues(p, yValues = data$yValues, 
			plotPointsEnabled = !addPowerAndAverageSampleNumber, 
			pointBorder = .getPointBorder(data, plotSettings))
	}
	
	if (qnormAlphaLineEnabled && ((!is.data.frame(parameterSet) && (yParameterName1 == "criticalValues" || 
					(yParameterName1 == "futilityBounds" && !is.null(yParameterName2) && yParameterName2 == "criticalValues"))) || 
			(yParameterName1 %in% c("futilityBounds", "criticalValues") && !is.null(yParameterName2) && 
				yParameterName2 %in% c("criticalValues", "criticalValuesMirrored")))) {
		p <- .addQnormAlphaLine(p, designMaster, plotSettings, data)
	}
	
	if ((xParameterName == "informationRates" || xParameterName == "eventsPerStage") && 
			yParameterName1 == "stageLevels") {
		yValue <- designMaster$alpha
		if (designMaster$sided == 2) {
			yValue <- yValue / 2
		}
		p <- p + ggplot2::geom_hline(yintercept = yValue, linetype = "dashed")
		yValueLabel <- paste0("alpha == ", round(yValue, 4))
		hjust <- -0.2
		p <- p + ggplot2::annotate("label", x = -Inf, hjust = hjust, y = yValue, 
			label = yValueLabel, size = 2.5, parse = TRUE, colour = "white", fill = "white")
		p <- p + ggplot2::annotate("text", x = -Inf, hjust = hjust - 0.15, y = yValue, 
			label = yValueLabel, size = 2.5, parse = TRUE)
	}
	
	return(p)
}

.getScalingFactors <- function(leftAxisValues, rightAxisValues) {
	m1 <- ifelse(length(na.omit(leftAxisValues)) == 0, 1, max(na.omit(leftAxisValues)))
	m2 <- ifelse(length(na.omit(rightAxisValues)) == 0, 1, max(na.omit(rightAxisValues)))
	if (is.na(m1)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "y-values, left (", 
			.arrayToString(leftAxisValues), ") are not specified correctly")
	}
	if (is.na(m2)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "y-values, right (", 
			.arrayToString(rightAxisValues), ") are not specified correctly")
	}
	
	if (m1 > m2) {
		scalingFactor1 <- 1
		scalingFactor2 <- m1 / m2
	} else if (m1 < m2) {
		scalingFactor1 <- m2 / m1
		scalingFactor2 <- 1
	} else {
		scalingFactor1 <- 1
		scalingFactor2 <- 1
	}
	
	return(list(scalingFactor1 = scalingFactor1, scalingFactor2 = scalingFactor2))
}

.plotDataFrame <- function(data, mainTitle = NA_character_, 
		xlab = NA_character_, ylab = NA_character_, xAxisLabel = NA_character_,
		yAxisLabel1 = NA_character_, yAxisLabel2 = NA_character_, 
		palette = "Set1", plotPointsEnabled = NA, legendTitle = NA_character_,
		legendPosition = NA_integer_, scalingFactor1 = 1, scalingFactor2 = 1,
		addPowerAndAverageSampleNumber = FALSE, mirrorModeEnabled = FALSE, 
		ratioEnabled = FALSE, plotSettings = NULL, sided = 1, ...) {
	
	if (!is.data.frame(data)) {
		stop("'data' must be a data.frame (is ", class(data), ")")
	}
	
	if (is.null(plotSettings)) {
		plotSettings <- PlotSettings()
	}
	
	nRow <- nrow(data)
	data <- data[!(data$xValues == 0 & data$xValues == data$yValues), ]
	removedRows1 <- nRow - nrow(data)
	
	nRow <- nrow(data)
	data <- data[!is.na(data$yValues), ]
	removedRows2 <- nRow - nrow(data)
	
	if (getLogLevel() == C_LOG_LEVEL_WARN && (removedRows1 > 0 || removedRows2 > 0)) {
		warning(sprintf("Removed %s rows containing (0, 0)-points and %s rows containing missing values", 
				removedRows1, removedRows2), call. = FALSE)
	}
	
	categoryEnabled <- !is.null(data$categories) && (sum(is.na(data$categories)) < length(data$categories))
	if (categoryEnabled) {
		data <- data[, c("xValues", "yValues", "categories")]
	} else {
		data <- data[, c("xValues", "yValues")]
	}
	
	if (mirrorModeEnabled) {
		p <- ggplot2::ggplot(data, ggplot2::aes(x = data$xValues, y = data$yValues, 
				fill = factor(data$categories))) 
	} else if (categoryEnabled) {	
		p <- ggplot2::ggplot(data, ggplot2::aes(x = data$xValues, y = data$yValues, 
				colour = factor(data$categories)))
	} else {
		p <- ggplot2::ggplot(data, ggplot2::aes(x = data$xValues, y = data$yValues))
	}
	
	p <- plotSettings$setTheme(p)
	p <- plotSettings$hideGridLines(p)
	
	# set main title
	p <- plotSettings$setMainTitle(p, mainTitle)
	
	# set legend
	if (mirrorModeEnabled || (!is.na(legendPosition) && legendPosition == -1)) {
		p <- p + ggplot2::theme(legend.position = "none")
	} else if (categoryEnabled) {
		p <- plotSettings$setLegendPosition(p, legendPosition = legendPosition)
		p <- plotSettings$setLegendBorder(p)
		p <- plotSettings$setLegendTitle(p, legendTitle)
		p <- plotSettings$setLegendLabelSize(p)
	}
	
	# add dashed line to y = 0 or y = 1
	if (mirrorModeEnabled) {
		p <- p + ggplot2::geom_hline(yintercept = ifelse(ratioEnabled, 1, 0), linetype = "dashed")
	}
	
	xAxisLabel <- .toCapitalized(xAxisLabel)
	yAxisLabel1 <- .toCapitalized(yAxisLabel1)
	yAxisLabel2 <- .toCapitalized(yAxisLabel2)
	
	p <- plotSettings$setAxesLabels(p, 
		xAxisLabel = xAxisLabel, yAxisLabel1 = yAxisLabel1, yAxisLabel2 = yAxisLabel2, 
		xlab = xlab, ylab = ylab, scalingFactor1 = scalingFactor1, scalingFactor2 = scalingFactor2)
	
	# plot lines and points
	plotPointsEnabled <- ifelse(is.na(plotPointsEnabled), !addPowerAndAverageSampleNumber, plotPointsEnabled) 
	if (length(data$xValues) > 20) {
		plotPointsEnabled <- FALSE
	}
	p <- plotSettings$plotValues(p, plotPointsEnabled = plotPointsEnabled, 
		pointBorder = .getPointBorder(data, plotSettings))
	
	p <- plotSettings$setAxesAppearance(p)
	p <- plotSettings$setColorPalette(p, palette)
	p <- plotSettings$enlargeAxisTicks(p)
	
	companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
	if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
		companyAnnotationEnabled <- FALSE
	}
	p <- plotSettings$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)
	
	# start plot generation
	return(p)
}

.getPointBorder <- function(data, plotSettings) {
	numberOfCategories <- 1
	if (sum(is.na(data$categories)) < length(data$categories)) {
		numberOfCategories <- length(unique(as.character(data$categories)))
	}
	
	pointBorder <- 4
	if (length(data$xValues) > 10) {
		pointBorder <- 1
		plotSettings$adjustPointSize(-2)
	}
	else if (numberOfCategories > 8) {
		pointBorder <- 1
	}
	else if (numberOfCategories > 6) {
		pointBorder <- 2
	}
	else if (numberOfCategories > 4) {
		pointBorder <- 3
	}
	return(pointBorder)
}

.getLegendPosition <- function(plotSettings, designMaster, data, yParameterName1, 
		yParameterName2, addPowerAndAverageSampleNumber) {
	
	if (length(unique(data$categories)) > 6) {
		plotSettings$adjustPointSize(-0.5)
		plotSettings$adjustLegendFontSize(-2)
		return(C_POSITION_OUTSIDE_PLOT)
	}
	
	if (.isTrialDesignWithValidFutilityBounds(designMaster) &&
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

.addQnormAlphaLine <- function(p, designMaster, plotSettings, data, annotationEnabled = TRUE) {
	alpha <- designMaster$alpha
	if (designMaster$sided == 2) {
		alpha <- alpha / 2
	}
	yValue <- stats::qnorm(1 - alpha)
	yValueLabel <- paste0("qnorm(1 - ", alpha, " ) == ", round(yValue, 4))
	if (designMaster$sided == 1) {
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
	if (designMaster$sided == 1) {
		
		yMax <- max(stats::na.omit(data$yValues))
		if (!is.null(data$yValues2) && length(data$yValues2) > 0) {
			yMax <- max(yMax, stats::na.omit(data$yValues2))
		}
		eps <- (yMax - yValue) * 0.15
		
		p <- plotSettings$expandAxesRange(p, y = yValue - eps)
	}
	
	return(p)
}

.getLambdaStepFunctionByTime <- function(time, piecewiseSurvivalTime, lambda2) {
	if (length(piecewiseSurvivalTime) == 0 || any(is.na(piecewiseSurvivalTime))) {
		return(lambda2[1])
	}
	
	for (i in 1:length(piecewiseSurvivalTime)) {
		if (time <= piecewiseSurvivalTime[i]) {
			return(lambda2[i])
		}
	}
	return(lambda2[length(lambda2)])
}

.getLambdaStepFunction <- function(timeValues, piecewiseSurvivalTime, lambda) {
	if (length(piecewiseSurvivalTime) != length(lambda)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"length of 'piecewiseSurvivalTime' (", length(piecewiseSurvivalTime), 
			") must be equal to length of 'lambda' (", length(lambda), ") - 1")
	}
	
	piecewiseSurvivalTime <- .getPiecewiseExpStartTimesWithoutLeadingZero(piecewiseSurvivalTime)
	if (length(piecewiseSurvivalTime) == 0) {
		return(lambda[1])
	}
	
	lambdaValues <- c()
	for (time in timeValues) {
		lambdaValues <- c(lambdaValues, .getLambdaStepFunctionByTime(time, piecewiseSurvivalTime, lambda))
	}
	return(lambdaValues)
}
