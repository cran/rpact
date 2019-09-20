######################################################################################
#                                                                                    #
# -- RPACT output formats --                                                         #
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

.getFormattedValue <- function(value, digits, nsmall = NA_integer_, futilityProbabilities = FALSE) {
	if (missing(value)) {
		return("NA")
	}
	
	if (is.null(value) || length(value) == 0) {
		return(value)
	}
	
	if (!is.numeric(value)) {
		stop("'value' must be a numeric vector")
	}
	
	if (futilityProbabilities) {
		value[value >= 0 & value < 1e-09] <- 0 # nur futility probilities
	}
	
	if (is.na(nsmall)) {
		formattedValue <- format(value, digits = digits, justify = "left", trim = TRUE)
	} else {
		formattedValue <- format(value, digits = digits, nsmall = nsmall, justify = "left", trim = TRUE)
	}
	
	if (futilityProbabilities) {
		formattedValue[value == 0] <- "0"
	}
	
	#formattedValue <- trimws(formattedValue)
	
	return(formattedValue)
}

.getZeroCorrectedValue <- function(value) {
	if (is.numeric(value)) {
		value[abs(value) < 1e-08] <- 0
	}
	return(value)
}

.getDecimalPlaces <- function(value) {
	value <- stats::na.omit(value)
	if (length(value) == 0) {
		return(4)
	}
	
	fv <- .getFormattedValue(value[value >= 1e-4], digits = 4, nsmall = 4)
	fv <- fv[!((1:length(fv)) %in% grep("e", fv))]
	numberOfCharacters <- ifelse(length(fv) > 0, nchar(fv[1]), 6)
	numberOfCharacters <- ifelse(numberOfCharacters < 6, 6, numberOfCharacters)
	decimalPlaces <- numberOfCharacters - 2
	return(decimalPlaces)
}

# 
# @title 
# Format P Values
# 
# @description 
# Formats the output of p-values.
# 
# @details 
# Digits = 4, nsmall = 4.
# Replaces p-values in scientific format (e.g., 1e-07) by a non-scientific format (e.g., <0.00001).
# 
# @param value a vector of p-values.
# 
# @keywords internal
# 
formatPValues <- function(value) {
	if (sum(is.na(value)) == length(value)) {
		return(value)
	}
	
	decimalPlaces <- .getDecimalPlaces(value)
	if (is.na(decimalPlaces) || is.nan(decimalPlaces)) {
		decimalPlaces <- 4
	}
	
	threshold <- 10^-decimalPlaces
	text <- "<0."
	for (i in 1:(decimalPlaces - 1)) {
		text <- paste0(text, "0")
	}
	text <- paste0(text, "1")
	
	indices <- (value < threshold)
	value[indices] <- threshold
	formattedValue <- .getFormattedValue(value, digits = 4, nsmall = 4)
	formattedValue[indices] <- text
	return(formattedValue)
}

# 
# @title 
# Format Repeated P Values
# 
# @description 
# Formats the output of repeated p-values.
# 
# @details
# If p-value > 0.4999 then ">=0.5" will be returned.
# 
# @param value a vector of p-values.
# 
# @keywords internal
# 
formatRepeatedPValues <- function(value) {
	pValues <- formatPValues(value)
	pValues[value > 0.4999] <- ">0.5"
	#pValues[value < 1e-05] <- "<1e-05"
	return(pValues)
}

# 
# @title 
# Format Probabilities
# 
# @description 
# Formats the output of probabilities.
# 
# @details
# Digits = 4, nsmall = 4
# 
# @keywords internal
# 
formatProbabilities <- function(value) {
	return(.getFormattedValue(value, digits = 4, nsmall = 4))
}

# 
# @title 
# Format Sample Sizes
# 
# @description 
# Formats the output of sample sizes.
# 
# @details
# Digits = 1, nsmall = 1
# 
# @keywords internal
# 
formatSampleSizes <- function(value) {
	return(.getFormattedValue(value, digits = 1, nsmall = 1))
}

# 
# @title 
# Format Conditional Power
# 
# @description 
# Formats the output of contional power.
# 
# @details
# Digits = 4
# 
# @keywords internal
# 
formatConditionalPower <- function(value) {
	value <- round(value, digits = 4)
	conditionalPower <- .getFormattedValue(value, digits = 4)
	conditionalPower[value == 0] <- "0"
	#conditionalPower <- sprintf("%.4f", value)
	return(conditionalPower)
}

# 
# @title 
# Format Futility Probabilities
# 
# @description 
# Formats the output of futility probabilities.
# 
# @details
# Digits = 4, nsmall = 4
# 
# @keywords internal
# 
formatFutilityProbabilities <- function(value) {
	return(.getFormattedValue(value, digits = 4, nsmall = 4, futilityProbabilities = TRUE))
}

# 
# @title 
# Format Group Sequential Critical Values
# 
# @description 
# Formats the output of group sequential critical values.
# 
# @details
# Digits = 3, nsmall = 3
# 
# @keywords internal
# 
formatGroupSequentialCriticalValues <- function(value) {
	value[value == C_FUTILITY_BOUNDS_DEFAULT] <- -Inf
	return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

# 
# @title 
# Format Fisher Critical Values
# 
# @description 
# Formats the output of Fisher's combination critical values.
# 
# @details
# Digits = 4
# 
# @keywords internal
# 
formatFisherCriticalValues <- function(value) {
	return(.getFormattedValue(value, digits = 4))
}

# 
# @title 
# Format Fisher Test Statistics
# 
# @description 
# Formats the output of Fisher's combination test statistics.
# 
# @details
# Digits = 4
# 
# @keywords internal
# 
formatFisherTestStatistics <- function(value) {
	return(.getFormattedValue(value, digits = 4))
}

# 
# @title 
# Format Test Statistics
# 
# @description 
# Formats the output of test statistics (e.g., inverse normal).
# 
# @details
# Digits = 3, nsmall = 3
# 
# @keywords internal
# 
formatTestStatistics <- function(value) {
	return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

# 
# @title 
# Format Rates
# 
# @description 
# Formats the output of rates.
# 
# @details
# Digits = 3, nsmall = 3
# 
# @keywords internal
# 
formatRates <- function(value) {
	return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

# 
# @title 
# Format Rates Dynamic
# 
# @description 
# Formats the output of rates.
# 
# @details
# Digits = 3, nsmall = 3 if value < 1; digits = 1, nsmall = 1 otherwise
# 
# @keywords internal
# 
formatRatesDynamic <- function(value) {
	if (!any(is.na(value)) && all(value >= 1)) {
		return(.getFormattedValue(value, digits = 1, nsmall = 1))
	}
	return(.getFormattedValue(value, digits = 3, nsmall = 3))
}

# 
# @title 
# Format Accrual Intensities
# 
# @description 
# Formats the output of accrual intensities.
# 
# @details
# Digits = 1, nsmall = 1
# 
# @keywords internal
# 
formatAccrualIntensities <- function(value) {
	return(.getFormattedValue(value, digits = 2, nsmall = 1)) 
}

# 
# @title 
# Format Means
# 
# @description 
# Formats the output of means.
# 
# @details
# Digits = 4
# 
# @keywords internal
# 
formatMeans <- function(value) {
	return(.getFormattedValue(value, digits = 4))
}

# 
# @title 
# Format Ratios
# 
# @description 
# Formats the output of ratios.
# 
# @details
# Digits = 3
# 
# @keywords internal
# 
formatRatios <- function(value) {
	return(.getFormattedValue(value, digits = 3))
}

# 
# @title 
# Format StDevs
# 
# @description 
# Formats the output of standard deviations.
# 
# @details
# Digits = 4
# 
# @keywords internal
# 
formatStDevs <- function(value) {
	return(.getFormattedValue(value, digits = 4))
}

# 
# @title 
# Format Double
# 
# @description 
# Formats the output of double values.
# 
# @details
# Digits = 3
# 
# @keywords internal
# 
formatDouble <- function(value) {
	return(.getFormattedValue(.getZeroCorrectedValue(value), digits = 3))
}

# 
# @title 
# Format Durations
# 
# @description 
# Formats the output of study durations.
# 
# @details
# Digits = 3
# 
# @keywords internal
# 
formatDurations <- function(value) {
	#return(sprintf("%.2f", value))
	return(.getFormattedValue(value, digits = 2, nsmall = 2))
}

# 
# @title 
# Format Time
# 
# @description 
# Formats the output of time values, e.g. months.
# 
# @details
# Digits = 3
# 
# @keywords internal
# 
formatTime <- function(value) {
	#return(sprintf("%.2f", value))
	return(.getFormattedValue(value, digits = 2, nsmall = 2))
}

# 
# @title 
# Format Simulation Output
# 
# @description 
# Formats the output of simulations.
# 
# @details
# Digits = 3
# 
# @keywords internal
# 
formatSimulationOutput <- function(value) {
	return(.getFormattedValue(.getZeroCorrectedValue(value), digits = 3))
}


# 
# @title 
# Format Variable Name
# 
# @description 
# Formats a variable name.
# 
# @details
# An optional prefix and postix can be added.
# 
# @keywords internal
# 
formatVariableName <- function(name, n, prefix = "", postfix = "") {
	if (!is.character(name)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'name' must be of type 'character' (is '", class(name), "')")
	}
	
	if (!is.numeric(n)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'n' must be of type 'numeric' (is '", class(n), "')")
	}
	
	if (n < 1 || n > 300) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_OUT_OF_BOUNDS, "'n' (", n, ") is out of bounds [1; 300]")
	}
	
	if (nchar(prefix) > 0) {
		name <- paste(prefix, name)
	}
	
	if (nchar(postfix) > 0) {
		name <- paste(name, postfix)
	}
	
	while (nchar(name) < n) {
		name <- paste0(name, " ")
	}
	
	name <- paste0("  ", name, " :")
	
	return(name)
}

