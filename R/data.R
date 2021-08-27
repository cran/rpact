#:#
#:#  *Data*
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
#:#  File version: $Revision: 5094 $
#:#  Last changed: $Date: 2021-08-03 09:15:25 +0200 (Di, 03 Aug 2021) $
#:#  Last changed by: $Author: pahlke $
#:#


#' One-Arm Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of one group.
#' Use \code{getDataset(dataMeans)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataMeans"

#' One-Arm Dataset of Rates
#'
#' A dataset containing the sample sizes and events of one group.
#' Use \code{getDataset(dataRates)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataRates"

#' One-Arm Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of one group.
#' Use \code{getDataset(dataSurvival)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataSurvival"

## Mulit-arm

#' Multi-Arm Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of four groups.
#' Use \code{getDataset(dataMultiArmMeans)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataMultiArmMeans"

#' Multi-Arm Dataset of Rates
#'
#' A dataset containing the sample sizes and events of three groups.
#' Use \code{getDataset(dataMultiArmRates)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataMultiArmRates"

#' Multi-Arm Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of three groups.
#' Use \code{getDataset(dataMultiArmSurvival)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataMultiArmSurvival"

## Enrichment

#' Enrichment Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of two groups.
#' Use \code{getDataset(dataEnrichmentMeans)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataEnrichmentMeans"

#' Enrichment Dataset of Rates
#'
#' A dataset containing the sample sizes and events of two groups.
#' Use \code{getDataset(dataEnrichmentRates)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataEnrichmentRates"

#' Enrichment Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of two groups.
#' Use \code{getDataset(dataEnrichmentSurvival)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataEnrichmentSurvival"

## Enrichment Stratified

#' Stratified Enrichment Dataset of Means
#'
#' A dataset containing the sample sizes, means, and standard deviations of two groups.
#' Use \code{getDataset(dataEnrichmentMeansStratified)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataEnrichmentMeansStratified"

#' Stratified Enrichment Dataset of Rates
#'
#' A dataset containing the sample sizes and events of two groups.
#' Use \code{getDataset(dataEnrichmentRatesStratified)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataEnrichmentRatesStratified"

#' Stratified Enrichment Dataset of Survival Data
#'
#' A dataset containing the log-rank statistics, events, and allocation ratios of two groups.
#' Use \code{getDataset(dataEnrichmentSurvivalStratified)} to create a dataset object that can be processed by \code{\link{getAnalysisResults}}.
#'
#' @format A \code{\link[base]{data.frame}} object.
#' 
"dataEnrichmentSurvivalStratified"

