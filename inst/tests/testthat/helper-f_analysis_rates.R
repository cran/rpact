######################################################################################
#                                                                                    #
# -- Unit test helper functions --                                                   #
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


testGetAnalysisResultsPlotData <- function(x, ..., nPlanned = NA_real_,
		stage = x$getNumberOfStages(), allocationRatioPlanned = NA_real_) {
	
	plotArgs <- .getAnalysisResultsPlotArguments(x = x, nPlanned = nPlanned, 
		stage = stage, allocationRatioPlanned = allocationRatioPlanned)
	if (x$getDataInput()$isDatasetMeans()) {
		assumedStDev <- .getOptionalArgument("assumedStDev", ...)
		if (is.null(assumedStDev)) {
			assumedStDev <- x$assumedStDev
			return(.getConditionalPowerPlot(stageResults = plotArgs$stageResults, 
					nPlanned = plotArgs$nPlanned, stage = plotArgs$stage,
					allocationRatioPlanned = plotArgs$allocationRatioPlanned,
					assumedStDev = assumedStDev, ...))
		}
	}
	else if (x$getDataInput()$isDatasetRates()) {
		pi2 <- .getOptionalArgument("pi2", ...)
		if (is.null(pi2)) {
			pi2 <- x$pi2
			return(.getConditionalPowerPlot(stageResults = plotArgs$stageResults, 
					nPlanned = plotArgs$nPlanned, stage = plotArgs$stage,
					allocationRatioPlanned = plotArgs$allocationRatioPlanned,
					pi2 = pi2, ...))
		}
	}
	
	return(.getConditionalPowerPlot(stageResults = plotArgs$stageResults, 
			nPlanned = plotArgs$nPlanned, stage = plotArgs$stage,
			allocationRatioPlanned = plotArgs$allocationRatioPlanned, ...))
}
