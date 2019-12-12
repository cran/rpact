######################################################################################
#                                                                                    #
# -- Unit tests --                                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    #
# File version: 1.0.0                                                                #
# Date: 06 November 2019, 17:08:57                                                   #
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

context("Testing class 'SummaryFactory'")


test_that("Testing 'summary.ParameterSet': no errors occur", {

	design <- getDesignGroupSequential(alpha = 0.05, kMax = 4, 
		sided = 1, typeOfDesign = "WT", deltaWT = 0.1)
	
	designFisher <- getDesignFisher(kMax = 4, alpha = 0.025, 
		informationRates = c(0.2, 0.5, 0.8, 1), alpha0Vec = rep(0.4, 3))
	
	designCharacteristics <- getDesignCharacteristics(design)
	
	powerAndASN <- getPowerAndAverageSampleNumber(design, theta = 1, nMax = 100)
	
	designSet <- getDesignSet(design = design, deltaWT = c(0.3, 0.4))
	
	dataset <- getDataset(
		n1 = c(22, 11, 22, 11),
		n2 = c(22, 13, 22, 13),
		means1 = c(1, 1.1, 1, 1),
		means2 = c(1.4, 1.5, 3, 2.5),
		stDevs1 = c(1, 2, 2, 1.3),
		stDevs2 = c(1, 2, 2, 1.3)
	)
	
	stageResults <- getStageResults(design, dataset)
	
	analysisResults <- getAnalysisResults(design, dataset)
	
	designPlan <- getSampleSizeMeans(design)
	
	simulationResults <- getSimulationSurvival(design, 
		maxNumberOfSubjects = 100, plannedEvents = c(50, 100, 150, 200), seed = 12345)
	
	piecewiseSurvivalTime <- getPiecewiseSurvivalTime(list(
		"0 - <6"   = 0.025, 
		"6 - <9"   = 0.04, 
		"9 - <15"  = 0.015, 
		"15 - <21" = 0.01, 
		">=21"     = 0.007), hazardRatio = 0.8)
	
	accrualTime <- getAccrualTime(list(
		"0  - <12" = 15,
		"12 - <13" = 21,
		"13 - <14" = 27,
		"14 - <15" = 33,
		"15 - <16" = 39,
		">=16"     = 45), maxNumberOfSubjects = 1400)

	invisible(capture.output(names(design)))
	invisible(capture.output(names(designFisher)))
	invisible(capture.output(names(designCharacteristics)))
	invisible(capture.output(names(powerAndASN)))
	invisible(capture.output(names(designSet)))
	invisible(capture.output(names(dataset)))
	invisible(capture.output(names(stageResults)))
	invisible(capture.output(names(analysisResults)))
	invisible(capture.output(names(designPlan)))
	invisible(capture.output(names(simulationResults)))
	invisible(capture.output(names(piecewiseSurvivalTime)))
	invisible(capture.output(names(accrualTime)))
	
	invisible(capture.output(design$criticalValues))
	invisible(capture.output(design[["criticalValues"]]))
	
	invisible(capture.output(print(design)))
	invisible(capture.output(print(designFisher)))
	invisible(capture.output(print(designCharacteristics)))
	invisible(capture.output(print(powerAndASN)))
	invisible(capture.output(print(designSet)))
	invisible(capture.output(print(dataset)))
	invisible(capture.output(print(stageResults)))
	invisible(capture.output(print(analysisResults)))
	invisible(capture.output(print(designPlan)))
	invisible(capture.output(print(simulationResults)))
	invisible(capture.output(print(piecewiseSurvivalTime)))
	invisible(capture.output(print(accrualTime)))
	
	invisible(capture.output(summary(design)))
	invisible(capture.output(summary(designFisher)))
	invisible(capture.output(summary(designCharacteristics)))
	invisible(capture.output(summary(powerAndASN)))
	invisible(capture.output(summary(designSet)))
	invisible(capture.output(summary(dataset)))
	invisible(capture.output(summary(stageResults)))
	invisible(capture.output(summary(analysisResults)))
	invisible(capture.output(summary(designPlan)))
	invisible(capture.output(summary(simulationResults)))
	invisible(capture.output(summary(piecewiseSurvivalTime)))
	invisible(capture.output(summary(accrualTime)))
	
	invisible(capture.output(as.data.frame(design)))
	invisible(capture.output(as.data.frame(designFisher)))
	invisible(capture.output(as.data.frame(designCharacteristics)))
	invisible(capture.output(as.data.frame(powerAndASN)))
	invisible(capture.output(as.data.frame(designSet)))
	invisible(capture.output(as.data.frame(dataset)))
	invisible(capture.output(as.data.frame(stageResults)))
	invisible(capture.output(as.data.frame(analysisResults)))
	invisible(capture.output(as.data.frame(designPlan)))
	invisible(capture.output(as.data.frame(simulationResults)))
	invisible(capture.output(as.data.frame(piecewiseSurvivalTime)))
	invisible(capture.output(as.data.frame(accrualTime)))
	
	invisible(capture.output(as.data.frame(design, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(designFisher, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(designCharacteristics, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(powerAndASN, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(designSet, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(dataset, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(stageResults, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(analysisResults, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(designPlan, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(simulationResults, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(piecewiseSurvivalTime, niceColumnNamesEnabled = FALSE)))
	invisible(capture.output(as.data.frame(accrualTime, niceColumnNamesEnabled = FALSE)))
	
	invisible(capture.output(as.matrix(design)))
	invisible(capture.output(as.matrix(designFisher)))
	invisible(capture.output(as.matrix(designCharacteristics)))
	invisible(capture.output(as.matrix(powerAndASN)))
	invisible(capture.output(as.matrix(designSet)))
	invisible(capture.output(as.matrix(dataset)))
	invisible(capture.output(as.matrix(stageResults)))
	invisible(capture.output(as.matrix(analysisResults)))
	invisible(capture.output(as.matrix(designPlan)))
	invisible(capture.output(as.matrix(simulationResults)))
	invisible(capture.output(as.matrix(piecewiseSurvivalTime)))
	invisible(capture.output(as.matrix(accrualTime)))

})
