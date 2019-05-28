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


#  Example in book p. 207f, one-treatment case, variance known 
getTestSettings1 = function() {
	
	kMax = 2
	
	nActual = c(20, 60)
	
	return(list(
		alpha = 0.025,
		kMax = kMax,
		typeOfDesign = C_TYPE_OF_DESIGN_WT,
		deltaWT = 0.25,
		fisherMethod = "equalAlpha",
		informationRates = (1 : kMax) / kMax,
		futilityBounds = c(stats::qnorm(0.7)),
		alpha0Vec = c(1),
		sided = 1,
		nActual = nActual,
		dataExample = DatasetMeans(dataFrame = data.frame(stage = (1 : kMax), 
			n1 = c(20, 20), 
			n2 = nActual, 
			means1 = c(0.32, 0.35), 
			means2 = c(1.92, 0.56), 
			stds1 = c(1, 1), 
			stds2 = c(1, 1))),
		stage = kMax
	))
}

getTestSettings2 = function() {
	
	kMax = 4
	
	return(list(
		alpha = 0.025,
		kMax = kMax,
		typeOfDesign = C_TYPE_OF_DESIGN_WT,
		deltaWT = 0.25,
		fisherMethod = "equalAlpha",
		informationRates = (1 : kMax) / kMax,
		sided = 1,
		futilityBoundsForPower = c(-0.5, 0, 0.5),
		futilityBounds = rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - 1),
		
		alpha0Vec = c(0.7, 0.6, 0.5),
		alpha0Vec = rep(1, kMax - 1),
		nPlanned = rep(11, kMax),
		nActual = rep(11, kMax),
		
		dataExample = DatasetMeans(dataFrame = data.frame(stage = (1 : kMax), 
			n1 = c(8, 10, 9, 11), 
			n2 = c(11, 13, 12, 13), 
			means1 = c(323, 514, 511, 611), 
			means2 = c(452, 561, 635, 698), 
			stds1 = c(111, 131, 145, 111), 
			stds2 = c(118, 117, 104, 119))),
		
		stage = kMax
	))
}

getTestSettings3 = function() {
	
	kMax = 4
	
	return(list(
		alpha = 0.025,
		kMax = kMax,
		typeOfDesign = C_TYPE_OF_DESIGN_WT_OPTIMUM,
		deltaWT = 0.25,
		fisherMethod = "equalAlpha",
		informationRates = (1 : kMax) / kMax,
		sided = 1,
		futilityBoundsForPower = c(-0.5, 0, 0.5),
		futilityBounds = rep(C_FUTILITY_BOUNDS_DEFAULT, kMax - 1),
		
		alpha0Vec = c(0.7, 0.6, 0.5),
		alpha0Vec = rep(1, kMax - 1),
		nPlanned = rep(11, kMax),
		nActual = rep(11, kMax),
		
		dataExample = DatasetMeans(dataFrame = data.frame(stage = (1 : kMax), 
			n1 = c(8, 10, 9, 11), 
			n2 = c(11, 13, 12, 13), 
			means1 = c(323, 514, 511, 611), 
			means2 = c(452, 561, 635, 698), 
			stds1 = c(111, 131, 145, 111), 
			stds2 = c(118, 117, 104, 119))),
		
		stage = kMax
	))
}

