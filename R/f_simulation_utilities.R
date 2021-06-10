#:#
#:#  *Simulation of multiarm design with combination test and conditional error approach*
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
#:#  File version: $Revision: 4863 $
#:#  Last changed: $Date: 2021-05-11 19:50:08 +0200 (Di, 11 Mai 2021) $
#:#  Last changed by: $Author: pahlke $
#:# 

.getSimulationParametersFromRawData <- function(data, ..., variantName = c("alternative", "pi1"), 
		maxNumberOfIterations = max(data$iterationNumber)) {
	
	if (!is.data.frame(data)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'data' (", class(data), ") must be a data.frame")
	}
		
	variantName <- match.arg(variantName)
		
	stageNumbers <- sort(unique(na.omit(data$stageNumber)))
	kMax <- max(stageNumbers)
	
	variantLevels <- sort(unique(na.omit(data[[variantName]])))
	numberOfVariants <- length(variantLevels)
	sampleSizes <- matrix(0, kMax, numberOfVariants)
	rejectPerStage <- matrix(0, kMax, numberOfVariants)
	futilityPerStage <- matrix(0, kMax - 1, numberOfVariants)
	expectedNumberOfSubjects <- rep(0, numberOfVariants)
	conditionalPowerAchieved <- matrix(NA_real_, kMax, numberOfVariants)	
	
	index <- 1
	for (variantValue in variantLevels) {
		subData <- data[data[[variantName]] == variantValue, ]
		iterations <- table(subData$stageNumber)
		for (k in sort(unique(na.omit(subData$stageNumber)))) {
			subData2 <- subData[subData$stageNumber == k, ]
			sampleSizes[k, index]  <- sum(subData2$numberOfSubjects) / iterations[k]
			rejectPerStage[k, index] <- sum(subData2$rejectPerStage) / maxNumberOfIterations
			if (k < kMax) {
				futilityPerStage[k, index] <- sum(na.omit(subData2$futilityPerStage)) / maxNumberOfIterations
			}
			expectedNumberOfSubjects[index] <- expectedNumberOfSubjects[index] + sum(subData2$numberOfSubjects) / maxNumberOfIterations 	
			if (k > 1) {
				conditionalPowerAchieved[k, index] <- sum(subData$conditionalPowerAchieved[subData$stageNumber == k]) / iterations[k]
			}
		}
		
		index <- index + 1
	}
	overallReject <- colSums(rejectPerStage)
	futilityStop <- colSums(futilityPerStage)
	iterations <- table(data$stageNumber, data[[variantName]])
	
	if (kMax > 1) {
		if (numberOfVariants == 1) {
			earlyStop <- sum(futilityPerStage) + sum(rejectPerStage[1:(kMax - 1)])
		} else {	
			if (kMax > 2) {
				rejectPerStageColSum <- colSums(rejectPerStage[1:(kMax - 1), ])
			} else {
				rejectPerStageColSum <- rejectPerStage[1, ]
			}
			earlyStop <- colSums(futilityPerStage) + rejectPerStageColSum
		}
	} else {
		earlyStop <- rep(0, numberOfVariants)
	}
	
	sampleSizes[is.na(sampleSizes)] <- 0
	
	return(list(
		sampleSizes = sampleSizes,
		rejectPerStage = rejectPerStage,
		overallReject = overallReject,
		futilityPerStage = futilityPerStage,
		futilityStop = futilityStop,
		iterations = iterations,
		earlyStop = earlyStop,
		expectedNumberOfSubjects = expectedNumberOfSubjects,
		conditionalPowerAchieved = conditionalPowerAchieved
	))
}

