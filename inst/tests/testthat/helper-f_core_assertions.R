######################################################################################
#                                                                                    #
# -- Unit test helper functions --                                                   #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 07-11-2018                                                                   #
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


getAssertionTestDesign <- function(kMax = NA_integer_, informationRates = NA_real_, futilityBounds = NA_real_,
	designClass = C_CLASS_NAME_TRIAL_DESIGN_INVERSE_NORMAL) {
	
	if (designClass == C_CLASS_NAME_TRIAL_DESIGN_FISHER) {
		return(TrialDesignFisher(
				kMax = kMax, 
				alpha = C_ALPHA_DEFAULT, 
				method = C_FISHER_METHOD_DEFAULT, 
				alpha0Vec = futilityBounds,  
				informationRates = informationRates, 
				tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT,
				iterations = 0,
				seed = 9498485
			))
	}	
	
	return(.createDesign(
			designClass             = designClass,
			kMax                    = kMax, 
			alpha                   = C_ALPHA_DEFAULT, 
			beta                    = C_BETA_DEFAULT, 
			sided                   = 1, 
			informationRates        = informationRates, 
			futilityBounds          = futilityBounds, 		
			typeOfDesign            = C_DEFAULT_TYPE_OF_DESIGN, 
			delta                   = 0, 
			optimizationCriterion   = C_OPTIMIZATION_CRITERION_DEFAULT, 
			gammaA                  = 1, 
			typeBetaSpending        = C_TYPE_OF_DESIGN_BS_NONE, 
			userAlphaSpending       = NA_real_, 
			userBetaSpending        = NA_real_, 
			gammaB                  = 1, 
			tolerance               = 1e-06))
}
