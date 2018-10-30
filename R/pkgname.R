
#' 
#' RPACT - R Package for Adaptive Clinical Trials
#' 
#' RPACT is a comprehensive package that enables the design and analysis of confirmatory 
#' adaptive group sequential designs. 
#' Particularly, the methods described in the recent 
#' \href{http://monograph.wassmer.brannath.rpact.com}{monograph by Wassmer and Brannath} 
#' (published by Springer, 2016) are implemented.
#' 
#' RPACT includes the classical group sequential designs (incl. user spending function approaches) 
#' where the sample sizes per stage 
#' (or the time points of interim analysis) cannot be changed in a data-driven way. 
#' Confirmatory adaptive designs explicitly allow for this under control of the Type I error rate.
#' They are either based on the combination testing or the conditional rejection probability (CRP) principle.
#' Both are available, for the former the inverse normal combination test and Fisher's combination test can be used.
#' 
#' Specific results of the adaptive methodology are also available, e.g., 
#' overall confidence intervals and p-values and conditional and predictive power assessments.
#' 
#' Designs are available for trials with continuous, binary, and survival endpoint.
#'
#' For more information please visit \href{https://www.rpact.org}{www.rpact.org}.
#' If you are interested in professional services round about the package or need 
#' a comprehensive validation documentation to fulfill regulatory requirements 
#' please visit \href{https://www.rpact.com}{www.rpact.com}.
#' 
#' RPACT is developed by 
#' \itemize{
#'   \item Gernot Wassmer (\href{mailto:gernot.wassmer@rpact.com}{gernot.wassmer@rpact.com}) and
#'   \item Friedrich Pahlke (\href{mailto:friedrich.pahlke@rpact.com}{friedrich.pahlke@rpact.com}).
#' }
#' 
#' @references 
#' Wassmer, G., Brannath, W. (2016) Group Sequential and Confirmatory Adaptive Designs 
#' in Clinical Trials (Springer Series in Pharmaceutical Statistics) <doi:10.1007/978-3-319-32562-0>
#' 
#' @useDynLib rpact, .registration = TRUE
#' 
#' @import methods
#' @import stats
#' @import utils
#' @import graphics
#' 
"_PACKAGE"
#> [1] "_PACKAGE"

.onLoad <- function(libname, pkgname) {
}

.onAttach <- function(libname, pkgname) {
	packageStartupMessage("Thank you for using RPACT! Need help or more information? Visit www.rpact.com")
}

.onUnload <- function(libpath) {
	if (!is.null(.parallelComputingCluster)) {
		tryCatch({	
			parallel::stopCluster(.parallelComputingCluster)
		}, error = function(e) {
			.logWarn("Failed to stop parallel computing cluster", e)
		})
	}
	tryCatch({	
		library.dynam.unload("rpact", libpath)
	}, error = function(e) {
		.logWarn("Failed to unload dynamic C library", e)
	})
}

.onDetach <- function(libpath) {
	packageStartupMessage("RPACT successfully unloaded\n")
}

