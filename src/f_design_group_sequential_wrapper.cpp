/**
 *
 * -- Group sequential design --
 *
 * This file is part of the R package rpact:
 * Confirmatory Adaptive Clinical Trial Design and Analysis
 *
 * Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
 * Licensed under "GNU Lesser General Public License" version 3
 * License text can be found here: https://www.r-project.org/Licenses/LGPL-3
 *
 * RPACT company website: https://www.rpact.com
 * rpact package website: https://www.rpact.org
 *
 * Contact us for information about our services: info@rpact.com
 *
 * File version: $Revision: 3101 $
 * Last changed: $Date: 2020-04-30 12:52:39 +0200 (Do, 30 Apr 2020) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>
using namespace Rcpp;

// See https://stackoverflow.com/questions/54000015/r-package-with-both-c-and-cpp-files-with-rcpp

extern "C" SEXP R_getDensityValues(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

// [[Rcpp::export]]
SEXP getDensityValuesFast(SEXP x, SEXP k, SEXP informationRates, SEXP epsilonVec, SEXP x2, SEXP dn2) {
	return R_getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
}


