/**
 *
 * -- Type definitions --
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
 * File version: $Revision: 6705 $
 * Last changed: $Date: 2022-11-29 13:59:52 +0100 (Di, 29 Nov 2022) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

void assertIsInInterval(double x, Rcpp::CharacterVector xName, double lower, double upper,
		bool lowerInclusive, bool upperInclusive);

void assertIsInInterval(double x, Rcpp::CharacterVector xName, double lower, double upper);

