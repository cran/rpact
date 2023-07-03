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
 * File version: $Revision: 7019 $
 * Last changed: $Date: 2023-05-31 07:23:47 +0200 (Mi, 31 Mai 2023) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

void assertIsInInterval(double x, Rcpp::CharacterVector xName, double lower, double upper,
		bool lowerInclusive, bool upperInclusive);

void assertIsInInterval(double x, Rcpp::CharacterVector xName, double lower, double upper);


