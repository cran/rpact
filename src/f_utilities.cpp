/**
 *
 * -- Simulation of survival data with group sequential and combination test --
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
 * File version: $Revision: 4248 $
 * Last changed: $Date: 2021-01-22 15:57:53 +0100 (Fri, 22 Jan 2021) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>
using namespace Rcpp;

NumericVector vectorSum(NumericVector x, NumericVector y) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x[i] + y[i];
	}
	return result;
}

NumericVector vectorSqrt(NumericVector x) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = sqrt(x[i]);
	}
	return result;
}

NumericVector vectorDivide(NumericVector x, double value) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x[i] / value;
	}
	return result;
}

NumericVector vectorDivide(NumericMatrix x, int rowIndex, double value) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x(rowIndex, i) / value;
	}
	return result;
}

NumericVector vectorDivide(NumericVector x, NumericVector y) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		if (y[i] != 0.0) {
			result[i] = x[i] / y[i];
		}
	}
	return result;
}

NumericVector vectorMultiply(NumericVector x, double multiplier) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x[i] * multiplier;
	}
	return result;
}

NumericVector vectorMultiply(NumericVector x, NumericVector y) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x[i] * y[i];
	}
	return result;
}

NumericVector vectorPow(NumericVector x, NumericVector y) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = pow(x[i], y[i]);
	}
	return result;
}

NumericVector vectorPow(double x, NumericVector y) {
	int n = y.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = pow(x, y[i]);
	}
	return result;
}

NumericVector vectorRepEachValue(NumericVector x, int kMax) {
	int n = x.size();
	NumericVector result = NumericVector(n * kMax, NA_REAL);
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < kMax; j++) {
			result[i * kMax + j] = x[i];
		}
	}
	return result;
}

double vectorProduct(NumericVector x) {
	int n = x.size();
	if (n == 0) {
		return 0;
	}

	if (n == 1) {
		return x[0];
	}

	double s = x[0];
	for (int i = 1; i < n; i++) {
		s *= x[i];
	}
	return s;
}

double vectorProduct(NumericVector x, NumericVector y) {
	int n = x.size();
	double s = 0;
	for (int i = 0; i < n; i++) {
		s += x[i] * y[i];
	}
	return s;
}

double round(double value, int digits) {
	double mult = std::pow(10.0, (double)digits);
	return round(value * mult) / mult;
}

void vectorSumC(int i, int j, int kMax, double* x, NumericMatrix y) {
	for (int k = 0; k < kMax; k++) {
    	x[i * kMax + k] += y(k, j);
    }
}

void vectorInitC(int i, int kMax, double* x, double value) {
    for (int k = 0; k < kMax; k++) {
    	x[i * kMax + k] = value;
    }
}

NumericVector concat(NumericVector a, NumericVector b) {
	for (int i = 0; i < b.size(); i++) {
		a.insert( a.end(), b[i] );
	}
	return a;
}

void logDebug(std::string s) {
	Rcout << s << std::endl;
}
