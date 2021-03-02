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
 * File version: $Revision$
 * Last changed: $Date$
 * Last changed by: $Author$
 *
 */

#include <Rcpp.h>
#include "f_utilities.h"
using namespace Rcpp;

double dnorm2(const double x, const double mean, const double stDev) {
	static const double inv_sqrt_2pi = 0.3989422804014327;
	double a = (x - mean) / stDev;

	return inv_sqrt_2pi / stDev * exp(-0.5f * a * a);
}

double getDensityValue(double x, int k, NumericVector informationRates,
		NumericVector epsilonVec, NumericVector x2, NumericVector dn2, int n) {

	k--;
	double part1 = sqrt(informationRates[k - 1] / epsilonVec[k - 1]);
	double sqrtInfRates1 = sqrt(informationRates[k - 1]);
	double sqrtInfRates2 = sqrt(informationRates[k - 2]);

	const double mean = 0;
	const double stDev = 1;

	double prod1 = x * sqrtInfRates1;
	double divisor = sqrt(epsilonVec[k - 1]);
	double resultValue = 0;
	for (int i = 0; i < n; i++) {
		double dnormValue = dnorm2((prod1 - (x2[i] * sqrtInfRates2)) / divisor,
				mean, stDev);
		double prod = part1 * dnormValue * dn2[i];
		resultValue += prod;
	}

	return resultValue;
}

NumericVector getDensityValues(NumericVector x, int k,
		NumericVector informationRates, NumericVector epsilonVec,
		NumericVector x2, NumericVector dn2) {

	int n = x.size();
	NumericVector results = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		if (k == 2) {
			results[i] = dnorm2(x[i], 0.0, 1.0);
		} else {
			results[i] = getDensityValue(x[i], k, informationRates, epsilonVec, x2, dn2, n);
		}
	}
	return results;
}

NumericVector getW(double dx) {
	NumericVector vec = NumericVector::create(492, 1296, 162, 1632, 162, 1296);
	vec = vectorMultiply(vec, dx / 840);
	vec = rep(vec, 15); // M %/% 6 = 91 %/% 6 = 15
	double x = 246.0 * dx / 840.0;
	NumericVector result = NumericVector(vec.size() + 1, NA_REAL);
	result[0] = x;
	for (int i = 1; i < vec.size(); i++) {
		result[i] = vec[i];
	}
	result[result.size() - 1] = x;
	return result;
}

double getSeqValue(int paramIndex, int k,
		NumericVector dn, NumericVector x,
		NumericMatrix decisionMatrix,
		NumericVector informationRates, NumericVector epsilonVec) {

	int kIndex = k - 1;
	NumericVector vec = NumericVector(x.size(), NA_REAL);
	for (int i = 0; i < x.size(); i++) {
		vec[i] = (decisionMatrix(paramIndex, kIndex) * sqrt(informationRates[kIndex]) -
			x[i] * sqrt(informationRates[kIndex - 1])) / sqrt(epsilonVec[kIndex]);
	}
	vec = pnorm(as<NumericVector>(vec));
	return vectorProduct(vec, dn);
}

double getDxValue(NumericMatrix decisionMatrix, int k, int M, int rowIndex) {
	return (decisionMatrix(rowIndex + 1, k - 2) - decisionMatrix(rowIndex, k - 2)) / (M - 1);
}

NumericVector getXValues(NumericMatrix decisionMatrix, int k, int M, int rowIndex) {
	NumericVector x = rep(decisionMatrix(rowIndex, k - 2), M);
	double dx = getDxValue(decisionMatrix, k, M, rowIndex);
	for (int i = 0; i < x.size(); i++) {
		x[i] = x[i] + i * dx;
	}
	return x;
}

// [[Rcpp::export]]
NumericMatrix getGroupSequentialProbabilitiesCpp(NumericMatrix decisionMatrix,
		NumericVector informationRates) {

	double C_UPPER_BOUNDS_DEFAULT = 8;
	double C_CONST_NEWTON_COTES = 15;
	double C_FUTILITY_BOUNDS_DEFAULT = -6;

	NumericMatrix decMatrix(Rcpp::clone(decisionMatrix));

	for (int i = 0; i < decMatrix.nrow(); i++) {
		for (int j = 0; j < decMatrix.ncol(); j++) {
			if (decMatrix(i, j) >= C_UPPER_BOUNDS_DEFAULT) {
				decMatrix(i, j) = C_UPPER_BOUNDS_DEFAULT;
			}
		}
	}

	// number of grid points with constant of Newton Cotes algorithm (n * 6 + 1)
	int M = C_CONST_NEWTON_COTES * 6 + 1;

	// maximum number of stages
	int kMax = informationRates.size();

	// probability matrix output
	NumericMatrix probs(decMatrix.nrow() + 1, kMax);

	NumericVector pnormValues = pnorm(decMatrix(_, 0));
	for (int i = 0; i < pnormValues.size(); i++) {
		probs(i, 0) = pnormValues[i];
	}
	probs(probs.nrow() - 1, 0) = 1;
	if (kMax <= 1) {
		return probs;
	}

	NumericVector epsilonVec = NumericVector(informationRates.size(), NA_REAL);
	epsilonVec[0] = informationRates[0];
	for (int i = 1; i < epsilonVec.size(); i++) {
		epsilonVec[i] = informationRates[i] - informationRates[i - 1];
	}

	if (decMatrix.nrow() == 2) {

		for (int i = 0; i < decMatrix.nrow(); i++) {
			for (int j = 0; j < decMatrix.ncol(); j++) {
				if (decMatrix(i, j) <= C_FUTILITY_BOUNDS_DEFAULT) {
					decMatrix(i, j) = C_FUTILITY_BOUNDS_DEFAULT;
				}
			}
		}

		// density values in recursion
		NumericVector dn2 = NumericVector(M, NA_REAL);

		// grid points in recursion
		NumericVector x2  = NumericVector(M, NA_REAL);

		for (int k = 2; k <= kMax; k++) {
			double dx = getDxValue(decMatrix, k, M, 0);

			NumericVector x = getXValues(decMatrix, k, M, 0);
			NumericVector w = getW(dx);
			NumericVector densityValues = getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
			NumericVector dn = vectorMultiply(w, densityValues);

			double seq1 = getSeqValue(0, k, dn, x, decMatrix, informationRates, epsilonVec);
			double seq2 = getSeqValue(1, k, dn, x, decMatrix, informationRates, epsilonVec);

			x2 = x;
			dn2 = dn;
			probs(0, k - 1) = seq1;
			probs(1, k - 1) = seq2;
			probs(2, k - 1) = probs(1, k - 2) - probs(0, k - 2);
		}

	}
	else if (decMatrix.nrow() == 4) {

		for (int i = 0; i < decMatrix.nrow(); i++) {
			for (int j = 0; j < decMatrix.ncol(); j++) {
				if (decMatrix(i, j) <= -C_UPPER_BOUNDS_DEFAULT) {
					decMatrix(i, j) = -C_UPPER_BOUNDS_DEFAULT;
				}
			}
		}

		// density values in recursion
		NumericVector dn2 = NumericVector(2 * M, NA_REAL);

		// grid points in recursion
		NumericVector x2  = NumericVector(2 * M, NA_REAL);

		for (int k = 2; k <= kMax; k++) {
			double dx0 = getDxValue(decMatrix, k, M, 0);
			double dx1 = getDxValue(decMatrix, k, M, 2);

			NumericVector x0 = getXValues(decMatrix, k, M, 0);
			NumericVector x1 = getXValues(decMatrix, k, M, 2);
			NumericVector x = concat(x0, x1);

			NumericVector w0 = getW(dx0);
			NumericVector w1 = getW(dx1);
			NumericVector w = concat(w0, w1);

			NumericVector densityValues = getDensityValues(x, k, informationRates, epsilonVec, x2, dn2);
			NumericVector dn = vectorMultiply(w, densityValues);

			double seq1 = getSeqValue(0, k, dn, x, decMatrix, informationRates, epsilonVec);
			double seq2 = getSeqValue(1, k, dn, x, decMatrix, informationRates, epsilonVec);
			double seq3 = getSeqValue(2, k, dn, x, decMatrix, informationRates, epsilonVec);
			double seq4 = getSeqValue(3, k, dn, x, decMatrix, informationRates, epsilonVec);

			x2 = x;
			dn2 = dn;
			probs(0, k - 1) = seq1;
			probs(1, k - 1) = seq2;
			probs(2, k - 1) = seq3;
			probs(3, k - 1) = seq4;
			probs(4, k - 1) = probs(3, k - 2) - probs(2, k - 2) + probs(1, k - 2) - probs(0, k - 2);
		}
	}

	return probs;
}

