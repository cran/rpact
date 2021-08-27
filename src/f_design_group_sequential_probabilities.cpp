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
#include <cmath>
#include "f_utilities.h"

using namespace Rcpp;

double dnorm2(const double x, const double mean, const double stDev) {
	static const double inv_sqrt_2pi = 0.3989422804014327;
	double a = (x - mean) / stDev;

	return inv_sqrt_2pi / stDev * exp(-0.5f * a * a);
}

double getDensityValue(const double x, int k, NumericVector informationRates,
		NumericVector epsilonVec, NumericVector x2, NumericVector dn2, int n) {

	k--;
	double part1 = sqrt((double) informationRates[k - 1] / epsilonVec[k - 1]);
	double sqrtInfRates1 = sqrt((double) informationRates[k - 1]);
	double sqrtInfRates2 = sqrt((double) informationRates[k - 2]);

	const double mean = 0;
	const double stDev = 1;

	double prod1 = x * sqrtInfRates1;
	double divisor = sqrt((double) epsilonVec[k - 1]);
	double resultValue = 0;
	for (int i = 0; i < n; i++) {
		double dnormValue = dnorm2((prod1 - (x2[i] * sqrtInfRates2)) / divisor,
				mean, stDev);
		double prod = part1 * dnormValue * dn2[i];
		resultValue += prod;
	}

	return resultValue;
}

NumericVector getDensityValues(const NumericVector x, const int k,
		const NumericVector informationRates, const NumericVector epsilonVec,
		const NumericVector x2, const NumericVector dn2) {

	const int n = x.size();
	NumericVector results = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		if (k == 2) {
			results[i] = dnorm2((double) x[i], 0.0, 1.0);
		} else {
			results[i] = getDensityValue((double) x[i], k, informationRates, epsilonVec, x2, dn2, n);
		}
	}
	return results;
}

// [[Rcpp::export]]
NumericVector getW(double dx, const int M) {
	NumericVector vec = NumericVector::create(492.0, 1296.0, 162.0, 1632.0, 162.0, 1296.0);
	vec = vec * dx / 840;
	int repFactor = (int) ((double) M / 6); // 15
	vec = rep(vec, repFactor); // M %/% 6 = 91 %/% 6 = 15
	const double x = 246.0 * dx / 840.0;
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
		double value = decisionMatrix(paramIndex, kIndex);
		vec[i] = (value * sqrt((double) informationRates[kIndex]) -
			x[i] * sqrt((double) informationRates[kIndex - 1])) / sqrt((double) epsilonVec[kIndex]);
	}
	vec = pnorm(as<NumericVector>(vec));
	return vectorProduct(vec, dn);
}

double getDxValue(NumericMatrix decisionMatrix, int k, int M, int rowIndex) {
	double a = decisionMatrix(rowIndex + 1, k - 2);
	double b = decisionMatrix(rowIndex, k - 2);
	return (a - b) / (M - 1);
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
NumericMatrix getGroupSequentialProbabilitiesCpp(
		NumericMatrix decisionMatrix,
		NumericVector informationRates) {

	const double C_UPPER_BOUNDS_DEFAULT = 8;
	const double C_FUTILITY_BOUNDS_DEFAULT = -6;

	NumericMatrix decMatrix(Rcpp::clone(decisionMatrix));

	for (int i = 0; i < decMatrix.nrow(); i++) {
		for (int j = 0; j < decMatrix.ncol(); j++) {
			if (!R_IsNA(decMatrix(i, j)) && decMatrix(i, j) >= C_UPPER_BOUNDS_DEFAULT) {
				decMatrix(i, j) = C_UPPER_BOUNDS_DEFAULT;
			}
		}
	}

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
				if (!R_IsNA(decMatrix(i, j)) && decMatrix(i, j) <= C_FUTILITY_BOUNDS_DEFAULT) {
					decMatrix(i, j) = C_FUTILITY_BOUNDS_DEFAULT;
				}
			}
		}

		const int C_CONST_NEWTON_COTES = 15;

		// number of grid points with constant of Newton Cotes algorithm (n * 6 + 1)
		const int M = C_CONST_NEWTON_COTES * 6 + 1;

		// density values in recursion
		NumericVector dn2 = NumericVector(M, NA_REAL);

		// grid points in recursion
		NumericVector x2  = NumericVector(M, NA_REAL);

		for (int k = 2; k <= kMax; k++) {
			double dx = getDxValue(decMatrix, k, M, 0);

			NumericVector x = getXValues(decMatrix, k, M, 0);
			NumericVector w = getW(dx, M);
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
				if (!R_IsNA(decMatrix(i, j)) && decMatrix(i, j) <= -C_UPPER_BOUNDS_DEFAULT) {
					decMatrix(i, j) = -C_UPPER_BOUNDS_DEFAULT;
				}
			}
		}

		const int C_CONST_NEWTON_COTES = 8;

		// number of grid points with constant of Newton Cotes algorithm (n * 6 + 1)
		const int M = C_CONST_NEWTON_COTES * 6 + 1;

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

			NumericVector w0 = getW(dx0, M);
			NumericVector w1 = getW(dx1, M);
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

// [[Rcpp::export]]
List getDesignGroupSequentialPampallonaTsiatisCpp(
		double tolerance, double beta, double alpha, double kMax, double deltaPT0,
		double deltaPT1, NumericVector informationRates, int sided,
		bool bindingFutility) {

    NumericVector futilityBounds(kMax);
    NumericVector rejectionBounds(kMax);
    NumericMatrix probs(5, kMax);
    int rows = sided == 1 ? 2 : 4;
    double size;
    double prec2 = 1;
    double cLower2 = 0;
    double cUpper2 = 10;
    double c2m;
    double delst;
    double power;
    double prec1 = 1;
    double cUpper1 = 10;
    double cLower1 = 0;
    NumericMatrix helper(rows, kMax);
    NumericVector sqrtInformationRates = sqrt(informationRates);
    NumericVector deltaPT0KMaxInformationRates = pow(informationRates * kMax, deltaPT0 - 0.5);
    NumericVector deltaPT1KMaxInformationRates = pow(informationRates * kMax, deltaPT1 - 0.5);

    double pow1 = pow(kMax, deltaPT0 - 0.5);
    double pow2 = pow(kMax, deltaPT1 - 0.5);

    if (bindingFutility) {
        NumericMatrix decisionMatrix(rows, kMax);
        while (prec2 > tolerance) {
            c2m = (cLower2 + cUpper2) / 2;
            prec1 = 1;
            cUpper1 = 10;
            cLower1 = 0;
            double c1m;
            while (prec1 > tolerance) {
                c1m = (cLower1 + cUpper1) / 2;
                delst = c2m * pow1 + c1m * pow2;
                futilityBounds = sqrtInformationRates * delst - deltaPT0KMaxInformationRates * c2m;
                rejectionBounds = deltaPT1KMaxInformationRates * c1m;
                for (int i = 0; i < futilityBounds.length(); i++) {
                    if (futilityBounds[i] > rejectionBounds[i]) {
                    	futilityBounds[i] = rejectionBounds[i];
                    }
                    if (sided == 2 && futilityBounds[i] < 0) {
                    	futilityBounds[i] = 0;
                    }
                }

                if (sided == 1) {
                    decisionMatrix.row(0) = futilityBounds;
                    decisionMatrix.row(1) = rejectionBounds;
                } else {
                    decisionMatrix.row(0) = -rejectionBounds;
                    decisionMatrix.row(1) = -futilityBounds;
                    decisionMatrix.row(2) = futilityBounds;
                    decisionMatrix.row(3) = rejectionBounds;
                }

                probs = getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates);

                if (sided == 1) {
                	size = sum(probs.row(2) - probs.row(1));
                } else {
                	size = sum(probs.row(4) - probs.row(3) + probs.row(0));
                }

                if (size < alpha) {
                    cUpper1 = c1m;
                } else {
                	cLower1 = c1m;
                }
                prec1 = cUpper1 - cLower1;
            }

            for (int i = 0; i < rows; i++) {
            	helper.row(i) = sqrtInformationRates * delst;
            }

            NumericMatrix decisionMatrixH1 = matrixSub(decisionMatrix, helper);
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrixH1, informationRates);

            if (sided == 1) {
            	power = sum(probs.row(2) - probs.row(1));
            } else {
            	power = sum(probs.row(4) - probs.row(3) + probs.row(0));
            }

            if (power > 1.0 - beta) {
                cUpper2 = c2m;
            } else {
            	cLower2 = c2m;
            }
            prec2 = cUpper2 - cLower2;
        }
    } else {
    	double c1m = 0;
        while (prec1 > tolerance) {
            c1m = (cLower1 + cUpper1) / 2;
            rejectionBounds = deltaPT1KMaxInformationRates * c1m;
            NumericMatrix decisionMatrix(2, kMax);

            if (sided == 1) {
            	decisionMatrix.row(0) = rep(-6, kMax);
            } else {
            	decisionMatrix.row(0) = -rejectionBounds;
            }

            decisionMatrix.row(1) = rejectionBounds;
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrix, informationRates);
            size = sum(probs.row(2) - probs.row(1));
            if (sided != 1) size += sum(probs.row(0));
            if (size < alpha) {
                cUpper1 = c1m;
            } else {
            	cLower1 = c1m;
            }
            prec1 = cUpper1 - cLower1;
        }
        rejectionBounds = deltaPT1KMaxInformationRates * c1m;
        while (prec2 > tolerance) {
            c2m = (cLower2 + cUpper2) / 2;
            delst = c2m * pow1 + c1m * pow2;
            futilityBounds = sqrtInformationRates * delst - deltaPT0KMaxInformationRates * c2m;
            for (int i = 0; i < futilityBounds.length(); i++) {
                if (futilityBounds[i] > rejectionBounds[i]) {
                	futilityBounds[i] = rejectionBounds[i];
                }
            }
            NumericMatrix decisionMatrix(rows,kMax);

            if (sided == 1) {
                decisionMatrix.row(0) = futilityBounds;
                decisionMatrix.row(1) = rejectionBounds;
            } else {
                for (int i = 0; i < futilityBounds.length(); i++) {
                    if (futilityBounds[i] < 0) {
                    	futilityBounds[i] = 0;
                    }
                }
                decisionMatrix.row(0) = -rejectionBounds;
                decisionMatrix.row(1) = -futilityBounds;
                decisionMatrix.row(2) = futilityBounds;
                decisionMatrix.row(3) = rejectionBounds;
            }
              for(int i = 0; i < helper.nrow();i++) {
              	helper.row(i) = sqrtInformationRates * delst;
              }

			NumericMatrix decisionMatrixH1 = matrixSub(decisionMatrix,helper);
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrixH1, informationRates);

            if (sided == 1) power = sum(probs.row(2) - probs.row(1));
            else power = sum(probs.row(4) + probs.row(0) - probs.row(3));
            if (power > 1 - beta) {
                cUpper2 = c2m;
            } else {
                cLower2 = c2m;
            }
            prec2 = cUpper2 - cLower2;
        }
    }
    return List::create(futilityBounds, rejectionBounds, probs);
}
