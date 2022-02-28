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
 * File version: $Revision: 5875 $
 * Last changed: $Date: 2022-02-23 08:12:38 +0100 (Mi, 23 Feb 2022) $
 * Last changed by: $Author: pahlke $
 *
 */

#include <Rcpp.h>
#include <cmath>
#include "f_utilities.h"
#include "f_simulation_survival_utilities.h"

using namespace Rcpp;

const int C_MAX_NUMBER_OF_ITERATIONS = 100;
const double C_UPPER_BOUNDS_DEFAULT = 8;
const double C_CONST_NEWTON_COTES = 15; // set to 5, 10, 15
const int M = C_CONST_NEWTON_COTES * 6 + 1; // number of grid points with constant of Newton Cotes algorithm (n * 6 + 1)
const double C_FUTILITY_BOUNDS_DEFAULT = -6;
const String C_TYPE_OF_DESIGN_AS_USER = "asUser";
const String C_TYPE_OF_DESIGN_BS_USER = "bsUser";
const String C_TYPE_OF_DESIGN_AS_P = "asP";
const String C_TYPE_OF_DESIGN_BS_P = "bsP";
const String C_TYPE_OF_DESIGN_AS_OF = "asOF";
const String C_TYPE_OF_DESIGN_BS_OF = "bsOF";
const String C_TYPE_OF_DESIGN_AS_KD = "asKD";
const String C_TYPE_OF_DESIGN_BS_KD = "bsKD";
const String C_TYPE_OF_DESIGN_AS_HSD = "asHSD";
const String C_TYPE_OF_DESIGN_BS_HSD = "bsHSD";
const String C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY = "noEarlyEfficacy";

double dnorm2(const double x, const double mean, const double stDev) {
	static const double inv_sqrt_2pi = 0.3989422804014327;
	double a = (x - mean) / stDev;

	return inv_sqrt_2pi / stDev * exp(-0.5f * a * a);
}

double getDensityValue(double x, int k, NumericVector informationRates,
		NumericVector epsilonVec, NumericVector x2, NumericVector dn2, int n) {

	k--;
	double part1 = sqrt((double) informationRates[k - 1] / (double) epsilonVec[k - 1]);
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

NumericVector getDensityValues(NumericVector x, int k,
		NumericVector informationRates, NumericVector epsilonVec,
		NumericVector x2, NumericVector dn2) {

	int n = x.size();
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

NumericVector getW(double dx) {
	NumericVector vec = NumericVector::create(492, 1296, 162, 1632, 162, 1296);
	vec = vectorMultiply(vec, dx / 840);
	vec = rep(vec, C_CONST_NEWTON_COTES); // M %/% 6 = 91 %/% 6 = 15
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
		vec[i] = (decisionMatrix(paramIndex, kIndex) * sqrt((double) informationRates[kIndex]) -
			x[i] * sqrt((double) informationRates[kIndex - 1])) / sqrt((double) epsilonVec[kIndex]);
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

NumericVector getGroupSequentialProbabilitiesFast(NumericMatrix decisionMatrix,
		NumericVector informationRates) {

	// maximum number of stages
	int kMax = informationRates.size();

	// probability matrix output
	NumericVector probs(kMax);

	double decValue = decisionMatrix(0, 0);
	if (decValue > C_UPPER_BOUNDS_DEFAULT) {
		decValue = C_UPPER_BOUNDS_DEFAULT;
	}
	probs[0] = getNormalDistribution(decValue);
	if (kMax == 1) {
		return probs;
	}

	NumericVector epsilonVec = NumericVector(informationRates.size(), NA_REAL);
	epsilonVec[0] = informationRates[0];
	for (int i = 1; i < epsilonVec.size(); i++) {
		epsilonVec[i] = informationRates[i] - informationRates[i - 1];
	}

	NumericMatrix decMatrix(Rcpp::clone(decisionMatrix));
	for (int i = 0; i < decMatrix.nrow(); i++) {
		for (int j = 0; j < decMatrix.ncol(); j++) {
			if (decMatrix(i, j) < C_FUTILITY_BOUNDS_DEFAULT) {
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

		x2 = x;
		dn2 = dn;
		probs[k - 1] = seq1;
	}

	return probs;
}

// [[Rcpp::export]]
NumericMatrix getGroupSequentialProbabilitiesCpp(NumericMatrix decisionMatrix,
		NumericVector informationRates) {

	NumericMatrix decMatrix(Rcpp::clone(decisionMatrix));

	for (int i = 0; i < decMatrix.nrow(); i++) {
		for (int j = 0; j < decMatrix.ncol(); j++) {
			if (decMatrix(i, j) >= C_UPPER_BOUNDS_DEFAULT) {
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
    double delst;
    double power;
    NumericMatrix helper(rows, kMax);
    NumericVector sqrtInformationRates = sqrt(informationRates);
    NumericVector deltaPT0KMaxInformationRates = pow(informationRates * kMax, deltaPT0 - 0.5);
    NumericVector deltaPT1KMaxInformationRates = pow(informationRates * kMax, deltaPT1 - 0.5);

    double pow1 = pow(kMax, deltaPT0 - 0.5);
    double pow2 = pow(kMax, deltaPT1 - 0.5);

    if (bindingFutility) {
        NumericMatrix decisionMatrix(rows, kMax);
        bizero([&](double c2m) {
        	bizero([&](double c1m) {
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

                return size - alpha;
            }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

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

            return 1.0 - beta - power;
        }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    } else { // non-binding
        double c1m = 0;
        bizero([&](double x) {
        	c1m = x;
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
            if (sided != 1) {
            	size += sum(probs.row(0));
            }
            return size - alpha;
        }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

        rejectionBounds = deltaPT1KMaxInformationRates * c1m;
        bizero([&](double c2m) {
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
            for (int i = 0; i < helper.nrow();i++) {
                helper.row(i) = sqrtInformationRates * delst;
            }

            NumericMatrix decisionMatrixH1 = matrixSub(decisionMatrix, helper);
            probs = getGroupSequentialProbabilitiesCpp(decisionMatrixH1, informationRates);
            if (sided == 1) {
            	power = sum(probs.row(2) - probs.row(1));
            } else {
            	power = sum(probs.row(4) + probs.row(0) - probs.row(3));
            }
            return 1.0 - beta - power;
        }, 0, 10, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    }
    return List::create(
    	_["futilityBounds"] = futilityBounds,
		_["criticalValues"] = rejectionBounds,
		_["probs"] = probs
	);
}

NumericMatrix getDecisionMatrixOneSided(int kMax,
		NumericVector criticalValues, NumericVector futilityBounds, bool bindingFutility) {
	NumericMatrix decisionMatrix(2, criticalValues.length());
	if (bindingFutility) {
		NumericVector helperVec = futilityBounds;
		// adds C_FUTILITY_BOUNDS_DEFAULT at the end of the vector, after its current last element
		helperVec.push_back(C_FUTILITY_BOUNDS_DEFAULT);
		decisionMatrix(0, _) = helperVec;
		decisionMatrix(1, _) = criticalValues;
	} else {
		decisionMatrix(0, _) = rep(C_FUTILITY_BOUNDS_DEFAULT, kMax);
		decisionMatrix(1, _) = criticalValues;
	}
	return decisionMatrix;
}

NumericMatrix getDecisionMatrixTwoSided(NumericVector criticalValues) {
	NumericMatrix decisionMatrix(2, criticalValues.length());
    decisionMatrix(0, _) = -criticalValues;
    decisionMatrix(1, _) = criticalValues;
	return decisionMatrix;
}

NumericMatrix getDecisionMatrixHelper(NumericMatrix decisionMatrix, int k) {
	NumericMatrix decisionMatrixHelper(2, k);
    for (int i = 0; i < k; i++) {
    	decisionMatrixHelper(_, i) = decisionMatrix(_, i); // decisionMatrix[, 1:k]
    }
	return decisionMatrixHelper;
}

NumericMatrix getDecisionMatrix(int kMax,
		NumericVector criticalValues, NumericVector futilityBounds,
		bool bindingFutility, int sided, int k = -1) {
	NumericMatrix decisionMatrix;
	if (sided == 1) {
		decisionMatrix = getDecisionMatrixOneSided(kMax,
			criticalValues, futilityBounds, bindingFutility);
	} else {
		decisionMatrix = getDecisionMatrixTwoSided(criticalValues);
	}
	if (k < 0) {
		return decisionMatrix;
	}
	return getDecisionMatrixHelper(decisionMatrix, k);
}

double getZeroApproximation(NumericMatrix probs, double alpha, int sided) {
	if (sided == 1) {
		return sum(probs(2, _) - probs(1, _)) - alpha;
	}

	return sum(probs(2, _) - probs(1, _) + probs(0, _)) - alpha;
}

// [[Rcpp::export]]
double getSpendingValueCpp(double alpha, double x, double sided, String typeOfDesign, double gamma) {
    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_P || typeOfDesign == C_TYPE_OF_DESIGN_BS_P) {
        return alpha * log(1 + (exp(1) - 1) * x);
    }

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_OF || typeOfDesign == C_TYPE_OF_DESIGN_BS_OF) {
        return 2 * sided * (1 - R::pnorm(getOneMinusQNorm(alpha / (2 * sided)) / sqrt(x), 0, 1, 1, 0));
    }

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_KD || typeOfDesign == C_TYPE_OF_DESIGN_BS_KD) {
        return alpha * pow(x, gamma);
    }

    if (typeOfDesign == C_TYPE_OF_DESIGN_AS_HSD || typeOfDesign == C_TYPE_OF_DESIGN_BS_HSD) {
        if (gamma == 0)	{
        	return alpha * x;
        }
        return alpha * (1 - exp(-gamma * x)) / (1 - exp(-gamma));
    }

    return NA_REAL;
}

double getCriticalValue(
		int kMax,
		int k,
		NumericVector criticalValues,
		NumericVector userAlphaSpending,
		double alpha,
		double gammaA,
		String typeOfDesign,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {

	double alphaSpendingValue;
	if (typeOfDesign == C_TYPE_OF_DESIGN_AS_USER) {
		alphaSpendingValue = userAlphaSpending[k - 1];
	} else {
		alphaSpendingValue = getSpendingValueCpp(alpha, (double) informationRates[k - 1], sided, typeOfDesign, gammaA);
	}

	if (k == 1) {
		return(getOneMinusQNorm(alphaSpendingValue / sided));
	}

	double criticalValue = NA_REAL;
	NumericVector criticalValuesTemp = Rcpp::clone(criticalValues);
    bisection2([&](double scale) {
    	criticalValue = scale;
    	criticalValuesTemp[k - 1] = criticalValue;
		NumericMatrix decisionMatrix = getDecisionMatrix(kMax,
			criticalValuesTemp, futilityBounds,
			bindingFutility, sided, k);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(
			decisionMatrix, rangeVector(informationRates, 0, k - 1));
		return getZeroApproximation(probs, alphaSpendingValue, sided);
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

    return criticalValue;
}

NumericVector getDesignGroupSequentialAlphaSpending(
		int kMax,
		NumericVector userAlphaSpending,
		double alpha,
		double gammaA,
		String typeOfDesign,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {

	NumericVector criticalValues(kMax);
    for (int k = 1; k <= kMax; k++) {
    	criticalValues[k - 1] = getCriticalValue(
			kMax,
			k,
			criticalValues,
			userAlphaSpending,
			alpha,
			gammaA,
			typeOfDesign,
			sided,
			informationRates,
			bindingFutility,
			futilityBounds,
			tolerance);
    }
    return criticalValues;
}

// [[Rcpp::export]]
NumericVector getDesignGroupSequentialUserDefinedAlphaSpendingCpp(
		int kMax,
		NumericVector userAlphaSpending,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialAlphaSpending(
		kMax,
		userAlphaSpending,
		NA_REAL,
		NA_REAL,
		C_TYPE_OF_DESIGN_AS_USER,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance);
}

// [[Rcpp::export]]
NumericVector getDesignGroupSequentialAlphaSpendingCpp(
		int kMax,
		double alpha,
		double gammaA,
		String typeOfDesign,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialAlphaSpending(
		kMax,
		NumericVector(0),
		alpha,
		gammaA,
		typeOfDesign,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance);
}

// [[Rcpp::export]]
NumericVector getDesignGroupSequentialDeltaWTCpp(
		int kMax,
		double alpha,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance,
		double deltaWT) {

	NumericVector criticalValues(kMax);
    double scale = bizero([&](double scale) {
		for (int k = 0; k < kMax; k++) {
			criticalValues[k] = scale * pow((double) informationRates[k], deltaWT - 0.5);
		}
		NumericMatrix decisionMatrix = getDecisionMatrix(kMax,
				criticalValues, futilityBounds,
				bindingFutility, sided);
		NumericMatrix probs = getGroupSequentialProbabilitiesCpp(
			decisionMatrix, informationRates);
		return getZeroApproximation(probs, alpha, sided);
	}, 0.0, 8.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);

	for (int k = 0; k < kMax; k++) {
		criticalValues[k] = scale * pow((double) informationRates[k], deltaWT - 0.5);
	}

    return criticalValues;
}

// [[Rcpp::export]]
NumericVector getDesignGroupSequentialPocockCpp(
		int kMax,
		double alpha,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialDeltaWTCpp(
		kMax,
		alpha,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance,
		0.5);
}

// [[Rcpp::export]]
NumericVector getDesignGroupSequentialOBrienAndFlemingCpp(
		int kMax,
		double alpha,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		NumericVector futilityBounds,
		double tolerance) {
    return getDesignGroupSequentialDeltaWTCpp(
		kMax,
		alpha,
		sided,
		informationRates,
		bindingFutility,
		futilityBounds,
		tolerance,
		0);
}

double getFutilityBound(int k,
		NumericVector betaSpendingValues,
		NumericVector informationRates,
		NumericVector futilityBounds,
		NumericVector criticalValues,
		double shift,
		double tolerance) {
	if (k == 1) {
		return getQNorm((double) betaSpendingValues[0]) + sqrt((double) informationRates[0]) * shift;
	}

	double futilityBound = NA_REAL;
	NumericVector futilityBoundsTemp = Rcpp::clone(futilityBounds);
	bisection2([&](double scale) {
		futilityBound = scale;
		futilityBoundsTemp[k - 1] = futilityBound;
		NumericMatrix decisionMatrix(2, futilityBoundsTemp.length());
		decisionMatrix(0, _) = futilityBoundsTemp - sqrt(informationRates) * shift;
		decisionMatrix(1, _) = criticalValues - sqrt(informationRates) * shift;
		NumericVector probs = getGroupSequentialProbabilitiesFast(
			getDecisionMatrixHelper(decisionMatrix, k),
			rangeVector(informationRates, 0, k - 1));
		return (double) betaSpendingValues[k - 1] - sum(probs);
	}, -6.0, 5.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
	return futilityBound;
}

NumericVector getFutilityBounds(int kMax,
		NumericVector betaSpendingValues,
		NumericVector informationRates,
		NumericVector criticalValues,
		double shift,
		double tolerance) {
	NumericVector futilityBounds = NumericVector(kMax, NA_REAL);
    for (int k = 1; k <= kMax; k++) {
    	futilityBounds[k - 1] = getFutilityBound(k,
			betaSpendingValues,
			informationRates,
			futilityBounds,
			criticalValues,
			shift,
			tolerance);
    }
	return futilityBounds;
}

// [[Rcpp::export]]
List getDesignGroupSequentialBetaSpendingCpp(
		NumericVector criticalValues,
		int kMax,
		NumericVector userAlphaSpending,
		NumericVector userBetaSpending,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		double tolerance,
		String typeOfDesign,
		String typeBetaSpending,
		double gammaA,
		double gammaB,
		double alpha,
		double beta
		) {

	if (typeOfDesign == C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
        for (int k = 0; k < kMax - 1; k++) {
        	userAlphaSpending[k] = 0;
        	criticalValues[k] = getQNormThreshold();
        }
        userAlphaSpending[kMax - 1] = alpha;
		criticalValues[kMax - 1] = getOneMinusQNorm(alpha / sided);
    }

	NumericVector betaSpendingValues;
	if (typeBetaSpending == C_TYPE_OF_DESIGN_BS_USER) {
		betaSpendingValues = userBetaSpending;
	} else {
		betaSpendingValues = NumericVector(kMax, NA_REAL);
		for (int k = 0; k < kMax; k++) {
			betaSpendingValues[k] = getSpendingValueCpp(beta, (double) informationRates[k], sided, typeBetaSpending, gammaB);
		}
	}

    NumericVector futilityBounds;
    double shiftResult;
    if (!bindingFutility) {
		shiftResult = bizero([&](double shift) {
            futilityBounds = getFutilityBounds(kMax,
				betaSpendingValues,
				informationRates,
				criticalValues,
				shift,
				tolerance);
            return (double) futilityBounds[kMax - 1] - (double) criticalValues[kMax - 1];
        }, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS); // bisection: -4.0, 10.0
    } else {
    	futilityBounds = NumericVector(kMax, NA_REAL);
    	shiftResult = bisection2([&](double shift) {
            for (int k = 1; k <= kMax; k++) {
                if (typeOfDesign != C_TYPE_OF_DESIGN_NO_EARLY_EFFICACY) {
                	criticalValues[k - 1] = getCriticalValue(
						kMax,
						k,
						criticalValues,
						userAlphaSpending,
						alpha,
						gammaA,
						typeOfDesign,
						sided,
						informationRates,
						bindingFutility,
						futilityBounds,
						tolerance);
                }

            	futilityBounds[k - 1] = getFutilityBound(k,
        			betaSpendingValues,
        			informationRates,
        			futilityBounds,
					criticalValues,
        			shift,
        			tolerance);
			}
            return (double) criticalValues[kMax - 1] - (double) futilityBounds[kMax - 1];
        }, -4.0, 10.0, tolerance, C_MAX_NUMBER_OF_ITERATIONS);
    	// zeroin: -8.0, 5.0
    	// bisection: -4.0, 10.0
    }
    return List::create(
    	_["futilityBounds"] = futilityBounds,
		_["criticalValues"] = criticalValues,
		_["shift"] = shiftResult
	);
}

// [[Rcpp::export]]
List getDesignGroupSequentialUserDefinedBetaSpendingCpp(
		NumericVector criticalValues,
		int kMax,
		NumericVector userAlphaSpending,
		NumericVector userBetaSpending,
		double sided,
		NumericVector informationRates,
		bool bindingFutility,
		double tolerance,
		String typeOfDesign,
		double gammaA,
		double alpha
		) {
	String typeBetaSpending = C_TYPE_OF_DESIGN_BS_USER;
	double gammaB = NA_REAL;
	double beta = NA_REAL;
	return getDesignGroupSequentialBetaSpendingCpp(
		criticalValues,
		kMax,
		userAlphaSpending,
		userBetaSpending,
		sided,
		informationRates,
		bindingFutility,
		tolerance,
		typeOfDesign,
		typeBetaSpending,
		gammaA,
		gammaB,
		alpha,
		beta
		);
}
