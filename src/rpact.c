#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>
//#include <R_ext/Applic.h> /* for dgemm */

double dnorm2(const double x, const double mean, const double stDev) {
    static const double inv_sqrt_2pi = 0.3989422804014327;
    double a = (x - mean) / stDev;

    return inv_sqrt_2pi / stDev * exp(-0.5f * a * a);
}

SEXP R_dnorm(SEXP x, SEXP mean, SEXP stDev) {
    double result = dnorm2(REAL(x)[0], REAL(mean)[0], REAL(stDev)[0]);
    return ScalarReal(result);
}

/**
 * k must be >= 3
 */
double getDensityValue(double x, int k, double *informationRates,
		double *epsilonVec, double *x2, double *dn2, int n) {

    int i;
    double resultValue;
    double prod;
    double dnormValue;

    k = k - 1;

    double part1 = sqrt(informationRates[k - 1] / epsilonVec[k - 1]);
    double sqrtInfRates1 = sqrt(informationRates[k - 1]);
    double sqrtInfRates2 = sqrt(informationRates[k - 2]);

    const double mean = 0;
    const double stDev = 1;

    double prod1 = x * sqrtInfRates1;
    double divisor = sqrt(epsilonVec[k - 1]);
    resultValue = 0;
    for (i = 0; i < n; i++) {
    	dnormValue = dnorm2((prod1 - (x2[i] * sqrtInfRates2)) / divisor, mean, stDev);
    	prod = part1 * dnormValue * dn2[i];
    	resultValue += prod;
    }

    //Free(p);
    //Free(dnormValues);

    return(resultValue);
}

void getDensityValues(double *x, int *k, double *informationRates,
		double *epsilonVec, double *x2, double *dn2, int n, double *results) {

    int i;
    for (i = 0; i < n; i++) {
    	if (*k == 2) {
    		results[i] = dnorm2(x[i], 0.0, 1.0);
    	} else {
    		results[i] = getDensityValue(x[i], *k, informationRates, epsilonVec, x2, dn2, n);
    	}
    }
}

/**
 * .Call interface to R_getDensityValues
 * \param x a numeric vector
 */
SEXP R_getDensityValues(SEXP x, SEXP k, SEXP informationRates, SEXP epsilonVec, SEXP x2, SEXP dn2)
{
    SEXP results;
    int n;

    n = LENGTH(x2);
    PROTECT(results = allocVector(REALSXP, n));
    getDensityValues(REAL(x), INTEGER(k), REAL(informationRates), REAL(epsilonVec),
    		REAL(x2), REAL(dn2), n, REAL(results));
    UNPROTECT(1);
    return(results);
}


