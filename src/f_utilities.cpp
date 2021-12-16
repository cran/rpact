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

// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::string getCipheredValue(String x) {
	std::size_t hashValue = std::hash<std::string>{}(x);
	return std::to_string(hashValue);
}

std::string toString(const double i) {
    std::ostringstream ostr;
    ostr << i;
    return ostr.str();
}

template <int RTYPE>
IntegerVector order_impl(const Vector<RTYPE>& x, bool desc) {
    auto n = x.size();
    IntegerVector idx = no_init(n);
    std::iota(idx.begin(), idx.end(), static_cast<size_t>(1));
    if (desc) {
        auto comparator = [&x](size_t a, size_t b){ return x[a - 1] > x[b - 1]; };
        std::stable_sort(idx.begin(), idx.end(), comparator);
    } else {
        auto comparator = [&x](size_t a, size_t b){ return x[a - 1] < x[b - 1]; };
        std::stable_sort(idx.begin(), idx.end(), comparator);
        // simulate na.last
        size_t nas = 0;
        for (int i = 0; i < n; ++i, ++nas)
            if (!Vector<RTYPE>::is_na(x[idx[i] - 1])) break;
        std::rotate(idx.begin(), idx.begin() + nas, idx.end());
    }
    return idx;
}

// identical to the R function base::order()
IntegerVector getOrder(SEXP x, bool desc = false) {
    switch(TYPEOF(x)) {
    case INTSXP: return order_impl<INTSXP>(x, desc);
    case REALSXP: return order_impl<REALSXP>(x, desc);
    case STRSXP: return order_impl<STRSXP>(x, desc);
    default: stop("Unsupported type.");
    }
    return IntegerVector::create();
}

NumericVector vectorSum(NumericVector x, NumericVector y) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x[i] + y[i];
	}
	return result;
}

NumericVector vectorSub(NumericVector x, NumericVector y) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = x[i] - y[i];
	}
	return result;
}

double vectorSum(NumericVector x) {
    int n = x.size();
    if (n <= 1) {
        return n == 0 ? 0 : x[0];
    }

    double s = x[0];
    for (int i = 1; i < n; i++) {
        s += x[i];
    }
    return s;
}

NumericVector vectorSqrt(NumericVector x) {
	int n = x.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = sqrt((double) x[i]);
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
		result[i] = pow((double) x[i], (double) y[i]);
	}
	return result;
}

NumericVector vectorPow(double x, NumericVector y) {
	int n = y.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = pow(x, (double) y[i]);
	}
	return result;
}

NumericVector vectorPow2(NumericVector y, double exp) {
	int n = y.size();
	NumericVector result = NumericVector(n, NA_REAL);
	for (int i = 0; i < n; i++) {
		result[i] = pow((double) y[i], exp);
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

NumericMatrix matrixAdd(NumericMatrix x, NumericMatrix y) {
	NumericMatrix result(x.nrow(),x.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < x.ncol(); ++j) {
            result(i,j) = x(i,j) + y(i,j);
        }
    }
    return result;
}

NumericMatrix matrixSub(NumericMatrix x, NumericMatrix y) {
	NumericMatrix result(x.nrow(), x.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < x.ncol(); ++j) {
            result(i, j) = x(i, j) - y(i, j);
        }
    }
    return result;
}

NumericMatrix matrixMultiply(NumericMatrix x, double y) {
	NumericMatrix result(x.nrow(),x.ncol());
    for (int i = 0; i < x.nrow(); ++i) {
        for (int j = 0; j < x.ncol(); ++j) {
            result(i,j) = x(i,j) * y;
        }
    }
    return result;
}

NumericVector repInt(int x, int y) {
	NumericVector result(y);
	for(int i = 0; i < y ; i++) {
		result[i] = x;
	}
	return result;
}

std::string vectorToString(NumericVector x) {
	if (x.length() == 0) return "[]";
	std::ostringstream os;
	os << "[";
	for(int i = 0; i < x.length(); i++) {
		os << x[i];
		if(i + 1 < x.length()) os << ", ";
	}
	os << "]";
	return os.str();
}

double secant(std::function<double(double)> f, double x0, double x1, double tolerance, int maxIter) {
    int step = 1;
    double f0, f1, f2, x2;
    do {
        f0 = f(x0);
        f1 = f(x1);
        if (f0 == f1) {
            f0 = f(x0 + (x0 / 2.0));
            throw std::invalid_argument("Mathematical Error: m is 0");
        }
        x2 = x1 - f1 * (x1 - x0) / (f1 - f0);
        f2 = f(x2);

        x0 = x1;
        f0 = f1;
        x1 = x2;
        f1 = f2;

        step++;
        if (step > maxIter) {
            throw std::invalid_argument("Not convergent.");
        }
    } while (abs(f2) > tolerance);
    return x2;
}

double max(NumericVector x) {
	if(x.length() == 0) throw std::invalid_argument("Vector is Empty.");
	double max = x[0];
	for(int i = 1; i < x.length(); i++) {
		if(x[i] > max) max = x[i];
	}
	return max;
}

double min(NumericVector x) {
	if(x.length() == 0) throw std::invalid_argument("Vector is Empty.");
	double min = x[0];
	for(int i = 1; i < x.length(); i++) {
		if(x[i] < min) min = x[i];
	}
	return min;
}

NumericVector range(int from, int to) {
	NumericVector res;
	if(from <= to) {
		for(int i = from; i <= to; i++) {
			res.push_back(i);
		}
	} else {
		for(int i = from; i >= to; i--) {
			res.push_back(i);
		}
	}
	return res;
}

NumericVector rangeVector(NumericVector x, int from, int to) {
	NumericVector res;
	if(from <= to) {
		for(int i = from; i <= to; i++) {
			res.push_back(x[i]);
		}
	} else {
		for(int i = from; i >= to; i--) {
			res.push_back(x[i]);
		}
	}
	return res;
}

NumericVector append(NumericVector x, NumericVector y) {
	NumericVector res = clone(x);
		for(NumericVector::iterator i = y.begin(); i != y.end(); i++) {
			res.push_back(*i);
		}
	return res;
}


void logDebug(std::string s) {
	Rcout << s << std::endl;
}
