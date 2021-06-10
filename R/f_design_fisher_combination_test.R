#:#
#:#  *Fisher combination test*
#:# 
#:#  This file is part of the R package rpact: 
#:#  Confirmatory Adaptive Clinical Trial Design and Analysis
#:# 
#:#  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
#:#  Licensed under "GNU Lesser General Public License" version 3
#:#  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
#:# 
#:#  RPACT company website: https://www.rpact.com
#:#  rpact package website: https://www.rpact.org
#:# 
#:#  Contact us for information about our services: info@rpact.com
#:# 
#:#  File version: $Revision: 4878 $
#:#  Last changed: $Date: 2021-05-17 17:39:11 +0200 (Mon, 17 May 2021) $
#:#  Last changed by: $Author: pahlke $
#:# 

#' @include f_core_constants.R
#' @include f_core_utilities.R
NULL

.isEqual <- function(x, y) {
	return(abs(x - y) < 1e-10)
}

.getFisherCombinationCaseKmax2 <- function(tVec) {
	return(ifelse(.isEqual(tVec[1], 1), 1L, 2L))
}

.getFisherCombinationSizeKmax2 <- function(alpha0Vec, criticalValues, tVec, piValue, 
		case = .getFisherCombinationCaseKmax2(tVec)) {
	a1 <- alpha0Vec[1]
	c1 <- criticalValues[1]
	c2 <- criticalValues[2]
	t2 <- tVec[1]
	
	.assertIsValidForLogarithmization(list(a1 = a1, c1 = c1))
	
	if (case == 1) {
		return(piValue + c2 * (log(a1) - log(c1)))
	} else {	
		return(piValue + c2^(1/t2) * t2/(t2 - 1) * (a1^(1 - 1/t2) - c1^(1 - 1/t2)))
	}
}

.getFisherCombinationCaseKmax3 <- function(tVec) {
	t2 <- tVec[1]
	t3 <- tVec[2]
	
	if (.isEqual(t2, 1) && .isEqual(t3, 1)) {
		return(1L)
	}
	else if (!.isEqual(t2, t3) && !.isEqual(t2, 1) && !.isEqual(t3, 1)) {
		return(2L)
	}
	else if (.isEqual(t2, t3) && !.isEqual(t2, 1)) {
		return(3L)
	}	
	else if (.isEqual(t2, 1) && !.isEqual(t3, 1)) {
		return(4L)
	}
	else if (!.isEqual(t2, 1) && .isEqual(t3, 1)) {
		return(5L)
	}
}

.getFisherCombinationSizeKmax3 <- function(alpha0Vec, criticalValues, tVec, piValue,
		case = .getFisherCombinationCaseKmax3(tVec)) {
	
	a1 <- alpha0Vec[1]
	a2 <- alpha0Vec[2]
	c1 <- criticalValues[1]
	c2 <- criticalValues[2]
	c3 <- criticalValues[3]
	t2 <- tVec[1]
	t3 <- tVec[2]
	
	.assertIsValidForLogarithmization(list(a1=a1, a2=a2, c1=c1, c2=c2))
	
	if (case == 1) {
		## Wassmer 1999, recursive formula
		return(piValue + c3*(log(a2)*log(a1) - log(a2)*log(c1) + 
					0.5*(log(a1/c2))^2 - 0.5*(log(c1/c2))^2))
	}
	else if (case == 2) {
		return(piValue + c3^(1/t3)*t3/(t3 - t2)*(
					a2^(1 - t2/t3)*t3/(t3 - 1)*(a1^(1 - 1/t3) - c1^(1 - 1/t3)) - 
					c2^(1/t2 - 1/t3)*t2/(t2 - 1)*(a1^(1 - 1/t2) - c1^(1 - 1/t2))))
	}
	else if (case == 3) {
		return(piValue + c3^(1/t3)*t3/(t3 - 1)*(
					a1^(1 - 1/t3)*(log(a2) - 1/t2*(log(c2) - log(a1) + t3/(t3 - 1))) - 
					c1^(1 - 1/t3)*(log(a2) - 1/t2*(log(c2) - log(c1) + t3/(t3 - 1)))))
	}	
	else if (case == 4) {
		return(piValue + c3^(1/t3)*t3/(t3 - 1)*
				(a2^(1 - 1/t3)*t3/(t3 - 1)*(a1^(1 - 1/t3) - c1^(1 - 1/t3)) - 
					c2^(1 - 1/t3)*(log(a1) - log(c1))))
	}
	else if (case == 5) {
		return(piValue + c3 /(1 - t2)*(a2^(1 - t2)*(log(a1) - log(c1)) - 
					c2^(1/t2 - 1)*t2/(t2 - 1)*(a1^(1 - 1/t2) - c1^(1 - 1/t2))))
	}
}

.getFisherCombinationCaseKmax4 <- function(tVec) {
	t2 <- tVec[1]
	t3 <- tVec[2]
	t4 <- tVec[3]
	return(ifelse(
		.isEqual(t2, 1) && .isEqual(t3, 1) && .isEqual(t4, 1),
		1L, 2L
	))
}

.getFisherCombinationSizeApproximatelyKmax4 <- function(alpha0Vec, criticalValues, tVec, piValue,
		case = .getFisherCombinationCaseKmax4(tVec)) {
	a1 <- alpha0Vec[1]
	a2 <- alpha0Vec[2]
	a3 <- alpha0Vec[3]
	c1 <- criticalValues[1]
	c2 <- criticalValues[2]
	c3 <- criticalValues[3]
	c4 <- criticalValues[4]
	t2 <- tVec[1]
	t3 <- tVec[2]
	t4 <- tVec[3]
	
	.assertIsValidForLogarithmization(list(a1=a1, a2=a2, a3=a3, c1=c1, c2=c2, c3=c3))
	
	## Wassmer 1999, recursive formula
	if (case == 1) {	
		return(piValue + c4*(1/6*log(a1*a2/c3)^3 - 1/6*log(c1*a2/c3)^3 + 
					0.5*log(c2/c3)^2*log(c1) - 0.5*log(c2/c3)^2*log(a1) + 
					0.5*log(a1/c2)^2*log(a3) - 0.5*log(c1/c2)^2*log(a3) + 
					log(a3)*log(a2)*log(a1) - log(c1)*log(a2)*log(a3)))
	} 
	
	##  general case for K = 4
	else {
		eps <- 1e-05		
		if (.isEqual(t2, 1)) t2 <- t2 + eps
		if (.isEqual(t3, 1)) t3 <- t3 + eps
		if (.isEqual(t4, 1)) t4 <- t4 + eps
		if (.isEqual(t2, t3)) t3 <- t2 + eps
		if (.isEqual(t2, t4)) t4 <- t2 + eps
		if (.isEqual(t3, t4)) t4 <- t3 + eps
		
		return(piValue + c4^(1/t4)*t4/(t4 - t3)*(
			t4/(t4 - t2)*t4/(t4 - 1)*a3^(1 - t3/t4)*a2^(1 - t2/t4)*(a1^(1 - 1/t4) - c1^(1 - 1/t4)) -
			t4/(t4 - t2)*t2/(t2 - 1)*a3^(1 - t3/t4)*c2^(1/t2 - 1/t4)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) - 
			t3/(t3 - t2)*t3/(t3 - 1)*c3^(1/t3 - 1/t4)*a2^(1 - t2/t3)*(a1^(1 - 1/t3) - c1^(1 - 1/t3)) +
			t3/(t3 - t2)*t2/(t2 - 1)*c3^(1/t3 - 1/t4)*c2^(1/t2 - 1/t3)*(a1^(1 - 1 /t2) - c1^(1 - 1/t2))))
	}
}

.getFisherCombinationCaseKmax5 <- function(tVec) {
	t2 <- tVec[1]
	t3 <- tVec[2]
	t4 <- tVec[3]
	t5 <- tVec[4]
	return(ifelse(
		.isEqual(t2, 1) && .isEqual(t3, 1) && .isEqual(t4, 1) && .isEqual(t5, 1),
		1L, 2L
	))
}

.getFisherCombinationSizeApproximatelyKmax5 <- function(alpha0Vec, criticalValues, tVec, piValue,
		case = .getFisherCombinationCaseKmax5(tVec)) {
	a1 <- alpha0Vec[1]
	a2 <- alpha0Vec[2]
	a3 <- alpha0Vec[3]
	a4 <- alpha0Vec[4]
	c1 <- criticalValues[1]
	c2 <- criticalValues[2]
	c3 <- criticalValues[3]
	c4 <- criticalValues[4]
	c5 <- criticalValues[5]
	t2 <- tVec[1]
	t3 <- tVec[2]
	t4 <- tVec[3]
	t5 <- tVec[4]	
	
	.assertIsValidForLogarithmization(list(a1=a1, a2=a2, a3=a3, a4=a4, c1=c1, c2=c2, c3=c3, c4=c4))
	
	## Wassmer 1999, recursive formula
	if (case == 1) {
		return(piValue + c5 *(1/24*log(a1*a2*a3/c4)^4 - 1/24*log(c1*a2*a3/c4)^4 +
			1/6*log(c2*a3/c4)^3*log(c1) - 1/6*log(c2*a3/c4)^3*log(a1) + 
			1/4*log(c3/c4)^2*log(c1/c2)^2 -	1/4*log(c3/c4)^2*log(a1/c2)^2 + 
			0.5*log(c3/c4)^2*log(a2)*log(c1) - 0.5*log(c3/c4)^2*log(a2)*log(a1) + 
			1/6*log(a1*a2/c3)^3*log(a4) - 1/6*log(c1*a2/c3)^3*log(a4) + 
			0.5*log(c2/c3)^2*log(a4)*log(c1) - 0.5*log(c2/c3)^2*log(a4)*log(a1) + 
			0.5*log(a1/c2)^2*log(a3)*log(a4) - 0.5*log(c1/c2)^2*log(a3)*log(a4) + 
			log(a4)*log(a3)*log(a2)*log(a1) - log(c1)*log(a2)*log(a3)*log(a4)))
	}
	
	##  general case for K = 5
	else {
		eps <- 1e-05		
		if (.isEqual(t2, 1)) t2 <- t2 + eps
		if (.isEqual(t3, 1)) t3 <- t3 + eps
		if (.isEqual(t4, 1)) t4 <- t4 + eps
		if (.isEqual(t5, 1)) t5 <- t5 + eps
		if (.isEqual(t2, t3)) t3 <- t2 + eps
		if (.isEqual(t2, t4)) t4 <- t2 + eps
		if (.isEqual(t2, t5)) t5 <- t2 + eps		
		if (.isEqual(t3, t4)) t4 <- t3 + eps
		if (.isEqual(t3, t5)) t5 <- t3 + eps
		if (.isEqual(t4, t5)) t5 <- t4 + eps
		
		return(piValue + c5^(1/t5)*t5/(t5 - t4)*(
			t5/(t5 - t3)*t5/(t5 - t2)*t5/(t5 - 1)*a4^(1 - t4/t5)*a3^(1 - t3/t5)*a2^(1 - t2/t5)*(a1^(1 - 1/t5) - c1^(1 - 1/t5)) -			
			t5/(t5 - t3)*t5/(t5 - t2)*t2/(t2 - 1)*a4^(1 - t4/t5)*a3^(1 - t3/t5)*c2^(1/t2 - 1/t5)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) -
			t5/(t5 - t3)*t3/(t3 - t2)*t3/(t3 - 1)*a4^(1 - t4/t5)*c3^(1/t3 - 1/t5)*a2^(1 - t2/t3)*(a1^(1 - 1/t3) - c1^(1 - 1/t3)) +
			t5/(t5 - t3)*t3/(t3 - t2)*t2/(t2 - 1)*a4^(1 - t4/t5)*c3^(1/t3 - 1/t5)*c2^(1/t2 - 1/t3)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) -
			t4/(t4 - t3)*t4/(t4 - t2)*t4/(t4 - 1)*c4^(1/t4 - 1/t5)*a3^(1 - t3/t4)*a2^(1 - t2/t4)*(a1^(1 - 1/t4) - c1^(1 - 1/t4)) +
			t4/(t4 - t3)*t4/(t4 - t2)*t2/(t2 - 1)*c4^(1/t4 - 1/t5)*a3^(1 - t3/t4)*c2^(1/t2 - 1/t4)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) +
			t4/(t4 - t3)*t3/(t3 - t2)*t3/(t3 - 1)*c4^(1/t4 - 1/t5)*c3^(1/t3 - 1/t4)*a2^(1 - t2/t3)*(a1^(1 - 1/t3) - c1^(1 - 1/t3)) -
			t4/(t4 - t3)*t3/(t3 - t2)*t2/(t2 - 1)*c4^(1/t4 - 1/t5)*c3^(1/t3 - 1/t4)*c2^(1/t2 - 1/t3)*(a1^(1 - 1/t2) - c1^(1 - 1/t2))))
	}
}

.getFisherCombinationCaseKmax6 <- function(tVec) {
	t2 <- tVec[1]
	t3 <- tVec[2]
	t4 <- tVec[3]
	t5 <- tVec[4]
	t6 <- tVec[5]
	return(ifelse(
		.isEqual(t2, 1) && .isEqual(t3, 1) && .isEqual(t4, 1) && .isEqual(t5, 1) && .isEqual(t6, 1),
		1L, 2L
	))
}

.getFisherCombinationSizeApproximatelyKmax6 <- function(alpha0Vec, criticalValues, tVec, piValue,
		case = .getFisherCombinationCaseKmax6(tVec)) {
	a1 <- alpha0Vec[1]
	a2 <- alpha0Vec[2]
	a3 <- alpha0Vec[3]
	a4 <- alpha0Vec[4]
	a5 <- alpha0Vec[5]
	c1 <- criticalValues[1]
	c2 <- criticalValues[2]
	c3 <- criticalValues[3]
	c4 <- criticalValues[4]
	c5 <- criticalValues[5]
	c6 <- criticalValues[6]
	t2 <- tVec[1]
	t3 <- tVec[2]
	t4 <- tVec[3]
	t5 <- tVec[4]
	t6 <- tVec[5]	
	
	.assertIsValidForLogarithmization(list(a1=a1, a2=a2, a3=a3, a4=a4, a5=a5, c1=c1, c2=c2, c3=c3, c4=c4, c5=c5))
	
	## Wassmer 1999, recursive formula
	if (case == 1) {
		return(piValue + c6*(
			log(a1)*log(a2)*log(a3)*log(a4)*log(a5) + 1/24*log(a1*a2*a3/c4)^4*log(a5) + 1/120*log(a1*a2*a3*a4/c5)^5 - 0.5*log(c4/c5)^2*log(a3)*log(a2)*log(a1) + 
			1/6*log(a1*a2/c3)^3*log(a4)*log(a5) - 0.5*log(c3/c4)^2*log(a5)*log(a2)*log(a1) - 1/6*log(c3*a4/c5)^3*log(a2)*log(a1) - 1/12*log(a1*a2/c3)^3*log(c4/c5)^2 + 
			0.5*log(a1/c2)^2*log(a3)*log(a4)*log(a5) - 1/6*log(c2*a3/c4)^3*log(a5)*log(a1) - 1/24*log(c2*a3*a4/c5)^4*log(a1) - 1/4*log(c4/c5)^2*log(a3)*log(a1/c2)^2 - 
			0.5*log(c2/c3)^2*log(a4)*log(a5)*log(a1) - 1/4*log(c3/c4)^2*log(a5)*log(a1/c2)^2 - 1/12*log(c3*a4/c5)^3*log(a1/c2)^2 + 1/4*log(c2/c3)^2*log(c4 /c5)^2*log(a1) - 
			log(c1)*log(a2)*log(a3)*log(a4)*log(a5) - 1/24*log(c1*a2*a3/c4)^4*log(a5) - 1/120*log(c1*a2*a3*a4/c5)^5 + 0.5*log(c4/c5)^2*log(a3)*log(a2) * log(c1) - 
			1/6*log(c1*a2/c3)^3*log(a4)*log(a5) + 0.5*log(c3/c4)^2*log(a5)*log(a2)*log(c1) + 1/6*log(c3*a4/c5)^3*log(a2)*log(c1) + 1/12*log(c1*a2/c3)^3*log(c4/c5)^2 - 
			0.5*log(c1/c2)^2*log(a3)*log(a4)*log(a5) + 1/6*log(c2*a3/c4)^3*log(a5)*log(c1) + 1/24*log(c2*a3*a4/c5)^4*log(c1) + 1/4*log(c4/c5)^2*log(a3)*log(c1/c2)^2 + 
			0.5*log(c2/c3)^2*log(a4)*log(a5)*log(c1) + 1/4*log(c3/c4)^2*log(a5)*log(c1/c2)^2 + 1/12*log(c3*a4/c5)^3*log(c1/c2)^2 - 1/4*log(c2/c3)^2*log(c4/c5)^2*log(c1)))
	}
	
	##  general case for K = 6
	else {
		eps <- 1e-04
		if (.isEqual(t2, 1)) t2 <- t2 + eps
		if (.isEqual(t3, 1)) t3 <- t3 + eps
		if (.isEqual(t4, 1)) t4 <- t4 + eps
		if (.isEqual(t5, 1)) t5 <- t5 + eps
		if (.isEqual(t6, 1)) t6 <- t6 + eps
		if (.isEqual(t2, t3)) t3 <- t2 + eps
		if (.isEqual(t2, t4)) t4 <- t2 + eps
		if (.isEqual(t2, t5)) t5 <- t2 + eps
		if (.isEqual(t2, t6)) t6 <- t2 + eps
		if (.isEqual(t3, t4)) t4 <- t3 + eps
		if (.isEqual(t3, t5)) t5 <- t3 + eps
		if (.isEqual(t3, t6)) t6 <- t3 + eps
		if (.isEqual(t4, t5)) t5 <- t4 + eps
		if (.isEqual(t4, t6)) t6 <- t4 + eps
		if (.isEqual(t5, t6)) t6 <- t5 + eps
		
		return(piValue + c6^(1/t6)*t6/(t6 - t5)*(
			t6/(t6 - t4)*t6/(t6 - t3)*t6/(t6 - t2)*t6/(t6 - 1)*a5^(1 - t5/t6)*a4^(1 - t4/t6)* a3^(1 - t3/t6)* a2^(1 - t2/t6)* (a1^(1 - 1/t6) - c1^(1 - 1/t6)) -			
			t6/(t6 - t4)*t6/(t6 - t3)*t6/(t6 - t2)*t2/(t2 - 1)*a5^(1 - t5/t6)*a4^(1 - t4/t6)* a3^(1 - t3/t6)* c2^(1/t2 - 1/t6)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) -
			t6/(t6 - t4)*t6/(t6 - t3)*t3/(t3 - t2)*t3/(t3 - 1)*a5^(1 - t5/t6)*a4^(1 - t4/t6)* c3^(1/t3 - 1/t6)*a2^(1 - t2/t3)* (a1^(1 - 1/t3) - c1^(1 - 1/t3)) +
			t6/(t6 - t4)*t6/(t6 - t3)*t3/(t3 - t2)*t2/(t2 - 1)*a5^(1 - t5/t6)*a4^(1 - t4/t6)* c3^(1/t3 - 1/t6)*c2^(1/t2 - 1/t3)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) -
			t6/(t6 - t4)*t4/(t4 - t3)*t4/(t4 - t2)*t4/(t4 - 1)*a5^(1 - t5/t6)*c4^(1/t4 - 1/t6)*a3^(1 - t3/t4)* a2^(1 - t2/t4)* (a1^(1 - 1/t4) - c1^(1 - 1/t4)) +
			t6/(t6 - t4)*t4/(t4 - t3)*t4/(t4 - t2)*t2/(t2 - 1)*a5^(1 - t5/t6)*c4^(1/t4 - 1/t6)*a3^(1 - t3/t4)* c2^(1/t2 - 1/t4)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) +
			t6/(t6 - t4)*t4/(t4 - t3)*t3/(t3 - t2)*t3/(t3 - 1)*a5^(1 - t5/t6)*c4^(1/t4 - 1/t6)*c3^(1/t3 - 1/t4)*a2^(1 - t2/t3)* (a1^(1 - 1/t3) - c1^(1 - 1/t3)) -
			t6/(t6 - t4)*t4/(t4 - t3)*t3/(t3 - t2)*t2/(t2 - 1)*a5^(1 - t5/t6)*c4^(1/t4 - 1/t6)*c3^(1/t3 - 1/t4)*c2^(1/t2 - 1/t3)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) -
			t5/(t5 - t4)*t5/(t5 - t3)*t5/(t5 - t2)*t5/(t5 - 1)*c5^(1/t5 - 1/t6)*a4^(1 - t4/t5)* a3^(1 - t3/t5)* a2^(1 - t2/t5)* (a1^(1 - 1/t5) - c1^(1 - 1/t5)) +			
			t5/(t5 - t4)*t5/(t5 - t3)*t5/(t5 - t2)*t2/(t2 - 1)*c5^(1/t5 - 1/t6)*a4^(1 - t4/t5)* a3^(1 - t3/t5)* c2^(1/t2 - 1/t5)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) +
			t5/(t5 - t4)*t5/(t5 - t3)*t3/(t3 - t2)*t3/(t3 - 1)*c5^(1/t5 - 1/t6)*a4^(1 - t4/t5)* c3^(1/t3 - 1/t5)*a2^(1 - t2/t3)* (a1^(1 - 1/t3) - c1^(1 - 1/t3)) -
			t5/(t5 - t4)*t5/(t5 - t3)*t3/(t3 - t2)*t2/(t2 - 1)*c5^(1/t5 - 1/t6)*a4^(1 - t4/t5)* c3^(1/t3 - 1/t5)*c2^(1/t2 - 1/t3)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) +
			t5/(t5 - t4)*t4/(t4 - t3)*t4/(t4 - t2)*t4/(t4 - 1)*c5^(1/t5 - 1/t6)*c4^(1/t4 - 1/t5)*a3^(1 - t3/t4)* a2^(1 - t2/t4)* (a1^(1 - 1/t4) - c1^(1 - 1/t4)) -
			t5/(t5 - t4)*t4/(t4 - t3)*t4/(t4 - t2)*t2/(t2 - 1)*c5^(1/t5 - 1/t6)*c4^(1/t4 - 1/t5)*a3^(1 - t3/t4)* c2^(1/t2 - 1/t4)*(a1^(1 - 1/t2) - c1^(1 - 1/t2)) -
			t5/(t5 - t4)*t4/(t4 - t3)*t3/(t3 - t2)*t3/(t3 - 1)*c5^(1/t5 - 1/t6)*c4^(1/t4 - 1/t5)*c3^(1/t3 - 1/t4)*a2^(1 - t2/t3)* (a1^(1 - 1/t3) - c1^(1 - 1/t3)) +
			t5/(t5 - t4)*t4/(t4 - t3)*t3/(t3 - t2)*t2/(t2 - 1)*c5^(1/t5 - 1/t6)*c4^(1/t4 - 1/t5)*c3^(1/t3 - 1/t4)*c2^(1/t2 - 1/t3)*(a1^(1 - 1/t2) - c1^(1 - 1/t2))))
	}
}

.getFisherCombinationSize <- function(kMax, alpha0Vec, criticalValues, tVec, 
		cases = .getFisherCombinationCases(kMax = kMax, tVec = tVec)) {
	if (length(criticalValues) < 1 || length(criticalValues) > C_KMAX_UPPER_BOUND_FISHER) {
		stop(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
			"length of 'criticalValues' (", length(criticalValues), 
			") is out of bounds [1; ", C_KMAX_UPPER_BOUND_FISHER, "]")
	}
	
	piValue <- criticalValues[1]
	if (kMax > 1) {
		piValue <- .getFisherCombinationSizeKmax2(alpha0Vec, criticalValues, 
			tVec, piValue, case = cases[1])
	}
	if (kMax > 2) {
		piValue <- .getFisherCombinationSizeKmax3(alpha0Vec, criticalValues, 
			tVec, piValue, case = cases[2])
	}
	if (kMax > 3) {
		piValue <- .getFisherCombinationSizeApproximatelyKmax4(alpha0Vec, criticalValues, 
			tVec, piValue, case = cases[3])
	}
	if (kMax > 4) {
		piValue <- .getFisherCombinationSizeApproximatelyKmax5(alpha0Vec, criticalValues, 
			tVec, piValue, case = cases[4])
	}
	if (kMax > 5) {
		piValue <- .getFisherCombinationSizeApproximatelyKmax6(alpha0Vec, criticalValues, 
			tVec, piValue, case = cases[5])
	}
	return(piValue)
}

.getRejectValueForOneTrial <- function(kMax, alpha0, criticalValues, weightsFisher, stage, pValues) {
	
	if (stage < kMax && pValues[stage] >= alpha0[stage]) {
		return(0)
	}
	
	p <- prod(pValues[1:stage] ^ weightsFisher[1:stage])
	if (p < criticalValues[stage]) {
		return(1)
	}
	
	return(-1)
}

.getRejectValueRejectionProbability <- function(settings) {
	
	pValues <- stats::runif(settings$kMax)
	
	for (stage in 1:settings$kMax) {
		rejectValue <- .getRejectValueForOneTrial(settings$kMax, settings$alpha0, 
			settings$criticalValues, settings$weightsFisher, stage, pValues)
		if (rejectValue >= 0) {
			return(rejectValue)
		}
	}
	
	return(0)
}

.getSimulatedAlpha <- function(
	kMax, 
	alpha,  
	alpha0, 
	criticalValues,
	tVec,
	iterations,
	seed) {
	
	weightsFisher <- c(1, tVec) 
	
	settings <- list(
		kMax = kMax, 
		alpha = alpha,
		alpha0 = alpha0, 
		criticalValues = criticalValues, 
		weightsFisher = weightsFisher,
		iterations = iterations,
		seed = seed
	)
	
	cases <- rep(list(settings), iterations)
	# 'mclapply' requires package 'parallel'
	# Improvement: implement as cluster based routine
	if (requireNamespace("parallel", quietly = TRUE)) {
		simResults <- parallel::mclapply(cases, .getRejectValueRejectionProbability, mc.preschedule = TRUE) 
	} else {
		simResults <- base::lapply(cases, .getRejectValueRejectionProbability)
	}
	
	settings$alphaSimulated <- do.call(sum, simResults) / iterations
	
	return(settings)
}

.setKMaxToDesign <- function(design, kMax) {
	if (.isUndefinedArgument(design$kMax)) {
		design$kMax <- as.integer(kMax)
		design$.setParameterType("kMax", C_PARAM_GENERATED)
	} else {
		design$.setParameterType("kMax", ifelse(design$kMax == C_KMAX_DEFAULT, 
				C_PARAM_DEFAULT_VALUE, C_PARAM_USER_DEFINED))
	}
}

#' @title
#' Get Design Fisher
#' 
#' @description
#' Performs Fisher's combination test and returns critical values for this design.
#'
#' @inheritParams param_kMax
#' @inheritParams param_alpha
#' @param method \code{"equalAlpha"}, \code{"fullAlpha"}, \code{"noInteraction"}, or \code{"userDefinedAlpha"}, 
#' default is \code{"equalAlpha"} (for details, see Wassmer, 1999).
#' @inheritParams param_userAlphaSpending
#' @param alpha0Vec Stopping for futility bounds for stage-wise p-values.
#' @inheritParams param_informationRates
#' @inheritParams param_sided
#' @param bindingFutility If \code{bindingFutility = TRUE} is specified the calculation of 
#'        the critical values is affected by the futility bounds (default is \code{TRUE}).
#' @param tolerance The numerical tolerance, default is \code{1e-14}.
#' @param iterations The number of simulation iterations, e.g.,
#'        \code{getDesignFisher(iterations = 100000)} checks the validity of the critical values for the default design. 
#'        The default value of \code{iterations} is 0, i.e., no simulation will be executed.
#' @param seed Seed for simulating the power for Fisher's combination test. See above, default is a random seed.
#' @inheritParams param_three_dots
#' 
#' @details 
#' \code{getDesignFisher} calculates the critical values and stage levels for 
#' Fisher's combination test as described in Bauer (1989), Bauer and Koehne (1994), 
#' Bauer and Roehmel (1995), and Wassmer (1999) for equally and unequally sized stages.
#' 
#' @seealso \code{\link{getDesignSet}} for creating a set of designs to compare.
#' 
#' @template return_object_trial_design
#' @template how_to_get_help_for_generics
#' 
#' @family design functions
#'
#' @template examples_get_design_Fisher   
#'
#' @export
#' 
getDesignFisher <- function(...,
		kMax = NA_integer_, 
		alpha = NA_real_, 
		method = c("equalAlpha", "fullAlpha", "noInteraction", "userDefinedAlpha"), # C_FISHER_METHOD_DEFAULT
		userAlphaSpending = NA_real_, 
		alpha0Vec = NA_real_, 
		informationRates = NA_real_, 
		sided = 1,                         # C_SIDED_DEFAULT
		bindingFutility = NA, 
		tolerance = 1e-14, 				   # C_ANALYSIS_TOLERANCE_FISHER_DEFAULT
		iterations = 0L, 
		seed = NA_real_) {
	
	.assertIsValidTolerance(tolerance)
	.assertIsValidIterationsAndSeed(iterations, seed)
	.warnInCaseOfUnknownArguments(functionName = "getDesignFisher", ...)
	
	return(.getDesignFisher(
		kMax = kMax, alpha = alpha, method = method, 
		userAlphaSpending = userAlphaSpending, alpha0Vec = alpha0Vec, informationRates = informationRates, 
		sided = sided, bindingFutility = bindingFutility, 
		tolerance = tolerance, iterations = iterations, seed = seed, userFunctionCallEnabled = TRUE)
	)
}

.getDesignFisherDefaultValues <- function() {
	return(list(
		kMax = NA_integer_, 
		alpha = NA_real_, 
		method = C_FISHER_METHOD_DEFAULT, 
		userAlphaSpending = NA_real_, 
		alpha0Vec = NA_real_, 
		informationRates = NA_real_, 
		sided = 1, 
		bindingFutility = C_BINDING_FUTILITY_FISHER_DEFAULT,
		tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT, 
		iterations = 0,
		seed = NA_real_
	))
}

.getFisherCombinationCases <- function(kMax, tVec) {
	if (kMax == 1) {
		return(c())
	}
	
	cases <- c()
	if (kMax > 1) {
		cases <- c(cases, .getFisherCombinationCaseKmax2(tVec))
	}
	if (kMax > 2) {
		cases <- c(cases, .getFisherCombinationCaseKmax3(tVec))
	}
	if (kMax > 3) {
		cases <- c(cases, .getFisherCombinationCaseKmax4(tVec))
	}
	if (kMax > 4) {
		cases <- c(cases, .getFisherCombinationCaseKmax5(tVec))
	}
	if (kMax > 5) {
		cases <- c(cases, .getFisherCombinationCaseKmax6(tVec))
	}
	return(cases)
}

# 
# @param userFunctionCallEnabled if \code{TRUE}, additional parameter validation methods will be called.
# 
.getDesignFisher <- function(
		kMax = NA_integer_, alpha = NA_real_, method = C_FISHER_METHOD_DEFAULT, 
		userAlphaSpending = NA_real_, alpha0Vec = NA_real_, informationRates = NA_real_, 
		sided = 1, bindingFutility = C_BINDING_FUTILITY_FISHER_DEFAULT, 
		tolerance = C_ANALYSIS_TOLERANCE_FISHER_DEFAULT, iterations = 0L, seed = NA_real_, 
		userFunctionCallEnabled = FALSE) {
		
	method <- .matchArgument(method, C_FISHER_METHOD_DEFAULT)
	
	.assertIsNumericVector(alpha0Vec, "alpha0Vec", naAllowed = TRUE)
	
	if (.isDefinedArgument(kMax, argumentExistsValidationEnabled = userFunctionCallEnabled)) {
		.assertIsValidKMax(kMax, kMaxUpperBound = C_KMAX_UPPER_BOUND_FISHER)
		
		if (!is.integer(kMax)) {
			kMax <- as.integer(kMax)
		}
	}
	
	if (!is.integer(sided) && sided %in% c(1, 2)) {
		sided <- as.integer(sided)
	}
	
	if (sided != 1) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "Fisher's combination test only available for one-sided testing")
	}
	
	if (is.na(bindingFutility)) {
		bindingFutility <- C_BINDING_FUTILITY_FISHER_DEFAULT
	} else if (userFunctionCallEnabled && 
			((!is.na(kMax) && kMax == 1) || 
			(!any(is.na(alpha0Vec)) && all(alpha0Vec == C_ALPHA_0_VEC_DEFAULT)))) {
		warning("'bindingFutility' (", bindingFutility, ") will be ignored", call. = FALSE)
	}
	
	design <- TrialDesignFisher(
		kMax = kMax, 
		alpha = alpha, 
		method = method, 
		sided = sided,
		userAlphaSpending = userAlphaSpending,
		alpha0Vec = alpha0Vec,  
		informationRates = informationRates, 
		bindingFutility = bindingFutility,
		tolerance = tolerance,
		iterations = as.integer(iterations),
		seed = seed
	)
	
	.assertDesignParameterExists(design, "sided", C_SIDED_DEFAULT)
	.assertIsValidSidedParameter(design$sided)
	
	.assertDesignParameterExists(design, "method", C_FISHER_METHOD_DEFAULT)
	.assertIsSingleCharacter(design$method, "method")
	if (!.isFisherMethod(design$method)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"'method' must be one of the following: ", .printFisherMethods())
	} 
	
	.assertDesignParameterExists(design, "bindingFutility", C_BINDING_FUTILITY_FISHER_DEFAULT)
	
	.assertDesignParameterExists(design, "tolerance", C_ANALYSIS_TOLERANCE_FISHER_DEFAULT)
	
	.setKmaxBasedOnAlphaSpendingDefintion(design)
	
	design$informationRates <- .getValidatedInformationRates(design)	
	design$alpha0Vec <- .getValidatedAlpha0Vec(design)
	
	if (design$sided == 2 && design$bindingFutility && any(design$alpha0Vec < 1)) {
		warning("Binding futility will be ignored because the test is defined as two-sided", call. = FALSE)
	}
	
	if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
		.validateUserAlphaSpending(design)		
	} else {
		design$.setParameterType("userAlphaSpending", C_PARAM_NOT_APPLICABLE)
		if (.isDefinedArgument(design$userAlphaSpending)) {
			warning("'userAlphaSpending' will be ignored because 'method' is not '", 
				C_FISHER_METHOD_USER_DEFINED_ALPHA, "'", call. = FALSE)
		}
	}
	
	if (.isUndefinedArgument(design$alpha)) {
		design$alpha = C_ALPHA_DEFAULT
	}
	
	.assertDesignParameterExists(design, "alpha", C_ALPHA_DEFAULT)
	.assertIsSingleNumber(design$alpha, "alpha")	
	.assertIsValidSidedParameter(sided)
	if (sided != 1) {
		design$alpha <- design$alpha / sided
	}
	if (userFunctionCallEnabled) {
		.assertIsValidAlpha(design$alpha)
	}
	
	.assertDesignParameterExists(design, "kMax", 3)	
	.assertIsSingleInteger(design$kMax, "kMax")
	.assertIsValidKMax(design$kMax, kMaxUpperBound = C_KMAX_UPPER_BOUND_FISHER)
	if (design$method == C_FISHER_METHOD_NO_INTERACTION && design$kMax < 3) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT,
			"method '", C_FISHER_METHOD_NO_INTERACTION, 
			"' is only allowed for kMax > 2 (kMax is ", design$kMax, ")")
	}
	
	if (design$kMax > 1) {
		design$scale <- round(sqrt((design$informationRates[2:design$kMax] - 
						design$informationRates[1:(design$kMax - 1)]) / design$informationRates[1]), 10)
	}
	design$criticalValues <- rep(NA_real_, design$kMax)
	
	design$.setParameterType("scale", C_PARAM_GENERATED)
	design$.setParameterType("criticalValues", C_PARAM_GENERATED)
	
	if (design$bindingFutility) {
		alpha0Vec <- design$alpha0Vec
	} else {
		alpha0Vec <- rep(1, design$kMax - 1)
	}
	
	if (design$method == C_FISHER_METHOD_NO_INTERACTION && !any(is.na(alpha0Vec)) && 
		all(alpha0Vec == C_ALPHA_0_VEC_DEFAULT)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
			"for specified 'method' (\"", C_FISHER_METHOD_NO_INTERACTION, 
			"\") the 'alpha0Vec' must be unequal to ", .arrayToString(alpha0Vec, vectorLookAndFeelEnabled = TRUE))
	}
	
	design$.setParameterType("stageLevels", C_PARAM_GENERATED)
	design$.setParameterType("alphaSpent", C_PARAM_GENERATED)
	design$.setParameterType("nonStochasticCurtailment", C_PARAM_GENERATED)
	
	tryCatch({
		cases <- .getFisherCombinationCases(kMax = design$kMax, tVec = design$scale)
		if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
			design$criticalValues[1] <- design$userAlphaSpending[1]
			design$alphaSpent <- design$criticalValues
			if (design$kMax > 1) {
				
				for (k in 2:design$kMax) {
					cLower <- 0
					cUpper <- design$alpha
					prec <- 1
					
					while (prec > design$tolerance) {
						alpha1 <- (cLower + cUpper) * 0.5
						design$criticalValues[k] <- alpha1
						size <- .getFisherCombinationSize(k, alpha0Vec[1:(k - 1)], 
							design$criticalValues, design$scale, cases = cases)
						ifelse(size < design$userAlphaSpending[k], cLower <- alpha1, cUpper <- alpha1)
						prec <- cUpper - cLower
					}
				}
			}
		} else {
			prec <- 1
			cLower <- 0
			cUpper <- design$alpha
			maxIter <- 100
			while (prec > design$tolerance && maxIter >= 0) {
				# no use of uniroot because there might be no positive solution 
				# (f(cl) and f(cu) might not have opposite signs)
				alpha1 <- (cLower + cUpper) * 0.5
				if (design$method == C_FISHER_METHOD_EQUAL_ALPHA) {
					design$criticalValues <- sapply(1:design$kMax, 
						function(k) .getOneDimensionalRoot(function(c) {
							.getFisherCombinationSize(k, rep(1, k - 1), rep(c, k), 
								design$scale, cases = cases) - alpha1
						}, lower = design$tolerance, upper = design$alpha, tolerance = design$tolerance,
						callingFunctionInformation = ".getDesignFisher")
					) 
				}
				else if (design$method == C_FISHER_METHOD_FULL_ALPHA) {			 
					design$criticalValues[1:(design$kMax - 1)] <- sapply(1:(design$kMax - 1), function(k) {  
						prec2 <- 1
						cLower2 <- 0	
						cUpper2 <- design$alpha
						while (prec2 > design$tolerance) {
							c <- (cLower2 + cUpper2) * 0.5
							y <- .getFisherCombinationSize(k, rep(1, k - 1), rep(c, k), 
								design$scale, cases = cases) 
							ifelse(y < alpha1, cLower2 <- c, cUpper2 <- c)
							prec2 <- cUpper2 - cLower2
						}
						return(c)
					})
					design$criticalValues[design$kMax] <- .getOneDimensionalRoot(
						function(c) {
							.getFisherCombinationSize(design$kMax, rep(1, design$kMax - 1), 
								rep(c, design$kMax), design$scale, cases = cases) - design$alpha
						}, lower = design$tolerance, upper = design$alpha, tolerance = design$tolerance,
						callingFunctionInformation = ".getDesignFisher"
					)
				}
				else if (design$method == C_FISHER_METHOD_NO_INTERACTION) {
					design$criticalValues[design$kMax] <- .getOneDimensionalRoot(
						function(c) {
							.getFisherCombinationSize(design$kMax, rep(1, design$kMax - 1), 
								rep(c, design$kMax), design$scale, cases = cases) - design$alpha
						}, lower = design$tolerance, upper = design$alpha, tolerance = design$tolerance,
						callingFunctionInformation = ".getDesignFisher"
					)
					design$criticalValues[1] <- alpha1
					for (k in (design$kMax - 1):2) {
						design$criticalValues[k] <- design$criticalValues[k + 1] / design$alpha0Vec[k]^(1/design$scale[k])
					}
				}
				size <- .getFisherCombinationSize(design$kMax, alpha0Vec, 
					design$criticalValues, design$scale, cases = cases)
				ifelse(size < design$alpha, cLower <- alpha1, cUpper <- alpha1)
				prec <- cUpper - cLower
				maxIter <- maxIter - 1
			}
		}
		
		design$stageLevels <- sapply(1:design$kMax, function(k) {
			.getFisherCombinationSize(k, rep(1, k - 1), 
				rep(design$criticalValues[k], k), design$scale, cases = cases)
		})
		
		design$alphaSpent <- sapply(1:design$kMax, function(k) {
			.getFisherCombinationSize(k, alpha0Vec[1:(k - 1)], 
				design$criticalValues[1:k], design$scale, cases = cases)
		})
		
		design$nonStochasticCurtailment <- FALSE
		if (design$stageLevels[1] < 1e-10) {
			design$criticalValues[1:(design$kMax - 1)] <- design$criticalValues[design$kMax]
			design$stageLevels <- sapply(1:design$kMax, 
				function(k) {
					.getFisherCombinationSize(k, rep(1, k - 1), 
						rep(design$criticalValues[k], k), design$scale, cases = cases)
				}
			)
			design$alphaSpent <- sapply(1:design$kMax, 
				function(k) {
					.getFisherCombinationSize(k, alpha0Vec[1:(k - 1)], 
						design$criticalValues[1:k], design$scale, cases = cases)
				}
			)		
			design$nonStochasticCurtailment <- TRUE
		}
	}, error = function(e) {
		warning("Output may be wrong because an error occured: ", e$message, call. = FALSE)
	})
	
	if (userFunctionCallEnabled) {	
		if (design$method == C_FISHER_METHOD_NO_INTERACTION && abs(size - design$alpha) > 1e-03) {
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 'numerical overflow in computation routine')
		}
		
		if (design$method == C_FISHER_METHOD_EQUAL_ALPHA && !all(is.na(design$stageLevels)) &&
				abs(mean(na.omit(design$stageLevels)) - design$stageLevels[1]) > 1e-03) {
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 'numerical overflow in computation routine')
		}
		
		if (design$kMax > 1) {
			if (any(na.omit(design$criticalValues[2:design$kMax] - 
					design$criticalValues[1:(design$kMax - 1)]) > 1e-12)) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 'no calculation possible')
			} 
			
			if (!all(is.na(design$stageLevels)) && any(na.omit(design$stageLevels[1:(design$kMax - 1)]) > design$alpha)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'alpha' (", design$alpha, ") not correctly specified")
			} 
		}
		
		if (design$method == C_FISHER_METHOD_USER_DEFINED_ALPHA) {
			if (any(abs(design$alphaSpent - design$userAlphaSpending) > 1e-05)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'alpha' (", design$alpha, ") or 'userAlphaSpending' (", 
					.arrayToString(design$userAlphaSpending), ") not correctly specified")
			}
		}
	}
	
	design$.setParameterType("simAlpha", C_PARAM_NOT_APPLICABLE)
	design$simAlpha <- NA_real_
	if (!is.null(design$iterations) && design$iterations > 0) {
		design$seed <- .setSeed(design$seed)
		simResult <- .getSimulatedAlpha(
			kMax = design$kMax, 
			alpha = design$alpha,  
			alpha0 = design$alpha0Vec, 
			criticalValues = design$criticalValues,
			tVec = design$scale,
			iterations = iterations,
			seed = seed)
		design$simAlpha <- simResult$alphaSimulated
		design$.setParameterType("simAlpha", C_PARAM_GENERATED)
	}
	
	if (design$kMax == 1) {
		design$.setParameterType("alpha0Vec", C_PARAM_NOT_APPLICABLE)
	}
	
	if (length(design$alpha0Vec) == 0 ||
			all(design$alpha0Vec == C_ALPHA_0_VEC_DEFAULT)) {
		# design$bindingFutility <- NA
		design$.setParameterType("bindingFutility", C_PARAM_NOT_APPLICABLE)
	}
	
	design$.initStages()
	
	return(design)
}

