% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_design_sample_size_calculator.R
\name{getPowerRates}
\alias{getPowerRates}
\title{Get Power Rates}
\usage{
getPowerRates(design = NULL, ..., normalApproximation = TRUE,
  riskRatio = FALSE, thetaH0 = ifelse(riskRatio, 1, 0),
  pi1 = C_PI_1_DEFAULT, pi2 = 0.2, directionUpper = NA,
  maxNumberOfSubjects = NA_real_, groups = 2,
  allocationRatioPlanned = NA_real_)
}
\arguments{
\item{design}{The trial design. If no trial design is specified, a fixed sample size design is used. 
In this case, \code{alpha}, \code{beta}, and \code{sided} can be directly entered as argument}

\item{...}{Ensures that all arguments are be named and 
that a warning will be displayed if unknown arguments are passed.}

\item{normalApproximation}{If \code{normalApproximation = FALSE} is specified, the sample size 
for the case of one treatment group is calculated exactly using the binomial distribution, 
default is \code{TRUE}.}

\item{riskRatio}{If \code{riskRatio = TRUE} is specified, the sample size for one-sided 
testing of H0: \code{pi1/pi2 = thetaH0} is calculated, default is \code{FALSE}.}

\item{thetaH0}{The null hypothesis value. For one-sided testing, a value != 0 
(or != 1 for testing the risk ratio \code{pi1/pi2}) can be specified, default is \code{0} or \code{1} for difference and ratio testing, respectively.}

\item{pi1}{The assumed probability in the active treatment group if two treatment groups 
are considered, or the alternative probability for a one treatment group design, 
default is \code{seq(0.2,0.5,0.1)}.}

\item{pi2}{The assumed probability in the reference group if two treatment groups are considered, default is \code{0.2}.}

\item{directionUpper}{Specifies the direction of the alternative, only applicable for one-sided testing, default is \code{TRUE}.}

\item{maxNumberOfSubjects}{\code{maxNumberOfSubjects > 0} needs to be specified.
If accrual time and accrual intensity is specified, this will be calculated.}

\item{groups}{The number of treatment groups (1 or 2), default is \code{2}.}

\item{allocationRatioPlanned}{The planned allocation ratio for a two treatment groups design, default is \code{1}.}
}
\value{
Returns a \code{\link{TrialDesignPlanRates}} object.
}
\description{
Returns the power, stopping probabilities, and expected sample size for testing rates in one or two samples at given sample sizes.
}
\details{
At given design the function calculates the power, stopping probabilities, and expected sample size, for testing rates for given maximum sample size.
The sample sizes over the stages are calculated according to the specified information rate in the design. 
In a two treatment groups design, additionally, an allocation ratio = n1/n2 can be specified. 
If a null hypothesis value thetaH0 != 0 for testing the difference of two rates 
or thetaH0 != 1 for testing the risk ratio is specified, the  
formulas according to Farrington & Manning (Statistics in Medicine, 1990) are used (only one-sided testing).
Critical bounds and stopping for futility bounds are provided at the effect scale (rate, rate difference, or rate ratio, respectively). 
For the two-sample case, the calculation here is performed at fixed pi2 as given as argument in the function.
}
\examples{

# Calculate the power, stopping probabilities, and expected sample size in a two-armed 
# design at given maximum sample size N = 200 
# in a three-stage O'Brien & Fleming design with information rate vector (0.2,0.5,1), 
# non-binding futility boundaries (0,0), i.e.,
# the study stops for futility if the p-value exceeds 0.5 at interm, and 
# allocation ratio = 2 for a range of pi1 values when testing H0: pi1 - pi2 = -0.1:
getPowerRates(getDesignGroupSequential(informationRates = c(0.2,0.5,1), 
    futilityBounds = c(0,0)), groups = 2, thetaH0 = -0.1, 
    pi1 = seq(0.3, 0.6, 0.1), directionUpper = FALSE, 
    pi2 = 0.7, allocationRatioPlanned = 2, maxNumberOfSubjects = 200)

# Calculate the power, stopping probabilities, and expected sample size in a single 
# arm design at given maximum sample size N = 60 in a three-stage two-sided 
# O'Brien & Fleming design with information rate vector (0.2,0.5,1) 
# for a range of pi1 values when testing H0: pi = 0.3:
getPowerRates(getDesignGroupSequential(informationRates = c(0.2,0.5,1), 
    sided = 2), groups = 1, thetaH0 = 0.3, pi1 = seq(0.3, 0.5, 0.05),  
    maxNumberOfSubjects = 60)

}