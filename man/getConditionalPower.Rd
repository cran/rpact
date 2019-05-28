% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_analysis.R
\name{getConditionalPower}
\alias{getConditionalPower}
\title{Get Conditional Power}
\usage{
getConditionalPower(design, stageResults, ..., nPlanned)
}
\arguments{
\item{design}{The trial design.}

\item{stageResults}{The results at given stage, obtained from \code{\link{getStageResults}}.}

\item{nPlanned}{The sample size planned for the subsequent stages.
It should be a vector with length equal to the remaining stages and is the 
overall sample size in the two treatment groups if two groups are considered.}

\item{stage}{The stage number (optional). Default: total number of existing stages in the data input.}

\item{allocationRatioPlanned}{The allocation ratio for two treatment groups planned for 
the subsequent stages, the default value is 1.}

\item{thetaH1}{or pi1, pi2 Assumed effect sizes or assumed rates pi1 to calculate the 
conditional power. Depending on the type of dataset, either thetaH1 (means and survival) 
or pi1, pi2 (rates) needs to be specified. 
   Additionally, if testing means is specified, an assumed standard (\code{assumedStDev}) 
   deviation can be specified, default is 1.}

\item{iterations}{Iterations for simulating the power for Fisher's combination test. 
If the power for more than one remaining stages is to be determined for Fisher's combination test, 
it is estimated via simulation with specified \code{iterations}, the default value is 10000.}

\item{seed}{Seed for simulating the power for Fisher's combination test. See above, default is a random seed.}
}
\description{
Calculates and returns the conditional power.
}
\details{
The conditional power is calculated only if effect size and sample size is specified.

For Fisher's combination test, the conditional power for more than one remaining stages is 
estimated via simulation.
}
\seealso{
\code{\link{plot.StageResults}} or \code{\link{plot.AnalysisResults}} for plotting the conditional power.
}
\keyword{internal}