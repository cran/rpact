% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_simulation_results.R
\name{SimulationResults}
\alias{SimulationResults}
\title{Class for Simulation Results}
\description{
A class for simulation results.
}
\details{
\code{SimulationResults} is the basic class for
\itemize{
\item \code{\link{SimulationResultsMeans}},
\item \code{\link{SimulationResultsRates}},
\item \code{\link{SimulationResultsSurvival}},
\item \code{\link{SimulationResultsCountData}},
\item \code{\link{SimulationResultsMultiArmMeans}},
\item \code{\link{SimulationResultsMultiArmRates}},
\item \code{\link{SimulationResultsMultiArmSurvival}},
\item \code{\link{SimulationResultsEnrichmentMeans}},
\item \code{\link{SimulationResultsEnrichmentRates}}, and
\item \code{\link{SimulationResultsEnrichmentSurvival}}.
}
}
\section{Fields}{

\describe{
\item{\code{seed}}{The seed used for random number generation. Is a numeric vector of length 1.}

\item{\code{iterations}}{The number of iterations used for simulations. Is a numeric vector of length 1 containing a whole number.}
}}

\keyword{internal}
