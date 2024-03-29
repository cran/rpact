% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_analysis_base.R
\name{getFinalPValue}
\alias{getFinalPValue}
\title{Get Final P Value}
\usage{
getFinalPValue(stageResults, ...)
}
\arguments{
\item{stageResults}{The results at given stage, obtained from \code{\link[=getStageResults]{getStageResults()}}.}

\item{...}{Only available for backward compatibility.}
}
\value{
Returns a \code{\link[base]{list}} containing
\itemize{
\item \code{finalStage},
\item \code{pFinal}.
}
}
\description{
Returns the final p-value for given stage results.
}
\details{
The calculation of the final p-value is based on the stage-wise ordering of the sample space.
This enables the calculation for both the non-adaptive and the adaptive case.
For Fisher's combination test, it is available for \code{kMax = 2} only.
}
\examples{
\dontrun{
design <- getDesignInverseNormal(kMax = 2)
data <- getDataset(
    n      = c( 20,  30),
    means  = c( 50,  51),
    stDevs = c(130, 140)
)
getFinalPValue(getStageResults(design, dataInput = data))
}

}
\seealso{
Other analysis functions: 
\code{\link{getAnalysisResults}()},
\code{\link{getClosedCombinationTestResults}()},
\code{\link{getClosedConditionalDunnettTestResults}()},
\code{\link{getConditionalPower}()},
\code{\link{getConditionalRejectionProbabilities}()},
\code{\link{getFinalConfidenceInterval}()},
\code{\link{getRepeatedConfidenceIntervals}()},
\code{\link{getRepeatedPValues}()},
\code{\link{getStageResults}()},
\code{\link{getTestActions}()}
}
\concept{analysis functions}
