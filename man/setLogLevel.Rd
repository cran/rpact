% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_logger.R
\name{setLogLevel}
\alias{setLogLevel}
\title{Set Log Level}
\usage{
setLogLevel(
  logLevel = c("PROGRESS", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "DISABLED")
)
}
\arguments{
\item{logLevel}{The new log level to set. Can be one of
"PROGRESS", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "DISABLED".
Default is "PROGRESS".}
}
\description{
Sets the \code{rpact} log level.
}
\details{
This function sets the log level of the \code{rpact} internal log message system.
By default only calculation progress messages will be shown on the output console,
particularly \code{\link[=getAnalysisResults]{getAnalysisResults()}} shows this kind of messages.
The output of these messages can be disabled by setting the log level to \code{"DISABLED"}.
}
\examples{
\dontrun{
# show debug messages
setLogLevel("DEBUG")

# disable all log messages
setLogLevel("DISABLED")
}

}
\seealso{
\itemize{
\item \code{\link[=getLogLevel]{getLogLevel()}} for getting the current log level,
\item \code{\link[=resetLogLevel]{resetLogLevel()}} for resetting the log level to default.
}
}
\keyword{internal}
