% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_core_output_formats.R
\name{getOutputFormat}
\alias{getOutputFormat}
\title{Get Output Format}
\usage{
getOutputFormat(
  parameterName = NA_character_,
  ...,
  file = NA_character_,
  default = FALSE,
  fields = TRUE
)
}
\arguments{
\item{parameterName}{The name of the parameter whose output format shall be returned.
Leave the default \code{NA_character_} if
the output format of all parameters shall be returned.}

\item{...}{Ensures that all arguments (starting from the "...") are to be named and
that a warning will be displayed if unknown arguments are passed.}

\item{file}{An optional file name where to write the output formats
(see Details for more information).}

\item{default}{If \code{TRUE} the default output format of the specified parameter(s)
will be returned, default is \code{FALSE}.}

\item{fields}{If \code{TRUE} the names of all affected object fields will be displayed, default is \code{TRUE}.}
}
\value{
A named list of output formats.
}
\description{
With this function the format of the standard outputs of all \code{rpact}
objects can be shown and written to a file.
}
\details{
Output formats can be written to a text file by specifying a \code{file}.
See \code{\link[=setOutputFormat]{setOutputFormat()}}() to learn how to read a formerly saved file.

Note that the \code{parameterName} must not match exactly, e.g., for p-values the
following parameter names will be recognized amongst others:
\enumerate{
\item \code{p value}
\item \code{p.values}
\item \code{p-value}
\item \code{pValue}
\item \code{rpact.output.format.p.value}
}
}
\examples{
\dontrun{
# show output format of p values
getOutputFormat("p.value")

# set new p value output format
setOutputFormat("p.value", digits = 5, nsmall = 5)

# show sample sizes as smallest integers not less than the not rounded values
setOutputFormat("sample size", digits = 0, nsmall = 0, roundFunction = "ceiling")
getSampleSizeMeans()

# show sample sizes as smallest integers not greater than the not rounded values
setOutputFormat("sample size", digits = 0, nsmall = 0, roundFunction = "floor")
getSampleSizeMeans()

# set new sample size output format without round function
setOutputFormat("sample size", digits = 2, nsmall = 2)
getSampleSizeMeans()

# reset sample size output format to default
setOutputFormat("sample size")
getSampleSizeMeans()
getOutputFormat("sample size")
}
}
\seealso{
Other output formats: 
\code{\link{setOutputFormat}()}
}
\concept{output formats}
