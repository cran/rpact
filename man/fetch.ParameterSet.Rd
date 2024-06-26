% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_core_parameter_set.R
\name{pull}
\alias{pull}
\alias{pull.ParameterSet}
\alias{obtain}
\alias{obtain.ParameterSet}
\alias{fetch}
\alias{fetch.ParameterSet}
\title{Extract a single parameter}
\usage{
pull(x, var, output)

\method{pull}{ParameterSet}(x, var = -1, output = c("named", "value", "list"))

obtain(x, var, output)

\method{obtain}{ParameterSet}(x, var = -1, output = c("named", "value", "list"))

fetch(x, var, output)

\method{fetch}{ParameterSet}(x, var = -1, output = c("named", "value", "list"))
}
\arguments{
\item{x}{The \code{\link{ParameterSet}} object to fetch from.}

\item{var}{A variable specified as:
\itemize{
\item a literal variable name
\item a positive integer, giving the position counting from the left
\item a negative integer, giving the position counting from the right.
The default returns the last parameter.
This argument is taken by expression and supports quasiquotation (you can unquote column names and column locations).
}}

\item{output}{A character defining the output type as follows:
\itemize{
\item "named" (default) returns the named value if the value is a single value, the value inside a named list otherwise
\item "value" returns only the value itself
\item "list" returns the value inside a named list
}}
}
\description{
Fetch a parameter from a parameter set.
}
\examples{
\dontrun{
getDesignInverseNormal() |> fetch(kMax)
getDesignInverseNormal() |> fetch(kMax, output = "list")
}

}
