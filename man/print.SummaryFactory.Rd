% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_summary.R
\name{print.SummaryFactory}
\alias{print.SummaryFactory}
\title{Summary Factory Printing}
\usage{
\method{print}{SummaryFactory}(x, ..., markdown = NA, sep = NA_character_)
}
\arguments{
\item{x}{The summary factory object.}

\item{...}{Optional plot arguments. At the moment \code{xlim} and \code{ylim} are implemented
for changing x or y axis limits without dropping data observations.}

\item{markdown}{If \code{TRUE}, the object \code{x}
will be printed using markdown syntax;
normal representation will be used otherwise (default is \code{FALSE})}

\item{sep}{The separator line between the summary and the print output, default is \code{"\n\n-----\n\n"}.}
}
\description{
Prints the result object stored inside a summary factory.
}
\details{
Generic function to print all kinds of summary factories.
}
