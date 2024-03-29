% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_core_plot_settings.R
\docType{class}
\name{PlotSettings}
\alias{PlotSettings}
\title{Plot Settings}
\description{
Class for plot settings.
}
\details{
Collects typical plot settings in an object.
}
\section{Fields}{

\describe{
\item{\code{lineSize}}{The line size.}

\item{\code{pointSize}}{The point size.}

\item{\code{pointColor}}{The point color, e.g., "red" or "blue".}

\item{\code{mainTitleFontSize}}{The main tile font size.}

\item{\code{axesTextFontSize}}{The text font size.}

\item{\code{legendFontSize}}{The legend font size.}

\item{\code{scalingFactor}}{The scaling factor.}
}}

\section{Methods}{

\describe{
\item{\code{adjustLegendFontSize(adjustingValue)}}{Adjusts the legend font size, e.g., run \cr
\code{adjustLegendFontSize(-2)} # makes the font size 2 points smaller}

\item{\code{enlargeAxisTicks(p)}}{Enlarges the axis ticks}

\item{\code{expandAxesRange(p, x = NA_real_, y = NA_real_)}}{Expands the axes range}

\item{\code{hideGridLines(p)}}{Hides the grid lines}

\item{\code{setAxesAppearance(p)}}{Sets the font size and face of the axes titles and texts}

\item{\code{setColorPalette(p, palette, mode = c("colour", "fill", "all"))}}{Sets the color palette}

\item{\code{setLegendBorder(p)}}{Sets the legend border}

\item{\code{setMainTitle(p, mainTitle, subtitle = NA_character_)}}{Sets the main title}

\item{\code{setMarginAroundPlot(p, margin = 0.2)}}{Sets the margin around the plot, e.g., run \cr
\code{setMarginAroundPlot(p, .2)} or \cr
\code{setMarginAroundPlot(p, c(.1, .2, .1, .2)}}

\item{\code{setTheme(p)}}{Sets the theme}
}}

\keyword{internal}
