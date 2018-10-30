######################################################################################
#                                                                                    #
# -- Plot setting classes --                                                         #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.0                                                                #
# Date: 25-09-2018                                                                   #
# Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD                             #
# Licensed under "GNU Lesser General Public License" version 3                       #
# License text can be found here: https://www.r-project.org/Licenses/LGPL-3          #
#                                                                                    #
# RPACT company website: https://www.rpact.com                                       #
# RPACT package website: https://www.rpact.org                                       #
#                                                                                    #
# Contact us for information about our services: info@rpact.com                      #
#                                                                                    #
######################################################################################


#' 
#' @name PlotSettings
#' 
#' @title
#' Plot Settings
#' 
#' @description
#' Class for plot settings.
#' 
#' @field lineSize The line size.
#' @field pointSize The point size.
#' @field mainTitleFontSize The main tile font size.
#' @field axesTextFontSize The text font size.
#' @field legendFontSize The legend font size.
#' 
#' @details
#' Collects typical plot settings in an object.
#' 
#' @name PlotSettings_initialize
#' Initializes the object.
#' 
#' @keywords internal
#' 
#' @include class_core_parameter_set.R
#' 
#' @importFrom methods new
#'
PlotSettings <- setRefClass("PlotSettings",
	contains = "ParameterSet",
	
	fields = list(
		.legendLineBreakIndex = "numeric",
		.pointSize = "numeric",
		.legendFontSize = "numeric",
		lineSize = "numeric",
		pointSize = "numeric",
		mainTitleFontSize = "numeric",
		axesTextFontSize = "numeric",
		legendFontSize = "numeric"
	),
	
	methods = list(
		initialize = function(
				lineSize = 0.8, 
				pointSize = 3, 
				mainTitleFontSize = 14,
				axesTextFontSize = 10,
				legendFontSize = 11, 
				...) {
			callSuper(
				lineSize = lineSize, 
				pointSize = pointSize, 
				mainTitleFontSize = mainTitleFontSize, 
				axesTextFontSize = axesTextFontSize, 
				legendFontSize = legendFontSize, 
				...)
			.legendLineBreakIndex <<- 15
			.pointSize <<- pointSize
			.legendFontSize <<- legendFontSize
			
			.parameterNames <<- list(
				"lineSize" = "Line size",
				"pointSize" = "Point size",
				"mainTitleFontSize" = "Main title font size",
				"axesTextFontSize" = "Axes text font size",
				"legendFontSize" = "Legend font size"
			)
		},
		
		show = function(showType = 1) {
			'Method for automatically printing plot setting objects'	

			.showParametersOfOneGroup(parameters = .getVisibleFieldNames(), 
				title = "Plot settings", orderByParameterName = FALSE)
		},
	
		setColorPalette = function(p, palette, mode = c("colour", "fill", "all")) {
			"Sets the color palette"
			# l = 45: make colors slightly darker
			if (is.null(palette) || is.na(palette)) {
				if (mode[1] %in% c("colour", "all")) {
					p <- p + ggplot2::scale_colour_hue(l = 45) 
				}
				if (mode[1] %in% c("fill", "all")) {
					p <- p + ggplot2::scale_fill_hue(l = 45) 
				}
			}
			else if (is.character(palette)) {
				if (mode[1] %in% c("colour", "all")) {
					p <- p + ggplot2::scale_colour_brewer(palette = palette)
				}
				if (mode[1] %in% c("fill", "all")) {
					p <- p + ggplot2::scale_fill_brewer(palette = palette)
				}
			} 
			else if (palette == 0) {
				if (mode[1] %in% c("colour", "all")) {
					p <- p + ggplot2::scale_colour_grey()
				}
				if (mode[1] %in% c("fill", "all")) {
					p <- p + ggplot2::scale_fill_grey()
				}
			}	
			else {
				if (mode[1] %in% c("colour", "all")) {
					p <- p + ggplot2::scale_colour_hue(l = 45)
				}
				if (mode[1] %in% c("fill", "all")) {
					p <- p + ggplot2::scale_fill_hue(l = 45)
				}
			}
			return(p)
		},
		 
		enlargeAxisTicks = function(p) {
			"Enlarges the axis ticks"
			p <- p + ggplot2::theme(axis.ticks.length = ggplot2::unit(0.3, "cm"))
			return(p)
		},
		
		setAxesAppearance = function(p) {
			"Sets the font size and face of the axes titles and texts"
			p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(size = axesTextFontSize + 1, face = "bold"))
			p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(size = axesTextFontSize + 1, face = "bold"))
			p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(size = axesTextFontSize))
			p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(size = axesTextFontSize))
			return(p)
		},
		
		# Sets the axes labels
		setAxesLabels = function(p, xAxisLabel = NULL, yAxisLabel1 = NULL, yAxisLabel2 = NULL, 
				xlab = NA_character_, ylab = NA_character_, 
				scalingFactor1 = 1, scalingFactor2 = 1) {
			if (!is.na(xlab)) {
				xAxisLabel = xlab
			}
			if (xAxisLabel == "Theta") {
				p <- p + ggplot2::xlab(bquote("Theta"~Theta))
			} else {
				p <- p + ggplot2::xlab(xAxisLabel)
			}
			if (sum(is.na(ylab)) == 0) {
				yAxisLabel1 = ylab[1]
				if (length(ylab) == 2) {
					yAxisLabel2 = ylab[2]
				}
			}
			p <- p + ggplot2::ylab(yAxisLabel1)

			p <- setSecondYAxisOnRightSide(p, yAxisLabel1, yAxisLabel2, scalingFactor1, scalingFactor2)
			
			return(p)
		},
		
		setSecondYAxisOnRightSide = function(p, yAxisLabel1, yAxisLabel2, scalingFactor1 = 1, scalingFactor2 = 1) {
			if (!is.null(yAxisLabel2) && scalingFactor1 != scalingFactor2) {
				p <- p + ggplot2::scale_y_continuous(yAxisLabel1, 
					sec.axis = ggplot2::sec_axis(~ . * scalingFactor1 / scalingFactor2, name = yAxisLabel2))
			}
			return(p)
		},
		
		setLegendTitle = function(p, legendTitle, mode = c("colour", "fill")) {
			if (!is.null(legendTitle) && !is.na(legendTitle) && trimws(legendTitle) != "") {
				if (mode[1] == "colour") {
					p <- p + ggplot2::labs(colour = .getTextLineWithLineBreak(legendTitle, 
							lineBreakIndex = .legendLineBreakIndex)) 
				} else {
					p <- p + ggplot2::labs(fill = .getTextLineWithLineBreak(legendTitle, 
							lineBreakIndex = .legendLineBreakIndex)) 
				}
				p <- p + ggplot2::theme(legend.title = ggplot2::element_text(
						colour = "black", size = legendFontSize + 1, face = "bold"))
			} else {
				p <- p + ggplot2::labs(colour = NULL)
			}
			return(p)
		},
		
		setLegendLabelSize = function(p) {
			p <- p + ggplot2::theme(legend.text = ggplot2::element_text(size = legendFontSize))
			return(p)
		},
		
		setLegendPosition = function(p, legendPosition) {
			.assertIsValidLegendPosition(legendPosition)
			
			switch(as.character(legendPosition),
				'-1' = {
					p <- p + ggplot2::theme(legend.position = "none")
				},
				'0' = {
					p <- p + ggplot2::theme(aspect.ratio = 1)
				},
				'1' = {
					p <- p + ggplot2::theme(legend.position = c(0.05, 1), legend.justification = c(0, 1))
				},
				'2' = {
					p <- p + ggplot2::theme(legend.position = c(0.05, 0.5), legend.justification = c(0, 0.5))
				},
				'3' = {
					p <- p + ggplot2::theme(legend.position = c(0.05, 0.05), legend.justification = c(0, 0))
				},
				'4' = {
					p <- p + ggplot2::theme(legend.position = c(0.95, 1), legend.justification = c(1, 1))
				},
				'5' = {
					p <- p + ggplot2::theme(legend.position = c(0.95, 0.5), legend.justification = c(1, 0.5))
				},
				'6' = {
					p <- p + ggplot2::theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0))
				}
			)
	
			return(p)
		},
		
		setLegendBorder = function(p) {
			"Sets the legend border"
			p <- p + ggplot2::theme(legend.background = 
					ggplot2::element_rect(fill = "white", colour = "black", size = 0.4))
			return(p)
		},
		
		adjustPointSize = function(adjustingValue) {
			pointSize <<- .pointSize + adjustingValue
		},
		
		adjustLegendFontSize = function(adjustingValue) {
			"Adjusts the legend font size, e.g., run \\cr
			\\code{adjustLegendFontSize(-2)} # makes the font size 2 points smaller"
			legendFontSize <<- .legendFontSize + adjustingValue
		},
		
		setMainTitle = function(p, mainTitle, subtitle = NA_character_) {
			"Sets the main title"
			p <- p + ggplot2::ggtitle(mainTitle)
			p <- p + ggplot2::theme(plot.title = 
					ggplot2::element_text(hjust = 0.5, size = mainTitleFontSize, face = "bold"))
			
			if (!is.na(subtitle)) {
				p <- p + ggplot2::ggtitle(mainTitle, subtitle = subtitle)
				p <- p + ggplot2::theme(
					plot.title = ggplot2::element_text(hjust = 0.5, size = mainTitleFontSize, face = "bold"), 
					plot.subtitle = ggplot2::element_text(hjust = 0.5, size = mainTitleFontSize - 2))
			} else {
				p <- p + ggplot2::ggtitle(mainTitle)
				p <- p + ggplot2::theme(plot.title = ggplot2::element_text(
						hjust = 0.5, size = mainTitleFontSize, face = "bold"))				
			}
			
			return(p)
		},
		
		setMarginAroundPlot = function(p, margin = 0.2) {
			"Sets the margin around the plot, e.g., run \\cr
			\\code{setMarginAroundPlot(p, .2)} or \\cr
			\\code{setMarginAroundPlot(p, c(.1, .2, .1, .2)}"
			if (length(margin == 1)) {
				margin = base::rep(margin, 4)
			}
			if (!(length(margin) %in% c(1, 4))) {
				stop("'margin' must be a numeric vector with length 1 or 4")
			}
			p <- p + ggplot2::theme(plot.margin = ggplot2::unit(margin, "cm"))
			return(p)
		},
		
		expandAxesRange = function(p, x = NA_real_, y = NA_real_) {
			"Expands the axes range"
			if (!is.na(x)) {
				p <- p + ggplot2::expand_limits(x = x)
			}
			if (!is.na(y)) {
				p <- p + ggplot2::expand_limits(y = y)
			}
			return(p)
		},
		
		hideGridLines = function(p) {
			"Hides the grid lines"
			p <- p + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
			p <- p + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
			p <- p + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
			p <- p + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
			return(p)
		},
		
		setTheme = function(p) {
			"Sets the theme"
			p <- p + ggplot2::theme_bw() 
			p <- p + ggplot2::theme(panel.border = ggplot2::element_blank(), 
				axis.line = ggplot2::element_line(colour = "black"))
			return(p)
		},
		
		plotValues = function(p, plotLineEnabled = TRUE, plotPointsEnabled = TRUE, pointBorder = 4) {
			if (plotLineEnabled) {
				p <- p + ggplot2::geom_line(size = lineSize)
			}
			if (plotPointsEnabled) {
				# plot white border around the points
				if (pointBorder > 0) {
					p <- p + ggplot2::geom_point(color = "white", size = pointSize + pointBorder)
				}
				p <- p + ggplot2::geom_point(size = pointSize)
			}
			return(p)
		},
		
		mirrorYValues = function(p, yValues, plotLineEnabled = TRUE, plotPointsEnabled = TRUE, pointBorder = 4) {
			if (plotLineEnabled) {
				p <- p + ggplot2::geom_line(ggplot2::aes(y = -yValues), size = lineSize)
			}
			if (plotPointsEnabled) {
				# plot white border around the points
				if (pointBorder > 0) {
					p <- p + ggplot2::geom_point(ggplot2::aes(y = -yValues), 
						color = "white", size = pointSize + pointBorder)
				}
				p <- p + ggplot2::geom_point(ggplot2::aes(y = -yValues), size = pointSize)
			}
			return(p)
		},
		
		addCompanyAnnotation = function(p, enabled = TRUE) {
			if (!enabled) {
				return(p)
			}
			
			label <- "www.rpact.org"
			p <- p + ggplot2::annotate("label", x = -Inf, y = Inf, hjust = -0.1, vjust=1, 
				label = label, size = 2.8, colour = "white", fill = "white")
			
			p <- p + ggplot2::annotate("text", x = -Inf, y = Inf, label = label,
				hjust=-.12, vjust=1, colour = "lightgray", size = 2.7)
			return(p)
		}
	)
)

