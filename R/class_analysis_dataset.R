######################################################################################
#                                                                                    #
# -- Dataset classes --                                                              #
#                                                                                    #
# This file is part of the R package RPACT - R Package for Adaptive Clinical Trials. #
#                                                                                    # 
# File version: 1.0.1                                                                #
# Date: 23-11-2018                                                                   #
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

C_KEY_WORDS_GROUPS <- c("group", "groups")
C_KEY_WORDS_STAGES <- c("stage", "stages")

C_KEY_WORDS_SAMPLE_SIZES <- c("n", "sampleSizes", "sampleSize")
C_KEY_WORDS_MEANS <- c("means", "mean")
C_KEY_WORDS_ST_DEVS <- c("stDevs", "stDev", "stds", "st.dev", "sd")

C_KEY_WORDS_EVENTS <- c("event", "events")
C_KEY_WORDS_EVENTS_1 <- c("event1", "events1")
C_KEY_WORDS_EVENTS_2 <- c("event2", "events2")

C_KEY_WORDS_OVERALL_EVENTS <- c("overallEvents", "overallEvent", "overall.events", "overall.event")
C_KEY_WORDS_OVERALL_EVENTS_1 <- c("overallEvents1", "overallEvent1", "overall.events.1", "overall.event.1")
C_KEY_WORDS_OVERALL_EVENTS_2 <- c("overallEvents2", "overallEvent2", "overall.events.2", "overall.event.2")

C_KEY_WORDS_SAMPLE_SIZES_1 <- c("n1", "sampleSize1", "sampleSizes1")
C_KEY_WORDS_MEANS_1 <- c("means1", "mean1")
C_KEY_WORDS_ST_DEVS_1 <- c("stDevs1", "stDev1", "stds1", "st.dev1", "sd1")

C_KEY_WORDS_SAMPLE_SIZES_2 <- c("n2", "sampleSize2", "sampleSizes2")
C_KEY_WORDS_MEANS_2 <- c("means2", "mean2")
C_KEY_WORDS_ST_DEVS_2 <- c("stDevs2", "stDev2", "stds2", "st.dev2", "sd2")

C_KEY_WORDS_OVERALL_SAMPLE_SIZES <- c("overallN", "overall.n", 
	"overallSampleSizes", "overallSampleSize")
C_KEY_WORDS_OVERALL_MEANS <- c("overallMeans", "overallMean", "overall.means", "overall.mean")
C_KEY_WORDS_OVERALL_ST_DEVS <- c("overallStDevs", "overallStDev", 
	"overall.st.dev", "overall.stds", "overall.sd")

C_KEY_WORDS_OVERALL_SAMPLE_SIZES_1 <- c("overallN1", "overall.n.1", 
	"overallSampleSizes1", "overallSampleSize1")
C_KEY_WORDS_OVERALL_MEANS_1 <- c("overallMeans1", "overallMean1", 
	"overall.means.1", "overall.mean.1")
C_KEY_WORDS_OVERALL_ST_DEVS_1 <- c("overallStDevs1", "overallStDev1", 
	"overall.st.dev.1", "overall.stds.1", "overall.sd.1")

C_KEY_WORDS_OVERALL_SAMPLE_SIZES_2 <- c("overallN2", "overall.n.2", 
	"overallSampleSizes2", "overallSampleSize2")
C_KEY_WORDS_OVERALL_MEANS_2 <- c("overallMeans2", "overallMean2", "overall.means.2", "overall.mean.2")
C_KEY_WORDS_OVERALL_ST_DEVS_2 <- c("overallStDevs2", "overallStDev2", 
	"overall.st.dev.2", "overall.stds.2", "overall.sd.2")

C_KEY_WORDS_ALLOCATION_RATIOS <- c("allocationRatios", "allocationRatio", "ar", 
	"allocation.ratios", "allocation.ratio")
C_KEY_WORDS_LOG_RANKS <- c("logRanks", "logRank", "lr", "log.ranks", "log.rank")

C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS <- c("overallAllocationRatios", "overallAllocationRatio", "oar",
	"overall.allocation.ratios", "overall.allocation.ratio")
C_KEY_WORDS_OVERALL_LOG_RANKS <- c("overallLogRanks", "overallLogRank", "olr",
	"overall.log.ranks", "overall.log.rank")

C_KEY_WORDS <- c(
	C_KEY_WORDS_GROUPS,
	C_KEY_WORDS_STAGES,
	C_KEY_WORDS_SAMPLE_SIZES, 
	C_KEY_WORDS_MEANS, 
	C_KEY_WORDS_ST_DEVS,
	C_KEY_WORDS_EVENTS, 
	C_KEY_WORDS_EVENTS_1, 
	C_KEY_WORDS_EVENTS_2, 
	C_KEY_WORDS_OVERALL_EVENTS, 
	C_KEY_WORDS_OVERALL_EVENTS_1, 
	C_KEY_WORDS_OVERALL_EVENTS_2, 
	C_KEY_WORDS_SAMPLE_SIZES_1, 
	C_KEY_WORDS_MEANS_1, 
	C_KEY_WORDS_ST_DEVS_1, 
	C_KEY_WORDS_SAMPLE_SIZES_2, 
	C_KEY_WORDS_MEANS_2, 
	C_KEY_WORDS_ST_DEVS_2, 
	C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 
	C_KEY_WORDS_OVERALL_MEANS, 
	C_KEY_WORDS_OVERALL_ST_DEVS, 
	C_KEY_WORDS_OVERALL_SAMPLE_SIZES_1, 
	C_KEY_WORDS_OVERALL_MEANS_1, 
	C_KEY_WORDS_OVERALL_ST_DEVS_1, 
	C_KEY_WORDS_OVERALL_SAMPLE_SIZES_2, 
	C_KEY_WORDS_OVERALL_MEANS_2, 
	C_KEY_WORDS_OVERALL_ST_DEVS_2, 
	C_KEY_WORDS_ALLOCATION_RATIOS, 
	C_KEY_WORDS_LOG_RANKS, 
	C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, 
	C_KEY_WORDS_OVERALL_LOG_RANKS
)

#' @title
#' Read Dataset
#' 
#' @description  
#' Reads a data file and returns it as dataset object.    
#' 
#' @param file A CSV file (see \code{\link[utils]{read.table}}).
#' @param header A logical value indicating whether the file contains the names of 
#'        the variables as its first line.
#' @param sep The field separator character. Values on each line of the file are separated 
#'        by this character. If sep = "," (the default for \code{readDataset}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use 
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only 
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param fill logical. If \code{TRUE} then in case the rows have unequal length, blank fields 
#'        are implicitly added. 
#' @param comment.char character: a character vector of length one containing a single character 
#'        or an empty string. Use "" to turn off the interpretation of comments altogether.
#' @param fileEncoding character string: if non-empty declares the encoding used on a file 
#'        (not a connection) so the character data can be re-encoded. 
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to code{\link[utils]{read.table}}.
#' 
#' @details
#' \code{readDataset} is a wrapper function that uses \code{\link[utils]{read.table}} to read the
#' CSV file into a data frame, transfers it from long to wide format with \code{\link[stats]{reshape}} 
#' and puts the data to \code{\link{getDataset}}.
#'  
#' @return Returns a \code{\link{Dataset}} object.
#' 
#' @seealso 
#' \itemize{
#'   \item \code{\link{readDatasets}} for reading multiple datasets,
#'   \item \code{\link{writeDataset}} for writing a single dataset, 
#'   \item \code{\link{writeDatasets}} for writing multiple datasets.
#' }
#' 
#' @export
#'
readDataset <- function(file, ..., header = TRUE, sep = ",", quote = "\"",
		dec = ".", fill = TRUE, comment.char = "", fileEncoding = "UTF-8") {

	if (!file.exists(file)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the file '", file, "' does not exist")
	}
		
	data <- utils::read.table(file = file, header = header, sep = sep, 
		quote = quote, dec = dec, fill = fill, fileEncoding = fileEncoding, ...)
	dataWide <- stats::reshape(data = data, direction = "wide", idvar = "stages", timevar = "groups")
	colnames(dataWide) <- gsub("\\.", "", colnames(dataWide))
	return(getDataset(dataWide))
}

#' @title
#' Write Dataset
#' 
#' @description  
#' Writes a dataset to a CSV file.    
#' 
#' @param dataset A dataset.
#' @param file The target CSV file.
#' @param append Logical. Only relevant if file is a character string. 
#'        If \code{TRUE}, the output is appended to the file. If \code{FALSE}, any existing file of the name is destroyed.
#' @param sep The field separator character. Values on each line of the file are separated 
#'        by this character. If sep = "," (the default for \code{writeDataset}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use 
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only 
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param eol The character(s) to print at the end of each line (row). 
#' @param na The string to use for missing values in the data.
#' @param row.names Either a logical value indicating whether the row names of \code{dataset} are 
#'        to be written along with  \code{dataset}, or a character vector of row names to be written.
#' @param col.names Either a logical value indicating whether the column names of  \code{dataset} are 
#'        to be written along with  \code{dataset}, or a character vector of column names to be written. 
#'        See the section on 'CSV files' for the meaning of \code{col.names = NA}.
#' @param qmethod A character string specifying how to deal with embedded double quote characters 
#'        when quoting strings. Must be one of "double" (default in \code{writeDataset}) or "escape".
#' @param fileEncoding Character string: if non-empty declares the encoding used on a file 
#'        (not a connection) so the character data can be re-encoded. 
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{write.table}}.
#' 
#' @details
#' \code{\link{writeDataset}} is a wrapper function that coerces the dataset to a data frame and uses \cr 
#' \code{\link[utils]{write.table}} to write it to a CSV file.
#' 
#' @seealso 
#' \itemize{
#'   \item \code{\link{writeDatasets}} for writing multiple datasets,
#'   \item \code{\link{readDataset}} for reading a single dataset, 
#'   \item \code{\link{readDatasets}} for reading multiple datasets.
#' }
#' 
#' @export
#'
writeDataset <- function(dataset, file, ..., append = FALSE, quote = TRUE, sep = ",",
		eol = "\n", na = "NA", dec = ".", row.names = TRUE,
		col.names = NA, qmethod = "double",
		fileEncoding = "UTF-8") {
		
	.assertIsDataset(dataset)
	
	x <- as.data.frame(dataset, niceColumnNamesEnabled = FALSE)
		
	utils::write.table(x = x, file = file, append = append, quote = quote, sep = sep,
		eol = eol, na = na, dec = dec, row.names = FALSE,
		col.names = TRUE, qmethod = qmethod,
		fileEncoding = fileEncoding)
}

#' @title
#' Read Multiple Datasets
#' 
#' @description  
#' Reads a data file and returns it as a list of dataset objects.    
#' 
#' @param file A CSV file (see \code{\link[utils]{read.table}}).
#' @param header A logical value indicating whether the file contains the names of 
#'        the variables as its first line.
#' @param sep The field separator character. Values on each line of the file are separated 
#'        by this character. If sep = "," (the default for \code{readDatasets}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use 
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only 
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param fill logical. If \code{TRUE} then in case the rows have unequal length, blank fields 
#'        are implicitly added. 
#' @param comment.char character: a character vector of length one containing a single character 
#'        or an empty string. Use "" to turn off the interpretation of comments altogether.
#' @param fileEncoding character string: if non-empty declares the encoding used on a file 
#'        (not a connection) so the character data can be re-encoded. 
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{read.table}}.
#' 
#' @details
#' Reads a file that was written by \code{\link{writeDatasets}} before.
#'  
#' @return Returns a list of \code{\link{Dataset}} objects.
#' 
#' @seealso 
#' \itemize{
#'   \item \code{\link{readDataset}} for reading a single dataset,
#'   \item \code{\link{writeDatasets}} for writing multiple datasets, 
#'   \item \code{\link{writeDataset}} for writing a single dataset.
#' }
#' 
#' @export
#'
readDatasets <- function(file, ..., header = TRUE, sep = ",", quote = "\"",
		dec = ".", fill = TRUE, comment.char = "", fileEncoding = "UTF-8") {
	
	if (!file.exists(file)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the file '", file, "' does not exist")
	}
	
	data <- utils::read.table(file = file, header = header, sep = sep, 
		quote = quote, dec = dec, fill = fill, fileEncoding = fileEncoding, ...)
	
	if (is.null(data[["datasetId"]])) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "data file must contain the column 'datasetId'")
	}
	
	datasets <- list()
	for (datasetId in unique(data$datasetId)) {
		subData <- data[data$datasetId == datasetId, ]
		dataFrame <- subset(subData, select = -datasetId)
		description <- NA_character_
		if (!is.null(dataFrame[["description"]])) {
			description <- as.character(dataFrame$description[1])
			dataFrame <- subset(dataFrame, select = -description)
		}
		if (length(unique(subData$groups)) == 2) {
			dataWide <- stats::reshape(dataFrame, direction = "wide", idvar = "stages", timevar = "groups")
			colnames(dataWide) <- gsub("\\.", "", colnames(dataWide))
			dataset <- getDataset(dataWide)
		} else {
			dataset <- getDataset(dataFrame)
		}
		dataset$setDescription(description)
		datasets <- c(datasets, dataset)
	}
	return(datasets)
}

#' @title
#' Write Multiple Datasets
#' 
#' @description  
#' Writes a list of datasets to a CSV file.    
#' 
#' @param datasets A list of datasets.
#' @param file The target CSV file.
#' @param append Logical. Only relevant if file is a character string. 
#'        If \code{TRUE}, the output is appended to the file. If FALSE, any existing file of the name is destroyed.
#' @param sep The field separator character. Values on each line of the file are separated 
#'        by this character. If sep = "," (the default for \code{writeDatasets}) the separator is a comma.
#' @param quote The set of quoting characters. To disable quoting altogether, use 
#'        quote = "". See scan for the behavior on quotes embedded in quotes. Quoting is only 
#'        considered for columns read as character, which is all of them unless \code{colClasses} is specified.
#' @param dec The character used in the file for decimal points.
#' @param eol The character(s) to print at the end of each line (row). 
#' @param na The string to use for missing values in the data.
#' @param row.names Either a logical value indicating whether the row names of \code{dataset} are 
#'        to be written along with  \code{dataset}, or a character vector of row names to be written.
#' @param col.names Either a logical value indicating whether the column names of  \code{dataset} are 
#'        to be written along with  \code{dataset}, or a character vector of column names to be written. 
#'        See the section on 'CSV files' for the meaning of \code{col.names = NA}.
#' @param qmethod A character string specifying how to deal with embedded double quote characters 
#'        when quoting strings. Must be one of "double" (default in \code{writeDatasets}) or "escape".
#' @param fileEncoding Character string: if non-empty declares the encoding used on a file 
#'        (not a connection) so the character data can be re-encoded. 
#'        See the 'Encoding' section of the help for file, the 'R Data Import/Export Manual' and 'Note'.
#' @param ... Further arguments to be passed to \code{\link[utils]{write.table}}.
#' 
#' @details 
#' The format of the CSV file is optimized for usage of \code{\link{readDatasets}}.
#' 
#' @seealso 
#' \itemize{
#'   \item \code{\link{writeDataset}} for writing a single dataset,
#'   \item \code{\link{readDatasets}} for reading multiple datasets, 
#'   \item \code{\link{readDataset}} for reading a single dataset.
#' }
#' 
#' @export
#'
writeDatasets <- function(datasets, file, ..., append = FALSE, quote = TRUE, sep = ",",
		eol = "\n", na = "NA", dec = ".", row.names = TRUE,
		col.names = NA, qmethod = "double",
		fileEncoding = "UTF-8") {
		
	if (!is.list(datasets)) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'datasets' must be a list of datasets")
	}
	
	if (length(datasets) == 0) {
		stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'datasets' is empty")
	}
	
	datasetType <- NA_character_
	dataFrames <- NULL
	for (i in 1:length(datasets)) {		
		dataset <- datasets[[i]]		
		.assertIsDataset(dataset)
		if (is.na(datasetType)) {
			datasetType <- class(dataset)
		}
		else if (class(dataset) != datasetType) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all datasets must have the same type")
		}
		
		data <- as.data.frame(dataset, niceColumnNamesEnabled = FALSE)
		datasetId <- ifelse(!is.null(dataset$getId()) && !is.na(dataset$getId()), dataset$getId(), i)
		data <- cbind(rep(datasetId, nrow(data)), data)
		colnames(data)[1] <- "datasetId"
		
		if (!is.null(dataset$getDescription()) && !is.na(dataset$getDescription())) {
			data <- cbind(data, rep(dataset$getDescription(), nrow(data)))
			colnames(data)[ncol(data)] <- "description"
		}
		
		if (is.null(dataFrames)) {
			dataFrames <- data						
		} else {
			dataFrames <- rbind(dataFrames, data)
		}
	}
	
	if (is.null(dataFrames)) {
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "failed to bind datasets")
	}
		
	utils::write.table(x = dataFrames, file = file, append = append, quote = quote, sep = sep,
		eol = eol, na = na, dec = dec, row.names = FALSE,
		col.names = TRUE, qmethod = qmethod,
		fileEncoding = fileEncoding)
}

#' @title
#' Get Dataset
#' 
#' @description  
#' Creates a dataset object and returns it.    
#' 
#' @param ... A \code{data.frame} or some data vectors defining the dataset. 
#' @param floatingPointNumbersEnabled If \code{TRUE}, 
#'        sample sizes can be specified as floating-point numbers 
#'        (in general this only make sense for simulation purposes); \cr
#'        by default \code{floatingPointNumbersEnabled = FALSE}, i.e., 
#'        samples sizes defined as floating-point numbers will be truncated.
#' 
#' @details
#' The different dataset types \code{DatasetMeans}, of \code{DatasetRates}, or  
#' \code{DatasetSurvival} can be created as follows:
#' \itemize{
#'   \item An element of \code{\link{DatasetMeans}} for one sample is created by \cr
#'     \code{getDataset(sampleSizes =, means =, stDevs =)} where \cr
#'     \code{sampleSizes}, \code{means}, \code{stDevs} are vectors with stagewise sample sizes, 
#'     means and standard deviations of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetMeans}} for two samples is created by \cr
#'     \code{getDataset(sampleSizes1 =, sampleSizes2 =, means1 =, means2 =, } \cr
#'     \code{stDevs1 =, stDevs2 =)} where 
#'     \code{sampleSizes1}, \code{sampleSizes2}, \code{means1}, \code{means2}, 
#'     \code{stDevs1}, \code{stDevs2} are vectors with 
#'     stagewise sample sizes, means and standard deviations for the two treatment groups 
#'     of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetRates}} for one sample is created by \cr
#'     \code{getDataset(sampleSizes =, events =)} where \code{sampleSizes}, \code{events} are vectors 
#'     with stagewise sample sizes and events of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetRates}} for two samples is created by \cr
#'     \code{getDataset(sampleSizes1 =, sampleSizes2 =, events1 =, events2 =)} where 
#'     \code{sampleSizes1}, \code{sampleSizes2}, \code{events1}, \code{events2} 
#'     are vectors with stagewise sample sizes 
#'     and events  for the two treatment groups of length given by the number of available stages.
#'   \item An element of \code{\link{DatasetSurvival}} is created by \cr
#'     \code{getDataset(events=, logRanks =, allocationRatios =)} where 
#'     \code{events}, \code{logRanks}, and \code{allocation ratios} are the stagewise events, 
#'     (one-sided) logrank statistics, and allocation ratios. 
#' }
#' Prefix \code{overall[Capital case of first letter of variable name]...} for the variable 
#' names enables entering the overall results and calculates stagewise statistics.
#' 
#' Note that in survival design usually the overall events and logrank test statistics are provided
#' in the output, so \cr
#' \code{getDataset(overallEvents=, overallLogRanks =, overallAllocationRatios =)} \cr
#' is the usual command for entering survival data. Note also that for \code{overallLogranks} also the
#' z scores from a Cox regression can be used.
#' 
#' \code{n} can be used in place of \code{samplesizes}.  
#'  
#' @return Returns a \code{\link{Dataset}} object.
#'  
#' @examples
#' 
#' # Create a Dataset of Means (one group):
#' 
#' datasetOfMeans <- getDataset(
#' 	   n = c(22, 11, 22, 11),
#' 	   means = c(1, 1.1, 1, 1),
#' 	   stDevs = c(1, 2, 2, 1.3)
#' )
#' datasetOfMeans
#' datasetOfMeans$show(showType = 2)
#' 
#' datasetOfMeans <- getDataset(
#' 	   overallSampleSizes = c(22, 33, 55, 66),
#' 	   overallMeans = c(1.000, 1.033, 1.020, 1.017 ),
#' 	   overallStDevs = c(1.00, 1.38, 1.64, 1.58)
#' )
#' datasetOfMeans
#' datasetOfMeans$show(showType = 2)
#' as.data.frame(datasetOfMeans)
#' 
#' # Create a Dataset of Means (two groups):
#' 
#' datasetOfMeans <- getDataset(
#' 	   n1 = c(22, 11, 22, 11),
#' 	   n2 = c(22, 13, 22, 13),
#' 	   means1 = c(1, 1.1, 1, 1),
#' 	   means2 = c(1.4, 1.5, 3, 2.5),
#' 	   stDevs1 = c(1, 2, 2, 1.3),
#' 	   stDevs2 = c(1, 2, 2, 1.3)
#' )
#' datasetOfMeans
#' 
#' datasetOfMeans <- getDataset(
#' 	   overallSampleSizes1 = c(22, 33, 55, 66),
#' 	   overallSampleSizes2 = c(22, 35, 57, 70),
#' 	   overallMeans1 = c(1, 1.033, 1.020, 1.017),
#' 	   overallMeans2 = c(1.4, 1.437, 2.040, 2.126),
#' 	   overallStDevs1 = c(1, 1.38, 1.64, 1.58),
#' 	   overallStDevs2 = c(1, 1.43, 1.82, 1.74)
#' )
#' datasetOfMeans
#' 
#' df <- data.frame(
#' 	   stages = 1:4,
#' 	   n1 = c(22, 11, 22, 11),
#' 	   n2 = c(22, 13, 22, 13),
#' 	   means1 = c(1, 1.1, 1, 1),
#' 	   means2 = c(1.4, 1.5, 3, 2.5),
#' 	   stDevs1 = c(1, 2, 2, 1.3),
#' 	   stDevs2 = c(1, 2, 2, 1.3)
#' )
#' datasetOfMeans <- getDataset(df)
#' datasetOfMeans
#' 
#' ## Create a Dataset of Rates (one group):
#' 
#' datasetOfRates <- getDataset(
#' 	   n = c(8, 10, 9, 11), 
#' 	   events = c(4, 5, 5, 6)
#' )
#' datasetOfRates
#' 
#' ## Create a Dataset of Rates (two groups):
#' 
#' datasetOfRates <- getDataset(
#' 	   n2 = c(8, 10, 9, 11),
#' 	   n1 = c(11, 13, 12, 13),
#' 	   events2 = c(3, 5, 5, 6),
#' 	   events1 = c(10, 10, 12, 12)
#' )
#' datasetOfRates
#' 
#' 
#' ## Create a Survival Dataset
#' 
#' dataset <- getDataset(
#'     overallEvents = c(8, 15, 19, 31),
#' 	   overallAllocationRatios = c(1, 1, 1, 2),
#' 	   overallLogRanks = c(1.52, 1.98, 1.99, 2.11)
#' )
#' dataset
#'
#' @export
#' 
getDataset <- function(..., floatingPointNumbersEnabled = FALSE) {
	
	args <- list(...)
	if (length(args) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
	}
	
	dataFrame <- .getDataFrameFromArgs(...)
	
	if (is.null(dataFrame)) {
		
		paramNames <- names(args)
		paramNames <- paramNames[paramNames != ""]
		if (length(paramNames) != length(args)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all parameters must be named")
		}
		
		if (length(paramNames) != length(unique(paramNames))) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the parameter names must be unique")
		}
		
		dataFrame <- .createDataFrame(...)
	}
	
	if (.isDataObjectMeans(...)) {
		return(DatasetMeans(dataFrame = dataFrame, 
			floatingPointNumbersEnabled = floatingPointNumbersEnabled))
	}
	
	if (.isDataObjectRates(...)) {
		return(DatasetRates(dataFrame = dataFrame,
			floatingPointNumbersEnabled = floatingPointNumbersEnabled))
	}
	
	if (.isDataObjectSurvival(...)) {
		return(DatasetSurvival(dataFrame = dataFrame,
			floatingPointNumbersEnabled = floatingPointNumbersEnabled))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "failed to identify dataset type")
}

getDataSet <- function(..., floatingPointNumbersEnabled = FALSE) {
	
	args <- list(...)
	if (length(args) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
	}
	
	dataFrame <- .getDataFrameFromArgs(...)
	
	if (is.null(dataFrame)) {
		
		paramNames <- names(args)
		paramNames <- paramNames[paramNames != ""]
		if (length(paramNames) != length(args)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all parameters must be named")
		}
		
		if (length(paramNames) != length(unique(paramNames))) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "the parameter names must be unique")
		}
		
		dataFrame <- .createDataFrame(...)
	}
	
	if (.isDataObjectMeans(...)) {
		return(DatasetMeans(dataFrame = dataFrame, 
						floatingPointNumbersEnabled = floatingPointNumbersEnabled))
	}
	
	if (.isDataObjectRates(...)) {
		return(DatasetRates(dataFrame = dataFrame,
						floatingPointNumbersEnabled = floatingPointNumbersEnabled))
	}
	
	if (.isDataObjectSurvival(...)) {
		return(DatasetSurvival(dataFrame = dataFrame,
						floatingPointNumbersEnabled = floatingPointNumbersEnabled))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "failed to identify dataset type")
}


.arraysAreEqual <- function(a1, a2) {
	if (length(a1) != length(a2)) {
		return(FALSE)
	}
	
	l <- length(a1)
	if (l > 0) {
		a1 <- sort(a1)
		a2 <- sort(a2)
		if (base::sum(a1 == a2) < l) {
			return(FALSE)
		}
	}
	
	return(TRUE)
}

any(grepl("3", c("name2", "name3")))

.createDataFrame <- function(...) {
	args <- list(...)
	argNames <- .getArgumentNames(...)
	if (length(args) == 0 || length(argNames) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
	}
	
	multiArmEnabled <- any(grep("3", argNames))
	numberOfValues <- length(args[[1]])
	naIndicesBefore <- NULL
	for (argName in argNames) {	
		argValues <- args[[argName]]
		if (length(argValues) != numberOfValues) {
			stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
				"all data vectors must have the same length: '", 
				argName, "' (", length(argValues), ") differs from '", 
				argNames[1], "' (", numberOfValues, ")")
		}
		
		if (.equalsRegexpIgnoreCase(argName, "^stages?$")) {
			if (length(stats::na.omit(argValues)) != length(argValues)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"NA's not allowed for '", argName, "'; stages must be defined completely")
			}
			
			definedStages <- sort(intersect(unique(argValues), 1:numberOfValues))
			if (length(definedStages) < numberOfValues) {
				if (length(definedStages) == 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "no valid stages are defined; ",
						"stages must be defined completely (", .arrayToString(1:numberOfValues), ")")
				}
				msg <- ifelse(length(definedStages) == 1,  
					paste0("only stage ", definedStages, " is defined"), 
					paste0("only stages ", .arrayToString(definedStages), " are defined"))
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, msg, "; stages must be defined completely")
			}
		}
		
		naIndices <- which(is.na(argValues))
		if (length(naIndices) > 0) {
			stageIndex <- naIndices[length(naIndices)]
			if (stageIndex != numberOfValues) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'", argName, "' contains a NA at stage ", stageIndex, 
					" followed by a value for a higher stage; NA's must be the last values")
			}
		}
		if (length(naIndices) > 1) {
			indexBefore <- naIndices[length(naIndices)]
			for (i in (length(naIndices) - 1):1) {
				index <- naIndices[i]
				if (indexBefore - index > 1) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"'", argName, "' contains alternating values and NA's; ",
						"NA's must be the last values")
				}
				indexBefore <- index
			}
		}
		if (!multiArmEnabled && !is.null(naIndicesBefore) && 
				!.equalsRegexpIgnoreCase(argName, "^stages?$")) {
			if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
				stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"if NA's exist, then they are mandatory for each data vector at the same position")
			}
		}
		naIndicesBefore <- naIndices
		
		if (base::sum(is.infinite(argValues)) > 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all data values must be finite; ",
				"'", argName, "' contains infinite values")
		}
		
		if (!is.numeric(argValues)) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all data vectors must be numeric ('", 
				argName, "' is ", class(argValues), ")")
		}
		
		if (length(argValues) > C_KMAX_UPPER_BOUND) {
			stop(C_EXCEPTION_TYPE_ARGUMENT_LENGTH_OUT_OF_BOUNDS, 
				"'", argName, "' is out of bounds [1, ", C_KMAX_UPPER_BOUND, "]")
		}
	}
	
	dataFrame <- as.data.frame(args)
	if (length(intersect(tolower(names(dataFrame)), c("stage", "stages"))) == 0) {
		dataFrame$stages <- 1:nrow(dataFrame)
	}
	return(dataFrame)
}

.getDataFrameFromArgs <- function(...) {
	args <- list(...)
	if (length(args) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
			"cannot initialize dataset because no data are defined")
	}
	
	dataFrame <- NULL
	dataFrameCounter <- 0
	for (arg in args) {	
		if (is.data.frame(arg)) {
			dataFrameCounter <- dataFrameCounter + 1
			if (is.null(dataFrame)) {
				dataFrame <- arg
			}
		}
	}
	
	if (dataFrameCounter > 1) {
		warning("Found ", dataFrameCounter, ", data.frame arguments; ", 
			"only the first data.frame will be used for the initialization of the dataset", call. = FALSE)
	}
	
	return(dataFrame)
}

.getArgumentNames <- function(...) {
	dataFrame <- .getDataFrameFromArgs(...)
	if (!is.null(dataFrame)) {
		return(names(dataFrame))
	} 
	
	args <- list(...)
	if (length(args) == 0) {
		return(character(0))
	}
	
	return(names(args))
}

.assertIsValidDatasetArgument <- function(...) {
	argNames <- .getArgumentNames(...)
	if (length(argNames) == 0) {
		return(TRUE)
	}
	
	argNamesLower <- tolower(argNames)
	dataObjectkeyWords <- tolower(C_KEY_WORDS)
	
	multiArmKeywords <- tolower(c(C_KEY_WORDS_EVENTS, 
		C_KEY_WORDS_OVERALL_EVENTS, 
		C_KEY_WORDS_SAMPLE_SIZES, 
		C_KEY_WORDS_MEANS, 
		C_KEY_WORDS_ST_DEVS, 
		C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 
		C_KEY_WORDS_OVERALL_MEANS, 
		C_KEY_WORDS_OVERALL_ST_DEVS))
	unknownArgs <- setdiff(argNamesLower, dataObjectkeyWords)
	unknownArgsChecked <- unknownArgs
	unknownArgs <- c()
	for (unknownArg in unknownArgsChecked) {
		unknown <- TRUE
		for (multiArmKeyword in multiArmKeywords) {
			if (grepl(paste0(multiArmKeyword, "\\d{1,4}"), unknownArg)) {
				unknown <- FALSE
			}
		}
		if (unknown) {
			unknownArgs <- c(unknownArgs, unknownArg)
		}
	}
	
	if (length(unknownArgs) > 0) {
		for (i in 1:length(unknownArgs)) {
			unknownArgs[i] <- argNames[argNamesLower == unknownArgs[i]][1]
		}
		if (length(unknownArgs) == 1) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"the argument '", unknownArgs, "' is not a valid dataset argument")
		} else {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"the arguments ", .arrayToString(unknownArgs, encapsulate = TRUE), 
				" are no valid dataset arguments")
		}
	}
	
	invisible(TRUE)
}

.isDataObject <- function(..., dataObjectkeyWords) {
	.assertIsValidDatasetArgument(...)
	
	argNames <- .getArgumentNames(...)
	if (length(argNames) == 0) {
		return(FALSE)
	}
		
	argNames <- tolower(argNames)
	matching <- intersect(argNames, tolower(dataObjectkeyWords))
	
	return(length(matching) > 0)
}

.isDataObjectMeans <- function(...) {
	return(.isDataObject(..., dataObjectkeyWords = 
		c(C_KEY_WORDS_MEANS, C_KEY_WORDS_ST_DEVS, 
			C_KEY_WORDS_MEANS_1, C_KEY_WORDS_ST_DEVS_1,
			C_KEY_WORDS_MEANS_2, C_KEY_WORDS_ST_DEVS_2,
			C_KEY_WORDS_OVERALL_MEANS, C_KEY_WORDS_OVERALL_ST_DEVS, 
			C_KEY_WORDS_OVERALL_MEANS_1, C_KEY_WORDS_OVERALL_ST_DEVS_1,
			C_KEY_WORDS_OVERALL_MEANS_2, C_KEY_WORDS_OVERALL_ST_DEVS_2)))
}

.isDataObjectRates <- function(...) {	
	return(.isDataObject(..., dataObjectkeyWords = 
		c(C_KEY_WORDS_EVENTS, C_KEY_WORDS_EVENTS_1, C_KEY_WORDS_EVENTS_2, 
			C_KEY_WORDS_OVERALL_EVENTS, C_KEY_WORDS_OVERALL_EVENTS_1, C_KEY_WORDS_OVERALL_EVENTS_2)) &&
		!.isDataObject(..., dataObjectkeyWords = 
				c(C_KEY_WORDS_OVERALL_LOG_RANKS, C_KEY_WORDS_LOG_RANKS, 
					C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, C_KEY_WORDS_ALLOCATION_RATIOS)))
}

.isDataObjectSurvival <- function(...) {
	return(.isDataObject(..., dataObjectkeyWords = 
		c(C_KEY_WORDS_OVERALL_LOG_RANKS, C_KEY_WORDS_LOG_RANKS,
			C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, C_KEY_WORDS_ALLOCATION_RATIOS)))
}

#' 
#' @name Dataset
#' 
#' @title
#' Dataset
#' 
#' @description 
#' Basic class for datasets.
#' 
#' @field stages The stage numbers.
#' @field groups The group numbers.
#' 
#' @details 
#' \code{Dataset} is the basic class for 
#' \itemize{
#'   \item \code{\link{DatasetMeans}}, 
#'   \item \code{\link{DatasetRates}}, and 
#'   \item \code{\link{DatasetSurvival}}.
#' }
#' This basic class contains the fields \code{stages} and \code{groups} and several commonly used
#' functions.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#' 
Dataset <- setRefClass("Dataset",
	contains = "ParameterSet",
	fields = list(
		.data = "data.frame",
		.plotSettings = "PlotSettings",
		.id = "integer",
		.description = "character",
		.floatingPointNumbersEnabled = "logical",
		stages = "integer",
		groups = "integer"
	),
	methods = list(
		initialize = function(dataFrame, ..., floatingPointNumbersEnabled = FALSE) {
			callSuper(...)
			.plotSettings <<- PlotSettings()
			.parameterNames <<- .getParameterNames()
			.parameterFormatFunctions <<- C_PARAMETER_FORMAT_FUNCTIONS
			.floatingPointNumbersEnabled <<- floatingPointNumbersEnabled
			
			.id <<- NA_integer_
			.description <<- NA_character_
			
			if (!missing(dataFrame)) {
				.initByDataFrame(dataFrame)
			}
		},
		
		getPlotSettings = function() {
			return(.plotSettings)
		},

		show = function(showType = 1, digits = NA_integer_) {
			'Method for automatically printing dataset objects'	
			.show(showType = showType, digits = digits, consoleOutputEnabled = TRUE)
		},
		
		.show = function(showType = 1, digits = NA_integer_, consoleOutputEnabled = TRUE) {
			.resetCat()
			if (showType == 2) {
				.cat("Technical summary of the dataset object of class",
					methods::classLabel(class(.self)), ":\n", heading = 1, 
					consoleOutputEnabled = consoleOutputEnabled)
				.showAllParameters(consoleOutputEnabled = consoleOutputEnabled)
				.showParameterTypeDescription(consoleOutputEnabled = consoleOutputEnabled)
			} else {
				.showParametersOfOneGroup(.getUserDefinedParameters(), 
					title = .toString(startWithUpperCase = TRUE), orderByParameterName = FALSE,
					consoleOutputEnabled = consoleOutputEnabled)
				
				.showParametersOfOneGroup(.getGeneratedParameters(), 
					title = "Calculated data", orderByParameterName = FALSE,
					consoleOutputEnabled = consoleOutputEnabled)
				
				.showUnknownParameters(consoleOutputEnabled = consoleOutputEnabled)
				
				if (!is.na(.description) && nchar(.description) > 0) {
					.cat("Description: ", .description, "\n\n", 
						consoleOutputEnabled = consoleOutputEnabled)
				}
			}
		},
		
		.initByDataFrame = function(dataFrame) {
			if (!is.data.frame(dataFrame)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'dataFrame' must be a data.frame (is an instance of class ", class(dataFrame), ")")
			}
			
			if (!.paramExists(dataFrame, "stage") && !.paramExists(dataFrame, "stages")) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"'dataFrame' must contain parameter 'stages' or 'stage'")
			}
			
			stages <<- as.integer(.getValuesByParameterName(dataFrame, c("stages", "stage")))
			if (length(unique(stages)) < length(stages)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stages' (", .arrayToString(stages), 
					") must be a unique vector of stage numbers")
			}
			groups <<- rep(1L, length(stages))
			
			.setParameterType("groups", C_PARAM_USER_DEFINED)
			.setParameterType("stages", C_PARAM_USER_DEFINED)
		},
		
		.validateDataset = function() {
			.assertIsValidKMax(kMax = getNumberOfStages())
		},
		
		.validateValues = function(values, name) {
			l1 <- length(unique(stages))
			l2 <- length(values)
			if (l1 != l2) {
				stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
					"there ", ifelse(l1 == 1, paste("is", l1, "stage"), 
						paste("are", l1, "stages")), " defined",
					" (", .arrayToString(unique(stages)), ") and '", name, "' has length ", l2)
			}
		},
		
		.fillWithNAs = function(kMax) {
			numberOfStages <- getNumberOfStages()
			if (numberOfStages >= kMax) {
				return(invisible())
			}
			
			numberOfGroups <- getNumberOfGroups()
			for (s in (numberOfStages + 1):kMax) {
				for (g in 1:numberOfGroups) {
					stages <<- c(stages, s)
					groups <<- c(groups, g)
				}
			}
		},
		
		.orderDataByStageAndGroup = function() {
			.data <<- .data[order(.data[, 1], .data[, 2]), ]
		},
		
		.getNumberOfNAsToAdd = function(kMax) {
			n <- kMax - getNumberOfStages()
			if (n <= 0) {
				return(0)
			}
			
			n <- n * getNumberOfGroups()
			return(n)
		},
		
		.paramExists = function(dataFrame, parameterName) {
			for (p in parameterName) {
				value <- dataFrame[[p]]
				if (!is.null(value)) {
					return(TRUE)
				}
			}
			return(FALSE)
		},
		
		.getValuesByParameterName = function(dataFrame, parameterNameVariants, ...,
				defaultValues = NULL, suffix = "") {
			for (parameterName in parameterNameVariants) {
				key <- paste0(parameterName, suffix)
				if (.paramExists(dataFrame, key)) {
					return(dataFrame[[key]])
				}
			}
			
			if (!is.null(defaultValues)) {
				return(defaultValues)
			}
			
			stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "parameter '", 
				paste0(parameterNameVariants[1], suffix), "' is missing or not correctly specified")
		},
		
		.getIndices = function(stage, group) {
			if (is.null(.data)) {
				stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'.data' must be defined")
			}
			
			if (!is.null(stage) && !any(is.na(stage)) && all(stage < 0)) {
				i <- 1:getNumberOfStages()
				stage <- i[!(i %in% abs(stage))]
			}
			
			if (!is.null(group) && !any(is.na(group)) && all(group < 0)) {
				i <- 1:getNumberOfGroups()
				group <- i[!(i %in% abs(group))]
			}
			
			if (!is.null(group) && length(group) == 1 && is.na(group)) {
				if (!all(stage %in% .data$stage)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stage' (", .arrayToString(stage), 
						") out of range [", .arrayToString(sort(unique(.data$stage))), "]")
				}
				
				indices <- (.data$stage %in% stage)
				indices[is.na(indices)] <- FALSE
				return(indices)
			}
			
			if (!is.null(stage) && length(stage) == 1 && is.na(stage)) {
				if (!all(group %in% .data$group)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'group' (", .arrayToString(group), 
						") out of range [", .arrayToString(sort(unique(.data$group))), "]")
				}
				
				indices <- (.data$group %in% group)
				indices[is.na(indices)] <- FALSE
				return(indices)
			}

			if (!all(stage %in% .data$stage)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'stage' (", .arrayToString(stage), 
					") out of range [", .arrayToString(sort(unique(.data$stage))), "]")
			}
			
			if (!all(group %in% .data$group)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'group' (", .arrayToString(group), 
					") out of range [", .arrayToString(sort(unique(.data$group))), "]")
			}
			
			if (length(stage) > 1) {
				indices <- (.data$stage %in% stage & .data$group %in% group)
				indices[is.na(indices)] <- FALSE
				return(indices)
			}
			
			indices <- (.data$stage %in% stage & .data$group %in% group)
			indices[is.na(indices)] <- FALSE
			return(indices)
		},
		
		.getValidatedFloatingPointNumbers = function(n, type = "Sample sizes") {
			
			if (.floatingPointNumbersEnabled) {
				return(n)
			}
			
			nToCheck <- stats::na.omit(n)
			if (any(nToCheck != as.integer(nToCheck))) {
				warning(type, " specified as floating-point numbers were truncated", call. = FALSE)
			}
			
			n[!is.na(n)] <- as.integer(n[!is.na(n)])
			return(n)
		},
		
		.keyWordExists = function(dataFrame, keyWords, suffix = "") {
			for (key in keyWords) {
				if (.paramExists(dataFrame, paste0(key, suffix))) {
					return(TRUE)
				}
			}
			return(FALSE)
		},
		
		.getNumberOfGroups = function(dataFrame, keyWords) {
			for (group in 3:1000) {
				if (!.keyWordExists(dataFrame, keyWords, group)) {
					return(group - 1)
				}
			}
			return(2)
		},
		
		.getValidatedStage = function(..., stage = NA_integer_, group = NA_integer_) {
			if (length(list(...)) > 0) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"argument 'stage' or 'group' is missing, all arguments must be named")
			}
			if (all(is.na(stage))) {
				stage <- c(1:getNumberOfStages())
			}
			return(stage)
		},
		
		getNumberOfGroups = function() {
			data <- stats::na.omit(.data)
			return(length(unique(data$group)))
		},
		
		getNumberOfStages = function() {
			data <- stats::na.omit(.data)
			return(length(unique(data$stage)))
		},
		
		isDatasetMeans = function() {
			return(class(.self) == "DatasetMeans")
		},
		
		isDatasetRates = function() {
			return(class(.self) == "DatasetRates")
		},
		
		isDatasetSurvival = function() {
			return(class(.self) == "DatasetSurvival")
		},
		
		setId = function(id) {
			.id <<- as.integer(id)
		},
		
		getId = function() {
			return(.id)
		},
		
		setDescription = function(description) {
			.description <<- description
		},
		
		getDescription = function() {
			return(.description)
		},
		
		.toString = function(startWithUpperCase = FALSE) {
			s <- "unknown dataset"
			if (isDatasetMeans()) {
				s <- "dataset of means"
			}
			else if (isDatasetRates()) {
				s <- "dataset of rates"
			}
			else if (isDatasetSurvival()) {
				s <- "dataset of survival data"
			}
			return(ifelse(startWithUpperCase, .firstCharacterToUpperCase(s), s))
		}
	)
)

#' 
#' @name DatasetMeans
#' 
#' @title
#' Dataset of Means
#' 
#' @description 
#' Class for a dataset of means.
#' 
#' @field groups The group numbers.
#' @field stages The stage numbers.
#' @field sampleSizes The sample sizes.
#' @field means The means.
#' @field stDevs The standard deviations.
#' 
#' @details 
#' This object can not be created directly; better use \code{\link{getDataset}} 
#' with suitable arguments to create a dataset of means.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
DatasetMeans <- setRefClass("DatasetMeans",
	contains = "Dataset",
	fields = list(
		sampleSizes = "numeric",
		means = "numeric",
		stDevs = "numeric",
		overallSampleSizes = "numeric",
		overallMeans = "numeric",
		overallStDevs = "numeric"
	),
	methods = list(
		
		getSampleSize = function(stage, group = 1) {
			return(.data$sampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getMean = function(stage, group = 1) {
			return(.data$mean[.getIndices(stage = stage, group = group)])
		},
		
		getStDev = function(stage, group = 1) {
			return(.data$stDev[.getIndices(stage = stage, group = group)])
		},
		
		getSampleSizes = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$sampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getMeans = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$mean[.getIndices(stage = stage, group = group)])
		},
		
		getStDevs = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$stDev[.getIndices(stage = stage, group = group)])
		},
		
		getSampleSizesUpTo = function(to, group = 1) {
			return(.data$sampleSize[.getIndices(stage = c(1:to), group = group)])
		},
		
		getMeansUpTo = function(to, group = 1) {
			return(.data$mean[.getIndices(stage = c(1:to), group = group)])
		},
		
		getStDevsUpTo = function(to, group = 1) {
			return(.data$stDev[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallSampleSize = function(stage, group = 1) {
			return(.data$overallSampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getOverallMean = function(stage, group = 1) {
			return(.data$overallMean[.getIndices(stage = stage, group = group)])
		},
		
		getOverallStDev = function(stage, group = 1) {
			return(.data$overallStDev[.getIndices(stage = stage, group = group)])
		},
		
		getOverallSampleSizes = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallSampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getOverallMeans = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallMean[.getIndices(stage = stage, group = group)])
		},
		
		getOverallStDevs = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallStDev[.getIndices(stage = stage, group = group)])
		},
		
		getOverallSampleSizesUpTo = function(to, group = 1) {
			return(.data$overallSampleSize[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallMeansUpTo = function(to, group = 1) {
			return(.data$overallMean[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallStDevsUpTo = function(to, group = 1) {
			return(.data$overallStDev[.getIndices(stage = c(1:to), group = group)])
		},
		
		.getValidatedSampleSizes = function(n) {
			return(.getValidatedFloatingPointNumbers(n, type = "Sample sizes"))
		},
		
		.initByDataFrame = function(dataFrame) {
			callSuper(dataFrame)	

			# case: one mean - stage wise
			if (.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
				sampleSizes <<- .getValidatedSampleSizes(.getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_SAMPLE_SIZES))
				.validateValues(sampleSizes, "n")
				means <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_MEANS)
				.validateValues(means, "means")
				stDevs <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_ST_DEVS)
				.validateValues(stDevs, "stDevs")
								
				kMax <- length(sampleSizes)
				stageNumber <- length(stats::na.omit(sampleSizes))
				dataInput <- data.frame(
					sampleSizes = sampleSizes,
					means = means,
					stDevs = stDevs)
				dataInput <- .getOverallData(dataInput, kMax, stage = stageNumber)
				overallSampleSizes <<- .getValidatedSampleSizes(dataInput$overallSampleSizes)
				overallMeans <<- dataInput$overallMeans
				overallStDevs <<- dataInput$overallStDevs
				
				.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("means", C_PARAM_USER_DEFINED)
				.setParameterType("stDevs", C_PARAM_USER_DEFINED)
				
				.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
				.setParameterType("overallMeans", C_PARAM_GENERATED)
				.setParameterType("overallStDevs", C_PARAM_GENERATED)
			}
			
			# case: one mean - overall
			else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)) {
				overallSampleSizes <<- .getValidatedSampleSizes(.getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_SAMPLE_SIZES))
				.validateValues(overallSampleSizes, "overallSampleSizes")
				overallMeans <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_MEANS)
				.validateValues(overallMeans, "overallMeans")
				overallStDevs <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_ST_DEVS)
				.validateValues(overallStDevs, "overallStDevs")
				
				kMax <- length(overallSampleSizes)
				stageNumber <- length(stats::na.omit(overallSampleSizes))
				dataInput <- data.frame(
					overallSampleSizes = overallSampleSizes,
					overallMeans = overallMeans,
					overallStDevs = overallStDevs)
				dataInput <- .getStageWiseData(dataInput, kMax, stage = stageNumber)
				sampleSizes <<- .getValidatedSampleSizes(dataInput$sampleSizes)
				means <<- dataInput$means
				stDevs <<- dataInput$stDevs
				
				.setParameterType("sampleSizes", C_PARAM_GENERATED)
				.setParameterType("means", C_PARAM_GENERATED)
				.setParameterType("stDevs", C_PARAM_GENERATED)
				
				.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("overallMeans", C_PARAM_USER_DEFINED)
				.setParameterType("overallStDevs", C_PARAM_USER_DEFINED)
			}
			
			# case: two or more means - stage wise
			else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 1)) && 
					.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 2))) {
					
				numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)
				
				stages <<- rep(stages, numberOfTreatmentGroups)
				
				groups <<- integer(0)
				sampleSizes <<- numeric(0)
				means <<- numeric(0)
				stDevs <<- numeric(0)
				overallSampleSizes <<- numeric(0)
				overallMeans <<- numeric(0)
				overallStDevs <<- numeric(0)
				for (group in 1:numberOfTreatmentGroups) {
					sampleSizesTemp <- .getValidatedSampleSizes(.getValuesByParameterName(
							dataFrame, C_KEY_WORDS_SAMPLE_SIZES, suffix = group))
					.validateValues(sampleSizesTemp, paste0("n", group))
					sampleSizes <<- c(sampleSizes, sampleSizesTemp)
					
					meansTemp <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_MEANS, suffix = group)
					.validateValues(meansTemp, paste0("means", group))
					means <<- c(means, meansTemp)
					
					stDevsTemp <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_ST_DEVS, suffix = group)
					.validateValues(stDevsTemp, paste0("stDevs", group))
					stDevs <<- c(stDevs, stDevsTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(sampleSizesTemp)))
					
					kMax <- length(sampleSizesTemp)
					numberOfValidStages <- length(stats::na.omit(sampleSizesTemp))
					overallData <- .getOverallData(data.frame(
							sampleSizes = sampleSizesTemp,
							means = meansTemp,
							stDevs = stDevsTemp), kMax, stage = numberOfValidStages)
					
					overallSampleSizes <<- c(overallSampleSizes, 
						.getValidatedSampleSizes(overallData$overallSampleSizes))
					overallMeans <<- c(overallMeans, overallData$overallMeans)
					overallStDevs <<- c(overallStDevs, overallData$overallStDevs)
				}
				if (base::sum(stats::na.omit(sampleSizes) < 0) > 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
				}
				if (base::sum(stats::na.omit(stDevs) < 0) > 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all standard deviations must be >= 0")
				}
			
				.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("means", C_PARAM_USER_DEFINED)
				.setParameterType("stDevs", C_PARAM_USER_DEFINED)
				
				.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
				.setParameterType("overallMeans", C_PARAM_GENERATED)
				.setParameterType("overallStDevs", C_PARAM_GENERATED)
			}
					
			# case: two or more means - overall
			else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 1)) &&
					.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 2))) {
					
				numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)
				
				stages <<- rep(stages, numberOfTreatmentGroups)
				
				groups <<- integer(0)
				sampleSizes <<- numeric(0)
				means <<- numeric(0)
				stDevs <<- numeric(0)
				overallSampleSizes <<- numeric(0)
				overallMeans <<- numeric(0)
				overallStDevs <<- numeric(0)
				for (group in 1:numberOfTreatmentGroups) {
					overallSampleSizesTemp <- .getValidatedSampleSizes(.getValuesByParameterName(
							dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES, suffix = group))
					.validateValues(overallSampleSizesTemp, paste0("overallSampleSizes", group))
					overallSampleSizes <<- c(overallSampleSizes, overallSampleSizesTemp)
					
					overallMeansTemp <- .getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_MEANS, suffix = group)
					.validateValues(overallMeansTemp, paste0("overallMeans", group))
					overallMeans <<- c(overallMeans, overallMeansTemp)
					
					overallStDevsTemp <- .getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_ST_DEVS, suffix = group)
					.validateValues(overallStDevsTemp, paste0("overallStDevs", group))
					overallStDevs <<- c(overallStDevs, overallStDevsTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(overallSampleSizesTemp)))
					
					kMax <- length(overallSampleSizesTemp)
					numberOfValidStages <- length(stats::na.omit(overallSampleSizesTemp))
					overallData <- .getStageWiseData(data.frame(
							overallSampleSizes = overallSampleSizesTemp,
							overallMeans = overallMeansTemp,
							overallStDevs = overallStDevsTemp), kMax, stage = numberOfValidStages)
					
					validatedSampleSizes <- .getValidatedSampleSizes(overallData$sampleSizes)
					.validateValues(validatedSampleSizes, paste0("n", group))
					sampleSizes <<- c(sampleSizes, validatedSampleSizes)
					means <<- c(means, overallData$means)
					stDevs <<- c(stDevs, overallData$stDevs)
					
					if (base::sum(stats::na.omit(sampleSizes) < 0) > 0) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
					}
					if (base::sum(stats::na.omit(stDevs) < 0) > 0) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all standard deviations must be >= 0")
					}
				}
				
				.setParameterType("sampleSizes", C_PARAM_GENERATED)
				.setParameterType("means", C_PARAM_GENERATED)
				.setParameterType("stDevs", C_PARAM_GENERATED)
				
				.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("overallMeans", C_PARAM_USER_DEFINED)
				.setParameterType("overallStDevs", C_PARAM_USER_DEFINED)
			}
			
			else {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"sample sizes are missing or not correctly specified")
			}
			
			.data <<- data.frame(stage = stages, group = groups, sampleSize = sampleSizes, 
				mean = means, stDev = stDevs, overallSampleSize = overallSampleSizes, 
				overallMean = overallMeans, overallStDev = overallStDevs)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
			
			.validateDataset()
		},
		
		.setDataToVariables = function() {
			stages <<- .data$stage
			groups <<- .data$group
			sampleSizes <<- .data$sampleSize
			means <<- .data$mean
			stDevs <<- .data$stDev
			overallSampleSizes <<- .data$overallSampleSize
			overallMeans <<- .data$overallMean
			overallStDevs <<- .data$overallStDev
		},
		
		.fillWithNAs = function(kMax) {
			callSuper(kMax)
			n <- .getNumberOfNAsToAdd(kMax)
			
			naRealsToAdd <- rep(NA_real_, n)
			
			sampleSizes <<- c(sampleSizes, naRealsToAdd)
			means <<- c(means, naRealsToAdd)
			stDevs <<- c(stDevs, naRealsToAdd)
			
			overallSampleSizes <<- c(overallSampleSizes, naRealsToAdd)
			overallMeans <<- c(overallMeans, naRealsToAdd)
			overallStDevs <<- c(overallStDevs, naRealsToAdd)
			
			.data <<- data.frame(stage = stages, group = groups, sampleSize = sampleSizes, 
				mean = means, stDev = stDevs, overallSampleSize = overallSampleSizes, 
				overallMean = overallMeans, overallStDev = overallStDevs)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
		},
		
		getRandomData = function() {
			data <- NULL
			for (stage in 1:getNumberOfStages()) {
				for (group in 1:getNumberOfGroups()) {
					randomData <- stats::rnorm(n = getSampleSize(stage = stage, group = group), 
						mean = getMean(stage = stage, group = group), 
						sd = getStDev(stage = stage, group = group))
					row <- data.frame(
						stage = stage,
						group = group, 
						randomData = randomData
					)
					if (is.null(data)) {
						data <- row
					} else {
						data <- rbind(data, row)
					}
				}
			}
			data$stage <- factor(data$stage)
			data$group <- factor(data$group, label=paste("Group", c(1:getNumberOfGroups())))
			return(data)
		},
		
		.getOverallData = function(dataInput, kMax, stage) {
			"Calculates overall means and standard deviation if stagewise data is available"
			if (is.null(dataInput[["sampleSizes"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'sampleSizes'")
			}
			if (is.null(dataInput[["means"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'means'")
			}
			if (is.null(dataInput[["stDevs"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'stDevs'")
			}
			
			dataInput$overallSampleSizes <- c(base::cumsum(dataInput$sampleSizes[1:stage]), 
				rep(NA_real_, kMax - stage))
			
			dataInput$overallMeans <- c(base::cumsum(dataInput$sampleSizes[1:stage] * 
				dataInput$means[1:stage]) /
				base::cumsum(dataInput$sampleSizes[1:stage]), rep(NA_real_, kMax - stage))
			
			dataInput$overallStDevs <- rep(NA_real_, kMax)
			for (k in 1:stage) {
				dataInput$overallStDevs[k] <- 
					base::sqrt((base::sum((dataInput$sampleSizes[1:k] - 1) * dataInput$stDevs[1:k]^2) + 
					base::sum(dataInput$sampleSizes[1:k] * 
					(dataInput$means[1:k] - dataInput$overallMeans[k])^2)) /
					(base::sum(dataInput$sampleSizes[1:k]) - 1))
			}	
			return(dataInput)
		},
		
		.getStageWiseData = function(dataInput, kMax, stage) {
			"Calculates stagewise means and standard deviation if overall data is available"
			if (is.null(dataInput[["overallSampleSizes"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'overallSampleSizes'")
			}
			if (is.null(dataInput[["overallMeans"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'overallMeans'")
			}
			if (is.null(dataInput[["overallStDevs"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'overallStDevs'")
			}
			
			dataInput$sampleSizes <- c(dataInput$overallSampleSizes[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				dataInput$sampleSizes[2:stage] <- dataInput$overallSampleSizes[2:stage] - 
					dataInput$overallSampleSizes[1:(stage - 1)]
			}	
			
			dataInput$means <- c(dataInput$overallMeans[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1){
				for (k in 2:stage) {
					dataInput$means[k] <- (dataInput$overallSampleSizes[k] * dataInput$overallMeans[k] -
							dataInput$overallSampleSizes[k - 1] * dataInput$overallMeans[k - 1])/	
						dataInput$sampleSizes[k]
				}
			}
			
			dataInput$stDevs <- c(dataInput$overallStDevs[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1){
				for (k in 2:stage) {
					dataInput$stDevs[k] <- 
						base::sqrt(((dataInput$overallSampleSizes[k] - 1) * dataInput$overallStDevs[k]^2 - 
						(dataInput$overallSampleSizes[k - 1] - 1) * dataInput$overallStDevs[k - 1]^2 + 
						base::sum(dataInput$sampleSizes[1:(k - 1)] * 
						(dataInput$means[1:(k - 1)] - dataInput$overallMeans[k - 1])^2) -
						base::sum(dataInput$sampleSizes[1:k] * 
						(dataInput$means[1:k] - dataInput$overallMeans[k])^2)) /
						(dataInput$sampleSizes[k] - 1))
				}
			}	
			
			return(dataInput)
		}
	)
)

#'
#' @title
#' Dataset Plotting
#' 
#' @param x The \code{\link{Dataset}} object to plot.
#' @param y Not available for this kind of plot (is only defined to be compatible 
#'        to the generic plot function).
#' @param main The main title, default is \code{"Dataset"}.
#' @param xlab The x-axis label, default is \code{"Stage"}.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title, default is \code{"Group"}.
#' @param palette The palette, default is \code{"Set1"}.
#' @param showSource If \code{TRUE}, the parameter names of the object will 
#'        be printed which were used to create the plot; that may be, e.g., 
#'        useful to check the values or to create own plots with \code{\link[graphics]{plot}}.
#' @param ... Optional \code{ggplot2} arguments.
#' 
#' @description
#' Plots a dataset.
#' 
#' @details
#' Generic function to plot all kinds of datasets.
#' 
#' @examples 
#' 
#' # Plot a dataset of means
#' dataExample <- getDataset(
#'     n1 = c(22, 11, 22, 11),
#'     n2 = c(22, 13, 22, 13),
#'     means1 = c(1, 1.1, 1, 1),
#'     means2 = c(1.4, 1.5, 3, 2.5),
#'     stDevs1 = c(1, 2, 2, 1.3),
#'     stDevs2 = c(1, 2, 2, 1.3))
#' 
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of means")
#' 
#' # Plot a dataset of rates
#' dataExample <- getDataset(
#'     n1 = c(8, 10, 9, 11),
#'     n2 = c(11, 13, 12, 13),
#'     events1 = c(3, 5, 5, 6),
#'     events2 = c(8, 10, 12, 12)
#' )
#' 
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of rates")
#' 
#' @export
#'
plot.Dataset <- function(x, y, ..., main = "Dataset", xlab = "Stage", ylab = NA_character_,
		legendTitle = "Group", palette = "Set1", showSource = FALSE) {
		
	.assertGgplotIsInstalled()
	
	if (x$isDatasetMeans()) {
		data <- x$getRandomData()
		if (is.na(ylab)) {
			ylab <- "Random data"
		}
	} 
	else if (x$isDatasetRates()) {
		data <- x$.data
		if (is.na(ylab)) {
			ylab <- "Frequency (Events and Sample Size)"
		}
	}
	else if (x$isDatasetSurvival()) {
		# Open work: implement dataset plot of survival data
		stop("Plot of survival data is not implemented yet")
	}
	
	if (showSource) {
		warning("'showSource' = TRUE is not implemented yet for class ", class(x))
	}
	
	if (x$getNumberOfGroups() == 1) {	
		if (x$isDatasetMeans()) {
			p <- ggplot2::ggplot(ggplot2::aes(y = data$randomData, x = factor(data$stage)), data = data) 
			p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = data$stage))
			p <- p + ggplot2::geom_point(colour = "#0e414e", shape = 20, 
				position = ggplot2::position_jitter(width = .1), 
				size = x$getPlotSettings()$pointSize) 
			p <- p + ggplot2::stat_summary(fun.y = "mean", geom = "point", 
				shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white", 
				colour = "black", show.legend = FALSE)
		} 
		
		else if (x$isDatasetRates()) {
			p <- ggplot2::ggplot(show.legend = FALSE) 
			
			# plot sample size
			p <- p + ggplot2::geom_bar(ggplot2::aes(y = data$sampleSize, 
				x = factor(data$stage), fill = factor(data$stage)),
				data = data, position = "dodge", stat = "identity", alpha = 0.4)
			
			# plot events
			p <- p + ggplot2::geom_bar(ggplot2::aes(y = data$event, x = factor(data$stage), 	
				fill = factor(data$stage)), data = data, 
				position = "dodge", stat = "identity") 
		}
		
		else if (x$isDatasetSurvival()) {
			# implement survival plot here
		}
	} else {
		data$stageGroup <- interaction(data$stage, data$group)	
			
		if (x$isDatasetMeans()) {
			p <- ggplot2::ggplot(ggplot2::aes(y = data$randomData, x = factor(data$stage), 
				fill = factor(data$group)), data = data) 
			p <- p + ggplot2::geom_point(ggplot2::aes(colour = data$group), shape = 20, 
				position = ggplot2::position_dodge(.75), 
				size = x$getPlotSettings()$pointSize)
			p <- p + ggplot2::geom_boxplot()
			p <- p + ggplot2::stat_summary(ggplot2::aes(colour = data$group), 
				fun.y = "mean", geom = "point", 
				shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white", 
				show.legend = FALSE)
		} 
		
		else if (x$isDatasetRates()) {
			p <- ggplot2::ggplot(show.legend = FALSE) 
		
			# plot sample size
			p <- p + ggplot2::geom_bar(ggplot2::aes(y = data$sampleSize, 
				x = factor(data$stage), fill = factor(data$group)),
				data = data, position = "dodge", stat = "identity", alpha = 0.4)
			
			# plot events
			p <- p + ggplot2::geom_bar(ggplot2::aes(y = data$event, x = factor(data$stage), 
				fill = factor(data$group)), data = data, 
				position = "dodge", stat = "identity") 
		}
		
		else if (x$isDatasetSurvival()) {
			# implement survival plot here
		}
	}

	# hide second legend
	if (x$getNumberOfGroups() == 1) {
		p <- p + ggplot2::guides(fill = FALSE, colour = FALSE)
	} else {
		p <- p + ggplot2::guides(colour = FALSE)
	}

	# set theme
	p <- x$getPlotSettings()$setTheme(p)
	#p <- designSet$getPlotSettings()$hideGridLines(p)
	
	# set main title
	p <- x$getPlotSettings()$setMainTitle(p, main)

	# set axes labels
	p <- x$getPlotSettings()$setAxesLabels(p, xlab = xlab, ylab = ylab)
	
	# set legend
	if (x$getNumberOfGroups() > 1) {
		p <- x$getPlotSettings()$setLegendPosition(p, legendPosition = C_POSITION_OUTSIDE_PLOT)
		p <- x$getPlotSettings()$setLegendBorder(p)
		p <- x$getPlotSettings()$setLegendTitle(p, legendTitle, mode = "fill")
		p <- x$getPlotSettings()$setLegendLabelSize(p)
	}
	
	p <- x$getPlotSettings()$setAxesAppearance(p)
	p <- x$getPlotSettings()$setColorPalette(p, palette, mode = "all")
	p <- x$getPlotSettings()$enlargeAxisTicks(p)

	companyAnnotationEnabled <- .getOptionalArgument("companyAnnotationEnabled", ...)
	if (is.null(companyAnnotationEnabled) || !is.logical(companyAnnotationEnabled)) {
		companyAnnotationEnabled <- FALSE
	}
	p <- x$getPlotSettings()$addCompanyAnnotation(p, enabled = companyAnnotationEnabled)
	
	p
}

#' 
#' @name DatasetRates
#' 
#' @title
#' Dataset of Rates
#' 
#' @description 
#' Class for a dataset of rates.
#' 
#' @field group The group numbers.
#' @field stage The stage numbers.
#' @field sampleSize The sample sizes.
#' @field event The events.
#' 
#' @details 
#' This object can not be created directly; better use \code{\link{getDataset}} 
#' with suitable arguments to create a dataset of rates.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
DatasetRates <- setRefClass("DatasetRates",
	contains = "Dataset",
	fields = list(
		sampleSizes = "numeric",
		events = "numeric",
		overallSampleSizes = "numeric",
		overallEvents = "numeric"
	),
	methods = list(

		getSampleSize = function(stage, group = 1) {
			return(.data$sampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getSampleSizes = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$sampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getSampleSizesUpTo = function(to, group = 1) {
			return(.data$sampleSize[.getIndices(stage = c(1:to), group = group)])
		},
		
		getEvent = function(stage, group = 1) {
			return(.data$event[.getIndices(stage = stage, group = group)])
		},
		
		getEvents = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$event[.getIndices(stage = stage, group = group)])
		},
		
		getEventsUpTo = function(to, group = 1) {
			return(.data$event[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallSampleSize = function(stage, group = 1) {
			return(.data$overallSampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getOverallSampleSizes = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallSampleSize[.getIndices(stage = stage, group = group)])
		},
		
		getOverallSampleSizesUpTo = function(to, group = 1) {
			return(.data$overallSampleSize[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallEvent = function(stage, group = 1) {
			return(.data$overallEvent[.getIndices(stage = stage, group = group)])
		},
		
		getOverallEvents = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallEvent[.getIndices(stage = stage, group = group)])
		},
		
		getOverallEventsUpTo = function(to, group = 1) {
			return(.data$overallEvent[.getIndices(stage = c(1:to), group = group)])
		},
		
		.getValidatedSampleSizes = function(n) {
			return(.getValidatedFloatingPointNumbers(n, type = "Sample sizes"))
		},
		
		.initByDataFrame = function(dataFrame) {
			callSuper(dataFrame)	
			
			# case: one rate - stage wise
			if (.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
				sampleSizes <<- .getValidatedSampleSizes(
					.getValuesByParameterName(dataFrame, C_KEY_WORDS_SAMPLE_SIZES))
				.validateValues(sampleSizes, "n")
				events <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS)
				.validateValues(events, "events")
				
				kMax <- length(sampleSizes)
				stageNumber <- length(stats::na.omit(sampleSizes))
				dataInput <- data.frame(
					sampleSizes = sampleSizes,
					events = events)
				dataInput <- .getOverallData(dataInput, kMax, stage = stageNumber)
				overallSampleSizes <<- .getValidatedSampleSizes(dataInput$overallSampleSizes)
				overallEvents <<- dataInput$overallEvents
				
				.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("events", C_PARAM_USER_DEFINED)
				
				.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
				.setParameterType("overallEvents", C_PARAM_GENERATED)
			}
			
			# case: one rate - overall
			else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)) {
				overallSampleSizes <<- .getValidatedSampleSizes(.getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_SAMPLE_SIZES))
				.validateValues(overallSampleSizes, "overallSampleSizes")
				overallEvents <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS)
				.validateValues(overallEvents, "overallEvents")
				
				kMax <- length(overallSampleSizes)
				stageNumber <- length(stats::na.omit(overallSampleSizes))
				dataInput <- data.frame(
					overallSampleSizes = overallSampleSizes,
					overallEvents = overallEvents)
				dataInput <- .getStageWiseData(dataInput, kMax, stage = stageNumber)
				sampleSizes <<- .getValidatedSampleSizes(dataInput$sampleSizes)
				events <<- dataInput$events
				
				.setParameterType("sampleSizes", C_PARAM_GENERATED)
				.setParameterType("events", C_PARAM_GENERATED)
				
				.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
			}
			
			# case: two or more rates - stage wise
			else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 1)) && 
				.paramExists(dataFrame, paste0(C_KEY_WORDS_SAMPLE_SIZES, 2))) {
				
				numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)
				
				stages <<- rep(stages, numberOfTreatmentGroups)
				
				groups <<- integer(0)
				sampleSizes <<- numeric(0)
				events <<- numeric(0)
				overallSampleSizes <<- numeric(0)
				overallEvents <<- numeric(0)
				for (group in 1:numberOfTreatmentGroups) {
					sampleSizesTemp <- .getValidatedSampleSizes(.getValuesByParameterName(
							dataFrame, C_KEY_WORDS_SAMPLE_SIZES, suffix = group))
					.validateValues(sampleSizesTemp, paste0("n", group))
					sampleSizes <<- c(sampleSizes, sampleSizesTemp)
					
					eventsTemp <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS, suffix = group)
					.validateValues(eventsTemp, paste0("events", group))
					events <<- c(events, eventsTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(sampleSizesTemp)))
					
					kMax <- length(sampleSizesTemp)
					numberOfValidStages <- length(stats::na.omit(sampleSizesTemp))
					overallData <- .getOverallData(data.frame(
							sampleSizes = sampleSizesTemp,
							events = eventsTemp), kMax, stage = numberOfValidStages)
					
					overallSampleSizes <<- c(overallSampleSizes, 
						.getValidatedSampleSizes(overallData$overallSampleSizes))
					overallEvents <<- c(overallEvents, overallData$overallEvents)
				}
				if (base::sum(stats::na.omit(sampleSizes) < 0) > 0) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
				}
				
				.setParameterType("sampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("events", C_PARAM_USER_DEFINED)
				
				.setParameterType("overallSampleSizes", C_PARAM_GENERATED)
				.setParameterType("overallEvents", C_PARAM_GENERATED)
			}
			
#			# case: two rates - overall
#			else if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES_1) &&
#				.paramExists(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES_2)) {
#				
#				stages <<- c(stages, stages)
#				
#				overallSampleSizes1 <- .getValidatedSampleSizes(.getValuesByParameterName(dataFrame, 
#						C_KEY_WORDS_OVERALL_SAMPLE_SIZES_1))
#				.validateValues(overallSampleSizes1, "overallSampleSizes1")
#				overallSampleSizes2 <- .getValidatedSampleSizes(.getValuesByParameterName(dataFrame, 
#						C_KEY_WORDS_OVERALL_SAMPLE_SIZES_2))
#				.validateValues(overallSampleSizes2, "overallSampleSizes2")
#				overallSampleSizes <<- c(overallSampleSizes1, overallSampleSizes2)
#				
#				overallEvents1 <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS_1)
#				.validateValues(overallEvents1, "overallEvents1")
#				overallEvents2 <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS_2)
#				.validateValues(overallEvents2, "overallEvents2")
#				overallEvents <<- c(overallEvents1, overallEvents2)
#				
#				kMax <- length(overallSampleSizes1)
#				stageNumber <- length(stats::na.omit(overallSampleSizes1))
#				
#				dataInput1 <- data.frame(
#					overallSampleSizes = overallSampleSizes1,
#					overallEvents = overallEvents1)
#				dataInput1 <- .getStageWiseData(dataInput1, kMax, stage = stageNumber)
#				
#				dataInput2 <- data.frame(
#					overallSampleSizes = overallSampleSizes2,
#					overallEvents = overallEvents2)
#				dataInput2 <- .getStageWiseData(dataInput2, kMax, stage = stageNumber)
#				
#				n1 <- .getValidatedSampleSizes(dataInput1$sampleSizes)
#				n2 <- .getValidatedSampleSizes(dataInput2$sampleSizes)
#				.validateValues(n1, "n1")
#				.validateValues(n2, "n2")
#				sampleSizes <<- c(n1, n2)
#				if (base::sum(stats::na.omit(sampleSizes) < 0) > 0) {
#					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
#				}
#				
#				events1 <- dataInput1$events
#				events2 <- dataInput2$events
#				.validateValues(events1, "events1")
#				.validateValues(events2, "events2")
#				events <<- c(events1, events2)
#				
#				groups <<- c(rep(1L, length(n1)), rep(2L, length(n1)))
#				
#				.setParameterType("sampleSizes", C_PARAM_GENERATED)
#				.setParameterType("events", C_PARAM_GENERATED)
#				
#				.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
#				.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
#			}
			
			# case: two or more rates - overall
			else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 1)) &&
				.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 2))) {
				
				numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES)
				
				stages <<- rep(stages, numberOfTreatmentGroups)
				
				groups <<- integer(0)
				sampleSizes <<- numeric(0)
				events <<- numeric(0)
				overallSampleSizes <<- numeric(0)
				overallEvents <<- numeric(0)
				for (group in 1:numberOfTreatmentGroups) {
					overallSampleSizesTemp <- .getValidatedSampleSizes(.getValuesByParameterName(
							dataFrame, C_KEY_WORDS_OVERALL_SAMPLE_SIZES, suffix = group))
					.validateValues(overallSampleSizesTemp, paste0("overallSampleSizes", group))
					overallSampleSizes <<- c(overallSampleSizes, overallSampleSizesTemp)
					
					overallEventsTemp <- .getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_EVENTS, suffix = group)
					.validateValues(overallEventsTemp, paste0("overallEvents", group))
					overallEvents <<- c(overallEvents, overallEventsTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(overallSampleSizesTemp)))
					
					kMax <- length(overallSampleSizesTemp)
					numberOfValidStages <- length(stats::na.omit(overallSampleSizesTemp))
					overallData <- .getStageWiseData(data.frame(
							overallSampleSizes = overallSampleSizesTemp,
							overallEvents = overallEventsTemp), kMax, stage = numberOfValidStages)
					
					validatedSampleSizes <- .getValidatedSampleSizes(overallData$sampleSizes)
					.validateValues(validatedSampleSizes, paste0("n", group))
					sampleSizes <<- c(sampleSizes, validatedSampleSizes)
					events <<- c(events, overallData$events)
					
					if (base::sum(stats::na.omit(sampleSizes) < 0) > 0) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all sample sizes must be >= 0")
					}
				}
				
				.setParameterType("sampleSizes", C_PARAM_GENERATED)
				.setParameterType("events", C_PARAM_GENERATED)
				
				.setParameterType("overallSampleSizes", C_PARAM_USER_DEFINED)
				.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
			}
			
			else {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"sample sizes are missing or not correctly specified")
			}
			
			if (base::sum(stats::na.omit(events) < 0) > 0) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0")
			}
			
			.data <<- data.frame(stage = stages, group = groups, 
				sampleSize = sampleSizes, event = events,
				overallSampleSize = overallSampleSizes, overallEvent = overallEvents)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
			
			.validateDataset()
		},
		
		.setDataToVariables = function() {
			stages <<- .data$stage
			groups <<- .data$group
			sampleSizes <<- .data$sampleSize
			events <<- .data$event
			overallSampleSizes <<- .data$overallSampleSize
			overallEvents <<- .data$overallEvent
		},
		
		.fillWithNAs = function(kMax) {
			callSuper(kMax)
			n <- .getNumberOfNAsToAdd(kMax)
			
			sampleSizes <<- c(sampleSizes, rep(NA_real_, n))
			events <<- c(events, rep(NA_real_, n))
			
			overallSampleSizes <<- c(overallSampleSizes, rep(NA_real_, n))
			overallEvents <<- c(overallEvents, rep(NA_real_, n))
			
			.data <<- data.frame(stage = stages, group = groups, 
				sampleSize = sampleSizes, event = events,
				overallSampleSize = overallSampleSizes, overallEvent = overallEvents)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
		},
		
		getRandomData = function() {
			data <- NULL
			for (stage in 1:getNumberOfStages()) {
				for (group in 1:getNumberOfGroups()) {
					
					n = getSampleSize(stage = stage, group = group)
					numberOfEvents <- getEvent(stage = stage, group = group)
					randomIndizes <- sample(x = c(1:n), size = numberOfEvents, replace = FALSE)
					randomData <- rep(0, n)
					randomData[randomIndizes] <- 1
					
					row <- data.frame(
						stage = stage,
						group = group, 
						randomData = randomData
					)
					if (is.null(data)) {
						data <- row
					} else {
						data <- rbind(data, row)
					}
				}
			}
			data$stage <- factor(data$stage)		
			data$group <- factor(data$group, label=paste("Group", c(1:getNumberOfGroups())))
			return(data)
		},
		
		.getOverallData = function(dataInput, kMax, stage) {
			"Calculates overall values if stagewise data is available"
			if (is.null(dataInput[["sampleSizes"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'sampleSizes'")
			}
			if (is.null(dataInput[["events"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'events'")
			}
			
			dataInput$overallSampleSizes <- c(base::cumsum(dataInput$sampleSizes[1:stage]), 
				rep(NA_real_, kMax - stage))
			
			dataInput$overallEvents <- c(base::cumsum(dataInput$events[1:stage]), 
				rep(NA_real_, kMax - stage))
				
			return(dataInput)
		},
		
		.getStageWiseData = function(dataInput, kMax, stage) {
			"Calculates stagewise values if overall data is available"
			if (is.null(dataInput[["overallSampleSizes"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'overallSampleSizes'")
			}
			if (is.null(dataInput[["overallEvents"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'overallEvents'")
			}
			
			dataInput$sampleSizes <- c(dataInput$overallSampleSizes[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				dataInput$sampleSizes[2:stage] <- dataInput$overallSampleSizes[2:stage] - 
					dataInput$overallSampleSizes[1:(stage - 1)]
			}	
			
			dataInput$events <- c(dataInput$overallEvents[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				dataInput$events[2:stage] <- dataInput$overallEvents[2:stage] - 
					dataInput$overallEvents[1:(stage - 1)]
			}	
			
			return(dataInput)
		}
	)
)

#' 
#' @name DatasetSurvival
#' 
#' @title
#' Dataset of Survival Data
#' 
#' @description 
#' Class for a dataset of survival data.
#' 
#' @field group The group numbers.
#' @field stage The stage numbers.
#' @field overallEvent The overall events.
#' @field overallAllocationRatio The overall allocations ratios.
#' @field overallLogRank The overall logrank test statistics.
#' 
#' @details 
#' This object can not be created directly; better use \code{\link{getDataset}} 
#' with suitable arguments to create a dataset of survival data.
#' 
#' @include class_core_parameter_set.R
#' @include class_core_plot_settings.R
#' @include f_core_constants.R
#' 
#' @keywords internal
#' 
#' @importFrom methods new
#'
DatasetSurvival <- setRefClass("DatasetSurvival",
	contains = "Dataset",
	fields = list(
		overallEvents = "numeric",
		overallAllocationRatios = "numeric",
		overallLogRanks = "numeric",
		events = "numeric",
		allocationRatios = "numeric",
		logRanks = "numeric"
	),
	methods = list(
		
		getEvent = function(stage, group = 1) {
			return(.data$event[.getIndices(stage = stage, group = group)])
		},
		
		getAllocationRatio = function(stage, group = 1) {
			return(.data$allocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getLogRank = function(stage, group = 1) {
			return(.data$logRank[.getIndices(stage = stage, group = group)])
		},
		
		getEventsUpTo = function(to, group = 1) {
			return(.data$event[.getIndices(stage = c(1:to), group = group)])
		},
		
		getAllocationRatiosUpTo = function(to, group = 1) {
			return(.data$allocationRatio[.getIndices(stage = c(1:to), group = group)])
		},
		
		getLogRanksUpTo = function(to, group = 1) {
			return(.data$logRank[.getIndices(stage = c(1:to), group = group)])
		},
		
		getEvents = function(stage = NA_integer_, group = 1) {
			return(.data$event[.getIndices(stage = stage, group = group)])
		},
		
		getAllocationRatios = function(stage = NA_integer_, group = 1) {
			return(.data$allocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getLogRanks = function(stage = NA_integer_, group = 1) {
			return(.data$logRank[.getIndices(stage = stage, group = group)])
		},

		getOverallEvent = function(stage, group = 1) {
			return(.data$overallEvent[.getIndices(stage = stage, group = group)])
		},
		
		getOverallAllocationRatio = function(stage, group = 1) {
			return(.data$overallAllocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getOverallLogRank = function(stage, group = 1) {
			return(.data$overallLogRank[.getIndices(stage = stage, group = group)])
		},
		
		getOverallEventsUpTo = function(to, group = 1) {
			return(.data$overallEvent[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallAllocationRatiosUpTo = function(to, group = 1) {
			return(.data$overallAllocationRatio[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallLogRanksUpTo = function(to, group = 1) {
			return(.data$overallLogRank[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallEvents = function(stage = NA_integer_, group = 1) {
			return(.data$overallEvent[.getIndices(stage = stage, group = group)])
		},
		
		getOverallAllocationRatios = function(stage = NA_integer_, group = 1) {
			return(.data$overallAllocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getOverallLogRanks = function(stage = NA_integer_, group = 1) {
			return(.data$overallLogRank[.getIndices(stage = stage, group = group)])
		},
		
		.getValidatedEvents = function(n) {
			return(.getValidatedFloatingPointNumbers(n, type = "Events"))
		},
		
		.initByDataFrame = function(dataFrame) {
			callSuper(dataFrame)	
						
			# case: survival - overall
			if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)) {
				
				overallEvents <<- .getValidatedEvents(
					.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS))
				.validateValues(overallEvents, "overallEvents")
				overallAllocationRatios <<- .getValuesByParameterName(
					dataFrame, parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
					defaultValues = rep(1, length(stages)))
				.validateValues(overallAllocationRatios, "overallAllocationRatios")
				overallLogRanks <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)
				.validateValues(overallLogRanks, "overallLogRanks")
				
				kMax <- length(overallEvents)
				stageNumber <- length(stats::na.omit(overallEvents))
				dataInput <- data.frame(
					overallEvents = overallEvents,
					overallAllocationRatios = overallAllocationRatios,
					overallLogRanks = overallLogRanks)
				dataInput <- .getStageWiseData(dataInput, kMax, stage = stageNumber)
				events <<- .getValidatedEvents(dataInput$events)
				allocationRatios <<- dataInput$allocationRatios
				logRanks <<- dataInput$logRanks
				
				.setParameterType("events", C_PARAM_GENERATED)
				.setParameterType("allocationRatios", C_PARAM_GENERATED)
				.setParameterType("logRanks", C_PARAM_GENERATED)
				
				.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
				.setParameterType("overallAllocationRatios", C_PARAM_USER_DEFINED)
				.setParameterType("overallLogRanks", C_PARAM_USER_DEFINED)
				
				.setParameterType("groups", C_PARAM_NOT_APPLICABLE)
			}
			
			# case: survival - stage wise
			else if (.paramExists(dataFrame, C_KEY_WORDS_LOG_RANKS)) {
				
				events <<- .getValidatedEvents(.getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS))
				.validateValues(events, "events")
				allocationRatios <<- .getValuesByParameterName(
					dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
					defaultValues = rep(1, length(stages)))
				.validateValues(allocationRatios, "allocationRatios")
				logRanks <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_LOG_RANKS)
				.validateValues(logRanks, "logRanks")
				
				kMax <- length(events)
				stageNumber <- length(stats::na.omit(events))
				dataInput <- data.frame(
					events = events,
					allocationRatios = allocationRatios,
					logRanks = logRanks)
				dataInput <- .getOverallData(dataInput, kMax, stage = stageNumber)
				overallEvents <<- .getValidatedEvents(dataInput$overallEvents)
				overallAllocationRatios <<- dataInput$overallAllocationRatios
				overallLogRanks <<- dataInput$overallLogRanks
				
				.setParameterType("events", C_PARAM_USER_DEFINED)
				.setParameterType("allocationRatios", C_PARAM_USER_DEFINED)
				.setParameterType("logRanks", C_PARAM_USER_DEFINED)
				
				.setParameterType("overallEvents", C_PARAM_GENERATED)
				.setParameterType("overallAllocationRatios", C_PARAM_GENERATED)
				.setParameterType("overallLogRanks", C_PARAM_GENERATED)
				
				.setParameterType("groups", C_PARAM_NOT_APPLICABLE)
			}
			
			.data <<- data.frame(stage = stages, group = groups, 
				overallEvent = overallEvents, 
				overallAllocationRatio = overallAllocationRatios, 
				overallLogRank = overallLogRanks, 
				event = events,
				allocationRatio = allocationRatios,
				overallLogRank = overallLogRanks)
			
			.orderDataByStageAndGroup()	
			.setDataToVariables()
			
			.validateDataset()
		},
		
		.setDataToVariables = function() {
			stages <<- .data$stage
			groups <<- .data$group
			overallEvents <<- .data$overallEvent
			overallAllocationRatios <<- .data$overallAllocationRatio
			overallLogRanks <<- .data$overallLogRank
			events <<- .data$event
			allocationRatios <<- .data$allocationRatio
			overallLogRanks <<- .data$overallLogRank
		},
		
		.fillWithNAs = function(kMax) {
			callSuper(kMax)
			n <- .getNumberOfNAsToAdd(kMax)
			
			overallEvents <<- c(overallEvents, rep(NA_real_, n))
			overallAllocationRatios <<- c(overallAllocationRatios, rep(NA_real_, n))
			overallLogRanks <<- c(overallLogRanks, rep(NA_real_, n))
			
			events <<- c(events, rep(NA_real_, n))
			allocationRatios <<- c(allocationRatios, rep(NA_real_, n))
			logRanks <<- c(logRanks, rep(NA_real_, n))
			
			.data <<- data.frame(stage = stages, group = groups, 
				overallEvent = overallEvents, 
				overallAllocationRatio = overallAllocationRatios, 
				overallLogRank = overallLogRanks,
				event = events,
				allocationRatio = allocationRatios,
				overallLogRank = overallLogRanks)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
		},
		
		getRandomData = function() {
			stop("The function 'DatasetSurvival.getRandomData()' is not implemented yet") 
		},
		
		.getOverallData = function(dataInput, kMax, stage) {
			"Calculates overall logrank statistics, events, and allocation ratios if stagewise data is available"
			if (is.null(dataInput[["events"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'events'")
			}
			if (is.null(dataInput[["logRanks"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data input must contain variable 'logRanks'")
			}
			if (is.null(dataInput[["allocationRatios"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'allocationRatios'")
			}
			
			dataInput$overallEvents <- c(base::cumsum(dataInput$events[1:stage]), 
				rep(NA_real_, kMax - stage))
			
			dataInput$overallLogRanks <- c(dataInput$logRanks[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				for (k in 2:stage){
					dataInput$overallLogRanks[k] <- 
						(base::sqrt(dataInput$events[k]) * dataInput$logRanks[k] + 
						base::sqrt(dataInput$overallEvents[k - 1]) * 
						dataInput$overallLogRanks[k - 1]) / base::sqrt(dataInput$overallEvents[k])  
				}		
			}
						
			dataInput$overallAllocationRatios <- c(dataInput$allocationRatios[1:stage], 
				rep(NA_real_, kMax - stage))
			if (stage > 1) {
			for (k in 2:stage){
					dataInput$overallAllocationRatios[k] <- (dataInput$events[k] * 
						dataInput$allocationRatios[k] + dataInput$overallEvents[k - 1] * 
						dataInput$overallAllocationRatios[k - 1]) / dataInput$overallEvents[k]  
				}
			}
			
			return(dataInput)
		},
		
		.getStageWiseData = function(dataInput, kMax, stage) {
			"Calculates stagewise logrank statistics, events, and allocation ratios if overall data is available"
			if (is.null(dataInput[["overallEvents"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'overallEvents'")
			}
			if (is.null(dataInput[["overallLogRanks"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'overallLogRanks'")
			}
			if (is.null(dataInput[["overallAllocationRatios"]])) {
				stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, 
					"data input must contain variable 'overallAllocationRatios'")
			}	
			
			dataInput$events <- c(dataInput$overallEvents[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				dataInput$events[2:stage] <- dataInput$overallEvents[2:stage] - 
					dataInput$overallEvents[1:(stage - 1)]
			}	
			
			dataInput$logRanks <- c(dataInput$overallLogRanks[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				dataInput$logRanks[2:stage] <- (base::sqrt(dataInput$overallEvents[2:stage]) * 
					dataInput$overallLogRanks[2:stage] - 
					base::sqrt(dataInput$overallEvents[1:(stage - 1)]) * 
					dataInput$overallLogRanks[1:(stage - 1)]) /
					base::sqrt(dataInput$overallEvents[2:stage] - dataInput$overallEvents[1:(stage - 1)])
			}	
			
			dataInput$allocationRatios <- c(dataInput$overallAllocationRatios[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
				dataInput$allocationRatios[2:stage] <- (dataInput$overallAllocationRatios[2:stage] - 
					dataInput$overallAllocationRatios[1:(stage - 1)] * 
					dataInput$overallEvents[1:(stage - 1)] / 
					dataInput$overallEvents[2:stage]) /	(dataInput$events[2:stage] / 
					dataInput$overallEvents[2:stage])
			}
			if (any(stats::na.omit(dataInput$allocationRatios) <= 0)) {
				stop("Overall allocation ratios not correctly specified")
			}	
			
			return(dataInput)
		}
	)
)

