#:#
#:#  *Dataset classes*
#:# 
#:#  This file is part of the R package rpact: 
#:#  Confirmatory Adaptive Clinical Trial Design and Analysis
#:# 
#:#  Author: Gernot Wassmer, PhD, and Friedrich Pahlke, PhD
#:#  Licensed under "GNU Lesser General Public License" version 3
#:#  License text can be found here: https://www.r-project.org/Licenses/LGPL-3
#:# 
#:#  RPACT company website: https://www.rpact.com
#:#  rpact package website: https://www.rpact.org
#:# 
#:#  Contact us for information about our services: info@rpact.com
#:# 
#:#  File version: $Revision: 3977 $
#:#  Last changed: $Date: 2020-11-20 12:21:54 +0100 (Fr, 20 Nov 2020) $
#:#  Last changed by: $Author: pahlke $
#:# 

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
#' @template return_object_dataset
#' 
#' @seealso 
#' \itemize{
#'   \item \code{\link{readDatasets}} for reading multiple datasets,
#'   \item \code{\link{writeDataset}} for writing a single dataset, 
#'   \item \code{\link{writeDatasets}} for writing multiple datasets.
#' }
#' 
#' @examples 
#' dataFileRates <- system.file("extdata", 
#'     "dataset_rates.csv", package = "rpact")
#' if (dataFileRates != "") {
#'     datasetRates <- readDataset(dataFileRates)
#'     datasetRates
#' }
#' 
#' dataFileMeansMultiArm <- system.file("extdata", 
#'     "dataset_means_multi-arm.csv", package = "rpact")
#' if (dataFileMeansMultiArm != "") {
#'     datasetMeansMultiArm <- readDataset(dataFileMeansMultiArm)
#'     datasetMeansMultiArm
#' }
#' 
#' dataFileRatesMultiArm <- system.file("extdata", 
#'     "dataset_rates_multi-arm.csv", package = "rpact")
#' if (dataFileRatesMultiArm != "") {
#'     datasetRatesMultiArm <- readDataset(dataFileRatesMultiArm)
#'     datasetRatesMultiArm
#' }
#' 
#' dataFileSurvivalMultiArm <- system.file("extdata", 
#'     "dataset_survival_multi-arm.csv", package = "rpact")
#' if (dataFileSurvivalMultiArm != "") {
#'     datasetSurvivalMultiArm <- readDataset(dataFileSurvivalMultiArm)
#'     datasetSurvivalMultiArm
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
#' @examples 
#' \dontrun{
#' datasetOfRates <- getDataset(
#'     n1 = c(11, 13, 12, 13),
#'     n2 = c(8, 10, 9, 11),
#'     events1 = c(10, 10, 12, 12),
#'     events2 = c(3, 5, 5, 6)
#' )
#' writeDataset(datasetOfRates, "dataset_rates.csv")
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
#' @return Returns a \code{\link[base]{list}} of \code{\link{Dataset}} objects.
#' 
#' @seealso 
#' \itemize{
#'   \item \code{\link{readDataset}} for reading a single dataset,
#'   \item \code{\link{writeDatasets}} for writing multiple datasets, 
#'   \item \code{\link{writeDataset}} for writing a single dataset.
#' }
#' 
#' @examples 
#' dataFile <- system.file("extdata", "datasets_rates.csv", package = "rpact")
#' if (dataFile != "") {
#'     datasets <- readDatasets(dataFile)
#'     datasets
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
#' @examples 
#' \dontrun{
#' d1 <- getDataset(
#'     n1 = c(11, 13, 12, 13),
#'     n2 = c(8, 10, 9, 11),
#'     events1 = c(10, 10, 12, 12),
#'     events2 = c(3, 5, 5, 6)
#' )
#' d2 <- getDataset(
#'     n1 = c(9, 13, 12, 13),
#'     n2 = c(6, 10, 9, 11),
#'     events1 = c(10, 10, 12, 12),
#'     events2 = c(4, 5, 5, 6)
#' )
#' datasets <- list(d1, d2)
#' writeDatasets(datasets, "datasets_rates.csv")
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
#'        (this make sense, e.g., for theoretical comparisons); \cr
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
#'     \code{getDataset(events =, logRanks =, allocationRatios =)} where 
#'     \code{events}, \code{logRanks}, and \code{allocation ratios} are the stagewise events, 
#'     (one-sided) logrank statistics, and allocation ratios. 
#' 	  \item An element of \code{\link{DatasetMeans}}, \code{\link{DatasetRates}}, and \code{\link{DatasetSurvival}} 
#' 	  for more than one comparison is created by adding subsequent digits to the variable names. 
#' 	  The system can analyze these data in a multi-arm many-to-one comparison setting where the
#' 	  group with the highest index represents the control group.		  		
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
#' For multi-arm designs the index refers to the considered comparison. For example,\cr
#' \code{
#' 	getDataset(events1=c(13, 33), logRanks1 = c(1.23, 1.55), events2 = c(16, NA), logRanks2 = c(1.55, NA))
#' } \cr
#' refers to the case where one active arm (1) is considered at both stages whereas active arm 2  
#' was dropped at interim. Number of events and logrank statistics are entered for the corresponding
#' comparison to control (see Examples). 
#' 
#' \code{n} can be used in place of \code{samplesizes}.  
#'  
#' @template return_object_dataset
#'  
#' @template examples_get_dataset
#' 
#' @export
#' 
getDataset <- function(..., floatingPointNumbersEnabled = FALSE) {
	
	args <- list(...)
	if (length(args) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
	}
	
	exampleType <- args[["example"]]
	if (!is.null(exampleType) && exampleType %in% c("means", "rates", "survival")) {
		return(.getDatasetExample(exampleType = exampleType))
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

.getDatasetExample <- function(exampleType) {
	if (exampleType == "means") {
		return(getDataset(
				n1 = c(13, 25), 
				n2 = c(15, NA), 
				n3 = c(14, 27), 
				n4 = c(12, 29), 
				means1 = c(24.2, 22.2), 
				means2 = c(18.8, NA),
				means3 = c(26.7, 27.7), 
				means4 = c(9.2, 12.2), 
				stDevs1 = c(24.4, 22.1), 
				stDevs2 = c(21.2, NA), 
				stDevs3 = c(25.6, 23.2), 
				stDevs4 = c(21.5, 22.7)))
	}
	else if (exampleType == "rates") {
		return(getDataset(
				n1 = c(23, 25),
				n2 = c(25, NA),	
				n3 = c(24, 27),	
				n4 = c(22, 29), 
				events1 = c(15, 12), 
				events2 = c(19, NA), 
				events3 = c(18, 22), 
				events4 = c(12, 13)))
	}
	else if (exampleType == "survival") {
		return(getDataset(
				events1   = c(25, 32), 
				events2   = c(18, NA),
				events3   = c(22, 36), 
				logRanks1 = c(2.2,1.8),	
				logRanks2 = c(1.99, NA), 
				logRanks3 = c(2.32, 2.11)))
	}
	
	stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "'exampleType' (", exampleType, ") is not allowed")
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

.getNumberOfGroupsFromArgumentNames <- function(argNames) {
	numbers <- gsub("\\D", "", argNames)
	numbers <- numbers[numbers != ""]
	return(ifelse(length(numbers) == 0, 1, max(as.numeric(numbers))))
}

.getGroupNumberFromArgumentName <- function(argName) {
	n <- gsub("\\D", "", argName)
	return(ifelse(n == "", 1, as.numeric(n)))
}

.isControlGroupArgument <- function(argName, numberOfGroups) {
	if (numberOfGroups <= 2) {
		return(FALSE)
	}
	
	return(ifelse(numberOfGroups == 1, FALSE, .getGroupNumberFromArgumentName(argName) == numberOfGroups))
}

#.isControlGroupArgument("n", 1)
#.isControlGroupArgument("n1", 2)
#.isControlGroupArgument("n2", 2)

.naOmitBackward <- function(x) {
	indices <- which(is.na(x))
	if (length(indices) == 0) {
		return(x)
	}
	
	if (length(x) == 1 || !is.na(x[length(x)])) {
		return(x)
	}
	
	if (length(indices) == 1) {
		return(x[1:(length(x) - 1)])
	}
	
	indexBefore <- NA_real_
	for (i in length(indices):1) {
		index <- indices[i]
		if (!is.na(indexBefore) && index != indexBefore - 1) {
			return(x[1:(indexBefore - 1)])
		}
		indexBefore <- index
	}
	if (!is.na(indexBefore)) {
		return(x[1:(indexBefore - 1)])
	}
	return(x)
}

.getNumberOfStagesFromArguments <- function(args, argNames) {
	numberOfStages <- 1
	for (argName in argNames) {	
		argValues <- args[[argName]]
		n <- length(.naOmitBackward(argValues))
		if (n > numberOfStages) {
			numberOfStages <- n
		}
	}
	return(numberOfStages)
}

.assertIsValidTreatmentArmArgumentDefined <- function(args, argNames, numberOfGroups, numberOfStages) {
	tratmentArgNames <- argNames[!grepl(paste0(".*\\D{1}", numberOfGroups, "$"), argNames)]
	for (argName in tratmentArgNames) {
		argValues <- args[[argName]]
		if (!is.null(argValues) && length(.naOmitBackward(argValues)) == numberOfStages) {
			return(invisible())
		}
	}
	stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
		"at least for one treatment arm the values for ", numberOfStages, " stages must be defined ",
		"because the control arm defines ", numberOfStages, " stages")
}

.createDataFrame <- function(...) {
	args <- list(...)
	argNames <- .getArgumentNames(...)
	if (length(args) == 0 || length(argNames) == 0) {
		stop(C_EXCEPTION_TYPE_MISSING_ARGUMENT, "data.frame or data vectors expected")
	}
	
	multiArmEnabled <- any(grep("3", argNames))
	numberOfGroups <- .getNumberOfGroupsFromArgumentNames(argNames)
	numberOfStages <- .getNumberOfStagesFromArguments(args, argNames)
	survivalDataEnabled <- .isDataObjectSurvival(...)
	if (multiArmEnabled) {
		.assertIsValidTreatmentArmArgumentDefined(args, argNames, numberOfGroups, numberOfStages)
	}
	
	numberOfValues <- length(args[[1]])
	naIndicesBefore <- NULL
	if (!survivalDataEnabled && multiArmEnabled) {
		naIndicesBefore <- list()
	}
	for (argName in argNames) {	
		argValues <- args[[argName]]
		if (is.null(argValues) || length(argValues) == 0) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'", argName, "' is not a valid numeric vector")
		}
		
		if (is.na(argValues[1])) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"'", argName, "' is NA at first stage; a valid numeric value must be specified at stage 1")
		}
		
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
		
		if (!survivalDataEnabled && .isControlGroupArgument(argName, numberOfGroups) &&
			length(na.omit(argValues)) < numberOfStages) {
			stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
				"control group '", argName, "' (", .arrayToString(argValues, digits = 2), ") must be defined for all stages")
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
		
		if (!survivalDataEnabled) {
			if (!multiArmEnabled) {
				if (!is.null(naIndicesBefore) && !.equalsRegexpIgnoreCase(argName, "^stages?$")) {
					if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
						stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
							"inconsistent NA definition; ",
							"if NA's exist, then they are mandatory for each group at the same stage")
					}
				}
				naIndicesBefore <- naIndices
			} else {
				groupNumber <- .getGroupNumberFromArgumentName(argName)
				if (!is.null(naIndicesBefore[[as.character(groupNumber)]]) && 
					!.equalsRegexpIgnoreCase(argName, "^stages?$") &&
					!.isControlGroupArgument(argName, numberOfGroups)) {
					if (!.arraysAreEqual(naIndicesBefore[[as.character(groupNumber)]], naIndices)) {
						stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
							"values of treatment ", groupNumber, " not correctly specified; ",
							"if NA's exist, then they are mandatory for each parameter at the same stage")
					}
				}
				if (!.isControlGroupArgument(argName, numberOfGroups)) {
					naIndicesBefore[[as.character(groupNumber)]] <- naIndices
				}
			}
		}
		
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
	
	for (groupNumber in 1:numberOfGroups) {
		groupVars <- argNames[grepl(paste0("\\D", groupNumber, "$"), argNames)]
		naIndicesBefore <- NULL
		for (argName in groupVars) {	
			argValues <- args[[argName]]
			naIndices <- which(is.na(argValues))
			if (!is.null(naIndicesBefore) && !.equalsRegexpIgnoreCase(argName, "^stages?$")) {
				if (!.arraysAreEqual(naIndicesBefore, naIndices)) {
					stop(C_EXCEPTION_TYPE_CONFLICTING_ARGUMENTS, 
						"inconsistent NA definition for group ", groupNumber, "; ",
						"if NA's exist, then they are mandatory for each group at the same stage")
				}
			}
			naIndicesBefore <- naIndices
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
	
	multiArmKeywords <- tolower(c(
			C_KEY_WORDS_EVENTS, 
			C_KEY_WORDS_OVERALL_EVENTS, 
			C_KEY_WORDS_SAMPLE_SIZES, 
			C_KEY_WORDS_OVERALL_SAMPLE_SIZES, 
			C_KEY_WORDS_MEANS, 
			C_KEY_WORDS_OVERALL_MEANS, 
			C_KEY_WORDS_ST_DEVS, 
			C_KEY_WORDS_OVERALL_ST_DEVS,
			C_KEY_WORDS_ALLOCATION_RATIOS,
			C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
			C_KEY_WORDS_LOG_RANKS,
			C_KEY_WORDS_OVERALL_LOG_RANKS))
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
	dataObjectkeyWords1 <- c(C_KEY_WORDS_EVENTS, C_KEY_WORDS_OVERALL_EVENTS)
	dataObjectkeyWords2 <- c(C_KEY_WORDS_OVERALL_LOG_RANKS, 
		C_KEY_WORDS_LOG_RANKS, 
		C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, 
		C_KEY_WORDS_ALLOCATION_RATIOS)
	
	dataObjectkeyWords1 <- c(dataObjectkeyWords1, paste0(dataObjectkeyWords1, c(1, 2)))
	dataObjectkeyWords2 <- c(dataObjectkeyWords2, paste0(dataObjectkeyWords2, c(1, 2)))
	
	return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords1) &&
			!.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords2))
}

.isDataObjectSurvival <- function(...) {
	dataObjectkeyWords <- c(C_KEY_WORDS_OVERALL_LOG_RANKS, 
		C_KEY_WORDS_LOG_RANKS, 
		C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, 
		C_KEY_WORDS_ALLOCATION_RATIOS)
	dataObjectkeyWords <- c(dataObjectkeyWords, paste0(dataObjectkeyWords, c(1, 2)))
	return(.isDataObject(..., dataObjectkeyWords = dataObjectkeyWords))
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
		.kMax = "integer",
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
				.kMax <<- getNumberOfStages()
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
				callSuper(showType = showType, digits = digits, consoleOutputEnabled = consoleOutputEnabled)
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
			
			for (var in names(.self)) {
				values <- .self[[var]]
				if (any(is.nan(values)) || any(is.infinite(values))) {
					stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "'", var, "' (", .arrayToString(values), 
						") contains illegal values, i.e., something went wrong")
				}
			}
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
			.kMax <<- numberOfStages
			if (numberOfStages >= kMax) {
				return(invisible())
			}
			
			numberOfGroups <- getNumberOfGroups(survivalCorrectionEnabled = FALSE)
			for (s in (numberOfStages + 1):kMax) {
				for (g in 1:numberOfGroups) {
					stages <<- c(stages, s)
					groups <<- c(groups, g)
				}
			}
		},
		
		.trim = function(kMax) {
			if (is.na(kMax)) {
				kMax <- .kMax
			}
			numberOfStages <- getNumberOfStages(FALSE)
			if (numberOfStages <= kMax) {
				return(invisible(numeric(0)))
			}
			
			indices <- which(stages <= kMax)
			
			stages <<- stages[indices]
			groups <<- groups[indices]
			
			return(indices)
		},
		
		.orderDataByStageAndGroup = function() {
			.data <<- .data[order(.data[, 1], .data[, 2]), ]
		},
		
		.getNumberOfNAsToAdd = function(kMax) {
			n <- kMax - getNumberOfStages()
			if (n <= 0) {
				return(0)
			}
			
			n <- n * getNumberOfGroups(survivalCorrectionEnabled = FALSE)
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
				i <- 1:getNumberOfGroups(survivalCorrectionEnabled = FALSE)
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
		
		.getValidatedFloatingPointNumbers = function(x, parameterName = "Sample sizes") {
			if (.floatingPointNumbersEnabled) {
				return(x)
			}
			
			nToCheck <- stats::na.omit(x)
			if (any(nToCheck != as.integer(nToCheck))) {
				warning(parameterName, " specified as floating-point numbers were truncated", call. = FALSE)
			}
			
			x[!is.na(x)] <- as.integer(x[!is.na(x)])
			return(x)
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
		
		getNumberOfGroups = function(survivalCorrectionEnabled = TRUE) {
			data <- stats::na.omit(.data)
			if (!survivalCorrectionEnabled) {
				return(length(unique(data$group)))
			}
			return(length(unique(data$group)) + ifelse(inherits(.self, "DatasetSurvival"), 1, 0))
		},
		
		getNumberOfStages = function(naOmitEnabled = TRUE) {
			if (naOmitEnabled) {
				data <- stats::na.omit(.data)
				return(length(unique(data$stage)))
			}
			return(length(unique(.data$stage)))
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
#' This object cannot be created directly; better use \code{\link{getDataset}} 
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
			return(.getValidatedFloatingPointNumbers(n, parameterName = "Sample sizes"))
		},
		
		.initByDataFrame = function(dataFrame) {
			callSuper(dataFrame)	
			
			# case: one mean - stage wise
			if (.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
				sampleSizes <<- .getValidatedSampleSizes(.getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_SAMPLE_SIZES))
				.validateValues(sampleSizes, "n")
				if (any(stats::na.omit(sampleSizes) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"all sample sizes must be > 0, but 'n' = ",
						.arrayToString(sampleSizes, vectorLookAndFeelEnabled = TRUE))
				}
				
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
				.assertValuesAreStrictlyIncreasing(overallSampleSizes, "overallSampleSizes", endingNasAllowed = TRUE)
				
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
					if (any(stats::na.omit(sampleSizesTemp) <= 0)) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"all sample sizes must be > 0, but 'n", group, "' = ",
							.arrayToString(sampleSizesTemp, vectorLookAndFeelEnabled = TRUE))
					}
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
					.assertValuesAreStrictlyIncreasing(overallSampleSizesTemp, 
						paste0("overallSampleSizes", group), endingNasAllowed = TRUE)
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
		
		.trim = function(kMax = NA_integer_) {
			indices <- callSuper(kMax)
			if (length(indices) == 0) {
				return(invisible(FALSE))
			}
			
			sampleSizes <<- sampleSizes[indices]
			means <<- means[indices]
			stDevs <<- stDevs[indices]
			
			overallSampleSizes <<- overallSampleSizes[indices]
			overallMeans <<- overallMeans[indices]
			overallStDevs <<- overallStDevs[indices]
			
			.data <<- data.frame(stage = stages, group = groups, sampleSize = sampleSizes, 
				mean = means, stDev = stDevs, overallSampleSize = overallSampleSizes, 
				overallMean = overallMeans, overallStDev = overallStDevs)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
			
			return(invisible(TRUE))
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
			if (stage > 1) {
				for (k in 2:stage) {
					dataInput$means[k] <- (dataInput$overallSampleSizes[k] * dataInput$overallMeans[k] -
							dataInput$overallSampleSizes[k - 1] * dataInput$overallMeans[k - 1])/	
						dataInput$sampleSizes[k]
				}
			}
			
			dataInput$stDevs <- c(dataInput$overallStDevs[1:stage], rep(NA_real_, kMax - stage))
			if (stage > 1) {
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
#' @description
#' Plots a dataset.
#' 
#' @param x The \code{\link{Dataset}} object to plot.
#' @param y Not available for this kind of plot (is only defined to be compatible 
#'        to the generic plot function).
#' @param main The main title, default is \code{"Dataset"}.
#' @param xlab The x-axis label, default is \code{"Stage"}.
#' @param ylab The y-axis label.
#' @param legendTitle The legend title, default is \code{"Group"}.
#' @inheritParams param_palette
#' @inheritParams param_showSource
#' @inheritParams param_three_dots_plot
#' 
#' @details
#' Generic function to plot all kinds of datasets.
#' 
#' @template return_object_ggplot
#' 
#' @examples 
#' # Plot a dataset of means
#' dataExample <- getDataset(
#'     n1 = c(22, 11, 22, 11),
#'     n2 = c(22, 13, 22, 13),
#'     means1 = c(1, 1.1, 1, 1),
#'     means2 = c(1.4, 1.5, 3, 2.5),
#'     stDevs1 = c(1, 2, 2, 1.3),
#'     stDevs2 = c(1, 2, 2, 1.3))
#' \donttest{
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of Means")
#' }
#' 
#' # Plot a dataset of rates
#' dataExample <- getDataset(
#'     n1 = c(8, 10, 9, 11),
#'     n2 = c(11, 13, 12, 13),
#'     events1 = c(3, 5, 5, 6),
#'     events2 = c(8, 10, 12, 12)
#' )
#' \donttest{
#' if (require(ggplot2)) plot(dataExample, main = "Comparison of Rates")
#' }
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
		stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, "plot of survival data is not implemented yet")
	}
	
	if (!is.logical(showSource) || isTRUE(showSource)) {
		warning("'showSource' != FALSE is not implemented yet for class ", class(x))
	}
	
	if (x$getNumberOfGroups() == 1) {	
		if (x$isDatasetMeans()) {
			p <- ggplot2::ggplot(data = data, 
				ggplot2::aes(y = .data[["randomData"]], x = factor(.data[["stage"]]))) 
			p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = .data[["stage"]]))
			p <- p + ggplot2::geom_point(colour = "#0e414e", shape = 20, 
				position = ggplot2::position_jitter(width = .1), 
				size = x$getPlotSettings()$pointSize) 
			p <- p + ggplot2::stat_summary(fun = "mean", geom = "point", 
				shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white", 
				colour = "black", show.legend = FALSE)
		} 
		
		else if (x$isDatasetRates()) {
			p <- ggplot2::ggplot(show.legend = FALSE) 
			
			# plot sample size
			p <- p + ggplot2::geom_bar(data = data, 
				ggplot2::aes(y = .data[["sampleSize"]], 
					x = factor(.data[["stage"]]), fill = factor(.data[["stage"]])),
				position = "dodge", stat = "identity", alpha = 0.4)
			
			# plot events
			p <- p + ggplot2::geom_bar(data = data, 
				ggplot2::aes(y = .data[["event"]], x = factor(.data[["stage"]]), 	
					fill = factor(.data[["stage"]])), 
				position = "dodge", stat = "identity") 
		}
		
		else if (x$isDatasetSurvival()) {
			# implement survival plot here
		}
	} else {
		data$stageGroup <- interaction(data$stage, data$group)	
		
		if (x$isDatasetMeans()) {
			p <- ggplot2::ggplot(ggplot2::aes(y = .data[["randomData"]], x = factor(.data[["stage"]]), 
					fill = factor(.data[["group"]])), data = data) 
			p <- p + ggplot2::geom_point(ggplot2::aes(colour = .data[["group"]]), shape = 20, 
				position = ggplot2::position_dodge(.75), 
				size = x$getPlotSettings()$pointSize)
			p <- p + ggplot2::geom_boxplot()
			p <- p + ggplot2::stat_summary(ggplot2::aes(colour = .data[["group"]]), 
				fun = "mean", geom = "point", 
				shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white", 
				show.legend = FALSE)
		} 
		
		else if (x$isDatasetRates()) {
			p <- ggplot2::ggplot(show.legend = FALSE) 
			
			# plot sample size
			p <- p + ggplot2::geom_bar(ggplot2::aes(y = .data[["sampleSize"]], 
					x = factor(.data[["stage"]]), fill = factor(.data[["group"]])),
				data = data, position = "dodge", stat = "identity", alpha = 0.4)
			
			# plot events
			p <- p + ggplot2::geom_bar(data = data, 
				ggplot2::aes(y = .data[["event"]], x = factor(.data[["stage"]]), 
					fill = factor(.data[["group"]])), 
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
#' @field groups The group numbers.
#' @field stages The stage numbers.
#' @field sampleSizes The sample sizes.
#' @field events The events.
#' @field overallSampleSizes The overall sample sizes.
#' @field overallEvents The overall events.
#' 
#' @details 
#' This object cannot be created directly; better use \code{\link{getDataset}} 
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
			return(.getValidatedFloatingPointNumbers(n, parameterName = "Sample sizes"))
		},
		
		.initByDataFrame = function(dataFrame) {
			callSuper(dataFrame)	
			
			# case: one rate - stage wise
			if (.paramExists(dataFrame, C_KEY_WORDS_SAMPLE_SIZES)) {
				sampleSizes <<- .getValidatedSampleSizes(
					.getValuesByParameterName(dataFrame, C_KEY_WORDS_SAMPLE_SIZES))
				.validateValues(sampleSizes, "n")
				if (any(stats::na.omit(sampleSizes) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
						"all sample sizes must be > 0, but 'n' = ",
						.arrayToString(sampleSizes, vectorLookAndFeelEnabled = TRUE))
				}
				
				events <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS)
				.validateValues(events, "events")
				if (any(stats::na.omit(events) < 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events' = ",
						.arrayToString(events, vectorLookAndFeelEnabled = TRUE))
				}
				
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
				.assertValuesAreStrictlyIncreasing(overallSampleSizes, "overallSampleSizes", endingNasAllowed = TRUE)
				
				overallEvents <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS)
				.validateValues(overallEvents, "overallEvents")
				.assertValuesAreMonotoneIncreasing(overallEvents, "overallEvents", endingNasAllowed = TRUE)
				
				kMax <- length(overallSampleSizes)
				stageNumber <- length(stats::na.omit(overallSampleSizes))
				stageWiseData <- .getStageWiseData(data.frame(
						overallSampleSizes = overallSampleSizes,
						overallEvents = overallEvents), kMax, stage = stageNumber)
				sampleSizes <<- .getValidatedSampleSizes(stageWiseData$sampleSizes)
				events <<- stageWiseData$events
				
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
					if (any(stats::na.omit(sampleSizesTemp) <= 0)) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
							"all sample sizes must be > 0, but 'n", group, "' = ",
							.arrayToString(sampleSizesTemp, vectorLookAndFeelEnabled = TRUE))
					}
					sampleSizes <<- c(sampleSizes, sampleSizesTemp)
					
					eventsTemp <- .getValuesByParameterName(dataFrame, C_KEY_WORDS_EVENTS, suffix = group)
					.validateValues(eventsTemp, paste0("events", group))
					if (any(stats::na.omit(eventsTemp) < 0)) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be >= 0, but 'events", group, "' = ",
							.arrayToString(eventsTemp, vectorLookAndFeelEnabled = TRUE))
					}
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
					.assertValuesAreStrictlyIncreasing(overallSampleSizesTemp, 
						paste0("overallSampleSizes", group), endingNasAllowed = TRUE)
					overallSampleSizes <<- c(overallSampleSizes, overallSampleSizesTemp)
					
					overallEventsTemp <- .getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_EVENTS, suffix = group)
					.validateValues(overallEventsTemp, paste0("overallEvents", group))
					.assertValuesAreMonotoneIncreasing(overallEventsTemp, 
						paste0("overallEvents", group), endingNasAllowed = TRUE)
					overallEvents <<- c(overallEvents, overallEventsTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(overallSampleSizesTemp)))
					
					kMax <- length(overallSampleSizesTemp)
					numberOfValidStages <- length(stats::na.omit(overallSampleSizesTemp))
					stageWiseData <- .getStageWiseData(data.frame(
							overallSampleSizes = overallSampleSizesTemp,
							overallEvents = overallEventsTemp), kMax, stage = numberOfValidStages)
					
					validatedSampleSizes <- .getValidatedSampleSizes(stageWiseData$sampleSizes)
					.validateValues(validatedSampleSizes, paste0("n", group))
					sampleSizes <<- c(sampleSizes, validatedSampleSizes)
					events <<- c(events, stageWiseData$events)
					
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
		
		
		.trim = function(kMax = NA_integer_) {
			indices <- callSuper(kMax)
			if (length(indices) == 0) {
				return(invisible(FALSE))
			}
			
			sampleSizes <<- sampleSizes[indices]
			events <<- events[indices]
			
			overallSampleSizes <<- overallSampleSizes[indices]
			overallEvents <<- overallEvents[indices]
			
			.data <<- data.frame(stage = stages, group = groups, 
				sampleSize = sampleSizes, event = events,
				overallSampleSize = overallSampleSizes, overallEvent = overallEvents)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
			
			return(invisible(TRUE))
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
#' @field groups The group numbers.
#' @field stages The stage numbers.
#' @field overallEvents The overall events.
#' @field overallAllocationRatios The overall allocations ratios.
#' @field overallLogRanks The overall logrank test statistics.
#' @field allocationRatios The allocation ratios.
#' @field logRanks The logrank test statistics.
#' 
#' @details 
#' This object cannot be created directly; better use \code{\link{getDataset}} 
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
		
		getEvents = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$event[.getIndices(stage = stage, group = group)])
		},
		
		getEventsUpTo = function(to, group = 1) {
			return(.data$event[.getIndices(stage = c(1:to), group = group)])
		},
		
		getAllocationRatio = function(stage, group = 1) {
			return(.data$allocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getAllocationRatios = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$allocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getAllocationRatiosUpTo = function(to, group = 1) {
			return(.data$allocationRatio[.getIndices(stage = c(1:to), group = group)])
		},
		
		getLogRank = function(stage, group = 1) {
			return(.data$logRank[.getIndices(stage = stage, group = group)])
		},
		
		getLogRanks = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$logRank[.getIndices(stage = stage, group = group)])
		},
		
		getLogRanksUpTo = function(to, group = 1) {
			return(.data$logRank[.getIndices(stage = c(1:to), group = group)])
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
		
		getOverallAllocationRatio = function(stage, group = 1) {
			return(.data$overallAllocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getOverallAllocationRatios = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallAllocationRatio[.getIndices(stage = stage, group = group)])
		},
		
		getOverallAllocationRatiosUpTo = function(to, group = 1) {
			return(.data$overallAllocationRatio[.getIndices(stage = c(1:to), group = group)])
		},
		
		getOverallLogRank = function(stage, group = 1) {
			return(.data$overallLogRank[.getIndices(stage = stage, group = group)])
		},
		
		getOverallLogRanks = function(..., stage = NA_integer_, group = NA_integer_) {
			stage <- .getValidatedStage(..., stage = stage, group = group)
			return(.data$overallLogRank[.getIndices(stage = stage, group = group)])
		},
		
		getOverallLogRanksUpTo = function(to, group = 1) {
			return(.data$overallLogRank[.getIndices(stage = c(1:to), group = group)])
		},
		
		.getAllocationRatioDefaultValues = function(stages, events, logRanks) {
			allocationRatioDefaultValues <- rep(C_ALLOCATION_RATIO_DEFAULT, length(stages))
			indices <- which(is.na(events) | is.na(logRanks))
			allocationRatioDefaultValues[indices] <- NA_real_
			return(allocationRatioDefaultValues)
		},
		
		.initByDataFrame = function(dataFrame) {
			callSuper(dataFrame)	
			
			# case: survival, two groups - overall
			if (.paramExists(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)) {
				
				overallEvents <<- .getValidatedFloatingPointNumbers(
					.getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_EVENTS), parameterName = "Overall events")
				.validateValues(overallEvents, "overallEvents")
				.assertValuesAreStrictlyIncreasing(overallEvents, "overallEvents", endingNasAllowed = TRUE)
				
				overallLogRanks <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)
				.validateValues(overallLogRanks, "overallLogRanks")
				
				overallAllocationRatios <<- .getValuesByParameterName(
					dataFrame, parameterNameVariants = C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS,
					defaultValues = .getAllocationRatioDefaultValues(stages, overallEvents, overallLogRanks))
				.validateValues(overallAllocationRatios, "overallAllocationRatios")
				
				kMax <- length(overallEvents)
				stageNumber <- length(stats::na.omit(overallEvents))
				dataInput <- data.frame(
					overallEvents = overallEvents,
					overallAllocationRatios = overallAllocationRatios,
					overallLogRanks = overallLogRanks)
				dataInput <- .getStageWiseData(dataInput, kMax, stage = stageNumber)
				events <<- .getValidatedFloatingPointNumbers(dataInput$events, parameterName = "Events")
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
			
			# case: survival, two groups - stage wise
			else if (.paramExists(dataFrame, C_KEY_WORDS_LOG_RANKS)) {
				
				events <<- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
						dataFrame, C_KEY_WORDS_EVENTS), parameterName = "Events")
				.validateValues(events, "events")
				if (any(stats::na.omit(events) <= 0)) {
					stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be > 0")
				}
				
				logRanks <<- .getValuesByParameterName(dataFrame, C_KEY_WORDS_LOG_RANKS)
				.validateValues(logRanks, "logRanks")
				
				allocationRatios <<- .getValuesByParameterName(
					dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS,
					defaultValues = .getAllocationRatioDefaultValues(stages, events, logRanks))
				.validateValues(allocationRatios, "allocationRatios")
				
				kMax <- length(events)
				stageNumber <- length(stats::na.omit(events))
				dataInput <- data.frame(
					events = events,
					allocationRatios = allocationRatios,
					logRanks = logRanks)
				dataInput <- .getOverallData(dataInput, kMax, stage = stageNumber)
				overallEvents <<- .getValidatedFloatingPointNumbers(dataInput$overallEvents, parameterName = "Overall events")
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
			
			# case: survival, three ore more groups - overall
			else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_LOG_RANKS, 1)) &&
				.paramExists(dataFrame, paste0(C_KEY_WORDS_OVERALL_LOG_RANKS, 2))) {
				
				numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS)
				
				stages <<- rep(stages, numberOfTreatmentGroups)
				
				groups <<- integer(0)
				events <<- numeric(0)
				allocationRatios <<- numeric(0)
				logRanks <<- numeric(0)
				overallEvents <<- numeric(0)
				overallAllocationRatios <<- numeric(0)
				overallLogRanks <<- numeric(0)
				for (group in 1:numberOfTreatmentGroups) {
					overallEventsTemp <- .getValuesByParameterName(dataFrame, 
						C_KEY_WORDS_OVERALL_EVENTS, suffix = group)
					.validateValues(overallEventsTemp, paste0("overallEvents", group))
					.assertValuesAreStrictlyIncreasing(overallEventsTemp, 
						paste0("overallEvents", group), endingNasAllowed = TRUE)
					overallEvents <<- c(overallEvents, overallEventsTemp)
					
					overallLogRanksTemp <- .getValuesByParameterName(
						dataFrame, C_KEY_WORDS_OVERALL_LOG_RANKS, suffix = group)
					.validateValues(overallLogRanksTemp, paste0("overallLogRanks", group))
					overallLogRanks <<- c(overallLogRanks, overallLogRanksTemp)
					
					overallAllocationRatiosTemp <- .getValuesByParameterName(
						dataFrame, C_KEY_WORDS_OVERALL_ALLOCATION_RATIOS, suffix = group,
						defaultValues = .getAllocationRatioDefaultValues(overallEventsTemp, 
							overallEventsTemp, overallLogRanksTemp))
					.validateValues(overallAllocationRatiosTemp, paste0("overallAllocationRatios", group))
					overallAllocationRatios <<- c(overallAllocationRatios, overallAllocationRatiosTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(overallLogRanksTemp)))
					
					kMax <- length(overallLogRanksTemp)
					numberOfValidStages <- length(stats::na.omit(overallLogRanksTemp))
					stageWiseData <- .getStageWiseData(data.frame(
							overallLogRanks = overallLogRanksTemp,
							overallAllocationRatios = overallAllocationRatiosTemp,
							overallEvents = overallEventsTemp), 
						kMax, stage = numberOfValidStages)
					
					validatedLogRanks <- stageWiseData$logRanks
					.validateValues(validatedLogRanks, paste0("n", group))
					logRanks <<- c(logRanks, validatedLogRanks)
					allocationRatios <<- c(allocationRatios, stageWiseData$allocationRatios)
					events <<- c(events, stageWiseData$events)
				}
				
				.setParameterType("events", C_PARAM_GENERATED)
				.setParameterType("allocationRatios", C_PARAM_GENERATED)
				.setParameterType("logRanks", C_PARAM_GENERATED)
				
				.setParameterType("overallEvents", C_PARAM_USER_DEFINED)
				.setParameterType("overallAllocationRatios", C_PARAM_USER_DEFINED)
				.setParameterType("overallLogRanks", C_PARAM_USER_DEFINED)
			}
			
			# case: survival, three ore more groups - stage wise
			else if (.paramExists(dataFrame, paste0(C_KEY_WORDS_LOG_RANKS, 1)) && 
				.paramExists(dataFrame, paste0(C_KEY_WORDS_LOG_RANKS, 2))) {
				
				numberOfTreatmentGroups <- .getNumberOfGroups(dataFrame, C_KEY_WORDS_LOG_RANKS)
				
				stages <<- rep(stages, numberOfTreatmentGroups)
				
				groups <<- integer(0)
				events <<- numeric(0)
				allocationRatios <<- numeric(0)
				logRanks <<- numeric(0)
				overallEvents <<- numeric(0)
				overallAllocationRatios <<- numeric(0)
				overallLogRanks <<- numeric(0)
				for (group in 1:numberOfTreatmentGroups) {
					
					eventsTemp <- .getValidatedFloatingPointNumbers(.getValuesByParameterName(
							dataFrame, C_KEY_WORDS_EVENTS, suffix = group), parameterName = "Events")
					if (any(stats::na.omit(eventsTemp) <= 0)) {
						stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, "all events must be > 0, but 'events", group, "' = ",
							.arrayToString(eventsTemp, vectorLookAndFeelEnabled = TRUE))
					}
					events <<- c(events, eventsTemp)
					
					logRanksTemp <- .getValuesByParameterName(
						dataFrame, C_KEY_WORDS_LOG_RANKS, suffix = group)
					.validateValues(logRanksTemp, paste0("n", group))
					logRanks <<- c(logRanks, logRanksTemp)
					
					allocationRatiosTemp <- .getValuesByParameterName(
						dataFrame, C_KEY_WORDS_ALLOCATION_RATIOS, suffix = group,
						defaultValues = .getAllocationRatioDefaultValues(eventsTemp, 
							eventsTemp, logRanksTemp))
					.validateValues(allocationRatiosTemp, paste0("allocationRatios", group))
					allocationRatios <<- c(allocationRatios, allocationRatiosTemp)
					
					groups <<- c(groups, rep(as.integer(group), length(eventsTemp)))
					
					kMax <- length(eventsTemp)
					numberOfValidStages <- length(stats::na.omit(eventsTemp))
					overallData <- .getOverallData(data.frame(
							events = eventsTemp,
							allocationRatios = allocationRatiosTemp,
							logRanks = logRanksTemp), kMax, stage = numberOfValidStages)
					
					overallEvents <<- c(overallEvents, overallData$overallEvents)
					overallAllocationRatios <<- c(overallAllocationRatios, overallData$overallAllocationRatios)
					overallLogRanks <<- c(overallLogRanks, overallData$overallLogRanks)
				}
				
				.setParameterType("events", C_PARAM_USER_DEFINED)
				.setParameterType("allocationRatios", C_PARAM_USER_DEFINED)
				.setParameterType("logRanks", C_PARAM_USER_DEFINED)
				
				.setParameterType("overallEvents", C_PARAM_GENERATED)
				.setParameterType("overallAllocationRatios", C_PARAM_GENERATED)
				.setParameterType("overallLogRanks", C_PARAM_GENERATED)
			}
			
			.data <<- data.frame(stage = stages, group = groups, 
				overallEvent = overallEvents, 
				overallAllocationRatio = overallAllocationRatios, 
				overallLogRank = overallLogRanks, 
				event = events,
				allocationRatio = allocationRatios,
				logRank = logRanks)
			
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
			logRanks <<- .data$logRank
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
				logRank = logRanks)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
		},
		
		.trim = function(kMax = NA_integer_) {
			indices <- callSuper(kMax)
			if (length(indices) == 0) {
				return(invisible(FALSE))
			}
			
			events <<- events[indices]
			allocationRatios <<- allocationRatios[indices]
			logRanks <<- logRanks[indices]
			
			overallEvents <<- overallEvents[indices]
			overallAllocationRatios <<- overallAllocationRatios[indices]
			overallLogRanks <<- overallLogRanks[indices]
			
			.data <<- data.frame(stage = stages, group = groups, 
				overallEvent = overallEvents, 
				overallAllocationRatio = overallAllocationRatios, 
				overallLogRank = overallLogRanks,
				event = events,
				allocationRatio = allocationRatios,
				logRank = logRanks)
			
			.orderDataByStageAndGroup()
			.setDataToVariables()
			
			return(invisible(TRUE))
		},
		
		getRandomData = function() {
			stop(C_EXCEPTION_TYPE_RUNTIME_ISSUE, 
				"the function 'DatasetSurvival.getRandomData()' is not implemented yet") 
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
				for (k in 2:stage) {
					dataInput$overallLogRanks[k] <- 
						(base::sqrt(dataInput$events[k]) * dataInput$logRanks[k] + 
							base::sqrt(dataInput$overallEvents[k - 1]) * 
							dataInput$overallLogRanks[k - 1]) / base::sqrt(dataInput$overallEvents[k])  
				}		
			}
			
			dataInput$overallAllocationRatios <- c(dataInput$allocationRatios[1:stage], 
				rep(NA_real_, kMax - stage))
			if (stage > 1) {
				for (k in 2:stage) {
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
				dataInput$allocationRatios[2:stage] <- (
						dataInput$overallAllocationRatios[2:stage] - 
						dataInput$overallAllocationRatios[1:(stage - 1)] * 
						dataInput$overallEvents[1:(stage - 1)] / 
						dataInput$overallEvents[2:stage]
						) /	
					(dataInput$events[2:stage] / dataInput$overallEvents[2:stage])
			}
			if (any(stats::na.omit(dataInput$allocationRatios) <= 0)) {
				stop(C_EXCEPTION_TYPE_ILLEGAL_ARGUMENT, 
					"overall allocation ratios not correctly specified, must be > 0") 
			}	
			
			return(dataInput)
		}
	)
)

summary.Dataset <- function(object, ..., type = 1, digits = NA_integer_) {
	.warnInCaseOfUnknownArguments(functionName = "summary", ...)
	
	if (type == 1 && inherits(object, "SummaryFactory")) {
		return(object)
	}
	
	if (type != 1) {
		return(summary.ParameterSet(object, type = type, digits = digits, ...))
	}
	
	intervalFormat <- getOption("rpact.summary.intervalFormat", "[%s; %s]")
	.assertIsValidSummaryIntervalFormat(intervalFormat)
	
	summaryFactory <- SummaryFactory(object = object, intervalFormat = intervalFormat)
	
	s <- ""
	if (object$isDatasetMeans()) {
		s <- "dataset of means"
	}
	else if (object$isDatasetRates()) {
		s <- "dataset of rates"
	}
	else if (object$isDatasetSurvival()) {
		s <- "dataset of survival data"
	}
	
	kMax <- object$getNumberOfStages()
	if (kMax == 1) {
		summaryFactory$title <- paste0("Fixed ", s)
	} else {
		summaryFactory$title <- .firstCharacterToUpperCase(s)
	}
	
	numberOfGroups <- object$getNumberOfGroups()
	
	if (numberOfGroups == 1) {
		groups <- "one sample"
	}
	else if (numberOfGroups == 2) {
		groups <- c("one treatment", "one control group")
		if (object$isDatasetSurvival()) {
			groups <- paste0(groups, c(" (1)", " (2)"))
		}
	}
	else {
		groups <- c(paste0(.integerToWrittenNumber(numberOfGroups - 1), 
				" treatment groups"), "one control group")
		if (object$isDatasetSurvival()) {
			groups <- paste0(groups, c(
					paste0(" (", .arrayToString(1:(numberOfGroups - 1)), ")"), 
					paste0(" (", numberOfGroups, ")")))
		}
	}
	
	prefix <- ""
	if (object$isDatasetMeans()) {
		prefix <- "the sample sizes, means, and standard deviations of "
	}
	else if (object$isDatasetRates()) {
		prefix <- "the sample sizes and events of "
	}
	else if (object$isDatasetSurvival()) {
		prefix <- "the events and log rank statistics of the comparison of "
	}
	if (numberOfGroups > 1) {
		prefix <- paste0(prefix, "\n")
	}
	header <- paste0("The dataset contains ", prefix, 
		paste0(groups, collapse = ifelse(object$isDatasetSurvival(), " with ", " and ")))
	if (kMax > 1) {
		header <- paste0(header, ".\nThe total number of looks is ", .integerToWrittenNumber(kMax), 
			"; stage-wise and overall data are included")
	}
	header <- paste0(header, ".")
	summaryFactory$header <- header
	
	digitSettings <- .getSummaryDigits(digits)
	digits <- digitSettings$digits
	digitsSampleSize <- digitSettings$digitsSampleSize
	digitsGeneral <- digitSettings$digitsGeneral
	digitsProbabilities <- digitSettings$digitsProbabilities
	
	if (kMax > 1) {
		summaryFactory$addItem("Stage", object$stages)
	}
	
	if (numberOfGroups > 1) {
		
		groupNumbers <- object$groups
		if (object$isDatasetSurvival()) {
			groupNumbers <- paste0(object$groups, " vs ", numberOfGroups)
			summaryFactory$addItem("Comparison", groupNumbers)
		} else {
			summaryFactory$addItem("Group", groupNumbers)
		}		
	}
	
	parameterCaptionPrefix <- ifelse(kMax == 1, "", "Stage-wise ")
	
	if (object$isDatasetMeans() || object$isDatasetRates()) {
		summaryFactory$addParameter(object, parameterName = "sampleSizes", 
			parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "sample size"), roundDigits = digitsSampleSize)
		if (kMax > 1) {
			summaryFactory$addParameter(object, parameterName = "overallSampleSizes", 
				parameterCaption = "Overall sample size", roundDigits = digitsSampleSize)
		}
	}
	
	if (object$isDatasetMeans()) {
		summaryFactory$addParameter(object, parameterName = "means", 
			parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "mean"), roundDigits = digitsGeneral)
		if (kMax > 1) {
			summaryFactory$addParameter(object, parameterName = "overallMeans", 
				parameterCaption = "Overall mean", roundDigits = digitsGeneral)
		}
		summaryFactory$addParameter(object, parameterName = "stDevs", 
			parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "standard deviation"), roundDigits = digitsGeneral)
		if (kMax > 1) {
			summaryFactory$addParameter(object, parameterName = "overallStDevs", 
				parameterCaption = "Overall standard deviation", roundDigits = digitsGeneral)
		}
	}
	else if (object$isDatasetRates()) {
		summaryFactory$addParameter(object, parameterName = "events", 
			parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "number of events"), roundDigits = digitsSampleSize)
		if (kMax > 1) {
			summaryFactory$addParameter(object, parameterName = "overallEvents", 
				parameterCaption = "Overall number of events", roundDigits = digitsSampleSize)
		}
	}
	else if (object$isDatasetSurvival()) {
		summaryFactory$addParameter(object, parameterName = "events", 
			parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "number of events"), roundDigits = digitsSampleSize)
		if (kMax > 1) {
			summaryFactory$addParameter(object, parameterName = "overallEvents", 
				parameterCaption = "Overall number of events", roundDigits = digitsSampleSize)
		}
		summaryFactory$addParameter(object, parameterName = "logRanks", 
			parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "log rank statistic"), roundDigits = digitsGeneral)
		if (kMax > 1) {
			summaryFactory$addParameter(object, parameterName = "overallLogRanks", 
				parameterCaption = "Overall log rank statistic", roundDigits = digitsGeneral)
		}
		if (!any(is.na(object$allocationRatios)) && any(object$allocationRatios != 1)) {
			summaryFactory$addParameter(object, parameterName = "allocationRatios", 
				parameterCaption = .firstCharacterToUpperCase(parameterCaptionPrefix, "allocation ratio"), roundDigits = digitsGeneral)
			if (kMax > 1) {
				summaryFactory$addParameter(object, parameterName = "overallAllocationRatios", 
					parameterCaption = "Overall allocation ratio", roundDigits = digitsGeneral)
			}
		}
	}
	
	return(summaryFactory)
}
