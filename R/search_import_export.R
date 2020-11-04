#' Exports search results
#' 
#' Search results from a search object will be saved to a Excel-XLSX or a CSV (comma separated values) file.
#' By default a XLSX file will be saved. If you want to save a CSV file, use \code{saveAsCSV=TRUE}.
#' Please note: 
#' - The function will '=' signs at the beginning of annotation by ".=". This is because the content would be interpreted as the beginning of a formula (leading to an error).
#' - In the case of writing to an excel file, line breaks will be replaced by "|". This is because line breaks will lead to an error.
#' 
#' @param s Search object. Search object containing the results you wish to export.
#' @param path Character string; path where file will be saved. Please add the suffix '*.csv' or '*.xlsx' to the filename.
#' @param sheetNameXLSX Character string, set the name of the excel sheet.
#' @param saveAsCSV Logical; if \code{TRUE} results will be saved as CSV file; Logical; if \code{FALSE} a XLS file will be saved.
#' @param encodingCSV Character string; text encoding for CSV files.
#' @param separatorCSV Character; single character that is used to separate the columns.
#'
#' @export
#'
#' @example inst/examples/search_import_export.R
#' 
search_results_export <- function(s, path, sheetNameXLSX="data", saveAsCSV=FALSE, encodingCSV="UTF-8", separatorCSV=",") {
	#check colnames
	mycolnames <- colnames(s@results)
	necessarycolnames <- c("resultID", "dataID", "transcriptName", "tierName", "startSec", "endSec", "content", "content.norm", "hit", "hit.nr", "hit.length", "hit.pos.content", "hit.pos.fulltext", "searchMode", "hit.span")
	missingcolnames <- necessarycolnames[!necessarycolnames %in% mycolnames]
	if (length(missingcolnames>0)) {
		stop(	stringr::str_c(c("Some necessary columns are missing in the data frame '@results' in your search object. Missing columns: ", missingcolnames), sep="", collapse=" "))
	}
	
	#replace .  by , in numbers
	s@results$startSec		<-	gsub("\\.", ",", s@results$startSec)
	s@results$endSec		<-	gsub("\\.", ",", s@results$endSec) 

	#replace = at he beginning of cells
	searchString<-"^="
	replacementString <- ".="
	s@results$content		<-	stringr::str_replace_all(s@results$content, searchString, replacementString )
	s@results$content.norm	<-	stringr::str_replace_all(s@results$content.norm,searchString, replacementString)	
	s@results$hit			<-	stringr::str_replace_all(s@results$hit, searchString, replacementString )
	s@results$concLeft1		<-	stringr::str_replace_all(s@results$concLeft1, searchString, replacementString )
	s@results$concLeft2		<-	stringr::str_replace_all(s@results$concLeft2, searchString, replacementString )
	s@results$concHit		<-	stringr::str_replace_all(s@results$concHit, searchString, replacementString )
	s@results$concRight1	<-	stringr::str_replace_all(s@results$concRight1, searchString, replacementString )
	s@results$concRight2	<-	stringr::str_replace_all(s@results$concRight2, searchString, replacementString )
	s@results$transcript	<-	stringr::str_replace_all(s@results$transcript, searchString, replacementString )
	


	#write
	if (saveAsCSV) {
		utils::write.table(s@results, file = path, sep = separatorCSV, col.names = mycolnames, row.names=FALSE, qmethod = "double", fileEncoding= encodingCSV)
	} else {
			#WriteXLS::WriteXLS(s@results, ExcelFileName = path)#, AdjWidth=TRUE ,verbose=FALSE )
			openxlsx::write.xlsx(s@results, file=path, sheetName="data")
	}
}





#' Import search results 
#' 
#' Search results will be imported from an Excel file ('*.xlsx') CSV file ('*.csv', comma separated values) into a search object.
#'
#' @param path Character string; path to file from where data will be loaded.
#' @param revertReplacements Logical, when exporting search results from act, '=' at the beginning of lines are replaced by '.=", and in numbers the decimal separator '.' is replaced by a ",". If \code{TRUE}, this replacement will be reverted when loading search results.
#' @param sheetNameXLSX Character string, set the name of the excel sheet containing the data.
#' @param encodingCSV Character string; text encoding in the case of CVS files.
#' @param separatorCSV Character; single character that is used to separate the columns in CSV files.
#'
#' @return Search object.
#' 
#' @export
#'
#' @example inst/examples/search_import_export.R
#' 
search_results_import <- function(path, revertReplacements=TRUE, sheetNameXLSX="data", encodingCSV="UTF-8", separatorCSV=",") {
	filetype <- tools::file_ext(path)
	if (filetype=="csv") {
		temp <- utils::read.table(path, header = TRUE, sep = separatorCSV, fileEncoding = encodingCSV, encoding=encodingCSV )
		if (is.null(temp$resultID)==FALSE){
			rownames(temp)<-temp$resultID
		}
	} else {
		temp<-openxlsx::read.xlsx(xlsxFile=path, sheet=sheetNameXLSX)
	}
	
	#check colnames
	necessarycolnames <- c("resultID", "dataID", "transcriptName", "tierName", "startSec", "endSec", "content", "content.norm", "hit", "hit.nr", "hit.length", "hit.pos.content", "hit.pos.fulltext", "searchMode", "hit.span")
	mycolnames<-colnames(temp)
	missingcolnames <- necessarycolnames[!necessarycolnames %in% mycolnames]
	if (length(missingcolnames>0)) {
		stop(	stringr::str_c(c("Some necessary columns are missing in your input file'. Missing columns: ", missingcolnames), sep="", collapse=" "))
	}
	
	if(revertReplacements) {
		#replace .  by , in numbers
		temp$startSec	<-	gsub(",", "\\.", temp$startSec)
		temp$endSec		<-	gsub(",", "\\.", temp$endSec) 
		
		#replace = at he beginning of cells
		searchString<-"^\\.="
		replacementString <- "="
		temp$content		<-	stringr::str_replace_all(temp$content, searchString, replacementString )
		temp$content.norm	<-	stringr::str_replace_all(temp$content.norm,searchString, replacementString)	
		temp$hit			<-	stringr::str_replace_all(temp$hit, searchString, replacementString )
		temp$concLeft1		<-	stringr::str_replace_all(temp$concLeft1, searchString, replacementString )
		temp$concLeft2		<-	stringr::str_replace_all(temp$concLeft2, searchString, replacementString )
		temp$concHit		<-	stringr::str_replace_all(temp$concHit, searchString, replacementString )
		temp$concRight1		<-	stringr::str_replace_all(temp$concRight1, searchString, replacementString )
		temp$concRight2		<-	stringr::str_replace_all(temp$concRight2, searchString, replacementString )
		temp$transcript		<-	stringr::str_replace_all(temp$transcript, searchString, replacementString )
	}
	
	#turn factors into vectors
	fctr.cols 					<- sapply(temp, is.factor)
	temp[, fctr.cols] 			<- sapply(temp[, fctr.cols], as.character)
	temp[is.na(temp)]			<- " "

	temp$startSec				<- as.double(temp$startSec)
	temp$endSec					<- as.double(temp$endSec)
	
	temp$dataID					<- as.integer(temp$dataID)
	temp$hit.length				<- as.integer(temp$hit.length)
	temp$hit.nr					<- as.integer(temp$hit.nr)
	temp$hit.pos.content		<- as.integer(temp$hit.pos.content)
	temp$hit.pos.fulltext		<- as.integer(temp$hit.pos.fulltext)
	temp$nrWordsLeft			<- as.integer(temp$nrWordsLeft)
	temp$nrWordsRight			<- as.integer(temp$nrWordsRight)
	temp$nrWordsHitPosition		<- as.integer(temp$nrWordsHitPosition)
	temp$nrWordsTotal			<- as.integer(temp$nrWordsTotal)
	
	#Create a search object
	s <- methods::new("search")
	#s@pattern                   <- pattern
	#s@searchMode                <- searchMode
	#s@searchNormalized          <- searchNormalized
	#s@startSec                  <- if(!is.na(startSec)) {if(!is.null(startSec)) {startSec}} else {s@startSec}
	#s@endSec                    <- if(!is.na(endSec))   {if(!is.null(endSec))   {endSec}}   else {s@endSec}
	#s@filter.tiers.include      <- ""
	#s@filter.tiers.exclude      <- ""
	#s@filter.transcripts.include<- if(!is.na(filterTranscriptsInclude))   {if(!is.null(filterTranscriptsInclude))   {filterTranscriptsInclude}}   else {s@filter.transcripts.include}
	#s@filter.transcripts.exclude<- if(!is.na(filterTranscriptsExclude))   {if(!is.null(filterTranscriptsExclude))   {filterTranscriptsExclude}}   else {s@filter.transcripts.exclude}
	s@results <- temp
	
	return(s)
}