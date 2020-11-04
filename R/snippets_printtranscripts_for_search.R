#' Create print transcripts for all search results
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param collection Character string; name of the collection/search. Will be used to create a sub folder in the output folder.
#' @param contextBeforeSec Double; how much context before the search result will be included (in seconds).
#' @param contextAfterSec Double; how much context after the search result will be included (in seconds).
#' @param outputFolder Character string; if set to "", the print transcripts will only be inserted in s@results; if the path to a existing folder is given transcripts will be saved in *.txt format.
#'
#' @return returns the search results with snippets inserted into a new column
#' @export
#'
#' @example inst/examples/snippets_printtranscripts_for_search.R
#' 
snippets_printtranscripts_search <- function( x = NULL, s=NULL, collection = "collection", contextBeforeSec	= 0,  contextAfterSec = 0, outputFolder=""  ) {
	#x<-corpus
	#s<-mysearch
	#collection       ="ponele"
	#contextBeforeSec	<- 0
	#contextAfterSec 	<- 0
	
	if (is.null(x)) 								{ stop("x is null, but needs to be a corpus object") 	}
	if (is.null(s)) 					            { stop("s is null, but needs to be a search object") 	}
	if (is.null(s@results$transcriptName)) 			{ stop("Data frame s@results does not contain column 'transcriptName'") 	}
	
	#--- check if output folder is given
	destination_folder<-""
	if (outputFolder!="") {
		destination_folder <- normalizePath(outputFolder)
		if (destination_folder!="") {
			if (file.exists(destination_folder)==FALSE) 	{
				stop("Output folder does not exist.")
			}
			
			#create a sub folder for the collection
			if (collection!="") 	{
				destination_folder <- file.path(destination_folder, collection)
				dir.create(destination_folder, showWarnings = FALSE)
				if (file.exists(destination_folder)==FALSE) 		{
					stop("Unable to create output directory")
				}
			}
		}	
	}
	
	myWarnings <- ""
	
	#create sub folders for text
	if (destination_folder!="") {
		dir.create(file.path(destination_folder, "transcripts"), showWarnings = FALSE)
	}
	
	#set progress bar
	act.environment$pb <- progress::progress_bar$new(
		format = "  Creating snippets  [:bar] :percent missing: :eta",
		total = max(1,nrow(s@results)), 
		clear = FALSE, 
		show_after = 0,
		width= 60)
	
	alltranscripts <- c()
	
	for (i in 1:nrow(s@results)) 	{
		
		#update progress
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				act.environment$pb$tick()
			}
		}
		
		#=== get transcript
		t <- NULL
		
		if (is.null(s@results$transcriptName[i])) {
			#transcript not found
			myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
			
		} else {
			t <- x@transcripts[[ s@results$transcriptName[i] ]]
			
			if (is.null(t)) {
				#transcript not found
				myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
			}
		}
		
		if (!is.null(t)) {
			#=== assemble_file NAME
			filename <- as.character(s@results[i, options()$act.export.filename.fromColumnName])
			
			if (!exists("filename")) {
				filename <- as.character(i)
			} else if (is.na(filename)) {
				filename <- as.character(i)
			} else if (length(filename)==0) {
				filename <- as.character(i)
			} else {
				if (filename == "") {filename <- as.character(i)}
			}
			
			#=== get start & end
			startSec 	<- max(0, s@results$startSec[i] - contextBeforeSec)
			endSec 		<- min(s@results$endSec[i] + contextAfterSec, t@length)
			
			#arrow
			if (!options()$act.transcript.arrow.insert) {
				arrowID <- -1
			} else {
				arrowID <- s@results$dataID[i]
			}
			
			#header
			header_heading 		<- ""
			header_firstinfo 	<- ""
			if (options()$act.transcript.header.insert==TRUE) {
				if (options()$act.transcript.header.heading.fromColumnName %in% colnames(s@results)) {
					header_heading <- as.character(s@results[i, options()$act.transcript.header.heading.fromColumnName])
				}
				if (options()$act.transcript.header.firstInfo.fromColumnName %in% colnames(s@results)) {
					header_firstinfo <- as.character(s@results[i, options()$act.transcript.header.firstInfo.fromColumnName])
				}
			}
			
			#assemble file PATH
			myFilepath <- ""
			if (destination_folder!="") {
				myFilepath <- file.path(destination_folder, "transcripts", paste(filename, ".txt", sep=""))
			} 
			
			printtrans <- snippets_printtranscript(   t = x@transcripts[[ s@results$transcriptName[i] ]],
													  startSec = startSec,
													  endSec = endSec,
													  header_heading 	 	 	= header_heading,  						   
													  header_firstinfo 	 	 	= header_firstinfo,  						 
													  insert_arrow_dataID 		= arrowID,								
													  filepath					= myFilepath )    						
			
			#insert into column
			if (!options()$act.transcript.insertIntoColumn=="") {
				#add  column if missing
				if (!options()$act.transcript.insertIntoColumn %in% colnames(s@results)) {
					s@results <- cbind(s@results, newcolumn=as.character(""), stringsAsFactors=FALSE)
					colnames(s@results)[ncol(s@results)] <- options()$act.transcript.insertIntoColumn
				}
				#insert transcript into search results
				output<-stringr::str_flatten(printtrans, collapse="\n")
				s@results[i, options()$act.transcript.insertIntoColumn] <- output
			}
			
			#cummulate transcripts
			alltranscripts<-c(alltranscripts, printtrans, "","")
			
		}
	} #next i
	
	#alltranscripts<-stringr::str_flatten(alltranscripts, collapse="\\n")
	
	# if output folder is given
	if (destination_folder!="") {
		
		#--- save cummulated transcripts as TXT
		myFilepath 	<- file.path(destination_folder, "transcripts_all.txt")
		fileConn 	<- file(myFilepath)
		writeLines(alltranscripts, fileConn)
		close(fileConn)
		
		#--- save modified results
		# R
		path_R 	    <-	file.path(destination_folder, "searchResults.RData")
		save(s, file = path_R)
		
		# CSV
		path_CSV 		<- file.path(destination_folder, "searchResults.csv")
		act::search_results_export(s, path_CSV, saveAsCSV = TRUE)
		
		# XLSX
		path_XLSX  <- file.path(destination_folder, "searchResults.xlsx")
		act::search_results_export(s, path_XLSX)
	}
	
	
	
	#=== print warnings
	if (!myWarnings=="") {
		warning(myWarnings)		
	}
	
	
	#=== give modified results back
	return(s@results)
}