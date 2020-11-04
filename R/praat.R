#' Search corpus and open first result in Praat
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It first searches your corpus object and uses the first search hit. 
#' The corresponding TextGrid will be opened in the 'Praat' TextGrid Editor and the search hit will be displayed.
#' 
#' To make this function work you need to set the path to the 'sendpraat' executable using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' 
#' @export
#'
#' @examples
#' library(act)
#' 
#' # You can only use this functions if you have located the 'sendpraat' executable properly
#' # in the package options.
#' \dontrun{
#' act::praat_search_and_open(x=examplecorpus, "pero")
#' }
#' 
#' 
#' 
praat_search_and_open <- function(x, pattern) {
	s <- act::search_new(x=x, pattern=pattern, makeConcordance=FALSE)
	if (is.null(s@results)) {
		#No results
		#return(NULL)
	} else if (nrow(s@results)==0) {
		#No results.
		#return(NULL)
	} else {
		praat_open_result(x, s@results[1,])
	}
}

#' Open a search result in 'Praat'
#'
#' The function remote controls 'Praat' by using 'sendpraat' and a 'Praat' script. 
#' It opens a search result in the 'Praat' TextGrid Editor.
#' 
#' To make this function work you need to set the path to the 'sendpraat' executable using 'options(act.path.sendpraat = ...)'.
#' 
#' @param x Corpus object.
#' @param result Search result from a search. 
#'
#' @export
#'
#' @examples
#' library(act)

#' mysearch <- act::search_new(x=examplecorpus, pattern = "pero")
#' 
#' # You can only use this functions if you have located the 'sendpraat' executable properly
#' # in the package options.
#' \dontrun{
#' act::praat_open_result(x=examplecorpus, result=mysearch@results[1,])
#' }
praat_open_result  <- function(x, result) {
	helper_checkPraatScriptFolder()
	# result <- mysearch@results[1,]
	# x<-examplecorpus
	# praat_open_result(x, searchresults[1,])

	#--- check for sendpraat
	if (file.exists(options()$act.path.sendpraat)==FALSE)	{
		stop("Sendpraat not found. Please indicate the location of sendpraat in 'options(act.path.sendpraat = ...)'.")
	}

	#--- get  corresponding transcript
	t <- x@transcripts[[result$transcriptName]]
	if (is.null(t))	{
		stop("Transcript not found in corpus object'.")
	}
	
	#--- get path of textgrid
	path_textgrid <- helper_getTextGridForTranscript(t)
	name_textgrid <- tools::file_path_sans_ext(basename(path_textgrid))
	#replace blanks by underscores, as praat does
	name_textgrid	<- stringr::str_replace_all(string = t@name, pattern=" ", replacement="_")
	
	#---get path of sound
	path_longsound <- media_getpath(t, filterFile=".*\\.(wav|mp3|aif|aiff)") 
	if (is.null(path_longsound))	{
		name_longsound <-""
		path_longsound <-""
		warning("No media file(s) found.")
	} else {
		name_longsound <- path_longsound
		if (nchar(path_longsound)>=0) {
			name_longsound      <- sub("[.][^.]*$", "", basename(path_longsound))
		}
		#replace blanks by underscores, as praat does
		name_longsound  <- stringr::str_replace_all(string = name_longsound, pattern=" ", replacement="_")
	}
	
	#--- get path to praat script
	praatScriptPath	<-	file.path(options()$act.folder.praatScripts, "OpenSelectionInPraat.praat")

	#read script
	tx <- readLines(con= praatScriptPath, n=-1, warn=FALSE)
	
	#set values of variables
	
	tx  <- stringr::str_replace_all(string = tx, pattern = "PATHTEXTGRID",  replacement = path_textgrid)
	tx  <- stringr::str_replace_all(string = tx, pattern = "PATHLONGSOUND", replacement = path_longsound)
	tx  <- stringr::str_replace_all(string = tx, pattern = "SELSTARTSEC",   replacement = as.character(result$startSec))
	tx  <- stringr::str_replace_all(string = tx, pattern = "SELENDSEC",     replacement = as.character(result$endSec))

	#write temporary script
	tempScriptPath <- file.path(tempdir(), "temp.praat")
	writeLines(tx, con=tempScriptPath)
	
	#send script
	cmd <- sprintf("%s praat \"runScript: \\\"%s\\\" \"", options()$act.path.sendpraat, tempScriptPath)
	result=system(cmd, intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
	
	
	# if execution of sendpraat resulted in an error, try to start praat
	if (!is.null(attributes(result)) ) {
		if (file.exists(options()$act.path.praat)==FALSE)	{
			stop("Praat is not running. Please start Praat first. To start Praat automatically indicate its location 'options(act.path.praat = ...)'.")
		}
		result = system(paste ("open" , options()$act.path.praat), intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
		result = system(cmd, intern=TRUE, ignore.stderr = TRUE, ignore.stdout=TRUE)
	}

	#delete temporary script
	if (file.exists(tempScriptPath))  {file.remove(tempScriptPath)}
	
}
