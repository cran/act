# Make names for search results
#
# @param mySearchResults Data.frame; data frame containing search results.
# @param prefix Character string; prefix for the name of the consecutively numbered search results.
# 
# @return Vector of character strings; names created for the search results.
# @export
#
# @examples
# library(act)

# # Search 
# myRegEx <- "yo"
# searchresults <- act::search_corpus(examplecorpus, pattern=myRegEx, makeConcordance=FALSE)
# 
# # Make custom names
# mynames <- act::search_names(searchresults, prefix="yo")
# 
# # Replace old names in search by new names
# searchresults$resultID <- mynames
helper_makeNamesForSearch <- function(mySearchResults, prefix="result") {
	myFormat <- paste(prefix, "%0", nchar(toString(nrow(mySearchResults))), "d", sep="")
	myNames <- sprintf(myFormat, 1:nrow(mySearchResults))
	return (myNames)
}


# Gets the path of a '*.TextGrid' for a transcript
#
# Returns either the path to the original '*.TextGrid' file or to a temporary TextGrid created on the fly.
# 
# @param t transcript object; transcript for which you want to get the TextGrid
#
# @return Character string; path to TextGrid file.
# 
# 
# @examples
# print("")
#
helper_getTextGridForTranscript <- function(t=NULL) {
	if (is.null(t))	{
		stop("Transcript object needs to be specified.")
	}
	
	#=== check in corpus object if textgrid is given and exits
	if (!is.na(t@path)) {
		if (file.exists(t@path)) {
			if (stringr::str_to_lower(tools::file_ext(t@path))=="textgrid") {
				return(t@path)
			}
		}
	}
	
	#=== create temporary textgrid
	path <- file.path(tempdir(), stringr::str_c(t@name, ".TextGrid", collapse=""))
	act::textgrid_export(t, path)
	warning("Original TextGrid has not been found. A temporary TextGrid has been created")
	return(path)
}

# Formats seconds as character string
#
# @param t Double; seconds.
#
# @return  Character string.
#
# @noRd
#
# @examples
# print("")
# 
helper_format_time <- function (t) {
	h<- floor(t/3600)
	m <- t-(h*3600)
	m <- floor(m/60)
	s <- t-(h*3600)-(m*60)
	s <- round(s,0)
	if (t<60) {
		f <- paste(round(t,3)," sec", sep="")
	} else {
		if (h>0) {
			f <- sprintf("%02.fhrs %02.fmin %02.fsec", h, m, s)
		} else if (m>0) {
			f <- sprintf("%2.fmin %02.fsec", m, s)
		}
		f <- paste(f, " (=",round(t,3)," sec)", sep="")
	}
	return(f)
}


helper_test_read <- function(input_path, testencoding, testlinenrs) {
	assign("last.warning", NULL, envir = baseenv())
	input_path<-toString(input_path)
	tryCatch(
		{
			myCon <- file(input_path, encoding = testencoding)
			myLines <- readLines(myCon, n = testlinenrs)
			close(myCon)
			return (myLines)
		},

		error = function(c)
		{
			close(myCon)
			"error"
		},

		warning = function(c)
		{
			close(myCon)
			"error"
			#paste("warning:", warnings())
		},
		message = function(c)
		{
			"error"
			close(myCon)
			#"message"
		}
	)
}

helper_checkPraatScriptFolder <- function() {
	#--- check praat script folders
	myPath<-options()$act.folder.praatScripts
	if (is.null(myPath)) { 		myPath <- "" 	}
	if (!file.exists(myPath)) {
		#if folder has not been found
		
		#set to standard value and try again
		options(act.folder.praatScripts	= system.file("extdata", "praat", package="act"))
		myPath<-options()$act.folder.praatScripts
		if (is.null(myPath)) {  			myPath <- ""  		}
		
		if (file.exists(myPath)) {
			#warning("Note: The path to the 'praat script folder' has been set to the default location.")
		} else {
			stop("There has been a problem locating the 'praat script folder'. Please install the act package again.")
		}
	}
	return(myPath)
}


