#' Create a new search
#'
#' Creates a new search object and runs the search in a corpus object.
#' Only 'x' and 'pattern' are obligatory. 
#' The other arguments can be left to their default values.
#'
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' @param searchMode Character string; takes the following values: content, fulltext (=default, includes both fulltext modes ), fulltext.byTime, fulltext.byTier.
#' @param searchNormalized Logical; if \code{TRUE} function will search in the normalized content, if \code{FALSE} function will search in the original content.
#' @param makeConcordance Logical; if \code{TRUE} concordance will be added to search results.
#' @param prefix Character string; prefix for the name of the consecutively numbered search results.
#' @param filterTranscriptsInclude Character string; as regular expression, limit search to certain transcripts matching the expression.
#' @param filterTranscriptsExclude Character string; as regular expression, exclude certain transcripts matching the expression.
#' @param filterTiersInclude Character string; as regular expression, limit search to certain tiers matching the expression.
#' @param filterTiersExclude Character string; as regular expression, exclude certain tiers matching the expression.
#' @param startSec Double; start time of region for search.
#' @param endSec Double; end time of region for search. 
#' @param runSearch Logical; if \code{TRUE} search will be run in corpus object, if \code{FALSE} only the search object will be created.
#' @param showProgress Logical; if \code{TRUE} progress bar will be shown.
#' @param updateX Logical; If \code{TRUE} the original corpus object passed as x to the function will also be updated! If you do not want this, set to \code{FALSE}. 
#' 
#' @return Search object.
#' @export
#'
#' @example inst/examples/search_new.R
#' 
search_new <- function(x, pattern, searchMode="fulltext", searchNormalized=TRUE, filterTranscriptsInclude=NA, filterTranscriptsExclude=NA, filterTiersInclude=NA, filterTiersExclude=NA, startSec=NA, endSec=NA,  makeConcordance=TRUE, prefix="result", runSearch=TRUE, showProgress=TRUE, updateX=TRUE) {
	#=== capture original x
	captured_x <- substitute(x)
	original_x <- x
	
	start.time <- Sys.time()

	#=== check x object
	if (is.null(x)) 				{	stop("Corpus object x is null.")					}
	if (is.null(x@transcripts)) 	{	stop("No transcripts found in corpus object x.")	}
	if (is.null(pattern)) 	        {	stop("Search pattern x is null.")					}
	
	#=== create search object
	s <- methods::new("search")
	s@pattern                   <- pattern
	s@searchMode                <- searchMode
	s@searchNormalized          <- searchNormalized
	s@startSec                  <- if(!is.na(startSec)) {if(!is.null(startSec)) {startSec}} else {s@startSec}
	s@endSec                    <- if(!is.na(endSec))   {if(!is.null(endSec))   {endSec}}   else {s@endSec}
	s@filter.transcripts.include<- if(!is.na(filterTranscriptsInclude))   {if(!is.null(filterTranscriptsInclude))   {filterTranscriptsInclude}}   else {s@filter.transcripts.include}
	s@filter.transcripts.exclude<- if(!is.na(filterTranscriptsExclude))   {if(!is.null(filterTranscriptsExclude))   {filterTranscriptsExclude}}   else {s@filter.transcripts.exclude}
	s@filter.tiers.include      <- if(!is.na(filterTiersInclude))   {if(!is.null(filterTiersInclude))   {filterTiersInclude}}   else {s@filter.transcripts.include}
	s@filter.tiers.exclude      <- if(!is.na(filterTiersExclude))   {if(!is.null(filterTiersExclude))   {filterTiersExclude}}   else {s@filter.transcripts.exclude}
	
	#s@cutlist.mac               <- 
	#s@cutlist.win               <- 
	
	s@corpus                    <- x@name
	s@makeConcordance           <- makeConcordance
	s@prefix                    <- prefix
	
	#s@results                   <- 
	#s@results.nr                <- 
	#s@results.tiers.nr          <- 
	#s@results.transcripts.nr    <- 

	#=== run the search
	if (runSearch) {
		s <- act::search_run(x=x, s=s, showProgress=TRUE)
	}

	#if corpus object has changed
	if (!identical(original_x,x)) {
		#assign to original corpus object
		if (updateX) {
			p <- parent.frame() 
			p[[deparse(captured_x)]] <- x
		}	
	}
	
	#=== return the results
	return(s)
}
	