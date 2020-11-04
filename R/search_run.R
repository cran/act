#' Run a search
#'
#' Runs a search, based on an existing search object s, in a corpus object x.
#' 
#' @param x Corpus object.
#' @param s Search object.
#' @param showProgress Logical; if \code{TRUE} progress bar will be shown.
#' @param updateX Logical; If \code{TRUE} the original corpus object passed as x to the function will also be updated, in case that fulltexts are created or the normalization is performed. If you do not want this, set to \code{FALSE}. 
#'  
#' @return Search object.
#' @export
#'
#' @example inst/examples/search_run.R
#' 
search_run <- function(x, s, showProgress=TRUE, updateX=TRUE) {
	captured_x <- substitute(x)
	original_x <- x
	
	temp <- NULL
	start.time <- Sys.time()
	

	#=== normalization needed?
	if (s@searchNormalized & !x@normalization.upToDate) {	
		x <- normalize(x)
		x@fulltext.upToDate <- FALSE
	}
		
	#=== fulltext update needed ?
	if (s@searchMode=="fulltext" | s@searchMode=="fulltext.byTime" | s@searchMode=="fulltext.byTier" )  {

		#check if filters have been changed
		if (x@fulltext.currentlyset.filter.tiers.include!=s@filter.tiers.include|
			x@fulltext.currentlyset.filter.tiers.exclude!=s@filter.tiers.exclude) {
		
			x@fulltext.currentlyset.filter.tiers.include<-s@filter.tiers.include
			x@fulltext.currentlyset.filter.tiers.exclude<-s@filter.tiers.exclude 
		
			x@fulltext.upToDate <- FALSE
		}
		
		
		# if fulltext is outdated
		if (!x@fulltext.upToDate) {
			x <- act::fulltext_update(x)
		}
	}

	#==filter transcripts
	if (!is.na(s@filter.transcripts.include)) {
		if (s@filter.transcripts.include!="") {
			x@transcripts <- x@transcripts[grep(pattern=s@filter.transcripts.include, names(x@transcripts))]
		}
	}
	if (!is.na(s@filter.transcripts.exclude)) {
		if(s@filter.transcripts.exclude!="") {
			x@transcripts <- x@transcripts[-grep(pattern=s@filter.transcripts.exclude, names(x@transcripts))]
		}
	}
	
	#=== full text or content
	#progress
	if (showProgress) {
		act.environment$pb <- progress::progress_bar$new(
			format = "  Searching          [:bar] :percent missing: :eta",
			total = length(x@transcripts), 
			clear = FALSE, 
			show_after = 0,
			width= 60)
	}
	
	if (s@searchMode=="fulltext" | s@searchMode=="fulltext.byTime" | s@searchMode=="fulltext.byTier" ) {
		temp 	  			<-	lapply(x@transcripts, search_transcript_fulltext, s=s, showProgress=showProgress)
		temp	  			<-	do.call("rbind", temp)
		
	} else if (s@searchMode=="content" ) {
		temp 	  			<-	lapply(x@transcripts, search_transcript_content, s=s, showProgress=showProgress)
		temp	  			<-	do.call("rbind", temp)
	} else {
		#=== some user error
		stop ("Unknow 'searchMode'. Please select 'fulltext', 'fulltext.byTime', 'fulltext.byTier' or 'content' .")
	}
	
	if(is.null(temp)) {
		myColNames<-c("dataID", "transcriptName","tierName", "startSec","endSec", "content", "content.norm", "char.orig.bytime.start", "char.orig.bytime.end", "char.norm.bytime.start", "char.norm.bytime.end", "char.orig.bytier.start", "char.orig.bytier.end", "char.norm.bytier.start", "char.norm.bytier.end", "hit", "hit.nr" ,"hit.length", "hit.pos.fulltext", "hit.pos.content", "searchMode", "hit.span")
		temp <- data.frame(matrix(ncol = length(myColNames), nrow = 0))
		colnames(temp) <- myColNames	
	}
	
	#=== keep only some columns
	keep <- c("dataID","transcriptName","tierName","startSec","endSec","content","content.norm", "hit","hit.nr","hit.length", "hit.pos.content", "hit.pos.fulltext", "searchMode", "hit.span")
	temp <- temp[ , keep]
	
	#=== make  adaptations and concordance
	if (nrow(temp)>0)	{
		if (s@makeConcordance==TRUE)	{
			if (showProgress==TRUE) {
				act.environment$pb <- progress::progress_bar$new(
					format = "  Concor dancing     [:bar] :percent missing: :eta",
					total = max(1,nrow(temp)), 
					clear = FALSE,
					show_after = 0,
					width= 60)
			}
			s@results <- temp
			
			if ( s@searchMode=="content" ) {
				concs		<- act::search_concordance(x, s, baseForConcordance="content", searchNormalized=s@searchNormalized, showProgress = TRUE)
			} else {
				concs		<- act::search_concordance(x, s, baseForConcordance="fulltext", searchNormalized=s@searchNormalized, showProgress = TRUE)
			}
			temp <- cbind(temp, concs)
		}
		
		#=== add names for results
		resultID      <- 	helper_makeNamesForSearch(temp, s@prefix)
		
		#=== bind together results and concordance
		temp     	  <- 	cbind(resultID, temp)
		
		#=== turn factors into strings
		fctr.cols <- sapply(temp, is.factor)
		temp[, fctr.cols] <- sapply(temp[, fctr.cols], as.character)
	}
	
	#=== how long did it take?
	#end.time <- Sys.time()
	#if  (is.null(temp)==TRUE) {
	# d <- round(as.numeric(difftime(end.time, start.time, units = "secs")),2) #c("auto", "secs", "mins", "hours", "days", "weeks"))
	#}   else   {
	#	d <- round(as.numeric(difftime(end.time, start.time, units = "secs")),2) #c("auto", "secs", "mins", "hours", "days", "weeks"))
	#}		

	
	#if corpus object has changed
	if (!identical(original_x,x)) {
		#assign to original corpus object
		if (updateX) {
			p <- parent.frame() 
			p[[deparse(captured_x)]] <- x
		}	
	}
	
	s@results	             <- temp
	s@results.nr             <- nrow(temp)
	s@results.tiers.nr       <- length(unique(temp$tierName))
	s@results.transcripts.nr <- length(unique(temp$transcriptName))
	s@corpus                 <- x@name
	return(s)
}


