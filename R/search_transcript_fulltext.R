#' Search in full text of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' @param showProgress Logical; if \code{TRUE} progress bar will be shown.
#' 
#' @return Data.frame; data frame with search results.
#' 
#'   
#' # @example inst/examples/search_transcript_fulltext.R
#' 
search_transcript_fulltext<- function(t, s, showProgress=TRUE) {
	if (is.null(t)) 				{	stop("Transcript object is null.")		}
	
	#progress
	if (showProgress)		{	
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				act.environment$pb$tick()
			}
		}
	}
	
	mySearchResults 	 	<- NULL
	mySearchResults.byTime	<- NULL
	mySearchResults.byTier	<- NULL
	
	#== helper function: get the numbers of the record set where the hit starts
	getRecordsetForHit <- function(hit.length, start, end)	{
		#compare the position of the match with the cummulated positions 	= where the recordset ENDS in the big text
		# start <- myData$char.norm.bytime.start
		# end <- myData$char.norm.bytime.end
		# XXX XXX min(which(hit.length<end))
		
		#this gives an arror for hits that are length==1
		#which(hit.length>=start & hit.length<end)[1]
		which(hit.length>=start & hit.length<=end)[1]
	}
	
	#== helper function: detect the extension of a hit, based on separators
	detectHitSpan <- function (myhit) {
		if (is.na(myhit)) 	{ return ("error")}
		if (myhit=="") 		{ return ("error")}
		
		#=== across tiers
		#check if hit contains separator
		p <- options()$act.separator_between_tiers
		if (stringr::str_detect(myhit, stringr::fixed(p))) {
			#check if separator ist not at the beginning of the hit
			results <- as.data.frame(stringr::str_locate_all(myhit, stringr::fixed(p)))
			
			#get rid of sepearators at the beginning
			results <- results[!(results$start==1),]
			
			#get rid of separators at the end
			results <- results[!(results$end==nchar(myhit)),]
			
			#if there is a hit left
			if (nrow(results)>0) {
				return("across tiers")
			}
		}
		
		#=== across intervals
		p <- options()$act.separator_between_intervals
		if (stringr::str_detect(myhit, stringr::fixed(p))) {
			#check if separator ist not at the beginning of the hit
			results <- as.data.frame(stringr::str_locate_all(myhit, stringr::fixed(p)))
			
			#get rid of separators at the beginning
			results <- results[!(results$start==1),]
			
			#get rid of sepearators at the end
			results <- results[!(results$end==nchar(myhit)),]
			
			#if there is a hit left
			if (nrow(results)>0) {
				return("across intervals")
			}
		}
		return("within interval")
	}
	
	#================================ By TIME
	if (s@searchMode=="fulltext" | s@searchMode=="fulltext.byTime") {
		#=== get full text
		if (s@searchNormalized==TRUE)  {
			myFulltext <- t@fulltext.bytime.norm
		} else {
			myFulltext <- t@fulltext.bytime.orig
		}
		
		#=== check if fulltext is given
		continue <- TRUE
		if (length(myFulltext) == 0)  	{
			continue <- FALSE
		} else {
			if (is.na(myFulltext) == TRUE)  {
				continue <- FALSE
			}
		}
		
		if (continue) {
			#check if there are results
			if (stringr::str_detect(myFulltext, s@pattern))	{
				#=== get the text and info of the matches
				hit 		  		<- 	unlist(stringr::str_extract_all(myFulltext, s@pattern))
				hit.length			<- 	stringr::str_length(hit)
				hit.nr		  		<-	c(1:length(hit))
				hit.pos.fulltext	<- 	data.frame(stringr::str_locate_all(myFulltext, s@pattern))$start
				
				#=== get original data
				myData <- t@data
				
				if (s@searchNormalized==TRUE) 	{
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myData$char.norm.bytime.start, end = myData$char.norm.bytime.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTime 	<-	myData[matches.recordsetNrs,]
					rm(myData)
					
					# calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTime$char.norm.bytime.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTime$char.norm.bytime.end - mySearchResults.byTime$char.norm.bytime.start - nchar(mySearchResults.byTime$content.norm) + 1)
				} else {
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myData$char.orig.bytime.start, end = myData$char.orig.bytime.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTime 	<-	myData[matches.recordsetNrs,]
					rm(myData)
					
					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTime$char.orig.bytime.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTime$char.orig.bytime.end - mySearchResults.byTime$char.orig.bytime.start - nchar(mySearchResults.byTime$content) + 1)
				}
				
				
				#=== calculate if hit is across tiers
				hit.span 			<- unlist(lapply(hit, detectHitSpan))
				
				#=== add further columns
				mySearchResults.byTime 	<-	cbind(mySearchResults.byTime, hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content, searchMode="byTime", hit.span=hit.span)
				rowNumbers				<-	row.names(mySearchResults.byTime)
			}
		}
	}
	
	#================================ By TIER
	if (s@searchMode=="fulltext" | s@searchMode=="fulltext.byTier") {
		#=== get full text
		if (s@searchNormalized==TRUE)  {
			myFulltext <- t@fulltext.bytier.norm
		} else {
			myFulltext <- t@fulltext.bytier.orig
		}
		
		#=== check if fulltext is given
		continue <- TRUE
		if (length(myFulltext) == 0)  	{
			continue <- FALSE
		} else {
			if (is.na(myFulltext) == TRUE)  {
				continue <- FALSE
			}
		}
		
		if (continue) {
			#check if there are results
			if (stringr::str_detect(myFulltext, s@pattern))	{
				#=== get the text and info of the matches
				hit 		  		<- 	unlist(stringr::str_extract_all(myFulltext, s@pattern))
				hit.length			<- 	stringr::str_length(hit)
				hit.nr		  		<-	c(1:length(hit))
				hit.pos.fulltext	<- 	data.frame(stringr::str_locate_all(myFulltext, s@pattern))$start
				
				#=== get original data
				myData <- t@data
				
				if (s@searchNormalized==TRUE) 	{
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myData$char.norm.bytier.start, end = myData$char.norm.bytier.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTier 	<-	myData[matches.recordsetNrs,]
					rm(myData)
					
					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTier$char.norm.bytier.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTier$char.norm.bytier.end - mySearchResults.byTier$char.norm.bytier.start - nchar(mySearchResults.byTier$content.norm )+ 1)
					
				} else {
					#calculate the interval that contains the hit
					matches.recordsetNrs <- sapply(hit.pos.fulltext, getRecordsetForHit, start = myData$char.orig.bytier.start, end = myData$char.orig.bytier.end)
					
					#select the recordsets that contain the match
					mySearchResults.byTier 	<-	myData[matches.recordsetNrs,]
					rm(myData)
					
					#calculate position start of hit in content
					hit.pos.content <- hit.pos.fulltext - mySearchResults.byTier$char.orig.bytier.start + 1
					
					#subtract length of separator
					hit.pos.content <- hit.pos.content - (mySearchResults.byTier$char.orig.bytier.end - mySearchResults.byTier$char.orig.bytier.start - nchar(mySearchResults.byTier$content)+ 1)
				}
				
				#=== calculate if hit is across tiers
				hit.span 	<- unlist(lapply(hit, detectHitSpan))
				
				#=== add further columns
				mySearchResults.byTier 	<-	cbind(mySearchResults.byTier, hit, hit.nr, hit.length, hit.pos.fulltext, hit.pos.content, searchMode="byTier", hit.span=hit.span)
				rowNumbers				<-	row.names(mySearchResults.byTier)
				
				#=== delete results that are across tiers
				mySearchResults.byTier<- mySearchResults.byTier[mySearchResults.byTier$hit.span!="across tiers", ]
			}
		}
	}
	
	#=== get rid of duplicated results
	#only by time
	if (!is.null(mySearchResults.byTime) & is.null(mySearchResults.byTier)) {
		mySearchResults <- mySearchResults.byTime
		
		#only by Tier
	} else if (is.null(mySearchResults.byTime) & !is.null(mySearchResults.byTier)) {
		mySearchResults <- mySearchResults.byTier
		
		#both
	} else if (!is.null(mySearchResults.byTime) & !is.null(mySearchResults.byTier) ) {
		#merge the results of both searches
		mySearchResults <- rbind(mySearchResults.byTime, mySearchResults.byTier)
		
		#delete double hits
		compare <- mySearchResults[, c("transcriptName", "dataID", "hit.pos.content")]
		
		doubles <- duplicated(compare)
		mySearchResults <- mySearchResults[!doubles,]
		
	} else {
		mySearchResults <- NULL
	}
	
	#=== filter by time
	#--- time section
	if (length(s@startSec)!=0) {
		if (!is.na(s@startSec)) {
			mySearchResults<- mySearchResults[(mySearchResults$endSec>=s@startSec), ]
		}
	}
	if (length(s@endSec)!=0) {
		if (!is.na(s@endSec)) {
			mySearchResults<- mySearchResults[(mySearchResults$startSec<s@endSec), ]
		}
	}
	
	if(	is.null(mySearchResults)) {
		myColNames<-c("dataID", "transcriptName","tierName", "startSec","endSec", "content", "content.norm", "char.orig.bytime.start", "char.orig.bytime.end", "char.norm.bytime.start", "char.norm.bytime.end", "char.orig.bytier.start", "char.orig.bytier.end", "char.norm.bytier.start", "char.norm.bytier.end", "hit", "hit.nr" ,"hit.length", "hit.pos.fulltext", "hit.pos.content", "searchMode", "hit.span")
		mySearchResults <- data.frame(matrix(ncol = length(myColNames), nrow = 0))
		colnames(mySearchResults) <- myColNames	
	}
	
	#=== return results
	return(mySearchResults)
}

#' Search in original content of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' @param showProgress Logical; if \code{TRUE} progress bar will be shown.
#' 
#' @return Data.frame; data frame with search results.
#' @export
#' 
#' @example inst/examples/search_transcript_content.R
#' 
search_transcript_content <- function(t, s, showProgress=TRUE) {
	if (is.null(t)) 				{	stop("Transcript object is null.")		}
	
	#=== update progress
	if (showProgress==TRUE) {	
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				act.environment$pb$tick()
			}
		}
	}
	
	temp <- NULL
	
	#=== get matches
	myData <-t@data
	
	#=== filter 
	#---tiers
	if(length(s@filter.tiers.exclude)==0) {s@filter.tiers.exclude<-""}
	if(length(s@filter.tiers.include)==0) {s@filter.tiers.include<-""}
		
	include <- c(1:length(myData$content))
	#if any filter is set
	if (!s@filter.tiers.exclude=="" | !s@filter.tiers.include=="" ) 	{
		#if include filter is set
		if (s@filter.tiers.include!="") {
			include <- grep(s@filter.tiers.include, myData$tierName, ignore.case =TRUE, perl = TRUE)
		}
		
		#if exclude filter is set
		if (!s@filter.tiers.exclude=="") {
			exclude	<- grep(s@filter.tiers.exclude, myData$tierName, ignore.case =TRUE, perl = TRUE)
			include <- setdiff(include, exclude)
		}
		myData <- myData[include,]
	}
	
	#--- time section
	if (length(s@startSec)!=0) {
		if (!is.na(s@startSec)) {
			myData<- myData[(myData$endSec>=s@startSec), ]
		}
	}
	if (length(s@endSec)!=0) {
		if (!is.na(s@endSec)) {
			myData<- myData[(myData$startSec<s@endSec), ]
		}
	}
	
	if (!is.null(myData)) {
		if (s@searchNormalized==TRUE) {
			if (is.na(myData$content.norm[1]))				{
				matches.df    <- NULL
			} else {
				indices 	<- stringr::str_detect(myData$content.norm, s@pattern)
				if (!any(indices)) {
					matches.df    <- NULL
				} else {
					hits.pos			<- stringr::str_locate_all(myData$content.norm[indices], s@pattern)
					hits.count 			<- stringr::str_count(myData$content.norm[indices], s@pattern)
					hits.match			<- stringr::str_extract_all(myData$content.norm[indices], s@pattern)
					dataID 				<- myData$dataID[indices]
					matches.df 			<- cbind(dataID=dataID[1], hits.pos[[1]], hit.nr=1, hit=hits.match[[1]])
					if (length(hits.pos)>1) {
						for(j in 2:length(hits.pos)) {
							matches.df <- rbind(matches.df, cbind(dataID=dataID[j], hits.pos[[j]], hit.nr=j, hit=hits.match[[j]]))
						}
					}
					colnames(matches.df)[2] <-"hit.pos.content"
				}
			}
		} else {
			if (is.na(myData$content[1]))				{
				matches.df    <- NULL
			} else {
				indices 	<- stringr::str_detect(myData$content, s@pattern)
				if (!any(indices)) {
					matches.df    <- NULL
				} else {
					hits.pos   			<- stringr::str_locate_all(myData$content[indices], s@pattern)
					hits.count 			<- stringr::str_count(myData$content[indices], s@pattern)
					hits.match			<- stringr::str_extract_all(myData$content[indices], s@pattern)
					dataID 				<- myData$dataID[indices]
					matches.df 			<- cbind(dataID=dataID[1], hits.pos[[1]], hit.nr=1, hit=hits.match[[1]])
					if (length(hits.pos)>1) {
						for(j in 2:length(hits.pos)) {
							matches.df <- rbind(matches.df, cbind(dataID=dataID[j], hits.pos[[j]], hit.nr=j, hit=hits.match[[j]]))
						}
					}
					colnames(matches.df)[2] <-"hit.pos.content"
				}
			}
		}
		
		if (!is.null(matches.df)) {
			if (nrow(matches.df)>0)	{
				#turn matrix into data frame
				sResults <- as.data.frame(matches.df)
				
				#add column with length of hit
				sResults <- cbind(sResults, hit.length=as.numeric(stringr::str_length(sResults$hit)))
				
				#add columns: hit.pos.fulltext, searchMode, hit.span
				sResults <- cbind(sResults, hit.pos.fulltext=as.numeric(NA), searchMode=as.character("byTier"), hit.span=as.character("within interval"))
				
				#turn factors into vectors
				sResults$dataID    		   	<-  as.numeric(sResults$dataID) # as.numeric(levels(sResults$dataID))[sResults$dataID]
				sResults$hit       		   	<-   as.character(sResults$hit) #as.character(levels(sResults$hit))[sResults$hit]
				sResults$hit.pos.content	<-   as.numeric(sResults$hit.pos.content) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$end				<-   as.numeric(sResults$end) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.nr				<-   as.numeric(sResults$hit.nr) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.length			<-   as.numeric(sResults$hit.length) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				sResults$hit.pos.fulltext	<-   as.numeric(sResults$hit.pos.fulltext) #as.numeric(levels(sResults$hit.pos.content))[sResults$hit.pos.content]
				
				#merge results and data by column 
				temp         <- merge(x=myData, y=sResults , by.x = "dataID", by.y ="dataID", all.y = TRUE)
			}
		}
	}
	return(temp)
}

