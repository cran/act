#' Search in original content of a single transcript
#'
#' @param t Transcript object; transcript to search in.
#' @param s Search object.
#' @param showProgress Logical; if \code{TRUE} progress bar will be shown.
#' 
#' @return Data.frame; data frame with search results.
#'  
#' # @example inst/examples/search_transcript_content.R
#' 
search_transcript_content <- function(t, s, showProgress=TRUE) {
	if (is.null(t)) 				{	stop("Transcript object is null.")		}

	
	if (showProgress==TRUE) {	
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				if (!act.environment$pb$finished) {			act.environment$pb$tick()}
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
