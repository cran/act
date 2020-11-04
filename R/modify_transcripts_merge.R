#' Merge several transcripts
#' 
#' Merges several transcripts. One transcript is the destination transcript (the transcript that will be updated). 
#' The other transcripts are the update transcripts and contain the updates. 
#' The update transcripts need to contain a tier in which the update sections are marked.
#' 
#' You may chose between the following two options:
#' - The update sections in the destination transcript will first be erased completely and then the updates will be filled in.
#' - The update sections in the destination transcript will NOT be erased completely. Rater only the contents of tiers will be erased that are also present in the update tiers. e.g. if your destination transcript contains more tiers than the update transcripts, the contents of those tiers will be preserved in the destination tier during the update.
#' @param trans_destination Transcript object; transcript that serves as destination (and will receive the updates).
#' @param trans_updates List of transcript objects; transcript objects that will be inserted into the destination transcripts (entirely or in part).
#' @param identifier_tier Character string;  regular expression that identifies tier (in all trans_updates) in which the sections that will be inserted into trans_destination are marked.
#' @param identifier_intervall Character string; regular expression that identifies  (in trans_updates) the sections that will be inserted into trans_destination.
#' @param erase_update_sections_completely Logical; if \code{TRUE} update sections in destination transcript will be erased completely, if \code{FALSE} update sections in the destination tier will not be erased completely but only the tiers that are present in the trans_updates be erased.
#' 
#' @return Transcript object
#' @export
#'
#' @example inst/examples/modify_transcripts_merge.R
#' 
modify_transcripts_merge <- function (trans_destination, trans_updates, identifier_tier="update", identifier_intervall=".+", erase_update_sections_completely=TRUE) {
	#trans_destination 	<- corpus@transcripts[[1]]
	#trans_updates 		<- corpus@transcripts[2:length(corpus@transcripts)]
	#identifier_tier <- "update"
	#identifier_intervall <- ".+"
	#erase_update_sections_completely <- TRUE
	#identifier_tier <- NULL
	#identifier_intervall <- NULL
	
	
	#get data of destinaton transcript
	myData <- trans_destination@data
	
	#for all transcripts in updates
	for (trans_update in trans_updates) {
		#trans_update<-trans_updates[[1]]
		
		#names(trans_update@tiers)
		#names(trans_destination@tiers)
		
		#skip transcript if update is empty
		#get all data
		myDestIntervals <- trans_update@data
		
		#if tier identifier is specified, get first tiers that fits condition
		if (!is.null(identifier_tier)) {
			#get all intervals in this tier
			myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$tierName, identifier_tier), ]
			
			#if a specifier in this tier is set,  select those that fit condition
			if (!is.null(identifier_intervall)) {
				myDestIntervals <- myDestIntervals[stringr::str_detect(myDestIntervals$content, identifier_intervall), ]
			}
		} else {
			#auto create an update interval, starting with first and ending with last interval
			startSec <- min(myDestIntervals$startSec)
			endSec <- max(myDestIntervals$endSec)
			myDestIntervals <- myDestIntervals[1:1,]
			
			myDestIntervals$startSec[1] 	<- startSec
			myDestIntervals$endSec[1] 	<- endSec
		}
		
		if(nrow(myDestIntervals)>0) {
			#run through all update intervalls
			for (i in 1:nrow(myDestIntervals)) {
				myDestInterval <- myDestIntervals[i,]
				
				#get data form the update transcript
				myDataUpdate <- trans_update@data
				myDataUpdate <- myDataUpdate[((myDataUpdate$startSec>=myDestInterval$startSec & myDataUpdate$startSec<=myDestInterval$endSec) | (myDataUpdate$endSec>myDestInterval$startSec & myDataUpdate$endSec<=myDestInterval$endSec) | (myDataUpdate$startSec<myDestInterval$startSec & myDataUpdate$endSec>myDestInterval$endSec)), ]
				
				#---truncate intervals that do no fall entirely into the intervall
				#intervals that start before
				for (j in 1:nrow(myDataUpdate)) {
					myDataUpdate$startSec[j] <- max(myDataUpdate$startSec[j], myDestInterval$startSec)
				}
				
				#intervals that end after
				for (j in 1:nrow(myDataUpdate)) {
					myDataUpdate$endSec[j] <-min(myDataUpdate$endSec[j], myDestInterval$endSec)
				}
				
				#destination
				if (erase_update_sections_completely==TRUE) {
					#for for all tiers
					
					#destination: get data outside of update region
					myData <- myData[myData$startSec<myDestInterval$startSec | myData$endSec>myDestInterval$endSec,]
					
					#truncate intervals that reach into the update area
					myData$endSec[myData$startSec<myDestInterval$startSec & myData$endSec>myDestInterval$startSec] <- myDestInterval$startSec
					
					#truncate intervals that reach out of the update area
					myData$startSec[myData$startSec<myDestInterval$endSec & myData$endSec>myDestInterval$endSec] <- myDestInterval$endSec
					
				} else {
					#for tiers that are in update transcript
					
					#get IDs of data in tiers that are also in the update file
					myIDs_tiers <- which(myData$tierName %in% names(trans_update@tiers))
					myIDS_times <- which(myData$startSec>myDestInterval$startSec & myData$endSec<myDestInterval$endSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					
					#revove intervals that fall entirely into the update region
					if (length(myIDs_tiers_timesection)>0) {
						#remove intervals 
						myData <- myData[-myIDs_tiers_timesection,]
					}
					#truncate intervals that reach into the update area
					myIDs_tiers <- which(myData$tierName %in% names(trans_update@tiers))
					myIDS_times <- which(myData$startSec<myDestInterval$startSec & myData$endSec>myDestInterval$startSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					myData$endSec[myIDs_tiers_timesection] <- myDestInterval$startSec
					
					#truncate intervals that reach out of the update area
					myIDs_tiers <- which(myData$tierName %in% names(trans_update@tiers))
					myIDS_times <- which(myData$startSec<myDestInterval$endSec & myData$endSec>myDestInterval$endSec)
					myIDs_tiers_timesection <- intersect(myIDs_tiers, myIDS_times)
					myData$startSec[myIDs_tiers_timesection] <- myDestInterval$endSec
				}
				
				#merge data of update and destination transcript
				myData <- rbind(myData, myDataUpdate)
				
			}
		}
	}
	#assign new data to trasncript
	trans_destination@data <- myData
	
	
	#change name & path
	trans_destination@name <- paste(trans_destination@name, "_UPDATED",sep="")
	trans_destination@path<-""
	
	#---recaluculate tiers
	#get tier names and classes
	tierNames <- unique(as.character(myData$tierName))
	
	#try to get from transcript
	tierNamesInList <- names(trans_destination@tiers)
	if (!is.null(tierNamesInList)) {
		tierNames 		<- union(tierNamesInList, tierNames)
	}
	
	if (!is.null(identifier_tier)) {
		#start with identifier tier
		tierNames <- unique(c(identifier_tier, tierNames))
	}
	
	#class
	tierClassesInList <- unlist(trans_destination@tiers)
	names(tierClassesInList)<-NULL
	
	#add Classes if missing, assume they are interval tiers
	
	if (is.null(tierClassesInList))
	{
		#if no classes are found at all
		tierClasses <- rep("IntervalTier", length(tierNames))
		
	} else	{
		
		#if some are found, add only classes for the ones that are missing
		missing <- length(tierNames)-length(tierClassesInList)
		if (missing>0)
		{
			tierClassesInList <- c(tierClassesInList,rep("IntervalTier", missing))
		}
		tierClasses <- tierClassesInList
	}
	
	
	names(tierClasses) <- tierNames
	trans_destination@tiers<-tierClasses
	
	return(trans_destination)
}

