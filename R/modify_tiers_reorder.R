#' Reorder tiers in transcripts
#'
#' Reorders the positions of tiers in all transcripts of a corpus object. The ordering of the tiers will be done according to a vector of regular expressions.
#' 
#' @param x Corpus object.
#' @param sortVector Vector of one or more character strings; strings are regular expressions, the order within the vector presents the new order of the tiers. Use \* (=two backslashes and a star) to indicate where tiers that are not present in the sort vector but in the transcript should be inserted.
#' @param filterTranscriptsInclude Character string; only transcripts that match the regular expression will be affected. Use an empty string for processing all transcripts.
#' @param addMissingTiers Logical; if \code{TRUE} all tiers that are given in the sort vector but are missing in the transcript will be added.
#' @param deleteTiersThatAreNotInTheSortVector Logical; if \code{TRUE} tiers that are present in the original transcript but not in the sort vector will be deleted.
#'
#' @return Corpus object.
#' @export
#'
#' @example inst/examples/modify_tiers_reorder.R
#'                            
modify_tiers_reorder <- function(x, sortVector, filterTranscriptsInclude="", addMissingTiers=FALSE, deleteTiersThatAreNotInTheSortVector=FALSE) {
	#reset corpus log
	x@lastModification <- list()

	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()

	#for (t in corpus@transcripts) {
	for (i in 1:length(x@transcripts)) {

		#reset transcript log
		x@transcripts[[i]]@lastModification <- list()
		x@transcripts[[i]]@modified <- FALSE

		if (filterTranscriptsInclude=="") {
			processThisTranscript <- TRUE
		} else {
			#if transcript name matches the rexex filter
			if(stringr::str_detect(x@transcripts[[i]]@name, filterTranscriptsInclude)) {
				processThisTranscript <- TRUE
			}else {
				processThisTranscript <- FALSE
			}
		}

		#if this transcript is to be processed
		if (processThisTranscript) {
			tiers_before 	<- x@transcripts[[i]]@tiers
			tiers_after 	<- modify_tiers_reorder_generate_list(x@transcripts[[i]], sortVector, addMissingTiers, deleteTiersThatAreNotInTheSortVector)
			tiers_deleted 	<- setdiff(names(tiers_before), names(tiers_after))
			tiers_added 	<- setdiff(names(tiers_after), names(tiers_before))

			tiers_same_orderbefore <- intersect(names(tiers_before), names(tiers_after))
			tiers_same_orderafter <- intersect(names(tiers_after), names(tiers_before))
			tiers_orderofcopiedtiershaschanged <- !(all.equal(tiers_same_orderbefore, tiers_same_orderafter)==TRUE)

			modificationTrans <- list(modification="tiers_reorder",
									  tiers_orderchanged=tiers_orderofcopiedtiershaschanged,
									  tiers_deleted_numberof=length(tiers_deleted),
									  tiers_deleted=tiers_deleted,
									  tiers_added_numberof=length(tiers_added),
									  tiers_added=tiers_added,
									  tiers_before=names(tiers_before),
									  tiers_after=names(tiers_after)								  )

						#check if there are  changes
			anyChanges <- FALSE
			if (length(tiers_before)!=length(tiers_after)) {
				anyChanges <- TRUE
			} else {
				if(all.equal(tiers_before, tiers_after)!=TRUE) {
					anyChanges <- TRUE
				}
			}

			#if there are changes
			if(anyChanges) {
				#write changes to transcipt log
				x@transcripts[[i]]@lastModification <- modificationTrans
				x@transcripts[[i]]@modified <- TRUE

				#remember values for corpus object
				transcripts_modified_nr <- transcripts_modified_nr + 1
				transcripts_modified_ids <- c(transcripts_modified_ids, i)

				#apply changes
				#create new tier list
				x@transcripts[[i]]@tiers <- tiers_after

				#copy only data that is contained in new list
					x@transcripts[[i]]@data <- x@transcripts[[i]]@data[ (x@transcripts[[i]]@data$tierName %in% names(x@transcripts[[i]]@tiers)) , ]
			}
		}
	}
	modificationCorpus <- list(modification="tiers_reorder",
							   transcripts_modified_nr=transcripts_modified_nr,
							   transcripts_modified_ids=transcripts_modified_ids)
	
	x@lastModification <- modificationCorpus
	x@fulltext.upToDate 		<-FALSE

	return (x)
}

modify_tiers_reorder_generate_list <- function (t, sortVector, addMissingTiers=TRUE, deleteTiersThatAreNotInTheSortVector=FALSE) {
	newList <- c()
	oldList <- t@tiers

	#check each pattern in the sort vector
	insertPosition <- -1
	for (myPattern in sortVector) {
		#if this is the insert position, remember the length
		if (myPattern=="\\*") {
			insertPosition <- length(newList)
		} else {
			#are there hits for this pattern?
			hits<-stringr::str_which(names(oldList), myPattern)

			#if there are
			if (length(hits) >0) {
				#add items to new list
				newList<-c(newList,oldList[hits] )

				#delete items from old list
				oldList <- oldList[-hits]
			} else {
				#if missing tiers should be added
				if (addMissingTiers==TRUE) {
					newElement <- "IntervalTier"
					names(newElement) <- myPattern
					newList<-c(newList,newElement )
				}

			}
		}
	}
	#if there are still elements in the old list, insert those too
	if (deleteTiersThatAreNotInTheSortVector==FALSE) {
		if (length(oldList)>0) {
			#if no insert position has been found, insert at the end
			if (insertPosition==-1) {
				insertPosition <- length(newList)
			}
			newList<-	append(newList, oldList, insertPosition)
		}
	}

	return(newList)
}













