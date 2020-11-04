#' Search, replace and copy the contents of annotations
#' 
#' The function searches within the contents of annotations and replaces the search hits. 
#' In addition the search hit may be copied to another tier. 
#' In case that there is no overlapping annotation in the destination tier a new annotation will be created (based on the time values of the original annotation). In case that there is an overlapping annotation in the destination tier, the search result will be added at the end. 
#'
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' @param replacement Character string; replacement.
#' @param destTier Character string; name of the tier to which the hit should be copied (if no copying is intended set to "").
#' @param filterTranscriptsInclude Character string; filter as a regular expression, in which transcripts should be searched.
#' @param filterTiersInclude Character string; filter as a regular expression, in which tiers should be searched.
#' @param collapseString Character string; will be used to collapse multiple search results into one string.
#'
#' @return Corpus object.
#' @export
#'
#' @example inst/examples/modify_data_replace_copy.R
#' 
modify_data_replace_copy <- function (x, pattern, replacement=NA, destTier="",  filterTranscriptsInclude="", filterTiersInclude="", collapseString=" | ") {
	
	#reset corpus log
	x@lastModification <- list()
	
	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	
	recordsets_copied_nr <- 0
	recordsets_replaced_nr <- 0
	recordsets_copied_total_nr <- 0
	recordsets_replaced_total_nr <- 0
	recodsets_copiederror_tiermissingintranscript_nr <- 0
	recodsets_copiederror_tiermissingintranscript_ids <- c()
	#i<-1
	#x<-corpus
	
	#all transcripts
	for (i in 1:length(x@transcripts)) {
		#reset transcript log
		x@transcripts[[i]]@lastModification <- list()
		x@transcripts[[i]]@modified <- FALSE
		
		processThisTranscript<- TRUE
		if (filterTranscriptsInclude=="") {
			processThisTranscript <- TRUE
		} else {
			#if transcript name matches the filter
			if(stringr::str_detect(x@transcripts[[i]]@name, filterTranscriptsInclude)) {
				processThisTranscript <- TRUE
			}else {
				processThisTranscript <- FALSE
			}
		}
		
		#check if destination tier exists (in case that destTier is set)
		destTierIsPresent <- FALSE
		if (destTier!="") {
			#if destination tier is not in the tier list
			destTierIsPresent <- any((names(x@transcripts[[i]]@tiers)==destTier)==TRUE)
		}
		
		#if this transcript is to be processed
		if (processThisTranscript) {
			#each data row
			if (nrow(x@transcripts[[i]]@data)>0) {
				for (j in 1:nrow(x@transcripts[[i]]@data)) {
					#check if this record set should be processed
					processThisRecordset <- TRUE
					if (filterTiersInclude=="") {
						processThisRecordset <- TRUE
					} else {
						#if tier name matches the filter
						if(stringr::str_detect(x@transcripts[[i]]@data$tierName[[j]], filterTiersInclude)) {
							processThisRecordset <- TRUE
						}else {
							processThisRecordset <- FALSE
						}
					}
		
					if (processThisRecordset) {
						#search and copy
						if ((is.na(destTier)!=TRUE) & (destTierIsPresent==TRUE)) {
							hits 		<- stringr::str_extract_all(x@transcripts[[i]]@data$content[[j]], pattern)
							hits_merged <- c()
							
							#if (length(hits) ==1) {
							#	hits_merged <- unlist(hits)[1]
							#} else if (length(hits)>1) { 
							#	hits_merged <- stringr::str_flatten(unlist(hits), collapse=collapseString)
							#}
							if (length(unlist(hits))>0) {
								hits_merged <- stringr::str_flatten(unlist(hits), collapse=collapseString)
							}

							#if there is a hit
							if (length(hits_merged) > 0) {
								#get record set in destination tier that possibly overlaps
								temp <- (	x@transcripts[[i]]@data$tierName==destTier) & (x@transcripts[[i]]@data$startSec < x@transcripts[[i]]@data$endSec[[j]]) & (x@transcripts[[i]]@data$endSec > x@transcripts[[i]]@data$startSec[[j]])
								
								#if there is no overlapping record set on destination tier
								if (length(which(temp, TRUE))==0) {
									#create new record set
									myrow <- x@transcripts[[i]]@data[j,]
									myrow$tierName     <- destTier
									myrow$content      <- hits_merged
									myrow$content.norm <- ""
									myrow$dataID       <- max(x@transcripts[[i]]@data$dataID)+1
									
									#add to table
									x@transcripts[[i]]@data <- rbind(x@transcripts[[i]]@data, myrow)
									
								} else {
									#if there is one or several overlapping record sets on the destination tiers, take the first one
									index<-which(temp, TRUE) [[1]]
									#add text in the end
									x@transcripts[[i]]@data$content[[index]] <- paste (x@transcripts[[i]]@data$content[[index]], hits_merged, collapse= collapseString)
								}
								
								recordsets_copied_nr <- recordsets_copied_nr + 1
								x@transcripts[[i]]@modified <- TRUE
								
							} #end there are hits
						} #end copy
						
						#search and replace
						if (is.na(replacement)!=TRUE) {
							newvalue <- stringr::str_replace_all(x@transcripts[[i]]@data$content[[j]], pattern, replacement)
							if (newvalue!=x@transcripts[[i]]@data$content[[j]]) {
								#set new value
								x@transcripts[[i]]@data$content[[j]] <- newvalue
								
								#increase counter
								recordsets_replaced_nr <-recordsets_replaced_nr + 1
								x@transcripts[[i]]@modified <- TRUE
							}
						}
					}
				} #end each data row
			}
			
			#update info for transcript
			if (x@transcripts[[i]]@modified==TRUE) {
				#update modification info
				modificationTrans <- list(modification="data_search_replace_copy")
				
				#if copy option is set but tier is not in the tier list
				if ((destTier!="") & (destTierIsPresent==FALSE)) {
					modificationTrans <- list(modificationTrans,
											  recordsets_copy_error="ERROR: destination tier for copying is not in the tier list of this transcript.")
					#remember values for corpus object
					recodsets_copiederror_tiermissingintranscript_nr = recodsets_copiederror_tiermissingintranscript_nr + 1
					recodsets_copiederror_tiermissingintranscript_ids = c(recodsets_copiederror_tiermissingintranscript_ids, i)
				}
				
				modificationTrans <- list(modificationTrans,
										  recordsets_replaced_nr=recordsets_replaced_nr,
										  recordsets_copied_nr=recordsets_copied_nr)
				
				x@transcripts[[i]]@lastModification <- modificationTrans
				
				#increase counter for corpus object
				transcripts_modified_nr<-transcripts_modified_nr +1
				transcripts_modified_ids=c(transcripts_modified_ids, i)
				recordsets_replaced_total_nr = recordsets_replaced_total_nr + recordsets_replaced_nr
				recordsets_copied_total_nr = recordsets_copied_total_nr + recordsets_copied_nr
			}
		} #end process transcript
		
	} #next transcript
	
	#update modification in corpus file
	modificationCorpus <- list(modification="data_search_replace_copy",
							   pattern=pattern,
							   replacement=replacement,
							   destTier=destTier,
							   transcripts_modified_nr=transcripts_modified_nr,
							   transcripts_modified_ids=transcripts_modified_ids,
							   recordsets_replaced_total_nr=recordsets_replaced_total_nr,
							   recordsets_copied_total_nr=recordsets_copied_total_nr
	)
	if (recodsets_copiederror_tiermissingintranscript_nr>0) {
		modificationCorpus <- list(modificationCorpus,
								   recodsets_copiederror = "ERROR: the destination tier for copying was missing in some transcripts. No data copied.",
								   recodsets_copiederror_tiermissingintranscript_nr=recodsets_copiederror_tiermissingintranscript_nr,
								   recodsets_copiederror_tiermissingintranscript_ids=recodsets_copiederror_tiermissingintranscript_ids)
		
	}
	
	x@lastModification <- modificationCorpus
	x@normalization.upToDate 	<- FALSE
	x@fulltext.upToDate 		<- FALSE
	
	return (x)
}


