#' Delete empty annotations
#'
#' @param x Corpus object.
#' @param trimBeforeCheck Logical; if \code{TRUE} leading and trailing spaces will be removed before checking (as a consequence record sets that contain only spaces will be deleted, too).
#' @param filterTranscriptsInclude Character string; filter as a regular expression, which transcripts should be processed.
#' @param filterTiersInclude Character string; filter as a regular expression, which tiers should be processed.
#'
#' @return Corpus object.
#' 
#' @export
#'
#' @example inst/examples/modify_data_delete_empty.R
#' 
modify_data_delete_empty <- function (x, trimBeforeCheck=FALSE, filterTiersInclude="", filterTranscriptsInclude="") {
	#reset corpus log
	x@lastModification <- list()
	
	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	recordsets_deleted_total_nr <- 0
	
	
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
		
		#if this transcript is to be processed
		recordsets_deleted_ids<- c()
		if (processThisTranscript) {
			
			#each data row
			if (nrow(x@transcripts[[i]]@data)>0) {
				
				for (j in 1:nrow(x@transcripts[[i]]@data)) {
					#check if this recordset should be processed
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
						if (trimBeforeCheck) {
							if (stringr::str_trim(x@transcripts[[i]]@data$content[[j]])=="") {
								recordsets_deleted_ids <- c(recordsets_deleted_ids,j)
							}
						} else {
							if (x@transcripts[[i]]@data$content[[j]]=="") {
								recordsets_deleted_ids <- c(recordsets_deleted_ids,j)
							}
						}
					}
				} #end each data row
			}
			
			if (length(recordsets_deleted_ids)>0) {
				x@transcripts[[i]]@data <- x@transcripts[[i]]@data[-recordsets_deleted_ids, ]
				
				#update info for transcript
				x@transcripts[[i]]@modified<-TRUE
				#update modification info
				modificationTrans <- list(modification="modify_data_delete_empty")
				modificationTrans <- list(modificationTrans,
										  recordsets_deleted_nr=length(recordsets_deleted_ids))
				
				x@transcripts[[i]]@lastModification <- modificationTrans
				
				#increase counter for corpus object
				transcripts_modified_nr<-transcripts_modified_nr +1
				transcripts_modified_ids=c(transcripts_modified_ids, i)
				recordsets_deleted_total_nr = recordsets_deleted_total_nr + length(recordsets_deleted_ids)
			}
		}
	} #next transcript
	
	#update modifaction in copus file
	modificationCorpus <- list(modification="modify_data_delete_empty",
							   transcripts_modified_nr=transcripts_modified_nr,
							   transcripts_modified_ids=transcripts_modified_ids,
							   recordsets_deleted_total_nr=recordsets_deleted_total_nr
	)
	
	x@lastModification <- modificationCorpus
	x@fulltext.upToDate 		<-FALSE
	return (x)
}

