#' Delete annotations
#'
#' @param x Corpus object.
#' @param filterContent Character string; all annotations will be deleted that match this regular expression.
#' @param filterTranscriptsInclude Character string; filter as a regular expression, which transcripts will be affected.
#' @param filterTiersInclude Character string; filter as a regular expression, which tiers will be affected.
#'
#' @return Corpus object.
#' @export
#'
#' @examples
#' library(act)
#' 
#' # Set the regular expression which annotations should be deleted.
#' # In this case: all annotations that contain the letter "a"
#' myRegEx <- "a"

#' # Have a look at all annotations in the first transcript
#' examplecorpus@transcripts[[1]]@data$content
#' 
#' # Some of them match to the regular expression
#' hits <- grep(pattern=myRegEx, x=examplecorpus@transcripts[[1]]@data$content)
#' examplecorpus@transcripts[[1]]@data$content[hits]
#' # Others don't match the regular expression
#' examplecorpus@transcripts[[1]]@data$content[-hits]
#' 
#' # Run the function and delete the annotations that match the regular expression
#' test <- act::modify_data_delete (x=examplecorpus, filterContent=myRegEx)
#' 
#' # Compare how many data rows are in the first transcript in 
#' # the example corpus and in the newly created test corpus:
#' nrow(examplecorpus@transcripts[[1]]@data)
#' nrow(test@transcripts[[1]]@data)
#' 
#' # Only the annotations are left, that did not match the regular expression:
#' test@transcripts[[1]]@data$content
#' 
modify_data_delete <- function (x, filterContent="", filterTiersInclude="", filterTranscriptsInclude="") {
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
						
						if(filterContent=="") {
							recordsets_deleted_ids <- c(recordsets_deleted_ids,j)
						} else {
							if (stringr::str_detect(string=x@transcripts[[i]]@data$content[[j]], pattern=filterContent)) {
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
				modificationTrans <- list(modification="modify_data_delete")
				modificationTrans <- list(modificationTrans,
										  filterContent=filterContent,
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
	modificationCorpus <- list(modification="modify_data_delete",
							   filterContent=filterContent,
							   transcripts_modified_nr=transcripts_modified_nr,
							   transcripts_modified_ids=transcripts_modified_ids,
							   recordsets_deleted_total_nr=recordsets_deleted_total_nr
	)
	
	x@lastModification <- modificationCorpus
	x@normalization.upToDate 	<- FALSE
	x@fulltext.upToDate 		<-FALSE
	return (x)
}
