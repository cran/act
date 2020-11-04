#' Rename tiers 
#' 
#' Renames all tiers in all transcript objects of a corpus.
#' The tiers will only be renamed if the resulting names preserve the uniqueness of the tier names.
#' Results  will be reported in \code{@lastModification} of the corpus object.
#' Please be aware that this function is not optimized for speed and may take quite a while to run, depending on the size of your corpus object.
#'
#' @param x Corpus object.
#' @param pattern Character string; search pattern as regular expression.
#' @param replacement Character string; replacement string.
#' @param filterTranscriptsInclude Character string; only transcripts that match the regular expression will be affected.
#'
#' @return Corpus object.
#' @export
#'
#' @example inst/examples/modify_tiers_rename.R
#' 
modify_tiers_rename <- function(x, pattern, replacement, filterTranscriptsInclude="") {
	#reset corpus log
	x@lastModification <- list()
	
	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	transcripts_problematic_nr <- 0
	transcripts_problematic_ids <- c()
	
	tiers_renamed_nr <- 0
	tiers_problematic_nr <- 0
	
	for (i in 1:length(x@transcripts)) {
		
		#reset transcript log
		x@transcripts[[i]]@lastModification <- list()
		x@transcripts[[i]]@modified <- FALSE
		
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
		if (processThisTranscript) {
			#create test names
			tiers_before 		<- x@transcripts[[i]]@tiers
			tiers_after 		<- tiers_before
			names(tiers_after) 	<- stringr::str_replace_all(names(x@transcripts[[i]]@tiers), pattern, replacement)
			
			x@transcripts[[i]]@lastModification <- list()
			
			if(all.equal(tiers_before, tiers_after)==TRUE) {
				#no changes at all
				modificationTrans <- list(modification="tiers_rename",
										  result="OK: no matches, no tiers renamed.",
										  tiers_renamed_numberof=0,
										  tiers_before=names(tiers_before),
										  tiers_after=names(tiers_after))
				
				x@transcripts[[i]]@lastModification <- modificationTrans
				x@transcripts[[i]]@modified <- FALSE
				
			} else {
				#check if new names are unique
				if (max(table(names(tiers_after)))>1) {
					#non-unique names
					modificationTrans <- list(modification="tiers_rename",
											  result="ERROR: No tiers renamed. Renaming would result in non unique tier names (see: tiers_problematic)",
											  tiers_problematic=setdiff(names(tiers_before), names(tiers_after)),
											  tiers_problematic_resulting_name=setdiff(names(tiers_after), names(tiers_before)),
											  tiers_renamed_numberof=0,
											  tiers_before=names(tiers_before),
											  tiers_after=names(tiers_after))
					
					x@transcripts[[i]]@lastModification <- modificationTrans
					x@transcripts[[i]]@modified <- FALSE
					
					#counter for corpus object
					tiers_problematic_nr <- tiers_problematic_nr + length(setdiff(names(tiers_before), names(tiers_after)))
					
					transcripts_problematic_nr <- transcripts_problematic_nr + 1
					transcripts_problematic_ids <- c(transcripts_problematic_ids, i)
					
				} else {
					#all names only 1 ocurrence
					modificationTrans <- list(modification="tiers_rename",
											  result=paste("OK:", as.character(length(setdiff(names(tiers_after), names(tiers_before)))), "tier(s) renamed"),
											  tiers_renamed=setdiff(names(tiers_after), names(tiers_before)),
											  tiers_original=setdiff(names(tiers_before), names(tiers_after)),
											  tiers_renamed_numberof=length(setdiff(names(tiers_after), names(tiers_before))),
											  tiers_before=names(tiers_before),
											  tiers_after=names(tiers_after))
					x@transcripts[[i]]@lastModification <- modificationTrans
					x@transcripts[[i]]@modified <- TRUE
					
					#counter for corpus object
					tiers_renamed_nr <- tiers_renamed_nr + length(setdiff(names(tiers_after), names(tiers_before)))
					transcripts_modified_nr <- transcripts_modified_nr +1
					transcripts_modified_ids <- c(transcripts_modified_ids,i)
					
					#set new values in tiers list
					x@transcripts[[i]]@tiers <- tiers_after
					
					#apply changes to data
					x@transcripts[[i]]@data$tierName <- stringr::str_replace_all(x@transcripts[[i]]@data$tierName, pattern, replacement)
				}
			}
		}
	}
	modificationCorpus <- list(modification="tiers_rename",
							   tiers_renamed_nr=tiers_renamed_nr,
							   tiers_problematic_nr=tiers_problematic_nr,
							   transcripts_modified_nr=transcripts_modified_nr,
							   transcripts_modified_ids=transcripts_modified_ids,
							   transcripts_problematic_nr=transcripts_problematic_nr,
							   transcripts_problematic_ids=transcripts_problematic_ids
	)
	
	x@lastModification <- modificationCorpus
	return (x)
}