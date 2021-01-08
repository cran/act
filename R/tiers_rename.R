#' Rename tiers 
#' 
#' Renames all tiers in all transcript objects of a corpus.
#' If only certain transcripts should be affected set the parameter \code{filterTranscriptNames}. 
#' In case that you want to select transcripts by using regular expressions use the function \code{act::search_meta} first.
#' 
#' The tiers will only be renamed if the resulting names preserve the uniqueness of the tier names.
#' Results  will be reported in \code{@history} of the transcript objects.
#' Please be aware that this function is not optimized for speed and may take quite a while to run, depending on the size of your corpus object.
#'
#' @param x Corpus object.
#' @param searchPattern Character string; search pattern as regular expression.
#' @param searchReplacement Character string; replacement string.
#' @param filterTranscriptNames Vector of character strings; names of the transcripts to be included. 
#'
#' @return Corpus object.
#' 
#' @seealso \link{tiers_convert}, \link{tiers_sort}
#' 
#' @export
#'
#' @example inst/examples/tiers_rename.R
#' 
tiers_rename <- function(x, 
						 searchPattern, 
						 searchReplacement, 
						 filterTranscriptNames=NULL) {
	
	if (missing(x)) 	{stop("Corpus object in parameter 'x' is missing.") 		} else { if (class(x)[[1]]!="corpus") 		{stop("Parameter 'x' needs to be a corpus object.") 	} }
	
	transcripts_modified_nr <- 0
	transcripts_modified_ids <- c()
	transcripts_problematic_nr <- 0
	transcripts_problematic_ids <- c()
	
	tiers_renamed_nr <- 0
	tiers_problematic_nr <- 0
	
	
	#=== get the transcript names
	#if none are given, take all names
	if (is.null(filterTranscriptNames)) {		
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==0) {
		filterTranscriptNames <- NULL
	} else if (length(filterTranscriptNames)==1) {
		if (filterTranscriptNames[1]=="") { filterTranscriptNames <- NULL }
	}
	if (is.null(filterTranscriptNames)) {	filterTranscriptNames <- names(x@transcripts)	}
	
	for (i in filterTranscriptNames) {
		#reset transcript log
		x@transcripts[[i]]@modification.systime <- character()
		
		#create test names
		tiers_before 		<- x@transcripts[[i]]@tiers
		tiers_after 		<- tiers_before
		tiers_after$name 	<- stringr::str_replace_all(x@transcripts[[i]]@tiers$name, searchPattern, searchReplacement)
		
		if(length(setdiff(tiers_before$name, tiers_after$name))==0) {
			#no changes at all
			x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
				modification    = "tiers_rename",
				systime          = Sys.time(),
				result           = "OK: no matches, no tiers renamed.",
				tiers_renamed_nr = 0,
				tiers_before     = tiers_before$name,
				tiers_after      = tiers_after$name
			)
														 
			x@transcripts[[i]]@modification.systime <- character()
			
		} else {
			#check if new names are unique
			if (max(table(tiers_after$name))>1) {
				#non-unique names
				x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
					modification                    = "tiers_rename",
					systime                          = Sys.time(),
					result="ERROR: No tiers renamed. Renaming would result in non unique tier names (see: tiers_problematic)",
					tiers.problematic                = setdiff(tiers_before$name, tiers_after$name),
					tiers.problematic.resulting.name = setdiff(tiers_after$name, tiers_before$name),
					tiers.renamed.count              = 0,
					tiers.before.ids                 = tiers_before$name,
					tiers.after.ids                  = tiers_after$name
				)
				x@transcripts[[i]]@modification.systime <- Sys.time()
				
				#counter for corpus object
				tiers_problematic_nr <- tiers_problematic_nr + length(setdiff(tiers_before$name, tiers_after$name))
				
				transcripts_problematic_nr <- transcripts_problematic_nr + 1
				transcripts_problematic_ids <- c(transcripts_problematic_ids, i)
				
			} else {
				#all names only 1 occurrence
				x@transcripts[[i]]@modification.systime <- Sys.time()
				x@transcripts[[i]]@history[[length(x@transcripts[[i]]@history)+1]] <-	list( 
					modification         ="tiers_rename",
					systime              = Sys.time(),
					result               =paste("OK:", as.character(length(setdiff(tiers_after$name, tiers_before$name))), "tier(s) renamed"),
					tiers.renamed        =setdiff(tiers_after$name, tiers_before$name),
					tiers.original       =setdiff(tiers_before$name, tiers_after$name),
					tiers.renamed.count  =length(setdiff(tiers_after$name, tiers_before$name)),
					tiers.before.ids     =tiers_before$name,
					tiers.after.ids      =tiers_after$name
				)
				
				
				#counter for corpus object
				tiers_renamed_nr <- tiers_renamed_nr + length(setdiff(tiers_after$name, tiers_before$name))
				transcripts_modified_nr <- transcripts_modified_nr +1
				transcripts_modified_ids <- c(transcripts_modified_ids,i)
				
				#set new values in tiers list
				x@transcripts[[i]]@tiers <- tiers_after
				
				#apply changes
				x@transcripts[[i]]@annotations$tier.name <- stringr::str_replace_all(x@transcripts[[i]]@annotations$tier.name, searchPattern, searchReplacement)
			}
		}
	}
	x@history[[length(x@history)+1]] <- list(  
		modification                  ="tiers_rename",
		systime                       = Sys.time(),
		tiers.renamed.count           =tiers_renamed_nr,
		tiers.problematic.count       =tiers_problematic_nr,
		transcripts.modified.count    =transcripts_modified_nr,
		transcripts.modified.ids      =transcripts_modified_ids,
		transcripts.problematic.count =transcripts_problematic_nr,
		transcripts.problematic.ids   =transcripts_problematic_ids
	)
	
	return (x)
}