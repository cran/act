#' Update full texts
#'
#' Creates/updates the full texts of the transcripts in a corpus.
#'The full text may be created in two different ways: 
#'- The contents of a transcription will be joined consecutively based on the time information. 
#'- The contents of each tier will be joined consecutively, and then the next tier will be joined.
#' 
#' @param x Corpus object.
#' @param mode Character string; Which full text should be created; accepts the following values: fulltext.bytier, fulltext.bytime, fulltext;  
#' @param updateX Logical; If \code{TRUE} the original corpus object passed as x to the function will also be updated! If you do not want this, set to \code{FALSE}. 
#' 
#' @return Corpus object.
#' @export
#'
#' @examples
#' library(act)
#' 
#' examplecorpus<-act::fulltext_update(x=examplecorpus)
#' 
fulltext_update <- function(x, mode="fulltext", updateX=TRUE) {
	captured_x <- substitute(x)
	
	#=== check x object
	if (is.null(x)) 				{stop("Corpus object x is null.")		}
	if (is.null(x@transcripts)) 	{stop("No transcripts found in corpus object x.")	}
	
    #=== set progress bar
	pb1 <- progress::progress_bar$new(
		format = "  Updating fulltexts [:bar] :percent missing: :eta",
		total = length(x@transcripts), 
		clear = FALSE, 
		show_after = 0,
		width= 60)
	
	#iterate through all transcripts/ all data
	#i<-121
	#x<-corpus
	
	for (i in 1:length(x@transcripts))
	{
		#=== Status
		pb1$tick()
		myFulltext.bytime.orig <-""
		myFulltext.bytime.norm <-""
		myFulltext.bytier.orig <-""
		myFulltext.bytier.norm <-""
		
		myData <- x@transcripts[[i]]@data
		
		if (!is.null(myData))  	{
			if (nrow(myData)>0) 	{
				
				#===add new cols
				myData <- cbind.data.frame(myData, separator=as.character(""), char.separator=0, char.content.orig=0, char.content.norm=0, char.content.orig.plussep=0,char.content.norm.plussep=0, char.orig.bytime.start=0, char.norm.bytime.start=0 , stringsAsFactors = FALSE)
				
				#---------------------- by time
				if (mode=="fulltext" | mode=="fulltext.bytime") {
					
					#=== sort the data: start sec - tier name
					myData <- 	myData[order(myData$startSec, myData$tierName), ]
					
					#=== filter by tiers
					include <- c(1:length(myData$content))
					#if any filter is set
					if (x@fulltext.currentlyset.filter.tiers.exclude!="" | x@fulltext.currentlyset.filter.tiers.include!="" )
					{
						#if include filter is set
						if (x@fulltext.currentlyset.filter.tiers.include!="") {
							include <- grep(x@fulltext.currentlyset.filter.tiers.include, myData$tierName, ignore.case =TRUE, perl = TRUE)
						}
						
						#if exclude filter is set
						if (!x@fulltext.currentlyset.filter.tiers.exclude=="") {
							exclude	<- grep(x@fulltext.currentlyset.filter.tiers.exclude, myData$tierName, ignore.case =TRUE, perl = TRUE)
							include <- setdiff(include, exclude)
						}
					}
					
					#=== make vector with separator character
					#check if tier of preceeding interval is the same
					included.sametier 				<- c(FALSE, myData$tierName[include[2:length(include)]] == myData$tierName[include[1:length(include)-1]])
					
					#make vector containing corresponding separators
					included.separator				<- unlist(lapply(included.sametier, function(x) if(x==TRUE) {options()$act.separator_between_intervals} else {options()$act.separator_between_tiers}))
					
					#set the separators in my Data
					#myData$separator 				<- ""  	# rep("", nrow(myData))
					myData$separator[include] 		<- included.separator
					
					#=== calculate the lengths only of included recordsets
					#separator
					myData$char.separator[include]				<- nchar(myData$separator[include])
					
					#content
					myData$char.content.orig[include]			<- nchar(myData$content[include] )
					myData$char.content.norm[include]			<- nchar(myData$content.norm[include] )
					
					#separator + content
					myData$char.content.orig.plussep[include]	<- myData$char.separator[include] + myData$char.content.orig[include]
					myData$char.content.norm.plussep[include]	<- myData$char.separator[include] + myData$char.content.norm[include]
					
					#end position within full text
					myData$char.orig.bytime.end[include]   		<- cumsum(myData$char.content.orig.plussep[include])
					myData$char.norm.bytime.end[include]    	<- cumsum(myData$char.content.norm.plussep[include])
					
					#star position within full text
					myData$char.orig.bytime.start[include]  	<- myData$char.orig.bytime.end[include]  - myData$char.content.orig.plussep[include] + 1
					myData$char.norm.bytime.start[include]  	<- myData$char.norm.bytime.end[include] - myData$char.content.norm.plussep[include] + 1
					
					#=== create full text
					myFulltext.bytime.orig 						<- paste(myData$separator[include], myData$content[include], sep="", collapse="")
					myFulltext.bytime.norm 						<- paste(myData$separator[include], myData$content.norm[include], sep="", collapse="")
					
					#end full text with separator for tiers
					myFulltext.bytime.orig 						<- paste(myFulltext.bytime.orig, options()$act.separator_between_tiers, sep="", collapse="")
					myFulltext.bytime.norm 						<- paste(myFulltext.bytime.norm, options()$act.separator_between_tiers, sep="", collapse="")
				}
				
				#---------------------- by tier
				if (mode=="fulltext" | mode=="fulltext.bytier") {
					
					#=== sort the data: tier name - start sec
					myData <- 	myData[order(myData$tierName, myData$startSec), ]
					
					#=== filter by tiers
					include <- c(1:length(myData$content))
					#if any filter is set
					if (x@fulltext.currentlyset.filter.tiers.exclude!="" | x@fulltext.currentlyset.filter.tiers.include!="" )
					{
						#if include filter is set
						if (x@fulltext.currentlyset.filter.tiers.include!="") {
							include <- grep(x@fulltext.currentlyset.filter.tiers.include, myData$tierName, ignore.case =TRUE, perl = TRUE)
						}
						
						#if exclude filter is set
						if (x@fulltext.currentlyset.filter.tiers.exclude!="") {
							exclude	<- grep(x@fulltext.currentlyset.filter.tiers.exclude, myData$tierName, ignore.case =TRUE, perl = TRUE)
							include <- setdiff(include, exclude)
						}
					}
					
					
					#=== make vector with separator character
					#check if tier of preceeding interval is the same
					included.sametier 					<- c(FALSE, myData$tierName[include[2:length(include)]] == myData$tierName[include[1:length(include)-1]])
					
					#make vector containing corresponding separators
					included.separator					<- unlist(lapply(included.sametier, function(x) if(x==TRUE) {options()$act.separator_between_intervals} else {options()$act.separator_between_tiers}))
					
					#set the separators in my Data
					myData$separator 					<- rep("", nrow(myData))
					myData$separator[include] 			<- included.separator
					
					#=== calculate the lengths only of included recordsets
					#separator
					myData$char.separator[include]				<- nchar(myData$separator[include])
					
					#content
					myData$char.content.orig[include]			<- nchar(myData$content[include] )
					myData$char.content.norm[include]			<- nchar(myData$content.norm[include] )
					
					#separator + content
					myData$char.content.orig.plussep[include]	<- myData$char.separator[include] + myData$char.content.orig[include]
					myData$char.content.norm.plussep[include]	<- myData$char.separator[include] + myData$char.content.norm[include]
					
					#end position within full text
					myData$char.orig.bytier.end[include]   		<- cumsum(myData$char.content.orig.plussep[include])
					myData$char.norm.bytier.end[include]    	<- cumsum(myData$char.content.norm.plussep[include])
					
					#star position within full text
					myData$char.orig.bytier.start[include]  	<- myData$char.orig.bytier.end[include]  - myData$char.content.orig.plussep[include] + 1
					myData$char.norm.bytier.start[include]  	<- myData$char.norm.bytier.end[include] - myData$char.content.norm.plussep[include] + 1
					
					#=== create full text
					myFulltext.bytier.orig 						<- paste(myData$separator[include], myData$content[include], sep="", collapse="")
					myFulltext.bytier.norm 						<- paste(myData$separator[include], myData$content.norm[include], sep="", collapse="")
					
					#end full text with separator for tiers
					myFulltext.bytier.orig 						<- paste(myFulltext.bytier.orig, options()$act.separator_between_tiers, sep="", collapse="")
					myFulltext.bytier.norm 						<- paste(myFulltext.bytier.norm, options()$act.separator_between_tiers, sep="", collapse="")
				}
				
				
				#=== get rid of superfluous columns
				myData <- myData[,setdiff(colnames(myData), c("separator", "char.separator","char.content.orig","char.content.norm", "char.content.orig.plussep","char.content.norm.plussep"))]
				
				#=== save modified data back to corpus object
				x@transcripts[[i]]@data <- myData 
			}
		}
		
		#=== save full text to corpus object
		x@transcripts[[i]]@fulltext.bytime.orig <- myFulltext.bytime.orig 
		x@transcripts[[i]]@fulltext.bytime.norm <- myFulltext.bytime.norm 
		
		x@transcripts[[i]]@fulltext.bytier.orig <- myFulltext.bytier.orig 
		x@transcripts[[i]]@fulltext.bytier.norm <- myFulltext.bytier.norm 
		
		#count words
		wordNr.orig <- max(stringr::str_count(myFulltext.bytime.orig, options()$act.wordCount.regex), stringr::str_count(myFulltext.bytier.orig, options()$act.wordCount.regex)  )
		wordNr.norm <- max(stringr::str_count(myFulltext.bytime.norm, options()$act.wordCount.regex), stringr::str_count(myFulltext.bytier.norm, options()$act.wordCount.regex)  )
		x@transcripts[[i]]@words.fulltext.orig <- wordNr.orig  
		x@transcripts[[i]]@words.fulltext.norm <- wordNr.norm  
	}
	
	x@fulltext.upToDate <- TRUE
	x@fulltext.currentlyset.filter.tiers.include <- x@fulltext.currentlyset.filter.tiers.include
	x@fulltext.currentlyset.filter.tiers.exclude <- x@fulltext.currentlyset.filter.tiers.exclude

	x@lastModification <- list(x@lastModification, "fulltext_update")
	
	#assign to original corpus object
	if (updateX) {
		p <- parent.frame() 
		p[[deparse(captured_x)]] <- x
	}	
	
	return(x)
}