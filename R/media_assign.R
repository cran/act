#' Assign media file links to transcript objects
#'
#' Searches for media files in folders and assigns the links to transcript objects in a corpus. 
#' The function uses the name of the transcript to find the media files, 
#' e.g. the function assumes that the annotation files have the same name as the media files, except from the suffix/the file type.
#'
#' @param x Corpus object.
#' @param filterFile Character string; regular expression of file types to look for.
#' @param searchFolder Character string; folder in which media files should be searched (if \code{NULL} the folder specified in \code{x@folders.media} will be used).
#' @param searchInSubfolders Logical; if \code{FALSE} only the main level of the directory will be scanned for media, if \code{TRUE} sub folders will be scanned for media, too.
#' @param filterTranscript Character string; search media files only for  transcripts that match the regular expression.
#' @param removeExistingMedia Logical; if \code{TRUE} existing media links will be removed, if \code{FALSE} existing media links will be preserved and new links will be added.
#' @param onlyUniqueFiles Logical; if \code{TRUE} media files with the same name (in different locations) will only be added once; if \code{FALSE} all media files found will be added, irrespective of possible doublets.
#'
#' @return Corpus object.
#' 
#' @seealso \link{media_remove}, \link{media_getpath}
#' 
#' @export
#'
#' @example inst/examples/media_assign.R
#' 
media_assign <- function(x=NULL,  filterFile="*\\.(wav|mp3|aif|aiff|mp4)", searchFolder=NULL, searchInSubfolders=TRUE, filterTranscript=NULL, removeExistingMedia=TRUE, onlyUniqueFiles=TRUE) {
	if (is.null(filterTranscript) ) {
		filterTranscript<-""
	}
	
	if (is.null(searchFolder)) {
		searchFolder<- x@folders.media
	}
	
	#--- get all files from folder
	#remove tailing slashes
	myPaths 	<- gsub("/*$", "", searchFolder , perl=TRUE)
	#get all files in folders recursively
	myPaths 	<- unlist(lapply(myPaths, FUN = function(x) list.files(x, recursive=searchInSubfolders, pattern=filterFile, ignore.case=TRUE,  full.names=TRUE)))
	
	#get names
	myFilenames <- basename(myPaths)

	#--- run through all transcripts in the corpus file
	i<-1
	for (i in 1:length(x@transcripts)) {

			#check if transcript name falls under the regular expression
		if (stringr::str_detect(x@transcripts[[i]]@name, filterTranscript)) {
			
			#get transcript name
			name_transcript	<- x@transcripts[[i]]@name
			#name_transcript	<- gsub(" ", "_", name_transcript)
			search <- paste("^", name_transcript, sep="")
			myMediaFiles<-	unlist(myPaths[grep(pattern=search, myFilenames)])
			
			#onlyUniqueFile <- TRUE
			if (onlyUniqueFiles) {
				#get only filename
				myMediaFilenames <- basename(myMediaFiles)
				#select for filepaths only unique filenames
				myMediaFiles <- myMediaFiles[!duplicated(myMediaFilenames)]
			}
			
			if (removeExistingMedia) {
				x@transcripts[[i]]@path.media <- myMediaFiles
			} else {
				x@transcripts[[i]]@path.media <- c(x@transcripts[[i]]@path.media, myMediaFiles)
			}
		}
	}
	return (x)
}

