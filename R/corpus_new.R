#' Create a new corpus
#'
#' @param corpusname Character string; name of the corpus to be created.
#' @param folders_annotationfiles Vector of character strings; paths to folders that contain annotation files.
#' @param folders_mediafiles Vector of character strings; paths to folders that contain media files.
#' @param loadFiles Logical; if \code{TRUE} annotation files will be loaded immediately when the function is called, if \code{FALSE} corpus object will be created without loading the annotation files.
#'
#' @return Corpus object.
#' 
#' @seealso \link{corpus_load}, \link{examplecorpus}

#' 
#' @export
#'
#' @example inst/examples/corpus_new.R
#' 
corpus_new <- function(corpusname, folders_annotationfiles, folders_mediafiles, loadFiles=TRUE) {
	if (missing(folders_annotationfiles)) {
		stop("No input folder(s) specified in argument 'folders_annotationfiles'")
	}
	#=== create a new corpus
	x <- methods::new("corpus")
	x@folders.annotationfiles <- gsub("/*$", "", folders_annotationfiles, perl=TRUE)
	
	if (!missing(folders_mediafiles)) {
		x@folders.media <- gsub("/*$", "", folders_mediafiles, perl=TRUE)
	}
	
	if (!missing(corpusname) ) {
		x@name <- corpusname
	}
	
	if (loadFiles) {
		return(act::corpus_load(x))
	} else {
		return(x)
	}
}
