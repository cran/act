#' Remove media files links from transcript objects 
#'
#' @param x Corpus object.
#' @param filterTranscript Character string; if defined, only transcripts that match this regular expression will be affected.
#'
#' @return Corpus object.
#' @export
#'
#' @seealso \link{media_assign}, \link{media_getpath}
#'
#' @examples
#' examplecorpus <- act::media_remove(examplecorpus)
#' 
media_remove <- function(x=NULL, filterTranscript="") {
	#--- run through all transcripts in the corpus file
	for (i in 1:length(x@transcripts)) {
		#check if transcript name falls under the regular expression
		if (stringr::str_detect(x@transcripts[[i]]@name, filterTranscript)) {
			x@transcripts[[i]]@path.media <- character()
		}
	}
	return (x)
}


