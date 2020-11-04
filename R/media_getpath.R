#' Gets the path of a media file for a transcript
#'
#' @param t transcript object; transcript for which you want to get the media path.
#' @param filterFile Character string; regular expression of file types to look for.
#' @param returnOnlyFirst Logical; if \code{TRUE} only the first hit will be returned.
#'
#' @return Character string; path to a media file.
#' @export
#' 
#' @seealso \link{media_assign}, \link{media_remove}
#'
#' @example inst/examples/media_getpath.R
#' 
media_getpath <- function(t, filterFile=".*\\.(wav|mp3|aif|aiff|mp4)", returnOnlyFirst=TRUE) {
	#filterFile<-".*\\.(wav|mp3|aif|aiff|mp4)"
	if (is.null(t))	{
		stop("Transcript object needs to be specified.")
	}
	if (typeof(t)=="list") {
		stop("Your transcript  is of the wrong type (it is a list). Probably you have used single square brackets to access the transcript in your corpus object (corpus@transcripts[...]). Please use double square brackets (corpus@transcripts[[...]])")
	}
	
	
	hits <- stringr::str_detect(t@path.media, filterFile)
	hits <- t@path.media[hits]
	
	if (returnOnlyFirst) {
		return (hits[1])
	} else {
		return (hits)
	}
}
