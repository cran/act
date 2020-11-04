#' Exports transcripts in different annotation formats
#' 
#' There are two kinds of data accepted as input 'x' to this function:
#' (1) a corpus object
#' (2) a list of transcripts.
#'
#' @param x Corpus object or list of transcripts.
#' @param outputFolder Character string; path to a folder where the transcription files will be saved.
#' @param formats Vector with one or more character strings; output formats, accepted values: eaf, textgrid.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created (affects only eaf files).
#' @param writeOnlyModified Logical; if \code{TRUE} only modified transcripts will be exported.
#'
#' @export
#' 
#' @seealso \link{eaf_export}, \link{textgrid_export}, \link{textgrid_import}
#'
#' @example inst/examples/io_transcripts_export.R
#' 
transcripts_export <-  function(x, outputFolder, formats, createMediaLinks=FALSE, writeOnlyModified=FALSE) {
	if (missing(x)) {
		stop("No corpus object specified in argument 'x'")
	}
	
	#if an entire corpus is given, select the transcripts only
	if (class(x)[[1]]=="corpus") {
		transcripts<-x@transcripts
	} else {
		if (class(x)=="list")
		{
			if (class(x[[1]])=="transcript") {
				transcripts<-x
			} else {
				stop("'x' must be a corpus x or a list of transcript xs")
			}
		} else {
			if (class(x)=="transcript") {
				transcripts<-x
			} else {
				stop("'x' must be a corpus x or a list of transcript xs")
			}
		}
	}
	
	pb1 <- progress::progress_bar$new(
		format = "  Exporting          [:bar] :percent missing: :eta",
		total = length(transcripts), 
		clear = FALSE, 
		show_after = 0,
		width= 60)

	for (t in transcripts) {
		pb1$tick()
		export<-TRUE
		if (writeOnlyModified) {
			export <- t@modified
		}
		if (export) {
			if ("TEXTGRID" %in% stringr::str_to_upper(formats)) {
				outputPath <- file.path(outputFolder, paste(t@name, "TextGrid", sep="."))
				textgrid_export(t=t, outputPath=outputPath)
			}
			
			if ("EAF" %in% stringr::str_to_upper(formats)) {
				outputPath <- file.path(outputFolder, paste(t@name, "eaf", sep="."))
				act::eaf_export(t=t, outputPath=outputPath, createMediaLinks=createMediaLinks)
			}
		}
	}
}


