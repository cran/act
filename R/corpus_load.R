#' Load annotation files into corpus object
#' 
#' Scans all folders specified in if \code{x@folders.annotationfiles} for annotation files an loads them as transcript objects into the corpus object. 
#' All prior transcript objects will be removed.
#'
#' @param x Corpus object.
#' @param createFullText Logical; if \code{TRUE} full text will be created.
#' @param assignMedia Logical; if \code{TRUE} the folder(s) specified in \code{@folders.media} of your corpus object will be scanned for media. 
#'
#' @return Corpus object.
#' 
#' @seealso \link{corpus_new}, \link{examplecorpus}
#' 
#' @export
#' 
#' @example inst/examples/corpus_load.R
#' 
corpus_load <- function(x, createFullText=TRUE, assignMedia=TRUE) {
	#x<-corpus
	if (missing(x)) {
		stop("No corpus specified in argument 'x'")
	}
	
	start.time <- Sys.time()
	result <- ""
	
	x@read.warnings <- data.frame()
	x@read.errors  <- data.frame()
	x@read.skipped <- data.frame()
	x@transcripts <- list()
	
	
	#=== check if input folder(s) exist
	if(all(dir.exists(x@folders.annotationfiles)==FALSE)) {
		#none of the input folders exits
		result 	      <-  append(result, "ERROR: None of the folders specified in @folders.annotation files exits. Please check input folder name(s).")
		stop(result)
		return(result)
	} else if(all(dir.exists(x@folders.annotationfiles)==TRUE)) {
		#all input folders exist
		
	} else {
		#at least one folder does not exist
		result 	      <-  append(result, "Warning: the following folder(s) specified in @folders.annotation do not exist. Please check input folder name(s):")
		result 	      <-  append(result, paste("-", x@folders.annotationfiles[dir.exists(x@folders.annotationfiles)==FALSE]))
	}
	
	#=== get all paths from input folder(s)
	#get all files in folders recursively
	if (options()$act.load.scanSubfolders==TRUE) {
		filepaths <- lapply(x@folders.annotationfiles, FUN = function(x) 	list.files(x, recursive=T, pattern="*\\.textgrid", ignore.case=TRUE,  full.names=TRUE))
	} else {
		filepaths <- lapply(x@folders.annotationfiles, FUN = function(x) 	list.files(x, recursive=F, pattern="*\\.textgrid", ignore.case=TRUE,  full.names=TRUE))
	}
	filepaths <- unlist(filepaths)
	
	if (length(filepaths)==0) {
		result <- "ERROR: No textgrid files found. Please check input folder name(s)."
		stop(result)
		return(result)
	}
	
	#=== SKIPPED: double textgrids - select only the first of double textgrids
	filenames     <-  sapply(filepaths, FUN= function (x) tools::file_path_sans_ext(basename(x)) )
	result 	      <-  append(result, paste("Files found: ", length(filepaths), sep=""))
	
	#replace strings in filenames if necessary
	if (options()$act.load.replaceInFilenames.search!="") {
		#temp<-names(filenames)
		#filenames <- stringr::str_replace_all(filenames, options()$act.load.replaceInFilenames.search, options()$act.load.replaceInFilenames.replace)
		#names(filenames) <- temp
		filenames <- stringr::str_replace_all(filenames, options()$act.load.replaceInFilenames.search, options()$act.load.replaceInFilenames.replace)
		names(filenames) <- filenames
	}
	
	#check for duplicates
	if(	any(duplicated(filenames)) ) {
		doubles <- which(duplicated(filenames)==TRUE)
		result 	<- append(result, paste("- Warning:", length(doubles), "files with identical names have been skipped. Please see '@read.skipped' in your corpus x.") )
		x@read.skipped <- as.data.frame(cbind(name=filenames[doubles], path=filepaths[doubles], skipped="files with identical names"))
		filepaths 	  <- filepaths[setdiff(1:length(filepaths), doubles) ]
		filenames     <- sapply(filepaths, FUN= function (x) tools::file_path_sans_ext(basename(x)) )
		if (options()$act.load.replaceInFilenames.search!="") {
			#temp<-names(filenames)
			#filenames <- stringr::str_replace_all(filenames, options()$act.load.replaceInFilenames.search, options()$act.load.replaceInFilenames.replace)
			#names(filenames) <- temp
			filenames <- stringr::str_replace_all(filenames, options()$act.load.replaceInFilenames.search, options()$act.load.replaceInFilenames.replace)
			names(filenames) <- filenames
		}
	}
	
	#sort all textgrids alphabetically by name
	sortvector <- order(filenames)
	filepaths<-filepaths[sortvector]
	filenames<-filenames[sortvector]
	
	#=== read files
	act.environment$pb <- progress::progress_bar$new(
		format = "  Reading files      [:bar] :percent missing: :eta",
		total = length(filenames), 
		clear = FALSE, 
		show_after = 0,
		width= 60)
	
#	x@transcripts		<- lapply(filepaths, textgrid_import)

	test <- list()
	for (i in 1:length(filepaths)) {
		test[[i]] <- textgrid_import(filepaths[i])
		act.environment$pb$tick()
	}
	x@transcripts <- test
	
	if (is.null(x@transcripts))	{	stop("no textgrid files found in input directory.")	}
	
	#=== name the list
	names(x@transcripts) <- filenames
	
	#=== reading ERRORS: files that have not been read
	read.result <- lapply(x@transcripts, "slot", "read.result")
	readOk 		<- which(read.result=="ok")
	readErr 	<- which(read.result!="ok")
	
	if (length(readErr)>0) {
		result <- append(result, paste( "Reading Errors: ", length(readErr), " file(s) not read. Please see '@read.errors' in your corpus x.",sep=""))
		
		#erroneous
		x@read.errors <- 	as.data.frame(cbind(
			name = lapply(x@transcripts[readErr], "slot", "name"),
			read.encoding = lapply(x@transcripts[readErr], "slot", "read.encoding"),
			read.result = lapply(x@transcripts[readErr], "slot", "read.result"),
			read.errormessage = lapply(x@transcripts[readErr], "slot", "read.errormessage"),
			path = lapply(x@transcripts[readErr], "slot", "path")
		))
		
		#select only the OK files
		if (length(readOk)==0) 	{
			x@transcripts <- list()
		} else {
			x@transcripts <- x@transcripts[readOk]
		}
	}
	
	#=== reading WARNINGS: files that have been read with warnings
	read.errormessage 	<- lapply(x@transcripts, "slot", "read.errormessage")
	readWarn 			<- which(read.errormessage!="")
	if (length(readWarn)>0) {
		result <- append(result, paste( "Warning: ", length(readWarn), " file(s) caused problems but have been read. Please see '@read.warnings' in your corpus x.",sep=""))
		
		#warnings
		x@read.warnings <- 	as.data.frame(cbind(
			name = lapply(x@transcripts[readWarn], "slot", "name"),
			read.encoding = lapply(x@transcripts[readWarn], "slot", "read.encoding"),
			read.result = lapply(x@transcripts[readWarn], "slot", "read.result"),
			read.errormessage = lapply(x@transcripts[readWarn], "slot", "read.errormessage"),
			path = lapply(x@transcripts[readWarn], "slot", "path")
		))
	}
	
	#=== normalize transcription content in database : normalized content will be in $content.norm
	x <- act::normalize(x)
	
	#=== create full text for searches
	if (createFullText==TRUE) {
		x <- act::fulltext_update(x)
		
		#=== count words
		for (i in 1:length(x@transcripts)) {
			x@transcripts[[i]]@words.fulltext.orig 	   <- stringr::str_count(x@transcripts[[i]]@fulltext.bytime.orig, options()$act.wordCount.regex)
			x@transcripts[[i]]@words.fulltext.norm 		<- stringr::str_count(x@transcripts[[i]]@fulltext.bytime.norm, options()$act.wordCount.regex)
		}
	}
	
	#=== scan for media
	if (assignMedia) {
		x<-act::media_assign(x)
	}
	x@lastModification <- list(x@lastModification, "corpus_load")
	
	end.time <- Sys.time()
	d <- round(as.numeric(difftime(end.time, start.time, units = "secs")),2) #c("auto", "secs", "mins", "hours", "days", "weeks"))

	
	return(x)
}