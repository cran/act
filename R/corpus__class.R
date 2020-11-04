methods::setClass("corpus", 
				  representation(
				  	name="character",
				  	folders.annotationfiles="character",
				  	folders.media="character",
				  	read.warnings="data.frame",
				  	read.errors="data.frame",
				  	read.skipped="data.frame",
				  	overview="data.frame",
				  	transcripts ="list",
				  	lastModification ="list",
				  	normalization.upToDate ="logical",
				  	fulltext.upToDate ="logical",
				  	fulltext.currentlyset.filter.tiers.include="character",
				  	fulltext.currentlyset.filter.tiers.exclude="character" 
				  ), prototype = list (
				  	name="NewCorpus",
				  	folders.annotationfiles=character(),
				  	folders.media=character(),
				  	read.warnings=data.frame(),
				  	read.errors=data.frame(),
				  	read.skipped=data.frame(),
				  	overview=data.frame(),
				  	transcripts =list(),
				  	lastModification =list(),
				  	normalization.upToDate =FALSE,
				  	fulltext.upToDate =FALSE,
				  	fulltext.currentlyset.filter.tiers.include="",
				  	fulltext.currentlyset.filter.tiers.exclude=""
				  )
)

corpus_show <- function (object) {
	cat("===================== corpus", fill=TRUE)
	
	cat("@name                       : ", object@name, fill=TRUE)
	cat("@transcripts                : ", length(object@transcripts), "transcripts", fill=TRUE)
	
	cat("words.fulltext.orig         : ", sum(unlist(lapply(object@transcripts, "slot", "words.fulltext.orig"))), fill=TRUE)
	cat("words.fulltext.norm         : ", sum(unlist(lapply(object@transcripts, "slot", "words.fulltext.norm"))), fill=TRUE)
	t <- sum(unlist(lapply(object@transcripts, "slot", "length")))
	cat("length                      : ", helper_format_time(t)	, fill=TRUE)
	
	
	cat("\n")
	cat("@folders.annotationfiles    : ", length(object@folders.annotationfiles), "folder(s)", fill=TRUE)
	cat("@folders.media              : ", length(object@folders.media), "folder(s)", fill=TRUE)
	cat("\n")
	cat("@read.warnings              : ", nrow(object@read.warnings), "warning(s)", fill=TRUE)
	cat("@read.errors                : ", nrow(object@read.errors), "error(s)", fill=TRUE)
	cat("@read.skipped               : ", nrow(object@read.skipped), "skipped", fill=TRUE)

	cat("\n")
	cat("@lastModification:                           ", length(unlist(object@lastModification)), "messages [check directly]" , fill=TRUE)
	cat("@normalization.upToDate:                     ", object@normalization.upToDate, fill=TRUE)
	cat("@fulltext.upToDate:                          ", object@fulltext.upToDate, fill=TRUE)
	
	cat("@fulltext.currentlyset.filter.tiers.include: ", paste("'", object@fulltext.currentlyset.filter.tiers.include,"'",sep="", collapse=""), fill=TRUE)
	cat("@fulltext.currentlyset.filter.tiers.exclude: ", paste("'", object@fulltext.currentlyset.filter.tiers.exclude,"'",sep="", collapse=""), fill=TRUE)
	cat()
	
}

methods::setMethod("show", signature = "corpus", definition = corpus_show)
#methods::setMethod("test", signature = "corpus", definition = corpus_test)
#removeMethod("show", signature = "transcript")

#' Information about all transcripts in a corpus object
#'
#' @param x Corpus object.
#'
#' @return Data frame.
#' @export
#'
#' @examples
#' library(act)
#' 
#' myoverview<- act::corpus_overview(examplecorpus)
#' myoverview
#' 
corpus_overview <- function(x) {
	#alternative code
	#myTable <- as.data.frame(cbind(
	#	name 				= lapply(x@transcripts, "slot", "name"),
	#	length 				= lapply(x@transcripts, "slot", "length"),
	#	tiers	            = lapply(lapply(x@transcripts, "slot", "tiers"),length),
	#	words.fulltext.orig = lapply(x@transcripts, "slot", "words.fulltext.orig"),
	#	words.fulltext.norm = lapply(x@transcripts, "slot", "words.fulltext.norm"),
	#	
	#	read.encoding 		= lapply(x@transcripts, "slot", "read.encoding"),
	#	read.result 		= lapply(x@transcripts, "slot", "read.result"),
	#	read.errormessage 	= lapply(x@transcripts, "slot", "read.errormessage"),
	#	path 				= lapply(x@transcripts, "slot", "path")
	#
	#))
	#return(myTable)
	
		myDF <- 	data.frame( 
		name=character(),
		length.sec=double(),
		length.formatted=character(),
		tiers=integer(),
		data=integer(),
		words.fulltext.orig=integer(),
		words.fulltext.norm=integer(),
		path=character(),
		read.encoding=character(),
		read.result=character(),
		read.errormessage=character(),
		path.media=integer(),
		modified=integer(),
		lastModification=character(), 
		stringsAsFactors=FALSE
	)

	for (i in 1:length(x@transcripts)) {
		myRow <- 	as.data.frame(c( 
			c(name=x@transcripts[[i]]@name),
			c(length=x@transcripts[[i]]@length),
			c(length.formatted=helper_format_time(x@transcripts[[i]]@length)),
			c(tiers=length(x@transcripts[[i]]@tiers)),
			c(data=nrow(x@transcripts[[i]]@data)),
			c(words.fulltext.orig=x@transcripts[[i]]@words.fulltext.orig),
			c(words.fulltext.norm=x@transcripts[[i]]@words.fulltext.norm),
			c(path=x@transcripts[[i]]@path),
			c(read.encoding=x@transcripts[[i]]@read.encoding),
			c(read.result=x@transcripts[[i]]@read.result),
			c(read.errormessage=x@transcripts[[i]]@read.errormessage),
			c(path.media=length(x@transcripts[[i]]@path.media)),
			c(modified=x@transcripts[[i]]@modified),
			c(lastModification=x@transcripts[[i]]@lastModification)
		))
		myDF<-rbind(myDF,myRow)
	}
	return (myDF)
}



#' Get all data in transcripts
#'
#' Merges data from all transcripts in a corpus and returns a data frame.
#' 
#' @param x Corpus object.
#'
#' @return Data.frame.
#' @export
#'
#' @examples
#' 
#' library(act)
#' 
#' #Get data frame with all data
#' alldata <- act::corpus_alldata(examplecorpus)
#' 
#' #Have a look at the number of annotations
#' nrow(alldata)
#' 
corpus_alldata <- function(x) {
	temp <- data.frame()
	for (i in x@transcripts){
		temp<- rbind(temp, i@data)	
	}
	return(temp)
}


