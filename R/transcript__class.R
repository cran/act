methods::setClass("transcript", 
				  representation(
				  	name="character",
				  	path="character",
				  	read.encoding="character",
				  	read.result="character",
				  	read.errormessage="character",
				  	length="numeric",
				  	words.fulltext.orig="numeric",
				  	words.fulltext.norm="numeric",
				  	fulltext.bytime.orig="character",
				  	fulltext.bytime.norm="character",
				  	fulltext.bytier.orig="character",
				  	fulltext.bytier.norm ="character",
				  	tiers="ANY",
				  	data="ANY",
				  	path.media="character",
				  	modified="logical",
				  	lastModification ="list"
				  ), prototype = list (
				  	name="",
				  	path="",
				  	read.encoding="",
				  	read.result="",
				  	read.errormessage="",
				  	length=0,
				  	words.fulltext.orig=0,
				  	words.fulltext.norm=0,
				  	fulltext.bytime.orig="",
				  	fulltext.bytime.norm="",
				  	fulltext.bytier.orig="",
				  	fulltext.bytier.norm ="",
				  	tiers=list(),
				  	data=data.frame(),
				  	path.media=character(),
				  	modified=FALSE,
				  	lastModification =list()
				  )
)

transcript_show <- function (object) {
	cat("--------- transcript", fill=TRUE)
	
	cat("@name               : ", object@name, fill=TRUE)
	cat("@length             : ", helper_format_time(object@length), fill=TRUE)
	cat("@tiers              : ", length(object@tiers), fill=TRUE)
	cat("@data               : ", nrow(object@data), "datarows", fill=TRUE)
	cat("\n")
	cat("@words.fulltext.orig: ", object@words.fulltext.orig, fill=TRUE)
	cat("@words.fulltext.norm: ", object@words.fulltext.norm, fill=TRUE)
	cat("\n")
	cat("@path               : ", object@path, fill=TRUE)
	cat("@read.encoding      : ", object@read.encoding, fill=TRUE)
	cat("@read.result        : ", object@read.result, fill=TRUE)
	cat("@read.errormessage  : ", object@read.errormessage, fill=TRUE)
	cat("@path.media         : ", length(object@path.media), fill=TRUE)
	cat("@modified           : ", object@modified, fill=TRUE)
	cat("@lastModification   : ", length(object@lastModification), "messages [check directly]")
	cat()
}

methods::setMethod("show", signature = "transcript", definition = transcript_show)
