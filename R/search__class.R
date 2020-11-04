methods::setClass("search", 
				  representation(
				  	pattern="character",
				  	searchMode="character",
				  	searchNormalized="logical",
				  	startSec="numeric",
				  	endSec="numeric",
				  	filter.tiers.include="character",
				  	filter.tiers.exclude="character",
				  	filter.transcripts.include="character",
				  	filter.transcripts.exclude="character",
				  	
				  	#cutlist.mac="character",
				  	#cutlist.win="character",
				  	
				  	makeConcordance="logical",
				  	prefix="character",
				  	
				  	results="data.frame",
				  	results.nr="numeric",
				  	results.tiers.nr="numeric",
				  	results.transcripts.nr="numeric",
				  	corpus="character"
				  ), prototype = list (
				  	pattern="",
				  	searchMode="fulltext",
				  	searchNormalized=TRUE,
				  	startSec=numeric(),
				  	endSec=numeric(),
				  	filter.tiers.include="",
				  	filter.tiers.exclude="",
				  	filter.transcripts.include="",
				  	filter.transcripts.exclude="",
				  	
				  	#cutlist.mac=character(),
				  	#cutlist.win=character(),
				  	
				  	makeConcordance=TRUE,
				  	prefix="resultID",
				  	
				  	results=data.frame(),
				  	results.nr=0,
				  	results.tiers.nr=0,
				  	results.transcripts.nr=0,
				  	corpus=character()
				  )
)

search_show <- function (object) {
	cat("--------- search", fill=TRUE)
	cat("@pattern                    : ", paste("'", object@pattern,"'",sep="", collapse=""), fill=TRUE)
	cat("@searchNormalized           : ", object@searchNormalized, fill=TRUE)
	cat("@startSec                   : ", if (length(object@startSec)==0) {"[not set]"} else {if (is.na(object@startSec)){"[not set]"} else {object@startSec}}, fill=TRUE)
	cat("@endSec                     : ", if (length(object@endSec)==0  ) {"[not set]"} else {if (is.na(object@endSec)){"[not set]"} else {object@endSec}}, fill=TRUE)
	cat("@filter.tiers.include       : ", paste("'", object@filter.tiers.include,"'",sep="", collapse=""), fill=TRUE)
	cat("@filter.tiers.exclude       : ", paste("'", object@filter.tiers.exclude,"'",sep="", collapse=""), fill=TRUE)
	cat("@filter.transcripts.include : ", paste("'", object@filter.transcripts.include,"'",sep="", collapse=""), fill=TRUE)
	cat("@filter.transcripts.exclude : ", paste("'", object@filter.transcripts.exclude,"'",sep="", collapse=""), fill=TRUE)
	#cat("\n")
	#cat("@cutlist.mac                : ", if(!'hit' %in% colnames(object@results))        {"[search not run yet]"} else {if(length(object@cutlist.mac)==0) {"[not created yet]" } else {"[ok]"}}, fill=TRUE)
	#cat("@cutlist.win                : ", if(!'hit' %in% colnames(object@results))        {"[search not run yet]"} else {if(length(object@cutlist.win)==0) {"[not created yet]" } else {"[ok]"}}, fill=TRUE)
	#cat("\n")
	cat("@makeConcordance            : ", object@makeConcordance, fill=TRUE)
	cat("@prefix                     : ", paste("'", object@prefix,"'",sep="", collapse=""), fill=TRUE)
	cat("\n")
	cat("@results                    : ", if(!'hit' %in% colnames(object@results))        {"[search not run yet]"} else {"[ok]"}, fill=TRUE)
	cat("results.nr                  : ", if(!'hit' %in% colnames(object@results))        {"[search not run yet]"} else {nrow(object@results)}                       , fill=TRUE)
	cat("results.tiers.nr            : ", if(!'tier' %in% colnames(object@results))       {"[search not run yet]"} else {length(unique(object@results$tierName))}        , fill=TRUE)
	cat("results.transcripts.nr      : ", if(!'transcript' %in% colnames(object@results)) {"[search not run yet]"} else {length(unique(object@results$transcriptName))}  , fill=TRUE)
	cat("@corpus                     : ", paste("'", object@corpus,"'",sep="", collapse=""), fill=TRUE)
	#	cat()
}

methods::setMethod("show", signature = "search", definition = search_show)
