#' Make concordance for search results
#'
#' @param x Corpus object.
#' @param s Search object.
#' @param baseForConcordance Character string; takes the following values: content, fulltext (= default)
#' @param searchNormalized Logical; if \code{TRUE} function will search in the normalized content, if \code{FALSE} function will search in the original content.
#' @param showProgress Logical; if \code{TRUE} progress bar will be shown.
#'
#' @return Data.frame; data frame with search results and concordance added.
#' @export
#'
#' @example inst/examples/search_concordance.R
#' 
search_concordance <- function(x, s, baseForConcordance="fulltext", searchNormalized=TRUE, showProgress=FALSE) {
	
	search_concordance_single <- function(mySR, x, showProgress)	{
		
		if(is.na(mySR["dataID"])) { return(rep("",5)) }
		
		#nrow(searchResults)
		hit.pos.fulltext    <- 	strtoi(mySR["hit.pos.fulltext"])
		hit.pos.content    	<- 	strtoi(mySR["hit.pos.content"])
		hit.length	    	<-	strtoi(mySR["hit.length"])
		myTName	<-	as.character(mySR["transcriptName"])
		
		# progress
		if (showProgress==TRUE)		{	
			if (exists("act.environment", mode="environment")) {
				if(exists("pb", envir=act.environment)) {
					act.environment$pb$tick()
				}
			}
		}
		
		
		# get text for concordance
		if (baseForConcordance=="fulltext") 		{
			#get hit pos
			hit.pos 				<- hit.pos.fulltext
			
			#get the full text
			if (as.character(mySR["searchMode"]) =="byTier") {
				if (s@searchNormalized==TRUE) {
					myFulltext     <- x@transcripts[[myTName]]@fulltext.bytier.norm
				} else  {
					myFulltext     <- x@transcripts[[myTName]]@fulltext.bytier.orig
				}
			} else {
				if (s@searchNormalized==TRUE) {
					myFulltext     <- x@transcripts[[myTName]]@fulltext.bytime.norm
				} else  {
					myFulltext     <- x@transcripts[[myTName]]@fulltext.bytime.orig
				}
			}
			
			if (is.na(myFulltext) == TRUE) {
				stop("Please recreate full text.")
			}
		} else {
			#get hit pos
			hit.pos 				<- hit.pos.content
			
			#get content
			if (s@searchNormalized==TRUE) {
				myFulltext     <- mySR["content.norm"]
			} else  {
				myFulltext     <- mySR["content"]
			}
		}
		
		#===left part
		leftPart 	<- 	""
		concLeft1	<- ""
		concLeft2	<- ""
		
		if (hit.pos>1)		{
			#get everything left of the hit
			leftMargin 	<-	max(0, hit.pos-options()$act.concordanceWidth-1)
			leftPart  	<- 	substr(myFulltext, leftMargin, hit.pos-1)
			
			#regex
			#regex_last_word <- paste("(?<concLeft1>[^\\s|\\|\\'|\\#|\\/|\\\\\\\\", options()$act.separator_between_intervals, options()$act.separator_between_tiers, "]*[\\W]*$)",sep="", collapse="")
			regex_last_word <- paste("(?<concLeft1>[", options()$act.separator_between_words, options()$act.separator_between_intervals, options()$act.separator_between_tiers, "]*[\\W]*$)",sep="", collapse="")
			
			#get last word
			concLeft1 <- stringr::str_extract(leftPart, regex_last_word)
			
			#get position last word
			pos <- stringr::str_locate(leftPart, regex_last_word)[1]-1
			
			#get everything before
			concLeft2 <-  stringr::str_sub(leftPart, 1, pos)
			
			if(is.na(concLeft1)) { concLeft1 <- ""}
			if(is.na(concLeft2)) { concLeft2 <- ""}
			
			#remove spaces
			concLeft1 <- stringr::str_trim(concLeft1, side="both")
			concLeft2 <- stringr::str_trim(concLeft2, side="both")
		}
		
		#===right part
		concRight1	<- ""
		concRight2	<- ""
		rightPart 	<- 	""
		if (hit.pos +  hit.length < nchar(myFulltext) )		{
			#get everything right of the hit
			rightMargin <- 	hit.pos +  hit.length + options()$act.concordanceWidth
			rightMargin <- 	min(rightMargin, nchar(myFulltext))
			rightPart 	<- 	substr(myFulltext, hit.pos + hit.length , rightMargin)
			
			#regex
			#regex_first_word <- paste("(?<concRight1>^[\\W]*[^\\s|\\|]", options()$act.separator_between_intervals, options()$act.separator_between_tiers, "]*)",sep="", collapse="")
			regex_first_word <- paste("(?<concRight1>^[\\W]*[", options()$act.separator_between_words, options()$act.separator_between_intervals, options()$act.separator_between_tiers, "]*)",sep="", collapse="")
			
			#get first word
			concRight1 <- stringr::str_extract(rightPart, regex_first_word)
			
			#get position last word
			pos <- stringr::str_locate(rightPart, regex_first_word)[2]+1
			
			#get everything after
			concRight2 <-  stringr::str_sub(rightPart,pos, nchar(rightPart))
			
			if(is.na(concRight1)) { concRight1 <- ""}
			if(is.na(concRight2)) { concRight2 <- ""}
			
			#remove spaces
			concRight1 <- stringr::str_trim(concRight1, side="both")
			concRight2 <- stringr::str_trim(concRight2, side="both")
		}
		
		concHit <- mySR["hit"]
		concHit <- unname(unlist(concHit))
		
		#count words
		if (baseForConcordance=="content") 		{
			nrWordsLeft <- as.integer(stringi::stri_count_words(leftPart))
			nrWordsHitPosition <- as.integer(nrWordsLeft + 1)
			nrWordsHit <- as.integer(stringi::stri_count_words(mySR["hit"]))
			nrWordsRight <- as.integer(stringi::stri_count_words(rightPart))
			nrWordsTotal <- as.integer(nrWordsLeft + nrWordsHit + nrWordsRight)
		} else {
			nrWordsLeft <- NA
			nrWordsHitPosition <- NA
			nrWordsHit <- NA
			nrWordsRight <- NA
			nrWordsTotal <- NA
		}
		return(c(concLeft2, concLeft1, concHit, concRight1, concRight2, nrWordsLeft, nrWordsHitPosition, nrWordsHit, nrWordsRight, nrWordsTotal))
	}
	
	
	if (nrow(s@results)==0 ) {
		concs 				  <- data.frame(concLeft2=character(), concLeft1=character(), concHit=character(), concRight1=character(), concRight2=character(), nrWordsLeft=integer(), nrWordsHitPosition=integer(), nrWordsHit=integer(), nrWordsRight=integer(), nrWordsTotal=integer())
	} else {
		concs			      <- t(apply(s@results, MARGIN=1, x=x, showProgress=showProgress, search_concordance_single))
		concs			      <- data.frame(concs)
		conccolnames	  	  <- c("concLeft2", "concLeft1", "concHit", "concRight1", "concRight2", "nrWordsLeft", "nrWordsHitPosition", "nrWordsHit", "nrWordsRight", "nrWordsTotal")
		colnames(concs) 	  <- conccolnames
	}
	return(concs)
}
