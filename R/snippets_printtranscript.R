#' Create a single print transcript
#'
#' @param t Transcript object.
#' @param startSec Double, start of selection in seconds, -1 for no selection.
#' @param endSec Double, end of selection in seconds, -1 for no selection.
#' @param header_heading Character string; text that will be used as heading.
#' @param header_firstinfo Character string; text that will used as first information in the header.
#' @param insert_arrow_dataID Integer; ID of the data row in front of which the arrow will be placed.
#' @param filepath Character string; path where to save the transcript.
#' @param alignBrackets Logical; if \code{TRUE} the function will align [] brackets that signal overlapping speech (attention: experimental function; results may not satisfy)
#'
#' @return Character string; transcript as text.
#' 
#' @export
#'
#' @example inst/examples/snippets_printtranscript.R
#' 
snippets_printtranscript <- function (t, startSec = -1, endSec = -1, insert_arrow_dataID = "", header_heading ="", header_firstinfo = "", filepath ="" , alignBrackets=FALSE) {
	#=== settings
	if (is.null(t)) {
		return("No transcript given.")
	}
	
	#===================================== check user inputs
	#--- exit if transcript width is too short or too long
	if (options()$act.transcript.transcript.width == -1) {
	} else if (options()$act.transcript.transcript.width < 40) {
		return( "ERROR: The width of the transcript is to low. Minimum is 40. Check option 'options()$act.transcript.transcript.width'")
	}
	
	#--- exit if tier length is too short or too long
	if (options()$act.transcript.speaker.width == -1) {
	} else if (options()$act.transcript.speaker.width==0) {
		return( "ERROR: Length of tier names is to short. Minimum is 1. Check option 'options()$act.transcript.speaker.width'.")
	} else if (options()$act.transcript.speaker.width < -1) {
		return("ERROR: Length of tier names is to short. Minimum is 1. Check option 'options()$act.transcript.speaker.width'.")
	} else if (options()$act.transcript.speaker.width > 25) {
		return("ERROR: Length of tier names is to long. Maximum is 25. Check option 'options()$act.transcript.speaker.width'.")
	}
	
	#--- increase
	options(act.transcript.spacesbefore= max(0, options()$act.transcript.spacesbefore))
	
	#===================================== get data
	#---get data from transcript
	if (is.null(t)) {
		return("No data found")
	} else{
		myData<- t@data
	}
	
	if (nrow(myData)==0) {
		return("no data found")
	}
	
	#--- get only relevant columns
	myCols <- c("tierName", "startSec","endSec","content")
	if (!all(myCols %in% colnames(myData))) {
		return(paste("ERROR: missing colums in data. Data needs to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	#if dataID colum is provided too
	if ("dataID" %in% colnames(myData)) {
		myData <- myData[, c("dataID", myCols)]
	} else {
		myData <- cbind(dataID=NA, myData[,myCols])
	}
	
	myData$tierName <- as.character(myData$tierName)
	
	#---filter data by tiers, if filter is set
	#if include filter is set
	if (!options()$act.export.filter.tiers.include=="") {
		myData <- myData[stringr::str_detect(myData$tierName, options()$act.export.filter.tiers.include), ]
	}
	
	#if exclude filter is set
	if (!options()$act.export.filter.tiers.exclude=="") {
		myData <- myData[!stringr::str_detect(myData$tierName, options()$act.export.filter.tiers.exclude), ]
	}
	
	
	if (nrow(myData)==0) {
		return("no data after filtering tiers")
	}
	
	
	#--- select temporal part
	if (startSec>=0 & endSec>=0) {
		if (startSec>endSec){
			stop("wrong startSec or endSec")
			return("wrong startSec or endSec")
		}
		#	myData <- myData[(myData$startSec>=startSec & myData$startSec<endSec), ]
		myData <- myData[(myData$endSec>startSec & myData$startSec<endSec), ]
	}
	
	#--- exit if no data is selcted are selected
	if (nrow(myData)==0 ){
		return ("")
	}
	
	#--- sort data by start time
	myData <- myData[order(myData$startSec), ]
	
	#===================================== space before
	myData$spacebefore <- stringr::str_pad("", width=options()$act.transcript.spacesbefore, side="left", pad=" ")
	#insert arrow
	if (!is.na(insert_arrow_dataID)) {
		if (length(which(myData$dataID==insert_arrow_dataID))>0) {
			myData$spacebefore[which(myData$dataID==insert_arrow_dataID)[1]] <- stringr::str_pad(options()$act.transcript.arrow.shape, width=options()$act.transcript.spacesbefore, side="right", pad=" ")
		}
	}
	
	#===================================== line numbers
	line_numbers <- as.character(1:nrow(myData))
	#add 0 for 1-9
	line_numbers[1:min(length(line_numbers),9)] <- stringr::str_pad(line_numbers[1:min(length(line_numbers),9)], width=2, side="left", pad="0")
	#set line numbers
	myData$line <- line_numbers
	
	#===================================== speakers
	#--- get tier names
	tierNames 				<- as.character(unique(myData$tierName))
	if (options()$act.transcript.pauseTier.regex=="") {
		#if no pause filter -> get all names
		tierNames_withoutPause <- tierNames
	} else {
		#if  pause filter -> get all names but the pause tier
		tierNames_withoutPause	<- tierNames[setdiff(1:length(tierNames), grep(options()$act.transcript.pauseTier.regex, tierNames, ignore.case =TRUE, perl = TRUE))]
	}
	
	# width of the speaker abbreviations (without end character)
	text_body_width_speaker <- options()$act.transcript.speaker.width
	if (text_body_width_speaker == -1) {
		#if full name : get maximum
		text_body_width_speaker <- max(nchar(as.character(tierNames_withoutPause)))
	}
	
	#--- take the tier name
	myData$speaker <- myData$tierName
	
	
	#--- same speaker as before ? --> set to ""
	myData_speaker_previous <- c("", myData$speaker[1:length(myData$speaker)-1] )
	sameSpeaker_pos <- myData_speaker_previous==myData$speaker
	myData$speaker[sameSpeaker_pos] <- ""
	included_speakers_pos <- !sameSpeaker_pos
	
	#--- pause tier ? --> set to ""
	if (options()$act.transcript.pauseTier.regex!="") {
		#set pauses to ""
		pauses_pos <- grep(options()$act.transcript.pauseTier.regex, myData$speaker, ignore.case =TRUE, perl = TRUE)
		myData$speaker[pauses_pos] <- ""
		included_speakers_pos[pauses_pos] <- FALSE
	}
	
	#all others: cut to correct length and add end sign
	myData$speaker[included_speakers_pos] <- paste(substr(myData$speaker[included_speakers_pos],1, text_body_width_speaker), options()$act.transcript.speaker.ending, sep="")
	
	#add missing spaces
	myData$speaker <- stringr::str_pad(myData$speaker, width=text_body_width_speaker+nchar(options()$act.transcript.speaker.ending), side="right", pad=" ")
	
	
	#===================================== text section
	myData <- myData[order(myData$startSec), ]
	text_exdent <- options()$act.transcript.spacesbefore + text_body_width_speaker + nchar(options()$act.transcript.speaker.ending) + 3
	text_body_width   <- options()$act.transcript.transcript.width - text_exdent
	text_all <- c()	
	
	
	myData$text <- myData$content
	myData$text <- stringr::str_trim(myData$text)
	myData$bracketsLeftAligned <- FALSE
	#myData$text[21]
	
	#---- brackets
	if (options()$act.transcript.brackets.tryToAlign 		== TRUE) {
		for (i in 1:length(myData$text)) {
			
			#check if the first bracket is already aligned
			if (myData$bracketsLeftAligned[i]) {
				startWithBracketI<-2					
			} else { 
				startWithBracketI<-1	
			}
			
			#check if text contains a bracket
			openingBracketsI.nr 	<- 	stringr::str_count(myData$text[i], "\\[")
			
			#check if there are more brackets to be aligned
			if (startWithBracketI<=openingBracketsI.nr) {
				
				#search correspondance for each opening bracket
				for (x in startWithBracketI:openingBracketsI.nr) {
					
					#exit if next recourdset would be outside the data frame 
					startSearching <- i+1
					if (startSearching>length(myData$text)) {
						break
					}
					
					#start searching at the next dataset
					for (j in startSearching:length(myData$text)) {
						
						#exit: if this recordset does not temporally overlap
						if (myData$startSec[j]> myData$endSec[i]) {
							break
						} 
						
						#skip recordset : if it has already been move/aligned
						if (!myData$bracketsLeftAligned[j]) {
							openingBracketsJ.nr <- 		stringr::str_count(myData$text[j], "\\[")
							
							
							if (openingBracketsJ.nr>0 ) {
								#get positions and contents of brackets
								bracket.i <- as.data.frame(stringr::str_locate_all(myData$text[i], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.i <- bracket.i[x, ]
								bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(myData$text[i], "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
								bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
								bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(myData$text[i], bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
								bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(myData$text[i], bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)	
								bracket.i <- cbind(bracket.i, posSpaceInside=as.data.frame(stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"))$start, row.names = NULL)
								
								bracket.j <- as.data.frame(stringr::str_locate_all(myData$text[j], "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.j <- bracket.j[1, ]
								bracket.j <- cbind(bracket.j, content=as.character(unlist(stringr::str_extract_all(myData$text[j], "\\[.*?\\]"))[1]), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, bracketLength=stringr::str_length(bracket.j$content) , row.names = NULL)
								bracket.j <- cbind(bracket.j, before=stringr::str_trim(stringr::str_sub(myData$text[j], bracket.j$start-1, bracket.j$start-1 )), row.names = NULL)
								bracket.j <- cbind(bracket.j, after=stringr::str_trim(stringr::str_sub(myData$text[j], bracket.j$end+1, bracket.j$end+1 )), row.names = NULL) #mind the 1
								bracket.j <- cbind(bracket.j, posSpaceInside=as.data.frame(stringi::stri_locate(as.character(bracket.j$content), regex=" ", mode="last"))$start, row.names = NULL)
								
								#something went wrong, probably missing closing bracket
								if (is.null(bracket.i$bracketLength)) {		break}
								if (is.na(bracket.i$bracketLength)) {		break}
								if (is.null(bracket.j$bracketLength)) {		break}
								if (is.na(bracket.j$bracketLength)) {		break}
								
								#--- put j annotation j in the right place 
								myData$text[j] <- stringr::str_flatten(c(strrep(" ", abs(bracket.i$start-bracket.j$start)), myData$text[j]) , collapse="")
								myData$bracketsLeftAligned[j] <- TRUE
								
								
								
								#---- adjust length of brackets by inserting spaces
								#check which is the longer bracket
								difference <- bracket.j$bracketLength - bracket.i$bracketLength
								new<-""
								
								if (difference>0) {
									#modify i
									difference <- abs(difference)
									
									if (bracket.i$after=="" | stringr::str_detect(bracket.i$after,"\\W")) {
										#if content ends after ] or a non-word charater follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(myData$text[i], start=1, end=bracket.i$end-1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myData$text[i], bracket.i$end, stringr::str_length(myData$text[i])), sep="",
										#					  collapse="")
										
									} else if (!is.na(bracket.i$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										
										insert.pos <- bracket.i$start + bracket.i$posSpaceInside-1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(myData$text[i], start=1,  end=bracket.i$start + bracket.i$posSpaceInside-1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myData$text[i], bracket.i$start +bracket.i$posSpaceInside, stringr::str_length(myData$text[i])),
										#					  sep="", collapse="")
										
									} else if (bracket.i$before=="") {
										#	if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.i$start
										insert.char <- " "
										
										#	 new <- stringr::str_c(stringr::str_sub( myData$text[i], start=1, end= bracket.i$start), 
										#						  strrep(" ", difference), 
										#						  stringr::str_sub(myData$text[i], bracket.i$start+1, stringr::str_length(myData$text[i])), sep="",
										#						  collapse="")
										
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
										
										#new <- stringr::str_c(stringr::str_sub(myData$text[i], start=1, end= bracket.i$end-1), 
										#					  strrep("_", difference), 
										#					  stringr::str_sub(myData$text[i], bracket.i$end, stringr::str_length(myData$text[i])), sep="",
										#					  collapse="")
									}
									new <- stringr::str_c(stringr::str_sub(myData$text[i], start=1, end=insert.pos), 
														  strrep(insert.char, difference), 
														  stringr::str_sub(myData$text[i], end=insert.pos+1, stringr::str_length(myData$text[i])), sep="",
														  collapse="")
									myData$text[i] <- new
									
								} else if (difference<0) {
									difference <- abs(difference)
									
									#modify j
									if (bracket.j$after=="" | stringr::str_detect(bracket.j$after,"\\W")) {
										#if content ends after ] or a non-word charater follows --> space immediately before ]
										insert.pos <- bracket.i$end-1
										insert.char <- " "
										
										#				new <- stringr::str_c(stringr::str_sub(myData$text[j], start=1,  end=bracket.j$end-1), 
										#				  strrep(" ", difference), 
										#				  stringr::str_sub(myData$text[j], bracket.j$end, stringr::str_length(myData$text[j])), 
										#				  sep="", collapse="")
										
									} else if (!is.na(bracket.j$posSpaceInside)) {
										#if there is a space inside the bracket --> insert spaces there
										insert.pos <- bracket.j$start + bracket.j$posSpaceInside -1
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub(myData$text[j], start=1,  end=bracket.j$start + bracket.j$posSpaceInside -1), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myData$text[j], bracket.j$start + bracket.j$posSpaceInside, stringr::str_length(myData$text[j])),
										#					  sep="", collapse="")
										
									} else if (bracket.j$before=="") {
										#if blanks or annotation starts before [ -> insert spaces after the opening bracket 
										insert.pos <- bracket.j$start
										insert.char <- " "
										
										#new <- stringr::str_c(stringr::str_sub( myData$text[j], start=1, end=bracket.j$start), 
										#					  strrep(" ", difference), 
										#					  stringr::str_sub(myData$text[j], bracket.j$start+1,  end=stringr::str_length(myData$text[j])), sep="",
										#					  collapse="")
										
									} else {
										#insert underscores before ]
										insert.pos <- bracket.i$end-1
										insert.char <- "_"
										
										#new <- stringr::str_c(stringr::str_sub(myData$text[j], start=1,  end=bracket.j$end-1), 
										#					  strrep("_", difference), 
										#					  stringr::str_sub(myData$text[j], bracket.j$end,  end=stringr::str_length(myData$text[j])), sep="",
										#					  collapse="")	
									}
									
									new <- stringr::str_c(stringr::str_sub( myData$text[j], start=1, end=insert.pos), 
														  strrep(insert.char, difference), 
														  stringr::str_sub(myData$text[j], insert.pos+1,  end=stringr::str_length(myData$text[j])), sep="",
														  collapse="")
									
									
									myData$text[j] <- new
								}
								
								#---- align j annotation
								#wrap i without initioal or exdent
								text_i_wrapped <- myData$text[i]
								text_i_wrapped <-  stringi::stri_wrap(text_i_wrapped, width = text_body_width, indent=0, normalize=FALSE, whitespace_only=TRUE)
								
								#add spaces in the end to fill to full width of text
								text_i_wrapped <- stringr::str_pad(string=text_i_wrapped, width=text_body_width, side="right")
								
								#joint text to long string
								text_i_wrapped <- stringr::str_flatten(text_i_wrapped, collapse="")
								
								
								#calculate positions of brackets again
								bracket.i <- as.data.frame(stringr::str_locate_all(text_i_wrapped, "\\[.*?\\]"), stringsAsFactors = FALSE)
								bracket.i <- bracket.i[x, ]
								bracket.i <- cbind(bracket.i, content=as.character(unlist(stringr::str_extract_all(text_i_wrapped, "\\[.*?\\]"))[x]), row.names = NULL) #mind the x
								bracket.i <- cbind(bracket.i, bracketLength=stringr::str_length(bracket.i$content) , row.names = NULL)
								bracket.i <- cbind(bracket.i, before=stringr::str_trim(stringr::str_sub(text_i_wrapped, bracket.i$start-1, bracket.i$start-1 )), row.names = NULL)
								bracket.i <- cbind(bracket.i, after=stringr::str_trim(stringr::str_sub(text_i_wrapped, bracket.i$end+1, bracket.i$end+1 )), row.names = NULL)	
								bracket.i <- cbind(bracket.i, posSpaceInside=as.data.frame(stringi::stri_locate(as.character(bracket.i$content), regex=" ", mode="last"))$start, row.names = NULL)
								
								
							} #there are opening brackets in j
						} #recordset has already been aligned
					} # j
				} #next w
			} #i bracket needs still to be processed
		} #next i
	} #if brackets need to be aligned
	
	
	#wrap and build entire text
	for (i in 1:length(myData$text)) {
		
		if (i==100) {	text_exdent <- text_exdent +1 }
		if (i==1000) { 	text_exdent <- text_exdent +1 }
		if (i==10000) { text_exdent <- text_exdent +1 }
		
		text_line <- myData$text[i]	
		
		#remove unnexessary spaces that make up for an entire line	
		searchString <- paste("^ {", as.character(text_body_width),"}", sep="")
		for (j in 1:10){
			text_line <- stringr::str_replace(text_line, searchString, "")
		}
		#remove line breaks that will lead to an error
		searchString <- "\\r?\\n"
		text_line <- stringr::str_replace_all(text_line, searchString, "")
		
		#wrap
		text_initial <- paste(myData$spacebefore[i], myData$line[i], " ", myData$speaker[i],  sep="")
		text_line <-  stringi::stri_wrap(text_line, width = text_body_width, indent=0, exdent=text_exdent, normalize=FALSE, initial=text_initial, whitespace_only=TRUE)
		
		#add line to entire text
		text_all  <- c(text_all, text_line)
		
		#add additional  lines
		if (options()$act.transcript.additionalline1.insert == TRUE) {
			text_additionalline <- options()$act.transcript.additionalline1.text
			if (options()$act.transcript.additionalline1.indent == TRUE) {
				text_additionalline <- stringi::stri_wrap(text_additionalline, width = text_body_width, indent=stringi::stri_length(text_initial) , exdent=0, normalize=FALSE, initial="", whitespace_only=TRUE)
			} 
			text_all  <- c(text_all, text_additionalline)
		}
		if (options()$act.transcript.additionalline2.insert == TRUE) {
			text_additionalline <- options()$act.transcript.additionalline2.text
			if (options()$act.transcript.additionalline2.indent == TRUE) {
				text_additionalline <- stringi::stri_wrap(text_additionalline, width = text_body_width, indent=stringi::stri_length(text_initial), exdent=0, normalize=FALSE, initial="", whitespace_only=TRUE)
			} 
			text_all  <- c(text_all, text_additionalline)
		}
	} 
	
	output<-text_all
	
	#=== header
	if (options()$act.transcript.header.insert==TRUE) {
		header <- paste(header_heading, "\n(", sep="")
		if (header_firstinfo!="") {
			header <- paste(header, header_firstinfo, ", ", sep="")
		}
		header <- paste(header, t@name , ", ", sep="")
		
		if (startSec<0) {
			header <- paste(header, "0-", sep="")
		} else {
			header <- paste(header, round(startSec, digits=0), "-", sep="")
		}
		if (endSec<0) {
			header <- paste(header, round(t@length, digits=0), " sec)\n", sep="")
		} else {
			header <- paste(header, round(endSec, digits=0), " sec)\n", sep="")
		}
		
		output <- c(header, output)
	}
	
	
	if (filepath!="") {
		fileConn<-file(filepath)
		writeLines(output, fileConn)
		close(fileConn)
	}
	
	#	output<-stringr::str_flatten(output, collapse="\\n")
	
	
	return(output)
}