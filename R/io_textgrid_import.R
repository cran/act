#' Import a single 'Praat' '*.TextGrid' file
#'
#' @param input_path Character string; input path of a single 'Praat' '*.TextGrid' file.
#' 
#' @return Transcript object.
#' @export
#'
#' @example inst/examples/io_textgrid_import.R
#' 
textgrid_import <- function(input_path) {
	#--- empty
	# input_path <- "/Users/oliverehmer/Dropbox/Körperwissen_dropbox/conocimientos_incorporados/TextGrids_Argentina/esp_icas/esp_icas103__2020-05-04b.TextGrid"
	#input_path <- filepaths[1]
	# input_path <- "/Users/oliverehmer/Documents/Corpora/textgrids/test/empty/Parte3_Pers31_PRAAT.textGrid" #Empty
	#--- normal
	# input_path <- "/Users/oliverehmer/Documents/Corpora/textgrids/fra/fra_cm/fra_cm08__mic.TextGrid"
	#--- point
	# input_path <- "/Users/oliverehmer/Documents/Corpora/textgrids/test/point/point 1.textgrid"
	# input_path <- "/Users/oliverehmer/Documents/Corpora/textgrids/test/point/point 2.TextGrid"
	#  textgrid_import(input_path)
	# input_path <- "/Users/oliverehmer/Desktop/correction.TextGrid"
	#--doublequotes
	#input_path<-"/Users/oliverehmer/Documents/Corpora_tech/act/_sample_textgrids/fehler.TextGrid"
	#double tier names
	#input_path<-"/Users/oliverehmer/körperwissen/kw_files/Transkriptionen/TextGrids/esp_icas204__2019-05-29.TextGrid"
	
	#test
	#input_path<-"/Users/oliverehmer/Dropbox/corpora/esp_everyday/esp_1101__2020-07-10.TextGrid"
	
	myTrans <- methods::new("transcript")
	#--- check if file exists
	if (!file.exists(input_path)) {
		myTrans@read.result 		<- "error"
		myTrans@read.errormessage   <- "File does not exist."
		return(myTrans)
	}
	
	#---set result
	myResult 		<- "ok"
	myErrormessage 	<- ""
	
	#--- get transcript name
	myTrans 					<- methods::new("transcript")
	myTrans@name 				<- tools::file_path_sans_ext(basename(input_path))
	myTrans@path 				<- input_path
	myTrans@read.result 		<- myResult
	myTrans@read.errormessage 	<- myErrormessage
	myTrans@modified 			<- FALSE
	
	
	
	#replace strings in file names if necessary
	if (options()$act.load.replaceInFilenames.search!="") {
		myTrans@name <- stringr::str_replace_all(myTrans@name, options()$act.load.replaceInFilenames.search, options()$act.load.replaceInFilenames.replace)
	}
	
	#=========================================================================================
	# i get bad results  reading: UTF8 files with enc=LATIN1		# (but latin-files will not be read in UTF8
	#--> thats why i need to try first with utf
	myEncodings	<-c("UTF-16", "UTF-8", "LATIN1")
	
	#test all encondings, tell me in the end which worked
	mytg <- NULL
	myTrans@read.encoding <- "unknown"
	for (myEncoding in myEncodings)	{
		#try to read first 2 lines
		result	<- helper_test_read(input_path, myEncoding, 2)
		
		#if that worked
		if (result[1]!="error")		{
			# try to read all lines
			result	<- helper_test_read(input_path, myEncoding, -1)
			
			#if that worked
			if (result[1]!="error") 			{
				
				#  Check here if firstline[1] contains "ooTextFile"
				#  check if the second line says textgrid
				if(length(grep("ooTextFile", result[1]))!=0)
				{
					if(length(grep("TextGrid", result[2]))!=0)
					{
						mytg <- result
						myTrans@read.encoding 		<- myEncoding
						break()
					}
				}
			}
		}
	}
	
	
	if(is.null(mytg)) 	{
		myTrans@read.result 		<- "error"
		myTrans@read.errormessage   <- "File not recognized as TextGrid."
		return(myTrans)
	}
	
	#=== merge lines into a long text
	mytg.merge <- stringr::str_c(mytg, collapse = "\n")
	
	#===set transcript length
	rexeg_alltimes <- '((?:xmin|number)\\s=\\D*)([\\d\\.]*)(?:(?:[\\r\\n\\s]*xmax\\s=\\D*)([\\d\\.]*))'
	alltimes <- stringr::str_match_all(mytg.merge, rexeg_alltimes)
	myTrans@length <- max(as.double(alltimes[[1]][,4]))
	
	#== extract tier info
	#regex_tierinfo <- '(?<!Object\\s)(?:class\\s=\\s")(.+?)(?s:\\".*?name\\s=\\s")(.*?)(?s:\\".*?xmin\\s=\\s)([\\d\\.]*)(?s:.*?xmax\\s=\\s)([\\d\\.]*)(?s:.*?(?:intervals|points):\\ssize\\s=\\s)(\\d*)'
	regex_tierinfo <- '(?<!Object\\s)(?:class\\s=\\s")(.+?)(?s:\\".*?name\\s=\\s")(.*?)(?s:\\".*?xmin\\s=)(.*\\d)(?s:.*?xmax\\s=)(.*\\d)(?s:.*?(?:intervals|points):\\ssize\\s=)(.*\\d)'
	tierinfo <- stringr::str_match_all(mytg.merge, regex_tierinfo)
	tierinfo <- do.call(rbind, lapply(tierinfo, data.frame, stringsAsFactors=FALSE))
	colnames(tierinfo)<-c("none","type","tierName", "xmin","xmax","size")
	tierinfo<-tierinfo[,c("type","tierName","xmin","xmax","size")]
	
	tierinfo$xmin <- as.double(tierinfo$xmin)
	tierinfo$xmax <- as.double(tierinfo$xmax)
	tierinfo$size <- as.integer(tierinfo$size)
	
	if (nrow(tierinfo)==0)  	{
		myTrans@read.result 		<- "error"
		myTrans@read.errormessage   <- "TextGrid did not contain any tiers."
		return(myTrans)
	}
	
	#---create unique tierNames
	while (length(tierinfo$tierName[duplicated(tierinfo$tierName)])>0)  	{
	#get multiple tierNames

			multiple <- tierinfo$tierName[duplicated(tierinfo$tierName)]
			for (myName in multiple)
			{
				tierinfo$tierName[tierinfo$tierName==myName]<-paste(tierinfo$tierName[tierinfo$tierName==myName],1:length(tierinfo$tierName[tierinfo$tierName==myName]),sep="-")
			}

		myTrans@read.result 		<- "ok"
		myTrans@read.errormessage   <- "Some tiers have been renamed since their names were not unique."
	}

	alltierNames <- rep(tierinfo$tierName, tierinfo$size)
	
	#== extract data
	#regex_main <- '(?:(?:intervals|points)\\s\\[)(\\d*)(?:\\]:[\\r\\n\\s]*(?:xmin|number)\\s=\\s)([\\d\\.]*)(?:(?:[\\r\\n\\s]*xmax\\s=\\s)([\\d\\.]*)){0,1}(?:[\\r\\n\\s]*(?:text|mark)\\s=\\s")((.|\\r|\\n)*?)(?="[\\r\\n\\s]*(?:item\\s\\[\\d|intervals\\s\\[\\d|points\\s\\[\\d|$))'
	#regex_main <- '(?:(?:intervals|points)\\s\\[)(.*\\d)(?:\\]:*[\\r\\n\\s]*(?:xmin|number|time)\\s=)(.*\\d)(?:(?:[\\r\\n\\s]*xmax\\s=)(.*\\d)){0,1}(?:[\\r\\n\\s]*(?:text|mark)\\s=\\s")((.|\\r|\\n)*?)(?="[\\r\\n\\s]*(?:item\\s\\[\\d|intervals\\s\\[\\d|points\\s\\[\\d|$))'
	regex_main <- '(?:(?:intervals|points)\\s*\\[)(.*\\d)(?:\\]:*[\\r\\n\\s]*(?:xmin|number|time)\\s=)(.*\\d)(?:(?:[\\r\\n\\s]*xmax\\s=)(.*\\d)){0,1}(?:[\\r\\n\\s]*(?:text|mark)\\s=\\s")((.|\\r|\\n)*?)(?="[\\r\\n\\s]*(?:item\\s*\\[\\d|intervals\\s*\\[\\d|points\\s*\\[\\d|$))'
	
	tiercontent <- stringr::str_match_all(mytg.merge, regex_main)
	
	#bind all rows together and rename columns
	tiercontent <- do.call(rbind, lapply(tiercontent, data.frame, stringsAsFactors=FALSE))
	colnames(tiercontent) <- c("none1","intervalnr","startSec","endSec", "content","none6")
	
	#replace double "" from praat textgrids
	tiercontent$content <- stringr::str_replace_all(tiercontent$content, "\"\"", "\"")
	
	#check if actual data and calculated values are the same
	if(	length(alltierNames)!=nrow(tiercontent) ) 	{
		myTrans@read.result 		<- "Error"
		myTrans@read.errormessage   <- "Unkown error."
		return(myTrans)
	}
	
	if (nrow(tiercontent)==0) {
		myData <-data.frame(
			dataID = I(as.integer(NULL)),
			transcriptName = NULL,
			tierName = alltierNames,
			startSec = I(as.double(tiercontent$startSec)),
			endSec = I(as.double(tiercontent$endSec)),
			content = I(tiercontent$content),
			content.norm = I(tiercontent$content),
			char.orig.bytime.start = I(as.integer(NULL)),
			char.orig.bytime.end= I(as.integer(NULL)),
			char.norm.bytime.start= I(as.integer(NULL)),
			char.norm.bytime.end= I(as.integer(NULL)),
			char.orig.bytier.start = I(as.integer(NULL)),
			char.orig.bytier.end = I(as.integer(NULL)),
			char.norm.bytier.start = I(as.integer(NULL)),
			char.norm.bytier.end = I(as.integer(NULL)),
			row.names=NULL, stringsAsFactors=TRUE)
		
	} else {
		dataID<-c(1:nrow(tiercontent))
		myData <-data.frame(
			dataID = as.integer(dataID),
			transcriptName = myTrans@name,
			tierName = alltierNames,
			startSec = I(as.double(tiercontent$startSec)),
			endSec = I(as.double(tiercontent$endSec)),
			content = I(tiercontent$content),
			content.norm = I(tiercontent$content),
			char.orig.bytime.start = I(rep(as.integer(NA),length(alltierNames))),
			char.orig.bytime.end= I(rep(as.integer(NA),length(alltierNames))),
			char.norm.bytime.start= I(rep(as.integer(NA),length(alltierNames))),
			char.norm.bytime.end= I(rep(as.integer(NA),length(alltierNames))),
			char.orig.bytier.start = I(rep(as.integer(NA),length(alltierNames))),
			char.orig.bytier.end = I(rep(as.integer(NA),length(alltierNames))),
			char.norm.bytier.start = I(rep(as.integer(NA),length(alltierNames))),
			char.norm.bytier.end = I(rep(as.integer(NA),length(alltierNames))),
			row.names=dataID, stringsAsFactors=TRUE)
	}
	
	
	
	#===set correct column format
	myData$dataID   <- as.integer(myData$dataID)
	myData$startSec <- as.double(myData$startSec)
	myData$endSec   <- as.double(myData$endSec)
	
	#=== if it is a completely empty transcript
	if (nrow(tiercontent)==0)  	{
		myData <- data.frame(dataID=character(), transcriptName=character(), tierName=character(), startSec=character(), endSec=character(), content=character(), row.names=character())
	} else {
		
		#=== get rid of empty intervals
		if (options()$act.load.readEmptyIntervals==FALSE) 		{
			myData <- myData[myData$content!="",]
		}
		myData <- myData[is.na(myData["content"])==FALSE,]
		
		if (nrow(myData)>0) 		{
			#=== sort transcript by start times
			myData <- myData[order(myData$startSec, myData$tierName), ]
			
			#=== set endSec of points to startSec
			myData$endSec[is.na(myData$endSec)] <- myData$startSec[is.na(myData$endSec)]
			
			#=== set data.id again
			myData$dataID <- c(1:nrow(myData))
			
			#=== set the new row names
			rownames(myData) <- myData$dataID
		}
	}
	
	myTrans@data 	<- myData
	
	#=== assign other values to object
	myTiers 			<- tierinfo$type
	names(myTiers) 		<- tierinfo$tierName
	myTrans@tiers 		<- myTiers
	
	return(myTrans)
}
