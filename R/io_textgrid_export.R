
#' Export a single transcript object to a 'Praat' *.TextGrid' file.
#'
#' @param t Transcript object; transcript to be saved.
#' @param outputPath Character string; path where TextGrid will be saved.
#' 
#' @export
#' 
#' @seealso \link{transcripts_export}, \link{eaf_export}, \link{textgrid_import}
#'
#' @example inst/examples/io_transcripts_export.R
#' 
textgrid_export <- function(t, outputPath) {
	#---get data from transcript
	myData<- t@data
	
	#--- get only relevant columns
	myCols <- c("tierName", "startSec","endSec","content")
	if (!all(myCols %in% colnames(myData))) {
		stop(paste("Missing colums in data. Data needs to contain: ", paste(myCols, collapse = " ", sep="")))
		return("please set either 'myData' or 'transcript'")
	}
	myData<-myData[,myCols]
	
	
	#filter data by tiers, if filter is set
	#... xxx
	
	#sort data by start time
	myData<- myData[order(myData$startSec), ]
	
	
	#--- get length of textgrid
	if (is.null(t)) {
		texgridLength = -1
	} else {
		texgridLength <- t@length
	}
	#get maximum time from data
	texgridLength <- max(c(texgridLength, myData$startSec, myData$endSec))
	
	
	#--- get  tier names and classes
	#from the data
	tierNames <- unique(myData$tierName)
	tierClassesInList <- NULL
	
	#try to get from transcript
	if (!is.null(t))	{
		tierNamesInList <- names(t@tiers)
		if (!is.null(tierNamesInList)) {
			tierNames 		<- union(tierNamesInList, tierNames)
		}
		
		#class
		tierClassesInList <- unlist(t@tiers)
		names(tierClassesInList)<-NULL
	}
	
	#add Classes if missing, assume they are interval tiers
	if (is.null(tierClassesInList))
	{
		#if no classes are found at all
		tierClasses <- rep("IntervalTier", length(tierNames))
	} else	{
		#if some are found, add only classes for the ones that are missing
		missing <- length(tierNames)-length(tierClassesInList)
		if (missing>0)
		{
			tierClassesInList <- c(tierClassesInList,rep("IntervalTier", missing))
		}
		tierClasses <- tierClassesInList
	}
	
	
	#--- create textgrid header
	myTG <- 			"File type = \"ooTextFile\""
	myTG <- append(myTG, "Object class = \"TextGrid\"" )
	
	myTG <- append(myTG, "xmin = 0 ")
	myTG <- append(myTG, sprintf("xmax = %s ", texgridLength))
	myTG <- append(myTG, "tiers? <exists> ")
	myTG <- append(myTG, sprintf("size = %s ", length(tierNames) ))
	myTG <- append(myTG, "item []: ")
	
	#iterate though all tierNames
	for (tierNr in 1:length(tierNames))
	{
		# tierNr<-18
		tier_name 	<- tierNames[tierNr]
		tier_class 	<- tierClasses[tierNr]
		
		#get data within tier
		data.tier <- myData[myData$tierName==tier_name,]
		
		if (tier_class == "IntervalTier") {
			#get number of intervals in tier
			intervalNr <- nrow(data.tier)
			if (intervalNr==0)
			{
				addLevel <- function(x, newlevel=NULL) {
					if(is.factor(x)) {
						if (is.na(match(newlevel, levels(x))))
							return(factor(x, levels=c(levels(x), newlevel)))
					}
					return(x)
				}
				data.tier$tierName <- addLevel(data.tier$tierName, tier_name)
				
				#add an empty interval
				data.tier[1, ] <- c(tierName=tier_name, startSec=0, endSec=texgridLength, content="")
			} else {
				#check for overlap of intervals, if there are more than one
				if (intervalNr>1) {
					#just for testing
					#data.tier$endSec[2] <- 5000
					
					#get intervals whose endSec is bigger then the startSec of the following
					overlaps <- data.tier$endSec[1:intervalNr-1]>data.tier$startSec[2:intervalNr]
					
					#if there are
					if (any(overlaps==TRUE)) {
						#get the indices of those intervals
						overlaps <- c(1:length(overlaps))[overlaps]
						
						#replace endSec with startSec of the following interval
						data.tier$endSec[overlaps]<-data.tier$startSec[overlaps+1]
					}
				}
				
				#get all times from tier, and add 0 and texgridLength
				allTimes<- sort(unique(c(0, texgridLength, data.tier$startSec, data.tier$endSec )))
				
				#create empty intervals for all times
				newData <- data.frame(cbind(tierName=tier_name, startSec=as.double(allTimes[1:length(allTimes)-1]), endSec=as.double(allTimes[2:length(allTimes)]), content=""), stringsAsFactors=FALSE		)
				
				#merge new and actual data
				merged <- merge(data.tier, newData, all.y =TRUE, by= c("tierName","startSec", "endSec"))
				
				#set empty content to ""
				merged$content.x[is.na(merged$content.x)]<-""
				
				#remove superflous colums
				merged$content.y<-NULL
				
				data.tier <- merged
			}
			
			#add consecutive numbers
			data.tier <- cbind(as.character(1:nrow(data.tier)),data.tier)
			
			#rename columns
			colnames(data.tier) <- c("intervalNr", "tierName", "startSec","endSec", "content")
			
			#get number of intervals
			intervalNr <- nrow(data.tier)
			
			myTG <- append(myTG, sprintf("    item [%s]:", tierNr))
			myTG <- append(myTG,         "        class = \"IntervalTier\" ")
			myTG <- append(myTG, sprintf("        name = \"%s\" " , tier_name))
			myTG <- append(myTG,         "        xmin = 0 ")
			myTG <- append(myTG, sprintf("        xmax = %s ", texgridLength))
			myTG <- append(myTG, sprintf("        intervals: size = %s " , intervalNr))
			
			createIntervalBlock<-function(myInterval)
			{
				#myInter <- sprintf(                   "        intervals [%s]:" , gsub("^\\s+|\\s+$", "", myInterval[1]))
				myInter <- sprintf(                   "        intervals [%s]:" , myInterval[1])
				myInter <- append(myInter, sprintf(	"            xmin = %s " , myInterval[3]))
				myInter <- append(myInter, sprintf( "            xmax = %s " , myInterval[4]))
				myInter <- append(myInter, sprintf( "            text = \"%s\"" , stringr::str_replace_all( myInterval[5], "\"", "\"\"")))
				return(myInter)
			}
			
			
			a <- unlist(apply(data.tier,  1, FUN=createIntervalBlock))
			myTG <- append(myTG, a)
		} else if (tier_class == "TextTier") {
			
			#get number of points in tier
			pointnr <- nrow(data.tier)
			
			#add consecutive numbers
			data.tier <- cbind(as.character(1:nrow(data.tier)),data.tier)
			
			myTG <- append(myTG, sprintf("    item [%s]:", tierNr))
			myTG <- append(myTG,         "        class = \"TextTier\" ")
			myTG <- append(myTG, sprintf("        name = \"%s\" " , tier_name))
			myTG <- append(myTG,         "        xmin = 0 ")
			myTG <- append(myTG, sprintf("        xmax = %s ", texgridLength))
			myTG <- append(myTG, sprintf("        points: size = %s " , pointnr))
			
			createPointBlock<-function(myPoint)
			{
				myInter <- sprintf(                 "        points [%s]:" , myPoint[1])
				myInter <- append(myInter, sprintf(	"            number = %s " , myPoint[3]))
				myInter <- append(myInter, sprintf( "            mark = \"%s\"" , stringr::str_replace_all(  myPoint[5], "\"", "\"\"")))
				return(myInter)
			}
			a <- unlist(apply(data.tier,  1, FUN=createPointBlock))
			myTG <- append(myTG, a)
		}
	}
	
	
	#myTG
	
	#---write to file
	fileConn<-file(outputPath)
	writeLines(myTG, fileConn)
	close(fileConn)
}

