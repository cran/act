#' Exports a transcript object to a single 'Elan' '*.eaf' file
#'
#' @param outputPath Character string; path where EAF file will be saved.
#' @param t Transcript object; transcript to be exported.
#' @param createMediaLinks Logical; if \code{TRUE} media links will be created.
#'
#' @export
#' 
#' @seealso \link{transcripts_export}, \link{textgrid_export}, , \link{textgrid_import}
#'
#' @example inst/examples/io_eaf_export.R
#' 
eaf_export <- function(t, outputPath, createMediaLinks=FALSE) {
	#t <- corpus@transcripts[[5]]
	myData<- t@data
	
	#--- get only relevant columns
	myCols <- c("tierName", "startSec","endSec","content")
	if (!all(myCols %in% colnames(myData))) {
		stop(paste("Missing colums in data. Data needs to contain: ", paste(myCols, collapse = " ", sep="")))
	}
	myData<-myData[,myCols]
	
	#convert data to html save characters
	myData$content <-	XML::xmlValue(XML::xmlTextNode(as.vector(myData$content)))
	myData$tierName <-	XML::xmlValue(XML::xmlTextNode(as.vector(myData$tierName)))
	
	#filter data by tiers, if filter is set
	#... xxx
	
	#--- get  tier names and classes
	#from the data
	tierNames <- unique(myData$tierName)
	tierClassesInList <- NULL
	
	tierNamesInList <- names(t@tiers)
	if (!is.null(tierNamesInList)) {
		tierNames 		<- union(tierNamesInList, tierNames)
	}
	#class
	tierClassesInList <- unlist(t@tiers)
	names(tierClassesInList)<-NULL
	
	#add Classes if missing, assume they are interval tiers
	if (is.null(tierClassesInList)) 	{
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
	
	#--- generate EAF-XML-document
	myEAF <-               "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	myEAF <- append(myEAF, "<ANNOTATION_DOCUMENT AUTHOR=\"\" DATE=\"2018-05-04T19:33:08+01:00\" FORMAT=\"2.7\" VERSION=\"2.7\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.mpi.nl/tools/elan/EAFv2.7.xsd\">" )
	
	#--- generate media header
	myEAF <- append(myEAF, "    <HEADER MEDIA_FILE=\"\" TIME_UNITS=\"milliseconds\">")
	if (createMediaLinks==TRUE)
	{
		if (length(t@path.media)>0)
		{
			for (mediaPath in t@path.media )
			{
				myMimeType <- "unknown"
				if (tools::file_ext(mediaPath) %in% c("wav")) {
					myMimeType <-"audio/x-wav"
				} else if (tools::file_ext(mediaPath) %in% c("mp3", "aif", "aiff")) {
					myMimeType <-"audio/*"
				} else if (tools::file_ext(mediaPath) %in% c("mp4", "mov")) {
					myMimeType <-"video/mp4"
				}
				myEAF <- append(myEAF, sprintf("         <MEDIA_DESCRIPTOR MEDIA_URL=\"file://%s\" MIME_TYPE=\"%s\"/>", mediaPath, myMimeType))
			}
		}
	}
	
	
	
	#<PROPERTY NAME="URN">urn:nl-mpi-tools-elan-eaf:cfb1957f-d6fa-4f7d-b124-6a74700d014d</PROPERTY>
	#<PROPERTY NAME="lastUsedAnnotationId">21</PROPERTY>
	myEAF <- append(myEAF, "    </HEADER>")
	
	
	if (nrow(myData)>0) {
		#--- generate time order
		myData$annotationID <- paste("a",1:nrow(myData),sep="")
		myData$TIME_SLOT_REF1 <- paste("ts",1:nrow(myData),sep="")
		myData$TIME_SLOT_REF2 <- paste("ts", (nrow(myData)+1):(nrow(myData)*2),sep="")
		myData$startSec <- as.integer(myData$startSec*1000)
		myData$endSec <- as.integer(myData$endSec*1000)
		
		myEAF <- append(myEAF,         "    <TIME_ORDER>")
		myEAF <- append(myEAF, sprintf("        <TIME_SLOT TIME_SLOT_ID=\"%s\" TIME_VALUE=\"%s\"/>", myData$TIME_SLOT_REF1, myData$startSec))
		myEAF <- append(myEAF, sprintf("        <TIME_SLOT TIME_SLOT_ID=\"%s\" TIME_VALUE=\"%s\"/>", myData$TIME_SLOT_REF2, myData$endSec))
		myEAF <- append(myEAF,         "    </TIME_ORDER>")
	} else {
		myEAF <- append(myEAF,         "    <TIME_ORDER>")
		myEAF <- append(myEAF,         "    </TIME_ORDER>")
	}
	
	#iterate through all tierNames
	tierNr<-1
	for (tierNr in 1:length(tierNames))
	{
		tier_name 	<- tierNames[tierNr]
		tier_class 	<- tierClasses[tierNr]
		
		#--- get data within tier
		data.tier <- myData[myData$tierName==tier_name,]
		
		if (nrow(data.tier)==0) {
			myEAF <- append(myEAF, sprintf("    <TIER LINGUISTIC_TYPE_REF=\"praat\" TIER_ID=\"%s\"/>", tier_name))
		} else {
			#--- generate tier
			myEAF <- append(myEAF, sprintf("    <TIER LINGUISTIC_TYPE_REF=\"praat\" TIER_ID=\"%s\">", tier_name))
			
			if (tier_class == "IntervalTier")
			{
			} else if (tier_class == "TextTier") {
				#--- add end times
				data.tier$endSec <- data.tier$startSec + 100
			}
			
			#--- check for overlap of intervals, if there are more than one intervals
			if (nrow(data.tier)>1) 	{
				#get intervals whose endSec is bigger then the startSec of the following
				overlaps <- data.tier$endSec[1:nrow(data.tier)-1]>data.tier$startSec[2:nrow(data.tier)]
				
				#if there are
				if (any(overlaps==TRUE))
				{
					#get the indices of those intervals
					overlaps <- c(1:length(overlaps))[overlaps]
					
					#replace endSec with startSec of the following interval
					data.tier$endSec[overlaps]<-data.tier$startSec[overlaps+1]
				}
			}
			
			annotations <- paste("        <ANNOTATION>",
								 sprintf("            <ALIGNABLE_ANNOTATION ANNOTATION_ID=\"%s\" TIME_SLOT_REF1=\"%s\" TIME_SLOT_REF2=\"%s\">", data.tier$annotationID, data.tier$TIME_SLOT_REF1, data.tier$TIME_SLOT_REF2),
								 sprintf("                <ANNOTATION_VALUE>%s</ANNOTATION_VALUE>",data.tier$content ),
								 "            </ALIGNABLE_ANNOTATION>",
								 "        </ANNOTATION>", sep="\n")
			
			myEAF <- append(myEAF, annotations)
			myEAF <- append(myEAF,         "    </TIER>")
		}
	}
	
	myEAF <- append(myEAF, sprintf("    <TIER LINGUISTIC_TYPE_REF=\"praat\" TIER_ID=\"%s\"/>", tierNames, sep="\n"))
	
	#---
	myEAF <- append(myEAF, "    <LINGUISTIC_TYPE GRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID=\"praat\" TIME_ALIGNABLE=\"true\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"Time subdivision of parent annotation\'s time interval, no time gaps allowed within this interval\" STEREOTYPE=\"Time_Subdivision\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"Symbolic subdivision of a parent annotation. Annotations refering to the same parent are ordered\" STEREOTYPE=\"Symbolic_Subdivision\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"1-1 association with a parent annotation\" STEREOTYPE=\"Symbolic_Association\"/>")
	myEAF <- append(myEAF, "    <CONSTRAINT DESCRIPTION=\"Time alignable annotations within the parent annotation's time interval, gaps are allowed\" STEREOTYPE=\"Included_In\"/>")
	
	myEAF <- append(myEAF, "</ANNOTATION_DOCUMENT>")
	
	#---write to file
	fileConn <- file(outputPath, open="wb")
	myEAF<-stringr::str_flatten(myEAF, collapse="\n")
	writeBin(charToRaw(myEAF), fileConn, endian="little")
	close(fileConn)
}
