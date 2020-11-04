#' Create a cut list for 'FFmpeg' 
#' 
#' This function creates a file that can be executed in the Terminal (Apple) or the Command Line Interface (Windows). 
#' The file contains commands to call 'FFmpeg' with all necessary information to cut media snippets for each search result.
#' 
#' @param x Corpus object; Please note: all media paths for a transcript need to be given as a list in the corpus object in \code{corpus@transcripts[[ ]]@path.media} . You can use the respective media functions. . 
#' @param s Search object.
#' @param outputFolder Character string; path to folder where files will be written.
#' @param collection Character string; name of collection that will be uses to create a sub folder in the output folder where all sequences will be stored
#' @param outputFormat Character string; file format of output, may be 'mp4', 'wav' or 'mp3'
#' @param mediaFileFilterInclude Character string; regular expression to use only some of the media files specified in corpus@transcripts[[ ]]@path.media )
#' @param returnCutlistForMacInsteadOfWindows Logical; if \code{TRUE} function will return a cut list for mac, if \code{FALSE} function will return a cutlist for windows.
#' @param contextBeforeSec Double; how much context before the search result will be included in seconds 
#' @param contextAfterSec Double; how much context after the search result will be included in seconds
#' @param CodecCopy Logical; if \code{TRUE} FFMPEG will use the option *codec copy*.
#' @param Panning Integer; 0=leave audio as is (ch1&ch2) , 1=only channel 1 (ch1), 2=only channel 2 (ch2), 3=both channels separated (ch1&ch2), 4=all three versions (ch1&ch2, ch1, ch2). This setting will override the option made in 'act.ffmpeg.exportchannels.fromColumnName' .
#' #'
#' @return Character string; the cut list.
#' @export
#'
#' @example inst/examples/snippets_cutlist.R
#' 
snippets_cutlist <- function(x = NULL, s= NULL, outputFolder="", collection = "", outputFormat = "",  mediaFileFilterInclude="",  returnCutlistForMacInsteadOfWindows=FALSE, contextBeforeSec = 0,  contextAfterSec = 0, CodecCopy=TRUE, Panning=NA) {
	#x<-corpus
	#s <- mysearch
	#subfolder <- ""
	#outputFormat <- ""
	#mediaFileFilterInclude <-""
	#returnCutlistForMacInsteadOfWindows<-FALSE
	#contextBeforeSec <- 0
	#contextAfterSec <- 0
	#CodecCopy<-TRUE
	#Panning<-NA
	#cutlistFileNameSansExt<-		"FFMPEG_cutlist"
	

	if (is.null(x)) 								{ stop("x is null, but needs to be a corpus object.") 	}
	if (is.null(s@results)) 				     	{ stop("s is null, but needs to be a search object.") 	}
	if (is.null(s@results$transcriptName))       	{ stop("Data frame s@results does not contain column 'transcriptName'.") 	}
	if (nrow(s@results)==0) 	                    { stop("Data frame s@results does not contain any search results (data frame with 0 rows)") 	}

	if (outputFormat=="") {
	} else if (outputFormat=="mp4") {
	} else if (outputFormat=="mp3") {
	} else if (outputFormat=="wav") {
	} else {
		stop('outputFormat must be "mp4", "wav" or "mp3"') 	
	}
	myWarnings <- c()
	
	#set progress bar
	act.environment$pb <- progress::progress_bar$new(
		format = "  Creating cut list  [:bar] :percent missing: :eta",
		total = max(1,nrow(s@results)), 
		clear = FALSE, 
		show_after = 0,
		width= 60)
	
	cutlist_win<-c()
	cutlist_mac<-c()

	#--- if cut list should be saved - check it  output folder exists
	output_folder_cutlist <- outputFolder
	if (output_folder_cutlist!="") {
		output_folder_cutlist<-normalizePath(output_folder_cutlist)
		if (file.exists(output_folder_cutlist)==FALSE) 	{
			stop("Output folder does not exist.")
		}		
	}

	output_folder_all <- c()
	#for each search result
	for (i in 1:nrow(s@results)) 	{
	
		#update progress
		if (exists("act.environment", mode="environment")) {
			if(exists("pb", envir=act.environment)) {
				act.environment$pb$tick()
			}
		}
		

		#=== get transcript
		t <- NULL
		
		if (is.null(s@results$transcriptName[i])) {
			#transcript not found
			myWarnings <- paste(myWarnings, sprintf("- result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
			
		} else {
			t <- x@transcripts[[ s@results$transcriptName[i] ]]

			if (is.null(t)) {
				#transcript not found
				myWarnings <- paste(myWarnings, sprintf("- Result %s '%s': transcript '%s' not found in corpus. ", i, as.character(s@results[i, options()$act.export.filename.fromColumnName]),  as.character(s@results$transcriptName[i]) ), collapse="\n", sep="\n")
			}
		}
		
		if (!is.null(t)) {
			
			#---get paths of input files
			input_paths <- t@path.media
			
			#mediaFileFilterInclude
			if (mediaFileFilterInclude!="") {
				input_paths <- input_paths[grep(pattern=mediaFileFilterInclude, input_paths)]
			}
			
			if (length(input_paths)==0) {
				myWarnings <- c(myWarnings, "- No media files found for: '", t@name, "' No cuts added to cut list. \n")
			} else {
				#for each media file
				for (j in 1:length(input_paths)) {
					#get filename of original
					myMediaFileName <- stringr::str_to_lower(basename(tools::file_path_sans_ext(   input_paths[j])))
					
					#get video format of original
					myMediaFileType <- stringr::str_to_lower(tools::file_ext(input_paths[j]))
					
					#====== FOLDERS
					output_folder <- rep(output_folder_cutlist, 3)

					#--- collection
					if (collection!="") 	{
						output_folder <- file.path(output_folder, collection)	
					}
					
					#--- foldergrouping1: subfolder for each search result
					foldergrouping.fromColumnName <- options()$act.export.foldergrouping1.fromColumnName
					if (foldergrouping.fromColumnName!="") {
						if(foldergrouping.fromColumnName %in% colnames(s@results)) {
						} else {
							foldergrouping.fromColumnName <- "resultID"
						}
	
						foldername <- as.character(s@results[i, foldergrouping.fromColumnName])
						if(!is.na(foldername)) {
							if(!length(foldername)==0) {
								if (foldername!="") {
									output_folder <- file.path(output_folder, foldername)
								}
							}
						}
					}
	
					#--- panned versions
					#if  CreatePannedVersions in the arguments if the functions is not set
					if (is.na(Panning)) {
						#check if channels are set in the search results
						if(options()$act.ffmpeg.exportchannels.fromColumnName %in% colnames(s@results)) {
							#if it is set, take the value given there
							CreatePannedVersions <- s@results[i, options()$act.ffmpeg.exportchannels.fromColumnName]
						} else {
							#do not create a panned version
							CreatePannedVersions <- 0
						}
					} else {
						CreatePannedVersions <- Panning
					}

					if (CreatePannedVersions==0 ) {			#no panning
					} else if (CreatePannedVersions==1 ) {	#only left
					} else if (CreatePannedVersions==2 ) {	#only right
					} else if (CreatePannedVersions==3)  {	#left and right
						output_folder[2] <- file.path(output_folder[2], "ch1")
						output_folder[3] <- file.path(output_folder[3], "ch2")
					} else if (CreatePannedVersions==4 ) {	#all three versions
						output_folder[1] <- file.path(output_folder[1], "ch1_ch2")
						output_folder[2] <- file.path(output_folder[2], "ch1")
						output_folder[3] <- file.path(output_folder[3], "ch2")
					}
	
				
					#--- create subfolder folder for each mediafile if more than 1 media file
					if (length(input_paths)>1) {
						output_folder <- file.path(output_folder, stringr::str_replace_all(myMediaFileName, ".*_icas\\d*_*", ""))
					}
					
					#--- foldergrouping2: subfolder for each cut
					foldergrouping.fromColumnName <- options()$act.export.foldergrouping2.fromColumnName
					if (foldergrouping.fromColumnName!="") {
						if(foldergrouping.fromColumnName %in% colnames(s@results)) {
							foldername <- as.character(s@results[i, foldergrouping.fromColumnName])
							if(!is.na(foldername)) {
								if(!length(foldername)==0) {
									if (foldername!="") {
										output_folder <- file.path(output_folder, foldername)
									}
								}
							}
						} 
					}
						
					#====== FILE NAME
					#add name of result and filetype
					#--- name of sequence : subfolder for each search result
					filename.fromColumnName <- options()$act.export.filename.fromColumnName
					if(filename.fromColumnName %in% colnames(s@results)) {
					} else {							
						filename.fromColumnName <- "resultID" 				
					}
					
					#destination path
					myExt <- outputFormat
					if (myExt=="") {
						myExt <- tools::file_ext(input_paths[j]) 
					}
					out_filepath <- rep("",3)
					out_filepath[1] <- file.path(output_folder[1], paste(as.character(s@results[i, filename.fromColumnName]), ".", myExt, sep=""))
					out_filepath[2] <- file.path(output_folder[2], paste(as.character(s@results[i, filename.fromColumnName]), "_ch1.", myExt, sep=""))
					out_filepath[3] <- file.path(output_folder[3], paste(as.character(s@results[i, filename.fromColumnName]), "_ch2.", myExt, sep=""))

					#=== get start & end
					startSec 	<- max(0, s@results$startSec[i] - contextBeforeSec)
					endSec 		<- min(s@results$endSec[i] + contextAfterSec, t@length)
					
					startSecMinus10 <- max(0, startSec - 10)
					
					#=== FFMPEG cmd cmd
					ffmpeg_cmd_general <- 	options()$act.ffmpeg.command.default
					if (outputFormat=="mp4") {
						ffmpeg_cmd_general <- 	options()$act.ffmpeg.command.mp4
						if (options()$act.ffmpeg.command.UseFastVideoPostioning) {
							ffmpeg_cmd_general <- 	options()$act.ffmpeg.command.mp4.FastVideoPostioning
						}
					} else if (outputFormat=="mp3") {
						ffmpeg_cmd_general <- 	options()$act.ffmpeg.command.mp3
						
					} else if (outputFormat=="wav") {
						ffmpeg_cmd_general <- 	options()$act.ffmpeg.command.wav
					} 
		
					#--- replace general place holders	
					ffmpeg_cmd_general <- 	stringr::str_replace_all(ffmpeg_cmd_general, "INFILEPATH", 				input_paths[j])
					ffmpeg_cmd_general <- 	stringr::str_replace_all(ffmpeg_cmd_general, "TIMESTART\\b", 			as.character(startSec))
					ffmpeg_cmd_general <- 	stringr::str_replace_all(ffmpeg_cmd_general, "TIMESTARTMINUS10SECONDS",	as.character(startSecMinus10))
					ffmpeg_cmd_general <- 	stringr::str_replace_all(ffmpeg_cmd_general, "TIMEDURATION", 			as.character(endSec-startSec))
					
					#--- codec
					#if (CodecCopy==TRUE) {
					#	ffmpeg_cmd_general  <- 	stringr::str_replace_all(ffmpeg_cmd_general, " OPTIONS ", " -codec copy ")
					#} else {
				#		ffmpeg_cmd_general <- 	stringr::str_replace_all(ffmpeg_cmd_general, " OPTIONS ", " ")
				#	}

					#--- versions
					ffmpeg_cmd 	<- rep(ffmpeg_cmd_general,3)
					ffmpeg_cmd[1] <- 	stringr::str_replace_all(ffmpeg_cmd[1], "OUTFILEPATH", out_filepath[1])			
					ffmpeg_cmd[2] <- 	stringr::str_replace_all(ffmpeg_cmd[2], "OUTFILEPATH", out_filepath[2])
					ffmpeg_cmd[3] <- 	stringr::str_replace_all(ffmpeg_cmd[3], "OUTFILEPATH", out_filepath[3])											
					
					if (CodecCopy==TRUE) {
						ffmpeg_cmd[1]  <- 	stringr::str_replace_all(ffmpeg_cmd[1], " OPTIONS ", " -c:v copy ")
						ffmpeg_cmd[2] <- 	stringr::str_replace_all(ffmpeg_cmd[2], " OPTIONS ", " -af \"pan=1c|c0=c0\" -c:v copy ")
						ffmpeg_cmd[3] <- 	stringr::str_replace_all(ffmpeg_cmd[3], " OPTIONS ", " -af \"pan=1c|c0=c1\" -c:v copy ")
					} else {
						ffmpeg_cmd[1]  <- 	stringr::str_replace_all(ffmpeg_cmd[1], " OPTIONS ", " ")									#replacing with a space is important
						ffmpeg_cmd[2] <- 	stringr::str_replace_all(ffmpeg_cmd[2], " OPTIONS ", " -af \"pan=1c|c0=c0\" ")
						ffmpeg_cmd[3] <- 	stringr::str_replace_all(ffmpeg_cmd[3], " OPTIONS ", " -af \"pan=1c|c0=c1\" ")
					}
					#old panning options: c0=1.0*c0+0.0*c1
					
					titletext <- stringr::str_flatten(c(s@results[i, filename.fromColumnName], " (", i, " of ", nrow(s@results)," sequences)"), collapse="")

					#=== windows
					cmd <- makeCommand("win", CreatePannedVersions, in_filepath=input_paths[j],  out_filename=as.character(s@results[i, filename.fromColumnName]), ffmpeg_cmd, titletext)
					cutlist_win <-  c(cutlist_win, cmd)	

					#=== mac
					cmd <- makeCommand("mac", CreatePannedVersions, in_filepath=input_paths[j],  out_filename=as.character(s@results[i, filename.fromColumnName]), ffmpeg_cmd, titletext)
					cutlist_mac <-  c(cutlist_mac, cmd)
					
					#=== collect output folders
					if (CreatePannedVersions==0) { output_folder_all <- c(output_folder_all, output_folder[1])					}
					if (CreatePannedVersions==1) { output_folder_all <- c(output_folder_all, output_folder[2])					}
					if (CreatePannedVersions==2) { output_folder_all <- c(output_folder_all, output_folder[3])					}
					if (CreatePannedVersions==3) { output_folder_all <- c(output_folder_all, output_folder[2:3])					}
					if (CreatePannedVersions==4) { output_folder_all <- c(output_folder_all, output_folder[1:3])}
					
				}
			}
		}
	}

	if (length(cutlist_win)==0) {
		stop(c(myWarnings, "No cut list created."))
	} else {
		
		#makedirs
		output_folder_all <- unique(output_folder_all)
		if (length(output_folder_all)>0) {
			cmd_makedir  <- 'IF NOT EXIST "OUTFOLDER" ( md "OUTFOLDER" )'
			cmd_makedir  <- stringr::str_replace_all(cmd_makedir, "OUTFOLDER", output_folder_all)
			cmd_makedir  <- stringr::str_flatten(cmd_makedir, collapse='\n')
			cutlist_win <- c(cmd_makedir, cutlist_win)
			
			cmd_makedir  <- 'mkdir -p "OUTFOLDER"'
			cmd_makedir  <- stringr::str_replace_all(cmd_makedir, "OUTFOLDER", output_folder_all)
			cmd_makedir  <- stringr::str_flatten(cmd_makedir, collapse='\n')
			cutlist_mac <- c(cmd_makedir, cutlist_mac)
		}
		
		#for windows make some replacements
		cutlist_win <-  stringr::str_flatten(cutlist_win)
		cutlist_win <-  stringr::str_replace_all(cutlist_win, "/", "\\\\")
		
		#for windows make some replacements
		cutlist_mac <- c("#!/bin/sh",cutlist_mac)
		cutlist_mac <-  stringr::str_replace_all(cutlist_mac, "\\\\", "/")
		
		#--- save cutlists
		# if output folder is given
		
		cutlistFileNameSansExt="FFMPEG_cutlist"
		if (collection!="") {
			cutlistFileNameSansExt <- stringr::str_c(cutlistFileNameSansExt, "_",collection)
		}
		
		if (output_folder_cutlist!="") {
			#--- save cutlist WIN as cmd
			myFilepath 	<- file.path(output_folder_cutlist, paste(cutlistFileNameSansExt, "_win.cmd", sep=""))
			fileConn 	<- file(myFilepath)
			writeLines(cutlist_win, fileConn)
			close(fileConn)
			
			#--- save cutlist MAC as executable
			myFilepath 	<- file.path(output_folder_cutlist, paste(cutlistFileNameSansExt, "_mac", sep=""))
			fileConn 	<- file(myFilepath)
			writeLines(cutlist_mac, fileConn)
			close(fileConn)
			system(paste("chmod 755 ", myFilepath, sep=""))
		}
	}
	
	
	#=== print warnings
	if (length(myWarnings)>0) {
		warning(myWarnings)
	}
	
	#=== return
	cutlist <- cutlist_win
	if (returnCutlistForMacInsteadOfWindows==TRUE) {
		cutlist <- cutlist_mac
	}
	
	return(cutlist)
}

makeCommand <- function(os, CreatePannedVersions, in_filepath, out_filename, ffmpeg_cmd, titletext) {
	if (os=="win") {
		cmd <- '
IF EXIST "INFILEPATH" ( 
	title "TITLETEXT"
	FFMPEGcmd1
	FFMPEGcmd2
	FFMPEGcmd3
) \n\n\n'

	} else {

		cmd <-'
if [ -f "INFILEPATH" ]
then
	FFMPEGcmd1
	FFMPEGcmd2
	FFMPEGcmd3
fi \n\n\n'
	}
	
	cmd <- stringr::str_replace_all(cmd, "TITLETEXT", titletext)
	cmd <- stringr::str_replace_all(cmd, "OUTFILENAME", out_filename)
	cmd <- stringr::str_replace_all(cmd, "INFILEPATH", in_filepath)
	
	#0 only all audio
	#1 ch1
	#2 ch2
	#3 ch1 & 2
	#4 all audio & ch1 & ch2
	if (CreatePannedVersions==0) { 
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd1", ffmpeg_cmd[1])
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd2", "" )
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd3", "")
	}
	
	if (CreatePannedVersions==1) { 
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd1", "")
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd2", ffmpeg_cmd[2])
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd3", " ")
	}
	
	if (CreatePannedVersions==2) { 
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd1", "")
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd2", "")
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd3", ffmpeg_cmd[3])
	}
	
	if (CreatePannedVersions==3) { 
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd1", "")
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd2", ffmpeg_cmd[2])
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd3", ffmpeg_cmd[3])
	}
	
	if (CreatePannedVersions==4) { 
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd1", ffmpeg_cmd[1])
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd2", ffmpeg_cmd[2])
		cmd <- 	stringr::str_replace_all(cmd, "FFMPEGcmd3", ffmpeg_cmd[3])
	}
	return (cmd)
}