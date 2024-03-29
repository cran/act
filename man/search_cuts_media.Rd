% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_cuts_media.R
\name{search_cuts_media}
\alias{search_cuts_media}
\title{Create cut lists for 'FFmpeg'}
\usage{
search_cuts_media(
  x,
  s,
  cutSpanBeforesec = NULL,
  cutSpanAftersec = NULL,
  outputFolder = NULL,
  filterMediaInclude = "",
  fastVideoPostioning = TRUE,
  videoCodecCopy = FALSE,
  audioCutsAsMP3 = FALSE,
  Panning = NULL,
  filename.append.medianame = FALSE,
  filename.append.separator = "__"
)
}
\arguments{
\item{x}{Corpus object; Please note: all media paths for a transcript need to be given as a list in the corpus object in \code{corpus@transcripts[[ ]]@media.path} . You can use the respective media functions. .}

\item{s}{Search object.}

\item{cutSpanBeforesec}{Double; Start the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.}

\item{cutSpanAftersec}{Double; End the cut some seconds before the hit to include some context; the default NULL will take the value as set in @cuts.span.beforesec of the search object.}

\item{outputFolder}{Character string; path to folder where files will be written.}

\item{filterMediaInclude}{Character string; regular expression to match only some of the media files in \code{corpus@transcripts[[ ]]@media.path}.}

\item{fastVideoPostioning}{Logical; If \code{TRUE} the FFmpeg command will be using the parameter fast video positioning as specified in \code{options()$act.ffmpeg.command.fastVideoPostioning}.}

\item{videoCodecCopy}{Logical; if \code{TRUE} FFMPEG will use the option \emph{codec copy} for videos.}

\item{audioCutsAsMP3}{Logical; If \code{TRUE} audio cuts will be exported as '.mp3' files, using  \code{options()$act.ffmpeg.command.audioCutsAsMP3}.}

\item{Panning}{Integer; 0=leave audio as is (ch1&ch2) , 1=only channel 1 (ch1), 2=only channel 2 (ch2), 3=both channels separated (ch1&ch2), 4=all three versions (ch1&ch2, ch1, ch2). This setting will override the option made in 'act.ffmpeg.exportchannels.fromColumnName' .}

\item{filename.append.medianame}{Logical; if \code{TRUE} the file name of the original media file will be appended to the snippet(s).}

\item{filename.append.separator}{Character string; characters that separate the result id and the media file name.}
}
\value{
Search object; cut lists will be stored in \code{s@cuts.cutlist.mac} and \code{s@cuts.cutlist.win}.
}
\description{
This function creates FFmpeg commands to cut media files for each search results.
If you want to execute the commands (and cut the media files) you need to have FFmpeg installed on you computer. To install FFmpeg you can follow the instructions given in the vignette 'installation-ffmpeg'. Show the vignette with \code{vignette("installation-ffmpeg")}.
}
\details{
\emph{Cut lists}\cr
The commands are collected in cut lists.
The cut lists will be stored in different ways:
\itemize{
\item A cut list for for ALL search results will be stored in \code{s@cuts.cutlist.mac} to be used on MacOS and \code{s@cuts.cutlist.win} to be used on Windows.
\item Individual cut lists for EACH search result will be stored in additional columns in the data frame \code{s@results}.
The cut lists that can be executed in the Terminal (Apple) or the Command Line Interface (Windows).
}

\emph{Input media files}\cr
The function will use all files in  \code{corpus@transcripts[[ ]]@media.path}.
Therefore you will need to set the options \code{filterMediaInclude} filtering for which input media files you want to create the cuts.
The filter is a regular expression, e.g.  \code{'\\.(wav|aif)'} for '.wav' and '.aif' audio files or \code{'\\.mp4'} for '.mp4' video files.

\emph{Output format}\cr
The output format is predefined by in the options:
\itemize{
\item \code{act.ffmpeg.command} defines the basic FFmpeg command
\item \code{act.ffmpeg.command.fastVideoPostioning} defines the FFmpeg command to be used with large video files.
}

The default is to generate mp4 video cuts. You can also use the following commands to change the output format:\cr\cr
MP4 video cuts with original video quality:
\itemize{
\item \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp4" -hide_banner')}
\item \code{options(act.ffmpeg.command.fastVideoPostioning = 'ffmpeg -ss TIMESTARTMINUS10SECONDS -i "INFILEPATH" -ss 10.000 -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp4" -hide_banner')}
}

MP4 video cuts with reduced video quality:
\itemize{
\item \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -vf scale=1920:-1 -b:v 1M -b:a 192k -y "OUTFILEPATH.mp4" -hide_banner')}
\item \code{options(act.ffmpeg.command.fastVideoPostioning = 'ffmpeg -ss TIMESTARTMINUS10SECONDS -i "INFILEPATH" -ss 10.000 -t TIMEDURATION OPTIONS -vf scale=1920:-1 -b:v 6M -b:a 192k -y "OUTFILEPATH.mp4" -hide_banner')}
}

WAV audio cuts:
\itemize{
\item \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH.wav" -hide_banner')}
\item \code{options(act.ffmpeg.command                     = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp3" -hide_banner')}
}

\emph{Advanced options}\cr
You can adjust the FFmpeg commands according to your needs.
The following options define the FFmpeg command that will be used by the package. The command needs to contain place holders which will be replaced by the actual values in the package. If you want to define your own ffmpeg command, please make sure to use the following placeholders:
\itemize{
\item \code{INFILEPATH} path to the input media file.
\item \code{OUTFILEPATH} path where the output media file will be saved
\item \code{OPTIONS} FFmpeg options that will be applied additionally, in particular fast video positioning.
\item \code{TIMESTART} time in seconds where to begin the cutting
\item \code{TIMESTARTMINUS10SECONDS} time in seconds where to begin the cutting, in case that fast video positioning is being used.
\item \code{TIMEDURATION} duration of cuts.
}
}
\examples{
library(act)

# IMPORTANT: In the example corpus all transcripts are assigned media links.
# The actual media files are, however, not included in when installing the package 
# due to size limitations of CRAN.
# But you may download the media files separately.
# Please see the section 'examplecorpus' for instructions. 
# --> You will need the media files to execute the following example code.

\dontrun{
	# Search
	mysearch <- act::search_new(examplecorpus, pattern="yo")
	
	# Create cut lists 
	mysearch <- act::search_cuts_media (x=examplecorpus, s=mysearch)
	
	# Check results for Mac:
	# Get entire cut list for Mac and display on screen, 
	# so you can copy&paste this into the Terminal
	mycutlist <- mysearch@cuts.cutlist.mac 
	cat(mycutlist)
	# Cut list for first search result
	mycutlist <- mysearch@results$cuts.cutlist.mac[[1]]
	cat(mycutlist)
	
	# Check results for Windows:
	# Get entire cut list for Mac and display on screen, 
	# so you can copy&paste this into the CLI
	mycutlist <- mysearch@cuts.cutlist.win 
	cat(mycutlist)
	# Cut list for first search result
	mycutlist <- mysearch@results$cuts.cutlist.win[[1]]
	cat(mycutlist)
	
	# It is, however, more convenient to specify the argument 'outputFolder' in order to get
	# the cut list as a (executable) file/batch list.
}
}
