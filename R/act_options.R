act.options.default <- list (
	#--- do not reset
	# *will be checked on loading of package
	act.path.praat					    	= "",
	act.path.sendpraat					    = "",
	
	act.excamplecorpusURL                      = " http://www.romanistik.uni-freiburg.de/ehmer/files/digitalhumanities/act_examplecorpus.zip",
	
	act.ffmpeg.command.mp4	 	 	 	       = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH" -hide_banner',
	act.ffmpeg.command.mp4.FastVideoPostioning = 'ffmpeg -ss TIMESTARTMINUS10SECONDS -i "INFILEPATH" -ss 10.000 -t TIMEDURATION OPTIONS -y "OUTFILEPATH.mp4" -hide_banner',
	act.ffmpeg.command.wav	 		 		   = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH" -hide_banner',
	act.ffmpeg.command.mp3	 		 	 	   = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH" -hide_banner',
	act.ffmpeg.command.default		 	 	   = 'ffmpeg -i "INFILEPATH" -ss TIMESTART -t TIMEDURATION OPTIONS -y "OUTFILEPATH" -hide_banner',
	act.ffmpeg.command.UseFastVideoPostioning  = TRUE,
	act.ffmpeg.exportchannels.fromColumnName   = "channels", 

	act.load.readEmptyIntervals 			= FALSE,
	act.load.replaceInFilenames.search      = "", 
	act.load.replaceInFilenames.replace     = "",
	act.load.scanSubfolders                 = TRUE,

	act.export.foldergrouping1.fromColumnName 		= "resultID",
	act.export.foldergrouping2.fromColumnName 		= "",
	act.export.filename.fromColumnName 			    = "resultID",
	act.export.filter.tiers.include  		= "",
	act.export.filter.tiers.exclude   		= "",
	

	act.transcript.pauseTier.regex   				= "",
	act.transcript.insertIntoColumn 				= "transcript",
	act.transcript.transcript.width 				= 65,
	act.transcript.speaker.width    				= 3,
	act.transcript.speaker.ending 					= ":  ",
	act.transcript.spacesbefore 					= 3,
	act.transcript.arrow.insert 					= TRUE,
	act.transcript.arrow.shape  					= "->",
	act.transcript.header.insert 					= TRUE,
	act.transcript.header.heading.fromColumnName 	= "header.heading",
	act.transcript.header.firstInfo.fromColumnName 	= "header.firstinfo",
	act.transcript.additionalline1.insert           = FALSE,
	act.transcript.additionalline1.text             = "",
	act.transcript.additionalline1.indent           = TRUE,
	act.transcript.additionalline2.insert           = FALSE,
	act.transcript.additionalline2.text             = "",
	act.transcript.additionalline2.indent           = FALSE,
	act.transcript.brackets.tryToAlign 				= TRUE,
	
	act.concordanceWidth					= 120,
	act.separator_between_intervals 		= "&",
	act.separator_between_tiers				= "#",
	act.separator_between_words				= "^\\s|\\|\\'|\\#|\\/|\\\\\\\\",
	act.wordCount.regex 					= '(?<=[^|\\b])[A-z\\u00C0-\\u00FA\\-\\:]+(?=\\b|\\s|_|$)'
	
)

#' Remove all options set by the package from R options
#'
#' @export
#'
#' @examples
#' library(act)
#' act::options_remove()
options_remove <- function() {
	options(act.path.praat = NULL)
	options(act.path.sendpraat						= NULL)
	
	options(act.excamplecorpusURL                   = NULL)
	
	options(act.ffmpeg.command.mp4	 	 	 	       = NULL)
	options(act.ffmpeg.command.mp4.FastVideoPostioning = NULL)
	options(act.ffmpeg.command.wav	 		 		   = NULL)
	options(act.ffmpeg.command.mp3	 		 	 	   = NULL)
	options(act.ffmpeg.command.default		 	 	   = NULL)
	options(act.ffmpeg.command.UseFastVideoPostioning  = NULL)
	options(act.ffmpeg.exportchannels.fromColumnName   = NULL)
			
	options(act.load.readEmptyIntervals 		    = NULL)
	options(act.load.replaceInFilenames.search      = NULL)
	options(act.load.replaceInFilenames.replace     = NULL)
	options(act.load.scanSubfolders                 = NULL)
	
	options(act.export.foldergrouping1.fromColumnName= NULL)
	options(act.export.foldergrouping2.fromColumnName= NULL)
	options(act.export.filename.fromColumnName= NULL)
	options(act.export.filter.tiers.exclude = NULL)
	options(act.export.filter.tiers.include = NULL)
	
	options(act.transcript.pauseTier.regex 	 	= NULL)
	options(act.transcript.insertIntoColumn 	= NULL)
	options(act.transcript.transcript.width 	= NULL)
	options(act.transcript.speaker.width    	= NULL)
	options(act.transcript.speaker.ending 		= NULL)
	options(act.transcript.spacesbefore 		= NULL)
	options(act.transcript.arrow.insert 		= NULL)
	options(act.transcript.arrow.shape  		= NULL)
	options(act.transcript.header.insert 		= NULL)
	options(act.transcript.header.heading.fromColumnName =  NULL)
	options(act.transcript.header.firstInfo.fromColumnName = NULL)
	options(act.transcript.additionalline1.insert           = NULL)
	options(act.transcript.additionalline1.text             = NULL)
	options(act.transcript.additionalline1.indent           = NULL)
	options(act.transcript.additionalline2.insert           = NULL)
	options(act.transcript.additionalline2.text             = NULL)
	options(act.transcript.additionalline2.indent           = NULL)
	options(act.transcript.brackets.tryToAlign 		= NULL)

	options(act.concordanceWidth                    = NULL)
	options(act.separator_between_intervals = NULL)
	options(act.separator_between_tiers = NULL)
	options(act.separator_between_words = NULL)
	options(act.wordCount.regex = NULL)
}


#' Reset options to default values
#'
#' @export
#'
#' @examples
#' library(act)
#' act::options_reset()
options_reset <- function () {
	options(act.options.default[2:length(act.options.default)])
}
