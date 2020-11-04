#' Aligned Corpus Toolkit
#' @description The Aligned Corpus Toolkit (act) is designed for linguists that work with time aligned transcription data. It offers advanced search possibilities in transcriptions (full text search, normalized search, concordance etc.), functions to modify the data, import-export functionality for 'Praat' '*.TextGrid' files, export for 'ELAN' '*.eaf' files, the creation of batch lists for cutting audio and video files with 'FFmpeg', the creation of printable transcripts in the style of conversation analysis, and interaction with 'Praat' using 'Praat'-scripts. The package is itself written in R and may be expanded by other users.
#' 
#' @section act functions:
#' ...
#' @md
#' 
#' @section Package options:
#' There are several options that change the way the package works and that are set globally.
#' * Use \code{options(name.of.option = value)} to set an option.
#' * Use \code{options()$name.of.option} to get the current value of an option.
#' * Use \code{act::options_reset()} to set all options to the default value.
#' * Use \code{act::options_remove()} to clean up and remove all option settings.
#'
#' The package uses the following options.
#' 
#' \emph{Paths}
#' * \code{act.path.praat} Character string; path to the Praat executable on your computer. Only necessary if you use the functions to remote control Praat using Praat scripts.
#' * \code{act.path.sendpraat} Character string; path to the sendpraat executable on your computer. Only necessary if you use the functions to remote control Praat using Praat scripts.
#' 
#' \emph{FFMPEG commands}
#' The following options define the ffmpeg command that will be used by the package. The command needs to contain place holders which will be replaced by the actual values in the package. If you want to define your own ffmpeg command, please make sure to use the following placeholders:
#' * \code{INFILEPATH}  path to the input media file.
#' * \code{OUTFILEPATH} path where the output media file will be saved
#' * \code{OPTIONS} ffmpeg options that will be applied additionally, in particular fast video positioning.
#' * \code{TIMESTART} time in seconds where to begin the cutting
#' * \code{TIMESTARTMINUS10SECONDS} time in seconds where to begin the cutting, in case that fast video positioning is being used.
#' * \code{TIMEDURATION} duration of cut.
#' 
#' * \code{act.ffmpeg.command.mp4} Character string; ffmpeg command that is used to cut mp4 files.
#' * \code{act.ffmpeg.command.mp4.FastVideoPostioning} Character string; ffmpeg command that is used to cut mp4 files using the ffmpeg option of fast video positioning.
#' * \code{act.ffmpeg.command.wav} Character string; ffmpeg command that is used to cut wav files.
#' * \code{act.ffmpeg.command.mp3} Character string; ffmpeg command that is used to cut mp3 files.
#' * \code{act.ffmpeg.command.default} Character string; ffmpeg command that is used to cut all other files besides mp4, mp3, and wav.
#' * \code{act.ffmpeg.command.UseFastVideoPostioning} Logical; if \code{TRUE} the ffmpeg option using fast video positioning (ant the respective commands as defined in the other options) will be used.
#' 
#' \emph{Load annotation files}
#' * \code{act.load.readEmptyIntervals} Logical; if \code{TRUE} empty intervals in you annotation files will be read, if \code{FALSE} empty intervals will be skipped.
#' * \code{act.load.replaceInFilenames.search} Character string; This option is useful if your annotation files contain character sequences that you do not want to include into the transcript name in the corpus (e.g. if you regularly add a date to the file name of your annotations files  as 'myFile_2020-09-21.TextGrid'). This character string is a regular expression Regular expression that will be applied to the file names when loading an annotation file. The hits will be replaced by \code{act.load.replaceInFilenames.replace}.
#' * \code{act.load.replaceInFilenames.replace} Character string; replacement for matches to \code{act.load.replaceInFilenames.search} in file names.
#' * \code{act.load.scanSubfolders} Logical; if \code{TRUE} sub folders will also be scanned for annotation files; if \code{FALSE} only the main level of the folders specified in \code{folders.annotationfiles} of your corpus object will be scanned. 
#'  
#' \emph{Export}
#' * \code{act.export.foldergrouping1.fromColumnName} Character string; Name of sub folders that will be created in the folder of the search result, level 1.
#' * \code{act.export.foldergrouping2.fromColumnName}  Character string; Name of sub folders that will be created in the folder of the search result, level 2.
#' * \code{act.export.filename.fromColumnName}  Character string; Name of the column from which the file names for exported files will be taken.
#' * \code{act.export.filter.tiers.exclude} Character string; regular expression which tiers to EXclude in the exported files.
#' * \code{act.export.filter.tiers.include} Character string; regular expression which tiers to INclude in the exported files.
#' 
#' \emph{Transcript}
#' * \code{act.transcript.pauseTier.regex} Character string; regular expression to identify pause tier for auto formating pauses.
#' * \code{act.transcript.insertIntoColumn} Character string; name of column destination column in the search results data frame into which context will be inserted; column will be created if not present in data frame; set to "" if for no insertion.
#' * \code{act.transcript.transcript.width} Integer; width of transcript, -1 for no line wrapping.
#' * \code{act.transcript.speaker.width} Integer; width of speaker abbreviation, -1 for full name without shortening.
#' * \code{act.transcript.speaker.ending} Character string; string that is added at the end of the speaker name.
#' * \code{act.transcript.spacesbefore} Integer; number of spaces inserted before line number.
#' * \code{act.transcript.arrow.insert} Logical; if \code{TRUE} an arrow will be inserted, highlighing the transcript line containing the search hit.
#' * \code{act.transcript.arrow.shape} Character string; shape of the arrow. 
#' * \code{act.transcript.header.insert} Logical; if \code{TRUE} a transcript header is inserted.
#' * \code{act.transcript.header.heading.fromColumnName} Character string; from which column the heading is taken (if \code{options()$act.transcript.header.insert==TRUE})
#' * \code{act.transcript.header.firstInfo.fromColumnName} Character string; from which column the first info is taken (if \code{options()$act.transcript.header.insert==TRUE})
#' * \code{act.transcript.additionalline1.insert} Logical; if \code{TRUE} an additional dummy line will be inserted after each annotation line, the text is defined in \code{act.transcript.additionalline1.text}.
#' * \code{act.transcript.additionalline1.text} Character string; Content of additional dummy line 1.
#' * \code{act.transcript.additionalline1.indent} Logical; if \code{TRUE} the content of the dummy line 1 will be indented to begin where the content of the annotations start.
#' * \code{act.transcript.additionalline2.insert} Logical; if \code{TRUE} an additional dummy line will be inserted after each annotation line, the text is defined in \code{act.transcript.additionalline2.text}.
#' * \code{act.transcript.additionalline2.text} Character string; Content of additional dummy line 2.
#' * \code{act.transcript.additionalline2.indent} Logical; if \code{TRUE} the content of the dummy line 2 will be indented to begin where the content of the annotations start.
#' * \code{act.transcript.brackets.tryToAlign} Logical; if \code{TRUE} act will try to align brackets [] for parralel speaking (Attention: experimental function; results may not satisfy).
#' 
#' \emph{Miscellaneous}
#' * \code{act.concordanceWidth} Integer; width of concordance.
#' * \code{act.separator_between_intervals} Character; Single character that is used for separating intervals when creating the full text.
#' * \code{act.separator_between_tiers} Character; Single character that is used for separating tiers when creating the full text.
#' * \code{act.separator_between_words} Character string; regular expression with alternatives that count as separators between words. Used for preparing the concordance.
#' * \code{act.wordCount.regex} Character string; regular expression that is used to count words.
#' 
#' @docType package
#' @name act
#' 
#' @example inst/examples/act-package.R
#' 
NULL
