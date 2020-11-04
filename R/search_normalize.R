#' Normalize transcriptions
#'
#' Normalizes the contents of transcriptions in a corpus using a normalization matrix.
#' Function returns a corpus object with normalized transcription and updates the original corpus object passed as argument to x.
#'
#' @param x Corpus object.
#' @param showProgress Logical; if \code{TRUE} the progress bar will be displayed.
#' @param path_replacementMatrixCSV Character string; path to replacement matrix in CSV format. If empty, the default replacement matrix that comes with the package will be used.
#' @param updateX Logical; If \code{TRUE} the original corpus object passed as x to the function will also be updated! If you do not want this, set to \code{FALSE}. 
#' @export
#'
#' @examples
#' library(act)
#' 
#' examplecorpus <- act::normalize(x=examplecorpus)
#' 
normalize <- function(x, showProgress=TRUE, path_replacementMatrixCSV="", updateX=TRUE){
	#=== capture original x
	captured_x <- substitute(x)
	
	#=== load the matrix
	if  (path_replacementMatrixCSV=="") {
		path_replacementMatrixCSV <- normalization_getPathToDefaultMatrix()
	}
	act_replacementMatrix <- act::matrix_load(path_replacementMatrixCSV, "UTF-8")
	
	#=== check matrix
	if (is.null(act_replacementMatrix)) 					  {	stop("Normalization matrix not read.")		}
	if ("search" %in% colnames(act_replacementMatrix)==FALSE) {	stop("Column 'search' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")}
	if ("replace" %in% colnames(act_replacementMatrix)==FALSE){	stop("Column 'replace' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")	}
	
	#=== check data
	if (is.null(x)) 				{	stop("Corpus object x is null.")	}
	if (is.null(x@transcripts)) 	{	stop("No transcripts found in corpus object x.")	}
	
	#replace NA by empty string
	act_replacementMatrix$replace[is.na(act_replacementMatrix$replace)] <- ""
	
	#=== create named vector for replacement
	mymatrix 		<- as.character(act_replacementMatrix$replace)
	names(mymatrix) <- act_replacementMatrix$search
	#as.data.frame(mymatrix)
	
	
	#=== check if the marix works
	out <- tryCatch(
		{
			#This is the 'try' part
			stringr::str_replace_all("test string", mymatrix)
		},
		error=function(cond) {
			#this is the error part
			NULL
		}
	)
	if (is.null(out)) 						{	stop("Normalization matrix seems to be containing invalid regular expressions.")		}
	
	#=== do the replacement
	if (length(mymatrix)<1) {
		warning("Replacement matrix is empty.")
		for (i in 1:length(x@transcripts)) 		{
			#towower
			x@transcripts[[i]]@data$content.norm <- stringr::str_to_lower(x@transcripts[[i]]@data$content)   
			#trim
			x@transcripts[[i]]@data$content.norm <- stringr::str_trim(x@transcripts[[i]]@data$content.norm, side="both")
		}
		
	} else {
		
		if (showProgress) {
			pb1 <- progress::progress_bar$new(
				format = "  Normalizing        [:bar] :percent missing: :eta",
				total = length(x@transcripts), 
				clear = FALSE, 
				show_after = 0,
				width= 60)
		}
		for (i in 1:length(x@transcripts)) 		{
			if (showProgress) {pb1$tick()}
			#towower
			x@transcripts[[i]]@data$content.norm <- stringr::str_to_lower(x@transcripts[[i]]@data$content)  
			#replace
			x@transcripts[[i]]@data$content.norm <- stringr::str_replace_all(x@transcripts[[i]]@data$content.norm, mymatrix)  
			#trim
			x@transcripts[[i]]@data$content.norm <- stringr::str_trim(x@transcripts[[i]]@data$content.norm, side="both")
		}
	}
	x@normalization.upToDate<-TRUE
	x@lastModification <- list(x@lastModification, "normalize")
	
	#assign to original corpus object
	if (updateX) {
		p <- parent.frame() 
		p[[deparse(captured_x)]] <- x
	}
	
	return(x)
}

normalization_getPathToDefaultMatrix <- function() {
	myPath<-options()$act.path.normalizationMatrixCSV.default
	if (is.null(myPath)) { 		myPath <- "" 	}
	
	#if matrix has not been found
	if (!file.exists(myPath)) {
		#set to standard value and try again
		options(act.path.normalizationMatrixCSV.default	= system.file("extdata", "normalization", "normalizationMatrix.csv", package="act"))
		myPath<-options()$act.path.normalizationMatrixCSV.default
		if (is.null(myPath)) {  			myPath <- ""  		}
		
		if (file.exists(myPath)) {
			#warning("The path to the 'normalization matrix' has been set to default location.")
		} else {
			stop("The has been a problem locating the default 'normalization matrix'. Please install the act package again.")
		}
	}
	return(myPath)
}


