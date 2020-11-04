#' Search and replace contents of annotations using a matrix
#'
#' This functions performs a search and replace in the contents of an annotation. 
#' A simple matrix consisting of two columns will be used. 
#' The first column of the matrix needs to contain the search string, the second column  the replacement string.
#' The matrix needs to be in CSV format.
#' 
#' @seealso [matrix_load()] for loading the matrix 
#' and [matrix_save()] for saving the matrix to a CSV file.
#'
#' @param x Corpus object.
#' @param path_replacementMatrixCSV Character string; path to replacement matrix (a CSV file).
#' @param showProgress Logical; if \code{TRUE} the progress bar will be displayed.
#'
#' @return Corpus object.
#' @export
#'
#'@seealso \link{media_remove}, \link{media_getpath}
#'
#' @example inst/examples/modify_data_matrix.R
#'  
modify_data_matrix <- function(x, path_replacementMatrixCSV, showProgress=TRUE){
	#=== load the matrix
	act_replacementMatrix <- matrix_load(path_replacementMatrixCSV)
	
	#=== check matrix
	if (is.null(act_replacementMatrix)) 						{	stop("Normalization matrix not read.")		}
	if ("search" %in% colnames(act_replacementMatrix)==FALSE)   {	stop("Column 'search' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")}
	if ("replace" %in% colnames(act_replacementMatrix)==FALSE)  {	stop("Column 'replace' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")	}
	
	
	#=== check data
	if (is.null(x)) 				{	stop("Corpus object x is null.")	}
	if (is.null(x@transcripts)) 	{	stop("No transcripts found in corpus object x.")	}
	
	#replace NA by empty string
	act_replacementMatrix$replace[is.na(act_replacementMatrix$replace)] <- ""
	
	#=== create named vector for replacement
	mymatrix 		<- as.character(act_replacementMatrix$replace)
	names(mymatrix) <- act_replacementMatrix$search
	#as.data.frame(mymatrix)
	
	#=== check if the matrix works
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
	if (is.null(out)) 						{	stop("Replacement matrix seems to be containing invalid regular expressions.")		}
	
	#=== do the replacement
	if (length(mymatrix)<1) {
		warning("Replacement matrix is empty.")
		
	} else {
		
		if (showProgress) {
			pb1 <- progress::progress_bar$new(
				format = "  Processing  [:bar] :percent missing: :eta",
				total = length(x@transcripts), 
				clear = FALSE, 
				show_after = 0,
				width= 60)
			
			
			for (i in 1:length(x@transcripts)) 		{
				pb1$tick()
				
				#towower
				x@transcripts[[i]]@data$content <- stringr::str_to_lower(x@transcripts[[i]]@data$content)   
				
				#replace
				x@transcripts[[i]]@data$content <- stringr::str_replace_all(x@transcripts[[i]]@data$content, mymatrix)  
			}
		} else {
			for (i in 1:length(x@transcripts))  		{
				#tolower
				#x@transcripts[[i]]@data$content <- stringr::str_to_lower(x@transcripts[[i]]@data$content)   
				
				#replace
				x@transcripts[[i]]@data$content <- stringr::str_replace_all(x@transcripts[[i]]@data$content, mymatrix)
			}
		}
	}
	
	x@normalization.upToDate<-FALSE
	x@fulltext.upToDate<-FALSE
	
	return (x)
}


