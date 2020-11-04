#' Load replacement matrix
#'
#' This function is only for checking how the normalization matrix will be loaded internally.
#'
#' @param path Character string; path to the replacement matrix (a CSV file).
#' @param myFileEncoding Character string; encoding of the file.
#'
#' @return data.frame
#' @export
#'
#' @example inst/examples/matrix_load.R
#' 
matrix_load <- function(path="", myFileEncoding="UTF-8") {
	if (file.exists(path)==FALSE) 	{
		stop("Path to normalization CSV does not exist")
	}
	temp <- utils::read.table(path, header = TRUE, sep = ";", fileEncoding = myFileEncoding, encoding=myFileEncoding )
	
	if ("search" %in% colnames(temp)==FALSE) 	{
		stop("Column 'search' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")
	}
	if ("replace" %in% colnames(temp)==FALSE) 	{
		stop("Column 'replace' is missing in normalization matrix CSV file. File needs to contain colums 'search' and 'replace'")
	}
	return(temp)
}