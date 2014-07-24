

##' List the installed dictionary.
##' 
##' @title List the installed dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
listDict <- function() {
	Metafile <- file.path(getOption("app.dir"), "dicmeta")
	if (file.exists(Metafile)) {
		OUT <- readRDS(Metafile)
	} else {
		OUT <- data.frame(Name = character(0), Type = character(0), Des = character(0), Path = character(0), stringsAsFactors = FALSE)
	}
	return(OUT)
}



