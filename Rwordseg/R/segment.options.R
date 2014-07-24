

##' Set options of segmentation.
##' 
##' The are 3 built-in options can be used for segmentation. Each of them has logical value. 
##' \code{isNameRecognition} means whether to recognize person name automatically, default is FALSE.
##' \code{isNumRecognition} means whether to recognize numbers automatically, default is TRUE.
##' \code{isQuantifierRecognition} means whether to recognize quantifier automatically, default is TRUE.
##' @title Set options of segmentation.
##' @param ... any options can be defined, the same as \code{\link{options}}.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
segment.options <- function(...) {
	arglist <- list(...)
	arglist <- arglist[nzchar(names(arglist))]
	argnames <- names(arglist)
	if (length(arglist) > 0) {
		for (i in 1:length(arglist)) {
			eval(parse(text = paste("options(", argnames[i], "=", arglist[[i]], ")")))
			if (argnames[i] == "isNameRecognition") {
				if (identical(arglist[[i]], TRUE)) {
					.setNameReco(TRUE)
				} else {
					.setNameReco(FALSE)
				}
			}
			if (argnames[i] == "isNumRecognition") {
				if (identical(arglist[[i]], TRUE)) {
					.setNumReco(TRUE)
				} else {
					.setNumReco(FALSE)
				}
			}
			if (argnames[i] == "isQuantifierRecognition") {
				if (identical(arglist[[i]], TRUE)) {
					.setQuantifierReco(TRUE)
				} else {
					.setQuantifierReco(FALSE)
				}
			}
		}
	}
}






