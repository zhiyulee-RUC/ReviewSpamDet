

##' Remove words into dictionary.
##' 
##' @title Remove words into dictionary.
##' @param strwords Vector of words.
##' @param analyzer A JAVA object of analyzer.
##' @param save Whether to save to dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
deleteWords <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv), save = FALSE) {
	if (!is.character(strwords)) stop("Please input character!")
	strwords <- tolower(strwords)
	for (strword in strwords) {
		.jcall(analyzer, "V", "removeWord", strword)
	}
	if (identical(save, TRUE)) {
		tmp <- .writeDictFile(strwords, file.path(getOption("app.dir"), "deldic"), "add")
		tmp <- .writeDictFile(strwords, file.path(getOption("app.dir"), "userdic"), "remove")
	}
}






