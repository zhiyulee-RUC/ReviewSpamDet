


##' Insert new words into dictionary.
##' 
##' @title Insert new words into dictionary.
##' @param strwords Vector of words.
##' @param analyzer A JAVA object of analyzer.
##' @param strtype The type of the nature of word.
##' @param numfreq The frequency of the word.
##' @param save Whether to save to dictionary.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

insertWords <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv), 
		strtype = rep("userDefine", length(strwords)), numfreq = rep(1000, length(strwords)), 
		save = FALSE) 
{
	if (!is.character(strwords)) stop("Please input character!")
	strwords <- tolower(strwords)
	numfreq <- abs(as.integer(numfreq))
	for (i in 1:length(strwords)) {
		.jcall(analyzer, "V", "insertWord", strwords[i], strtype[i], as.character(numfreq[i]))
	}
	if (identical(save, TRUE)) {
		tmp <- .writeDictFile(strwords, file.path(getOption("app.dir"), "userdic"), "add")
		tmp <- .writeDictFile(strwords, file.path(getOption("app.dir"), "deldic"), "remove")
	}
}






