

##' Uninstall the user defined dictionary.
##' 
##' @title Uninstall the user defined dictionary.
##' @param removedict Names of the dictionary to be uninstalled.
##' @param remove Whether to remove the words immediately.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
uninstallDict <- function(removedict = listDict()$Name, remove = TRUE) {
	if (length(removedict) > 0) {
		
		outfiles <- file.path(getOption("dic.dir"), paste(removedict, "dic", sep = "."))
		for (i in 1:length(outfiles)) {
			outfile <- outfiles[i]
			if (file.exists(outfile)) {
				if (identical(remove, TRUE)) {
					tmp.dic <- readLines(outfile, encoding = "UTF-8")	
					Encoding(tmp.dic) <- "UTF-8"
					delwords <- try(sapply(strsplit(tmp.dic, "\t"), FUN = function(X) X[1]), silent = TRUE)
					deleteWords(delwords, analyzer = get("Analyzer", envir = .RwordsegEnv), save = FALSE) 
					cat(length(delwords))
					cat(" words were removed! ... ")
				}
				.removeDictMeta(removedict[i])
				try(unlink(outfile, force = TRUE), silent = TRUE)
				cat("The dictionary '")
				cat(removedict[i])
				cat("' was uninstalled!\n")
			}
			
		}
	}
}



