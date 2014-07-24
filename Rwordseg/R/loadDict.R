


##' Load user defined dictionary.
##' 
##' @title Load user defined dictionary.
##' @param dictpath The path of dictionary file or folder .
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

loadDict <- function(dictpath = getOption("dic.dir")) {
	dicisdir <- file.info(dictpath)$isdir
	dictfiles <- file.path(getOption("app.dir"), "userdic")
	removefiles <- file.path(getOption("app.dir"), "deldic")
	
	for (i in 1:length(dicisdir)) {
		if (!is.na(dicisdir[i]) && dicisdir[i]) {
			dictfiles <- c(dictfiles, list.files(dictpath[i], pattern = ".*\\.dic", 
					full.names = TRUE, ignore.case = TRUE))
		}
		if (!is.na(dicisdir[i]) && !dicisdir[i]) {
			dictfiles <- c(dictfiles, dictpath[i])
		}
	}
	
	if (length(dictfiles) > 0) {
		for (i in seq_along(dictfiles)) {
			tmp.enc <- .detectEncoding(dictfiles[i])[1]
			tmp <- readLines(dictfiles[i])
			tmp <- iconv(tmp, tmp.enc, "UTF-8")
			tmp <- tmp[nzchar(tmp)]
			tmp.list <- strsplit(tmp, split = "\t", fixed = TRUE)
			l1 <- try(sapply(tmp.list, FUN = function(X) X[1]), silent = TRUE)
			l2 <- try(sapply(tmp.list, FUN = function(X) X[2]), silent = TRUE)
			l3 <- try(sapply(tmp.list, FUN = function(X) X[3]), silent = TRUE)
			l1 <- .toSim(l1)
			if (all(is.na(l2))) l2 <- rep("userDefine", length(l1))
			if (all(is.na(l3))) l3 <- rep(1000, length(l1))
			
			insertWords(l1, strtype = l2, numfreq = l3, save = FALSE) 
		}
	}
	
	del.vec <- readLines(removefiles, encoding = "UTF-8")
	Encoding(del.vec) <- "UTF-8"
	del.vec <- del.vec[nzchar(del.vec)]
	del.list <- strsplit(del.vec, split = "\t", fixed = TRUE)
	del1 <- try(sapply(del.list, FUN = function(X) X[1]), silent = TRUE)
	del1 <- .toSim(unique(del1))
	if (length(del1) > 0) deleteWords(del1, save = FALSE)
}





