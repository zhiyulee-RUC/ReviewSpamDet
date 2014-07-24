

##' Install new dictionary.
##' 
##' @title Install new dictionary.
##' @param dictpath Path of dictionary.
##' @param dictname English name of the dictionary.
##' @param dicttype Type of the dictionary.
##' @param load Whether to load the dictionary immediately.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>

installDict <- function(dictpath, dictname, dicttype = c("text", "scel"), load = TRUE) {
	pathverify <- try(file.exists(dictpath), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the dic file!")
	if (!any(pathverify)) stop ("Wrong path of the dic file!")
	dictpath <- dictpath[pathverify][1]
	
	dic.type <- match.arg(dicttype)
	dic.enc <- .detectEncoding(dictpath)[1]
	dic.suffix <- tolower(gsub("^.*\\.", "", dictpath))
	if (dic.type == "text" && dic.suffix %in% dicttype) dic.type <- dic.suffix

	outfile <- file.path(getOption("dic.dir"), paste(dictname, "dic", sep = "."))
			
	switch(dic.type, 
			text = {
				tmp.dic <- readLines(dictpath, encoding = dic.enc)	
				tmp.dic <- iconv(tmp.dic, dic.enc, "UTF-8")
				tmp.list <- strsplit(tmp.dic, "\t| ")
				newwords <- try(sapply(tmp.list, FUN = function(X) X[1]), silent = TRUE)
				newtypes <- try(sapply(tmp.list, FUN = function(X) X[2]), silent = TRUE)
				if (all(grepl("^[^\t]+\t[^\t]+\t[^\t]+$", tmp.dic[nzchar(tmp.dic)]))) {
					outwords <- tmp.dic
					dictype = "Ansj"
				} else {
					newwords <- unique(newwords[!is.na(newwords)])
					newwords <- tolower(gsub(" ", "", newwords))
					newtypes <- rep(dictname, length(newwords))
					outwords <- paste(newwords, dictname, 1000, sep = "\t")
					dictype = "Text"
				}

				.writeUTF8Lines(outwords, outfile)
				.addDictMeta(dictname, dictype, basename(dictpath), outfile)
				
				if (identical(load, TRUE)) {
					insertWords(newwords, analyzer = get("Analyzer", envir = .RwordsegEnv), 
							strtype = newtypes, numfreq = rep(1000, length(newwords)), 
							save = FALSE) 
					cat(length(newwords))
					cat(" words were loaded! ... ")
				}
			},
			scel = {
				sogouV <- .importSogouScel(dictpath)
				newwords <- tolower(as.vector(sogouV))
				outwords <- paste(newwords, dictname, 1000, sep = "\t")
				.writeUTF8Lines(outwords, outfile)
				.addDictMeta(dictname, attributes(sogouV)$Type, attributes(sogouV)$Description, outfile)
				if (identical(load, TRUE)) {
					insertWords(newwords, analyzer = get("Analyzer", envir = .RwordsegEnv), 
							strtype = rep(dictname, length(newwords)), numfreq = rep(1000, length(newwords)), 
							save = FALSE) 
					cat(length(newwords))
					cat(" words were loaded! ... ")
				}
			}
	)
	
	cat("New dictionary '")
	cat(dictname)
	cat("' was installed!\n")
}




