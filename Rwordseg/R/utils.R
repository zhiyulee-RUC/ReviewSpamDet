
.detectEncoding <- function(strpaths) {
	pathverify <- try(file.exists(strpaths), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the dic file!")
	if (!any(pathverify)) stop ("Wrong path of the dic file!")
	strpath <- strpaths[pathverify][1]
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "S", "detectEncoding", strpath), silent = TRUE)
	if (inherits(tmp, "try-error")) {
		stop(paste("Fail to detect encoding:\n", as.character(tmp), "\n"))
	} else {
		tmp <- gsub(" *", "", tmp)
	}
	iconv.list <- iconvlist()
	OUT <- iconv.list[grep(toupper(tmp), toupper(iconv.list))]
	return(OUT)
}

.addDictMeta <- function(Name, Type = "", Des = "", Path = "") {
	Metafile <- file.path(getOption("app.dir"), "dicmeta")
	if (file.exists(Metafile)) {
		oriDf <- readRDS(Metafile)
	} else {
		oriDf <- data.frame(Name = character(0), Type = character(0), Des = character(0), Path = character(0), stringsAsFactors = FALSE)
	}	
	newDf <- data.frame(Name = Name, Type = Type, Des = Des, Path = Path, stringsAsFactors = FALSE)
	if (Name %in% oriDf$Name) {
		warning(paste("'", Name, "' was installed!"))
	} else {
		outDf <- rbind(oriDf, newDf)
		saveRDS(outDf, Metafile)
	}
}

.removeDictMeta <- function(Names) {
	Metafile <- file.path(getOption("app.dir"), "dicmeta")
	oriDf <- readRDS(Metafile)
	if (!any(Names %in% oriDf$Name)) {
		warning(paste("There is no '", Names, "' installed!"))
	} else {
		outDf <- oriDf[-which(oriDf$Name %in% Names), ]
		saveRDS(outDf, Metafile)
	}
}

.setNameReco <- function(isReco = TRUE) {
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "V", "setNameRecognition", isReco), silent = TRUE)
}

.setNumReco <- function(isReco = TRUE) {
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "V", "setNumRecognition", isReco), silent = TRUE)
}

.setQuantifierReco <- function(isReco = TRUE) {
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "V", "setQuantifierRecognition", isReco), silent = TRUE)
}

.toSim <- function(string)
{
	transDf <- get("data.trad", envir = .RwordsegEnv)
	OUT <- chartr(transDf$Tra, transDf$Sim, string)
	return(OUT)
}

.segWord <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	OUT <- .jcall(analyzer, "S", "segWord", strwords)
	Encoding(OUT) <- "UTF-8"
	return(OUT)
}

.segWordInd <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	OUT <- .jcall(analyzer, "S", "segWordInd", strwords)
	return(OUT)
}

.segWord <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv)) {
	OUT <- .jcall(analyzer, "S", "segWord", strwords)
	Encoding(OUT) <- "UTF-8"
	return(OUT)
}

.cleanjars <- function() {
	cur.jars <- list.files(system.file("java", package = "Rwordseg"), full.names = TRUE)
	tar.jars <- c("ansj_seg-0.9.1-jli.jar", "jianl_seg.jar", "juniversalchardet-1.0.3.jar","tree_split-1.0.1.jar")
	del.jars <- cur.jars[!basename(cur.jars) %in% tar.jars]
	OUT <- NULL
	if (length(del.jars) > 0) OUT <- try(unlink(del.jars, force = TRUE), silent = TRUE)
	invisible(OUT)
}

.importSogouScel <- function(strpaths) {
	pathverify <- try(file.exists(strpaths), silent = TRUE)
	if (inherits(pathverify, "try-error")) stop("Please input the path string of the Scel file!")
	if (!any(pathverify)) stop ("Wrong path of the Scel file!")
	strpath <- strpaths[pathverify][1]
	analyzer = get("Analyzer", envir = .RwordsegEnv)
	tmp <- try(.jcall(analyzer, "S", "importSogou", strpath), silent = TRUE)
	if (inherits(tmp, "try-error")) {
		stop(paste("Fail to import", basename(strpath), ":\n", as.character(tmp), "\n"))
	} else {
		Encoding(tmp) <- "UTF-8"
		out.type <- sub("Type: *", "", sub("Des:.*$", "", tmp))
		out.des <- sub("^.*?Des: +", "", sub("Dict:.*$", "", tmp))
		out.dict <- sub(paste("^.*?Des: *", out.des, "Dict: +", sep = ""), "", tmp)
	}
	OUT <- strsplit(out.dict, split = " ")[[1]]
	attr(OUT, "Type") <- out.type
	attr(OUT, "Description") <- out.des
	return(OUT)
	
}

.writeDictFile <- function(newwords, dictfile, type = c("add", "remove"), dictname = "userDefine") {
	type <- match.arg(type)
	
	ori.dic <- readLines(dictfile)
	Encoding(ori.dic) <- "UTF-8"
	ori.dic <- ori.dic[nzchar(ori.dic)]
	if (length(ori.dic) == 0) return(NULL)
	oriwords <- sapply(strsplit(ori.dic, "\t"), FUN = function(X) X[1])
	
	if (type == "add") {
		oriwords <- tolower(unique(oriwords[!is.na(oriwords)]))
		addwords <- newwords[! newwords %in% oriwords]
		if (length(addwords) == 0) return(NULL)
		outwords <- c(ori.dic, paste(addwords, dictname, 1000, sep = "\t"))
	}
	
	if (type == "remove") {
		keeprows <- which(!oriwords %in% newwords)
		outwords <- ori.dic[keeprows]
		if (length(outwords) == 0) return(NULL)
	}
	
	.writeUTF8Lines(outwords, dictfile)
	
	invisible(TRUE)
}

.writeUTF8Lines <- function(text, con, sep = "\n") {
	if (.Platform$OS.type == "windows") {
		old.locale <- Sys.getlocale("LC_CTYPE")
		Sys.setlocale(category = "LC_CTYPE", locale = "chs")
		conn.w <- file(con, open = "w", encoding = "UTF-8")
		writeLines(text, conn.w, sep = sep, useBytes = FALSE)
		close(conn.w)
		Sys.setlocale(category = "LC_CTYPE", locale = old.locale)
	} else {
		conn.w <- file(con, open = "w", encoding = "UTF-8")
		writeLines(text, conn.w, sep = sep, useBytes = FALSE)
		close(conn.w)
	}
}

