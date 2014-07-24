# TODO: Add comment
# 
# Author: jli
###############################################################################

.onAttach <- function(libname, pkgname ){
	packageStartupMessage( paste("# Version:", utils:::packageDescription("Rwordseg", fields = "Version")) )
	options(dic.dir = system.file("dict", package = "Rwordseg"))
	options(app.dir = system.file("config", package = "Rwordseg"))
	.cleanjars()
	.jpackage(pkgname, lib.loc=libname)
	dictpath <- chartr("\\", "/", file.path(getOption("app.dir"), "userdic"))
	if (!exists(".RwordsegEnv", envir = .GlobalEnv)) {
		assign(".RwordsegEnv", new.env(), envir = .GlobalEnv)
	}
	if (exists("Analyzer", envir = .RwordsegEnv)) {
		rm("Analyzer", envir = .RwordsegEnv)
	}
	Analyzer <- .jnew("org/jianl/rinterface/Analyzer")
	.jcall(Analyzer, "V", "setDicPath", dictpath)
	.jcall(Analyzer, "V", "initialAnalyzer")
	assign("Analyzer", Analyzer, envir = .RwordsegEnv)
	
	data.trad <- readLines(system.file("config", "jianFan.dic", package = "Rwordseg"), encoding = "UTF-8")
	data.trad <- strsplit(data.trad, split = "\t")
	data.trad <- data.frame(Tra = paste(sapply(data.trad, FUN = function(X) X[1]), collapse = ""),
			Sim = paste(sapply(data.trad, FUN = function(X) X[2]), collapse = ""), 
			stringsAsFactors = FALSE)
	assign("data.trad", data.trad, envir = .RwordsegEnv)
	
	tryload <- try(loadDict(), silent = TRUE)
	if (inherits(tryload, "try-error")) warning(paste("Fail to load the user defined dictionary:\n", as.character(tryload)))
	
	segment.options(isNameRecognition = FALSE)
	segment.options(isNumRecognition = TRUE)
	segment.options(isQuantifierRecognition = TRUE)
}

.onUnload <- function(libpath) {
	rm(.RwordsegEnv, envir = .GlobalEnv)
	library.dynam.unload("Rwordseg", libpath)
}
