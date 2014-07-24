
##' A function segment Chinese sentence into words.
##' 
##' @title Sengment a sentence.
##' @param strwords A Chinese sentence in UTF-8 or the path of a text file.
##' @param analyzer A JAVA object of analyzer.
##' @param nature Whether to recognise the nature of the words.
##' @param nosymbol Whether to keep symbols in the sentence.
##' @param returnType Default is a string vector but we also can choose 'tm' 
##' to output a single string separated by space so that it can be used by \code{\link[tm]{Corpus}} directly. 
##' @param isfast Whether to run the fast analyzer.
##' @param outfile The path of output if strwords is a file.
##' @param blocklines The (maximal) number of lines to read at one time when strwords is a file.
##' @return a vector of words (list if input is vecter) which have been segmented or the path of output file.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples \dontrun{
##' segmentCN("hello world!")
##' }

segmentCN <- function(strwords, analyzer = get("Analyzer", envir = .RwordsegEnv), 
		nature = FALSE, nosymbol = TRUE, returnType = c("vector", "tm"), isfast = FALSE,
		outfile = "", blocklines = 1000) 
{
	if (!is.character(strwords)) stop("Please input character!")
	
	if (length(strwords) == 1 && file.exists(strwords)) {
		if (!file.exists(dirname(outfile))) {
			filebase <- gsub("\\.[^\\.]*$", "", strwords)
			fileext <- gsub(filebase, "", strwords, fixed = TRUE)
			outfile <- paste(filebase, ".segment", fileext, sep = "")
		}
		tmp.enc <- .detectEncoding(strwords)[1]
		#if (tmp.enc != "UTF-8") tmp.enc <- "GBK"
		nlines <- blocklines
		
		if (.Platform$OS.type == "windows"){
			old.locale <- Sys.getlocale("LC_CTYPE")
			Sys.setlocale(category = "LC_CTYPE", locale = "chs")
		}
		
		conn.r <- file(strwords, open = "r")
		conn.w <- file(outfile, open = "a", encoding = "UTF-8")
		OUT <- FALSE
		tryCatch(
				{  
					while(nlines == blocklines) {
						tmp.lines <- readLines(conn.r, n = blocklines, encoding = tmp.enc)
						nlines <- length(tmp.lines)
						if (nlines > 0) {
							tmp.lines <- iconv(tmp.lines, tmp.enc, "UTF-8")
							out.lines <- segmentCN(tmp.lines, analyzer = analyzer, 
									nature = FALSE, nosymbol = TRUE, returnType = "tm")
							writeLines(out.lines, conn.w)
						}
					}
					OUT <- TRUE
					cat(paste("Output file: ", outfile, "\n"))
				},
				finally = {
					try(close(conn.r), silent = TRUE)
					try(close(conn.w), silent = TRUE)
					if (.Platform$OS.type == "windows"){
						Sys.setlocale(category = "LC_CTYPE", locale = old.locale)
					}
				}
		)
	} else {
		returnType <- match.arg(returnType)
		if (nosymbol) strwords <- gsub("[^\u4e00-\u9fa5a-zA-Z0-9]", " ", strwords)
		strwords <- gsub("^\\s+|\\s+$", "", gsub("\\s+", " ", strwords))
		
		if (isfast) {
			OUT <- as.character(sapply(strwords, FUN = function(X) .jcall(analyzer, "S", "segWord", X)))
			Encoding(OUT) <- "UTF-8"
		} else {
			strfunc <- ifelse(nature, "segWordNatureInd", "segWordInd")
			strout <- as.character(sapply(strwords, FUN = function(X) .jcall(analyzer, "S", strfunc, X)))
			listnature <- rep(list(NULL), length(strwords))
			listout <- strsplit(strout, split = " ")
			if (nature) {
				listnature <- lapply(listout, FUN = function(X) 
							sapply(strsplit(X, split = "|", fixed = TRUE), FUN = function(Y) Y[2]))
				listout <- lapply(listout, FUN = function(X) 
							sapply(strsplit(X, split = "|", fixed = TRUE), FUN = function(Y) Y[1]))
			}
			listwords <- strsplit(strwords, split = "", fixed = TRUE)
			listsplit <- lapply(listout, FUN = function(X) rep(seq_along(X), times = as.numeric(X)))
			OUT <- lapply(seq_along(listwords), FUN = function(X) {
						RES <- sapply(split(listwords[[X]], f = listsplit[[X]]), paste, collapse = "");
						names(RES) <- listnature[[X]];
						RES <- RES[nzchar(gsub("^\\s+|\\s+$", "", RES))];
						if(length(RES)==0) RES <- "";
						RES
					}
			)	
			if (returnType == "tm") OUT <- sapply(OUT, paste, collapse = " ")
			if (length(OUT) == 1) OUT <- OUT[[1]]
		}
	}
	return(OUT)
}


