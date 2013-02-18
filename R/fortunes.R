#' Read the fortunes file
#' 
#' Read the fortunes file
#' 
#' @param file A character string giving a fortune database in csv format (in
#' UTF-8 encoding).  By default all csv files in the data directory of the
#' fortune package are used.
#' @export
read.fortunes <- function(file = NULL)
{
    if(!is.null(file)) {
        
        fortunes <- file[file.exists(file)]
    } else {
        path <- system.file("fortunes", package = "tsfortunes")
        datafiles <- list.files(path)
        if(!is.null(file) && file.exists(file.path(path, file))) {
            fortunes <- file.path(path, file)
        } else {
            if(length(file) > 0L) stop("sorry, ", sQuote(file), " not found")
            file <- datafiles[grep("\\.csv$", datafiles)]
            if(length(file) == 0L) stop("sorry, no fortunes data found")
            fortunes <- file.path(path, file)
        }
    }
    
    rval <- NULL
    for(file in fortunes) {
        rval <- rbind(rval, read.table(file, header = TRUE, sep = ";",
                                       quote = "\"", colClasses = "character"))
    }
    
    return(rval)
}

fortunes.env <- new.env()



#'R Fortunes
#'
#'Read and print R fortunes.
#'
#'
#'@aliases tsfortunes tsfortune print.tsfortune
#'@param which An integer specifying the row number of \code{fortunes.data}.
#'Alternatively \code{which} can be a character and \code{grep} is used to try
#'to find a suitable row.
#'@param fortunes.data A data frame containing a fortune in each row. By
#'default the fortune data from the tsfortune package are used.
#'@param fixed logical passed to \code{\link{grep}} if \code{which} is a
#'character, indicating if it should work (if \code{TRUE}, as by default) with
#'a simple character string or (if \code{FALSE}) with regular expressions.
#'@param \dots potential further arguments passed to \code{\link{grep}}.
#'
#'\code{read.fortunes()} returns a data frame of fortunes, each row contains:
#'@export
#'@examples
#'
#'tsfortune()
#'
tsfortune <- function(which = NULL, fortunes.data = NULL, fixed = TRUE, ...)
{
    if(is.null(fortunes.data)) {
        if(is.null(fortunes.env$fortunes.data)) fortunes.env$fortunes.data <- read.fortunes()
        fortunes.data <- fortunes.env$fortunes.data
    }
    
    if(is.null(which)) which <- sample(1:nrow(fortunes.data), 1)
    if(is.character(which)) {
        fort <- apply(fortunes.data, 1, function(x) paste(x, collapse = " "))
        which1 <- grep(which, fort, useBytes = TRUE, fixed = fixed, ...)
        if(length(which1) < 1) which1 <- grep(tolower(which), tolower(fort),
                                              useBytes = TRUE, fixed = TRUE)
        which <- which1
        if(length(which) > 1) which <- sample(which)
    }
    if(length(which) > 1) which <- which[1]
    if(length(which) > 0 && which %in% seq(along = rownames(fortunes.data))) {
        rval <- fortunes.data[which, ]
        class(rval) <- "tsfortune"
    } else {
        rval <- character(0)
    }
    return(rval)
}

#'@export
#'@method print tsfortune
print.tsfortune <- function(x, width = NULL, ...)
{
    if(is.null(width)) width <- getOption("width")
    if(width < 10) stop("'width' must be greater than 10")
    
    if(is.na(x$context)) {
        x$context <- ""
    } else {
        x$context <- paste(" (", x$context, ")", sep = "")
    }
    if(is.na(x$source)) {
        x$source <- ""
    }
    if(is.na(x$date)) {
        x$date <- ""
    } else {
        x$date <- paste(" (", x$date, ")", sep = "")
    }
    if(any(is.na(x))) stop("'quote' and 'author' are required")
    
    line1 <- x$quote
    line2 <- paste("   -- ", x$author, x$context, sep = "")
    line3 <- paste("      ", x$source, x$date, sep = "")
    
    ## Problem: account for chase where line contains "\n"
    linesplit <- function(line, width, gap = "      ") {
        if(nchar(line) < width) return(line)
        rval <- NULL
        while(nchar(line) > width) {
            line <- strsplit(line, " ")[[1]]
            if(any((nchar(line) + 1 + nchar(gap)) > width))
                stop("'width' is too small for fortune")
            breakat <- which(cumsum(nchar(line) + 1) > width)[1] - 1
            rval <- paste(rval, paste(line[1:breakat], collapse = " "), "\n", sep = "")
            line <- paste(gap, paste(line[-(1:breakat)], collapse = " "), sep = "")
        }
        rval <- paste(rval, line, sep = "")
        return(rval)
    }
    
    line1 <- strsplit(line1, "<x>")[[1]]
    for(i in 1:length(line1))
        line1[i] <- linesplit(line1[i], width, gap = "")
    line1 <- paste(line1, collapse = "\n")
    line2 <- linesplit(line2, width)
    line3 <- linesplit(line3, width)
    
    cat(paste("\n", line1, "\n",
              line2, "\n",
              line3, "\n\n", sep = ""))
}

#' Convert fortunes to Latex
#' 
#' Convert fortunes to Latex
#' 
#' @param object Object to convert
#' @param number Logical. Do you want the fortune number printed?
#' @param width integer specifying the character width.  By default
#' \code{getOption("width")} is used.
#' @param ... Additional arguments.  Does nothing.
#' @importFrom utils toLatex
#' @export
#' @method toLatex tsfortune
toLatex.tsfortune <- function(object, number = FALSE, width = c(1, 0.85), ...) {
    width <- rep(width, length.out = 2)
    escape_latex <- function(x) {
        x <- gsub("\\\\ ", "\\textbackslash\\ ", x, fixed=TRUE)
        x <- gsub("\\\\", "\\textbackslash ", x, fixed=TRUE)
        x <- gsub("\\n", "\\textbackslash n", x, fixed=TRUE)
        x <- gsub("#", "\\#", x, fixed=TRUE)
        x <- gsub("$", "\\$", x, fixed=TRUE)
        x <- gsub("&", "\\&", x, fixed=TRUE)
        x <- gsub("~ ", "\\textasciitilde\\ ", x, fixed=TRUE)
        x <- gsub("~", "\\textasciitilde ", x, fixed=TRUE)
        x <- gsub("_", "\\_", x, fixed=TRUE)
        x <- gsub("^", "\\verb|^|", x, fixed=TRUE)
        x <- gsub("%", "\\%", x, fixed=TRUE)
        x <- gsub("{", "\\{", x, fixed=TRUE)
        x <- gsub("}", "\\}", x, fixed=TRUE)
        x <- gsub(" '", " `", x, fixed=TRUE)
        x <- gsub(" \"", " ``", x, fixed=TRUE)
        x <- gsub("...", "{\\dots}", x, fixed=TRUE)
        x <- gsub(" - ", " -- ", x, fixed=TRUE)
        x
    }
    if(is.na(object$context)) {
        object$context <- ""
    }
    if(is.na(object$source)) {
        object$source <- ""
    }
    if(is.na(object$date)) {
        object$date <- ""
    } else {
        object$date <- paste(" (", object$date, ")", sep = "")
    }
    if(any(is.na(object))) stop("'quote' and 'author' are required")
    quote <- strsplit(object$quote,"<x>")[[1]]
    quote <- c(rbind(t(quote), t(rep("",length(quote)))))
    z <- paste("\\begin{minipage}{", width[1], "\\textwidth}", sep = "")
    z <- c(z, paste(
        if(number) paste("\\makebox[0pt][r]{\\tiny ", attr(object, "row.names"), "} ", sep = "") else "",
        escape_latex(quote[1]), sep="")
    )
    z <- c(z, escape_latex(quote[-1]))
    z <- c(z, paste("\\hfill---\\parbox[t]{", width[2], "\\textwidth}{\\emph{",
                    escape_latex(object$author), "}", sep = ""),
           if(object$context == "") "" else paste("(", escape_latex(object$context), ")", sep = ""),
           "",
           paste(escape_latex(object$source), escape_latex(object$date), "}", sep=""),
           "\\end{minipage}")
    class(z) <- "Latex"
    z
}

