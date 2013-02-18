\name{read.fortunes}
\alias{print.tsfortune}
\alias{read.fortunes}
\alias{read.tsfortunes}
\alias{toLatex.tsfortune}
\alias{tsfortune}
\alias{tsfortunes}
\title{R Fortunes}
\usage{
  read.fortunes(file = NULL)

  tsfortune(which = NULL, fortunes.data = NULL,
    fixed = TRUE, ...)

  \method{print}{tsfortune} (x, width = NULL, ...)

  \method{toLatex}{tsfortune} (object, number = FALSE,
    width = c(1, 0.85), ...)
}
\arguments{
  \item{which}{An integer specifying the row number of
  \code{fortunes.data}. Alternatively \code{which} can be a
  character and \code{grep} is used to try to find a
  suitable row.}

  \item{fortunes.data}{A data frame containing a fortune in
  each row. By default the fortune data from the tsfortune
  package are used.}

  \item{fixed}{logical passed to \code{\link{grep}} if
  \code{which} is a character, indicating if it should work
  (if \code{TRUE}, as by default) with a simple character
  string or (if \code{FALSE}) with regular expressions.}

  \item{\dots}{potential further arguments passed to
  \code{\link{grep}}.}

  \item{x}{an object of class \code{"fortune"}, usually a
  single row from \code{fortunes.data}.}

  \item{width}{integer specifying the character width.  By
  default \code{getOption("width")} is used.}

  \item{file}{A character string giving a fortune database
  in csv format (in UTF-8 encoding).  By default all csv
  files in the data directory of the fortune package are
  used.

  \code{read.fortunes()} returns a data frame of fortunes,
  each row contains:}
}
\description{
  Read and print R fortunes.
}
\examples{
tsfortune()
}

