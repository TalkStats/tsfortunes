\name{tsfortune}
\alias{print.tsfortune}
\alias{tsfortune}
\alias{tsfortunes}
\title{R Fortunes}
\usage{
  tsfortune(which = NULL, fortunes.data = NULL,
    fixed = TRUE, ...)
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
  \code{\link{grep}}.

  \code{read.fortunes()} returns a data frame of fortunes,
  each row contains:}
}
\description{
  Read and print R fortunes.
}
\examples{
tsfortune()
}

