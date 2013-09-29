tsfortunes
==========

The `tsfortunes` package provides some fortunes that come from the [TalkStats community](www.talkstats.com)

## Contributing

If you would like to submit a proposal for a fortune you can do one of two things

* Fork the repository, add the fortune to inst/fortunes/fortunes.csv (which is actually semicolon delimited), and submit a pull request OR
* Add your suggestion for a fortune as an issue: https://github.com/TalkStats/tsfortunes/issues

Either way you'll get feedback on whether the suggestion is appropriate or if we need more information (author, date, ...) before it is incorporated into the database.
    
## Installation

Currently there isn't a release on [CRAN](http://cran.r-project.org/).

You can, however, download the [zip ball](https://github.com/TalkStats/tsfortunes/zipball/master) or [tar ball](https://github.com/TalkStats/tsfortunes/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
## Make sure your current packages are up to date
update.packages()
## devtools is required
library(devtools)
install_github("tsfortunes", "TalkStats")
```
