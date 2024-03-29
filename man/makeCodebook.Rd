% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeCodebook.R
\name{makeCodebook}
\alias{makeCodebook}
\title{Produce a data codebook}
\usage{
makeCodebook(data, vol = "", reportTitle = NULL, file = NULL, ...)
}
\arguments{
\item{data}{The dataset to be checked. This dataset should be of class \code{data.frame},
\code{tibble} or \code{matrix}. If it is of class \code{matrix}, it will be converted to a
\code{data.frame}.}

\item{vol}{Extra text string or numeric that is appended on the end of the output
file name(s). For example, if the dataset is called "myData", no file argument is
 supplied and \code{vol=2}, the output file will be called "codebook_myData2.Rmd"}

\item{reportTitle}{A text string. If supplied, this will be the printed title of the
report. If left unspecified, the title with the name of the supplied dataset.}

\item{file}{The filename of the outputted rmarkdown (.Rmd) file.
If set to \code{NULL} (the default), the filename will be the name of \code{data}
prefixed with "codebook_", if this qualifies as a valid file name (e.g. no special
characters allowed). Otherwise, \code{makeCodebook()} tries to create a valid filename by
substituting illegal characters. Note that a valid file is of type .Rmd, hence all
filenames should have a ".Rmd"-suffix.}

\item{...}{Additional parameters passed to \code{makeDataReport}.}
}
\description{
Make a data codebook that summarizes the contents of a dataset.
The result is saved to an R markdown file which can be
rendered into an easy-to-read codebook in pdf, html or word formats.
}
\references{
Petersen AH, Ekstrøm CT (2019). “dataMaid: Your Assistant for Documenting Supervised Data Quality Screening in R.” _Journal of Statistical Software_, *90*(6), 1-38. doi: 10.18637/jss.v090.i06 ( \doi{10.18637/jss.v090.i06}).
}
