% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smartNum.R
\name{smartNum}
\alias{smartNum}
\title{Smart class to handle numerics as factor}
\usage{
smartNum(v)
}
\arguments{
\item{v}{A numeric vector}
}
\value{
A \code{smartNum} object that is handled in \code{makeDataReport} in the same way as a factor.
}
\description{
S3 class meant for representing numeric variables that act like 
factor variables by taking only a few different values. This class
is used in makeDataReport() in order to get appropriate summaries, visualizations
and checks for such variables. In other words, such variables will be
treated like factor variables instead of numerics.
}
