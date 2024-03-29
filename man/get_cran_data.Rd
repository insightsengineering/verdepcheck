% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ref.R
\name{get_cran_data}
\alias{get_cran_data}
\title{Get CRAN/Bioconductor metadata information on packages}
\usage{
get_cran_data(package)
}
\description{
Get CRAN/Bioconductor metadata information on packages
}
\examples{
\dontshow{if (Sys.getenv("R_USER_CACHE_DIR", "") != "") (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
verdepcheck:::get_cran_data("dplyr")
verdepcheck:::get_cran_data("SummarizedExperiment")
\dontshow{\}) # examplesIf}
}
\keyword{internal}
