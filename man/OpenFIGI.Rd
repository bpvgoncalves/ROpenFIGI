% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OpenFIGI_Func.R
\name{OpenFIGI}
\alias{OpenFIGI}
\title{OpenFIGI main function}
\usage{
OpenFIGI(
  input,
  apikey = NULL,
  openfigiurl = "https://api.openfigi.com/v1/mapping",
  preferdf = FALSE,
  proxy = NULL
)
}
\arguments{
\item{input}{input data.frame or json, and others that can be feed into toJSON}

\item{apikey}{your API key}

\item{openfigiurl}{Bloomberg's OpenFIGI URL, please see https://openfigi.com/api}

\item{preferdf}{if only supply 1 input, do you prefer to see the data.frame directly}

\item{proxy}{if needed, web proxy information can be passed to the function.
Accepts: NULL (default) = No proxy,   "auto" = Tries to auto detect a suitable proxy or an
object created by httr::use_proxy().}
}
\value{
a list of data.frame, of a data.frame if preferdf=T and only 1 request
}
\description{
OpenFIGI main function
}
\examples{
\dontrun{
  figirst = OpenFIGI(sampleOpenFIGIdf())
}
}
