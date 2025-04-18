% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_functions.R
\name{fileInput_UploadedComps}
\alias{fileInput_UploadedComps}
\title{Generate the user upload option to upload compounds and their physical-chemical
data}
\usage{
fileInput_UploadedComps(id)
}
\arguments{
\item{id}{Shiny identifier name}
}
\value{
A browser and file upload box
}
\description{
Generate the user upload option to upload compounds and their physical-chemical
data
}
\examples{
fileInput_UploadedComps("file1")

}
\seealso{
\code{\link[=CS_UploadedData]{CS_UploadedData()}}, which calls the current function
}
