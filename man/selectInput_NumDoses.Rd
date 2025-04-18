% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_functions.R
\name{selectInput_NumDoses}
\alias{selectInput_NumDoses}
\title{Generate a drop down list that describes the dosing frequency for ADME simulations}
\usage{
selectInput_NumDoses(id)
}
\arguments{
\item{id}{Shiny identifier name}
}
\value{
A drop down list
}
\description{
Generate a drop down list that describes the dosing frequency for ADME simulations
}
\examples{
selectInput_NumDoses("dosenum")

}
\seealso{
\code{\link[=MS_Dosing]{MS_Dosing()}}, which calls the current function
}
