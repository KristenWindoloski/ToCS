% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_functions.R
\name{names_ICs}
\alias{names_ICs}
\title{Generate a vector of model compartment variables and names}
\usage{
names_ICs()
}
\value{
A list of full compartment names and variable names of compartments
}
\description{
This function creates a list of compartment names and variable names of the
compartments in the model to be used to generate initial condition options
to the user during concentration-time profile simulations; used in the global.R \
file.
}
\examples{
names_ICs()

}
\seealso{
\code{\link[=validate_text_ADME]{validate_text_ADME()}}, which calls the current function
}
