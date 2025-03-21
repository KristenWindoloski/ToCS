% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SS_outputfuncs.R
\name{StorePars_SS}
\alias{StorePars_SS}
\title{Store all relevant module parameters and physical-chemical in a data frame
for user download}
\usage{
StorePars_SS(pars)
}
\arguments{
\item{pars}{A list of all user input parameters for the entire app}
}
\value{
A data frame of simulation parameters and chemical-physical data for
the chemicals simulated
}
\description{
This function stores all user-selected simulation parameters for the steady
state concentrations module as well as all physical-chemical data for simulated
compounds into one data frame.
}
\seealso{
\code{\link[=SS_sol]{SS_sol()}}, which calls the current function
}
