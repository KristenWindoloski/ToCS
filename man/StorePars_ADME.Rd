% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ADME_outputfuncs.R
\name{StorePars_ADME}
\alias{StorePars_ADME}
\title{Store simulation parameters and physical-chemical data of compounds simulated}
\usage{
StorePars_ADME(pars)
}
\arguments{
\item{pars}{A list of all user input parameters for the entire app}
}
\value{
A data frame with all relevant simulation parameters chosen by the user
and all physical-chemical data used for the simulated compounds
}
\description{
This function collects all user-selected parameters and compound data relevant
to the concentration-time profile simulation and compiles it into one data
frame.
}
\seealso{
\code{\link[=modsol]{modsol()}}, which calls the current function, and \code{\link[=Bind_Chem_Data]{Bind_Chem_Data()}},
which the current function calls
}
