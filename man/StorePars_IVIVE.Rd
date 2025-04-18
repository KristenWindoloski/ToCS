% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IVIVE_outputfuncs.R
\name{StorePars_IVIVE}
\alias{StorePars_IVIVE}
\title{Compile the simulation parameters and physical-chemical data used into one
data frame}
\usage{
StorePars_IVIVE(pars, bioactive_df)
}
\arguments{
\item{pars}{A list of all user input parameters for the entire app}

\item{bioactive_df}{A data frame with the chemical name, CAS number, and
bioactive concentration (in uM units) for each chemical to simulate}
}
\value{
A data frame with simulation parameters and physical-chemical data
for simulated compounds
}
\description{
This function combines the used simulation parameters and chemical-physical
data for simulated compounds into one data frame. The returned parameters are
only those used to generate the simulation, and the chemical-physical data is
retrieved from the 'httk' package's 'chem.physical_and_invitro.data' data frame.
}
\seealso{
\code{\link[=IVIVEsol]{IVIVEsol()}}, which calls the current function
}
