% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IVIVE_outputfuncs.R
\name{Calc_OEDBER_RS_False}
\alias{Calc_OEDBER_RS_False}
\title{Calculate the oral equivalent dose and bioactivity exposure ratio when OED
samples are not returned}
\usage{
Calc_OEDBER_RS_False(n, pars, bioactive_conc, exposuredata)
}
\arguments{
\item{n}{The number of compounds being simulated}

\item{pars}{A list of all user input parameters for the entire app}

\item{bioactive_conc}{A data frame with chemical names, their CAS numbers, and
their bioactive concentrations}

\item{exposuredata}{A data frame with chemical names, their CAS numbers, and
an upper, median, and lower exposure estimate}
}
\value{
A list with two elements: a data frame with oral equivalent doses and
a data frame with bioactivity exposure ratios
}
\description{
This function calculates the oral equivalent dose (OED) and bioactivity exposure
(BER) ratio when the user wants a specific quantile of OED returned. The BER
is calculated using the OED quantile selected by the user. If no exposure
data is uploaded by the user, then the BER is not calculated.
}
\seealso{
\code{\link[=CalcOED]{CalcOED()}}, which is called by the current function, and \code{\link[=IVIVEsol]{IVIVEsol()}},
which calls the current function
}
