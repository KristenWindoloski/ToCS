% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ADMEmod_multiplot.R
\name{ADME_MultPlt_server}
\alias{ADME_MultPlt_server}
\title{Server function for the concentration-time profile multi-plot output}
\usage{
ADME_MultPlt_server(id, adme_args)
}
\arguments{
\item{id}{Shiny identifier name; must be the same id used as in ADME_MultPlt_ui()}

\item{adme_args}{A Shiny reactive list with the output of modsol() and all shiny
parameters in pars()}
}
\value{
Server outputs for the concentration-time profile multi-plot drop down
which includes three elements
}
\description{
This function generates the outputs defined in the ADME_MultPlt_ui()
function. This connects the download button, plot, and plot caption to the
elements that fill their spaces.
}
\seealso{
\code{\link[=plottingfunc_all]{plottingfunc_all()}} and \code{\link[=caption_text]{caption_text()}}, which are called by the
current function, and \code{\link[=ADME_server]{ADME_server()}}, which calls this function
}
