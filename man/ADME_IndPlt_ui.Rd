% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ADMEmod_indivplot.R
\name{ADME_IndPlt_ui}
\alias{ADME_IndPlt_ui}
\title{User interface function for the concentration-time profile individual plots}
\usage{
ADME_IndPlt_ui(id)
}
\arguments{
\item{id}{Shiny identifier name; must be the same id used as in ADME_IndPlt_server()}
}
\value{
User interface for the individual plots drop down with four elements
}
\description{
This function outputs the user interface for the concentration-time profile
individual plots drop down in the results card under the 'Run Simulation' tab.
The interface has four outputs: a plot download button, plotting text (if
applicable), plots, and a plotting caption.
}
\seealso{
\code{\link[=ADME_ui]{ADME_ui()}}, which calls this function
}
