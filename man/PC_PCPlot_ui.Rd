% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCmod_PartitionCoefficientsPlot.R
\name{PC_PCPlot_ui}
\alias{PC_PCPlot_ui}
\title{User interface function for the parameter calculations partition coefficient
plot}
\usage{
PC_PCPlot_ui(id)
}
\arguments{
\item{id}{Shiny identifier name; must be the same id used as in PC_PCPlot_server()}
}
\value{
User interface for the partition coefficient plot drop down with three
elements
}
\description{
This function contains the user interface elements for the parameter calculations
partition coefficient plot. This interface itself contains three UI elements: a
download button, a plot, and a plot caption.
}
\seealso{
\code{\link[=PC_ui]{PC_ui()}}, which calls the current function
}
