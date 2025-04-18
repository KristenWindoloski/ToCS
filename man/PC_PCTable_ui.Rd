% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCmod_PartitionCoefficientsTable.R
\name{PC_PCTable_ui}
\alias{PC_PCTable_ui}
\title{User interface function for the parameter calculations partition coefficients
table}
\usage{
PC_PCTable_ui(id)
}
\arguments{
\item{id}{Shiny identifier name; must be the same id used as in PC_PCTable_server()}
}
\value{
User interface for the partition coefficients table drop down with
three elements
}
\description{
This function contains the user interface elements for the parameter calculations
partition coefficient table. This interface itself contains three UI elements:
a table download button, a table, and a table caption.
}
\seealso{
\code{\link[=PC_ui]{PC_ui()}}, which calls the current function
}
