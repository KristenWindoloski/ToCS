% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCmod_Pars.R
\name{PC_Pars_server}
\alias{PC_Pars_server}
\title{Server function for the parameter calculations simulation parameters}
\usage{
PC_Pars_server(id, pc_args)
}
\arguments{
\item{id}{Shiny identifier name; must be the same id used as in PC_Pars_ui()}

\item{pc_args}{A Shiny reactive list with the output of Parsol(), all shiny
parameters in pars(), and the logscale() input by the user}
}
\value{
Server outputs for the parameter download drop down with one element
}
\description{
Server function for the parameter calculations simulation parameters
}
\seealso{
\code{\link[=PC_server]{PC_server()}}, which calls the current function
}
