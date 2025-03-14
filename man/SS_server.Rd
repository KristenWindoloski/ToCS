% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SSmod_main.R
\name{SS_server}
\alias{SS_server}
\title{Server output for the steady state concentrations module and is composed of
three submodules}
\usage{
SS_server(id, pars, runsim, logscale)
}
\arguments{
\item{id}{Shiny identifier name; must match the identifier in the SS_ui()}

\item{pars}{A list of all user input parameters for the entire app}

\item{runsim}{Action button titled 'Run Simulation' pressed by the user}

\item{logscale}{Checkbox input value indicating if the user wanted the y-axis
of plots to be a log10 scale}
}
\value{
Server output for the steady state concentrations module on the 'Run Simulation'
tab; includes one plot and two table drop down panels
}
\description{
Server output for the steady state concentrations module and is composed of
three submodules
}
