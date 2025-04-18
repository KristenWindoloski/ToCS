% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PC_outputfuncs.R
\name{Parsol}
\alias{Parsol}
\title{Calculate the elimination rate, volume of distribution, half-life, and total
plasma clearance of all selected compounds}
\usage{
Parsol(pars)
}
\arguments{
\item{pars}{A list of all user input parameters for the entire app}
}
\value{
A list consisting of a data frame with calculated elimination rates,
volumes of distribution, half life, and total clearance measures, a data
frame with calculated partition coefficients, and a data frame with simulation
parameters and chemical-physical data
}
\description{
This function is the main function that calls for the calculation of a list of
compounds' elimination rates, volumes of distribution, half lives, and total
plasma clearances. The data frames are set up in this function, and then other
functions are called to do the calculation for these parameters.
}
\seealso{
\code{\link[=PC_server]{PC_server()}}, which calls the current function, and \code{\link[=CalcElimRate]{CalcElimRate()}},
\code{\link[=CalcVdist]{CalcVdist()}}, \code{\link[=CalcHalfLife]{CalcHalfLife()}}, \code{\link[=CalcClearance]{CalcClearance()}}, \code{\link[=CalcPCs]{CalcPCs()}}, and \code{\link[=StorePars_PC]{StorePars_PC()}},
which the current function calls
}
