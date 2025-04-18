% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ADME_outputfuncs.R
\name{plottingfunc_all}
\alias{plottingfunc_all}
\title{Generate the ggplot2 plotting object for the multi-curve plot}
\usage{
plottingfunc_all(sol_array)
}
\arguments{
\item{sol_array}{The concentration-time profile solution array where entries
signify the concentration of a compound at a specific time in a particular
compartment. Pages of the array correspond to compounds, rows to time points,
and columns to model compartments and outputs.}
}
\value{
A list containing two elements, the first one being a plot
list with each element of the list representing a different compound (no plot
legends) and the second element being a data frame of hexadecimal RGB
triplet colors that defines the plotting color for each compound
}
\description{
This function creates plots (number of plots = number of model compartments
outputted from httk's solve_model function) that display the concentration-
time profile curves for all simulated compounds within each model compartment.
Plots are created using ggplot from the ggplot2 R package. A list of plots are
returned (one plot per compartment plus a plot displaying the plot legend).
}
\seealso{
\code{\link[=Create_Plotting_df]{Create_Plotting_df()}} and \code{\link[=Set_Plot_Legend_Color]{Set_Plot_Legend_Color()}}, which the
current function calls. \code{\link[=ADME_MultPlt_server]{ADME_MultPlt_server()}} and \link{ADME_IndPlt_server}, which
call the current function.
}
