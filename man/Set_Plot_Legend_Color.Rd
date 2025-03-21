% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ADME_outputfuncs.R
\name{Set_Plot_Legend_Color}
\alias{Set_Plot_Legend_Color}
\title{Remove plot legends and determine plot color}
\usage{
Set_Plot_Legend_Color(plt_lst, n_states)
}
\arguments{
\item{plt_lst}{A list of ggplot2 plotting objects. Each list element is
a ggplot2 plotting object of a single compound and is composed of subplots}

\item{n_states}{Number of model compartments/outputs for the simulated model
from httk's 'solve_model' function.}
}
\value{
A list containing two elements. The first one is the inputted plot
list with the legends on each compound plot removed and a new subplot with the
plotting legend. The second element is a data frame of hexadecimal RGB triplet
colors that defines the plotting color for each compound.
}
\description{
This function extracts the line color legend from a plot, transfers that legend
to a new panel on the plot, and then removes all the other legends from the
plot so there is only one legend on the entire plot.
}
\seealso{
\code{\link[=plottingfunc_all]{plottingfunc_all()}}, which calls this function
}
