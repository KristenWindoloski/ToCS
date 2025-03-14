% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ADME_outputfuncs.R
\name{Set_Individ_Plot}
\alias{Set_Individ_Plot}
\title{Remove current plot legend and generate individual plot legends}
\usage{
Set_Individ_Plot(individ_plt_lst, n_states)
}
\arguments{
\item{individ_plt_lst}{A list of ggplot2 plotting objects where each list element is
the plot of a single compound and is composed of subplots}

\item{n_states}{Number of model compartments/outputs}
}
\value{
A list containing the inputted plot list with the legends on each
compound plot removed and a new subplot with the plotting legend
}
\description{
Remove current plot legend and generate individual plot legends
}
