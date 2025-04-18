% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_Multiple.R
\name{CompoundList}
\alias{CompoundList}
\title{Compile a data frame of compound names to simulate (both compounds uploaded
by the user and any preloaded httk compounds selected)}
\usage{
CompoundList(preload_comp, uploaded_comps)
}
\arguments{
\item{preload_comp}{A vector of selected compounds from the preloaded
compounds drop down list}

\item{uploaded_comps}{A CSV file uploaded by the user with compounds and
their physical-chemical data, where the compounds are not already available
in the preloaded compounds drop down list}
}
\value{
A data frame with the names of all compounds the user wants to simulate
}
\description{
This function compiles the list of chemical names that the user wants to simulate.
It combines both any preloaded compounds selected by the user and any uploaded
compounds that a user uploaded in a CSV file. Compounds are then arranged in
ascending order by name.
}
\seealso{
\code{\link[=CompileCompLst]{CompileCompLst()}}, which calls the current function
}
