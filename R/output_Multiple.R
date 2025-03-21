
################################################################################
# This file contains functions that is used for outputs of
# multiple modules within ToCS
################################################################################

################################################################################
################################################################################

#' Compile a data frame of compound names to simulate (both compounds uploaded
#' by the user and any preloaded httk compounds selected)
#'
#' @description
#' This function compiles the list of chemical names that the user wants to simulate.
#' It combines both any preloaded compounds selected by the user and any uploaded
#' compounds that a user uploaded in a CSV file. Compounds are then arranged in
#' ascending order by name.
#'
#'
#' @param preload_comp A vector of selected compounds from the preloaded
#' compounds drop down list
#' @param uploaded_comps A CSV file uploaded by the user with compounds and
#' their physical-chemical data, where the compounds are not already available
#' in the preloaded compounds drop down list
#'
#' @return A data frame with the names of all compounds the user wants to simulate
#' @seealso [CompileCompLst()], which calls the current function
#' @export
#'
CompoundList <- function(preload_comp, uploaded_comps){

  #-----------------------------------
  #--- LOAD ANY PRELOADED COMPOUNDS
  #-----------------------------------

    compounds_preload <- sub("^.*?, ", "", preload_comp)

  #---------------------------------------------
  #--- DETERMINE FULL LIST OF COMPOUNDS TO RUN
  #---------------------------------------------

  # --- Set data frame with all desired compounds (preloaded and/or uploaded)
  if (is.null(uploaded_comps)){

    # Set preloaded data as a data frame
    df_compounds <- data.frame(Selected_Compounds = compounds_preload)
  }
  else{

    # Load in uploaded compounds
    compounds_file <- read.csv(uploaded_comps$datapath)

    # Set as data frames to combine rows later
    preload_df <- data.frame(Selected_Compounds = compounds_preload)
    upload_df <- data.frame(Selected_Compounds = compounds_file[,1])

    # Add uploaded compound data to httk data frame of compound data
    n <- nrow(upload_df)

    for (i in 1:n) {
      compounds_file[i,3] <- httk::CAS.checksum(compounds_file[i,2])
      if (compounds_file[i,2] %in% chem.physical_and_invitro.data$CAS){
        index <- which(chem.physical_and_invitro.data$CAS == compounds_file[i,2])
        chem.physical_and_invitro.data[index,] <- compounds_file[i,]
      }
      else{
        chem.physical_and_invitro.data <- rbind(chem.physical_and_invitro.data, compounds_file[i,])
      }
      # Assigns to the user's global environment - do NOT think this will work if app is on the web
      assign('chem.physical_and_invitro.data',chem.physical_and_invitro.data,envir=.GlobalEnv)
    }

    # Add uploaded compound names to any preloaded compounds
    df_compounds <- rbind(preload_df,upload_df)
  }

  # Arrange compounds alphabetically to ensure correct plotting order
  df_compounds <- dplyr::arrange(df_compounds, Selected_Compounds)

  return(df_compounds)
}


################################################################################
################################################################################

#' Compile a final list of all parameters
#'
#' @description
#' This function creates a final list of all user-selected parameters to use in
#' simulation. It adjusts any parameters from the user selection to a function-ready
#' input for all simulation modules.
#'
#' @param pars A list of parameter values encompassing all user inputs for all modules
#'
#' @return A list of parameters to pass into modules
#' @seealso [GatherInputVars()], which calls the current function, and [DosingPar()],
#' [InitVals_Par()], and [OutputTimes_Par()], which the current function calls
#' @export
#'
UpdatePars <- function(pars){

  #---------------------------
  #----- GENERAL PARAMETERS
  #---------------------------

  if (pars[["defaulttoHuman"]] == "Yes"){
    pars[["defaulttoHuman"]] <- TRUE
  }
  else if (pars[["defaulttoHuman"]] == "No"){
    pars[["defaulttoHuman"]] <- FALSE
  }

  #------------------------------------
  #----- COMPOUND SELECTION PARAMETERS
  #------------------------------------

  if (pars[["HondaIVIVE"]] == "NULL"){
    pars["HondaIVIVE"] <- list(NULL)
  }

  #------------------------------------
  #----- MODEL SPECIFIC PARAMETERS
  #------------------------------------

  # --- DOSING PARAMETERS
  pars[["dosinginfo"]] <- DosingPar(pars[["dosenum"]],
                                    pars[["initdose"]],
                                    pars[["multdose"]],
                                    pars[["mult_dosetime"]],
                                    pars[["mult_doseamount"]],
                                    pars[["multdose_odd"]])

  # --- RETURN IVIVE SAMPLES
  if (pars[["returnsamples"]] == "Only return a specified dose quantile (default)"){
    pars[["returnsamples"]] <- FALSE
  }
  else if (pars[["returnsamples"]] == "Return all OED samples (will also return the 5th dose quantile)"){
    pars[["returnsamples"]] <- TRUE
  }

  #------------------------------------
  #----- MODEL CONDITION PARAMETERS
  #------------------------------------

  #--- INITIAL CONDITIONS
  pars[["initvals"]] <- InitVals_Par(pars[["model"]],pars[["init_cond_opts"]],pars)

  # --- BIOACTIVE CONCENTRATION AMOUNT
  if (pars[["bioactiveIVIVE"]] == "Total chemical concentration (default)"){
    pars[["bioactiveIVIVE"]] <- FALSE
  }
  else{
    pars[["bioactiveIVIVE"]] <- TRUE
  }

  # --- RBLOOD2PLASMA
  if (pars[["rb2p"]] == "Recalculate"){
    pars[["rb2p"]] <- TRUE
  }
  else{
    pars[["rb2p"]] <- FALSE
  }

  # --- RESTRICTIVE CLEARANCE
  if (pars[["restrict_clear"]] == "Yes, include protein binding (default)"){
    pars[["restrict_clear"]] <- TRUE
  }
  else{
    pars[["restrict_clear"]] <- FALSE
  }

  # --- FRACTION UNBOUND IN PLASMA
  if (pars[["adj_fub"]] == "Yes, adjust the fraction of unbound plasma (default)"){
    pars[["adj_fub"]] <- TRUE
  }
  else{
    pars[["adj_fub"]] <- FALSE
  }

  # --- REGRESSION
  if (pars[["regression"]] == "Use regressions (default)"){
    pars[["regression"]] <- TRUE
  }
  else{
    pars[["regression"]] <- FALSE
  }

  #------------------------------------
  #----- MODEL SOLVER PARAMETERS
  #------------------------------------

  # --- RELATIVE TOLERANCE
  pars[["rtol"]] <- 10^pars[["rtol"]]

  # --- ABSOLUTE TOLERANCE
  pars[["atol"]] <- 10^pars[["atol"]]

  #------------------------------------
  #----- BIOAVAILABILITY PARAMETERS
  #------------------------------------

  # --- Caco Fabs
  if (pars[["caco_fabs"]] == "Use the Caco2.Pab value selected above (default)"){
    pars[["caco_fabs"]] <- TRUE
  }
  else{
    pars[["caco_fabs"]] <- FALSE
  }

  # --- Caco Fgut
  if (pars[["caco_fgut"]] == "Use the Caco2.Pab value selected above (default)"){
    pars[["caco_fgut"]] <- TRUE
  }
  else{
    pars[["caco_fgut"]] <- FALSE
  }

  # --- Caco overwrite in vivo
  if (pars[["caco_overwriteinvivo"]] == "Do not overwrite in vivo values (default)"){
    pars[["caco_overwriteinvivo"]] <- FALSE
  }
  else{
    pars[["caco_overwriteinvivo"]] <- TRUE
  }

  # --- Caco keepit100
  if (pars[["caco_keep100"]] == "Do not keep Fabs and Fgut at 100% availability (default)"){
    pars[["caco_keep100"]] <- FALSE
  }
  else{
    pars[["caco_keep100"]] <- TRUE
  }

  #------------------------------------
  #----- OUTPUT SPECIFIC PARAMETERS
  #------------------------------------

  # --- OUTPUT TIMES
  pars[["returntimes"]] <- OutputTimes_Par(pars)

  # --- SS TISSUE OUTPUT
  if (pars[["tissueSS"]] == "NULL"){
    pars[["tissueSS"]] <- NULL
  }

  # --- IVIVE TISSUE OUTPUT
  if (pars[["tissueIVIVE"]] == "NULL"){
    pars[["tissueIVIVE"]] <- NULL
  }

  out <- pars
}


################################################################################
################################################################################

#' Create a vector of initial conditions
#'
#' @description
#' This function creates a named vector of initial conditions for the selected
#' model in concentration-time profile simulations. The names correspond to model
#' compartments.
#'
#'
#' @param model The model the user selects
#' @param ICopts The user's selection on whether they want to use initial
#' conditions other than the default, which is zero for all model states
#' @param pars A list of parameters to pass into modules
#'
#' @return A named vector of initial conditions for a specific model
#' @seealso [UpdatePars()], which calls the current function
#' @export
#'
InitVals_Par <- function(model,ICopts,pars){

  if (model == "1compartment") {

    CompNames <- c("Agutlumen","Acompartment","Ametabolized","AUC")
    if(ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(unlist(pars[24:27]), CompNames)}
    else{
      InitVals <- stats::setNames(rep(0,4), CompNames)}
  }
  else if (model == "3compartmentss" || model == "None"){
    InitVals <- NULL
  }
  else if (model == "3compartment"){

    CompNames <- c("Aintestine","Aportven","Aliver","Asyscomp","Ametabolized",
                   "Atubules","AUC")
    if (ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(unlist(pars[28:34]), CompNames)}
    else{
      InitVals <- stats::setNames(rep(0,7), CompNames)}
  }
  else if (model == "pbtk"){

    CompNames <- c("Agutlumen","Agut","Aliver","Aven","Alung","Aart","Arest",
                   "Akidney","Atubules","Ametabolized","AUC")
    if (ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(unlist(pars[35:45]), CompNames)}
    else {
      InitVals <- stats::setNames(rep(0,11), CompNames)}
  }
  else if (model == "fetal_pbtk"){

    CompNames <- c("Agutlumen", "Agut", "Aliver", "Aven", "Alung", "Aart",
                   "Aadipose", "Arest", "Akidney", "Atubules", "Ametabolized",
                   "AUC", "fAUC", "Athyroid","Aplacenta", "Afgut", "Aflung",
                   "Afliver", "Afven", "Afart", "Afrest", "Afthyroid",
                   "Afkidney", "Afbrain")
    if (ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(unlist(pars[46:68]), CompNames)}
    else{
      InitVals <- stats::setNames(rep(0,24), CompNames)}
  }
}


################################################################################
################################################################################

#' Create a list of dosing parameters
#'
#' @description
#' This function creates a list of dosing parameters based on user selections so
#' that the outputted list is in the appropriate form to pass through httk's
#' solve_model function.
#'
#'
#' @param dosenum The user's selection of the number of doses to administer,
#' either "Single dose" or "Multiple doses"
#' @param initdose A positive number; The user's input of the amount of initial
#' dose to administer
#' @param multdose The user's selection of the kind of multiple dosing to
#' simulate, either "Yes" or "No"
#' @param multdosetime A positive number; The user's slider selection of how
#' often to administer reoccurring doses
#' @param multdoseamount A positive number; The user's selection of the amount
#' of dose to give during evenly spaced intervals
#' @param multdoseodd A list of numbers; The user's text input of a list of times
#' to administer dose and the dose to administer at each time
#'
#' @return A list of dosing information taken in by httk's solve_model function.
#' The list has five entries: initial.dose, doses.per.day, daily.dose,
#' dosing.matrix, and forcings.
#' @seealso [UpdatePars()], which calls the current function
#' @export
#'
DosingPar <- function(dosenum,initdose,multdose,multdosetime,multdoseamount,multdoseodd){


  if (dosenum == "Single Dose"){
    dosinginfo <- list(initial.dose = initdose,
                                 doses.per.day=NULL,
                                 daily.dose=NULL,
                                 dosing.matrix=NULL,
                                 forcings = NULL)}
  else {
    if(multdose == "Yes"){
      dosinginfo <- list(initial.dose = NULL,
                                   doses.per.day = (24/multdosetime),
                                   daily.dose = multdoseamount*(24/multdosetime),
                                   dosing.matrix=NULL,
                                   forcings = NULL)}
    else if (multdose == "No"){
      dosinginfo <- list(initial.dose = NULL,
                                   doses.per.day = NULL,
                                   daily.dose = NULL,
                                   dosing.matrix = matrix(as.numeric(unlist(strsplit(multdoseodd,","))),
                                                          ncol = 2,
                                                          dimnames = list(c(),c("time","dose"))),
                                   forcings = NULL)}}
}


################################################################################
################################################################################

#' Generate a vector of output times
#'
#' @description
#' This function creates a vector of output times for the concentration-time profiles
#' module. The vector entries depend on the user's model, simulation time, and
#' return time specifications.
#'
#'
#' @param pars A list of parameters to pass into modules
#'
#' @return A vector of times to output the ADME concentration-time profile solution
#' @seealso [UpdatePars()], which calls the current function
#' @export
#'
OutputTimes_Par <- function(pars){

  # --- The user does not specify output times and the model is not for pregnancy
  if (pars[["returntimes"]] == "" && pars[["model"]] != 'fetal_pbtk'){
    out_times <- seq(0, pars[["simtime"]], signif(1/(96), round(-log10(1e-4)-1))) #output approx every 15 minutes
    out_times <- unique(c(out_times,pars[["simtime"]]))
  }
  # --- The user does not specify output times and the model is for pregnancy
  else if (pars[["returntimes"]] == "" && pars[["model"]] == 'fetal_pbtk'){
    end_time <- min(c(280,91+pars[["simtime"]]))
    out_times <- seq(91,end_time,1) #only runs weeks 13-40 of gestation
  }
  # --- The user specifies output times
  else{
    v1 <- unlist(strsplit(pars[["returntimes"]],","))
    out_times <- sapply(v1, function(x) eval(parse(text = x)))
    if (pars[["model"]] == 'fetal_pbtk'){
      out_times <- unique(sort(c(out_times,min(out_times)+pars[["simtime"]])))
    }
    else{
      out_times <- unique(sort(c(out_times,pars[["simtime"]])))
    }
  }
}


################################################################################
################################################################################

#' Notify the user of parameter error
#'
#' @description
#' This function notifies the user that there is are missing or invalid parameters
#' for the simulation when the "Run Simulation" button is clicked.
#'
#'
#' @return A pop-up notification to the user
#' @seealso [Run_Simulation()], which calls the current function
#' @export
#'
Notify_ParError <- function(){
  shiny::showNotification("Invalid Inputs: Check all previous tabs for missing or invalid parameters. Changing some parameters such as the output, species,
                                  and model will result in other parameters (such as selected compounds) needing to be reselected.", type = "error", duration = NULL)
}

################################################################################
################################################################################


#' Create log10 break points for a plot scale
#'
#' @description
#' This function creates a vector of 10^x breakpoints for the y-axis of a plot
#'
#'
#' @param ydata A vector of y-coordinate data to be plotted
#'
#' @return A vector of y-axis breaks
#' @seealso [plot_logscale()], [IVIVEplot_logscale()] and [BERplotting()], which
#' call the current function
#' @export
#'
log10breaks <- function(ydata) {

  x <- ydata[ydata > 0]

  bottom <- floor(log10(min(x)))
  top <- ceiling(log10(max(x)))
  10^(seq(bottom, top))
}


################################################################################
################################################################################

#' Generate a plot with a log10 scale y-axis
#'
#' @description
#' This function transforms a given plot so that its y-axis scale is a log10 scale
#' instead of a linear scale.
#'
#'
#' @param plt ggplot2 plot object to add the log10 y-axis to
#' @param sol_vec A vector of y-values to be plotted on plt
#'
#' @return A plot that is plt with a log10 y-axis
#' @seealso [scat_plot()], [plotPar()], and [plotPCs()], which calls the current
#' function, and [log10breaks()], which the current function calls
#' @export
#'
plot_logscale <- function(plt,sol_vec){

  break_seq <- log10breaks(sol_vec)

  plt <- plt +
    ggplot2::scale_y_log10(breaks = break_seq,
                           labels = scales::trans_format("log10", scales::math_format(10^.x)),
                           limits = c(min(break_seq),max(break_seq))) +
    ggplot2::annotation_logticks(sides = "l")

  return(plt)
}




