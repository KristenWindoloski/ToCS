
####################################################################
# This file contains functions that is used for outputs of
# multiple modules within the R Shiny app
####################################################################

###############################################
#--- COMPILE LIST OF ALL COMPOUNDS TO ANALYZE
###############################################

CompoundList <- function(preload_comp, preload_comp_Honda1, uploaded_comps){

  #-----------------------------------
  #--- PRELOADED COMPOUNDS SELECTED?
  #-----------------------------------

  if (!is.null(preload_comp)){

    preload_comp_name <- sub("^.*?, ", "", preload_comp)
    compounds_preload <- preload_comp_name
  }
  else{
    preload_comp_name <- sub("^.*?, ", "", preload_comp_Honda1)
    compounds_preload <- preload_comp_name
  }

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
    file <- uploaded_comps
    compounds_file <- read.csv(file$datapath)

    # Set as data frames to combine rows later
    preload_df <- data.frame(Selected_Compounds = compounds_preload)
    upload_df <- data.frame(Selected_Compounds = compounds_file[,1])

    # Add uploaded compound data to httk data frame of compound data
    n <- nrow(upload_df)

    for (i in 1:n) {
      compounds_file[i,3] <- httk::CAS.checksum(compounds_file[i,2])
    }

    # Assigns to the user's global environment - do NOT think this will work if app is on the web
    chem.physical_and_invitro.data <- rbind(httk::chem.physical_and_invitro.data, compounds_file)
    assign('chem.physical_and_invitro.data',chem.physical_and_invitro.data,envir=.GlobalEnv)

    # Add uploaded compound names to any preloaded compounds
    df_compounds <- rbind(preload_df,upload_df)
  }

  # Arrange compounds alphabetically to ensure correct plotting order
  df_compounds <- dplyr::arrange(df_compounds, Selected_Compounds)

  return(df_compounds)
}


################################################################
#--- COMPILE A LIST OF ALL PARAMETERS
#--- (ONLY UPDATE PARAMETERS WHOSE VALUE NEEDS TO BE CHANGED)
################################################################


UpdatePars <- function(pars){

  #---------------------------
  #----- GENERAL PARAMETERS
  #---------------------------

  if (pars[["defaulttoHuman"]] == "Yes, use human values"){
    pars[["defaulttoHuman"]] <- TRUE
  }
  else{
    pars[["defaulttoHuman"]] <- FALSE
  }

  #------------------------------------
  #----- COMPOUND SELECTION PARAMETERS
  #------------------------------------

  if (pars[["HondaIVIVE"]] == "NULL"){
    pars[["HondaIVIVE"]] <- NULL
  }

  #------------------------------------
  #----- MODEL SPECIFIC PARAMETERS
  #------------------------------------

  # --- DOSING PARAMETERS
  pars[["dosinginfo"]] <- DosingPar(pars[["dosenum"]],
                                    pars[["initdose"]],
                                    pars[["multdose"]],
                                    pars[["multdosetime"]],
                                    pars[["multdoseamount"]],
                                    pars[["multdoseodd"]])

  # --- RETURN IVIVE SAMPLES
  if (pars[["returnsamples"]] == "Only return a specified dose quantile (default)"){
    pars[["returnsamples"]] <- FALSE
  }
  else{
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
  if (pars[["returntimes"]] == "" && pars[["model"]] != 'fetal_pbtk'){
    out_times <- seq(0, pars[["simtime"]], signif(1/(96), round(-log10(1e-4)-1))) #output approx every 15 minutes
  }
  else if (pars[["returntimes"]] == "" && pars[["model"]] == 'fetal_pbtk'){
    out_times <- seq(13*7, 40*7, signif(1/(96), round(-log10(1e-4)-1))) #only runs weeks 13-40 of gestation
  }
  else{
    out_times <- as.numeric(unlist(strsplit(pars[["returntimes"]],",")))
  }
  pars[["returntimes"]] <- unique(c(out_times,pars[["simtime"]]))

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

InitVals_Par <- function(model,ICopts,pars){

  print(pars)
  if (model == "1compartment") {

    CompNames <- c("Agutlumen","Acompartment","Ametabolized","AUC")
    if(ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(c(pars[24:27]), CompNames)}
    else{
      InitVals <- stats::setNames(rep(0,4), CompNames)}
  }
  else if (model == "3compartmentss" || model == "None"){
    InitVals <- NULL
  }
  else if (model == "3compartment"){

    CompNames <- c("Aintestine","Aportven","Aliver","Asyscomp","Ametabolized","Atubules","AUC")
    if (ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(c(pars[28:34]), CompNames)}
    else{
      InitVals <- stats::setNames(rep(0,7), CompNames)}
  }
  else if (model == "pbtk"){

    CompNames <- c("Agutlumen","Agut","Aliver","Aven","Alung","Aart","Arest","Akidney","Atubules","Ametabolized","AUC")
    if (ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(c(pars[35:45]), CompNames)}
    else {
      InitVals <- stats::setNames(rep(0,11), CompNames)}
  }
  else if (model == "fetal_pbtk"){

    CompNames <- c("Agutlumen", "Agut", "Aliver", "Aven", "Alung", "Aart", "Aadipose", "Arest", "Akidney", "Atubules", "Ametabolized", "AUC", "fAUC", "Athyroid",
                   "Aplacenta", "Afgut", "Aflung", "Afliver", "Afven", "Afart", "Afrest", "Afthyroid", "Afkidney", "Afbrain")
    if (ICopts == "Yes, enter my own initial amounts"){
      InitVals <- stats::setNames(c(pars[46:68]), CompNames)}
    else{
      InitVals <- stats::setNames(rep(0,24), CompNames)}
  }
}

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

