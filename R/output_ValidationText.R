
################################################################################
################################################################################

#' Create the text for a figure/table caption
#'
#' @description
#' This function the correct AUC output concentration to go in a caption in the
#' concentration-time profiles module plot and table. The current function is
#' called by ADME_MultPlt_server() and ADME_IndPlt_server().
#'
#'
#' @param func User selection with the input$func ID; either "Concentration-time
#' profiles", "Steady state concentrations", "In vitro in vivo extrapolation
#' (IVIVE)", and "Parameter calculations"
#' @param model User selection with the input$model ID; either "3compartmentss",
#' "Schmitt","1compartment","3compartment", "pbtk" or "fetal_pbtk"
#'
#' @return A string with text describing the returned concentration of the model.
#' @noRd
#'
caption_text <- function(func,model){

  if (func == "ADME"){

    if (model == 'pbtk' || model == 'fetal_pbtk' || model == "full_pregnancy"){
      AUCoutput <- "venous plasma concentration."
    }
    else if (model == '3compartment') {
      AUCoutput <- "Csyscomp (aggregated remaining tissues - ART) plasma concentration."
    }
    else if (model == '1compartment') {
      AUCoutput <- "Ccompartment (aggregated remaining tissues - ART) plasma concentration."
    }
  }


}


################################################################################
################################################################################

#' Check if any inputs that are common among all four modules are invalid or
#' missing
#'
#' @description
#' This function validates that all common user-selected parameters across all
#' modules are acceptable. If not, the simulator will not compute until any
#' errors in inputs are fixed. The current function is called by CompileCompLst().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A blank error message to the user that halts computation until the
#' user fixes the input error.
#' @noRd
#'
validate_text_Common <- function(pars){

  if (pars[["func"]] == 'Select'){
    shiny::validate(shiny::need(pars[["func"]] != 'Select',message = paste("")))
  }
  if (pars[["spec"]] == 'Select'){
    shiny::validate(shiny::need(pars[["spec"]] != 'Select',message = paste("")))
  }
  if (pars[["defaulttoHuman"]] == 'Select'){
    shiny::validate(shiny::need(pars[["defaulttoHuman"]] != 'Select',message = paste("")))
  }
  if (pars[["model"]] == 'Select'){
    shiny::validate(shiny::need(pars[["model"]] != 'Select',message = paste("")))
  }
  if (pars[["insilicopars"]] == 'Select'){
    shiny::validate(shiny::need(pars[["insilicopars"]] != 'Select',message = paste("")))
  }
  if (is.null(c(pars[["httkPreloadComps"]],pars[["file1"]]))){
    shiny::validate(shiny::need(!is.null(c(pars[["httkPreloadComps"]],pars[["file1"]])),message = paste("")))
  }
  if (!is.null(pars[["file1"]])){

    # --- PROCESS UPLOADED DATA
    file_df <- utils::read.csv(pars[["file1"]]$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN NAMES AND COLUMN ORDER
    file_df_colnames <- colnames(file_df)
    httkdata_colnames <- colnames(httk::chem.physical_and_invitro.data)

    if (!all(file_df_colnames == httkdata_colnames)){
      shiny::validate(shiny::need(all(file_df_colnames == httkdata_colnames),message = paste("")))
    }

    # --- CHECK FOR MISSING REQUIRED DATA NO MATTER THE SIMULATION
    req_pars <- c("Compound","CAS","DTXSID","logP","MW")
    if (anyNA(file_df[,req_pars])){
      shiny::validate(shiny::need(!anyNA(file_df[,req_pars]),message = paste("")))
    }

    # --- CHECK FOR MISSING HONDA1 ASSUMPTION IVIVE REQURIED PARAMETERS, IF APPLICABLE
    if (pars[["func"]] == "In vitro in vivo extrapolation (IVIVE)"){
      if (!is.null(pars[["HondaIVIVE"]])){
        if (pars[["HondaIVIVE"]] == "Honda1"){
          if (anyNA(file_df[,c("logHenry","logWSol","MP")])){
            shiny::validate(shiny::need(!anyNA(file_df[,c("logHenry","logWSol","MP")]),message = paste("")))
          }
        }
      }
    }

    # --- CHECK FOR MISSING CLINT AND FUP VALUES
    # --- HUMAN SPECIES
    if (pars[["spec"]] == "Human"){
      if (anyNA(file_df[,c("Human.Clint","Human.Funbound.plasma")])){
        shiny::validate(shiny::need(!anyNA(file_df[,c("Human.Clint","Human.Funbound.plasma")]),message = paste("")))
      }
    }
    # --- RAT SPECIES AND NO HUMAN
    else if (pars[["spec"]] == "Rat" && pars[["defaulttoHuman"]] == "No"){
      if (anyNA(file_df[,c("Rat.Clint","Rat.Funbound.plasma")])){
        shiny::validate(shiny::need(!anyNA(file_df[,c("Rat.Clint","Rat.Funbound.plasma")]),message = paste("")))
      }
    }
    # --- RAT SPECIES AND HUMAN ALLOWABLE
    else if (pars[["spec"]] == "Rat" && pars[["defaulttoHuman"]] == "Yes"){

      Clint <- file_df[,c("Human.Clint","Rat.Clint")]
      ind_Clint <- which(is.na(Clint),arr.ind = TRUE)

      Fup <- file_df[,c("Human.Funbound.plasma","Rat.Funbound.plasma")]
      ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

      if (any(duplicated(ind_Clint[,1]) == TRUE) || any(duplicated(ind_Fup[,1]) == TRUE)){
        shiny::validate(shiny::need(!any(duplicated(ind_Clint[,1]) == TRUE) && !any(duplicated(ind_Fup[,1]) == TRUE),message = paste("")))
      }
    }
    # --- MOUSE SPECIES AND HUMAN ALLOWABLE
    else if (pars[["spec"]] == "Mouse" && pars[["defaulttoHuman"]] == "Yes"){

      Fup <- file_df[,c("Human.Funbound.plasma","Mouse.Funbound.plasma")]
      ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

      if (anyNA(file_df$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
        shiny::validate(shiny::need(!anyNA(file_df$Human.Clint) && !any(duplicated(ind_Fup[,1]) == TRUE),message = paste("")))
      }
    }
    # --- RABBIT SPECIES AND HUMAN ALLOWABLE
    else if (pars[["spec"]] == "Rabbit" && pars[["defaulttoHuman"]] == "Yes"){

      Fup <- file_df[,c("Human.Funbound.plasma","Rabbit.Funbound.plasma")]
      ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

      if (anyNA(file_df$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
        shiny::validate(shiny::need(!anyNA(file_df$Human.Clint) && !any(duplicated(ind_Fup[,1]) == TRUE),message = paste("")))
      }
    }
    # --- DOG SPECIES AND HUMAN ALLOWABLE
    else if (pars[["spec"]] == "Dog" && pars[["defaulttoHuman"]] == "Yes"){

      if (anyNA(file_df[,c("Human.Clint","Human.Funbound.plasma")])){
        shiny::validate(shiny::need(!anyNA(file_df[,c("Human.Clint","Human.Funbound.plasma")]),message = paste("")))
      }
    }
    # --- NON-HUMAN OR NON-RAT SPECIES THAT NEED HUMAN DATA FOR SIMULATION
    else{
      shiny::validate(shiny::need(pars[["defaulttoHuman"]] == "Yes",message = paste("")))
    }

    # --- CHECK FOR CORRECT INPUT DATA TYPES
    # --- DETERMINE FILE ENTRY DATA TYPES
    file_df_datatypes <- unname(sapply(file_df,class))
    httkdata_datatypes <- unname(sapply(httk::chem.physical_and_invitro.data,class))

    # --- EXTRACT COLUMNS THAT ARE SUPPOSED TO BE ONLY CHARACTERS
    file_df_ref <- magrittr::`%>%`(file_df, dplyr::select(dplyr::contains("reference")))
    df_non_ref <- file_df[,c("Compound","CAS","DTXSID","Formula","All.Compound.Names","All.Species","Chemical.Class")]
    char_only_df <- cbind(df_non_ref,file_df_ref)

    # --- CHECK IF UPLOADED FILE COLUMN DATA TYPES MATCH ALL HTTK COLUMN DATA TYPES
    if (!all(file_df_datatypes == httkdata_datatypes)){

      accept_datatypes <- DataTypeList()
      check_type <- unname(mapply(type_func,accept_datatypes,file_df_datatypes))

      # --- IF TYPES DON'T EXACTLY MATCH, SEE IF TYPE IS ACCEPTABLE AND MAKE SURE
      # --- ANY COLUMNS OF TYPE 'CHARACTER' ARE ACTUALLY ALL CHARACTERS AND NOT NUMERICS
      if (any(check_type == FALSE) || !all(is.na(as.numeric(unlist(char_only_df))))){

        for (i in 1:length(check_type)) {
          if (isFALSE(check_type[i])){
            if (any(is.na(as.numeric(file_df[,i])))){
              shiny::validate(shiny::need(!any(is.na(as.numeric(file_df[,i])))),message = paste(""))
            }
          }
        }
      }
    }
    else if (!all(is.na(as.numeric(unlist(char_only_df))))){
      shiny::validate(shiny::need(!all(is.na(as.numeric(unlist(char_only_df)))),message = paste("")))
    }

  }
}


################################################################################
################################################################################

#' Check if any inputs specific to the concentration-time profile module are
#' invalid or missing
#'
#' @description
#' This function validates that all user-selected parameters across the
#' concentration-time profile module are acceptable. If not, the simulator will
#' not compute until any errors in inputs are fixed. The current function is called
#' by ADME_server() and ADME_TCData_server() and calls names_ICs() and
#' adme_ic_errormess().
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A blank error message to the user that halts computation until the
#' user fixes the input error.
#' @noRd
#'
validate_text_ADME <- function(pars){

  if (pars[["dosenum"]] == 'Select'){
    shiny::validate(shiny::need(pars[["dosenum"]] != 'Select',message = paste("")))
  }
  if (pars[["dosenum"]] == 'Multiple Doses' && pars[["multdose"]] == 'Select' && pars[["model"]] != 'full_pregnancy'){
    shiny::validate(shiny::need(pars[["multdose"]] != 'Select',message = paste("")))
  }
  if (pars[["dosenum"]] == 'Multiple Doses' && pars[["multdose"]] == 'No' && pars[["multdose_odd"]] == ''){
    shiny::validate(shiny::need(pars[["multdose_odd"]] != '',message = paste("")))
  }
  if (pars[["spec"]] != 'Human' && pars[["model"]] == 'fetal_pbtk'){
    shiny::validate(shiny::need(pars[["spec"]] == 'Human',message = paste("")))
  }
  if (pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
    if (length(pars[["returntimes"]])>0){
      mintime <- min(pars[["returntimes"]])
      if (mintime < 91){
        shiny::validate(shiny::need(pars[["returntimes"]][1] >= 13*7,message = paste("")))
      }
    }
  }
  if (is.na(pars[["initdose"]])){
    shiny::validate(shiny::need(!is.na(pars[["initdose"]]),message = paste("")))
  }
  if (is.na(pars[["mult_doseamount"]])){
    shiny::validate(shiny::need(!is.na(pars[["mult_doseamount"]]),message = paste("")))
  }
  if (is.na(pars[["simtime"]])){
    shiny::validate(shiny::need(!is.na(pars[["simtime"]]),message = paste("")))
  }
  if (is.na(pars[["min_fub"]])){
    shiny::validate(shiny::need(!is.na(pars[["min_fub"]]),message = paste("")))
  }
  if (is.na(pars[["solversteps"]])){
    shiny::validate(shiny::need(!is.na(pars[["solversteps"]]),message = paste("")))
  }
  if (is.na(pars[["caco2default"]])){
    shiny::validate(shiny::need(!is.na(pars[["caco2default"]]),message = paste("")))
  }
  purrr::map2(unlist(names_ICs()[[1]]),rep(list(pars),45),adme_ic_errormess)
}


################################################################################
################################################################################

#' Check if any inputs specific to the steady state concentrations module are
#' invalid or missing
#'
#' @description
#' This function validates that all user-selected parameters across the
#' steady state concentrations module are acceptable. If not, the simulator will
#' not compute until any errors in inputs are fixed. The current function is called
#' by SS_server().
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A blank error message to the user that halts computation until the
#' user fixes the input error.
#' @noRd
#'
validate_text_SS <- function(pars){

  if (is.na(pars[["dailydose"]])){
    shiny::validate(shiny::need(!is.na(pars[["dailydose"]]),message = paste("")))
  }
  if (is.na(pars[["caco2default"]])){
    shiny::validate(shiny::need(!is.na(pars[["caco2default"]]),message = paste("")))
  }
}


################################################################################
################################################################################

#' Check if any inputs specific to the in vitro in vivo extrapolation (IVIVE)
#' module are invalid or missing
#'
#' @description
#' This function validates that all user-selected parameters across the
#' IVIVE module are acceptable. If not, the simulator will not compute until any
#' errors in inputs are fixed. The current function is called by IVIVE_server().
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A blank error message to the user that halts computation until the
#' user fixes the input error.
#' @noRd
#'
validate_text_IVIVE <- function(pars){

  if (is.null(pars[["BioactiveFile"]])){
    shiny::validate(shiny::need(!is.null(pars[["BioactiveFile"]]),message = paste("")))
  }
  if (!is.null(pars[["BioactiveFile"]])){

    file_df <- utils::read.csv(pars[["BioactiveFile"]]$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN ORDER
    file_df_colnames <- colnames(file_df)
    col_pars <- c("ChemicalName","CAS","BioactiveConcentration")
    if (!all(file_df_colnames == col_pars)){
      shiny::validate(shiny::need(all(file_df_colnames == col_pars),message = paste("")))
    }

    # --- CHECK FOR MISSING REQUIRED DATA
    if (anyNA(file_df)){
      shiny::validate(shiny::need(!anyNA(file_df),message = paste("")))
    }

    # --- CHECK FOR CORRECT DATA INPUT TYPES
    file_df_datatypes <- unname(sapply(file_df,class))
    par_types <- c("character","character","numeric")
    if (!all(file_df_datatypes == par_types) ||
        !all(is.na(as.numeric(file_df$ChemicalName))) ||
        !all(is.na(as.numeric(file_df$CAS)))){
      shiny::validate(shiny::need(all(file_df_datatypes == par_types) &&
                                  all(is.na(as.numeric(file_df$ChemicalName))) &&
                                  all(is.na(as.numeric(file_df$CAS))),message = paste("")))
    }
  }
  if (pars[["returnsamples"]] == 'Select'){
    shiny::validate(shiny::need(pars[["returnsamples"]] != 'Select',message = paste("")))
  }
  if (is.na(pars[["quantile"]])){
    shiny::validate(shiny::need(!is.na(pars[["quantile"]]),message = paste("")))
  }
  if (is.na(pars[["samples"]])){
    shiny::validate(shiny::need(!is.na(pars[["samples"]]),message = paste("")))
  }
  if (is.na(pars[["min_fub"]])){
    shiny::validate(shiny::need(!is.na(pars[["min_fub"]]),message = paste("")))
  }
  if (!is.null(pars[["HondaIVIVE"]])){
    if (pars[["HondaIVIVE"]] == "Honda1"){
      if (is.na(pars[["FSBf"]])){
        shiny::validate(shiny::need(!is.na(pars[["FSBf"]]),message = paste("")))
      }
    }
  }
  if (is.na(pars[["caco2default"]])){
    shiny::validate(shiny::need(!is.na(pars[["caco2default"]]),message = paste("")))
  }
  if (!is.null(pars[["fileExposure"]])){

    # --- PROCESS UPLOADED DATA
    file_df <- utils::read.csv(pars[["fileExposure"]]$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN NAMES AND ORDER
    file_df_colnames <- colnames(file_df)

    if (!all(file_df_colnames == c("ChemicalName","CAS","Upper","Median","Lower"))){
      shiny::validate(shiny::need(all(file_df_colnames == c("ChemicalName","CAS","Upper","Median","Lower")),message = paste("")))
    }

    # --- CHECK FOR MISSING REQUIRED DATA
    vec <- c(file_df$ChemicalName,file_df$CAS)
    if (anyNA(vec)){
      shiny::validate(shiny::need(!anyNA(vec),message = paste("")))
    }

    exp_df <- file_df[,3:5]
    NAindicies <- which(is.na(exp_df), arr.ind = TRUE)
    if (nrow(NAindicies) != 0){
      NArows <- unique(NAindicies[which(duplicated(NAindicies[,1])),1])
      if (length(NArows) != 0){
        for (i in 1:length(NArows)) {
          if (all(is.na(exp_df[NArows[i],]))){
            shiny::validate(shiny::need(!any(is.na(exp_df[NArows[i],])),message = paste("")))
          }
        }
      }
    }

    # --- CHECK FOR CORRECT DATA INPUT TYPES
    file_df_datatypes <- unname(sapply(file_df,class))

    if (!all(file_df_datatypes[1:2] == c("character","character")) ||
        file_df_datatypes[3] == "character" ||
        file_df_datatypes[4] == "character" ||
        file_df_datatypes[5] == "character" ||
        !all(is.na(as.numeric(file_df$ChemicalName))) ||
        !all(is.na(as.numeric(file_df$CAS)))){
      shiny::validate(shiny::need(all(file_df_datatypes[1:3] == c("character","character")) &&
                                  file_df_datatypes[3] == "character" &&
                                  file_df_datatypes[4] == "character" &&
                                  file_df_datatypes[5] == "character" &&
                                  all(is.na(as.numeric(file_df$ChemicalName))) &&
                                  all(is.na(as.numeric(file_df$CAS))),
                                  message = paste("")))
    }
  }
}


################################################################################
################################################################################

#' Check if any inputs specific to the parameter calculations module are invalid
#' or missing
#'
#' @description
#' This function validates that all user-selected parameters across the
#' parameter calculations module are acceptable. If not, the simulator will not
#' compute until any errors in inputs are fixed. The current function is called
#' by PC_server().
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A blank error message to the user that halts computation until the
#' user fixes the input error.
#' @noRd
#'
validate_text_PC <- function(pars){

  if (is.na(pars[["Clint_Pval"]])){
    shiny::validate(shiny::need(!is.na(pars[["Clint_Pval"]]),message = paste("")))
  }
  if (is.na(pars[["AlphaPar"]])){
    shiny::validate(shiny::need(!is.na(pars[["AlphaPar"]]),message = paste("")))
  }
  if (is.na(pars[["min_fub"]])){
    shiny::validate(shiny::need(!is.na(pars[["min_fub"]]),message = paste("")))
  }
}
