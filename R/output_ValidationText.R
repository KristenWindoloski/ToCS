caption_text <- function(func, model){

  if (func == "ADME"){

    if (model == 'pbtk'){
      AUCoutput <- "venous plasma concentration."
    }
    else if (model == 'fetal_pbtk'){
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

    # --- READ IN DATA
    file_df <- read.csv(pars[["file1"]]$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN NAMES AND ORDER
    file_df_colnames <- colnames(file_df)
    httkdata_colnames <- colnames(chem.physical_and_invitro.data)

    if (!all(file_df_colnames == httkdata_colnames)){
      shiny::validate(shiny::need(all(file_df_colnames == httkdata_colnames),message = paste("")))
    }

    # --- CHECK FOR MISSING REQUIRED DATA
    # Check for universally required parameters
    req_pars <- c("Compound","CAS","DTXSID","logP","MW")
    if (anyNA(file_df[,req_pars])){
      shiny::validate(shiny::need(!anyNA(file_df[,req_pars]),message = paste("")))
    }
    # Check for species specific parameters
    if (pars[["spec"]] == "Human"){
      col_pars <- c("Human.Clint","Human.Funbound.plasma")
      if (anyNA(file_df[,col_pars])){
        shiny::validate(shiny::need(!anyNA(file_df[,col_pars]),message = paste("")))
      }
    }
    else if (pars[["spec"]] == "Rat" && pars[["defaulttoHuman"]] == "No"){
      col_pars <- c("Rat.Clint","Rat.Funbound.plasma")
      if (anyNA(file_df[,col_pars])){
        shiny::validate(shiny::need(!anyNA(file_df[,col_pars]),message = paste("")))
      }
    }
    else if (pars[["spec"]] == "Rat" && pars[["defaulttoHuman"]] == "Yes"){

      Clint <- file_df[,c("Human.Clint","Rat.Clint")]
      ind_Clint <- which(is.na(Clint),arr.ind = TRUE)

      Fup <- file_df[,c("Human.Funbound.plasma","Rat.Funbound.plasma")]
      ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

      if (any(duplicated(ind_Clint[,1]) == TRUE) || any(duplicated(ind_Fup[,1]) == TRUE)){
        shiny::validate(shiny::need(all(duplicated(ind_Clint[,1]) == FALSE) && all(duplicated(ind_Fup[,1]) == FALSE),message = paste("")))
      }
    }
    else if (pars[["spec"]] == "Mouse" && pars[["defaulttoHuman"]] == "Yes"){

      Fup <- file_df[,c("Human.Funbound.plasma","Mouse.Funbound.plasma")]
      ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

      if (anyNA(file_df$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
        shiny::validate(shiny::need(!anyNA(file_df$Human.Clint) && all(duplicated(ind_Fup[,1]) == FALSE),message = paste("")))
      }
    }
    else if (pars[["spec"]] == "Rabbit" && pars[["defaulttoHuman"]] == "Yes"){

      Fup <- file_df[,c("Human.Funbound.plasma","Rabbit.Funbound.plasma")]
      ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

      if (anyNA(file_df$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
        shiny::validate(shiny::need(!anyNA(file_df$Human.Clint) && all(duplicated(ind_Fup[,1]) == FALSE),message = paste("")))
      }
    }
    else if (pars[["spec"]] == "Dog" && pars[["defaulttoHuman"]] == "Yes"){

      col_pars <- c("Human.Clint","Human.Funbound.plasma")
      if (anyNA(file_df[,col_pars])){
        shiny::validate(shiny::need(!anyNA(file_df[,col_pars]),message = paste("")))
      }
    }
    else{
      shiny::validate(shiny::need(pars[["defaulttoHuman"]] == "Yes",message = paste("")))
    }

    # --- CHECK FOR CORRECT INPUT DATA TYPES
    file_df_datatypes <- unname(sapply(file_df,class))
    httkdata_datatypes <- unname(sapply(chem.physical_and_invitro.data,class))

    if (!all(file_df_datatypes == httkdata_datatypes)){
      accept_datatypes <- DataTypeList()
      check_type <- unname(mapply(type_func,
                                  accept_datatypes,
                                  file_df_datatypes))
      if (any(check_type == FALSE)){
        shiny::validate(shiny::need(all(check_type == TRUE),message = paste("")))
      }
    }

  }
}

validate_text_ADME <- function(pars){

  validate_text_Common(pars)

  if (pars[["dosenum"]] == 'Select'){
    shiny::validate(shiny::need(pars[["dosenum"]] != 'Select',message = paste("")))
  }
  if (pars[["dosenum"]] == 'Multiple Doses' && pars[["multdose"]] == 'Select'){
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

validate_text_SS <- function(pars){

  validate_text_Common(pars)

  if (is.na(pars[["dailydose"]])){
    shiny::validate(shiny::need(!is.na(pars[["dailydose"]]),message = paste("")))
  }
  if (is.na(pars[["caco2default"]])){
    shiny::validate(shiny::need(!is.na(pars[["caco2default"]]),message = paste("")))
  }
}

validate_text_IVIVE <- function(pars){

  validate_text_Common(pars)

  if (is.null(pars[["BioactiveFile"]])){
    shiny::validate(shiny::need(!is.null(pars[["BioactiveFile"]]),message = paste("")))
  }
  if (!is.null(pars[["BioactiveFile"]])){

    file_df <- read.csv(pars[["BioactiveFile"]]$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR MISSING REQUIRED DATA
    if (anyNA(file_df)){
      shiny::validate(shiny::need(!anyNA(file_df),message = paste("")))
    }

    # --- CHECK FOR CORRECT COLUMN ORDER
    file_df_colnames <- colnames(file_df)
    col_pars <- c("ChemicalName","CAS","BioactiveConcentration")
    if (!all(file_df_colnames == col_pars)){
      shiny::validate(shiny::need(all(file_df_colnames == col_pars),message = paste("")))
    }

    # --- CHECK FOR CORRECT DATA INPUT TYPES
    file_df_datatypes <- unname(sapply(file_df,class))
    par_types <- c("character","character","numeric")
    if (!all(file_df_datatypes == par_types)){
      shiny::validate(shiny::need(all(file_df_datatypes == par_types),message = paste("")))
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
}

validate_text_PC <- function(pars){

  validate_text_Common(pars)

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
