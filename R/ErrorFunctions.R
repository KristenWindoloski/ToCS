

######################################################
# RULE FUNCTIONS FOR INPUT ERRORS
######################################################

################################################################################
################################################################################

#' Has the user selected and/or uploaded compounds to simulate?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#' @param message A character string to return to the user if condition is true
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
not_null <- function(value,input, message = "At least one compound must be selected or uploaded"){
  if (is.null(c(value,input$file1))) message
}


################################################################################
################################################################################

#' Are there any formatting or data errors in the new compounds file uploaded by
#' the user?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
UploadComps_Check <- function(value,input){

  if (!is.null(value)){

    # --- PROCESS UPLOADED DATA
    file_df <- read.csv(value$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN NAMES AND COLUMN ORDER
    file_df_colnames <- colnames(file_df)
    httkdata_colnames <- colnames(chem.physical_and_invitro.data)

    if (!all(file_df_colnames == httkdata_colnames)){
      return("Error: Check the uploaded file to make sure the correct column names
               were used and are in the correct order. See the 'Instructions' card on the
               left.")
    }

    # --- CHECK FOR MISSING REQUIRED DATA NO MATTER THE SIMULATION
    req_pars <- c("Compound","CAS","DTXSID","logP","MW")
    if (anyNA(file_df[,req_pars])){
      return("Error: Check the uploaded file for missing values in the 'Compound', 'CAS',
               'DTXSID', 'logP', and 'MW' columns.")
    }

    # --- CHECK FOR MISSING HONDA1 ASSUMPTION IVIVE REQURIED PARAMETERS, IF APPLICABLE
    if (input$func == "In vitro in vivo extrapolation (IVIVE)"){
      if (!is.null(input$HondaIVIVE)){
        if (input$HondaIVIVE == "Honda1"){
          if (anyNA(file_df[,c("logHenry","logWSol","MP")])){
            return("Error: Check the uploaded file for missing values in the
                   'logHenry', 'logWSol', and 'MP' columns.")
          }
        }
      }
    }


    # --- CHECK FOR MISSING CLINT AND FUP VALUES
    out1 <- MissingClintFup(file_df,input$spec,input$defaulttoHuman)
    if (!is.null(out1)) return(out1)

    # --- CHECK FOR CORRECT INPUT DATA TYPES
    out2 <- DataInputType(file_df,file_df_colnames)
    if (!is.null(out2)) return(out2)
  }
}


################################################################################
################################################################################

#' Title
#'
#' @param file
#' @param spec
#' @param defaultHuman
#'
#' @return
#' @export
#'
MissingClintFup <- function(file,spec,defaultHuman){

  out <- NULL

  # --- HUMAN SPECIES
  if (spec == "Human"){
    if (anyNA(file[,c("Human.Clint","Human.Funbound.plasma")])){
      out <- "Error: Check the uploaded file for missing 'Human.Clint' and 'Human.Funbound.plasma' values."
    }
  }
  # --- RAT SPECIES AND NO HUMAN
  else if (spec == "Rat" && defaultHuman == "No"){
    if (anyNA(file[,c("Rat.Clint","Rat.Funbound.plasma")])){
      out <- "Error: Check the uploaded file for missing 'Rat.Clint' and 'Rat.Funbound.plasma' values."
    }
  }
  # --- RAT SPECIES AND HUMAN ALLOWABLE
  else if (spec == "Rat" && defaultHuman == "Yes"){

    Clint <- file[,c("Human.Clint","Rat.Clint")]
    ind_Clint <- which(is.na(Clint),arr.ind = TRUE)

    Fup <- file[,c("Human.Funbound.plasma","Rat.Funbound.plasma")]
    ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

    if (any(duplicated(ind_Clint[,1]) == TRUE) || any(duplicated(ind_Fup[,1]) == TRUE)){
      out <- "Error: Check the uploaded file for missing Clint or Funbound.plasma values. One of
                 rat or human clint and one of rat or human funbound plasma must have data."
    }
  }
  # --- MOUSE SPECIES AND HUMAN ALLOWABLE
  else if (spec == "Mouse" && defaultHuman == "Yes"){

    Fup <- file[,c("Human.Funbound.plasma","Mouse.Funbound.plasma")]
    ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

    if (anyNA(file$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
      out <- "Error: Check the uploaded file for missing Clint or Funbound.plasma values. One of
                   'Human.Clint' and one of mouse or human funbound plasma must have data."
    }
  }
  # --- RABBIT SPECIES AND HUMAN ALLOWABLE
  else if (spec == "Rabbit" && defaultHuman == "Yes"){

    Fup <- file[,c("Human.Funbound.plasma","Rabbit.Funbound.plasma")]
    ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

    if (anyNA(file$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
      out <- "Error: Check the uploaded file for missing Clint or Funbound.plasma values. One of
                   'Human.Clint' and one of rabbit or human funbound plasma must have data."
    }
  }
  # --- DOG SPECIES AND HUMAN ALLOWABLE
  else if (spec == "Dog" && defaultHuman == "Yes"){

    if (anyNA(file[,c("Human.Clint","Human.Funbound.plasma")])){
      out <- "Error: Check the uploaded file for missing Human.Clint or Human.Funbound.plasma values."
    }
  }
  # --- NON-HUMAN OR NON-RAT SPECIES THAT NEED HUMAN DATA FOR SIMULATION
  else{
    out <- "Error: You must select 'Yes' for the default to human parameter selection on the 'General
           Parameters' tab. Human.Clint and/or Human.Funbound.plasma values are needed to run simulations for
           the selected species."
  }
  return(out)
}


################################################################################
################################################################################

#' Title
#'
#' @param file
#' @param column_names
#'
#' @return
#' @export
#'
DataInputType <- function(file,column_names){

  # --- DETERMINE FILE ENTRY DATA TYPES
  out <- NULL
  file_df_datatypes <- unname(sapply(file,class))
  httkdata_datatypes <- unname(sapply(chem.physical_and_invitro.data,class))

  # --- EXTRACT COLUMNS THAT ARE SUPPOSED TO BE ONLY CHARACTERS
  file_df_ref <- file %>% dplyr::select(dplyr::contains("reference"))
  df_non_ref <- file[,c("Compound","CAS","DTXSID","Formula","All.Compound.Names","All.Species","Chemical.Class")]
  char_only_df <- cbind(df_non_ref,file_df_ref)

  # --- CHECK IF UPLOADED FILE COLUMN DATA TYPES MATCH ALL HTTK COLUMN DATA TYPES
  if (!all(file_df_datatypes == httkdata_datatypes)){

    accept_datatypes <- DataTypeList()
    check_type <- unname(mapply(type_func,accept_datatypes,file_df_datatypes))

    # --- IF TYPES DON'T EXACTLY MATCH, SEE IF TYPE IS ACCEPTABLE AND MAKE SURE
    # --- ANY COLUMNS OF TYPE 'CHARACTER' ARE ACTUALLY ALL CHARACTERS AND NOT NUMERICS
    if (any(check_type == FALSE) || !all(is.na(as.numeric(unlist(char_only_df))))){
      out <- "Error: Check the uploaded file to make sure the correct type of data (numbers,
                 words) was used for each entry. See the 'Instructions' card on the left."
    }
  }
  else if (!all(is.na(as.numeric(unlist(char_only_df))))){
    out <- "Error: Check the uploaded file to make sure the correct type of data (numbers,
                 words) was used for each entry. See the 'Instructions' card on the left."
  }
  return(out)
}


################################################################################
################################################################################

#' Determine if the data type is appropriate
#'
#' @param x A vector
#' @param y A vector
#'
#' @return TRUE or FALSE
#' @export
#'
type_func <- function(x,y){

  if (y %in% x) TRUE
  else FALSE
}


################################################################################
################################################################################

#' Generate a list of acceptable input data types for the compound file user
#' upload
#'
#' @return A list of all acceptable data types for each column in the 'httk'
#' package's chem.physical_and_invitro.data
#' @export
#'
DataTypeList <- function(){

  # --- TYPES OF ACCEPTABLE COLUMN OUTPUTS
  t1 <- c("character")
  t2 <- c("numeric")
  t3 <- c("character","numeric")
  t4 <- c("character","logical")
  t5 <- c("numeric","logical")
  t6 <- c("character","numeric","logical")

  # --- LIST ACCEPTABLE OUTPUTS FOR EACH SPECIFIC COLUMN
  lst <- list(compound = t1,
              cas = t1,
              cas.checksum = t4,
              dtxsid = t1,
              formula = t4,
              all.compound.names = t4,
              loghenry = t5,
              loghenryreference = t4,
              logma = t5,
              logmareference = t4,
              logP = t2,
              logpreference = t4,
              logpwa = t5,
              logpwareference = t4,
              logwsol = t5,
              logwsolreference = t4,
              mp = t5,
              mpreference = t4,
              mw = t2,
              mwreference = t4,
              pkaaccept = t6,
              pkaaceeptreference = t4,
              pkadonor = t6,
              pkadonorreference = t4,
              allspecies = t5,
              dogforal = t5,
              dogforalreference = t4,
              dtxsidreference = t4,
              formulareference = t4,
              humancaco2pab = t6,
              humancaco2pabreference = t4,
              humanclint = t6,
              humanclintpvalue = t5,
              humanclintpvaluereference = t4,
              humanclintreference = t4,
              humanfabs = t5,
              humanfabsreference = t4,
              humanfgut = t5,
              humanfgutreference = t4,
              humanfhep = t5,
              humanfhepreference = t4,
              humanforal = t5,
              humanforalreference = t4,
              humanfunboundplasma = t6,
              humanfunboundplasmareference = t4,
              humanrblood2plasma = t5,
              humanrblood2plasmareference = t4,
              monkeyforal = t5,
              monkeyforalreference = t4,
              mouseforal = t5,
              mouseforalreference = t4,
              mousefunboundplasma = t6,
              mousefunboundplasmareference = t4,
              rabbitfunboundplasma = t6,
              rabbitfunboundplasmareference = t4,
              ratclint = t6,
              ratclintpvalue = t5,
              ratclintpvaluereference = t4,
              ratclintreference = t4,
              ratforal = t5,
              ratforalreference = t4,
              ratfunboundplasma = t6,
              ratfunboundplasmareference = t4,
              ratrblood2plasma = t5,
              ratrblood2plasmareference = t4,
              chemicalclass = t4)

  return(lst)
}


################################################################################
################################################################################

#' Has the user selected a dosing frequency?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#' @param message A character string to return to the user if condition is true
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
multdose_Select <- function(value,input,message = "The dosing frequency must be selected"){
  if (input$dosenum == "Multiple Doses" && value == "Select") message
}


################################################################################
################################################################################

#' Has the user entered dosing amounts and times?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#' @param message A character string to return to the user if condition is true
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
multdose_odd <- function(value,input,message = "The dosing administration amounts and times must be entered"){
  if (input$dosenum == "Multiple Doses" && input$multdose == "No" && value == "") message
}


################################################################################
################################################################################

#' Was the human species selected when running the fetal_pbtk model?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#' @param message A character string to return to the user if condition is true
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
fetal_cond <- function(value,input,message = "The 'Human' species must be selected to run the fetal_pbtk model"){
  if (!is.null(value)){
    if (value == "fetal_pbtk" && input$spec != "Human") message
  }
}


################################################################################
################################################################################

#' Has the user entered a beginning output time of 91 days or later?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#' @param message A character string to return to the user if condition is true
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
returntimes_cond <- function(value,input,message = "A beginning output time of 91 days (13 weeks) or later must be entered"){

  if (input$model == "fetal_pbtk" && value != ''){
    v1 <- unlist(strsplit(value,","))
    out_times <- sapply(v1, function(x) eval(parse(text = x)))
    if (min(out_times)<91){
      message
    }
  }
}


################################################################################
################################################################################

#' Title
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#'
#' @return
#' @export
#'
BioUpload_Check <- function(value,input){

  if (!is.null(value)){

    # --- PROCESS UPLOADED DATA
    file_df <- read.csv(value$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN NAMES AND ORDER
    file_df_colnames <- colnames(file_df)

    if (!all(file_df_colnames == c("ChemicalName","CAS","BioactiveConcentration"))){
      return("Error: Check the uploaded file to make sure the correct column names
               were used and are in the correct order.")
    }

    # --- CHECK FOR MISSING REQUIRED DATA
    if (anyNA(file_df)){
      return("Error: Check the uploaded file for missing values.")
    }

    # --- CHECK FOR CORRECT DATA INPUT TYPES
    file_df_datatypes <- unname(sapply(file_df,class))

    if (!all(file_df_datatypes == c("character","character","numeric")) ||
        !all(is.na(as.numeric(file_df$ChemicalName))) ||
        !all(is.na(as.numeric(file_df$CAS)))){
        return("Error: Check the uploaded file to make sure the correct type of data (numbers,
                 words) was used for each entry.")
    }
  }
}


################################################################################
################################################################################

#' Has the user defined an FSBf value?
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#'
#' @return If condition if FALSE, then return the message. If the condition is
#' TRUE, return nothing.
#' @export
#'
FSBf_Check <- function(value,input){

  if (!is.null(input$HondaIVIVE)){
    if (input$HondaIVIVE == "Honda1"){
      if (is.na(value)){
        "Required"
      }
    }
  }
}


################################################################################
################################################################################

#' Title
#'
#' @param value A Shiny input ID corresponding to an input object
#' @param input The reactive inputs associated with the session
#'
#' @return
#' @export
#'
ExposureUpload_Check <- function(value,input){

  if (!is.null(value)){

    # --- PROCESS UPLOADED DATA
    file_df <- read.csv(value$datapath)
    file_df[file_df == ""] <- NA

    # --- CHECK FOR CORRECT COLUMN NAMES AND ORDER
    file_df_colnames <- colnames(file_df)

    if (!all(file_df_colnames == c("ChemicalName","CAS","Upper","Median","Lower"))){
      return("Error: Check the uploaded file to make sure the correct column names
               were used and are in the correct order.")
    }

    # --- CHECK FOR MISSING REQUIRED DATA
    if (anyNA(c(file_df$ChemicalName,file_df$CAS))){
      return("Error: Check the uploaded file for missing values in the
             'ChemicalName' and 'CAS' columns.")
    }

    exp_df <- file_df[,3:5]
    NAindicies <- which(is.na(exp_df), arr.ind = TRUE)
    if (nrow(NAindicies) != 0){
      NArows <- unique(NAindicies[which(duplicated(NAindicies[,1])),1])
      if (length(NArows) != 0){
        for (i in 1:length(NArows)) {
          if (all(is.na(exp_df[NArows[i],]))){
            return("Error: Check the uploaded file for missing exposure data. There must
                   be at least one exposure estimate per chemical.")
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
      return("Error: Check the uploaded file to make sure the correct type of data (numbers,
                 words) was used for each entry.")
    }
  }
}


################################################################################
################################################################################

#' Title
#'
#' @param iv_adme
#' @param identifier
#'
#' @return
#' @export
#'
addrule_adme_ics <- function(iv_adme,identifier){
  iv_adme$add_rule(identifier,shinyvalidate::sv_required())
}


################################################################################
################################################################################

#' Title
#'
#' @param identifier
#' @param pars
#'
#' @return
#' @export
#'
adme_ic_errormess <- function(identifier,pars){
  if (is.na(pars[[identifier]])){
    shiny::validate(shiny::need(!is.na(pars[[identifier]]),message = paste("")))
  }
}
