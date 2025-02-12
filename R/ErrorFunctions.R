

######################################################
# RULE FUNCTIONS FOR INPUT ERRORS
######################################################

not_null <- function(value,input, message = "At least one compound must be selected or uploaded"){
  if (is.null(c(value,input$file1))) message
}

UploadComps_Check <- function(value,input){

  if (!is.null(value)){

    # --- Process uploaded data
    file_df <- read.csv(value$datapath)
    file_df[file_df == ""] <- NA

    # --- Check for correct column names and order
    file_df_colnames <- colnames(file_df)
    httkdata_colnames <- colnames(chem.physical_and_invitro.data)

    if (!all(file_df_colnames == httkdata_colnames)){
      return("Error: Check the uploaded file to make sure the correct column names
               were used and are in the correct order. See the 'Instructions' card on the
               left.")
    }

    # --- Check for missing required data
    req_pars <- c("Compound","CAS","DTXSID","logP","MW")
    if (anyNA(file_df[,req_pars])){
      return("Error: Check the uploaded file for missing values in the 'Compound', 'CAS',
               'DTXSID', 'logP', and 'MW' columns.")
    }

    # --- Check for missing clint and fup values
    out1 <- MissingClintFup(file_df,input$spec,input$defaulttoHuman)
    if (!is.null(out1)) return(out1)



    # --- Check for correct data input types
    out2 <- DataInputType(file_df,file_df_colnames)
    if (!is.null(out2)) return(out2)
  }
}

MissingClintFup <- function(file,spec,defaultHuman){

  out <- NULL
  # --- Check for missing clint and fup values
  if (spec == "Human"){
    if (anyNA(file[,c("Human.Clint","Human.Funbound.plasma")])){
      out <- "Error: Check the uploaded file for missing 'Human.Clint' and 'Human.Funbound.plasma' values."
    }
  }
  else if (spec == "Rat" && defaultHuman == "No"){
    if (anyNA(file[,c("Rat.Clint","Rat.Funbound.plasma")])){
      out <- "Error: Check the uploaded file for missing 'Rat.Clint' and 'Rat.Funbound.plasma' values."
    }
  }
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
  else if (spec == "Mouse" && defaultHuman == "Yes"){

    Fup <- file[,c("Human.Funbound.plasma","Mouse.Funbound.plasma")]
    ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

    if (anyNA(file$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
      out <- "Error: Check the uploaded file for missing Clint or Funbound.plasma values. One of
                   'Human.Clint' and one of mouse or human funbound plasma must have data."
    }
  }
  else if (spec == "Rabbit" && defaultHuman == "Yes"){

    Fup <- file[,c("Human.Funbound.plasma","Rabbit.Funbound.plasma")]
    ind_Fup <- which(is.na(Fup),arr.ind = TRUE)

    if (anyNA(file$Human.Clint) || any(duplicated(ind_Fup[,1]) == TRUE)){
      out <- "Error: Check the uploaded file for missing Clint or Funbound.plasma values. One of
                   'Human.Clint' and one of rabbit or human funbound plasma must have data."
    }
  }
  else if (spec == "Dog" && defaultHuman == "Yes"){

    if (anyNA(file[,c("Human.Clint","Human.Funbound.plasma")])){
      out <- "Error: Check the uploaded file for missing Human.Clint or Human.Funbound.plasma values."
    }
  }
  else{
    out <- "Error: You must select 'Yes' for the default to human parameter selection on the 'General
           Parameters' page. Human.Clint and/or Human.Funbound.plasma values are needed to run simulations for
           the selected species."
  }
  return(out)
}

DataInputType <- function(file,column_names){

  out <- NULL
  file_df_datatypes <- unname(sapply(file,class))
  httkdata_datatypes <- unname(sapply(chem.physical_and_invitro.data,class))

  if (!all(file_df_datatypes == httkdata_datatypes)){

    accept_datatypes <- DataTypeList()
    check_type <- unname(mapply(type_func,
                                accept_datatypes,
                                file_df_datatypes))

    if (any(check_type == FALSE)){
      out <- "Error: Check the uploaded file to make sure the correct type of data (numbers,
                 words) was used for each entry. See the 'Instructions' card on the left."
    }
  }
  return(out)
}

type_func <- function(x,y){

  if (y %in% x){
    TRUE
  }
  else{
    FALSE
  }
}

DataTypeList <- function(){

  t1 <- c("character")
  t2 <- c("numeric")
  t3 <- c("character","numeric")
  t4 <- c("character","logical")
  t5 <- c("numeric","logical")
  t6 <- c("character","numeric","logical")

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
              humanclint = t3,
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
              humanfunboundplasma = t3,
              humanfunboundplasmareference = t4,
              humanrblood2plasma = t5,
              humanrblood2plasmareference = t4,
              monkeyforal = t5,
              monkeyforalreference = t4,
              mouseforal = t5,
              mouseforalreference = t4,
              mousefunboundplasma = t5,
              mousefunboundplasmareference = t4,
              rabbitfunboundplasma = t5,
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

multdose_Select <- function(value,input,message = "The dosing frequency must be selected"){
  if (input$dosenum == "Multiple Doses" && value == "Select") message
}

multdose_odd <- function(value,input,message = "The dosing administration amounts and times must be entered"){
  if (input$dosenum == "Multiple Doses" && input$multdose == "No" && value == "") message
}

fetal_cond <- function(value,input,message = "The 'Human' species must be selected to run the fetal_pbtk model"){
  if (!is.null(value)){
    if (value == "fetal_pbtk" && input$spec != "Human") message
  }
}

returntimes_cond <- function(value,input,message = "A beginning output time of 91 days (13 weeks) or later must be entered"){

  if (input$model == "fetal_pbtk" && value != ''){
    v1 <- unlist(strsplit(value,","))
    out_times <- sapply(v1, function(x) eval(parse(text = x)))
    if (min(out_times)<91){
      message
    }
  }
}

BioUpload_Check <- function(value,input){

  if (!is.null(value)){

    # --- Process uploaded data
    file_df <- read.csv(value$datapath)
    file_df[file_df == ""] <- NA

    # --- Check for missing required data
    if (anyNA(file_df)){
      return("Error: Check the uploaded file for missing values.")
    }

    # --- Check for correct column names and order
    file_df_colnames <- colnames(file_df)

    if (!all(file_df_colnames == c("ChemicalName","CAS","BioactiveConcentration"))){
      return("Error: Check the uploaded file to make sure the correct column names
               were used and are in the correct order.")
    }

    # --- Check for correct data input types
    file_df_datatypes <- unname(sapply(file_df,class))

    if (!all(file_df_datatypes == c("character","character","numeric"))){
        return("Error: Check the uploaded file to make sure the correct type of data (numbers,
                 words) was used for each entry. .")
    }
  }
}

FSBf_Check <- function(value,input){

  if (!is.null(input$HondaIVIVE)){
    if (input$HondaIVIVE == "Honda1"){
      if (is.na(value)){
        "Required"
      }
    }
  }
}

addrule_adme_ics <- function(iv_adme,identifier){
  iv_adme$add_rule(identifier,shinyvalidate::sv_required())
}

adme_ic_errormess <- function(identifier,pars){
  if (is.na(pars[[identifier]])){
    shiny::validate(shiny::need(!is.na(pars[[identifier]]),message = paste("")))
  }
}
