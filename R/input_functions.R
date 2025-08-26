

################################################################################
################################################################################

#' Compile the names of all user-selected parameters across all modules.
#' GatherInputVars(), which calls the current function
#'
#' @return A list of parameter id names
#' @noRd
#'
ParNames <- function(){
  GenPars <- c("func","spec","defaulttoHuman")
  CompoundSelectPars <- c("HondaIVIVE","FSBf","httkPreloadComps","file1","insilicopars")
  ModelSpecifPars <- c("doseroute","doseunits","dosenum","initdose","multdose","mult_doseamount","mult_dosetime","multdose_odd","dailydose",
                       "model","simtime","BioactiveFile","returnsamples","quantile")
  ModelConditionPars <- c("init_cond_opts",unlist(names_ICs()[[1]]),"samples","bioactiveIVIVE",
                          "Clint_Pval","restrict_clear","adj_fub","min_fub","regression")
  ModelSolverPars <- c("odemethod","solversteps","rtol","atol")
  BioavailPars <- c("caco2default","caco_fabs","caco_fgut","caco_overwriteinvivo","caco_keep100")
  OutputSpecifPars <- c("returntimes","modelSSout_units","output_concSS","tissueSS","modelIVIVEout_units","output_concIVIVE","tissueIVIVE","fileExposure")
  RunSimPars <- c("runsim","logscale","ResetButton")

  out_lst <- c(GenPars,CompoundSelectPars,ModelSpecifPars,ModelConditionPars,ModelSolverPars,BioavailPars,OutputSpecifPars,RunSimPars)
}


################################################################################
################################################################################

#' Generate text for the instructions card under the 'General Parameters' tab.
#' Instructions_GenPars(), which calls the current function
#'
#' @return Text describing the major uses of ToCS, where to find additional
#' resources, and where to report bugs
#' @noRd
#'
Instructions_GenPars <- function(){
  htmltools::tagList(shiny::helpText("Fill out the prompts on each of the above tabs moving left to right. Then,
                     click the 'Run Simulation' tab to run the simulation or reset all selections."),
                     shiny::helpText("ToCS provides four outputs:
                                     1) Concentration-time profiles (returns chemical concentrations in body compartments over time),
                                     2) Steady state (SS) concentration (returns SS concentrations in body compartments from an oral infusion),
                                     3) In vitro to in vivo extrapolation (IVIVE) (returns oral equivalent doses to in vitro bioactive concentrations),
                                     4) Parameter calculations (returns elimination rates, volumes of distribution, tissue to unbound plasma
                                     partition coefficients, half-lives, and total plasma clearances)."),
                     shiny::helpText("This application uses the U.S. EPA's R package 'httk'. For more information on ToCS and 'httk', please
                                     refer to the following links."),
                     shiny::tags$a(href = "https://github.com/KristenWindoloski/ToCS/tree/main/vignettes","Vignettes (ToCS tutorials)",
                                   style = "font-size: 15px",
                                   target="_blank"),
                     shiny::tags$a(href = "https://github.com/KristenWindoloski/ToCS/issues", "Report ToCS issues/suggestions",
                                   style = "font-size: 15px",
                                   target="_blank"),
                     shiny::tags$a(href="https://doi.org/10.18637/jss.v079.i04", "httk publication",
                                   style = "font-size: 15px",
                                   target="_blank"),
                     shiny::tags$a(href="https://cran.r-project.org/web/packages/httk", "httk CRAN webpage",
                                   style = "font-size: 15px",
                                   target="_blank"))
}

################################################################################
################################################################################

#' Generate text to notify user that they must select or upload at least one
#' compound to simulate. CS_Instructions(), which calls the current function
#'
#' @return Text notifying the user of compound requirements
#' @noRd
#'
Instructions_CompSelect_Part1 <- function(){
    htmltools::h6(shiny::strong("You must choose at least one compound from the preloaded compounds,
            upload a CSV file with data for at least one compound not included
            in the preloaded compounds, or both."))
}


################################################################################
################################################################################

#' Generate text to notify the user of where to see sample uploaded compound data.
#' CS_Instructions(), which calls the current function
#'
#' @return Text notifying the user of uploaded compound data instructions
#' @noRd
#'
Instructions_CompSelect_Part2 <- function(){
  shiny::helpText("Click on the appropriate link(s) below to download guidance on how to upload data under the 'Uploaded Data' card.
                  Follow the 'Instructions' document in the downloaded folder to correctly format the file you want to upload.")
}


################################################################################
################################################################################

#' Generate text for the 'Run Simulation' button. RS_Actions(), which calls the
#' current function
#'
#' @return Text notifying the user of the 'Run Simulation' button
#' @noRd
#'
Instructions_RunSim <- function(){
  shiny::helpText("Click on the 'Run Simulation' button when all information has been entered.")
}


################################################################################
################################################################################

#' Generate text for the 'Reset Session' button. RS_Actions(), which calls the
#' current function
#'
#' @return Text notifying the user of the 'Reset Session' button
#' @noRd
#'
Instructions_Reset <- function(){
  shiny::helpText("Click on the button below to reset your session. This will clear all
           selections and any uploaded data, and is recommended to be done every
           time a new simulation is run.")
}


################################################################################
################################################################################

#' Generate the 'Output' drop down menu. GP_Output(), which calls the current function
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return Drop down menu for the desired output of ToCS
#' @noRd
#'
selectInput_Function <- function(id,
                                 choice_default = "Select"){
  shiny::selectInput(id, label = "Select the desired output.",
              choices = list("Select", "Concentration-time profiles",
                             "Steady state concentrations",
                             "In vitro in vivo extrapolation (IVIVE)",
                             "Parameter calculations"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate the 'Species' drop down menu. GP_Species(), which calls the current function
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return Drop down menu for the desired species to simulate in ToCS
#' @noRd
#'
selectInput_Species <- function(id,
                                choice_default = "Select"){

  shiny::selectInput(id, label = "Select the species to analyze.",
              choices = list("Select", "Dog", "Human", "Mouse", "Rabbit", "Rat"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate the 'use of human data in place of animal data if unavailable' drop
#' down menu. GP_Species(), which calls the current function
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return Drop down menu for the human data preference use in ToCS, if applicable
#' @noRd
#'
selectInput_DefaultToHuman <- function(id,
                                       choice_default = "Yes"){
  shiny::selectInput(id,
              label = "Do you want to use human in vitro data if in vitro data for
                      the selected species is missing?",
              choices = list("Yes", "No"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate the 'In silico parameter preference' drop down menu. MS_Model(), which
#' calls the current function
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return Drop down menu with options to either use in silico generated clint
#' (instrinic hepatic clearance) and fup (fraction unbound in plasma) in absence
#' of in vitro data for the selected species
#' @noRd
#'
selectInput_InSilicoPars <- function(id,
                                     choice_default = "No, do not load in silico parameters"){
  shiny::selectInput(id,
              label = "Select whether to use in silico generated parameters for
                      compounds with missing in vitro data. These parameters will
                      not overwrite existing in vitro data, and it will expand
                      the number of compounds available.",
              choices = list("No, do not load in silico parameters",
                             "Yes, load in silico parameters"),
              selected = "No, do not load in silico parameters",
              width = "100%")
}

################################################################################
################################################################################

#' Generate the drop down menu where the user can specify the types of chemicals
#' to choose from. CS_PreloadedCompounds(), which calls the current function
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return Drop down menu with options to either set simulated chemicals as a
#' demo list, select chemicals from the entire available list, or select chemicals
#' from a list of food or food-related chemicals only
#' @noRd
#'
selectInput_CompPreference <- function(id,
                                       choice_default = "Choose from all available chemicals"){
  shiny::selectInput(id,
                     label = "Select the types of compounds you want to simulate.",
                     choices = list("Choose from all available chemicals",
                                    "Choose from only food relevant chemicals"),
                     selected = choice_default,
                     width = "100%")
}


################################################################################
################################################################################

#' Generate the drop down list of preloaded compounds available to simulate in httk
#'
#' @description
#' This function creates the drop down list of preloaded compounds for users to
#' select from under the 'Preloaded Compounds' card on the 'Compound Selection'
#' tab. Four sets of in silico parameters (three for clint and fup values, one
#' for caco2 values) will be loaded if desired by the user. The current function
#' is called by PreloadComps_UI() and calls getCASnums() and getPiped().
#'
#'
#' @param func The user-selected desired output
#' @param species The user-selected species
#' @param defaulthuman The user-selected human in vitro data preference if an
#' animal species is selected
#' @param insilico The user-selected preference for use of in silico generated
#' parameters if in vitro data is missing
#' @param model The user-selected model
#' @param honda The selected IVIVE assumption (either NULL, Honda1, Honda2,
#' Honda3, or Honda4), if applicable
#' @param comptype The user-selected subset of compounds to search; either "Choose
#' from all available chemicals" or "Choose from only food relevant chemicals"
#'
#' @return A drop down list of preloaded available compounds within ToCS
#' @noRd
#'
PreloadCompsInput <- function(func,species,defaulthuman,insilico,model,honda,comptype){

  # --- Reset the chem.physical_and_invitro.data table in case any
  # --- in silico loaded parameters or uploaded chemicals/parameters
  # --- were in the environment
  httk::reset_httk(target.env = the)

  # --- Load in in silico fup, clint, and caco2 if selected
  # --- Get CAS numbers for all compounds with enough data to run simulations
  if (insilico == "Yes, load in silico parameters"){
    CASnums <- loadInSilicoPars(func,species,model,defaulthuman)
  }
  else {
    CASnums <- getCASnums(func,species,model,defaulthuman)
  }

  # --- Get all available preloaded compounds
  piped <- getPiped(CASnums,honda,comptype)

  # --- Output to be returned (either nothing or a select drop down menu)
  if (is.null(piped)){
    shiny::strong(htmltools::h6("No preloaded compounds are available for the selected species
              and human in vitro data substitution status. Please return to the General Parameters
              tab and switch the human in vitro data selection, select another species, or upload
              your own chemical data under the 'Uploaded Data' card to the right."))
    }
  else{
    choice_list <- append(c(''), piped)
    shiny::selectInput("httkPreloadComps",
                label = "Select any preloaded compounds. Search through the list by
                          clicking on the box and scrolling or typing in a name. The list may
                          not show all available compounds. Click on a compound to select it.
                          You may select multiple.",
                choices = choice_list,
                selected = '',
                multiple = TRUE,
                width = "100%")
    }
}


################################################################################
################################################################################

#' Load in silico clint, fup, and caco-2 values
#'
#' @description
#' This function loads in silico-generated clint, fup, and caco-2 values for
#' compounds in httk's chem.physical_and_invitro.data that are lacking enough
#' data to run simulations. After loading the in silico data, the list of
#' compounds is paired down to only those with enough parameter data now that
#' the in silico data has been loaded.
#'
#'
#' @param func The user-selected desired output
#' @param species The user-selected species
#' @param model The user-selected model
#' @param defaulthuman The user-selected human in vitro data preference if an
#' animal species is selected
#'
#' @return A vector of CAS numbers, each of which has enough data to run a simulation
#' @noRd
#'
loadInSilicoPars <- function(func,species,model,defaulthuman){

  # --- Satisfy R CMD Check condition ("no visible binding...")
  # --- THESE VARIABLES ARE NOT ACTUALLY NULL
  Human.Clint<-Human.Funbound.plasma<-MW<-logP<-CAS<-CAS.Checksum<-NULL
  `Clint QSAR AD Outlier`<-`Fup QSAR AD Outlier`<-`CASRN`<-`QSAR_Clint`<-`QSAR_Fup`<-Pab.Pred.AD<-NULL

  shiny::withProgress(message = "Loading the available chemicals to simulate under the 'Preloaded Compounds' card. Please wait.",
                      value = 0, {

                        # --- Load Sipes2017 parameters
                        shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 1))
                        assign("chem.physical_and_invitro.data",
                               httk::add_chemtable(httk::sipes2017,
                                                   current.table=the$chem.physical_and_invitro.data,
                                                   data.list=list(CAS='CAS',
                                                                  Funbound.plasma = 'Human.Funbound.plasma',
                                                                  Clint = 'Human.Clint'),
                                                   reference = 'Sipes 2017',
                                                   species= 'Human',
                                                   overwrite=FALSE),
                               envir = the)

                        # --- Load Pradeep2020 parameters
                        shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 2))
                        assign("chem.physical_and_invitro.data",
                               httk::add_chemtable(httk::pradeep2020,
                                                   current.table=the$chem.physical_and_invitro.data,
                                                   data.list=list(CAS = 'CASRN',
                                                                  DTXSID='DTXSID',
                                                                  Funbound.plasma = 'Consensus (SVM,RF)',
                                                                  Clint = 'pred_clint_rf'),
                                                   reference = 'Pradeep 2020',
                                                   species= 'Human',
                                                   overwrite=FALSE),
                               envir=the)

                        # --- Load Dawson2021 parameters
                        shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 3))
                        df1 <- magrittr::`%>%`(httk::dawson2021,
                                               dplyr::filter(`Clint QSAR AD Outlier`==0,
                                                             `Fup QSAR AD Outlier`==0))
                        tmp_dawson2021 <- as.data.frame(magrittr::`%>%`(df1,dplyr::select(`CASRN`,`QSAR_Clint`,`QSAR_Fup`)))


                        assign("chem.physical_and_invitro.data",
                               httk::add_chemtable(tmp_dawson2021,
                                                   current.table=the$chem.physical_and_invitro.data,
                                                   data.list=list(CAS='CASRN',
                                                                  Funbound.plasma = 'QSAR_Fup',
                                                                  Clint = 'QSAR_Clint'),
                                                   reference = 'Dawson 2021',
                                                   species= 'Human',
                                                   overwrite=FALSE),
                               envir=the)

                        shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 4))

                        # --- Get CAS numbers that the model for the given species and model selected will run for
                        CASnums <- getCASnums(func,species,model,defaulthuman)

                        # --- Load Honda2023 parameters
                        tmp_honda2023 <- subset(httk::honda2023.qspr, Pab.Pred.AD == 1)
                        assign("chem.physical_and_invitro.data",
                               httk::add_chemtable(tmp_honda2023,
                                                   current.table=the$chem.physical_and_invitro.data,
                                                   data.list=list(DTXSID='DTXSID',
                                                                  CAS="CAS",
                                                                  Caco2.Pab="Pab.Quant.Pred"),
                                                   reference = 'HondaUnpublished',
                                                   species="Human",
                                                   overwrite=FALSE),
                               envir=the)

                        shiny::incProgress(1/5, detail = paste("All in silico parameter sets loaded"))
                      })
  return(CASnums)
}


################################################################################
################################################################################

#' Generate a vector of CAS numbers to simulate
#'
#' @description
#' This function creates a vector of CASRNs that are available for the user-selected
#' output, species, model, and human in vitro data preference. The current function
#' is called by PreloadCompsInput().
#'
#'
#' @param func The user-selected desired output
#' @param species The user-selected species
#' @param model The user-selected species
#' @param defaulttohuman The user-selected human in vitro data preference if an
#' animal species is selected
#'
#' @return A vector of CASRNs for compounds that are available to simulate in ToCS
#' @noRd
#'
#'
getCASnums <- function(func,species,model,defaulttohuman){

  # --- Satisfy R CMD Check condition ("no visible binding...")
  # --- THESE VARIABLES ARE NOT ACTUALLY NULL
  CAS <- CAS.Checksum <- NULL

  # --- Transform default to human response
  if (defaulttohuman == "Yes"){
    defaulttohuman <- TRUE
  } else{
    defaulttohuman <- FALSE
  }

  if (model == "full_pregnancy"){
    model <- "fetal_pbtk"
  }

  # --- Attach the 'the' environment to add chem.physical_and_invitro.data data frame to path
  attach(the)

  # --- Detach the attached 'the' environment
  on.exit(detach(the))

  CASnums <- httk::get_cheminfo(species = species,
                                model = model,
                                default.to.human = defaulttohuman)

  if (func == "Parameter calculations" || (func == "Steady state concentrations" && model == "1compartment")){
    CASnums_3compss <- httk::get_cheminfo(species = species,
                                          model = "3compartmentss",
                                          default.to.human = defaulttohuman)
    CASnums <- intersect(CASnums,CASnums_3compss)
  }

  if (!is.null(CASnums)){
      chem.data <- the$chem.physical_and_invitro.data
      df <- chem.data[chem.data$CAS %in% CASnums,]
      df <- magrittr::`%>%`(df,dplyr::filter(!grepl(CAS,pattern = "CAS"),
                                             !grepl(CAS,pattern = "cas"),
                                             grepl(CAS.Checksum, pattern = "TRUE")))

      CASnums <- df$CAS
  }

  return(CASnums)
}


################################################################################
################################################################################

#' Generate the final list of available compounds available for simulation
#'
#' @description
#' This function narrows down the list of compounds from the getCASnums function
#' by taking into account any IVIVE conditions that the user selected or if the
#' user is only interested in a list of food relevant chemicals. The current
#' function is called byPreloadCompsInput().
#'
#'
#' @param CASnums The CASRN numbers of compounds that have enough data in httk
#' to simulate for the user's selected parameters
#' @param honda The selected IVIVE assumption (either NULL, Honda1, Honda2,
#' Honda3, or Honda4), if applicable
#' @param comptype The user-selected subset of compounds to search; either "Choose
#' from all available chemicals" or "Choose from only food relevant chemicals"
#'
#' @return A vector of CASRN and compound name pairings for compounds with enough
#' available data to simulate in httk
#' @noRd
#'
#'
getPiped <- function(CASnums,honda,comptype){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  CAS <- logHenry <- logWSol <- MP <- NULL

  # --- Available preloaded compounds if the Honda1 condition for IVIVE is selected
  if (is.null(CASnums)){
    piped <- NULL
  }
  # --- Available preloaded compounds otherwise
  else{

    if (comptype == "Choose from only food relevant chemicals"){
      FoodCAS <- unique(c(DirectFoodAdditives$`CAS Reg No (or other ID)`,IndirectFoodAdditives$`CAS Registry No. (or other ID)`))
      CASnums <- CASnums[CASnums %in% FoodCAS]
    }

    chemdata <- the$chem.physical_and_invitro.data

    if (honda == "Honda1"){
      chemlist <- magrittr::`%>%`(chemdata, dplyr::filter(CAS %in% CASnums,
                                                          !is.na(logHenry),
                                                          !is.na(logWSol),
                                                          !is.na(MP)))
      piped <- paste(chemlist$CAS, chemlist$Compound, sep = ", ")
    }
    else {
      chemlist <- chemdata[chemdata$CAS %in% CASnums,]
      piped <- paste(chemlist$CAS, chemlist$Compound, sep = ", ")
    }
  }

  return(piped)
}


################################################################################
################################################################################

#' Generate the user upload option to upload compounds and their physical-chemical
#' data. The current function is called by CS_UploadedData().
#'
#' @param id Shiny identifier name
#'
#' @return A browser and file upload box
#' @noRd
#'
fileInput_UploadedComps <- function(id){
  shiny::fileInput(id,
            label = "Upload a CSV file of physical and chemical data for compounds not
                    in the preloaded list (if desired). Download the 'Uploaded
                    Physical-Chemical Data File Folder' under the 'Instructions' card
                    for file formatting instructions.",
            multiple = FALSE,
            accept = c(".csv"),
            width = "100%")
}


################################################################################
################################################################################

#' Generate a drop down menu for administration method of compounds during ADME
#' simulation. The current function is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#'
#' @return A drop drop list
#' @noRd
#'
selectInput_DoseRoute <- function(id){
  shiny::selectInput(id,
              label = "Select the administration method of the compound(s).",
              choices = list("oral"),
              selected = "oral",
              width = "100%")
}


################################################################################
################################################################################

#' Generate a numeric input box for users to enter the total daily dose administered
#' during steady state concentration simulations. The current function is called
#' by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_DailyDose <- function(id,
                                   value_default = 1){
  shiny::numericInput(id,
               label = "Enter the total daily dose (in mg/kg BW).",
               value = value_default,
               min = 0,
               max = NA,
               step = 1,
               width = "100%")
}


################################################################################
################################################################################

#' Generate a drop down list of dose units to select from. The current function
#' is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_ADMEdoseunits <- function(id,
                                      choice_default = "mg/kg"){
  shiny::selectInput(id,
              label = "Select the units of the administered dose(s).",
              choices = list("mg/kg", "mg", "umol"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate a drop down list that describes the dosing frequency for ADME simulations.
#' The current function is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_NumDoses <- function(id,
                                 choice_default = "Select"){
  shiny::selectInput(id,
              label = "Select the dosing frequency.",
              choices = list("Select", "Single Dose", "Multiple Doses"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate a numeric input box that has users select the amount of initial dose
#' administered. The current function is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_InitialDose <- function(id,
                                     value_default = 1){
  shiny::numericInput(id,
               label = "Enter the dose amount administered (in the specified units).",
               value = value_default,
               min = 0,
               max = NA,
               step = 0.01,
               width = "100%")
}


################################################################################
################################################################################

#' Generate a drop down list of multiple dosing frequency options for ADME
#' simulations. The current function is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_MultipleDosesQ <- function(id,
                                       choice_default = "Select"){
  shiny::selectInput(id,
              label = "Are equal doses given evenly across a 24 hour period (i.e. 1 mg/kg BW every 8 hours)?",
              choices = list("Select", "Yes", "No"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate a numeric input box for users to enter the uniform amount of compound
#' administered during each dose of a multiple dose administration regimen for
#' ADME simulations. The current function is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_MultiDoseAmount <- function(id,
                                         value_default = 1){
  shiny::numericInput(id,
               label = "Enter the amount administered during every dose
                        (in the specified units).",
               value = value_default,
               min = 0,
               max = NA,
               step = 1,
               width = "100%")
}


################################################################################
################################################################################

#' Generate a slider input bar to select how often a uniform dose of compound
#' is administered during an ADME simulation. The current function is called by
#' MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A slider input
#' @noRd
#'
sliderInput_MultiDoseTime <- function(id,
                                      value_default = 6){
  shiny::sliderInput(id,
              label = "Select how often the above dose is administered (every ____ hours).",
              min = 0,
              max = 24,
              value = value_default,
              step = 0.5,
              width = "100%")
}


################################################################################
################################################################################

#' Generate a text input where users can enter a dosing regimen of times and doses
#' to administer of compounds selected for concentration-time profile simulations.
#' The current function is called by MS_Dosing().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A text input box
#' @noRd
#'
textInput_DoseMatrix <- function(id,
                                 value_default = ""){
  shiny::textInput(id,
            label = "Enter a list of dose amounts (in the specified units) and
                    times (in days) administered. The list must be entered as
                    time1, time2, dose1, dose2, etc. For example, if at 0, 0.5,
                    and 2 days the doses of 1, 3, and 4 mg/kg/BW were given,
                    respectively, enter 0, 0.5, 2, 1, 3, 4 in the box.",
            value = value_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of possible models to simulate
#'
#' @description
#' The current function is called by app_server().
#'
#'
#' @param func The user-selected output
#' @param spec The user-selected species
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
Model_Input <- function(func,spec, choice_default = "Select"){

  if (func == "Concentration-time profiles"){
    if (spec == "Human"){
      choice_lst <- list("Select","1compartment","3compartment","pbtk","fetal_pbtk","full_pregnancy")
    }
    else{
      choice_lst <- list("Select","1compartment","3compartment","pbtk")
    }
  }
  else if (func == "Steady state concentrations"){
    choice_lst = list("Select","1compartment","3compartment","pbtk")
  }
  else if (func == "Steady state concentrations" || func == "In vitro in vivo extrapolation (IVIVE)"){
    choice_lst = list("Select","3compartmentss","1compartment","3compartment","pbtk")
  }

  if (func == "In vitro in vivo extrapolation (IVIVE)"){
    label_txt <- "Select the model to simulate. If a species other than 'Human' is selected, '3compartmentss' must be chosen."
  }
  else{
    label_txt <- "Select a model to simulate."
  }

  if (func != "Select"){
    if (func == "Parameter calculations"){
      shiny::selectInput("model",
                         label = label_txt,
                         choices = list("Schmitt"),
                         selected = "Schmitt",
                         width = "100%")
    }
    else{
      shiny::selectInput("model",
                         label = label_txt,
                         choices = choice_lst,
                         selected = choice_default,
                         width = "100%")
    }
  }
}


################################################################################
################################################################################

#' Generates a numeric input box where the user enters the simulation time for
#' concentration-time profile simulations. The current function is called by MS_Model().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_SimTime <- function(id,
                                 value_default = 10){
  shiny::numericInput(id,
               label = "Enter the total simulation time (in days).",
               value = value_default,
               min = 1,
               max = NA,
               step = 1,
               width = "100%")
}


################################################################################
################################################################################

#' Generates a text input box for the user to enter concentration-time profile
#' output times, if desired. The current function is called by AP_OutputSpecification().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A text input box
#' @noRd
#'
textInput_OutputTimes <- function(id,
                                  value_default = ""){

  shiny::textInput(id,
            label = "Enter the times (in days) to output concentrations. Leave
                    blank if no specific times are needed. Enter a comma-separated
                    list, such as 0, 1, 2, ... signifying output 0, 1, and 2 days
                    after dosing begins.",
          value = value_default,
          width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of options for the ODE solver method for concentration-time
#' profile simulations. The current function is called by AP_ModelSolver().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_ODEmethod <- function(id,
                                  choice_default = "lsoda"){

  shiny::selectInput(id,
                     label = "Select the ODE solver method. See R documentation on the
                      'deSolve' function for method details.",
                     choices = list("lsoda", "lsode", "lsodes","lsodar","vode","daspk",
                                    "euler", "rk4", "ode23", "ode45", "radau",
                                    "bdf", "bdf_d", "adams","impAdams","impAdams_d",
                                    "iteration"),
                     selected = choice_default,
                     width = "100%")

}


################################################################################
################################################################################

#' Generates a numeric input box for the user to specify the number of solver
#' steps for the ODE solver in the concentration-time profile simulation.
#' The current function is called by AP_ModelSolver().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_SolSteps <- function(id,
                                  value_default = 4){
  shiny::numericInput(id,
               label = "Enter the number of time steps per hour for the solver to take.",
               value = value_default,
               min = 1,
               max = NA,
               step = 1,
               width = "100%")
}


################################################################################
################################################################################

#' Generates a slider input where the user can specify the ODE solver's relative
#' tolerance for concentration-time profile simulations. The current function is
#' called by AP_ModelSolver().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A slider input
#' @noRd
#'
sliderInput_RTol <- function(id,
                             value_default = -8){
  shiny::sliderInput(id,
              label = "Select the exponent (power of 10) of the relative tolerance
                      for the ODE solver.",
              min = -20,
              max = -1,
              value = value_default,
              step = 1,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a slider input where the user can specify the ODE solver's absolute
#' tolerance for concentration-time profile simulations. The current function is
#' called by AP_ModelSolver().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A slider input
#' @noRd
#'
sliderInput_ATol <- function(id,
                             value_default = -12){
  shiny::sliderInput(id,
              label = "Select the exponent (power of 10) of the desired absolute
                      tolerance for the ODE solver.",
              min = -20,
              max = -1,
              value = value_default,
              step = 1,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of options for whether the user wants to customize
#' initial condition. The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_InitialCondCustom <- function(id,
                                          choice_default = "No, keep the default amounts (default)"){
  shiny::selectInput(id,
              label = "Would you like to change the initial compound amount in
                      each compartment from its default value of 0 (no compound
                      in the compartment when the simulation begins)?",
              choices = list("No, keep the default amounts (default)",
                             "Yes, enter my own initial amounts"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generate a list of model compartment variables and names
#'
#' @description
#' A function not typically called by the user. This function creates a list of
#' compartment names and variable names of the
#' compartments in the model to be used to generate initial condition options
#' to the user during concentration-time profile simulations; used in the global.R
#' file. The current function is called by validate_text_ADME().
#'
#' @return A list of full compartment names and variable names of compartments
#' @noRd
#'
#'
names_ICs <- function(){

  var1 <- c("One_gutlumen","One_gut","One_met","One_AUC")
  comp1 <- c("gut lumen","gut","metabolized","AUC")


  var3 <- c("Three_gutlumen","Three_gut","Three_liver","Three_rest",
            "Three_met","Three_tub","Three_AUC")
  comp3 <- c("gut lumen","gut","liver","rest of body","metabolized","tubules",
             "AUC")


  varPBTK <- c("P_gutlumen","P_gut","P_liver","P_lung",
               "P_kidney","P_art","P_ven","P_rest","P_met",
               "P_tub","P_AUC")
  compPBTK <- c("gut lumen","gut","liver","lung","kidney","arteries","veins",
                "rest of body","metabolized","tubules","AUC")


  varFetal <- c("F_gutlumen","F_gut","F_liver","F_lung",
                "F_adipose","F_kidney","F_art","F_ven","F_thy",
                "F_placenta","F_rest","F_met","F_tub","F_AUC",
                "F_fgut","F_fliver","F_flung","F_fkidney",
                "F_fart","F_fven","F_fbrain","F_rest","F_AUC")
  compFetal <- c("gut lumen","gut","liver","lung","adipose","kidney","arteries",
                 "veins","thyroid","placenta","rest of body","metabolized",
                 "tubules","AUC",
                 "fetal gut","fetal liver","fetal lung","fetal kidney",
                 "fetal arteries","fetal veins","fetal brain","fetal rest of body",
                 "fetal AUC")

  names <- list(var1, var3, varPBTK, varFetal)
  comps <- list(comp1, comp3, compPBTK, compFetal)

  out <- list(names,comps)
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to specify the initial condition of
#' a particular model compartment during concentration-time profile simulations.
#' The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param compartment Model compartment name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_ICvalue <- function(id, compartment, value_default = 0){

  if (compartment == 'AUC'){
    shiny::numericInput(id,
                 label = paste("Enter the initial area under the curve (in uM*days)
                               of the", compartment, "at t = 0."),
                 value = value_default,
                 min = 0,
                 max = NA,
                 step = 1,
                 width = "100%")
  }
  else if(compartment == 'fetal AUC'){
    shiny::numericInput(id,
                 label = paste("Enter the initial area under the curve (in
                               uM*days) of the", compartment, "at t = 0."),
                 value = value_default,
                 min = 0,
                 max = NA,
                 step = 1,
                 width = "100%")
  }
  else if(compartment == 'metabolized'){
    shiny::numericInput(id,
                 label = paste("Enter the initial amount (in umol) of
                               compound(s)", compartment, "at t = 0."),
                 value = value_default,
                 min = 0,
                 max = NA,
                 step = 1,
                 width = "100%")
  }
  else{
    shiny::numericInput(id,
                 label = paste("Enter the initial amount (in umol) of compound(s)
                               in the", compartment, "at t = 0."),
                 value = value_default,
                 min = 0,
                 max = NA,
                 step = 1,
                 width = "100%")
  }
}


################################################################################
################################################################################


#' Generates a numeric input box where the user can customize the caco-2 apical-to-basal
#' membrane permeability value if desired. The current function is called by
#' AP_Bioavailability().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_CacoDefault <- function(id,
                                     value_default = 1.6){
  shiny::numericInput(id,
               label = "Enter a default value for the Caco-2 apical-to-basal
                        membrane permeability (denoted Caco2.Pab, 10^-6 cm/s).",
               value = value_default,
               min = 0,
               max = NA,
               step = 0.1,
               width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for whether
#' the caco-2 apical-to-basal value defined in numericInput_CacoDefault() should
#' be used to estimate the in vivo measured fraction of an oral dose absorbed
#' from the gut lumen into the gut if bioavailability data is unavailable.
#' The current function is called by AP_Bioavailability().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Fabs <- function(id,
                             choice_default = "Use the Caco2.Pab value selected above (default)"){
  shiny::selectInput(id,
              label = "Select whether to use the Caco2.Pab value set above
                      to estimate F_abs (the in vivo measured fraction of an
                      oral dose absorbed from the gut lumen into the gut) if
                      bioavailability data is unavailable.",
            choices = list("Use the Caco2.Pab value selected above (default)",
                           "Do not use the Caco2.Pab value selected above"),
            selected = choice_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for whether
#' the caco-2 apical-to-basal value defined in numericInput_CacoDefault() should
#' be used to estimate the in vivo measured fraction of an oral dose that passes
#' gut metabolism and clearance if bioavailability data is unavailable.
#' The current function is called by AP_Bioavailability().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Fgut <- function(id,
                             choice_default = "Use the Caco2.Pab value selected above (default)"){
  shiny::selectInput(id,
              label = "Select whether to use the Caco2.Pab value set above to
                      calculate F_gut (the in vivo measured fraction of an oral
                      dose that passes gut metabolism and clearance) if
                      bioavailability data is unavailable.",
            choices = list("Use the Caco2.Pab value selected above (default)",
                           "Do not use the Caco2.Pab value selected above"),
            selected = choice_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for whether
#' to overwrite in vivo F_abs and F_gut data (if available). The current function
#' is called by AP_Bioavailability().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Overwrite <- function(id,
                                  choice_default = "Do not overwrite in vivo values (default)"){
  shiny::selectInput(id,
              label = "Select whether to overwrite in vivo F_abs and F_gut data
                      (if available).",
            choices = list("Do not overwrite in vivo values (default)",
                           "Overwrite in vivo values"),
            selected = choice_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for
#' whether to keep F_abs and F_gut at 100% availability. The current function is
#' called by AP_Bioavailability().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Keep100 <- function(id,
                                choice_default = "Do not keep Fabs and Fgut at 100% availability (default)") {
  shiny::selectInput(id,
              label = "Select whether to keep F_abs and F_gut at 100%
                      availability (which overwrites all other bioavailability
                      parameter settings above).",
            choices = list("Do not keep Fabs and Fgut at 100% availability (default)",
                           "Keep Fabs and Fgut at 100% availability"),
            selected = choice_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of potential output concentrations for the steady
#' state concentrations output. The current function is called by AP_OutputSpecification().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_SSoutunits <- function(id,
                                   choice_default = "uM"){
  shiny::selectInput(id,
              label = "Select the output concentration units.",
              choices = list("uM", "mg/L"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of the output concentration type for the steady state
#' concentration and IVIVE outputs. The current function is called by AP_OutputSpecification().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_OutConc <- function(id,
                                choice_default = "plasma"){
  shiny::selectInput(id,
              label = "Select the output concentration type.",
              choices = list("blood", "plasma"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of tissue types for the steady state concentration
#' and IVIVE outputs. The current function is called by AP_OutputSpecification().
#'
#' @param id Shiny identifier
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Tissue <- function(id,
                               choice_default = "NULL"){
  shiny::selectInput(id,
              label = "Select a tissue you want the output concentration in.
                      Leave on 'NULL' if the whole body concentration is desired.",
              choices = list("NULL", "adipose", "bone", "brain", "gut", "heart",
                             "kidney", "liver", "lung", "muscle", "skin",
                             "spleen", "rest"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of clearance method preferences for the user to
#' select from. The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_RestrictClear <- function(id,
                                      choice_default = "Yes, include protein binding (default)"){
  shiny::selectInput(id,
              label = "Select whether protein binding is taken into account in
                      liver clearance.",
              choices = list("Yes, include protein binding (default)",
                             "No, do not include protein binding"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of chemical fraction unbound in plasma calculation
#' preferences for the user to select from. The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_AdjFub <- function(id,
                               choice_default = "Yes, adjust the fraction of unbound plasma (default)"){
  shiny::selectInput(id,
              label = "Select whether to adjust the chemical fraction unbound
                      in presence of plasma proteins for lipid binding.",
              choices = list("Yes, adjust the fraction of unbound plasma (default)",
                             "No, do not adjust the fraction of unbound plasma"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to specify the minimum acceptable
#' fraction unbound in plasma. The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_MinFub <- function(id,
                                value_default = 0.0001){
  shiny::numericInput(id,
               label = "Enter the minimum acceptable chemical fraction unbound
                        in presence of plasma proteins. All values below this
                        will be set to this value.",
               value = value_default,
               min = 0,
               max = 1,
               step = 0.0001,
               width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list where the user chooses their preference on whether
#' regressions are used in the calculation of partition coefficients.
#' The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Regression <- function(id,
                                   choice_default = "Use regressions (default)"){
  shiny::selectInput(id,
              label = "Select whether to use regressions when calculating
                      partition coefficients.",
            choices = list("Use regressions (default)",
                           "Do not use regressions"),
            selected = choice_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a numeric input box where the user specifies their preference for
#' the hepatic clearance p-value threshold. The current function is called by
#' AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_ClintPval <- function(id,
                                   value_default = 0.05){
  shiny::numericInput(id,
               label = "Enter the p-value threshold for the in vitro
                        intrinsic hepatic clearance rate where clearance assay
                        results with p-values above this threshold are set to zero.",
               value = value_default,
               min = 0,
               max = 1,
               step = 0.01,
               width = "100%")
}


################################################################################
################################################################################

#' Generates a browser and file upload box for the user to upload a CSV file of
#' bioactivity data for the selected compounds during the in vitro in vivo
#' extrapolation (IVIVE) simulation. The current function is called by CS_UploadedData().
#'
#' @param id Shiny identifier name
#'
#' @return A browser and file upload box
#' @noRd
#'
fileInput_BioactiveConc <- function(id){
  shiny::fileInput(id,
            "Upload a CSV file with in vitro bioactive concentrations (uM units)
            for all selected compounds. Download the 'Bioactivity Data File Folder'
            under the 'Instructions' card for file formatting instructions.",
            multiple = FALSE,
            accept = c(".csv"),
            width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of options for the user to select which chemical
#' concentration is treated as bioactive in vivo. The current function is called
#' by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_Bioactive <- function(id,
                                  choice_default = "Total chemical concentration (default)"){
  shiny::selectInput(id,
              label = "Select which chemical concentration is treated as
                      bioactive in vivo.",
              choices = list("Total chemical concentration (default)",
                             "Unbound (free) plasma concentration"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to enter the number of Monte Carlo
#' samples to generate during in vitro in vivo extrapolation (IVIVE) simulations.
#' The current function is called by AP_ModelConditions().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_Samples <- function(id,
                                 value_default = 1000){
  shiny::numericInput(id,
               label = "Enter the number of Monte Carlo samples generated for
                        each compound.",
               value = value_default,
               min = 1,
               max = NA,
               step = 100,
               width = "100%")

}


################################################################################
################################################################################

#' Generates a drop down list for the user to select whether the want to return
#' only a single OED estimate or all OED estimates for the in vitro in vivo
#' extrapolation (IVIVE) simulation. The current function is called by MS_Model().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_ReturnSamps <- function(id,
                                    choice_default = "Select"){
  shiny::selectInput(id,
              label = "Select whether to return all oral equivalent
                      dose (OED) samples for each compound or a selected quantile.",
            choices = list("Select", "Only return a specified dose quantile (default)",
                           "Return all OED samples (will also return the 5th dose quantile)"),
            selected = choice_default,
            width = "100%")
}


################################################################################
################################################################################

#' Generates a numeric input box for users to specify the steady state concentration
#' quantile used in the in vitro in vivo extrapolation (IVIVE) simulations.
#' The current function is called by MS_Model().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_Quantile <- function(id,
                                  value_default = 0.95){
  shiny::numericInput(id,
               label = "Enter the steady state concentration quantile (as a decimal)
                        to be used in the OED calculation. Selecting the 95th
                        concentration quantile will output the 5th OED quantile.",
               value = value_default,
               min = 0,
               max = 1,
               step = 0.05,
               width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of dose output units for the in vitro in vivo
#' extrapolation (IVIVE) simulations. The current function is called by
#' AP_OutputSpecification().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_IVIVEoutunits <- function(id,
                                      choice_default = "mgpkgpday"){
  shiny::selectInput(id,
              label = "Select the dose output units from either mg/kg BW/day
                      (mgpkgpday) (default) or umol/kg BW/day (umolpkgpday).",
              choices = list("mgpkgpday"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a drop down list of IVIVE assumptions for the user to select from
#' during the in vitro in vivo extrapolation (IVIVE) simulations.
#' The current function is called by CS_PreloadedCompounds().
#'
#' @param id Shiny identifier name
#' @param choice_default Default drop down selection option
#'
#' @return A drop down list
#' @noRd
#'
selectInput_HondaCond <- function(id,
                                  choice_default = "NULL"){
  shiny::selectInput(id,
              label = "Select an IVIVE assumption to implement. For any input
                      nominal bioactive concentration in vitro, the Honda1
                      assumption is recommended. Leave on 'NULL' if no
                      assumptions are to be applied. See the 'IVIVE Simulation Examples' vignette for the
                      description of the below assumption categories.",
              choices = list("NULL",
                             "Honda1",
                             "Honda2",
                             "Honda3",
                             "Honda4"),
              selected = choice_default,
              width = "100%")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to customize the fraction of fetal
#' bovine serum used in the in vitro assay for bioactivity data uploaded in in
#' vitro in vivo extrapolation (IVIVE) simulations. The current function is called
#' by PreloadComps_UI().
#'
#' @param id Shiny identifier name
#' @param value_default Default value
#'
#' @return A numeric input box
#' @noRd
#'
numericInput_FSBf <- function(id,
                              value_default = 0.1){
  shiny::numericInput(id,
               label = "Enter the volume fraction of fetal bovine serum used
                        in the in vitro assay.",
               value = value_default,
               min = 0,
               max = 1,
               step = 0.05,
               width = "100%")

}


################################################################################
################################################################################

#' Generates a browse and upload box for users to upload a CSV file of exposure
#' data for in vitro in vivo extrapolation (IVIVE) simulations. The current function
#' is called by CS_UploadedData().
#'
#' @param id Shiny identifier name
#'
#' @return A browse and file upload box
#' @noRd
#'
fileInput_ExposureData <- function(id){
  shiny::fileInput(id,
                   label = "Upload a CSV file of exposure data for all selected compounds (optional).
                   Download the 'Exposure Data File Folder' under the 'Instructions' card
                   for file formatting instructions.",
                   multiple = FALSE,
                   accept = c(".csv"),
                   width = "100%")
}


################################################################################
################################################################################

#' Generates a checkbox for the user to click if the user wants a log10 y-axis
#' on their respective plots. The current function is called by RS_Actions().
#'
#' @param id Shiny identifier name
#'
#' @return A checkbox input
#' @noRd
#'
checkboxInput_Log <- function(id){
  shiny::checkboxInput(id,
                label = "Check the box to display plots with a log10 scale y-axis.",
                value = FALSE,
                width = "100%")
}
