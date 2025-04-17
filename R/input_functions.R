

################################################################################
################################################################################

#' Compile the names of all user-selected parameters across all modules
#'
#' @return A list of parameter id names
#' @seealso [GatherInputVars()], which calls the current function
#' @export
#'
ParNames <- function(){
  GenPars <- c("func","spec","defaulttoHuman")
  CompoundSelectPars <- c("HondaIVIVE","FSBf","httkPreloadComps","file1","insilicopars")
  ModelSpecifPars <- c("doseroute","doseunits","dosenum","initdose","multdose","mult_doseamount","mult_dosetime","multdose_odd","dailydose",
                       "model","simtime","BioactiveFile","returnsamples","quantile")
  ModelConditionPars <- c("init_cond_opts",unlist(names_ICs()[[1]]),"samples","bioactiveIVIVE",
                          "Clint_Pval","AlphaPar","rb2p","restrict_clear","adj_fub","min_fub","regression")
  ModelSolverPars <- c("odemethod","solversteps","rtol","atol")
  BioavailPars <- c("caco2default","caco_fabs","caco_fgut","caco_overwriteinvivo","caco_keep100")
  OutputSpecifPars <- c("returntimes","modelSSout_units","output_concSS","tissueSS","modelIVIVEout_units","output_concIVIVE","tissueIVIVE","fileExposure")
  RunSimPars <- c("runsim","logscale","ResetButton")

  out_lst <- c(GenPars,CompoundSelectPars,ModelSpecifPars,ModelConditionPars,ModelSolverPars,BioavailPars,OutputSpecifPars,RunSimPars)
}


################################################################################
################################################################################

#' Generate text for the instructions card under the 'General Parameters' tab
#'
#' @return Text describing the major uses of ToCS, where to find additional
#' resources, and where to report bugs
#' @seealso [Instructions_GenPars()], which calls the current function
#' @export
#'
Instructions_GenPars <- function(){
  htmltools::tagList(shiny::helpText("Fill out the prompts on each of the above tabs moving left to right. Then,
                    click the 'Run Simulation' tab to run the simulation or reset
                    all selections."),
                     shiny::helpText("ToCS provides four toxicokinetic (TK) outputs:
                   1) Concentration-time profiles, which returns chemical concentrations in bodily compartments over time,
                   2) Steady state (SS) concentrations, which returns SS concentrations in bodily compartments from an oral infusion,
                   3) In vitro in vivo extrapolation (IVIVE), which returns oral equivalent doses to in vitro bioactive concentrations,
                   4) Parameter calculations, which returns elimination rates, volumes of distribution, tissue to unbound plasma
                      partition coefficients, half-lifes, and total plasma clearances."),
                     shiny::helpText("This application uses the U.S. EPA's R package 'httk'. For more information on
                    'httk', refer to https://doi.org/10.18637/jss.v079.i04 and/or https://cran.r-project.org/web/packages/httk"),
                     shiny::helpText("For additional guidance on ToCS, please refer to the vignettes
                                     (https://github.com/KristenWindoloski/ToCS/tree/main/vignettes). To report issues or suggestions
                                     for improvement, visit https://github.com/KristenWindoloski/ToCS/issues."))
}


################################################################################
################################################################################

#' Generate text to notify user that they must select or upload at least one
#' compound to simulate
#'
#' @return Text notifying the user of compound requirements
#' @seealso [CS_Instructions()], which calls the current function
#' @export
#'
Instructions_CompSelect_Part1 <- function(){
    htmltools::h6(shiny::strong("You must choose at least one compound from the preloaded compounds,
            upload a CSV file with data for at least one compound not included
            in the preloaded compounds, or both."))
}


################################################################################
################################################################################

#' Generate text to notify the user of where to see sample uploaded compound data
#'
#' @return Text notifying the user of uploaded compound data instructions
#' @seealso [CS_Instructions()], which calls the current function
#' @export
#'
Instructions_CompSelect_Part2 <- function(){
  shiny::helpText("Click on the appropriate link(s) below to download guidance on how to upload data under the 'Uploaded Data' card.
                  Follow the 'Instructions' document in the downloaded folder to correctly format the file you want to upload.")
}


################################################################################
################################################################################

#' Generate text for the 'Run Simulation' button
#'
#' @return Text notifying the user of the 'Run Simulation' button
#' @seealso [RS_Actions()], which calls the current function
#' @export
#'
Instructions_RunSim <- function(){
  shiny::helpText("Click on the 'Run Simulation' button when all information has been entered.")
}


################################################################################
################################################################################

#' Generate text for the 'Reset Session' button
#'
#' @return Text notifying the user of the 'Reset Session' button
#' @seealso [RS_Actions()], which calls the current function
#' @export
#'
Instructions_Reset <- function(){
  shiny::helpText("Click on the button below to reset your session. This will clear all
           selections and any uploaded data, and is recommended to be done every
           time a new simulation is run.")
}


################################################################################
################################################################################

#' Generate the 'Output' drop down menu
#'
#' @param id Shiny identifier name
#'
#' @return Drop down menu for the desired output of ToCS
#' @seealso [GP_Output()], which calls the current function
#' @export
#'
#' @examples selectInput_Function("func")
#'
selectInput_Function <- function(id){
  shiny::selectInput(id, label = "Select the desired output.",
              choices = list("Select", "Concentration-time profiles",
                             "Steady state concentrations",
                             "In vitro in vivo extrapolation (IVIVE)",
                             "Parameter calculations"),
              selected = "Select")
}


################################################################################
################################################################################

#' Generate the 'Species' drop down menu
#'
#' @param id Shiny identifier name
#'
#' @return Drop down menu for the desired species to simulate in ToCS
#' @seealso [GP_Species()], which calls the current function
#' @export
#'
#' @examples selectInput_Species("spec")
#'
selectInput_Species <- function(id){

  shiny::selectInput(id, label = "Select the species to analyze.",
              choices = list("Select", "Dog", "Human", "Mouse", "Rabbit", "Rat"),
              selected = "Select")
}


################################################################################
################################################################################

#' Generate the 'use of human data in place of animal data if unavailable' drop
#' down menu
#'
#' @param id Shiny identifier name
#'
#' @return Drop down menu for the human data preference use in ToCS, if applicable
#' @seealso [GP_Species()], which calls the current function
#' @export
#'
#' @examples selectInput_DefaultToHuman("defaulttohuman")
#'
selectInput_DefaultToHuman <- function(id){
  shiny::selectInput(id,
              label = "Do you want to use human in vitro data if in vitro data for
                      the selected species is missing?",
              choices = list("Yes", "No"),
              selected = "Yes")
}


################################################################################
################################################################################

#' Generate the 'In silico parameter preference' drop down menu
#'
#' @param id Shiny identifier name
#'
#' @return Drop down menu with options to either use in silico generated clint
#' (instrinic hepatic clearance) and fup (fraction unbound in plasma) in absence
#' of in vitro data for the selected species
#' @seealso [MS_Model()], which calls the current function
#' @export
#'
#' @examples selectInput_InSilicoPars("insilicopars")
#'
selectInput_InSilicoPars <- function(id){
  shiny::selectInput(id,
              label = "Select whether to use in silico generated parameters for
                      compounds with missing in vitro data. These parameters will
                      not overwrite existing in vitro data, and it will expand
                      the number of compounds available.",
              choices = list("Select",
                             "Yes, load in silico parameters",
                             "No, do not load in silico parameters"),
              selected = "Select")
}

################################################################################
################################################################################

#' Generate the drop down menu where the user can specify the types of chemicals
#' to choose from
#'
#' @param id Shiny identifier name
#'
#' @return Drop down menu with options to either set simulated chemicals as a
#' demo list, select chemicals from the entire available list, or select chemicals
#' from a list of food or food-related chemicals only
#' @seealso [CS_PreloadedCompounds()], which calls the current function
#' @export
#'
#' @examples selectInput_CompPreference("PreferredCompLst")
#'
selectInput_CompPreference <- function(id){
  shiny::selectInput(id,
                     label = "Select the types of compounds you want to simulate.",
                     choices = list("Choose from all available chemicals",
                                    "Choose from only food relevant chemicals"),
                     selected = "Choose from all available chemicals")
}


################################################################################
################################################################################

#' Generate the drop down list of preloaded compounds available to simulate in httk
#'
#' @description
#' This function creates the drop down list of preloaded compounds for users to
#' select from under the 'Preloaded Compounds' card on the 'Compound Selection'
#' tab. Four sets of in silico parameters (three for clint and fup values, one
#' for caco2 values) will be loaded if desired by the user.
#'
#'
#' @param func The user-selected desired output
#' @param species The user-selected species
#' @param defaulthuman The user-selected human in vitro data preference if an
#' animal species is selected
#' @param insilico The user-selected preference for use of in silico generated
#' parameters if in vitro data is missing
#' @param model The user-selected species
#' @param honda The selected IVIVE assumption (either NULL, Honda1, Honda2,
#' Honda3, or Honda4), if applicable
#' @param comptype The user-selected subset of compounds to search; either "Choose
#' from all available chemicals" or "Choose from only food relevant chemicals"
#'
#' @return A drop down list of preloaded available compounds within ToCS
#' @seealso [PreloadComps_UI()], which calls the current function, and [getCASnums()]
#' and [getPiped()], wich the current function calls
#' @export
#'
PreloadCompsInput <- function(func,species,defaulthuman,insilico,model,honda,comptype){

  # --- Reset the chem.physical_and_invitro.data table in case any
  # --- in silico loaded parameters or uploaded chemicals/parameters
  # --- were in the environment
  httk::reset_httk()

  # --- Load in in silico fup, clint, and caco2 if selected
  if(insilico == "Yes, load in silico parameters"){

    shiny::withProgress(message = "Loading the the of available chemicals to simulate under the 'Preloaded Compounds' card. Please wait.",
                        value = 0, {

      shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 1))
      httk::load_sipes2017(overwrite = FALSE)
      shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 2))
      httk::load_pradeep2020(overwrite = FALSE)
      shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 3))
      httk::load_dawson2021(overwrite = FALSE)
      shiny::incProgress(1/5, detail = paste("Loading in silico parameter set", 4))
      httk::load_honda2023(overwrite = FALSE)
      shiny::incProgress(1/5, detail = paste("All in silico parameter sets loaded"))
      })
  }

  # --- Transform default to human response
  if (defaulthuman == "Yes"){
    defaulttohuman <- TRUE
  } else{
    defaulttohuman <- FALSE
  }

  # --- Get CAS numbers that the model for the given species and model selected will run for
  CASnums <- getCASnums(func,species,model,defaulttohuman)

  # --- Get all available preloaded compounds
  piped <- getPiped(CASnums,honda,comptype)

  # --- Output to be returned (either nothing or a select drop down menu)
  if (is.null(piped)){
    shiny::strong(htmltools::h5("No preloaded compounds are available for the selected species
              and default to human status."))
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
                multiple = TRUE)
    }
}


################################################################################
################################################################################

#' Generate a vector of CAS numbers to simulate
#'
#' @description
#' This function creates a vector of CASRNs that are available for the user-selected
#' output, species, model, and human in vitro data preference.
#'
#'
#' @param func The user-selected desired output
#' @param species The user-selected species
#' @param model The user-selected species
#' @param defaulttohuman The user-selected human in vitro data preference if an
#' animal species is selected
#'
#' @return A vector of CASRNs for compounds that are available to simulate in ToCS
#' @seealso [PreloadCompsInput()], which calls the current function
#' @export
#'
#' @examples getCASnums("Concentration-time profiles","Human","pbtk",TRUE)
#' getCASnums("Steady state concentrations","Rat","1compartment",FALSE)
#'
getCASnums <- function(func,species,model,defaulttohuman){

  CASnums <- httk::get_cheminfo(species = species,
                                model = model,
                                default.to.human = defaulttohuman)

  if (func == "Parameter calculations" || (func == "Steady state concentrations" && model == "1compartment")){
    CASnums_3compss <- httk::get_cheminfo(species = species,
                                          model = "3compartmentss",
                                          default.to.human = defaulttohuman)
    CASnums <- intersect(CASnums,CASnums_3compss)
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
#' user is only interested in a list of food relevant chemicals.
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
#' @seealso [PreloadCompsInput()], which calls the current function, and
#' [DirectFoodAdditives()] and [IndirectFoodAdditives()] data frames, which are
#' called by the current function
#' @export
#'
#' @examples getPiped(c("135410-20-7","94-82-6","30560-19-1","71751-41-2","34256-82-1"),
#' "NULL","Choose from only food relevant chemicals")
#' getPiped(c("15972-60-8","116-06-3","834-12-8","33089-61-1","1912-24-9"),"Honda1",
#' "Choose from all available chemicals")
#'
getPiped <- function(CASnums,honda,comptype){

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

    if (honda == "Honda1"){
      chemlist <- magrittr::`%>%`(httk::chem.physical_and_invitro.data, dplyr::filter(CAS %in% CASnums,
                                                                                      !is.na(logHenry),
                                                                                      !is.na(httk::chem.physical_and_invitro.data$logWSol),
                                                                                      !is.na(httk::chem.physical_and_invitro.data$MP)))
      piped <- paste(chemlist$CAS, chemlist$Compound, sep = ", ")
    }
    else {
      chemlist <- httk::chem.physical_and_invitro.data[httk::chem.physical_and_invitro.data$CAS %in% CASnums,]
      piped <- paste(chemlist$CAS, chemlist$Compound, sep = ", ")
    }
  }

  return(piped)
}


################################################################################
################################################################################

#' Generate the user upload option to upload compounds and their physical-chemical
#' data
#'
#' @param id Shiny identifier name
#'
#' @return A browser and file upload box
#' @seealso [CS_UploadedData()], which calls the current function
#' @export
#'
#' @examples fileInput_UploadedComps("file1")
#'
fileInput_UploadedComps <- function(id){
  shiny::fileInput(id,
            label = "Upload a CSV file of physical and chemical data for compounds not
                    in the preloaded list (if desired). Download the 'Uploaded
                    Physical-Chemical Data File Folder' under the 'Instructions' card
                    for file formatting instructions.",
            multiple = FALSE,
            accept = c(".csv"))
}


################################################################################
################################################################################

#' Generate a drop down menu for administration method of compounds during ADME
#' simulation
#'
#' @param id Shiny identifier name
#'
#' @return A drop drop list
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples selectInput_DoseRoute("doseroute")
#'
selectInput_DoseRoute <- function(id){
  shiny::selectInput(id,
              label = "Select the administration method of the compound(s).",
              choices = list("oral"),
              selected = "oral")
}


################################################################################
################################################################################

#' Generate a numeric input box for users to enter the total daily dose administered
#' during steady state concentration simulations
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples numericInput_DailyDose("dailydose")
#'
numericInput_DailyDose <- function(id){
  shiny::numericInput(id,
               label = "Enter the total daily dose (in mg/kg BW).",
               value = 1,
               min = 0,
               max = NA,
               step = 1)
}


################################################################################
################################################################################

#' Generate a drop down list of dose units to select from
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples selectInput_ADMEdoseunits("doseunits")
#'
selectInput_ADMEdoseunits <- function(id){
  shiny::selectInput(id,
              label = "Select the units of the administered dose(s).",
              choices = list("mg/kg", "mg", "umol"),
              selected = "mg/kg")
}


################################################################################
################################################################################

#' Generate a drop down list that describes the dosing frequency for ADME simulations
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples selectInput_NumDoses("dosenum")
#'
selectInput_NumDoses <- function(id){
  shiny::selectInput(id,
              label = "Select the dosing frequency.",
              choices = list("Select", "Single Dose", "Multiple Doses"),
              selected = "Select")
}


################################################################################
################################################################################

#' Generate a numeric input box that has users select the amount of initial dose
#' administered
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples numericInput_InitialDose("initdose")
#'
numericInput_InitialDose <- function(id){
  shiny::numericInput(id,
               label = "Enter the dose amount administered (in the specified units).",
               value = 1,
               min = 0,
               max = NA,
               step = 0.01)
}


################################################################################
################################################################################

#' Generate a drop down list of multiple dosing frequency options for ADME
#' simulations
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples selectInput_MultipleDosesQ("multdose")
#'
selectInput_MultipleDosesQ <- function(id){
  shiny::selectInput(id,
              label = "Are equal doses given evenly across a 24 hour period?
                      For example, 1 mg/kg BW every 8 hours.",
                             choices = list("Select", "Yes", "No"), selected = "Select")
}


################################################################################
################################################################################

#' Generate a numeric input box for users to enter the uniform amount of compound
#' administered during each dose of a multiple dose administration regimen for
#' ADME simulations
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples numericInput_MultiDoseAmount("mult_doseamount")
#'
numericInput_MultiDoseAmount <- function(id){
  shiny::numericInput(id,
               label = "Enter the amount administered during every dose
                        (in the specified units).",
               value = 1,
               min = 0,
               max = NA,
               step = 1)
}


################################################################################
################################################################################

#' Generate a slider input bar to select how often a uniform dose of compound
#' is administered during an ADME simulation
#'
#' @param id Shiny identifier name
#'
#' @return A slider input
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples sliderInput_MultiDoseTime("mult_dosetime")
#'
sliderInput_MultiDoseTime <- function(id){
  shiny::sliderInput(id,
              label = "Select how often the above dose is administered (every ____ hours).",
              min = 0,
              max = 24,
              value = 6,
              step = 0.5)
}


################################################################################
################################################################################

#' Generate a text input where users can enter a dosing regimen of times and doses
#' to administer of compounds selected for concentration-time profile simulations
#'
#' @param id Shiny identifier name
#'
#' @return A text input box
#' @seealso [MS_Dosing()], which calls the current function
#' @export
#'
#' @examples textInput_DoseMatrix("multdose_odd")
#'
textInput_DoseMatrix <- function(id){
  shiny::textInput(id,
            label = "Enter a list of dose amounts (in the specified units) and
                    times (in days) administered. The list must be entered as
                    time1, time2, dose1, dose2, etc. For example, if at 0, 0.5,
                    and 2 days the doses of 1, 3, and 4 mg/kg/BW were given,
                    respectively, enter 0, 0.5, 2, 1, 3, 4 in the box.",
            value = "")
}


################################################################################
################################################################################

#' Generates a drop down list of possible models to simulate
#'
#' @param func The user-selected output
#' @param spec The user-selected species
#'
#' @return A drop down list
#' @seealso [app_server()], which calls the current function
#' @export
#'
#' @examples Model_Input("Concentration-time profiles","Human")
#' Model_Input("Parameter calculations","Rat")
#'
Model_Input <- function(func,spec){

  if (func == "Concentration-time profiles"){
    if (spec == "Human"){
      choice_lst <- list("Select","1compartment","3compartment","pbtk","fetal_pbtk")
    }
    else{
      choice_lst <- list("Select","1compartment","3compartment","pbtk")
    }
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
      shiny::selectInput("model",label = label_txt,choices = list("Select","Schmitt"),selected = "Select")
    }
    else{
      shiny::selectInput("model",label = label_txt,choices = choice_lst,selected = "Select")
    }
  }
}


################################################################################
################################################################################

#' Generates a numeric input box where the user enters the simulation time for
#' concentration-time profile simulations
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [MS_Model()], which calls the current function
#' @export
#'
#' @examples numericInput_SimTime("simtime")
numericInput_SimTime <- function(id){
  shiny::numericInput(id,
               label = "Enter the total simulation time (in days).",
               value = 10,
               min = 1,
               max = NA,
               step = 1)
}


################################################################################
################################################################################

#' Generates a text input box for the user to enter concentration-time profile
#' output times, if desired
#'
#' @param id Shiny identifier name
#'
#' @return A text input box
#' @seealso [AP_OutputSpecification()], which calls the current function
#' @export
#'
#' @examples textInput_OutputTimes("returntimes")
textInput_OutputTimes <- function(id){
  shiny::textInput(id,
            label = "Enter the times (in days) to output concentrations. Leave
                    blank if no specific times are needed. Enter a comma-separated
                    list, such as 0, 1, 2, ... signifying output 0, 1, and 2 days
                    after dosing begins.",
          value = "")
}


################################################################################
################################################################################

#' Generates a drop down list of options for the ODE solver method for concentration-time
#' profile simulations
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelSolver()], which calls the current function
#' @export
#'
#' @examples selectInput_ODEmethod("odemethod")
#'
selectInput_ODEmethod <- function(id){
  shiny::selectInput(id,
              label = "Select the ODE solver method. See R documentation on the
                      'deSolve' function for method details.",
              choices = list("lsoda", "lsode", "lsodes","lsodar","vode","daspk",
                             "euler", "rk4", "ode23", "ode45", "radau",
                             "bdf", "bdf_d", "adams","impAdams","impAdams_d",
                             "iteration"),
              selected = "lsoda")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to specify the number of solver
#' steps for the ODE solver in the concentration-time profile simulation
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [AP_ModelSolver()], which calls the current function
#' @export
#'
#' @examples numericInput_SolSteps("solversteps")
#'
numericInput_SolSteps <- function(id){
  shiny::numericInput(id,
               label = "Enter the number of time steps per hour for the solver to take.",
               value = 4,
               min = 1,
               max = NA,
               step = 1)
}


################################################################################
################################################################################

#' Generates a slider input where the user can specify the ODE solver's relative
#' tolerance for concentration-time profile simulations
#'
#' @param id Shiny identifier name
#'
#' @return A slider input
#' @seealso [AP_ModelSolver()], which calls the current function
#' @export
#'
#' @examples sliderInput_RTol("rtol")
sliderInput_RTol <- function(id){
  shiny::sliderInput(id,
              label = "Select the exponent (power of 10) of the relative tolerance
                      for the ODE solver.",
              min = -20,
              max = -1,
              value = -8,
              step = 1)
}


################################################################################
################################################################################

#' Generates a slider input where the user can specify the ODE solver's absolute
#' tolerance for concentration-time profile simulations
#'
#' @param id Shiny identifier name
#'
#' @return A slider input
#' @seealso [AP_ModelSolver()], which calls the current function
#' @export
#'
#' @examples sliderInput_ATol("atol")
#'
sliderInput_ATol <- function(id){
  shiny::sliderInput(id,
              label = "Select the exponent (power of 10) of the desired absolute
                      tolerance for the ODE solver.",
              min = -20,
              max = -1,
              value = -12,
              step = 1)
}


################################################################################
################################################################################

#' Generates a drop down list of options for whether the user wants to customize
#' initial condition
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples selectInput_InitialCondCustom("init_cond_opts")
#'
selectInput_InitialCondCustom <- function(id){
  shiny::selectInput(id,
              label = "Would you like to change the initial compound amount in
                      each compartment from its default value of 0 (no compound
                      in the compartment when the simulation begins)?",
              choices = list("No, keep the default amounts (default)",
                             "Yes, enter my own initial amounts"),
              selected = "No, keep the default amounts (default)")
}


################################################################################
################################################################################

#' Generate a vector of model compartment variables and names
#'
#' @description
#' This function creates a list of compartment names and variable names of the
#' compartments in the model to be used to generate initial condition options
#' to the user during concentration-time profile simulations; used in the global.R \
#' file.
#'
#' @return A list of full compartment names and variable names of compartments
#' @seealso [validate_text_ADME()], which calls the current function
#' @export
#'
#' @examples names_ICs()
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
#' a particular model compartment during concentration-time profile simulations
#'
#' @param id Shiny identifier name
#' @param compartment Model compartment name
#'
#' @return A numeric input box
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples numericInput_ICvalue("One_gut","gut")
#' numericInput_ICvalue("Three_AUC","AUC")
#' numericInput_ICvalue("P_liver","liver")
#'
numericInput_ICvalue <- function(id, compartment){

  if (compartment == 'AUC'){
    shiny::numericInput(id,
                 label = paste("Enter the initial area under the curve (in uM*days)
                               of the", compartment, "at t = 0."),
                 value = 0,
                 min = 0,
                 max = NA,
                 step = 1)
  }
  else if(compartment == 'fetal AUC'){
    shiny::numericInput(id,
                 label = paste("Enter the initial area under the curve (in
                               uM*days) of the", compartment, "at t = 0."),
                 value = 0,
                 min = 0,
                 max = NA,
                 step = 1)
  }
  else if(compartment == 'metabolized'){
    shiny::numericInput(id,
                 label = paste("Enter the initial amount (in umol) of
                               compound(s)", compartment, "at t = 0."),
                 value = 0,
                 min = 0,
                 max = NA,
                 step = 1)
  }
  else{
    shiny::numericInput(id,
                 label = paste("Enter the initial amount (in umol) of compound(s)
                               in the", compartment, "at t = 0."),
                 value = 0,
                 min = 0,
                 max = NA,
                 step = 1)
  }
}


################################################################################
################################################################################


#' Generates a numeric input box where the user can customize the caco-2 apical-to-basal
#' membrane permeability value if desired
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [AP_Bioavailability()], which calls the current function
#' @export
#'
#' @examples numericInput_CacoDefault("caco2default")
#'
numericInput_CacoDefault <- function(id){
  shiny::numericInput(id,
               label = "Enter a default value for the Caco-2 apical-to-basal
                        membrane permeability (denoted Caco2.Pab, 10^-6 cm/s).",
               value = 1.6,
               min = 0,
               max = NA,
               step = 0.1)
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for whether
#' the caco-2 apical-to-basal value defined in numericInput_CacoDefault() should
#' be used to estimate the in vivo measured fraction of an oral dose absorbed
#' from the gut lumen into the gut if bioavailability data is unavailable
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_Bioavailability()], which calls the current function
#' @export
#'
#' @examples selectInput_Fabs("caco_fabs")
#'
selectInput_Fabs <- function(id){
  shiny::selectInput(id,
              label = "Select whether to use the Caco2.Pab value set above
                      to estimate F_abs (the in vivo measured fraction of an
                      oral dose absorbed from the gut lumen into the gut) if
                      bioavailability data is unavailable.",
            choices = list("Use the Caco2.Pab value selected above (default)",
                           "Do not use the Caco2.Pab value selected above"),
            selected = "Use the Caco2.Pab value selected above (default)")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for whether
#' the caco-2 apical-to-basal value defined in numericInput_CacoDefault() should
#' be used to estimate the in vivo measured fraction of an oral dose that passes
#' gut metabolism and clearance if bioavailability data is unavailable
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_Bioavailability()], which calls the current function
#' @export
#'
#' @examples selectInput_Fgut("caco_fgut")
#'
selectInput_Fgut <- function(id){
  shiny::selectInput(id,
              label = "Select whether to use the Caco2.Pab value set above to
                      calculate F_gut (the in vivo measured fraction of an oral
                      dose that passes gut metabolism and clearance) if
                      bioavailability data is unavailable.",
            choices = list("Use the Caco2.Pab value selected above (default)",
                           "Do not use the Caco2.Pab value selected above"),
            selected = "Use the Caco2.Pab value selected above (default)")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for whether
#' to overwrite in vivo F_abs and F_gut data (if available)
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_Bioavailability()], which calls the current function
#' @export
#'
#' @examples selectInput_Overwrite("caco_overwriteinvivo")
#'
selectInput_Overwrite <- function(id){
  shiny::selectInput(id,
              label = "Select whether to overwrite in vivo F_abs and F_gut data
                      (if available).",
            choices = list("Do not overwrite in vivo values (default)",
                           "Overwrite in vivo values"),
            selected = "Do not overwrite in vivo values (default)")
}


################################################################################
################################################################################

#' Generates a drop down list where the user can select their preference for
#' whether to keep F_abs and F_gut at 100% availability
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_Bioavailability()], which calls the current function
#' @export
#'
#' @examples selectInput_Keep100("caco_keep100")
#'
selectInput_Keep100 <- function(id) {
  shiny::selectInput(id,
              label = "Select whether to keep F_abs and F_gut at 100%
                      availability (which overwrites all other bioavailability
                      parameter settings above).",
            choices = list("Do not keep Fabs and Fgut at 100% availability (default)",
                           "Keep Fabs and Fgut at 100% availability"),
            selected = "Do not keep Fabs and Fgut at 100% availability (default)")
}


################################################################################
################################################################################

#' Generates a drop down list of potential output concentrations for the steady
#' state concentrations output
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_OutputSpecification()], which calls the current function
#' @export
#'
#' @examples selectInput_SSoutunits("modelSSout_units")
#'
selectInput_SSoutunits <- function(id){
  shiny::selectInput(id,
              label = "Select the output concentration units.",
              choices = list("uM", "mg/L"),
              selected = "uM")
}


################################################################################
################################################################################

#' Generates a drop down list of the output concentration type for the steady state
#' concentration and IVIVE outputs
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_OutputSpecification()], which calls the current function
#' @export
#'
#' @examples selectInput_OutConc("output_concSS")
#' selectInput_OutConc("output_concIVIVE")
#'
selectInput_OutConc <- function(id){
  shiny::selectInput(id,
              label = "Select the output concentration type. Selecting 'Tissue'
                      for the 3compartmentss model will return the whole body
                      plasma concentration.",
              choices = list("blood", "plasma", "tissue"),
              selected = "plasma")
}


################################################################################
################################################################################

#' Generates a drop down list of tissue types for the steady state concentration
#' and IVIVE outputs
#'
#' @param id Shiny identifier
#'
#' @return A drop down list
#' @seealso [AP_OutputSpecification()], which calls the current function
#' @export
#'
#' @examples selectInput_Tissue("tissueSS")
#' selectInput_Tissue("tissueIVIVE")
#'
selectInput_Tissue <- function(id){
  shiny::selectInput(id,
              label = "Select a tissue you want the output concentration in.
                      Leave on 'NULL' if the whole body concentration is desired.",
              choices = list("NULL", "adipose", "bone", "brain", "gut", "heart",
                             "kidney", "liver", "lung", "muscle", "skin",
                             "spleen", "rest"),
              selected = "NULL")
}


################################################################################
################################################################################

#' Generates a drop down list where the user selects its ratio of blood to plasma
#' preferences for the specified model
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples selectInput_rb2p("rb2p")
#'
selectInput_rb2p <- function(id){
  shiny::selectInput(id,
              label = "Select whether to recalculate the chemical concentration
                      blood to plasma ratio from its in vitro or estimated value
                      using the hematocrit, fraction unbound in presence of
                      plasma proteins, and red blood cell partition coefficient.",
              choices = list("Recalculate", "Do not recalculate (default)"),
              selected = "Do not recalculate (default)")
}


################################################################################
################################################################################

#' Generates a drop down list of clearance method preferences for the user to
#' select from
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples selectInput_RestrictClear("restrict_clear")
#'
selectInput_RestrictClear <- function(id){
  shiny::selectInput(id,
              label = "Select whether protein binding is taken into account in
                      liver clearance.",
              choices = list("Yes, include protein binding (default)",
                             "No, do not include protein binding"),
              selected = "Yes, include protein binding (default)")
}


################################################################################
################################################################################

#' Generates a drop down list of chemical fraction unbound in plasma calculation
#' preferences for the user to select from
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples selectInput_AdjFub("adj_fub")
#'
selectInput_AdjFub <- function(id){
  shiny::selectInput(id,
              label = "Select whether to adjust the chemical fraction unbound
                      in presence of plasma proteins for lipid binding.",
              choices = list("Yes, adjust the fraction of unbound plasma (default)",
                             "No, do not adjust the fraction of unbound plasma"),
              selected = "Yes, adjust the fraction of unbound plasma (default)")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to specify the minimum acceptable
#' fraction unbound in plasma.
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples numericInput_MinFub("min_fub")
#'
numericInput_MinFub <- function(id){
  shiny::numericInput(id,
               label = "Enter the minimum acceptable chemical fraction unbound
                        in presence of plasma proteins. All values below this
                        will be set to this value.",
               value = 0.0001,
               min = 0,
               max = 1,
               step = 0.0001)
}


################################################################################
################################################################################

#' Generates a drop down list where the user chooses their preference on whether
#' regressions are used in the calculation of partition coefficients
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples selectInput_Regression("regression")
#'
selectInput_Regression <- function(id){
  shiny::selectInput(id,
              label = "Select whether to use regressions when calculating
                      partition coefficients.",
            choices = list("Use regressions (default)",
                           "Do not use regressions"),
            selected = "Use regressions (default)")
}


################################################################################
################################################################################

#' Generates a numeric input box where the user specifies their preference for
#' the hepatic clearance p-value threshold
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples numericInput_ClintPval("Clint_Pval")
#'
numericInput_ClintPval <- function(id){
  shiny::numericInput(id,
               label = "Enter the p-value threshold for the in vitro
                        intrinsic hepatic clearance rate where clearance assay
                        results with p-values above this threshold are set to zero.",
               value = 0.05,
               min = 0,
               max = 1,
               step = 0.01)
}


################################################################################
################################################################################

#' Generates a numeric input box where the user selects their preference on the
#' ratio of distribution coefficient for totally charges species and that of the
#' neutral form
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples numericInput_Alpha("AlphaPar")
#'
numericInput_Alpha <- function(id){
  shiny::numericInput(id,
               label = "Enter the Ratio of Distribution coefficient D of totally
                        charged species and that of the neutral form.",
               value = 0.001,
               min = 0,
               max = NA,
               step = 0.001)
}


################################################################################
################################################################################

#' Generates a browser and file upload box for the user to upload a CSV file of
#' bioactivity data for the selected compounds during the in vitro in vivo
#' extrapolation (IVIVE) simulation
#'
#' @param id Shiny identifier name
#'
#' @return A browser and file upload box
#' @seealso [CS_UploadedData()], which calls the current function
#' @export
#'
#' @examples fileInput_BioactiveConc("BioactiveFile")
#'
fileInput_BioactiveConc <- function(id){
  shiny::fileInput(id,
            "Upload a CSV file with in vitro bioactive concentrations (uM units)
            for all selected compounds. Download the 'Bioactivity Data File Folder'
            under the 'Instructions' card for file formatting instructions.",
            multiple = FALSE,
            accept = c(".csv"))
}


################################################################################
################################################################################

#' Generates a drop down list of options for the user to select which chemical
#' concentration is treated as bioactive in vivo
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples selectInput_Bioactive("bioactiveIVIVE")
selectInput_Bioactive <- function(id){
  shiny::selectInput(id,
              label = "Select which chemical concentration is treated as
                      bioactive in vivo.",
              choices = list("Total chemical concentration (default)",
                             "Unbound (free) plasma concentration"),
              selected = "Total chemical concentration (default)")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to enter the number of Monte Carlo
#' samples to generate during in vitro in vivo extrapolation (IVIVE) simulations
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [AP_ModelConditions()], which calls the current function
#' @export
#'
#' @examples numericInput_Samples("samples")
#'
numericInput_Samples <- function(id){
  shiny::numericInput(id,
               label = "Enter the number of Monte Carlo samples generated for
                        each compound.",
               value = 1000,
               min = 1,
               max = NA,
               step = 100)

}


################################################################################
################################################################################

#' Generates a drop down list for the user to select whether the want to return
#' only a single OED estimate or all OED estimates for the in vitro in vivo
#' extrapolation (IVIVE) simulation
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [MS_Model()], which calls the current function
#' @export
#'
#' @examples selectInput_ReturnSamps("returnsamples")
#'
selectInput_ReturnSamps <- function(id){
  shiny::selectInput(id,
              label = "Select whether to return all oral equivalent
                      dose (OED) samples for each compound or a selected quantile.",
            choices = list("Select", "Only return a specified dose quantile (default)",
                           "Return all OED samples (will also return the 5th dose quantile)"),
            selected = "Select")
}


################################################################################
################################################################################

#' Generates a numeric input box for users to specify the steady state concentration
#' quantile used in the in vitro in vivo extrapolation (IVIVE) simulations
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [MS_Model()], which calls the current function
#' @export
#'
#' @examples numericInput_Quantile("quantile")
#'
numericInput_Quantile <- function(id){
  shiny::numericInput(id,
               label = "Enter the steady state concentration quantile (as a decimal)
                        to be used in the OED calculation. Selecting the 95th
                        concentration quantile will output the 5th OED quantile.",
               value = 0.95,
               min = 0,
               max = 1,
               step = 0.05)
}


################################################################################
################################################################################

#' Generates a drop down list of dose output units for the in vitro in vivo
#' extrapolation (IVIVE) simulations
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [AP_OutputSpecification()], which calls the current function
#' @export
#'
#' @examples selectInput_IVIVEoutunits("modelIVIVEout_units")
#'
selectInput_IVIVEoutunits <- function(id){
  shiny::selectInput(id,
              label = "Select the dose output units from either mg/kg BW/day
                      (mgpkgpday) (default) or umol/kg BW/day (umolpkgpday).",
              choices = list("mgpkgpday", "umolpkgpday"),
              selected = "mgpkgpday")
}


################################################################################
################################################################################

#' Generates a drop down list of IVIVE assumptions for the user to select from
#' during the in vitro in vivo extrapolation (IVIVE) simulations
#'
#' @param id Shiny identifier name
#'
#' @return A drop down list
#' @seealso [CS_PreloadedCompounds()], which calls the current function
#' @export
#'
#' @examples selectInput_HondaCond("HondaIVIVE")
#'
selectInput_HondaCond <- function(id){
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
              selected = "NULL")
}


################################################################################
################################################################################

#' Generates a numeric input box for the user to customize the fraction of fetal
#' bovine serum used in the in vitro assay for bioactivity data uploaded in in
#' vitro in vivo extrapolation (IVIVE) simulations
#'
#' @param id Shiny identifier name
#'
#' @return A numeric input box
#' @seealso [PreloadComps_UI()], which calls the current function
#' @export
#'
#' @examples numericInput_FSBf("FSBf")
#'
numericInput_FSBf <- function(id){
  shiny::numericInput(id,
               label = "Enter the volume fraction of fetal bovine serum used
                        in the in vitro assay.",
               value = 0.1,
               min = 0,
               max = 1,
               step = 0.05)

}


################################################################################
################################################################################

#' Generates a browse and upload box for users to upload a CSV file of exposure
#' data for in vitro in vivo extrapolation (IVIVE) simulations
#'
#' @param id Shiny identifier name
#'
#' @return A browse and file upload box
#' @seealso [CS_UploadedData()], which calls the current function
#' @export
#'
#' @examples fileInput_ExposureData("fileExposure")
#'
fileInput_ExposureData <- function(id){
  shiny::fileInput(id,
                   label = "Upload a CSV file of exposure data for all selected compounds (optional).
                   Download the 'Exposure Data File Folder' under the 'Instructions' card
                   for file formatting instructions.",
                   multiple = FALSE,
                   accept = c(".csv"))
}


################################################################################
################################################################################

#' Generates a checkbox for the user to click if the user wants a log10 y-axis
#' on their respective plots
#'
#' @param id Shiny identifier name
#'
#' @return A checkbox input
#' @seealso [RS_Actions()], which calls the current function
#' @export
#'
#' @examples checkboxInput_Log("logscale")
#'
checkboxInput_Log <- function(id){
  shiny::checkboxInput(id,
                label = "Check the box to display plots with a log10 scale y-axis.",
                value = FALSE)
}
