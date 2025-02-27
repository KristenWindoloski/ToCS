

###################################
# ALL PARAMETER NAMES
###################################

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

###################################
# HELPER TEXT
###################################

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

Instructions_CompSelect_Part1 <- function(){
    htmltools::h6(shiny::strong("You must choose at least one compound from the preloaded compounds,
            upload a CSV file with data for at least one compound not included
            in the preloaded compounds, or both."))
}

Instructions_CompSelect_Part2 <- function(){
  shiny::helpText("Click on the appropriate link(s) below to download guidance on how to upload data under the 'Uploaded Data' card.
                  Follow the 'Instructions' document in the downloaded folder to correctly format the file you want to upload.")
}

Instructions_RunSim <- function(){
  shiny::helpText("Click on the 'Run Simulation' button when all information has been entered.")
}

Instructions_Reset <- function(){
  shiny::helpText("Click on the button below to reset your session. This will clear all
           selections and any uploaded data, and is recommended to be done every
           time a new simulation is run.")
}


###################################
# COMMON INPUTS
###################################

selectInput_Function <- function(id){
  shiny::selectInput(id, label = "Select the desired output.",
              choices = list("Select", "Concentration-time profiles",
                             "Steady state concentrations",
                             "In vitro in vivo extrapolation (IVIVE)",
                             "Parameter calculations"),
              selected = "Select")
}

selectInput_Species <- function(id){
  shiny::selectInput(id, label = "Select the species to analyze.",
              choices = list("Select", "Dog", "Human", "Mouse", "Rabbit", "Rat"),
              selected = "Select")
}

selectInput_DefaultToHuman <- function(id){
  shiny::selectInput(id,
              label = "Do you want to use human in vitro data if in vitro data for
                      the selected species is missing?",
              choices = list("Yes", "No"),
              selected = "Yes")
}


###################################
# COMPOUND PARAMETERS
###################################

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

PreloadCompsInput <- function(func,species,defaulthuman,insilico,model,honda){

  # --- Reset the chem.physical_and_invitro.data table in case any
  # --- in silico loaded parameters or uploaded chemicals/parameters
  # --- were in the environment
  httk::reset_httk()

  # --- Load in in silico fup, clint, and caco2 if selected
  if(insilico == "Yes, load in silico parameters"){
    httk::load_sipes2017(overwrite = FALSE)
    httk::load_pradeep2020(overwrite = FALSE)
    httk::load_dawson2021(overwrite = FALSE)
    httk::load_honda2023(overwrite = FALSE)
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
  piped <- getPiped(CASnums,honda)

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

getPiped <- function(CASnums,honda){

  # --- Available preloaded compounds if the Honda1 condition for IVIVE is selected
  if (is.null(CASnums)){
    piped <- NULL
  }
  # --- Available preloaded compounds otherwise
  else{

    if (honda == "Honda1"){
      chemlist <- httk::chem.physical_and_invitro.data %>% dplyr::filter(CAS %in% CASnums,
                                                                  !is.na(logHenry),
                                                                  !is.na(httk::chem.physical_and_invitro.data$logWSol),
                                                                  !is.na(httk::chem.physical_and_invitro.data$MP))
      piped <- paste(chemlist$CAS, chemlist$Compound, sep = ", ")
    }
    else {
      chemlist <- httk::chem.physical_and_invitro.data[httk::chem.physical_and_invitro.data$CAS %in% CASnums,]
      piped <- paste(chemlist$CAS, chemlist$Compound, sep = ", ")
    }
  }

  return(piped)
}

fileInput_UploadedComps <- function(id){
  shiny::fileInput(id,
            label = "Upload a CSV file of physical and chemical data for compounds not
                    in the preloaded list (if desired). Download the 'Uploaded
                    Physical-Chemical Data File Folder' under the 'Instructions' card
                    for file formatting instructions.",
            multiple = FALSE,
            accept = c(".csv"))
}


###################################
# DOSING PARAMETERS
###################################

selectInput_DoseRoute <- function(id){
  shiny::selectInput(id,
              label = "Select the administration method of the compound(s).",
              choices = list("oral"),
              selected = "oral")
}

numericInput_DailyDose <- function(id){
  shiny::numericInput(id,
               label = "Enter the total daily dose (in mg/kg BW).",
               value = 1,
               min = 0,
               max = NA,
               step = 1)
}

selectInput_ADMEdoseunits <- function(id){
  shiny::selectInput(id,
              label = "Select the units of the administered dose(s).",
              choices = list("mg/kg", "mg", "umol"),
              selected = "mg/kg")
}

selectInput_NumDoses <- function(id){
  shiny::selectInput(id,
              label = "Select the dosing frequency.",
              choices = list("Select", "Single Dose", "Multiple Doses"),
              selected = "Select")

}

numericInput_InitialDose <- function(id){
  shiny::numericInput(id,
               label = "Enter the dose amount administered (in the specified units).",
               value = 1,
               min = 0,
               max = NA,
               step = 0.01)
}

selectInput_MultipleDosesQ <- function(id){
  shiny::selectInput(id,
              label = "Are equal doses given evenly across a 24 hour period?
                      For example, 1 mg/kg BW every 8 hours.",
                             choices = list("Select", "Yes", "No"), selected = "Select")
}

numericInput_MultiDoseAmount <- function(id){
  shiny::numericInput(id,
               label = "Enter the amount administered during every dose
                        (in the specified units).",
               value = 1,
               min = 0,
               max = NA,
               step = 1)
}

sliderInput_MultiDoseTime <- function(id){
  shiny::sliderInput(id,
              label = "Select how often the above dose is administered (every ____ hours).",
              min = 0,
              max = 24,
              value = 6,
              step = 0.5)
}

textInput_DoseMatrix <- function(id){
  shiny::textInput(id,
            label = "Enter a list of dose amounts (in the specified units) and
                    times (in days) administered. The list must be entered as
                    time1, time2, dose1, dose2, etc. For example, if at 0, 0.5,
                    and 2 days the doses of 1, 3, and 4 mg/kg/BW were given,
                    respectively, enter 0, 0.5, 2, 1, 3, 4 in the box.",
            value = "")
}


###################################
# MODEL PARAMETERS
###################################

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

numericInput_SimTime <- function(id){
  shiny::numericInput(id,
               label = "Enter the total simulation time (in days).",
               value = 10,
               min = 1,
               max = NA,
               step = 1)
}

textInput_OutputTimes <- function(id){
  shiny::textInput(id,
            label = "Enter the times (in days) to output concentrations. Leave
                    blank if no specific times are needed. Enter a comma-separated
                    list, such as 0, 1, 2, ... signifying output 0, 1, and 2 days
                    after dosing begins.",
          value = "")
}

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

numericInput_SolSteps <- function(id){
  shiny::numericInput(id,
               label = "Enter the number of time steps per hour for the solver to take.",
               value = 4,
               min = 1,
               max = NA,
               step = 1)

}

sliderInput_RTol <- function(id){
  shiny::sliderInput(id,
              label = "Select the exponent (power of 10) of the relative tolerance
                      for the ODE solver.",
              min = -20,
              max = -1,
              value = -8,
              step = 1)
}

sliderInput_ATol <- function(id){
  shiny::sliderInput(id,
              label = "Select the exponent (power of 10) of the desired absolute
                      tolerance for the ODE solver.",
              min = -20,
              max = -1,
              value = -12,
              step = 1)
}


###################################
# INITIAL CONDITION PARAMETERS
###################################

selectInput_InitialCondCustom <- function(id){
  shiny::selectInput(id,
              label = "Would you like to change the initial compound amount in
                      each compartment from its default value of 0 (no compound
                      in the compartment when the simulation begins)?",
              choices = list("No, keep the default amounts (default)",
                             "Yes, enter my own initial amounts"),
              selected = "No, keep the default amounts (default)")
}

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


###################################
# BIOAVAILABILITY PARAMETERS
###################################

numericInput_CacoDefault <- function(id){
  shiny::numericInput(id,
               label = "Enter a default value for the Caco-2 apical-to-basal
                        membrane permeability (denoted Caco2.Pab, 10^-6 cm/s).",
               value = 1.6,
               min = 0,
               max = NA,
               step = 0.1)
}

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

selectInput_Overwrite <- function(id){
  shiny::selectInput(id,
              label = "Select whether to overwrite in vivo F_abs and F_gut data
                      (if available).",
            choices = list("Do not overwrite in vivo values (default)",
                           "Overwrite in vivo values"),
            selected = "Do not overwrite in vivo values (default)")
}

selectInput_Keep100 <- function(id) {
  shiny::selectInput(id,
              label = "Select whether to keep F_abs and F_gut at 100%
                      availability (which overwrites all other bioavailability
                      parameter settings above).",
            choices = list("Do not keep Fabs and Fgut at 100% availability (default)",
                           "Keep Fabs and Fgut at 100% availability"),
            selected = "Do not keep Fabs and Fgut at 100% availability (default)")
}


###################################
# STEADY STATE MODULE PARAMETERS
###################################

selectInput_SSoutunits <- function(id){
  shiny::selectInput(id,
              label = "Select the output concentration units.",
              choices = list("uM", "mg/L"),
              selected = "uM")
}

selectInput_OutConc <- function(id){
  shiny::selectInput(id,
              label = "Select the output concentration type. Selecting 'Tissue'
                      for the 3compartmentss model will return the whole body
                      plasma concentration.",
              choices = list("blood", "plasma", "tissue"),
              selected = "plasma")
}

selectInput_Tissue <- function(id){
  shiny::selectInput(id,
              label = "Select a tissue you want the output concentration in.
                      Leave on 'NULL' if the whole body concentration is desired.",
              choices = list("NULL", "adipose", "bone", "brain", "gut", "heart",
                             "kidney", "liver", "lung", "muscle", "skin",
                             "spleen", "rest"),
              selected = "NULL")
}


###################################
# MODEL CONDITIONS PARAMETERS
###################################

selectInput_rb2p <- function(id){
  shiny::selectInput(id,
              label = "Select whether to recalculate the chemical concentration
                      blood to plasma ratio from its in vitro or estimated value
                      using the hematocrit, fraction unbound in presence of
                      plasma proteins, and red blood cell partition coefficient.",
              choices = list("Recalculate", "Do not recalculate (default)"),
              selected = "Do not recalculate (default)")
}

selectInput_RestrictClear <- function(id){
  shiny::selectInput(id,
              label = "Select whether protein binding is taken into account in
                      liver clearance.",
              choices = list("Yes, include protein binding (default)",
                             "No, do not include protein binding"),
              selected = "Yes, include protein binding (default)")
}

selectInput_AdjFub <- function(id){
  shiny::selectInput(id,
              label = "Select whether to adjust the chemical fraction unbound
                      in presence of plasma proteins for lipid binding.",
              choices = list("Yes, adjust the fraction of unbound plasma (default)",
                             "No, do not adjust the fraction of unbound plasma"),
              selected = "Yes, adjust the fraction of unbound plasma (default)")
}

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

selectInput_Regression <- function(id){
  shiny::selectInput(id,
              label = "Select whether to use regressions when calculating
                      partition coefficients.",
            choices = list("Use regressions (default)",
                           "Do not use regressions"),
            selected = "Use regressions (default)")
}

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

numericInput_Alpha <- function(id){
  shiny::numericInput(id,
               label = "Enter the Ratio of Distribution coefficient D of totally
                        charged species and that of the neutral form.",
               value = 0.001,
               min = 0,
               max = NA,
               step = 0.001)
}


###################################
# IVIVE PARAMETERS
###################################

fileInput_BioactiveConc <- function(id){
  shiny::fileInput(id,
            "Upload a CSV file with in vitro bioactive concentrations (uM units)
            for all selected compounds. Download the 'Bioactivity Data File Folder'
            under the 'Instructions' card for file formatting instructions.",
            multiple = FALSE,
            accept = c(".csv"))
}

selectInput_Bioactive <- function(id){
  shiny::selectInput(id,
              label = "Select which chemical concentration is treated as
                      bioactive in vivo.",
              choices = list("Total chemical concentration (default)",
                             "Unbound (free) plasma concentration"),
              selected = "Total chemical concentration (default)")
}

numericInput_Samples <- function(id){
  shiny::numericInput(id,
               label = "Enter the number of Monte Carlo samples generated for
                        each compound.",
               value = 1000,
               min = 1,
               max = NA,
               step = 100)

}

selectInput_ReturnSamps <- function(id){
  shiny::selectInput(id,
              label = "Select whether to return all oral equivalent
                      dose (OED) samples for each compound or a selected quantile.",
            choices = list("Select", "Only return a specified dose quantile (default)",
                           "Return all OED samples (will also return the 5th dose quantile)"),
            selected = "Select")
}

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

selectInput_IVIVEoutunits <- function(id){
  shiny::selectInput(id,
              label = "Select the dose output units from either mg/kg BW/day
                      (mgpkgpday) (default) or umol/kg BW/day (umolpkgpday).",
              choices = list("mgpkgpday", "umolpkgpday"),
              selected = "mgpkgpday")
}

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

              # label = "Select an IVIVE assumption to implement. For any input nominal concentration in vitro, the Honda1 assumption is
              #         recommended. Leave on 'NULL' if no assumptions are to be applied. Selection options are 1) Honda1 - restrictive hepatic
              #         clearance (protein binding taken into account), unbound (free) venous plasma in vivo concentration as bioactive,
              #         and the unbound (free) in vitro concentration as bioactive, 2) Honda2 - restrictive hepatic clearance (protein
              #         binding taken into account), unbound (free) venous plasma in vivo concentration as bioactive, and the nominal in
              #         vitro concentration as bioactive, 3) Honda3 - restrictive hepatic clearance (protein binding taken into account),
              #         total venous plasma in vivo concentration as bioactive, and the nominal in vitro concentration as bioactive, 4) Honda4 -
              #         Use non-restrictive hepatic clearance (protein binding not taken into account), total tissue in vivo concentration as
              #         bioactive, and the nominal in vitro concentration as bioactive",

numericInput_FSBf <- function(id){
  shiny::numericInput(id,
               label = "Enter the volume fraction of fetal bovine serum used
                        in the in vitro assay.",
               value = 0.1,
               min = 0,
               max = 1,
               step = 0.05)

}

fileInput_ExposureData <- function(id){
  shiny::fileInput(id,
                   label = "Upload a CSV file of exposure data for all selected compounds (optional).
                   Download the 'Exposure Data File Folder' under the 'Instructions' card
                   for file formatting instructions.",
                   multiple = FALSE,
                   accept = c(".csv"))
}


###################################
# PLOTTING PARAMETERS
###################################

checkboxInput_Log <- function(id){
  shiny::checkboxInput(id,
                label = "Check the box to display plots with a log10 scale y-axis.",
                value = FALSE)
}
