
######################################################
# UI CODE SUB FUNCTIONS
######################################################

################################################################################
################################################################################

#' User interface for the 'Instructions' card on the 'General Parameters' tab
#'
#' @return Text with instructions for the user on how to use the app, where to get
#' help, and where to report bugs
#' @seealso [app_ui()], which calls the current function, and [Instructions_GenPars()],
#' which is called by the current function
#' @export
#'
GP_Instructions <- function(){
  bslib::card(bslib::card_header("INSTRUCTIONS"),
              Instructions_GenPars())
}

################################################################################
################################################################################

#' User interface for the 'Output' card on the 'General Parameters' tab
#'
#' @return The 'Output' card with the function selection option
#' @seealso [app_ui()], which calls the current function, and [selectInput_Function()],
#' which is called by the current function
#' @export
#'
GP_Output <- function(){
  bslib::card(bslib::card_header("OUTPUT"),
              selectInput_Function("func"))
}


################################################################################
################################################################################

#' User interface for the 'Species' card on the 'General Parameters' tab
#'
#' @return The 'Species' card with species selection options
#' @seealso [app_ui()], which calls the current function, and [selectInput_Species()]
#' and [selectInput_DefaultToHuman()], which are called by the current function
#' @export
#'
GP_Species <- function(){
  bslib::card(bslib::card_header("SPECIES"),
              selectInput_Species("spec"),
              shiny::conditionalPanel(condition = "input.spec == 'Dog' || input.spec == 'Mouse' || input.spec == 'Rabbit' || input.spec == 'Rat'",
                                      selectInput_DefaultToHuman("defaulttoHuman")))
}


################################################################################
################################################################################

#' User interface for the 'Dosing' card on the 'Model Specifications' tab
#'
#' @return The 'Dosing' card with dosing selection options depending on the
#' module function chosen
#' @seealso [app_ui()], which calls the current function, and [selectInput_DoseRoute()],
#' [selectInput_ADMEdoseunits()], [selectInput_NumDoses()], [numericInput_InitialDose()],
#' [selectInput_MultipleDosesQ()], [numericInput_MultiDoseAmount()],
#' [sliderInput_MultiDoseTime()], [textInput_DoseMatrix()], and
#'  [numericInput_DailyDose()], which are called by the current function
#' @export
#'
MS_Dosing <- function(){
  bslib::card(bslib::card_header("DOSING"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                                      selectInput_DoseRoute("doseroute"),
                                      selectInput_ADMEdoseunits("doseunits"),
                                      selectInput_NumDoses("dosenum"),
                                      shiny::conditionalPanel(condition = "input.dosenum == 'Single Dose'",
                                                              numericInput_InitialDose("initdose")),
                                      shiny::conditionalPanel(condition = "input.dosenum == 'Multiple Doses'",
                                                              selectInput_MultipleDosesQ("multdose")),
                                      shiny::conditionalPanel(condition = "input.multdose == 'Yes'",
                                                              numericInput_MultiDoseAmount("mult_doseamount"),
                                                              sliderInput_MultiDoseTime("mult_dosetime")),
                                      shiny::conditionalPanel(condition = "input.dosenum == 'Multiple Doses' && input.multdose == 'No'",
                                                              textInput_DoseMatrix("multdose_odd"))),
              shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations'",
                                      numericInput_DailyDose("dailydose")),
              shiny::conditionalPanel(condition = "(input.func == 'In vitro in vivo extrapolation (IVIVE)' || input.func == 'Parameter calculations')",
                                      shiny::helpText("No options for this category.")))
}


################################################################################
################################################################################

#' User interface for the 'Model' card on the 'Model Specifications' tab
#'
#' @return The 'Model' card with model selection options depending on the module
#' function chosen
#' @seealso [app_ui()], which calls the current function, and [selectInput_InSilicoPars()],
#' [numericInput_SimTime()], [selectInput_ReturnSamps()], and [numericInput_Quantile()],
#' which are called by the current function
#' @export
#'
MS_Model <- function(){
  bslib::card(bslib::card_header("MODEL"),
              shiny::conditionalPanel(condition = "input.func != 'Select'",
                                      shiny::uiOutput("Model"),
                                      selectInput_InSilicoPars("insilicopars")),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                                      numericInput_SimTime("simtime")),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      selectInput_ReturnSamps("returnsamples"),
                                      shiny::conditionalPanel(condition = "input.returnsamples == 'Only return a specified dose quantile (default)'",
                                                              numericInput_Quantile("quantile"))))
}


################################################################################
################################################################################

#' User interface for the 'Instructions' card on the 'Compound Selection' tab
#'
#' @return The 'Instructions' card with instructions for selecting and uploading
#' compound data, including folders with sample data
#' @seealso [app_ui()], which calls the current function, and [Instructions_CompSelect_Part1()],
#' which is called by the current function
#' @export
#'
CS_Instructions <- function(){
  bslib::card(bslib::card_header("INSTRUCTIONS"),
              shiny::conditionalPanel(condition = "input.httkPreloadComps == '' && output.fileUploaded == false && input.model != 'Select' && input.insilicopars != 'Select'",
                                      Instructions_CompSelect_Part1()),
              Instructions_CompSelect_Part2(),
              shiny::tags$a("Uploaded Physical-Chemical Data File Folder", href="UploadedPhysicalChemicalDataFileFolder.zip"),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      shiny::tags$a("Bioactivity Data File Folder", href="BioactivityDataFileFolder.zip"),
                                      shiny::tags$a("Exposure Data File Folder", href="ExposureDataFileFolder.zip"))
              )
}


################################################################################
################################################################################

#' User interface for the 'Preloaded Compounds' card on the 'Compound Selection'
#' tab
#'
#' @return The 'Preloaded Compounds' card that allows the user to select desired
#' chemicals to simulate from a list as well as any chemical-related simulation
#' preferences
#' @seealso [app_ui()], which calls the current function, and [selectInput_HondaCond()]
#' and [selectInput_CompPreference()], which are called by the current function
#' @export
#'
CS_PreloadedCompounds <- function(){
  bslib::card(bslib::card_header("PRELOADED COMPOUNDS"),
              shiny::conditionalPanel(condition = "input.func !== 'Select' && input.spec != 'Select' && input.defaultoHuman != 'Select'",
                                      shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                                              selectInput_HondaCond("HondaIVIVE")),
                                      selectInput_CompPreference("CompType"),
                                      shiny::uiOutput("PreloadedComps")))
}


################################################################################
################################################################################

#' User interface for the 'Uploaded Data' card on the 'Compound Selection' tab
#'
#' @return The 'Uploaded Data' card which gives the user the option to upload a
#' file with new chemicals and their data. If the IVIVE module is chosen, then
#' users can also upload bioactivity and exposure data.
#' @seealso [app_ui()], which calls the current function, and [fileInput_BioactiveConc()]
#' and [fileInput_ExposureData()], which are called by the current function
#' @export
#'
CS_UploadedData <- function(){
  bslib::card(bslib::card_header("UPLOADED DATA"),
              fileInput_UploadedComps("file1"),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      fileInput_BioactiveConc("BioactiveFile"),
                                      fileInput_ExposureData("fileExposure"))
              )
}


################################################################################
################################################################################

#' User interface for the 'Model Conditions' card on the 'Advanced Parameters'
#' tab
#'
#' @return The 'Model Conditions' card which allows the user to select various
#' different model conditions depending upon the module function chosen
#' @seealso [app_ui()], which calls the current function, and [selectInput_InitialCondCustom()],
#' [numericInput_ICvalue()], [selectInput_rb2p()], [numericInput_Samples()],
#' [selectInput_Bioactive()], [numericInput_ClintPval()], [numericInput_Alpha()],
#' [numericInput_MinFub()], [selectInput_RestrictClear()], [selectInput_AdjFub()],
#' [selectInput_Regression()], which are called by the current function
#' @export
#'
AP_ModelConditions <- function(ic_names,ic_comps){
  bslib::card(bslib::card_header("MODEL CONDITIONS"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                                      selectInput_InitialCondCustom("init_cond_opts"),
                                      shiny::conditionalPanel(condition = "input.init_cond_opts == 'Yes, enter my own initial amounts' && input.model == '1compartment'",
                                                              purrr::map2(ic_names[[1]],ic_comps[[1]], numericInput_ICvalue)),
                                      shiny::conditionalPanel(condition = "input.init_cond_opts == 'Yes, enter my own initial amounts' && input.model == '3compartment'",
                                                              purrr::map2(ic_names[[2]],ic_comps[[2]], numericInput_ICvalue)),
                                      shiny::conditionalPanel(condition = "input.init_cond_opts == 'Yes, enter my own initial amounts' && input.model == 'pbtk'",
                                                              purrr::map2(ic_names[[3]],ic_comps[[3]], numericInput_ICvalue)),
                                      shiny::conditionalPanel(condition = "input.init_cond_opts == 'Yes, enter my own initial amounts' && input.model == 'fetal_pbtk'",
                                                              purrr::map2(ic_names[[4]],ic_comps[[4]], numericInput_ICvalue)),
                                      selectInput_rb2p("rb2p")),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      numericInput_Samples("samples"),
                                      shiny::conditionalPanel(condition = "input.HondaIVIVE == 'NULL' && input.tissueIVIVE == 'NULL'",
                                                              selectInput_Bioactive("bioactiveIVIVE"))),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'",
                                      numericInput_ClintPval("Clint_Pval"),
                                      numericInput_Alpha("AlphaPar")),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Steady state concentrations'",
                                      numericInput_MinFub("min_fub")),
              shiny::conditionalPanel(condition = "(input.func == 'In vitro in vivo extrapolation (IVIVE)' && input.HondaIVIVE == 'NULL') ||
                                                                                 (input.func != 'In vitro in vivo extrapolation (IVIVE)')",
                                      selectInput_RestrictClear("restrict_clear")),
              selectInput_AdjFub("adj_fub"),
              selectInput_Regression("regression"))
}


################################################################################
################################################################################

#' User interface for the 'Model Solver' card on the 'Advanced Parameters' tab
#'
#' @return The 'Model Solver' card which allows the user options to select various
#' solver conditions if the concentration-time profiles module function is chosen
#' @seealso [app_ui()], which calls the current function, and [selectInput_ODEmethod()],
#' [numericInput_SolSteps()], [sliderInput_RTol()], and [sliderInput_ATol()],
#' which are called by the current function
#' @export
#'
AP_ModelSolver <- function(){
  bslib::card(bslib::card_header("MODEL SOLVER"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                                      selectInput_ODEmethod("odemethod"),
                                      numericInput_SolSteps("solversteps"),
                                      sliderInput_RTol("rtol"),
                                      sliderInput_ATol("atol")),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Concentration-time profiles'",
                                      shiny::helpText("No options for this category.")))
}


################################################################################
################################################################################

#' User interface for the 'Bioavailability' card on the 'Advanced Parameters' tab
#'
#' @return The 'Bioavailability' card which offers users the option to customize
#' five bioavailability parameters, unless the parameter calculations module
#' function is chosen
#' @seealso [app_ui()], which calls the current function, and [numericInput_CacoDefault()],
#' [selectInput_Fabs()], [selectInput_Fgut()], [selectInput_Overwrite()], and
#' [selectInput_Keep100()], which are called by the current function
#' @export
#'
AP_Bioavailability <- function(){
  bslib::card(bslib::card_header("BIOAVAILABILITY"),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Parameter calculations'",
                                      numericInput_CacoDefault("caco2default"), selectInput_Fabs("caco_fabs"), selectInput_Fgut("caco_fgut"),
                                      selectInput_Overwrite("caco_overwriteinvivo"), selectInput_Keep100("caco_keep100")),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'",
                                      shiny::helpText("No options for this category.")))
}


################################################################################
################################################################################

#' User interface for the 'Output Specification' card on the 'Advanced Parameters'
#' tab
#'
#' @return The 'Output Specification' card which offers the users different
#' selection options regarding the output for different module functions chosen
#' @seealso [app_ui()], which calls the current function, and [textInput_OutputTimes()],
#' [selectInput_SSoutunits()], [selectInput_OutConc()], [selectInput_Tissue()],
#' and [selectInput_IVIVEoutunits()], which are called by the current function
#' @export
#'
AP_OutputSpecification <- function(){
  bslib::card(bslib::card_header("OUTPUT SPECIFICATION"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                                      textInput_OutputTimes("returntimes")),
              shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations'",
                                      selectInput_SSoutunits("modelSSout_units"),
                                      selectInput_OutConc("output_concSS"),
                                      shiny::conditionalPanel(condition = "input.modelSS !== 'Select' && input.modelSS !== '3compartmentss'",
                                                              selectInput_Tissue("tissueSS"))),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      selectInput_IVIVEoutunits("modelIVIVEout_units"),
                                      shiny::conditionalPanel(condition = "input.HondaIVIVE == 'NULL'",
                                                              selectInput_OutConc("output_concIVIVE")),
                                      shiny::conditionalPanel(condition = "input.modelIVIVE !== '3compartmentss' && (input.output_concIVIVE == 'tissue' || input.HondaIVIVE == 'Honda4')",
                                                              selectInput_Tissue("tissueIVIVE"))),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'",
                                      shiny::helpText("No options for this category.")))
}


################################################################################
################################################################################

#' User interface for the 'Actions' card on the 'Run Simulation' tab
#'
#' @return The 'Actions' card which provides instructions to the user on how to
#' run simulations, two buttons for the user to press to run or reset the
#' simulation, and the option to log scale plots (if applicable for the module
#' chosen)
#' @seealso [app_ui()], which calls the current function, and [Instructions_RunSim()],
#' [Instructions_Reset()], and [checkboxInput_Log()], which are called by the
#' current function
#' @export
#'
RS_Actions <- function(){
  bslib::card(bslib::card_header("ACTIONS"),
              Instructions_RunSim(),
              shiny::actionButton("runsim", label = "Run Simulation"),
              Instructions_Reset(),
              shiny::actionButton("ResetButton", "Reset Session"),
              shiny::conditionalPanel(condition = "input.func !== 'Select' && input.func !== 'Concentration-time profiles'",
                                      checkboxInput_Log("logscale")))
}


################################################################################
################################################################################

#' User interface for the 'Selected Compounds' card on the 'Run Simulation' tab
#'
#' @return The 'Selected Compounds' card which shows a table of all compounds
#' selected by the user for simulation
#' @seealso [app_ui()], which calls the current function
#' @export
#'
RS_SelectedCompounds <- function(){
  bslib::card(bslib::card_header("SELECTED COMPOUNDS"),
              shiny::tableOutput("comptext"))
}


################################################################################
################################################################################

#' User interface for the 'Results' card on the 'Run Simulation' tab
#'
#' @return The 'Results' card which displays a number of drop down menus filled
#' with output results depending on the module function chosen
#' @seealso [app_ui()], which calls the current function, and [ADME_ui()],
#' [SS_ui()], [IVIVE_ui()], and [PC_ui()], which are called by the current function
#' @export
#'
RS_Results <- function(){
  bslib::card(bslib::card_header("RESULTS"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'", ADME_ui("ADME_AllOut")),
              shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations'", SS_ui("SS_AllOut")),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'", IVIVE_ui("IVIVE_AllOut")),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'", PC_ui("IP_AllOut")))
}





#######################################
# SERVER
#######################################

################################################################################
################################################################################

#' Compile outputs for the 'Preloaded Compounds' card under the 'Compound Selection'
#' card
#'
#' @param func User selection with the input$func ID; either "Concentration-time
#' profiles", "Steady state concentrations", "In vitro in vivo extrapolation
#' (IVIVE)", and "Parameter calculations"
#' @param spec User selection with the input$spec ID; either "Dog","Human","Mouse",
#' "Rabbit","Rat"
#' @param defaulthuman User selection with input$defaulttoHuman; either "Yes" for
#' using human in vitro data in place of animal data or "No" for only using
#' animal data
#' @param model User selection with the input$model ID; either "3compartmentss",
#' "Schmitt","1compartment","3compartment", "pbtk" or "fetal_pbtk"
#' @param insilicopars User selection with the input$insilicopars ID; either
#' "Yes, load in silico parameters" or "No, do not load in silico parameters"
#' @param honda User selection with the input$HondaIVIVE ID; either "NULL","Honda1",
#' "Honda2","Honda3","Honda4"
#' @param comptype User selection with the input$comptype ID; either "Choose from
#' all available chemicals" or "Choose from only food relevant chemicals"
#'
#' @return The user interface objects (drop down selections) for the 'Preloaded
#' Compounds' card on the 'Compound Selection' tab. The objects outputted vary
#' depending on the user's input selection.
#' @seealso [app_server()], which calls the current function
#' @export
#'
PreloadComps_UI <- function(func,spec,defaulthuman,model,insilicopars,honda,comptype){

  if (spec != "Select" && func != "Select" && insilicopars != "Select" && model != "Select" && defaulthuman != "Select"){
    if (func == "In vitro in vivo extrapolation (IVIVE)" && honda == "Honda1"){
      htmltools::tagList(numericInput_FSBf("FSBf"),
                         PreloadCompsInput(func,spec,defaulthuman,insilicopars,model,honda,comptype))
    }
    else {
      PreloadCompsInput(func,spec,defaulthuman,insilicopars,model,honda,comptype)
    }
  }
  else{
    shiny::showModal(shiny::modalDialog(title = "Missing Parameters",
                                        "Click the 'Dismiss' button and return to the 'General Parameters' and 'Model Specifications'
                                        tabs to select any missing parameters. Then return to this tab."))
  }
}


################################################################################
################################################################################

#' Compile a list of all chemicals to simulate
#'
#' @description
#' This function creates a data frame of chemical names in alphabetical order to
#' simulate. It checks the function's input parameters for validity. Then, if
#' applicable, the user's uploaded file of chemical data is uploaded and merged
#' any preloaded chemicals selected by the user.
#'
#'
#' @param func User selection with the input$func ID; either "Concentration-time
#' profiles", "Steady state concentrations", "In vitro in vivo extrapolation
#' (IVIVE)", and "Parameter calculations"
#' @param spec User selection with the input$spec ID; either "Dog","Human","Mouse",
#' "Rabbit","Rat"
#' @param defaulthuman User selection with input$defaulttoHuman; either "Yes" for
#' using human in vitro data in place of animal data or "No" for only using
#' animal data
#' @param model User selection with the input$model ID; either "3compartmentss",
#' "Schmitt","1compartment","3compartment", "pbtk" or "fetal_pbtk"
#' @param insilico User selection with the input$insilicopars ID; either
#' "Yes, load in silico parameters" or "No, do not load in silico parameters"
#' @param preloadcomps
#' @param file
#' @param honda User selection with the input$HondaIVIVE ID; either "NULL","Honda1",
#' "Honda2","Honda3","Honda4"
#'
#' @return A data frame with a single column of compound names in alphabetical
#' order
#' @seealso [app_server()], which calls the current function
#' @export
#'
CompileCompLst <- function(func,spec,defaulthuman,model,insilico,preloadcomps,file,honda){

  #--- OUTPUT ERROR WARNINGS IF NEEDED VARIABLES ARE MISSING
  pars <- list(func,spec,defaulthuman,model,insilico,preloadcomps,file,honda)
  names(pars) <- c("func","spec","defaulttoHuman","model","insilicopars",
                   "httkPreloadComps","file1","HondaIVIVE")
  validate_text_Common(pars)

  #--- COMPILES A LIST OF ALL COMPOUNDS
  CompoundList(preloadcomps,file)
}


################################################################################
################################################################################

#' Compile all parameters into one final list
#'
#' @param input The 'input' object used as the prefix for all ID's (should be
#' entered as 'input')
#' @param compoundlist A data frame with a single column that has the alphabetized
#' list of compounds to simulate
#'
#' @return A named list of all parameters in the GUI to be used in the main
#' module functions
#' @seealso [app_server()], which calls the current function
#' @export
#'
GatherInputVars <- function(input,compoundlist){

  pars <- purrr::map2(ParNames(),rep(list(input),length(ParNames())),CompilePars)
  names(pars) <- ParNames()
  pars[["CompoundList"]] <- compoundlist
  pars_new <- UpdatePars(pars)
}


################################################################################
################################################################################

#' Parse together the input ID name of a user selection
#'
#' @param VarName The name of the parameter
#' @param input The 'input' object used as the prefix for all ID's (should be
#' entered as 'input')
#'
#' @return A value for the given parameter
#' @export
#'
CompilePars <- function(VarName,input){
  eval(parse(text = paste("input$",VarName,sep = "")))
}


################################################################################
################################################################################

#' Main function to update inputs when a trigger input is changed
#'
#' @param input The 'input' object used as the prefix for all ID's (should be
#' entered as 'input')
#' @param session Shiny session
#'
#' @return Cleared inputs for the applicable function when a main input is
#' changed
#' @seealso [app_server()], which calls the current function
#' @export
#'
UpdateInputs <- function(input,session){

  shiny::observeEvent(input$func,{UpdateFunc(session)})
  shiny::observeEvent(input$spec,{UpdateSpec(session)})
  shiny::observeEvent(input$defaulttoHuman,{UpdateComps(session)})
  shiny::observeEvent(input$model,{UpdateComps(session)})
  shiny::observeEvent(input$insilicopars,{UpdateComps(session)})
}


################################################################################
################################################################################

#' Reset inputs if a new main GUI function is selected
#'
#' @param session Shiny session
#'
#' @return Clear "model", "in silico parameter preference", and "preloaded
#' compounds" selections and return to their default values
#' @export
#'
UpdateFunc <- function(session){

    updateSelectInput(session = session, inputId = 'model', selected = "Select")
    updateSelectInput(session = session, inputId = 'insilicopars', selected = "Select")
    updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
}


################################################################################
################################################################################

#' Reset inputs if a new species is selected
#'
#' @param session Shiny session
#'
#' @return Clear "model" and "preloaded compounds" selections and return to their
#' default values
#' @export
#'
UpdateSpec <- function(session){

  updateSelectInput(session = session, inputId = 'model', selected = "Select")
  updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
}


################################################################################
################################################################################

#' Reset the "preloaded compounds" input selection
#'
#' @param session Shiny session
#'
#' @return Clear the "preloaded compounds" selection and return it to its default
#' value
#' @export
#'
UpdateComps <- function(session){

  updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
}


################################################################################
################################################################################

#' Generate input validation rules for each individual module
#'
#' @param iv_common A new shiny input validator object for all common inputs
#' across modules (function, species, default to human, model, in silico
#' parameters, preloaded compounds, and uploaded compounds)
#' @param iv_adme A new shiny input validator object for all concentration-time
#' profile specific parameters that other modules do not have
#' @param iv_ss A new shiny input validator object for all steady state
#' concentrations specific parameters that other modules do not have
#' @param iv_ivive A new shiny input validator object for all IVIVE specific
#' parameters that other modules do not have
#' @param iv_pc A new shiny input validator object for all parameter calculations
#' specific parameters that other modules do not have
#' @param input The 'input' object used as the prefix for all ID's (should be
#' entered as 'input')
#' @param ic_names A named vector of all initial condition compartment names.
#' See global.R and input_functions.R for more information.
#'
#' @return Validation rules attached to each child input validator
#' @seealso [app_server()], which calls the current function
#' @export
#'
InputRules_Children <- function(iv_common,iv_adme,iv_ss,iv_ivive,iv_pc,input,ic_names){

  iv_common$add_rule("func", shinyvalidate::sv_not_equal("Select"))
  iv_common$add_rule("spec", shinyvalidate::sv_not_equal("Select"))
  iv_common$add_rule("defaulttoHuman", shinyvalidate::sv_not_equal("Select"))
  iv_common$add_rule("model", shinyvalidate::sv_not_equal("Select"))
  iv_common$add_rule("insilicopars", shinyvalidate::sv_not_equal("Select"))
  iv_common$add_rule("httkPreloadComps", not_null,input)
  iv_common$add_rule("file1",UploadComps_Check,input)

  iv_adme$add_rule("dosenum", shinyvalidate::sv_not_equal("Select"))
  iv_adme$add_rule("multdose", multdose_Select,input)
  iv_adme$add_rule("multdose_odd", multdose_odd,input)
  iv_adme$add_rule("model", fetal_cond, input)
  iv_adme$add_rule("returntimes", returntimes_cond,input)
  iv_adme$add_rule("initdose",shinyvalidate::sv_required())
  iv_adme$add_rule("mult_doseamount",shinyvalidate::sv_required())
  iv_adme$add_rule("simtime",shinyvalidate::sv_required())
  iv_adme$add_rule("min_fub",shinyvalidate::sv_required())
  iv_adme$add_rule("solversteps",shinyvalidate::sv_required())
  iv_adme$add_rule("caco2default",shinyvalidate::sv_required())
  purrr::map2(rep(list(iv_adme),45),unlist(ic_names),addrule_adme_ics)

  iv_ss$add_rule("dailydose",shinyvalidate::sv_required())
  iv_ss$add_rule("caco2default",shinyvalidate::sv_required())

  iv_ivive$add_rule("BioactiveFile", shinyvalidate::sv_required())
  iv_ivive$add_rule("BioactiveFile",BioUpload_Check,input)
  iv_ivive$add_rule("returnsamples", shinyvalidate::sv_not_equal("Select"))
  iv_ivive$add_rule("quantile", shinyvalidate::sv_required())
  iv_ivive$add_rule("samples", shinyvalidate::sv_required())
  iv_ivive$add_rule("min_fub",shinyvalidate::sv_required())
  iv_ivive$add_rule("FSBf",FSBf_Check,input)
  iv_ivive$add_rule("caco2default",shinyvalidate::sv_required())
  iv_ivive$add_rule("fileExposure",ExposureUpload_Check,input)

  iv_pc$add_rule("Clint_Pval",shinyvalidate::sv_required())
  iv_pc$add_rule("AlphaPar",shinyvalidate::sv_required())
  iv_pc$add_rule("min_fub",shinyvalidate::sv_required())
}


################################################################################
################################################################################

#' Combine common validation rules and individual module validation rules into a
#' single comprehensive input validator for each module
#'
#' @param parent_adme_iv A new shiny input validator object for all concentration-
#' time profile module inputs
#' @param iv_adme A shiny input validator object for all concentration-time
#' profile specific parameters that other modules do not have
#' @param parent_ss_iv A new shiny input validator object for all steady state
#' concentration module inputs
#' @param iv_ss A shiny input validator object for all steady state
#' concentrations specific parameters that other modules do not have
#' @param parent_ivive_iv A new shiny input validator object for all IVIVE
#' module inputs
#' @param iv_ivive A shiny input validator object for all IVIVE specific
#' parameters that other modules do not have
#' @param parent_pc_iv A new shiny input validator object for all parameter
#' calculations module inputs
#' @param iv_pc A shiny input validator object for all parameter calculations
#' specific parameters that other modules do not have
#' @param iv_common A shiny input validator object for all common inputs
#' across modules (function, species, default to human, model, in silico
#' parameters, preloaded compounds, and uploaded compounds)
#'
#' @return Validation rules attached to the four parent input validators
#' @seealso [app_server()], which calls the current function
#' @export
#'
InputRules_Parents <- function(parent_adme_iv,iv_adme,
                               parent_ss_iv,iv_ss,
                               parent_ivive_iv,iv_ivive,
                               parent_pc_iv,iv_pc,
                               iv_common){

  parent_adme_iv$add_validator(iv_common)
  parent_adme_iv$add_validator(iv_adme)
  parent_adme_iv$enable()

  parent_ss_iv$add_validator(iv_common)
  parent_ss_iv$add_validator(iv_ss)
  parent_ss_iv$enable()

  parent_ivive_iv$add_validator(iv_common)
  parent_ivive_iv$add_validator(iv_ivive)
  parent_ivive_iv$enable()

  parent_pc_iv$add_validator(iv_common)
  parent_pc_iv$add_validator(iv_pc)
  parent_pc_iv$enable()
}


################################################################################
################################################################################

#' Run the selected module with all user selected inputs
#'
#' @param parent_adme_iv A shiny input validator object for all concentration-
#' time profile module inputs
#' @param parent_ss_iv A shiny input validator object for all steady state
#' concentration module inputs
#' @param parent_ivive_iv A shiny input validator object for all IVIVE
#' module inputs
#' @param parent_pc_iv A shiny input validator object for all parameter
#' calculations module inputs
#' @param input The 'input' object used as the prefix for all ID's (should be
#' entered as 'input')
#' @param AllInputs The named list of all parameter values created in the GUI,
#' named accordingly in parnames()
#'
#' @return The module outputs for the selected module or an error notification
#' indicating missing or invalid parameters
#' @seealso [app_server()], which calls the current function
#' @export
#'
Run_Simulation <- function(parent_adme_iv,
                           parent_ss_iv,
                           parent_ivive_iv,
                           parent_pc_iv,
                           input, AllInputs){

  if (input$func == "Concentration-time profiles"){
    if (parent_adme_iv$is_valid()){
      ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
    }
    else {
      Notify_ParError()
      req(parent_adme_iv$is_valid())
    }
  }
  else if (input$func == "Steady state concentrations"){
    if (parent_ss_iv$is_valid()){
      SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
    }
    else {
      Notify_ParError()
      req(parent_ss_iv$is_valid())
    }
  }
  else if (input$func == "In vitro in vivo extrapolation (IVIVE)"){

    if (parent_ivive_iv$is_valid()){
      IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
    }
    else {
      Notify_ParError()
      req(parent_ivive_iv$is_valid())
    }
  }
  else if (input$func == "Parameter calculations"){
    if (parent_pc_iv$is_valid()){
      PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
    }
    else{
      Notify_ParError()
      shiny::req(parent_pc_iv$is_valid())
    }
  }
}



