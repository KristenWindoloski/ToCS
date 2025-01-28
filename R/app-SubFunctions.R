

######################################################
# UI CODE SUB FUNCTIONS
######################################################

#######################################
# GENERAL PARAMETERS TAB
#######################################

#### INSTRUCTIONS CARD
GP_Instructions <- function(){
  bslib::card(bslib::card_header("INSTRUCTIONS"),
              Instructions_GenPars())
}

#### OUTPUT CARD
GP_Output <- function(){
  bslib::card(bslib::card_header("OUTPUT"),
              selectInput_Function("func"))
}

#### SPECIES CARD
GP_Species <- function(){
  bslib::card(bslib::card_header("SPECIES"),
              selectInput_Species("spec"),
              selectInput_DefaultToHuman("defaulttoHuman"))
}

#######################################
# MODEL SPECIFICATIONS TAB
#######################################

#### DOSING CARD
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
                                      shiny::conditionalPanel(condition = "input.multdose == 'No'",
                                                              textInput_DoseMatrix("multdose_odd"))),
              shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations'",
                                      numericInput_DailyDose("dailydose")),
              shiny::conditionalPanel(condition = "(input.func == 'In vitro in vivo extrapolation (IVIVE)' || input.func == 'Parameter calculations')",
                                      shiny::helpText("No options for this category.")))
}

#### MODEL CARD
MS_Model <- function(){
  bslib::card(bslib::card_header("MODEL"),
              shiny::conditionalPanel(condition = "input.func != 'Select'",
                                      shiny::uiOutput("Model"),
                                      selectInput_InSilicoPars("insilicopars")),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                                      numericInput_SimTime("simtime")),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      fileInput_BioactiveConc("BioactiveFile"),
                                      selectInput_ReturnSamps("returnsamples"),
                                      shiny::conditionalPanel(condition = "input.returnsamples == 'Only return a specified dose quantile (default)'",
                                                              numericInput_Quantile("quantile"))))
}

#######################################
# COMPOUND SELECTION TAB
#######################################

#### INSTRUCTIONS CARD
CS_Instructions <- function(){
  bslib::card(bslib::card_header("INSTRUCTIONS"),
              shiny::conditionalPanel(condition = "input.httkPreloadComps == '' && output.fileUploaded == false && input.model != 'Select' && input.insilicopars != 'Select'",
                                      Instructions_CompSelect_Part1()),
              shiny::conditionalPanel(condition = "(input.httkPreloadComps != '' || output.fileUploaded == true) && input.model != 'Select' && input.insilicopars != 'Select'",
                                      Instructions_CompSelect_Part2(),
                                      shiny::actionButton("runCompounds", label = "Load Compounds")),
              Instructions_CompSelect_Part3(),
              shiny::a("Uploaded Compound File Folder", target="_blank", href="UploadedCompoundDetailsFolder.zip"))
}

#### PRELOADED COMPOUNDS CARD
CS_PreloadedCompounds <- function(){
  bslib::card(bslib::card_header("PRELOADED COMPOUNDS"),
              shiny::conditionalPanel(condition = "input.func !== 'Select' && input.spec != 'Select' && input.defaultoHuman != 'Select'",
                                      shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                                              selectInput_HondaCond("HondaIVIVE")),
                                      shiny::uiOutput("PreloadedComps")))
}

#### UPLOADED COMPOUNDS CARD
CS_UploadedCompounds <- function(){
  bslib::card(bslib::card_header("UPLOADED COMPOUNDS"),
              fileInput_UploadedComps("file1"))
}

#######################################
# ADVANCED (OPTIONAL) PARAMETERS TAB
#######################################

#### MODEL CONDITIONS CARD
AP_ModelConditions <- function(ic_names,ic_comps){
  bslib::card(bslib::card_header("MODEL CONDITIONS"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles' && input.runCompounds>0",
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
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)' && input.runCompounds>0",
                                      numericInput_Samples("samples"),
                                      shiny::conditionalPanel(condition = "input.HondaIVIVE == 'NULL' && input.tissueIVIVE == 'NULL'",
                                                              selectInput_Bioactive("bioactiveIVIVE"))),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations' && input.runCompounds>0",
                                      numericInput_ClintPval("Clint_Pval"),
                                      numericInput_Alpha("AlphaPar")),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Steady state concentrations' && input.runCompounds>0",
                                      numericInput_MinFub("min_fub")),
              shiny::conditionalPanel(condition = "(input.func == 'In vitro in vivo extrapolation (IVIVE)' && input.HondaIVIVE == 'NULL' && input.runCompounds>0) ||
                                                                                 (input.func != 'In vitro in vivo extrapolation (IVIVE)' && input.runCompounds>0)",
                                      selectInput_RestrictClear("restrict_clear")),
              selectInput_AdjFub("adj_fub"),
              selectInput_Regression("regression"))
}

#### MODEL SOLVER CARD
AP_ModelSolver <- function(){
  bslib::card(bslib::card_header("MODEL SOLVER"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles' && input.runCompounds>0",
                                      selectInput_ODEmethod("odemethod"),
                                      numericInput_SolSteps("solversteps"),
                                      sliderInput_RTol("rtol"),
                                      sliderInput_ATol("atol")),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Concentration-time profiles' && input.runCompounds>0",
                                      shiny::helpText("No options for this category.")))
}

#### BIOAVAILABILITY CARD
AP_Bioavailability <- function(){
  bslib::card(bslib::card_header("BIOAVAILABILITY"),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Parameter calculations' && input.runCompounds>0",
                                      numericInput_CacoDefault("caco2default"), selectInput_Fabs("caco_fabs"), selectInput_Fgut("caco_fgut"),
                                      selectInput_Overwrite("caco_overwriteinvivo"), selectInput_Keep100("caco_keep100")),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations' && input.runCompounds>0",
                                      shiny::helpText("No options for this category.")))
}

#### OUTPUT SPECIFICATION CARD
AP_OutputSpecification <- function(){
  bslib::card(bslib::card_header("OUTPUT SPECIFICATION"),
              shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles' && input.runCompounds>0",
                                      textInput_OutputTimes("returntimes")),
              shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations' && input.runCompounds>0",
                                      selectInput_SSoutunits("modelSSout_units"),
                                      selectInput_OutConc("output_concSS"),
                                      shiny::conditionalPanel(condition = "input.modelSS !== 'Select' && input.modelSS !== '3compartmentss'",
                                                              selectInput_Tissue("tissueSS"))),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)' && input.runCompounds>0",
                                      selectInput_IVIVEoutunits("modelIVIVEout_units"),
                                      shiny::conditionalPanel(condition = "input.HondaIVIVE == 'NULL'",
                                                              selectInput_OutConc("output_concIVIVE")),
                                      shiny::conditionalPanel(condition = "input.modelIVIVE !== '3compartmentss' && (input.output_concIVIVE == 'tissue' || input.HondaIVIVE == 'Honda4')",
                                                              selectInput_Tissue("tissueIVIVE"))),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations' && input.runCompounds>0",
                                      shiny::helpText("No options for this category.")))
}

#######################################
# RUN SIMULATION TAB
#######################################

#### ACTIONS CARD
RS_Actions <- function(){
  bslib::card(bslib::card_header("ACTIONS"),
              Instructions_RunSim(),
              shiny::actionButton("runsim", label = "Run Simulation"),
              Instructions_Reset(),
              shiny::actionButton("ResetButton", "Reset Session"),
              shiny::conditionalPanel(condition = "input.func !== 'Select' && input.func !== 'Concentration-time profiles'",
                                      checkboxInput_Log("logscale")))
}

#### SELECTED COMPOUNDS CARD
RS_SelectedCompounds <- function(){
  bslib::card(bslib::card_header("SELECTED COMPOUNDS"),
              shiny::tableOutput("comptext"))
}

#### RESULTS CARD
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


UpdateFunc <- function(session){

    updateSelectInput(session = session, inputId = 'model', selected = "Select")
    updateSelectInput(session = session, inputId = 'insilicopars', selected = "Select")
    updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
    updateSelectInput(session = session, inputId = 'httkPreloadComps_Honda', selected = "")
}

UpdateSpec <- function(session){

  updateSelectInput(session = session, inputId = 'model', selected = "Select")
  updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
  updateSelectInput(session = session, inputId = 'httkPreloadComps_Honda', selected = "")
}

UpdateComps <- function(session){

  updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
  updateSelectInput(session = session, inputId = 'httkPreloadComps_Honda', selected = "")
}

