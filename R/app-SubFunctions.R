
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
              shiny::conditionalPanel(condition = "input.spec == 'Dog' || input.spec == 'Mouse' || input.spec == 'Rabbit' || input.spec == 'Rat'",
                                      selectInput_DefaultToHuman("defaulttoHuman")))
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
                                      shiny::conditionalPanel(condition = "input.dosenum == 'Multiple Doses' && input.multdose == 'No'",
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
              Instructions_CompSelect_Part2(),
              shiny::tags$a("Uploaded Physical-Chemical Data File Folder", href="UploadedPhysicalChemicalDataFileFolder.zip"),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      shiny::tags$a("Bioactivity Data File Folder", href="BioactivityDataFileFolder.zip"),
                                      shiny::tags$a("Exposure Data File Folder", href="ExposureDataFileFolder.zip"))
              )
}

#### PRELOADED COMPOUNDS CARD
CS_PreloadedCompounds <- function(){
  bslib::card(bslib::card_header("PRELOADED COMPOUNDS"),
              shiny::conditionalPanel(condition = "input.func !== 'Select' && input.spec != 'Select' && input.defaultoHuman != 'Select'",
                                      shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                                              selectInput_HondaCond("HondaIVIVE")),
                                      selectInput_CompPreference("CompType"),
                                      shiny::uiOutput("PreloadedComps")))
}

#### UPLOADED DATA CARD
CS_UploadedData <- function(){
  bslib::card(bslib::card_header("UPLOADED DATA"),
              fileInput_UploadedComps("file1"),
              shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                      fileInput_BioactiveConc("BioactiveFile"),
                                      fileInput_ExposureData("fileExposure"))
              )
}

#######################################
# ADVANCED (OPTIONAL) PARAMETERS TAB
#######################################

#### MODEL CONDITIONS CARD
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

#### MODEL SOLVER CARD
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

#### BIOAVAILABILITY CARD
AP_Bioavailability <- function(){
  bslib::card(bslib::card_header("BIOAVAILABILITY"),
              shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Parameter calculations'",
                                      numericInput_CacoDefault("caco2default"), selectInput_Fabs("caco_fabs"), selectInput_Fgut("caco_fgut"),
                                      selectInput_Overwrite("caco_overwriteinvivo"), selectInput_Keep100("caco_keep100")),
              shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'",
                                      shiny::helpText("No options for this category.")))
}

#### OUTPUT SPECIFICATION CARD
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

CompileCompLst <- function(func,spec,defaulthuman,model,insilico,preloadcomps,file,honda){

  #--- OUTPUT ERROR WARNINGS IF NEEDED VARIABLES ARE MISSING
  pars <- list(func,spec,defaulthuman,model,insilico,preloadcomps,file,honda)
  names(pars) <- c("func","spec","defaulttoHuman","model","insilicopars",
                   "httkPreloadComps","file1","HondaIVIVE")
  validate_text_Common(pars)

  #--- COMPILES A LIST OF ALL COMPOUNDS
  CompoundList(preloadcomps,file)
}

GatherInputVars <- function(input,compoundlist){

  pars <- purrr::map2(ParNames(),rep(list(input),length(ParNames())),CompilePars)
  names(pars) <- ParNames()
  pars[["CompoundList"]] <- compoundlist
  pars_new <- UpdatePars(pars)
}

CompilePars <- function(VarName,input){
  eval(parse(text = paste("input$",VarName,sep = "")))
}

UpdateInputs <- function(input,session){

  shiny::observeEvent(input$func,{UpdateFunc(session)})
  shiny::observeEvent(input$spec,{UpdateSpec(session)})
  shiny::observeEvent(input$defaulttoHuman,{UpdateComps(session)})
  shiny::observeEvent(input$model,{UpdateComps(session)})
  shiny::observeEvent(input$insilicopars,{UpdateComps(session)})
}

UpdateFunc <- function(session){

    updateSelectInput(session = session, inputId = 'model', selected = "Select")
    updateSelectInput(session = session, inputId = 'insilicopars', selected = "Select")
    updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
}

UpdateSpec <- function(session){

  updateSelectInput(session = session, inputId = 'model', selected = "Select")
  updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
}

UpdateComps <- function(session){

  updateSelectInput(session = session, inputId = 'httkPreloadComps', selected = "")
}

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



