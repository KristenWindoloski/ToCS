
# --- Created by Kristen Windoloski
# --- Last Updated: January 2, 2025
# --- Description: A graphical user interface that utilizes the EPA's
#                  high-throughput toxicokinetics 'httk' R package to generate
#                  toxicokinetic ADME (absorption, distribution, metabolism,
#                  excretion) simulations, steady state concentrations, and
#                  common toxicokinetic (TK) parameters as well as perform in
#                  vitro in vivo extrapolation (IVIVE) for user-selected chemicals.
#                  Users select from the four main outputs described above, and
#                  then select all the input parameters and preferences. Once
#                  ready, they hit a 'run simulation' button and are able to
#                  download any outputted plots, tables, or input data.

##########################################################################
# MAIN GUI R FILE
##########################################################################


ToCS <- function(...){

  ##########################################################################
  # GENERATE INITIAL CONDITIONS
  ##########################################################################

  ics <- names_ICs()
  ic_names <- ics[[1]]
  ic_comps <- ics[[2]]

  ##########################################################################
  # USER INTERFACE SECTION
  ##########################################################################

  ui <- bslib::page_navbar(title = "Toxicokinetic Chemical Simulator (ToCS)",
                    shinyjs::useShinyjs(),

                    ##########################################################################
                    # GENERAL PARAMETERS TAB
                    ##########################################################################

                    bslib::nav_panel(title = "General Parameters",
                                     bslib::layout_columns(

                                ###############################################
                                # INSTRUCTIONS, OUTPUT, AND SPECIES CARD
                                ###############################################

                                bslib::card(bslib::card_header("INSTRUCTIONS"),
                                            Instructions_GenPars()),


                                ###############################################
                                # OUTPUT CARD
                                ###############################################

                                bslib::card(bslib::card_header("OUTPUT"),
                                            selectInput_Function("func")),

                                ###############################################
                                # SPECIES CARD
                                ###############################################

                                bslib::card(bslib::card_header("SPECIES"),
                                            selectInput_Species("spec"),
                                            selectInput_DefaultToHuman("defaulttoHuman")),
                                col_widths = c(4,4,4))
                    ),

                    ##########################################################################
                    # MODEL SPECIFICATIONS TAB
                    ##########################################################################

                    bslib::nav_panel(title = "Model Specifications",
                                     bslib::layout_columns(

                                ###############################################
                                # DOSING CARD
                                ###############################################

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
                                                                    shiny::helpText("No options for this category."))),

                                ###############################################
                                # MODEL CARD
                                ###############################################

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
                                                                                            numericInput_Quantile("quantile")))),
                                col_widths = c(6,6))),

                    ##########################################################################
                    # COMPOUND SELECTION TAB
                    ##########################################################################

                    bslib::nav_panel(title = "Compound Selection",
                                     bslib::layout_columns(

                                ###############################################
                                # INSTRUCTIONS CARD
                                ###############################################

                                bslib::card(bslib::card_header("INSTRUCTIONS"),
                                            shiny::conditionalPanel(condition = "input.httkPreloadComps == '' && output.fileUploaded == false && input.insilicopars != 'Select'",
                                                                    Instructions_CompSelect_Part1()),
                                            shiny::conditionalPanel(condition = "(input.httkPreloadComps != '' || output.fileUploaded == true) && input.insilicopars != 'Select'",
                                                                    Instructions_CompSelect_Part2(),
                                                                    shiny::actionButton("runCompounds", label = "Load Compounds")),
                                            Instructions_CompSelect_Part3(),
                                            shiny::a("Uploaded Compound File Folder", target="_blank", href="UploadedCompoundDetailsFolder.zip")),

                                ###############################################
                                # PRELOADED COMPOUNDS CARD
                                ###############################################

                                bslib::card(bslib::card_header("PRELOADED COMPOUNDS"),

                                            shiny::conditionalPanel(condition = "input.func !== 'Select' && input.spec != 'Select' && input.defaultoHuman != 'Select'",
                                                                    shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                                                                                            selectInput_HondaCond("HondaIVIVE")),
                                                                    shiny::uiOutput("PreloadedComps"))),

                                ###############################################
                                # UPLOADED COMPOUNDS CARD
                                ###############################################

                                bslib::card(bslib::card_header("UPLOADED COMPOUNDS"),
                                            fileInput_UploadedComps("file1")),
                                col_widths = c(4,4,4))),

                    ##########################################################################
                    # ADVANCED (OPTIONAL) PARAMETERS TAB
                    ##########################################################################

                    bslib::nav_panel(title = "Advanced (Optional) Parameters",
                                     bslib::layout_columns(

                                ###############################################
                                # MODEL CONDITIONS CARD
                                ###############################################

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
                                            selectInput_Regression("regression")),

                                ###############################################
                                # MODEL SOLVER CARD
                                ###############################################

                                bslib::card(bslib::card_header("MODEL SOLVER"),
                                            shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles' && input.runCompounds>0",
                                                                    selectInput_ODEmethod("odemethod"),
                                                                    numericInput_SolSteps("solversteps"),
                                                                    sliderInput_RTol("rtol"),
                                                                    sliderInput_ATol("atol")),
                                            shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Concentration-time profiles' && input.runCompounds>0",
                                                                    shiny::helpText("No options for this category."))),

                                ###############################################
                                # BIOAVAILABILITY CARD
                                ###############################################

                                bslib::card(bslib::card_header("BIOAVAILABILITY"),
                                            shiny::conditionalPanel(condition = "input.func != 'Select' && input.func != 'Parameter calculations' && input.runCompounds>0",
                                                                    numericInput_CacoDefault("caco2default"), selectInput_Fabs("caco_fabs"), selectInput_Fgut("caco_fgut"),
                                                                    selectInput_Overwrite("caco_overwriteinvivo"), selectInput_Keep100("caco_keep100")),
                                            shiny::conditionalPanel(condition = "input.func == 'Parameter calculations' && input.runCompounds>0",
                                                                    shiny::helpText("No options for this category."))),

                                ###############################################
                                # OUTPUT SPECIFICATION CARD
                                ###############################################

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
                                                                    shiny::helpText("No options for this category."))),
                                col_widths = c(3,3,3,3))),

                    ##########################################################################
                    # RUN SIMULATION TAB (RESULTS)
                    ##########################################################################

                    bslib::nav_panel(title = "Run Simulation",
                                     bslib::layout_columns(

                                ###############################################
                                # ACTION BUTTONS CARD
                                ###############################################

                                bslib::card(bslib::card_header("ACTIONS"),
                                            Instructions_RunSim(),
                                            shiny::actionButton("runsim", label = "Run Simulation"),
                                            Instructions_Reset(),
                                            shiny::actionButton("ResetButton", "Reset Session"),
                                            shiny::conditionalPanel(condition = "input.func !== 'Select' && input.func !== 'Concentration-time profiles'",
                                                                    checkboxInput_Log("logscale"))),

                                ###############################################
                                # SELECTED COMPOUNDS CARD
                                ###############################################

                                bslib::card(bslib::card_header("SELECTED COMPOUNDS"),
                                            shiny::tableOutput("comptext")),

                                ###############################################
                                # RESULTS CARD
                                ###############################################

                                bslib::card(bslib::card_header("RESULTS"),
                                            shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'", ADME_ui("ADME_AllOut")),
                                            shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations'", SS_ui("SS_AllOut")),
                                            shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'", IVIVE_ui("IVIVE_AllOut")),
                                            shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'", PC_ui("IP_AllOut"))),
                                col_widths = c(2,2,8))
                    )
  )


  ##########################################################################
  # SERVER SECTION
  ##########################################################################

  server <- function(input, output, session) {

    ##########################################################################
    # RESET BUTTON OUTPUT
    ##########################################################################

    shiny::observeEvent(input$ResetButton, {
      session$reload()
      return()})

    ##########################################################################
    # HAS A FILE WITH COMPOUND DATA BEEN UPLOADED?
    ##########################################################################

    getData <- shiny::reactive({
      if(is.null(input$file1))
        return(FALSE)
      else
        return(TRUE)})
    output$fileUploaded <- shiny::reactive({
      return(getData())})
    shiny::outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

    ##########################################################################
    # GENERATES THE MODEL CARD OUTPUT
    ##########################################################################

    output$Model <- shiny::renderUI({Model_Input(input$func)})

    ##########################################################################
    # GENERATES THE PRELOADED COMPOUNDS OUTPUT
    ##########################################################################

    output$PreloadedComps <- shiny::renderUI({

      if (input$spec != "Select" && input$func != "Select" && input$insilicopars != "Select" && input$model != "Select"){
        if (input$func == "In vitro in vivo extrapolation (IVIVE)" && input$HondaIVIVE == "Honda1"){
          htmltools::tagList(numericInput_FSBf("FSBf"),
                             PreloadCompsInput(input$func,input$spec,input$defaulttoHuman,input$insilicopars,input$model,input$HondaIVIVE))
        }
        else {
          compout <- PreloadCompsInput(input$func,input$spec,input$defaulttoHuman,input$insilicopars,input$model,input$HondaIVIVE)

        }
      }
    })

    ##########################################################################
    # COMPILES LIST OF ALL COMPOUNDS TO RUN
    ##########################################################################

    CompLst <- shiny::eventReactive(input$runCompounds,{

      #--- OUTPUT ERROR WARNINGS IF NEEDED VARIABLES ARE MISSING
      pars <- list(input$func,input$spec,input$defaulttoHuman,input$runCompounds,input$model)
      names(pars) <- c("func","spec","defaulttoHuman","runCompounds","model")
      validate_text_Common(pars,"Yes")

      #--- COMPILES A LIST OF ALL LIKE VARIABLES (DEFAULT HUMAN AND ALL COMPOUNDS)
      CompoundList(input$httkPreloadComps,input$httkPreloadComps_Honda,input$file1)})

    output$comptext <- shiny::renderTable({CompLst()})

    ##########################################################################
    # GATHER ALL INPUT VARIABLES
    ##########################################################################

    AllInputs <- shiny::reactive({

      CompilePars <- function(VarName){
        eval(parse(text = paste("input$",VarName,sep = "")))
      }

      pars <- purrr::map(ParNames(),CompilePars)
      names(pars) <- ParNames()
      pars[["CompoundList"]] <- CompLst()
      pars_new <- UpdatePars(pars)})

    ##########################################################################
    # ADME TIME COURSE OUTPUTS (PLOTS, SIMULATION DATA, TK SUMMARY STATS)
    ##########################################################################

    shiny::conditionalPanel(condition = "input.func == 'Concentration-time profiles'",
                            ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim)))

    ##########################################################################
    # STEADY STATE OUTPUTS (PLOT, TABLE)
    ##########################################################################

    shiny::conditionalPanel(condition = "input.func == 'Steady state concentrations'",
                            SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale)))

    ##########################################################################
    # IVIVE OUTPUTS (TABLE, PLOT)
    ##########################################################################

    shiny::conditionalPanel(condition = "input.func == 'In vitro in vivo extrapolation (IVIVE)'",
                            IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale)))

    ##########################################################################
    # IMPORTANT PARAMETER CALCULATIONS (PLOTS, TABLES)
    ##########################################################################

    shiny::conditionalPanel(condition = "input.func == 'Parameter calculations'",
                            PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale)))
  }

  ####################################################################################
  # RUN R SHINY APP
  ####################################################################################

  shiny::shinyApp(ui, server)
}
