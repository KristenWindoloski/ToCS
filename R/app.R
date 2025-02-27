
# --- Created by Kristen Windoloski
# --- Last Updated: February 27, 2025
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
  # GENERATE INITIAL CONDITIONS & ERROR MESSAGES
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
                    # INSTRUCTIONS, OUTPUT, AND SPECIES CARDS
                    ##########################################################################

                    bslib::nav_panel(title = "General Parameters",
                                     bslib::layout_columns(GP_Instructions(),
                                                           GP_Output(),
                                                           GP_Species(),
                                                           col_widths = c(4,4,4))),

                    ##########################################################################
                    # MODEL SPECIFICATIONS TAB
                    # DOSING AND MODEL CARDS
                    ##########################################################################

                    bslib::nav_panel(title = "Model Specifications",
                                     bslib::layout_columns(MS_Dosing(),
                                                           MS_Model(),
                                                           col_widths = c(6,6))),

                    ##########################################################################
                    # COMPOUND SELECTION TAB
                    # INSTRUCTIONS, PRELOADED COMPOUNDS, AND UPLOADED COMPOUNDS CARDS
                    ##########################################################################

                    bslib::nav_panel(title = "Compound Selection",
                                     bslib::layout_columns(CS_Instructions(),
                                                           CS_PreloadedCompounds(),
                                                           CS_UploadedData(),
                                                           col_widths = c(4,4,4))),

                    ##################################################################################
                    # ADVANCED (OPTIONAL) PARAMETERS TAB
                    # MODEL CONDITIONS, MODEL SOLVER, BIOAVAILABILITY, AND OUTPUT SPECIFICATION CARDS
                    ##################################################################################

                    bslib::nav_panel(title = "Advanced (Optional) Parameters",
                                     bslib::layout_columns(AP_ModelConditions(ic_names,ic_comps),
                                                           AP_ModelSolver(),
                                                           AP_Bioavailability(),
                                                           AP_OutputSpecification(),
                                                           col_widths = c(3,3,3,3))),

                    ##########################################################################
                    # RUN SIMULATION TAB (RESULTS)
                    # ACTION, SELECTED COMPOUNDS, AND RESULTS CARDS
                    ##########################################################################

                    bslib::nav_panel(title = "Run Simulation",
                                     bslib::layout_columns(RS_Actions(),
                                                           RS_SelectedCompounds(),
                                                           RS_Results(),
                                                           col_widths = c(2,2,8)))
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
      if(is.null(input$file1)) return(FALSE)
      else return(TRUE)})
    output$fileUploaded <- shiny::reactive({return(getData())})
    shiny::outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

    ##########################################################################
    # GENERATES THE MODEL CARD OUTPUT
    ##########################################################################

    output$Model <- shiny::renderUI({Model_Input(input$func,input$spec)})

    ##########################################################################
    # GENERATES THE PRELOADED COMPOUNDS OUTPUT
    ##########################################################################

    output$PreloadedComps <- shiny::renderUI({
      PreloadComps_UI(input$func,input$spec,input$defaulttoHuman,
                      input$model,input$insilicopars,input$HondaIVIVE)})

    ##########################################################################
    # COMPILES LIST OF ALL COMPOUNDS TO RUN
    ##########################################################################

    CompLst <- shiny::reactive({

      CompileCompLst(input$func,input$spec,input$defaulttoHuman,input$model,
                     input$insilicopars,input$httkPreloadComps,input$file1)})

    output$comptext <- shiny::renderTable({CompLst()})

    ##########################################################################
    # RESET INPUT COMPOUND-RELATED VARIABLES IF USER CHANGES DEPENDENT VARIABLES
    ##########################################################################

    UpdateInputs(input,session)

    ##########################################################################
    # GATHER ALL INPUT VARIABLES
    ##########################################################################

    AllInputs <- eventReactive(input$runsim,{GatherInputVars(input,CompLst())})

    ##########################################################################
    # DEFINE INPUT ERRORS FOR EACH MODULE
    ##########################################################################

    iv_common <- shinyvalidate::InputValidator$new()
    iv_adme <- shinyvalidate::InputValidator$new()
    iv_ss <- shinyvalidate::InputValidator$new()
    iv_ivive <- shinyvalidate::InputValidator$new()
    iv_pc <- shinyvalidate::InputValidator$new()
    InputRules_Children(iv_common,iv_adme,iv_ss,iv_ivive,iv_pc,input,ic_names)

    parent_adme_iv <- shinyvalidate::InputValidator$new()
    parent_ss_iv <- shinyvalidate::InputValidator$new()
    parent_ivive_iv <- shinyvalidate::InputValidator$new()
    parent_pc_iv <- shinyvalidate::InputValidator$new()
    InputRules_Parents(parent_adme_iv,iv_adme,
                       parent_ss_iv,iv_ss,
                       parent_ivive_iv,iv_ivive,
                       parent_pc_iv,iv_pc,
                       iv_common)

    ##########################################################################
    # ADME TIME COURSE OUTPUTS (PLOTS, SIMULATION DATA, TK SUMMARY STATS)
    # STEADY STATE OUTPUTS (PLOT, TABLE)
    # IVIVE OUTPUTS (TABLE, PLOT)
    # PARAMETER CALCULATIONS OUTPUTS (PLOTS, TABLES)
    ##########################################################################

    shiny::observeEvent(input$runsim,{
      Run_Simulation(parent_adme_iv,parent_ss_iv,parent_ivive_iv,parent_pc_iv,input, AllInputs)
    })
  }

  ####################################################################################
  # RUN R SHINY APP
  ####################################################################################

  shiny::shinyApp(ui, server)
}
