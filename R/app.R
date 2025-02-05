
# --- Created by Kristen Windoloski
# --- Last Updated: January 29, 2025
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

  not_null <- function(value,input, message = "At least one compound must be selected or uploaded"){
    if (is.null(c(value,input$file1))) message
  }

  multdose_Select <- function(value,input,message = "The dosing frequency must be selected"){
    if (input$dosenum == "Multiple Doses" && value == "Select") message
  }

  multdose_odd <- function(value,input,message = "The dosing administration amounts and times must be entered"){
    if (input$dosenum == "Multiple Doses" && input$multdose == "No" && value == "") message
  }

  fetal_cond <- function(value,input,message = "The 'Human' species must be selected to run the fetal_pbtk model"){
    if (value == "fetal_pbtk" && input$spec != "Human") message
  }

  returntimes_cond <- function(value,input,message = "A beginning output time of 91 days (13 weeks) or later must be entered"){
    if (input$model == "fetal_pbtk" && length(value)>0){
      v1 <- unlist(strsplit(value,","))
      out_times <- sapply(v1, function(x) eval(parse(text = x)))
      if (min(out_times)<91){
        message
        }
    }
  }


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
                                                           CS_UploadedCompounds(),
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

    output$Model <- shiny::renderUI({Model_Input(input$func,input$spec)})

    ##########################################################################
    # GENERATES THE PRELOADED COMPOUNDS OUTPUT
    ##########################################################################

    output$PreloadedComps <- shiny::renderUI({

      PreloadComps_UI(input$func,input$spec,input$defaulttoHuman,
                      input$model,input$insilicopars,input$HondaIVIVE)
    })

    ##########################################################################
    # COMPILES LIST OF ALL COMPOUNDS TO RUN
    ##########################################################################

    CompLst <- shiny::reactive({

      #--- COMPILES A LIST OF ALL COMPOUNDS
      CompoundList(input$httkPreloadComps,input$file1)
      })

    output$comptext <- shiny::renderTable({

      #--- OUTPUT ERROR WARNINGS IF NEEDED VARIABLES ARE MISSING
      pars <- list(input$func,input$spec,input$defaulttoHuman,input$model,input$insilicopars,
                   input$httkPreloadComps,input$file1)
      names(pars) <- c("func","spec","defaulttoHuman","model","insilicopars",
                       "httkPreloadComps","file1")
      validate_text_Common(pars)

      CompLst()
      })

    ##########################################################################
    # RESET INPUT VARIABLES IF USER CHANGES VARIABLES AFTER SIMULATION
    ##########################################################################

    shiny::observeEvent(input$func,{UpdateFunc(session)})
    shiny::observeEvent(input$spec,{UpdateSpec(session)})
    shiny::observeEvent(input$defaulttoHuman,{UpdateComps(session)})
    shiny::observeEvent(input$model,{UpdateComps(session)})
    shiny::observeEvent(input$insilicopars,{UpdateComps(session)})

    ##########################################################################
    # GATHER ALL INPUT VARIABLES
    ##########################################################################

    AllInputs <- eventReactive(input$runsim,{

      CompilePars <- function(VarName){
        eval(parse(text = paste("input$",VarName,sep = "")))
      }

      pars <- purrr::map(ParNames(),CompilePars)
      names(pars) <- ParNames()
      pars[["CompoundList"]] <- CompLst()
      pars_new <- UpdatePars(pars)})

    ##########################################################################
    # DEFINE ERRORS FOR EACH MODULE
    ##########################################################################

    iv_common <- shinyvalidate::InputValidator$new()
    iv_common$add_rule("func", shinyvalidate::sv_not_equal("Select"))
    iv_common$add_rule("spec", shinyvalidate::sv_not_equal("Select"))
    iv_common$add_rule("defaulttoHuman", shinyvalidate::sv_not_equal("Select"))
    iv_common$add_rule("model", shinyvalidate::sv_not_equal("Select"))
    iv_common$add_rule("insilicopars", shinyvalidate::sv_not_equal("Select"))
    iv_common$add_rule("httkPreloadComps", not_null, input)

    iv_adme <- shinyvalidate::InputValidator$new()
    iv_adme$add_rule("dosenum", shinyvalidate::sv_not_equal("Select"))
    iv_adme$add_rule("multdose", multdose_Select, input)
    iv_adme$add_rule("multdose_odd", multdose_odd, input)
    iv_adme$add_rule("model", fetal_cond, input)
    iv_adme$add_rule("returntimes", returntimes_cond, input)

    parent_adme_iv <- shinyvalidate::InputValidator$new()
    parent_adme_iv$add_validator(iv_common)
    parent_adme_iv$add_validator(iv_adme)
    parent_adme_iv$enable()

    iv_ivive <- shinyvalidate::InputValidator$new()
    iv_ivive$add_rule("BioactiveFile", shinyvalidate::sv_required())
    iv_ivive$add_rule("returnsamples", shinyvalidate::sv_not_equal("Select"))

    parent_ivive_iv <- shinyvalidate::InputValidator$new()
    parent_ivive_iv$add_validator(iv_common)
    parent_ivive_iv$add_validator(iv_ivive)
    parent_ivive_iv$enable()

    ##########################################################################
    # ADME TIME COURSE OUTPUTS (PLOTS, SIMULATION DATA, TK SUMMARY STATS)
    # STEADY STATE OUTPUTS (PLOT, TABLE)
    # IVIVE OUTPUTS (TABLE, PLOT)
    # PARAMETER CALCULATIONS (PLOTS, TABLES)
    ##########################################################################

    shiny::observeEvent(input$runsim,{

      if (input$func == "Concentration-time profiles"){
        if (parent_adme_iv$is_valid()){
          Notify_Computing()
          ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
        }
        else {
          Notify_ParError()
          ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
          req(parent_adme_iv$is_valid())
        }
      }
      else if (input$func == "Steady state concentrations"){
        if (iv_common$is_valid()){
          Notify_Computing()
          SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
        }
        else {
          Notify_ParError()
          SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
          req(iv_common$is_valid())
        }
      }
      else if (input$func == "In vitro in vivo extrapolation (IVIVE)"){
        if (parent_ivive_iv$is_valid()){
          Notify_Computing()
          IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
        }
        else {
          Notify_ParError()
          IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
          req(parent_ivive_iv$is_valid())
        }
      }
      else if (input$func == "Parameter calculations"){
        if (iv_common$is_valid()){
          Notify_Computing()
          PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
        }
        else{
          Notify_ParError()
          PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
          req(iv_common$is_valid())
        }
      }
    })
  }

  ####################################################################################
  # RUN R SHINY APP
  ####################################################################################

  shiny::shinyApp(ui, server)
}
