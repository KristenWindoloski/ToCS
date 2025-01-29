
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
  # GENERATE INITIAL CONDITIONS
  ##########################################################################

  ics <- names_ICs()
  ic_names <- ics[[1]]
  ic_comps <- ics[[2]]

  not_null <- function(value,input, message = "At least one compound must be selected or uploaded"){
    if (is.null(c(value,input$httkPreloadComps_Honda,input$file1))) message
  }

  multdose_Select <- function(value,input,message = "The dosing frequency must be selected"){
    if (input$dosenum == "Multiple Doses" && value == "Select") message
  }

  multdose_odd <- function(value,input,message = "The dosing administration amounts and times must be entered"){
    if (input$dosenum == "Multiple Doses" && input$multdose == "No" && value == "") message
  }

  fetal_cond <- function(value,input,message = "The 'Human' species must be selected to run the fetal_pbtk model"){
    print(value)
    print(input$spec)
    if (value == "fetal_pbtk" && input$spec != "Human") message
  }

  # returntimes_cond <- function(value,input,message = "A beginning output time of 91 days (13 weeks) or later must be entered"){
  #   if (input$model == "fetal_pbtk" && length(value)>0){
  #     v1 <- unlist(strsplit(value,","))
  #     out_times <- sapply(v1, function(x) eval(parse(text = x)))
  #     if (min(out_times)<91){
  #       message
  #       }
  #   }
  # }


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


      if (input$spec != "Select" && input$func != "Select" && input$insilicopars != "Select" && input$model != "Select"){
        if (input$func == "In vitro in vivo extrapolation (IVIVE)" && input$HondaIVIVE == "Honda1"){
          htmltools::tagList(numericInput_FSBf("FSBf"),
                             shiny::showModal(shiny::modalDialog(title = "System Running",
                                                                 "Compiling chemical list. The Preloaded Compounds card will update once completed, and
                                                                 may take a moment if loading compounds with in silico parameters.
                                                                 You may click the 'Dismiss' button.")),
                             PreloadCompsInput(input$func,input$spec,input$defaulttoHuman,input$insilicopars,input$model,input$HondaIVIVE))
        }
        else {
          htmltools::tagList(shiny::showModal(shiny::modalDialog(title = "System Running",
                                                                 "Compiling chemical list. The Preloaded Compounds card will update once completed, and
                                                                 may take a moment if loading compounds with in silico parameters.
                                                                 You may click the 'Dismiss' button.")),
                             PreloadCompsInput(input$func,input$spec,input$defaulttoHuman,input$insilicopars,input$model,input$HondaIVIVE))
        }
      }
      else{
        shiny::showModal(shiny::modalDialog(title = "Missing Parameters",
                                            "Click the 'Dismiss' button and return to the 'General Parameters' and 'Model Specifications'
                                            tabs to select any missing parameters. Then return to this tab."))
      }
    })

    ##########################################################################
    # COMPILES LIST OF ALL COMPOUNDS TO RUN
    ##########################################################################

    shiny::observeEvent(input$runCompounds,{
      shiny::showModal(shiny::modalDialog("Compounds are loading into the system.
                                          Click 'Dismiss' and proceed to the next tab."))
    })

    CompLst <- shiny::reactive({

      #--- COMPILES A LIST OF ALL COMPOUNDS
      CompoundList(input$httkPreloadComps,input$httkPreloadComps_Honda,input$file1)
      })

    output$comptext <- shiny::renderTable({

      #--- OUTPUT ERROR WARNINGS IF NEEDED VARIABLES ARE MISSING
      pars <- list(input$func,input$spec,input$defaulttoHuman,input$runCompounds,input$model,
                   input$insilicopars,input$httkPreloadComps,input$httkPreloadComps_Honda,input$file1)
      names(pars) <- c("func","spec","defaulttoHuman","runCompounds","model",
                       "insilicopars","httkPreloadComps","httkPreloadComps_Honda","file1")
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
    iv_common$add_rule("runCompounds", shinyvalidate::sv_not_equal(0))
    iv_common$enable()

    iv_adme <- shinyvalidate::InputValidator$new()
    iv_adme$add_rule("dosenum", shinyvalidate::sv_not_equal("Select"))
    iv_adme$add_rule("multdose", multdose_Select, input)
    iv_adme$add_rule("multdose_odd", multdose_odd, input)
    iv_adme$add_rule("model", fetal_cond, input)
    # iv_adme$add_rule("returntimes", returntimes_cond, input)
    iv_adme$enable()

    iv_ivive <- shinyvalidate::InputValidator$new()
    iv_ivive$add_rule("BioactiveFile", shinyvalidate::sv_required())
    iv_ivive$add_rule("returnsamples", shinyvalidate::sv_not_equal("Select"))
    iv_ivive$enable()

    ##########################################################################
    # ADME TIME COURSE OUTPUTS (PLOTS, SIMULATION DATA, TK SUMMARY STATS)
    # STEADY STATE OUTPUTS (PLOT, TABLE)
    # IVIVE OUTPUTS (TABLE, PLOT)
    # PARAMETER CALCULATIONS (PLOTS, TABLES)
    ##########################################################################

    shiny::observeEvent(input$runsim,{


      if (input$func == "Concentration-time profiles"){
        if (iv_common$is_valid()){
          shiny::showNotification("Computing solution - this may take a moment. Plots and tables will be updated once completed.", type = "message", duration = NULL)
          ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
        }
        else {
          shiny::showNotification("Invalid Inputs: Check all previous tabs for missing or invalid parameters.", type = "error", duration = NULL)
          ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
          req(iv_common$is_valid(),iv_adme$is.valid())
        }
      }
      else if (input$func == "Steady state concentrations"){
        if (iv_common$is.valid()){
          shiny::showNotification("Computing solution - this may take a moment. Plots and tables will be updated once completed.", type = "message", duration = NULL)
          SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
        }
        else {
          shiny::showNotification("Invalid Inputs: Check all previous tabs for missing or invalid parameters.", type = "error", duration = NULL)
          SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
          req(iv_common$is_valid())
        }
      }
      else if (input$func == "In vitro in vivo extrapolation (IVIVE)"){
        if (iv_common$is.valid() && iv_ivive$is.valid()){
          shiny::showNotification("Computing solution - this may take a moment. Plots and tables will be updated once completed.", type = "message", duration = NULL)
          IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
        }
        else {
          shiny::showNotification("Invalid Inputs: Check all previous tabs for missing or invalid parameters.", type = "error", duration = NULL)
          IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
          req(iv_common$is_valid(),iv_ivive$is.valid())
        }
      }
      else if (input$func == "Parameter calculations"){
        if (iv_common$is_valid()){
          shiny::showNotification("Computing solution - this may take a moment. Plots and tables will be updated once completed.", type = "message", duration = NULL)
          PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
        }
        else{
          shiny::showNotification("Invalid Inputs: Check all previous tabs for missing or invalid parameters.", type = "error", duration = NULL)
          PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
          req(iv_common$is_valid())
        }

      }





      # if (iv$is_valid()){
      #   shiny::showNotification("Computing solution - this may take a moment. Plots and tables will be updated once completed.",
      #                           type = "message",
      #                           duration = NULL)
      #   if (input$func == "Concentration-time profiles"){
      #   ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
      # }
      #   else if (input$func == "Steady state concentrations"){
      #   SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      # }
      #   else if (input$func == "In vitro in vivo extrapolation (IVIVE)"){
      #   IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      # }
      #   else if (input$func == "Parameter calculations"){
      #   PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      # }
      # }
      # else{
      #   # shiny::showNotification("Error: Return to the previous tabs and check for missing or invalid parameters.", type = "error")
      #   if (input$func == "Concentration-time profiles"){
      #     ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
      #   }
      #   else if (input$func == "Steady state concentrations"){
      #     SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      #   }
      #   else if (input$func == "In vitro in vivo extrapolation (IVIVE)"){
      #     IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      #   }
      #   else if (input$func == "Parameter calculations"){
      #     PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      #   }
      #   req(iv$is_valid())
      # }


    })
  }

  ####################################################################################
  # RUN R SHINY APP
  ####################################################################################

  shiny::shinyApp(ui, server)
}
