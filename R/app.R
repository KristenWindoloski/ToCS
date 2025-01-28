
# --- Created by Kristen Windoloski
# --- Last Updated: January 28, 2025
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

    AllInputs <- shiny::eventReactive(input$runsim,{

      CompilePars <- function(VarName){
        eval(parse(text = paste("input$",VarName,sep = "")))
      }

      pars <- purrr::map(ParNames(),CompilePars)
      names(pars) <- ParNames()
      pars[["CompoundList"]] <- CompLst()
      pars_new <- UpdatePars(pars)})

    ##########################################################################
    # ADME TIME COURSE OUTPUTS (PLOTS, SIMULATION DATA, TK SUMMARY STATS)
    # STEADY STATE OUTPUTS (PLOT, TABLE)
    # IVIVE OUTPUTS (TABLE, PLOT)
    # PARAMETER CALCULATIONS (PLOTS, TABLES)
    ##########################################################################

    shiny::observeEvent(input$runsim,{

      # shiny::showModal(shiny::modalDialog(title = "Error: Missing Parameter(s)", "Click the 'Dismiss' button and view the message(s)
      #                                     under the 'Selected Compounds' and/or 'Results' card to identify missing parameters.
      #                                     Required parameters were not selected."))

      shiny::showModal(shiny::modalDialog(title = "System Running", "Computing solution - this may take a moment.
                                          Plots and tables will update once completed.
                                          You may click the 'Dismiss' button."))

      if (input$func == "Concentration-time profiles"){
        ADME_server("ADME_AllOut",AllInputs,shiny::reactive(input$runsim))
      }
      else if (input$func == "Steady state concentrations"){
        SS_server("SS_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      }
      else if (input$func == "In vitro in vivo extrapolation (IVIVE)"){
        IVIVE_server("IVIVE_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      }
      else if (input$func == "Parameter calculations"){
        PC_server("IP_AllOut",AllInputs,shiny::reactive(input$runsim),shiny::reactive(input$logscale))
      }
    })
  }

  ####################################################################################
  # RUN R SHINY APP
  ####################################################################################

  shiny::shinyApp(ui, server)
}
