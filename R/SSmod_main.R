
##########################
# UI - SS MAIN MODULE
##########################

#' User interface layout for the steady state concentrations module and is composed
#' of three submodules
#'
#' @description
#' This function acts as the main function to set up the user interface for the
#' steady state concentrations module. The UI layout includes three elements: a
#' plot and two tables. Each UI element has its own UI function. The current
#' function calls SS_ConcPlot_ui(), SS_ConcTable_ui(), and SS_DayTable_ui() and
#' is called by RS_Results().
#'
#' @param id Shiny identifier name; must match the identifier in the SS_server()
#'
#' @return User interface layout for the steady state concentrations module on the 'Run Simulation'
#' tab; includes one plot and two table drop down panels
#' @noRd
#'
SS_ui <- function(id) {

  ####################################################################################
  # 1) SS CONCENTRATION PLOT WITH DOWNLOAD FIGURE OPTION
  # 2) SS CONCENTRATIONS TABLE AND DOWNLOAD OPTION
  # 3) NUMER OF DAYS TO STEADY STATE TABLE AND DOWNLOAD OPTION
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Steady State Concentration Plot", SS_ConcPlot_ui(shiny::NS(id,"SS_ConcPlot")),height = "100%"),
                   bslib::accordion_panel("Steady State Concentrations Table", SS_ConcTable_ui(shiny::NS(id,"SS_ConcTable"))),
                   bslib::accordion_panel("Days to Steady State Table", SS_DayTable_ui(shiny::NS(id,"SS_DayTable")))
            )

}

##########################
# SERVER - SS MAIN MODULE
##########################

#' Server output for the steady state concentrations module and is composed of
#' three submodules
#'
#' @description
#' This function acts as the main function to fill the user interface for the
#' steady state concentrations module. The server outputs include three elements: a
#' plot and two tables. Each server element has its own server function. The
#' current function calls validate_text_SS(), SS_sol(), SS_ConcPlot_server(),
#' SS_ConcTable_server(), and SS_DayTable_server() and is called by
#' Run_Simulation().
#'
#' @param id Shiny identifier name; must match the identifier in the SS_ui()
#' @param pars A list of all user input parameters for the entire app
#' @param runsim Action button titled 'Run Simulation' pressed by the user
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale
#'
#' @return Server output for the steady state concentrations module on the 'Run Simulation'
#' tab; includes one plot and two table drop down panels
#' @noRd
#'
SS_server <- function(id,pars,runsim,logscale) {

  shiny::moduleServer(id, function(input, output, session) {

    out <- shiny::eventReactive(runsim(),{

      validate_text_SS(pars())
      SS_sol(pars())
    })
    ss_out <- shiny::reactive({list(out(),pars(),logscale())})

    ####################################################################################
    # 1) SS CONCENTRATION PLOT WITH DOWNLOAD FIGURE OPTION
    # 2) SS CONCENTRATIONS TABLE AND DOWNLOAD OPTION
    # 3) NUMER OF DAYS TO STEADY STATE TABLE AND DOWNLOAD OPTION
    ####################################################################################

    SS_ConcPlot_server("SS_ConcPlot",ss_out)
    SS_ConcTable_server("SS_ConcTable",ss_out)
    SS_DayTable_server("SS_DayTable",ss_out)

  })

}
