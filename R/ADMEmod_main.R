
##########################
# UI - ADME MAIN MODULE
##########################

#' Main user interface function to set up the 'Results' card under the
#' concentration-time profiles module's 'Run Simulation' tab
#'
#'#' @description
#' This function outputs the structure of the user interface for the concentration-
#' time profile 'Results' card under the 'Run Simulation' tab. Four drop down
#' tabs appear: a single plot with all compound concentrations in all model
#' compartments, multiple plots with one plot per compound, a download option for
#' users to download time course data and simulation parameters, and a table with
#' toxicokinetic summary statistics (Cmax, Tmax, AUC). The user interface contents
#' of each drop down is then filled by a separate UI module.
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_server()
#'
#' @return Four user interface drop down tabs identifying space for two plots, two
#' download buttons, and one table of concentration-time profile results.
#' @seealso [ADME_MultPlt_ui()], [ADME_IndPlt_ui()], [ADME_TCData_ui()], and
#' [ADME_TKTable_ui()], which are all called by this function, and [RS_Results()],
#' which calls the current function
#' @export
#'
ADME_ui <- function(id) {

  ####################################################################################
  # 1) MULTI-COMPOUND PLOT RETURN AND OPTION TO DOWNLOAD FIGURE
  # 2) INDIVIDUAL COMPOUND PLOTS RETURN AND OPTION TO DOWNLOAD FIGURES
  # 3) TIME COURSE DATA FILE DOWNLOAD
  # 4) TK SUMMARY STATS OUTPUT AND DATA DOWNLOAD
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Multi-Curve Time Course Plot", ADME_MultPlt_ui(shiny::NS(id,"MultPlt")),height = "100%"),
                   bslib::accordion_panel("Individual Time Course Plots", ADME_IndPlt_ui(shiny::NS(id,"IndPlt"))),
                   bslib::accordion_panel("Time Course Data", ADME_TCData_ui(shiny::NS(id,"TCData"))),
                   bslib::accordion_panel("TK Summary Data", ADME_TKTable_ui(shiny::NS(id,"TKTable")))
  )
}

###############################
# SERVER - ADME MAIN MODULE
###############################

#' Main server function to set up the 'Results' card under the concentration-time
#' profile module's 'Run Simulation' tab
#'
#' #' @description
#' This function calculates the concentration-time profile's solution and outputs
#' the contents of the UI objects for the concentration-time profiles' 'Results'
#' card under the 'Run Simulation' tab. The contents of each UI object (drop
#' down) are then filled by four separate server modules.
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_ui()
#' @param pars A list of all user input parameters for the entire app
#' @param runsim Action button titled 'Run Simulation' pressed by the user
#'
#' @return The four server outputs that fill the main concentration-time profile
#' user interface function consisting of two plots, two download buttons, and
#' one table of concentration-time profile results
#' @seealso [validate_text_ADME()], [modsol()], [ADME_MultPlt_server()],
#' [ADME_IndPlt_server()], [ADME_TCData_server()], and [ADME_TKTable_server()],
#' which are called by this function, and [Run_Simulation()], which calls the
#' current function
#' @export
#'
ADME_server <- function(id,pars,runsim) {

  shiny::moduleServer(id, function(input, output, session) {

    adme_out <- shiny::eventReactive(runsim(),{

      validate_text_ADME(pars())
      sol <- modsol(pars())
      out <- list(sol,pars())
    })

    ####################################################################################
    # 1) MULTI-COMPOUND PLOT RETURN AND OPTION TO DOWNLOAD FIGURE
    # 2) INDIVIDUAL COMPOUND PLOTS RETURN AND OPTION TO DOWNLOAD FIGURES
    # 3) TIME COURSE DATA FILE DOWNLOAD
    # 4) TK SUMMARY STATS OUTPUT AND DATA DOWNLOAD
    ####################################################################################

    ADME_MultPlt_server("MultPlt",adme_out)
    ADME_IndPlt_server("IndPlt",adme_out)
    ADME_TCData_server("TCData",adme_out)
    ADME_TKTable_server("TKTable",adme_out)
  })

}
