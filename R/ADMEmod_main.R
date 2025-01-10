
##########################
# UI - ADME MAIN MODULE
##########################

ADME_ui <- function(id) {

  ####################################################################################
  # 1) MULTI-COMPOUND PLOT RETURN AND OPTION TO DOWNLOAD FIGURE
  # 2) INDIVIDUAL COMPOUND PLOTS RETURN AND OPTION TO DOWNLOAD FIGURES
  # 3) TIME COURSE DATA FILE DOWNLOAD
  # 4) TK SUMMARY STATS OUTPUT AND DATA DOWNLOAD
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Multi-Curve Time Course Plot", ADME_MultPlt_ui(shiny::NS(id,"MultPlt"))),
                   bslib::accordion_panel("Individual Time Course Plots", ADME_IndPlt_ui(shiny::NS(id,"IndPlt"))),
                   bslib::accordion_panel("Time Course Data", ADME_TCData_ui(shiny::NS(id,"TCData"))),
                   bslib::accordion_panel("TK Summary Data", ADME_TKTable_ui(shiny::NS(id,"TKTable")))
  )
}

###############################
# SERVER - ADME MAIN MODULE
###############################

ADME_server <- function(id,pars,runsim) {

  shiny::moduleServer(id, function(input, output, session) {

    adme_out <- shiny::eventReactive(runsim(),{

      shiny::showModal(shiny::modalDialog(title = "System Running",
                                          "Computing solution. Plots and tables will update once completed. Please wait..."))
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
