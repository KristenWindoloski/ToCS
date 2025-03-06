
##########################
# UI - IVIVE MAIN MODULE
##########################

#' Main user interface function to set up the 'Results' card under the IVIVE module
#'
#' @param id Shiny identifier name; must be the same id used as in IVIVE_server()
#'
#' @return The four user interface panels consisting of two plots and two tables
#' of IVIVE results; this main module is made up of four submodules
#' @export
#'
IVIVE_ui <- function(id) {

  ####################################################################################
  # 1) IVIVE AED DOSE TABLE WITH DOWNLOAD OPTION
  # 2) IVIVE AED DOSE PLOT WITH DOWNLOAD FIGURE OPTION
  # 3) BIOACTIVITY EXPOSURE RATIO TABLE WITH DOWNLOAD OPTION, IF APPLICABLE
  # 4) BIOACTIVITY EXPOSURE RATION PLOT WITH DOWNLOAD OPTION, IF APPLICABLE
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Oral Equivalent Dose Table", IVIVE_Table_ui(shiny::NS(id,"IVIVE_table"))),
                   bslib::accordion_panel("Oral Equivalent Dose Plot", IVIVE_Plot_ui(shiny::NS(id,"IVIVE_plot"))),
                   bslib::accordion_panel("Bioactivity Exposure Ratio Table", BER_Table_ui(shiny::NS(id,"BER_table"))),
                   bslib::accordion_panel("Bioactivity Exposure Ratio Plot", BER_Plot_ui(shiny::NS(id,"BER_plot")))
  )

}

##############################
# SERVER - IVIVE MAIN MODULE
##############################

#' Main server function to set up the 'Results' card under the IVIVE module
#'
#' @param id Shiny identifier name; must be the same id used as in IVIVE_ui()
#' @param pars A list of all user input parameters for the entire app
#' @param runsim Action button titled 'Run Simulation' pressed by the user
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale
#'
#' @return The four outputs that correspond to the main IVIVE user interface
#' function consisting of two plots and two tables of IVIVE results; this main
#' module is made up of four submodules
#' @export
#'
IVIVE_server <- function(id,pars,runsim,logscale) {

  shiny::moduleServer(id, function(input, output, session) {

    out <- shiny::eventReactive(runsim(),{

      validate_text_IVIVE(pars())
      sol <- IVIVEsol(pars())
    })

    ivive_out <- shiny::reactive({list(out(),pars(),logscale())})


    #############################################################
    # 1) IVIVE AED DOSE TABLE WITH DOWNLOAD OPTION
    # 2) IVIVE AED DOSE PLOT WITH DOWNLOAD FIGURE OPTION
    # 3) BIOACTIVITY EXPOSURE RATIO TABLE WITH DOWNLOAD OPTION, IF APPLICABLE
    # 4) BIOACTIVITY EXPOSURE RATION PLOT WITH DOWNLOAD OPTION, IF APPLICABLE
    #############################################################

    IVIVE_Table_server("IVIVE_table",ivive_out)
    IVIVE_Plot_server("IVIVE_plot",ivive_out)
    BER_Table_server("BER_table",ivive_out)
    BER_Plot_server("BER_plot",ivive_out)
  })

}
