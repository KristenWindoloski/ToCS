
##########################
# UI - IVIVE MAIN MODULE
##########################

#' Main user interface function to set up the 'Results' card under the IVIVE
#' module's 'Run Simulation' tab
#'
#' @description
#' This function outputs the structure of the user interface for the IVIVE 'Results'
#' card under the 'Run Simulation' tab. Four drop down tabs appear: one for an
#' oral equivalent dose (OED) table, one for an OED plot, one for a bioactivity
#' exposure ratio (BER) table, and one for a BER plot. The user interface contents
#' of each drop down is then filled by a separate UI module. The current function
#' calls IVIVE_Table_ui(), IVIVE_Plot_ui(), BER_Table_ui(), and BER_Plot_ui() and
#' is called by RS_Results().
#'
#'
#' @param id Shiny identifier name; must be the same id used as in IVIVE_server()
#'
#' @return Four user interface drop down tabs identifying space for two plots
#' and two tables of IVIVE results.
#' @noRd
#'
IVIVE_ui <- function(id) {

  ####################################################################################
  # 1) IVIVE OED DOSE TABLE WITH DOWNLOAD OPTION
  # 2) IVIVE OED DOSE PLOT WITH DOWNLOAD FIGURE OPTION
  # 3) BIOACTIVITY EXPOSURE RATIO TABLE WITH DOWNLOAD OPTION, IF APPLICABLE
  # 4) BIOACTIVITY EXPOSURE RATIO PLOT WITH DOWNLOAD OPTION, IF APPLICABLE
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Oral Equivalent Dose Table", IVIVE_Table_ui(shiny::NS(id,"IVIVE_table"))),
                   bslib::accordion_panel("Oral Equivalent Dose Plot", IVIVE_Plot_ui(shiny::NS(id,"IVIVE_plot")),height = "100%"),
                   bslib::accordion_panel("Bioactivity Exposure Ratio Table", BER_Table_ui(shiny::NS(id,"BER_table"))),
                   bslib::accordion_panel("Bioactivity Exposure Ratio Plot", BER_Plot_ui(shiny::NS(id,"BER_plot")),height = "100%")
  )

}

##############################
# SERVER - IVIVE MAIN MODULE
##############################

#' Main server function to set up the 'Results' card under the IVIVE module's
#' 'Run Simulation' tab
#'
#' @description
#' This function calculates the IVIVE solution and outputs the contents of the
#' UI objects for the IVIVE 'Results' card under the 'Run Simulation' tab. The
#' contents of each UI object (drop down) are then filled by four separate
#' server modules. The current function is called by validate_text_IVIVE(),
#' IVIVEsol(), IVIVE_Table_server(), IVIVE_Plot_server(), BER_Table_server(),
#' and BER_Plot_server() and calls Run_Simulation().
#'
#' @param id Shiny identifier name; must be the same id used as in IVIVE_ui()
#' @param pars A list of all user input parameters for the entire app
#' @param runsim Action button titled 'Run Simulation' pressed by the user
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale
#'
#' @return The four server outputs that fill the main IVIVE user interface
#' function consisting of two plots and two tables of IVIVE results
#' @noRd
#'
IVIVE_server <- function(id,pars,runsim,logscale) {

  shiny::moduleServer(id, function(input, output, session) {

    out <- shiny::eventReactive(runsim(),{

      validate_text_IVIVE(pars())
      sol <- IVIVEsol(pars())
    })

    ivive_out <- shiny::reactive({list(out(),pars(),logscale())})

    #############################################################
    # 1) IVIVE OED DOSE TABLE WITH DOWNLOAD OPTION
    # 2) IVIVE OED DOSE PLOT WITH DOWNLOAD FIGURE OPTION
    # 3) BIOACTIVITY EXPOSURE RATIO TABLE WITH DOWNLOAD OPTION, IF APPLICABLE
    # 4) BIOACTIVITY EXPOSURE RATION PLOT WITH DOWNLOAD OPTION, IF APPLICABLE
    #############################################################

    IVIVE_Table_server("IVIVE_table",ivive_out)
    IVIVE_Plot_server("IVIVE_plot",ivive_out)
    BER_Table_server("BER_table",ivive_out)
    BER_Plot_server("BER_plot",ivive_out)
  })
}
