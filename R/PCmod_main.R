
##########################
# UI - IP MAIN MODULE
##########################

#' Main user interface function to set up the 'Results' card under the Parameter
#' Calculations module's 'Run Simulation' tab
#'
#' #' @description
#' This function outputs the structure of the user interface for the Parameter
#' Calculations 'Results' card under the 'Run Simulation' tab. Five drop down
#' tabs appear: one for a parameter plot, one for a parameter table, one for a
#' partition coefficient plot, one for a partition coefficient table, and one for
#' a download option for simulation parameters. The user interface contents of
#' each drop down is then filled by a separate UI module.
#'
#' @param id Shiny identifier name; must be the same id used as in PC_server()
#'
#' @return Five user interface drop down tabs identifying space for two plots,
#' two tables, and a download button of parameter calculation results.
#' @seealso [PC_EVPlot_ui()], [PC_EVTable_ui()], [PC_PCPlot_ui()], [PC_PCTable_ui()],
#' and [PC_Pars_ui()], which are all called by this function
#' @export
#'
PC_ui <- function(id) {

  ####################################################################################
  # 1) ELIM_VDIST PARAMETER PLOTS WITH DOWNLOAD OPTION
  # 2) ELIM_VDIST PARAMETER TABLE WITH DOWNLOAD OPTION
  # 3) PARTITION COEFFICIENT PARAMETER PLOT WITH DOWNLOAD OPTION
  # 4) PARTITION COEFFICIENT PARAMETER TABLE WITH DOWNLOAD OPTION
  # 5) SIMULATION PARAMETERS DOWNLOAD
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Parameter Plots", PC_EVPlot_ui(shiny::NS(id,"PC_EVPlot")),height = "100%"),
                   bslib::accordion_panel("Parameter Table", PC_EVTable_ui(shiny::NS(id,"PC_EVTable"))),
                   bslib::accordion_panel("Partition Coefficient Plots", PC_PCPlot_ui(shiny::NS(id,"PC_PCPlot")),height = "100%"),
                   bslib::accordion_panel("Partition Coefficient Table", PC_PCTable_ui(shiny::NS(id,"PC_PCTable"))),
                   bslib::accordion_panel("Simulation Parameters", PC_Pars_ui(shiny::NS(id,"PC_Pars")))
  )

}

##########################
# SERVER - IP MAIN MODULE
##########################

#' Main server function to set up the 'Results' card under the Parameter
#' Calculation module's Run Simulation' tab
#'
#' @param id Shiny identifier name; must be the same id used as in PC_ui()
#' @param pars A list of all user input parameters for the entire app
#' @param runsim Action button titled 'Run Simulation' pressed by the user
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale
#'
#' @return The five server outputs that fill the main Parameter Calculations
#' user interface function consisting of two plots, two tables, and a download
#' button of simulation parameters of Parameter Calculation results
#' @seealso [validate_text_pc()], [Parsol()], [PC_EVPlot_server()], [PC_EVTable_server()],
#' [PC_PCPlot_server()], [PC_PCTable_server()], and [PC_Pars_server()], which
#' are all called by this function, and [RS_Results()], which calls the current
#' function
#' @export
#'
PC_server <- function(id,pars,runsim,logscale) {

  shiny::moduleServer(id, function(input, output, session) {


    out <- shiny::eventReactive(runsim(),{

      validate_text_PC(pars())
      Parsol(pars())
    })

    pc_out <- shiny::reactive({list(out(),pars(),logscale())})

    ####################################################################################
    # 1) ELIM_VDIST PARAMETER PLOTS WITH DOWNLOAD OPTION
    # 2) ELIM_VDIST PARAMETER TABLE WITH DOWNLOAD OPTION
    # 3) PARTITION COEFFICIENT PARAMETER PLOT WITH DOWNLOAD OPTION
    # 4) PARTITION COEFFICIENT PARAMETER TABLE WITH DOWNLOAD OPTION
    # 5) SIMULATION PARAMETERS DOWNLOAD
    ####################################################################################

    PC_EVPlot_server("PC_EVPlot",pc_out)
    PC_EVTable_server("PC_EVTable",pc_out)
    PC_PCPlot_server("PC_PCPlot",pc_out)
    PC_PCTable_server("PC_PCTable",pc_out)
    PC_Pars_server("PC_Pars",pc_out)

  })

}
