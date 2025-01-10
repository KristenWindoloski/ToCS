
##########################
# UI - IP MAIN MODULE
##########################

PC_ui <- function(id) {

  ####################################################################################
  # 1) ELIM_VDIST PARAMETER PLOTS WITH DOWNLOAD OPTION
  # 2) ELIM_VDIST PARAMETER TABLE WITH DOWNLOAD OPTION
  # 3) PARTITION COEFFICIENT PARAMETER PLOT WITH DOWNLOAD OPTION
  # 4) PARTITION COEFFICIENT PARAMETER TABLE WITH DOWNLOAD OPTION
  # 5) SIMULATION PARAMETERS DOWNLOAD
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Parameter Plots", PC_EVPlot_ui(shiny::NS(id,"PC_EVPlot"))),
                   bslib::accordion_panel("Parameter Table", PC_EVTable_ui(shiny::NS(id,"PC_EVTable"))),
                   bslib::accordion_panel("Partition Coefficient Plots", PC_PCPlot_ui(shiny::NS(id,"PC_PCPlot"))),
                   bslib::accordion_panel("Partition Coefficient Table", PC_PCTable_ui(shiny::NS(id,"PC_PCTable"))),
                   bslib::accordion_panel("Simulation Parameters", PC_Pars_ui(shiny::NS(id,"PC_Pars")))
  )

}

##########################
# SERVER - IP MAIN MODULE
##########################

PC_server <- function(id,pars,runsim,logscale) {

  shiny::moduleServer(id, function(input, output, session) {


    out <- shiny::eventReactive(runsim(),{

      shiny::showModal(shiny::modalDialog(title = "System Running",
                                        "Computing solution. Plots and tables will update once completed. Please wait..."))
      validate_text_Pars(pars())
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
