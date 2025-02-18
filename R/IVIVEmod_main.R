
##########################
# UI - IVIVE MAIN MODULE
##########################

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
