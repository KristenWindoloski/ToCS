
##########################
# UI - SS MAIN MODULE
##########################

SS_ui <- function(id) {

  ####################################################################################
  # 1) SS CONCENTRATION PLOT WITH DOWNLOAD FIGURE OPTION
  # 2) SS CONCENTRATIONS TABLE AND DOWNLOAD OPTION
  # 3) NUMER OF DAYS TO STEADY STATE TABLE AND DOWNLOAD OPTION
  ####################################################################################

  bslib::accordion(bslib::accordion_panel("Steady State Concentration Plot", SS_ConcPlot_ui(shiny::NS(id,"SS_ConcPlot"))),
                   bslib::accordion_panel("Steady State Concentrations Table", SS_ConcTable_ui(shiny::NS(id,"SS_ConcTable"))),
                   bslib::accordion_panel("Days to Steady State Table", SS_DayTable_ui(shiny::NS(id,"SS_DayTable")))
            )

}

##########################
# SERVER - SS MAIN MODULE
##########################

SS_server <- function(id,pars,runsim,logscale) {

  shiny::moduleServer(id, function(input, output, session) {

    out <- shiny::eventReactive(runsim(),{

      validate_text_Common(pars())
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
