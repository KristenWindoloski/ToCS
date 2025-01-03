
#####################################
# UI - ADME TK SUMMARY TABLE MODULE
#####################################

ADME_TKTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadtk_cond")),
                     DT::DTOutput(shiny::NS(id,"tksummary")),
                     shiny::textOutput(shiny::NS(id,"tksummaryCaption"))
  )
}

##########################################
# SERVER - ADME TK SUMMARY TABLE MODULE
##########################################

ADME_TKTable_server <- function(id,adme_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Set reactives to be used
    sol <- shiny::reactive({adme_args()[[1]]})
    pars <- shiny::reactive({adme_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of TK summary data
    output$tksummary <- DT::renderDT({
      sol()[[2]]}, options = list(scrollX = TRUE, scrollY = TRUE))

    #--- Outputs caption of table
    output$tksummaryCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      "Table 1: Table of summary statistics (Tmax - time to maximal concentration,
      MaxValue - maximal amount (A, umol) or concentration (C, uM), AUC - area
      under the curve (uM*days)) for each compartment for each selected compound."})

    #--- Creates download button
    output$downloadtk_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadTK"), "Download Table 1")})

    #--- Downloads TK data summary as a CSV
    output$downloadTK <- shiny::downloadHandler(
      filename = function(){paste("tkdata-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[2]], file)})

  })
}
