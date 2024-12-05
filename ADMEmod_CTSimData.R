
############################################
# UI - ADME CT SIMULATION DATA MODULE
############################################

ADME_TCData_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadADME_data")),
                     shiny::uiOutput(shiny::NS(id,"downloadADME_pars")))
}

############################################
# SERVER - ADME CT SIMULATION DATA MODULE
############################################

ADME_TCData_server <- function(id,adme_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Set reactives to be used
    sol <- shiny::reactive({adme_args()[[1]]})
    pars <- shiny::reactive({adme_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Creates download button
    output$downloadADME_data <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadADME"), "Download ADME Time Course Data")})

    #--- Downloads time-course simulation data
    output$downloadADME <- shiny::downloadHandler(
      filename = function(){paste("ADMEdata-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[1]], file)})

    #--- Creates download button
    output$downloadADME_pars <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadADMEpars"), "Download ADME Simulation Parameters")})

    #--- Downloads time-course simulation parameters
    output$downloadADMEpars <- shiny::downloadHandler(
      filename = function(){paste("ADME_Pars-",Sys.Date(),".csv", sep = "")},
      content = function(file){utils::write.csv(sol()[[3]], file)})

  })
}
