
############################################
# UI - ADME CT SIMULATION DATA MODULE
############################################

#' User interface function for the concentration-time profile data download
#'
#' @description
#' This function outputs the user interface for the concentration-time profile
#' data download drop down in the results card under the 'Run Simulation' tab.
#' The interface has two outputs: a download button for concentration-time profile
#' data and a download button for simulation parameters. The current function is
#' called by ADME_ui().
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_TCData_server()
#'
#' @return User interface for the data download drop down with two download buttons
#' @noRd
#'
ADME_TCData_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadADME_data")),
                     shiny::uiOutput(shiny::NS(id,"downloadADME_pars")))
}

############################################
# SERVER - ADME CT SIMULATION DATA MODULE
############################################

#' Server function for the concentration-time profile data download
#'
#' @description
#' This function generates the outputs defined in the ADME_TCData_ui()
#' function. This connects the two download buttons with the data to download.
#' The current function is called by ADME_server() and calls validate_text_ADME().
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_TCData_ui()
#' @param adme_args A Shiny reactive list with the output of modsol() and all shiny
#' parameters in pars()
#'
#' @return Server outputs for the concentration-time profile data download drop
#' down which includes two elements
#' @noRd
#'
ADME_TCData_server <- function(id,adme_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Set reactives to be used
    sol <- shiny::reactive({adme_args()[[1]]})
    pars <- shiny::reactive({adme_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Creates download button
    output$downloadADME_data <- shiny::renderUI({
      shiny::req(sol(),runsim())
      validate_text_ADME(pars())
      shiny::downloadButton(session$ns("downloadADME"), "Download ADME Time Course Data")})

    #--- Downloads time-course simulation data
    output$downloadADME <- shiny::downloadHandler(
      filename = function(){paste("ADMEdata-",Sys.Date(),".csv", sep = "")},
      content = function(file){utils::write.csv(sol()[[1]], file)})

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
