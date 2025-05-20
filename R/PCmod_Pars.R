
#######################################
# UI - PC PARAMETERS
#######################################

#' User interface function for the paramater calculations simulation parameters
#' download option
#'
#' @description
#' Connected to PC_ui(), which calls the current function.
#'
#' @param id Shiny identifier name; must be the same id used as in PC_Pars_server()
#'
#' @return User interface for the parameter download drop down with one element
#' @noRd
#'
PC_Pars_ui <- function(id){

  shiny::uiOutput(shiny::NS(id,"downloadPars"))
}

#######################################
# SERVER - PC PARAMETERS
#######################################

#' Server function for the parameter calculations simulation parameters
#'
#' @description Connected to PC_server(), which calls the current function.
#'
#' @param id Shiny identifier name; must be the same id used as in PC_Pars_ui()
#' @param pc_args A Shiny reactive list with the output of Parsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the parameter download drop down with one element
#' @noRd
#'
PC_Pars_server <- function(id,pc_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({pc_args()[[1]]})
    pars <- shiny::reactive({pc_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Creates table download button
    output$downloadPars <- shiny::renderUI({
      shiny::req(runsim(),sol())
      shiny::downloadButton(session$ns("downloadSimPars"), "Download Simulation Parameters")})

    #--- Downloads table
    output$downloadSimPars <- shiny::downloadHandler(
      filename = function(){paste("ParameterSimData-",Sys.Date(),".csv", sep = "")},
      content = function(file){utils::write.csv(sol()[[3]], file)})
  })
}
