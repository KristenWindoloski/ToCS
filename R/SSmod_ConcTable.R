
###################################
# UI - SS CONCENTRATION TABLE
###################################

#' User interface for the steady state concentrations table
#'
#' @param id Shiny identifier name; must match the identifier in the SS_ConcTable_server()
#'
#' @return User interface for the steady state concentrations table which includes
#' four elements: a table download button, a simulation parameters download button,
#' the steady state concentrations table, and a table caption
#' @seealso [SS_ui()], which calls the current function
#' @export
#'
SS_ConcTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadSStable_cond")),
                     shiny::uiOutput(shiny::NS(id,"downloadSStable_pars")),
                     DT::DTOutput(shiny::NS(id,"SStable")),
                     shiny::textOutput(shiny::NS(id,"SStableCaption"))
  )
}

###################################
# SERVER - SS CONCENTRATION TABLE
###################################

#' Server function for the steady state concentrations table
#'
#' @param id Shiny identifier name; must match the identifier in SS_ConcTable_ui()
#' @param ss_args A Shiny reactive list with the output of SS_sol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the steady state concentrations table which includes
#' four elements: a table download button, a simulation parameters download button,
#' the steady state concentrations table, and a table caption
#' @seealso [SS_server()], which calls the current function
#' @export
#'
SS_ConcTable_server <- function(id, ss_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({ss_args()[[1]]})
    pars <- shiny::reactive({ss_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs a table of the SS concentrations
    output$SStable <- DT::renderDT({
      sol()[[1]]}, options = list(scrollX = TRUE, scrollY = TRUE))

    #--- Outputs table caption
    output$SStableCaption <- shiny::renderText({
      shiny::req(sol(), runsim())
      paste("Table 1: Table of the long-term constant infusion (steady state) concentrations (",
            pars()[["modelSSout_units"]], ") for the selected compounds. Compounds are arranged in ascending
            order of their concentration values.", sep = "")})

    #--- Creates download button
    output$downloadSStable_cond <- shiny::renderUI({
      shiny::req(sol(), runsim())
      downloadButton(session$ns("downloadSS"), "Download Table 1")})

    #--- Downloads SS table
    output$downloadSS <- shiny::downloadHandler(
      filename = function(){paste("SteadyStateData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[1]], file)})

    #--- Creates download button
    output$downloadSStable_pars <- shiny::renderUI({
      shiny::req(sol(), runsim())
      downloadButton(session$ns("downloadSSpars"), "Download Simulation Parameters")})

    #--- Downloads SS simulation parameters
    output$downloadSSpars <- shiny::downloadHandler(
      filename = function(){paste("SteadyStatePars-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[3]], file)})
  })
}
