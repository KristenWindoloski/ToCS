
###################################
# UI - SS DAY TABLE
###################################

#' User interface for the days to steady state table
#'
#' @description
#' This function contains the user interface elements for the days to steady state
#' table. This interface itself contains three UI elements: a table download
#' button, a table, and a table caption. The current function is called by SS_ui().
#'
#' @param id Shiny identifier name
#'
#' @return The user interface for the days to steady state table which includes
#' three outputs: a table download button, a table, and a table caption
#' @noRd
#'
SS_DayTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadSSDaytable_cond")),
                     DT::DTOutput(shiny::NS(id,"SSDaytable")),
                     shiny::textOutput(shiny::NS(id,"SSDaytableCaption"))
  )
}

###################################
# SERVER - SS DAY TABLE
###################################

#' The server function for the days to steady state table
#'
#' @description
#' This function contains the output elements for the days to steady state table.
#' This server contains three output elements: a table download button, a table,
#' and a table caption. The current function is called by SS_server().
#'
#' @param id Shiny identifier name; identifier must match that in SS_DayTable_ui()
#' @param ss_args A Shiny reactive list with the output of SS_sol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return The server function for the days to steady state table which includes
#' three outputs: a table download button, a table, and a table caption
#' @noRd
#'
SS_DayTable_server <- function(id, ss_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({ss_args()[[1]]})
    pars <- shiny::reactive({ss_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of the day were SS is reached
    output$SSDaytable <- DT::renderDT({
      sol()[[2]]}, options = list(scrollX = TRUE, scrollY = TRUE))

    #--- Outputs the table caption
    output$SSDaytableCaption <- shiny::renderText({
      shiny::req(sol(), runsim())
      paste("Table 2: Table of steady state (SS) characteristics. CssDay represents
            the number of days it takes for the model to reach the analytical plasma SS
            concentration or the fractional change of daily SS plasma concentration
            is below the set threshold, AvgConc represents the average plasma concentration (",
            pars()[["modelSSout_units"]], ") on the final day of the simulation, RatioAvgAnalytical
            represents the fraction of the analytical SS plasma concentration reached on CssDay, and
            MaxConc is the maximum plasma concentration (", pars()[["modelSSout_units"]],
            ") of the simulation.", sep = "")})

    #--- Creates download button
    output$downloadSSDaytable_cond <- shiny::renderUI({
      shiny::req(sol(), runsim())
      shiny::downloadButton(session$ns("downloadSSDay"), "Download Table 2")})

    #--- Downloads table
    output$downloadSSDay <- shiny::downloadHandler(
      filename = function(){paste("SSDayData-",Sys.Date(),".csv", sep = "")},
      content = function(file){utils::write.csv(sol()[[2]], file)})
  })
}
