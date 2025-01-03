
###################################
# UI - SS DAY TABLE
###################################

SS_DayTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadSSDaytable_cond")),
                     DT::DTOutput(shiny::NS(id,"SSDaytable")),
                     shiny::textOutput(shiny::NS(id,"SSDaytableCaption"))
  )
}

###################################
# SERVER - SS DAY TABLE
###################################

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
            is below the set threshold, AvgConc represents the average plasma concentration (uM)
            on the final day of the simulation, RatioAvgAnalytical represents the
            fraction of the analytical SS plasma concentration reached on CssDay, and
            MaxConc is the maximum plasma concentration (uM) of the simulation.", sep = "")})

    #--- Creates download button
    output$downloadSSDaytable_cond <- shiny::renderUI({
      shiny::req(sol(), runsim())
      shiny::downloadButton(session$ns("downloadSSDay"), "Download Table 2")})

    #--- Downloads table
    output$downloadSSDay <- shiny::downloadHandler(
      filename = function(){paste("SSDayData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[2]], file)})
  })
}
