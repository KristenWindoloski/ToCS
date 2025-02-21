
##########################
# UI - IVIVE CONC TABLE
##########################

IVIVE_Table_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadIVIVEtable_cond")),
                     shiny::uiOutput(shiny::NS(id,"downloadIVIVEtable_par")),
                     DT::DTOutput(shiny::NS(id,"IVIVEtable")),
                     shiny::textOutput(shiny::NS(id,"IVIVEtableCaption"))
  )
}

##############################
# SERVER - IVIVE CONC TABLE
##############################

IVIVE_Table_server <- function(id,ivive_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactives set to be used
    sol <- shiny::reactive({ivive_args()[[1]]})
    pars <- shiny::reactive({ivive_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of OED values
    output$IVIVEtable <- DT::renderDT({
      sol()[[1]]}, options = list(scrollX = TRUE, scrollY = TRUE))

    #--- Outputs table caption
    output$IVIVEtableCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      if (pars()[["returnsamples"]] == TRUE){
        paste("Table 1: Table of", pars()[["samples"]], "IVIVE oral equivalent
              dose (OED) samples (", pars()[["modelIVIVEout_units"]], ") for each
              selected compound. OED_5 represents the 5th OED quantile
              (using the 95th quantile steady state concentration).")
      }
      else{
        paste("Table 1: Table of the IVIVE oral equivalent doses (OED)
              (", pars()[["modelIVIVEout_units"]], ") for each selected compound.", sep = "")
      }})

    #--- Creates download button
    output$downloadIVIVEtable_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadIVIVE"), "Download Table 1")})

    #--- Downloads table of OED values
    output$downloadIVIVE <- shiny::downloadHandler(
      filename = function(){paste("IVIVEData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[1]], file)})

    #--- Creates download button
    output$downloadIVIVEtable_par <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadIVIVEpars"), "Download OED Simulation Parameters")})

    #--- Downloads table of OED simulation parameters
    output$downloadIVIVEpars <- shiny::downloadHandler(
      filename = function(){paste("IVIVEData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[3]], file)})

  })
}
