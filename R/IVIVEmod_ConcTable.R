
##########################
# UI - IVIVE CONC TABLE
##########################

IVIVE_Table_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadIVIVEtable_cond")),
                     shiny::uiOutput(shiny::NS(id,"downloadIVIVEtable_par")),
                     shiny::tableOutput(shiny::NS(id,"IVIVEtable")),
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

    #--- Outputs table of AED values
    output$IVIVEtable <- shiny::renderTable({
      sol()[[1]]},rownames = TRUE, digits = 4)

    #--- Outputs table caption
    output$IVIVEtableCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      if (ncol(sol()[[1]])>2){
        paste("Table 1: Table of", pars()[["samples"]], "IVIVE administered equivalent
              dose (AED) samples (", pars()[["modelIVIVEout_units"]], ") for each
              selected compound.")
      }
      else{
        paste("Table 1: Table of the IVIVE administered equivalent doses (AED)
              (", pars()[["modelIVIVEout_units"]], ") for each selected compound.", sep = "")
      }})

    #--- Creates download button
    output$downloadIVIVEtable_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadIVIVE"), "Download Table 1")})

    #--- Downloads table of AED values
    output$downloadIVIVE <- shiny::downloadHandler(
      filename = function(){paste("IVIVEData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[1]], file)})

    #--- Creates download button
    output$downloadIVIVEtable_par <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadIVIVEpars"), "Download AED Simulation Parameters")})

    #--- Downloads table of AED simulation parameters
    output$downloadIVIVEpars <- shiny::downloadHandler(
      filename = function(){paste("IVIVEData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[3]], file)})

  })
}
