
##########################
# UI - BER TABLE
##########################

BER_Table_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadBERtable_cond")),
                     DT::DTOutput(shiny::NS(id,"BERtable")),
                     shiny::textOutput(shiny::NS(id,"BERtableCaption"))
  )
}

##############################
# SERVER - BER TABLE
##############################

BER_Table_server <- function(id,ivive_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactives set to be used
    sol <- shiny::reactive({ivive_args()[[1]]})
    pars <- shiny::reactive({ivive_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of BER values
    output$IVIVEtable <- DT::renderDT({
      sol()[[1]]}, options = list(scrollX = TRUE, scrollY = TRUE))

    #--- Outputs table caption
    output$IVIVEtableCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      paste("Table 2: Table of the bioactivity exposure ratio (BER) for each selected
            compound. A BER < 1 signifies that a chemical's level of exposure is larger
            than its bioactivity threshold, and thus indicates a potential health risk.")})

    #--- Creates download button
    output$downloadBERtable_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadBER"), "Download Table 2")})

    #--- Downloads table of BER values
    output$downloadBER <- shiny::downloadHandler(
      filename = function(){paste("BERData-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[1]], file)})

  })
}
