
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

    if (!is.null(pars()[["fileExposure"]])){

      #--- Outputs table of BER values
      output$BERtable <- DT::renderDT({
      sol()[[4]]}, options = list(scrollX = TRUE, scrollY = TRUE))

      #--- Outputs table caption
      output$BERtableCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      paste("Table 2: Table of the bioactivity exposure ratio (BER) for each selected
            compound.")})

      #--- Creates download button
      output$downloadBERtable_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadBER"), "Download Table 2")})

      #--- Downloads table of BER values
      output$downloadBER <- shiny::downloadHandler(
        filename = function(){paste("BERData-",Sys.Date(),".csv", sep = "")},
        content = function(file){write.csv(sol()[[4]], file)})
    }
    else{
      output$BERtableCaption <- shiny::renderText({
        shiny::req(sol(),runsim())
        paste("Chemical exposure data was not uploaded under the 'Advanced Parameters' tab,
              so the bioactivity exposure ratio (BER) cannot be calculated. If the BER is desired,
              please upload exposure data on the 'Advanced Parameters' tab under the 'Output Specification' card.")})
    }


  })
}
