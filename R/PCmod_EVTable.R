
#######################################
# UI - PC ELIM RATE AND VDIST TABLE
#######################################

PC_EVTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadPartable_cond")),
          DT::DTOutput(shiny::NS(id,"Partable")),
          shiny::textOutput(shiny::NS(id,"PartableCaption"))
  )
}

##########################################
# SERVER - PC ELIM RATE AND VDIST TABLE
##########################################

PC_EVTable_server <- function(id, pc_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({pc_args()[[1]]})
    pars <- shiny::reactive({pc_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of elimination rate/volume of distribution
    output$Partable <- DT::renderDT({
        sol()[[1]]}, options = list(scrollX = TRUE))

    #--- Outputs table caption
    output$PartableCaption <- shiny::renderText({
      shiny::req(runsim(),sol())
      "Table 1: Table of estimated elimination rates (1/h), volumes of
      distribution (L/kg BW), half lifes (h), and total plasma clearances
      (L/h/kg BW) for all selected compounds. Compounds are listed
      in ascending order of the elimination rate."})

    #--- Creates table download button
    output$downloadPartable_cond <- shiny::renderUI({
      shiny::req(runsim(),sol())
      shiny::downloadButton(session$ns("downloadPartable"), "Download Table 1")})

    #--- Downloads table
    output$downloadPartable <- shiny::downloadHandler(
      filename = function(){paste("Pardata-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[1]], file)})
  })
}
