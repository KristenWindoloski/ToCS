
#######################################
# UI - PC PARAMETERS
#######################################

PC_Pars_ui <- function(id){

  shiny::uiOutput(shiny::NS(id,"downloadPars"))
}

#######################################
# SERVER - PC PARAMETERS
#######################################

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
      content = function(file){write.csv(sol()[[3]], file)})
  })
}
