
#######################################
# UI - PC PARTITION COEFFICIENTS TABLE
#######################################

#' User interface function for the parameter calculations partition coefficients
#' table
#'
#' @description
#' This function contains the user interface elements for the parameter calculations
#' partition coefficient table. This interface itself contains three UI elements:
#' a table download button, a table, and a table caption. The current function is
#' called by PC_ui().
#'
#' @param id Shiny identifier name; must be the same id used as in PC_PCTable_server()
#'
#' @return User interface for the partition coefficients table drop down with
#' three elements
#' @noRd
#'
PC_PCTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadPCtable_cond")),
                     DT::DTOutput(shiny::NS(id,"PCtable")),
                     shiny::textOutput(shiny::NS(id,"PCtableCaption"))
  )
}

############################################
# SERVER - PC PARTITION COEFFICIENTS TABLE
############################################

#' Server function for the parameter calculations partition coefficients table
#'
#' @description
#' This function contains the output elements for the parameter calculations
#' partition coefficient table. This server contains three output elements: a
#' table download button, a table, and a table caption. The current function is
#' called by PC_server().
#'
#' @param id Shiny identifier name; must be the same id used as in PC_PCTable_ui()
#' @param pc_args A Shiny reactive list with the output of Parsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the partition coefficients table with three elements
#' @noRd
#'
PC_PCTable_server <- function(id,pc_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({pc_args()[[1]]})
    pars <- shiny::reactive({pc_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of partition coefficients
    # output$PCtable<- shiny::renderTable({
    #   sol()[[2]]}, digits = 4, display = rep("fg", 16), rownames = TRUE)

    output$PCtable<- DT::renderDT({
      sol()[[2]]}, options = list(scrollX = TRUE))

    #--- Outputs partition coefficient table caption
    output$PCtableCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      "Table 2: Table of partition coefficients for all selected compounds in
      each of the available tissues (adipose, bone, brain, gut, heart, kidney,
      liver, lung, muscle, skin, spleen, red blood cells (rbc), rest - collective
      term for remaining tissues)."})

    #--- Creates table download button
    output$downloadPCtable_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadPCtable"), "Download Table 2")})

    #--- Downloads partition coefficient table
    output$downloadPCtable <- shiny::downloadHandler(
      filename = function(){paste("PCdata-",Sys.Date(),".csv", sep = "")},
      content = function(file){utils::write.csv(sol()[[2]], file)})
  })
}
