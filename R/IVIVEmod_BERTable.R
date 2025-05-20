
##########################
# UI - BER TABLE
##########################

#' User interface function for the BER table
#'
#' #' @description
#' This function outputs the user interface for the BER table drop down in the
#' results card under the 'Run Simulation' tab. The interface has three outputs:
#' a table download button, a table with calculated BERs, and a table caption.
#'
#' @param id Shiny identifier name; must be the same id used as in BER_Table_server()
#'
#' @return User interface for the BER table drop down with three elements
#' @seealso [IVIVE_ui()], which calls the current function
#' @export
#'
BER_Table_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadBERtable_cond")),
                     DT::DTOutput(shiny::NS(id,"BERtable")),
                     shiny::textOutput(shiny::NS(id,"BERtableCaption"))
  )
}

##############################
# SERVER - BER TABLE
##############################

#' Server function for the IVIVE dose table
#'
#' #' @description
#' This function generates the outputs defined in the BER_Table_ui()
#' function. This connects the download buttons with the table to download,
#' fills the data table with content, and creates the text for the table caption.
#'
#' @param id Shiny identifier name; must be the same id used as in BER_Table_ui()
#' @param ivive_args A Shiny reactive list with the output of IVIVEsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the BER table drop down which includes three elements
#' @seealso [IVIVE_server()], which calls the current function
#' @export
#'
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
        content = function(file){utils::write.csv(sol()[[4]], file)})
    }
    else{
      output$BERtableCaption <- shiny::renderText({
        shiny::req(sol(),runsim())
        paste("Chemical exposure data was not uploaded under the 'Compound Selection' tab,
              so the bioactivity exposure ratio (BER) cannot be calculated. If the BER is desired,
              please upload exposure data on the 'Compound Selection' tab under the 'Output Specification' card.")})
    }


  })
}
