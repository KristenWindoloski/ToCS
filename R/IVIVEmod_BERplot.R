
##########################
# UI - BER PLOT
##########################

#' User interface function for the BER plot
#'
#' @description
#' This function outputs the user interface for the BER plot drop down in the
#' results card under the 'Run Simulation' tab. The interface has three outputs:
#' a plot download button, a plot with BER values, and a plot caption.
#'
#' @param id Shiny identifier name; must be the same id used as in BER_Plot_server()
#'
#' @return User interface for the BER plot drop down with three elements
#' @seealso [IVIVE_ui()], which calls the current function
#' @export
#'
BER_Plot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadBERplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"BERPlot"),height = "100%"),
                     shiny::textOutput(shiny::NS(id,"BERPlotCaption"))
  )
}

#############################
# SERVER - BER PLOT
#############################

#' Server function for the BER plot
#'
#' @description
#' This function generates the outputs defined in the IVIVE_Table_ui()
#' function. This connects the download buttons with the plot to download,
#' generates the plot content, and creates the text for the plot caption.
#'
#' @param id Shiny identifier name; must be the same id used as in BER_Plot_ui()
#' @param ivive_args A Shiny reactive list with the output of IVIVEsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the BER plot drop down which includes three elements
#' @seealso [BERplotting()], which is called by this function, and [IVIVE_server()],
#' which calls the current function
#' @export
#'
BER_Plot_server <- function(id,ivive_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactives set to be used
    sol <- shiny::reactive({ivive_args()[[1]]})
    pars <- shiny::reactive({ivive_args()[[2]]})
    logscale <- shiny::reactive({ivive_args()[[3]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    if (!is.null(pars()[["fileExposure"]])){

      #--- Generates plot of BER values
      BERplt <- shiny::reactive({BERplotting(sol()[[4]])})

      #--- Outputs plot of BER values
      output$BERPlot <- shiny::renderPlot({BERplt()},
                                          height = 600)

      #--- Outputs plot caption
      output$BERPlotCaption <- shiny::renderText({
        shiny::req(sol(),runsim())
        paste("Figure 2: Plot of the bioactivity exposure ratio (BER) values calculated from the oral equivalent dose and exposure estimate from each chemical.
            The red dotted line (BER = 1) separates compounds with BER < 1, where these chemicals should be prioritized for risk.")})

      #--- Creates download button
      output$downloadBERplot_cond <- shiny::renderUI({
        shiny::req(sol(),runsim())
        shiny::downloadButton(session$ns("downloadBERplot"), "Download Figure 2")})

      #--- Downloads OED plot
      output$downloadBERplot <- shiny::downloadHandler(
        filename = function() {paste("BERplot", Sys.Date(), ".jpg", sep="")},
        content = function(file){
          ggplot2::ggsave(file, plot = BERplt(), height = 12, width = 14, dpi = 1200)})
    }
    else{
      output$BERPlotCaption <- shiny::renderText({
        shiny::req(sol(),runsim())
        paste("Chemical exposure data was not uploaded under the 'Advanced Parameters' tab,
              so the bioactivity exposure ratio (BER) cannot be calculated. If the BER is desired,
              please upload exposure data on the 'Advanced Parameters' tab under the 'Output Specification' card.")})
    }

  })
}
