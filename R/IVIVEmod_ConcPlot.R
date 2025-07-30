
##########################
# UI - IVIVE DOSE PLOT
##########################

#' User interface function for the IVIVE dose plot
#'
#' #' @description
#' This function outputs the user interface for the IVIVE plot drop down in the
#' results card under the 'Run Simulation' tab. The interface has three outputs:
#' a plot download button, a plot with calculated IVIVE doses, and a plot caption.
#' The current function is called by IVIVE_ui().
#'
#' @param id Shiny identifier name; must be the same id used as in IVIVE_Plot_server()
#'
#' @return User interface for the IVIVE plot drop down with four elements
#' @noRd
#'
IVIVE_Plot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadIVIVEplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"IVIVEPlot"), height = "100%"),
                     shiny::textOutput(shiny::NS(id,"IVIVEPlotCaption"))
  )
}

#############################
# SERVER - IVIVE DOSE PLOT
#############################

#' Server function for the IVIVE dose plot
#'
#' #' @description
#' This function generates the outputs defined in the IVIVE_Plot_ui()
#' function. This connects the download button with the plot to download,
#' generates the IVIVE dose plot, and creates the text for the plot caption.
#' The current function calls IVIVEplotting() and IVIVEplot_caption() and
#' is called by IVIVE_server().
#'
#' @param id Shiny identifier name; must be the same id used as in IVIVE_Plot_ui()
#' @param ivive_args A Shiny reactive list with the output of IVIVEsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the IVIVE plot drop down which includes three elements
#' @noRd
#'
IVIVE_Plot_server <- function(id,ivive_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactives set to be used
    sol <- shiny::reactive({ivive_args()[[1]]})
    pars <- shiny::reactive({ivive_args()[[2]]})
    logscale <- shiny::reactive({ivive_args()[[3]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Generates plot of OED values
    IVIVEplt <- shiny::reactive({IVIVEplotting(sol()[[1]],
                                               sol()[[2]],
                                               pars(),
                                               logscale(),
                                               sol()[[5]])})

    #--- Outputs plot of OED values
    output$IVIVEPlot <- shiny::renderPlot({IVIVEplt()},
                                          height = 600)

    #--- Outputs plot caption
    output$IVIVEPlotCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      IVIVEplot_caption(pars())
      })

    #--- Creates download button
    output$downloadIVIVEplot_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadIVIVEplot"), "Download Figure 1")})

    #--- Downloads OED plot
    output$downloadIVIVEplot <- shiny::downloadHandler(
      filename = function() {paste("IVIVEplot", Sys.Date(), ".jpg", sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot = IVIVEplt(), height = 12, width = 14, dpi = 300)})
  })
}
