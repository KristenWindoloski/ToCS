
###################################
# UI - SS CONCENTRATION PLOT
###################################

#' User interface for the steady state concentrations plot
#'
#' @description
#' This function contains the user interface elements for the steady state
#' concentrations plot. This interface itself contains three UI elements: a
#' download button, a plot, and a plot caption. This function is called by SS_ui().
#'
#'
#' @param id Shiny identifier name; must match the identifier in the SS_ConcPlot_server()
#'
#' @return User interface for the steady state concentrations plot which includes
#' four elements: a plot download button, the steady state concentrations plot,
#' and a plot caption
#' @noRd
#'
SS_ConcPlot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadSSplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"SSPlot"),height = "100%"),
                     shiny::textOutput(shiny::NS(id,"SSPlotCaption"))
  )
}

###################################
# SERVER - SS CONCENTRATION PLOT
###################################

#' Server function for the steady state concentrations plot
#'
#' @description
#' This function contains the output elements for the steady state
#' concentrations plot. This server contains three output elements: a
#' download button, a plot, and a plot caption. This function calls scat_plot()
#' and is called by SS_server().
#'
#' @param id Shiny identifier name; must match the identifier in the SS_ConcPlot_ui()
#' @param ss_args A Shiny reactive list with the output of SS_sol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server function for the steady state concentrations plot which includes
#' four elements: a plot download button, the steady state concentrations plot,
#' and a plot caption
#' @noRd
#'
SS_ConcPlot_server <- function(id,ss_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({ss_args()[[1]]})
    pars <- shiny::reactive({ss_args()[[2]]})
    logscale <- shiny::reactive({ss_args()[[3]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Generates plot
    SSplt <- shiny::reactive({
      scat_plot(sol()[[1]],pars(),logscale())
    })

    #--- Outputs plot
    output$SSPlot <- shiny::renderPlot({
      SSplt()},
      height = 600)

    #--- Outputs plot caption
    output$SSPlotCaption <- shiny::renderText({
      shiny::req(sol(), runsim())
      "Figure 1: Plot of the long-term constant infusion (steady state) concentrations for
      the selected compounds. Compounds are arranged in ascending order of their
      concentration values."})

    #--- Creates download button
    output$downloadSSplot_cond <- shiny::renderUI({
      shiny::req(sol(), runsim())
      shiny::downloadButton(session$ns("downloadSSplot"), "Download Figure 1")})

    #--- Downloads plot
    output$downloadSSplot <- shiny::downloadHandler(
      filename = function() {paste("SSplot", Sys.Date(), ".jpg", sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot = SSplt(), height = 12, width = 14, dpi = 1200)})
  })
}
