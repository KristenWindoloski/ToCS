
#######################################
# UI - PC ELIM RATE AND VDIST PLOT
#######################################

#' User interface function for the parameter calculations parameter plot
#'
#' @description
#' This function contains the user interface elements for the parameter calculations
#' parameter plot. This interface itself contains three UI elements: a
#' download button, a plot, and a plot caption. The current function is called
#' by PC_ui().
#'
#' @param id Shiny identifier name; must be the same id used as in PC_EVPlot_server()
#'
#' @return User interface for the parameter plot drop down with three elements
#' @noRd
#'
PC_EVPlot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadParplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"ParPlot"), height = "100%"),
                     shiny::textOutput(shiny::NS(id,"ParPlotCaption"))
  )
}

#######################################
# SERVER - PC ELIM RATE AND VDIST PLOT
#######################################

#' Server function for the parameter calculations parameter plot
#'
#' @description
#' This function contains the output elements for the parameter calculations
#' parameter plot. This server contains three output elements: a
#' download button, a plot, and a plot caption. The current function calls
#' plotPar() and is called by PC_server().
#'
#' @param id Shiny identifier name; must be the same id used as in PC_EVPlot_ui()
#' @param pc_args A Shiny reactive list with the output of Parsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the parameter plot drop down which includes three
#' elements
#' @noRd
#'
PC_EVPlot_server <- function(id,pc_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({pc_args()[[1]]})
    pars <- shiny::reactive({pc_args()[[2]]})
    logscale <- shiny::reactive({pc_args()[[3]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Generates elimination/volume of distribution plots
    ElimVdistPlots <- shiny::reactive({
      plotPar(sol()[[1]],pars(),logscale())})
    # ElimVdistPlots <- shiny::reactive({
    #   plot1 <- plotPar(sol()[[1]],pars(),logscale())
    #   gridExtra::grid.arrange(grobs = plot1, nrow = 2)})

    #--- Outputs elimination/volume of distribution plots
    output$ParPlot <- shiny::renderPlot({
      ElimVdistPlots()},
      height = 600)

    #--- Outputs caption of the elimination/volume of distribution plots
    output$ParPlotCaption <- shiny::renderText({
      shiny::req(runsim(), sol())
      "Figure 1: Plots of the estimated elimination rate (1/h), volume of
      distribution (L/kg BW), half life (h), and total plasma clearance
      (L/h/kg BW) for all selected compounds. Compounds are listed in
      ascending order in each plot based on their parameter values."})

    #--- Creates download plot button
    output$downloadParplot_cond <- shiny::renderUI({
      shiny::req(runsim(), sol())
      shiny::downloadButton(session$ns("downloadParplots"), "Download Figure 1")})

    #--- Downloads elimination/volume of distribution plots
    output$downloadParplots <- shiny::downloadHandler(
      filename = function() {paste("ParPlot", Sys.Date(), ".jpg", sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot = ElimVdistPlots(), height = 12, width = 18, dpi = 1200)})

  })
}
