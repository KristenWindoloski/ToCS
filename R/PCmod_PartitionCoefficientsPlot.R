
#######################################
# UI - PC PARTITION COEFFICIENTS PLOT
#######################################

#' User interface function for the parameter calculations partition coefficient
#' plot
#'
#' @description
#' This function contains the user interface elements for the parameter calculations
#' partition coefficient plot. This interface itself contains three UI elements: a
#' download button, a plot, and a plot caption. The current function is called
#' by PC_ui().
#'
#' @param id Shiny identifier name; must be the same id used as in PC_PCPlot_server()
#'
#' @return User interface for the partition coefficient plot drop down with three
#' elements
#' @noRd
#'
PC_PCPlot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadPCplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"PCPlot"),height = "100%"),
                     shiny::textOutput(shiny::NS(id,"PCPlotCaption"))
  )
}

############################################
# SERVER - PC PARTITION COEFFICIENTS PLOT
############################################

#' Server function for the parameter calculations partition coefficient plot
#'
#' @description
#' This function contains the output elements for the parameter calculations
#' partition coefficient plot. This server contains three output elements: a
#' download button, a plot, and a plot caption. The current function calls
#' plotPCs() and is called by PC_server().
#'
#' @param id Shiny identifier name; must be the same id used as in PC_PCPlot_ui()
#' @param pc_args A Shiny reactive list with the output of Parsol(), all shiny
#' parameters in pars(), and the logscale() input by the user
#'
#' @return Server outputs for the partition coefficient plot drop down which
#' includes three elements
#' @noRd
#'
PC_PCPlot_server <- function(id,pc_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({pc_args()[[1]]})
    pars <- shiny::reactive({pc_args()[[2]]})
    logscale <- shiny::reactive({pc_args()[[3]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Generates and arranges partition coefficient plots
    PCPlots <- shiny::reactive({
      plot2 <- plotPCs(sol()[[2]], pars(),logscale())
      gridExtra::grid.arrange(grobs = plot2, nrow = 3, top = "Calculated Tissue Partition Coefficients")})

    #--- Outputs partition coefficient plot
    output$PCPlot <- shiny::renderPlot({
      PCPlots()},
      height = 800)

    #--- Outputs caption to the partition coefficient plot
    output$PCPlotCaption <- shiny::renderText({
      shiny::req(runsim(),sol())
      "Figure 2: Plots of the estimated partition coefficients (unitless) using
      Schmitt's method for all tissues available (adipose, bone, brain, gut,
      heart, kidney, liver, lung, muscle, skin, spleen, red blood cells (rbc),
      rest - collective term for remaining tissues)."})

    #--- Creates plot download button
    output$downloadPCplot_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadPCplots"), "Download Figure 2")})

    #--- Downloads plot
    output$downloadPCplots <- shiny::downloadHandler(
      filename = function() {paste("PCPlot", Sys.Date(), ".jpg", sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot = PCPlots(), height = 12, width = 18, dpi = 1200)})
  })
}
