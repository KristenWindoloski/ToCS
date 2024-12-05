
#######################################
# UI - PC PARTITION COEFFICIENTS PLOT
#######################################

PC_PCPlot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadPCplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"PCPlot")),
                     shiny::textOutput(shiny::NS(id,"PCPlotCaption"))
  )
}

############################################
# SERVER - PC PARTITION COEFFICIENTS PLOT
############################################

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
      PCPlots()})

    #--- Outputs caption to the partition coefficient plot
    output$PCPlotCaption <- shiny::renderText({
      shiny::req(runsim(),sol())
      "Figure 2: Plots of the estimated partition coefficients (no units) using
      Schmitt's method for all tissues available (adipose, bone, brain, gut,
      heart, kidney, liver, lung, muscle, skin, spleen, red blood cells (rbc),
      rest - collective term for remaining tissues). Compounds are arranged in
      ascending order based on the median partition coefficient for each compound."})

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
