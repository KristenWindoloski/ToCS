
###################################
# UI - SS CONCENTRATION PLOT
###################################

SS_ConcPlot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadSSplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"SSPlot")),
                     shiny::textOutput(shiny::NS(id,"SSPlotCaption"))
  )
}

###################################
# SERVER - SS CONCENTRATION PLOT
###################################

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
      SSplt()})

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
