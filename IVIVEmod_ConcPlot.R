
##########################
# UI - IVIVE CONC PLOT
##########################

IVIVE_Plot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadIVIVEplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"IVIVEPlot")),
                     shiny::textOutput(shiny::NS(id,"IVIVEPlotCaption"))
  )
}

#############################
# SERVER - IVIVE CONC PLOT
#############################

IVIVE_Plot_server <- function(id,ivive_args){

  moduleServer(id, function(input, output, session) {

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
    output$IVIVEPlot <- shiny::renderPlot({IVIVEplt()})

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
        ggplot2::ggsave(file, plot = IVIVEplt(), height = 12, width = 14, dpi = 1200)})
  })
}
