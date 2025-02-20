
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
      if (ncol(sol()[[1]])>3){
        if (!is.null(pars()[["fileExposure"]])){
          paste("Figure 1: Boxplots of", pars()[["samples"]], "oral equivalent dose
              (OED) samples for each selected compound (red shaded) and user-uploaded exposure estimates (purple).
              The black dots represent outliers and the red dots indicate the 5th quantile OED for
              each compound. Compounds are arranged in ascending order of their median OED value.
              Exposure estimates are shown as a distribution if more than one exposure estimate was provided
              for each compound. The purple dot represents the median exposure either uploaded by the user
              or calculated within the program. If the user only uploaded one exposure value for a compound, then
              the purple dot represents that value.")
        }
        else{
          paste("Figure 1: Boxplots of", pars()[["samples"]], "oral equivalent dose
              (OED) samples for each selected compound. The black dots represent outliers
              and the red dots indicate the 5th quantile OED for each compound. Compounds
              are arranged in ascending order of their median OED value.")
        }

      }
      else{
        if (!is.null(pars()[["fileExposure"]])){
          paste("Figure 1: Plot of the estimated oral equivalent dose (OED) for
          each selected compound (blue) and user-uploaded exposure estimates (red).
          Compounds are arranged in ascending order of their OED values. Exposure estimates
          are shown as a distribution if more than one exposure estimate was provided
          for each compound. The purple dot represents the median exposure either uploaded
          by the user or calculated within the program. If the user only uploaded one
          exposure value for a compound, then the purple dot represents that value.")
        }
        else{
          "Figure 1: Plot of the estimated oral equivalent dose (OED) for
          each selected compound. Compounds are arranged in ascending order of
          their OED values."
        }
      }})

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
