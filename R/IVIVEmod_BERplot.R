
##########################
# UI - BER PLOT
##########################

BER_Plot_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadBERplot_cond")),
                     shiny::plotOutput(shiny::NS(id,"BERPlot")),
                     shiny::textOutput(shiny::NS(id,"BERPlotCaption"))
  )
}

#############################
# SERVER - BER PLOT
#############################

BER_Plot_server <- function(id,ivive_args){

  moduleServer(id, function(input, output, session) {

    #--- Reactives set to be used
    sol <- shiny::reactive({ivive_args()[[1]]})
    pars <- shiny::reactive({ivive_args()[[2]]})
    logscale <- shiny::reactive({ivive_args()[[3]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    if (!is.null(pars()[["fileExposure"]])){

      #--- Generates plot of BER values
      BERplt <- shiny::reactive({BERplotting(sol()[[4]])})

      #--- Outputs plot of BER values
      output$BERPlot <- shiny::renderPlot({BERplt()})

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
