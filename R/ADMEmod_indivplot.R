
######################################
# UI - ADME INDIVIDUAL-PLOT MODULE
######################################

ADME_IndPlt_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadADME2plots_cond")),
                     shiny::textOutput(shiny::NS(id,"plottextWarning")),
                     shiny::uiOutput(shiny::NS(id,"plots")),
                     shiny::textOutput(shiny::NS(id,"ADME2plotsCaption"))
  )
}

######################################
# SERVER- ADME INDIVIDUAL-PLOT MODULE
######################################

ADME_IndPlt_server <- function(id,adme_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Set reactives to be used
    sol <- shiny::reactive({adme_args()[[1]]})
    pars <- shiny::reactive({adme_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})
    model <- shiny::reactive({pars()[["model"]]})

    #--- Generates color list for plots to match ADME_MultPlt_server colors
    allplt_out <- shiny::reactive({
      plottingfunc_all(sol()[[1]])})

    #--- Generates master plotting list where each entry corresponds to a compound
    #--- Each entry is a list of subplots for that compound
    ADME2plots_list <- shiny::reactive({
      plottingfunc_individual(sol()[[1]], allplt_out()[[2]])})

    #--- Returns the number of plots to be generated (= to num of compounds)
    num_ADMEplots <- shiny::reactive({

      sol_array <- sol()[[1]]
      (dim(sol_array)[3])
    })

    #--- Creates the user interface adaptable to the number of plots needed
    #--- and outputs the names of the plots needed
    output$plots <- renderUI({

      individ_plt_name_lst <- lapply(1:num_ADMEplots(),
                                     function(i) {

                                       plotname <- paste("plot", i, sep="")
                                       shiny::plotOutput(session$ns(plotname))
                                     }
      )
      do.call(htmltools::tagList, individ_plt_name_lst)
    })

    #--- Outputs warning if more than 20 compounds are asked for
    #--- Will only return the first 20 plots
    output$plottextWarning <- renderText({
      if (length(ADME2plots_list()) > 20){
        paste("Maximum number of individual compound plots reached. Only the first 20 will be plotted below.
              Please click the 'Download ADME time course data' to independently plot the remaining compounds.")
      }})

    #--- Outputs the plots to be saved if user downloads them
    reactive_pltname_list <- shiny::reactive({
      plt_arrange(ADME2plots_list())})

    #--- Outputs plots for each individual compound
    for (i in 1:20) {

      # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances.
      local({

        my_i <- i
        plotname <- paste("plot", my_i, sep="")

        output[[plotname]] <- shiny::renderPlot({
          gridExtra::grid.arrange(grobs = ADME2plots_list()[[my_i]])})
      })
    }

    #--- Outputs caption for individual plots
    output$ADME2plotsCaption <- shiny::renderText({
      shiny::req(sol(),runsim(),model())
      AUCoutput <- caption_text("ADME",model())
      paste("Figure 2: Plots of the time course predictions for each individual compound.
            The y-axis indicates the output type (A = amount (umol), C = concentration (uM)) and compartments
            for the selected model. The AUC plot shows the area under the curve of the ", AUCoutput)})

    #--- Creates download button for plots
    output$downloadADME2plots_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadADME2plots"), "Download Individual Compound Plots")})

    #--- Downloads a zip file of all individual plots
    output$downloadADME2plots <- shiny::downloadHandler(
      filename = function() {paste("ADME_Individual_Plots", Sys.Date(), ".zip", sep="")},
      content = function(file){

        savedfiles <- c()
        for (i in 1:num_ADMEplots()) {

          f <- paste("Compound_", i, ".jpg", sep="")
          ggplot2::ggsave(f, plot = reactive_pltname_list()[[i]], height = 12, width = 16, dpi = 1200)
          savedfiles <- append(savedfiles, f)
        }

        # Zip the files
        zip(file, savedfiles)})
  })
}
