
######################################
# UI - ADME INDIVIDUAL-PLOT MODULE
######################################

#' User interface function for the concentration-time profile individual plots
#'
#' @description
#' This function outputs the user interface for the concentration-time profile
#' individual plots drop down in the results card under the 'Run Simulation' tab.
#' The interface has four outputs: a plot download button, plotting text (if
#' applicable), plots, and a plotting caption. The current function is
#' called by ADME_ui().
#'
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_IndPlt_server()
#'
#' @return User interface for the individual plots drop down with four elements
#' @noRd
#'
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

#' Server function for the concentration-time profile individual plots
#'
#' @description
#' This function generates the outputs defined in the ADME_IndPlt_ui()
#' function. This connects the download button, plot text, plot, and plot
#' caption to the elements that fill their spaces. The current function is called
#' by ADME_server() and calls plottingfunc_all(), plottingfunc_individual(),
#' plt_arrange(), caption_text().
#'
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_IndPlt_ui()
#' @param adme_args A Shiny reactive list with the output of modsol() and all shiny
#' parameters in pars()
#'
#' @return Server outputs for the concentration-time profile individual plots
#' drop down which includes four elements
#' @noRd
#'
ADME_IndPlt_server <- function(id,adme_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Set reactives to be used
    sol <- shiny::reactive({adme_args()[[1]]})
    pars <- shiny::reactive({adme_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})
    model <- shiny::reactive({pars()[["model"]]})

    # #--- Generates color list for plots to match ADME_MultPlt_server colors
    # allplt_out <- shiny::reactive({plottingfunc_all(sol()[[1]])})

    #--- Generates master plotting list where each entry corresponds to a compound
    #--- Each entry is a list of subplots for that compound
    ADME2plots_list <- shiny::reactive({plottingfunc_individual(sol()[[1]],"color")})

    #--- Returns the number of plots to be generated (= to num of compounds)
    num_ADMEplots <- shiny::reactive({

      sol_array <- sol()[[1]]
      (dim(sol_array)[3])
    })

    #--- Creates the user interface adaptable to the number of plots needed
    #--- and outputs the names of the plots needed
    output$plots <- shiny::renderUI({

        n <- num_ADMEplots()
        individ_plt_name_lst <- list()

        for (i in 1:n) {
          individ_plt_name_lst[[i]] <- shiny::plotOutput(session$ns(paste("plot",i,sep = "")))
        }
      do.call(htmltools::tagList, individ_plt_name_lst)
    })

    #--- Outputs warning if more than 20 compounds are asked for
    #--- Will only return the first 20 plots
    output$plottextWarning <- shiny::renderText({
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

          output[[plotname]] <- shiny::renderPlot({ADME2plots_list()[[my_i]]})
            # gridExtra::grid.arrange(grobs = ADME2plots_list()[[my_i]])})
      })
    }

    #--- Outputs caption for individual plots
    output$ADME2plotsCaption <- shiny::renderText({
      shiny::req(sol(),runsim(),model())
      AUCoutput <- caption_text("ADME",model())
      if (model() == "full_pregnancy"){
        paste("Figure 2: Plots of the time course predictions for each individual compound. The y-axis indicates the output type
            (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
            area under the curve of the ", AUCoutput, " Y-labels that contain 'conceptus' represent the conceptus amount
            or concentration. Y-labels that contain an 'f' represent respective fetal compartments. The model transitions
            from conceptus to fetal on day 91.")
      }
      else if (model() == "fetal_pbtk"){
        paste("Figure 2: Plots of the time course predictions for each individual compound. The y-axis indicates the output type
            (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
            area under the curve of the ", AUCoutput, " Y-labels that contain an 'f' represent respective fetal compartments.")
      }
      else{
        paste("Figure 2: Plots of the time course predictions for each individual compound. The y-axis indicates the output type
            (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
            area under the curve of the ", AUCoutput)
      }
      })

    #--- Creates download button for plots
    output$downloadADME2plots_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadADME2plots"), "Download Individual Compound Plots")})

    #--- Downloads a zip file of all individual plots
    output$downloadADME2plots <- shiny::downloadHandler(
      filename = function() {paste("ADME_Individual_Plots", Sys.Date(), ".zip", sep="")},
      content = function(file){

        savedfiles <- c()

        shiny::withProgress(message = "Compiling zip file. Please wait.", value = 0, {

          for (i in 1:num_ADMEplots()) {

            # --- Increment the progress bar and update detail text
            shiny::incProgress(1/(num_ADMEplots()+1), detail = paste("Generating plot file for chemical", i))

            f <- paste("Compound_", i, ".jpg", sep="")
            ggplot2::ggsave(f, plot = reactive_pltname_list()[[i]], height = 12, width = 16, dpi = 1200)
            savedfiles <- append(savedfiles, f)
          }

          # Zip the files
          # --- Increment the progress bar and update detail text
          shiny::incProgress(1/(num_ADMEplots()+1), detail = paste("Zipping file together"))
          utils::zip(file, savedfiles)})
        })

    # #--- Set reactives to be used
    # sol <- shiny::reactive({adme_args()[[1]]})
    # pars <- shiny::reactive({adme_args()[[2]]})
    # runsim <- shiny::reactive({pars()[["runsim"]]})
    # model <- shiny::reactive({pars()[["model"]]})
    #
    # #--- Generates color list for plots to match ADME_MultPlt_server colors
    # allplt_out <- shiny::reactive({plottingfunc_all(sol()[[1]])})
    #
    # #--- Generates master plotting list where each entry corresponds to a compound
    # #--- Each entry is a list of subplots for that compound
    # ADME2plots_list <- shiny::reactive({plottingfunc_individual(sol()[[1]], allplt_out()[[2]])})
    #
    # #--- Returns the number of plots to be generated (= to num of compounds)
    # num_ADMEplots <- shiny::reactive({
    #
    #   sol_array <- sol()[[1]]
    #   (dim(sol_array)[3])
    # })
    #
    # #--- Creates the user interface adaptable to the number of plots needed
    # #--- and outputs the names of the plots needed
    # output$plots <- shiny::renderUI({
    #
    #   n <- num_ADMEplots()
    #   individ_plt_name_lst <- list()
    #
    #   for (i in 1:n) {
    #     individ_plt_name_lst[[i]] <- shiny::plotOutput(session$ns(paste("plot",i,sep = "")))
    #   }
    #   do.call(htmltools::tagList, individ_plt_name_lst)
    # })
    #
    # #--- Outputs warning if more than 20 compounds are asked for
    # #--- Will only return the first 20 plots
    # output$plottextWarning <- shiny::renderText({
    #   if (length(ADME2plots_list()) > 20){
    #     paste("Maximum number of individual compound plots reached. Only the first 20 will be plotted below.
    #           Please click the 'Download ADME time course data' to independently plot the remaining compounds.")
    #   }})
    #
    # #--- Outputs the plots to be saved if user downloads them
    # reactive_pltname_list <- shiny::reactive({
    #   plt_arrange(ADME2plots_list())})
    #
    # #--- Outputs plots for each individual compound
    # for (i in 1:20) {
    #
    #   # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances.
    #   local({
    #
    #     my_i <- i
    #     plotname <- paste("plot", my_i, sep="")
    #
    #     output[[plotname]] <- shiny::renderPlot({
    #       gridExtra::grid.arrange(grobs = ADME2plots_list()[[my_i]])})
    #   })
    # }
    #
    # #--- Outputs caption for individual plots
    # output$ADME2plotsCaption <- shiny::renderText({
    #   shiny::req(sol(),runsim(),model())
    #   AUCoutput <- caption_text("ADME",model())
    #   if (model() == "full_pregnancy"){
    #     paste("Figure 2: Plots of the time course predictions for each individual compound. The y-axis indicates the output type
    #         (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
    #         area under the curve of the ", AUCoutput, " Y-labels that contain 'conceptus' represent the conceptus amount
    #         or concentration. Y-labels that contain an 'f' represent respective fetal compartments. The model transitions
    #         from conceptus to fetal on day 91.")
    #   }
    #   else if (model() == "fetal_pbtk"){
    #     paste("Figure 2: Plots of the time course predictions for each individual compound. The y-axis indicates the output type
    #         (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
    #         area under the curve of the ", AUCoutput, " Y-labels that contain an 'f' represent respective fetal compartments.")
    #   }
    #   else{
    #     paste("Figure 2: Plots of the time course predictions for each individual compound. The y-axis indicates the output type
    #         (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
    #         area under the curve of the ", AUCoutput)
    #   }
    # })
    #
    # #--- Creates download button for plots
    # output$downloadADME2plots_cond <- shiny::renderUI({
    #   shiny::req(sol(),runsim())
    #   shiny::downloadButton(session$ns("downloadADME2plots"), "Download Individual Compound Plots")})
    #
    # #--- Downloads a zip file of all individual plots
    # output$downloadADME2plots <- shiny::downloadHandler(
    #   filename = function() {paste("ADME_Individual_Plots", Sys.Date(), ".zip", sep="")},
    #   content = function(file){
    #
    #     savedfiles <- c()
    #
    #     shiny::withProgress(message = "Compiling zip file. Please wait.", value = 0, {
    #
    #       for (i in 1:num_ADMEplots()) {
    #
    #         # --- Increment the progress bar and update detail text
    #         shiny::incProgress(1/(num_ADMEplots()+1), detail = paste("Generating plot file for chemical", i))
    #
    #         f <- paste("Compound_", i, ".jpg", sep="")
    #         ggplot2::ggsave(f, plot = reactive_pltname_list()[[i]], height = 12, width = 16, dpi = 1200)
    #         savedfiles <- append(savedfiles, f)
    #       }
    #
    #       # Zip the files
    #       # --- Increment the progress bar and update detail text
    #       shiny::incProgress(1/(num_ADMEplots()+1), detail = paste("Zipping file together"))
    #       utils::zip(file, savedfiles)})
    #   })
  })
}
