
##################################
# UI - ADME MULTI-PLOT MODULE
##################################

#' User interface function for the concentration-time profile multi-plot output
#'
#' @description
#' This function outputs the user interface for the concentration-time profile
#' multi-plot drop down in the results card under the 'Run Simulation' tab.
#' The interface has three outputs: a download plot button, a plot, and a plot
#' caption. The current function is called by ADME_ui().
#'
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_MultPlt_server()
#'
#' @return User interface for the multi-plot drop down which has three elements
#' @noRd
#'
ADME_MultPlt_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadADME1plots_cond")),
                     shiny::plotOutput(shiny::NS(id,"ADME1plots"),height = "100%"),
                     shiny::textOutput(shiny::NS(id,"ADME1plotsCaption"))
  )
}

##################################
# SERVER - ADME MULTI-PLOT MODULE
##################################

#' Server function for the concentration-time profile multi-plot output
#'
#' @description
#' This function generates the outputs defined in the ADME_MultPlt_ui()
#' function. This connects the download button, plot, and plot caption to the
#' elements that fill their spaces. The current function calls plottingfunc_all()
#' and caption_text() and called by ADME_server().
#'
#'
#' @param id Shiny identifier name; must be the same id used as in ADME_MultPlt_ui()
#' @param adme_args A Shiny reactive list with the output of modsol() and all shiny
#' parameters in pars()
#'
#' @return Server outputs for the concentration-time profile multi-plot drop down
#'  which includes three elements
#' @noRd
#'
ADME_MultPlt_server <- function(id, adme_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Set reactives to be used
    sol <- shiny::reactive({adme_args()[[1]]})
    pars <- shiny::reactive({adme_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})
    model <- shiny::reactive({pars()[["model"]]})

    #--- Generates plotting list and colors for plots
    allplt_out <- shiny::reactive({
      plottingfunc_all(sol()[[1]])})

    #--- Arranges multiple plots on one grid
    ADME1plots <- shiny::reactive({
      gridExtra::grid.arrange(grobs = allplt_out()[[1]],top = "Toxicokinetic Profiles")})

    #--- Outputs plot
    output$ADME1plots <- shiny::renderPlot({
      shiny::req(runsim(),sol())
      ADME1plots()},
      height = 800)

    #--- Outputs plot caption
    output$ADME1plotsCaption <- shiny::renderText({
      shiny::req(sol(),runsim(),model())
      AUCoutput <- caption_text("ADME",model())
      if (model() == "full_pregnancy"){
        paste("Figure 1: Plot of the time course predictions for selected compounds. The y-axis indicates the output type
            (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
            area under the curve of the ", AUCoutput, " Y-labels that contain 'conceptus' represent the conceptus amount
            or concentration. Y-labels that contain an 'f' represent respective fetal compartments. The model transitions
            from conceptus to fetal on day 91.")
      }
      else if (model() == "fetal_pbtk"){
        paste("Figure 1: Plot of the time course predictions for selected compounds. The y-axis indicates the output type
            (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
            area under the curve of the ", AUCoutput, " Y-labels that contain an 'f' represent respective fetal compartments.")
      }
      else{
        paste("Figure 1: Plot of the time course predictions for selected compounds. The y-axis indicates the output type
            (A = amount (umol), C = concentration (uM)) and compartments for the selected model. The AUC plot shows the
            area under the curve of the ", AUCoutput)
      }
      })

    #--- Shows download button for plot
    output$downloadADME1plots_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadADME1plots"), "Download Figure 1")})

    #--- Downloads the plot
    output$downloadADME1plots <- shiny::downloadHandler(
      filename = function() {paste("ADMEplots", Sys.Date(), ".jpg", sep="")},
      content = function(file){
        ggplot2::ggsave(file, plot = ADME1plots(), height = 12, width = 16, dpi = 1200)})
  })
}
