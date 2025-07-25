
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
                     shiny::textOutput(shiny::NS(id,"ADME1plotsCaption")),
                     htmltools::tagList(shiny::tags$a(href = "https://github.com/KristenWindoloski/ToCS/blob/main/vignettes/Concentration-Time%20Profile%20Simulation%20Examples.pdf",
                                                      "Click here to see definitions of each subplot heading (model compartment)",
                                                      style = "font-size: 16px",
                                                      target="_blank"))
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
      plottingfunc_all(sol_array = sol()[[1]], pars = pars())})

    #--- Outputs plot
    output$ADME1plots <- shiny::renderPlot({
      shiny::req(runsim(),sol())
      allplt_out()[[1]]},
    height = 800)

    #--- Outputs plot caption
    output$ADME1plotsCaption <- shiny::renderText({
      shiny::req(sol(),runsim(),model())
      AUCoutput <- caption_text("ADME",model())
      if (model() == "full_pregnancy"){
        paste("Figure 1: Plot of the time course predictions for selected compounds. The y-axis represents the chemical amount (A = amount (umol))
              or concentration (C = concentration (uM)) in the selected model's compartments (subplot). The AUC subplot shows the area under the curve (uM*days) of the
              ", AUCoutput, " Y-labels that contain 'conceptus' represent the conceptus amount
            or concentration. Y-labels that contain an 'f' represent respective fetal compartments. The model transitions
            from conceptus to fetal on day 91.")
      }
      else if (model() == "fetal_pbtk"){
        paste("Figure 1: Plot of the time course predictions for selected compounds. The y-axis represents the chemical amount (A = amount (umol))
              or concentration (C = concentration (uM)) in the selected model's compartments (subplot).  The AUC subplot shows the
            area under the curve (uM*days) of the ", AUCoutput, " and y-labels that contain an 'f' represent respective fetal compartments.")
      }
      else{
        paste("Figure 1: Plot of the time course predictions for selected compounds. The y-axis represents the chemical amount (A = amount (umol))
              or concentration (C = concentration (uM)) in the selected model's compartments (subplot). The AUC subplot shows the
            area under the curve (uM*days) of the ", AUCoutput)
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
        ggplot2::ggsave(file, plot = allplt_out()[[1]], height = 12, width = 16, dpi = 1200)})
  })
}
