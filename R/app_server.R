
##########################################################################
# MAIN SERVER FUNCTION FOR TOCS APP
##########################################################################

#' App server function
#'
#' @description
#' This function is the top-level server function for the ToCS application and
#' dictates the outputs to be displayed in the app_ui function. Calls to all other
#' server functions stem from this function.
#'
#'
#' @param input Input objects from the user interface
#' @param output Output objects from the server
#' @param session Session identifier
#'
#' @return Various server outputs
#' @seealso [Model_Input()], [PreloadComps_UI()], [CompileCompLst()], [UpdateInputs()],
#' [GatherInputVars()], [InputRules_Children()], [InputRules_Parents()], and
#' [Run_Simulation()]
#' @export
#'
#' @examples server(input, output, session)
app_server <- function(input, output, session) {

  # --- RESET BUTTON OUTPUT
  shiny::observeEvent(input$ResetButton, {
    session$reload()
    return()})

  # --- HAS A FILE WITH COMPOUND DATA BEEN UPLOADED?
  getData <- shiny::reactive({
    if(is.null(input$file1)) return(FALSE)
    else return(TRUE)})
  output$fileUploaded <- shiny::reactive({return(getData())})
  shiny::outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  # --- GENERATES THE MODEL CARD OUTPUT
  output$Model <- shiny::renderUI({Model_Input(input$func,input$spec)})

  # --- GENERATES THE PRELOADED COMPOUNDS OUTPUT
  output$PreloadedComps <- shiny::renderUI({
    PreloadComps_UI(input$func,input$spec,input$defaulttoHuman,
                    input$model,input$insilicopars,input$HondaIVIVE,
                    input$CompType)})

  # --- COMPILES LIST OF ALL COMPOUNDS TO RUN
  CompLst <- shiny::reactive({
    CompileCompLst(input$func,input$spec,input$defaulttoHuman,input$model,
                   input$insilicopars,input$httkPreloadComps,input$file1,input$HondaIVIVE)})
  output$comptext <- shiny::renderTable({CompLst()})

  # --- RESET INPUT COMPOUND-RELATED VARIABLES IF USER CHANGES DEPENDENT VARIABLES
  UpdateInputs(input,session)

  # --- GATHER ALL INPUT VARIABLES
  AllInputs <- shiny::eventReactive(input$runsim,{GatherInputVars(input,CompLst())})

  # --- DEFINE INPUT ERRORS FOR EACH MODULE
  iv_common <- shinyvalidate::InputValidator$new()
  iv_adme <- shinyvalidate::InputValidator$new()
  iv_ss <- shinyvalidate::InputValidator$new()
  iv_ivive <- shinyvalidate::InputValidator$new()
  iv_pc <- shinyvalidate::InputValidator$new()
  InputRules_Children(iv_common,iv_adme,iv_ss,iv_ivive,iv_pc,input,ic_names)

  parent_adme_iv <- shinyvalidate::InputValidator$new()
  parent_ss_iv <- shinyvalidate::InputValidator$new()
  parent_ivive_iv <- shinyvalidate::InputValidator$new()
  parent_pc_iv <- shinyvalidate::InputValidator$new()
  InputRules_Parents(parent_adme_iv,iv_adme,
                     parent_ss_iv,iv_ss,
                     parent_ivive_iv,iv_ivive,
                     parent_pc_iv,iv_pc,
                     iv_common)

  # --- APP OUTPUTS (ADME, STEADY STATE, IVIVE, PARAMETER CALCULATIONS)
  shiny::observeEvent(input$runsim,{
    Run_Simulation(parent_adme_iv,parent_ss_iv,parent_ivive_iv,parent_pc_iv,input, AllInputs)
  })
}
