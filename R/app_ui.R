
##########################################################################
# MAIN UI CODE FOR TOCS APP
##########################################################################

#' Not to be called directly by the user - app ui function
#'
#' @description
#' This function is the top-level ui function for the ToCS application and
#' dictates where the outputs from the app_server function will be displayed.
#' Calls to all other ui functions stem from this function. The current function
#' calls the following internal functions: GP_Instructions(), GP_Output(),
#' GP_Species(), MS_Dosing(), MS_Model(), CS_Instructions(), CS_PreloadedCompounds(),
#' CS_UploadedData(), AP_ModelConditions(), AP_ModelSolver(), AP_Bioavailability(),
#' AP_OutputSpecification(), RS_Actions(), RS_SelectedCompounds(), and RS_Results().
#' @param ui_pars A list only used to alter the default arguments of the GUI when
#' opened to run the vignette examples quicker. The list contains each user-selection.
#'
#'
#' @return User interface
#' @export
#'
app_ui <- function(ui_pars){

    ui <- bslib::page_navbar(title = "Toxicokinetic Chemical Simulator (ToCS)",
                             shinyjs::useShinyjs(),

                             ##########################################################################
                             # GENERAL PARAMETERS TAB
                             # INSTRUCTIONS, OUTPUT, AND SPECIES CARDS
                             ##########################################################################

                             bslib::nav_panel(title = "General Parameters",
                                              bslib::layout_columns(GP_Instructions(),
                                                                    GP_Output(ui_pars),
                                                                    GP_Species(ui_pars),
                                                                    col_widths = c(4,4,4))),

                             ##########################################################################
                             # MODEL SPECIFICATIONS TAB
                             # DOSING AND MODEL CARDS
                             ##########################################################################

                             bslib::nav_panel(title = "Model Specifications",
                                              bslib::layout_columns(MS_Model(ui_pars),
                                                                    MS_Dosing(ui_pars),
                                                                    col_widths = c(6,6))),

                             ##########################################################################
                             # COMPOUND SELECTION TAB
                             # INSTRUCTIONS, PRELOADED COMPOUNDS, AND UPLOADED COMPOUNDS CARDS
                             ##########################################################################

                             bslib::nav_panel(title = "Compound Selection",
                                              bslib::layout_columns(CS_Instructions(),
                                                                    CS_PreloadedCompounds(ui_pars),
                                                                    CS_UploadedData(),
                                                                    col_widths = c(4,4,4))),

                             ##################################################################################
                             # ADVANCED (OPTIONAL) PARAMETERS TAB
                             # MODEL CONDITIONS, MODEL SOLVER, BIOAVAILABILITY, AND OUTPUT SPECIFICATION CARDS
                             ##################################################################################

                             bslib::nav_panel(title = "Advanced (Optional) Parameters",
                                              bslib::layout_columns(AP_ModelConditions(the$ic_names,the$ic_comps,ui_pars),
                                                                    AP_ModelSolver(ui_pars),
                                                                    AP_Bioavailability(ui_pars),
                                                                    AP_OutputSpecification(ui_pars),
                                                                    col_widths = c(3,3,3,3))),

                             ##########################################################################
                             # RUN SIMULATION TAB (RESULTS)
                             # ACTION, SELECTED COMPOUNDS, AND RESULTS CARDS
                             ##########################################################################

                             bslib::nav_panel(title = "Run Simulation",
                                              bslib::layout_columns(RS_Actions(),
                                                                    RS_SelectedCompounds(),
                                                                    RS_Results(),
                                                                    col_widths = c(2,2,8)))
    )
  return(ui)
}
