########################
# RUN THE TOCS APP
########################

#' Use this function to run the ToCS App
#'
#' @description
#' This is the primary function to run the ToCS app once the ToCS package is
#' loaded into R. A window should open after typing 'run_ToCS' into the console.
#' @param ui_pars A list with the default selections for each ui element (drop down,
#' numeric input, text box, and file upload). Default is null, but this is used
#' for easier generation of vignette images.
#'
#'
#' @return A popup window with the ToCS user interface
#' @export
#'
run_ToCS <- function(ui_pars = list()){

  if (exists("chem.physical_and_invitro.data",envir = .GlobalEnv)){
    shiny::stopApp()
    print("Please rename your global environment variable 'chem.physical_and_invitro.data'. Then, run ToCS again.")
  }
  else{
    if (interactive()){
      # shiny::runApp(appDir = applocal)
      shiny::runApp(list(ui = ToCS::app_ui(ui_pars),
                         server = ToCS::app_server))
      }
    else{
      applocal <- system.file("app",package = "ToCS")
      shiny::shinyAppDir(appDir = applocal)
    }
  }
}

#' Run the ToCS app
#' @noRd
l <- run_ToCS
