########################
# RUN THE TOCS APP
########################

#' Use this function to run the ToCS App
#'
#' @description
#' This is the primary function to run the ToCS app once the ToCS package is
#' loaded into R. A window should open after typing 'run_ToCS' into the console.
#'
#'
#' @return A popup window with the ToCS user interface
#' @export
#'
run_ToCS <- function(){

  applocal <- system.file("app",package = "ToCS")
  if (exists("chem.physical_and_invitro.data",envir = .GlobalEnv)){
    shiny::stopApp()
    print("Please rename your global environment variable 'chem.physical_and_invitro.data'. Then, run ToCS again.")
  }
  else{
    if (interactive()){
      shiny::runApp(appDir = applocal)
      }
    else{
      shiny::shinyAppDir(appDir = applocal)
    }
  }
}

#' Run the ToCS app
#' @noRd
l <- run_ToCS
