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
#' @examples
#' # Open the ToCS interface
#' run_ToCS()
#'
#'
run_ToCS <- function(){

  applocal <- system.file("app",package = "ToCS")
  if (interactive()){
    shiny::runApp(appDir = applocal)
  }
  else{
    shiny::shinyAppDir(appDir = applocal)
  }
}

#' Run the ToCS app
#' @noRd
l <- run_ToCS
