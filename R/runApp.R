########################
# RUN THE TOCS APP
########################

#' Run the ToCS App
#'
#' @return The ToCS interface
#' @export
#'
#' @examples run_ToCS()
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

#' @export
l <- run_ToCS
