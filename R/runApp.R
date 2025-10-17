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

  # Force the shiny app to open in the browser - ensures links in interface work
  options(shiny.launch.browser = TRUE)

  # Force the shiny app to see the contents (downloadable folders) in the www
  # folder in inst/www/
  shiny::addResourcePath(prefix = "ZipFiles",
                         directoryPath = system.file("www",package = "ToCS"))

  # Run the shiny app using the UI and server functions
  # Constructed this way to pass UI pars through to make testing faster
    shiny::runApp(list(ui = ToCS::app_ui(ui_pars),
                       server = ToCS::app_server))
}

#' Run the ToCS app
#' @noRd
l <- run_ToCS
