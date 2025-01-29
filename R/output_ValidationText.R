caption_text <- function(func, model){

  if (func == "ADME"){

    if (model == 'pbtk'){
      AUCoutput <- "venous plasma concentration."
    }
    else if (model == 'fetal_pbtk'){
      AUCoutput <- "venous plasma concentration."
    }
    else if (model == '3compartment') {
      AUCoutput <- "Csyscomp (aggregated remaining tissues - ART) plasma concentration."
    }
    else if (model == '1compartment') {
      AUCoutput <- "Ccompartment (aggregated remaining tissues - ART) plasma concentration."
    }
  }


}

validate_text_Common <- function(pars){


  if (pars[["func"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["func"]] != 'Select',
                  message = paste("Return to the 'General Parameters' tab and select the desired output module for this simulation."))
     )
  }
  if (pars[["spec"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["spec"]] != 'Select',
                  message = paste("Return to the 'General Parameters' tab and select the desired species for this simulation."))
    )
  }
  if (pars[["defaulttoHuman"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["defaulttoHuman"]] != 'Select',
                  message = paste("Return to the 'General Parameters' tab and select the desired default to human preference for this simulation."))
    )
  }
  if (pars[["model"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["model"]] != 'Select',
                  message = paste(""))
                  #message = paste("Return to the 'Model Specifications' tab and select the desired model for this simulation."))
    )
  }
  if (pars[["insilicopars"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["insilicopars"]] != 'Select',
                  message = paste("Return to the 'Model Specifications' tab and select the desired in silico parameter preference for this simulation.")))
  }
  if (is.null(c(pars[["httkPreloadComps"]],pars[["httkPreloadComps_Honda"]],pars[["file1"]]))){
    shiny::validate(
          shiny::need(!is.null(c(pars[["httkPreloadComps"]],pars[["httkPreloadComps_Honda"]],pars[["file1"]])),
                      message = paste("At least one compound must be selected or uploaded on the 'Compound Selection' tab. Return to that tab, and make sure to click the 'Load Compounds' button too."))
          )
  }
  if (pars[["runCompounds"]]==0){
    shiny::validate(
      shiny::need(pars[["runCompounds"]]>0,
                  message = paste("Return to the 'Compound Selection' tab and click the 'Load Compounds' button."))
    )
  }
}

validate_text_ADME <- function(pars){

    validate_text_Common(pars)

    if (pars[["dosenum"]] == 'Select' && pars[["runsim"]]>0){
      shiny::validate(
        shiny::need(pars[["dosenum"]] != 'Select',
                    message = paste("Return to the 'Model Specifications' tab and select the desired dosing frequency for this simulation."))
      )
    }

    if (pars[["dosenum"]] == 'Multiple Doses' && pars[["runsim"]]>0 && pars[["multdose"]] == 'Select'){
      shiny::validate(
        shiny::need(pars[["multdose"]] != 'Select',
                    message = paste("Return to the 'Model Specifications' tab and select the desired multiple dosing frequency for this simulation."))
      )
    }

    if (pars[["dosenum"]] == 'Multiple Doses' && pars[["runsim"]]>0 && pars[["multdose"]] == 'No' && pars[["multdose_odd"]] == ''){
      shiny::validate(
        shiny::need(pars[["multdose_odd"]] != '',
                    message = paste("Return to the 'Model Specifications' tab and select the desired multiple dosing administration amounts and times for this simulation."))
      )
    }

    if (pars[["spec"]] != 'Human' && pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
      shiny::validate(
        shiny::need(pars[["spec"]] == 'Human',
                    message = paste("To run the fetal pbtk model, the species 'Human' must selected. Either return to the 'General Parameters' tab and change the species to 'Human' or select a different model under the 'Model Specifications' tab."))
      )
    }

    if (pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
      if (length(pars[["returntimes"]])>0){
        mintime <- min(pars[["returntimes"]])
        if (mintime < 91){
          shiny::validate(
            shiny::need(pars[["returntimes"]][1] >= 13*7,
                        message = paste("A beginning output time of 13 weeks (91 days) or later must be specified. Return to the 'Advanced Parameters' tab and update the output times entered."))
            )
        }
      }
    }


}

validate_text_SS <- function(pars){

  validate_text_Common(pars)
}

validate_text_IVIVE <- function(pars){

  validate_text_Common(pars)

  if (is.null(pars[["BioactiveFile"]]) && pars[["runsim"]]>0){
    shiny::validate(
      shiny::need(!is.null(pars[["BioactiveFile"]]),
                  message = paste("A file with in vitro bioactive concentrations must be uploaded under the 'Model Specifications' tab. Return to this tab and upload the file."))
    )
  }

  if (pars[["returnsamples"]] == 'Select' && pars[["runsim"]]>0){
    shiny::validate(
      shiny::need(pars[["returnsamples"]] != 'Select',
                  message = paste("The desired OED output format (single OED or all OED samples) must be selected. Return to the 'Model Specifications' tab."))
    )
  }


}

validate_text_Pars <- function(pars){

  validate_text_Common(pars)
}
