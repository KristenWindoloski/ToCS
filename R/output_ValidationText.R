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
                  message = paste(""))
     )
  }
  if (pars[["spec"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["spec"]] != 'Select',
                  message = paste(""))
    )
  }
  if (pars[["defaulttoHuman"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["defaulttoHuman"]] != 'Select',
                  message = paste(""))
    )
  }
  if (pars[["model"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["model"]] != 'Select',
                  message = paste(""))
    )
  }
  if (pars[["insilicopars"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["insilicopars"]] != 'Select',
                  message = paste("")))
  }
  if (is.null(c(pars[["httkPreloadComps"]],pars[["file1"]]))){
    shiny::validate(
          shiny::need(!is.null(c(pars[["httkPreloadComps"]],pars[["file1"]])),
                      message = paste(""))
          )
  }
}

validate_text_ADME <- function(pars){

    validate_text_Common(pars)

    if (pars[["dosenum"]] == 'Select' && pars[["runsim"]]>0){
      shiny::validate(
        shiny::need(pars[["dosenum"]] != 'Select',
                    message = paste(""))
      )
    }

    if (pars[["dosenum"]] == 'Multiple Doses' && pars[["runsim"]]>0 && pars[["multdose"]] == 'Select'){
      shiny::validate(
        shiny::need(pars[["multdose"]] != 'Select',
                    message = paste(""))
      )
    }

    if (pars[["dosenum"]] == 'Multiple Doses' && pars[["runsim"]]>0 && pars[["multdose"]] == 'No' && pars[["multdose_odd"]] == ''){
      shiny::validate(
        shiny::need(pars[["multdose_odd"]] != '',
                    message = paste(""))
      )
    }

    if (pars[["spec"]] != 'Human' && pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
      shiny::validate(
        shiny::need(pars[["spec"]] == 'Human',
                    message = paste(""))
      )
    }

    if (pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
      if (length(pars[["returntimes"]])>0){
        mintime <- min(pars[["returntimes"]])
        if (mintime < 91){
          shiny::validate(
            shiny::need(pars[["returntimes"]][1] >= 13*7,
                        message = paste(""))
            )
        }
      }
    }
}

validate_text_IVIVE <- function(pars){

  validate_text_Common(pars)

  if (is.null(pars[["BioactiveFile"]]) && pars[["runsim"]]>0){
    shiny::validate(
      shiny::need(!is.null(pars[["BioactiveFile"]]),
                  message = paste(""))
    )
  }

  if (pars[["returnsamples"]] == 'Select' && pars[["runsim"]]>0){
    shiny::validate(
      shiny::need(pars[["returnsamples"]] != 'Select',
                  message = paste(""))
    )
  }
}
