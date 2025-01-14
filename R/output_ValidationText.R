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

validate_text_Common <- function(pars,comprun){


  if (pars[["func"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["func"]] != 'Select', label = "Desired output")
     )
  }
  if (pars[["spec"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["spec"]] != 'Select', label = "Species")
    )
  }
  if (pars[["defaulttoHuman"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["defaulttoHuman"]] != 'Select', label = "Human values default preference")
    )
  }
  if (pars[["runCompounds"]]==0){
    shiny::validate(
      shiny::need(pars[["runCompounds"]]>0, label = "Check that at least one compound has been selected or uploaded. If so, clicking the 'Load Compounds' button")
    )
  }
  if (pars[["model"]] == 'Select'){
    shiny::validate(shiny::need(pars[["model"]] != 'Select', label = "Model"))
    }
}

validate_text_ADME <- function(pars){

    validate_text_Common(pars,"No")

    if (pars[["dosenum"]] == 'Select' && pars[["runsim"]]>0){
      shiny::validate(
        shiny::need(pars[["dosenum"]] != 'Select', label = "Dosing frequency")
      )
    }

    if (pars[["dosenum"]] == 'Multiple Doses' && pars[["runsim"]]>0 && pars[["multdose"]] == 'Select'){
      shiny::validate(
        shiny::need(pars[["multdose"]] != 'Select', label = "Multiple dosing frequency")
      )
    }

    if (pars[["dosenum"]] == 'Multiple Doses' && pars[["runsim"]]>0 && pars[["multdose"]] == 'No' && pars[["multdose_odd"]] == ''){
      shiny::validate(
        shiny::need(multdose_odd != '', label = "Multiple dosing administration amounts and times")
      )
    }

    if (pars[["spec"]] != 'Human' && pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
      shiny::validate(
        shiny::need(pars[["spec"]] == 'Human', label = "To run the fetal pbtk model, the species 'Human'")
      )
    }

    if (pars[["runsim"]]>0 && pars[["model"]] == 'fetal_pbtk'){
      if (length(pars[["returntimes"]])>0){
        mintime <- min(pars[["returntimes"]])
        if (mintime < 91){
          shiny::validate(
            shiny::need(pars[["returntimes"]][1] >= 13*7, label = "A beginning output time of 13 weeks (91 days) or later")
            )
        }
      }
    }
}

validate_text_SS <- function(pars){

  validate_text_Common(pars,"No")

  # if (!is.null(pars[["tissueSS"]])){
  #   if (pars[["tissueSS"]] == 'gut'){
  #     shiny::validate(
  #       shiny::need(pars[["tissueSS"]] != 'gut', label = "Gut tissue is not available at this time. Another tissue")
  #       )
  #     }
  # }
}

validate_text_IVIVE <- function(pars){

  validate_text_Common(pars,"No")

  if (is.null(pars[["BioactiveFile"]]) && pars[["runsim"]]>0){
    shiny::validate(
      shiny::need(!is.null(pars[["BioactiveFile"]]), label = "A file with in vitro bioactive concentrations")
    )
  }
  # if (!is.null(pars[["tissueIVIVE"]])){
  #   if (pars[["tissueIVIVE"]] == 'gut' && pars[["runsim"]]>0){
  #     shiny::validate(
  #       shiny::need(pars[["tissueIVIVE"]] != 'gut', label = "Gut tissue is not available at this time. Another tissue")
  #       )
  #     }
  # }

  if (pars[["returnsamples"]] == 'Select' && pars[["runsim"]]>0){
    shiny::validate(
      shiny::need(pars[["returnsamples"]] != 'Select', label = "Desired AED output")
    )
  }


}

validate_text_Pars <- function(pars){

  if (pars[["func"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["func"]] != 'Select', label = "Desired output")
    )
  }
  if (pars[["spec"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["spec"]] != 'Select', label = "Species")
    )
  }
  if (pars[["defaulttoHuman"]] == 'Select'){
    shiny::validate(
      shiny::need(pars[["defaulttoHuman"]] != 'Select', label = "Human values default preference")
    )
  }
  if (pars[["runCompounds"]]==0){
    shiny::validate(
      shiny::need(pars[["runCompounds"]]>0, label = "Check that at least one compound has been selected or uploaded. If so, clicking the 'Load Compounds' button")
    )
  }



}
