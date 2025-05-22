
################################################################################
# This file contains functions used in the steady state concentrations module
# to generate the output needed for the 'Run Simulation' tab
################################################################################


################################################################################
################################################################################

#' Calculate the steady state concentration solutions
#'
#' @description
#' This function is the main steady state module solution function from which the
#' server outputs are derived. This function orders the calculation of steady state
#' concentrations and the number of days to steady state. It formats the outputs
#' for appropriate display in the interface. The current function is called by
#' SS_server() and calls CalcAnalyticCss(), CalcCssDay(), and StorePars_SS().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list with steady state concentrations, steady state characteristics,
#' and simulation and chemical-physical parameters used in the simulation
#' @noRd
#'
SS_sol <- function(pars){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  SteadyState <- CssDay <- NULL

  # Get row, column, and page dimensions for arrays used to store solutions
  n <- nrow(pars[["CompoundList"]])

  # --- sol is for storing the steady state concentration for each compound (each row)
  sol <- data.frame(CompoundName = pars[["CompoundList"]][,1],
                    SteadyState = rep(0,n))

  css_char <- data.frame(CompoundName = pars[["CompoundList"]][,1],
                         AvgConc = rep(0,n),
                         RatioAvgAnalytical = rep(0,n),
                         MaxConc = rep(0,n),
                         CssDay = rep(0,n))

  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {

    # --- Solve model for each compound
    for (i in (1:n)) {

      # --- Increment the progress bar and update detail text
      shiny::incProgress(1/n, detail = paste("Generating the steady state concentrations for chemical", i))

      #--- Calculate the steady state concentration and save in data frame
      sol[i,2] <- CalcAnalyticCss(pars,i)

      #--- Calculate the number of days it takes for each compound to approximately reach steady state
      css_char[i,2:5] <- CalcCssDay(pars,i)
    }
  })

  # --- Sort data frames from smallest to largest
  sol_ascend <- dplyr::arrange(sol, SteadyState)
  css_char_ascend <- dplyr::arrange(css_char, CssDay)
  css_char_ascend <- magrittr::`%>%`(css_char_ascend, dplyr::relocate("CssDay", .after = "CompoundName"))

  # --- Create a data frame with all used parameters and chemical data
  pars_df <- StorePars_SS(pars)

  # --- Create list with both outputs (plot list and sol array)
  out_list <- list(sol_ascend,css_char_ascend,pars_df)
}

################################################################################
################################################################################

#' Calculate the analytical steady state concentration of a chemical
#'
#' @description
#' This function calculates the analytical steady state concentration of an
#' individual compound using all user-selected parameters. The 'httk' function
#' calc_analytic_css is used to generate the output. The current function is
#' called by SS_sol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i Chemical index on the simulated chemicals list
#'
#' @return A numerical value representing the analytical steady state concentration
#' in the specified units
#' @noRd
#'
CalcAnalyticCss <- function(pars,i){

  out <- httk::calc_analytic_css(chem.name = pars[["CompoundList"]][i,1],
                           species = pars[["spec"]],
                           dose = pars[["dailydose"]],
                           route = pars[["doseroute"]],
                           output.units = pars[["modelSSout_units"]],
                           model = pars[["model"]],
                           concentration = pars[["output_concSS"]],
                           tissue = pars[["tissueSS"]],
                           restrictive.clearance = pars[["restrict_clear"]],
                           bioactive.free.invivo = pars[["bioactiveIVIVE"]],
                           suppress.messages = TRUE,
                           Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                Caco2.Fabs = pars[["caco_fabs"]],
                                                Caco2.Fgut = pars[["caco_fgut"]],
                                                overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                keepit100 = pars[["caco_keep100"]]),
                           parameterize.args.list = list(default.to.human = pars[["defaulttoHuman"]],
                                                         adjusted.Funbound.plasma = pars[["adj_fub"]],
                                                         minimum.Funbound.plasma = pars[["min_fub"]],
                                                         regression = pars[["regression"]],
                                                         clint.pvalue.threshold = pars[["Clint_Pval"]]))
}

################################################################################
################################################################################

#' Calculate the day steady state is reached
#'
#' @description
#' This function calculates the days to steady state of an individual compound
#' as well as other related steady state attributes using all user-selected
#' parameters. The 'httk' function calc_css is used to generate the output.
#' The current function is called by SS_sol().
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i Chemical index on the simulated chemicals list
#'
#' @return A list composed of the average concentration at the end of the
#' simulation, the fraction of the true steady state reached on the steady state
#' day, the maximum concentration of the simulation, and the day steady state
#' was reached
#' @noRd
#'
CalcCssDay <- function(pars,i){

  out <- httk::calc_css(chem.name = pars[["CompoundList"]][i,1],
                        species = pars[["spec"]],
                        daily.dose = pars[["dailydose"]],
                        route = pars[["doseroute"]],
                        output.units = pars[["modelSSout_units"]],
                        model = pars[["model"]],
                        suppress.messages = TRUE,
                        parameterize.args.list = list(default.to.human = pars[["defaulttoHuman"]],
                                                      adjusted.Funbound.plasma = pars[["adj_fub"]],
                                                      regression = pars[["regression"]],
                                                      restrictive.clearance = pars[["restrict_clear"]],
                                                      minimum.Funbound.plasma = pars[["min_fub"]],
                                                      clint.pvalue.threshold = pars[["Clint_Pval"]],
                                                      Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                                           Caco2.Fabs = pars[["caco_fabs"]],
                                                                           Caco2.Fgut = pars[["caco_fgut"]],
                                                                           overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                                           keepit100 = pars[["caco_keep100"]])))
}

################################################################################
################################################################################

#' Store all relevant module parameters and physical-chemical in a data frame
#' for user download
#'
#' @description
#' This function stores all user-selected simulation parameters for the steady
#' state concentrations module as well as all physical-chemical data for simulated
#' compounds into one data frame. The current function is called by SS_sol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A data frame of simulation parameters and chemical-physical data for
#' the chemicals simulated
#' @noRd
#'
StorePars_SS <- function(pars){

  if (!is.null(pars[["tissueSS"]])){
    tissueSS <- pars[["tissueSS"]]
  }
  else{
    tissueSS <- "NULL"
  }

  out <- data.frame(chem.name = pars[["CompoundList"]][,1],
                    species = pars[["spec"]],
                    dose = pars[["dailydose"]],
                    route = pars[["doseroute"]],
                    output.units = pars[["modelSSout_units"]],
                    model = pars[["model"]],
                    concentration = pars[["output_concSS"]],
                    tissue = tissueSS,
                    restrictive.clearance = pars[["restrict_clear"]],
                    bioactive.free.invivo = pars[["bioactiveIVIVE"]],
                    Caco2.Pab.default = pars[["caco2default"]],
                    Caco2.Fabs = pars[["caco_fabs"]],
                    Caco2.Fgut = pars[["caco_fgut"]],
                    overwrite.invivo = pars[["caco_overwriteinvivo"]],
                    keepit100 = pars[["caco_keep100"]],
                    default.to.human = pars[["defaulttoHuman"]],
                    adjusted.Funbound.plasma = pars[["adj_fub"]],
                    minimum.Funbound.plasma = pars[["min_fub"]],
                    regression = pars[["regression"]])

  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,out$chem.name)),]
  out <-cbind(out,chemdata)
}

################################################################################
################################################################################

#' Create a scatter plot of steady state concentrations for the steady state
#' concentrations module
#'
#' @description
#' This function generates the scatter plot ggplot object containing the steady
#' state concentrations for all simulated compounds. The plot has a log10 y-axis
#' scale if desired by the user. The current function is called by
#' SS_ConcPlot_server() and calls plot_labels() and plot_logscale().
#'
#'
#' @param sol A data frame of steady state concentrations
#' @param pars A list of all user input parameters for the entire app
#' @param logscale The log10 y-axis scale checkbox input to signify whether the
#' user wants a plot to have a log10 y-axis scale
#'
#' @return A scatter plot object of steady state concentrations, either with a
#' linear or log10 scale y-axis
#' @noRd
#'
scat_plot <- function(sol,pars,logscale){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  CompoundName <- SteadyState <- NULL

  # --- Set Label Names
  plt_dynamic_labels <- plot_labels(pars)
  title_exp <- plt_dynamic_labels[[1]]
  y_exp <- plt_dynamic_labels[[2]]

  # --- Plot each steady state concentration
  sol$CompoundName <- factor(sol$CompoundName, levels = sol$CompoundName)

  plt <- ggplot2::ggplot(sol, ggplot2::aes(x = CompoundName, y = SteadyState)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
          plot.title = ggplot2::element_text(hjust = 0.5))

  if (logscale == TRUE){
    plt <- plot_logscale(plt,sol$SteadyState)
  }
  return(plt)
}

################################################################################
################################################################################

#' Steady state module plot labeling function
#'
#' @description
#' This function generates the plotting labels (y-axis and title expressions) for
#' the steady state concentrations plot. The current function is called by
#' scat_plot().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list containing plots labels for the title and y-axis.
#' @noRd
#'
plot_labels <- function(pars){

  # --- Set Label Names of title and y axis
  title_exp <- paste("Steady state concentrations generated from the ", pars[["model"]], " model", sep = "")

  if (pars[["output_concSS"]] != 'tissue'){
    if (is.null(pars[["tissueSS"]])){
      y_exp <- paste("Whole body ", pars[["output_concSS"]], " \n concentration (", pars[["modelSSout_units"]], ")", sep = "")
    }
    else{
      y_exp <- paste(pars[["output_concSS"]], " concentration \n in the ", pars[["tissueSS"]], " (", pars[["modelSSout_units"]], ")", sep = "")
    }
  }
  else{
    y_exp <- paste(pars[["tissueSS"]], " concentration (", pars[["modelSSout_units"]], ")", sep = "")
  }

  out <- list(title_exp,y_exp)
}
