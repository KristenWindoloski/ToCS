

#########################################################
# --- SOLVE MODEL FOR STEADY STATE SOLUTION
#########################################################

SS_sol <- function(pars){

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

  # --- Solve model for each compound
  for (i in (1:n)) {

    #--- Calculate the steady state concentration and save in data frame
    sol[i,2] <- CalcAnalyticCss(pars,i)

    #--- Calculate the number of days it takes for each compound to approximately reach steady state
    css_char[i,2:5] <- CalcCssDay(pars,i)

  }
  # --- Sort data frames from smallest to largest
  sol_ascend <- dplyr::arrange(sol, SteadyState)
  css_char_ascend <- dplyr::arrange(css_char, CssDay)
  css_char_ascend <- css_char_ascend %>% dplyr::relocate("CssDay", .after = "CompoundName")

  # --- Create a data frame with all used parameters and chemical data
  pars_df <- StorePars_SS(pars)

  # --- Create list with both outputs (plot list and sol array)
  out_list <- list(sol_ascend,css_char_ascend,pars_df)
}

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
                           parameterize.args = list(default.to.human = pars[["defaulttoHuman"]],
                                                    adjusted.Funbound.plasma = pars[["adj_fub"]],
                                                    minimum.Funbound.plasma = pars[["min_fub"]],
                                                    regression = pars[["regression"]]))
}

CalcCssDay <- function(pars,i){

  out <- httk::calc_css(chem.name = pars[["CompoundList"]][i,1],
                  species = pars[["spec"]],
                  daily.dose = pars[["dailydose"]],
                  route = pars[["doseroute"]],
                  output.units = pars[["modelSSout_units"]],
                  tissue = NULL,
                  model = pars[["model"]],
                  default.to.human = pars[["defaulttoHuman"]],
                  adjusted.Funbound.plasma = pars[["adj_fub"]],
                  regression = pars[["regression"]],
                  restrictive.clearance = pars[["restrict_clear"]],
                  suppress.messages = TRUE)
}

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

######################################################
# --- CREATE SCATTER PLOT OF STEADY STATE VALUES
######################################################

scat_plot <- function(sol,pars,logscale){

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
