
################################################################################
################################################################################

#' Main function to calculate the oral equivalent dose and bioactivity exposure
#' ratio for a list of chemicals
#'
#' @description
#' This function is the primary function used to calculate the oral equivalent
#' dose and bioactivity exposure ratio for a list of chemicals, if applicable.
#' This function calls other functions that do the actual calculations, but it
#' returns the final output used to generate the specified outputs in the IVIVE
#' server functions. The current function is called by IVIVE_server() and calls
#' ConvertBioactive(), PrepExposureData(), Calc_OEDBER_RS_False(),
#' Calc_OEDBER_RS_True(), and StorePars_IVIVE().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list consisting of the data frame of oral equivalent dose solutions,
#' the bioactive concentration data frame, a data frame with simulation parameters
#' and physical-chemical data for chemicals simulated, a data frame with bioactivity
#' exposure ratios (BER), and a data frame with user-uploaded exposure data
#' @noRd
#'
IVIVEsol <- function(pars){

  # --- PROCESS BIOACTIVE CONCENTRATIONS FILE
  file <- pars[["BioactiveFile"]]
  bioactive <- utils::read.csv(file$datapath)

  # --- REARRANGE ROWS OF BIOACTIVE FILE TO BE IN SAME ORDER AS COMPOUNDS FILE
    # --- CONVERT BIOACTIVE CONCENTRATION IF HONDA1 IS SELECTED
  chemdf <- the$chemdata
  df <- chemdf[chemdf$Compound %in% pars[["CompoundList"]][,1],]
  df <- df[match(pars[["CompoundList"]][,1], df$Compound),]
  bioactive_conc <- bioactive[match(df$CAS, bioactive$CAS),]
  bioactive_conc <- ConvertBioactive(pars,bioactive_conc)

  # --- EXTRACT DIMENSIONS NEEDED FOR SOLUTION
  n <- nrow(pars[["CompoundList"]])

  # --- CALCULATE EXPOSURE DATA FOR BER CALCULATION
  if (!is.null(pars[["fileExposure"]])){
    exposuredata <- PrepExposureData(pars)
  }
  else{
    exposuredata <- NULL
  }

  # --- SET OUTPUT TYPE AND SIZE: DATA FRAME (return.samples = FALSE) OR ARRAY (return.samples = TRUE)
  if (pars[["returnsamples"]] == FALSE){
    out <- Calc_OEDBER_RS_False(n,pars,bioactive_conc,exposuredata)
  }
  else if (pars[["returnsamples"]] == TRUE) {
    out <- Calc_OEDBER_RS_True(n,pars,bioactive_conc,exposuredata)
  }
  sol <- out[[1]]
  BER <- out[[2]]

  # --- STORE PARAMETERS USED FOR THE SIMULATION
  pars_df <- StorePars_IVIVE(pars,bioactive_conc)

  # --- RETURN LIST OF OUTPUTS
  out <- list(sol,bioactive_conc,pars_df,BER,exposuredata)
}

################################################################################
################################################################################

#' Calculate the oral equivalent dose (OED) of a chemical
#'
#' @description
#' This function calculates the oral equivalent dose (OED) of a chemical given
#' its bioactive concentration. The 'httk' package function 'calc_mc_oral_equiv"
#' is used to generate either a single OED or a vector of OEDs depending on
#' whether the user wants only a specific quantile returned or all OED samples
#' returned. The current function is called by Calc_OEDBER_RS_False() and
#' Calc_OEDBER_RS_True().
#'
#'
#' @param i Index indicating which chemical on the list to simulate
#' @param pars A list of all user input parameters for the entire app
#' @param bioactive_df A data frame with the chemical name, CAS number, and
#' bioactive concentration (in uM units) for each chemical to simulate
#'
#' @return A numeric value or vector of values, depending on the parameter
#' @noRd
#'
CalcOED <- function(i,pars,bioactive_df){

  # --- SET RANDOM GENERATOR SEED
  set.seed(100)

  # --- CALC OED
  OED <- httk::calc_mc_oral_equiv(conc = bioactive_df[i,3],
                                  chem.name = pars[["CompoundList"]][i,1],
                                  which.quantile = pars[["quantile"]],
                                  species = pars[["spec"]],
                                  input.units = "uM",
                                  output.units = pars[["modelIVIVEout_units"]],
                                  return.samples = pars[["returnsamples"]],
                                  restrictive.clearance = pars[["restrict_clear"]],
                                  bioactive.free.invivo = pars[["bioactiveIVIVE"]],
                                  tissue = pars[["tissueIVIVE"]],
                                  concentration = pars[["output_concIVIVE"]],
                                  IVIVE = pars[["HondaIVIVE"]],
                                  model = pars[["model"]],
                                  samples = pars[["samples"]],
                                  chemdata = the$chemdata,
                                  Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                       Caco2.Fabs = pars[["caco_fabs"]],
                                                       Caco2.Fgut = pars[["caco_fgut"]],
                                                       overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                       keepit100 = pars[["caco_keep100"]]),
                                  calc.analytic.css.arg.list = list(adjusted.Funbound.plasma = pars[["adj_fub"]],
                                                                    restrictive.clearance = pars[["restrict_clear"]],
                                                                    default.to.human = pars[["defaulttoHuman"]],
                                                                    minimum.Funbound.plasma = pars[["min_fub"]],
                                                                    regression = pars[["regression"]],
                                                                    clint.pvalue.threshold = pars[["Clint_Pval"]],
                                                                    Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                                                         Caco2.Fabs = pars[["caco_fabs"]],
                                                                                         Caco2.Fgut = pars[["caco_fgut"]],
                                                                                         overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                                                         keepit100 = pars[["caco_keep100"]])),
                                  parameterize.args.list = list(adjusted.Funbound.plasma = pars[["adj_fub"]],
                                                                restrictive.clearance = pars[["restrict_clear"]],
                                                                default.to.human = pars[["defaulttoHuman"]],
                                                                minimum.Funbound.plasma = pars[["min_fub"]],
                                                                regression = pars[["regression"]],
                                                                clint.pvalue.threshold = pars[["Clint_Pval"]],
                                                                Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                                                     Caco2.Fabs = pars[["caco_fabs"]],
                                                                                     Caco2.Fgut = pars[["caco_fgut"]],
                                                                                     overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                                                     keepit100 = pars[["caco_keep100"]])))

}

################################################################################
################################################################################

#' Convert a bioactive concentration from a nominal concentration to a free
#' concentration in vitro
#'
#' @description
#' This function converts nominal bioactive concentrations to a free concentration
#' in vitro, if the user selects the 'Honda1' IVIVE assumption. If chosen, this
#' assumption uses the 'httk' package's 'armitage_eval' function to perform the
#' conversion. If the 'Honda1' assumption is not chosen, then the original
#' 'bioactive_df' data frame is returned. The current function is called by
#' IVIVEsol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param bioactive_df A data frame with the chemical name, CAS number, and
#' bioactive concentration (in uM units) for each chemical to simulate
#'
#' @return A data frame with the chemical name, CAS number, and bioactive
#' concentration
#' @noRd
#'
ConvertBioactive <- function(pars,bioactive_df){

  if (is.null(pars[["HondaIVIVE"]])){
    bioactive_conc <- bioactive_df
  }
  else if (pars[["HondaIVIVE"]] == "Honda1"){

    arm_out <- httk::armitage_eval(casrn.vector = bioactive_df[,2],
                                   this.FBSf = pars[["FSBf"]],
                                   nomconc.vector = bioactive_df[,3],
                                   chemdata = the$chemdata)

    bioactive_conc <- data.frame(ChemicalName = pars[["CompoundList"]][,1],
                                 CAS = arm_out$casrn,
                                 BioactiveConcentration = arm_out$cfree.invitro)
  }
  else {
    bioactive_conc <- bioactive_df
  }
}

################################################################################
################################################################################

#' Compile the simulation parameters and physical-chemical data used into one
#' data frame
#'
#' @description
#' This function combines the used simulation parameters and chemical-physical
#' data for simulated compounds into one data frame. The returned parameters are
#' only those used to generate the simulation, and the chemical-physical data is
#' retrieved from the 'httk' package's 'chem.physical_and_invitro.data' data frame.
#' The current function is called by IVIVEsol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param bioactive_df A data frame with the chemical name, CAS number, and
#' bioactive concentration (in uM units) for each chemical to simulate
#'
#' @return A data frame with simulation parameters and physical-chemical data
#' for simulated compounds
#' @noRd
#'
StorePars_IVIVE <- function(pars,bioactive_df){

  # --- CONVERT POTENTIAL NULL PARAMETERS
  if (!is.null(pars[["tissueIVIVE"]])){
    tissueIVIVE <- pars[["tissueIVIVE"]]
  }
  else{
    tissueIVIVE <- "NULL"
  }

  if (!is.null(pars[["HondaIVIVE"]])){
    HondaIVIVE <- pars[["HondaIVIVE"]]
  }
  else{
    HondaIVIVE <- "NULL"
  }

  pars_df <- data.frame(chem.name = pars[["CompoundList"]][,1],
                        conc = bioactive_df[,3],
                        which.quantile = pars[["quantile"]],
                        species = pars[["spec"]],
                        input.units = "uM",
                        output.units = pars[["modelIVIVEout_units"]],
                        return.samples = pars[["returnsamples"]],
                        restrictive.clearance = pars[["restrict_clear"]],
                        bioactive.free.invivo = pars[["bioactiveIVIVE"]],
                        tissue = tissueIVIVE,
                        concentration = pars[["output_concIVIVE"]],
                        IVIVE = HondaIVIVE,
                        model = pars[["model"]],
                        Caco2.Pab.default = pars[["caco2default"]],
                        Caco2.Fabs = pars[["caco_fabs"]],
                        Caco2.Fgut = pars[["caco_fgut"]],
                        overwrite.invivo = pars[["caco_overwriteinvivo"]],
                        keepit100 = pars[["caco_keep100"]],
                        adjusted.Funbound.plasma = pars[["adj_fub"]],
                        default.to.human = pars[["defaulttoHuman"]],
                        minimum.Funbound.plasma = pars[["min_fub"]],
                        regression = pars[["regression"]])

  chemdf <- the$chemdata
  chemdata <- chemdf[chemdf$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,pars_df$chem.name)),]
  pars_df <-cbind(pars_df,chemdata)

}

################################################################################
################################################################################

#' Plot the oral equivalent dose (OED) and exposure data (if applicable)
#'
#' @description
#' This function plots all OEDs and any uploaded exposure data. How the OEDs are
#' plotted is dependent upon the user-selected output type (single OED quantile
#' or all OED samples). Exposure data (if applicable) is plotted as a point range.
#' The current function is called by IVIVE_Plot_server() and calls
#' OEDPoint_Exposure_Plot(), OEDPoint_NoExposure_Plot(), Plotdf_Prep(),
#' OEDSample_Exposure_Plot(), OEDSample_NoExposure_Plot(), and
#' IVIVEplot_logscale().
#'
#'
#' @param OED_data A data frame of chemical names and their OEDs (either one per
#' chemical or many per chemical if samples were returned); The first output
#' from the IVIVEsol function
#' @param BioactiveConc An input not used
#' @param pars A list of all user input parameters for the entire app
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale
#' @param expdata A data frame with chemical names and exposure data estimates
#'
#' @return A ggplot2 plotting object
#' @noRd
#'
IVIVEplotting <- function(OED_data,BioactiveConc,pars,logscale,expdata){

  OED <- OEDsample_df <- combined_df <- Q5_OED_df <- NULL

  # --- SET PLOT LABEL NAMES
  plt_labels <- IVIVEplot_labels(pars)
  y_exp <- plt_labels[[2]]
  title_exp <- plt_labels[[1]]

  OEDsample_df <- NULL
  combined_df <- NULL
  Q5_OED_df <- NULL

  if (pars[["returnsamples"]] == FALSE){

    # --- ARRANGE OED DATA FOR PLOTTING
    OED_data <- dplyr::arrange(OED_data, OED)
    OED_data$CompoundName <- factor(OED_data$CompoundName, levels = OED_data$CompoundName)

    # --- PLOT SCATTER PLOT OF ALL OED VALUES
    if (!is.null(pars[["fileExposure"]])){
      out <- OEDPoint_Exposure_Plot(OED_data,expdata,y_exp,title_exp)
      plt <- out[[1]]
      combined_df <- out[[2]]
    }
    else{
      plt <- OEDPoint_NoExposure_Plot(OED_data,y_exp,title_exp)
    }

  }
  else if (pars[["returnsamples"]] == TRUE){

    # --- CREATE DATA FRAME FOR OED SAMPLES TO PLOT
    plt_df_list <- Plotdf_Prep(OED_data,pars)
    OEDSamples_df <- plt_df_list[[1]]
    Q5_OED_df <- plt_df_list[[2]]

    # --- PLOT OED SAMPLES
    if (!is.null(pars[["fileExposure"]])){
      out <- OEDSample_Exposure_Plot(OEDSamples_df,Q5_OED_df,expdata,y_exp,title_exp)
      plt <- out[[1]]
      expdata <- out[[2]]
    }
    else{
      plt <- OEDSample_NoExposure_Plot(OEDSamples_df,Q5_OED_df,y_exp,title_exp)
    }
  }

  # --- PLOT Y-AXIS ON LOG SCALE IF DESIRED
  if (logscale == TRUE){
    plt <- IVIVEplot_logscale(plt,pars,OEDSamples_df,combined_df,OED_data,expdata,Q5_OED_df)
  }
  return(plt)
}

################################################################################
################################################################################

#' Plot the oral equivalent dose (OED) for each chemical when there is no exposure
#' data uploaded
#'
#' @description
#' This function generates the OED point ggplot object when no exposure data is
#' uploaded by the user. The current function is called by IVIVEplotting().
#'
#'
#' @param OED_data A data frame with chemical names and their OEDs
#' @param y_exp The plot's y-axis label expression
#' @param title_exp The plot's title expression
#'
#' @return A ggplot2 plotting object
#' @noRd
#'
OEDPoint_NoExposure_Plot <- function(OED_data,y_exp,title_exp){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  OED <- CompoundName <- NULL

  # --- PLOT DATA
  plt <- ggplot2::ggplot(data = OED_data, ggplot2::aes(x = CompoundName, y = OED),) +
    ggplot2::geom_point(size = 4) +
    ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                   plot.title = ggplot2::element_text(hjust = 0.5))

  return(plt)
}

################################################################################
################################################################################

#' Plot the oral equivalent dose (OED) for each chemical when the user uploads
#' exposure data
#'
#' @description
#' This function generates the OED point ggplot object when exposure data is
#' uploaded by the user. The current function is called by [IVIVEplotting()] and
#' calls [FillExposureData()].
#'
#' @param OED_data A data frame with chemical names and their OEDs
#' @param expdata A data frame with chemical names and their exposure estimates
#' @param y_exp The plot's y-axis label expression
#' @param title_exp The plot's title expression
#'
#' @return A list with two elements: a ggplot2 plotting object and a data frame
#' with both OED and exposure data
#' @noRd
#'
OEDPoint_Exposure_Plot <- function(OED_data,expdata,y_exp,title_exp){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  OED <- Upper <- CompoundName <- Median <- Type <- Lower <- NULL

  # --- TRANSFORM OED_DATA INTO NEEDED FORMAT
  OED_data$Upper <- OED_data$OED + 1e-8
  OED_data$Lower <- OED_data$OED - 1e-8
  OED_data$Type <- "OED"
  OED_data <-  magrittr::`%>%`(OED_data, dplyr::relocate(OED, .after = Upper))
  names(OED_data)[names(OED_data) == "OED"] <- "Median"

  # --- TRANSFORM EXPOSURE DATA INTO NEEDED FORMAT
  expdata$Type <- "Exposure"
  expdata <- magrittr::`%>%`(expdata, dplyr::select(-c("maxval")))
  expdata <- FillExposureData(expdata)

  combined_df <- rbind(OED_data,expdata)

  # --- PLOT DATA
  plt <- ggplot2::ggplot(data = combined_df, ggplot2::aes(x = CompoundName, y = Median, color= Type)) +
    ggplot2::geom_pointrange(mapping = ggplot2::aes(ymin = Lower, ymax = Upper),
                             position=ggplot2::position_dodge(width = 0.5)) +
    ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                   plot.title = ggplot2::element_text(hjust = 0.5))

  return(list(plt,combined_df))
}

################################################################################
################################################################################

#' Plot the oral equivalent dose samples and user-uploaded exposure data estimates
#'
#' @description
#' This function generates the OED samples ggplot object no exposure data is
#' uploaded by the user. The current function is called by IVIVEplotting() and
#' calls FillExposureData().
#'
#' @param OEDSamples_df A data frame with chemical names and their OED samples
#' @param Q5_OED_df A data frame with chemical names and their 5th OED quantile value
#' @param expdata A data frame with chemical names and their exposure estimates
#' @param y_exp The plot's y-axis label expression
#' @param title_exp The plot's title expression
#'
#' @return A list with two elements: a ggplot2 plotting object and a data frame
#' with exposure data
#' @noRd
#'
OEDSample_Exposure_Plot <- function(OEDSamples_df,Q5_OED_df,expdata,y_exp,title_exp){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  OED <- CompoundName <- Type <- Lower <- Upper <- NULL

  OEDSamples_df$Type <- "OED"
  Q5_OED_df$Type <- "OED"

  expdata$Type <- "Exposure"
  expdata <- FillExposureData(expdata)
  names(expdata)[names(expdata) == "Median"] <- "OED"

  plt <- ggplot2::ggplot(OEDSamples_df, ggplot2::aes(x = CompoundName, y = OED, fill = Type)) +
    ggplot2::geom_boxplot(position=ggplot2::position_nudge(x=-0.15),
                          width = 0.2,
                          fill = "lightcoral") +
    ggplot2::geom_point(data = Q5_OED_df,
                        ggplot2::aes(x = CompoundName, y = OED),
                        color = 'red',
                        size = 3,
                        position=ggplot2::position_nudge(x=-0.15))+
    ggplot2::geom_pointrange(data = expdata,
                             mapping = ggplot2::aes(x = CompoundName, y = OED, ymin = Lower, ymax = Upper),
                             color = "slateblue1",
                             linewidth = 1,
                             size = 0.5,
                             position=ggplot2::position_nudge(x=0.15)) +
    ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                   plot.title = ggplot2::element_text(hjust = 0.5))

  return(list(plt,expdata))
}

################################################################################
################################################################################

#' Plot the oral equivalent dose samples when the user does not upload exposure data
#'
#' @description
#' This function generates the OED samples ggplot object when no exposure data is
#' uploaded by the user. The current function is called by IVIVEplotting().
#'
#' @param OEDSamples_df A data frame with chemical names and their OED samples
#' @param Q5_OED_df A data frame with chemical names and their 5th OED quantile value
#' @param y_exp The plot's y-axis label expression
#' @param title_exp The plot's title expression
#'
#' @return A ggplot2 plotting object
#' @noRd
#'
OEDSample_NoExposure_Plot <- function(OEDSamples_df,Q5_OED_df,y_exp,title_exp){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  CompoundName <- OED <- NULL

  # --- PLOT
  plt <- ggplot2::ggplot(OEDSamples_df, ggplot2::aes(x = CompoundName, y = OED)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_point(data = Q5_OED_df, ggplot2::aes(x = CompoundName, y = OED), color = 'red', size = 4)+
    ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                   plot.title = ggplot2::element_text(hjust = 0.5))
}

################################################################################
################################################################################

#' Converts a plot with a linear y-axis to a plot with a log10 y-axis scale
#'
#' @description
#' This function converts the y-axis scale on a plot object from a linear to a
#' log10 y-axis. The current function is called by IVIVEplotting() and
#' calls log10breaks().
#'
#'
#' @param plt A complete ggplot2 plotting object
#' @param pars A list of all user input parameters for the entire app
#' @param OEDSamples_df A data frame of oral equivalent dose samples for each
#' compound; The first list output from PlotPrep_df.
#' @param combined_df A data frame with OEDs and exposure estimates; The second
#' output from OEDPoint_Exposure_Plot.
#' @param OED_data A data frame of compound names and their OEDs;
#' The first output from the IVIVEsol function arranged in ascending order of
#' the OEDs
#' @param expdata A data frame of compound names and their exposure data estimates;
#' The fifth output of the IVIVEsol function
#' @param Q5_OED_df A data frame of compound names and their 5th quantile OED;
#' The second output of the Plotdf_Prep function
#'
#' @return A plot object of OEDs and exposure data (if applicable) with a log10
#' y-axis scale
#' @noRd
#'
IVIVEplot_logscale <- function(plt,pars,OEDSamples_df,combined_df,OED_data,expdata,Q5_OED_df){

  # --- Declare variables (avoids 'no visible binding for global variable in R CMD check)
  .x <- NULL

  if (pars[["returnsamples"]] == TRUE){
    if (!is.null(pars[["fileExposure"]])){
      break_seq <- log10breaks(c(OEDSamples_df$OED,Q5_OED_df$OED,expdata$Lower,expdata$Upper))
    }
    else{
      break_seq <- log10breaks(OEDSamples_df$OED)
    }
  }
  else{
    if (!is.null(pars[["fileExposure"]])){
      break_seq <- log10breaks(c(combined_df$Lower,combined_df$Upper))
    }
    else{
      break_seq <- log10breaks(OED_data$OED)
    }
  }

  plt <- plt +
    ggplot2::scale_y_log10(breaks = break_seq,
                           labels = scales::trans_format("log10", scales::math_format(10^.x)),
                           limit = c(min(break_seq),max(break_seq))) +
    ggplot2::annotation_logticks(sides = "l")

  return(plt)
}

################################################################################
################################################################################

#' Generate the title and y-axis label for the IVIVE oral equivalent dose plot
#'
#' @description
#' This function generates the title and y-axis expressions for the oral equivalent
#' dose plot in the IVIVE module. The plot's title is dependent upon the model chosen,
#' and the y-axis label is dependent upon the output concentration and tissue and
#' output units selected.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list with two elements: a title expression and a y-axis expression
#' @noRd
#'
IVIVEplot_labels <- function(pars){

  # --- SET TITLE LABEL
  title_exp <- paste("In vitro-in vivo extrapolation (IVIVE) \n from the ",
                     pars[["model"]], " model", sep = "")

  if (is.null(pars[["HondaIVIVE"]])){
    if (is.null(pars[["tissueIVIVE"]])){
      if (!is.null(pars[["fileExposure"]])){
        y_exp <- paste("OED in whole body ",
                        pars[["output_concIVIVE"]], " \n or exposure (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
      else {
        y_exp <- paste("Oral equivalent dose (OED) \n in whole body ",
                      pars[["output_concIVIVE"]], " (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
    }
    else{
      if (!is.null(pars[["fileExposure"]])){
        y_exp <- paste("OED in ", pars[["tissueIVIVE"]], " ", pars[["output_concIVIVE"]],
                     " \n or exposure (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
      else{
        y_exp <- paste("Oral equivalent dose (OED) \n in ", pars[["tissueIVIVE"]], " ", pars[["output_concIVIVE"]],
                    " (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
    }
  }
  else if (pars[["HondaIVIVE"]] == "Honda4"){
    if (is.null(pars[["tissueIVIVE"]])){

      if (!is.null(pars[["fileExposure"]])){
        y_exp <- paste("OED in liver plasma \n or exposure (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
      else {
        y_exp <- paste("OED in liver plasma (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
    }
    else{
      if (!is.null(pars[["fileExposure"]])){
        y_exp <- paste("OED in ", pars[["tissueIVIVE"]], " plasma \n or exposure (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
      else{
        y_exp <- paste("OED in ", pars[["tissueIVIVE"]], " plasma (", pars[["modelIVIVEout_units"]], ")", sep = "")
      }
    }
  }
  else{
    if (!is.null(pars[["fileExposure"]])){
      y_exp <- paste("OED in whole body plasma or exposure (", pars[["modelIVIVEout_units"]], ")", sep = "")
    }
    else {
      y_exp <- paste("OED in whole body plasma (", pars[["modelIVIVEout_units"]], ")", sep = "")
    }
  }

  # --- SET Y-AXIS LABEL

  out <- list(title_exp, y_exp)
}

################################################################################
################################################################################

#' Generate the caption for the IVIVE plot
#'
#' @description
#' This function generates the caption for the IVIVE oral equivalent dose plot
#' output. The caption differs depending on whether a single OED or a vector of
#' OEDs is returned as well as whether exposure data is available. The current
#' function is called by IVIVE_Plot_server().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A text caption for Figure 1
#' @noRd
#'
IVIVEplot_caption <- function(pars){

  if (pars[["returnsamples"]] == TRUE){
    if (!is.null(pars[["fileExposure"]])){
      paste("Figure 1: Boxplots of", pars[["samples"]], "oral equivalent dose
              (OED) samples for each selected compound (red shaded) and user-uploaded exposure estimates (purple).
              The black dots represent outliers and the red dots indicate the 5th quantile OED for
              each compound. Compounds are arranged in ascending order of their median OED value.
              Exposure estimates are shown as a distribution if more than one exposure estimate was provided
              for each compound. The purple dot represents the median exposure either uploaded by the user
              or calculated within ToCS. If the user only uploaded one exposure value for a compound, then
              the purple dot represents that value.")
    }
    else{
      paste("Figure 1: Boxplots of", pars[["samples"]], "oral equivalent dose
              (OED) samples for each selected compound. The black dots represent outliers
              and the red dots indicate the 5th quantile OED for each compound. Compounds
              are arranged in ascending order of their median OED value.")
    }
  }
  else{
    if (!is.null(pars[["fileExposure"]])){
      paste("Figure 1: Plot of the oral equivalent dose (OED) for
          each selected compound (blue) and user-uploaded exposure estimates (pink).
          Compounds are arranged in ascending order of their OED values. Exposure estimates
          are shown as a distribution if more than one exposure estimate was provided
          for each compound. The pink dot represents the median exposure either uploaded
          by the user or calculated within ToCS. If the user only uploaded one
          exposure value for a compound, then the pink dot represents that value.")
    }
    else{
      paste("Figure 1: Plot of the oral equivalent dose (OED) for each
            selected compound. Compounds are arranged in ascending order of their
            OED values.")
    }
  }
}

################################################################################
################################################################################

#' Rearrange the data frame of compounds and their OED samples by median OED
#' sample and separate the 5th quantile OED for each chemical
#'
#' @description
#' This function prepares the data frames for plotting the OED when all OED samples
#' are returned instead of a singular OED per compound. The compounds are arranged
#' in ascending order based on the median OED sample for each compound and then
#' the data frame with the 5th quantile OED is arranged in that same order.
#' The current function is called by IVIVEplotting().
#'
#'
#' @param df A data frame with chemical names and outputted OED samples from
#' IVIVEsol()
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list with two elements: the OED samples and the 5th quantile OEDs
#' arranged by median value
#' @noRd
#'
Plotdf_Prep <- function(df,pars){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  OED <- CompoundName <- NULL

  # --- EXTRACT OUT 5TH QUANTILE OED DOSE FROM SOLUTION DATA FRAME
  q5_OED <- df[1,]

  # --- EXTRACT OUT OED SAMPLE DATA FROM SOLUTION DATA FRAME
  Samples_OED <- df[3:(pars[["samples"]]+2),]
  m <- nrow(Samples_OED)
  n <- ncol(Samples_OED)
  cnames <- colnames(Samples_OED)

  # --- CREATE DATA FRAME WITH 5TH QUANTILE OED DOSE
  q5_OED_df <- data.frame(CompoundName = cnames, OED = q5_OED)
  #q5_OED_df$CompoundName <- factor(q5_OED_df$CompoundName, levels = q5_OED_df$CompoundName)

  # --- CREATE DATA FRAME WITH 2 COLUMNS TO STORE SAMPLE DATA FOR BOXPLOT
  ChemNames <- c()
  CASvalues <- c()
  OEDvalues <- c()
  chem.physical_and_invitro.data <- the$chemdata

  for (i in 1:n) {
    ChemNames <- append(ChemNames, rep(cnames[i], m))
    CASvalues <- append(CASvalues, rep(chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound == cnames[i],2], m))
    OEDvalues <- append(OEDvalues, Samples_OED[,i])
  }

  OED_Samples_df <- data.frame(CompoundName = ChemNames, CAS = CASvalues, OED = OEDvalues)
  OED_Samples_df <- stats::na.omit(OED_Samples_df)

  # --- ARRANGE LEVELS OF DATA FRAME BASED ON THE MEDIAN SAMPLE OED OF EACH COMPOUND
  OED_Samples_df_rearr <- dplyr::mutate(OED_Samples_df,
                                        CompoundName = forcats::fct_reorder(CompoundName, OED, .fun='median'))

  # --- REARRANGE 5TH OED DOSE QUANTILE DATA FOR BOXPLOT
  Plt_order <- levels(OED_Samples_df_rearr$CompoundName)
  Q5_OED_rearr <- q5_OED_df[match(Plt_order, q5_OED_df$CompoundName),]

  out <- list(OED_Samples_df_rearr, Q5_OED_rearr)
}

################################################################################
################################################################################

#' Plot the bioactivity exposure ratio (BER) for all simulated chemicals
#'
#' @description
#' This function plots the bioactivity exposure ratios (BERs) for a list of
#' chemicals. The compounds are arranged in ascending order and then plotted using
#' ggplot2. A red line is also plotted at BER = 1, indicating the threshold where
#' compounds below the red line should be prioritized for further assessment.
#' The current function is called by BER_Plot_server() and calls log10breaks().
#'
#'
#' @param BERdata A data frame of chemical names and BER values
#'
#' @return A ggplot plotting object with all BERs plotted and a red line indicating
#' the chemical prioritization cutoff
#' @noRd
#'
BERplotting <- function(BERdata){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  BER <- CompoundName <- .x <- NULL

  # --- ARRANGE BER DATA FOR PLOTTING
  BERdata <- dplyr::arrange(BERdata, BER)
  BERdata$CompoundName <- factor(BERdata$CompoundName, levels = BERdata$CompoundName)
  break_seq <- log10breaks(c(BERdata$BER,1))

  # --- PLOT SCATTER PLOT OF ALL OED VALUES
  plt <- ggplot2::ggplot(BERdata, ggplot2::aes(x = CompoundName, y = BER)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_hline(yintercept = 1, linetype = 5, linewidth = 1, color = "red") +
    ggplot2::labs(x = "Compounds",
                  y = "Bioactivity Exposure Ratio (Unitless)",
                  title = "Bioactivity Exposure Ratio (BER) for Simulated Chemicals") +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_y_log10(breaks = break_seq,
                           labels = scales::trans_format("log10", scales::math_format(10^.x)),
                           limit = c(min(break_seq,1),max(break_seq,1))) +
    ggplot2::annotation_logticks(sides = "l")


  return(plt)
}

################################################################################
################################################################################

#' Prepare the exposure data file for plotting
#'
#' @description
#' This function prepares the user uploaded exposure data file for plotting. The
#' function assures that the chemicals in the exposure data file are in the same
#' order as the original chemical list, updates the data if the model output units
#' are different than the default of mg/kg/day, and calculates the maximal exposure
#' data estimate for each compound. The updated exposure data frame is returned.
#' The current function is called by IVIVEsol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A data frame with exposure data
#' @noRd
#'
PrepExposureData <- function(pars){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  MW <- ChemicalName <- CAS <- NULL

  # --- LOAD EXPOSURE DATA
  fileExposure <- pars[["fileExposure"]]
  exposuredata <- utils::read.csv(fileExposure$datapath)

  # --- REARRANGE ROWS OF EXPOSURE DATA FILE TO BE IN SAME ORDER AS COMPOUNDS FILE
  chemdf <- the$chemdata
  df <- chemdf[chemdf$Compound %in% pars[["CompoundList"]][,1],]
  df <- df[match(pars[["CompoundList"]][,1], df$Compound),]
  exposuredata <- exposuredata[match(df$CAS, exposuredata$CAS),]

  # --- CONVERT UNITS TO UMOL/KG/DAY IF NEEDED
  if (pars[["modelIVIVEout_units"]] == "umolpkgpday"){
    df <- chemdf[chemdf$CAS %in% exposuredata$CAS,]
    df <- df[match(exposuredata$CAS,df$CAS),]
    exposuredata$MW <- df$MW
    exposuredata$Upper <- signif((1000*exposuredata$Upper)/exposuredata$MW,4)
    exposuredata$Median <- signif((1000*exposuredata$Median)/exposuredata$MW,4)
    exposuredata$Lower <- signif((1000*exposuredata$Lower)/exposuredata$MW,4)
    exposuredata <- magrittr::`%>%`(exposuredata, dplyr::select(-c(MW)))
  }

  exposuredata_trimmed <- magrittr::`%>%`(exposuredata, dplyr::select(-c(ChemicalName,CAS)))

  # --- FIND UPPER EXPOSURE ESTIMATE FOR EACH CHEMICAL
  exposuredata$maxval <- apply(exposuredata_trimmed, 1, max, na.rm=TRUE)
  names(exposuredata)[names(exposuredata) == "ChemicalName"] <- "CompoundName"
  return(exposuredata)
}

################################################################################
################################################################################

#' Calculate the oral equivalent dose and bioactivity exposure ratio when OED
#' samples are not returned
#'
#' @description
#' This function calculates the oral equivalent dose (OED) and bioactivity exposure
#' (BER) ratio when the user wants a specific quantile of OED returned. The BER
#' is calculated using the OED quantile selected by the user. If no exposure
#' data is uploaded by the user, then the BER is not calculated. The current
#' function is called by CalcOED() and calls IVIVEsol().
#'
#'
#' @param n The number of compounds being simulated
#' @param pars A list of all user input parameters for the entire app
#' @param bioactive_conc A data frame with chemical names, their CAS numbers, and
#' their bioactive concentrations
#' @param exposuredata A data frame with chemical names, their CAS numbers, and
#' an upper, median, and lower exposure estimate
#'
#' @return A list with two elements: a data frame with oral equivalent doses and
#' a data frame with bioactivity exposure ratios
#' @noRd
#'
Calc_OEDBER_RS_False <- function(n,pars,bioactive_conc,exposuredata){

  # --- CALCULATE OED
  sol <- data.frame(CompoundName = pars[["CompoundList"]][,1],
                    CAS = bioactive_conc$CAS,
                    OED = rep(0,n))
  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {
    for (i in 1:n) {
      shiny::incProgress(1/n, detail = paste("Generating the OED for chemical", i))
      sol[i,3] <- CalcOED(i,pars,bioactive_conc)
    }

  })

  # --- CALCULATE BIOACTIVITY EXPOSURE RATIO, IF APPLICABLE
  if (!is.null(pars[["fileExposure"]])){
    BER <- data.frame(CompoundName = exposuredata$CompoundName,
                      BER = signif(sol$OED/exposuredata$maxval, digits = 4))
  }
  else{
    BER <- NULL
  }
  out <- list(sol,BER)
  return(out)
}

################################################################################
################################################################################

#' Calculate the oral equivalent doses and bioactivity exposure ratio when OED
#' samples are returned
#'
#' @description
#' This function calculates an array of oral equivalent dose (OED) samples and the 5th
#' quantile OED as well a data frame of bioactivity exposure ratios (BERs). The
#' BER is calculated using the 5th quantile OED. If no exposure data is uploaded
#' by the user, then no bioactivity exposure ratios are returned. The current
#' function is called by IVIVEsol() and calls CalcOED().
#'
#'
#' @param n The number of compounds being simulated
#' @param pars A list of all user input parameters for the entire app
#' @param bioactive_conc A data frame with chemical names, their CAS numbers, and
#' their bioactive concentrations
#' @param exposuredata A data frame with chemical names, their CAS numbers, and
#' an upper, median, and lower exposure estimate
#'
#' @return A list with two elements: an array with oral equivalent doses and
#' a data frame with bioactivity exposure ratios
#' @noRd
#'
Calc_OEDBER_RS_True <- function(n,pars,bioactive_conc,exposuredata){

  # --- CREATE ARRAY WITH 1ST ROW BEING 5TH DOSE OED AND 3-END ROWS BEING SAMPLES
  sol <- array(data = rep(0,n*(pars[["samples"]]+2)),
               dim = c(pars[["samples"]]+2,n))
  dimnames(sol) <- list(c("OED_5","Samples",seq(1,pars[["samples"]])),
                        pars[["CompoundList"]][,1])

  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {

    for (i in 1:n) {

      shiny::incProgress(1/n, detail = paste("Generating the OEDs for chemical", i))

      OED <- CalcOED(i,pars,bioactive_conc)

      # --- CALCULATE 95% CSS THEN CONVERT TO BIOACTIVE CONCENTRATION (SAME PROCESS AS HTTK CODE)
      q <- stats::quantile(bioactive_conc[i,3]/OED, 0.95, na.rm=TRUE)
      sol[1,i] <- signif(bioactive_conc[i,3]/q, digits = 4)
      sol[2,i] <- NA
      sol[seq(3,pars[["samples"]]+2),i] <- OED
    }
  })

  # --- CALCULATE BIOACTIVITY EXPOSURE RATIO, IF APPLICABLE
  if (!is.null(pars[["fileExposure"]])){
    BER <- data.frame(CompoundName = exposuredata$CompoundName,
                      BER = signif(unname(sol[1,])/exposuredata$maxval,digits = 4))
  }
  else{
    BER <- NULL
  }

  out <- list(sol,BER)
  return(out)
}

################################################################################
################################################################################

#' Replace NAs in exposure data frame with values
#'
#' @description
#' This function fills in the NA values in the exposure data frame by placing a
#' small deviation (1e-8) from the non-NA elements of the data frame for that
#' particular chemical. This is done so that the exposure data can be plotted as
#' line and point ranges. The current function is called by OEDSample_Exposure_Plot()
#' and OEDPoint_Exposure_Plot().
#'
#'
#' @param exposure_df A data frame with chemical names and exposure estimates. '
#'
#' @return A data frame with chemical names and exposure estimates.
#' There should be no NA values in the outputted data frame.
#' @noRd
#'
FillExposureData <- function(exposure_df){

  # --- DETERMINE ROWS WITH MISSING DATA
  indicies <- which(is.na(exposure_df), arr.ind = TRUE)

  # --- DETERMINE ROWS MISSING BOTH MEDIAN AND LOWER VALUE (TWO VALUES)
  dup_rows <- unique(indicies[which(duplicated(indicies[,1])),1])
  for (i in dup_rows) {
    if (is.na(exposure_df$Lower[i]) && is.na(exposure_df$Median[i]) && !is.na(exposure_df$Upper[i])){
      exposure_df$Lower[i] <- exposure_df$Upper[i] - 1e-12
      exposure_df$Median[i] <- stats::median(c(exposure_df$Upper[i],exposure_df$Lower[i]))
    }
    else if (is.na(exposure_df$Lower[i]) && !is.na(exposure_df$Median[i]) && is.na(exposure_df$Upper[i])){
      exposure_df$Lower[i] <- exposure_df$Median[i] - 1e-12
      exposure_df$Upper[i] <- exposure_df$Median[i] + 1e-12
    }
    else if (!is.na(exposure_df$Lower[i]) && is.na(exposure_df$Median[i]) && is.na(exposure_df$Upper[i])){
      exposure_df$Upper[i] <- exposure_df$Lower[i] + 1e-12
      exposure_df$Median[i] <- stats::median(c(exposure_df$Upper[i],exposure_df$Lower[i]))
    }
  }

  # --- DETERMINE ROWS MISSING ONLY MEDIAN VALUE (ROWS MISSING ONE VALUE)
  nondup_rows <- unique(indicies[!(indicies[,1] %in% dup_rows),1])
  for (j in nondup_rows){
    if (is.na(exposure_df$Lower[j])){
      exposure_df$Lower[j] <- exposure_df$Median[j] - 1e-12
    }
    else if (is.na(exposure_df$Median[j])){
      exposure_df$Median[j] <- stats::median(c(exposure_df$Upper[j],exposure_df$Lower[j]))
    }
    else if (is.na(exposure_df$Upper[j])){
      exposure_df$Upper[j] <- exposure_df$Median[j] + 1e-12
    }

  }
  return(exposure_df)
}
