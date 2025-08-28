
################################################################################
################################################################################

#' Calculate the elimination rate, volume of distribution, half-life, and total
#' plasma clearance of all selected compounds
#'
#' @description
#' This function is the main function that calls for the calculation of a list of
#' compounds' elimination rates, volumes of distribution, half lives, and total
#' plasma clearances. The data frames are set up in this function, and then other
#' functions are called to do the calculation for these parameters. The current
#' function is called by PC_server() and calls CalcElimRate(), CalcVdist(),
#' CalcHalfLife(), CalcClearance(), CalcPCs(), and StorePars_PC().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list consisting of a data frame with calculated elimination rates,
#' volumes of distribution, half life, and total clearance measures, a data
#' frame with calculated partition coefficients, and a data frame with simulation
#' parameters and chemical-physical data
#' @noRd
#'
Parsol <- function(pars){

  # --- Attach the 'the' environment to add chem.physical_and_invitro.data data frame to path
  attach(the)

  # --- Detach the attached 'the' environment
  on.exit(detach(the))

  # --- Get number of compounds
  n <- nrow(pars[["CompoundList"]])

  # --- Generate empty data frames for two tables
  df1 <- data.frame(CompoundName = pars[["CompoundList"]][,1],
                    EliminationRate = rep(0,n),
                    VolumeOfDistribution = rep(0,n),
                    HalfLife = rep(0,n),
                    TotalClearance = rep(0,n))

  colnam <- c("AdiposePC","BonePC","BrainPC","GutPC","HeartPC","KidneyPC",
              "LiverPC","LungPC","MusclePC","SkinPC","SpleenPC","RbcPC","RestPC")
  df2_1 <- matrix(rep(0,n*13),ncol = 13,dimnames = list(c(),colnam))
  df2_1 < as.data.frame(df2_1)

  df2 <- data.frame(CompoundName = pars[["CompoundList"]][,1])
  df2 <- cbind(df2,df2_1)

  # --- For each compound, find all five metrics

  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {

    for (i in 1:n) {

      # --- Increment the progress bar and update detail text
      shiny::incProgress(1/n, detail = paste("Generating parameters for chemical", i))

      df1[i,2] <- CalcElimRate(pars,i)
      df1[i,3] <- CalcVdist(pars,i)
      df1[i,4] <- CalcHalfLife(pars,i)
      df1[i,5] <- CalcClearance(pars,i)
      df2[i,2:14] <- CalcPCs(pars,i)
    }
  })

  # --- Create a data frame to store all simulation parameters
  pars_df <- StorePars_PC(pars)

  # --- Store output as a list
  out_list <- list(df1,df2,pars_df)
}

################################################################################
################################################################################

#' Calculate the elimination rate of a chemical
#'
#' @description
#' This function calculates the elimination rate of a compound using the 'httk'
#' package's 'calc_elimination_rate' function. The current function is called by
#' Parsol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the elimination rate for compound i
#' @noRd
#'
CalcElimRate <- function(pars,i){

  sol_elim <- httk::calc_elimination_rate(chem.name = pars[["CompoundList"]][i,1],
                                          species = pars[["spec"]],
                                          default.to.human = pars[["defaulttoHuman"]],
                                          adjusted.Funbound.plasma = pars[["adj_fub"]],
                                          minimum.Funbound.plasma = pars[["min_fub"]],
                                          regression = pars[["regression"]],
                                          restrictive.clearance = pars[["restrict_clear"]],
                                          clint.pvalue.threshold = pars[["Clint_Pval"]],
                                          Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                               Caco2.Fabs = pars[["caco_fabs"]],
                                                               Caco2.Fgut = pars[["caco_fgut"]],
                                                               overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                               keepit100 = pars[["caco_keep100"]]))
}

################################################################################
################################################################################

#' Calculate the volume of distribution of a chemical
#'
#' #' @description
#' This function calculates the volume of distribution of a compound using the
#' 'httk' package's 'calc_vdist' function. The current function is called by
#' Parsol().
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the volume of distribution for compound i
#' @noRd
#'
CalcVdist <- function(pars,i){

  out <- httk::calc_vdist(chem.name = pars[["CompoundList"]][i,1],
                          default.to.human = pars[["defaulttoHuman"]],
                          species = pars[["spec"]],
                          adjusted.Funbound.plasma = pars[["adj_fub"]],
                          regression = pars[["regression"]],
                          minimum.Funbound.plasma = pars[["min_fub"]],
                          restrictive.clearance = pars[["restrict_clear"]],
                          clint.pvalue.threshold = pars[["Clint_Pval"]],
                          Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                               Caco2.Fabs = pars[["caco_fabs"]],
                                               Caco2.Fgut = pars[["caco_fgut"]],
                                               overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                               keepit100 = pars[["caco_keep100"]]))
}

################################################################################
################################################################################

#' Calculate the half-life of a chemical
#'
#' #' @description
#' This function calculates the half-life of a compound using the 'httk' package's
#' 'calc_half_life' function. The current function is called by
#' Parsol().
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the half-life for compound i
#' @noRd
#'
CalcHalfLife <- function(pars,i){

  HL <- httk::calc_half_life(chem.name = pars[["CompoundList"]][i,1],
                             species = pars[["spec"]],
                             default.to.human = pars[["defaulttoHuman"]],
                             adjusted.Funbound.plasma = pars[["adj_fub"]],
                             minimum.Funbound.plasma = pars[["min_fub"]],
                             regression = pars[["regression"]],
                             restrictive.clearance = pars[["restrict_clear"]],
                             clint.pvalue.threshold = pars[["Clint_Pval"]],
                             Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                  Caco2.Fabs = pars[["caco_fabs"]],
                                                  Caco2.Fgut = pars[["caco_fgut"]],
                                                  overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                  keepit100 = pars[["caco_keep100"]]))
}

################################################################################
################################################################################

#' This function calculates the total plasma clearance of a chemical
#'
#' #' @description
#' This function calculates the total plasma clearance of a compound using the
#' 'httk' package's 'calc_total_clearance' function. The current function is
#' called by Parsol().
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the plasma total clearance for compound i
#' @noRd
#'
CalcClearance <- function(pars,i){

  TotClear <- httk::calc_total_clearance(chem.name = pars[["CompoundList"]][i,1],
                                         species = pars[["spec"]],
                                         default.to.human = pars[["defaulttoHuman"]],
                                         adjusted.Funbound.plasma = pars[["adj_fub"]],
                                         minimum.Funbound.plasma = pars[["min_fub"]],
                                         regression = pars[["regression"]],
                                         restrictive.clearance = pars[["restrict_clear"]],
                                         clint.pvalue.threshold = pars[["Clint_Pval"]],
                                         Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                              Caco2.Fabs = pars[["caco_fabs"]],
                                                              Caco2.Fgut = pars[["caco_fgut"]],
                                                              overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                              keepit100 = pars[["caco_keep100"]]))
}

################################################################################
################################################################################

#' Calculate the partition coefficients of a chemical
#'
#' @description
#' This function calculates the tissue to unbound plasma partition coefficients
#' of a given chemical. This is done using the 'httk' package's
#' 'predict_partitioning_schmitt' function. The current function is called by
#' Parsol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A list, which consists of the partition coefficients for compound i
#' @noRd
#'
CalcPCs <- function(pars,i){

  out <- httk::predict_partitioning_schmitt(chem.name = pars[["CompoundList"]][i,1],
                                            default.to.human = pars[["defaulttoHuman"]],
                                            species = pars[["spec"]],
                                            adjusted.Funbound.plasma = pars[["adj_fub"]],
                                            regression = pars[["regression"]],
                                            minimum.Funbound.plasma = pars[["min_fub"]])
}

################################################################################
################################################################################

#' Create a data frame that stores all simulation parameters and physical-chemical
#' data for simulated compounds
#'
#' @description
#' This function creates a data frame that contains all simulated user-selected
#' parameters as well as chemical-physical compound data for simulated compounds.
#' Chemical-physical data is taken from the 'httk' package's
#' chem.physical_and_invitro.data data frame, where the data was either uploaded
#' by the GUI user or already present when httk was loaded. The current function
#' is called by Parsol().
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A data frame with simulation parameters and physical-chemical
#' data from the compounds simulated
#' @noRd
#'
StorePars_PC <- function(pars){

  # --- Create a data frame to store all simulation parameters
  pars_df <- data.frame(chem.name = pars[["CompoundList"]][,1],
                        species = pars[["spec"]],
                        default.to.human = pars[["defaulttoHuman"]],
                        restrictive.clearance = pars[["restrict_clear"]],
                        adjusted.Funbound.plasma = pars[["adj_fub"]],
                        regression = pars[["regression"]],
                        clint.pvalue.threshold = pars[["Clint_Pval"]],
                        minimum.Funbound.plasma = pars[["min_fub"]])

  chemdf <- the$chem.physical_and_invitro.data
  chemdata <- chemdf[chemdf$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,pars_df$chem.name)),]
  pars_df <-cbind(pars_df,chemdata)
}

################################################################################
################################################################################


#' Generate plots of calculated elimination rate, volume of distribution, half-life,
#' and total plasma clearance values for a list of chemicals
#'
#' @description
#' This function generates plots for calculated elimination rate, volume of
#' distribution, half-life, and total plasma clearance values for a list of
#' compounds. Each plot has its values arranged in ascending order and plot on a
#' log10 y-axis scale, if desired by the user. The current function is called by
#' PC_EVPlot_server() and calls plot_logscale().
#'
#'
#' @param soldata A data frame with calculated elimination rates, volumes of
#' distribution, half life, and total clearance measures for each simulated
#' compound.
#' @param pars A list of all user input parameters for the entire app
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale; either 'TRUE' for log10 y-axis or 'FALSE' for a
#' linear y-axis
#'
#' @return A list of four ggplot2 objects for plots of elimination rate, volume
#' of distribution, half life, and total plasma clearance
#' @noRd
#'
plotPar <- function(soldata,pars,logscale){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  CompoundName <- EliminationRate <- VolumeOfDistribution <- HalfLife <- TotalClearance <- Parameter <- Value <- NULL

  n <- nrow(soldata)

  colvals <- list(soldata$EliminationRate,soldata$VolumeOfDistribution,soldata$HalfLife,soldata$TotalClearance)

  plot_df <- data.frame(CompoundName = rep(soldata$CompoundName,4),
                        Value = unlist(colvals),
                        Parameter = c(rep("Elimination Rate (1/h)",n),
                                      rep("Volume of Distribution (L/kg BW)",n),
                                      rep("Half Life (h)",n),
                                      rep("Total Plasma Clearance (L/h/kg BW)",n)))

  if (logscale == TRUE){

    # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
    .x <- NULL

    break_seq_all <- log10breaks(plot_df$Value)
    break_seq1 <- log10breaks(soldata$EliminationRate)
    break_seq2 <- log10breaks(soldata$VolumeOfDistribution)
    break_seq3 <- log10breaks(soldata$HalfLife)
    break_seq4 <- log10breaks(soldata$TotalClearance)

    ParNames <- c("Elimination Rate (1/h)",
                  "Volume of Distribution (L/kg BW)",
                  "Half Life (h)",
                  "Total Plasma Clearance (L/h/kg BW)")

    min_vec <- sapply(list(break_seq1,break_seq2,break_seq3,break_seq4), min)
    max_vec <- sapply(list(break_seq1,break_seq2,break_seq3,break_seq4), max)

    df_limits = data.frame(Value = c(min_vec,max_vec),
                           Parameter = rep(ParNames,2))

    out <- ggplot2::ggplot() +
      ggplot2::geom_blank(ggplot2::aes(y = Value), data = df_limits) +
      ggplot2::geom_point(data = plot_df, ggplot2::aes(x = tidytext::reorder_within(CompoundName,Value,Parameter),
                                                y = Value,
                                                color = CompoundName),
                          size = 4) +
      ggplot2::labs(x = "Compound", y = "Parameter Output") +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 15),
                     axis.text = ggplot2::element_text(size = 15),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                     axis.title = ggplot2::element_text(size = 15),
                     legend.title = ggplot2::element_text(size = 15),
                     legend.text = ggplot2::element_text(size = 15)) +
      ggplot2::scale_y_log10(breaks = break_seq_all,
                             labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      ggplot2::annotation_logticks(sides = "l") +
      ggplot2::facet_wrap(~Parameter, scales = "free") +
      tidytext::scale_x_reordered()
  }
  else{
    # --- Plot curves for compartment i for all compounds
    out <- ggplot2::ggplot(plot_df, ggplot2::aes(x = tidytext::reorder_within(CompoundName,Value,Parameter),
                                                 y = Value,
                                                 color = CompoundName)) +
      ggplot2::geom_point(size = 4) +
      ggplot2::labs(x = "Compound", y = "Parameter Output") +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 15),
                     axis.text = ggplot2::element_text(size = 15),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                     axis.title = ggplot2::element_text(size = 15),
                     legend.title = ggplot2::element_text(size = 15),
                     legend.text = ggplot2::element_text(size = 15)) +
      ggplot2::facet_wrap(~Parameter, scales = "free") +
      tidytext::scale_x_reordered()
  }

  return(out)
}

################################################################################
################################################################################

#' Generate partition coefficient plots for simulated chemicals
#'
#' @description
#' This function generates plots of partition coefficients for all simulated
#' compounds. Each plot represents a different tissue and that tissue's partition
#' coefficients are arranged in ascending order. The plots are plotted on a log10
#' y-axis scale, if desired by the user. The current function is called by
#' PC_PCPlot_server() and calls plot_logscale().
#'
#'
#' @param soldata A data frame with calculated partition coefficients for each
#' simulated compound.
#' @param pars A list of all user input parameters for the entire app
#' @param logscale Checkbox input value indicating if the user wanted the y-axis
#' of plots to be a log10 scale; either 'TRUE' for log10 y-axis or 'FALSE' for a
#' linear y-axis
#'
#' @return A list of four ggplot2 objects for plots of elimination rate, volume
#' of distribution, half life, and total plasma clearance
#' @noRd
#'
plotPCs <- function(soldata,pars,logscale){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  CompoundName <- Parameter <- Value <- NULL

  # --- Create empty list to be filled with number of plots (each plot will have multiple curves on it)
  n <- nrow(soldata)
  plot_df <- data.frame(CompoundName = rep(soldata$CompoundName,13),
                        Value = unlist(dplyr::select(soldata, -c(CompoundName))),
                        Parameter = c(rep("Adipose",n),rep("Bone",n),rep("Brain",n),
                                      rep("Gut",n),rep("Heart",n),rep("Kidney",n),
                                      rep("Liver",n),rep("Lung",n),rep("Muscle",n),
                                      rep("Skin",n),rep("Spleen",n),rep("Red Blood Cells",n),
                                      rep("Rest",n)))

  if (logscale == TRUE){

    # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
    .x <- NULL

    break_seq_all <- log10breaks(plot_df$Value)
    min_vec <- c()
    max_vec <- c()

    for (i in 2:14) {
      break_seq_ind <- log10breaks(soldata[,i])
      min_vec <- append(min_vec,min(break_seq_ind))
      max_vec <- append(max_vec,max(break_seq_ind))
    }

    ParNames <- c("Adipose","Bone","Brain","Gut","Heart","Kidney","Liver","Lung",
                  "Muscle","Skin","Spleen","Red Blood Cells","Rest")

    df_limits = data.frame(Value = c(min_vec,max_vec),
                           Parameter = rep(ParNames,2))

    out <- ggplot2::ggplot() +
      ggplot2::geom_blank(ggplot2::aes(y = Value), data = df_limits) +
      ggplot2::geom_point(data = plot_df, ggplot2::aes(x = tidytext::reorder_within(CompoundName,Value,Parameter),
                                                       y = Value,
                                                       color = CompoundName),
                          size = 4) +
      ggplot2::labs(x = "Compound", y = "Partition Coefficient (Unitless)") +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 15),
                     axis.text = ggplot2::element_text(size = 15),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                     axis.title = ggplot2::element_text(size = 15),
                     legend.title = ggplot2::element_text(size = 15),
                     legend.text = ggplot2::element_text(size = 15)) +
      ggplot2::scale_y_log10(breaks = break_seq_all,
                             labels = scales::trans_format("log10", scales::math_format(10^.x))) +
      ggplot2::annotation_logticks(sides = "l") +
      ggplot2::facet_wrap(~Parameter, scales = "free") +
      tidytext::scale_x_reordered()
  }
  else{
    # --- Plot curves for compartment i for all compounds
    out <- ggplot2::ggplot(plot_df, ggplot2::aes(x = tidytext::reorder_within(CompoundName,Value,Parameter),
                                                 y = Value,
                                                 color = CompoundName)) +
      ggplot2::geom_point(size = 4) +
      ggplot2::labs(x = "Compound", y = "Partition Coefficient (Unitless)") +
      ggplot2::theme_bw() +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 15),
                     axis.text = ggplot2::element_text(size = 15),
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                     axis.title = ggplot2::element_text(size = 15),
                     legend.title = ggplot2::element_text(size = 15),
                     legend.text = ggplot2::element_text(size = 15)) +
      ggplot2::facet_wrap(~Parameter, scales = "free") +
      tidytext::scale_x_reordered()
  }

  return(out)
}

