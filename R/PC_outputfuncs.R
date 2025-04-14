
################################################################################
################################################################################

#' Calculate the elimination rate, volume of distribution, half-life, and total
#' plasma clearance of all selected compounds
#'
#' @description
#' This function is the main function that calls for the calculation of a list of
#' compounds' elimination rates, volumes of distribution, half lives, and total
#' plasma clearances. The data frames are set up in this function, and then other
#' functions are called to do the calculation for these parameters.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list consisting of a data frame with calculated elimination rates,
#' volumes of distribution, half life, and total clearance measures, a data
#' frame with calculated partition coefficients, and a data frame with simulation
#' parameters and chemical-physical data
#' @seealso [PC_server()], which calls the current function, and [CalcElimRate()],
#' [CalcVdist()], [CalcHalfLife()], [CalcClearance()], [CalcPCs()], and [StorePars_PC()],
#' which the current function calls
#' @export
#'
Parsol <- function(pars){

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
#' package's 'calc_elimination_rate' function.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the elimination rate for compound i
#' @seealso [Parsol()], which calls the current function
#' @export
#'
CalcElimRate <- function(pars,i){

  sol_elim <- httk::calc_elimination_rate(chem.name = pars[["CompoundList"]][i,1],
                                    species = pars[["spec"]],
                                    default.to.human = pars[["defaulttoHuman"]],
                                    restrictive.clearance = pars[["restrict_clear"]],
                                    adjusted.Funbound.plasma = pars[["adj_fub"]],
                                    regression = pars[["regression"]],
                                    clint.pvalue.threshold = pars[["Clint_Pval"]],
                                    minimum.Funbound.plasma = pars[["min_fub"]])
}

################################################################################
################################################################################

#' Calculate the volume of distribution of a chemical
#'
#' #' @description
#' This function calculates the volume of distribution of a compound using the
#' 'httk' package's 'calc_vdist' function.
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the volume of distribution for compound i
#' @seealso [Parsol()], which calls the current function
#' @export
#'
CalcVdist <- function(pars,i){

  out <- httk::calc_vdist(chem.name = pars[["CompoundList"]][i,1],
             default.to.human = pars[["defaulttoHuman"]],
             species = pars[["spec"]],
             adjusted.Funbound.plasma = pars[["adj_fub"]],
             regression = pars[["regression"]],
             minimum.Funbound.plasma = pars[["min_fub"]])
}

################################################################################
################################################################################

#' Calculate the half-life of a chemical
#'
#' #' @description
#' This function calculates the half-life of a compound using the 'httk' package's
#' 'calc_half_life' function.
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the half-life for compound i
#' @seealso [Parsol()], which calls the current function
#' @export
#'
CalcHalfLife <- function(pars,i){

  HL <- httk::calc_half_life(chem.name = pars[["CompoundList"]][i,1],
                             species = pars[["spec"]],
                             default.to.human = pars[["defaulttoHuman"]],
                             restrictive.clearance = pars[["restrict_clear"]],
                             adjusted.Funbound.plasma = pars[["adj_fub"]],
                             regression = pars[["regression"]],
                             clint.pvalue.threshold = pars[["Clint_Pval"]],
                             minimum.Funbound.plasma = pars[["min_fub"]])
}

################################################################################
################################################################################

#' This function calculates the total plasma clearance of a chemical
#'
#' #' @description
#' This function calculates the total plasma clearance of a compound using the
#' 'httk' package's 'calc_total_clearance' function.
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A numerical value, which is the plasma total clearance for compound i
#' @seealso [Parsol()], which calls the current function
#' @export
#'
CalcClearance <- function(pars,i){

  TotClear <- httk::calc_total_clearance(chem.name = pars[["CompoundList"]][i,1],
                                         species = pars[["spec"]],
                                         default.to.human = pars[["defaulttoHuman"]],
                                         restrictive.clearance = pars[["restrict_clear"]],
                                         adjusted.Funbound.plasma = pars[["adj_fub"]])
}

################################################################################
################################################################################

#' Calculate the partition coefficients of a chemical
#'
#' @description
#' This function calculates the tissue to unbound plasma partition coefficients
#' of a given chemical. This is done using the 'httk' package's
#' 'predict_partitioning_schmitt' function.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param i A numerical value representing the index number of the compound to
#' simulate
#'
#' @return A list, which consists of the partition coefficients for compound i
#' @seealso [Parsol()], which calls the current function
#' @export
#'
CalcPCs <- function(pars,i){

  out <- httk::predict_partitioning_schmitt(chem.name = pars[["CompoundList"]][i,1],
                               default.to.human = pars[["defaulttoHuman"]],
                               species = pars[["spec"]],
                               alpha = pars[["AlphaPar"]],
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
#' by the GUI user or already present when httk was loaded.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A data frame with simulation parameters and physical-chemical
#' data from the compounds simulated
#' @seealso [Parsol()], which calls the current function
#' @export
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
                        minimum.Funbound.plasma = pars[["min_fub"]],
                        alpha = pars[["AlphaPar"]])

  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
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
#' log10 y-axis scale, if desired by the user.
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
#' @seealso [PC_EVPlot_server()], which calls the current function, and [plot_logscale()],
#' which the current function calls
#' @export
#'
plotPar <- function(soldata,pars,logscale){

  df_elim <- dplyr::select(soldata, CompoundName, EliminationRate)
  df_elim <- dplyr::arrange(df_elim, EliminationRate)
  df_elim$CompoundName <- factor(df_elim$CompoundName, levels = df_elim$CompoundName)

  df_vdist <- dplyr::select(soldata, CompoundName, VolumeOfDistribution)
  df_vdist <- dplyr::arrange(df_vdist, VolumeOfDistribution)
  df_vdist$CompoundName <- factor(df_vdist$CompoundName, levels = df_vdist$CompoundName)

  df_halflife <- dplyr::select(soldata, CompoundName, HalfLife)
  df_halflife <- dplyr::arrange(df_halflife, HalfLife)
  df_halflife$CompoundName <- factor(df_halflife$CompoundName, levels = df_halflife$CompoundName)

  df_TotalClearance <- dplyr::select(soldata, CompoundName, TotalClearance)
  df_TotalClearance <- dplyr::arrange(df_TotalClearance, TotalClearance)
  df_TotalClearance$CompoundName <- factor(df_TotalClearance$CompoundName, levels = df_TotalClearance$CompoundName)

  plt_lst = vector('list', 3)

  plt_lst[[1]] <- ggplot2::ggplot(df_elim, ggplot2::aes(CompoundName, EliminationRate)) +
    ggplot2::geom_point(size=4) +
    ggplot2::labs(x = "Compounds", y = "Elim \n Rate (1/h)") +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  if (logscale == TRUE){
    plt_lst[[1]] <- plot_logscale(plt_lst[[1]],df_elim$EliminationRate)
  }

  plt_lst[[2]] <- ggplot2::ggplot(df_vdist, ggplot2::aes(CompoundName, VolumeOfDistribution)) +
    ggplot2::geom_point(size=4) +
    ggplot2::labs(x = "Compounds", y = "Volume of \n Distribution \n (L/kg BW)") +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  if (logscale == TRUE){
    plt_lst[[2]] <- plot_logscale(plt_lst[[2]],df_vdist$VolumeOfDistribution)
  }

  plt_lst[[3]] <- ggplot2::ggplot(df_halflife, ggplot2::aes(CompoundName, HalfLife)) +
    ggplot2::geom_point(size=4) +
    ggplot2::labs(x = "Compounds", y = "Half Life (h)") +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  if (logscale == TRUE){
    plt_lst[[3]] <- plot_logscale(plt_lst[[3]],df_halflife$HalfLife)
  }

  plt_lst[[4]] <- ggplot2::ggplot(df_TotalClearance, ggplot2::aes(CompoundName, TotalClearance)) +
    ggplot2::geom_point(size=4) +
    ggplot2::labs(x = "Compounds", y = "Total Plasma \n Clearance \n (L/h/kg BW)") +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  if (logscale == TRUE){
    plt_lst[[4]] <- plot_logscale(plt_lst[[4]],df_TotalClearance$TotalClearance)
  }

  return(plt_lst)
}

################################################################################
################################################################################

#' Generate partition coefficient plots for simulated chemicals
#'
#' @description
#' This function generates plots of partition coefficients for all simulated
#' compounds. Each plot represents a different tissue and that tissue's partition
#' coefficients are arranged in ascending order. The plots are plotted on a log10
#' y-axis scale, if desired by the user.
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
#' @seealso [PC_PCPlot_server()], which calls the current function, and
#' [plot_logscale()], which the current function calls
#' @export
#'
plotPCs <- function(soldata,pars,logscale){

  # --- Create empty list to be filled with number of plots (each plot will have multiple curves on it)
  plt_lst = vector('list', 13)
  tissuenames <- colnames(soldata[,2:14])

  # --- Generate a plot for each tissue
  for (i in 1:13) {

    # --- Create data frame for tissue i to plot all state i curves at once
    tissue_df <- data.frame(CompoundName = soldata[,1],
                            TissuePC = soldata[,i+1])
    tissue_df <- dplyr::arrange(tissue_df, TissuePC)
    tissue_df$CompoundName <- factor(tissue_df$CompoundName, levels = tissue_df$CompoundName)

    # --- Plot curves for tissue i for all compounds
    plt_lst[[i]] <- ggplot2::ggplot(tissue_df, ggplot2::aes(CompoundName, TissuePC)) +
      ggplot2::geom_point(size=2) +
      ggplot2::labs(x = "Compounds", y = tissuenames[i]) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5))
    if (logscale == TRUE){
      plt_lst[[i]] <- plot_logscale(plt_lst[[i]],tissue_df$TissuePC)
    }
  }

  return(plt_lst)
}

