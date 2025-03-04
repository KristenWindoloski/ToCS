
###########################################
# --- CALCULATE OUTPUT PARAMETER VALUES
###########################################

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
      incProgress(1/n, detail = paste("Generating parameters for chemical", i))

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

CalcVdist <- function(pars,i){

  out <- httk::calc_vdist(chem.name = pars[["CompoundList"]][i,1],
             default.to.human = pars[["defaulttoHuman"]],
             species = pars[["spec"]],
             adjusted.Funbound.plasma = pars[["adj_fub"]],
             regression = pars[["regression"]],
             minimum.Funbound.plasma = pars[["min_fub"]])
}

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

CalcClearance <- function(pars,i){

  TotClear <- httk::calc_total_clearance(chem.name = pars[["CompoundList"]][i,1],
                                         species = pars[["spec"]],
                                         default.to.human = pars[["defaulttoHuman"]],
                                         restrictive.clearance = pars[["restrict_clear"]],
                                         adjusted.Funbound.plasma = pars[["adj_fub"]])
}

CalcPCs <- function(pars,i){

  out <- httk::predict_partitioning_schmitt(chem.name = pars[["CompoundList"]][i,1],
                               default.to.human = pars[["defaulttoHuman"]],
                               species = pars[["spec"]],
                               alpha = pars[["AlphaPar"]],
                               adjusted.Funbound.plasma = pars[["adj_fub"]],
                               regression = pars[["regression"]],
                               minimum.Funbound.plasma = pars[["min_fub"]])
}

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

##################################################
# --- GENERATE PLOT WITH ELIM RATE AND VDIST
##################################################


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

##################################################
# --- GENERATE PLOT WITH PARTITION COEFFICIENTS
##################################################

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

