
######################################################
# --- DETERMINE LOG BREAKS IN ADME PLOTS
######################################################

log10breaks_ADME <- function(ydata) {

  x <- ydata[ydata > 0]

  bottom <- floor(log10(min(x)))
  top <- ceiling(log10(max(x)))

  10^(seq(bottom, top, 2))
}

######################################################
# --- PLOT ALL ADME COMPOUND CURVES ON ONE PLOT
######################################################

plottingfunc_all <- function(sol_array, logscale){

  # --- Set needed variables; subtracted 1 from num_states because 'time' is one of the columns
  cn <- dimnames(sol_array)[2]
  col_names <- cn[[1]]
  num_states <- length(col_names) - 1
  num_compounds <-dim(sol_array)[3]
  n_times <- dim(sol_array)[1]
  dn <- dimnames(sol_array)[3]
  compound_names <- dn[[1]]

  # --- Create empty list to be filled with number of plots (each plot will have multiple curves on it)
  plt_lst = vector('list', num_states + 1)

  # --- Generate plots for each compartment
  for (i in 1:num_states) {

    # --- Create empty vectors
    yvals = c()
    compound_name_compartment = c()

    # --- Create yvalue vector which includes the compartment data for all compounds
    for (j in 1:num_compounds) {
      yvals <- append(yvals, sol_array[,i+1,j])
      compound_name_compartment <-append(compound_name_compartment, rep(compound_names[j],n_times))
    }

    # --- Create data frame for state i to plot all state i curves at once
    compartment_df <- data.frame(Time = sol_array[,1,1],
                                 Yvalues = yvals,
                                 Compound = compound_name_compartment)

    compartment_df <- dplyr::arrange(compartment_df, Compound)
    compartment_df$Compound <- factor(compartment_df$Compound, levels = unique(compartment_df$Compound))

    # --- Plot curves for compartment i for all compounds
    plt_lst[[i]] <- ggplot2::ggplot(compartment_df, ggplot2::aes(Time, Yvalues, col = Compound)) +
      ggplot2::geom_line(linewidth=1) +
      ggplot2::labs(x = "Time (Days)", y = col_names[i+1]) +
      ggplot2::theme_bw() +
      ggplot2::guides(col = ggplot2::guide_legend(ncol = 2))

    if (logscale == TRUE){
      break_seq <- log10breaks_ADME(compartment_df$Yvalues)
      plt_lst[[i]] <- plt_lst[[i]] +
        ggplot2::scale_y_log10(breaks = break_seq,
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        ggplot2::annotation_logticks(sides = "l")
    }
  }

  # --- Extract color pattern data frame for individual compound plots
  plt_attrib <- ggplot2::ggplot_build(plt_lst[[num_states]])
  plt_colors <- data.frame(colors = unique(plt_attrib[["data"]][[1]][["colour"]]))

  # --- Save legend to plot later
  # plt_lst[[num_states+1]] <- get_legend(plt_lst[[1]])
  plt_lst[[num_states+1]] <- cowplot::get_plot_component(plt_lst[[1]],"guide-box",return_all = TRUE)[[1]]

  # --- Remove legend from current plots
  for (j in 1:num_states) {
    plt_lst[[j]] <- plt_lst[[j]] + ggplot2::theme(legend.position = "none")
  }

  out_list <- list(plt_lst, plt_colors)

  return(out_list)
}

########################################################
# --- PLOT ALL ADME COMPOUND CURVES ON INDIVIDUAL PLOTS
########################################################

plottingfunc_individual <- function(sol_array, plt_colors, logscale){

  # --- Set needed variables; subtracted 1 from num_states because 'time' is
  # --- one of the columns
  cn <- dimnames(sol_array)[2]
  col_names <- cn[[1]]
  num_states <- length(col_names) - 1
  num_compounds <-dim(sol_array)[3]
  n_times <- dim(sol_array)[1]
  dn <- dimnames(sol_array)[3]
  compound_names <- dn[[1]]

  # --- Create a list where each entry corresponds to one compound
  # --- and each entry will be a list containing the subplots for that compound
  wholeplot_list <- vector('list', num_compounds)

  # --- Generates 1 plot for each compound. Each plot contains a subplot for
  # --- each of the model compartments outputted.
  for (i in 1:num_compounds) {

    # --- Extract model solution for compound i and create blank plot list
    comp_sol <- as.data.frame(sol_array[,,i], col.names = col_names)
    individ_plt_lst <- vector('list', num_states+1)

    # --- Fill the list with each subplot for compound i
    for (j in 1:num_states) {

      compound_compartment_df <- data.frame(Time = comp_sol[,1],
                                            Ydata = comp_sol[,j+1])

      individ_plt_lst[[j]] <- ggplot2::ggplot(compound_compartment_df, ggplot2::aes(Time, Ydata, lty = compound_names[i])) +
        ggplot2::geom_line(linewidth=1, color = plt_colors[i,1]) +
        ggplot2::labs(x = "Time (Days)", y = col_names[j+1]) +
        ggplot2::theme_bw() +
        ggplot2::scale_linetype("Compound")

      # --- change y-axis to log10 scale if wanted
      if (logscale == TRUE){
        break_seq <- log10breaks_ADME(compound_compartment_df$Ydata)
        individ_plt_lst[[j]] <- individ_plt_lst[[j]] +
          ggplot2::scale_y_log10(breaks = break_seq,
                        labels = scales::trans_format("log10", scales::math_format(10^.x))) +
          ggplot2::annotation_logticks(sides = "l")
      }
    }

    # --- Save legend to plot later
    individ_plt_lst[[num_states+1]] <- cowplot::get_plot_component(individ_plt_lst[[1]],"guide-box",return_all = TRUE)[[1]]

    # --- Remove legend from current plots
    for (j in 1:num_states) {
      individ_plt_lst[[j]] <- individ_plt_lst[[j]] + ggplot2::theme(legend.position = "none")
    }

    # --- Save all subplots for compound i to the ith entry of the master plot
    # --- list for all compounds
    wholeplot_list[[i]] <- individ_plt_lst
  }

  return(wholeplot_list)
}

########################################################
# --- ARRANGES PLOTS ON THE SAME GRID
########################################################

plt_arrange <- function(plt_list){

  num_plots <- length(plt_list)
  out_list <- list()
  for (i in 1:num_plots) {
    out_list[[i]] <- gridExtra::grid.arrange(grobs = plt_list[[i]])
  }
  return(out_list)
}

########################################################
# --- GENERATE A TABLE WITH TK SUMMARY STATISTICS
########################################################

TKsummary <- function(sol) {

  # --- Determine the number of states to be plotted (subtracted 1 because 'time' is one of the columns)
  col_names <- colnames(sol)
  numstates <- length(col_names) - 1

  # --- Create empty data frame to then enter TK parameters
  TKsum <- data.frame(Compartment = col_names[2:(numstates+1)],
                      Tmax = rep('NA',numstates),
                      MaxValue = rep('NA', numstates),
                      AUC = rep('NA', numstates))

  # --- Determine data frame entries (TK parameters for specific compound)
  for (i in 1:numstates) {

    # --- Determine index where the max occurs
    max_index <- which.max(sol[,(i+1)])

    # --- Set Tmax value, the Maximum value (Amax or Cmax depending on compartment), and AUC values for each compartment
    TKsum[i,2] <- signif(sol[max_index,1],4)
    TKsum[i,3] <- signif(max(sol[,(i+1)]),4)
    TKsum[i,4] <- signif(DescTools::AUC(x = sol[,'time'], y = sol[,col_names[i+1]], method = "trapezoid"),4)
  }
  return(TKsum)
}

########################################################
# --- SOLVE MODEL FOR ADME SOLUTION AND TK SUMMARY
########################################################

modsol <- function(pars){

  # Get row, column, and page dimensions for arrays used to store solutions
  n <- nrow(pars[["CompoundList"]])

  # --- Solve model for each compound
  for (i in (1:n)) {

    modsolution <- httk::solve_model(chem.name = pars[["CompoundList"]][i,1],
                                     route = pars[["doseroute"]],
                                     input.units = pars[["doseunits"]],
                                     dosing = pars[["dosinginfo"]],
                                     species = pars[["spec"]],
                                     model = pars[["model"]],
                                     initial.values = pars[["initvals"]],
                                     times = pars[["returntimes"]],
                                     days = pars[["simtime"]],
                                     method = pars[["odemethod"]],
                                     tsteps = pars[["solversteps"]],
                                     rtol = pars[["rtol"]],
                                     atol = pars[["atol"]],
                                     recalc.blood2plasma = pars[["rb2p"]],
                                     restrictive.clearance = pars[["restrict_clear"]],
                                     adjusted.Funbound.plasma = pars[["adj_fub"]],
                                     minimum.Funbound.plasma = pars[["min_fub"]],
                                     suppress.messages = TRUE,
                                     parameterize.arg.list = list(default.to.human = pars[["defaulttoHuman"]],
                                                                  regression = pars[["regression"]],
                                                                  Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                                                       Caco2.Fabs = pars[["caco_fabs"]],
                                                                                       Caco2.Fgut = pars[["caco_fgut"]],
                                                                                       overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                                                       keepit100 = pars[["caco_keep100"]])))


    #--- Set sizes of arrays
    if (i==1){
      p <- nrow(modsolution)
      q <- ncol(modsolution)

      # --- sol is for storing the solution after its been plotted
      sol <- array(data = rep(0,p*q*n), dim = c(p,q,n)) # (row, column, page)

      # --- tk_sum_array is for storing the TK summary data (Tmax, Cmax, AUC)
      tk_sum_array <- array(data = rep(0,(q-1)*3*n), dim = c((q-1),3,n))
    }

    # --- Save solution in array
    sol[,,i] <- modsolution

    # --- Generate TK Summary (needs to be a data frame) and then save as an array
    df_sol <- as.data.frame(modsolution, stringsAsFactors = FALSE)
    df_tksum <- TKsummary(df_sol)
    tk_sum_array[,,i] <- apply(as.matrix(df_tksum[,2:4]), 2, as.numeric)

  }
  # --- Assign row, column, and page names to the arrays
  columns <- colnames(modsolution)
  compartmentnames <- columns[2:length(columns)]
  compoundnames <- pars[["CompoundList"]][,1]
  dimnames(sol) <- list(c(), columns, compoundnames)
  statnames <- c("Tmax","MaxValue","AUC")
  dimnames(tk_sum_array) <- list(c(compartmentnames),statnames,compoundnames)

  # --- Rearrange TK summary array to be 2D data frame
  combdim <- list(outer(statnames,compoundnames, paste))
  mat_colnames <- gsub(" ", ".",combdim[[1]])
  dim_tk <- dim(tk_sum_array)
  tk_sum_array <- matrix(tk_sum_array,dim_tk[1],dim_tk[2]*dim_tk[3])
  dimnames(tk_sum_array) <- list(compartmentnames,mat_colnames)


  if (!is.null(pars[["dosinginfo"]]$initial.dose)){
    initial.dose <- pars[["dosinginfo"]]$initial.dose
    doses.per.day <- "NULL"
    daily.dose <- "NULL"
    dosing.matrix <- "NULL"
  }
  else if (!is.null(pars[["dosinginfo"]]$doses.per.day)){
    initial.dose <- "NULL"
    doses.per.day <- pars[["dosinginfo"]]$doses.per.day
    daily.dose <- pars[["dosinginfo"]]$daily.dose
    dosing.matrix <- "NULL"
  }
  else {
    initial.dose <- "NULL"
    doses.per.day <- "NULL"
    daily.dose <- "NULL"
    dosing.matrix <- paste(pars[["dosinginfo"]]$dosing.matrix, collapse = ",")
  }

  # --- Extract parameter values
  pars_df <- data.frame(chem.name = pars[["CompoundList"]][,1],
                        route = pars[["doseroute"]],
                        input.units = pars[["doseunits"]],
                        dosing.initialdose = initial.dose,
                        dosing.dosesperday = doses.per.day,
                        dosing.dailydose = daily.dose,
                        dosing.dosing.matrix = dosing.matrix,
                        species = pars[["spec"]],
                        model = pars[["model"]],
                        initial.values = paste(pars[["initvals"]],collapse = ","),
                        times = paste(pars[["returntimes"]],collapse = ","),
                        days = pars[["simtime"]],
                        method = pars[["odemethod"]],
                        tsteps = pars[["solversteps"]],
                        rtol = pars[["rtol"]],
                        atol = pars[["atol"]],
                        recalc.blood2plasma = pars[["rb2p"]],
                        restrictive.clearance = pars[["restrict_clear"]],
                        adjusted.Funbound.plasma = pars[["adj_fub"]],
                        minimum.Funbound.plasma = pars[["min_fub"]],
                        default.to.human = pars[["defaulttoHuman"]],
                        regression = pars[["regression"]],
                        Caco2.Pab.default = pars[["caco2default"]],
                        Caco2.Fabs = pars[["caco_fabs"]],
                        Caco2.Fgut = pars[["caco_fgut"]],
                        overwrite.invivo = pars[["caco_overwriteinvivo"]],
                        keepit100 = pars[["caco_keep100"]])

  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,pars_df$chem.name)),]

  pars_df <-cbind(pars_df,chemdata)

  # --- Create list with all outputs
  out_list <- list(sol,tk_sum_array,pars_df)

  return(out_list)
}

