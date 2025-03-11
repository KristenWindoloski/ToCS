################################################################################
################################################################################

#' Reformat the ADME solution array
#'
#' @param i A number representing the index of the current model state to be plotted
#' @param n A number representing the number of compounds being plotted
#' @param sol_array An array (row = time steps, column = model state, sheet = compound) of the model solution from other code
#' @param chemnames A vector containing the names of the compounds being simulated
#' @param numtimes A vector containing the times the solution is evaluated
#'
#' @return A data frame that reformats the sol_array so concentration-time profiles can be plotted according to compound
#' @export
#'
Create_Plotting_df <- function(i,n,sol_array,chemnames,numtimes){

  # --- Create empty vectors
  yvals = c()
  compound_name_compartment = c()

  # --- Create yvalue vector which includes the compartment data for all compounds
  for (j in 1:n) {
    yvals <- append(yvals, sol_array[,i+1,j])
    compound_name_compartment <-append(compound_name_compartment, rep(chemnames[j],numtimes))
  }

  # --- Create data frame for state i to plot all state i curves at once
  compartment_df <- data.frame(Time = sol_array[,1,1],
                               Yvalues = yvals,
                               Compound = compound_name_compartment)

  compartment_df <- dplyr::arrange(compartment_df, Compound)
  compartment_df$Compound <- factor(compartment_df$Compound, levels = unique(compartment_df$Compound))

  return(compartment_df)
}


################################################################################
################################################################################

#' Title
#'
#' @param plt_lst
#' @param n_states
#'
#' @return
#' @export
#'
Set_Plot_Legend_Color <- function(plt_lst,n_states){

  # --- Extract color pattern data frame for individual compound plots
  plt_attrib <- ggplot2::ggplot_build(plt_lst[[n_states]])
  plt_colors <- data.frame(colors = unique(plt_attrib[["data"]][[1]][["colour"]]))

  # --- Save legend to plot later
  plt_lst[[n_states+1]] <- cowplot::get_plot_component(plt_lst[[1]],"guide-box",return_all = TRUE)[[1]]

  # --- Remove legend from current plots
  for (j in 1:n_states) {
    plt_lst[[j]] <- plt_lst[[j]] + ggplot2::theme(legend.position = "none")
  }

  out <- list(plt_lst, plt_colors)
}


################################################################################
################################################################################

#' Title
#'
#' @param sol_array
#'
#' @return
#' @export
#'
plottingfunc_all <- function(sol_array){

  # --- Set needed variables; subtracted 1 from num_states because 'time' is one of the columns
  cn <- dimnames(sol_array)[2]
  col_names <- cn[[1]]
  num_states <- length(col_names) - 1
  num_compounds <- dim(sol_array)[3]
  n_times <- dim(sol_array)[1]
  dn <- dimnames(sol_array)[3]
  compound_names <- dn[[1]]

  # --- Create empty list to be filled with number of plots (each plot will have multiple curves on it)
  plt_lst = vector('list', (num_states+1))

  # --- Generate plots for each compartment
  for (i in 1:num_states) {

    compartment_df <- Create_Plotting_df(i,num_compounds,sol_array,compound_names,n_times)

    # --- Plot curves for compartment i for all compounds
    plt_lst[[i]] <- ggplot2::ggplot(compartment_df, ggplot2::aes(Time, Yvalues, col = Compound)) +
      ggplot2::geom_line(linewidth=1) +
      ggplot2::labs(x = "Time (Days)", y = col_names[i+1]) +
      ggplot2::theme_bw() +
      ggplot2::guides(col = ggplot2::guide_legend(ncol = 2))
  }

  # --- Outputs the plotting list and legend colors
  out <- Set_Plot_Legend_Color(plt_lst,num_states)
}


################################################################################
################################################################################

#' Title
#'
#' @param i
#' @param j
#' @param comp_sol
#' @param compound_names
#' @param plt_colors
#' @param col_names
#'
#' @return
#' @export
#'
Create_Individ_Subplot <- function(i,j,comp_sol,compound_names,plt_colors,col_names){

  # --- Create data frame with plotting data for compound i
  compound_compartment_df <- data.frame(Time = comp_sol[,1],
                                        Ydata = comp_sol[,j+1])

  # --- Create plot
  outplot <- ggplot2::ggplot(compound_compartment_df, ggplot2::aes(Time, Ydata, lty = compound_names[i])) +
    ggplot2::geom_line(linewidth=1, color = plt_colors[i,1]) +
    ggplot2::labs(x = "Time (Days)", y = col_names[j+1]) +
    ggplot2::theme_bw() +
    ggplot2::scale_linetype("Compound")
}


################################################################################
################################################################################

#' Title
#'
#' @param individ_plt_lst
#' @param n_states
#'
#' @return
#' @export
#'
Set_Individ_Plot <- function(individ_plt_lst,n_states){

  # --- Save legend to plot later
  individ_plt_lst[[n_states+1]] <- cowplot::get_plot_component(individ_plt_lst[[1]],"guide-box",return_all = TRUE)[[1]]

  # --- Remove legend from current plots
  for (j in 1:n_states) {
    individ_plt_lst[[j]] <- individ_plt_lst[[j]] + ggplot2::theme(legend.position = "none")
  }

  # --- Save all subplots for compound i to the ith entry of the master plot
  # --- list for all compounds
  out <- individ_plt_lst
}


################################################################################
################################################################################

#' Title
#'
#' @param sol_array
#' @param plt_colors
#'
#' @return
#' @export
#'
plottingfunc_individual <- function(sol_array, plt_colors){

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

  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {

    for (i in 1:num_compounds) {

      # --- Increment the progress bar and update detail text
      incProgress(1/num_compounds, detail = paste("Generating plot for chemical", i))

      # --- Extract model solution for compound i and create blank plot list
      comp_sol <- as.data.frame(sol_array[,,i], col.names = col_names)
      individ_plt_lst <- vector('list', num_states+1)

      # --- Fill the list with each subplot for compound i
      for (j in 1:num_states) {
        individ_plt_lst[[j]] <- Create_Individ_Subplot(i,j,comp_sol,compound_names,plt_colors,col_names)
      }

      # --- Set legend and final plot for compound i
      wholeplot_list[[i]] <- Set_Individ_Plot(individ_plt_lst,num_states)
    }
  })

  return(wholeplot_list)
}


################################################################################
################################################################################

#' Title
#'
#' @param plt_list
#'
#' @return
#' @export
#'
plt_arrange <- function(plt_list){

  num_plots <- length(plt_list)
  out_list <- list()
  for (i in 1:num_plots) {
    out_list[[i]] <- gridExtra::grid.arrange(grobs = plt_list[[i]])
  }
  return(out_list)
}


################################################################################
################################################################################

#' Title
#'
#' @param modsol
#'
#' @return
#' @export
#'
TKsummary <- function(modsol) {

  sol <- as.data.frame(modsol, stringsAsFactors = FALSE)

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

  TKsum_final <- apply(as.matrix(TKsum[,2:4]), 2, as.numeric)
}


################################################################################
################################################################################

#' Title
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return
#' @export
#'
modsol <- function(pars){

  # Get row, column, and page dimensions for arrays used to store solutions
  n <- nrow(pars[["CompoundList"]])

  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {

    for (i in 1:n) {

      # --- Increment the progress bar and update detail text
      incProgress(1/n, detail = paste("Generating the concentration-time profile for chemical", i))

      # --- Solve model for compound i
      modsolution <- Run_ADME_Model(i,pars)

      # --- Set sizes of output arrays
      if (i==1){
        arrs <- SetArraySize(modsolution,n)
        sol <- arrs[[1]]
        tk_sum_array <- arrs[[2]]
      }

      # --- Save solution in array and generate TK Summary
      sol[,,i] <- modsolution
      tk_sum_array[,,i] <- TKsummary(modsolution)
    }
  })

  # --- Assign row, column, and page names to the arrays
  arr_out <- AssignArrayNames(sol,modsolution,tk_sum_array,pars)
  sol <- arr_out[[1]]

  # --- Rearrange TK summary array to be 2D data frame
  tk_sum_array <- Rearr_TKSumArray(arr_out[[2]],pars)

  # --- Generate simulation parameters data frame
  pars_df <- StorePars_ADME(pars)

  # --- Create list with all outputs
  out_list <- list(sol,tk_sum_array,pars_df)
}


################################################################################
################################################################################

#' Solve the ADME model
#'
#' @description
#' This function solves the PBTK model to generate concentration-time profiles for
#' the list of selected compounds. These concentrations are generated using the
#' 'httk' package's 'solve_model' function for the selected model and parameters.
#'
#'
#' @param i A numerical value; the index number of the compound to simulate from
#' the user selected chemical list
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A matrix with time course data for the selected model and simulation
#' parameters. The first column has time points and the remaining columns have
#' concentrations for each model compartment, plasma concentration, and area
#' under the curve of the plasma compartment. Each row represents a concentration
#' at the listed time.
#' @export
#'
Run_ADME_Model <- function(i,pars){

  out <- httk::solve_model(chem.name = pars[["CompoundList"]][i,1],
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
                           parameterize.arg.list = list(default.to.human = pars[["defaulttoHuman"]],
                                                        regression = pars[["regression"]],
                                                        Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                                             Caco2.Fabs = pars[["caco_fabs"]],
                                                                             Caco2.Fgut = pars[["caco_fgut"]],
                                                                             overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                                             keepit100 = pars[["caco_keep100"]])))

  out <- RemoveCols(out,pars[["model"]])
}


################################################################################
################################################################################

#' Remove columns from the concentration-time profile solution matrix
#'
#' @param sol The solution matrix from httk's 'solve_model' function
#' @param model The simulated model; either "1compartment","3compartment","pbtk",
#' "fetal_pbtk"
#'
#' @return The trimmed solution matrix with less columns than sol; All columns
#' after the AUC column are removed if the model is not "fetal_pbtk" and if the
#' model is "fetal_pbtk", then all columns after Qthyroid are removed.
#' @export
#'
RemoveCols <- function(sol,model){

  if (model == "fetal_pbtk"){
    index <- which(colnames(sol) == "Qthyroid")
  }
  else {
    index <- which(colnames(sol) == "AUC")
  }
  sol <- sol[,1:index]
}


################################################################################
################################################################################

#' Title
#'
#' @param modelsol
#' @param n
#'
#' @return
#' @export
#'
#' @examples
SetArraySize <- function(modelsol,n){

  # --- Find solution size
  p <- nrow(modelsol)
  q <- ncol(modelsol)

  # --- sol is for storing the solution after its been plotted
  sol <- array(data = rep(0,p*q*n), dim = c(p,q,n)) # (row, column, page)

  # --- tk_sum_array is for storing the TK summary data (Tmax, Cmax, AUC)
  tk_sum_array <- array(data = rep(0,(q-1)*3*n), dim = c((q-1),3,n))

  out <- list(sol,tk_sum_array)
}


################################################################################
################################################################################

#' Title
#'
#' @param sol
#' @param modsol
#' @param tk_sum_array
#' @param pars A list of all user input parameters for the entire app
#'
#' @return
#' @export
#'
#' @examples
AssignArrayNames <- function(sol,modsol,tk_sum_array,pars){

  # --- Extract compound and compartment names
  columns <- colnames(modsol)
  compartmentnames <- columns[2:length(columns)]
  compoundnames <- pars[["CompoundList"]][,1]
  statnames <- c("Tmax","MaxValue","AUC")

  # --- Assign dimension names
  dimnames(sol) <- list(c(), columns, compoundnames)
  dimnames(tk_sum_array) <- list(c(compartmentnames),statnames,compoundnames)

  out <- list(sol,tk_sum_array)
}


################################################################################
################################################################################

#' Title
#'
#' @param tk_sum_array
#' @param pars A list of all user input parameters for the entire app
#'
#' @return
#' @export
#'
#' @examples
Rearr_TKSumArray <- function(tk_sum_array,pars){

  # --- Extract compound and compartment names
  compartmentnames <- rownames(tk_sum_array)
  statnames <- c("Tmax","MaxValue","AUC")
  compoundnames <- pars[["CompoundList"]][,1]

  # --- Rearrange as a matrix and assign dimension names
  combdim <- list(outer(statnames,compoundnames, paste))
  mat_colnames <- gsub(" ", ".",combdim[[1]])
  dim_tk <- dim(tk_sum_array)
  tk_sum_array <- matrix(tk_sum_array,dim_tk[1],dim_tk[2]*dim_tk[3])
  dimnames(tk_sum_array) <- list(compartmentnames,c(mat_colnames))

  return(tk_sum_array)
}


################################################################################
################################################################################

#' Title
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return
#' @export
#'
#' @examples
Dosing_Output <- function(pars){

  # --- Set values for dosing output (must have "NULL" instead of NULL)
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

  out <- list(initial.dose,doses.per.day,daily.dose,dosing.matrix)
}


################################################################################
################################################################################

#' Title
#'
#' @param pars A list of all user input parameters for the entire app
#' @param pars_df
#'
#' @return
#' @export
#'
#' @examples
Bind_Chem_Data <- function(pars,pars_df){

  # --- Combine parameter and chemical data into one data frame
  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,pars_df$chem.name)),]
  pars_df <- cbind(pars_df,chemdata)
}


################################################################################
################################################################################

#' Title
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return
#' @export
#'
#' @examples
StorePars_ADME <- function(pars){

  dosinginfo <- Dosing_Output(pars)

  # --- Extract parameter values
  pars_df <- data.frame(chem.name = pars[["CompoundList"]][,1],
                        route = pars[["doseroute"]],
                        input.units = pars[["doseunits"]],
                        dosing.initialdose = dosinginfo[[1]],
                        dosing.dosesperday = dosinginfo[[2]],
                        dosing.dailydose = dosinginfo[[3]],
                        dosing.dosing.matrix = dosinginfo[[4]],
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

  # --- Bind parameter and chemical data
  out <- Bind_Chem_Data(pars,pars_df)
}

