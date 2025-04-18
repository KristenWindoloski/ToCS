################################################################################
################################################################################

#' Reformat the ADME solution array for a specific compartment
#'
#' @description
#' This function rearranges the concentration-time profile solution array (where
#' each page represents a compound, each column a model compartment, and each
#' row a time point) into a data frame with three columns (time, yvalues, and
#' compound name) for the ith model compartment.
#'
#'
#' @param i A number representing the index of the current model state to be plotted
#' @param n A number representing the number of compounds being plotted
#' @param sol_array An array (row = time steps, column = model state, sheet = compound) of the model solution from other code
#' @param chemnames A vector containing the names of the compounds being simulated
#' @param numtimes A vector containing the times the solution is evaluated
#'
#' @return A data frame with concentration-time profile data for compartment
#' i and it contains three columns: time, yvalues, and compound.
#' @seealso [plottingfunc_all()], which calls this function
#' @export
#'
Create_Plotting_df <- function(i,n,sol_array,chemnames,numtimes){

  # --- Declare variables (avoids 'no visible binding for global variable in R CMD check)
  Compound <- NULL

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

#' Remove plot legends and determine plot color
#'
#' @description
#' This function extracts the line color legend from a plot, transfers that legend
#' to a new panel on the plot, and then removes all the other legends from the
#' plot so there is only one legend on the entire plot.
#'
#'
#' @param plt_lst A list of ggplot2 plotting objects. Each list element is
#' a ggplot2 plotting object of a single compound and is composed of subplots
#' @param n_states Number of model compartments/outputs for the simulated model
#' from httk's 'solve_model' function.
#'
#' @return A list containing two elements. The first one is the inputted plot
#' list with the legends on each compound plot removed and a new subplot with the
#' plotting legend. The second element is a data frame of hexadecimal RGB triplet
#' colors that defines the plotting color for each compound.
#' @seealso [plottingfunc_all()], which calls this function
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

#' Generate the ggplot2 plotting object for the multi-curve plot
#'
#' @description
#' This function creates plots (number of plots = number of model compartments
#' outputted from httk's solve_model function) that display the concentration-
#' time profile curves for all simulated compounds within each model compartment.
#' Plots are created using ggplot from the ggplot2 R package. A list of plots are
#' returned (one plot per compartment plus a plot displaying the plot legend).
#'
#'
#' @param sol_array The concentration-time profile solution array where entries
#' signify the concentration of a compound at a specific time in a particular
#' compartment. Pages of the array correspond to compounds, rows to time points,
#' and columns to model compartments and outputs.
#'
#' @return A list containing two elements, the first one being a plot
#' list with each element of the list representing a different compound (no plot
#' legends) and the second element being a data frame of hexadecimal RGB
#' triplet colors that defines the plotting color for each compound
#' @seealso [Create_Plotting_df()] and [Set_Plot_Legend_Color()], which the
#' current function calls. [ADME_MultPlt_server()] and [ADME_IndPlt_server], which
#' call the current function.
#' @export
#'
plottingfunc_all <- function(sol_array){

  # --- Declare variables (avoids 'no visible binding for global variable' note in R CMD check)
  Time <- Yvalues <- Compound <- NULL

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

#' Generate a single concentration-time profile curve using ggplot2
#'
#' @description
#' This function generates a ggplot plotting object that displays the concentration-time
#' profile curve for the ith compound in the jth model compartment outputted
#' from httk's solve_model function.
#'
#'
#' @param i Index corresponding to the ith compound simulated
#' @param j Index corresponding to the jth model compartment
#' @param comp_sol A data frame of the matrix solution of concentration-time
#' profiles for compound i; same format as the output of the 'httk' package's
#' solve_model function
#' @param compound_names A vector of all compound names simulated in the order of
#' simulation
#' @param plt_colors A data frame of colors where each entry is a hexadecimal RGB
#' triplet for one compound
#' @param col_names A vector of all outputted model compartments, which should be
#' the same as the columns from comp_sol
#'
#' @return A ggplot2 plotting object with a concentration-time profile curve in
#' compartment j for compound i.
#' @seealso [plottingfunc_individual()], which calls the current function
#' @export
#'
Create_Individ_Subplot <- function(i,j,comp_sol,compound_names,plt_colors,col_names){

  # --- Declare variables (avoids 'no visible binding for global variable in R CMD check)
  Time <- Ydata <- NULL

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

#' Remove current plot legend and generate individual plot legends
#'
#' @description
#' This function extracts the plotting legend from the individ_plt_lst, creates
#' a new plotting object with just the legend, and then removes all the legends
#' from the remaining plot.
#'
#'
#' @param individ_plt_lst A list of ggplot2 plotting objects where each list element is
#' the plot of a single compound and is composed of subplots
#' @param n_states Number of model compartments/outputs
#'
#' @return A list containing the inputted plot list with the legends on each
#' compound plot removed and a new subplot with the plotting legend
#' @seealso [plottingfunc_individual()], which calls the current function
#' @export
#'
Set_Individ_Plot <- function(individ_plt_lst,n_states){

  # --- Save legend to plot later
  individ_plt_lst[[n_states+1]] <- cowplot::get_plot_component(individ_plt_lst[[1]],"guide-box",return_all = TRUE)[[1]]

  # --- Remove legend to current plots
  for (j in 1:n_states) {
    individ_plt_lst[[j]] <- individ_plt_lst[[j]] + ggplot2::theme(legend.position = "none")
  }

  # --- Save all subplots for compound i to the ith entry of the master plot
  # --- list for all compounds
  out <- individ_plt_lst
}


################################################################################
################################################################################

#' Generate the plots for the individual plots drop down
#'
#' @description
#' This function creates N plots, where N is the number of compounds simulated in
#' the concentration-time profile simulations. The plot list for each compound
#' consists of an individual plotting object for each model compartment.
#'
#'
#' @param sol_array The concentration-time profile solution array where entries
#' signify the concentration of a compound at a specific time in a particular
#' compartment. Pages of the array correspond to compounds, rows to time points,
#' and columns to model compartments and outputs.
#' @param plt_colors A data frame of colors where each entry is a hexadecimal RGB
#' triplet for one compound
#'
#' @return A list that contains other lists. Each list element makes up one full
#' plotting figure for one compound (i.e. a plot with subplots).
#' @seealso [Create_Individ_Subplot()] and [Set_Individ_Plot()], which the current
#' function calls. [ADME_IndPlt_server()], which calls the current function.
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
      shiny::incProgress(1/num_compounds, detail = paste("Generating plot for chemical", i))

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

#' Arrange individual compound plots
#'
#' @description
#' This function takes the list of plots for a single compound and arranges them
#' into one plot. The single final plot is then stored in a list to be rendered
#' when called in the module output.
#'
#'
#' @param plt_list The list of individual plots (ggplot2 objects) to output, where
#' each list object is the main plotting object for one compound. Each compound's
#' plotting object is itself a list with elements being a subplot (ggplot2 object)
#' of the compound's concentration-time profile within a specifc model
#' compartment
#'
#' @return A list of arranged individual concentration-time profile plots
#' @seealso [ADME_IndPlt_server()], which calls the current function.
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

#' Generate a toxicokinetic (TK) summary of each compound
#'
#' @description
#' This function generates a matrix of a TK summary consisting of Tmax (time to
#' maximal concentration), Cmax (maximal concentration), and AUC (area under the
#' curve) for each compound in all model output compartments. AUC is calculated
#' using the AUC function from the 'DescTools' R package with the trapezoid method
#'
#'
#' @param modsol The concentrations-time profile matrix resulting from solve_model,
#' where rows represent a time step and columns represent a model compartment
#'
#' @return A matrix of TK summary values with three columns (Tmax, Cmax, and AUC)
#' and the number of rows equal to the number of model output compartments (number
#' of columns of modelsol)
#' @seealso [modsol()], which calls the current function
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

#' Main function for the concentration-time profile output
#'
#' @description
#' This function is the main function responsible for generating the output for
#' the concentration-time profiles module. This function calls the Run_ADME_Model
#' function as well as the TKsummary function to actually calculate the
#' concentration-time profiles and toxicokinetic summaries, but it then formats
#' those and other outputs to return to the main module server to distribute.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list with three elements containing an array of the concentration-time
#' profile solution, a matrix of the toxicokinetic summary, and a data frame of
#' simulation parameters and physical-chemical data for compounds used in the
#' simulation
#' @seealso [Run_ADME_Model()]
#' [SetArraySize()]
#' [TKsummary()]
#' [AssignArrayNames()]
#' [Rearr_TKSumArray()]
#' [StorePars_ADME()], all of which the current function calls.
#' [ADME_server()], which calls the current function.
#'
#' @export
#'
modsol <- function(pars){

  # Get row, column, and page dimensions for arrays used to store solutions
  n <- nrow(pars[["CompoundList"]])

  shiny::withProgress(message = "Computation in progress. Please wait.", value = 0, {

    for (i in 1:n) {

      # --- Increment the progress bar and update detail text
      shiny::incProgress(1/n, detail = paste("Generating the concentration-time profile for chemical", i))

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
#' This function solves a model to generate concentration-time profiles for
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
#' @seealso [modsol()], which this function is called by and [RemoveCols()], which
#' the current function calls
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
#' @description
#' This function cuts off columns that are considered 'extra' from the
#' concentration-time profile solution matrix outputted from httk's solve_model
#' function. This includes columns that are essentially 'repeat' quantities (i.e.
#' removes a column specifying the amount of a chemical in compartment i when
#' the concentration of the chemical in compartment i is already outputted).
#'
#'
#' @param sol The solution matrix from httk's 'solve_model' function
#' @param model The simulated model; either "1compartment","3compartment","pbtk",
#' "fetal_pbtk"
#'
#' @return The trimmed solution matrix with less columns than sol; All columns
#' after the AUC column are removed if the model is not "fetal_pbtk" and if the
#' model is "fetal_pbtk", then all columns after Qthyroid are removed.
#' @seealso [Run_ADME_Model()], which calls the current function
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

#' Set the dimensions of the arrays for the model solution and the toxicokinetic
#' summary data
#'
#' @description
#' This function creates the arrays (filled with zeros) that will be used to store
#' the outputted model solution from httk's solve_model and outputted toxicokinetic
#' summary information.
#'
#'
#' @param modelsol The matrix resulting from solve_model, where rows represent
#' a time step and columns represent a model compartment
#' @param n The number of compounds to be simulated
#'
#' @return A list of two arrays, one for the model solution and the other
#' for the toxicokinetic summary data
#' @seealso [modsol()], which calls the current function
#' @export
#'
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

#' Assign dimension names to the model solution and toxicokinetic (TK) summary
#' arrays
#'
#' @description
#' This function names the rows, columns, and pages of the model solution and TK
#' summary arrays
#'
#'
#' @param sol The solution array for all compounds; each page of the array is a
#' matrix that's the concentration-time solution from solve_model for a particular
#' compound
#' @param modsol The matrix resulting from solve_model, where rows represent
#' a time step and columns represent a model compartment
#' @param tk_sum_array The array of TK summary data where each page of the array
#' represents the TK summary of a particular compound with rows representing
#' each model compartment and columns being Tmax, Cmax, and AUC
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list of two arrays, the model solution array and the TK summary array
#' @seealso [modsol()], which calls the current function
#' @export
#'
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

#' Rearrange the toxicokinetic (TK) array into a TK matrix
#'
#' @description
#' This function changes the inputted toxicokinetic summary array into a matrix
#' and assigns names to the matrix's rows (compartment names) and columns
#' (tkstatistic.compoundname).
#'
#'
#' @param tk_sum_array The array of TK summary data where each page of the array
#' represents the TK summary of a particular compound with rows representing
#' each model compartment and columns being Tmax, Cmax, and AUC
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A matrix with rows representing model compartments and 3n columns (where
#' n is the compounds of simulated compounds) with each compound having a Tmax,
#' Cmax, and AUC column
#' @seealso [modsol()], which calls the current function
#' @export
#'
Rearr_TKSumArray <- function(tk_sum_array,pars){

  # --- Extract compound and compartment names
  compartmentnames <- rownames(tk_sum_array)
  statnames <- c("Tmax","MaxValue","AUC")
  compoundnames <- pars[["CompoundList"]][,1]

  # --- Rearrange as a matrix and assign dimension names
  combdim <- list(outer(statnames,compoundnames, paste))
  mat_colnames <- gsub(" ", ".",combdim[[1]])
  dim_tk <- dim(tk_sum_array)
  tk_sum_mat <- matrix(tk_sum_array,dim_tk[1],dim_tk[2]*dim_tk[3])
  dimnames(tk_sum_mat) <- list(compartmentnames,c(mat_colnames))

  return(tk_sum_mat)
}


################################################################################
################################################################################

#' Turn dosing parameters into a format that can be outputted by a list in the
#' simulation parameters data frame
#'
#' @description
#' This function converts the user-selected dosing parameters into an acceptably
#' formatted list for export by the user if they want to download their simulation
#' parameters.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A list of dosing parameters (initial.dose, doses.per.day, daily.dose,
#' and dosing.matrix)
#' @seealso [StorePars_ADME()], which calls the current function
#' @export
#'
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

#' Combine simulation parameters and chemical-physical compound data data frames
#'
#' @description
#' This function adds the columns of the chemical-physical compound data (found
#' in the chem.physical_and_invitro.data data frame from the httk R package) for
#' simulated compounds to the simulation parameters data frame. Thus, each row
#' of the outputted data frame shows all simulation parameters and chemical data
#' used to simulate a single compound in ToCS.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#' @param pars_df A data frame with simulation parameters generated in the
#' StorePars_ADME() function, where each row represents one compound.
#'
#' @return A data frame with all relevant simulation parameters chosen by the user
#' and all physical-chemical data used for the simulated compounds
#' @seealso [StorePars_ADME()], which calls the current function
#' @export
#'
Bind_Chem_Data <- function(pars,pars_df){

  # --- Combine parameter and chemical data into one data frame
  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,pars_df$chem.name)),]
  pars_df <- cbind(pars_df,chemdata)
}


################################################################################
################################################################################

#' Store simulation parameters and physical-chemical data of compounds simulated
#'
#' @description
#' This function collects all user-selected parameters and compound data relevant
#' to the concentration-time profile simulation and compiles it into one data
#' frame.
#'
#'
#' @param pars A list of all user input parameters for the entire app
#'
#' @return A data frame with all relevant simulation parameters chosen by the user
#' and all physical-chemical data used for the simulated compounds
#' @seealso [modsol()], which calls the current function, and [Bind_Chem_Data()],
#' which the current function calls
#' @export
#'
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

