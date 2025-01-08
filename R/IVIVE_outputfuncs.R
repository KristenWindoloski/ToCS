

######################################################
# --- SOLVE MODEL FOR IVIVE SOLUTION
######################################################

IVIVEsol <- function(pars){

  # --- PROCESS BIOACTIVE CONCENTRATIONS FILE
  file <- pars[["BioactiveFile"]]
  bioactive <- read.csv(file$datapath)

  # --- REARRANGE ROWS OF BIOACTIVE FILE TO BE IN SAME ORDER AS COMPOUNDS FILE
    # --- CONVERT BIOACTIVE CONCENTRATION IF HONDA1 IS SELECTED
  bioactive_conc <- bioactive[match(pars[["CompoundList"]][,1], bioactive$ChemicalName),]
  bioactive_conc <- ConvertBioactive(pars,bioactive_conc)

  # --- EXTRACT DIMENSIONS NEEDED FOR SOLUTION
  n <- nrow(pars[["CompoundList"]])

  # --- SET OUTPUT TYPE AND SIZE: DATA FRAME (return.samples = FALSE) OR ARRAY (return.samples = TRUE)
  if (pars[["returnsamples"]] == FALSE){

    sol <- data.frame(CompoundName = pars[["CompoundList"]][,1], OED = rep(0,n))
  }
  else if (pars[["returnsamples"]] == TRUE) {

    # --- CREATE ARRAY WITH 1ST ROW BEING 5TH DOSE OED AND 3-END ROWS BEING SAMPLES
    sol <- array(data = rep(0,n*(pars[["samples"]]+2)),
                 dim = c(pars[["samples"]]+2,n))
    dimnames(sol) <- list(c("OED_5","Samples",seq(1,pars[["samples"]])),
                          pars[["CompoundList"]][,1])
  }

  # --- Generate OED solution for each chemical
  for (i in 1:n) {

    print(bioactive_conc)
    OED <- CalcOED(i,pars,bioactive_conc)

    if (pars[["returnsamples"]] == FALSE){
      sol[i,2] <- OED
    }
    else if (pars[["returnsamples"]] == TRUE) {

      # --- CALCULATE 95% CSS THEN CONVERT TO BIOACTIVE CONCENTRATION (SAME PROCESS AS HTTK CODE)
      q <- stats::quantile(bioactive_conc[i,3]/OED, 0.95, na.rm=TRUE)
      sol[1,i] <- signif(bioactive_conc[i,3]/q, digits = 4)
      sol[2,i] <- NA
      sol[seq(3,pars[["samples"]]+2),i] <- OED
    }
  }

  # --- STORE PARAMETERS USED FOR THE SIMULATION
  pars_df <- StorePars_IVIVE(pars,bioactive_conc)

  # --- RETURN LIST OF OUTPUTS
  out <- list(sol,bioactive_conc,pars_df)
}

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
                                  Caco2.options = list(Caco2.Pab.default = pars[["caco2default"]],
                                                       Caco2.Fabs = pars[["caco_fabs"]],
                                                       Caco2.Fgut = pars[["caco_fgut"]],
                                                       overwrite.invivo = pars[["caco_overwriteinvivo"]],
                                                       keepit100 = pars[["caco_keep100"]]),
                                  calc.analytic.css.arg.list = list(adjusted.Funbound.plasma = pars[["adj_fub"]]),
                                  parameterize.arg.list = list(default.to.human = pars[["defaulttoHuman"]],
                                                               minimum.Funbound.plasma = pars[["min_fub"]],
                                                               regression = pars[["regression"]]),
                                  samples = pars[["samples"]])
}

ConvertBioactive <- function(pars,bioactive_df){

  if (is.null(pars[["HondaIVIVE"]])){
    bioactive_conc <- bioactive_df
  }
  else if (pars[["HondaIVIVE"]] == "Honda1"){

    arm_out <- httk::armitage_eval(casrn.vector = bioactive_df[,2],
                                   this.FBSf = pars[["FSBf"]],
                                   nomconc.vector = bioactive_df[,3])

    bioactive_conc <- data.frame(ChemicalName = pars[["CompoundList"]][,1],
                                 CAS = arm_out$casrn,
                                 Bioactive.Concentration = arm_out$cfree.invitro)
  }
  else {
    bioactive_conc <- bioactive_df
  }
}

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
  chemdata <- httk::chem.physical_and_invitro.data[httk::chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  pars_df <-cbind(pars_df,chemdata)

}

######################################################
# --- DETERMINE LOG BREAKS IN SS PLOTS
######################################################

log10breaks_IVIVE <- function(ydata) {

  x <- ydata[ydata > 0]

  bottom <- floor(log10(min(x)))
  top <- ceiling(log10(max(x)))
  10^(seq(bottom, top))
}

######################################################
# --- CREATE SCATTER PLOT OF OED VALUES
######################################################

IVIVEplotting <- function(OED_data,BioactiveConc,pars,logscale){

  # --- SET PLOT LABEL NAMES
  plt_labels <- IVIVEplot_labels(pars)
  y_exp <- plt_labels[[2]]
  title_exp <- plt_labels[[1]]

  if (pars[["returnsamples"]] == FALSE){

    # --- ARRANGE OED DATA FOR PLOTTING
    OED_data <- dplyr::arrange(OED_data, OED)
    OED_data$CompoundName <- factor(OED_data$CompoundName, levels = OED_data$CompoundName)

    # --- PLOT SCATTER PLOT OF ALL OED VALUES
    plt <- ggplot2::ggplot(OED_data, ggplot2::aes(x = CompoundName, y = OED)) +
      ggplot2::geom_point(size = 4) +
      ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
      ggplot2::theme_bw(base_size = 18) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5))

  }
  else if (pars[["returnsamples"]] == TRUE){

    # --- CREATE DATA FRAME FOR OED SAMPLES TO PLOT
    plt_df_list <- Plotdf_Prep(OED_data,pars)
    OEDSamples_df <- plt_df_list[[1]]
    Q5_OED_df <- plt_df_list[[2]]

    # --- PLOT OED SAMPLES
    plt <- ggplot2::ggplot(OEDSamples_df, ggplot2::aes(x = CompoundName, y = OED)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(data = Q5_OED_df, ggplot2::aes(x = CompoundName, y = OED), color = 'red', size = 4)+
      ggplot2::labs(x = "Compounds", y = y_exp, title = title_exp) +
      ggplot2::theme_bw(base_size = 18) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.5, vjust = 0.5),
                     plot.title = ggplot2::element_text(hjust = 0.5))
  }

  # --- PLOT Y-AXIS ON LOG SCALE IF DESIRED
  if (logscale == TRUE){

    if (pars[["returnsamples"]] == TRUE){
      break_seq <- log10breaks_IVIVE(OEDSamples_df$OED)
    }
    else{
      break_seq <- log10breaks_IVIVE(OED_data$OED)
    }
     plt <- plt +
       ggplot2::scale_y_log10(breaks = break_seq,
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
       ggplot2::annotation_logticks(sides = "l")
  }
  return(plt)
}

IVIVEplot_labels <- function(pars){

  # --- SET TITLE LABEL
  title_exp <- paste("In vitro-in vivo extrapolation (IVIVE) \n from the ",
                     pars[["model"]], " model", sep = "")

  # --- SET Y-AXIS LABEL
  if (pars[["output_concIVIVE"]] != 'tissue'){
    if (is.null(pars[["tissueIVIVE"]])){
      y_exp <- paste("Oral equivalent dose (OED) \n in whole body ",
                     pars[["output_concIVIVE"]], " (", pars[["modelIVIVEout_units"]], ")", sep = "")
    }
    else{
      y_exp <- paste("Oral equivalent dose (OED) \n in ", pars[["tissueIVIVE"]],
                     " ", pars[["output_concIVIVE"]], " (", pars[["modelIVIVEout_units"]], ")", sep = "")
    }
  }
  else{
    y_exp <- paste("Oral equivalent dose (OED) \n in ", pars[["tissueIVIVE"]],
                   " (", pars[["modelIVIVEout_units"]], ")", sep = "")
  }

  out <- list(title_exp, y_exp)
}

Plotdf_Prep <- function(df,pars){

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
  OEDvalues <- c()
  for (i in 1:n) {
    ChemNames <- append(ChemNames, rep(cnames[i], m))
    OEDvalues <- append(OEDvalues, Samples_OED[,i])
  }

  OED_Samples_df <- data.frame(CompoundName = ChemNames, OED = OEDvalues)
  OED_Samples_df <- na.omit(OED_Samples_df)

  # --- ARRANGE LEVELS OF DATA FRAME BASED ON THE MEDIAN SAMPLE OED OF EACH COMPOUND
  OED_Samples_df_rearr <- dplyr::mutate(OED_Samples_df,
                                        CompoundName = forcats::fct_reorder(CompoundName, OED, .fun='median'))

  # --- REARRANGE 5TH OED DOSE QUANTILE DATA FOR BOXPLOT
  Plt_order <- levels(OED_Samples_df_rearr$CompoundName)
  Q5_OED_rearr <- q5_OED_df[match(Plt_order, q5_OED_df$CompoundName),]

  out <- list(OED_Samples_df_rearr, Q5_OED_rearr)
}
