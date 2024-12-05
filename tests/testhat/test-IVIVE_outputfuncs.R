
####################################################################
# --- FUNCTION CREATED TO GENERATE PARAMETERS FOR MODEL SOLUTION
####################################################################

Generate_Pars <- function(){

  BioFile <- read.csv("SampleBioactiveConcentrations.csv")

  pars <- list(CompoundList = data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate","Abamectin","Acetochlor",
                                                                "Alachlor","Aldicarb","Ametryn","Amitraz","Atrazine")),
               doseroute = "oral",
               doseunits = "mg/kg",
               dosinginfo = list(initial.dose = 1,
                                 doses.per.day=NULL,
                                 daily.dose=NULL,
                                 dosing.matrix=NULL,
                                 forcings = NULL),
               spec = "Human",
               model = "3compartment",
               initvals = setNames(rep(0,7),
                                   c("Aintestine","Aportven","Aliver","Asyscomp","Ametabolized","Atubules","AUC")),
               returntimes = seq(0,1,signif(1/(96),round(-log10(1e-4)-1))),
               simtime = 1,
               odemethod = "lsoda",
               solversteps = 4,
               rtol = 1e-08,
               atol = 1e-08,
               rb2p = FALSE,
               restrict_clear = TRUE,
               adj_fub = TRUE,
               min_fub = 1e-4,
               defaulttoHuman = TRUE,
               regression = TRUE,
               caco2default = 1.6,
               caco_fabs = TRUE,
               caco_fgut = TRUE,
               caco_overwriteinvivo = FALSE,
               caco_keep100 = FALSE,
               HondaIVIVE = NULL,
               FSBf = 0.1,
               BioactiveFile = BioFile,
               returnsamples = FALSE,
               quantile = 0.95,
               samples = 1000,
               bioactiveIVIVE = FALSE,
               Clint_Pval = 0.05,
               AlphaPar = 0.001,
               modelSSout_units = "uM",
               output_concSS = "plasma",
               tissueSS = NULL,
               modelIVIVEout_units = "mgpkgpday",
               output_concIVIVE = "plasma",
               tissueIVIVE = NULL,
               logscale = FALSE,
               dailydose = 1)
}

######################################################
# --- DETERMINE LOG BREAKS IN IVIVE PLOTS
######################################################

test_that("log10breaks_IVIVE() produces a power of 10 sequence",{

  # --- CREATE SAMPLE DATA
  set.seed(1)
  ydata <- runif(100,min = -1,max = 10)

  # --- TEST
  expect_equal(log10breaks_IVIVE(ydata),c(0.01,0.1,1,10))
})

######################################################
# --- CREATE SCATTER PLOT OF AED VALUES
######################################################

test_that("IVIVEplot_labels() produces the title and y-axis labels",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  expect_equal(length(IVIVEplot_labels(pars)),2)
  expect_equal(IVIVEplot_labels(pars)[[1]],"In vitro-in vivo extrapolation (IVIVE) \n from the 3compartment model")

  pars[["output_concIVIVE"]] <- "plasma"
  pars[["tissueIVIVE"]] <- NULL
  pars[["modelIVIVEout_units"]] <- "uM"
  expect_equal(IVIVEplot_labels(pars)[[2]],"Oral equivalent dose (OED) \n in whole body plasma (uM)")

  pars[["output_concIVIVE"]] <- "blood"
  pars[["tissueIVIVE"]] <- "kidney"
  pars[["modelIVIVEout_units"]] <- "mg/L"
  expect_equal(IVIVEplot_labels(pars)[[2]],"Oral equivalent dose (OED) \n in kidney blood (mg/L)")

  pars[["output_concIVIVE"]] <- "tissue"
  pars[["tissueIVIVE"]] <- "liver"
  pars[["modelIVIVEout_units"]] <- "uM"
  expect_equal(IVIVEplot_labels(pars)[[2]],"Oral equivalent dose (OED) \n in liver (uM)")
})

test_that("Plotdf_Prep() produces the data frame of organized samples for plotting",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))
  pars[["samples"]] <- 10

  # --- CREATE EXPECTED OUTPUT
  set.seed(1)
  samples <- runif(36,min = 0,max = 20)

  df <- array(samples,
              dim = c(12,3),
              dimnames = list(c("OED_5","Samples",seq(1,10)),
                              c("Acetamiprid","2,4-db","Acephate")))

  #OED samples data frame
  ChemNames <- c(rep("Acetamiprid",10),rep("2,4-db",10),rep("Acephate",10))
  df_samples <- data.frame(CompoundName = ChemNames, OED = c(df[3:12,]))
  df_samples <- mutate(df_samples, CompoundName = fct_reorder(CompoundName, OED, .fun='median'))

  #5th quantile OED dose data frame
  df_q5 = data.frame(CompoundName = colnames(df), OED = c(df[1,]))
  plt_order <- c("Acephate","Acetamiprid","2,4-db")
  df_q5 <- df_q5[match(plt_order, df_q5$CompoundName),]

  # --- TEST
  expect_equal(length(Plotdf_Prep(df,pars)),2)
  expect_equal(Plotdf_Prep(df,pars)[[1]],df_samples)
  expect_equal(Plotdf_Prep(df,pars)[[2]],df_q5)
})

######################################################
# --- SOLVE MODEL FOR IVIVE SOLUTION
######################################################

test_that("CalcOED() produces a single OED value or a vector of OED values",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))

  bioactive <- pars[["BioactiveFile"]]
  bioactive_conc <- bioactive[match(pars[["CompoundList"]][,1], bioactive$ChemicalName),]

  # --- CREATE EXPECTED OUTPUT
  OED <- CalcOED(1,pars,bioactive_conc)

  # --- TEST
  #output for return samples false
  expect_true(CalcOED(1,pars,bioactive_conc)>0)
  expect_equal(CalcOED(1,pars,bioactive_conc), OED) #check that seed is set
  expect_equal(names(CalcOED(1,pars,bioactive_conc)),"95%")

  #output for return samples true
  pars[["returnsamples"]] <- TRUE
  OEDsamples <- CalcOED(2,pars,bioactive_conc)

  expect_equal(length(CalcOED(2,pars,bioactive_conc)),1000)
  expect_true(all(CalcOED(2,pars,bioactive_conc)>0))
  expect_equal(CalcOED(2,pars,bioactive_conc), OEDsamples) #check that seed is set

  #output for honda = honda1
  pars[["returnsamples"]] <- FALSE
  pars[["HondaIVIVE"]] <- "Honda1"
  expect_true(CalcOED(1,pars,bioactive_conc)>0)

  #output for honda = honda2
  pars[["HondaIVIVE"]] <- "Honda2"
  expect_true(CalcOED(1,pars,bioactive_conc)>0)

  #output for tissue and concentrations
  pars[["HondaIVIVE"]] <- NULL
  pars[["concentration"]] <- "blood"
  pars[["tissueIVIVE"]] <- "kidney"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "tissue"
  pars[["tissueIVIVE"]] <- "adipose"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "plasma"
  pars[["tissueIVIVE"]] <- "brain"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "blood"
  pars[["tissueIVIVE"]] <- "gut"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "tissue"
  pars[["tissueIVIVE"]] <- "heart"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "plasma"
  pars[["tissueIVIVE"]] <- "liver"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "blood"
  pars[["tissueIVIVE"]] <- "lung"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "tissue"
  pars[["tissueIVIVE"]] <- "muscle"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "plasma"
  pars[["tissueIVIVE"]] <- "skin"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "blood"
  pars[["tissueIVIVE"]] <- "spleen"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["concentration"]] <- "tissue"
  pars[["tissueIVIVE"]] <- "rest"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)
})

test_that("ConvertBioactive() produces the desired bioactive concentration",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))

  bioactive <- pars[["BioactiveFile"]][1:3,]
  bioactive_conc <- bioactive[match(pars[["CompoundList"]][,1], bioactive$ChemicalName),]

  # --- CREATE EXPECTED OUTPUT
  converted_bc <- armitage_eval(casrn.vector = bioactive_conc[,2],
                                this.FBSf = pars[["FSBf"]],
                                nomconc.vector = bioactive_conc[,3])
  BC_convert_df <- bioactive_conc
  BC_convert_df[,3] <- converted_bc$cfree.invitro

  # --- TEST
  expect_equal(ConvertBioactive(pars,bioactive_conc),bioactive_conc)

  pars[["HondaIVIVE"]] <- "Honda1"
  expect_equal(ConvertBioactive(pars,bioactive_conc),BC_convert_df)

  pars[["HondaIVIVE"]] <- "Honda3"
  expect_equal(ConvertBioactive(pars,bioactive_conc),bioactive_conc)
})

test_that("StorePars_IVIVE() outputs a data frame of parameters used in the simulation",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))
  bioactive <- pars[["BioactiveFile"]]
  bioactive_conc <- bioactive[match(pars[["CompoundList"]][,1], bioactive$ChemicalName),]

  # --- CREATE EXPECTED OUTPUT
  out <- data.frame(chem.name = pars[["CompoundList"]][,1],
                    conc = bioactive_conc[,3],
                    which.quantile = pars[["quantile"]],
                    species = pars[["spec"]],
                    input.units = "uM",
                    output.units = pars[["modelIVIVEout_units"]],
                    return.samples = pars[["returnsamples"]],
                    restrictive.clearance = pars[["restrict_clear"]],
                    bioactive.free.invivo = pars[["bioactiveIVIVE"]],
                    tissue = "NULL",
                    concentration = pars[["output_concIVIVE"]],
                    IVIVE = "NULL",
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
  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  out <-cbind(out,chemdata)

  # --- TEST
  expect_equal(StorePars_IVIVE(pars,bioactive_conc),out)

  pars[["tissueIVIVE"]] <- "kidney"
  out$tissue <- "kidney"
  pars[["HondaIVIVE"]] <- "Honda1"
  out$IVIVE <- "Honda1"
  pars[["output_concIVIVE"]] <- "blood"
  out$concentration <- "blood"
  expect_equal(StorePars_IVIVE(pars,bioactive_conc),out)
})

test_that("IVIVEsol() returns a solution list",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))
  pars[["BioactiveFile"]] <- data.frame(name = "SampleBioactiveConcentrations.csv",
                                        size = 301,
                                        type = "text/csv",
                                        datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/SampleBioactiveConcentrations.csv")

  # --- CREATE EXPECTED OUTPUT
  CompNames <- dplyr::arrange(pars[["CompoundList"]])[,1]

  # --- TEST
  #test typeof of list
  expect_equal(typeof(IVIVEsol(pars)),"list")

  #test return samples = false
  expect_equal(nrow(IVIVEsol(pars)[[1]]), 3)
  expect_equal(ncol(IVIVEsol(pars)[[1]]), 2)
  expect_equal(IVIVEsol(pars)[[1]][,1], CompNames)
  expect_false(any(IVIVEsol(pars)[[1]][,2] == 0))

  #test return samples = true
  pars[["returnsamples"]] <- TRUE
  expect_equal(nrow(IVIVEsol(pars)[[1]]), 1002)
  expect_equal(ncol(IVIVEsol(pars)[[1]]), 3)
  expect_equal(colnames(IVIVEsol(pars)[[1]]), CompNames)
  expect_true(all(is.na(IVIVEsol(pars)[[1]][2,])))
  expect_false(any(IVIVEsol(pars)[[1]][1,] == 0))
  expect_false(any(IVIVEsol(pars)[[1]][3:1002,] == 0))
})
