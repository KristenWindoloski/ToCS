
####################################################################
# --- FUNCTION CREATED TO GENERATE PARAMETERS FOR MODEL SOLUTION
####################################################################

Generate_Pars <- function(){

  BioFile <- read.csv("SampleBioactiveConcentrations.csv")

  pars <- list(CompoundList = data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate","Abamectin","Acetochlor",
                                                                "Alachlor","Aldicarb","Ametryn","Amitraz","Atrazine")),
               spec = "Human",
               model = "3compartment",
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
               modelIVIVEout_units = "mgpkgpday",
               output_concIVIVE = "plasma",
               tissueIVIVE = NULL,
               logscale = FALSE)
}

######################################################
# --- CREATE SCATTER PLOT OF OED VALUES
######################################################

# test_that("IVIVEplot_labels() produces the title and y-axis labels",{
#
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#
#   # --- TEST
#   expect_equal(length(IVIVEplot_labels(pars)),2)
#   expect_equal(IVIVEplot_labels(pars)[[1]],"In vitro-in vivo extrapolation (IVIVE) \n from the 3compartment model")
#
#   # no exposure file uploaded
#   pars[["output_concIVIVE"]] <- "plasma"
#   pars[["tissueIVIVE"]] <- NULL
#   pars[["modelIVIVEout_units"]] <- "mgpkgpday"
#   expect_equal(IVIVEplot_labels(pars)[[2]],"Oral equivalent dose (OED) \n in whole body plasma (mgpkgpday)")
#
#   pars[["output_concIVIVE"]] <- "blood"
#   pars[["modelIVIVEout_units"]] <- "umolpkgpday"
#   expect_equal(IVIVEplot_labels(pars)[[2]],"Oral equivalent dose (OED) \n in whole body blood (umolpkgpday)")
#
#   pars[["output_concIVIVE"]] <- "tissue"
#   pars[["tissueIVIVE"]] <- "liver"
#   pars[["modelIVIVEout_units"]] <- "mgpkgpday"
#   expect_equal(IVIVEplot_labels(pars)[[2]],"Oral equivalent dose (OED) \n in liver (mgpkgpday)")
#
#
#   # exposure file uploaded
#   pars[["fileExposure"]] <- "Uploaded"
#   pars[["output_concIVIVE"]] <- "plasma"
#   pars[["tissueIVIVE"]] <- NULL
#   pars[["modelIVIVEout_units"]] <- "mgpkgpday"
#   expect_equal(IVIVEplot_labels(pars)[[2]],"OED in whole body plasma \n or exposure (mgpkgpday)")
#
#   pars[["output_concIVIVE"]] <- "blood"
#   pars[["modelIVIVEout_units"]] <- "umolpkgpday"
#   expect_equal(IVIVEplot_labels(pars)[[2]],"OED in whole body blood \n or exposure (umolpkgpday)")
#
#   pars[["output_concIVIVE"]] <- "tissue"
#   pars[["tissueIVIVE"]] <- "liver"
#   pars[["modelIVIVEout_units"]] <- "mgpkgpday"
#   expect_equal(IVIVEplot_labels(pars)[[2]],"OED in liver \n or exposure (mgpkgpday)")
# })

# test_that("Plotdf_Prep() produces the data frame of organized samples for plotting",{
#
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#   pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))
#   pars[["samples"]] <- 10
#
#   # --- CREATE EXPECTED OUTPUT
#   set.seed(1)
#   samples <- runif(36,min = 0,max = 20)
#
#   df <- array(samples,
#               dim = c(12,3),
#               dimnames = list(c("OED_5","Samples",seq(1,10)),
#                               c("Acetamiprid","2,4-db","Acephate")))
#
#   #OED samples data frame
#   ChemNames <- c(rep("Acetamiprid",10),rep("2,4-db",10),rep("Acephate",10))
#   CASvals <- c(rep("135410-20-7",10),rep("94-82-6",10),rep("30560-19-1",10))
#   df_samples <- data.frame(CompoundName = ChemNames, CAS = CASvals, OED = c(df[3:12,]))
#   df_samples <- dplyr::mutate(df_samples, CompoundName = forcats::fct_reorder(CompoundName, OED, .fun='median'))
#
#   #5th quantile OED dose data frame
#   df_q5 = data.frame(CompoundName = colnames(df), OED = c(df[1,]))
#   plt_order <- c("Acephate","Acetamiprid","2,4-db")
#   df_q5 <- df_q5[match(plt_order, df_q5$CompoundName),]
#
#   # --- TEST
#   out <- Plotdf_Prep(df,pars)
#   expect_equal(length(out),2)
#   expect_equal(out[[1]],df_samples)
#   expect_equal(out[[2]],df_q5)
# })
#
# ######################################################
# # --- SOLVE MODEL FOR IVIVE SOLUTION
# ######################################################
#
test_that("CalcOED() produces a single OED value or a vector of OED values",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))

  bioactive <- pars[["BioactiveFile"]]
  bioactive_conc <- bioactive[match(pars[["CompoundList"]][,1], bioactive$ChemicalName),]

  attach(the)

  # --- CREATE EXPECTED OUTPUT
  OED <- CalcOED(1,pars,bioactive_conc)

  # --- TEST
  #output for return samples false
  out <- CalcOED(1,pars,bioactive_conc)
  expect_true(out>0)
  expect_equal(out, OED) #check that seed is set
  expect_equal(names(out),"95%")

  #output for return samples true
  pars[["returnsamples"]] <- TRUE
  OEDsamples <- CalcOED(2,pars,bioactive_conc)

  out <- CalcOED(2,pars,bioactive_conc)
  expect_equal(length(out),1000)
  expect_true(all(out>0))
  expect_equal(out, OEDsamples) #check that seed is set

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
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["HondaIVIVE"]] <- "Honda4"
  pars[["concentration"]] <- "tissue"
  pars[["tissueIVIVE"]] <- "adipose"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "bone"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "brain"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "gut"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "heart"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "kidney"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "liver"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "lung"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "muscle"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "skin"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "spleen"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  pars[["tissueIVIVE"]] <- "rest"
  expect_true(CalcOED(3,pars,bioactive_conc)>0)

  detach(the)
})

test_that("ConvertBioactive() produces the desired bioactive concentration",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate"))

  bioactive <- pars[["BioactiveFile"]][1:3,]
  bioactive_conc <- bioactive[match(pars[["CompoundList"]][,1], bioactive$ChemicalName),]

  # --- CREATE EXPECTED OUTPUT
  attach(the)

  converted_bc <- httk::armitage_eval(casrn.vector = bioactive_conc[,2],
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

  detach(the)
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
  chemdata <- httk::chem.physical_and_invitro.data[httk::chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,out$chem.name)),]
  out <-cbind(out,chemdata)

  # --- TEST
  attach(the)

  IVIVE_sol_out <- StorePars_IVIVE(pars,bioactive_conc)
  expect_equal(IVIVE_sol_out,out)
  expect_equal(IVIVE_sol_out$chem.name[1],IVIVE_sol_out$Compound[1])
  expect_equal(IVIVE_sol_out$chem.name[2],IVIVE_sol_out$Compound[2])
  expect_equal(IVIVE_sol_out$chem.name[3],IVIVE_sol_out$Compound[3])

  detach(the)
})

######################################################
# --- IVIVEplot_caption
######################################################

test_that("IVIVEplot_caption() outputs a data frame of parameters used in the simulation",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- CREATE EXPECTED OUTPUT
  output1 <- paste("Figure 1: Boxplots of 1000 oral equivalent dose
              (OED) samples for each selected compound (red shaded) and user-uploaded exposure estimates (purple).
              The black dots represent outliers and the red dots indicate the 5th quantile OED for
              each compound. Compounds are arranged in ascending order of their median OED value.
              Exposure estimates are shown as a distribution if more than one exposure estimate was provided
              for each compound. The purple dot represents the median exposure either uploaded by the user
              or calculated within the program. If the user only uploaded one exposure value for a compound, then
              the purple dot represents that value.")

  output2 <- paste("Figure 1: Boxplots of 1000 oral equivalent dose
              (OED) samples for each selected compound. The black dots represent outliers
              and the red dots indicate the 5th quantile OED for each compound. Compounds
              are arranged in ascending order of their median OED value.")

  output3 <- paste("Figure 1: Plot of the estimated oral equivalent dose (OED) for
          each selected compound (blue) and user-uploaded exposure estimates (red).
          Compounds are arranged in ascending order of their OED values. Exposure estimates
          are shown as a distribution if more than one exposure estimate was provided
          for each compound. The purple dot represents the median exposure either uploaded
          by the user or calculated within the program. If the user only uploaded one
          exposure value for a compound, then the purple dot represents that value.")

  output4 <- paste("Figure 1: Plot of the estimated oral equivalent dose (OED) for each
            selected compound. Compounds are arranged in ascending order of their
            OED values.")

  # --- TEST
  # return samples false, no exposure data
  expect_equal(IVIVEplot_caption(pars),output4)
  # # return samples false, exposure data
  pars[["fileExposure"]] <-data.frame(name = "SampleExposureData.csv",
                                      size = 301,
                                      type = "text/csv",
                                      datapath = "SampleExposureData.csv")
  expect_equal(IVIVEplot_caption(pars),output3)
  # return samples true, exposure data
  pars[["returnsamples"]] <- TRUE
  expect_equal(IVIVEplot_caption(pars),output1)
  # return samples true, no exposure data
  pars[["fileExposure"]] <- NULL
  expect_equal(IVIVEplot_caption(pars),output2)
})

######################################################
# --- FillExposureData()
######################################################

test_that("FillExposureData() return a data frame with no missing values",{

  # --- CREATE SAMPLE DATA
  expdata <- data.frame(Compound = c("A","B","C","D","E","F","G"),
                        CAS = c("J","K","L","M","N","O","P"),
                        Upper = c(1,NA,NA,4,NA,9,10),
                        Median = c(NA,2,NA,5,6,NA,11),
                        Lower = c(NA,NA,3,NA,7,8,12))

  # --- TEST
  expect_false(anyNA(FillExposureData(expdata)))
})

######################################################
# --- PrepExposureData()
######################################################

test_that("PrepExposureData() return a data frame with no missing values",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("2,4-db","Acephate","Acetamiprid"))
  pars[["fileExposure"]] <-data.frame(name = "SampleExposureData2.csv",
                                      size = 301,
                                      type = "text/csv",
                                      datapath = "SampleExposureData2.csv")

  # --- CREATE EXPECTED OUTPUT
  expdata_out <- data.frame(CompoundName = c("2,4-db","Acephate","Acetamiprid"),
                            CAS = c("94-82-6","30560-19-1","135410-20-7"),
                            Upper = c(1,5,NA),
                            Median = c(1,8,4),
                            Lower = c(NA,3,6),
                            maxval = c(1,8,6))

  expdata_out2 <- data.frame(CompoundName = c("2,4-db","Acephate","Acetamiprid"),
                            CAS = c("94-82-6","30560-19-1","135410-20-7"),
                            Upper = c(4.014,27.29,NA),
                            Median = c(4.014,43.67,17.96),
                            Lower = c(NA,16.38,26.94),
                            maxval = c(4.014,43.67,26.94))

  attach(the)

  # --- TEST
  # mgpkgpday units
  expect_true(all.equal(PrepExposureData(pars),expdata_out,check.attributes = FALSE))
  # umolpkgpday units
  pars[["modelIVIVEout_units"]] <- "umolpkgpday"
  expect_true(all.equal(PrepExposureData(pars),expdata_out2,check.attributes = FALSE))

  detach(the)
})

