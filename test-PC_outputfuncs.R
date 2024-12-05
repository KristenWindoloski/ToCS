
rm(list = ls())

library(testthat)
library(httk)
library(DescTools)
library(ggplot2)
library(gridExtra)
library(dplyr)

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
               output_concIVIVE = "uM",
               tissueIVIVE = NULL,
               logscale = FALSE,
               dailydose = 1)
}

######################################################
# --- DETERMINE LOG BREAKS IN SS PLOTS
######################################################

test_that("log10breaks_Par() produces a power of 10 sequence",{

  # --- CREATE SAMPLE DATA
  set.seed(1)
  ydata <- runif(100,min = -1,max = 10)

  # --- TEST
  expect_equal(log10breaks_Par(ydata),c(0.01,0.1,1,10))
})

###########################################
# --- CALCULATE OUTPUT PARAMETER VALUES
###########################################

test_that("CalcElimRate() produces the elimination rate",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  expect_equal(CalcElimRate(pars,1),0.02854)

  pars[["spec"]] <- "Rat"
  expect_equal(CalcElimRate(pars,2),0.01064)
})

test_that("CalcVdist() produces the volume of distribution",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  expect_equal(CalcVdist(pars,1),1.674)

  pars[["spec"]] <- "Rat"
  expect_equal(CalcVdist(pars,2),0.1956)
})

test_that("CalcPCs() produces all partition coefficients",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- CREATE EXPECTED OUTPUT
  PCnames <- c("Kadipose2pu","Kbone2pu","Kbrain2pu","Kgut2pu","Kheart2pu","Kkidney2pu",
               "Kliver2pu","Klung2pu","Kmuscle2pu","Kskin2pu","Kspleen2pu","Krbc2pu","Krest2pu")

  out1 <- list(6.675, 2.175, 3.202, 6.141, 7.078, 10.63, 12.38, 2.395, 2.328,
               4.041, 3.203, 1.588, 8.894)
  out2 <- list(16.12, 15.38, 25.11, 27.41, 34.92, 93.61,
               111.7, 35.79, 24.15, 40.48, 33.73, 3.109, 18.79)

  names(out1) <- PCnames
  names(out2) <- PCnames

  # --- TEST
  expect_equal(CalcPCs(pars,1),out1)

  pars[["spec"]] <- "Rat"
  expect_equal(CalcPCs(pars,2),out2)
})

test_that("StorePars_PC() outputs a data frame of parameters used in the simulation",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- CREATE EXPECTED OUTPUT
  out <- data.frame(chem.name = pars[["CompoundList"]][,1],
                    species = pars[["spec"]],
                    default.to.human = pars[["defaulttoHuman"]],
                    restrictive.clearance = pars[["restrict_clear"]],
                    adjusted.Funbound.plasma = pars[["adj_fub"]],
                    regression = pars[["regression"]],
                    clint.pvalue.threshold = pars[["Clint_Pval"]],
                    minimum.Funbound.plasma = pars[["min_fub"]],
                    alpha = pars[["AlphaPar"]])

  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  out <-cbind(out,chemdata)

  # --- TEST
  expect_equal(StorePars_PC(pars),out)
})

test_that("Parsol() produces a list with three components",{
  
  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()
  pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","Acephate"))
  
  # --- CREATE EXPECTED OUTPUT,
  out1 <- data.frame(CompoundName = c("Acetamiprid","Acephate"), 
                     EliminationRate = c(0.02854,0.1447),
                     VolumeOfDistribution = c(1.674,0.6426))
  out1 <- arrange(out1,EliminationRate)
  out1$CompoundName <- factor(out1$CompoundName, levels = out1$CompoundName)
  
  out2 <- data.frame(CompoundName = c("Acetamiprid","Acephate"),
                     AdiposePC = c(6.675,0.4032),
                     BonePC = c(2.175,0.5117),
                     BrainPC = c(3.202,0.8297),
                     GutPC = c(6.141,1.276),
                     HeartPC = c(7.078,1.197),
                     KidneyPC = c(10.63,2.878),
                     LiverPC = c(12.38,3.068),
                     LungPC = c(2.395,0.9638),
                     MusclePC = c(2.328,0.8363),
                     SkinPC = c(4.041,1.034),
                     SpleenPC = c(3.203,1.001),
                     RbcPC = c(1.588,0.7175),
                     RestPC = c(8.894,1.001),
                     RowMedian = c(4.041,1.001))
  out2 <- arrange(out2,RowMedian)
  out2$CompoundName <- factor(out2$CompoundName, levels = out2$CompoundName)
  
  parout <- data.frame(chem.name = pars[["CompoundList"]][,1],
                        species = pars[["spec"]],
                        default.to.human = pars[["defaulttoHuman"]],
                        restrictive.clearance = pars[["restrict_clear"]],
                        adjusted.Funbound.plasma = pars[["adj_fub"]],
                        regression = pars[["regression"]],
                        clint.pvalue.threshold = pars[["Clint_Pval"]],
                        minimum.Funbound.plasma = pars[["min_fub"]],
                        alpha = pars[["AlphaPar"]])
  chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  parout <-cbind(parout,chemdata)
  
  # --- TEST
  expect_equal(typeof(Parsol(pars)),"list")
  expect_equal(Parsol(pars)[[1]], out1)
  expect_equal(Parsol(pars)[[2]], out2)
  expect_equal(Parsol(pars)[[3]], parout)
})
