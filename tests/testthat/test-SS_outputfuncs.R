
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

test_that("log10breaks_SS() produces a power of 10 sequence",{

  # --- CREATE SAMPLE DATA
  set.seed(1)
  ydata <- runif(100,min = -1,max = 10)

  # --- TEST
  expect_equal(log10breaks_SS(ydata),c(0.01,0.1,1,10))
})
# 
# ######################################################
# # --- CREATE SCATTER PLOT OF STEADY STATE VALUES
# ######################################################
# 
# test_that("plot_labels() produces a title and y axis label",{
#   
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#   
#   # --- TEST
#   expect_equal(length(plot_labels(pars)),2)
#   expect_equal(plot_labels(pars)[[1]],"Steady state concentrations generated from the 3compartment model")
#   
#   pars[["output_concSS"]] <- "plasma"
#   pars[["tissueSS"]] <- NULL
#   pars[["modelSSout_units"]] <- "uM"
#   expect_equal(plot_labels(pars)[[2]],"Whole body plasma \n concentration (uM)")
#   
#   pars[["output_concSS"]] <- "blood"
#   pars[["tissueSS"]] <- "kidney"
#   pars[["modelSSout_units"]] <- "mg/L"
#   expect_equal(plot_labels(pars)[[2]],"blood concentration \n in the kidney (mg/L)")
#   
#   pars[["output_concSS"]] <- "tissue"
#   pars[["tissueSS"]] <- "liver"
#   pars[["modelSSout_units"]] <- "uM"
#   expect_equal(plot_labels(pars)[[2]],"liver concentration (uM)")
# })

# test_that("CalcAnalyticCss() produces a steady state concentration",{
#   
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#   
#   # --- TEST
#   pars[["spec"]] <- "Rat"
#   pars[["model"]] <- "3compartmentss"
#   pars[["output_concSS"]] <- "plasma"
#   expect_equal(CalcAnalyticCss(pars,4),0.2002)
#   
#   pars[["spec"]] <- "Human"
#   pars[["model"]] <- "1compartment"
#   pars[["output_concSS"]] <- "blood"
#   pars[["tissueSS"]] <- "kidney"
#   expect_equal(CalcAnalyticCss(pars,1),18.07)
#   
#   pars[["model"]] <- "3compartment"
#   pars[["output_concSS"]] <- "plasma"
#   pars[["tissueSS"]] <- NULL
#   expect_equal(CalcAnalyticCss(pars,2),152)
#   
#   pars[["model"]] <- "pbtk"
#   pars[["output_concSS"]] <- "tissue"
#   pars[["tissueSS"]] <- "brain"
#   expect_equal(CalcAnalyticCss(pars,3),1.47)
# })
# 
# test_that("CalcCssDay() produces the relevant info when Css is reached",{
#   
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#   
#   # --- TEST
#   pars[["spec"]] <- "Rat"
#   pars[["model"]] <- "1compartment"
#   expect_equal(CalcCssDay(pars,1)[[1]],0.8492)
#   expect_equal(CalcCssDay(pars,1)[[2]],0.8445)
#   expect_equal(CalcCssDay(pars,1)[[3]],1.181)
#   expect_equal(CalcCssDay(pars,1)[[4]],8)
#   
#   pars[["spec"]] <- "Human"
#   pars[["model"]] <- "3compartment"
#   expect_equal(CalcCssDay(pars,2)[[1]],151.9)
#   expect_equal(CalcCssDay(pars,2)[[2]],0.999)
#   expect_equal(CalcCssDay(pars,2)[[3]],152.6)
#   expect_equal(CalcCssDay(pars,2)[[4]],75)
#   
#   pars[["model"]] <- "pbtk"
#   expect_equal(CalcCssDay(pars,3)[[1]],1.99)
#   expect_equal(CalcCssDay(pars,3)[[2]],0.9716)
#   expect_equal(CalcCssDay(pars,3)[[3]],2.506)
#   expect_equal(CalcCssDay(pars,3)[[4]],4)
# })

# test_that("StorePars_SS() outputs a data frame of parameters used in the simulation",{
#   
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#   
#   # --- CREATE EXPECTED OUTPUT
#   pars[["tissueSS"]] <- "NULL"
#   out <- data.frame(chem.name = pars[["CompoundList"]][,1],
#                     species = pars[["spec"]], 
#                     dose = pars[["dailydose"]],
#                     route = pars[["doseroute"]], 
#                     output.units = pars[["modelSSout_units"]],
#                     model = pars[["model"]], 
#                     concentration = pars[["output_concSS"]],
#                     tissue = pars[["tissueSS"]],
#                     restrictive.clearance = pars[["restrict_clear"]], 
#                     bioactive.free.invivo = pars[["bioactiveIVIVE"]],
#                     Caco2.Pab.default = pars[["caco2default"]],
#                     Caco2.Fabs = pars[["caco_fabs"]],
#                     Caco2.Fgut = pars[["caco_fgut"]],
#                     overwrite.invivo = pars[["caco_overwriteinvivo"]],
#                     keepit100 = pars[["caco_keep100"]],
#                     default.to.human = pars[["defaulttoHuman"]],
#                     adjusted.Funbound.plasma = pars[["adj_fub"]],
#                     minimum.Funbound.plasma = pars[["min_fub"]], 
#                     regression = pars[["regression"]])
#   
#   chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
#   out <-cbind(out,chemdata)
#   
#   # --- TEST
#   expect_equal(StorePars_SS(pars),out)
#   
#   pars[["tissueSS"]] <- "kidney"
#   out$tissue <- "kidney"
#   expect_equal(StorePars_SS(pars),out)
# })
# 
# test_that("SS_sol() produces a list with the SS concentrations, Css_Day statistics, and parameters",{
#   
#   # --- CREATE SAMPLE DATA
#   pars <- Generate_Pars()
#   pars[["CompoundList"]] <- data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate","Abamectin"))
#   
#   # --- CREATE EXPECTED OUTPUT
#   sol <- data.frame(CompoundName = c("Acephate","Abamectin","Acetamiprid","2,4-db"),
#                     SteadyState = c(2.042,2.333,3.813,152))
#   
#   CssDay <- data.frame(CompoundName = c("Acephate","Acetamiprid","Abamectin","2,4-db"),
#                        CssDay = c(6,11,74,75),
#                        AvgConc = c(1.986,3.785,2.332,151.9),
#                        RatioAvgAnalytical = c(0.9663,0.9915,0.9991,0.999),
#                        MaxConc = c(2.568,4.112,2.343,152.6))
# 
#   parout <- data.frame(chem.name = pars[["CompoundList"]][,1],
#                     species = pars[["spec"]],
#                     dose = pars[["dailydose"]],
#                     route = pars[["doseroute"]],
#                     output.units = pars[["modelSSout_units"]],
#                     model = pars[["model"]],
#                     concentration = pars[["output_concSS"]],
#                     tissue = "NULL",
#                     restrictive.clearance = pars[["restrict_clear"]],
#                     bioactive.free.invivo = pars[["bioactiveIVIVE"]],
#                     Caco2.Pab.default = pars[["caco2default"]],
#                     Caco2.Fabs = pars[["caco_fabs"]],
#                     Caco2.Fgut = pars[["caco_fgut"]],
#                     overwrite.invivo = pars[["caco_overwriteinvivo"]],
#                     keepit100 = pars[["caco_keep100"]],
#                     default.to.human = pars[["defaulttoHuman"]],
#                     adjusted.Funbound.plasma = pars[["adj_fub"]],
#                     minimum.Funbound.plasma = pars[["min_fub"]],
#                     regression = pars[["regression"]])
# 
#   chemdata <- chem.physical_and_invitro.data[chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
#   parout <-cbind(parout,chemdata)
#   
#   expect_equal(length(SS_sol(pars)),3)
#   expect_equal(SS_sol(pars)[[1]],sol)
#   expect_equal(SS_sol(pars)[[2]],CssDay)
#   expect_equal(SS_sol(pars)[[3]],parout)
# })


