
rm(list = ls())

####################################################################
# --- FUNCTION CREATED TO GENERATE PARAMETERS FOR MODEL SOLUTION
####################################################################

Generate_Pars <- function(){

  pars <- list(CompoundList = data.frame(Selected_Compounds = c("Acetamiprid","2,4-db","Acephate","Abamectin","Acetochlor",
                                                                "Alachlor","Aldicarb","Ametryn","Amitraz","Atrazine")),
               doseroute = "oral",
               spec = "Human",
               model = "3compartment",
               bioactiveIVIVE = FALSE,
               modelSSout_units = "uM",
               output_concSS = "plasma",
               tissueSS = NULL,
               dailydose = 1,
               restrict_clear = TRUE,
               adj_fub = TRUE,
               min_fub = 1e-4,
               defaulttoHuman = TRUE,
               regression = TRUE,
               caco2default = 1.6,
               caco_fabs = TRUE,
               caco_fgut = TRUE,
               caco_overwriteinvivo = FALSE,
               caco_keep100 = FALSE)
}

######################################################
# --- CREATE SCATTER PLOT OF STEADY STATE VALUES
######################################################

test_that("plot_labels() produces a title and y axis label",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  expect_equal(length(plot_labels(pars)),2)
  expect_equal(plot_labels(pars)[[1]],"Steady state concentrations generated from the 3compartment model")

  pars[["output_concSS"]] <- "plasma"
  pars[["tissueSS"]] <- NULL
  pars[["modelSSout_units"]] <- "uM"
  expect_equal(plot_labels(pars)[[2]],"Whole body plasma \n concentration (uM)")

  pars[["output_concSS"]] <- "blood"
  pars[["tissueSS"]] <- "kidney"
  pars[["modelSSout_units"]] <- "mg/L"
  expect_equal(plot_labels(pars)[[2]],"blood concentration \n in the kidney (mg/L)")

  pars[["output_concSS"]] <- "tissue"
  pars[["tissueSS"]] <- "liver"
  pars[["modelSSout_units"]] <- "uM"
  expect_equal(plot_labels(pars)[[2]],"liver concentration (uM)")
})

test_that("CalcAnalyticCss() produces a steady state concentration",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  pars[["spec"]] <- "Rat"
  pars[["model"]] <- "3compartmentss"
  pars[["output_concSS"]] <- "plasma"

  # --- Attach the 'the' environment to add chem.physical_and_invitro.data data frame to path
  attach(the)

  # --- Detach the attached 'the' environment
  on.exit(detach(the))

  expect_gt(CalcAnalyticCss(pars,4),0)

  pars[["spec"]] <- "Human"
  pars[["model"]] <- "1compartment"
  pars[["output_concSS"]] <- "blood"
  pars[["tissueSS"]] <- "kidney"
  expect_gt(CalcAnalyticCss(pars,1),0)

  pars[["model"]] <- "3compartment"
  pars[["output_concSS"]] <- "plasma"
  pars[["tissueSS"]] <- NULL
  expect_gt(CalcAnalyticCss(pars,2),0)

  pars[["model"]] <- "pbtk"
  pars[["output_concSS"]] <- "tissue"
  pars[["tissueSS"]] <- "brain"
  expect_gt(CalcAnalyticCss(pars,3),0)
})

test_that("CalcCssDay() produces the relevant info when Css is reached",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- Attach the 'the' environment to add chem.physical_and_invitro.data data frame to path
  attach(the)

  # --- Detach the attached 'the' environment
  on.exit(detach(the))

  # --- TEST
  pars[["spec"]] <- "Rat"
  pars[["model"]] <- "1compartment"
  out <- CalcCssDay(pars,1)
  expect_gt(out[[1]],0)
  expect_gt(out[[2]],0)
  expect_gt(out[[3]],0)
  expect_gt(out[[4]],0)

  pars[["spec"]] <- "Human"
  pars[["model"]] <- "3compartment"
  out <- CalcCssDay(pars,2)
  expect_gt(out[[1]],0)
  expect_gt(out[[2]],0)
  expect_gt(out[[3]],0)
  expect_gt(out[[4]],0)

  pars[["model"]] <- "pbtk"
  out <- CalcCssDay(pars,3)
  expect_gt(out[[1]],0)
  expect_gt(out[[2]],0)
  expect_gt(out[[3]],0)
  expect_gt(out[[4]],0)
})

test_that("StorePars_SS() outputs a data frame of parameters used in the simulation",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- CREATE EXPECTED OUTPUT
  pars[["tissueSS"]] <- "NULL"
  out <- data.frame(chem.name = pars[["CompoundList"]][,1],
                    species = pars[["spec"]],
                    dose = pars[["dailydose"]],
                    route = pars[["doseroute"]],
                    output.units = pars[["modelSSout_units"]],
                    model = pars[["model"]],
                    concentration = pars[["output_concSS"]],
                    tissue = pars[["tissueSS"]],
                    restrictive.clearance = pars[["restrict_clear"]],
                    bioactive.free.invivo = pars[["bioactiveIVIVE"]],
                    Caco2.Pab.default = pars[["caco2default"]],
                    Caco2.Fabs = pars[["caco_fabs"]],
                    Caco2.Fgut = pars[["caco_fgut"]],
                    overwrite.invivo = pars[["caco_overwriteinvivo"]],
                    keepit100 = pars[["caco_keep100"]],
                    default.to.human = pars[["defaulttoHuman"]],
                    adjusted.Funbound.plasma = pars[["adj_fub"]],
                    minimum.Funbound.plasma = pars[["min_fub"]],
                    regression = pars[["regression"]])

  chemdata <- httk::chem.physical_and_invitro.data[httk::chem.physical_and_invitro.data$Compound %in% pars[["CompoundList"]][,1],]
  chemdata <- chemdata[order(match(chemdata$Compound,out$chem.name)),]
  out <-cbind(out,chemdata)

  # --- TEST
  # --- Attach the 'the' environment to add chem.physical_and_invitro.data data frame to path
  attach(the)

  # --- Detach the attached 'the' environment
  on.exit(detach(the))

  SS_pars_out <- StorePars_SS(pars)
  expect_equal(SS_pars_out,out)
  expect_equal(SS_pars_out$chem.name[1],SS_pars_out$Compound[1])
  expect_equal(SS_pars_out$chem.name[2],SS_pars_out$Compound[2])
  expect_equal(SS_pars_out$chem.name[3],SS_pars_out$Compound[3])
  expect_equal(SS_pars_out$chem.name[4],SS_pars_out$Compound[4])
})

