
####################################################################
# --- FUNCTION CREATED TO GENERATE PARAMETERS FOR MODEL SOLUTION
####################################################################

Generate_Pars <- function(){

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
               Clint_Pval = 0.05,
               AlphaPar = 0.001,
               logscale = FALSE)
}

###########################################
# --- CALCULATE OUTPUT PARAMETER VALUES
###########################################

test_that("CalcElimRate() produces the elimination rate",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  testthat::expect_gt(CalcElimRate(pars,1),0)

  pars[["spec"]] <- "Rat"
  testthat::expect_gt(CalcElimRate(pars,2),0)
})

test_that("CalcVdist() produces the volume of distribution",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  testthat::expect_gt(CalcVdist(pars,1),0)

  pars[["spec"]] <- "Rat"
  testthat::expect_gt(CalcVdist(pars,2),0)
})

test_that("CalcHalfLife() produces the half-life",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  testthat::expect_gt(CalcHalfLife(pars,1),0)

  pars[["spec"]] <- "Rat"
  testthat::expect_gt(CalcHalfLife(pars,2),0)
})

test_that("CalcClearance() produces the total plasma clearance",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  testthat::expect_gt(CalcClearance(pars,1),0)

  pars[["spec"]] <- "Rat"
  testthat::expect_gt(CalcClearance(pars,2),0)
})

test_that("CalcPCs() produces all partition coefficients",{

  # --- CREATE SAMPLE DATA
  pars <- Generate_Pars()

  # --- TEST
  testthat::expect_true(all(CalcPCs(pars,1)>0))

  pars[["spec"]] <- "Rat"
  testthat::expect_true(all(CalcPCs(pars,2)>0))
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
  chemdata <- chemdata[order(match(chemdata$Compound,out$chem.name)),]
  out <-cbind(out,chemdata)

  # --- TEST
  PC_sol_out <- StorePars_PC(pars)
  testthat::expect_equal(PC_sol_out,out)
  testthat::expect_equal(PC_sol_out$chem.name[1],PC_sol_out$Compound[1])
  testthat::expect_equal(PC_sol_out$chem.name[2],PC_sol_out$Compound[2])
  testthat::expect_equal(PC_sol_out$chem.name[3],PC_sol_out$Compound[3])
  testthat::expect_equal(PC_sol_out$chem.name[4],PC_sol_out$Compound[4])
})
