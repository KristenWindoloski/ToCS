
####################################################################
# --- FUNCTION CREATED TO GENERATE PARAMETERS FOR MODEL SOLUTION
####################################################################

Generate_Pars <- function(){

  pars <- list(CompoundList = data.frame(Selected_Compounds = c("Ibuprofen","Terbufos")),
               doseroute = "oral",
               doseunits = "mg/kg",
               dosinginfo = list(initial.dose = 1,
                                 doses.per.day=NULL,
                                 daily.dose=NULL,
                                 dosing.matrix=NULL,
                                 forcings = NULL),
               spec = "Human",
               model = "1compartment",
               initvals = stats::setNames(rep(0,4),c("Agutlumen","Acompartment","Ametabolized","AUC")),
               returntimes = seq(0,1,signif(1/(96),round(-log10(1e-4)-1))),
               simtime = 1,
               odemethod = "lsoda",
               solversteps = 4,
               rtol = 1e-08,
               atol = 1e-08,
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


########################################################
# --- CHECK COMPILING COMPOUND LIST
########################################################

test_that("CompoundList() produces a data frame of selected compounds", {

  # --- CREATE SAMPLE DATA
  PreloadedComps <- c("71751-41-2, Abamectin","94-82-6, 2,4-db","148-79-8, Thiabendazole")
  UploadedComps <- data.frame(name = "4SampleChems.csv",
                              size = 1.58,
                              type = "text/csv",
                              datapath = "4SampleChems.csv")

  # --- CREATE EXPECTED OUTPUT
  df_PConly <- data.frame(Selected_Compounds = c("2,4-db","Abamectin","Thiabendazole"))
  df_UConly <- data.frame(Selected_Compounds = c("Chem1","Chem2","Chem3","Chem4"))
  df_PCUC <- data.frame(Selected_Compounds = c("2,4-db","Abamectin","Chem1","Chem2","Chem3","Chem4","Thiabendazole"))

  # --- TEST

  # --- Attach the 'the' environment to add chem.physical_and_invitro.data data frame to path
  attach(the)

  # --- Detach the attached 'the' environment
  on.exit(detach(the))

  #just preloadcomps
  expect_equal(CompoundList(PreloadedComps,NULL),df_PConly)

  #just uploaded comps
  expect_equal(CompoundList(NULL,UploadedComps),df_UConly)

  #preload and upload comps
  expect_equal(CompoundList(PreloadedComps,UploadedComps),df_PCUC)
})


########################################################
# --- CHECK RETURN TIMES PARAMETER
########################################################

test_that("OutputTimes_Par() produces a vector of times", {

  pars <- Generate_Pars()
  pars[["simtime"]] <- 15

  # --- no return times, model not fetal
  pars[["returntimes"]] <- ""
  pars[["model"]] <- "pbtk"
  out <- c(seq(0, 15, signif(1/(96), round(-log10(1e-4)-1))),15)
  expect_equal(OutputTimes_Par(pars),out)

  # --- no return times, model fetal
  pars[["returntimes"]] <- ""
  pars[["model"]] <- "fetal_pbtk"
  out <- seq(91,106,1)
  expect_equal(OutputTimes_Par(pars),out)

  # --- return times (regular), model not fetal
  pars[["returntimes"]] <- "0,1,2,3,4,5,6,7,8,9,10"
  pars[["model"]] <- "3compartment"
  out <- c(seq(0,10,1),15)
  expect_equal(OutputTimes_Par(pars),out)

  # --- return times (not regular), model not fetal
  pars[["returntimes"]] <- "0,1,2,3,0,4,5,6,7,9,8,9,10"
  pars[["model"]] <- "1compartment"
  expect_equal(OutputTimes_Par(pars),out)

  # --- return times (not regular), model fetal
  pars[["returntimes"]] <- "91,92,93,94,95,92,94"
  pars[["model"]] <- "fetal_pbtk"
  out <- c(seq(91,95,1),106)
  expect_equal(OutputTimes_Par(pars),out)

  # --- return times (regular), model fetal
  pars[["returntimes"]] <- "91,92,93,94,95"
  pars[["model"]] <- "fetal_pbtk"
  expect_equal(OutputTimes_Par(pars),out)
})

