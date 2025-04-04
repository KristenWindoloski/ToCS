
rm(list = ls())

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
               initvals = setNames(rep(0,4),c("Agutlumen","Acompartment","Ametabolized","AUC")),
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
               caco_keep100 = FALSE)
}

solve_httk <- function(i,pars){
  httk::solve_model(chem.name = pars[["CompoundList"]][i,1],
                    route = pars[["doseroute"]],
                    input.units = pars[["doseunits"]],
                    dosing = pars[["dosinginfo"]],
                    species = pars[["spec"]],
                    model = pars[["model"]],
                    initial.values = pars[["initvals"]],
                    suppress.messages = TRUE,
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

}

########################################################
# --- GENERATE A TABLE WITH TK SUMMARY STATISTICS
########################################################

test_that("TKsummary() produces a table of simulation summary statistics ",{

  # --- CREATE INPUT
  sol <- solve_model(chem.name="Ibuprofen",
                     model="1compartment",
                     days=1,
                     times = seq(0,1,0.1),
                     suppress.messages = TRUE,
                     dosing=list(initial.dose = 1,
                                 doses.per.day=NULL,
                                 dosing.matrix = NULL,
                                 daily.dose = NULL))

  # --- CREATE EXPECTED OUTPUT
  AUC1 <- signif(DescTools::AUC(x = sol[,1], y = sol[,"Agutlumen"], method = "trapezoid"),4)
  AUC2 <- signif(DescTools::AUC(x = sol[,1], y = sol[,"Ccompartment"], method = "trapezoid"),4)
  AUC3 <- signif(DescTools::AUC(x = sol[,1], y = sol[,"Ametabolized"], method = "trapezoid"),4)
  AUC4 <- signif(DescTools::AUC(x = sol[,1], y = sol[,"AUC"], method = "trapezoid"),4)
  df_final <- data.frame(Tmax = c('0','0.1','1','1'),
                         MaxValue = c('292.5','6.299','166.9','4.496'),
                         AUC = c(AUC1,AUC2,AUC3,AUC4))

  mat_final <- apply(as.matrix(df_final), 2, as.numeric)

  # --- TEST (current,target)
  expect_equal(TKsummary(sol),mat_final)
})

########################################################
# --- TEST RUN_ADME_MODEL()
########################################################

# ---------------- TEST FOR SINGLE DOSING

test_that("Run_ADME_Model() produces output for single dosing",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:5]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})


# --- TEST FOR DAILY DOSING

test_that("Run_ADME_Model() produces a solution output for daily dosing ",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()
  pars[["dosinginfo"]] <- list(initial.dose = NULL,
                               doses.per.day=3,
                               daily.dose=1,
                               dosing.matrix=NULL,
                               forcings = NULL)

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:5]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})


# --- TEST FOR DOSING MATRIX

test_that("Run_ADME_Model() produces a solution output for a dosing matrix",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()
  pars[["dosinginfo"]] <- list(initial.dose = NULL,
                               doses.per.day=NULL,
                               daily.dose=NULL,
                               dosing.matrix=matrix(c(0,0.25,0.5,0.75,1,1,1,1),
                                                    ncol = 2,
                                                    dimnames = list(c(),c("time","dose"))),
                               forcings = NULL)

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:5]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})


# --- TEST FOR 1COMP MODEL

test_that("Run_ADME_Model() produces a solution output for the 1comp model",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()
  pars[["model"]] <- "1compartment"
  pars[["initvals"]] <- setNames(rep(0,4),c("Agutlumen","Acompartment","Ametabolized","AUC"))

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:5]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})


# --- TEST FOR 3COMP MODEL

test_that("Run_ADME_Model() produces a solution output for the 3comp model",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()
  pars[["model"]] <- "3compartment"
  pars[["initvals"]] <- stats::setNames(rep(0,7),
                                        c("Aintestine","Aportven","Aliver","Asyscomp","Ametabolized","Atubules","AUC"))

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:8]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})

# --- TEST FOR PBTK MODEL

test_that("Run_ADME_Model() produces a solution output for the 3comp model",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()
  pars[["model"]] <- "pbtk"
  pars[["initvals"]] <- stats::setNames(rep(0,11),
                                        c("Agutlumen","Agut","Aliver","Aven","Alung","Aart","Arest","Akidney","Atubules","Ametabolized","AUC"))

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:13]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})


# --- TEST FOR FETAL PBTK MODEL

test_that("Run_ADME_Model() produces a solution output for the fetal_pbtk model",{

  # --- CREATE EXPECTED OUTPUT
  pars <- Generate_Pars()
  pars[["model"]] <- "fetal_pbtk"
  pars[["initvals"]] <- stats::setNames(rep(0,24),
                                        c("Agutlumen", "Agut", "Aliver", "Aven", "Alung",
                                          "Aart", "Aadipose", "Arest", "Akidney", "Atubules",
                                          "Ametabolized", "AUC", "fAUC", "Athyroid",
                                          "Aplacenta", "Afgut", "Aflung", "Afliver", "Afven",
                                          "Afart", "Afrest", "Afthyroid", "Afkidney", "Afbrain"))
  pars[["returntimes"]] <- seq(91,101,1)

  # --- CREATE INPUT
  sol <- solve_httk(1,pars)
  out <- sol[,1:31]

  # --- TEST (current,target)
  expect_equal(Run_ADME_Model(1,pars),out)
})


########################################################
# --- ASSIGN ARRAY NAMES
########################################################

test_that("AssignArrayNames() returns a list of 2 arrays",{

  # --- CREATE INPUT
  pars <- Generate_Pars()

  modsol1 <- solve_httk(1,pars)[,1:5]
  sol <- array(data = rep(0,nrow(modsol1)*5*2),
               dim = c(nrow(modsol1),5,2))
  sol[,,1] <- modsol1
  modsol2 <- solve_httk(2,pars)[,1:5]
  sol[,,2] <- modsol2

  TKSumArray <- array(data = rep(0,4*3*2),
                      dim = (c(4,3,2)))
  TKSumArray[,,1] <- TKsummary(modsol1)
  TKSumArray[,,2] <- TKsummary(modsol2)

  # --- CREATE EXPECTED OUTPUT
  sol_new <- sol
  TKSumArray_new <- TKSumArray
  dimnames(sol_new) <- list(c(),
                        c("time","Agutlumen","Ccompartment","Ametabolized","AUC"),
                        c("Ibuprofen","Terbufos"))
  dimnames(TKSumArray_new) <- list(c("Agutlumen","Ccompartment","Ametabolized","AUC"),
                               c("Tmax","MaxValue","AUC"),
                               c("Ibuprofen","Terbufos"))
  out <- list(sol_new,TKSumArray_new)

  # --- TEST (current,target)
  expect_equal(AssignArrayNames(sol,modsol1,TKSumArray,pars),out)
})

########################################################
# --- REARRANGE TK SUMMARY ARRAY INTO A MATRIX
########################################################

test_that("Rearr_TKSumArray() returns a matrix of TK summary statistics",{

  # --- CREATE INPUT
  pars <- Generate_Pars()
  modsol1 <- solve_httk(1,pars)[,1:5]
  modsol2 <- solve_httk(2,pars)[,1:5]

  TKSumArray <- array(data = rep(0,4*3*2),
                      dim = (c(4,3,2)),
                      dimnames = list(c("Agutlumen","Ccompartment","Ametabolized","AUC"),
                                      c("Tmax","MaxValue","AUC"),
                                      c("Ibuprofen","Terbufos")))
  TKSumArray[,,1] <- TKsummary(modsol1)
  TKSumArray[,,2] <- TKsummary(modsol2)

  # --- CREATE EXPECTED OUTPUT
  out_mat <- matrix(TKSumArray,
                    nrow = 4,
                    ncol = 6,
                    dimnames = list(c("Agutlumen","Ccompartment","Ametabolized","AUC"),
                                    c("Tmax.Ibuprofen","MaxValue.Ibuprofen","AUC.Ibuprofen","Tmax.Terbufos","MaxValue.Terbufos","AUC.Terbufos")))

  # --- TEST (current,target)
  expect_equal(Rearr_TKSumArray(TKSumArray,pars),out_mat)
})


