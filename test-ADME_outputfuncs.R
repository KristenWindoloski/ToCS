
rm(list = ls())

library(testthat)
library(httk)
library(DescTools)
library(ggplot2)
library(gridExtra)

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
               initvals = setNames(rep(0,4), 
                                   c("Agutlumen","Acompartment","Ametabolized","AUC")),
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

######################################################
# --- DETERMINE LOG BREAKS IN ADME PLOTS
######################################################

test_that("log10breaks_ADME() produces a power of 10 sequence",{

  # --- CREATE SAMPLE DATA
  set.seed(1)
  ydata <- runif(100,min = 0,max = 10)

  # --- TEST
  expect_equal(log10breaks_ADME(ydata),c(10e-2,10e0))
})

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
  AUC1 <- signif(AUC(x = sol[,1], y = sol[,"Agutlumen"], method = "trapezoid"),4)
  AUC2 <- signif(AUC(x = sol[,1], y = sol[,"Ccompartment"], method = "trapezoid"),4)
  AUC3 <- signif(AUC(x = sol[,1], y = sol[,"Ametabolized"], method = "trapezoid"),4)
  AUC4 <- signif(AUC(x = sol[,1], y = sol[,"AUC"], method = "trapezoid"),4)
  df_final <- data.frame(Compartment = c("Agutlumen","Ccompartment","Ametabolized","AUC"),
                         Tmax = c('0','0.1','1','1'),
                         MaxValue = c('292.5','6.36','151.4','4.729'),
                         AUC = as.character(c(AUC1,AUC2,AUC3,AUC4)))

  # --- TEST (current,target)
  expect_equal(TKsummary(sol),df_final)
})

########################################################
# --- SOLVE MODEL FOR ADME SOLUTION AND TK SUMMARY
########################################################

test_that("modsol() produces a list of 3 objects",{

  # --- CREATE INPUT
  pars <- Generate_Pars()

  # --- TEST
  expect_true(is.list(modsol(pars)))
  expect_true(is.array(modsol(pars)[[1]]))
  expect_true(is.array(modsol(pars)[[2]]))
  expect_true(is.data.frame(modsol(pars)[[3]]))
})

########################################################
# --- ARRANGES PLOTS ON THE SAME GRID
########################################################

test_that("plt_arrange() produces a list of plots with subplots for all compounds",{

  # --- INPUT
  p_list <- vector('list',2)
  arr = array(2:13, dim = c(2, 3, 2), dimnames = list(c(), c("A","B","C"), c())) 
  for (i in 1:2) {
    indiv_p_lst <- vector('list',2)
    for (j in 2:3) {
      df <- data.frame(Time = arr[,1,i], Ydata = arr[,j,i])
      indiv_p_lst[[j-1]] <- ggplot(df, aes(Time, Ydata)) +
        geom_line(linewidth=1) +
        labs(x = "Time (Days)", y = colnames(arr[,j,1]))
    }

    # --- Save all subplots for compound i 
    p_list[[i]] <- indiv_p_lst
  }
  
  # --- CREATE EXPECTED OUTPUT
  outlist <- list()
  outlist[[1]] <- grid.arrange(grobs = p_list[[1]])
  outlist[[2]] <- grid.arrange(grobs = p_list[[2]])
  
  new_df <- plt_arrange(p_list)

  # --- TEST
  expect_true(is.list(plt_arrange(p_list)))
  expect_equal(length(plt_arrange(p_list)),2)
  # expect_equal(plt_arrange(p_list),outlist)
})


