
###################################################
# TEST ALL SHINY REACTIVES
###################################################

# Set up a new environment in ToCS package
# the <- new.env(parent = emptyenv())
#
# # Save httk data frames needed (to avoid using global environment)
# the$chem.physical_and_invitro.data <- httk::chem.physical_and_invitro.data
# the$physiology.data <- httk::physiology.data
# the$tissue.data <- httk::tissue.data
# the$mecdt <- httk::mecdt
# the$mcnally_dt <- httk::mcnally_dt
# the$bmiage <- httk::bmiage
# the$wfl <- httk::wfl
# the$well_param <- httk::well_param

##########################################################################
# HAS A FILE WITH COMPOUND DATA BEEN UPLOADED?
##########################################################################

test_that("getData() reactive in app server returns whether a file with compound info has been uploaded",{
  shiny::testServer(run_ToCS(),exp = {

    # VALUE IF NO FILE WITH COMPOUNDS IS UPLOADED
    session$setInputs(file1 = NULL)
    expect_false(getData())

    # --- VALUE IF FILE IS UPLOADED
    session$setInputs(file1 = data.frame(name = "4SampleChems.csv",
                                         size = 1.58,
                                         type = "text//csv",
                                         datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv"))
    expect_true(getData())
  })
})


##########################################################################
# COMPILES LIST OF ALL COMPOUNDS TO RUN
##########################################################################

test_that("CompLst() reactive outputs a list of compound names",{

  shiny::testServer(run_ToCS(),exp = {

    # --- TEST 1 (ONLY PRELOADED)
    session$setInputs(func = "Concentration-time profiles",
                      spec = "Human",
                      defaulttoHuman = "Yes",
                      model = "pbtk",
                      insilicopars = "No, do not load in silico parameters",
                      httkPreloadComps = c("94-75-7, 2,4-d",
                                           "90-43-7, 2-phenylphenol",
                                           "1007-28-9, 6-desisopropylatrazine",
                                           "71751-41-2, Abamectin"),
                      file1 = NULL,
                      HondaIVIVE = "NULL")

    out <- data.frame(Selected_Compounds = c("2,4-d","2-phenylphenol","6-desisopropylatrazine","Abamectin"))
    expect_equal(CompLst(),out)

    #--- TEST 2 (ONLY UPLOADED - Concentration-time profiles)
    UploadedComps <- data.frame(name = "4SampleChems.csv",
                                size = 1.58,
                                type = "text/csv",
                                datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv")

    session$setInputs(func = "Concentration-time profiles",
                      spec = "Human",
                      defaulttoHuman = "Yes",
                      model = "pbtk",
                      insilicopars = "No, do not load in silico parameters",
                      httkPreloadComps = NULL,
                      file1 = UploadedComps,
                      HondaIVIVE = NULL)

    out <- data.frame(Selected_Compounds = c("Chem1","Chem2","Chem3","Chem4"))
    expect_equal(CompLst(),out)

    #--- TEST 3 (ONLY UPLOADED - IVIVE, Honda=NULL)
    UploadedComps <- data.frame(name = "4SampleChems.csv",
                                size = 1.58,
                                type = "text/csv",
                                datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv")

    session$setInputs(func = "In vitro in vivo extrapolation (IVIVE)",
                      spec = "Human",
                      defaulttoHuman = "Yes",
                      model = "pbtk",
                      insilicopars = "No, do not load in silico parameters",
                      httkPreloadComps = NULL,
                      file1 = UploadedComps,
                      HondaIVIVE = NULL)

    out <- data.frame(Selected_Compounds = c("Chem1","Chem2","Chem3","Chem4"))
    expect_equal(CompLst(),out)

    #--- TEST 4 (ONLY UPLOADED - IVIVE, Honda=Honda1)
    UploadedComps <- data.frame(name = "4SampleChems.csv",
                                size = 1.58,
                                type = "text/csv",
                                datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv")

    session$setInputs(func = "In vitro in vivo extrapolation (IVIVE)",
                      spec = "Human",
                      defaulttoHuman = "Yes",
                      model = "pbtk",
                      insilicopars = "No, do not load in silico parameters",
                      httkPreloadComps = NULL,
                      file1 = UploadedComps,
                      HondaIVIVE = "Honda1")

    expect_error(CompLst())

    #--- TEST 5 (ONLY UPLOADED - IVIVE, Honda=Honda2)
    UploadedComps <- data.frame(name = "4SampleChems.csv",
                                size = 1.58,
                                type = "text/csv",
                                datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv")

    session$setInputs(func = "In vitro in vivo extrapolation (IVIVE)",
                      spec = "Human",
                      defaulttoHuman = "Yes",
                      model = "pbtk",
                      insilicopars = "No, do not load in silico parameters",
                      httkPreloadComps = NULL,
                      file1 = UploadedComps,
                      HondaIVIVE = "Honda2")

    out <- data.frame(Selected_Compounds = c("Chem1","Chem2","Chem3","Chem4"))
    expect_equal(CompLst(),out)

    # --- TEST 5 (PRELOADED AND UPLOADED)

    session$setInputs(func = "Concentration-time profiles",
                      spec = "Human",
                      defaulttoHuman = "Yes",
                      model = "pbtk",
                      insilicopars = "No, do not load in silico parameters",
                      httkPreloadComps = c("94-75-7, 2,4-d",
                                           "90-43-7, 2-phenylphenol",
                                           "1007-28-9, 6-desisopropylatrazine",
                                           "71751-41-2, Abamectin"),
                      file1 = UploadedComps)

    out <- data.frame(Selected_Compounds = c("2,4-d","2-phenylphenol","6-desisopropylatrazine","Abamectin",
                                             "Chem1","Chem2","Chem3","Chem4"))
    expect_equal(CompLst(),out)

  })
})


##########################################################################
# COMPILES LIST OF ALL COMPOUNDS TO RUN
##########################################################################

# test_that("output$comptext outputs a table of compounds to simulate",{
#
#   shiny::testServer(run_ToCS(),exp = {
#
#   # --- TEST ALL COMBOS FOR WORKING AND VALIDATION TEXT
#   session$setInputs(func = "Concentration-time profiles",
#                   spec = "Human",
#                   defaulttoHuman = "Yes",
#                   model = "pbtk",
#                   insilicopars = "No, do not load in silico parameters",
#                   httkPreloadComps = c("94-75-7, 2,4-d",
#                                        "90-43-7, 2-phenylphenol",
#                                        "1007-28-9, 6-desisopropylatrazine",
#                                        "71751-41-2, Abamectin"),
#                   file1 = NULL)
#
#   attach(the)
#   out <- data.frame(Selected_Compounds = c("2,4-d","2-phenylphenol","6-desisopropylatrazine","Abamectin"))
#   new_output <- as.data.frame(rvest::read_html(output$comptext) %>% rvest::html_table(fill=TRUE))
#   expect_equal(new_output,out)
#   detach(the)
#   })
# })


##########################################################################
# TEST ADME MODULE
##########################################################################

# ### STILL WORKING ON THE BELOW TEST - NEED TO TROUBLESHOOT
#
# test_that("output$tksummaryCaption reactive returns table caption text",{
#
#   adme_args <- shiny::reactiveVal()
#   shiny::testServer(ADME_TKTable_server(),args = list(var = adme_args), exp = {
#
#     # --- GENERATE INPUT PARAMETERS
#     pars <- list(runsim = 1,model = "pbtk")
#     adme_args(list(1,pars))
#
#     # --- TEST
#     out <- paste("Table 1: Table of summary statistics (Tmax - time to maximal concentration,
#       MaxValue - maximal amount (A, umol) or concentration (C, uM), AUC - area
#       under the curve (uM*days)) for each compartment for each selected compound.")
#
#     session$flushReact()
#     expect_equal(output$tksummaryCaption,out)
#   })
# })
#
# ### STILL WORKING ON THE BELOW TEST - NEED TO TROUBLESHOOT
#
# test_that("output$ADME1plotsCaption reactive returns plot caption text",{
#
#   adme_args <- shiny::reactiveVal()
#   shiny::testServer(ADME_MultPlt_server(),args = list(var = adme_args), exp = {
#
#     # --- GENERATE INPUT PARAMETERS
#     pars <- list(runsim = 1,model = "pbtk")
#     adme_args(list(1,pars))
#
#     # --- TEST
#     out <- paste("Table 1: Table of summary statistics (Tmax - time to maximal concentration,
#       MaxValue - maximal amount (A, umol) or concentration (C, uM), AUC - area
#       under the curve (uM*days)) for each compartment for each selected compound.")
#
#     session$flushReact()
#     expect_equal(output$tksummaryCaption,out)
#   })
# })
