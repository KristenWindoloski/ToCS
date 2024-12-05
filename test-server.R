
###################################################
# TEST ALL SHINY REACTIVES
###################################################

##########################################################################
# HAS A FILE WITH COMPOUND DATA BEEN UPLOADED?
##########################################################################

# test_that("getData() reactive in app server returns whether a file with compound info has been uploaded",{
#   shiny::testServer(ToCS(),exp = {
#
#     session$setInputs(file1 = NULL)
#     expect_false(getData())
#
#     session$setInputs(file1 = data.frame(name = "4SampleChems.csv",
#                                          size = 1.58,
#                                          type = "text//csv",
#                                          datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv"))
#     expect_true(getData())
#   })
# })


##########################################################################
# COMPILES LIST OF ALL COMPOUNDS TO RUN
##########################################################################

test_that("CompLst event reactive outputs a list of compound names",{

  shiny::testServer(ToCS(),exp = {

    #--- TEST 1
    # session$setInputs(func = "Concentration-time profiles",
    #                   spec = "Human",
    #                   defaulttoHuman = "TRUE",
    #                   runCompounds = 1,
    #                   model = "pbtk",
    #                   httkPreloadComps = c("94-75-7, 2,4-d",
    #                                        "90-43-7, 2-phenylphenol",
    #                                        "1007-28-9, 6-desisopropylatrazine",
    #                                        "71751-41-2, Abamectin"),
    #                   httkPreloadComps_Honda = NULL,
    #                   file1 = NULL)
    #
    # out <- data.frame(Selected_Compounds = c("2,4-d","2-phenylphenol","6-desisopropylatrazine","Abamectin"))
    # expect_equal(CompLst(),out)

    #--- TEST 2
    session$setInputs(func = "Select",
                      spec = "Human",
                      defaulttoHuman = "TRUE",
                      runCompounds = 1,
                      model = "pbtk",
                      httkPreloadComps = c("94-75-7, 2,4-d",
                                           "94-82-6, 2,4-db",
                                           "90-43-7, 2-phenylphenol",
                                           "1007-28-9, 6-desisopropylatrazine",
                                           "71751-41-2, Abamectin"),
                      httkPreloadComps_Honda = NULL,
                      file1 = NULL)

    print(CompLst())
    expect_error(CompLst(),"Error: Desired output")
  })
})
