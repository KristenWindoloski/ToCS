

test_that("CompoundList() produces a data frame of selected compounds", {

  # --- CREATE SAMPLE DATA
  PreloadedComps <- c("71751-41-2, Abamectin","94-82-6, 2,4-db","148-79-8, Thiabendazole")
  PreloadedCompsHonda <- c("1912-24-9, Atrazine","15687-27-1, Ibuprofen","79-06-1, Acrylamide")
  UploadedComps <- data.frame(name = "4SampleChems.csv",
                              size = 1.58,
                              type = "text/csv",
                              datapath = "C:/Users/Kristen.Windoloski/OneDrive - FDA/httk Project/ToCS/tests/testthat/4SampleChems.csv")

  # --- CREATE EXPECTED OUTPUT
  df_PConly <- data.frame(Selected_Compounds = c("2,4-db","Abamectin","Thiabendazole"))
  df_PCHondaonly <- data.frame(Selected_Compounds = c("Acrylamide","Atrazine","Ibuprofen"))
  df_UConly <- data.frame(Selected_Compounds = c("Chem1","Chem2","Chem3","Chem4"))
  df_PCUC <- data.frame(Selected_Compounds = c("2,4-db","Abamectin","Chem1","Chem2","Chem3","Chem4","Thiabendazole"))
  df_PCHondaUC <- data.frame(Selected_Compounds = c("Acrylamide","Atrazine","Chem1","Chem2","Chem3","Chem4","Ibuprofen"))

  # --- TEST
  #just preloadcomps
  expect_equal(CompoundList(PreloadedComps,NULL,NULL),df_PConly)

  #just preloadcompsHonda1
  expect_equal(CompoundList(NULL,PreloadedCompsHonda,NULL),df_PCHondaonly)

  #just uploaded comps
  expect_equal(CompoundList(NULL,NULL,UploadedComps),df_UConly)

  #preload and upload comps
  expect_equal(CompoundList(PreloadedComps,NULL,UploadedComps),df_PCUC)

  #preloadhonda1 and uploaded comps
  expect_equal(CompoundList(NULL,PreloadedCompsHonda,UploadedComps),df_PCHondaUC)
})
