
# Set up a new environment in ToCS package
the <- new.env(parent = emptyenv())

# Save httk data frames needed (to avoid using global environment)
the$chem.physical_and_invitro.data <- httk::chem.physical_and_invitro.data
the$physiology.data <- httk::physiology.data
the$tissue.data <- httk::tissue.data
the$mecdt <- httk::mecdt
the$mcnally_dt <- httk::mcnally_dt
the$bmiage <- httk::bmiage
the$wfl <- httk::wfl
the$well_param <- httk::well_param

test_that("getCASnums() produces vector of CAS numbers", {

  attach(the)

  # --- TEST
  func <- "Concentration-time profiles"
  spec <- "Human"
  model <- "3compartmentss"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- "No"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- "No"
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Concentration-time profiles"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- "No"
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Concentration-time profiles"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- "No"
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Concentration-time profiles"
  spec <- "Human"
  model <- "full_pregnancy"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  # --------------
  func <- "Parameter calculations"
  spec <- "Human"
  model <- "3compartmentss"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- "Yes"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- "No"
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- "No"
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Parameter calculations"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- "No"
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Parameter calculations"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- "No"
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  detach(the)
})

test_that("getPiped() produces the list of compounds to display in the GUI",{

  # --- CREATE SAMPLE DATA
  CASnums <- c("94-75-7","94-82-6","90-43-7","1007-28-9","71751-41-2")
  OutList <- c("94-75-7, 2,4-d",
                "94-82-6, 2,4-db",
                "90-43-7, 2-phenylphenol",
                "1007-28-9, 6-desisopropylatrazine",
                "71751-41-2, Abamectin")
  OutListHonda <- c("94-75-7, 2,4-d",
                     "94-82-6, 2,4-db",
                     "90-43-7, 2-phenylphenol",
                     "1007-28-9, 6-desisopropylatrazine")
  CASnumsFood <- c("90-43-7","80-05-7","533-74-4","122-39-4")
  OutListFood <- c("90-43-7, 2-phenylphenol",
                    "80-05-7, Bisphenol-a",
                    "533-74-4, Dazomet",
                    "122-39-4, Diphenylamine")

  # --- TEST
  attach(the)

  expect_true(is.null(getPiped(NULL,NULL,"Choose from all available chemicals")))
  expect_equal(getPiped(CASnums,"NULL","Choose from all available chemicals"), OutList)
  expect_equal(getPiped(CASnums,"Honda1","Choose from all available chemicals"), OutListHonda)
  expect_true(is.null(getPiped(NULL,NULL,"Choose from only food relevant chemicals")))
  expect_equal(getPiped(CASnumsFood,"NULL","Choose from only food relevant chemicals"), OutListFood)
  expect_equal(getPiped(CASnumsFood,"Honda1","Choose from only food relevant chemicals"), OutListFood)

  detach(the)
})
