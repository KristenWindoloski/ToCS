test_that("getCASnums() produces vector of CAS numbers", {

  # --- TEST
  func <- "Concentration-time profiles"
  spec <- "Human"
  model <- "3compartmentss"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- FALSE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Concentration-time profiles"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- FALSE
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Concentration-time profiles"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- FALSE
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Concentration-time profiles"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- FALSE
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  # --------------
  func <- "Parameter calculations"
  spec <- "Human"
  model <- "3compartmentss"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- TRUE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Rat"
  model <- "1compartment"
  defaulthuman <- FALSE
  expect_gt(length(getCASnums(func,spec,model,defaulthuman)),0)

  func <- "Parameter calculations"
  spec <- "Mouse"
  model <- "3compartment"
  defaulthuman <- FALSE
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Parameter calculations"
  spec <- "Rabbit"
  model <- "pbtk"
  defaulthuman <- FALSE
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))

  func <- "Parameter calculations"
  spec <- "Dog"
  model <- "fetal_pbtk"
  defaulthuman <- FALSE
  expect_true(is.null(getCASnums(func,spec,model,defaulthuman)))
})


test_that("getPiped() produces the list of compounds to display in the GUI",{

  # --- CREATE SAMPLE DATA
  CASnums <- c("94-75-7","94-82-6","90-43-7","1007-28-9","71751-41-2")
  FullList <- c("94-75-7, 2,4-d",
                "94-82-6, 2,4-db",
                "90-43-7, 2-phenylphenol",
                "1007-28-9, 6-desisopropylatrazine",
                "71751-41-2, Abamectin")
  FullListHonda <- c("94-75-7, 2,4-d",
                     "94-82-6, 2,4-db",
                     "90-43-7, 2-phenylphenol",
                     "1007-28-9, 6-desisopropylatrazine")

  # --- TEST
  expect_true(is.null(getPiped(NULL,NULL)))
  expect_equal(getPiped(CASnums,"NULL"), FullList)
  expect_equal(getPiped(CASnums,"Honda1"), FullListHonda)
})










