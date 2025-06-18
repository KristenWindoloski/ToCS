
# Set up a new environment in ToCS package
the <- new.env(parent = emptyenv())

# Save httk data frames needed (to avoid using global environment)
the$physiology.data <- httk::physiology.data
the$tissue.data <- httk::tissue.data
the$mecdt <- httk::mecdt
the$mcnally_dt <- httk::mcnally_dt
the$bmiage <- httk::bmiage
the$wfl <- httk::wfl
the$well_param <- httk::well_param

# Define 'global' variables in ToCS
the$ics <- names_ICs()
the$ic_names <- the$ics[[1]]
the$ic_comps <- the$ics[[2]]

# if (exists("chem.physical_and_invitro.data",envir = .GlobalEnv)){
#   shiny::stopApp()
#   print("Please clear the global environment of the variable 'chem.physical_and_invitro.data'. Then, run ToCS again.")
# }
