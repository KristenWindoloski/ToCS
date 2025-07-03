
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

# Define 'global' variables in ToCS
the$ics <- names_ICs()
the$ic_names <- the$ics[[1]]
the$ic_comps <- the$ics[[2]]
